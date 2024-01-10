use std::cmp;
use std::error;
use std::fmt;
use std::io;
use std::mem;
use std::default::Default;

use crate::Repeat;
use crate::common::{AnyExtension, Block, DisposalMethod, Extension, Frame};
use crate::reader::DecodeOptions;

use weezl::{BitOrder, decode::Decoder as LzwDecoder, LzwError, LzwStatus};

/// GIF palettes are RGB
pub const PLTE_CHANNELS: usize = 3;

/// An error returned in the case of the image not being formatted properly.
#[derive(Debug)]
pub struct DecodingFormatError {
    underlying: Box<dyn error::Error + Send + Sync + 'static>,
}

impl fmt::Display for DecodingFormatError {
    #[cold]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&*self.underlying, fmt)
    }
}

impl error::Error for DecodingFormatError {
    #[cold]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&*self.underlying as _)
    }
}

#[derive(Debug)]
/// Decoding error.
pub enum DecodingError {
    /// Returned if the image is found to be malformed.
    Format(DecodingFormatError),
    /// Wraps `std::io::Error`.
    Io(io::Error),
}

impl DecodingError {
    #[cold]
    pub(crate) fn format(err: &'static str) -> Self {
        DecodingError::Format(DecodingFormatError {
            underlying: err.into(),
        })
    }
}

impl fmt::Display for DecodingError {
    #[cold]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            DecodingError::Format(ref d) => d.fmt(fmt),
            DecodingError::Io(ref err) => err.fmt(fmt),
        }
    }
}

impl error::Error for DecodingError {
    #[cold]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            DecodingError::Format(ref err) => Some(err),
            DecodingError::Io(ref err) => Some(err),
        }
    }
}

impl From<io::Error> for DecodingError {
    #[inline]
    fn from(err: io::Error) -> Self {
        DecodingError::Io(err)
    }
}

impl From<io::ErrorKind> for DecodingError {
    #[cold]
    fn from(err: io::ErrorKind) -> Self {
        DecodingError::Io(io::Error::from(err))
    }
}

impl From<DecodingFormatError> for DecodingError {
    #[inline]
    fn from(err: DecodingFormatError) -> Self {
        DecodingError::Format(err)
    }
}

/// Configures how extensions should be handled
#[derive(PartialEq, Debug)]
pub enum Extensions {
    /// Saves all extention data
    Save,
    /// Skips the data of unknown extensions
    /// and extracts the data from known ones
    Skip,
}

#[derive(Debug, Copy, Clone)]
pub enum FrameDataType {
    Pixels,
    Lzw { min_code_size: u8 },
}

/// Indicates whether a certain object has been decoded
#[derive(Debug)]
#[non_exhaustive]
pub enum Decoded<'a> {
    /// Decoded nothing.
    Nothing,
    /// Global palette.
    GlobalPalette(Vec<u8>),
    /// Index of the background color in the global palette.
    BackgroundColor(u8),
    /// Loop count is known
    Repetitions(Repeat),
    /// Palette and optional `Application` extension have been parsed,
    /// reached frame data.
    HeaderEnd,
    /// Decoded the image trailer.
    Trailer,
    /// The start of a block.
    BlockStart(Block),
    /// Decoded a sub-block. More sub-block are available.
    ///
    /// Indicates the label of the extension which might be unknown. A label of `0` is used when
    /// the sub block does not belong to an extension.
    SubBlockFinished(AnyExtension, &'a [u8]),
    /// Decoded the last (or only) sub-block of a block.
    ///
    /// Indicates the label of the extension which might be unknown. A label of `0` is used when
    /// the sub block does not belong to an extension.
    BlockFinished(AnyExtension, &'a [u8]),
    /// Decoded all information of the next frame, except the image data.
    ///
    /// The returned frame does **not** contain any owned image data.
    FrameMetadata(&'a mut Frame<'static>, FrameDataType),
    /// Decoded some data of the current frame.
    BytesDecoded(usize),
    /// Copied (or consumed and discarded) compressed data of the current frame. In bytes.
    LzwDataCopied(usize),
    /// No more data available the current frame.
    DataEnd,
}

/// Internal state of the GIF decoder
#[derive(Debug, Copy, Clone)]
enum State {
    Magic(u8, [u8; 6]),
    U16Byte1(U16Value, u8),
    U16(U16Value),
    Byte(ByteValue),
    GlobalPalette(usize),
    BlockStart(u8),
    /// Block end, with remaining expected data. NonZero for invalid EOF.
    BlockEnd(u8),
    ExtensionBlock(AnyExtension),
    ApplicationExtension,
    SkipBlock(usize),
    LocalPalette(usize),
    LzwInit(u8),
    /// Decompresses LZW
    DecodeSubBlock(usize),
    /// Keeps LZW compressed
    CopySubBlock(usize),
    FrameDecoded,
    Trailer,
}
use self::State::*;

/// U16 values that may occur in a GIF image
#[derive(Debug, Copy, Clone)]
enum U16Value {
    /// Logical screen descriptor width
    ScreenWidth,
    /// Logical screen descriptor height
    ScreenHeight,
    /// Delay time
    Delay,
    /// Left frame offset
    ImageLeft,
    /// Top frame offset
    ImageTop,
    /// Frame width
    ImageWidth,
    /// Frame height
    ImageHeight,
}

/// Single byte screen descriptor values
#[derive(Debug, Copy, Clone)]
enum ByteValue {
    GlobalFlags,
    Background { global_flags: u8 },
    AspectRatio { global_flags: u8 },
    ControlFlags,
    ImageFlags,
    TransparentIdx,
    CodeSize,
}

struct LzwReader {
    decoder: Option<LzwDecoder>,
    min_code_size: u8,
    check_for_end_code: bool,
}

impl LzwReader {
    pub fn new(check_for_end_code: bool) -> Self {
        Self {
            decoder: None,
            min_code_size: 0,
            check_for_end_code,
        }
    }

    pub fn reset(&mut self, min_code_size: u8) -> Result<(), DecodingError> {
        // LZW spec: max 12 bits per code
        if min_code_size > 11 {
            return Err(DecodingError::format("invalid minimal code size"));
        }

        // The decoder can be reused if the code size stayed the same
        if self.min_code_size != min_code_size || self.decoder.is_none() {
            self.min_code_size = min_code_size;
            self.decoder = Some(LzwDecoder::new(BitOrder::Lsb, min_code_size));
        } else {
            self.decoder.as_mut().unwrap().reset();
        }

        Ok(())
    }

    pub fn has_ended(&self) -> bool {
        self.decoder.as_ref().map_or(true, |e| e.has_ended())
    }

    pub fn decode_bytes(&mut self, lzw_data: &[u8], decode_buffer: &mut OutputBuffer<'_>) -> io::Result<(usize, usize)> {
        let decoder = self.decoder.as_mut().ok_or(io::ErrorKind::Unsupported)?;

        let decode_buffer = match decode_buffer {
            OutputBuffer::Slice(buf) => &mut **buf,
            OutputBuffer::None => &mut [],
            OutputBuffer::Vec(_) => return Err(io::Error::from(io::ErrorKind::Unsupported)),
        };

        let decoded = decoder.decode_bytes(lzw_data, decode_buffer);

        match decoded.status {
            Ok(LzwStatus::Done | LzwStatus::Ok) => {},
            Ok(LzwStatus::NoProgress) => {
                if self.check_for_end_code {
                    return Err(io::Error::new(io::ErrorKind::InvalidData, "no end code in lzw stream"));
                }
            },
            Err(err @ LzwError::InvalidCode) => {
                return Err(io::Error::new(io::ErrorKind::InvalidData, err).into());
            }
        }
        Ok((decoded.consumed_in, decoded.consumed_out))
    }
}

/// GIF decoder which supports streaming
pub struct StreamingDecoder {
    state: State,
    lzw_reader: LzwReader,
    skip_frame_decoding: bool,
    check_frame_consistency: bool,
    allow_unknown_blocks: bool,
    version: Version,
    width: u16,
    height: u16,
    global_color_table: Vec<u8>,
    background_color: [u8; 4],
    /// ext buffer
    ext: ExtensionData,
    /// Frame data
    current: Option<Frame<'static>>,
    /// Needs to emit `HeaderEnd` once
    header_end_reached: bool,
}

/// One version number of the GIF standard.
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum Version {
    /// Version 87a, from May 1987.
    V87a,
    /// Version 89a, from July 1989.
    V89a,
}

struct ExtensionData {
    id: AnyExtension,
    data: Vec<u8>,
    is_block_end: bool,
}

pub enum OutputBuffer<'a> {
    /// Overwrite bytes
    Slice(&'a mut [u8]),
    /// Append LZW bytes
    Vec(&'a mut Vec<u8>),
    /// Discard bytes
    None,
}

impl StreamingDecoder {
    /// Creates a new streaming decoder
    #[must_use]
    pub fn new() -> StreamingDecoder {
        let options = DecodeOptions::new();
        Self::with_options(&options)
    }

    pub(crate) fn with_options(options: &DecodeOptions) -> Self {
        StreamingDecoder {
            state: Magic(0, [0; 6]),
            lzw_reader: LzwReader::new(options.check_for_end_code),
            skip_frame_decoding: options.skip_frame_decoding,
            check_frame_consistency: options.check_frame_consistency,
            allow_unknown_blocks: options.allow_unknown_blocks,
            version: Version::V87a,
            width: 0,
            height: 0,
            global_color_table: Vec::new(),
            background_color: [0, 0, 0, 0xFF],
            ext: ExtensionData {
                id: AnyExtension(0),
                data: Vec::with_capacity(256), // 0xFF + 1 byte length
                is_block_end: true,
            },
            current: None,
            header_end_reached: false,
        }
    }

    /// Updates the internal state of the decoder.
    ///
    /// Returns the number of bytes consumed from the input buffer
    /// and the last decoding result.
    pub fn update<'a>(
        &'a mut self,
        mut buf: &[u8],
        write_into: &mut OutputBuffer<'_>,
    ) -> Result<(usize, Decoded<'a>), DecodingError> {
        // NOTE: Do not change the function signature without double-checking the
        //       unsafe block!
        let len = buf.len();
        while !buf.is_empty() {
            // This dead code is a compile-check for lifetimes that otherwise aren't checked
            // due to the `mem::transmute` used later.
            // Keep it in sync with the other call to `next_state`.
            #[cfg(test)]
            if false {
                return self.next_state(buf, write_into);
            }

            // It's not necessary to check here whether state is `Some`,
            // because `next_state` checks it anyway, and will return `DecodeError`
            // if the state has already been set to `None`.
            match self.next_state(buf, write_into) {
                Ok((bytes, Decoded::Nothing)) => {
                    buf = &buf[bytes..];
                }
                Ok((bytes, result)) => {
                    buf = &buf[bytes..];
                    return Ok(
                        (len-buf.len(), 
                        // This transmute just casts the lifetime away. Since Rust only 
                        // has SESE regions, this early return cannot be worked out and
                        // such that the borrow region of self includes the whole block.
                        // The explixit lifetimes in the function signature ensure that
                        // this is safe.
                        // ### NOTE
                        // To check that everything is sound, return the result without
                        // the match (e.g. `return Ok(self.next_state(buf)?)`). If
                        // it compiles the returned lifetime is correct.
                        unsafe { 
                            mem::transmute::<Decoded<'_>, Decoded<'a>>(result)
                        }
                    ))
                }
                Err(err) => return Err(err),
            }
        }
        Ok((len - buf.len(), Decoded::Nothing))
    }

    /// Returns the data of the last extension that has been decoded.
    #[must_use]
    pub fn last_ext(&self) -> (AnyExtension, &[u8], bool) {
        (self.ext.id, &self.ext.data, self.ext.is_block_end)
    }

    #[inline(always)]
    /// Current frame info as a mutable ref.
    pub fn current_frame_mut(&mut self) -> &mut Frame<'static> {
        self.current.as_mut().unwrap()
    }

    /// Current frame info as a ref.
    #[inline(always)]
    #[track_caller]
    #[must_use]
    pub fn current_frame(&self) -> &Frame<'static> {
        self.current.as_ref().unwrap()
    }

    /// Width of the image
    #[must_use]
    pub fn width(&self) -> u16 {
        self.width
    }

    /// Height of the image
    #[must_use]
    pub fn height(&self) -> u16 {
        self.height
    }

    /// The version number of the GIF standard used in this image.
    ///
    /// We suppose a minimum of `V87a` compatibility. This value will be reported until we have
    /// read the version information in the magic header bytes.
    #[must_use]
    pub fn version(&self) -> Version {
        self.version
    }

    fn next_state(&mut self, buf: &[u8], write_into: &mut OutputBuffer<'_>) -> Result<(usize, Decoded<'_>), DecodingError> {
        macro_rules! goto (
            ($n:expr, $state:expr) => ({
                self.state = $state;
                Ok(($n, Decoded::Nothing))
            });
            ($state:expr) => ({
                self.state = $state;
                Ok((1, Decoded::Nothing))
            });
            ($n:expr, $state:expr, emit $res:expr) => ({
                self.state = $state;
                Ok(($n, $res))
            });
            ($state:expr, emit $res:expr) => ({
                self.state = $state;
                Ok((1, $res))
            })
        );
        
        let b = *buf.get(0).ok_or_else(|| io::ErrorKind::UnexpectedEof)?;

        match self.state {
            Magic(i, mut version) => if i < 6 {
                version[i as usize] = b;
                goto!(Magic(i+1, version))
            } else if &version[..3] == b"GIF" {
                self.version = match &version[3..] {
                    b"87a" => Version::V87a,
                    b"89a" => Version::V89a,
                    _ => return Err(DecodingError::format("malformed GIF header"))
                };
                goto!(U16Byte1(U16Value::ScreenWidth, b))
            } else {
                Err(DecodingError::format("malformed GIF header"))
            },
            U16(next) => goto!(U16Byte1(next, b)),
            U16Byte1(next, value) => {
                use self::U16Value::*;
                let value = (u16::from(b) << 8) | u16::from(value);
                match (next, value) {
                    (ScreenWidth, width) => {
                        self.width = width;
                        goto!(U16(U16Value::ScreenHeight))
                    },
                    (ScreenHeight, height) => {
                        self.height = height;
                        goto!(Byte(ByteValue::GlobalFlags))
                    },
                    (Delay, delay) => {
                        self.current_frame_mut().delay = delay;
                        self.ext.data.push(value as u8);
                        self.ext.data.push(b);
                        goto!(Byte(ByteValue::TransparentIdx))
                    },
                    (ImageLeft, left) => {
                        self.current_frame_mut().left = left;
                        goto!(U16(U16Value::ImageTop))
                    },
                    (ImageTop, top) => {
                        self.current_frame_mut().top = top;
                        goto!(U16(U16Value::ImageWidth))
                    },
                    (ImageWidth, width) => {
                        self.current_frame_mut().width = width;
                        goto!(U16(U16Value::ImageHeight))
                    },
                    (ImageHeight, height) => {
                        self.current_frame_mut().height = height;
                        goto!(Byte(ByteValue::ImageFlags))
                    }
                }
            }
            Byte(value) => {
                use self::ByteValue::*;
                match value {
                    GlobalFlags => {
                        goto!(Byte(Background { global_flags: b }))
                    },
                    Background { global_flags } => {
                        goto!(
                            Byte(AspectRatio { global_flags }),
                            emit Decoded::BackgroundColor(b)
                        )
                    },
                    AspectRatio { global_flags } => {
                        let global_table = global_flags & 0x80 != 0;
                        let table_size = if global_table {
                            let table_size = PLTE_CHANNELS * (1 << ((global_flags & 0b111) + 1) as usize);
                            self.global_color_table.try_reserve_exact(table_size).map_err(|_| io::ErrorKind::OutOfMemory)?;
                            table_size
                        } else {
                            0usize
                        };
                        goto!(GlobalPalette(table_size))
                    },
                    ControlFlags => {
                        self.ext.data.push(b);
                        let frame = self.current_frame_mut();
                        let control_flags = b;
                        if control_flags & 1 != 0 {
                            // Set to Some(...), gets overwritten later
                            frame.transparent = Some(0);
                        }
                        frame.needs_user_input =
                            control_flags & 0b10 != 0;
                        frame.dispose = match DisposalMethod::from_u8(
                            (control_flags & 0b11100) >> 2
                        ) {
                            Some(method) => method,
                            None => DisposalMethod::Any
                        };
                        goto!(U16(U16Value::Delay))
                    }
                    TransparentIdx => {
                        self.ext.data.push(b);
                        if let Some(ref mut idx) = self.current_frame_mut().transparent {
                            *idx = b;
                        }
                        goto!(SkipBlock(0))
                    }
                    ImageFlags => {
                        let local_table = (b & 0b1000_0000) != 0;
                        let interlaced = (b & 0b0100_0000) != 0;
                        let table_size = b & 0b0000_0111;
                        let check_frame_consistency = self.check_frame_consistency;
                        let (width, height) = (self.width, self.height);

                        let frame = self.current_frame_mut();

                        frame.interlaced = interlaced;
                        if check_frame_consistency {
                            // Consistency checks.
                            if width.checked_sub(frame.width) < Some(frame.left)
                                || height.checked_sub(frame.height) < Some(frame.top)
                            {
                                return Err(DecodingError::format("frame descriptor is out-of-bounds"))
                            }
                        }

                        if local_table {
                            let entries = PLTE_CHANNELS * (1 << (table_size + 1));
                            let mut pal = Vec::new();
                            pal.try_reserve_exact(entries).map_err(|_| io::ErrorKind::OutOfMemory)?;
                            frame.palette = Some(pal);
                            goto!(LocalPalette(entries))
                        } else {
                            goto!(Byte(CodeSize))
                        }
                    }
                    CodeSize => goto!(LzwInit(b)),
                }
            }
            GlobalPalette(left) => {
                let n = cmp::min(left, buf.len());
                if left > 0 {
                    self.global_color_table.extend_from_slice(&buf[..n]);
                    goto!(n, GlobalPalette(left - n))
                } else {
                    let idx = self.background_color[0];
                    match self.global_color_table.chunks_exact(PLTE_CHANNELS).nth(idx as usize) {
                        Some(chunk) => self.background_color[..PLTE_CHANNELS]
                            .copy_from_slice(&chunk[..PLTE_CHANNELS]),
                        None => self.background_color[0] = 0
                    }
                    goto!(BlockStart(b), emit Decoded::GlobalPalette(
                        mem::take(&mut self.global_color_table)
                    ))
                }
            }
            BlockStart(type_) => {
                if !self.header_end_reached && type_ != Block::Extension as u8 {
                    self.header_end_reached = true;
                    return goto!(0, BlockStart(type_), emit Decoded::HeaderEnd);
                }

                match Block::from_u8(type_) {
                    Some(Block::Image) => {
                        self.add_frame();
                        goto!(U16Byte1(U16Value::ImageLeft, b), emit Decoded::BlockStart(Block::Image))
                    }
                    Some(Block::Extension) => {
                        goto!(ExtensionBlock(AnyExtension(b)), emit Decoded::BlockStart(Block::Extension))
                    }
                    Some(Block::Trailer) => {
                        goto!(0, State::Trailer, emit Decoded::BlockStart(Block::Trailer))
                    }
                    None => {
                        if self.allow_unknown_blocks {
                            goto!(SkipBlock(b as usize))
                        } else {
                            Err(DecodingError::format("unknown block type encountered"))
                        }
                    }
                }
            },
            BlockEnd(terminator) => {
                if terminator == 0 {
                    if b == Block::Trailer as u8 {
                        // can't consume, because the trailer is not a real block, and won't have futher data
                        goto!(0, BlockStart(b))
                    } else {
                        goto!(BlockStart(b))
                    }
                } else {
                    Err(DecodingError::format(
                        "expected block terminator not found"
                    ))
                }
            }
            ExtensionBlock(id) => {
                use Extension::*;
                self.ext.id = id;
                self.ext.data.clear();
                self.ext.data.push(b);
                if let Some(ext) = Extension::from_u8(id.0) {
                    match ext {
                        Control => {
                            goto!(self.read_control_extension(b)?)
                        }
                        Text | Comment | Application => {
                            goto!(SkipBlock(b as usize))
                        }
                    }
                } else {
                    Err(DecodingError::format("unknown block type encountered"))
                }
            }
            SkipBlock(left) => {
                let n = cmp::min(left, buf.len());
                if left > 0 {
                    self.ext.data.extend_from_slice(&buf[..n]);
                    goto!(n, SkipBlock(left - n))
                } else if b == 0 {
                    self.ext.is_block_end = true;
                    if self.ext.id.into_known() == Some(Extension::Application) {
                        goto!(ApplicationExtension, emit Decoded::BlockFinished(self.ext.id, &self.ext.data))
                    } else {
                        goto!(BlockEnd(b), emit Decoded::BlockFinished(self.ext.id, &self.ext.data))
                    }
                } else {
                    self.ext.is_block_end = false;
                    goto!(SkipBlock(b as usize), emit Decoded::SubBlockFinished(self.ext.id, &self.ext.data))
                }
            }
            ApplicationExtension => {
                // the parser removes sub-block lenghts, so app name and data are concatenated
                if self.ext.data.len() >= 15 && &self.ext.data[1..13] == b"NETSCAPE2.0\x01" {
                    let repeat = &self.ext.data[13..15];
                    let repeat = u16::from(repeat[0]) | u16::from(repeat[1]) << 8;
                    goto!(0, BlockEnd(0), emit Decoded::Repetitions(if repeat == 0 { Repeat::Infinite } else { Repeat::Finite(repeat) }))
                } else {
                    goto!(0, BlockEnd(0))
                }
            }
            LocalPalette(left) => {
                let n = cmp::min(left, buf.len());
                if left > 0 {
                    let src = &buf[..n];
                    if let Some(pal) = self.current_frame_mut().palette.as_mut() {
                        // capacity has already been reserved in ImageFlags
                        if pal.capacity() - pal.len() >= src.len() {
                            pal.extend_from_slice(src);
                        }
                    }
                    goto!(n, LocalPalette(left - n))
                } else {
                    goto!(LzwInit(b))
                }
            }
            LzwInit(min_code_size) => {
                if !self.skip_frame_decoding {
                    // Reset validates the min code size
                    self.lzw_reader.reset(min_code_size)?;
                    goto!(DecodeSubBlock(b as usize), emit Decoded::FrameMetadata(self.current_frame_mut(), FrameDataType::Pixels))
                } else {
                    goto!(CopySubBlock(b as usize), emit Decoded::FrameMetadata(self.current_frame_mut(), FrameDataType::Lzw { min_code_size }))
                }
            }
            CopySubBlock(left) => {
                debug_assert!(self.skip_frame_decoding);
                if left > 0 {
                    let n = cmp::min(left, buf.len());
                    let (consumed, copied) = match write_into {
                        OutputBuffer::Slice(slice) => {
                            let len = cmp::min(n, slice.len());
                            slice[..len].copy_from_slice(&buf[..len]);
                            (len, len)
                        },
                        OutputBuffer::Vec(vec) => {
                            vec.try_reserve(n).map_err(|_| io::ErrorKind::OutOfMemory)?;
                            vec.extend_from_slice(&buf[..n]);
                            (n, n)
                        },
                        // It's valid that bytes are discarded. For example,
                        // when using next_frame_info() with skip_frame_decoding to only get metadata.
                        OutputBuffer::None => (n, 0),
                    };
                    goto!(consumed, CopySubBlock(left - consumed), emit Decoded::LzwDataCopied(copied))
                } else if b != 0 {
                    goto!(CopySubBlock(b as usize))
                } else {
                    goto!(0, FrameDecoded)
                }
            }
            DecodeSubBlock(left) => {
                debug_assert!(!self.skip_frame_decoding);
                if left > 0 {
                    let n = cmp::min(left, buf.len());
                    if self.lzw_reader.has_ended() || matches!(write_into, OutputBuffer::None) {
                        return goto!(n, DecodeSubBlock(0), emit Decoded::BytesDecoded(0));
                    }

                    let (mut consumed, bytes_len) = self.lzw_reader.decode_bytes(&buf[..n], write_into)?;

                    // skip if can't make progress (decode would fail if check_for_end_code was set)
                    if consumed == 0 && bytes_len == 0 {
                        consumed = n;
                    }

                    goto!(consumed, DecodeSubBlock(left - consumed), emit Decoded::BytesDecoded(bytes_len))
                }  else if b != 0 { // decode next sub-block
                    goto!(DecodeSubBlock(b as usize))
                } else {
                    let (_, bytes_len) = self.lzw_reader.decode_bytes(&[], write_into)?;

                    if bytes_len > 0 {
                        goto!(0, DecodeSubBlock(0), emit Decoded::BytesDecoded(bytes_len))
                    } else {
                        goto!(0, FrameDecoded)
                    }
                }
            }
            FrameDecoded => {
                // end of image data reached
                self.current = None;
                goto!(BlockEnd(b), emit Decoded::DataEnd)
            }
            Trailer => Ok((0, Decoded::Trailer)),
        }
    }

    fn read_control_extension(&mut self, b: u8) -> Result<State, DecodingError> {
        self.add_frame();
        self.ext.data.push(b);
        if b != 4 {
            return Err(DecodingError::format("control extension has wrong length"));
        }
        Ok(Byte(ByteValue::ControlFlags))
    }

    fn add_frame(&mut self) {
        if self.current.is_none() {
            self.current = Some(Frame::default());
        }
    }
}

#[test]
fn error_cast() {
    let _ : Box<dyn error::Error> = DecodingError::format("testing").into();
}
