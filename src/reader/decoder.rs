use std::borrow::Cow;
use std::cmp;
use std::default::Default;
use std::error;
use std::fmt;
use std::io;
use std::mem;
use std::num::NonZeroUsize;

use crate::common::{AnyExtension, Block, DisposalMethod, Extension, Frame};
use crate::reader::DecodeOptions;
use crate::MemoryLimit;
use crate::Repeat;

use weezl::{decode::Decoder as LzwDecoder, BitOrder, LzwError, LzwStatus};

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
        Self::Format(DecodingFormatError {
            underlying: err.into(),
        })
    }
}

impl fmt::Display for DecodingError {
    #[cold]
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Format(ref d) => d.fmt(fmt),
            Self::Io(ref err) => err.fmt(fmt),
        }
    }
}

impl error::Error for DecodingError {
    #[cold]
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            Self::Format(ref err) => Some(err),
            Self::Io(ref err) => Some(err),
        }
    }
}

impl From<io::Error> for DecodingError {
    #[inline]
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl From<io::ErrorKind> for DecodingError {
    #[cold]
    fn from(err: io::ErrorKind) -> Self {
        Self::Io(io::Error::from(err))
    }
}

impl From<DecodingFormatError> for DecodingError {
    #[inline]
    fn from(err: DecodingFormatError) -> Self {
        Self::Format(err)
    }
}

/// Varies depending on `skip_frame_decoding`
#[derive(Debug, Copy, Clone)]
pub enum FrameDataType {
    /// `Frame.buffer` will be regular pixel data
    Pixels,
    /// Raw LZW data
    Lzw {
        /// Needed for decoding
        min_code_size: u8,
    },
}

/// Indicates whether a certain object has been decoded
#[derive(Debug)]
#[non_exhaustive]
pub enum Decoded {
    /// Decoded nothing.
    Nothing,
    /// Global palette.
    GlobalPalette(Box<[u8]>),
    /// Index of the background color in the global palette.
    BackgroundColor(u8),
    /// Loop count is known
    Repetitions(Repeat),
    /// Palette and optional `Application` extension have been parsed,
    /// reached frame data.
    HeaderEnd,
    /// The start of a block.
    /// `BlockStart(Block::Trailer)` is the very last decode event
    BlockStart(Block),
    /// Decoded a sub-block. More sub-block are available.
    ///
    /// Indicates the label of the extension which might be unknown. A label of `0` is used when
    /// the sub block does not belong to an extension.
    ///
    /// Call `last_ext()` to get the data
    SubBlockFinished(AnyExtension),
    /// Decoded the last (or only) sub-block of a block.
    ///
    /// Indicates the label of the extension which might be unknown. A label of `0` is used when
    /// the sub block does not belong to an extension.
    ///
    /// Call `last_ext()` to get the data
    BlockFinished(AnyExtension),
    /// Decoded all information of the next frame, except the image data.
    ///
    /// The returned frame does **not** contain any owned image data.
    ///
    /// Call `current_frame_mut()` to access the frame info.
    FrameMetadata(FrameDataType),
    /// Decoded some data of the current frame. Size is in bytes, always > 0
    BytesDecoded(NonZeroUsize),
    /// Copied (or consumed and discarded) compressed data of the current frame. In bytes.
    LzwDataCopied(usize),
    /// No more data available the current frame.
    DataEnd,
}

/// Internal state of the GIF decoder
#[derive(Debug, Copy, Clone)]
enum State {
    Magic,
    ScreenDescriptor,
    ImageBlockStart,
    GlobalPalette(usize),
    BlockStart(u8),
    BlockEnd,
    ExtensionBlockStart,
    /// Collects data in ext.data
    ExtensionDataBlock(usize),
    ApplicationExtension,
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

use super::converter::PixelConverter;

/// Decoder for `Frame::make_lzw_pre_encoded`
pub struct FrameDecoder {
    lzw_reader: LzwReader,
    pixel_converter: PixelConverter,
}

impl FrameDecoder {
    /// See also `set_global_palette`
    #[inline]
    #[must_use]
    pub fn new(options: DecodeOptions) -> Self {
        Self {
            lzw_reader: LzwReader::new(options.check_for_end_code),
            pixel_converter: PixelConverter::new(options.color_output, options.memory_limit),
        }
    }

    /// Palette used for RGBA conversion
    #[inline]
    pub fn set_global_palette(&mut self, palette: Vec<u8>) {
        self.pixel_converter.set_global_palette(palette);
    }

    /// Converts the frame in-place, replacing its LZW buffer with pixels.
    ///
    /// If you get an error about invalid min code size, the buffer was probably pixels, not compressed data.
    #[inline]
    pub fn decode_lzw_encoded_frame(&mut self, frame: &mut Frame<'_>) -> Result<(), DecodingError> {
        let pixel_bytes = self.pixel_converter.check_buffer_size(frame)?;
        let mut vec = vec![0; pixel_bytes];
        self.decode_lzw_encoded_frame_into_buffer(frame, &mut vec)?;
        frame.buffer = Cow::Owned(vec);
        frame.interlaced = false;
        Ok(())
    }

    /// Converts into the given buffer. It must be [`buffer_size()`] bytes large.
    ///
    /// Pixels are always deinterlaced, so update `frame.interlaced` afterwards if you're putting the buffer back into the frame.
    pub fn decode_lzw_encoded_frame_into_buffer(
        &mut self,
        frame: &Frame<'_>,
        buf: &mut [u8],
    ) -> Result<(), DecodingError> {
        let (&min_code_size, mut data) = frame.buffer.split_first().unwrap_or((&2, &[]));
        self.lzw_reader.reset(min_code_size)?;
        let lzw_reader = &mut self.lzw_reader;
        self.pixel_converter
            .read_into_buffer(frame, buf, &mut move |out| loop {
                let (bytes_read, bytes_written, status) = lzw_reader.decode_bytes(data, out)?;
                data = data.get(bytes_read..).unwrap_or_default();
                if bytes_written > 0 || matches!(status, LzwStatus::NoProgress) {
                    return Ok(bytes_written);
                }
            })?;
        Ok(())
    }

    /// Number of bytes required for `decode_lzw_encoded_frame_into_buffer`
    #[inline]
    #[must_use]
    pub fn buffer_size(&self, frame: &Frame<'_>) -> usize {
        self.pixel_converter.buffer_size(frame).unwrap()
    }
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

    pub fn check_code_size(min_code_size: u8) -> Result<(), DecodingError> {
        // LZW spec: max 12 bits per code. This check helps catch confusion
        // between LZW-compressed buffers and raw pixel data
        if min_code_size > 11 || min_code_size < 1 {
            return Err(DecodingError::format("invalid minimal code size"));
        }
        Ok(())
    }

    pub fn reset(&mut self, min_code_size: u8) -> Result<(), DecodingError> {
        Self::check_code_size(min_code_size)?;

        // The decoder can be reused if the code size stayed the same
        if self.min_code_size != min_code_size || self.decoder.is_none() {
            self.min_code_size = min_code_size;
            self.decoder = Some(LzwDecoder::new(BitOrder::Lsb, min_code_size));
        } else {
            self.decoder.as_mut().ok_or_else(|| DecodingError::format("bad state"))?.reset();
        }

        Ok(())
    }

    pub fn has_ended(&self) -> bool {
        self.decoder.as_ref().map_or(true, |e| e.has_ended())
    }

    pub fn decode_bytes(
        &mut self,
        lzw_data: &[u8],
        decode_buffer: &mut OutputBuffer<'_>,
    ) -> io::Result<(usize, usize, LzwStatus)> {
        let decoder = self.decoder.as_mut().ok_or(io::ErrorKind::Unsupported)?;

        let decode_buffer = match decode_buffer {
            OutputBuffer::Slice(buf) => &mut **buf,
            OutputBuffer::None => &mut [],
            OutputBuffer::Vec(_) => return Err(io::Error::from(io::ErrorKind::Unsupported)),
        };

        let decoded = decoder.decode_bytes(lzw_data, decode_buffer);

        let status = match decoded.status {
            Ok(ok @ LzwStatus::Done | ok @ LzwStatus::Ok) => ok,
            Ok(ok @ LzwStatus::NoProgress) => {
                if self.check_for_end_code {
                    return Err(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "no end code in lzw stream",
                    ));
                }

                ok
            }
            Err(err @ LzwError::InvalidCode) => {
                return Err(io::Error::new(io::ErrorKind::InvalidData, err));
            }
        };

        Ok((decoded.consumed_in, decoded.consumed_out, status))
    }
}

/// GIF decoder which emits [low-level events](Decoded) for items in the GIF file
///
/// To just get GIF frames, use [`crate::Decoder`] instead.
pub struct StreamingDecoder {
    state: State,
    /// Input bytes are collected here if `update` got `buf` smaller than the minimum required
    internal_buffer: [u8; 9],
    unused_internal_buffer_len: u8,
    lzw_reader: LzwReader,
    skip_frame_decoding: bool,
    check_frame_consistency: bool,
    allow_unknown_blocks: bool,
    memory_limit: MemoryLimit,
    version: Version,
    width: u16,
    height: u16,
    global_color_table: Vec<u8>,
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

/// Destination to write to for `StreamingDecoder::update`
pub enum OutputBuffer<'a> {
    /// Overwrite bytes
    Slice(&'a mut [u8]),
    /// Append LZW bytes
    Vec(&'a mut Vec<u8>),
    /// Discard bytes
    None,
}

impl OutputBuffer<'_> {
    fn append(&mut self, buf: &[u8], memory_limit: &MemoryLimit) -> Result<(usize, usize), DecodingError> {
        let (consumed, copied) = match self {
            OutputBuffer::Slice(slice) => {
                let len = cmp::min(buf.len(), slice.len());
                slice[..len].copy_from_slice(&buf[..len]);
                (len, len)
            },
            OutputBuffer::Vec(vec) => {
                let vec: &mut Vec<u8> = vec;
                let len = buf.len();
                memory_limit.check_size(vec.len() + len)?;
                vec.try_reserve(len).map_err(|_| io::ErrorKind::OutOfMemory)?;
                if vec.capacity() - vec.len() >= len {
                    vec.extend_from_slice(buf);
                }
                (len, len)
            },
            // It's valid that bytes are discarded. For example,
            // when using next_frame_info() with skip_frame_decoding to only get metadata.
            OutputBuffer::None => (buf.len(), 0),
        };
        Ok((consumed, copied))
    }
}

impl StreamingDecoder {
    /// Creates a new streaming decoder
    #[must_use]
    pub fn new() -> Self {
        let options = DecodeOptions::new();
        Self::with_options(&options)
    }

    pub(crate) fn with_options(options: &DecodeOptions) -> Self {
        Self {
            internal_buffer: [0; 9],
            unused_internal_buffer_len: 0,
            state: Magic,
            lzw_reader: LzwReader::new(options.check_for_end_code),
            skip_frame_decoding: options.skip_frame_decoding,
            check_frame_consistency: options.check_frame_consistency,
            allow_unknown_blocks: options.allow_unknown_blocks,
            memory_limit: options.memory_limit.clone(),
            version: Version::V87a,
            width: 0,
            height: 0,
            global_color_table: Vec::new(),
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
    pub fn update(
        &mut self,
        mut buf: &[u8],
        write_into: &mut OutputBuffer<'_>,
    ) -> Result<(usize, Decoded), DecodingError> {
        let len = buf.len();
        while !buf.is_empty() {
            let (bytes, decoded) = self.next_state(buf, write_into)?;
            buf = buf.get(bytes..).unwrap_or_default();
            match decoded {
                Decoded::Nothing => {},
                result => {
                    return Ok((len-buf.len(), result));
                },
            };
        }
        Ok((len - buf.len(), Decoded::Nothing))
    }

    /// Returns the data of the last extension that has been decoded.
    #[must_use]
    pub fn last_ext(&self) -> (AnyExtension, &[u8], bool) {
        (self.ext.id, &self.ext.data, self.ext.is_block_end)
    }

    /// Current frame info as a mutable ref.
    #[must_use]
    #[track_caller]
    pub fn current_frame_mut(&mut self) -> &mut Frame<'static> {
        self.current.as_mut().unwrap()
    }

    /// Current frame info as a ref.
    #[track_caller]
    #[must_use]
    pub fn current_frame(&self) -> &Frame<'static> {
        self.current.as_ref().unwrap()
    }

    /// Current frame info as a mutable ref.
    #[inline(always)]
    fn try_current_frame(&mut self) -> Result<&mut Frame<'static>, DecodingError> {
        self.current.as_mut().ok_or_else(|| DecodingError::format("bad state"))
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

    #[inline]
    fn next_state(&mut self, buf: &[u8], write_into: &mut OutputBuffer<'_>) -> Result<(usize, Decoded), DecodingError> {
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

        macro_rules! ensure_min_length_buffer (
            ($required:expr) => ({
                let required: usize = $required;
                if buf.len() >= required && self.unused_internal_buffer_len == 0 {
                    (required, &buf[..required])
                } else {
                    let has = usize::from(self.unused_internal_buffer_len);
                    let mut consumed = 0;
                    if has < required {
                        let to_copy = buf.len().min(required - has);
                        let new_len = has + to_copy;
                        self.internal_buffer[has .. new_len].copy_from_slice(&buf[..to_copy]);
                        consumed += to_copy;
                        if new_len < required {
                            self.unused_internal_buffer_len = new_len as u8;
                            return Ok((consumed, Decoded::Nothing));
                        } else {
                            self.unused_internal_buffer_len = 0;
                        }
                    }
                    (consumed, &self.internal_buffer[..required])
                }
            })
        );

        let b = *buf.first().ok_or(io::ErrorKind::UnexpectedEof)?;

        match self.state {
            Magic => {
                let (consumed, version) = ensure_min_length_buffer!(6);

                self.version = match version {
                    b"GIF87a" => Version::V87a,
                    b"GIF89a" => Version::V89a,
                    _ => return Err(DecodingError::format("malformed GIF header")),
                };

                goto!(consumed, ScreenDescriptor)
            },
            ScreenDescriptor => {
                let (consumed, desc) = ensure_min_length_buffer!(7);

                self.width = u16::from_le_bytes(desc[..2].try_into().unwrap());
                self.height = u16::from_le_bytes(desc[2..4].try_into().unwrap());
                let global_flags = desc[4];
                let background_color = desc[5];

                let global_table = global_flags & 0x80 != 0;
                let table_size = if global_table {
                    let table_size = PLTE_CHANNELS * (1 << ((global_flags & 0b111) + 1) as usize);
                    self.global_color_table.try_reserve_exact(table_size).map_err(|_| io::ErrorKind::OutOfMemory)?;
                    table_size
                } else {
                    0usize
                };

                goto!(
                    consumed,
                    GlobalPalette(table_size),
                    emit Decoded::BackgroundColor(background_color)
                )
            },
            ImageBlockStart => {
                let (consumed, header) = ensure_min_length_buffer!(9);

                let frame = self.current.as_mut().ok_or_else(|| DecodingError::format("bad state"))?;
                frame.left = u16::from_le_bytes(header[..2].try_into().unwrap());
                frame.top = u16::from_le_bytes(header[2..4].try_into().unwrap());
                frame.width = u16::from_le_bytes(header[4..6].try_into().unwrap());
                frame.height = u16::from_le_bytes(header[6..8].try_into().unwrap());

                let flags = header[8];
                frame.interlaced = (flags & 0b0100_0000) != 0;

                if self.check_frame_consistency {
                    // Consistency checks.
                    if self.width.checked_sub(frame.width) < Some(frame.left)
                        || self.height.checked_sub(frame.height) < Some(frame.top)
                    {
                        return Err(DecodingError::format("frame descriptor is out-of-bounds"));
                    }
                }

                let local_table = (flags & 0b1000_0000) != 0;
                if local_table {
                    let table_size = flags & 0b0000_0111;
                    let pal_len = PLTE_CHANNELS * (1 << (table_size + 1));
                    frame.palette.get_or_insert_with(Vec::new)
                        .try_reserve_exact(pal_len).map_err(|_| io::ErrorKind::OutOfMemory)?;
                    goto!(consumed, LocalPalette(pal_len))
                } else {
                    goto!(consumed, LocalPalette(0))
                }
            },
            GlobalPalette(left) => {
                // the global_color_table is guaranteed to have the exact capacity required
                if left > 0 {
                    let n = cmp::min(left, buf.len());
                    if n <= self.global_color_table.capacity() - self.global_color_table.len() {
                        self.global_color_table.extend_from_slice(&buf[..n]);
                    }
                    goto!(n, GlobalPalette(left - n))
                } else {
                    goto!(BlockStart(b), emit Decoded::GlobalPalette(
                        mem::take(&mut self.global_color_table).into_boxed_slice()
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
                        goto!(0, ImageBlockStart, emit Decoded::BlockStart(Block::Image))
                    }
                    Some(Block::Extension) => {
                        self.ext.data.clear();
                        self.ext.id = AnyExtension(b);
                        if self.ext.id.into_known().is_none() {
                            if !self.allow_unknown_blocks {
                                return Err(DecodingError::format(
                                    "unknown extension block encountered",
                                ));
                            }
                        }
                        goto!(ExtensionBlockStart, emit Decoded::BlockStart(Block::Extension))
                    }
                    Some(Block::Trailer) => {
                        // The `Trailer` is the final state, and isn't reachable without extraneous data after the end of file
                        goto!(Trailer, emit Decoded::BlockStart(Block::Trailer))
                    }
                    None => {
                        if self.allow_unknown_blocks {
                            goto!(ExtensionDataBlock(b as usize))
                        } else {
                            Err(DecodingError::format("unknown block type encountered"))
                        }
                    }
                }
            },
            BlockEnd => {
                if b == Block::Trailer as u8 {
                    // can't consume yet, because the trailer is not a real block,
                    // and won't have futher data for BlockStart
                    goto!(0, BlockStart(b))
                } else {
                    goto!(BlockStart(b))
                }
            }
            ExtensionBlockStart => {
                self.ext.data.push(b);
                goto!(ExtensionDataBlock(b as usize))
            }
            ExtensionDataBlock(left) => {
                if left > 0 {
                    let n = cmp::min(left, buf.len());
                    self.memory_limit.check_size(self.ext.data.len() + n)?;
                    self.ext.data.try_reserve(n).map_err(|_| io::Error::from(io::ErrorKind::OutOfMemory))?;
                    self.ext.data.extend_from_slice(&buf[..n]);
                    goto!(n, ExtensionDataBlock(left - n))
                } else if b == 0 {
                    self.ext.is_block_end = true;
                    match self.ext.id.into_known() {
                        Some(Extension::Application) => {
                            goto!(0, ApplicationExtension, emit Decoded::BlockFinished(self.ext.id))
                        }
                        Some(Extension::Control) => {
                            self.read_control_extension()?;
                            goto!(BlockEnd, emit Decoded::BlockFinished(self.ext.id))
                        },
                        _ => {
                            goto!(BlockEnd, emit Decoded::BlockFinished(self.ext.id))
                        }
                    }
                } else {
                    self.ext.is_block_end = false;
                    goto!(ExtensionDataBlock(b as usize), emit Decoded::SubBlockFinished(self.ext.id))
                }
            }
            ApplicationExtension => {
                debug_assert_eq!(0, b);
                // the parser removes sub-block lenghts, so app name and data are concatenated
                if self.ext.data.len() >= 15 && &self.ext.data[1..13] == b"NETSCAPE2.0\x01" {
                    let repeat = &self.ext.data[13..15];
                    let repeat = u16::from(repeat[0]) | u16::from(repeat[1]) << 8;
                    goto!(BlockEnd, emit Decoded::Repetitions(if repeat == 0 { Repeat::Infinite } else { Repeat::Finite(repeat) }))
                } else {
                    goto!(BlockEnd)
                }
            }
            LocalPalette(left) => {
                if left > 0 {
                    let n = cmp::min(left, buf.len());
                    let src = &buf[..n];
                    if let Some(pal) = self.try_current_frame()?.palette.as_mut() {
                        // capacity has already been reserved in ImageBlockStart
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
                    goto!(DecodeSubBlock(b as usize), emit Decoded::FrameMetadata(FrameDataType::Pixels))
                } else {
                    LzwReader::check_code_size(min_code_size)?;
                    goto!(CopySubBlock(b as usize), emit Decoded::FrameMetadata(FrameDataType::Lzw { min_code_size }))
                }
            }
            CopySubBlock(left) => {
                debug_assert!(self.skip_frame_decoding);
                if left > 0 {
                    let n = cmp::min(left, buf.len());
                    let (consumed, copied) = write_into.append(&buf[..n], &self.memory_limit)?;
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
                        return goto!(n, DecodeSubBlock(left - n), emit Decoded::Nothing);
                    }

                    let (mut consumed, bytes_len, status) =
                        self.lzw_reader.decode_bytes(&buf[..n], write_into)?;

                    // skip if can't make progress (decode would fail if check_for_end_code was set)
                    if matches!(status, LzwStatus::NoProgress) {
                        consumed = n;
                    }

                    let decoded = if let Some(bytes_len) = NonZeroUsize::new(bytes_len) {
                        Decoded::BytesDecoded(bytes_len)
                    } else {
                        Decoded::Nothing
                    };
                    goto!(consumed, DecodeSubBlock(left - consumed), emit decoded)
                } else if b != 0 {
                    // decode next sub-block
                    goto!(DecodeSubBlock(b as usize))
                } else {
                    let (_, bytes_len, status) = self.lzw_reader.decode_bytes(&[], write_into)?;

                    if let Some(bytes_len) = NonZeroUsize::new(bytes_len) {
                        goto!(0, DecodeSubBlock(0), emit Decoded::BytesDecoded(bytes_len))
                    } else if matches!(status, LzwStatus::Ok) {
                        goto!(0, DecodeSubBlock(0), emit Decoded::Nothing)
                    } else if matches!(status, LzwStatus::Done) {
                        goto!(0, FrameDecoded)
                    } else {
                        goto!(0, FrameDecoded)
                    }
                }
            }
            FrameDecoded => {
                // end of image data reached
                self.current = None;
                debug_assert_eq!(0, b);
                goto!(BlockEnd, emit Decoded::DataEnd)
            }
            Trailer => goto!(0, Trailer, emit Decoded::Nothing),
        }
    }

    fn read_control_extension(&mut self) -> Result<(), DecodingError> {
        if self.ext.data.len() != 5 {
            return Err(DecodingError::format("control extension has wrong length"));
        }
        let control = &self.ext.data[1..];

        let frame = self.current.get_or_insert_with(Frame::default);
        let control_flags = control[0];
        frame.needs_user_input = control_flags & 0b10 != 0;
        frame.dispose = match DisposalMethod::from_u8((control_flags & 0b11100) >> 2) {
            Some(method) => method,
            None => DisposalMethod::Any,
        };
        frame.delay = u16::from_le_bytes(control[1..3].try_into().unwrap());
        frame.transparent = (control_flags & 1 != 0).then_some(control[3]);
        Ok(())
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
