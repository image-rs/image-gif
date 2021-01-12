use std::cmp;
use std::error;
use std::fmt;
use std::io;
use std::mem;
use std::default::Default;

use crate::common::{AnyExtension, Block, DisposalMethod, Extension, Frame};
use crate::reader::DecodeOptions;

use weezl::{BitOrder, decode::Decoder as LzwDecoder, LzwStatus};

/// GIF palettes are RGB
pub const PLTE_CHANNELS: usize = 3;

/// An error returned in the case of the image not being formatted properly.
#[derive(Debug)]
pub struct DecodingFormatError {
    underlying: Box<dyn error::Error + Send + Sync + 'static>
}

impl fmt::Display for DecodingFormatError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&*self.underlying, fmt)
    }
}

impl error::Error for DecodingFormatError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        Some(&*self.underlying as _)
    }
}

impl DecodingFormatError {
    fn new(
        err: impl Into<Box<dyn error::Error + Send + Sync>>,
    ) -> Self {
        DecodingFormatError {
            underlying: err.into(),
        }
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
    #[inline]
    pub(crate) fn format(
        err: impl Into<Box<dyn error::Error + Send + Sync>>,
    ) -> Self {
        DecodingError::Format(DecodingFormatError::new(err))
    }
}

impl fmt::Display for DecodingError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            DecodingError::Format(ref d) => d.fmt(fmt),
            DecodingError::Io(ref err) => err.fmt(fmt),
        }
    }
}

impl error::Error for DecodingError {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        match *self {
            DecodingError::Format(ref err) => Some(err),
            DecodingError::Io(ref err) => Some(err),
        }
    }
}

impl From<io::Error> for DecodingError {
    fn from(err: io::Error) -> Self {
        DecodingError::Io(err)
    }
}

impl From<DecodingFormatError> for DecodingError {
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
    Skip
}

pub(crate) enum DecodedKind {
    /// Decoded nothing.
    Nothing,
    /// Global palette.
    GlobalPalette,
    /// Index of the background color in the global palette.
    BackgroundColor(u8),
    /// Decoded the image trailer.
    Trailer,
    /// The start of a block.
    BlockStart(Block),
    /// Decoded a sub-block. More sub-block are available.
    ///
    /// Indicates the label of the extension which might be unknown. A label of `0` is used when
    /// the sub block does not belong to an extension.
    SubBlockFinished(AnyExtension),
    /// Decoded the last (or only) sub-block of a block.
    ///
    /// Indicates the label of the extension which might be unknown. A label of `0` is used when
    /// the sub block does not belong to an extension.
    BlockFinished(AnyExtension),
    /// Decoded all information of the next frame.
    ///
    /// The returned frame does **not** contain any owned image data.
    Frame,
    /// Decoded some data of the current frame.
    Data(usize),
    /// No more data available the current frame.
    DataEnd,
}

/// Indicates whether a certain object has been decoded
#[derive(Debug)]
pub enum Decoded<'a> {
    /// Decoded nothing.
    Nothing,
    /// Global palette.
    GlobalPalette(Vec<u8>),
    /// Index of the background color in the global palette.
    BackgroundColor(u8),
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
    /// Decoded all information of the next frame.
    ///
    /// The returned frame does **not** contain any owned image data.
    Frame(&'a Frame<'static>),
    /// Decoded some data of the current frame.
    Data(&'a [u8]),
    /// No more data available the current frame.
    DataEnd,
}

/// Internal state of the GIF decoder
#[derive(Debug)]
enum State {
    Magic(usize, [u8; 6]),
    U16Byte1(U16Value, u8),
    U16(U16Value),
    Byte(ByteValue),
    GlobalPalette(usize),
    BlockStart(Option<Block>),
    /// Block end, with remaining expected data. NonZero for invalid EOF.
    BlockEnd(u8),
    ExtensionBlock(AnyExtension),
    SkipBlock(usize),
    LocalPalette(usize),
    LzwInit(u8),
    DecodeSubBlock(usize),
    FrameDecoded,
    Trailer
}
use self::State::*;

/// U16 values that may occur in a GIF image
#[derive(Debug)]
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
#[derive(Debug)]
enum ByteValue {
    GlobalFlags,
    Background { table_size: usize },
    AspectRatio { table_size: usize },
    ControlFlags,
    ImageFlags,
    TransparentIdx,
    CodeSize,
}

/// GIF decoder which supports streaming
pub struct StreamingDecoder {
    state: Option<State>,
    lzw_reader: Option<LzwDecoder>,
    decode_buffer: Vec<u8>,
    skip_extensions: bool,
    check_frame_consistency: bool,
    check_for_end_code: bool,
    version: &'static str,
    width: u16,
    height: u16,
    global_color_table: Vec<u8>,
    background_color: [u8; 4],
    /// ext buffer
    ext: (AnyExtension, Vec<u8>, bool),
    /// Frame data
    current: Option<Frame<'static>>,
}

impl StreamingDecoder {
    /// Creates a new streaming decoder
    pub fn new() -> StreamingDecoder {
        let options = DecodeOptions::new();
        Self::with_options(&options)
    }

    pub(crate) fn with_options(options: &DecodeOptions) -> Self {
        StreamingDecoder {
            state: Some(Magic(0, [0; 6])),
            lzw_reader: None,
            decode_buffer: vec![],
            skip_extensions: true,
            check_frame_consistency: options.check_frame_consistency,
            check_for_end_code: options.check_for_end_code,
            version: "",
            width: 0,
            height: 0,
            global_color_table: Vec::new(),
            background_color: [0, 0, 0, 0xFF],
            ext: (AnyExtension(0), Vec::with_capacity(256), true), // 0xFF + 1 byte length
            current: None
        }
    }
    
    /// Updates the internal state of the decoder. 
    ///
    /// Returns the number of bytes consumed from the input buffer 
    /// and the last decoding result.
    pub fn update<'a>(&'a mut self, buf: &[u8])
        -> Result<(usize, Decoded<'a>), DecodingError>
    {
        let (len, kind) = self.update_kind(buf)?;
        let decoded = self.decoded(kind);
        Ok((len, decoded))
    }

    pub(crate) fn update_kind(&mut self, mut buf: &[u8])
        -> Result<(usize, DecodedKind), DecodingError>
    {
        // NOTE: Do not change the function signature without double-checking the
        //       unsafe block!
        let len = buf.len();
        while buf.len() > 0 && self.state.is_some() {
            match self.next_state(buf) {
                Ok((bytes, DecodedKind::Nothing)) => {
                    buf = &buf[bytes..]
                }
                Ok((bytes, DecodedKind::Trailer)) => {
                    buf = &buf[bytes..];
                    break
                }
                Ok((bytes, result)) => {
                    buf = &buf[bytes..];
                    return Ok((len-buf.len(), result));
                }
                Err(err) => return Err(err)
            }
        }
        Ok((len-buf.len(), DecodedKind::Nothing))
        
    }
    
    /// Returns the data of the last extension that has been decoded.
    pub fn last_ext(&self) -> (AnyExtension, &[u8], bool) {
        (self.ext.0, &*self.ext.1, self.ext.2)
    }
    
    #[inline(always)]
    /// Current frame info as a mutable ref.
    pub fn current_frame_mut<'a>(&'a mut self) -> &'a mut Frame<'static> {
        self.current.as_mut().unwrap()
    }
    
    #[inline(always)]
    /// Current frame info as a ref.
    pub fn current_frame<'a>(&'a self) -> &'a Frame<'static> {
        self.current.as_ref().unwrap()
    }

    /// Width of the image
    pub fn width(&self) -> u16 {
        self.width
    }

    /// Height of the image
    pub fn height(&self) -> u16 {
        self.height
    }

    /// Configure whether extensions are saved or skipped.
    pub fn set_extensions(&mut self, extensions: Extensions) {
        self.skip_extensions = match extensions {
            Extensions::Skip => true,
            Extensions::Save => false,
        }
    }

    fn next_state<'a>(&'a mut self, buf: &[u8]) -> Result<(usize, DecodedKind), DecodingError> {
        macro_rules! goto (
            ($n:expr, $state:expr) => ({
                self.state = Some($state); 
                Ok(($n, DecodedKind::Nothing))
            });
            ($state:expr) => ({
                self.state = Some($state); 
                Ok((1, DecodedKind::Nothing))
            });
            ($n:expr, $state:expr, emit $res:expr) => ({
                self.state = Some($state); 
                Ok(($n, $res))
            });
            ($state:expr, emit $res:expr) => ({
                self.state = Some($state); 
                Ok((1, $res))
            })
        );
        
        let b = buf[0];
        
        // Driver should ensure that state is never None
        let state = self.state.take().unwrap();
        //println!("{:?}", state);
        
        match state {
            Magic(i, mut version) => if i < 6 {
                version[i] = b;
                goto!(Magic(i+1, version))
            } else if &version[..3] == b"GIF" {
                self.version = match &version[3..] {
                    b"87a" => "87a",
                    b"89a" => "89a",
                    _ => return Err(DecodingError::format("unsupported GIF version"))
                };
                goto!(U16Byte1(U16Value::ScreenWidth, b))
            } else {
                Err(DecodingError::format("malformed GIF header"))
            },
            U16(next) => goto!(U16Byte1(next, b)),
            U16Byte1(next, value) => {
                use self::U16Value::*;
                let value = ((b as u16) << 8) | value as u16;
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
                        self.ext.1.push(value as u8);
                        self.ext.1.push(b);
                        self.current_frame_mut().delay = delay;
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
                        let global_table = b & 0x80 != 0;
                        let entries = if global_table {
                            let entries = PLTE_CHANNELS*(1 << ((b & 0b111) + 1) as usize);
                            self.global_color_table.reserve_exact(entries);
                            entries
                        } else {
                            0usize
                        };
                        goto!(Byte(Background { table_size: entries }))
                    },
                    Background { table_size } => {
                        goto!(
                            Byte(AspectRatio { table_size: table_size }),
                            emit DecodedKind::BackgroundColor(b)
                        )
                    },
                    AspectRatio { table_size } => {
                        goto!(GlobalPalette(table_size))
                    },
                    ControlFlags => {
                        self.ext.1.push(b);
                        let control_flags = b;
                        if control_flags & 1 != 0 {
                            // Set to Some(...), gets overwritten later
                            self.current_frame_mut().transparent = Some(0)
                        }
                        self.current_frame_mut().needs_user_input =
                            control_flags & 0b10 != 0;
                        self.current_frame_mut().dispose = match DisposalMethod::from_u8(
                            (control_flags & 0b11100) >> 2
                        ) {
                            Some(method) => method,
                            None => DisposalMethod::Any
                        };
                        goto!(U16(U16Value::Delay))
                    }
                    TransparentIdx => {
                        self.ext.1.push(b);
                        if let Some(ref mut idx) = self.current_frame_mut().transparent {
                             *idx = b
                        }
                        goto!(SkipBlock(0))
                        //goto!(AwaitBlockEnd)
                    }
                    ImageFlags => {
                        let local_table = (b & 0b1000_0000) != 0;
                        let interlaced   = (b & 0b0100_0000) != 0;
                        let table_size  =  b & 0b0000_0111;

                        self.current_frame_mut().interlaced = interlaced;

                        if self.check_frame_consistency {
                            // Consistency checks.
                            let (width, height) = (self.width, self.height);
                            let frame = self.current_frame_mut();
                            if width.checked_sub(frame.width) < Some(frame.left)
                                || height.checked_sub(frame.height) < Some(frame.top)
                            {
                                return Err(DecodingError::format("frame descriptor is out-of-bounds"))
                            }
                        }

                        if local_table {
                            let entries = PLTE_CHANNELS * (1 << (table_size + 1));
                            
                            self.current_frame_mut().palette =
                                Some(Vec::with_capacity(entries));
                            goto!(LocalPalette(entries))
                        } else {
                            goto!(Byte(CodeSize))
                        }
                    },
                    CodeSize => goto!(LzwInit(b))
                }
            }
            GlobalPalette(left) => {
                let n = cmp::min(left, buf.len());
                if left > 0 {
                    self.global_color_table.extend(buf[..n].iter().cloned());
                    goto!(n, GlobalPalette(left - n))
                } else {
                    let idx = self.background_color[0];
                    match self.global_color_table.chunks(PLTE_CHANNELS).nth(idx as usize) {
                        Some(chunk) => for i in 0..PLTE_CHANNELS {
                            self.background_color[i] = chunk[i]
                        },
                        None => self.background_color[0] = 0
                    }
                    goto!(BlockStart(Block::from_u8(b)), emit DecodedKind::GlobalPalette)
                }
            }
            BlockStart(type_) => {
                match type_ {
                    Some(Block::Image) => {
                        self.add_frame();
                        goto!(U16Byte1(U16Value::ImageLeft, b), emit DecodedKind::BlockStart(Block::Image))
                    }
                    Some(Block::Extension) => {
                        goto!(ExtensionBlock(AnyExtension(b)), emit DecodedKind::BlockStart(Block::Extension))
                    }
                    Some(Block::Trailer) => {
                        goto!(0, State::Trailer, emit DecodedKind::BlockStart(Block::Trailer))
                    }
                    // TODO: permit option to enable handling/ignoring of unknown chunks?
                    None => {
                        return Err(DecodingError::format(
                        "unknown block type encountered"
                    ))}
                }
            }
            BlockEnd(terminator) => {
                if terminator == 0 {
                    if b == Block::Trailer as u8 {
                        goto!(0, BlockStart(Some(Block::Trailer)))
                    } else {
                        goto!(BlockStart(Block::from_u8(b)))
                    }
                } else {
                    return Err(DecodingError::format(
                        "expected block terminator not found"
                    ))
                }
            }
            ExtensionBlock(type_) => {
                use Extension::*;
                self.ext.0 = type_;
                self.ext.1.clear();
                self.ext.1.push(b);
                if let Some(ext) = Extension::from_u8(type_.0) {
                    match ext {
                        Control => {
                            goto!(self.read_control_extension(b)?)
                        }
                        Text | Comment | Application => {
                            goto!(SkipBlock(b as usize))
                        }
                    }
                } else {
                    return Err(DecodingError::format(
                        "unknown extention block encountered"
                    ))
                }
            }
            SkipBlock(left) => {
                let n = cmp::min(left, buf.len());
                if left > 0 {
                    self.ext.1.push(b);
                    goto!(n, SkipBlock(left - n))
                } else {
                    if b == 0 {
                        self.ext.2 = true;
                        goto!(BlockEnd(b), emit DecodedKind::BlockFinished(self.ext.0))
                    } else {
                        self.ext.2 = false;
                        goto!(SkipBlock(b as usize), emit DecodedKind::SubBlockFinished(self.ext.0))
                    }
                    
                }
            }
            LocalPalette(left) => {
                let n = cmp::min(left, buf.len());
                if left > 0 {
                    
                    self.current_frame_mut().palette
                        .as_mut().unwrap().extend(buf[..n].iter().cloned());
                    goto!(n, LocalPalette(left - n))
                } else {
                    goto!(LzwInit(b))
                }
            }
            LzwInit(code_size) => {
                // LZW spec: max 12 bits per code
                if code_size > 11 {
                    return Err(DecodingError::format(
                        "invalid minimal code size"
                    ))
                }
                // dbg!(code_size);
                self.lzw_reader = Some(LzwDecoder::new(BitOrder::Lsb, code_size));
                goto!(DecodeSubBlock(b as usize), emit DecodedKind::Frame)
            }
            DecodeSubBlock(left) => {
                if left > 0 {
                    let n = cmp::min(left, buf.len());
                    let max_bytes = self.current_frame().required_bytes();
                    let decoder = self.lzw_reader.as_mut().unwrap();
                    if decoder.has_ended() {
                        return goto!(left, DecodeSubBlock(0), emit DecodedKind::Data(0));
                    }
                    if self.decode_buffer.is_empty() {
                        let size = (1 << 14).min(max_bytes);
                        self.decode_buffer = vec![0; size];
                    }
                    let decoded = decoder.decode_bytes(&buf[..n], self.decode_buffer.as_mut_slice());
                    if let Err(err) = decoded.status {
                        return Err(io::Error::new(io::ErrorKind::InvalidData, &*format!("{:?}", err)).into());
                    }
                    let consumed = decoded.consumed_in;
                    goto!(consumed, DecodeSubBlock(left - consumed), emit DecodedKind::Data(decoded.consumed_out))
                }  else if b != 0 { // decode next sub-block
                    goto!(DecodeSubBlock(b as usize))
                } else {
                    let max_bytes = self.current_frame().required_bytes();
                    // The end of the lzw stream is only reached if left == 0 and an additional call
                    // to `decode_bytes` results in an empty slice.
                    let decoder = self.lzw_reader.as_mut().unwrap();
                    if self.decode_buffer.is_empty() {
                        let size = (1 << 14).min(max_bytes);
                        self.decode_buffer = vec![0; size];
                    }
                    let decoded = decoder.decode_bytes(&[], self.decode_buffer.as_mut_slice());
                    match decoded.status {
                        Ok(LzwStatus::Done) | Ok(LzwStatus::Ok) => {},
                        Ok(LzwStatus::NoProgress) => {
                            if self.check_for_end_code {
                                return Err(io::Error::new(io::ErrorKind::InvalidData, "No end code in lzw stream").into());
                            } else {
                                self.current = None;
                                return goto!(0, FrameDecoded, emit DecodedKind::DataEnd);
                            }
                        },
                        Err(err) => {
                            return Err(io::Error::new(io::ErrorKind::InvalidData, &*format!("{:?}", err)).into());
                        }
                    }

                    if decoded.consumed_out > 0 {
                        goto!(0, DecodeSubBlock(0), emit DecodedKind::Data(decoded.consumed_out))
                    } else {
                        // end of image data reached
                        self.current = None;
                        goto!(0, FrameDecoded, emit DecodedKind::DataEnd)
                    }
                }
            }
            FrameDecoded => {
                goto!(BlockEnd(b))
            }
            Trailer => {
                self.state = None;
                Ok((1, DecodedKind::Trailer))
            }
        }
    }

    /// Translate the descriptor of the decoding result to the data.
    pub(crate) fn decoded(&mut self, kind: DecodedKind) -> Decoded<'_> {
        match kind {
            DecodedKind::Nothing => Decoded::Nothing,
            DecodedKind::GlobalPalette => Decoded::GlobalPalette({
                mem::replace(&mut self.global_color_table, Vec::new())
            }),
            DecodedKind::BackgroundColor(c) => Decoded::BackgroundColor(c),
            DecodedKind::Trailer => Decoded::Trailer,
            DecodedKind::BlockStart(block) => Decoded::BlockStart(block),
            DecodedKind::SubBlockFinished(any) => Decoded::SubBlockFinished(any, &self.ext.1),
            DecodedKind::BlockFinished(any) => Decoded::BlockFinished(any, &self.ext.1),
            DecodedKind::Frame => Decoded::Frame(self.current_frame()),
            DecodedKind::Data(len) => Decoded::Data(&self.decode_buffer[..len]),
            DecodedKind::DataEnd => Decoded::DataEnd,
        }
    }
    
    fn read_control_extension(&mut self, b: u8) -> Result<State, DecodingError> {
        self.add_frame();
        self.ext.1.push(b);
        if b != 4 {
            return Err(DecodingError::format(
                "control extension has wrong length"
            ))
        }
        Ok(Byte(ByteValue::ControlFlags))
    }
    
    fn add_frame(&mut self) {
        if self.current.is_none() {
            self.current = Some(Frame::default())
        }
    }
}

#[test]
fn error_cast() {
    let _ : Box<dyn error::Error> = DecodingError::Format(DecodingFormatError::new("testing")).into();
}
