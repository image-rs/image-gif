use std::borrow::Cow;
use std::io;
use std::mem;
use std::iter;
use std::io::prelude::*;
use std::num::NonZeroU64;
use std::convert::{TryFrom, TryInto};

use crate::Repeat;
use crate::common::{Block, Frame};

mod decoder;
pub use self::decoder::{
    PLTE_CHANNELS, StreamingDecoder, Decoded, DecodingError, DecodingFormatError, Extensions,
    Version, FrameDataType, OutputBuffer
};

const N_CHANNELS: usize = 4;

/// Output mode for the image data
#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum ColorOutput {
    /// The decoder expands the image data to 32bit RGBA.
    /// This affects:
    ///
    ///  - The buffer buffer of the `Frame` returned by `Decoder::read_next_frame`.
    ///  - `Decoder::fill_buffer`, `Decoder::buffer_size` and `Decoder::line_length`.
    RGBA = 0,
    /// The decoder returns the raw indexed data.
    Indexed = 1,
}

#[derive(Clone, Debug)]
/// The maximum amount of memory the decoder is allowed to use for each frame
pub enum MemoryLimit {
    /// Enforce no memory limit.
    ///
    /// If you intend to process images from unknown origins this is a potentially dangerous
    /// constant to use, as your program could be vulnerable to decompression bombs. That is,
    /// malicious images crafted specifically to require an enormous amount of memory to process
    /// while having a disproportionately small file size.
    ///
    /// The risks for modern machines are a bit smaller as the size of each frame cannot
    /// exceed 16GiB, but this is still a significant amount of memory.
    Unlimited,
    /// Limit the amount of memory that can be used for a single frame to this many bytes.
    ///
    /// It may not be enforced precisely due to allocator overhead
    /// and the decoder potentially allocating small auxiliary buffers,
    /// but it will precisely limit the size of the output buffer for each frame.
    //
    // The `NonZero` type is used to make FFI simpler.
    // Due to the guaranteed niche optimization, `Unlimited` will be represented as `0`,
    // and the whole enum as a simple `u64`.
    Bytes(NonZeroU64),
}

impl MemoryLimit {
    fn buffer_size(&self, color: ColorOutput, width: u16, height: u16) -> Option<usize> {
        let pixels = u64::from(width) * u64::from(height);

        let bytes_per_pixel = match color {
            ColorOutput::Indexed => 1,
            ColorOutput::RGBA => 4,
        };

        // This cannot overflow because the maximum possible value is 16GiB, well within u64 range
        let total_bytes = pixels * bytes_per_pixel;

        // On 32-bit platforms the size of the output buffer may not be representable
        let usize_bytes = usize::try_from(total_bytes).ok()?;

        match self {
            MemoryLimit::Unlimited => Some(usize_bytes),
            MemoryLimit::Bytes(limit) => {
                if total_bytes > limit.get() {
                    None
                } else {
                    Some(usize_bytes)
                }
            },
        }
    }
}

/// Options for opening a GIF decoder.
#[derive(Clone, Debug)]
pub struct DecodeOptions {
    memory_limit: MemoryLimit,
    color_output: ColorOutput,
    check_frame_consistency: bool,
    skip_frame_decoding: bool,
    check_for_end_code: bool,
    allow_unknown_blocks: bool,
}

impl DecodeOptions {
    /// Creates a new decoder builder
    #[must_use]
    pub fn new() -> DecodeOptions {
        DecodeOptions {
            memory_limit: MemoryLimit::Bytes(50_000_000.try_into().unwrap()), // 50 MB
            color_output: ColorOutput::Indexed,
            check_frame_consistency: false,
            skip_frame_decoding: false,
            check_for_end_code: false,
            allow_unknown_blocks: false,
        }
    }

    /// Configure how color data is decoded.
    pub fn set_color_output(&mut self, color: ColorOutput) {
        self.color_output = color;
    }

    /// Configure a memory limit for decoding.
    pub fn set_memory_limit(&mut self, limit: MemoryLimit) {
        self.memory_limit = limit;
    }

    /// Configure if frames must be within the screen descriptor.
    ///
    /// The default is `false`.
    ///
    /// When turned on, all frame descriptors being read must fit within the screen descriptor or
    /// otherwise an error is returned and the stream left in an unspecified state.
    ///
    /// When turned off, frames may be arbitrarily larger or offset in relation to the screen. Many
    /// other decoder libraries handle this in highly divergent ways. This moves all checks to the
    /// caller, for example to emulate a specific style.
    pub fn check_frame_consistency(&mut self, check: bool) {
        self.check_frame_consistency = check;
    }

    /// Configure whether to skip decoding frames.
    ///
    /// The default is false.
    ///
    /// When turned on, LZW decoding is skipped. `Decoder::read_next_frame` will return
    /// compressed LZW bytes in frame's data.
    /// `Decoder::next_frame_info` will return the metadata of the next frame as usual.
    /// This is useful to count frames without incurring the overhead of decoding.
    pub fn skip_frame_decoding(&mut self, skip: bool) {
        self.skip_frame_decoding = skip;
    }

    /// Configure if LZW encoded blocks must end with a marker end code.
    ///
    /// The default is `false`.
    ///
    /// When turned on, all image data blocks—which are LZW encoded—must contain a special bit
    /// sequence signalling the end of the data. LZW processing terminates when this code is
    /// encountered. The specification states that it must be the last code output by the encoder
    /// for an image.
    ///
    /// When turned off then image data blocks can simply end. Note that this might silently ignore
    /// some bits of the last or second to last byte.
    pub fn check_lzw_end_code(&mut self, check: bool) {
        self.check_for_end_code = check;
    }

    /// Configure if unknown blocks are allowed to be decoded.
    ///
    /// The default is `false`.
    ///
    /// When turned on, the decoder will allow unknown blocks to be in the
    /// `BlockStart` position.
    ///
    /// When turned off, decoded block starts must mark an `Image`, `Extension`,
    /// or `Trailer` block. Otherwise, the decoded image will return an error.
    /// If an unknown block error is returned from decoding, enabling this
    /// setting may allow for a further state of decoding on the next attempt.
    pub fn allow_unknown_blocks(&mut self, check: bool) {
        self.allow_unknown_blocks = check;
    }

    /// Reads the logical screen descriptor including the global color palette
    ///
    /// Returns a `Decoder`. All decoder configuration has to be done beforehand.
    pub fn read_info<R: Read>(self, r: R) -> Result<Decoder<R>, DecodingError> {
        Decoder::with_no_init(r, StreamingDecoder::with_options(&self), self).init()
    }
}

struct ReadDecoder<R: Read> {
    reader: io::BufReader<R>,
    decoder: StreamingDecoder,
    at_eof: bool,
}

impl<R: Read> ReadDecoder<R> {
    fn decode_next(&mut self, write_into: &mut OutputBuffer<'_>) -> Result<Option<Decoded<'_>>, DecodingError> {
        while !self.at_eof {
            let (consumed, result) = {
                let buf = self.reader.fill_buf()?;
                if buf.is_empty() {
                    return Err(io::ErrorKind::UnexpectedEof.into());
                }

                // Dead code checks the lifetimes that the later mem::transmute can't.
                #[cfg(test)]
                if false {
                    return self.decoder.update(buf, write_into).map(|(_, res)| Some(res));
                }

                self.decoder.update(buf, write_into)?
            };
            self.reader.consume(consumed);
            match result {
                Decoded::Nothing => (),
                Decoded::BlockStart(Block::Trailer) => {
                    self.at_eof = true;
                },
                result => return Ok(unsafe{
                    // FIXME: #6393
                    Some(mem::transmute::<Decoded<'_>, Decoded<'_>>(result))
                }),
            }
        }
        Ok(None)
    }

    fn into_inner(self) -> io::BufReader<R> {
        self.reader
    }
}

#[allow(dead_code)]
/// GIF decoder
pub struct Decoder<R: Read> {
    decoder: ReadDecoder<R>,
    color_output: ColorOutput,
    memory_limit: MemoryLimit,
    bg_color: Option<u8>,
    repeat: Repeat,
    global_palette: Option<Vec<u8>>,
    current_frame: Frame<'static>,
    current_frame_data_type: FrameDataType,
    buffer: Vec<u8>,
}

impl<R> Decoder<R> where R: Read {
    /// Create a new decoder with default options.
    pub fn new(reader: R) -> Result<Self, DecodingError> {
        DecodeOptions::new().read_info(reader)
    }

    /// Return a builder that allows configuring limits etc.
    #[must_use]
    pub fn build() -> DecodeOptions {
        DecodeOptions::new()
    }

    fn with_no_init(reader: R, decoder: StreamingDecoder, options: DecodeOptions) -> Decoder<R> {
        Decoder {
            decoder: ReadDecoder {
                reader: io::BufReader::new(reader),
                decoder,
                at_eof: false,
            },
            bg_color: None,
            global_palette: None,
            buffer: vec![],
            repeat: Repeat::default(),
            color_output: options.color_output,
            memory_limit: options.memory_limit,
            current_frame: Frame::default(),
            current_frame_data_type: FrameDataType::Pixels,
        }
    }

    fn init(mut self) -> Result<Self, DecodingError> {
        loop {
            match self.decoder.decode_next(&mut OutputBuffer::None)? {
                Some(Decoded::BackgroundColor(bg_color)) => {
                    self.bg_color = Some(bg_color);
                }
                Some(Decoded::GlobalPalette(palette)) => {
                    self.global_palette = if !palette.is_empty() {
                        Some(palette)
                    } else {
                        None
                    };
                },
                Some(Decoded::Repetitions(repeat)) => {
                    self.repeat = repeat;
                },
                Some(Decoded::HeaderEnd | Decoded::Trailer) => {
                    break
                },
                Some(_) => {
                    // There will be extra events when parsing application extension
                    continue
                },
                None => return Err(DecodingError::format(
                    "file does not contain any image data"
                ))
            }
        }
        // If the background color is invalid, ignore it
        if let Some(ref palette) = self.global_palette {
            if self.bg_color.unwrap_or(0) as usize >= (palette.len() / PLTE_CHANNELS) {
                self.bg_color = None;
            }
        }
        Ok(self)
    }

    /// Returns the next frame info
    pub fn next_frame_info(&mut self) -> Result<Option<&Frame<'static>>, DecodingError> {
        loop {
            match self.decoder.decode_next(&mut OutputBuffer::None)? {
                Some(Decoded::FrameMetadata(frame, frame_data_type)) => {
                    self.current_frame = frame.take();
                    self.current_frame_data_type = frame_data_type;
                    if self.current_frame.palette.is_none() && self.global_palette.is_none() {
                        return Err(DecodingError::format(
                            "no color table available for current frame",
                        ));
                    }
                    break;
                }
                Some(_) => (),
                None => return Ok(None),
            }
        }
        Ok(Some(&self.current_frame))
    }

    /// Reads the next frame from the image.
    ///
    /// Do not call `Self::next_frame_info` beforehand.
    /// Deinterlaces the result.
    pub fn read_next_frame(&mut self) -> Result<Option<&Frame<'static>>, DecodingError> {
        if let Some(frame) = self.next_frame_info()? {
            let (width, height) = (frame.width, frame.height);
            let pixel_bytes = self.memory_limit
                .buffer_size(self.color_output, width, height)
                .ok_or_else(|| io::Error::new(io::ErrorKind::OutOfMemory, "image is too large"))?;

            debug_assert_eq!(
                pixel_bytes, self.buffer_size(),
                "Checked computation diverges from required buffer size"
            );
            match self.current_frame_data_type {
                FrameDataType::Pixels => {
                    let can_reuse_existing_buffer = matches!(self.current_frame.buffer, Cow::Owned(_)) &&
                        self.current_frame.buffer.to_mut().capacity() >= pixel_bytes;

                    let mut vec = if can_reuse_existing_buffer {
                        let mut vec = mem::replace(&mut self.current_frame.buffer, Cow::Borrowed(&[])).into_owned();
                        vec.resize(pixel_bytes, 0);
                        vec
                    } else {
                        // free mem of the previous buffer, if any
                        self.current_frame.buffer = Cow::Borrowed(&[]);
                        // resizing would realloc anyway, and 0-init is faster than a copy
                        vec![0; pixel_bytes]
                    };

                    self.read_into_buffer(&mut vec)?;
                    self.current_frame.buffer = Cow::Owned(vec);
                    self.current_frame.interlaced = false;
                }
                FrameDataType::Lzw { min_code_size } => {
                    let mut vec = if matches!(self.current_frame.buffer, Cow::Owned(_)) {
                        let mut vec = mem::replace(&mut self.current_frame.buffer, Cow::Borrowed(&[])).into_owned();
                        vec.clear();
                        vec
                    } else {
                        Vec::new()
                    };
                    // Guesstimate 2bpp
                    vec.try_reserve(usize::from(width) * usize::from(height) / 4)
                        .map_err(|_| io::Error::from(io::ErrorKind::OutOfMemory))?;
                    self.copy_lzw_into_buffer(min_code_size, &mut vec)?;
                    self.current_frame.buffer = Cow::Owned(vec);
                },
            }
            Ok(Some(&self.current_frame))
        } else {
            Ok(None)
        }
    }

    /// Reads the data of the current frame into a pre-allocated buffer.
    ///
    /// `Self::next_frame_info` needs to be called beforehand.
    /// The length of `buf` must be at least `Self::buffer_size`.
    /// Deinterlaces the result.
    pub fn read_into_buffer(&mut self, buf: &mut [u8]) -> Result<(), DecodingError> {
        if self.current_frame.interlaced {
            let width = self.line_length();
            let height = self.current_frame.height as usize;
            for row in (InterlaceIterator { len: height, next: 0, pass: 0 }) {
                let start = row * width;
                // Handle a too-small buffer without panicking
                let line = buf.get_mut(start .. start + width).ok_or_else(|| DecodingError::format("buffer too small"))?;
                if !self.fill_buffer(line)? {
                    return Err(DecodingError::format("image truncated"));
                }
            }
        } else {
            let buf = buf.get_mut(..self.buffer_size()).ok_or_else(|| DecodingError::format("buffer too small"))?;
            if !self.fill_buffer(buf)? {
                return Err(DecodingError::format("image truncated"));
            }
        };
        Ok(())
    }

    fn copy_lzw_into_buffer(&mut self, min_code_size: u8, buf: &mut Vec<u8>) -> Result<(), DecodingError> {
        // `write_lzw_pre_encoded_frame` smuggles `min_code_size` in the first byte.
        buf.push(min_code_size);
        loop {
            match self.decoder.decode_next(&mut OutputBuffer::Vec(buf))? {
                Some(Decoded::LzwDataCopied(_len)) => {},
                Some(Decoded::DataEnd) => return Ok(()),
                _ => return Err(DecodingError::format("unexpected data")),
            }
        }
    }

    /// Reads data of the current frame into a pre-allocated buffer until the buffer has been
    /// filled completely.
    ///
    /// The buffer length must be an even number of pixels (multiple of 4 if decoding RGBA).
    ///
    /// `Self::next_frame_info` needs to be called beforehand. Returns `true` if the supplied
    /// buffer could be filled completely. Should not be called after `false` had been returned.
    pub fn fill_buffer(&mut self, mut buf: &mut [u8]) -> Result<bool, DecodingError> {
        loop {
            let decode_into = match self.color_output {
                // When decoding indexed data, LZW can write the pixels directly
                ColorOutput::Indexed => &mut buf[..],
                // When decoding RGBA, the pixel data will be expanded by a factor of 4,
                // and it's simpler to decode indexed pixels to another buffer first
                ColorOutput::RGBA => {
                    let buffer_size = buf.len() / N_CHANNELS;
                    if buffer_size == 0 {
                        return Err(DecodingError::format("odd-sized buffer"));
                    }
                    if self.buffer.len() < buffer_size {
                        self.buffer.resize(buffer_size, 0);
                    }
                    &mut self.buffer[..buffer_size]
                }
            };
            match self.decoder.decode_next(&mut OutputBuffer::Slice(decode_into))? {
                Some(Decoded::BytesDecoded(bytes_decoded)) => {
                    match self.color_output {
                        ColorOutput::RGBA => {
                            let transparent = self.current_frame.transparent;
                            let palette: &[u8] = self.current_frame.palette.as_deref()
                                .or(self.global_palette.as_deref())
                                .unwrap_or_default(); // next_frame_info already checked it won't happen

                            let (pixels, rest) = buf.split_at_mut(bytes_decoded * N_CHANNELS);
                            buf = rest;

                            for (rgba, idx) in pixels.chunks_exact_mut(N_CHANNELS).zip(self.buffer.iter().copied().take(bytes_decoded)) {
                                let plte_offset = PLTE_CHANNELS * idx as usize;
                                if let Some(colors) = palette.get(plte_offset..plte_offset+PLTE_CHANNELS) {
                                    rgba[0] = colors[0];
                                    rgba[1] = colors[1];
                                    rgba[2] = colors[2];
                                    rgba[3] = if let Some(t) = transparent {
                                        if t == idx { 0x00 } else { 0xFF }
                                    } else {
                                        0xFF
                                    };
                                }
                            }
                        },
                        ColorOutput::Indexed => {
                            buf = &mut buf[bytes_decoded..];
                        }
                    }
                    if buf.is_empty() {
                        return Ok(true);
                    }
                }
                Some(_) => return Ok(false), // make sure that no important result is missed
                None => return Ok(false),
            }
        }
    }

    /// Output buffer size
    pub fn buffer_size(&self) -> usize {
        self.line_length() * self.current_frame.height as usize
    }

    /// Line length of the current frame
    pub fn line_length(&self) -> usize {
        use self::ColorOutput::*;
        match self.color_output {
            RGBA => self.current_frame.width as usize * N_CHANNELS,
            Indexed => self.current_frame.width as usize,
        }
    }

    /// Returns the color palette relevant for the frame that has been decoded
    pub fn palette(&self) -> Result<&[u8], DecodingError> {
        // TODO prevent planic
        Ok(match self.current_frame.palette {
            Some(ref table) => table,
            None => self.global_palette.as_ref().ok_or(DecodingError::format(
                "no color table available for current frame",
            ))?,
        })
    }

    /// The global color palette
    pub fn global_palette(&self) -> Option<&[u8]> {
        self.global_palette.as_deref()
    }

    /// Width of the image
    pub fn width(&self) -> u16 {
        self.decoder.decoder.width()
    }

    /// Height of the image
    pub fn height(&self) -> u16 {
        self.decoder.decoder.height()
    }

    /// Abort decoding and recover the `io::Read` instance
    pub fn into_inner(self) -> io::BufReader<R> {
        self.decoder.into_inner()
    }

    /// Index of the background color in the global palette
    ///
    /// In practice this is not used, and the background is
    /// always transparent
    pub fn bg_color(&self) -> Option<usize> {
        self.bg_color.map(|v| v as usize)
    }

    /// Number of loop repetitions
    pub fn repeat(&self) -> Repeat {
        self.repeat
    }
}

struct InterlaceIterator {
    len: usize,
    next: usize,
    pass: usize,
}

impl iter::Iterator for InterlaceIterator {
    type Item = usize;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if self.len == 0 {
            return None;
        }
        // although the pass never goes out of bounds thanks to len==0,
        // the optimizer doesn't see it. get()? avoids costlier panicking code.
        let mut next = self.next + *[8, 8, 4, 2].get(self.pass)?;
        while next >= self.len {
            debug_assert!(self.pass < 4);
            next = *[4, 2, 1, 0].get(self.pass)?;
            self.pass += 1;
        }
        mem::swap(&mut next, &mut self.next);
        Some(next)
    }
}

#[cfg(test)]
mod test {
    use std::fs::File;

    use super::{Decoder, InterlaceIterator};

    #[test]
    fn test_simple_indexed() {
        let mut decoder = Decoder::new(File::open("tests/samples/sample_1.gif").unwrap()).unwrap();
        let frame = decoder.read_next_frame().unwrap().unwrap();
        assert_eq!(&*frame.buffer, &[
            1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
            1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
            1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
            1, 1, 1, 0, 0, 0, 0, 2, 2, 2,
            1, 1, 1, 0, 0, 0, 0, 2, 2, 2,
            2, 2, 2, 0, 0, 0, 0, 1, 1, 1,
            2, 2, 2, 0, 0, 0, 0, 1, 1, 1,
            2, 2, 2, 2, 2, 1, 1, 1, 1, 1,
            2, 2, 2, 2, 2, 1, 1, 1, 1, 1,
            2, 2, 2, 2, 2, 1, 1, 1, 1, 1
        ][..]);
    }

    #[test]
    fn test_interlace_iterator() {
        for &(len, expect) in &[
            (0, &[][..]),
            (1, &[0][..]),
            (2, &[0, 1][..]),
            (3, &[0, 2, 1][..]),
            (4, &[0, 2, 1, 3][..]),
            (5, &[0, 4, 2, 1, 3][..]),
            (6, &[0, 4, 2, 1, 3, 5][..]),
            (7, &[0, 4, 2, 6, 1, 3, 5][..]),
            (8, &[0, 4, 2, 6, 1, 3, 5, 7][..]),
            (9, &[0, 8, 4, 2, 6, 1, 3, 5, 7][..]),
            (10, &[0, 8, 4, 2, 6, 1, 3, 5, 7, 9][..]),
            (11, &[0, 8, 4, 2, 6, 10, 1, 3, 5, 7, 9][..]),
            (12, &[0, 8, 4, 2, 6, 10, 1, 3, 5, 7, 9, 11][..]),
            (13, &[0, 8, 4, 12, 2, 6, 10, 1, 3, 5, 7, 9, 11][..]),
            (14, &[0, 8, 4, 12, 2, 6, 10, 1, 3, 5, 7, 9, 11, 13][..]),
            (15, &[0, 8, 4, 12, 2, 6, 10, 14, 1, 3, 5, 7, 9, 11, 13][..]),
            (16, &[0, 8, 4, 12, 2, 6, 10, 14, 1, 3, 5, 7, 9, 11, 13, 15][..]),
            (17, &[0, 8, 16, 4, 12, 2, 6, 10, 14, 1, 3, 5, 7, 9, 11, 13, 15][..]),
        ] {
            let iter = InterlaceIterator { len, next: 0, pass: 0 };
            let lines = iter.collect::<Vec<_>>();
            assert_eq!(lines, expect);
        }
    }
}
