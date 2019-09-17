use std::borrow::Cow;
use std::io;
use std::cmp;
use std::mem;
use std::iter;
use std::io::prelude::*;

use traits::{Parameter, SetParameter};
use common::Frame;

mod decoder;
pub use self::decoder::{
    PLTE_CHANNELS, StreamingDecoder, Decoded, DecodingError, Extensions
};

const N_CHANNELS: usize = 4;

impl<T, R> Parameter<Decoder<R>> for T
where T: Parameter<StreamingDecoder>, R: Read {
    type Result = ();
    fn set_param(self, this: &mut Decoder<R>) {
        this.decoder.set(self);
    }

}

/// Output mode for the image data
#[derive(PartialEq, Debug)]
#[repr(u8)]
pub enum ColorOutput {
    /// The decoder expands the image data to 32bit RGBA.
    /// This affects:
    ///
    ///  - The buffer buffer of the `Frame` returned by `Reader::read_next_frame`.
    ///  - `Reader::fill_buffer`, `Reader::buffer_size` and `Reader::line_length`.
    RGBA = 0,
    /// The decoder returns the raw indexed data.
    Indexed = 1,
}

impl<R: Read> Parameter<Decoder<R>> for ColorOutput {
    type Result = ();
    fn set_param(self, this: &mut Decoder<R>) {
        this.color_output = self
    }
}

#[derive(Debug)]
/// Memory limit in bytes. `MemoryLimit::Some(0)` means
/// that there is no memory limit set.
pub struct MemoryLimit(pub u32);

impl<R: Read> Parameter<Decoder<R>> for MemoryLimit {
    type Result = ();
    fn set_param(self, this: &mut Decoder<R>) {
        let MemoryLimit(limit) = self;
        this.memory_limit = limit
    }
}

/// GIF decoder
pub struct Decoder<R: Read> {
    r: R,
    decoder: StreamingDecoder,
    memory_limit: u32,
    color_output: ColorOutput,
}

impl<R: Read> Decoder<R> {
    /// Creates a new decoder builder
    pub fn new(r: R) -> Decoder<R> {
        Decoder {
            r: r,
            decoder: StreamingDecoder::new(),
            memory_limit: 50_000_000, // 50 MB
            color_output: ColorOutput::Indexed
        }
    }
    
    /// Reads the logical screen descriptor including the global color palette
    ///
    /// Returns a `Reader`. All decoder configuration has to be done beforehand.
    pub fn read_info(self) -> Result<Reader<R>, DecodingError> {
        Reader::new(self.r, self.decoder, self.color_output, self.memory_limit).init()
    }
}

struct ReadDecoder<R: Read> {
    reader: io::BufReader<R>,
    decoder: StreamingDecoder,
    at_eof: bool
}

impl<R: Read> ReadDecoder<R> {
    fn decode_next(&mut self) -> Result<Option<Decoded>, DecodingError> {
        while !self.at_eof {
            let (consumed, result) = {
                let buf = self.reader.fill_buf()?;
                if buf.len() == 0 {
                    return Err(DecodingError::Format(
                        "unexpected EOF"
                    ))
                }
                self.decoder.update(buf)?
            };
            self.reader.consume(consumed);
            match result {
                Decoded::Nothing => (),
                Decoded::BlockStart(::common::Block::Trailer) => {
                    self.at_eof = true
                },
                result => return Ok(unsafe{
                    // FIXME: #6393
                    Some(mem::transmute::<Decoded, Decoded>(result))
                }),
            }
        }
        Ok(None)
    }
}

#[allow(dead_code)]
/// GIF decoder
pub struct Reader<R: Read> {
    decoder: ReadDecoder<R>,
    color_output: ColorOutput,
    memory_limit: u32,
    bg_color: Option<u8>,
    global_palette: Option<Vec<u8>>,
    current_frame: Frame<'static>,
    buffer: Vec<u8>,
    // Offset in current frame
    offset: usize

}

impl<R> Reader<R> where R: Read {
    fn new(reader: R, decoder: StreamingDecoder,
           color_output: ColorOutput, memory_limit: u32
    ) -> Reader<R> {
        Reader {
            decoder: ReadDecoder {
                reader: io::BufReader::new(reader),
                decoder: decoder,
                at_eof: false
            },
            bg_color: None,
            global_palette: None,
            buffer: Vec::with_capacity(32),
            color_output: color_output,
            memory_limit: memory_limit,
            current_frame: Frame::default(),
            offset: 0
        }
    }
    
    fn init(mut self) -> Result<Self, DecodingError> {
        loop {
            match self.decoder.decode_next()? {
                Some(Decoded::BackgroundColor(bg_color)) => {
                    self.bg_color = Some(bg_color)
                }
                Some(Decoded::GlobalPalette(palette)) => {
                    self.global_palette = if palette.len() > 0 {
                        Some(palette)
                    } else {
                        None
                    };
                    break
                },
                Some(_) => {
                    // Unreachable since this loop exists after the global
                    // palette has been read.
                    unreachable!()
                },
                None => return Err(DecodingError::Format(
                    "File does not contain any image data"
                ))
            }
        }
        // If the background color is invalid, ignore it
        if let &Some(ref palette) = &self.global_palette {
            if self.bg_color.unwrap_or(0) as usize >= (palette.len() / PLTE_CHANNELS) {
                self.bg_color = None;
            }
        }
        Ok(self)
    }
    
    /// Returns the next frame info
    pub fn next_frame_info(&mut self) -> Result<Option<&Frame<'static>>, DecodingError> {
        loop {
            match self.decoder.decode_next()? {
                Some(Decoded::Frame(frame)) => {
                    self.current_frame = frame.clone();
                    if frame.palette.is_none() && self.global_palette.is_none() {
                        return Err(DecodingError::Format(
                            "No color table available for current frame."
                        ))
                    }
                    if self.memory_limit > 0  && (
                        (frame.width as u32 * frame.height as u32)
                        > self.memory_limit
                    ) {
                        return Err(DecodingError::Format(
                            "Image is too large to decode."
                        ))
                    }
                    break
                },
                Some(_) => (),
                None => return Ok(None)
                
            }
        }
        Ok(Some(&self.current_frame))
    }

    /// Reads the next frame from the image.
    ///
    /// Do not call `Self::next_frame_info` beforehand.
    /// Deinterlaces the result.
    pub fn read_next_frame(&mut self) -> Result<Option<&Frame<'static>>, DecodingError> {
        if self.next_frame_info()?.is_some() {
            let mut vec = vec![0; self.buffer_size()];
            self.read_into_buffer(&mut vec)?;
            self.current_frame.buffer = Cow::Owned(vec);
            self.current_frame.interlaced = false;
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
                if !self.fill_buffer(&mut buf[row*width..][..width])? {
                    return Err(DecodingError::Format("Image truncated"))
                }
            }
        } else {
            let buf = &mut buf[..self.buffer_size()];
            if !self.fill_buffer(buf)? {
                return Err(DecodingError::Format("Image truncated"))
            }
        };
        Ok(())
    }

    /// Reads data of the current frame into a pre-allocated buffer until the buffer has been
    /// filled completely.
    ///
    /// `Self::next_frame_info` needs to be called beforehand. Returns `true` if the supplied
    /// buffer could be filled completely. Should not be called after `false` had been returned.
    pub fn fill_buffer(&mut self, mut buf: &mut [u8]) -> Result<bool, DecodingError> {
        use self::ColorOutput::*;
        const PLTE_CHANNELS: usize = 3;
        macro_rules! handle_data(
            ($data:expr) => {
                match self.color_output {
                    RGBA => {
                        let transparent = self.current_frame.transparent;
                        let palette: &[u8] = match self.current_frame.palette {
                            Some(ref table) => &*table,
                            None => &*self.global_palette.as_ref().unwrap(),
                        };
                        let len = cmp::min(buf.len()/N_CHANNELS, $data.len());
                        for (rgba, &idx) in buf[..len*N_CHANNELS].chunks_mut(N_CHANNELS).zip($data.iter()) {
                            let plte_offset = PLTE_CHANNELS * idx as usize;
                            if palette.len() >= plte_offset + PLTE_CHANNELS {
                                let colors = &palette[plte_offset..];
                                rgba[0] = colors[0];
                                rgba[1] = colors[1];
                                rgba[2] = colors[2];
                                rgba[3] = if let Some(t) = transparent {
                                    if t == idx { 0x00 } else { 0xFF }
                                } else {
                                    0xFF
                                }
                            }
                        }
                        (len, N_CHANNELS)
                    },
                    Indexed => {
                        let len = cmp::min(buf.len(), $data.len());
                        buf[..len].copy_from_slice(&$data[..len]);
                        (len, 1)
                    }
                }
            }
        );
        let buf_len = self.buffer.len();
        if buf_len > 0 {
            let (len, channels) = handle_data!(&self.buffer);
            // This is WRONG!!!! Cuts form the wrong side…
            self.buffer.truncate(buf_len-len);
            let buf_ = buf; buf = &mut buf_[len*channels..];
            if buf.len() == 0 {
                return Ok(true)
            }
        }
        loop {
            match self.decoder.decode_next()? {
                Some(Decoded::Data(data)) => {
                    let (len, channels) = handle_data!(data);
                    let buf_ = buf; buf = &mut buf_[len*channels..]; // shorten buf
                    if buf.len() > 0 {
                        continue
                    } else if len < data.len() {
                        self.buffer.extend(data[len..].iter().map(|&v| v));
                    }
                    return Ok(true)
                },
                Some(_) => return Ok(false), // make sure that no important result is missed
                None => return Ok(false)
                
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
            Indexed => self.current_frame.width as usize
        }
    }
    
    /// Returns the color palette relevant for the current (next) frame
    pub fn palette(&self) -> Result<&[u8], DecodingError> {
        // TODO prevent planic
        Ok(match self.current_frame.palette {
            Some(ref table) => &*table,
            None => &*self.global_palette.as_ref().ok_or(DecodingError::Format(
                "No color table available for current frame."
            ))?,
        })
    }
    
    /// The global color palette
    pub fn global_palette(&self) -> Option<&[u8]> {
        self.global_palette.as_ref().map(|v| &**v)
    }

    /// Width of the image
    pub fn width(&self) -> u16 {
        self.decoder.decoder.width()
    }

    /// Height of the image
    pub fn height(&self) -> u16 {
        self.decoder.decoder.height()
    }

    /// Index of the background color in the global palette
    pub fn bg_color(&self) -> Option<usize> {
        self.bg_color.map(|v| v as usize)
    }
}

struct InterlaceIterator {
    len: usize,
    next: usize,
    pass: usize
}

impl iter::Iterator for InterlaceIterator {
    type Item = usize;

    fn next(&mut self) -> Option<Self::Item> {
        if self.len == 0 || self.pass > 3 {
            return None
        }
        let mut next = self.next + [8, 8, 4, 2][self.pass];
        while next >= self.len {
            next = [4, 2, 1, 0][self.pass];
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
    
    /* Commented because test::Bencher is unstable
    extern crate test;
    use std::io::prelude::*;
    #[bench]
    fn bench_tiny(b: &mut test::Bencher) {
        let mut data = Vec::new();
        File::open("tests/samples/sample_1.gif").unwrap().read_to_end(&mut data).unwrap();
        b.iter(|| {
            let mut decoder = Decoder::new(&*data).read_info().unwrap();
            let frame = decoder.read_next_frame().unwrap().unwrap();
            test::black_box(frame);
        });
        let mut decoder = Decoder::new(&*data).read_info().unwrap();
        b.bytes = decoder.read_next_frame().unwrap().unwrap().buffer.len() as u64
    }
    #[bench]
    fn bench_big(b: &mut test::Bencher) {
        let mut data = Vec::new();
        File::open("tests/sample_big.gif").unwrap().read_to_end(&mut data).unwrap();
        b.iter(|| {
            let mut decoder = Decoder::new(&*data).read_info().unwrap();
            let frame = decoder.read_next_frame().unwrap().unwrap();
            test::black_box(frame);
        });
        let mut decoder = Decoder::new(&*data).read_info().unwrap();
        b.bytes = decoder.read_next_frame().unwrap().unwrap().buffer.len() as u64
    }*/
    
    #[test]
    fn test_simple_indexed() {
        let mut decoder = Decoder::new(File::open("tests/samples/sample_1.gif").unwrap()).read_info().unwrap();
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
        ][..])
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
            let iter = InterlaceIterator { len: len, next: 0, pass: 0 };
            let lines = iter.collect::<Vec<_>>();
            assert_eq!(lines, expect);
        }
    }
}


#[cfg(feature = "c_api")]
mod c_interface {
    use std::io::prelude::*;
    use std::ptr;
    use std::borrow::Cow;

    use libc::c_int;
    
    use common::Block;

    use c_api::{self, GifWord};
    use c_api_utils::{CInterface, copy_colormap, copy_data, saved_images_new};

    use super::decoder::{Decoded, DecodingError};

    use super::{Reader};

    impl<R> Reader<R> where R: Read + 'static {
        /// Converts `Reader` into `CInterface`.
        pub fn into_c_interface(self) -> Box<CInterface> {
            Box::new(self)
        }
    }

    impl<R: Read> CInterface for Reader<R> {
        fn read_screen_desc(&mut self, this: &mut c_api::GifFileType) {
            this.SWidth = self.width() as GifWord;
            this.SHeight = self.height() as GifWord;
            this.SColorResolution = 255;//self.global_palette().len() as GifWord;
            this.SBackGroundColor = self.bg_color().unwrap_or(0) as GifWord;
            this.AspectByte = 0;
            self.offset = 0;
        }

        fn current_image_buffer(&mut self) -> Result<(&[u8], &mut usize), DecodingError> {
            if let Cow::Borrowed(_) = self.current_frame.buffer {
                self.read_next_frame()?;
            }
            Ok((&self.current_frame.buffer, &mut self.offset))
        }

        fn last_ext(&self) -> (u8, &[u8], bool) {
            self.decoder.decoder.last_ext()
        }

        fn next_record_type(&mut self) -> Result<Block, DecodingError> {
            loop {
                match self.decoder.decode_next()? {
                    Some(Decoded::BlockStart(type_)) => return Ok(type_),
                    Some(_) => (),
                    None => return Ok(Block::Trailer)
                }
            }
        }
    
        fn decode_next(&mut self) -> Result<Option<Decoded>, DecodingError> {
            self.decoder.decode_next()
        }

        /*
        unsafe fn read_to_end(&mut self, this: &mut c_api::GifFileType) -> Result<(), DecodingError> {
            self.read_screen_desc(this)?;
            self.read_to_end()?;
            this.ImageCount = self.frames().len() as c_int;
            let images = saved_images_new(this.ImageCount as usize);
            for (i, frame) in self.frames().iter().enumerate() {
                *images.offset(i as isize) = c_api::SavedImage {
                    ImageDesc: c_api::GifImageDesc {
                        Left: frame.left as GifWord,
                        Top: frame.top as GifWord,
                        Width: frame.width as GifWord,
                        Height: frame.height as GifWord,
                        Interlace: num::FromPrimitive::from_u8(frame.interlaced as u8).unwrap(),
                        ColorMap: copy_colormap(&frame.palette)
                    },
                    // on malloc(3) heap
                    RasterBits: copy_data(&*frame.buffer),
                    ExtensionBlockCount: 0,
                    ExtensionBlocks: ptr::null_mut()
                }
                
            }
            this.SavedImages = images;
            Ok(())
        }*/
    }
}
