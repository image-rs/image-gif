use std::io::prelude::*;
use std::borrow::Cow;

use crate::{GifWord, GifFileType};
use crate::c_api_utils::{CInterface, copy_colormap, copy_data, saved_images_new};

use gif::{Block, Decoded, DecodingError, Reader};

impl<R> From<Reader<R>> for Box<dyn CInterface> 
where
    R: Read + 'static
{
    /// Converts `Reader` into `CInterface`.
    fn from(reader: Reader<R>) -> Box<dyn CInterface> {
        Box::new(reader)
    }
}

/// The c-interface expects us to store more state.
///
/// This wraps the gif crate to do just that.
struct<R: Read + 'static> TrackingReader<R> {
    inner: Reader<R>,
    /// Offset within the frame for reading chunks.
    offset: usize,
}

impl<R: Read> CInterface for Reader<R> {
    fn read_screen_desc(&mut self, this: &mut GifFileType) {
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
            match self.decode_next()? {
                Some(Decoded::BlockStart(type_)) => return Ok(type_),
                Some(_) => (),
                None => return Ok(Block::Trailer)
            }
        }
    }

    fn decode_next(&mut self) -> Result<Option<Decoded>, DecodingError> {
        self.decode_next()
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
