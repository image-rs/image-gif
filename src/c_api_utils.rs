use std::io::{self, Read};
use std::mem;
use std::ops;
use std::slice;

use libc::{malloc, size_t, c_int, read, close};

use common::Block;
use reader::{Decoded, DecodingError, PLTE_CHANNELS};
use c_api::{GifFileType, SavedImage, ColorMapObject, GifColorType, c_bool,
           InputFunc
};

pub trait CInterface {
    fn read_screen_desc(&mut self, &mut GifFileType);
    fn current_image_buffer(&mut self) -> Result<(&[u8], &mut usize), DecodingError>;
    //fn seek_to(&mut self, position: Progress) -> Result<(), DecodingError>;
    fn last_ext(&self) -> (u8, &[u8], bool);
    fn next_record_type(&mut self) -> Result<Block, DecodingError>;
    fn decode_next(&mut self) -> Result<Option<Decoded>, DecodingError>;
    //unsafe fn read_to_end(&mut self, &mut GifFileType) -> Result<(), DecodingError>;
}

pub unsafe fn saved_images_new(count: usize) -> *mut SavedImage {
    mem::transmute::<_, *mut SavedImage>(malloc(
        (mem::size_of::<SavedImage>() *  count) as size_t
    ))
}

pub unsafe fn copy_data(buf: &[u8]) -> *mut u8 {
    let data = mem::transmute::<_, *mut u8>(malloc(
        (mem::size_of::<SavedImage>() *  buf.len()) as size_t
    ));
    slice::from_raw_parts_mut(data, buf.len()).copy_from_slice(buf);
    //for (i, &b) in buf.iter().enumerate() {
    //    *data.offset(i as isize) = b
    //}
    data
}

pub unsafe fn copy_colormap(map: &Option<Vec<u8>>) -> *mut ColorMapObject {
    let map: &[u8] = match *map {
        Some(ref map) => &*map,
        None => &[]
    };
    let new_map = mem::transmute::<_, *mut ColorMapObject>(malloc(mem::size_of::<ColorMapObject>() as size_t));
    (*new_map).ColorCount = (map.len()/PLTE_CHANNELS) as c_int;
    (*new_map).BitsPerPixel = 8;
    (*new_map).SortFlag = false;
    let colors = mem::transmute::<_, *mut GifColorType>(malloc(
        (mem::size_of::<GifColorType>() *  (*new_map).ColorCount as usize) as size_t
    ));
    for (i, c) in map.chunks(PLTE_CHANNELS).enumerate() {
        *colors.offset(i as isize) = GifColorType {
            Red: c[0],
            Green: c[1],
            Blue: c[2],
        }
    }
    (*new_map).Colors = colors;
    new_map
}

/// A simple wrapper around a C file handle
pub struct CFile {
    fp: c_int
}

impl CFile {
    pub fn new(fp: c_int) -> CFile {
        CFile {
            fp: fp
        }
    }
}

impl Read for CFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let count = unsafe { read(
            self.fp,
            mem::transmute(buf.as_mut_ptr()),
            buf.len() as size_t
        ) };
        match count {
            -1 => Err(io::Error::last_os_error()),
            n => Ok(n as usize)
        }
    }
}

impl ops::Drop for CFile {
    fn drop(&mut self) {
        unsafe {close(self.fp)};
    }
}

/// A wrapper around `InputFunc`
pub struct FnInputFile {
    func: InputFunc,
    file: *mut GifFileType
}

impl FnInputFile {
    pub fn new(func: InputFunc, file: *mut GifFileType) -> FnInputFile {
        FnInputFile {
            func: func,
            file: file
        }
    }
}

impl Read for FnInputFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let count = unsafe { (self.func)(
            self.file,
            mem::transmute(buf.as_mut_ptr()),
            buf.len() as c_int
        ) };
        match count {
            -1 => Err(io::Error::from_raw_os_error(count)),
            n => Ok(n as usize)
        }
    }
}
