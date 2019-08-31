//! C API, drop-in replacement for libgif

#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(missing_docs)] //FIXME

use std::cmp;
use std::mem;
use std::ptr;
use std::boxed;
use std::fs::File;
use std::ffi::CStr;
use std::str;
use std::slice;

use libc::{free, c_int, c_uint, c_char, c_uchar, c_void};

use reader::{Decoder, Reader, Decoded};
use c_api_utils::{CInterface, CFile, FnInputFile};

/// NOTE As of rust issue #954 `bool` is compatible with c_bool.
pub type c_bool = bool;

pub type GifPixelType = c_uchar;
pub type GifRowType = *mut c_uchar;
pub type GifByteType = c_uchar;
pub type GifPrefixType = c_uint;
pub type GifWord = c_int;

#[repr(C)]
pub struct GifColorType {
    pub Red: GifByteType,
    pub Green: GifByteType,
    pub Blue: GifByteType
}

#[repr(C)]
pub struct ColorMapObject {
    pub ColorCount: c_int,
    pub BitsPerPixel: c_int,
    pub SortFlag: c_bool,
    /// on malloc(3) heap
    pub Colors: *mut GifColorType // TODO USE MALLOC for this
}

#[repr(C)]
pub struct ExtensionBlock {
    pub ByteCount: c_int,
    /// on malloc(3) heap
    pub Bytes: *mut GifByteType, // TODO USE MALLOC for this
    /// The block function code
    pub Function: c_int
//#define CONTINUE_EXT_FUNC_CODE    0x00    /* continuation subblock */
//#define COMMENT_EXT_FUNC_CODE     0xfe    /* comment */
//#define GRAPHICS_EXT_FUNC_CODE    0xf9    /* graphics control (GIF89) */
//#define PLAINTEXT_EXT_FUNC_CODE   0x01    /* plaintext */
//#define APPLICATION_EXT_FUNC_CODE 0xff    /* application block */
}

#[repr(C)]
pub struct SavedImage {
    pub ImageDesc: GifImageDesc,
    /// on malloc(3) heap
    pub RasterBits: *mut GifByteType,
    /// Count of extensions before image
    pub ExtensionBlockCount: c_int,
    /// Extensions before image
    pub ExtensionBlocks: *mut ExtensionBlock
}

#[repr(C)]
pub struct GifImageDesc {
    /// Current image dimensions. (left)
    pub Left: GifWord,
    /// Current image dimensions. (top)
    pub Top: GifWord,
    /// Current image dimensions. (width)
    pub Width: GifWord,
    /// Current image dimensions. (height)
    pub Height: GifWord,
    /// Sequential/Interlaced lines.
    pub Interlace: c_bool,
    /// The local color map
    pub ColorMap: *mut ColorMapObject
}

#[repr(C)]
pub struct GifFileType {
    /// Size of virtual canvas (width)
    pub SWidth: GifWord,
    /// Size of virtual canvas (height)
    pub SHeight: GifWord,
    /// How many colors can we generate?
    pub SColorResolution: GifWord,
    /// Background color for virtual canvas
    pub SBackGroundColor: GifWord,
    /// Used to compute pixel aspect ratio
    pub AspectByte: GifByteType,
    /// Global colormap, NULL if nonexistent.
    pub SColorMap: *mut ColorMapObject,
    /// Number of current image (both APIs)
    pub ImageCount: c_int,
    /// Current image (low-level API)
    pub Image: GifImageDesc,
    /// Image sequence (high-level API)
    pub SavedImages: *mut SavedImage,
    /// Count extensions past last image
    pub ExtensionBlockCount: c_int,
    /// Extensions past last image
    pub ExtensionBlocks: *mut ExtensionBlock,
    /// Last error condition reported
    pub Error: c_int,
    /// hook to attach user data (TVT)
    pub UserData: *mut c_void,
    /// Don't mess with this!
    pub Private: *mut c_void,
}

#[repr(C)]
pub enum GifRecordType {
    UNDEFINED_RECORD_TYPE,
    SCREEN_DESC_RECORD_TYPE,
    IMAGE_DESC_RECORD_TYPE, /* Begin with ',' */
    EXTENSION_RECORD_TYPE,  /* Begin with '!' */
    TERMINATE_RECORD_TYPE   /* Begin with ';' */
}

/// Input callback for DGifOpen. Returns `c_int` bytes input the buffer
/// and returns the number of bytes read.
pub type InputFunc = extern "C" fn(*mut GifFileType, *mut GifByteType, c_int) -> c_int;

const D_GIF_SUCCEEDED         : c_int = 0;
const D_GIF_ERR_OPEN_FAILED   : c_int = 101;    /* And DGif possible errors. */
const D_GIF_ERR_READ_FAILED   : c_int = 102;
const D_GIF_ERR_NOT_GIF_FILE  : c_int = 103;
const D_GIF_ERR_NO_SCRN_DSCR  : c_int = 104;
const D_GIF_ERR_NO_IMAG_DSCR  : c_int = 105;
const D_GIF_ERR_NO_COLOR_MAP  : c_int = 106;
const D_GIF_ERR_WRONG_RECORD  : c_int = 107;
const D_GIF_ERR_DATA_TOO_BIG  : c_int = 108;
const D_GIF_ERR_NOT_ENOUGH_MEM: c_int = 109;
const D_GIF_ERR_CLOSE_FAILED  : c_int = 110;
const D_GIF_ERR_NOT_READABLE  : c_int = 111;
const D_GIF_ERR_IMAGE_DEFECT  : c_int = 112;
const D_GIF_ERR_EOF_TOO_SOON  : c_int = 113;

const GIF_ERROR: c_int = 0;
const GIF_OK   : c_int = 1;

macro_rules! try_capi {
    ($val:expr, $err:expr, $code:expr, $retval:expr) => (
        match $val {
            Ok(val) => val,
            Err(_) => {
                if $err != ptr::null_mut() {
                    *$err = $code
                }
                return $retval
            }
        }
    );
    ($val:expr) => (
        match $val {
            Ok(val) => val,
            Err(_) => return GIF_ERROR
        }
    );
}

macro_rules! try_get_decoder {
    ($this:expr) => (
        if $this != ptr::null_mut() {
            let decoder: &mut &mut CInterface = mem::transmute((*$this).Private);
            decoder
        } else {
            return GIF_ERROR
        }
    );
}

#[no_mangle] pub unsafe extern "C"
fn DGifOpenFileName(gif_file_name: *const c_char, err: *mut c_int) -> *mut GifFileType {
    let file = try_capi!(
        File::open(try_capi!(
            str::from_utf8(CStr::from_ptr(gif_file_name).to_bytes()),
            err, D_GIF_ERR_OPEN_FAILED, ptr::null_mut()
        )),
        err, D_GIF_ERR_OPEN_FAILED, ptr::null_mut()
    );
    let mut decoder = try_capi!(
        Decoder::new(file).read_info(),
        err, D_GIF_ERR_READ_FAILED, ptr::null_mut()
    ).into_c_interface();
    let this: *mut GifFileType = Box::into_raw(Box::new(mem::zeroed()));
    decoder.read_screen_desc(&mut *this);
    let decoder = Box::into_raw(Box::new(Box::into_raw(decoder)));
    (*this).Private = mem::transmute(decoder);
    this
}

#[no_mangle] pub unsafe extern "C"
fn DGifOpenFileHandle(fp: c_int, err: *mut c_int) -> *mut GifFileType {
    let mut decoder = try_capi!(
        Decoder::new(CFile::new(fp)).read_info(),
        err, D_GIF_ERR_READ_FAILED, ptr::null_mut()
    ).into_c_interface();
    let this: *mut GifFileType = Box::into_raw(Box::new(mem::zeroed()));
    decoder.read_screen_desc(&mut *this);
    let decoder = Box::into_raw(Box::new(Box::into_raw(decoder)));
    (*this).Private = mem::transmute(decoder);
    this
}

/*
#[no_mangle] pub unsafe extern "C"
fn DGifSlurp(this: *mut GifFileType) -> c_int {
    match try_get_decoder!(this).read_to_end(mem::transmute(this)) {
        Ok(()) => GIF_OK,
        Err(_) => GIF_ERROR
    }
}
*/
#[no_mangle] pub unsafe extern "C"
fn DGifOpen(user_data: *mut c_void, read_fn: InputFunc, err: *mut c_int) -> *mut GifFileType {
    let this: *mut GifFileType = Box::into_raw(Box::new(mem::zeroed()));
    (*this).UserData = user_data;
    let decoder = try_capi!(
        Decoder::new(FnInputFile::new(read_fn, this)).read_info(),
        err, D_GIF_ERR_READ_FAILED, {
            // TODO: check if it is ok and expected to free GifFileType
            // This is unclear since the API exposes the whole struct to the read
            // function and not only the user data
            let _: Box<GifFileType> = Box::from_raw(this);
            ptr::null_mut()
        }
    ).into_c_interface();
    let decoder = Box::into_raw(Box::new(Box::into_raw(decoder)));
    (*this).Private = mem::transmute(decoder);
    this
}

/// Closes the file and also frees all data structures.
#[no_mangle] pub unsafe extern "C"
fn DGifCloseFile(this: *mut GifFileType, _: *mut c_int)
-> c_int {
    if this != ptr::null_mut() {
        let this: Box<GifFileType> = Box::from_raw(this);
        let _: Box<Box<CInterface>> = mem::transmute(this.Private);
        for image in slice::from_raw_parts_mut(this.SavedImages, this.ImageCount as usize) {
            free(mem::transmute(image.RasterBits));
            if image.ImageDesc.ColorMap != ptr::null_mut() {
                free(mem::transmute((*image.ImageDesc.ColorMap).Colors))
            }
            free(mem::transmute(image.ImageDesc.ColorMap));
            if image.ExtensionBlockCount != 0 {
                GifFreeExtensions(&mut image.ExtensionBlockCount, &mut image.ExtensionBlocks)
            }
        }
        free(mem::transmute(this.SavedImages));
    }
    GIF_OK
}

// legacy but needed API
#[no_mangle] pub unsafe extern "C"
fn DGifGetScreenDesc(_: *mut GifFileType) -> c_int {
    GIF_OK
}
/*
#[no_mangle] pub unsafe extern "C"
fn DGifGetRecordType(this: *mut GifFileType, record_type: *mut GifRecordType) -> c_int {
    use common::Block::*;
    use self::GifRecordType::*;
    *record_type = match try_capi!(try_get_decoder!(this).next_record_type()) {
        Image => IMAGE_DESC_RECORD_TYPE,
        Extension => EXTENSION_RECORD_TYPE,
        Trailer => TERMINATE_RECORD_TYPE
    };
    GIF_OK
}
*/
#[no_mangle] pub unsafe extern "C"
fn DGifGetImageDesc(this: *mut GifFileType) -> c_int {
    match try_get_decoder!(this).current_image_buffer() {
        Ok(_) => GIF_OK,
        Err(_) => GIF_ERROR
    }
}

#[no_mangle] pub unsafe extern "C"
fn DGifGetLine(this: *mut GifFileType, line: *mut GifPixelType, len: c_int) -> c_int {
    let (buffer, offset) = try_capi!(try_get_decoder!(this).current_image_buffer());
    let buffer = &buffer[*offset..];
    let len = cmp::min(buffer.len(), len as usize);
    *offset = *offset + len;
    let line = slice::from_raw_parts_mut(line, len);
    line.copy_from_slice(&buffer[..len]);
    GIF_OK
}
//int DGifGetPixel(GifFileType *GifFile, GifPixelType GifPixel);
//int DGifGetComment(GifFi leType *GifFile, char *GifComment);

/// Returns the type of the extension and the first extension sub-block `(size, data...)`
#[no_mangle] pub unsafe extern "C"
fn DGifGetExtension(this: *mut GifFileType, ext_type: *mut c_int, ext_block: *mut *const GifByteType) -> c_int {
    use common::Block::*;
    let decoder = try_get_decoder!(this);
    match try_capi!(decoder.next_record_type()) {
        Image | Trailer => {
            if ext_block != ptr::null_mut() {
                *ext_block = ptr::null_mut();
            }
            if ext_type != ptr::null_mut() {
                *ext_type = 0;
            }
        }
        Extension => {
            match try_capi!(decoder.decode_next()) {
                Some(Decoded::SubBlockFinished(type_, data))
                | Some(Decoded::BlockFinished(type_, data)) => {
                    if ext_block != ptr::null_mut() {
                        *ext_block = data.as_ptr();
                    }
                    if ext_type != ptr::null_mut() {
                        *ext_type = type_ as c_int;
                    }
                }
                _ => return GIF_ERROR
            }
        }
    }
    GIF_OK
}

/// Returns the next extension sub-block `(size, data...)`
#[no_mangle] pub unsafe extern "C"
fn DGifGetExtensionNext(this: *mut GifFileType, ext_block: *mut *const GifByteType) -> c_int {
    // TODO extract next sub block
    let mut decoder = try_get_decoder!(this);
    if decoder.last_ext().2 {
        if ext_block != ptr::null_mut() {
            *ext_block = ptr::null_mut();
        }
        GIF_OK
    } else {
        match try_capi!(decoder.decode_next()) {
            Some(Decoded::SubBlockFinished(_, data))
            | Some(Decoded::BlockFinished(_, data)) => {
                if ext_block != ptr::null_mut() {
                    *ext_block = data.as_ptr();
                }
                GIF_OK
            }
            _ => GIF_ERROR
        }
    }
}
/*
/// This function reallocs `ext_blocks` and copies `data`
#[no_mangle] pub unsafe extern "C"
fn GifAddExtensionBlock(block_count: *mut c_int, ext_blocks: *mut *const ExtensionBlock,
                        ext_type: c_int, len: c_uint, data: *const c_uchar) -> c_int {
    GIF_OK
}
*/
#[no_mangle] pub unsafe extern "C"
fn GifFreeExtensions(block_count: *mut c_int, ext_blocks: *mut *mut ExtensionBlock) {
    if ext_blocks == ptr::null_mut() || block_count ==  ptr::null_mut() {
        return
    }
    for i in 0..(*block_count) as isize {
        let block = (*ext_blocks).offset(i);
        free(mem::transmute((*block).Bytes));
    }
    free(mem::transmute(ext_blocks));
    *ext_blocks = ptr::null_mut();
    *block_count = 0;
}
