//! C API, drop-in replacement for libgif

#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(missing_docs)]
mod c_api_utils;

use std::cmp;
use std::mem;
use std::ptr;
use std::boxed;
use std::fs::File;
use std::ffi::CStr;
use std::str;
use std::slice;
use std::panic;

use libc::{free, c_int, c_uint, c_char, c_uchar, c_void};

use crate::c_api_utils::{CInterface, CFile, FnInputFile};
use gif::{Decoder, Reader, Decoded};

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

/// Holds a complete decoder and current state.
///
/// ## Safety
/// This struct can be safely zeroed. When creating one manually you should not leave it
/// uninitialized.
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

#[repr(u8)]
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

/* And DGif possible errors. */
pub const D_GIF_SUCCEEDED         : c_int = 0;
pub const D_GIF_ERR_OPEN_FAILED   : c_int = 101;
pub const D_GIF_ERR_READ_FAILED   : c_int = 102;
pub const D_GIF_ERR_NOT_GIF_FILE  : c_int = 103;
pub const D_GIF_ERR_NO_SCRN_DSCR  : c_int = 104;
pub const D_GIF_ERR_NO_IMAG_DSCR  : c_int = 105;
pub const D_GIF_ERR_NO_COLOR_MAP  : c_int = 106;
pub const D_GIF_ERR_WRONG_RECORD  : c_int = 107;
pub const D_GIF_ERR_DATA_TOO_BIG  : c_int = 108;
pub const D_GIF_ERR_NOT_ENOUGH_MEM: c_int = 109;
pub const D_GIF_ERR_CLOSE_FAILED  : c_int = 110;
pub const D_GIF_ERR_NOT_READABLE  : c_int = 111;
pub const D_GIF_ERR_IMAGE_DEFECT  : c_int = 112;
pub const D_GIF_ERR_EOF_TOO_SOON  : c_int = 113;

const GIF_ERROR: c_int = 0;
const GIF_OK   : c_int = 1;

impl GifFileType {
    fn zeroed_box() -> Box<Self> {
        // SAFETY: GifFileType is zeroable.
        unsafe { Box::new(mem::zeroed()) }
    }

    fn attach_decoder(&mut self, interface: Box<dyn CInterface>) {
        assert_eq!(self.Private, ptr::null_mut());
        let decoder = Box::into_raw(Box::new(Box::into_raw(interface)));
        self.Private = decoder as *mut c_void;
    }

    fn detach_decoder(&mut self) -> Box<dyn CInterface> {
        assert_ne!(self.Private, ptr::null_mut());
        let boxed = unsafe { Box::<Box<dyn CInterface>>::from_raw(self.Private as *mut _) };
        self.Private = ptr::null_mut();
        *boxed
    }

    fn decoder_mut(&mut self) -> Result<&mut dyn CInterface, c_int> {
        match self.Private == ptr::null_mut() {
            true => Ok(unsafe { &mut **(self.Private as *mut Box<dyn CInterface>) }),
            false => Err(GIF_ERROR),
        }
    }
}

/// Open a file by name, returning a pointer to its allocated decoder.
///
/// # Return
///
/// Returns NULL on error, a pointer to `GifFileType` on success. If `err` is not NULL then an
/// additional error code is written to it.
#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifOpenFileName(gif_file_name: *const c_char, err: *mut c_int) -> *mut GifFileType {
    ErrPtr::with_err(&err).catch_unwind(|| {
        let name = str::from_utf8(CStr::from_ptr(gif_file_name).to_bytes())
            .map_err(|_| D_GIF_ERR_OPEN_FAILED)?;
        let file = File::open(name)
            .map_err(|_| D_GIF_ERR_OPEN_FAILED)?;
        let mut decoder = Decoder::new(file)
            .read_info()
            .map_err(|_| D_GIF_ERR_READ_FAILED)?
            .into_c_interface();
        let mut this: Box<GifFileType> = GifFileType::zeroed_box();
        decoder.read_screen_desc(&mut *this);
        this.attach_decoder(decoder);
        Ok(Box::into_raw(this))
    }).unwrap_or_else(ptr::null_mut)
}

/// Wrap a file as gif, returning a newly allocated decoder.
///
/// # Return
///
/// Returns NULL on error, a pointer to `GifFileType` on success. If `err` is not NULL then an
/// additional error code is written to it.
#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifOpenFileHandle(fp: c_int, err: *mut c_int) -> *mut GifFileType {
    ErrPtr::with_err(&err).catch_unwind(|| {
        let mut decoder = Decoder::new(CFile::new(fp))
            .read_info()
            .map_err(|_| D_GIF_ERR_READ_FAILED)?
            .into_c_interface();
        let mut this: Box<GifFileType> = GifFileType::zeroed_box();
        decoder.read_screen_desc(&mut *this);
        this.attach_decoder(decoder);
        Ok(Box::into_raw(this))
    }).unwrap_or_else(ptr::null_mut)
}

struct ErrPtr<'a>(Option<&'a mut c_int>);

impl<'a> ErrPtr<'a> {
    fn new(err: &'a mut c_int) -> Self {
        ErrPtr(Some(err))
    }

    unsafe fn with_err(err: &'a *mut c_int) -> Self {
        ErrPtr(match ptr::NonNull::new(*err) {
            Some(ptr) => Some(&mut *ptr.as_ptr()),
            None => None,
        })
    }

    fn catch_err<T>(mut self, result: Result<T, c_int>) -> Option<T> {
        if let Some(ref mut ptr) = self.0 {
            match result {
                Ok(val) => Some(val),
                Err(err) => {
                    **ptr = err;
                    None
                }
            }
        } else {
            result.ok()
        }
    }

    fn catch_with<T, E>(self, result: Result<T, E>, err: c_int) -> Option<T> {
        self.catch_err(result.map_err(|_| err))
    }

    fn catch_unwind<F, T>(self, then: F) -> Option<T>
    where
        F: FnOnce() -> Result<T, c_int> + panic::UnwindSafe,
    {
        match panic::catch_unwind(then) {
            Ok(result) => self.catch_err(result),
            Err(unwound) => {
                // Problem: We drop an arbitrary `dyn Any`. This may panic but panicking is exactly
                // what we can not allow right now. So, we make Rust abort for us by dropping it in
                // another Drop and panicking during unwinding when that drop does not succeed.
                struct DropInDrop<T>(Option<T>, Option<OopsReallyAbort>);
                struct OopsReallyAbort;
                impl<T> Drop for DropInDrop<T> {
                    fn drop(&mut self) {
                        let _ = self.0.take();
                        // If we reach here, do not actually panic.
                        mem::forget(self.1.take());
                    }
                }
                impl OopsReallyAbort {
                    fn drop(&mut self) {
                        std::process::abort()
                    }
                }
                let _ = DropInDrop(Some(unwound), Some(OopsReallyAbort));
                self.catch_err(Err(D_GIF_ERR_NOT_READABLE))
            },
        }
    }
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

/// Decode gif data supplied by a user defined reader function.
///
/// A pointer to the `GifFileType` containing the partially decoded data is passed to the reader.
/// This pointer pointer is invalidated and must not be dereferenced when this function returns
/// with an error value.
///
/// # Return
///
/// Returns NULL on error, a pointer to `GifFileType` on success. If `err` is not NULL then an
/// additional error code is written to it.
#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifOpen(user_data: *mut c_void, read_fn: InputFunc, err: *mut c_int) -> *mut GifFileType {
    ErrPtr::with_err(&err).catch_unwind(|| {
        // Problem: Provenance. We share this pointer between the return and the input file that's
        // stored within Private. Thus they can't be derived from each other according to stacked
        // borrows at least. Though we do not want to leak it.
        struct Reclaim(*mut GifFileType);
        impl Drop for Reclaim {
            fn drop(&mut self) {
                let _ = unsafe { Box::from_raw(self.0) };
            }
        }

        let mut this: Box<GifFileType> = GifFileType::zeroed_box();
        this.UserData = user_data;

        let reclaim = Reclaim(Box::into_raw(this));
        let decoder = Decoder::new(FnInputFile::new(read_fn, reclaim.0))
            .read_info()
            .map_err(|_| D_GIF_ERR_READ_FAILED)?
            .into_c_interface();
        unsafe { &mut *reclaim.0 }.attach_decoder(decoder);

        let unclaim = reclaim.0;
        mem::forget(reclaim);
        Ok(unclaim as *mut GifFileType)
    }).unwrap_or_else(ptr::null_mut)
}

/// Closes the file and also frees all data structures.
#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifCloseFile(this: *mut GifFileType, err: *mut c_int) -> c_int {
    if this == ptr::null_mut() {
        return GIF_OK;
    }

    let result = ErrPtr::with_err(&err).catch_unwind(|| {
        let mut this: Box<GifFileType> = Box::from_raw(this);
        // Detach the decoder, and keep it around until everything is detached.
        let _decoder = this.detach_decoder();
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
        Ok(())
    });

    match result {
        Some(()) => GIF_OK,
        None => GIF_ERROR,
    }
}

// legacy but needed API
#[no_mangle] pub unsafe extern "C"
fn DGifGetScreenDesc(_: *mut GifFileType) -> c_int {
    GIF_OK
}

#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifGetRecordType(this: *mut GifFileType, record_type: *mut GifRecordType) -> c_int {
    let mut status = GIF_OK;
    ErrPtr::new(&mut status).catch_unwind(|| {
        use self::GifRecordType::*;
        let decoder = unsafe { &mut *this };
        let next_type = decoder
            .decoder_mut()?
            .next_record_type()
            .map_err(|_| D_GIF_ERR_NOT_READABLE)?;
        use common::Block;
        *record_type = match next_type {
            Block::Image => IMAGE_DESC_RECORD_TYPE,
            Block::Extension => EXTENSION_RECORD_TYPE,
            Block::Trailer => TERMINATE_RECORD_TYPE
        };
        Ok(())
    });
    status
}

#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifGetImageDesc(this: *mut GifFileType) -> c_int {
    unsafe { &mut *this }
        .decoder_mut()
        .map(|_| GIF_OK)
        .unwrap_or_else(|x| x)
}

#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifGetLine(this: *mut GifFileType, line: *mut GifPixelType, len: c_int) -> c_int {
    let mut err: c_int = GIF_OK;
    ErrPtr::new(&mut err).catch_unwind(|| {
        let decoder = unsafe { &mut *this };
        let (buffer, offset) = decoder
            .decoder_mut()?
            .current_image_buffer()
            .map_err(|_| GIF_ERROR)?;
        let buffer = &buffer[*offset..];
        let len = cmp::min(buffer.len(), len as usize);
        *offset = *offset + len;
        let line = slice::from_raw_parts_mut(line, len);
        line.copy_from_slice(&buffer[..len]);
        Ok(())
    });
    err
}
//int DGifGetPixel(GifFileType *GifFile, GifPixelType GifPixel);
//int DGifGetComment(GifFi leType *GifFile, char *GifComment);

/// Returns the type of the extension and the first extension sub-block `(size, data...)`
#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifGetExtension(this: *mut GifFileType, ext_type: *mut c_int, ext_block: *mut *const GifByteType) -> c_int {
    let mut err: c_int = GIF_OK;
    ErrPtr::new(&mut err).catch_unwind(|| {
        use common::Block::*;
        let decoder = unsafe { &mut *this };
        let decoder = decoder.decoder_mut()?;

        match decoder.next_record_type().map_err(|_| GIF_ERROR)? {
            Image | Trailer => {
                if ext_block != ptr::null_mut() {
                    *ext_block = ptr::null_mut();
                }
                if ext_type != ptr::null_mut() {
                    *ext_type = 0;
                }
            }
            Extension => {
                match decoder.decode_next().map_err(|_| GIF_ERROR)? {
                    Some(Decoded::SubBlockFinished(type_, data))
                    | Some(Decoded::BlockFinished(type_, data)) => {
                        if ext_block != ptr::null_mut() {
                            *ext_block = data.as_ptr();
                        }
                        if ext_type != ptr::null_mut() {
                            *ext_type = type_ as c_int;
                        }
                    }
                    _ => return Err(GIF_ERROR)
                }
            }
        }
        Ok(())
    });
    err
}

/// Returns the next extension sub-block `(size, data...)`
#[allow(unused_unsafe)]
#[no_mangle] pub unsafe extern "C"
fn DGifGetExtensionNext(this: *mut GifFileType, ext_block: *mut *const GifByteType) -> c_int {
    let mut err: c_int = GIF_OK;
    ErrPtr::new(&mut err).catch_unwind(|| {
        let decoder = unsafe { &mut *this };
        let decoder = decoder.decoder_mut()?;

        // TODO extract next sub block
        if decoder.last_ext().2 {
            if ext_block != ptr::null_mut() {
                *ext_block = ptr::null_mut();
            }
            Ok(GIF_OK)
        } else {
            match decoder.decode_next().map_err(|_| GIF_ERROR)? {
                Some(Decoded::SubBlockFinished(_, data))
                | Some(Decoded::BlockFinished(_, data)) => {
                    if ext_block != ptr::null_mut() {
                        *ext_block = data.as_ptr();
                    }
                    Ok(GIF_OK)
                }
                _ => Err(GIF_ERROR)
            }
        }
    });
    err
}
/*
/// This function reallocs `ext_blocks` and copies `data`
#[no_mangle] pub unsafe extern "C"
fn GifAddExtensionBlock(block_count: *mut c_int, ext_blocks: *mut *const ExtensionBlock,
                        ext_type: c_int, len: c_uint, data: *const c_uchar) -> c_int {
    GIF_OK
}
*/
#[allow(unused_unsafe)]
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
