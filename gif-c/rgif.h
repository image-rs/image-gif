#include <cstdarg>
#include <cstdint>
#include <cstdlib>
#include <new>

static const int D_GIF_ERR_CLOSE_FAILED = 110;

static const int D_GIF_ERR_DATA_TOO_BIG = 108;

static const int D_GIF_ERR_EOF_TOO_SOON = 113;

static const int D_GIF_ERR_IMAGE_DEFECT = 112;

static const int D_GIF_ERR_NOT_ENOUGH_MEM = 109;

static const int D_GIF_ERR_NOT_GIF_FILE = 103;

static const int D_GIF_ERR_NOT_READABLE = 111;

static const int D_GIF_ERR_NO_COLOR_MAP = 106;

static const int D_GIF_ERR_NO_IMAG_DSCR = 105;

static const int D_GIF_ERR_NO_SCRN_DSCR = 104;

static const int D_GIF_ERR_OPEN_FAILED = 101;

static const int D_GIF_ERR_READ_FAILED = 102;

static const int D_GIF_ERR_WRONG_RECORD = 107;

static const int D_GIF_SUCCEEDED = 0;

enum class GifRecordType : uint8_t {
  UNDEFINED_RECORD_TYPE,
  SCREEN_DESC_RECORD_TYPE,
  IMAGE_DESC_RECORD_TYPE,
  EXTENSION_RECORD_TYPE,
  TERMINATE_RECORD_TYPE,
};

using GifWord = int;

using GifByteType = unsigned char;

/// NOTE As of rust issue #954 `bool` is compatible with c_bool.
using c_bool = bool;

struct GifColorType {
  GifByteType Red;
  GifByteType Green;
  GifByteType Blue;
};

struct ColorMapObject {
  int ColorCount;
  int BitsPerPixel;
  c_bool SortFlag;
  /// on malloc(3) heap
  GifColorType *Colors;
};

struct GifImageDesc {
  /// Current image dimensions. (left)
  GifWord Left;
  /// Current image dimensions. (top)
  GifWord Top;
  /// Current image dimensions. (width)
  GifWord Width;
  /// Current image dimensions. (height)
  GifWord Height;
  /// Sequential/Interlaced lines.
  c_bool Interlace;
  /// The local color map
  ColorMapObject *ColorMap;
};

struct ExtensionBlock {
  int ByteCount;
  /// on malloc(3) heap
  GifByteType *Bytes;
  /// The block function code
  int Function;
};

struct SavedImage {
  GifImageDesc ImageDesc;
  /// on malloc(3) heap
  GifByteType *RasterBits;
  /// Count of extensions before image
  int ExtensionBlockCount;
  /// Extensions before image
  ExtensionBlock *ExtensionBlocks;
};

/// Holds a complete decoder and current state.
///
/// ## Safety
/// This struct can be safely zeroed. When creating one manually you should not leave it
/// uninitialized.
struct GifFileType {
  /// Size of virtual canvas (width)
  GifWord SWidth;
  /// Size of virtual canvas (height)
  GifWord SHeight;
  /// How many colors can we generate?
  GifWord SColorResolution;
  /// Background color for virtual canvas
  GifWord SBackGroundColor;
  /// Used to compute pixel aspect ratio
  GifByteType AspectByte;
  /// Global colormap, NULL if nonexistent.
  ColorMapObject *SColorMap;
  /// Number of current image (both APIs)
  int ImageCount;
  /// Current image (low-level API)
  GifImageDesc Image;
  /// Image sequence (high-level API)
  SavedImage *SavedImages;
  /// Count extensions past last image
  int ExtensionBlockCount;
  /// Extensions past last image
  ExtensionBlock *ExtensionBlocks;
  /// Last error condition reported
  int Error;
  /// hook to attach user data (TVT)
  void *UserData;
  /// Don't mess with this!
  void *Private;
};

using GifPixelType = unsigned char;

/// Input callback for DGifOpen. Returns `c_int` bytes input the buffer
/// and returns the number of bytes read.
using InputFunc = int(*)(GifFileType*, GifByteType*, int);

extern "C" {

/// Closes the file and also frees all data structures.
int DGifCloseFile(GifFileType *this_, int *err);

/// Returns the type of the extension and the first extension sub-block `(size, data...)`
int DGifGetExtension(GifFileType *this_, int *ext_type, const GifByteType **ext_block);

/// Returns the next extension sub-block `(size, data...)`
int DGifGetExtensionNext(GifFileType *this_, const GifByteType **ext_block);

int DGifGetImageDesc(GifFileType *this_);

int DGifGetLine(GifFileType *this_, GifPixelType *line, int len);

int DGifGetRecordType(GifFileType *this_, GifRecordType *record_type);

int DGifGetScreenDesc(GifFileType*);

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
GifFileType *DGifOpen(void *user_data, InputFunc read_fn, int *err);

/// Wrap a file as gif, returning a newly allocated decoder.
///
/// # Return
///
/// Returns NULL on error, a pointer to `GifFileType` on success. If `err` is not NULL then an
/// additional error code is written to it.
GifFileType *DGifOpenFileHandle(int fp, int *err);

/// Open a file by name, returning a pointer to its allocated decoder.
///
/// # Return
///
/// Returns NULL on error, a pointer to `GifFileType` on success. If `err` is not NULL then an
/// additional error code is written to it.
GifFileType *DGifOpenFileName(const char *gif_file_name, int *err);

void GifFreeExtensions(int *block_count, ExtensionBlock **ext_blocks);

} // extern "C"
