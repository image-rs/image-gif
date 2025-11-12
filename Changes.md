# v0.14.0

- `EncodingError` and `DecodingError` are now `#[non_exhaustive]`
- Modified several error paths to return a new variant of `EncodingError` instead of boxing them into
  an `io::Error`, several return `Result` types are adjusted accordingly.
- The `Decoded` enum no longer communicates data from decoded sub-blocks such as the repetition count.
  It now only contains the meta information on framing and extension data. This ensures we are yielding
  less often for performance.
- `last_ext` was renamed to `last_extension_sub_block` for clarity.
- The `Decoder` will now collect XMP and ICC metadata subblocks, making them available after decoding.


# v0.13.3

- Fix interpretation of LZW stream when multiple intermediate reset codes are used.
- Allow extension blocks of unknown type with the `allow_unknown_blocks` option.

# v0.13.2

- Simplified internal decoder size

# v0.13.1

Bugfixes:
 - Fixed writing palettes with non-power-of-two sizes

# v0.13.0

Features:
 - Added `Iterator` interface for the `Decoder`
 - Added reading number of loop repetitions `Decoder::repeat`.
 - Added `skip_frame_decoding` option to read `Frame.buffer` as LZW data.
   It works together with `write_lzw_pre_encoded_frame` for quick rewriting of GIF files.
 - Added support pre-allocated `Vec`s in `from_palette_pixels`
 - Added ability to recover the `io::Read`er after decoding.
 - Added support for decompressing `Frame.buffer` with LZW data,
   which enables fully parallel GIF re-encoding (see examples/parallel.rs),

Optimization:
 - Less buffering, copying, and lower peak memory usage.

Other changes:
 - Removed `unsafe {}` code.
 - Enforced memory limit also on metadata extension blocks, and added out-of-memory checks where possible.
 - `EncodingFormatError` enum is public.
 - Removed defunct `skip_extensions`
 - Added validation of frame dimensions. The buffer must be large enough for all pixels,
   and if the width or height is 0, the buffer must be empty.

# v0.12.0

Features:
- Add compression of pre-compressed frame data, via `Encoder::write_lzw_pre_encoded_frame`.
- The `color_quant` dependency is now optional. Turning it off disables some
  interfaces that would internally build quantization tables. The generic
  implementation of creating such tables can be prohibitively costly compared
  to specialized algorithms in some use cases.

Optimization:
- Avoid some allocations in by replacing `flat_map` argument with arrays

# v0.11.4

Bufixes:
- Fix decoding confusing superfluous image data from previous frames with
  current frame data.
- Bump minimum required version of `weezl`.

Features:
- Add `Encoder::{get_ref, get_mut, into_inner}` to access underlying stream.

# v0.11.3

Bugfixes:
- Fix panic while decoding some images, has no precise cause in the file.
- Warn about `set_extensions` being unimplemented...

Features:
- Added `StreamingDecoder::version` to query the precise version of the
  standard used for encoding the file. This is merely a hint.
- Added `DecodeOptions::allow_unknown_blocks` to skip over unknown or
  unspecified block kinds.

Optimization:
- `Frame::from_rgba` now recognizes when less than 256 colors are being used,
  dynamically skipping the quantization phase.
- Encoding image chunks is faster and simpler 


# v0.11.2

- Fix panic when LZW code size is invalid
- Added option to omit check for lzw end code

# v0.11.1

- Frames out-of-bounds of the screen descriptor are again accepted by default.
- Added `DecodeOptions::check_frame_consistency` to turn this validation on.

# v0.11

- Rename `Reader` to `Decoder`.
- Reworked `Decoder` into `DecodeOptions`.
- The decoding error is now opaque and no longer allocates a string. Adding
  more information or more error conditions is forward compatible.
- Replace the lzw decoder with `weezl`, up to +350% throughput.
- The dysfunctional C-API has been (temporarily?) removed
  - It may get reintroduced as a separate crate at some point
- Added a `std` feature. It must be active for now.
