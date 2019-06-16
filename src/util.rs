//! Utility functions

/// Copies data from `src` to `dst`, but does not guarantee it will fill `dst`
///
/// Unlike slice::copy_from_slice(), this function does not guarantee
/// that `dst` will be completely overwritten:
/// `src` is allowed to be smaller than `dst`, and in that case
/// only `src.len()` bytes will be copied.
#[inline]
pub fn copy_memory(src: &[u8], dst: &mut [u8]) {
    let destination = &mut dst[..src.len()];
    destination.copy_from_slice(src);
}
