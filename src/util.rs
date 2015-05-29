//! Utility functions

// Copies data from `src` to `dst`
//
// Panics if the length of `dst` is less than the length of `src`.
// NOTE: this is a copy-paste of the unstable function `std::slice::bytes::copy_memory`.
#[inline]
pub fn copy_memory(src: &[u8], dst: &mut [u8]) {
    let len_src = src.len();
    assert!(dst.len() >= len_src);
    // `dst` is unaliasable, so we know statically it doesn't overlap
    // with `src`.
    unsafe {
        ::std::ptr::copy_nonoverlapping(src.as_ptr(),
                                      dst.as_mut_ptr(),
                                      len_src);
    }
}