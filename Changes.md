# v0.11

- Rename `Reader` to `Decoder`.
- Reworked `Decoder` into `DecodeOptions`.
- The decoding error is now opaque and no longer allocates a string. Adding
  more information or more error conditions is forward compatible.
- Replace the lzw decoder with `weezl`, up to +350% throughput.
- The dysfunctional C-API has been (temporarily?) removed
  - It may get reintroduced as a separate crate at some point
- Added a `std` feature. It must be active for now.
