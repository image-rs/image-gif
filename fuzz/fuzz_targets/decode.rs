#![no_main]
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let _ = try_decode(data);
});

fn try_decode(data: &[u8]) -> Result<(), gif::DecodingError> {
    let mut reader = gif::Decoder::new(data)?;

    while let Some(_) = reader.read_next_frame()? {}

    Ok(())
}
