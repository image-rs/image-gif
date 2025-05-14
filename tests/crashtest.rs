#![cfg(feature = "std")]

use gif::DecodeOptions;
use std::{fs, io};

#[test]
fn try_decode_crash_regression() {
    let files = fs::read_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/crashtest")).unwrap();
    let options = DecodeOptions::new();

    for entry in files {
        let entry = entry.unwrap();
        if let Some(ext) = entry.path().extension() {
            assert_eq!(
                ext.to_str(),
                Some("gif"),
                "Unexpected file {} in crashtests, should end with .gif",
                entry.path().display()
            );
        } else {
            panic!(
                "Unexpected file {} in crashtests, should end with .gif",
                entry.path().display()
            );
        }

        let file_data = fs::read(entry.path()).unwrap();
        let _ = try_decode_file(&options, file_data);
    }
}

fn try_decode_file(options: &DecodeOptions, data: Vec<u8>) -> Result<(), gif::DecodingError> {
    let mut reader = options.clone().read_info(io::Cursor::new(data))?;
    while reader.read_next_frame()?.is_some() {}
    Ok(())
}
