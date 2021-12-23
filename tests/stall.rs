use std::{fs, sync::mpsc, thread, time::Duration};

#[test]
fn try_decode_crash_regression() {
    let files = fs::read_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/stall")).unwrap();

    for entry in files {
        let entry = entry.unwrap();
        if let Some(ext) = entry.path().extension() {
            if ext.to_str() != Some("gif") {
                panic!("Unexpected file {} in crashtests, should end with .gif", entry.path().display());
            }
        } else {
            panic!("Unexpected file {} in crashtests, should end with .gif", entry.path().display());
        }

        let file_data = fs::read(entry.path()).unwrap();
        let _ = decode_on_timer(file_data);
    }
}

fn decode_on_timer(data: Vec<u8>) {
    let (send, recv) = mpsc::channel();

    thread::spawn(move || {
        let result = decode(&data);
        send.send(result).expect("still waiting");
    });

    let _ = recv.recv_timeout(Duration::from_secs(1))
        .expect("any result");
}

fn decode(data: &[u8]) -> Result<(), gif::DecodingError> {
    let mut options = gif::DecodeOptions::new();
    options.set_color_output(gif::ColorOutput::RGBA);

    let mut decoder = options.read_info(data)?;
    while let Some(_frame) = decoder.read_next_frame()? {}

    Ok(())
}
