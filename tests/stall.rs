#![cfg(feature = "std")]

use std::{fs, sync::mpsc, thread, time::Duration, io};

#[test]
fn try_decode_crash_regression() {
    let files = fs::read_dir(concat!(env!("CARGO_MANIFEST_DIR"), "/tests/stall")).unwrap();

    for entry in files {
        let entry = entry.unwrap();
        if let Some(ext) = entry.path().extension() {
            assert_eq!(ext.to_str(), Some("gif"), "Unexpected file {} in crashtests, should end with .gif", entry.path().display());
        } else {
            panic!("Unexpected file {} in crashtests, should end with .gif", entry.path().display());
        }

        let file_data = fs::read(entry.path()).unwrap();
        decode_on_timer(file_data);
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

#[test]
fn test_truncated_file() {
    let data = include_bytes!("../tests/samples/anim-gr.gif");
    for len in 0..data.len() - 1 {
        let truncated = &data[..len];
        // it's expected to fail often, but should not stall or panic
        if let Ok(d) = gif::DecodeOptions::new().read_info(truncated) {
            let _ = d.into_iter().count();
        }
    }
}

#[test]
fn one_byte_at_a_time() {
    let r = OneByte {
        data: include_bytes!("../tests/samples/moon_impact.gif"),
    };
    let frames = gif::DecodeOptions::new().read_info(r).unwrap()
        .into_iter().enumerate().map(|(n, f)| {
            f.expect(&n.to_string())
        }).count();
    assert_eq!(frames, 14);
}

struct OneByte<'a> {
    data: &'a [u8],
}

impl io::BufRead for OneByte<'_> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        Ok(&self.data[..self.data.len().min(1)])
    }
    fn consume(&mut self, n: usize) {
        debug_assert!(n <= 1);
        self.data = &self.data[n..];
    }
}

impl io::Read for OneByte<'_> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let n = self.data.len().min(buf.len()).min(1);
        buf[..n].copy_from_slice(&self.data[..n]);
        self.data = &self.data[n..];
        Ok(n)
    }
}
