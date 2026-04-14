#![cfg(feature = "std")]
use gif::{ColorOutput, DecodeOptions, DecodingError};
use std::fs::File;
use std::io::{BufWriter, Read};
use std::path::Path;

fn test_truncation(gif_path: &str, png_path: &str, truncate_len: usize) {
    let mut file = File::open(gif_path).expect("Failed to open GIF");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read GIF");

    data.truncate(truncate_len);

    let mut options = DecodeOptions::new();
    options.set_color_output(ColorOutput::RGBA);

    let mut decoder = options.read_info(&data[..]).expect("Failed to read info");

    let mut hit_truncated = false;
    let mut buf = Vec::new();

    while let Ok(Some(_)) = decoder.next_frame_info() {
        buf.resize(decoder.buffer_size(), 0);
        match decoder.read_into_buffer(&mut buf) {
            Ok(()) => {
                println!("Decoded a frame!");
            }
            Err(DecodingError::Truncated) => {
                println!("Hit Truncated error!");
                hit_truncated = true;
                break;
            }
            Err(e) => panic!("Unexpected error: {:?}", e),
        }
    }

    assert!(hit_truncated);

    // Save PNG if it hits truncated, for verification!
    if !Path::new(png_path).exists() {
        let width = decoder.width() as u32;
        let height = decoder.height() as u32;
        let file = File::create(png_path).expect("Failed to create PNG");
        let ref mut w = BufWriter::new(file);
        let mut encoder = png::Encoder::new(w, width, height);
        encoder.set_color(png::ColorType::Rgba);
        encoder.set_depth(png::BitDepth::Eight);
        let mut writer = encoder.write_header().expect("Failed to write header");
        writer
            .write_image_data(&buf)
            .expect("Failed to write image data");
        println!("Generated expected PNG: {}", png_path);
    } else {
        println!("Comparing against existing PNG: {}", png_path);
        // Read expected PNG and compare
        let file = File::open(png_path).expect("Failed to open PNG");
        let decoder = png::Decoder::new(std::io::BufReader::new(file));
        let mut reader = decoder.read_info().expect("Failed to read PNG info");
        let mut expected_buf = vec![
            0;
            reader
                .output_buffer_size()
                .expect("Failed to get output buffer size")
        ];
        reader
            .next_frame(&mut expected_buf)
            .expect("Failed to read PNG frame");
        assert_eq!(buf, expected_buf);
    }
}

#[test]
fn test_truncated_non_interlaced() {
    test_truncation(
        "tests/samples/moon_impact.gif",
        "tests/truncated/moon_impact-truncated.png",
        5000,
    );
}

#[test]
fn test_truncated_interlaced() {
    test_truncation(
        "tests/samples/interlaced.gif",
        "tests/truncated/interlaced-truncated.png",
        5000,
    );
}
