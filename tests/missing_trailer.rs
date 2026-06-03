//! Tolerating GIFs missing the final trailer byte (`0x3B`).
#![cfg(feature = "std")]

use gif::{ColorOutput, DecodeOptions, DecodingError, DisposalMethod, Encoder, Frame};

/// A valid, complete two-frame GIF (with the `0x3B` trailer).
fn build_two_frame_gif() -> Vec<u8> {
    let mut data = Vec::new();
    {
        let mut encoder = Encoder::new(&mut data, 2, 2, &[0, 0, 0, 255, 255, 255]).unwrap();

        let mut frame = Frame {
            delay: 1,
            dispose: DisposalMethod::Any,
            transparent: None,
            needs_user_input: false,
            top: 0,
            left: 0,
            width: 2,
            height: 2,
            interlaced: false,
            palette: None,
            buffer: vec![0, 1, 1, 0].into(),
        };
        encoder.write_frame(&frame).unwrap();

        frame.buffer = vec![1, 0, 0, 1].into();
        encoder.write_frame(&frame).unwrap();
    }
    data
}

fn decode_all(bytes: &[u8]) -> Result<Vec<Vec<u8>>, DecodingError> {
    let mut options = DecodeOptions::new();
    options.set_color_output(ColorOutput::Indexed);
    let mut decoder = options.read_info(bytes)?;

    let mut frames = Vec::new();
    loop {
        match decoder.read_next_frame()? {
            Some(frame) => frames.push(frame.buffer.to_vec()),
            None => return Ok(frames),
        }
    }
}

/// Index of the `n`th image separator (`0x2C`).
fn nth_image_sep(gif: &[u8], n: usize) -> usize {
    gif.iter()
        .enumerate()
        .filter(|&(_, &b)| b == 0x2C)
        .nth(n)
        .map(|(i, _)| i)
        .expect("image separator not found")
}

fn is_truncation(err: DecodingError) -> bool {
    matches!(err, DecodingError::UnexpectedEof | DecodingError::Truncated)
}

#[test]
fn missing_trailer_decodes_all_frames_cleanly() {
    let with_trailer = build_two_frame_gif();
    assert_eq!(*with_trailer.last().unwrap(), 0x3B);

    let trailerless = &with_trailer[..with_trailer.len() - 1];
    let baseline = decode_all(&with_trailer).expect("with-trailer GIF must decode");
    let stripped = decode_all(trailerless).expect("trailerless GIF must decode cleanly");

    assert_eq!(baseline.len(), 2);
    assert_eq!(stripped, baseline, "trailerless decode must match");
}

#[test]
fn truncation_mid_frame_still_errors() {
    let full = build_two_frame_gif();
    // A few bytes into the second frame's LZW data.
    let truncated = &full[..nth_image_sep(&full, 1) + 12];
    assert!(is_truncation(
        decode_all(truncated).expect_err("mid-frame truncation must error")
    ));
}

/// The review fix: once the next introducer byte is read the decoder is
/// mid-block, so EOF is truncation — even though a frame was already decoded.
#[test]
fn introducer_then_eof_after_frame_still_errors() {
    let full = build_two_frame_gif();
    // Right after the second image separator: frame one complete, `0x2C` read,
    // its descriptor gone.
    let truncated = &full[..nth_image_sep(&full, 1) + 1];
    assert!(is_truncation(
        decode_all(truncated).expect_err("introducer-then-EOF must error")
    ));
}

#[test]
fn zero_frames_then_eof_still_errors() {
    let full = build_two_frame_gif();
    let truncated = &full[..nth_image_sep(&full, 0) + 1];
    assert!(
        decode_all(truncated).is_err(),
        "zero-frame truncation must error"
    );
}
