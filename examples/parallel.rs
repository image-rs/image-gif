//! Reencodes GIF in parallel

use gif::streaming_decoder::FrameDecoder;
use gif::DecodeOptions;
use rayon::iter::ParallelBridge;
use rayon::iter::ParallelIterator;
use std::env;
use std::fs::File;
use std::io::BufWriter;
use std::path::PathBuf;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input_path = PathBuf::from(
        env::args_os()
            .nth(1)
            .ok_or("Specify a GIF path as the first argument")?,
    );

    let input = std::fs::read(&input_path)?;
    let input_size = input.len();

    let start = std::time::Instant::now();

    let mut options = DecodeOptions::new();
    options.skip_frame_decoding(true); // This gives LZW frames

    let decoder = options.read_info(std::io::Cursor::new(input))?;
    let repeat = decoder.repeat();
    let screen_width = decoder.width();
    let screen_height = decoder.height();
    let global_pal = decoder.global_palette().unwrap_or_default().to_vec();

    let output_file = format!(
        "{}-reencoded.gif",
        input_path.file_stem().unwrap().to_str().unwrap()
    );
    let output = BufWriter::new(File::create(output_file)?);
    let mut encoder = gif::Encoder::new(output, screen_width, screen_height, &global_pal)?;
    encoder.set_repeat(repeat)?;

    let (send, recv) = std::sync::mpsc::channel();

    decoder
        .into_iter()
        .enumerate()
        .par_bridge()
        .try_for_each(move |(frame_number, frame)| {
            let mut frame = frame?;
            FrameDecoder::new(DecodeOptions::new())
                .decode_lzw_encoded_frame(&mut frame)
                .unwrap();
            // frame is now pixels
            frame.make_lzw_pre_encoded();
            // frame is now LZW again, re-encoded
            send.send((frame_number, frame)).unwrap();
            Ok::<_, gif::DecodingError>(())
        })?;

    // Decoding and encoding can happen in parallel, but writing to the GIF file is sequential
    let mut next_frame_number = 0;
    let mut frames_to_process = Vec::new();
    for (frame_number, frame) in recv {
        // frames can arrive in any order, since they're processed in parallel,
        // so they have to be stored in a queue
        frames_to_process.push((frame_number, frame));
        while let Some(index) = frames_to_process
            .iter()
            .position(|&(num, _)| num == next_frame_number)
        {
            let frame = frames_to_process.remove(index).1;
            encoder.write_lzw_pre_encoded_frame(&frame)?;
            next_frame_number += 1;
        }
    }
    encoder.into_inner()?;

    let seconds = start.elapsed().as_millis() as f64 / 1000.;
    let rate = (input_size / 1024 / 1024) as f64 / seconds;

    eprintln!(
        "Finished in {seconds:0.2}s, {rate:0.0}MiB/s {}",
        if cfg!(debug_assertions) {
            ". Run with --release for more speed."
        } else {
            ""
        }
    );
    Ok(())
}
