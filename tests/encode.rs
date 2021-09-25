use std::path::Path;
use std::fs::File;

fn encode_frame() {
    // Get pixel data from some source
    let mut pixels: Vec<u8> = vec![0; 30_000];
    // Create frame from data
    let frame = gif::Frame::from_rgb(100, 100, &mut *pixels);
    let target = Path::new(env!("CARGO_TARGET_TMPDIR"))
        .join("target/indexed_color.gif");
    let mut image = File::create(target).unwrap();
    let mut encoder = gif::Encoder::new(&mut image, frame.width, frame.height, &[])
        .expect("this to be valid image parameter");
    // Write frame to file
    encoder.write_frame(&frame)
        .expect("this to be a valid frame");
}
