extern crate criterion;
extern crate gif;

use criterion::{black_box, Benchmark, Criterion};
use gif::Decoder;

fn read_image(image: &[u8]) -> Option<Vec<u8>> {
    let decoder = Decoder::new(black_box(image));
    //decoder.set_param(gif::ColorOutput::RGBA);
    let mut reader = decoder.read_info().unwrap();

    while let Some(_) = reader.next_frame_info().unwrap() {
        let mut v = vec![0; reader.buffer_size()];
        reader.fill_buffer(&mut v).unwrap();
        return Some(v);
    }
    None
}

fn read_metadata(image: &[u8]) {
    let decoder = Decoder::new(black_box(image));
    decoder.read_info().unwrap();
}

fn main() {
    let mut c = Criterion::default().configure_from_args();
    c.bench(
        "gif",
        Benchmark::new("decode a fairly compressible gif", |b| {
            b.iter(|| read_image(include_bytes!("note.gif")))
        })
        .sample_size(100),
    );
    c.bench(
        "gif",
        Benchmark::new("decode a photorealistic gif", |b| {
            b.iter(|| read_image(include_bytes!("photo.gif")))
        })
        .sample_size(20),
    );

    c.bench(
        "gif",
        Benchmark::new("extract metadata from a gif", |b| {
            b.iter(|| read_metadata(include_bytes!("note.gif")))
        }),
    );
    c.final_summary();
}
