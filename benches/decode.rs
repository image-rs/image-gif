use std::fs;
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use gif::Decoder;

fn bench_tiny(b: &mut Criterion) {
    let data = fs::read("tests/samples/sample_1.gif").unwrap();
    let mut g = b.benchmark_group("tiny");

    let mut decoder = Decoder::new(&*data).read_info().unwrap();
    let total_size = decoder.read_next_frame().unwrap().unwrap().buffer.len() as u64;
    g.throughput(Throughput::Bytes(total_size));

    g.bench_with_input(BenchmarkId::new("sample_1", total_size), data.as_slice(),
        |b, i| b.iter(|| {
            let mut decoder = Decoder::new(i).read_info().unwrap();
            black_box(decoder.read_next_frame().unwrap().unwrap());
        }));
}

fn bench_big(b: &mut Criterion) {
    let data = fs::read("tests/sample_big.gif").unwrap();
    let mut g = b.benchmark_group("big");

    let mut decoder = Decoder::new(&*data).read_info().unwrap();
    let total_size = decoder.read_next_frame().unwrap().unwrap().buffer.len() as u64;
    g.throughput(Throughput::Bytes(total_size));

    g.bench_with_input(BenchmarkId::new("sample_1", total_size), data.as_slice(),
        |b, i| b.iter(|| {
            let mut decoder = Decoder::new(i).read_info().unwrap();
            black_box(decoder.read_next_frame().unwrap().unwrap());
        }));
}

criterion_group!(benches, bench_tiny, bench_big);
criterion_main!(benches);
