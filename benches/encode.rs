use std::time::Duration;

use criterion::measurement::Measurement;
use criterion::{BenchmarkGroup, BenchmarkId, Criterion};
use gif::{DisposalMethod, Encoder, Frame, ExtensionData, Repeat};

fn main() {
    #[derive(Clone, Copy)]
    struct BenchDef {
        seed: usize,
        frame_size: u16,
        frame_count: usize,
        sample_size: usize,
    }

    fn run_bench_def<M: Measurement>(group: &mut BenchmarkGroup<M>, def: BenchDef) {
        let frames = std::iter::successors(Some(def.seed), |seed| Some(hash(*seed, 114514)))
            .map(|seed| random_frame(seed, def.frame_size))
            .take(def.frame_count)
            .collect::<Vec<_>>();

        group
            .sample_size(def.sample_size)
            .measurement_time(Duration::from_secs(15))
            .bench_with_input(
                BenchmarkId::from_parameter(format!("{}*{}", def.frame_size, def.frame_count)),
                frames.as_slice(),
                |b, input| {
                    b.iter(|| encode_image(input, def.frame_size))
                }
            );
    }

    let mut c = Criterion::default().configure_from_args();
    let mut group = c.benchmark_group("encode");

    run_bench_def(&mut group, BenchDef {
        seed: 42,
        frame_size: 256,
        frame_count: 32,
        sample_size: 50
    });

    run_bench_def(&mut group, BenchDef {
        seed: 666,
        frame_size: 128,
        frame_count: 128,
        sample_size: 60
    });

    group.finish();
    c.final_summary();
}

fn encode_image(frames: &[Frame<'_>], size: u16) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let mut buf = Vec::with_capacity(64 * 1024 * 1024);
    let mut enc = Encoder::new(&mut buf, size, size, &[])?;
    enc.write_extension(ExtensionData::Repetitions(Repeat::Infinite))?;
    for frame in frames.iter() {
        enc.write_frame(frame)?;
    }
    drop(enc);
    Ok(buf)
}

fn random_frame(mut seed: usize, size: u16) -> Frame<'static> {
    let mut pal = Vec::with_capacity(256);
    seed = random_fill(seed, &mut pal);

    let mut indices = Vec::with_capacity(size as usize * size as usize);
    seed = random_fill(seed, &mut indices);

    let delay = (seed % 300) as u16;
    Frame {
        delay,
        dispose: DisposalMethod::Background,
        transparent: None,
        needs_user_input: false,
        top: 0,
        left: 0,
        width: size,
        height: size,
        interlaced: false,
        palette: Some(pal),
        buffer: indices.into(),
    }
}

fn random_fill(mut seed: usize, vec: &mut Vec<u8>) -> usize {
    for i in 0..(vec.capacity() - vec.len()) {
        seed = hash(seed, i);
        let bytes = seed.to_ne_bytes();
        vec.push(IntoIterator::into_iter(bytes).reduce(|a, i| a ^ i).unwrap());
    }
    seed
}

// rustc_hash
fn hash(hash: usize, i: usize) -> usize {
    use std::ops::BitXor;
    const K: usize = 0x517cc1b727220a95;
    hash.rotate_left(5).bitxor(i).wrapping_mul(K)
}
