use std::{env, fs, process};

fn main() {
    let file = env::args().nth(1).unwrap_or_else(|| explain_usage());
    let file = fs::File::open(file).expect("failed to open input file");
    let mut reader = {
        let mut options = gif::DecodeOptions::new();
        options.allow_unknown_blocks(true);
        options.read_info(file).unwrap()
    };

    loop {
        let frame = match reader.read_next_frame() {
            Ok(Some(frame)) => frame,
            Ok(None) => break,
            Err(error) => {
                println!("Error: {error:?}");
                break;
            }
        };

        println!(
            " Frame:\n  \
                 delay: {:?}\n  \
                 canvas: {}x{}+{}+{}\n  \
                 dispose: {:?}\n  \
                 needs_input: {:?}",
            frame.delay,
            frame.width,
            frame.height,
            frame.left,
            frame.top,
            frame.dispose,
            frame.needs_user_input
        );

        reader.icc_profile().map(|icc| {
            println!("  ICC profile: {} bytes", icc.len());
        });

        reader.xmp_metadata().map(|xmp| {
            println!("  XMP metadata: {} bytes", xmp.len());
        });

        reader.photoshop_irb().map(|irb| {
            println!("  Photoshop IRB: {} bytes", irb.len());
            iterate_irb(irb);
        });
    }

    let _ = reader.next_frame_info();

    reader.icc_profile().map(|icc| {
        println!("  ICC profile: {} bytes", icc.len());
    });
}

fn explain_usage() -> ! {
    println!("Print information on the frames of a gif.\n\nUsage: check <file>");
    process::exit(1)
}

fn iterate_irb(mut data: &[u8]) {
    while data.len() >= 12 {
        let signature = &data[0..4];
        let key = u16::from_be_bytes(data[4..6].as_chunks::<2>().0[0]);

        let identifier_len = data[6];
        let identifier = &data[7..][..usize::from(identifier_len)];

        let padded_len = (usize::from(identifier_len) + 1).div_ceil(2) * 2;
        let length = u32::from_be_bytes(data[6..][padded_len..].as_chunks::<4>().0[0]) as usize;

        if signature != b"8BIM" {
            println!("   Invalid IRB signature: {:?}", signature);
            return;
        }

        if data.len() < 12 + length {
            println!("   Truncated IRB data");
            return;
        }

        println!(
            "   IRB block: key={:x}/{:?} length={}",
            key,
            str::from_utf8(identifier).unwrap_or("<invalid utf8>"),
            length
        );

        if key == 0x3f8 {
            println!("    (contains transfer functions)");
            println!("    {:?}", &data[6 + padded_len + 4..][..length]);
        }

        let total_length = 6 + padded_len + 4 + length + (length % 2); // padded to even length
        data = &data[total_length..];
    }
}
