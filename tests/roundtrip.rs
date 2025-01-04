#![cfg(feature = "std")]

use gif::{ColorOutput, Decoder, Encoder, Frame, AnyExtension, DecodeOptions};

#[test]
fn round_trip() {
    use std::io::prelude::*;
    use std::fs::File;
    let mut data = vec![];
    File::open("tests/samples/sample_1.gif").unwrap().read_to_end(&mut data).unwrap();
    let mut decoder = Decoder::new(&*data).unwrap();
    let palette: Vec<u8> = decoder.palette().unwrap().into();
    let frame = decoder.read_next_frame().unwrap().unwrap();
    let mut data2 = vec![];
    {
        let mut encoder = Encoder::new(&mut data2, frame.width, frame.height, &palette).unwrap();
        encoder.write_frame(frame).unwrap();
    }
    assert_eq!(&data[..], &data2[..]);
}

#[test]
fn max_frame_size() {
    let mut encoder = Encoder::new(vec![], 0xFFFF, 0xFFFF, &[1, 2, 3]).unwrap();
    let mut f = Frame::default();
    f.width = 0xFFFF;
    f.height = 0xFFFF;
    f.buffer = [5][..].into();
    encoder.write_lzw_pre_encoded_frame(&f).unwrap();
    let res = encoder.into_inner().unwrap();
    let mut decoder = Decoder::new(&res[..]).unwrap();
    let f = decoder.next_frame_info().unwrap().unwrap();
    assert_eq!(f.width, 0xFFFF);
    assert_eq!(f.height, 0xFFFF);
    assert_eq!(decoder.line_length(), 0xFFFF);

    #[cfg(target_pointer_width = "64")]
    assert_eq!(decoder.buffer_size(), 0xFFFF * 0xFFFF);
}

#[test]
fn encode_roundtrip() {
    const ORIGINAL: &[u8] = include_bytes!("samples/2x2.gif");
    round_trip_from_image(ORIGINAL);
}

#[test]
fn encode_roundtrip2() {
    const ORIGINAL: &[u8] = include_bytes!("samples/interlaced.gif");
    round_trip_from_image(ORIGINAL);
}

fn round_trip_from_image(original: &[u8]) {
    let (width, height, global_palette, repeat);
    let frames: Vec<_> = {
        let mut decoder = Decoder::new(original).unwrap();
        width = decoder.width();
        height = decoder.height();
        repeat = decoder.repeat();
        global_palette = decoder
            .global_palette()
            .unwrap_or_default()
            .to_vec();
        core::iter::from_fn(move || {
            decoder.read_next_frame().unwrap().cloned()
        }).collect()
    };

    let mut encoder = Encoder::new(vec![], width, height, &global_palette).unwrap();
    encoder.set_repeat(repeat).unwrap();
    encoder.write_raw_extension(AnyExtension(gif::Extension::Comment as _), &[b"hello"]).unwrap();
    for frame in &frames {
        encoder.write_frame(frame).unwrap();
    }
    let buffer = encoder.into_inner().unwrap();

    {
        let mut decoder = Decoder::new(&buffer[..]).expect("Invalid info encoded");
        assert_eq!(decoder.width(), width);
        assert_eq!(decoder.height(), height);
        assert_eq!(decoder.repeat(), repeat);
        assert_eq!(global_palette, decoder.global_palette().unwrap_or_default());
        let new_frames: Vec<_> = core::iter::from_fn(|| {
            decoder.read_next_frame().unwrap().cloned()
        }).collect();
        assert_eq!(new_frames.len(), frames.len(), "Diverging number of frames");
        for (new, reference) in new_frames.iter().zip(&frames) {
            assert_eq!(new.delay, reference.delay);
            assert_eq!(new.dispose, reference.dispose);
            assert_eq!(new.transparent, reference.transparent);
            assert_eq!(new.needs_user_input, reference.needs_user_input);
            assert_eq!(new.top, reference.top);
            assert_eq!(new.left, reference.left);
            assert_eq!(new.width, reference.width);
            assert_eq!(new.height, reference.height);
            assert_eq!(new.interlaced, reference.interlaced);
            assert_eq!(new.palette, reference.palette);
            assert_eq!(new.buffer, reference.buffer);
        }
        assert_eq!(0, decoder.into_inner().buffer().len());
    }
}

#[test]
#[cfg(feature = "color_quant")]
fn encode_roundtrip_few_colors() {
    const WIDTH: u16 = 128;
    const HEIGHT: u16 = 128;

    // Build an image with a single red pixel, that NeuQuant won't
    // sample, in order to check that we do appropriatelyq specialise the
    // few-colors case.
    let mut pixels: Vec<u8> = vec![255; WIDTH as usize * HEIGHT as usize * 4];
    // Top-left pixel is always sampled, so use the second pixel.
    pixels[5] = 0;
    pixels[6] = 0;
    // Set speed to 30 to handily avoid sampling that one pixel.
    //
    // We clone "pixels", since the parameter is replaced with a
    // paletted version, and later we want to compare the output with
    // the original RGBA image.
    let mut frame = Frame::from_rgba_speed(WIDTH, HEIGHT, &mut pixels.clone(), 30);

    let mut buffer = vec![];
    {
        let mut encoder = Encoder::new(&mut buffer, WIDTH, HEIGHT, &[]).unwrap();
        encoder.write_frame(&frame).unwrap();

        frame.make_lzw_pre_encoded();
        encoder.write_lzw_pre_encoded_frame(&frame).unwrap();
    }

    {
        let mut decoder = {
            let mut builder = Decoder::<&[u8]>::build();
            builder.set_color_output(ColorOutput::RGBA);
            builder.read_info(&buffer[..]).expect("Invalid info encoded")
        };

        // Only check key fields, assuming "round_trip_from_image"
        // covers the rest. We are primarily concerned with quantisation.
        assert_eq!(decoder.width(), WIDTH);
        assert_eq!(decoder.height(), HEIGHT);
        let new_frames: Vec<_> = core::iter::from_fn(move || {
            decoder.read_next_frame().unwrap().cloned()
        }).collect();
        assert_eq!(new_frames.len(), 2, "Diverging number of frames");
        // NB: reference.buffer can't be used as it contains the palette version.
        assert_eq!(new_frames[0].buffer, pixels);
        assert_eq!(new_frames[1].buffer, pixels);
    }
}

#[test]
fn palette_sizes() {
    let global_pal = (0..=255u8).flat_map(|i| [i, i/2, i.wrapping_add(13)]).collect::<Vec<_>>();
    let local_pal = (0..=255u8).flat_map(|i| [i^0x55, i, i.wrapping_add(7)]).collect::<Vec<_>>();

    for size in 1..=256 {
        let global = &global_pal[..size * 3];
        let local = &local_pal[..size * 3];

        let mut encoder = Encoder::new(vec![], 1, 1, global).unwrap();
        let mut f = Frame::default();
        f.width = 1;
        f.height = 1;
        f.buffer = [1][..].into();
        encoder.write_frame(&f).unwrap();

        let mut f = Frame::default();
        f.width = 1;
        f.height = 1;
        f.buffer = [1][..].into();
        f.palette = Some(local.to_vec());
        encoder.write_frame(&f).unwrap();
        let gif = encoder.into_inner().unwrap();
        let gif = &mut gif.as_slice();

        let mut d = DecodeOptions::new().read_info(gif).unwrap();
        let (decoded_global_pal, padding) = d.global_palette().unwrap().split_at(global.len());
        assert_eq!(global, decoded_global_pal);
        assert_eq!(padding.len(), 3 * (size.max(2).next_power_of_two() - size));
        assert!(padding.iter().all(|&b| b == 0));

        assert!(d.read_next_frame().unwrap().unwrap().palette.is_none());
        let decoded_local_pal = d.read_next_frame().unwrap().unwrap().palette.as_deref().unwrap();
        let (decoded_local_pal, padding) = decoded_local_pal.split_at(local.len());
        assert_eq!(local, decoded_local_pal);
        assert_eq!(padding.len(), 3 * (size.max(2).next_power_of_two() - size));
        assert!(padding.iter().all(|&b| b == 0));

        assert!(d.read_next_frame().unwrap().is_none());
    }
}

#[test]
fn palette_fail() {
    let mut encoder = Encoder::new(vec![], 0xFFFF, 0xFFFF, &[]).unwrap();
    let mut f = Frame::default();
    f.width = 1;
    f.height = 1;
    f.buffer = [1][..].into();
    assert!(matches!(encoder.write_frame(&f), Err(gif::EncodingError::Format(gif::EncodingFormatError::MissingColorPalette))));
}
