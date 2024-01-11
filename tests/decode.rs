use gif::{Decoder, DecodeOptions, DisposalMethod, Encoder, Frame};

#[test]
fn frame_consistency_is_configurable() {
    let image = create_image_with_oob_frames();

    {
        let options = DecodeOptions::new();
        let mut data = image.as_slice();
        let mut decoder = options.clone().read_info(&mut data).unwrap();
        assert!(decoder.read_next_frame().is_ok());
        assert!(decoder.read_next_frame().is_ok());
    }

    {
        let mut options = DecodeOptions::new();
        options.check_frame_consistency(true);
        let mut data = image.as_slice();
        let mut decoder = options.clone().read_info(&mut data).unwrap();
        assert!(decoder.read_next_frame().is_ok());
        assert!(decoder.read_next_frame().is_err());
    }

    {
        let mut options = DecodeOptions::new();
        options.check_frame_consistency(false);
        let mut data = image.as_slice();
        let mut decoder = options.clone().read_info(&mut data).unwrap();
        assert!(decoder.read_next_frame().is_ok());
        assert!(decoder.read_next_frame().is_ok());
    }
}

fn create_image_with_oob_frames() -> Vec<u8> {
    let mut data = vec![];
    let mut encoder = Encoder::new(&mut data, 2, 2, &[0, 0, 0]).unwrap();

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
        buffer: vec![0, 0, 0, 0].into(),
    };

    encoder.write_frame(&frame).unwrap();
    frame.top = 1;
    frame.left = 1;
    encoder.write_frame(&frame).unwrap();

    drop(encoder);
    data
}

#[test]
fn check_for_end_code_is_configurable() {
    // In this particular image, the image data of the 62nd frame has no end code.
    let image: &[u8] = include_bytes!("samples/gifplayer-muybridge.gif");

    {
        let options = DecodeOptions::new();
        let mut decoder = options.clone().read_info(image).unwrap();
        for _ in 0..61 {
            assert!(decoder.read_next_frame().is_ok());
        }
        assert!(decoder.read_next_frame().is_ok());
    }

    {
        let mut options = DecodeOptions::new();
        options.check_lzw_end_code(true);
        let mut decoder = options.clone().read_info(image).unwrap();
        for _ in 0..61 {
            assert!(decoder.read_next_frame().is_ok());
        }
        assert!(decoder.read_next_frame().is_err());
    }
}

#[test]
fn check_rebuild_without_reencode1() {
    rebuild_without_reencode(include_bytes!("samples/gifplayer-muybridge.gif"));
}

#[test]
fn check_rebuild_without_reencode2() {
    rebuild_without_reencode(include_bytes!("samples/interlaced.gif"));
}

fn rebuild_without_reencode(image: &[u8]) {
    let mut options = DecodeOptions::new();
    options.skip_frame_decoding(true);
    let mut decoder = options.read_info(&image[..]).unwrap();

    let mut encoder = Encoder::new(Vec::new(), decoder.width(), decoder.height(), decoder.global_palette().unwrap_or_default()).unwrap();

    let mut num_frames = 0;
    while let Some(frame) = decoder.read_next_frame().unwrap() {
        num_frames += 1;
        encoder.write_lzw_pre_encoded_frame(&frame).unwrap();
    }

    let gif = encoder.into_inner().unwrap();
    let mut rebuilt = Decoder::new(&gif[..]).unwrap().into_iter();
    let mut orig = Decoder::new(&image[..]).unwrap().into_iter();

    for (orig, rebuilt) in orig.by_ref().zip(rebuilt.by_ref()) {
        num_frames -= 1;
        let orig = orig.unwrap();
        let rebuilt = rebuilt.unwrap();
        assert_eq!(orig.delay, rebuilt.delay);
        assert_eq!(orig.transparent, rebuilt.transparent);
        assert_eq!(orig.palette, rebuilt.palette);
        assert_eq!(orig.width, rebuilt.width);
        assert_eq!(orig.height, rebuilt.height);
        assert_eq!(orig.interlaced, rebuilt.interlaced);
        assert_eq!(orig.buffer, rebuilt.buffer);
    }
    assert_eq!(num_frames, 0);

    assert!(orig.next().is_none());
    assert!(orig.next().is_none());

    assert!(rebuilt.next().is_none());
    assert_eq!(0, rebuilt.into_inner().buffer().len());
}

#[test]
fn check_skip_frame_data() {
    let image: &[u8] = include_bytes!("samples/moon_impact.gif");

    let mut options = DecodeOptions::new();
    options.skip_frame_decoding(true);
    let mut decoder = options.clone().read_info(image).unwrap();

    for _ in 0..14 {
        assert!(matches!(decoder.next_frame_info().unwrap(), Some(_)));
    }

    assert!(matches!(decoder.next_frame_info(), Ok(None)));
}

#[test]
fn check_skip_frame_data_decode_frame_error() {
    let image: &[u8] = include_bytes!("samples/moon_impact.gif");

    let mut options = DecodeOptions::new();
    options.skip_frame_decoding(true);
    let mut skipping_decoder = options.read_info(image).unwrap();
    let mut normal_decoder = DecodeOptions::new().read_info(image).unwrap();

    while let Ok(Some(normal_frame)) = normal_decoder.read_next_frame() {
        let compressed_frame = skipping_decoder.read_next_frame().unwrap().unwrap();
        assert_eq!(normal_frame.width, compressed_frame.width);
        assert_eq!(normal_frame.height, compressed_frame.height);
        assert_eq!(normal_frame.delay, compressed_frame.delay);
        assert!(!normal_frame.buffer.is_empty());
        assert!(!compressed_frame.buffer.is_empty());
    }
    assert!(skipping_decoder.read_next_frame().unwrap().is_none());

}
