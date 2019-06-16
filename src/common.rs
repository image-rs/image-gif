//! Common common used both by decoder and encoder
extern crate color_quant;

use std::borrow::Cow;

/// Disposal method
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum DisposalMethod {
    /// StreamingDecoder is not required to take any action.
    Any = 0,
    /// Do not dispose.
    Keep = 1,
    /// Restore to background color.
    Background = 2,
    /// Restore to previous.
    Previous = 3,
}

impl DisposalMethod {
    /// Converts `u8` to `Option<Self>`
    pub fn from_u8(n: u8) -> Option<DisposalMethod> {
        match n {
            0 => Some(DisposalMethod::Any),
            1 => Some(DisposalMethod::Keep),
            2 => Some(DisposalMethod::Background),
            3 => Some(DisposalMethod::Previous),
            _ => None
        }
    }
}

/// Known GIF block types
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Block {
    /// Image block.
    Image = 0x2C,
    /// Extension block.
    Extension = 0x21,
    /// Image trailer.
    Trailer = 0x3B
}

impl Block {
    /// Converts `u8` to `Option<Self>`
    pub fn from_u8(n: u8) -> Option<Block> {
        match n {
            0x2C => Some(Block::Image),
            0x21 => Some(Block::Extension),
            0x3B => Some(Block::Trailer),
            _ => None
        }
    }
}


/// Known GIF extensions
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(u8)]
pub enum Extension {
    /// Text extension.
    Text = 0x01,
    /// Control extension.
    Control = 0xF9,
    /// Comment extension.
    Comment = 0xFE,
    /// Application extension.
    Application = 0xFF
}

impl Extension {
    /// Converts `u8` to `Option<Self>`
    pub fn from_u8(n: u8) -> Option<Extension> {
        match n {
            0x01 => Some(Extension::Text),
            0xF9 => Some(Extension::Control),
            0xFE => Some(Extension::Comment),
            0xFF => Some(Extension::Application),
            _ => None
        }
    }
}

/// A GIF frame
#[derive(Debug, Clone)]
pub struct Frame<'a> {
    /// Frame delay in units of 10 ms.
    pub delay: u16,
    /// Disposal method.
    pub dispose: DisposalMethod,
    /// Transparent index (if available).
    pub transparent: Option<u8>,
    /// True if the frame needs user input to be displayed.
    pub needs_user_input: bool,
    /// Offset from the top border of the canvas.
    pub top: u16,
    /// Offset from the left border of the canvas.
    pub left: u16,
    /// Width of the frame.
    pub width: u16,
    /// Height of the frame.
    pub height: u16,
    /// True if the image is interlaced.
    pub interlaced: bool,
    /// Frame local color palette if available.
    pub palette: Option<Vec<u8>>,
    /// Buffer containing the image data.
    /// Only indices unless configured differently.
    pub buffer: Cow<'a, [u8]>
}

impl<'a> Default for Frame<'a> {
    fn default() -> Frame<'a> {
        Frame {
            delay: 0,
            dispose: DisposalMethod::Keep,
            transparent: None,
            needs_user_input: false,
            top: 0,
            left: 0,
            width: 0,
            height: 0,
            interlaced: false,
            palette: None,
            buffer: Cow::Borrowed(&[])
        }
    }
}

impl Frame<'static> {
    /// Creates a frame from pixels in RGBA format.
    /// *Note: This method is not optimized for speed.*
    ///
    /// # Panics:
    /// *   If the length of pixels does not equal `width * height * 4`.
    pub fn from_rgba(width: u16, height: u16, pixels: &mut [u8]) -> Frame<'static> {
        Frame::from_rgba_speed(width, height, pixels, 1)
    }

    /// Creates a frame from pixels in RGBA format.
    /// `speed` is a value in the range [1, 30].
    /// The higher the value the faster it runs at the cost of image quality.
    /// A `speed` of 10 is a good compromise between speed and quality.
    ///
    /// # Panics:
    /// *   If the length of pixels does not equal `width * height * 4`.
    /// *   If `speed < 1` or `speed > 30`
    pub fn from_rgba_speed(width: u16, height: u16, pixels: &mut [u8], speed: i32) -> Frame<'static> {
        assert_eq!(width as usize * height as usize * 4, pixels.len(), "Too much or too little pixel data for the given width and height to create a GIF Frame");
        assert!(speed >= 1 && speed <= 30, "speed needs to be in the range [1, 30]");
        let mut frame = Frame::default();
        let mut transparent = None;
        for pix in pixels.chunks_mut(4) {
            if pix[3] != 0 {
                pix[3] = 0xFF;
            } else {
                transparent = Some([pix[0], pix[1], pix[2], pix[3]])
            }
        }
        frame.width = width;
        frame.height = height;
        let nq = color_quant::NeuQuant::new(speed, 256, pixels);
        frame.buffer = Cow::Owned(pixels.chunks(4).map(|pix| nq.index_of(pix) as u8).collect());
        frame.palette = Some(nq.color_map_rgb());
        frame.transparent = if let Some(t) = transparent {
            Some(nq.index_of(&t) as u8)
        } else {
            None
        };

        frame
    }

    /// Creates a frame from a palette and indexed pixels.
    ///
    /// # Panics:
    /// *   If the length of pixels does not equal `width * height`.
    /// *   If the length of palette > `256 * 3`.
    pub fn from_palette_pixels(width: u16, height: u16, pixels: &[u8], palette: &[u8], transparent: Option<u8>) -> Frame<'static> {
        assert_eq!(width as usize * height as usize, pixels.len(), "Too many or too little pixels for the given width and height to create a GIF Frame");
        assert!(palette.len() <= 256*3, "Too many palette values to create a GIF Frame");
        let mut frame = Frame::default();

        frame.width = width;
        frame.height = height;

        frame.buffer = Cow::Owned(pixels.to_vec());
        frame.palette = Some(palette.to_vec());

        frame.transparent = transparent;

        frame
    }

    /// Creates a frame from indexed pixels in the global palette.
    ///
    /// # Panics:
    /// *   If the length of pixels does not equal `width * height`.
    pub fn from_indexed_pixels(width: u16, height: u16, pixels: &[u8], transparent: Option<u8>) -> Frame<'static> {
        assert_eq!(width as usize * height as usize, pixels.len(), "Too many or too little pixels for the given width and height to create a GIF Frame");
        let mut frame = Frame::default();

        frame.width = width;
        frame.height = height;

        frame.buffer = Cow::Owned(pixels.to_vec());
        frame.palette = None;

        frame.transparent = transparent;

        frame
    }

    /// Creates a frame from pixels in RGB format.
    /// *Note: This method is not optimized for speed.*
    ///
    /// # Panics:
    /// *   If the length of pixels does not equal `width * height * 3`.
    pub fn from_rgb(width: u16, height: u16, pixels: &[u8]) -> Frame<'static> {
        Frame::from_rgb_speed(width, height, pixels, 1)
    }

    /// Creates a frame from pixels in RGB format.
    /// `speed` is a value in the range [1, 30].
    /// The higher the value the faster it runs at the cost of image quality.
    /// A `speed` of 10 is a good compromise between speed and quality.
    ///
    /// # Panics:
    /// *   If the length of pixels does not equal `width * height * 3`.
    /// *   If `speed < 1` or `speed > 30`
    pub fn from_rgb_speed(width: u16, height: u16, pixels: &[u8], speed: i32) -> Frame<'static> {
        assert_eq!(width as usize * height as usize * 3, pixels.len(), "Too much or too little pixel data for the given width and height to create a GIF Frame");
        let mut vec: Vec<u8> = Vec::with_capacity(pixels.len() + width as usize * height as usize);
        for v in pixels.chunks(3) {
            vec.extend([v[0], v[1], v[2], 0xFF].iter().cloned())
        }
        Frame::from_rgba_speed(width, height, &mut vec, speed)
    }
}
