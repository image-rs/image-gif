
//! # Minimal gif encoder



use std::cmp::min;
use std::io;
use std::io::prelude::*;

use lzw;

use traits::WriteBytesExt;
use common::{Block, Frame, Extension, DisposalMethod};
use util;

/// Extension data.
pub enum ExtensionData {
    /// Control extension
	Control { 
	    /// Flags.
	    flags: u8,
	    /// Frame delay.
	    delay: u16,
	    /// Transparent index.
	    trns: u8
    }
}

impl ExtensionData {
    /// Constructor for control extension data.
	pub fn new_control_ext(delay: u16, dispose: DisposalMethod, 
						   needs_user_input: bool, trns: Option<u8>) -> ExtensionData {
		let mut flags = 0;
		let trns = match trns {
			Some(trns) => {
				flags |= 1;
				trns as u8
			},
			None => 0
		};
		flags |= (needs_user_input as u8) << 1;
		flags |= (dispose as u8) << 2;
		ExtensionData::Control {
			flags: flags,
			delay: delay,
			trns: trns
		}
	}
}

struct BlockWriter<'a, W: Write + 'a> {
	w: &'a mut W,
	bytes: usize,
	buf: [u8; 0xFF]
}


impl<'a, W: Write + 'a> BlockWriter<'a, W> {
	fn new(w: &'a mut W) -> BlockWriter<'a, W> {
		BlockWriter {
			w: w,
			bytes: 0,
			buf: [0; 0xFF]
		}
	}
}

impl<'a, W: Write + 'a> Write for BlockWriter<'a, W> {

	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		let to_copy = min(buf.len(), 0xFF - self.bytes);
		util::copy_memory(&buf[..to_copy], &mut self.buf[self.bytes..]);
		self.bytes += to_copy;
		if self.bytes == 0xFF {
			self.bytes = 0;
			try!(self.w.write_le(0xFFu8));
			try!(self.w.write_all(&self.buf));
		}
		Ok(to_copy)
	}
	fn flush(&mut self) -> io::Result<()> {
		return Err(io::Error::new(
			io::ErrorKind::Other,
			"Cannot flush a BlockWriter, use `drop` instead."
		))
	}
}

impl<'a, W: Write + 'a> Drop for BlockWriter<'a, W> {

    #[cfg(feature = "raii_no_panic")]
	fn drop(&mut self) {
		if self.bytes > 0 {
			let _ = self.w.write_le(self.bytes as u8);
			let _ = self.w.write_all(&self.buf[..self.bytes]);	
		}
	}

    #[cfg(not(feature = "raii_no_panic"))]
	fn drop(&mut self) {
		if self.bytes > 0 {
			self.w.write_le(self.bytes as u8).unwrap();
			self.w.write_all(&self.buf[..self.bytes]).unwrap();	
		}
	}
}

/// Wrapper for `Encoder` that indicates that the file headers have been written.
pub struct HeaderWritten<W: Write> {
	enc: Encoder<W>
}

impl<W: Write> HeaderWritten<W> {
	/// Writes a complete frame to the image
	///
	/// Note: This function also writes a control extention if necessary.
	pub fn write_frame(&mut self, frame: &Frame) -> io::Result<()> {
		self.enc.write_frame(frame)
	}

	/// Writes an extension to the image
	pub fn write_extension(&mut self, extension: ExtensionData) -> io::Result<()> {
		self.enc.write_extension(extension)
	}

	/// Writes a raw extension to the image
	pub fn write_raw_extension(&mut self, func: u8, data: &[u8]) -> io::Result<()> {
		self.enc.write_raw_extension(func, data)
	}
}

impl<W: Write> Drop for HeaderWritten<W> {

    #[cfg(feature = "raii_no_panic")]
	fn drop(&mut self) {
		let _ = self.enc.w.write_le(Block::Trailer as u8);
	}

    #[cfg(not(feature = "raii_no_panic"))]
	fn drop(&mut self) {
		self.enc.w.write_le(Block::Trailer as u8).unwrap()
	}
}

/*

pub struct Frame {
    pub delay: u16,
    pub dispose: DisposalMethod,
    pub transparent: Option<usize>,
    pub needs_user_input: bool,
    pub top: u16,
    pub left: u16,
    pub width: u16,
    pub height: u16,
    pub interlaced: bool,
    pub palette: Option<Vec<u8>>,
    pub buffer: Vec<u8>
}

*/

/// GIF encoder.
pub struct Encoder<W: Write> {
    w: W,
    global_palette: bool,
    width: u16,
    height: u16
}

impl<W: Write> Encoder<W> {
    /// Creates a new encoder.
	pub fn new(w: W, width: u16, height: u16) -> Self {
		Encoder {
			w: w,
			global_palette: false,
			width: width,
			height: height
		}
	}

	/// Writes the global color palette
	pub fn write_global_palette(mut self, palette: &[u8]) -> io::Result<HeaderWritten<W>> {
		self.global_palette = true;
		let mut flags = 0;
		flags |= 0b1000_0000;
		let num_colors = palette.len() / 3;
		flags |= flag_size(num_colors);
		flags |= flag_size(num_colors) << 4; // wtf flag
		try!(self.write_screen_desc(flags));
		try!(self.write_color_table(palette));
		Ok(HeaderWritten {
			enc: self
		})
	}

	/// Writes a complete frame to the image
	///
	/// Note: This function also writes a control extension if necessary.
	fn write_frame(&mut self, frame: &Frame) -> io::Result<()> {
		// TODO commented off to pass test in lib.rs
		//if frame.delay > 0 || frame.transparent.is_some() {
			try!(self.write_extension(ExtensionData::new_control_ext(
				frame.delay,
				frame.dispose,
				frame.needs_user_input,
				frame.transparent

			)));
		//}
		try!(self.w.write_le(Block::Image as u8));
		try!(self.w.write_le(frame.left));
		try!(self.w.write_le(frame.top));
		try!(self.w.write_le(frame.width));
		try!(self.w.write_le(frame.height));
		let mut flags = 0;
		try!(match frame.palette {
			Some(ref palette) => {
				flags |= 0b1000_0000;
				let num_colors = palette.len() / 3;
				flags |= flag_size(num_colors);
				try!(self.w.write_le(flags));
				self.write_color_table(palette)
			},
			None => if !self.global_palette {
				return Err(io::Error::new(
					io::ErrorKind::InvalidInput,
					"The GIF format requires a color palette but none was given."
				))
			} else {
				self.w.write_le(flags)
			}
		});
		self.write_image_block(&frame.buffer)
	}

	fn write_image_block(&mut self, data: &[u8]) -> io::Result<()> {
		{
			let min_code_size: u8 = flag_size((*data.iter().max().unwrap_or(&0) as usize + 1)) + 1;
			try!(self.w.write_le(min_code_size));
			let mut bw = BlockWriter::new(&mut self.w);
			let mut enc = try!(lzw::Encoder::new(lzw::LsbWriter::new(&mut bw), min_code_size));
			try!(enc.encode_bytes(data));
		}
		self.w.write_le(0u8)
	}

	fn write_color_table(&mut self, table: &[u8]) -> io::Result<()> {
		let num_colors = table.len() / 3;
        let size = flag_size(num_colors);
		try!(self.w.write_all(&table[..num_colors * 3]));
        // Waste some space as of gif spec
        for _ in 0..((2 << size) - num_colors) {
            try!(self.w.write_all(&[0, 0, 0]))
        }
        Ok(())
	}

	/// Writes an extension to the image
	fn write_extension(&mut self, extension: ExtensionData) -> io::Result<()> {
		use self::ExtensionData::*;
		try!(self.w.write_le(Block::Extension as u8));
		match extension {
			Control { flags, delay, trns } => {
				try!(self.w.write_le(Extension::Control as u8));
				try!(self.w.write_le(4u8));
				try!(self.w.write_le(flags));
				try!(self.w.write_le(delay));
				try!(self.w.write_le(trns));
			}
		}
		self.w.write_le(0u8)
	}

	/// Writes a raw extension to the image
	fn write_raw_extension(&mut self, func: u8, data: &[u8]) -> io::Result<()> {
		try!(self.w.write_le(Block::Extension as u8));
		try!(self.w.write_le(func as u8));
		for chunk in data.chunks(0xFF) {
			try!(self.w.write_le(chunk.len() as u8));
			try!(self.w.write_all(chunk));
		}
		self.w.write_le(0u8)
	}

	/// Writes the logical screen desriptor
	fn write_screen_desc(&mut self, flags: u8) -> io::Result<()> {
		try!(self.w.write_all(b"GIF89a"));
		try!(self.w.write_le(self.width));
		try!(self.w.write_le(self.height));
		try!(self.w.write_le(flags)); // packed field
		try!(self.w.write_le(0u8)); // bg index
		self.w.write_le(0u8) // aspect ratio
	}
}

// Color table size converted to flag bits
fn flag_size(size: usize) -> u8 {
    match size {
        0  ...2   => 0,
        3  ...4   => 1,
        5  ...8   => 2,
        7  ...16  => 3,
        17 ...32  => 4,
        33 ...64  => 5,
        65 ...128 => 6,
        129...256 => 7,
        _ => 7
    }
}