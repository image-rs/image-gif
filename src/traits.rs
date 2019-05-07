//! Traits used in this library
use std::io;

/// Configuration parameter trait.
///
/// Use the list of implementors to see which parameters are available for which struct.
pub trait Parameter<Object> {
    /// Result type of `set_param`.
    // TODO: Use default type () when associated type defaults get stable.
    type Result;
    /// Sets `self` as a parameter of `Object`.
    fn set_param(self, &mut Object) -> Self::Result;
}

/// Implemented for objects that have parameters.
///
/// Provides a unified `set`-method to simplify the configuration.
pub trait SetParameter: Sized {
    /// Sets `value` as a parameter of `self`.
    fn set<T: Parameter<Self>>(&mut self, value: T) -> <T as Parameter<Self>>::Result {
        value.set_param(self)
    }
}

impl<T> SetParameter for T {}

/// Writer extension to write little endian data
pub trait WriteBytesExt<T> {
    /// Writes `T` to a bytes stream. Least significant byte first.
    fn write_le(&mut self, n: T) -> io::Result<()>;

    /*
    #[inline]
    fn write_byte(&mut self, n: u8) -> io::Result<()> where Self: Write {
        self.write_all(&[n])
    }
    */
}

impl<W: io::Write + ?Sized> WriteBytesExt<u8> for W {
    #[inline]
    fn write_le(&mut self, n: u8) -> io::Result<()> {
        self.write_all(&[n])
        
    }
}

impl<W: io::Write + ?Sized> WriteBytesExt<u16> for W {
    #[inline]
    fn write_le(&mut self, n: u16) -> io::Result<()> {
        self.write_all(&[n as u8, (n>>8) as u8])
        
    }
}

impl<W: io::Write + ?Sized> WriteBytesExt<u32> for W {
    #[inline]
    fn write_le(&mut self, n: u32) -> io::Result<()> {
        self.write_le(n as u16)?;
        self.write_le((n >> 16) as u16)
        
    }
}

impl<W: io::Write + ?Sized> WriteBytesExt<u64> for W {
    #[inline]
    fn write_le(&mut self, n: u64) -> io::Result<()> {
        self.write_le(n as u32)?;
        self.write_le((n >> 32) as u32)
        
    }
}