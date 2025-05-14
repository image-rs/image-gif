//! Traits used in this library

use core::error::Error;

/// Intermediate for `std::io::BufRead` which is suitable for `no_std`.
pub trait BufRead {
    /// An [`Error`] returned when attempting to call [`fill_buf`](BufRead::fill_buf).
    type Error: Error + Send + Sync + 'static;

    /// Fill the internal buffer and return it as a slice, or an [`Error`](BufRead::Error)
    fn fill_buf(&mut self) -> Result<&[u8], Self::Error>;

    /// Mark that `amt` bytes have been consumed from a slice previously provided by [`fill_buf`](BufRead::fill_buf)
    fn consume(&mut self, amt: usize);
}

impl<R: BufRead + ?Sized> BufRead for &mut R {
    type Error = R::Error;

    #[inline]
    fn fill_buf(&mut self) -> Result<&[u8], Self::Error> {
        <R as BufRead>::fill_buf(self)
    }

    #[inline]
    fn consume(&mut self, amt: usize) {
        <R as BufRead>::consume(self, amt)
    }
}

impl<R: BufRead + ?Sized> BufRead for alloc::boxed::Box<R> {
    type Error = R::Error;

    #[inline]
    fn fill_buf(&mut self) -> Result<&[u8], Self::Error> {
        <R as BufRead>::fill_buf(self)
    }

    #[inline]
    fn consume(&mut self, amt: usize) {
        <R as BufRead>::consume(self, amt)
    }
}

/// Intermediate for `std::io::Write` which is suitable for `no_std`.
pub trait Write {
    /// An [`Error`] returned when attempting to call [`write_all`](Write::write_all).
    type Error: Error + Send + Sync + 'static;

    /// Write all the data in the provided `buf`, or return an [`Error`](Write::Error)
    fn write_all(&mut self, buf: &[u8]) -> Result<(), Self::Error>;
}

impl<W: Write + ?Sized> Write for &mut W {
    type Error = W::Error;

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> Result<(), Self::Error> {
        <W as Write>::write_all(self, buf)
    }
}

impl<W: Write + ?Sized> Write for alloc::boxed::Box<W> {
    type Error = W::Error;

    #[inline]
    fn write_all(&mut self, buf: &[u8]) -> Result<(), Self::Error> {
        <W as Write>::write_all(self, buf)
    }
}

/// Writer extension to write little endian data
pub(crate) trait WriteBytesExt<T>: Write {
    /// Writes `T` to a bytes stream. Least significant byte first.
    fn write_le(&mut self, n: T) -> Result<(), Self::Error>;

    /*
    #[inline]
    fn write_byte(&mut self, n: u8) -> io::Result<()> where Self: Write {
        self.write_all(&[n])
    }
    */
}

impl<W: Write + ?Sized> WriteBytesExt<u8> for W {
    #[inline(always)]
    fn write_le(&mut self, n: u8) -> Result<(), Self::Error> {
        self.write_all(&[n])
    }
}

impl<W: Write + ?Sized> WriteBytesExt<u16> for W {
    #[inline]
    fn write_le(&mut self, n: u16) -> Result<(), Self::Error> {
        self.write_all(&n.to_le_bytes())
    }
}

impl<W: Write + ?Sized> WriteBytesExt<u32> for W {
    #[inline]
    fn write_le(&mut self, n: u32) -> Result<(), Self::Error> {
        self.write_all(&n.to_le_bytes())
    }
}

impl<W: Write + ?Sized> WriteBytesExt<u64> for W {
    #[inline]
    fn write_le(&mut self, n: u64) -> Result<(), Self::Error> {
        self.write_all(&n.to_le_bytes())
    }
}

#[cfg(feature = "std")]
pub(crate) mod std_impls {
    use super::{BufRead, Write};

    /// Wrapper for a [`std::io::BufRead`] reader compatible with [`BufRead`].
    pub struct IoBufReader<R: std::io::BufRead + ?Sized>(pub R);

    impl<R: std::io::BufRead + ?Sized> BufRead for IoBufReader<R> {
        type Error = std::io::Error;

        #[inline]
        fn fill_buf(&mut self) -> Result<&[u8], Self::Error> {
            <R as std::io::BufRead>::fill_buf(&mut self.0)
        }

        #[inline]
        fn consume(&mut self, amt: usize) {
            <R as std::io::BufRead>::consume(&mut self.0, amt)
        }
    }

    /// Wrapper for an [`std::io::Write`] writer compatible with [`Write`].
    pub struct IoWriter<W: std::io::Write + ?Sized>(pub W);

    impl<W: std::io::Write + ?Sized> Write for IoWriter<W> {
        type Error = std::io::Error;

        #[inline]
        fn write_all(&mut self, buf: &[u8]) -> Result<(), Self::Error> {
            <W as std::io::Write>::write_all(&mut self.0, buf)
        }
    }
}
