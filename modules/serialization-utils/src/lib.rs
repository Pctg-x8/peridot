use std::io::prelude::{BufRead, Write};
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
use std::str::from_utf8;

/// u32 to break apart into bytes
pub struct UIntFragmentIterator(Option<u32>);
impl From<u32> for UIntFragmentIterator {
    fn from(v: u32) -> Self {
        UIntFragmentIterator(Some(v))
    }
}
impl Iterator for UIntFragmentIterator {
    type Item = u8;

    #[inline]
    fn next(&mut self) -> Option<u8> {
        self.0.map(|v| {
            let (n7, nr) = ((v & 0x7f) as u8, v >> 7);
            let rv = n7 | if nr != 0 { 0x80 } else { 0 };
            self.0 = if nr != 0 { Some(nr) } else { None };
            rv
        })
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        // 一番右の1の位置を7で切り上げ
        let s = (32 - self.0.unwrap_or(0).leading_zeros() as usize + 6) / 7;

        (s, Some(s))
    }
}

/// octet variadic unsigned integer
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableUInt(pub u32);
impl VariableUInt {
    pub fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        let write_bytes = UIntFragmentIterator::from(self.0).collect::<Vec<_>>();
        writer.write_all(&write_bytes)?;

        Ok(write_bytes.len())
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + ?Sized + Unpin),
    ) -> IOResult<usize> {
        let write_bytes = UIntFragmentIterator::from(self.0).collect::<Vec<_>>();
        async_std::io::WriteExt::write_all(writer, &write_bytes).await?;

        Ok(write_bytes.len())
    }

    pub fn read(reader: &mut (impl BufRead + ?Sized)) -> IOResult<Self> {
        let (mut v, mut shifts) = (0u32, 0usize);
        loop {
            let (consumed, done) = {
                let mut available = match reader.fill_buf() {
                    Ok(v) => v,
                    Err(e) => {
                        if e.kind() == ErrorKind::Interrupted {
                            continue;
                        } else {
                            return Err(e);
                        }
                    }
                };
                let (mut consumed, mut done) = (0, false);
                while !available.is_empty() {
                    v |= ((available[0] & 0x7f) as u32) << shifts;
                    shifts += 7;
                    consumed += 1;
                    if (available[0] & 0x80) == 0 {
                        done = true;
                        break;
                    }
                    available = &available[1..];
                }
                (consumed, done)
            };
            reader.consume(consumed);
            if done {
                return Ok(VariableUInt(v));
            }
        }
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn read_async(
        reader: &mut (impl async_std::io::BufRead + ?Sized + Unpin),
    ) -> IOResult<Self> {
        let (mut v, mut shifts) = (0u32, 0usize);

        loop {
            let done = std::future::poll_fn(|cx| {
                let available = match std::task::ready!(async_std::io::BufRead::poll_fill_buf(
                    std::pin::Pin::new(reader),
                    cx
                )) {
                    Ok(v) => v,
                    Err(e) if e.kind() == ErrorKind::Interrupted => {
                        return std::task::Poll::Ready(Ok(false))
                    }
                    Err(e) => return std::task::Poll::Ready(Err(e)),
                };
                let (mut consumed, mut done) = (0, false);
                while consumed < available.len() {
                    v |= ((available[consumed] & 0x7f) as u32) << shifts;
                    shifts += 7;
                    consumed += 1;

                    if (available[consumed - 1] & 0x80) == 0 {
                        // last byte
                        done = true;
                        break;
                    }
                }

                async_std::io::BufRead::consume(std::pin::Pin::new(reader), consumed);
                std::task::Poll::Ready(Ok(done))
            })
            .await?;

            if done {
                break Ok(VariableUInt(v));
            }
        }
    }
}

/// a utf-8 string representation leading its byte length as `VariableUInt`.
pub struct PascalString(pub String);
pub struct PascalStr<'s>(pub &'s str);
impl PascalString {
    pub fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        PascalStr(&self.0).write(writer)
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + ?Sized + Unpin),
    ) -> IOResult<usize> {
        PascalStr(&self.0).write_async(writer).await
    }

    pub fn read(reader: &mut (impl BufRead + ?Sized)) -> IOResult<Self> {
        let VariableUInt(bytelength) = VariableUInt::read(reader)?;
        let mut bytes = Vec::<u8>::with_capacity(bytelength as _);
        reader.read_exact(unsafe {
            core::mem::transmute::<&mut [core::mem::MaybeUninit<u8>], &mut [u8]>(
                bytes.spare_capacity_mut(),
            )
        })?;
        unsafe {
            bytes.set_len(bytelength as _);
        }

        from_utf8(&bytes[..])
            .map(|s| Self(s.to_owned()))
            .map_err(IOError::other)
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn read_async(
        reader: &mut (impl async_std::io::BufRead + ?Sized + Unpin),
    ) -> IOResult<Self> {
        let VariableUInt(byte_length) = VariableUInt::read_async(reader).await?;

        let mut bytes = Vec::<u8>::with_capacity(byte_length as _);
        async_std::io::ReadExt::read_exact(reader, unsafe {
            core::mem::transmute::<&mut [core::mem::MaybeUninit<u8>], &mut [u8]>(
                bytes.spare_capacity_mut(),
            )
        })
        .await?;
        unsafe {
            bytes.set_len(bytes.capacity());
        }

        from_utf8(&bytes[..])
            .map(|s| Self(s.to_owned()))
            .map_err(IOError::other)
    }
}
impl<'s> PascalStr<'s> {
    pub fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        VariableUInt(self.0.len() as _)
            .write(writer)
            .and_then(|wl| {
                writer
                    .write_all(self.0.as_bytes())
                    .map(move |_| wl + self.0.len())
            })
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + ?Sized + Unpin),
    ) -> IOResult<usize> {
        let len_bytes = VariableUInt(self.0.len() as _).write_async(writer).await?;
        async_std::io::WriteExt::write_all(writer, self.0.as_bytes()).await?;

        Ok(len_bytes + self.0.len())
    }
}
