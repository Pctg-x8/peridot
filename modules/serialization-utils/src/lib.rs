use std::io::prelude::{BufRead, Write};
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
use std::str::from_utf8;

use core::pin::Pin;
use futures_io::AsyncBufRead;
use peridot_native_io::{BufferedRandomBlobReader, RandomReadBlobAsync};
use pinned_futures_helper::read_exact_async_pinned;

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
    pub const MAX_BYTE_LENGTH: usize = 32usize.div_ceil(7); // 32bit全部が7bitずつ入ったときが最大長

    pub fn from_bytes_head(bytes: &[u8]) -> (Self, usize) {
        let (mut v, mut shifts, mut read_bytes) = (0u32, 0u8, 0usize);
        for b in bytes {
            read_bytes += 1;
            v |= ((b & 0x7f) as u32) << shifts;
            shifts += 7;

            if b & 0x80 == 0 || shifts >= 32 {
                break;
            }
        }

        (Self(v), read_bytes)
    }

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

    pub async fn read_async(mut reader: Pin<&mut (impl AsyncBufRead + ?Sized)>) -> IOResult<Self> {
        let (mut v, mut shifts) = (0u32, 0usize);

        loop {
            let done = core::future::poll_fn(|cx| {
                let mut available = core::task::ready!(reader.as_mut().poll_fill_buf(cx))?;

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

                reader.as_mut().consume(consumed);
                core::task::Poll::Ready(Ok::<_, IOError>(done))
            })
            .await?;

            if done {
                return Ok(Self(v));
            }
        }
    }

    pub fn read_with_byte_count(reader: &mut (impl BufRead + ?Sized)) -> IOResult<(Self, usize)> {
        let (mut v, mut shifts, mut byte_count) = (0u32, 0usize, 0usize);
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
                    byte_count += 1;
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
                return Ok((VariableUInt(v), byte_count));
            }
        }
    }

    pub fn read_at(
        reader: &(impl peridot_native_io::RandomReadBlob + ?Sized),
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        // 読めるだけ読む（ケツの方だとMAX_BYTE_LENGTH未満しかない場合は全然ある）
        let mut buf = Vec::with_capacity(Self::MAX_BYTE_LENGTH);
        read_at_try_fill_capacity(reader, pos, &mut buf)?;

        let (v, consumed) = Self::from_bytes_head(&buf);
        assert!(consumed <= buf.len(), "too long VariableUInt");
        Ok((v, consumed))
    }

    pub async fn read_at_async(
        reader: &(impl peridot_native_io::RandomReadBlobAsync + ?Sized),
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        // 読めるだけ読む（ケツの方だとMAX_BYTE_LENGTH未満しかない場合は全然ある）
        let mut buf = Vec::with_capacity(Self::MAX_BYTE_LENGTH);
        read_at_try_fill_capacity_async(reader, pos, &mut buf).await?;

        let (v, consumed) = Self::from_bytes_head(&buf);
        assert!(consumed <= buf.len(), "too long VariableUInt");
        Ok((v, consumed))
    }

    pub async fn read_at_buffered_async(
        reader: &mut BufferedRandomBlobReader<impl RandomReadBlobAsync>,
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        let (mut v, mut shifts, mut reads) = (0u32, 0usize, 0usize);
        loop {
            let b = reader.read_byte_at_async(pos + reads as u64).await?;
            v |= ((b & 0x7f) as u32) << shifts;
            shifts += 7;
            reads += 1;
            if b & 0x80 == 0 {
                break Ok((Self(v), reads));
            }
        }
    }
}

/// octet variadic unsigned long integer
#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VariableULong(pub u64);
impl VariableULong {
    pub const MAX_BYTE_LENGTH: usize = 64usize.div_ceil(7); // 64bit全部が7bitずつ入ったときが最大長

    pub fn to_bytes(&self) -> Vec<u8> {
        if self.0 == 0 {
            return vec![0];
        }

        // 一番右の1の位置を7で切り上げ
        let size = (64 - self.0.leading_zeros() as usize + 6) / 7;
        let mut buf = Vec::with_capacity(size);
        let mut left = self.0;
        while left > 0 {
            let b = (left & 0x7f) as u8;
            left >>= 7;
            buf.push(if left != 0 { 0x80 | b } else { b });
        }

        buf
    }

    pub fn from_bytes_head(bytes: &[u8]) -> (Self, usize) {
        let (mut v, mut shifts, mut read_bytes) = (0u64, 0u8, 0usize);
        for b in bytes {
            read_bytes += 1;
            v |= ((b & 0x7f) as u64) << shifts;
            shifts += 7;

            if b & 0x80 == 0 || shifts >= 64 {
                break;
            }
        }

        (Self(v), read_bytes)
    }

    pub fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        let buf = self.to_bytes();
        writer.write_all(&buf)?;

        Ok(buf.len())
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + ?Sized + Unpin),
    ) -> IOResult<usize> {
        let buf = self.to_bytes();
        async_std::io::WriteExt::write_all(writer, &buf).await?;

        Ok(buf.len())
    }

    pub fn read(reader: &mut (impl BufRead + ?Sized)) -> IOResult<Self> {
        let (mut v, mut shifts) = (0u64, 0usize);

        loop {
            let available = match reader.fill_buf() {
                Ok(v) => v,
                Err(e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => return Err(e),
            };

            let (mut consumed, mut done) = (0, false);
            for b in available {
                v |= ((b & 0x7f) as u64) << shifts;
                shifts += 7;
                consumed += 1;

                if b & 0x80 == 0 {
                    done = true;
                    break;
                }
            }

            reader.consume(consumed);
            if done {
                return Ok(Self(v));
            }
        }
    }

    pub async fn read_async(mut reader: Pin<&mut (impl AsyncBufRead + ?Sized)>) -> IOResult<Self> {
        let (mut v, mut shifts) = (0u64, 0usize);

        loop {
            let done = core::future::poll_fn(|cx| {
                let available = core::task::ready!(reader.as_mut().poll_fill_buf(cx))?;

                let (mut consumed, mut done) = (0, false);
                for b in available {
                    v |= ((b & 0x7f) as u64) << shifts;
                    shifts += 7;
                    consumed += 1;

                    if b & 0x80 == 0 {
                        done = true;
                        break;
                    }
                }

                reader.as_mut().consume(consumed);
                core::task::Poll::Ready(Ok::<_, IOError>(done))
            })
            .await?;

            if done {
                return Ok(Self(v));
            }
        }
    }

    pub fn read_at(
        reader: &(impl peridot_native_io::RandomReadBlob + ?Sized),
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        // 読めるだけ読む（ケツの方だとMAX_BYTE_LENGTH未満しかない場合は全然ある）
        let mut buf = Vec::with_capacity(Self::MAX_BYTE_LENGTH);
        read_at_try_fill_capacity(reader, pos, &mut buf)?;

        let (v, consumed) = Self::from_bytes_head(&buf);
        assert!(consumed <= buf.len(), "too long VariableULong");
        Ok((v, consumed))
    }

    pub async fn read_at_async(
        reader: &(impl peridot_native_io::RandomReadBlobAsync + ?Sized),
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        // 読めるだけ読む（ケツの方だとMAX_BYTE_LENGTH未満しかない場合は全然ある）
        let mut buf = Vec::with_capacity(Self::MAX_BYTE_LENGTH);
        read_at_try_fill_capacity_async(reader, pos, &mut buf).await?;

        let (v, consumed) = Self::from_bytes_head(&buf);
        assert!(consumed <= buf.len(), "too long VariableULong");
        Ok((v, consumed))
    }

    pub async fn read_at_buffered_async(
        reader: &mut BufferedRandomBlobReader<impl RandomReadBlobAsync>,
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        let (mut v, mut shifts, mut reads) = (0u64, 0usize, 0usize);
        loop {
            let b = reader.read_byte_at_async(pos + reads as u64).await?;
            v |= ((b & 0x7f) as u64) << shifts;
            shifts += 7;
            reads += 1;
            if b & 0x80 == 0 {
                break Ok((Self(v), reads));
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

    pub async fn read_async(mut reader: Pin<&mut (impl AsyncBufRead + ?Sized)>) -> IOResult<Self> {
        let VariableUInt(byte_length) = VariableUInt::read_async(reader.as_mut()).await?;
        let mut bytes = Vec::with_capacity(byte_length as _);
        read_exact_async_pinned(reader, bytes.spare_capacity_mut()).await?;
        unsafe {
            bytes.set_len(byte_length as _);
        }

        match from_utf8(&bytes) {
            Ok(x) => Ok(Self(x.to_owned())),
            Err(e) => Err(IOError::other(e)),
        }
    }

    pub fn read_at(
        reader: &(impl peridot_native_io::RandomReadBlob + ?Sized),
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        let (VariableUInt(byte_length), str_head) = VariableUInt::read_at(reader, pos)?;
        let mut bytes = Vec::with_capacity(byte_length as _);
        reader.read_exact(pos + str_head as u64, bytes.spare_capacity_mut())?;
        unsafe {
            bytes.set_len(byte_length as _);
        }

        match from_utf8(&bytes[..]) {
            Ok(x) => Ok((Self(x.to_owned()), str_head + byte_length as usize)),
            Err(e) => Err(IOError::other(e)),
        }
    }

    pub async fn read_at_async(
        reader: &(impl peridot_native_io::RandomReadBlobAsync + ?Sized),
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        let (VariableUInt(byte_length), str_head) =
            VariableUInt::read_at_async(reader, pos).await?;
        let mut bytes = Vec::with_capacity(byte_length as _);
        reader
            .read_exact_async(pos + str_head as u64, bytes.spare_capacity_mut())
            .await?;
        unsafe {
            bytes.set_len(byte_length as _);
        }

        match from_utf8(&bytes[..]) {
            Ok(x) => Ok((Self(x.to_owned()), str_head + byte_length as usize)),
            Err(e) => Err(IOError::other(e)),
        }
    }

    pub async fn read_at_buffered_async(
        reader: &mut BufferedRandomBlobReader<impl RandomReadBlobAsync>,
        pos: u64,
    ) -> IOResult<(Self, usize)> {
        let (VariableUInt(byte_length), str_head) =
            VariableUInt::read_at_buffered_async(reader, pos).await?;
        let mut bytes = Vec::with_capacity(byte_length as _);
        reader
            .read_exact_at_async(pos + str_head as u64, bytes.spare_capacity_mut())
            .await?;
        unsafe {
            bytes.set_len(byte_length as _);
        }

        match from_utf8(&bytes[..]) {
            Ok(x) => Ok((Self(x.to_owned()), str_head + byte_length as usize)),
            Err(e) => Err(IOError::other(e)),
        }
    }
}
impl<'s> PascalStr<'s> {
    pub fn from_bytes_head(bytes: &'s [u8]) -> Result<(Self, usize), core::str::Utf8Error> {
        let (VariableUInt(bytelength), bytelength_len) = VariableUInt::from_bytes_head(bytes);
        let s = from_utf8(&bytes[bytelength_len..bytelength_len + bytelength as usize])?;

        Ok((PascalStr(s), bytelength_len + bytelength as usize))
    }

    /// # Safety
    /// The input bytes must be valid-formed UTF-8 sequence.
    pub unsafe fn from_bytes_head_unchecked(bytes: &'s [u8]) -> (Self, usize) {
        let (VariableUInt(bytelength), bytelength_len) = VariableUInt::from_bytes_head(bytes);
        let s = unsafe {
            core::str::from_utf8_unchecked(
                &bytes[bytelength_len..bytelength_len + bytelength as usize],
            )
        };

        (PascalStr(s), bytelength_len + bytelength as usize)
    }

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

/// `buf.capacity()`以下で読めるだけ読む
fn read_at_try_fill_capacity(
    reader: &(impl peridot_native_io::RandomReadBlob + ?Sized),
    pos: u64,
    buf: &mut Vec<u8>,
) -> IOResult<()> {
    let mut rpos = pos;
    while buf.len() < buf.capacity() {
        match reader.read(rpos, buf.spare_capacity_mut()) {
            Ok(r) => {
                unsafe {
                    buf.set_len(buf.len() + r);
                }
                rpos += r as u64;
            }
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                break;
            }
            Err(e) => return Err(e),
        }
    }

    Ok(())
}

/// `buf.capacity()`以下で読めるだけ読む（非同期）
async fn read_at_try_fill_capacity_async(
    reader: &(impl peridot_native_io::RandomReadBlobAsync + ?Sized),
    pos: u64,
    buf: &mut Vec<u8>,
) -> IOResult<()> {
    let mut rpos = pos;
    while buf.len() < buf.capacity() {
        match reader.read_async(rpos, buf.spare_capacity_mut()).await {
            Ok(r) => {
                unsafe {
                    buf.set_len(buf.len() + r);
                }
                rpos += r as u64;
            }
            Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                break;
            }
            Err(e) => return Err(e),
        }
    }

    Ok(())
}
