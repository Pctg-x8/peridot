use core::ops::Range;
use std::io::{IoSliceMut, Read};

pub struct BufferedRandomBlobReader<R> {
    inner: R,
    buf: Vec<u8>,
    buffered_range: Range<u64>,
}
impl<R> BufferedRandomBlobReader<R> {
    const BUFFER_SIZE: usize = 4096;

    pub fn new(inner: R) -> Self {
        Self {
            inner,
            buf: Vec::with_capacity(Self::BUFFER_SIZE),
            buffered_range: 0..0,
        }
    }

    #[inline(always)]
    pub const fn inner_ref(&self) -> &R {
        &self.inner
    }
}
impl<R> BufferedRandomBlobReader<R>
where
    R: crate::RandomReadBlob,
{
    pub fn hint_buffering(&mut self, start: u64) -> std::io::Result<()> {
        if !self.buffered_range.is_empty() && self.buffered_range.contains(&start) {
            // already buffered
            return Ok(());
        }

        self.fill_buf_at(start)?;
        Ok(())
    }

    fn fill_buf_at(&mut self, start: u64) -> std::io::Result<usize> {
        let bufread = self.inner.read(start, self.buf.spare_capacity_mut())?;
        self.buffered_range = start..start + bufread as u64;
        Ok(bufread)
    }

    pub fn read_at(
        &mut self,
        pos: u64,
        buf: &mut [core::mem::MaybeUninit<u8>],
    ) -> std::io::Result<usize> {
        self.hint_buffering(pos)?;

        let offs = pos - self.buffered_range.start;
        let fill_size = ((self.buffered_range.end - pos) as usize).min(buf.len());
        unsafe {
            core::ptr::copy_nonoverlapping(
                self.buf.spare_capacity_mut().as_ptr().add(offs as usize),
                buf.as_mut_ptr(),
                fill_size,
            );
        }

        Ok(fill_size)
    }

    pub fn readv_at(&mut self, pos: u64, iovecs: &mut [IoSliceMut]) -> std::io::Result<usize> {
        self.hint_buffering(pos)?;

        let offs = pos - self.buffered_range.start;
        let max_size = self.buffered_range.end - pos;
        unsafe {
            core::slice::from_raw_parts(self.buf.as_ptr().add(offs as _), max_size as _)
                .read_vectored(iovecs)
        }
    }

    pub fn read_byte_at(&mut self, pos: u64) -> std::io::Result<u8> {
        self.hint_buffering(pos)?;
        Ok(self.buf[(pos - self.buffered_range.start) as usize])
    }

    pub fn read_exact_at(
        &mut self,
        pos: u64,
        buf: &mut [core::mem::MaybeUninit<u8>],
    ) -> std::io::Result<()> {
        if self.buffered_range.contains(&pos)
            && self.buffered_range.contains(&(pos + buf.len() as u64))
        {
            // perfectly contained
            let offs = pos - self.buffered_range.start;
            unsafe {
                core::ptr::copy_nonoverlapping(
                    self.buf.spare_capacity_mut().as_ptr().add(offs as usize),
                    buf.as_mut_ptr(),
                    buf.len(),
                );
            }

            return Ok(());
        }

        // simple read from blob
        self.inner.read_exact(pos, buf)
    }

    pub fn readv_all_at(
        &mut self,
        mut pos: u64,
        mut iovecs: &mut [IoSliceMut],
    ) -> std::io::Result<()> {
        IoSliceMut::advance_slices(&mut iovecs, 0);
        if iovecs.is_empty() {
            // no iovecs or empty
            return Ok(());
        }

        if self.buffered_range.contains(&pos)
            && self
                .buffered_range
                .contains(&(pos + iovecs[0].len() as u64))
        {
            // can fill from buffer at least one slice
            let offs = pos - self.buffered_range.start;
            let max_size = self.buffered_range.end - pos;
            let filled = unsafe {
                core::slice::from_raw_parts(self.buf.as_ptr().add(offs as _), max_size as _)
                    .read_vectored(iovecs)?
            };
            IoSliceMut::advance_slices(&mut iovecs, filled);
            pos += filled as u64;
        }

        self.inner.readv_all(pos, iovecs)
    }
}
impl<R> BufferedRandomBlobReader<R>
where
    R: crate::RandomReadBlobAsync,
{
    pub async fn hint_buffering_async(&mut self, start: u64) -> std::io::Result<()> {
        if !self.buffered_range.is_empty() && self.buffered_range.contains(&start) {
            // already buffered
            return Ok(());
        }

        self.fill_buf_at_async(start).await?;
        Ok(())
    }

    async fn fill_buf_at_async(&mut self, start: u64) -> std::io::Result<usize> {
        let bufread = self
            .inner
            .read_async(start, self.buf.spare_capacity_mut())
            .await?;
        self.buffered_range = start..start + bufread as u64;
        Ok(bufread)
    }

    pub async fn read_at_async(
        &mut self,
        pos: u64,
        buf: &mut [core::mem::MaybeUninit<u8>],
    ) -> std::io::Result<usize> {
        self.hint_buffering_async(pos).await?;

        let offs = pos - self.buffered_range.start;
        let fill_size = ((self.buffered_range.end - pos) as usize).min(buf.len());
        unsafe {
            core::ptr::copy_nonoverlapping(
                self.buf.spare_capacity_mut().as_ptr().add(offs as usize),
                buf.as_mut_ptr(),
                fill_size,
            );
        }

        Ok(fill_size)
    }

    pub async fn readv_at_async(
        &mut self,
        pos: u64,
        iovecs: &mut [IoSliceMut<'_>],
    ) -> std::io::Result<usize> {
        self.hint_buffering_async(pos).await?;

        let offs = pos - self.buffered_range.start;
        let max_size = self.buffered_range.end - pos;
        unsafe {
            core::slice::from_raw_parts(self.buf.as_ptr().add(offs as _), max_size as _)
                .read_vectored(iovecs)
        }
    }

    pub async fn read_byte_at_async(&mut self, pos: u64) -> std::io::Result<u8> {
        self.hint_buffering_async(pos).await?;
        Ok(self.buf[(pos - self.buffered_range.start) as usize])
    }

    pub async fn read_exact_at_async(
        &mut self,
        pos: u64,
        buf: &mut [core::mem::MaybeUninit<u8>],
    ) -> std::io::Result<()> {
        if self.buffered_range.contains(&pos)
            && self.buffered_range.contains(&(pos + buf.len() as u64))
        {
            // perfectly contained
            let offs = pos - self.buffered_range.start;
            unsafe {
                core::ptr::copy_nonoverlapping(
                    self.buf.spare_capacity_mut().as_ptr().add(offs as usize),
                    buf.as_mut_ptr(),
                    buf.len(),
                );
            }

            return Ok(());
        }

        // simple read from blob
        self.inner.read_exact_async(pos, buf).await
    }

    pub async fn readv_all_at_async(
        &mut self,
        mut pos: u64,
        mut iovecs: &mut [IoSliceMut<'_>],
    ) -> std::io::Result<()> {
        IoSliceMut::advance_slices(&mut iovecs, 0);
        if iovecs.is_empty() {
            // no iovecs or empty
            return Ok(());
        }

        if self.buffered_range.contains(&pos)
            && self
                .buffered_range
                .contains(&(pos + iovecs[0].len() as u64))
        {
            // can fill from buffer at least one slice
            let offs = pos - self.buffered_range.start;
            let max_size = self.buffered_range.end - pos;
            let filled = unsafe {
                core::slice::from_raw_parts(self.buf.as_ptr().add(offs as _), max_size as _)
                    .read_vectored(iovecs)?
            };
            IoSliceMut::advance_slices(&mut iovecs, filled);
            pos += filled as u64;
        }

        self.inner.readv_all_async(pos, iovecs).await
    }
}
