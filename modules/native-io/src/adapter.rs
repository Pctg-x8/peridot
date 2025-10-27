//! Adapter structs for other crate's traits.

use std::io::{Read, Seek};

pub struct RandomBlobReadSeekAdapter<R> {
    inner: R,
    pos: u64,
}
impl<R> RandomBlobReadSeekAdapter<R> {
    #[inline(always)]
    pub const fn new(inner: R) -> Self {
        Self { inner, pos: 0 }
    }
}
impl<R> Read for RandomBlobReadSeekAdapter<R>
where
    R: crate::RandomReadBlob,
{
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let transferred = self.inner.read(self.pos, unsafe {
            core::mem::transmute::<&mut [_], &mut [core::mem::MaybeUninit<_>]>(buf)
        })?;
        self.pos += transferred as u64;

        Ok(transferred)
    }
}
impl<R> Seek for RandomBlobReadSeekAdapter<R>
where
    R: crate::BlobMetadata,
{
    #[inline]
    fn seek(&mut self, pos: std::io::SeekFrom) -> std::io::Result<u64> {
        let new_pos = match pos {
            std::io::SeekFrom::Start(x) => x,
            std::io::SeekFrom::Current(x) => (self.pos as i64 + x) as _,
            std::io::SeekFrom::End(x) => (self.inner.byte_length()? as i64 - x) as _,
        };
        self.pos = new_pos;
        Ok(new_pos)
    }
}
