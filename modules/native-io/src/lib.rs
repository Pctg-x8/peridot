use core::mem::MaybeUninit;
use std::io::{IoSliceMut, Result as IOResult};

mod adapter;
pub use adapter::*;

#[cfg(target_os = "android")]
pub mod android;
#[cfg(unix)]
mod generic_unix;
#[cfg(target_os = "linux")]
pub mod linux;
#[cfg(target_os = "macos")]
pub mod macos;
#[cfg(windows)]
pub mod windows;

#[cfg(windows)]
pub type PlatformNativeFileReader = self::windows::NativeFileBlobRandomReader;
#[cfg(windows)]
pub type PlatformNativeFileReaderAsync = self::windows::NativeFileBlobAsyncRandomReader;
#[cfg(target_os = "linux")]
pub type PlatformNativeFileReader = self::linux::NativeFileBlobRandomReader;
#[cfg(target_os = "linux")]
pub type PlatformNativeFileReaderAsync = self::linux::NativeFileAsyncBlobRandomReader;
#[cfg(target_os = "android")]
pub type PlatformNativeFileReader = self::android::BundledAssetRandomReader;
#[cfg(target_os = "android")]
pub type PlatformNativeFileReaderAsync = self::android::BundledAssetAsyncRandomReader;
#[cfg(target_os = "macos")]
pub type PlatformNativeFileReader = self::macos::NativeFileReader;
#[cfg(target_os = "macos")]
pub type PlatformNativeFileReaderAsync = self::macos::NativeFileAsyncReader;

/// Metadata Accessor for an Blob(synchronous).
pub trait BlobMetadata {
    fn byte_length(&self) -> std::io::Result<u64>;
}

/// Metadata Accessor for an Blob(asynchronous).
pub trait BlobMetadataAsync {
    fn byte_length_async(&self) -> impl core::future::Future<Output = std::io::Result<u64>>;
}

/// Read-only random accessible Blob operations(synchronous).
pub trait RandomReadBlob {
    fn read(&self, pos: u64, buf: &mut [MaybeUninit<u8>]) -> std::io::Result<usize>;

    #[inline]
    fn readv(&self, offs: u64, iovecs: &mut [IoSliceMut]) -> std::io::Result<usize> {
        // default impl for unsupported platforms: read into single iovec
        match iovecs.first_mut() {
            None => Ok(0),
            Some(v) => self.read(offs, unsafe {
                core::mem::transmute::<&mut [u8], &mut [MaybeUninit<u8>]>(&mut v[..])
            }),
        }
    }

    // utility helpers
    fn read_exact(&self, offs: u64, buf: &mut [MaybeUninit<u8>]) -> std::io::Result<()> {
        let mut o = 0;
        while o < buf.len() {
            o += self.read(offs + o as u64, &mut buf[o..])?;
        }

        Ok(())
    }

    fn readv_all<'a, 'b, 'bb>(
        &'a self,
        offs: u64,
        mut iovecs: &'b mut [IoSliceMut<'bb>],
    ) -> std::io::Result<()> {
        // ensure iovecs are actually empty
        IoSliceMut::advance_slices(&mut iovecs, 0);

        let mut o = 0;
        while !iovecs.is_empty() {
            let r = self.readv(offs + o as u64, iovecs)?;
            IoSliceMut::advance_slices(&mut iovecs, r);
            o += r;
        }

        Ok(())
    }

    fn read_to_end(&self, offs: u64) -> std::io::Result<Vec<u8>> {
        const GROW_SIZE: usize = 8192;

        let mut buf = Vec::with_capacity(GROW_SIZE);
        let mut o = 0;
        loop {
            let r = match self.read(offs + o as u64, buf.spare_capacity_mut()) {
                Ok(0) => break,
                Ok(r) => r,
                Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                Err(e) => return Err(e),
            };

            o += r;
            unsafe {
                buf.set_len(o);
            }
            if o >= buf.capacity() {
                buf.reserve_exact(GROW_SIZE);
            }
        }

        buf.shrink_to_fit();
        Ok(buf)
    }
}

/// Read-only random accessible Blob operations(asynchronous).
pub trait RandomReadBlobAsync {
    type ReadFuture<'a, 'b>: Future<Output = std::io::Result<usize>>
    where
        Self: 'a;
    type ReadVecFuture<'a, 'b, 'bb>: Future<Output = std::io::Result<usize>>
    where
        Self: 'a,
        'bb: 'b;

    fn read_async<'a, 'b>(
        &'a self,
        offs: u64,
        buf: &'b mut [MaybeUninit<u8>],
    ) -> Self::ReadFuture<'a, 'b>;
    fn readv_async<'a, 'b, 'bb>(
        &'a self,
        offs: u64,
        iovecs: &'b mut [IoSliceMut<'bb>],
    ) -> Self::ReadVecFuture<'a, 'b, 'bb>;

    // utility helpers
    fn read_exact_async<'a, 'b>(
        &'a self,
        offs: u64,
        buf: &'b mut [MaybeUninit<u8>],
    ) -> impl core::future::Future<Output = IOResult<()>> + use<'a, 'b, Self> {
        async move {
            let mut o = 0;
            while o < buf.len() {
                let r = self.read_async(offs + o as u64, &mut buf[o..]).await?;
                o += r;
            }

            Ok(())
        }
    }

    fn readv_all_async<'a, 'b, 'bb>(
        &'a self,
        offs: u64,
        mut iovecs: &'b mut [IoSliceMut<'bb>],
    ) -> impl core::future::Future<Output = std::io::Result<()>> + use<'a, 'b, 'bb, Self> {
        async move {
            // ensure iovecs are actually empty
            IoSliceMut::advance_slices(&mut iovecs, 0);

            let mut o = 0;
            while !iovecs.is_empty() {
                let r = self.readv_async(offs + o as u64, iovecs).await?;
                IoSliceMut::advance_slices(&mut iovecs, r);
                o += r;
            }

            Ok(())
        }
    }

    fn read_to_end_async<'a>(
        &'a mut self,
        offs: u64,
    ) -> impl core::future::Future<Output = IOResult<Vec<u8>>> + use<'a, Self> {
        async move {
            const GROW_SIZE: usize = 8192;

            let mut buf = Vec::with_capacity(GROW_SIZE);
            let mut o = 0;
            loop {
                let r = match self
                    .read_async(offs + o as u64, buf.spare_capacity_mut())
                    .await
                {
                    Ok(0) => break,
                    Ok(r) => r,
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                    Err(e) => return Err(e),
                };

                o += r;
                unsafe {
                    buf.set_len(o);
                }
                if o >= buf.capacity() {
                    buf.reserve_exact(GROW_SIZE);
                }
            }

            buf.shrink_to_fit();
            Ok(buf)
        }
    }
}

/// An Blob object that can be mapped into a memory.
pub trait MemoryMapBlob {
    type MemoryUnmapData;

    fn mmap(
        &self,
        offs: u64,
        len: usize,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)>;
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()>;
}
