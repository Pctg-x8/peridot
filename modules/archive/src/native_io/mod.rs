use std::io::Result as IOResult;

#[cfg(target_os = "linux")]
pub mod linux;
#[cfg(windows)]
pub mod windows;

pub trait NativeFileMemoryMapProvider {
    type MemoryUnmapData;

    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)>;
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()>;
}

pub trait AsyncNativeFileReader: NativeFileMemoryMapProvider {
    type ReadFuture<'a>: core::future::Future<Output = IOResult<usize>>
    where
        Self: 'a;
    type ReadVecFuture<'a, 'b, 'b2>: core::future::Future<Output = IOResult<usize>>
    where
        Self: 'a,
        'b2: 'b;
    type PosReadFuture<'a, 'b>: core::future::Future<Output = IOResult<usize>>
    where
        Self: 'a;

    fn current_pointer_pos(&self) -> std::io::Result<u64>;
    fn read_async<'a>(&'a mut self, buf: &'a mut [u8]) -> Self::ReadFuture<'a>;
    fn readv_async<'a, 'b, 'b2>(
        &'a mut self,
        buf: &'b mut [std::io::IoSliceMut<'b2>],
    ) -> Self::ReadVecFuture<'a, 'b, 'b2>;
    fn pread_async<'a, 'b>(&'a self, buf: &'b mut [u8], offs: u64) -> Self::PosReadFuture<'a, 'b>;

    #[inline]
    fn read_exact(
        &mut self,
        mut buf: &mut [u8],
    ) -> impl core::future::Future<Output = IOResult<()>> {
        async move {
            while !buf.is_empty() {
                let r = self.read_async(buf).await?;
                buf = &mut buf[r..];
            }

            Ok(())
        }
    }

    #[inline]
    fn read_to_end(&mut self) -> impl core::future::Future<Output = IOResult<Vec<u8>>> {
        async move {
            const GROW_SIZE: usize = 8192;

            let mut buf = Vec::with_capacity(GROW_SIZE);
            let mut o = 0;
            loop {
                let r = match self
                    .read_async(unsafe {
                        core::mem::transmute::<&mut [core::mem::MaybeUninit<_>], &mut [_]>(
                            &mut buf.spare_capacity_mut()[o..],
                        )
                    })
                    .await
                {
                    Ok(0) => break,
                    Ok(r) => r,
                    Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                    Err(e) => return Err(e),
                };

                o += r;
                if o >= buf.capacity() {
                    buf.reserve_exact(buf.capacity() + GROW_SIZE);
                }
            }

            unsafe {
                buf.set_len(o);
            }
            buf.shrink_to_fit();
            Ok(buf)
        }
    }
}

pub trait NativeFileReader: NativeFileMemoryMapProvider {
    fn current_pointer_pos(&self) -> std::io::Result<u64>;
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>;
    fn readv(&mut self, buf: &mut [std::io::IoSliceMut]) -> std::io::Result<usize>;
    fn pread(&self, buf: &mut [u8], offs: u64) -> std::io::Result<usize>;

    fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
        let mut o = 0;
        while o < buf.len() {
            let r = self.read(&mut buf[o..])?;
            o += r;
        }

        Ok(())
    }

    fn read_to_end(&mut self) -> std::io::Result<Vec<u8>> {
        const GROW_SIZE: usize = 8192;

        let mut buf = Vec::with_capacity(GROW_SIZE);
        let mut o = 0;
        loop {
            let r = match self.read(unsafe {
                core::mem::transmute::<&mut [core::mem::MaybeUninit<_>], &mut [_]>(
                    &mut buf.spare_capacity_mut()[o..],
                )
            }) {
                Ok(0) => break,
                Ok(r) => r,
                Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
                Err(e) => return Err(e),
            };

            o += r;
            if o >= buf.capacity() {
                buf.reserve_exact(buf.capacity() + GROW_SIZE);
            }
        }

        unsafe {
            buf.set_len(o);
        }
        buf.shrink_to_fit();
        Ok(buf)
    }
}
