use core::mem::MaybeUninit;
use std::os::fd::AsRawFd;

#[repr(transparent)]
pub struct UnixFile(std::os::unix::prelude::RawFd);
impl Drop for UnixFile {
    #[inline]
    fn drop(&mut self) {
        let r = unsafe { libc::close(self.0) };
        if r < 0 {
            let e = std::io::Error::last_os_error();
            panic!("Error closing file descriptor: {e:?}");
        }
    }
}
impl AsRawFd for UnixFile {
    #[inline(always)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        self.0
    }
}
impl UnixFile {
    #[inline]
    pub fn open(path: &core::ffi::CStr, flags: core::ffi::c_int) -> std::io::Result<Self> {
        let fd = unsafe { libc::open(path.as_ptr(), flags) };
        if fd < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(Self(fd))
        }
    }

    #[inline]
    pub fn pread(&self, offs: libc::off_t, buf: &mut [MaybeUninit<u8>]) -> std::io::Result<usize> {
        let r = unsafe {
            libc::pread(
                self.0.as_raw_fd(),
                buf.as_mut_ptr() as _,
                buf.len(),
                offs as _,
            )
        };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }

    #[inline]
    pub fn preadv(&self, offs: libc::off_t, iovecs: &mut [libc::iovec]) -> std::io::Result<usize> {
        let r = unsafe {
            libc::preadv(
                self.0.as_raw_fd(),
                iovecs.as_ptr() as _,
                iovecs.len() as _,
                offs as _,
            )
        };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r.cast_unsigned())
        }
    }

    #[inline]
    pub fn mmap(
        &self,
        len: usize,
        prot: core::ffi::c_int,
        flags: core::ffi::c_int,
        offs: libc::off_t,
    ) -> std::io::Result<UnixFileUnmapData> {
        let p = unsafe { libc::mmap(core::ptr::null_mut(), len, prot, flags, self.0, offs) };
        if p == libc::MAP_FAILED {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(UnixFileUnmapData { addr: p, len })
        }
    }
}

pub struct UnixFileUnmapData {
    pub addr: *mut core::ffi::c_void,
    pub len: usize,
}
impl UnixFileUnmapData {
    #[inline]
    pub fn unmap(self) -> std::io::Result<()> {
        let r = unsafe { libc::munmap(self.addr, self.len) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}
