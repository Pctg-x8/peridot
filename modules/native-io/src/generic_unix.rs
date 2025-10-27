use core::mem::MaybeUninit;
use std::os::fd::{AsRawFd, RawFd};

#[repr(transparent)]
pub struct UnixFile(RawFd);
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
    fn as_raw_fd(&self) -> RawFd {
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

    pub unsafe fn mmap_raw(
        fd: RawFd,
        len: usize,
        prot: core::ffi::c_int,
        flags: core::ffi::c_int,
        offs: libc::off_t,
    ) -> std::io::Result<UnixFileUnmapData> {
        let page_size = unsafe { libc::sysconf(libc::_SC_PAGESIZE) };
        assert!(page_size >= 0, "sysconf(_SC_PAGESIZE) failed");
        let page_size = page_size.cast_unsigned() as usize;
        let aligned_offs = offs as usize & !(page_size - 1);
        let p = unsafe {
            libc::mmap(
                core::ptr::null_mut(),
                len,
                prot,
                flags,
                fd,
                aligned_offs as _,
            )
        };
        if p == libc::MAP_FAILED {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(UnixFileUnmapData {
                start_addr: p,
                offset: offs as usize - aligned_offs,
                len,
            })
        }
    }

    pub fn mmap(
        &self,
        len: usize,
        prot: core::ffi::c_int,
        flags: core::ffi::c_int,
        offs: libc::off_t,
    ) -> std::io::Result<UnixFileUnmapData> {
        unsafe { Self::mmap_raw(self.0, len, prot, flags, offs) }
    }
}

pub struct UnixFileUnmapData {
    pub start_addr: *mut core::ffi::c_void,
    pub offset: usize,
    pub len: usize,
}
impl UnixFileUnmapData {
    #[inline(always)]
    pub const fn data_addr(&self) -> *mut core::ffi::c_void {
        unsafe { self.start_addr.byte_add(self.offset) }
    }

    #[inline]
    pub fn unmap(self) -> std::io::Result<()> {
        let r = unsafe { libc::munmap(self.start_addr, self.len) };
        if r < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}
