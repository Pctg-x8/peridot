pub struct MappedMemory {
    addr: *mut core::ffi::c_void,
    len: usize,
}
impl Drop for MappedMemory {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.addr, self.len);
        }
    }
}
impl MappedMemory {
    #[inline(always)]
    pub fn new(
        addr: Option<core::ptr::NonNull<core::ffi::c_void>>,
        len: usize,
        prot: core::ffi::c_int,
        flags: core::ffi::c_int,
        fd: &(impl std::os::fd::AsRawFd + ?Sized),
        offset: libc::off_t,
    ) -> std::io::Result<Self> {
        debug_assert!(len > 0, "Length must be greater than zero");

        let addr = unsafe {
            libc::mmap(
                match addr {
                    Some(x) => x.as_ptr(),
                    None => core::ptr::null_mut(),
                },
                len,
                prot,
                flags,
                fd.as_raw_fd(),
                offset,
            )
        };
        if addr == libc::MAP_FAILED {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(Self { addr, len })
        }
    }

    #[inline(always)]
    pub const fn as_ptr(&self) -> *mut core::ffi::c_void {
        self.addr
    }
}

pub struct ReadonlyMappedFile {
    ptr: *const core::ffi::c_void,
    size: usize,
}
unsafe impl Sync for ReadonlyMappedFile {}
unsafe impl Send for ReadonlyMappedFile {}
impl Drop for ReadonlyMappedFile {
    fn drop(&mut self) {
        if unsafe { libc::munmap(self.ptr.cast_mut(), self.size) } < 0 {
            let e = std::io::Error::last_os_error();
            tracing::warn!(reason = %e, "munmap");
        }
    }
}
impl core::ops::Deref for ReadonlyMappedFile {
    type Target = [u8];

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { core::slice::from_raw_parts(self.ptr.cast(), self.size) }
    }
}
impl ReadonlyMappedFile {
    pub fn open(path: &core::ffi::CStr) -> std::io::Result<Self> {
        let fd = unsafe { libc::open(path.as_ptr(), libc::O_RDONLY) };
        if fd < 0 {
            return Err(std::io::Error::last_os_error());
        }

        let size = unsafe { libc::lseek(fd, 0, libc::SEEK_END) };
        if size < 0 {
            if unsafe { libc::close(fd) } < 0 {
                let e = std::io::Error::last_os_error();
                panic!("close: {e}");
            }

            return Err(std::io::Error::last_os_error());
        }
        let size: usize = size.try_into().expect("mapping too large file!");

        let ptr = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                size,
                libc::PROT_READ,
                libc::MAP_PRIVATE,
                fd,
                0,
            )
        };
        if ptr == libc::MAP_FAILED {
            if unsafe { libc::close(fd) } < 0 {
                let e = std::io::Error::last_os_error();
                panic!("close: {e}");
            }

            return Err(std::io::Error::last_os_error());
        }

        if unsafe { libc::close(fd) } < 0 {
            let e = std::io::Error::last_os_error();
            panic!("close: {e}");
        }

        Ok(Self { ptr, size })
    }
}
