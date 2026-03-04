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
