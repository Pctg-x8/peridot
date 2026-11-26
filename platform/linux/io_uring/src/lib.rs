use std::os::fd::{AsRawFd, RawFd};

pub mod ffi;

pub struct IoUring(RawFd);
impl Drop for IoUring {
    #[inline]
    fn drop(&mut self) {
        let _ = unsafe { ffi::close(self.0) };
    }
}
impl AsRawFd for IoUring {
    #[inline(always)]
    fn as_raw_fd(&self) -> RawFd {
        self.0
    }
}
impl IoUring {
    pub fn new(entries: u32, params: &mut ffi::io_uring_params) -> std::io::Result<Self> {
        let r = unsafe {
            ffi::syscall(ffi::__NR_io_uring_setup, entries, params as *mut _) as core::ffi::c_int
        };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(Self(r))
        }
    }

    pub fn enter(
        &self,
        to_submit: core::ffi::c_uint,
        min_complete: core::ffi::c_uint,
        flags: core::ffi::c_uint,
        sig: *mut core::ffi::c_void, /* sigset_t */
    ) -> std::io::Result<core::ffi::c_uint> {
        let r = unsafe {
            ffi::syscall(
                ffi::__NR_io_uring_enter,
                self.0,
                to_submit,
                min_complete,
                flags,
                sig,
            ) as core::ffi::c_int
        };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(r.cast_unsigned())
        }
    }

    pub unsafe fn register(
        &self,
        opcode: core::ffi::c_uint,
        arg: *mut core::ffi::c_void,
        nr_args: core::ffi::c_uint,
    ) -> std::io::Result<core::ffi::c_uint> {
        let r = unsafe {
            ffi::syscall(ffi::__NR_io_uring_register, self.0, opcode, arg, nr_args)
                as core::ffi::c_int
        };
        if r < 0 {
            Err(std::io::Error::from_raw_os_error(-r))
        } else {
            Ok(r.cast_unsigned())
        }
    }
}
