use core::ffi::*;
use std::os::fd::AsRawFd;

#[repr(transparent)]
pub struct EventFD(c_int);
impl Drop for EventFD {
    #[inline]
    fn drop(&mut self) {
        let r = unsafe { libc::close(self.0) };
        if r < 0 {
            let e = std::io::Error::last_os_error();
            eprintln!("EventFD::drop: close failed: {e:?}");
        }
    }
}
impl AsRawFd for EventFD {
    #[inline(always)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        self.0
    }
}
impl EventFD {
    #[inline(always)]
    pub fn new(initval: c_uint, flags: c_int) -> std::io::Result<Self> {
        debug_assert!(
            flags & libc::EFD_SEMAPHORE == 0,
            "no semaphore mode support"
        );

        match unsafe { libc::eventfd(initval, flags) } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            r => Ok(Self(r)),
        }
    }

    #[inline(always)]
    pub fn take(&self) -> std::io::Result<u64> {
        let mut b = core::mem::MaybeUninit::<u64>::uninit();
        match unsafe { libc::read(self.0, b.as_mut_ptr().cast(), 8) } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            _ => Ok(unsafe { b.assume_init() }),
        }
    }

    #[inline(always)]
    pub fn inc(&self, value: u64) -> std::io::Result<()> {
        match unsafe { libc::write(self.0, value.to_ne_bytes().as_ptr().cast(), 8) } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            _ => Ok(()),
        }
    }
}
