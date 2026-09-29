use core::ffi::*;
use std::os::fd::AsRawFd;

pub struct TimerFD {
    fd: c_int,
}
impl AsRawFd for TimerFD {
    #[inline(always)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        self.fd
    }
}
impl TimerFD {
    #[inline(always)]
    pub fn new() -> std::io::Result<Self> {
        match unsafe {
            timerfd_create(
                libc::CLOCK_MONOTONIC,
                libc::TFD_NONBLOCK | libc::TFD_CLOEXEC,
            )
        } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            fd => Ok(Self { fd }),
        }
    }

    #[inline(always)]
    pub fn set(&self, sec: libc::time_t, nsec: libc::time_t) -> std::io::Result<()> {
        match unsafe {
            libc::timerfd_settime(
                self.fd,
                0,
                &libc::itimerspec {
                    it_interval: libc::timespec {
                        tv_sec: 0,
                        tv_nsec: 0,
                    },
                    it_value: libc::timespec {
                        tv_sec: sec,
                        tv_nsec: nsec,
                    },
                },
                std::ptr::null_mut(),
            )
        } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            _ => Ok(()),
        }
    }

    #[inline(always)]
    pub fn set_interval(&self, sec: libc::time_t, nsec: libc::time_t) -> std::io::Result<()> {
        match unsafe {
            libc::timerfd_settime(
                self.fd,
                0,
                &libc::itimerspec {
                    it_interval: libc::timespec {
                        tv_sec: sec,
                        tv_nsec: nsec,
                    },
                    it_value: libc::timespec {
                        tv_sec: sec,
                        tv_nsec: nsec,
                    },
                },
                std::ptr::null_mut(),
            )
        } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            _ => Ok(()),
        }
    }

    #[inline(always)]
    pub fn unset(&self) -> std::io::Result<()> {
        match unsafe {
            libc::timerfd_settime(
                self.fd,
                0,
                &libc::itimerspec {
                    it_interval: libc::timespec {
                        tv_sec: 0,
                        tv_nsec: 0,
                    },
                    it_value: libc::timespec {
                        tv_sec: 0,
                        tv_nsec: 0,
                    },
                },
                std::ptr::null_mut(),
            )
        } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            _ => Ok(()),
        }
    }

    #[inline(always)]
    pub fn take(&self) -> std::io::Result<u64> {
        let mut b = core::mem::MaybeUninit::<u64>::uninit();
        match unsafe { libc::read(self.fd, b.as_mut_ptr().cast(), 8) } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            _ => Ok(unsafe { b.assume_init() }),
        }
    }
}

unsafe extern "C" {
    fn timerfd_create(clockid: c_int, flags: c_int) -> c_int;
}
