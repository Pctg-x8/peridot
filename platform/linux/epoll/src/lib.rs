use core::{ffi::*, mem::MaybeUninit};
use std::os::fd::AsRawFd;

#[repr(transparent)]
pub struct Epoll(c_int);
impl Drop for Epoll {
    #[inline]
    fn drop(&mut self) {
        let r = unsafe { libc::close(self.0) };
        if r < 0 {
            let e = std::io::Error::last_os_error();
            eprintln!("Epoll::drop: close failed: {e:?}");
        }
    }
}
impl AsRawFd for Epoll {
    #[inline(always)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        self.0
    }
}
impl Epoll {
    #[inline(always)]
    pub fn new(flags: c_int) -> std::io::Result<Self> {
        match unsafe { libc::epoll_create1(flags) } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            r => Ok(Self(r)),
        }
    }

    #[inline(always)]
    pub fn ctl(
        &self,
        op: c_int,
        fd: c_int,
        event: Option<&mut libc::epoll_event>,
    ) -> std::io::Result<()> {
        match unsafe {
            libc::epoll_ctl(
                self.0,
                op,
                fd,
                event.map_or_else(core::ptr::null_mut, |x| x as *mut _),
            )
        } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            _ => Ok(()),
        }
    }

    #[inline(always)]
    pub fn wait(
        &self,
        event_data: &mut [MaybeUninit<libc::epoll_event>],
        timeout: Option<c_int>,
    ) -> std::io::Result<c_uint> {
        match unsafe {
            libc::epoll_wait(
                self.0,
                event_data.as_mut_ptr().cast(),
                event_data.len() as _,
                timeout.unwrap_or(-1),
            )
        } {
            r if r < 0 => Err(std::io::Error::last_os_error()),
            r => Ok(r.cast_unsigned()),
        }
    }

    #[inline(always)]
    pub fn add(
        &self,
        fd: &(impl AsRawFd + ?Sized),
        events: u32,
        extras: u64,
    ) -> std::io::Result<()> {
        self.ctl(
            libc::EPOLL_CTL_ADD,
            fd.as_raw_fd(),
            Some(&mut libc::epoll_event {
                events,
                u64: extras,
            }),
        )
    }

    #[inline(always)]
    pub fn del(&self, fd: &(impl AsRawFd + ?Sized)) -> std::io::Result<()> {
        self.ctl(libc::EPOLL_CTL_DEL, fd.as_raw_fd(), None)
    }

    #[inline(always)]
    pub fn r#mod(
        &self,
        fd: &(impl AsRawFd + ?Sized),
        events: u32,
        extras: u64,
    ) -> std::io::Result<()> {
        self.ctl(
            libc::EPOLL_CTL_MOD,
            fd.as_raw_fd(),
            Some(&mut libc::epoll_event {
                events,
                u64: extras,
            }),
        )
    }
}
