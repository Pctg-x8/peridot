pub mod mmap;
pub mod shm;
pub mod xdg;

use std::path::PathBuf;

pub use self::{
    mmap::{MappedMemory, ReadonlyMappedFile},
    shm::TemporalSharedMemory,
};

/// A path to the home directory of a current user.
#[inline(always)]
pub fn home_dir() -> PathBuf {
    PathBuf::from(std::env::var_os("HOME").expect("no $HOME set"))
}

// free function wrapper
#[inline(always)]
pub unsafe fn ftruncate(
    fd: &(impl std::os::fd::AsRawFd + ?Sized),
    len: libc::off_t,
) -> std::io::Result<()> {
    if unsafe { libc::ftruncate(fd.as_raw_fd(), len) } < 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}
