pub mod mmap;
pub mod shm;

pub use self::{mmap::MappedMemory, shm::TemporalSharedMemory};

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
