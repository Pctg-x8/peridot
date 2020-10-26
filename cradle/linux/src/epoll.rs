
use libc::*;

#[repr(transparent)]
pub struct Epoll(libc::c_int);
impl Epoll {
	pub fn new() -> std::io::Result<Self> {
		let r = unsafe { libc::epoll_create1(0) };
		if r < 0 { Err(std::io::Error::last_os_error()) } else { Ok(Epoll(r)) }
	}

	pub fn add_fd(&self, fd: c_int, events: u32) -> std::io::Result<()> {
		let mut event_data = libc::epoll_event { events, u64: fd as _ };
		let r = unsafe {
			libc::epoll_ctl(self.0, libc::EPOLL_CTL_ADD, fd, &mut event_data)
		};
		if r < 0 { Err(std::io::Error::last_os_error()) } else { Ok(()) }
	}
	pub fn wait(
		&self, event_data: &mut [libc::epoll_event], timeout: Option<libc::c_int>
	) -> std::io::Result<libc::c_int> {
		let r = unsafe {
			libc::epoll_wait(self.0, event_data.as_mut_ptr(), event_data.len() as _, timeout.unwrap_or(-1))
		};
		if r < 0 { Err(std::io::Error::last_os_error()) } else { Ok(r) }
	}
}
impl Drop for Epoll {
	fn drop(&mut self) {
		unsafe { libc::close(self.0); }
	}
}
