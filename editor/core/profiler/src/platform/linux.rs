#[repr(transparent)]
pub struct StatMFile(core::ffi::c_int);
impl Drop for StatMFile {
    #[inline(always)]
    fn drop(&mut self) {
        if self.0 < 0 {
            // invalid
            return;
        }

        #[cfg(unix)]
        if unsafe { libc::close(self.0) } < 0 {
            tracing::error!(reason = %std::io::Error::last_os_error(), "failed close statm");
        }
    }
}
impl StatMFile {
    pub const unsafe fn invalid() -> Self {
        Self(-1)
    }

    pub fn open() -> std::io::Result<Self> {
        let fd = unsafe { libc::open(c"/proc/self/statm".as_ptr(), libc::O_RDONLY) };
        if fd < 0 {
            return Err(std::io::Error::last_os_error());
        }

        Ok(Self(fd))
    }

    pub fn read(&self) -> std::io::Result<StatMContent> {
        const BUFSIZE: usize = 64;
        let mut buf = [core::mem::MaybeUninit::<u8>::uninit(); BUFSIZE];
        let nread = unsafe { libc::pread(self.0, buf.as_mut_ptr().cast(), BUFSIZE as _, 0) };
        if nread < 0 {
            return Err(std::io::Error::last_os_error());
        }

        let mut buf: &[u8] = unsafe {
            &core::mem::transmute::<&[_; BUFSIZE], &[_; BUFSIZE]>(&buf)[..nread as usize]
        };
        let mut size = 0u64;
        while let &[c, ref rest @ ..] = buf
            && c != b' '
        {
            size = size * 10 + (c - b'0') as u64;
            buf = rest;
        }
        while let &[b' ', ref rest @ ..] = buf {
            buf = rest;
        }
        let mut resident = 0u64;
        while let &[c, ref rest @ ..] = buf
            && c != b' '
        {
            resident = resident * 10 + (c - b'0') as u64;
            buf = rest;
        }
        while let &[b' ', ref rest @ ..] = buf {
            buf = rest;
        }
        let mut resident_shared = 0u64;
        while let &[c, ref rest @ ..] = buf
            && c != b' '
        {
            resident_shared = resident_shared * 10 + (c - b'0') as u64;
            buf = rest;
        }

        Ok(StatMContent {
            size,
            resident,
            shared: resident_shared,
        })
    }
}

pub struct StatMContent {
    pub size: u64,
    pub resident: u64,
    pub shared: u64,
}
