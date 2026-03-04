pub struct TemporalSharedMemory {
    name: Vec<u8>,
    fd: core::ffi::c_int,
}
impl Drop for TemporalSharedMemory {
    fn drop(&mut self) {
        unsafe {
            libc::shm_unlink(self.name.as_ptr().cast());
        }
    }
}
impl std::os::fd::AsRawFd for TemporalSharedMemory {
    #[inline(always)]
    fn as_raw_fd(&self) -> std::os::unix::prelude::RawFd {
        self.fd
    }
}
impl TemporalSharedMemory {
    // ランダム名生成をリトライする最大回数 無限でもいいけどあきらめられるようにしたほうが応答返せるので親切
    const MAX_RETRY_COUNT: usize = 100;

    pub fn new_unique(
        prefix: &core::ffi::CStr,
        oflag: core::ffi::c_int,
        mode: libc::mode_t,
    ) -> std::io::Result<Option<Self>> {
        let mut shm_name: Vec<u8> = Vec::with_capacity(prefix.to_bytes().len() + 8);
        shm_name.extend(prefix.to_bytes());
        shm_name.extend(b"-000000\x00");

        for _ in 0..Self::MAX_RETRY_COUNT {
            let mut ts = core::mem::MaybeUninit::uninit();
            unsafe {
                libc::clock_gettime(libc::CLOCK_REALTIME, ts.as_mut_ptr());
            }
            let mut r = unsafe { ts.assume_init_ref().tv_nsec };
            for n in 0..6 {
                shm_name[9 + n] = (b'A' as i64 + (r & 15) + (r & 16) * 2) as _;
                r >>= 5;
            }

            let fd = unsafe {
                libc::shm_open(
                    shm_name.as_ptr().cast(),
                    libc::O_EXCL | libc::O_CREAT | oflag,
                    mode,
                )
            };
            if fd < 0 {
                match std::io::Error::last_os_error() {
                    e if e.kind() == std::io::ErrorKind::AlreadyExists => {
                        tracing::warn!(name = ?unsafe { core::ffi::CStr::from_bytes_with_nul_unchecked(&shm_name) }, "shm_open failed(EEXIST), retrying");
                        continue;
                    }
                    e => return Err(e),
                }
            }

            return Ok(Some(Self { name: shm_name, fd }));
        }

        Ok(None)
    }
}
