// always const value(clock_gettime returns in nanosecs)
pub const TIMESTAMP_FREQUENCY: i64 = 1_000_000_000;

#[inline(always)]
pub fn timestamp() -> i64 {
    let mut x = core::mem::MaybeUninit::uninit();
    if unsafe { libc::clock_gettime(libc::CLOCK_MONOTONIC_RAW, x.as_mut_ptr()) } < 0 {
        tracing::error!(reason = %std::io::Error::last_os_error(), "clock_gettime failed");
        return 0;
    }

    unsafe {
        let x = x.assume_init();
        x.tv_nsec + x.tv_sec * 1_000_000_000
    }
}

unsafe extern "C" {
    fn getpagesize() -> core::ffi::c_int;
}

#[inline(always)]
pub fn pagesize() -> usize {
    unsafe { getpagesize() as _ }
}
