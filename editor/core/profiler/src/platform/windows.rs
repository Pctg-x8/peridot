use core::mem::MaybeUninit;
use std::sync::LazyLock;

use windows::Win32::System::{
    Performance::{QueryPerformanceCounter, QueryPerformanceFrequency},
    ProcessStatus::{GetProcessMemoryInfo, PROCESS_MEMORY_COUNTERS_EX2},
    Threading::GetCurrentProcess,
};

#[inline(always)]
pub fn timestamp() -> i64 {
    let mut x = MaybeUninit::uninit();
    unsafe {
        // never fails on winxp or later
        let _ = QueryPerformanceCounter(x.as_mut_ptr());
    }
    unsafe { x.assume_init() }
}

pub static TIMESTAMP_FREQUENCY: LazyLock<i64> = LazyLock::new(|| {
    let mut x = MaybeUninit::uninit();
    unsafe {
        // never fails on winxp or later
        let _ = QueryPerformanceFrequency(x.as_mut_ptr());
    }
    unsafe { x.assume_init() }
});

pub fn get_self_process_memory_info() -> PROCESS_MEMORY_COUNTERS_EX2 {
    let mut stat = MaybeUninit::<PROCESS_MEMORY_COUNTERS_EX2>::uninit();
    if let Err(e) = unsafe {
        GetProcessMemoryInfo(
            GetCurrentProcess(),
            stat.as_mut_ptr().cast(),
            size_of::<PROCESS_MEMORY_COUNTERS_EX2>() as _,
        )
    } {
        tracing::error!(reason = %e, "GetProcessMemoryInfo failed");
        return unsafe { MaybeUninit::zeroed().assume_init() };
    }

    unsafe { stat.assume_init() }
}
