use std::os::windows::io::{AsHandle, AsRawHandle, BorrowedHandle};

use windows::Win32::{
    Foundation::{CloseHandle, HANDLE},
    System::Threading::{CancelWaitableTimer, CreateWaitableTimerW, SetWaitableTimer},
};

#[repr(transparent)]
pub struct WaitableTimer(HANDLE);
unsafe impl Sync for WaitableTimer {}
unsafe impl Send for WaitableTimer {}
impl Drop for WaitableTimer {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { CloseHandle(self.0) } {
            #[cfg(feature = "tracing")]
            tracing::error!(reason = %e, "CloseHandle failed");
            #[cfg(not(feature = "tracing"))]
            eprintln!("CloseHandle failed: {e}");
        }
    }
}
impl AsHandle for WaitableTimer {
    #[inline(always)]
    fn as_handle(&self) -> BorrowedHandle<'_> {
        unsafe { BorrowedHandle::borrow_raw(self.0.0) }
    }
}
impl AsRawHandle for WaitableTimer {
    #[inline(always)]
    fn as_raw_handle(&self) -> std::os::windows::prelude::RawHandle {
        self.0.0
    }
}
impl WaitableTimer {
    #[inline(always)]
    pub fn new(manual_reset: bool) -> windows::core::Result<Self> {
        Ok(Self(unsafe {
            CreateWaitableTimerW(None, manual_reset, None)?
        }))
    }

    #[inline(always)]
    pub const unsafe fn ref_from_handle<'a>(handle: &'a HANDLE) -> &'a Self {
        unsafe { core::mem::transmute(handle) }
    }

    #[inline(always)]
    pub fn set_oneshot_relative(&self, timeout_millis: u64) -> windows::core::Result<()> {
        unsafe {
            SetWaitableTimer(
                self.0,
                &(timeout_millis as i64 * -1_000_0),
                0,
                None,
                None,
                false,
            )
        }
    }

    #[inline(always)]
    pub fn set_interval_relative(&self, period_millis: u64) -> windows::core::Result<()> {
        unsafe {
            SetWaitableTimer(
                self.0,
                &(period_millis as i64 * -1_000_0),
                period_millis as _,
                None,
                None,
                false,
            )
        }
    }

    #[inline(always)]
    pub fn cancel(&self) -> windows::core::Result<()> {
        unsafe { CancelWaitableTimer(self.0) }
    }

    #[inline(always)]
    pub const fn as_handle_native(&self) -> HANDLE {
        self.0
    }
}
