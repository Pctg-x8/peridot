use std::os::windows::io::{AsHandle, AsRawHandle, BorrowedHandle};

use bitflags::bitflags;
use windows::Win32::{
    Foundation::{CloseHandle, HANDLE},
    System::Threading::{CreateEventW, ResetEvent, SetEvent},
};

pub struct Event(HANDLE);
unsafe impl Sync for Event {}
unsafe impl Send for Event {}
impl Drop for Event {
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
impl AsHandle for Event {
    #[inline(always)]
    fn as_handle(&self) -> BorrowedHandle<'_> {
        unsafe { BorrowedHandle::borrow_raw(self.0.0) }
    }
}
impl AsRawHandle for Event {
    #[inline(always)]
    fn as_raw_handle(&self) -> std::os::windows::prelude::RawHandle {
        self.0.0
    }
}
impl Event {
    #[inline(always)]
    pub fn new(flags: EventCreateFlags) -> windows::core::Result<Self> {
        Ok(Self(unsafe {
            CreateEventW(
                None,
                flags.contains(EventCreateFlags::MANUAL_RESET),
                flags.contains(EventCreateFlags::INITIAL_SET),
                None,
            )?
        }))
    }

    #[inline(always)]
    pub fn close_checked(self) -> Result<(), (Self, windows::core::Error)> {
        match unsafe { CloseHandle(self.0) } {
            Ok(_) => {
                core::mem::forget(self);
                Ok(())
            }
            Err(e) => Err((self, e)),
        }
    }

    #[inline(always)]
    pub fn set(&self) -> windows::core::Result<()> {
        unsafe { SetEvent(self.0) }
    }

    #[inline(always)]
    pub fn reset(&self) -> windows::core::Result<()> {
        unsafe { ResetEvent(self.0) }
    }

    #[inline(always)]
    pub const fn as_handle_native(&self) -> HANDLE {
        self.0
    }
}

bitflags! {
    #[derive(Clone, Copy)]
    pub struct EventCreateFlags : u8 {
        const MANUAL_RESET = 0x01;
        const INITIAL_SET = 0x01;
    }
}
