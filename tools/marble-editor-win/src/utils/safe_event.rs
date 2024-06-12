use windows::Win32::{
    Foundation::{CloseHandle, HANDLE},
    System::Threading::{CreateEventA, ResetEvent, SetEvent},
};

pub struct EventHandle(HANDLE);
unsafe impl Sync for EventHandle {}
unsafe impl Send for EventHandle {}
impl EventHandle {
    #[inline(always)]
    pub fn new() -> windows::core::Result<Self> {
        unsafe { CreateEventA(None, false, false, None).map(Self) }
    }

    #[inline(always)]
    pub fn set(&self) -> windows::core::Result<()> {
        unsafe { SetEvent(self.0) }
    }

    #[inline(always)]
    pub fn reset(&self) -> windows::core::Result<()> {
        unsafe { ResetEvent(self.0) }
    }
}
impl Drop for EventHandle {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { CloseHandle(self.0).expect("Failed to close event handle") }
    }
}
