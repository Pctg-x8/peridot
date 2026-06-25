mod log_writer;

use windows::{
    Win32::{
        Foundation::{HANDLE, HINSTANCE, HWND, LPARAM, RECT},
        Graphics::Gdi::{EnumDisplayMonitors, HDC, HMONITOR},
        System::{
            Diagnostics::Debug::OutputDebugStringA,
            LibraryLoader::GetModuleHandleW,
            Threading::{
                CancelWaitableTimer, CreateEventW, CreateWaitableTimerW, ResetEvent, SetEvent,
                SetWaitableTimer,
            },
        },
        UI::WindowsAndMessaging::{
            FindWindowExW, MB_ICONERROR, MB_OK, MessageBoxA, RegisterClassExW, WNDCLASSEXW,
        },
    },
    core::PCSTR,
};
use windows_core::{BOOL, PCWSTR};

pub use self::log_writer::DebugOutputWriter;

pub fn set_panic_hook() {
    std::panic::set_hook(Box::new(|panic| unsafe {
        let panic_msg = match std::ffi::CString::new(panic.to_string()) {
            Ok(x) => x,
            Err(_) => c"<<Could not convert panic message!>>".into(),
        };

        OutputDebugStringA(PCSTR(panic_msg.as_ptr().cast()));

        MessageBoxA(
            None,
            PCSTR(panic_msg.as_ptr().cast()),
            PCSTR(c"Program panic!".as_ptr().cast()),
            MB_OK | MB_ICONERROR,
        );
        std::process::abort();
    }));
}

#[inline(always)]
pub fn current_instance_handle() -> HINSTANCE {
    unsafe { GetModuleHandleW(None).expect("GetModuleHandleW").into() }
}

#[inline(always)]
pub unsafe fn register_class(x: &WNDCLASSEXW) -> std::io::Result<u16> {
    match unsafe { RegisterClassExW(x) } {
        r if r == 0 => Err(std::io::Error::last_os_error()),
        r => Ok(r),
    }
}

#[derive(Clone, Copy)]
pub enum EnumerateDisplayMonitorContinuous {
    Continue,
    Stop,
}

#[inline(always)]
pub fn enumerate_display_monitors<F>(mut f: F)
where
    F: FnMut(HMONITOR, &RECT) -> EnumerateDisplayMonitorContinuous,
{
    unsafe extern "system" fn callback<F>(
        mon: HMONITOR,
        _dc: HDC,
        rect: *mut RECT,
        param: LPARAM,
    ) -> BOOL
    where
        F: FnMut(HMONITOR, &RECT) -> EnumerateDisplayMonitorContinuous,
    {
        match (unsafe {
            &mut *core::ptr::with_exposed_provenance_mut::<F>(param.0.cast_unsigned())
        })(mon, unsafe { &*rect })
        {
            EnumerateDisplayMonitorContinuous::Continue => BOOL(1),
            EnumerateDisplayMonitorContinuous::Stop => BOOL(0),
        }
    }
    let _ = unsafe {
        EnumDisplayMonitors(
            None,
            None,
            Some(callback::<F>),
            LPARAM(core::ptr::from_mut(&mut f).addr().cast_signed()),
        )
    };
}

pub struct WindowByClassIter {
    class: PCWSTR,
    window_after: Option<HWND>,
}
impl WindowByClassIter {
    pub fn new(class: PCWSTR) -> Self {
        Self {
            class,
            window_after: None,
        }
    }
}
impl Iterator for WindowByClassIter {
    type Item = windows_core::Result<HWND>;

    fn next(&mut self) -> Option<Self::Item> {
        match unsafe { FindWindowExW(None, self.window_after, self.class, None) } {
            Ok(x) => {
                self.window_after = Some(x);
                Some(Ok(x))
            }
            Err(e) if e == windows_core::Error::empty() => None,
            Err(e) => Some(Err(e)),
        }
    }
}

pub struct Event(HANDLE);
unsafe impl Sync for Event {}
unsafe impl Send for Event {}
impl Drop for Event {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            windows::Win32::Foundation::CloseHandle(self.0).expect("Event.CloseHandle");
        }
    }
}
impl Event {
    #[inline(always)]
    pub fn new(manual_reset: bool, initial: bool) -> windows_core::Result<Self> {
        Ok(Self(unsafe {
            CreateEventW(None, manual_reset, initial, None)?
        }))
    }

    #[inline(always)]
    pub fn set(&self) -> windows_core::Result<()> {
        unsafe { SetEvent(self.0) }
    }

    #[inline(always)]
    pub fn reset(&self) -> windows_core::Result<()> {
        unsafe { ResetEvent(self.0) }
    }

    #[inline(always)]
    pub const fn as_handle(&self) -> HANDLE {
        self.0
    }
}

pub struct WaitableTimer(HANDLE);
unsafe impl Sync for WaitableTimer {}
unsafe impl Send for WaitableTimer {}
impl Drop for WaitableTimer {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            windows::Win32::Foundation::CloseHandle(self.0).expect("WaitableTimer.CloseHandle");
        }
    }
}
impl WaitableTimer {
    #[inline(always)]
    pub fn new(manual_reset: bool) -> windows_core::Result<Self> {
        Ok(Self(unsafe {
            CreateWaitableTimerW(None, manual_reset, None)?
        }))
    }

    #[inline(always)]
    pub fn set_oneshot_relative(&self, timeout_millis: u64) -> windows_core::Result<()> {
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
    pub fn set_interval_relative(&self, period_millis: u64) -> windows_core::Result<()> {
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
    pub fn cancel(&self) -> windows_core::Result<()> {
        unsafe { CancelWaitableTimer(self.0) }
    }

    #[inline(always)]
    pub const fn as_handle(&self) -> HANDLE {
        self.0
    }
}
