mod log_writer;

use windows::{
    Win32::{
        Foundation::{HINSTANCE, HWND},
        System::{Diagnostics::Debug::OutputDebugStringA, LibraryLoader::GetModuleHandleW},
        UI::WindowsAndMessaging::{
            FindWindowExW, MB_ICONERROR, MB_OK, MessageBoxA, RegisterClassExW, WNDCLASSEXW,
        },
    },
    core::PCSTR,
};
use windows_core::PCWSTR;

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
