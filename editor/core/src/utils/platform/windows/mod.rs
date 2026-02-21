mod log_writer;
pub mod winrt_bootstrap;

use windows::{
    Win32::{
        System::Diagnostics::Debug::OutputDebugStringA,
        UI::WindowsAndMessaging::{
            MB_ICONERROR, MB_OK, MessageBoxA, RegisterClassExW, WNDCLASSEXW,
        },
    },
    core::PCSTR,
};

pub use self::{log_writer::DebugOutputWriter, winrt_bootstrap::WindowsAppRuntimeBootstrap};

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
pub unsafe fn register_class(x: &WNDCLASSEXW) -> std::io::Result<u16> {
    match unsafe { RegisterClassExW(x) } {
        r if r == 0 => Err(std::io::Error::last_os_error()),
        r => Ok(r),
    }
}
