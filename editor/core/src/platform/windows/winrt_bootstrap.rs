use windows::Win32::{
    Foundation::{FreeLibrary, HMODULE},
    Storage::Packaging::Appx::PACKAGE_VERSION,
    System::LibraryLoader::{GetProcAddress, LOAD_LIBRARY_FLAGS, LoadLibraryExW},
};
use windows_core::{HRESULT, PCSTR, PCWSTR, w};

#[repr(C)]
#[derive(Clone, Copy)]
enum MddBootstrapInitializeOptions {
    ShowUI = 0x08,
}
// copy from WindowsAppSDK-VersionInfo.h
const APP_SDK_VERSION_U64: u64 = 0;
#[allow(non_snake_case)]
type FPMddBootstrapInitialize2 = unsafe extern "system" fn(
    majorMinorVersion: u32,
    versionTag: PCWSTR,
    minVersion: PACKAGE_VERSION,
    options: MddBootstrapInitializeOptions,
) -> HRESULT;
type FPMddBootstrapShutdown = unsafe extern "system" fn();

pub struct WindowsAppRuntimeBootstrap {
    lib: HMODULE,
    shutdown: FPMddBootstrapShutdown,
}
impl Drop for WindowsAppRuntimeBootstrap {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            if let Err(e) = FreeLibrary(self.lib) {
                tracing::error!(reason = %e, "freelibrary");
            }
        }
    }
}
impl WindowsAppRuntimeBootstrap {
    pub fn init() -> Self {
        let lib = unsafe {
            LoadLibraryExW(
                w!("Microsoft.WindowsAppRuntime.Bootstrap.dll"),
                None,
                LOAD_LIBRARY_FLAGS(0),
            )
            .expect("loadlibrary")
        };
        let initialize: FPMddBootstrapInitialize2 = unsafe {
            core::mem::transmute(GetProcAddress(
                lib,
                PCSTR(c"MddBootstrapInitialize2".as_ptr().cast()),
            ))
        };
        let shutdown: FPMddBootstrapShutdown = unsafe {
            core::mem::transmute(GetProcAddress(
                lib,
                PCSTR(c"MddBootstrapShutdown".as_ptr().cast()),
            ))
        };

        unsafe {
            initialize(
                0x00010008,
                w!(""),
                core::mem::transmute(APP_SDK_VERSION_U64),
                MddBootstrapInitializeOptions::ShowUI,
            )
            .ok()
            .expect("windowsappruntime.bootstrap.initialize");
        }
        Self { lib, shutdown }
    }

    #[inline(always)]
    pub fn shutdown(self) {
        unsafe {
            (self.shutdown)();
        }
    }
}
