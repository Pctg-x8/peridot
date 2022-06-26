use peridot::FeatureRequests;
use std::io::{Error as IOError, Result as IOResult};
use winapi::shared::minwindef::{DWORD, HINSTANCE, HIWORD, LOWORD, LPARAM, LRESULT, UINT, WPARAM};
use winapi::shared::minwindef::{HINSTANCE, HIWORD, LOWORD, LPARAM, LRESULT, UINT, WPARAM};
use winapi::shared::windef::{HWND, POINT, RECT};
use winapi::shared::windef::{HWND, RECT};
use winapi::shared::winerror::{HRESULT, SUCCEEDED};
use winapi::um::combaseapi::{CoInitializeEx, CoUninitialize};
use winapi::um::libloaderapi::GetModuleHandleA;
use winapi::um::libloaderapi::GetModuleHandleA;
use winapi::um::objbase::COINIT_MULTITHREADED;
use winapi::um::shellscalingapi::{SetProcessDpiAwareness, PROCESS_SYSTEM_DPI_AWARE};
use winapi::um::winuser::{
    AdjustWindowRectEx, CreateWindowExA, DefWindowProcA, DispatchMessageA, GetClientRect,
    GetWindowLongPtrA, LoadCursorA, MapWindowPoints, PeekMessageA, PostQuitMessage,
    RegisterClassExA, SetWindowLongPtrA, ShowWindow, TranslateMessage, CW_USEDEFAULT,
    GWLP_USERDATA, IDC_ARROW, PM_REMOVE, SW_SHOWNORMAL, WM_INPUT, WM_SIZE, WNDCLASSEXA,
    WS_EX_APPWINDOW, WS_OVERLAPPEDWINDOW,
};
use winapi::um::winuser::{WM_DESTROY, WM_QUIT};

use std::mem::MaybeUninit;
mod audio;
use audio::NativeAudioEngine;
use log::*;
mod input;
mod userlib;
use peridot::{EngineEvents, FeatureRequests};

mod presenter;
use self::presenter::Presenter;

#[cfg(not(feature = "mt"))]
use std::rc::Rc as SharedPtr;
#[cfg(feature = "mt")]
use std::sync::Arc as SharedPtr;

const LPSZCLASSNAME: &'static str = concat!(env!("PERIDOT_WINDOWS_APPID"), ".mainWindow\0");

fn module_handle() -> HINSTANCE {
    unsafe { GetModuleHandleA(std::ptr::null()) }
}
pub(crate) fn hr_into_result(hr: HRESULT) -> IOResult<()> {
    if SUCCEEDED(hr) {
        Ok(())
    } else {
        Err(IOError::from_raw_os_error(hr))
    }
}
struct CoScopeGuard;
impl CoScopeGuard {
    fn init(apartment: DWORD) -> IOResult<Self> {
        unsafe {
            hr_into_result(CoInitializeEx(std::ptr::null_mut(), apartment)).map(|_| CoScopeGuard)
        }
    }
}
impl Drop for CoScopeGuard {
    fn drop(&mut self) {
        unsafe { CoUninitialize() }
    }
}

pub struct ThreadsafeWindowOps(HWND);
unsafe impl Sync for ThreadsafeWindowOps {}
unsafe impl Send for ThreadsafeWindowOps {}
impl ThreadsafeWindowOps {
    pub fn map_point_from_desktop(&self, p: &mut POINT) {
        unsafe {
            MapWindowPoints(std::ptr::null_mut(), self.0, p, 1);
        }
    }

    pub fn get_client_rect(&self) -> RECT {
        let mut rc = std::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(self.0, rc.as_mut_ptr());
            rc.assume_init()
        }
    }
}

pub struct GameDriver {
    base: peridot::Engine<NativeLink>,
    usercode: userlib::Game<NativeLink>,
    _snd: NativeAudioEngine,
    current_size: peridot::math::Vector2<usize>,
    ri_handler: self::input::RawInputHandler,
}
impl GameDriver {
    fn new(window: HWND, init_size: peridot::math::Vector2<usize>) -> Self {
        let window = SharedPtr::new(ThreadsafeWindowOps(window));

        let nl = NativeLink {
            al: AssetProvider::new(),
            window: window.clone(),
        };
        let mut base = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            userlib::Game::<NativeLink>::requested_features(),
        );
        let usercode = userlib::Game::init(&mut base);
        let ri_handler = self::input::RawInputHandler::init();
        base.input_mut()
            .set_nativelink(Box::new(self::input::NativeInputHandler::new(
                window.clone(),
            )));
        base.postinit();
        let _snd =
            NativeAudioEngine::new(base.audio_mixer().clone()).expect("Initializing AudioEngine");

        /*let mut ap = PSGSine::new();
        ap.set_amp(1.0 / 32.0); ap.set_osc_hz(440.0);
        e.audio_mixer().write().expect("Adding PSGSine").add_process(Arc::new(RwLock::new(ap)));
        let mut ap2 = PSGSine::new();
        ap2.set_amp(1.0 / 32.0); ap2.set_osc_hz(882.0);
        e.audio_mixer().write().expect("Adding PSGSine").add_process(Arc::new(RwLock::new(ap2)));*/

        GameDriver {
            base,
            usercode,
            _snd,
            current_size: init_size,
            ri_handler,
        }
    }

    fn update(&mut self) {
        self.base.do_update(&mut self.usercode);
    }
    fn resize(&mut self, size: peridot::math::Vector2<usize>) {
        self.base.do_resize_backbuffer(size, &mut self.usercode);
    }
}

fn main() {
    env_logger::init();
    let _co = CoScopeGuard::init(COINIT_MULTITHREADED).expect("Initializing COM");

    unsafe {
        SetProcessDpiAwareness(PROCESS_SYSTEM_DPI_AWARE);
    }

    let wca = WNDCLASSEXA {
        cbSize: std::mem::size_of::<WNDCLASSEXA>() as _,
        hInstance: module_handle(),
        lpszClassName: LPSZCLASSNAME.as_ptr() as *const _,
        lpfnWndProc: Some(window_callback),
        hCursor: unsafe { LoadCursorA(std::ptr::null_mut(), IDC_ARROW as _) },
        ..unsafe { MaybeUninit::zeroed().assume_init() }
    };
    let wcatom = unsafe { RegisterClassExA(&wca) };
    if wcatom <= 0 {
        panic!("Register Class Failed!");
    }

    let wname_c =
        std::ffi::CString::new(userlib::APP_TITLE).expect("Unable to generate a c-style string");
    let wsex = if cfg!(feature = "transparent") {
        WS_EX_APPWINDOW | winapi::um::winuser::WS_EX_NOREDIRECTIONBITMAP
    } else {
        WS_EX_APPWINDOW
    };
    let (w, h, style) = match GameW::WINDOW_EXTENTS {
        peridot::WindowExtents::Fixed(w, h) => (
            w,
            h,
            WS_OVERLAPPED | WS_CAPTION | WS_BORDER | WS_SYSMENU | WS_MINIMIZEBOX,
        ),
        peridot::WindowExtents::Resizable(w, h) => (w, h, WS_OVERLAPPEDWINDOW),
        // TODO: for fullscreen window generation
        peridot::WindowExtents::Fullscreen => {
            unimplemented!("todo for full-screen window generation")
        }
    };
    let mut wrect = RECT {
        left: 0,
        top: 0,
        right: w as _,
        bottom: h as _,
    };
    unsafe {
        AdjustWindowRectEx(&mut wrect, style, false as _, WS_EX_APPWINDOW);
    }
    let w = unsafe {
        CreateWindowExA(
            wsex,
            wcatom as _,
            wname_c.as_ptr(),
            style,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            wrect.right - wrect.left,
            wrect.bottom - wrect.top,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
            wca.hInstance,
            std::ptr::null_mut(),
        )
    };
    if w.is_null() {
        panic!("Create Window Failed!");
    }

    let mut driver = GameDriver::new(w, peridot::math::Vector2(640, 480));
    unsafe {
        SetWindowLongPtrA(w, GWLP_USERDATA, &mut driver as *mut GameDriver as _);
    }
    unsafe {
        ShowWindow(w, SW_SHOWNORMAL);
    }

    while process_message_all() {
        driver.update();
    }
}
extern "system" fn window_callback(w: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    match msg {
        WM_DESTROY => unsafe {
            PostQuitMessage(0);
            return 0;
        },
        WM_SIZE => unsafe {
            let p = GetWindowLongPtrA(w, GWLP_USERDATA) as *mut GameDriver;
            if let Some(driver) = p.as_mut() {
                let (w, h) = (LOWORD(lparam as _), HIWORD(lparam as _));
                let size = peridot::math::Vector2(w as usize, h as usize);
                if driver.current_size != size {
                    driver.current_size = size.clone();
                    driver.resize(size);
                    driver.update();
                }
            }
            return 0;
        },
        WM_INPUT => {
            let p = unsafe { GetWindowLongPtrA(w, GWLP_USERDATA) as *mut GameDriver };
            if let Some(driver) = unsafe { p.as_mut() } {
                driver
                    .ri_handler
                    .handle_wm_input(driver.base.input_mut(), lparam);
            }
            0
        }
        _ => unsafe { DefWindowProcA(w, msg, wparam, lparam) },
    }
}

fn process_message_all() -> bool {
    let mut msg = MaybeUninit::uninit();
    while unsafe { PeekMessageA(msg.as_mut_ptr(), std::ptr::null_mut(), 0, 0, PM_REMOVE) != 0 } {
        if unsafe { (*msg.as_ptr()).message } == WM_QUIT {
            return false;
        }
        unsafe {
            TranslateMessage(msg.as_mut_ptr());
            DispatchMessageA(msg.as_mut_ptr());
        }
    }

    true
}

use std::path::PathBuf;

struct AssetProvider {
    base: PathBuf,
    #[cfg(feature = "IterationBuild")]
    builtin_assets_base: PathBuf,
}
impl AssetProvider {
    fn new() -> Self {
        #[cfg(feature = "UseExternalAssetPath")]
        let base = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))]
        let base = {
            let mut exe =
                std::env::current_exe().expect("Unable to determine the location of exe file");
            exe.pop();
            exe.push("/assets");
            exe
        };
        trace!("Asset BaseDirectory={}", base.display());
        AssetProvider {
            base,
            #[cfg(feature = "IterationBuild")]
            builtin_assets_base: PathBuf::from(env!("PERIDOT_BUILTIN_ASSET_PATH")),
        }
    }
}
impl peridot::PlatformAssetLoader for AssetProvider {
    type Asset = std::fs::File;
    type StreamingAsset = std::fs::File;

    fn get(&self, path: &str, ext: &str) -> std::io::Result<Self::Asset> {
        #[allow(unused_mut)]
        let mut segments = path.split('.').peekable();

        #[cfg(feature = "IterationBuild")]
        if segments.peek().map_or(false, |&s| s == "builtin") {
            let _ = segments.next();

            let mut p = self.builtin_assets_base.clone();
            p.extend(segments);
            p.set_extension(ext);
            log::debug!("Loading Builtin Asset: {:?}", p);

            return std::fs::File::open(&p);
        }

        let mut p = self.base.clone();
        p.extend(segments);
        p.set_extension(ext);
        log::debug!("Loading Asset: {:?}", p);

        std::fs::File::open(&p)
    }
    fn get_streaming(&self, path: &str, ext: &str) -> std::io::Result<Self::StreamingAsset> {
        #[allow(unused_mut)]
        let mut segments = path.split('.').peekable();

        #[cfg(feature = "IterationBuild")]
        if segments.peek().map_or(false, |&s| s == "builtin") {
            let _ = segments.next();

            let mut p = self.builtin_assets_base.clone();
            p.extend(segments);
            p.set_extension(ext);

            return std::fs::File::open(&p);
        }

        let mut p = self.base.clone();
        p.extend(segments);
        p.set_extension(ext);

        std::fs::File::open(&p)
    }
}

struct NativeLink {
    al: AssetProvider,
    window: SharedPtr<ThreadsafeWindowOps>,
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = AssetProvider;
    type Presenter = Presenter;

    #[cfg(not(feature = "transparent"))]
    fn instance_extensions(&self) -> Vec<&str> {
        vec!["VK_KHR_surface", "VK_KHR_win32_surface"]
    }
    #[cfg(feature = "transparent")]
    fn instance_extensions(&self) -> Vec<&str> {
        vec![]
    }
    #[cfg(not(feature = "transparent"))]
    fn device_extensions(&self) -> Vec<&str> {
        vec!["VK_KHR_swapchain"]
    }
    #[cfg(feature = "transparent")]
    fn device_extensions(&self) -> Vec<&str> {
        vec![
            "VK_KHR_external_memory_win32",
            "VK_KHR_external_semaphore_win32",
        ]
    }

    fn asset_loader(&self) -> &AssetProvider {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(g, self.window.clone())
    }
}
