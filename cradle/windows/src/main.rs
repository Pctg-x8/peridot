use std::mem::MaybeUninit;
mod audio;
use audio::NativeAudioEngine;
use log::*;
mod input;
mod userlib;
use peridot::mthelper::SharedRef;
use peridot::{EngineEvents, FeatureRequests};
use windows::Win32::Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM};
use windows::Win32::Graphics::Gdi::MapWindowPoints;
use windows::Win32::System::Com::{CoInitializeEx, CoUninitialize, COINIT, COINIT_MULTITHREADED};
use windows::Win32::System::LibraryLoader::GetModuleHandleA;
use windows::Win32::UI::HiDpi::{SetProcessDpiAwareness, PROCESS_SYSTEM_DPI_AWARE};
use windows::Win32::UI::WindowsAndMessaging::{
    AdjustWindowRectEx, CreateWindowExA, DefWindowProcA, DispatchMessageA, GetClientRect,
    GetWindowLongPtrA, LoadCursorW, PeekMessageA, PostQuitMessage, RegisterClassExA,
    SetWindowLongPtrA, ShowWindow, TranslateMessage, CW_USEDEFAULT, GWLP_USERDATA, IDC_ARROW,
    PM_REMOVE, SW_SHOWNORMAL, WM_DESTROY, WM_INPUT, WM_QUIT, WM_SIZE, WNDCLASSEXA, WS_EX_APPWINDOW,
    WS_EX_NOREDIRECTIONBITMAP, WS_OVERLAPPEDWINDOW,
};

mod presenter;
use self::presenter::Presenter;

const LPSZCLASSNAME: &'static str = "mainWindow\0";

#[inline]
const fn loword(dw: usize) -> u16 {
    (dw & 0xffff) as _
}
#[inline]
const fn hiword(dw: usize) -> u16 {
    ((dw >> 16) & 0xffff) as _
}

#[inline]
fn module_handle() -> HINSTANCE {
    unsafe { core::mem::transmute(GetModuleHandleA(None).expect("Failed to get module handle")) }
}

struct CoScopeGuard;
impl CoScopeGuard {
    fn init(apartment: COINIT) -> windows::core::Result<Self> {
        unsafe { CoInitializeEx(None, apartment).map(|_| Self) }
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
    #[inline]
    pub fn map_points_from_desktop(&self, p: &mut [POINT]) {
        unsafe {
            MapWindowPoints(None, self.0, p);
        }
    }

    #[inline]
    pub fn get_client_rect(&self) -> RECT {
        let mut rc = std::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(self.0, rc.as_mut_ptr()).expect("Failed to get client rect");
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
        let window = SharedRef::new(ThreadsafeWindowOps(window));

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
        base.post_init();
        let _snd =
            NativeAudioEngine::new(base.audio_mixer().clone()).expect("Initializing AudioEngine");

        /*let mut ap = PSGSine::new();
        ap.set_amp(1.0 / 32.0); ap.set_osc_hz(440.0);
        e.audio_mixer().write().expect("Adding PSGSine").add_process(Arc::new(RwLock::new(ap)));
        let mut ap2 = PSGSine::new();
        ap2.set_amp(1.0 / 32.0); ap2.set_osc_hz(882.0);
        e.audio_mixer().write().expect("Adding PSGSine").add_process(Arc::new(RwLock::new(ap2)));*/

        Self {
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
        self.base.do_resize_back_buffer(size, &mut self.usercode);
    }
}

fn main() {
    env_logger::init();
    let _co = CoScopeGuard::init(COINIT_MULTITHREADED).expect("Initializing COM");

    unsafe {
        SetProcessDpiAwareness(PROCESS_SYSTEM_DPI_AWARE).expect("Failed to set dpi awareness");
    }

    let wca = WNDCLASSEXA {
        cbSize: std::mem::size_of::<WNDCLASSEXA>() as _,
        hInstance: module_handle(),
        lpszClassName: windows::core::PCSTR(LPSZCLASSNAME.as_ptr() as *const _),
        lpfnWndProc: Some(window_callback),
        hCursor: unsafe { LoadCursorW(None, IDC_ARROW).expect("Failed to load default cursor") },
        ..unsafe { MaybeUninit::zeroed().assume_init() }
    };
    let wcatom = unsafe { RegisterClassExA(&wca) };
    if wcatom <= 0 {
        panic!("Register Class Failed!");
    }

    let wname_c =
        std::ffi::CString::new(userlib::APP_TITLE).expect("Unable to generate a c-style string");
    let wsex = if cfg!(feature = "transparent") {
        WS_EX_APPWINDOW | WS_EX_NOREDIRECTIONBITMAP
    } else {
        WS_EX_APPWINDOW
    };
    let style = WS_OVERLAPPEDWINDOW;
    let mut wrect = RECT {
        left: 0,
        top: 0,
        right: 640,
        bottom: 480,
    };
    unsafe {
        AdjustWindowRectEx(&mut wrect, style, false, WS_EX_APPWINDOW)
            .expect("Failed to calculate window geometry");
    }
    let w = unsafe {
        CreateWindowExA(
            wsex,
            windows::core::PCSTR(std::mem::transmute(wcatom as usize)),
            windows::core::PCSTR(wname_c.as_ptr() as _),
            style,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            wrect.right - wrect.left,
            wrect.bottom - wrect.top,
            None,
            None,
            wca.hInstance,
            None,
        )
    };
    if w.0 == 0 {
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

extern "system" fn window_callback(w: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    if msg == WM_DESTROY {
        unsafe {
            PostQuitMessage(0);
        }
        return LRESULT(0);
    }

    if msg == WM_SIZE {
        let p = unsafe { GetWindowLongPtrA(w, GWLP_USERDATA) as *mut GameDriver };
        if let Some(driver) = unsafe { p.as_mut() } {
            let (w, h) = (loword(lparam.0 as _), hiword(lparam.0 as _));
            let size = peridot::math::Vector2(w as usize, h as usize);
            if driver.current_size != size {
                driver.current_size = size.clone();
                driver.resize(size);
                driver.update();
            }
        }

        return LRESULT(0);
    }

    if msg == WM_INPUT {
        let p = unsafe { GetWindowLongPtrA(w, GWLP_USERDATA) as *mut GameDriver };
        if let Some(driver) = unsafe { p.as_mut() } {
            driver
                .ri_handler
                .handle_wm_input(driver.base.input_mut(), lparam);
        }

        return LRESULT(0);
    }

    unsafe { DefWindowProcA(w, msg, wparam, lparam) }
}

fn process_message_all() -> bool {
    let mut msg = MaybeUninit::uninit();
    while unsafe { PeekMessageA(msg.as_mut_ptr(), None, 0, 0, PM_REMOVE).as_bool() } {
        if unsafe { (*msg.as_ptr()).message } == WM_QUIT {
            return false;
        }
        unsafe {
            TranslateMessage(msg.as_ptr());
            DispatchMessageA(msg.as_ptr());
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
    window: SharedRef<ThreadsafeWindowOps>,
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
