use std::future::Future;
use std::mem::MaybeUninit;
mod audio;
use audio::NativeAudioEngine;
use log::*;
use parking_lot::RwLock;
mod input;
mod userlib;
use std::ffi::CStr;
use std::sync::Arc;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use windows::Win32::Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM};
use windows::Win32::Graphics::Gdi::MapWindowPoints;
use windows::Win32::System::Com::{CoInitializeEx, CoUninitialize, COINIT, COINIT_MULTITHREADED};
use windows::Win32::System::LibraryLoader::GetModuleHandleA;
use windows::Win32::UI::HiDpi::{SetProcessDpiAwareness, PROCESS_SYSTEM_DPI_AWARE};
use windows::Win32::UI::WindowsAndMessaging::{
    AdjustWindowRectEx, CreateWindowExA, DefWindowProcA, DispatchMessageA, GetClientRect,
    GetWindowLongPtrA, LoadCursorW, PeekMessageA, PostQuitMessage, RegisterClassExA,
    SetWindowLongPtrA, ShowWindow, TranslateMessage, CW_USEDEFAULT, GWLP_USERDATA, IDC_ARROW,
    PM_REMOVE, SW_SHOWNORMAL, WINDOW_LONG_PTR_INDEX, WM_DESTROY, WM_INPUT, WM_QUIT, WM_SIZE,
    WNDCLASSEXA, WS_EX_APPWINDOW, WS_EX_NOREDIRECTIONBITMAP, WS_OVERLAPPEDWINDOW,
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

    #[inline]
    pub fn set_window_long_ptr(&mut self, index: WINDOW_LONG_PTR_INDEX, long: isize) -> isize {
        unsafe { SetWindowLongPtrA(self.0, index, long) }
    }
}

pub struct GameDriver {
    base: peridot::Engine<'static, NativeLink>,
    _snd: NativeAudioEngine,
    current_size: peridot::math::Vector2<u32>,
    ri_handler: self::input::RawInputHandler,
    event_sender: async_std::channel::Sender<peridot::EngineEvent>,
}

static USERCODE_WAKER_VTABLE: core::task::RawWakerVTable = core::task::RawWakerVTable::new(
    |ptr| core::task::RawWaker::new(ptr, &USERCODE_WAKER_VTABLE),
    |_| {},
    |_| {},
    |_| {},
);

#[async_std::main]
async fn main() {
    let fmt = tracing_subscriber::fmt::layer().pretty();
    let filter = tracing_subscriber::filter::EnvFilter::from_default_env();
    tracing_subscriber::registry().with(fmt).with(filter).init();

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

    unsafe {
        ShowWindow(w, SW_SHOWNORMAL);
    }

    let w = Arc::new(RwLock::new(ThreadsafeWindowOps(w)));
    let io_reactor_thread = peridot::native_io::windows::spawn_io_reactor_thread();

    // Resizeをここに入れると詰まるので対策が必要（結局個別のイベントバスになるのか.......
    let (events_sender, events_receiver) = async_std::channel::unbounded::<peridot::EngineEvent>();
    let (_frame_timing_sender, frame_timing_receiver) = async_std::channel::bounded::<()>(1);
    let events_sender_th = events_sender.clone();

    let event_queue = core::pin::pin!(peridot::EventQueue::new());
    let event_queue_lifetime_extended: &'static peridot::EventQueue =
        unsafe { &*(&*event_queue as *const _) };
    let mut usercode_thread = core::pin::pin!(async move {
        let nl = NativeLink {
            al: AssetProvider::new(),
            window: w.clone(),
        };
        let mut base = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            bedrock::vk::VkPhysicalDeviceFeatures {
                ..unsafe { core::mem::MaybeUninit::zeroed().assume_init() }
            },
            (events_sender_th.clone(), events_receiver),
            frame_timing_receiver,
            event_queue_lifetime_extended,
        );
        let ri_handler = self::input::RawInputHandler::init();
        base.input_mut()
            .set_nativelink(Box::new(self::input::NativeInputHandler::new(w.clone())));
        base.post_init();
        let _snd =
            NativeAudioEngine::new(base.audio_mixer().clone()).expect("Initializing AudioEngine");

        let mut driver = GameDriver {
            base,
            _snd,
            current_size: peridot::math::Vector2(640, 480),
            ri_handler,
            event_sender: events_sender_th,
        };
        w.write()
            .set_window_long_ptr(GWLP_USERDATA, &mut driver as *mut GameDriver as _);

        userlib::game_main(&mut driver.base).await;
    });

    while process_message_all() {
        event_queue.enqueue(peridot::Event::NextFrame);

        let waker = unsafe {
            core::task::Waker::from_raw(core::task::RawWaker::new(
                core::ptr::null(),
                &USERCODE_WAKER_VTABLE,
            ))
        };
        let _ = usercode_thread
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&waker));
    }

    event_queue.enqueue(peridot::Event::Shutdown);
    loop {
        let waker = unsafe {
            core::task::Waker::from_raw(core::task::RawWaker::new(
                core::ptr::null(),
                &USERCODE_WAKER_VTABLE,
            ))
        };
        if usercode_thread
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&waker))
            .is_ready()
        {
            break;
        }
    }

    drop(io_reactor_thread);
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
            let size = peridot::math::Vector2(w as u32, h as u32);
            if driver.current_size != size {
                driver.current_size = size.clone();
                async_std::task::spawn(
                    driver.event_sender.send(peridot::EngineEvent::Resize(size)),
                );
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
    type AssetBlob<'a> = peridot::native_io::windows::NativeFileBlobRandomReader;
    type AssetBlobAsync<'a> = peridot::native_io::windows::NativeFileBlobAsyncRandomReader;
    type StreamingAsset<'a> = std::fs::File;

    #[tracing::instrument(name = "AssetProvider::get", skip(self))]
    fn get<'a>(&'a self, path: &str, ext: &str) -> std::io::Result<Self::AssetBlob<'a>> {
        #[allow(unused_mut)]
        let mut segments = path.split('.').peekable();

        #[cfg(feature = "IterationBuild")]
        if segments.peek().map_or(false, |&s| s == "builtin") {
            let _ = segments.next();

            let mut p = self.builtin_assets_base.clone();
            p.extend(segments);
            p.set_extension(ext);
            tracing::debug!(realpath = ?p, "Loading Builtin Asset");

            return peridot::native_io::windows::NativeFileBlobAsyncRandomReader::open(&p);
        }

        let mut p = self.base.clone();
        p.extend(segments);
        p.set_extension(ext);
        tracing::debug!(realpath = ?p, "Loading Asset");

        peridot::native_io::windows::NativeFileBlobRandomReader::open(&p)
    }

    #[tracing::instrument(name = "AssetProvider(windows)::get_async", skip(self))]
    fn get_async<'a>(
        &'a self,
        path: &str,
        ext: &str,
    ) -> impl core::future::Future<Output = std::io::Result<Self::AssetBlobAsync<'a>>> {
        async move {
            #[allow(unused_mut)]
            let mut segments = path.split('.').peekable();

            #[cfg(feature = "IterationBuild")]
            if segments.peek().map_or(false, |&s| s == "builtin") {
                let _ = segments.next();

                let mut p = self.builtin_assets_base.clone();
                p.extend(segments);
                p.set_extension(ext);
                tracing::debug!(realpath = ?p, "Loading Builtin Asset");

                return peridot::native_io::windows::NativeFileBlobAsyncRandomReader::open(&p);
            }

            let mut p = self.base.clone();
            p.extend(segments);
            p.set_extension(ext);
            tracing::debug!(realpath = ?p, "Loading Asset");

            peridot::native_io::windows::NativeFileBlobAsyncRandomReader::open(&p)
        }
    }

    fn get_streaming<'a>(
        &'a self,
        path: &str,
        ext: &str,
    ) -> std::io::Result<Self::StreamingAsset<'a>> {
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
    window: Arc<RwLock<ThreadsafeWindowOps>>,
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = AssetProvider;
    type Presenter = Presenter;

    #[cfg(not(feature = "transparent"))]
    fn instance_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_surface", c"VK_KHR_win32_surface"]
    }
    #[cfg(feature = "transparent")]
    fn instance_extensions(&self) -> Vec<&CStr> {
        vec![
            c"VK_KHR_external_memory_capabilities",
            c"VK_KHR_external_semaphore_capabilities",
        ]
    }
    #[cfg(not(feature = "transparent"))]
    fn device_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_swapchain"]
    }
    #[cfg(feature = "transparent")]
    fn device_extensions(&self) -> Vec<&CStr> {
        vec![
            c"VK_KHR_external_memory",
            c"VK_KHR_external_memory_win32",
            c"VK_KHR_external_semaphore",
            c"VK_KHR_external_semaphore_win32",
        ]
    }

    fn asset_loader(&self) -> &AssetProvider {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(g, self.window.clone())
    }
}
