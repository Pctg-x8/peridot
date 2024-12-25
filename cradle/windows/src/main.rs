use std::mem::MaybeUninit;
mod audio;
use audio::NativeAudioEngine;
use log::*;
use parking_lot::RwLock;
mod input;
mod userlib;
use peridot::mthelper::{DynamicMutabilityProvider, SharedMutableRef, SharedRef};
use peridot::{EngineEvent, EngineEvents, FeatureRequests};
use std::ffi::CStr;
use std::sync::Arc;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use windows::Win32::Foundation::{
    HANDLE, HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WAIT_OBJECT_0, WAIT_TIMEOUT, WPARAM,
};
use windows::Win32::Graphics::Gdi::MapWindowPoints;
use windows::Win32::System::Com::{CoInitializeEx, CoUninitialize, COINIT, COINIT_MULTITHREADED};
use windows::Win32::System::LibraryLoader::GetModuleHandleA;
use windows::Win32::System::Threading::{
    AvRevertMmThreadCharacteristics, AvSetMmThreadCharacteristicsA, Sleep,
};
use windows::Win32::UI::HiDpi::{SetProcessDpiAwareness, PROCESS_SYSTEM_DPI_AWARE};
use windows::Win32::UI::WindowsAndMessaging::{
    AdjustWindowRectEx, CreateWindowExA, DefWindowProcA, DispatchMessageA, GetClientRect,
    GetWindowLongPtrA, LoadCursorW, MsgWaitForMultipleObjectsEx, PeekMessageA, PostQuitMessage,
    RegisterClassExA, SetWindowLongPtrA, ShowWindow, TranslateMessage, CW_USEDEFAULT,
    GWLP_USERDATA, IDC_ARROW, MSG_WAIT_FOR_MULTIPLE_OBJECTS_EX_FLAGS, PM_REMOVE, QS_ALLINPUT,
    SW_SHOWNORMAL, WINDOW_LONG_PTR_INDEX, WM_DESTROY, WM_INPUT, WM_QUIT, WM_SIZE, WNDCLASSEXA,
    WS_EX_APPWINDOW, WS_EX_NOREDIRECTIONBITMAP, WS_OVERLAPPEDWINDOW,
};

mod presenter;
use self::presenter::Presenter;

const LPSZCLASSNAME: &'static str = "mainWindow\0";

const fn loword(dw: usize) -> u16 {
    (dw & 0xffff) as _
}
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
    base: peridot::Engine<NativeLink>,
    _snd: NativeAudioEngine,
    current_size: peridot::math::Vector2<u32>,
    ri_handler: self::input::RawInputHandler,
    event_sender: async_std::channel::Sender<peridot::EngineEvent>,
}

// non-sendable
pub struct AvrtHandle(HANDLE, core::marker::PhantomData<*mut u8>);
impl AvrtHandle {
    pub fn set_mm_thread_characteristics(
        name: windows::core::PCSTR,
        task_index: Option<u32>,
    ) -> windows::core::Result<Self> {
        let mut ti = task_index.unwrap_or(0);
        let h = unsafe { AvSetMmThreadCharacteristicsA(name, &mut ti)? };

        Ok(Self(h, core::marker::PhantomData))
    }
}
impl Drop for AvrtHandle {
    fn drop(&mut self) {
        if let Err(e) = unsafe { AvRevertMmThreadCharacteristics(self.0) } {
            tracing::warn!(cause = ?e, "Failed to revert mm-thread characteristics");
        }
    }
}

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

    // Resizeをここに入れると詰まるので対策が必要（結局個別のイベントバスになるのか.......
    let (events_sender, events_receiver) = async_std::channel::unbounded::<peridot::EngineEvent>();
    let (frame_timing_sender, frame_timing_receiver) = async_std::channel::bounded::<()>(1);
    let events_sender_th = events_sender.clone();

    let thread = async_std::task::spawn(async move {
        let nl = NativeLink {
            al: AssetProvider::new(),
            window: w.clone(),
        };
        let mut base = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            bedrock::vk::VkPhysicalDeviceFeatures {
                ..Default::default()
            },
            (events_sender_th.clone(), events_receiver),
            frame_timing_receiver,
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

    let _task_handle = match AvrtHandle::set_mm_thread_characteristics(
        windows::core::s!("Games"),
        None,
    ) {
        Ok(h) => Some(h),
        Err(e) => {
            tracing::warn!(cause = ?e, "Failed to set mm-thread characteristics (performance will be degraded)");
            None
        }
    };

    let mut msg = MaybeUninit::uninit();
    let mut nextframe_deadline = std::time::Instant::now();
    'app: loop {
        let r = unsafe {
            MsgWaitForMultipleObjectsEx(
                None,
                // 精度がミリ秒までしかないので、1ms早めに切り上げて残りはスピンで時間を合わせる
                nextframe_deadline
                    .duration_since(std::time::Instant::now())
                    .as_millis()
                    .saturating_sub(1) as _,
                QS_ALLINPUT,
                MSG_WAIT_FOR_MULTIPLE_OBJECTS_EX_FLAGS(0),
            )
        };

        if r == WAIT_TIMEOUT {
            // timeout
            while std::time::Instant::now() < nextframe_deadline {
                // ミリ秒以下の残りを待つ
                let r = unsafe { PeekMessageA(msg.as_mut_ptr(), None, 0, 0, PM_REMOVE).as_bool() };
                if !r {
                    continue;
                }

                if unsafe { (*msg.as_ptr()).message } == WM_QUIT {
                    break 'app;
                }

                unsafe {
                    TranslateMessage(msg.as_ptr());
                    DispatchMessageA(msg.as_ptr());
                }
            }

            match frame_timing_sender.try_send(()) {
                Ok(_) => (),
                // frame drop
                Err(async_std::channel::TrySendError::Full(_)) => (),
                Err(async_std::channel::TrySendError::Closed(_)) => {
                    // events bus gone
                    break;
                }
            }

            nextframe_deadline =
                std::time::Instant::now() + std::time::Duration::from_secs_f64(1.0 / 60.0);
        } else if r == WAIT_OBJECT_0 {
            // message
            while unsafe { PeekMessageA(msg.as_mut_ptr(), None, 0, 0, PM_REMOVE).as_bool() } {
                if unsafe { (*msg.as_ptr()).message } == WM_QUIT {
                    break 'app;
                }

                unsafe {
                    TranslateMessage(msg.as_ptr());
                    DispatchMessageA(msg.as_ptr());
                }
            }
        } else {
            // yield to system
            unsafe {
                Sleep(0);
            }
        }
    }

    drop(_task_handle);

    if events_sender
        .send(peridot::EngineEvent::Shutdown)
        .await
        .is_ok()
    {
        thread.await;
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
                .handle_wm_input(driver.base.input_event_dispatcher(), unsafe {
                    core::mem::transmute(lparam)
                });
        }

        return LRESULT(0);
    }

    unsafe { DefWindowProcA(w, msg, wparam, lparam) }
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
        vec![]
    }
    #[cfg(not(feature = "transparent"))]
    fn device_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_swapchain"]
    }
    #[cfg(feature = "transparent")]
    fn device_extensions(&self) -> Vec<&CStr> {
        vec![
            c"VK_KHR_external_memory_win32",
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
