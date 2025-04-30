use std::future::Future;
use std::mem::MaybeUninit;
mod audio;
use audio::NativeAudioEngine;
use log::*;
use parking_lot::RwLock;
mod input;
mod userlib;
use bedrock as br;
use presenter::{DisplayDeviceMode, DisplayDeviceTopologyCache};
use std::ffi::CStr;
use std::sync::Arc;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use windows::core::PCWSTR;
use windows::Win32::Foundation::{BOOL, HINSTANCE, HWND, LPARAM, LRESULT, POINT, RECT, WPARAM};
use windows::Win32::Graphics::Gdi::{
    EnumDisplayDevicesW, EnumDisplayMonitors, GetMonitorInfoW, MapWindowPoints, DISPLAY_DEVICEW,
    HMONITOR, MONITORINFOEXW,
};
use windows::Win32::System::Com::{CoInitializeEx, CoUninitialize, COINIT, COINIT_MULTITHREADED};
use windows::Win32::System::LibraryLoader::GetModuleHandleA;
use windows::Win32::UI::HiDpi::{
    SetProcessDpiAwareness, PROCESS_PER_MONITOR_DPI_AWARE, PROCESS_SYSTEM_DPI_AWARE,
};
use windows::Win32::UI::WindowsAndMessaging::{
    AdjustWindowRectEx, CreateWindowExA, DefWindowProcA, DispatchMessageA, GetClientRect,
    GetWindowLongPtrA, LoadCursorW, PeekMessageA, PostQuitMessage, RegisterClassExA,
    SetWindowLongPtrA, ShowWindow, TranslateMessage, CW_USEDEFAULT, EDD_GET_DEVICE_INTERFACE_NAME,
    GWLP_USERDATA, IDC_ARROW, PM_REMOVE, SHOW_WINDOW_CMD, SHOW_WINDOW_STATUS, SW_SHOWNORMAL,
    WINDOW_LONG_PTR_INDEX, WM_DESTROY, WM_INPUT, WM_QUIT, WM_SIZE, WNDCLASSEXA, WS_EX_APPWINDOW,
    WS_EX_NOREDIRECTIONBITMAP, WS_OVERLAPPEDWINDOW,
};

mod presenter;
use self::presenter::Presenter;

const LPSZCLASSNAME: &'static str = "mainWindow\0";

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
    pub fn show(&mut self, mode: SHOW_WINDOW_CMD) {
        unsafe {
            let _ = ShowWindow(self.0, mode);
        }
    }

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

#[derive(Debug, thiserror::Error)]
pub enum LocalPreferencesLoadError {
    #[error("IO Error: {0}")]
    IO(#[from] std::io::Error),
    #[error("Parse Error: {0}")]
    TOML(#[from] toml::de::Error),
}

#[derive(Debug, thiserror::Error)]
pub enum LocalPreferencesStoreError {
    #[error("IO Error: {0}")]
    IO(#[from] std::io::Error),
    #[error("Serialize Error: {0}")]
    TOML(#[from] toml::ser::Error),
}

pub struct LocalPreferencesFile {
    path: Option<PathBuf>,
}
impl LocalPreferencesFile {
    pub fn new() -> Self {
        Self {
            path: match std::env::var_os("LOCALAPPDATA") {
                // TODO: あとでプロジェクトのappidとかを考慮したパスにする
                Some(x) => Some(PathBuf::from(x).join("peridot/preferences.toml")),
                None => {
                    tracing::warn!("No LOCALAPPDATA environment variable set. any changes to preferences will be discarded.");

                    None
                }
            },
        }
    }

    pub fn exists(&self) -> bool {
        self.path.as_deref().map_or(false, std::path::Path::exists)
    }

    pub fn load(&self) -> Result<Option<peridot::EnginePreferences>, LocalPreferencesLoadError> {
        let Some(ref p) = self.path else {
            return Ok(None);
        };

        Ok(Some(toml::from_str(&std::fs::read_to_string(p)?)?))
    }

    pub fn store(
        &self,
        data: &peridot::EnginePreferences,
    ) -> Result<(), LocalPreferencesStoreError> {
        let Some(ref p) = self.path else {
            return Ok(());
        };

        std::fs::write(p, toml::to_string(data)?)?;
        Ok(())
    }
}

#[async_std::main]
async fn main() {
    let fmt = tracing_subscriber::fmt::layer().pretty();
    let filter = tracing_subscriber::filter::EnvFilter::from_default_env();
    tracing_subscriber::registry().with(fmt).with(filter).init();

    let _co = CoScopeGuard::init(COINIT_MULTITHREADED).expect("Initializing COM");

    unsafe {
        SetProcessDpiAwareness(PROCESS_PER_MONITOR_DPI_AWARE).expect("Failed to set dpi awareness");
    }

    let display_device_topology =
        peridot::mthelper::SharedRef::new(DisplayDeviceTopologyCache::new());

    let local_preferences_file = LocalPreferencesFile::new();
    let local_preferences = if !local_preferences_file.exists() {
        None
    } else {
        match local_preferences_file.load() {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to load local preferences");

                None
            }
        }
    };
    let local_preferences = local_preferences.unwrap_or_else(|| {
        let default = peridot::EnginePreferences {
            presentation: {
                let primary = display_device_topology.primary();
                let max_resolution_data = primary.and_then(|xs| {
                    xs.available_modes
                        .iter()
                        .fold(None::<&DisplayDeviceMode>, |a, x| {
                            let needs_update = a.is_none_or(|ax| {
                                let pixel_count = x.width_px * x.height_px;
                                let a_pixel_count = ax.width_px * ax.height_px;

                                pixel_count > a_pixel_count
                                    || (pixel_count == a_pixel_count
                                        && x.refresh_rate >= ax.refresh_rate)
                            });

                            if needs_update {
                                Some(x)
                            } else {
                                a
                            }
                        })
                });

                peridot::PresentationPreferences::Fullscreen {
                    display_index: 0,
                    desired_resolution_width: max_resolution_data.map_or(1280, |x| x.width_px),
                    desired_resolution_height: max_resolution_data.map_or(720, |x| x.height_px),
                    desired_refresh_rate: max_resolution_data.map_or(60.0, |x| x.refresh_rate as _),
                    matching_behavior: peridot::ResolutionMatchingBehavior::Nearest,
                }
            },
        };

        if let Err(e) = local_preferences_file.store(&default) {
            tracing::error!(cause = ?e, "Failed to store local preferences");
        }

        default
    });

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
            display_device_topology,
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
            event_queue_lifetime_extended,
            &local_preferences,
        );
        let presenter_window = base.presenter().window.clone();
        let ri_handler = self::input::RawInputHandler::init();
        base.input_mut()
            .set_nativelink(Box::new(self::input::NativeInputHandler::new(
                presenter_window.clone(),
            )));
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
        presenter_window
            .write()
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
    display_device_topology: peridot::mthelper::SharedRef<DisplayDeviceTopologyCache>,
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = AssetProvider;
    type Presenter = Presenter;

    #[cfg(not(feature = "transparent"))]
    fn instance_extensions(&self) -> Vec<&CStr> {
        vec![
            c"VK_KHR_surface",
            c"VK_KHR_win32_surface",
            c"VK_KHR_display",
            c"VK_KHR_get_physical_device_properties2",
            c"VK_KHR_external_fence_capabilities",
            c"VK_KHR_get_surface_capabilities2",
            // TODO: これoptionalにしたいので機能拡張が必要
            c"VK_EXT_direct_mode_display",
        ]
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
        // TODO: VK_NV_acquire_winrt_displayをoptionalにしたいので機能拡張が必要
        vec![
            c"VK_KHR_swapchain",
            c"VK_NV_acquire_winrt_display",
            c"VK_EXT_full_screen_exclusive",
        ]
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
    fn new_presenter(
        &self,
        g: &peridot::Graphics,
        prefs: &peridot::PresentationPreferences,
    ) -> Presenter {
        Presenter::new(g, prefs, &self.display_device_topology)
    }
}
