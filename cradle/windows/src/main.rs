use peridot::FeatureRequests;
use std::io::{Error as IOError, Result as IOResult};
use winapi::shared::basetsd::INT_PTR;
use winapi::shared::minwindef::{
    ATOM, BOOL, DWORD, FALSE, HINSTANCE, HIWORD, LOWORD, LPARAM, LRESULT, TRUE, UINT, WPARAM,
};
use winapi::shared::windef::{HDC, HMONITOR, HWND, LPRECT, POINT, RECT};
use winapi::shared::winerror::{HRESULT, SUCCEEDED};
use winapi::um::combaseapi::{CoInitializeEx, CoUninitialize};
use winapi::um::libloaderapi::GetModuleHandleA;
use winapi::um::objbase::COINIT_MULTITHREADED;
use winapi::um::shellscalingapi::{SetProcessDpiAwareness, PROCESS_SYSTEM_DPI_AWARE};
use winapi::um::wingdi::{
    CreateDCA, DeleteDC, GetDeviceCaps, DISPLAY_DEVICEA, DISPLAY_DEVICE_ACTIVE,
    DISPLAY_DEVICE_MIRRORING_DRIVER, DISPLAY_DEVICE_MODESPRUNED, DISPLAY_DEVICE_PRIMARY_DEVICE,
    DISPLAY_DEVICE_REMOVABLE, DISPLAY_DEVICE_VGA_COMPATIBLE, HORZRES, HORZSIZE, VERTRES, VERTSIZE,
};
use winapi::um::winuser::{
    AdjustWindowRectEx, CreateWindowExA, DefWindowProcA, DialogBoxParamA, DispatchMessageA,
    EndDialog, EnumDisplayDevicesA, EnumDisplayMonitors, GetClientRect, GetDlgItem,
    GetMonitorInfoA, GetWindowLongPtrA, LoadCursorA, MapWindowPoints, PeekMessageA,
    PostQuitMessage, RegisterClassExA, SendMessageA, SetWindowLongPtrA, ShowWindow,
    TranslateMessage, CB_ADDSTRING, CB_GETCURSEL, CB_SETCURSEL, CB_SETEXTENDEDUI, CW_USEDEFAULT,
    GWLP_USERDATA, IDC_ARROW, MONITORINFOEXA, PM_REMOVE, SW_SHOWNORMAL, WM_COMMAND, WM_INITDIALOG,
    WM_INPUT, WM_SIZE, WNDCLASSEXA, WS_BORDER, WS_CAPTION, WS_EX_APPWINDOW, WS_MINIMIZEBOX,
    WS_OVERLAPPED, WS_OVERLAPPEDWINDOW, WS_POPUP, WS_SYSMENU,
};
use winapi::um::winuser::{WM_DESTROY, WM_QUIT};

use std::mem::MaybeUninit;
mod audio;
use audio::NativeAudioEngine;
use log::*;
mod input;
mod userlib;
use peridot::mthelper::SharedRef;
use peridot::EngineEvents;
use userlib::Game;

mod presenter;
use self::presenter::Presenter;

const LPSZCLASSNAME: &'static str = concat!("mainWindow\0");

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
    fn new(
        window: HWND,
        init_size: peridot::math::Vector2<usize>,
        fullscreen_target_monitor: Option<HMONITOR>,
    ) -> Self {
        let window = SharedRef::new(ThreadsafeWindowOps(window));

        let nl = NativeLink {
            al: AssetProvider::new(),
            window: window.clone(),
            fullscreen_target_monitor,
        };
        let mut base = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            Game::<NativeLink>::requested_features(),
        );
        let usercode = Game::init(&mut base);
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

#[repr(transparent)]
struct WindowClass(WNDCLASSEXA);
impl WindowClass {
    fn register(&self) -> std::io::Result<ATOM> {
        let r = unsafe { RegisterClassExA(&self.0) };
        if r == 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(r)
        }
    }
}

struct DisplayDeviceIterator<'s> {
    device: Option<&'s std::ffi::CStr>,
    next_device_number: DWORD,
    flags: DWORD,
}
impl<'s> DisplayDeviceIterator<'s> {
    pub const fn new(device: Option<&'s std::ffi::CStr>, flags: DWORD) -> Self {
        Self {
            device,
            next_device_number: 0,
            flags,
        }
    }
}
impl Iterator for DisplayDeviceIterator<'_> {
    type Item = DISPLAY_DEVICEA;

    fn next(&mut self) -> Option<Self::Item> {
        let mut dd = std::mem::MaybeUninit::<DISPLAY_DEVICEA>::uninit();
        unsafe { (*dd.as_mut_ptr()).cb = std::mem::size_of::<DISPLAY_DEVICEA>() as _ };
        let r = unsafe {
            EnumDisplayDevicesA(
                self.device
                    .map_or_else(std::ptr::null, std::ffi::CStr::as_ptr),
                self.next_device_number,
                dd.as_mut_ptr(),
                self.flags,
            )
        };
        if r == 0 {
            None
        } else {
            self.next_device_number += 1;
            Some(unsafe { dd.assume_init() })
        }
    }
}

/// helper function until stabilization of std::ffi::CStr::from_bytes_until_nul
fn cstr_from_bytes_until_nul(bytes: &[u8]) -> std::borrow::Cow<std::ffi::CStr> {
    let len = bytes.iter().take_while(|&&c| c != 0).count();

    unsafe {
        if len == bytes.len() {
            // filled
            std::borrow::Cow::Owned(std::ffi::CString::from_vec_with_nul_unchecked(
                bytes.iter().copied().chain(std::iter::once(0)).collect(),
            ))
        } else {
            std::borrow::Cow::Borrowed(std::ffi::CStr::from_bytes_with_nul_unchecked(
                &bytes[..len + 1],
            ))
        }
    }
}

trait DisplayDeviceProvider {
    fn display_device(&self) -> &DISPLAY_DEVICEA;

    fn flags(&self) -> Vec<&'static str> {
        let mut flags = Vec::new();
        if (self.display_device().StateFlags & DISPLAY_DEVICE_ACTIVE) != 0 {
            flags.push("Active");
        }
        if (self.display_device().StateFlags & DISPLAY_DEVICE_MIRRORING_DRIVER) != 0 {
            flags.push("Mirroring");
        }
        if (self.display_device().StateFlags & DISPLAY_DEVICE_MODESPRUNED) != 0 {
            flags.push("ModesPruned");
        }
        if (self.display_device().StateFlags & DISPLAY_DEVICE_PRIMARY_DEVICE) != 0 {
            flags.push("Primary");
        }
        if (self.display_device().StateFlags & DISPLAY_DEVICE_REMOVABLE) != 0 {
            flags.push("Removable");
        }
        if (self.display_device().StateFlags & DISPLAY_DEVICE_VGA_COMPATIBLE) != 0 {
            flags.push("VGAComp");
        }

        flags
    }

    fn device_string(&self) -> std::borrow::Cow<std::ffi::CStr> {
        cstr_from_bytes_until_nul(unsafe {
            std::mem::transmute(&self.display_device().DeviceString[..])
        })
    }

    fn device_name(&self) -> std::borrow::Cow<std::ffi::CStr> {
        cstr_from_bytes_until_nul(unsafe {
            std::mem::transmute(&self.display_device().DeviceName[..])
        })
    }
}

#[repr(transparent)]
struct DisplayDevice(DISPLAY_DEVICEA);
impl DisplayDevice {
    fn all() -> impl Iterator<Item = Self> {
        DisplayDeviceIterator::new(None, 0).map(Self)
    }

    fn monitors<'s>(&'s self) -> impl Iterator<Item = MonitorDevice<'s>> + 's {
        DisplayDeviceIterator::new(
            Some(unsafe {
                std::ffi::CStr::from_bytes_with_nul_unchecked(std::mem::transmute(
                    &self.0.DeviceName[..],
                ))
            }),
            0,
        )
        .map(move |dd| MonitorDevice(self, dd))
    }
}
impl DisplayDeviceProvider for DisplayDevice {
    fn display_device(&self) -> &DISPLAY_DEVICEA {
        &self.0
    }
}

struct MonitorDevice<'d>(&'d DisplayDevice, DISPLAY_DEVICEA);
impl MonitorDevice<'_> {
    fn create_dc(&self) -> Option<DeviceContextHandle> {
        let dc = unsafe {
            CreateDCA(
                self.0 .0.DeviceName.as_ptr(),
                self.1.DeviceString.as_ptr(),
                std::ptr::null(),
                std::ptr::null(),
            )
        };

        if dc.is_null() {
            None
        } else {
            Some(DeviceContextHandle(dc))
        }
    }
}
impl DisplayDeviceProvider for MonitorDevice<'_> {
    fn display_device(&self) -> &DISPLAY_DEVICEA {
        &self.1
    }
}

#[repr(transparent)]
struct DeviceContextHandle(HDC);
impl Drop for DeviceContextHandle {
    fn drop(&mut self) {
        unsafe { DeleteDC(self.0) };
    }
}
impl DeviceContextHandle {
    fn device_caps(&self, cap: i32) -> i32 {
        unsafe { GetDeviceCaps(self.0, cap) }
    }
}

fn enum_display_monitors<F: FnMut(HMONITOR, HDC, &mut RECT) -> bool>(
    hdc: Option<HDC>,
    clip_rect: Option<&RECT>,
    mut callback: F,
) -> std::io::Result<()> {
    extern "system" fn wrapper<F: FnMut(HMONITOR, HDC, &mut RECT) -> bool>(
        monitor: HMONITOR,
        dc: HDC,
        rect: LPRECT,
        userdata: LPARAM,
    ) -> BOOL {
        let callback = unsafe { &mut *(userdata as *mut F) };
        if callback(monitor, dc, unsafe { &mut *rect }) {
            TRUE
        } else {
            FALSE
        }
    }

    let r = unsafe {
        EnumDisplayMonitors(
            hdc.unwrap_or_else(std::ptr::null_mut),
            clip_rect.map_or_else(std::ptr::null, |x| x as *const _),
            Some(wrapper::<F>),
            &mut callback as *mut _ as _,
        )
    };
    if r == 0 {
        Err(std::io::Error::last_os_error())
    } else {
        Ok(())
    }
}

fn main() {
    env_logger::init();
    let _co = CoScopeGuard::init(COINIT_MULTITHREADED).expect("Initializing COM");

    unsafe {
        SetProcessDpiAwareness(PROCESS_SYSTEM_DPI_AWARE);
    }

    let wcatom = WindowClass(WNDCLASSEXA {
        cbSize: std::mem::size_of::<WNDCLASSEXA>() as _,
        hInstance: module_handle(),
        lpszClassName: LPSZCLASSNAME.as_ptr() as *const _,
        lpfnWndProc: Some(window_callback),
        hCursor: unsafe { LoadCursorA(std::ptr::null_mut(), IDC_ARROW as _) },
        ..unsafe { MaybeUninit::zeroed().assume_init() }
    })
    .register()
    .expect("Failed to register window class");

    let wname_c =
        std::ffi::CString::new(userlib::APP_TITLE).expect("Unable to generate a c-style string");
    let wsex = if cfg!(feature = "transparent") {
        WS_EX_APPWINDOW | winapi::um::winuser::WS_EX_NOREDIRECTIONBITMAP
    } else {
        WS_EX_APPWINDOW
    };
    let (w, h, style, fs_monitor) = match userlib::APP_DEFAULT_EXTENTS {
        peridot::WindowExtents::Fixed(w, h) => (
            w,
            h,
            WS_OVERLAPPED | WS_CAPTION | WS_BORDER | WS_SYSMENU | WS_MINIMIZEBOX,
            None,
        ),
        peridot::WindowExtents::Resizable(w, h) => (w, h, WS_OVERLAPPEDWINDOW, None),
        peridot::WindowExtents::Fullscreen => {
            let mut candidates = Vec::new();
            let mut device_names = Vec::new();
            for (nd, d) in DisplayDevice::all().enumerate() {
                println!(
                    "Device #{nd}: {} {}",
                    d.device_string().to_str().expect("invalid str sequence"),
                    d.flags().join(",")
                );

                for (n, m) in d.monitors().enumerate() {
                    let dc = m.create_dc().expect("Failed to create DC for monitor");
                    let wres = dc.device_caps(HORZRES);
                    let hres = dc.device_caps(VERTRES);
                    let wmm = dc.device_caps(HORZSIZE);
                    let hmm = dc.device_caps(VERTSIZE);

                    println!(
                        "Monitor #{n}: {} ({wres}x{hres}, {wmm}mmx{hmm}mm) {}",
                        m.device_string().to_str().expect("invalid str sequence"),
                        m.flags().join(",")
                    );

                    candidates.push(
                        std::ffi::CString::new(format!(
                            "{} {wres}x{hres} ({})",
                            m.device_string().to_str().expect("invalid str sequence"),
                            d.device_string().to_str().expect("invalid str sequence")
                        ))
                        .expect("invalid sequence"),
                    );
                    device_names.push(d.device_name().into_owned());
                }
            }

            struct SelectMonitorDialogInitData {
                candidates: Vec<std::ffi::CString>,
            }
            extern "system" fn dlgfunc(dlg: HWND, msg: UINT, wp: WPARAM, lp: LPARAM) -> INT_PTR {
                if msg == WM_INITDIALOG {
                    unsafe {
                        let init_data = &*(lp as *const SelectMonitorDialogInitData);
                        let choices = GetDlgItem(dlg, 10);
                        SendMessageA(choices, CB_SETEXTENDEDUI, TRUE as _, 0);
                        for c in &init_data.candidates {
                            SendMessageA(choices, CB_ADDSTRING, 0, c.as_ptr() as _);
                        }
                        SendMessageA(choices, CB_SETCURSEL, 0, 0);
                    }
                }
                if msg == WM_COMMAND {
                    if wp == 1 {
                        // ok
                        unsafe {
                            EndDialog(dlg, SendMessageA(GetDlgItem(dlg, 10), CB_GETCURSEL, 0, 0))
                        };
                        return TRUE as _;
                    }
                    if wp == 2 {
                        // cancel
                        unsafe { EndDialog(dlg, -1) };
                        return TRUE as _;
                    }
                }
                FALSE as _
            }
            let init_data = SelectMonitorDialogInitData { candidates };
            let monitor_index = unsafe {
                DialogBoxParamA(
                    module_handle(),
                    1001 as _,
                    std::ptr::null_mut(),
                    Some(dlgfunc),
                    &init_data as *const _ as _,
                )
            };

            if monitor_index < 0 {
                return;
            }
            let monitor_index = monitor_index as usize;
            let mut hmonitor = None;
            let _ = enum_display_monitors(None, None, |mon, _, _| {
                let mut mi = std::mem::MaybeUninit::<MONITORINFOEXA>::uninit();
                let r = unsafe {
                    (*mi.as_mut_ptr()).cbSize = std::mem::size_of::<MONITORINFOEXA>() as _;
                    GetMonitorInfoA(mon, mi.as_mut_ptr() as _)
                };
                if r == 0 {
                    panic!(
                        "Failed to get monitor info: {:?}",
                        std::io::Error::last_os_error()
                    );
                }
                let mi = unsafe { mi.assume_init() };
                let device_name =
                    cstr_from_bytes_until_nul(unsafe { std::mem::transmute(&mi.szDevice[..]) });
                if &device_name as &std::ffi::CStr
                    == &device_names[monitor_index] as &std::ffi::CStr
                {
                    hmonitor = Some(mon);
                    false
                } else {
                    // continue finding
                    true
                }
            });
            let hmonitor = hmonitor.expect("No matching monitor found");

            (1280, 720, WS_POPUP | WS_SYSMENU, Some(hmonitor))
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
            module_handle(),
            std::ptr::null_mut(),
        )
    };
    if w.is_null() {
        panic!("Create Window Failed!");
    }

    let mut driver = GameDriver::new(w, peridot::math::Vector2(640, 480), fs_monitor);
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
    window: SharedRef<ThreadsafeWindowOps>,
    fullscreen_target_monitor: Option<HMONITOR>,
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = AssetProvider;
    type Presenter = Presenter;

    #[cfg(not(feature = "transparent"))]
    fn intercept_instance_builder(&self, builder: &mut bedrock::InstanceBuilder) {
        builder.add_extension("VK_KHR_surface");
        builder.add_extension("VK_KHR_display");
        builder.add_extension("VK_KHR_win32_surface");
        builder.add_extension("VK_KHR_get_physical_device_properties2");
        builder.add_extension("VK_KHR_get_surface_capabilities2");
    }
    #[cfg(feature = "transparent")]
    fn intercept_instance_builder(&self, builder: &mut bedrock::InstanceBuilder) {}

    #[cfg(not(feature = "transparent"))]
    fn intercept_device_builder(
        &self,
        builder: &mut bedrock::DeviceBuilder<impl bedrock::PhysicalDevice>,
    ) {
        builder.add_extensions(vec!["VK_KHR_swapchain"]);
        builder.add_extension("VK_EXT_full_screen_exclusive");
    }
    #[cfg(feature = "transparent")]
    fn intercept_device_builder(
        &self,
        builder: &mut bedrock::DeviceBuilder<impl bedrock::PhysicalDevice>,
    ) {
        builder.add_extensions(vec![
            "VK_KHR_external_memory_win32",
            "VK_KHR_external_semaphore_win32",
        ]);
    }

    fn asset_loader(&self) -> &AssetProvider {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(g, self.window.clone(), self.fullscreen_target_monitor)
    }
}
