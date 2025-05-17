use crate::{GameDriver, ThreadsafeWindowOps, LPSZCLASSNAME};
#[cfg(feature = "transparent")]
use bedrock::TypedVulkanSinkStructure;
use bedrock::{self as br, Device, Instance, ResolverInterface, VkHandle, VulkanSinkStructure};
#[cfg(not(feature = "transparent"))]
use bedrock::{InstanceChild, SurfaceCreateInfo};
use parking_lot::RwLock;
use std::collections::HashMap;
use std::mem::MaybeUninit;
use std::sync::Arc;
#[cfg(feature = "transparent")]
use windows::core::ComInterface;
use windows::core::PCWSTR;
use windows::Devices::Display::Core::DisplayModeQueryOptions;
use windows::Win32::Devices::DeviceAndDriverInstallation::{
    SetupDiDestroyDeviceInfoList, SetupDiEnumDeviceInfo, SetupDiGetClassDevsW,
    SetupDiGetDevicePropertyKeys, SetupDiGetDevicePropertyW, SetupDiOpenDevRegKey,
    DICS_FLAG_GLOBAL, DIGCF_DEFAULT, DIGCF_PRESENT, DIREG_DEV, GUID_DEVCLASS_MONITOR,
    SP_DEVINFO_DATA,
};
use windows::Win32::Devices::Display::{
    DisplayConfigGetDeviceInfo, GetDisplayConfigBufferSizes,
    GetNumberOfPhysicalMonitorsFromHMONITOR, GetPhysicalMonitorsFromHMONITOR, QueryDisplayConfig,
    DISPLAYCONFIG_DEVICE_INFO_GET_SOURCE_NAME, DISPLAYCONFIG_DEVICE_INFO_GET_TARGET_NAME,
    DISPLAYCONFIG_MODE_INFO_TYPE_SOURCE, DISPLAYCONFIG_MODE_INFO_TYPE_TARGET,
    DISPLAYCONFIG_SOURCE_DEVICE_NAME, DISPLAYCONFIG_TARGET_DEVICE_NAME, PHYSICAL_MONITOR,
    QDC_ONLY_ACTIVE_PATHS,
};
use windows::Win32::Devices::Properties::{
    DEVPKEY_NAME, DEVPROP_TYPE_STRING, DEVPROP_TYPE_UINT32, DEVPROP_TYPE_UINT64,
};
#[cfg(feature = "transparent")]
use windows::Win32::Foundation::GENERIC_ALL;
use windows::Win32::Foundation::{
    ERROR_INSUFFICIENT_BUFFER, ERROR_MORE_DATA, ERROR_NO_MORE_ITEMS, HINSTANCE, HWND, LPARAM,
    LRESULT, RECT, WPARAM,
};
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Direct3D::D3D_FEATURE_LEVEL_11_0;
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Direct3D12::{
    D3D12CreateDevice, D3D12GetDebugInterface, ID3D12CommandQueue, ID3D12Debug, ID3D12Device,
    ID3D12Fence, ID3D12Resource, D3D12_COMMAND_LIST_TYPE_DIRECT, D3D12_COMMAND_QUEUE_DESC,
    D3D12_FENCE_FLAG_NONE, D3D12_FENCE_FLAG_SHARED,
};
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::DirectComposition::{
    DCompositionCreateDevice3, IDCompositionDesktopDevice, IDCompositionTarget,
    IDCompositionVisual2,
};
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Dxgi::Common::{
    DXGI_ALPHA_MODE_PREMULTIPLIED, DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_SAMPLE_DESC,
};
use windows::Win32::Graphics::Dxgi::{CreateDXGIFactory, IDXGIFactory, DXGI_ERROR_NOT_FOUND};
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Dxgi::{
    CreateDXGIFactory2, IDXGIFactory2, IDXGISwapChain3, DXGI_CREATE_FACTORY_DEBUG,
    DXGI_SCALING_STRETCH, DXGI_SWAP_CHAIN_DESC1, DXGI_SWAP_EFFECT_FLIP_DISCARD,
    DXGI_USAGE_RENDER_TARGET_OUTPUT,
};
use windows::Win32::Graphics::Gdi::{
    ChangeDisplaySettingsExW, ChangeDisplaySettingsW, EnumDisplayDevicesW, EnumDisplayMonitors,
    EnumDisplaySettingsW, GetMonitorInfoW, CDS_FULLSCREEN, CDS_RESET, CDS_TYPE, DEVMODEW,
    DISPLAYCONFIG_PATH_SUPPORT_VIRTUAL_MODE, DISPLAY_DEVICEW, DISP_CHANGE, DISP_CHANGE_SUCCESSFUL,
    DM_DISPLAYFREQUENCY, DM_PELSHEIGHT, DM_PELSWIDTH, ENUM_CURRENT_SETTINGS,
    ENUM_DISPLAY_SETTINGS_MODE, HMONITOR, MONITORINFOEXW,
};
use windows::Win32::System::Com::{
    CoCreateInstance, CoSetProxyBlanket, CLSCTX_INPROC_SERVER, EOAC_NONE, RPC_C_AUTHN_LEVEL_CALL,
    RPC_C_IMP_LEVEL_IMPERSONATE,
};
use windows::Win32::System::LibraryLoader::GetModuleHandleW;
use windows::Win32::System::Ole::{SafeArrayAccessData, SafeArrayUnaccessData};
use windows::Win32::System::Registry::{
    RegCloseKey, RegEnumValueW, RegGetValueW, KEY_READ, REG_BINARY, RRF_RT_REG_BINARY,
};
use windows::Win32::System::Rpc::{RPC_C_AUTHN_WINNT, RPC_C_AUTHZ_NONE};
use windows::Win32::System::Threading::Sleep;
use windows::Win32::System::Variant::{VariantClear, VariantInit};
use windows::Win32::System::Wmi::{
    IWbemLocator, WbemLocator, WBEM_FLAG_FORWARD_ONLY, WBEM_FLAG_RETURN_IMMEDIATELY,
    WBEM_GENERIC_FLAG_TYPE, WBEM_INFINITE,
};
use windows::Win32::UI::WindowsAndMessaging::{
    AdjustWindowRectEx, CreateWindowExA, DefWindowProcA, GetWindowLongPtrA, GetWindowRect,
    LoadCursorW, PostQuitMessage, RegisterClassExA, SetWindowPos, ShowWindow, CW_USEDEFAULT,
    GWLP_USERDATA, IDC_ARROW, SW_MAXIMIZE, SW_SHOWMAXIMIZED, SW_SHOWNORMAL, WM_DESTROY,
    WM_DISPLAYCHANGE, WM_DPICHANGED, WM_GETMINMAXINFO, WM_INPUT, WM_SIZE, WNDCLASSEXA, WS_BORDER,
    WS_CAPTION, WS_CLIPCHILDREN, WS_CLIPSIBLINGS, WS_EX_APPWINDOW, WS_EX_NOREDIRECTIONBITMAP,
    WS_EX_TOPMOST, WS_MINIMIZEBOX, WS_OVERLAPPED, WS_OVERLAPPEDWINDOW, WS_POPUP, WS_SYSMENU,
    WS_THICKFRAME,
};

#[cfg(not(feature = "transparent"))]
struct Surface {
    device: peridot::VulkanGfx,
    handle: br::vk::VkSurfaceKHR,
}
#[cfg(not(feature = "transparent"))]
impl Drop for Surface {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_surface(
                self.device.instance().native_ptr(),
                self.handle,
                None,
            );
        }
    }
}
#[cfg(not(feature = "transparent"))]
impl br::VkHandle for Surface {
    type Handle = br::vk::VkSurfaceKHR;

    #[inline]
    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

#[repr(transparent)]
pub struct DeviceMode(DEVMODEW);

pub struct DisplaySettingsEnumerator<'s> {
    counter: u32,
    device_name: Option<&'s [u16]>,
}
impl<'s> DisplaySettingsEnumerator<'s> {
    pub fn new(device_name: Option<&'s [u16]>) -> Self {
        Self {
            counter: 0,
            device_name,
        }
    }
}
impl Iterator for DisplaySettingsEnumerator<'_> {
    type Item = DeviceMode;

    fn next(&mut self) -> Option<Self::Item> {
        let mut mode = MaybeUninit::<DEVMODEW>::uninit();
        let r = unsafe {
            core::ptr::write(
                &mut (*mode.as_mut_ptr()).dmSize,
                core::mem::size_of::<DEVMODEW>() as _,
            );
            core::ptr::write(&mut (*mode.as_mut_ptr()).dmDriverExtra, 0);

            EnumDisplaySettingsW(
                self.device_name
                    .map_or(PCWSTR::null(), |x| PCWSTR(x.as_ptr())),
                ENUM_DISPLAY_SETTINGS_MODE(self.counter),
                mode.as_mut_ptr(),
            )
        };

        if !r.as_bool() {
            return None;
        }

        self.counter += 1;
        Some(DeviceMode(unsafe { mode.assume_init() }))
    }
}

#[repr(transparent)]
pub struct DisplayDevice(DISPLAY_DEVICEW);
impl DisplayDevice {
    pub fn all() -> impl Iterator<Item = Self> {
        DisplayDeviceEnumerator {
            name: None,
            counter: 0,
            flags: 0,
        }
    }

    pub fn monitors<'s>(&'s self) -> impl Iterator<Item = Self> + 's {
        DisplayDeviceEnumerator {
            name: Some(&self.0.DeviceName),
            counter: 0,
            flags: 0,
        }
    }

    pub fn display_settings<'s>(&'s self) -> impl Iterator<Item = DeviceMode> + 's {
        DisplaySettingsEnumerator {
            counter: 0,
            device_name: Some(&self.0.DeviceName),
        }
    }

    pub fn render_device_string<'s>(
        &'s self,
    ) -> impl Iterator<Item = Result<char, core::char::DecodeUtf16Error>> + 's {
        core::char::decode_utf16(self.0.DeviceString.iter().copied().take_while(|&x| x != 0))
    }

    pub fn render_device_name<'s>(
        &'s self,
    ) -> impl Iterator<Item = Result<char, core::char::DecodeUtf16Error>> + 's {
        core::char::decode_utf16(self.0.DeviceName.iter().copied().take_while(|&x| x != 0))
    }
}

pub struct DisplayDeviceEnumerator<'s> {
    name: Option<&'s [u16]>,
    counter: u32,
    flags: u32,
}
impl Iterator for DisplayDeviceEnumerator<'_> {
    type Item = DisplayDevice;

    fn next(&mut self) -> Option<Self::Item> {
        let mut sink = MaybeUninit::<DISPLAY_DEVICEW>::uninit();
        let r = unsafe {
            core::ptr::write(
                &mut (*sink.as_mut_ptr()).cb,
                core::mem::size_of::<DISPLAY_DEVICEW>() as _,
            );
            EnumDisplayDevicesW(
                self.name.map_or(PCWSTR::null(), |x| PCWSTR(x.as_ptr())),
                self.counter,
                sink.as_mut_ptr(),
                self.flags,
            )
        };

        if !r.as_bool() {
            // no more devices
            return None;
        }

        self.counter += 1;
        Some(DisplayDevice(unsafe { sink.assume_init() }))
    }
}

pub struct DisplayDeviceMode {
    pub width_px: u32,
    pub height_px: u32,
    pub refresh_rate: u32,
}
impl DisplayDeviceMode {
    pub fn apply(&self, view_gdi_device_name: &[u16]) -> DISP_CHANGE {
        let mut devmode = MaybeUninit::<DEVMODEW>::uninit();
        unsafe {
            core::ptr::write(
                &mut (*devmode.as_mut_ptr()).dmSize,
                core::mem::size_of::<DEVMODEW>() as _,
            );
            core::ptr::write(
                &mut (*devmode.as_mut_ptr()).dmFields,
                DM_PELSWIDTH | DM_PELSHEIGHT | DM_DISPLAYFREQUENCY,
            );
            core::ptr::write(&mut (*devmode.as_mut_ptr()).dmPelsWidth, self.width_px);
            core::ptr::write(&mut (*devmode.as_mut_ptr()).dmPelsHeight, self.height_px);
            core::ptr::write(
                &mut (*devmode.as_mut_ptr()).dmDisplayFrequency,
                self.refresh_rate,
            );
            core::ptr::write(&mut (*devmode.as_mut_ptr()).dmBitsPerPel, 32);

            ChangeDisplaySettingsExW(
                PCWSTR(view_gdi_device_name.as_ptr()),
                Some(devmode.as_ptr()),
                None,
                CDS_FULLSCREEN,
                None,
            )
        }
    }
}

pub struct DisplayDeviceTopologyEntry {
    pub target_monitor_friendly_name: String,
    pub available_modes: Vec<DisplayDeviceMode>,
    pub view_gdi_device_name: Vec<u16>,
    pub monitor_handle: HMONITOR,
}

pub struct DisplayDeviceTopologyCache {
    entries: Vec<DisplayDeviceTopologyEntry>,
}
impl DisplayDeviceTopologyCache {
    pub fn new() -> Self {
        let mut this = Self {
            entries: Vec::new(),
        };

        this.refresh();
        this
    }

    #[inline]
    pub fn primary(&self) -> Option<&DisplayDeviceTopologyEntry> {
        self.entries.first()
    }

    #[inline]
    pub fn display_at(&self, index: usize) -> Option<&DisplayDeviceTopologyEntry> {
        self.entries.get(index)
    }

    pub fn refresh(&mut self) {
        let mut monitor_handles_by_view_gdi_path = HashMap::new();
        extern "system" fn edm_callback(
            mon: HMONITOR,
            _dc: windows::Win32::Graphics::Gdi::HDC,
            _rect: *mut windows::Win32::Foundation::RECT,
            lp: LPARAM,
        ) -> windows::Win32::Foundation::BOOL {
            let mut moninfo = MaybeUninit::<MONITORINFOEXW>::uninit();
            unsafe {
                core::ptr::write(
                    &mut (*moninfo.as_mut_ptr()).monitorInfo.cbSize,
                    core::mem::size_of::<MONITORINFOEXW>() as _,
                );
                GetMonitorInfoW(mon, moninfo.as_mut_ptr() as _).unwrap();
            }

            let sink = unsafe {
                &mut *(core::ptr::with_exposed_provenance_mut::<HashMap<String, HMONITOR>>(
                    lp.0 as _,
                ))
            };
            sink.insert(
                core::char::decode_utf16(unsafe {
                    moninfo
                        .assume_init_ref()
                        .szDevice
                        .iter()
                        .copied()
                        .take_while(|&x| x != 0)
                })
                .collect::<Result<String, _>>()
                .unwrap(),
                mon,
            );

            windows::Win32::Foundation::BOOL(1)
        }
        unsafe {
            EnumDisplayMonitors(
                None,
                None,
                Some(edm_callback),
                LPARAM(
                    (&mut monitor_handles_by_view_gdi_path as *mut HashMap<String, HMONITOR>)
                        .expose_provenance() as _,
                ),
            )
            .unwrap();
        }

        // どうやらHMONITORからだけだとモニタの正式名が取れないっぽい（Generic PnP Monitorになってしまう）のでトポロジも追ってちゃんと取得する必要がある
        let mut path_array_element_count = MaybeUninit::uninit();
        let mut mode_info_array_element_count = MaybeUninit::uninit();
        unsafe {
            GetDisplayConfigBufferSizes(
                QDC_ONLY_ACTIVE_PATHS,
                path_array_element_count.as_mut_ptr(),
                mode_info_array_element_count.as_mut_ptr(),
            )
            .unwrap();
        }
        let mut path_array =
            Vec::with_capacity(unsafe { path_array_element_count.assume_init() as _ });
        let mut mode_info =
            Vec::with_capacity(unsafe { mode_info_array_element_count.assume_init() as _ });
        unsafe {
            path_array.set_len(path_array.capacity());
            mode_info.set_len(mode_info.capacity());
            QueryDisplayConfig(
                QDC_ONLY_ACTIVE_PATHS,
                path_array_element_count.as_mut_ptr(),
                path_array.as_mut_ptr(),
                mode_info_array_element_count.as_mut_ptr(),
                mode_info.as_mut_ptr(),
                None,
            )
            .unwrap();
        }
        self.entries.clear();
        for (n, x) in path_array.iter().enumerate() {
            if (x.flags & DISPLAYCONFIG_PATH_SUPPORT_VIRTUAL_MODE) != 0 {
                println!("path #{n}: this path supports virtual mode");
            }

            let mut cfg_source_name = MaybeUninit::<DISPLAYCONFIG_SOURCE_DEVICE_NAME>::uninit();
            unsafe {
                core::ptr::write(
                    &mut (*cfg_source_name.as_mut_ptr()).header.r#type,
                    DISPLAYCONFIG_DEVICE_INFO_GET_SOURCE_NAME,
                );
                core::ptr::write(
                    &mut (*cfg_source_name.as_mut_ptr()).header.size,
                    core::mem::size_of::<DISPLAYCONFIG_SOURCE_DEVICE_NAME>() as _,
                );
                core::ptr::write(
                    &mut (*cfg_source_name.as_mut_ptr()).header.adapterId,
                    x.sourceInfo.adapterId,
                );
                core::ptr::write(
                    &mut (*cfg_source_name.as_mut_ptr()).header.id,
                    x.sourceInfo.id,
                );

                DisplayConfigGetDeviceInfo(cfg_source_name.as_mut_ptr() as _);
            }

            let mut cfg_target_name = MaybeUninit::<DISPLAYCONFIG_TARGET_DEVICE_NAME>::uninit();
            unsafe {
                core::ptr::write(
                    &mut (*cfg_target_name.as_mut_ptr()).header.r#type,
                    DISPLAYCONFIG_DEVICE_INFO_GET_TARGET_NAME,
                );
                core::ptr::write(
                    &mut (*cfg_target_name.as_mut_ptr()).header.size,
                    core::mem::size_of::<DISPLAYCONFIG_TARGET_DEVICE_NAME>() as _,
                );
                core::ptr::write(
                    &mut (*cfg_target_name.as_mut_ptr()).header.adapterId,
                    x.targetInfo.adapterId,
                );
                core::ptr::write(
                    &mut (*cfg_target_name.as_mut_ptr()).header.id,
                    x.targetInfo.id,
                );

                DisplayConfigGetDeviceInfo(cfg_target_name.as_mut_ptr() as _);
            }

            let available_modes = DisplaySettingsEnumerator::new(Some(unsafe {
                &cfg_source_name.assume_init_ref().viewGdiDeviceName
            }))
            .map(|x| DisplayDeviceMode {
                width_px: x.0.dmPelsWidth,
                height_px: x.0.dmPelsHeight,
                refresh_rate: x.0.dmDisplayFrequency,
            })
            .collect::<Vec<_>>();

            self.entries.push(DisplayDeviceTopologyEntry {
                target_monitor_friendly_name: core::char::decode_utf16(unsafe {
                    cfg_target_name
                        .assume_init_ref()
                        .monitorFriendlyDeviceName
                        .iter()
                        .copied()
                        .take_while(|&x| x != 0)
                })
                .collect::<Result<String, _>>()
                .unwrap(),
                available_modes,
                view_gdi_device_name: unsafe {
                    cfg_source_name.assume_init_ref().viewGdiDeviceName.to_vec()
                },
                monitor_handle: monitor_handles_by_view_gdi_path[&core::char::decode_utf16(
                    unsafe {
                        cfg_source_name
                            .assume_init_ref()
                            .viewGdiDeviceName
                            .iter()
                            .copied()
                            .take_while(|&x| x != 0)
                    },
                )
                .collect::<Result<String, _>>()
                .unwrap()],
            });
        }

        self.entries.shrink_to_fit();
    }
}

fn setup_window(
    hinstance: HINSTANCE,
    prefs: &peridot::PresentationPreferences,
    display_device_topology: &peridot::mthelper::SharedRef<DisplayDeviceTopologyCache>,
    allow_transparent: bool,
) -> windows::core::Result<ThreadsafeWindowOps> {
    let wca = WNDCLASSEXA {
        cbSize: std::mem::size_of::<WNDCLASSEXA>() as _,
        hInstance: hinstance,
        lpszClassName: windows::core::PCSTR(LPSZCLASSNAME.as_ptr() as *const _),
        lpfnWndProc: Some(window_callback),
        hCursor: unsafe { LoadCursorW(None, IDC_ARROW).expect("Failed to load default cursor") },
        ..unsafe { MaybeUninit::zeroed().assume_init() }
    };
    let wcatom = unsafe { RegisterClassExA(&wca) };
    if wcatom <= 0 {
        panic!("Register Class Failed!");
    }

    let (l, t, w, h, wsx, ws) = match prefs {
        &peridot::PresentationPreferences::Windowed {
            resolution_width,
            resolution_height,
            resizable,
        } => (
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            resolution_width,
            resolution_height,
            if allow_transparent {
                WS_EX_APPWINDOW | WS_EX_NOREDIRECTIONBITMAP
            } else {
                WS_EX_APPWINDOW
            },
            if resizable {
                WS_OVERLAPPEDWINDOW
            } else {
                WS_OVERLAPPED | WS_CAPTION | WS_BORDER | WS_SYSMENU | WS_MINIMIZEBOX
            },
        ),
        &peridot::PresentationPreferences::Borderless {
            resolution_width,
            resolution_height,
        } => (
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            resolution_width,
            resolution_height,
            if allow_transparent {
                WS_EX_APPWINDOW | WS_EX_NOREDIRECTIONBITMAP
            } else {
                WS_EX_APPWINDOW
            },
            WS_POPUP,
        ),
        &peridot::PresentationPreferences::Fullscreen {
            display_index,
            desired_resolution_width,
            desired_resolution_height,
            desired_refresh_rate,
            matching_behavior,
        } => {
            let target_display = match display_device_topology.display_at(display_index) {
                Some(x) => x,
                None => match display_device_topology.primary() {
                    Some(x) => {
                        tracing::warn!(
                            display_index,
                            "No display found at the index, falling back to primary"
                        );
                        x
                    }
                    None => {
                        tracing::error!("No display available on the system");
                        std::process::abort();
                    }
                },
            };
            let exact_match_mode = target_display.available_modes.iter().find(|x| {
                x.width_px == desired_resolution_width
                    && x.height_px == desired_resolution_height
                    && x.refresh_rate == desired_refresh_rate as u32
            });

            match exact_match_mode {
                Some(x) => {
                    let r = x.apply(&target_display.view_gdi_device_name);
                    if r != DISP_CHANGE_SUCCESSFUL {
                        tracing::error!(result = ?r, "Failed to change display mode");
                        std::process::abort();
                    }

                    (
                        0,
                        0,
                        x.width_px,
                        x.height_px,
                        WS_EX_TOPMOST,
                        WS_POPUP | WS_CLIPSIBLINGS | WS_CLIPCHILDREN,
                    )
                }
                None => {
                    todo!("find alternative mode using matching behavior");
                }
            }
        }
    };

    // TODO: これコンパイル時に生成できないか？
    let wname_c = std::ffi::CString::new(crate::userlib::APP_TITLE)
        .expect("Unable to generate a c-style string");
    let mut wrect = RECT {
        left: 0,
        top: 0,
        right: w as _,
        bottom: h as _,
    };
    unsafe {
        AdjustWindowRectEx(&mut wrect, ws, false, wsx)
            .expect("Failed to calculate window geometry");
    }
    let w = unsafe {
        CreateWindowExA(
            wsx,
            windows::core::PCSTR(std::mem::transmute(wcatom as usize)),
            windows::core::PCSTR(wname_c.as_ptr() as _),
            ws,
            l,
            t,
            wrect.right - wrect.left,
            wrect.bottom - wrect.top,
            None,
            None,
            wca.hInstance,
            None,
        )
    };

    if w.0 == 0 {
        Err(windows::core::Error::from_win32())
    } else {
        Ok(ThreadsafeWindowOps(w))
    }
}

#[cfg(not(feature = "transparent"))]
pub struct Presenter {
    pub(crate) window: Arc<RwLock<ThreadsafeWindowOps>>,
    _display_device_topology: peridot::mthelper::SharedRef<DisplayDeviceTopologyCache>,
    sc: peridot::IntegratedSwapchain<Surface>,
}
#[cfg(not(feature = "transparent"))]
impl Presenter {
    pub fn new(
        g: &peridot::Graphics,
        prefs: &peridot::PresentationPreferences,
        display_device_topology: &peridot::mthelper::SharedRef<DisplayDeviceTopologyCache>,
    ) -> Self {
        let hinstance = match unsafe { GetModuleHandleW(None) } {
            Ok(x) => unsafe { core::mem::transmute(x) },
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to get module handle");
                std::process::abort();
            }
        };

        let mut w = match setup_window(hinstance, prefs, display_device_topology, false) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to setup window");
                std::process::abort();
            }
        };

        w.show(SW_SHOWNORMAL);

        if unsafe {
            !br::vkfn_wrapper::get_physical_device_win32_presentation_support(
                g.adapter_raw(),
                g.graphics_queue_family_index(),
            )
        } {
            tracing::error!("The selected physical device does not support Vulkan Rendering");
            std::process::abort();
        }
        let s = Surface {
            handle: unsafe {
                br::Win32SurfaceCreateInfo::new(super::module_handle(), w.0)
                    .execute(g.device().instance(), None)
                    .expect("Failed to create Surface")
            },
            device: g.device().clone(),
        };
        let support = g
            .device()
            .surface_support(&s)
            .expect("Failed to query Surface Support");
        if !support {
            tracing::error!("Vulkan does not support this surface to render");
        }

        Presenter {
            window: Arc::new(RwLock::new(w)),
            _display_device_topology: display_device_topology.clone(),
            sc: peridot::IntegratedSwapchain::new(g, s, peridot::math::Vector2(0, 0)),
        }
    }
}
#[cfg(not(feature = "transparent"))]
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }

    fn back_buffer_count(&self) -> usize {
        self.sc.back_buffer_count()
    }

    fn back_buffer_size(&self) -> peridot::math::Vector2<u32> {
        self.sc.back_buffer_size()
    }

    fn back_buffer<'a>(&'a self, index: usize) -> Option<br::VkHandleRef<'a, br::vk::VkImage>> {
        self.sc.back_buffer(index)
    }

    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: br::CmdRecord<'r, peridot::VulkanGfx>,
    ) -> br::CmdRecord<'r, peridot::VulkanGfx> {
        self.sc.emit_initialize_back_buffer_commands(recorder)
    }

    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_back_buffer_index()
    }

    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
    }

    fn render_and_present<'s, 'r>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        back_buffer_index: u32,
        render_submission: peridot::SubmissionBatchBuilder<'r>,
        update_submission: Option<peridot::SubmissionBatchBuilder<'r>>,
    ) -> br::Result<()>
    where
        's: 'r,
    {
        self.sc.render_and_present(
            g,
            last_render_fence,
            back_buffer_index,
            render_submission,
            update_submission,
        )
    }

    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs re-initializing back-buffer resource
        true
    }

    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        peridot::math::Vector2(0, 0)
    }
}

#[cfg(feature = "transparent")]
#[repr(transparent)]
struct UnsafeThreadsafeHandle(windows::Win32::Foundation::HANDLE);
#[cfg(feature = "transparent")]
impl Drop for UnsafeThreadsafeHandle {
    fn drop(&mut self) {
        if let Err(e) = unsafe { windows::Win32::Foundation::CloseHandle(self.0) } {
            tracing::warn!(cause = ?e, "Error closing a handle");
        }
    }
}
#[cfg(feature = "transparent")]
impl From<windows::Win32::Foundation::HANDLE> for UnsafeThreadsafeHandle {
    fn from(h: windows::Win32::Foundation::HANDLE) -> Self {
        Self(h)
    }
}
#[cfg(feature = "transparent")]
impl UnsafeThreadsafeHandle {
    #[inline]
    pub const fn handle(&self) -> windows::Win32::Foundation::HANDLE {
        self.0
    }
}
#[cfg(feature = "transparent")]
unsafe impl Sync for UnsafeThreadsafeHandle {}
#[cfg(feature = "transparent")]
unsafe impl Send for UnsafeThreadsafeHandle {}

#[cfg(feature = "transparent")]
#[repr(transparent)]
struct ThreadsafeEvent(windows::Win32::Foundation::HANDLE);
#[cfg(feature = "transparent")]
impl Drop for ThreadsafeEvent {
    fn drop(&mut self) {
        if let Err(e) = unsafe { windows::Win32::Foundation::CloseHandle(self.0) } {
            tracing::warn!(cause = ?e, "Error closing an event handle");
        }
    }
}
#[cfg(feature = "transparent")]
impl ThreadsafeEvent {
    #[inline]
    pub fn new(manual_reset: bool, init_signaled: bool) -> windows::core::Result<Self> {
        unsafe {
            windows::Win32::System::Threading::CreateEventA(None, manual_reset, init_signaled, None)
                .map(Self)
        }
    }

    #[inline]
    pub fn wait(&mut self, timeout: u32) {
        unsafe {
            windows::Win32::System::Threading::WaitForSingleObject(self.0, timeout);
        }
    }
}
#[cfg(feature = "transparent")]
unsafe impl Sync for ThreadsafeEvent {}
#[cfg(feature = "transparent")]
unsafe impl Send for ThreadsafeEvent {}

#[cfg(feature = "transparent")]
struct InteropBackbufferResource {
    _shared_handle: UnsafeThreadsafeHandle,
    device: peridot::VulkanGfx,
    memory: br::vk::VkDeviceMemory,
    image: br::vk::VkImage,
}
#[cfg(feature = "transparent")]
impl Drop for InteropBackbufferResource {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image(self.device.native_ptr(), self.image, None);
            br::vkfn_wrapper::free_memory(self.device.native_ptr(), self.memory, None);
        }
    }
}
#[cfg(feature = "transparent")]
impl br::VkHandle for InteropBackbufferResource {
    type Handle = br::vk::VkImage;

    fn native_ptr(&self) -> Self::Handle {
        self.image
    }
}
#[cfg(feature = "transparent")]
impl InteropBackbufferResource {
    pub fn new(
        g: &peridot::Graphics,
        memory_property_fn: br::vk::PFN_vkGetMemoryWin32HandlePropertiesKHR,
        device: &ID3D12Device,
        resource: &ID3D12Resource,
        name_suffix: u32,
        size: br::vk::VkExtent2D,
        format: br::vk::VkFormat,
    ) -> Self {
        let hname = widestring::WideCString::from_str(format!(
            "LocalPeridotApiInteropHandleCradle{name_suffix}"
        ))
        .expect("Failed to encode to WideString");
        let shared_handle = UnsafeThreadsafeHandle(unsafe {
            device
                .CreateSharedHandle(
                    resource,
                    None,
                    GENERIC_ALL.0,
                    windows::core::PCWSTR(hname.as_ptr()),
                )
                .expect("Failed to create SharedHandle from D3D12")
        });
        let exportable = br::vk::VkExternalMemoryImageCreateInfoKHR::new(
            br::ExternalMemoryHandleTypeWin32::D3D12Resource as _,
        );
        let image = unsafe {
            br::vkfn_wrapper::create_image(
                g.device().native_ptr(),
                &br::ImageCreateInfo::new(size, format)
                    .as_color_attachment()
                    .with_next(&exportable),
                None,
            )
            .expect("Failed to create Interop Image")
        };
        let image_mreq = unsafe {
            br::vkfn_wrapper::get_image_memory_requirements(g.device().native_ptr(), image)
        };
        let handle_import_props = {
            let mut sink = br::vk::VkMemoryWin32HandlePropertiesKHR::uninit_sink();

            unsafe {
                (memory_property_fn.0)(
                    g.device().native_ptr(),
                    br::ExternalMemoryHandleTypeWin32::D3D12Resource as _,
                    shared_handle.handle(),
                    sink.as_mut_ptr(),
                )
                .into_result()
                .expect("Failed to query Handle Memory Properties");

                sink.assume_init()
            }
        };
        let memory_type_index = g
            .device()
            .device_local_memory_index(
                image_mreq.memoryTypeBits & handle_import_props.memoryTypeBits,
            )
            .expect("Failed to find matching memory type for importing");
        let memory = unsafe {
            br::vkfn_wrapper::allocate_memory(
                g.device().native_ptr(),
                &br::MemoryAllocateInfo::new(1, memory_type_index).with_next(
                    &br::ImportMemoryWin32HandleInfo::new(
                        br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                        shared_handle.handle(),
                        Some(&hname),
                    ),
                ),
                None,
            )
            .expect("Failed to import memory")
        };
        unsafe {
            g.device()
                .bind_image_raw(image, memory, 0)
                .expect("Failed to bind image backing memory");
        }

        Self {
            _shared_handle: shared_handle,
            device: g.device().clone(),
            memory,
            image,
        }
    }
}

#[cfg(feature = "transparent")]
struct Composition {
    device: IDCompositionDesktopDevice,
    target: IDCompositionTarget,
    root: IDCompositionVisual2,
}
#[cfg(feature = "transparent")]
impl Composition {
    fn new(w: &ThreadsafeWindowOps, swapchain: &IDXGISwapChain3) -> Self {
        let device: IDCompositionDesktopDevice = unsafe {
            DCompositionCreateDevice3(None).expect("Failed to create DirectComposition Device")
        };
        let target = unsafe {
            device
                .CreateTargetForHwnd(w.0, true)
                .expect("Failed to create DirectComposition Target")
        };
        let root = unsafe {
            device
                .CreateVisual()
                .expect("Failed to create DirectComposition Visual")
        };

        unsafe {
            root.SetContent(swapchain)
                .expect("Failed to set Swapchain for Composition");
            target
                .SetRoot(&root)
                .expect("Failed to set Composition Root Visual");
            device.Commit().expect("Failed to commit composition");
        }

        Self {
            device,
            target,
            root,
        }
    }
}

#[cfg(feature = "transparent")]
pub struct Presenter {
    pub(crate) window: Arc<RwLock<ThreadsafeWindowOps>>,
    _comp: Composition,
    device12: ID3D12Device,
    q: ID3D12CommandQueue,
    sc: IDXGISwapChain3,
    size: peridot::math::Vector2<u32>,
    back_buffers: Vec<InteropBackbufferResource>,
    buffer_ready_order: br::SemaphoreObject<peridot::VulkanGfx>,
    present_order: br::SemaphoreObject<peridot::VulkanGfx>,
    render_completion_fence: ID3D12Fence,
    present_completion_fence: ID3D12Fence,
    render_completion_counter: u64,
    present_completion_counter: u64,
    _render_completion_fence_handle: UnsafeThreadsafeHandle,
    present_completion_event: ThreadsafeEvent,
    present_inflight: bool,
}
#[cfg(feature = "transparent")]
unsafe impl Sync for Presenter {}
#[cfg(feature = "transparent")]
unsafe impl Send for Presenter {}
#[cfg(feature = "transparent")]
impl Presenter {
    pub fn new(
        g: &peridot::Graphics,
        prefs: &peridot::PresentationPreferences,
        display_device_topology: &peridot::mthelper::SharedRef<DisplayDeviceTopologyCache>,
    ) -> Self {
        let hinstance = match unsafe { GetModuleHandleW(None) } {
            Ok(x) => unsafe { core::mem::transmute(x) },
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to get module handle");
                std::process::abort();
            }
        };

        let mut w = match setup_window(hinstance, prefs, display_device_topology, false) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to setup window");
                std::process::abort();
            }
        };

        w.show(SW_SHOWNORMAL);
        let rc = w.get_client_rect();

        let factory: IDXGIFactory2 = unsafe {
            CreateDXGIFactory2(if cfg!(debug_assertions) {
                DXGI_CREATE_FACTORY_DEBUG
            } else {
                0
            })
            .expect("Failed to create DXGI Factory")
        };
        let adapter = unsafe {
            factory
                .EnumAdapters(0)
                .expect("Failed to query primary adapter")
        };

        if cfg!(debug_assertions) {
            let mut interface = std::mem::MaybeUninit::<Option<ID3D12Debug>>::uninit();
            unsafe {
                D3D12GetDebugInterface(interface.as_mut_ptr())
                    .expect("Failed to get D3D12 Debug Layer");
                interface
                    .assume_init_ref()
                    .as_ref()
                    .expect("no debug interface?")
                    .EnableDebugLayer();
            }
        }
        let mut device12 = std::mem::MaybeUninit::<Option<ID3D12Device>>::uninit();
        unsafe {
            D3D12CreateDevice(&adapter, D3D_FEATURE_LEVEL_11_0, device12.as_mut_ptr())
                .expect("Failed to create Direct3D12 Device")
        };
        let device12 = unsafe { device12.assume_init().expect("no device created?") };
        let q = unsafe {
            device12
                .CreateCommandQueue(&D3D12_COMMAND_QUEUE_DESC {
                    Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
                    Priority: 0,
                    NodeMask: 0,
                    Flags: Default::default(),
                })
                .expect("Failed to create Primary CommandQueue")
        };
        let sc = unsafe {
            factory
                .CreateSwapChainForComposition(
                    &q,
                    &DXGI_SWAP_CHAIN_DESC1 {
                        BufferCount: 2,
                        BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
                        Format: DXGI_FORMAT_R8G8B8A8_UNORM,
                        AlphaMode: DXGI_ALPHA_MODE_PREMULTIPLIED,
                        Width: (rc.right - rc.left) as _,
                        Height: (rc.bottom - rc.top) as _,
                        Stereo: false.into(),
                        SampleDesc: DXGI_SAMPLE_DESC {
                            Count: 1,
                            Quality: 0,
                        },
                        SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
                        Scaling: DXGI_SCALING_STRETCH,
                        Flags: Default::default(),
                    },
                    None,
                )
                .expect("Failed to create SwapChain")
        };
        let sc = sc
            .cast::<IDXGISwapChain3>()
            .expect("Failed to get swapchain 3 interface");
        let comp = Composition::new(&w, &sc);
        let bb_size = br::vk::VkExtent2D {
            width: (rc.right - rc.left) as _,
            height: (rc.bottom - rc.top) as _,
        };
        let memory_property_fn = unsafe {
            g.device()
                .load_function::<br::vk::PFN_vkGetMemoryWin32HandlePropertiesKHR>()
        };
        let back_buffers = (0..2)
            .map(|bb_index| {
                let back_buffer = unsafe {
                    sc.GetBuffer(bb_index)
                        .expect("Failed to get Backbuffer from Swapchain")
                };

                InteropBackbufferResource::new(
                    g,
                    memory_property_fn,
                    &device12,
                    &back_buffer,
                    bb_index as _,
                    bb_size.clone(),
                    br::vk::VK_FORMAT_R8G8B8A8_UNORM,
                )
            })
            .collect();

        let buffer_ready_order =
            br::SemaphoreObject::new(g.device().clone(), &br::SemaphoreCreateInfo::new())
                .expect("Failed to create Buffer Ready Semaphore");
        let present_order =
            br::SemaphoreObject::new(g.device().clone(), &br::SemaphoreCreateInfo::new())
                .expect("Failed to create Present Order Semaphore");
        let render_completion_fence = unsafe {
            device12
                .CreateFence(0, D3D12_FENCE_FLAG_SHARED)
                .expect("Failed to create Render Completion Fence")
        };
        let present_completion_fence = unsafe {
            device12
                .CreateFence(0, D3D12_FENCE_FLAG_NONE)
                .expect("Failed to create Present Completion Fence")
        };
        let render_completion_fence_name =
            widestring::WideCString::from_str("LocalRenderCompletionFenceShared")
                .expect("Failed to encode widestring");
        let render_completion_fence_handle = UnsafeThreadsafeHandle(unsafe {
            device12
                .CreateSharedHandle(
                    &render_completion_fence,
                    None,
                    GENERIC_ALL.0,
                    windows::core::PCWSTR(render_completion_fence_name.as_ptr()),
                )
                .expect("Failed to create Shared Handle for Render Completion Fence")
        });
        unsafe {
            (g.device()
                .load_function::<br::vk::PFN_vkImportSemaphoreWin32HandleKHR>()
                .0)(
                g.device().native_ptr(),
                &br::ImportSemaphoreWin32HandleInfo::by_handle(
                    &present_order,
                    br::ExternalSemaphoreHandleTypeWin32::D3DFence
                        .with_handle(render_completion_fence_handle.handle()),
                )
                .into_raw(),
            )
            .into_result()
            .expect("Failed to import Render Completion Fence")
        };
        let present_completion_event =
            ThreadsafeEvent::new(false, true).expect("Failed to create Present Completion Event");

        Self {
            window: Arc::new(RwLock::new(w)),
            _comp: comp,
            device12,
            q,
            sc,
            size: bb_size.into(),
            back_buffers,
            buffer_ready_order,
            present_order,
            render_completion_fence,
            present_completion_fence,
            _render_completion_fence_handle: render_completion_fence_handle,
            render_completion_counter: 0,
            present_completion_counter: 0,
            present_completion_event,
            present_inflight: false,
        }
    }
}
#[cfg(feature = "transparent")]
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat {
        br::vk::VK_FORMAT_R8G8B8A8_UNORM
    }

    fn back_buffer_count(&self) -> usize {
        2
    }

    fn back_buffer_size(&self) -> peridot::math::Vector2<u32> {
        self.size
    }

    fn back_buffer<'a>(&'a self, index: usize) -> Option<br::VkHandleRef<'a, br::vk::VkImage>> {
        self.back_buffers
            .get(index)
            .map(br::VkHandle::as_transparent_ref)
    }

    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: br::CmdRecord<'r, peridot::VulkanGfx>,
    ) -> br::CmdRecord<'r, peridot::VulkanGfx> {
        let barriers = self
            .back_buffers
            .iter()
            .map(|b| {
                br::ImageMemoryBarrier::new(
                    b,
                    br::vk::VkImageSubresourceRange {
                        aspectMask: br::AspectMask::COLOR.bits(),
                        baseMipLevel: 0,
                        levelCount: 1,
                        baseArrayLayer: 0,
                        layerCount: 1,
                    },
                    br::ImageLayout::Undefined.to(br::ImageLayout::General),
                )
            })
            .collect::<Vec<_>>();

        recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::TOP_OF_PIPE,
            br::vk::VK_DEPENDENCY_BY_REGION_BIT,
            &[],
            &[],
            &barriers,
        )
    }

    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        Ok(unsafe { self.sc.GetCurrentBackBufferIndex() })
    }

    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (
            br::ImageLayout::General,
            br::PipelineStageFlags::TOP_OF_PIPE,
        )
    }

    fn render_and_present<'s, 'r>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        _backbuffer_index: u32,
        mut render_submission: peridot::SubmissionBatchBuilder<'r>,
        update_submission: Option<peridot::SubmissionBatchBuilder<'r>>,
    ) -> br::Result<()>
    where
        's: 'r,
    {
        let signal_counters = [self.render_completion_counter + 1];
        let signal_info = br::D3D12FenceSubmitInfo::new(&[0], &signal_counters);
        render_submission.add_signal_semaphores([self.present_order.as_transparent_ref()]);
        if let Some(mut cs) = update_submission {
            // copy -> render
            cs.add_signal_semaphores([self.buffer_ready_order.as_transparent_ref()]);
            render_submission.add_wait_semaphores([(
                self.buffer_ready_order.as_transparent_ref(),
                br::PipelineStageFlags::VERTEX_INPUT,
            )]);

            g.submit_buffered_commands(
                &[
                    cs.build(),
                    render_submission.build().with_next(&signal_info),
                ],
                last_render_fence,
            )
            .expect("Failed to submit render and update commands");
        } else {
            // render only (old logic)
            g.submit_buffered_commands(
                &[render_submission.build().with_next(&signal_info)],
                last_render_fence,
            )
            .expect("Failed to submit render commands");
        }

        if self.present_inflight {
            self.present_completion_event
                .wait(windows::Win32::System::Threading::INFINITE);
            self.present_inflight = false;
        }

        self.render_completion_counter += 1;
        unsafe {
            self.q
                .Wait(
                    &self.render_completion_fence,
                    self.render_completion_counter,
                )
                .expect("Failed to wait Render Completion Fence");
            self.sc.Present(0, 0).ok().expect("Failed to present");
            self.q
                .Signal(
                    &self.present_completion_fence,
                    self.present_completion_counter + 1,
                )
                .expect("Failed to signal Render Completion Fence");
            self.present_completion_counter += 1;
            self.present_completion_fence
                .SetEventOnCompletion(
                    self.present_completion_counter,
                    self.present_completion_event.0,
                )
                .expect("Failed to set Completion Event");
        }
        self.present_inflight = true;

        Ok(())
    }

    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        if self.present_inflight {
            self.present_completion_event
                .wait(windows::Win32::System::Threading::INFINITE);
            self.present_inflight = false;
        }

        self.back_buffers.clear();
        unsafe {
            self.sc
                .ResizeBuffers(
                    2,
                    new_size.0 as _,
                    new_size.1 as _,
                    DXGI_FORMAT_R8G8B8A8_UNORM,
                    0,
                )
                .expect("Failed to resize backbuffers");
        }
        let memory_property_fn = unsafe {
            g.device()
                .load_function::<br::vk::PFN_vkGetMemoryWin32HandlePropertiesKHR>()
        };
        for bb_index in 0..2 {
            let back_buffer = unsafe {
                self.sc
                    .GetBuffer(bb_index)
                    .expect("Failed to get Backbuffer from Swapchain")
            };

            self.back_buffers.push(InteropBackbufferResource::new(
                g,
                memory_property_fn,
                &self.device12,
                &back_buffer,
                bb_index as _,
                br::vk::VkExtent2D {
                    width: new_size.0 as _,
                    height: new_size.1 as _,
                },
                br::vk::VK_FORMAT_R8G8B8A8_UNORM,
            ));
        }
        true
    }

    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        peridot::math::Vector2(0, 0)
    }
}
#[cfg(feature = "transparent")]
impl Drop for Presenter {
    fn drop(&mut self) {
        self.present_completion_event
            .wait(windows::Win32::System::Threading::INFINITE);
    }
}

const fn loword(dw: usize) -> u16 {
    (dw & 0xffff) as _
}
const fn hiword(dw: usize) -> u16 {
    ((dw >> 16) & 0xffff) as _
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
