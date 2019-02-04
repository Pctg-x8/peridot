use winapi::um::winuser::{
    DefWindowProcA, CreateWindowExA, PeekMessageA, DispatchMessageA, TranslateMessage, WNDCLASSEXA, RegisterClassExA,
    AdjustWindowRectEx, WS_OVERLAPPEDWINDOW, WS_EX_APPWINDOW, CW_USEDEFAULT, ShowWindow, SW_SHOWNORMAL, WM_SIZE,
    PostQuitMessage, PM_REMOVE,
    LoadCursorA, IDC_ARROW, SetWindowLongPtrA, GetWindowLongPtrA, GWLP_USERDATA
};
use winapi::um::winuser::{WM_DESTROY, WM_QUIT};
use winapi::um::libloaderapi::{GetModuleHandleA};
use winapi::shared::windef::{RECT, HWND};
use winapi::shared::minwindef::{LRESULT, WPARAM, LPARAM, UINT, HINSTANCE, LOWORD, HIWORD};
use winapi::um::combaseapi::CoInitializeEx;
use winapi::um::objbase::COINIT_MULTITHREADED;

#[macro_use] extern crate log;
mod userlib;

const LPSZCLASSNAME: &str = concat!(env!("PERIDOT_WINDOWS_APPID"), ".mainWindow\0");

fn module_handle() -> HINSTANCE { unsafe { GetModuleHandleA(std::ptr::null()) } }

fn main() {
    env_logger::init();
    unsafe {
        hr_into_result(CoInitializeEx(std::ptr::null_mut(), COINIT_MULTITHREADED)).expect("Initializing COM");
    }

    let wca = WNDCLASSEXA {
        cbSize: std::mem::size_of::<WNDCLASSEXA>() as _, hInstance: module_handle(),
        lpszClassName: LPSZCLASSNAME.as_ptr() as *const _,
        lpfnWndProc: Some(window_callback),
        hCursor: unsafe { LoadCursorA(std::ptr::null_mut(), IDC_ARROW as _) },
        .. unsafe { std::mem::zeroed() }
    };
    let wcatom = unsafe { RegisterClassExA(&wca) };
    if wcatom <= 0 { panic!("Register Class Failed!"); }

    let wname = format!("{} v{}.{}.{}", GameW::NAME, GameW::VERSION.0, GameW::VERSION.1, GameW::VERSION.2);
    let wname_c = std::ffi::CString::new(wname).expect("Unable to generate a c-style string");

    let style = WS_OVERLAPPEDWINDOW;
    let mut wrect = RECT { left: 0, top: 0, right: 640, bottom: 480 };
    unsafe { AdjustWindowRectEx(&mut wrect, style, false as _, WS_EX_APPWINDOW); }
    let w = unsafe {
        CreateWindowExA(WS_EX_APPWINDOW, wcatom as _, wname_c.as_ptr(), style,
            CW_USEDEFAULT, CW_USEDEFAULT, wrect.right - wrect.left, wrect.bottom - wrect.top,
            std::ptr::null_mut(), std::ptr::null_mut(), wca.hInstance, std::ptr::null_mut())
    };
    if w.is_null() { panic!("Create Window Failed!"); }

    let snd = NativeSoundEngine::new().expect("Initializing SoundEngine");

    let nl = NativeLink {
        al: AssetProvider::new(), prt: RenderTargetProvider(w),
        input: InputHandler::new()
    };
    let mut e = EngineW::launch(GameW::NAME, GameW::VERSION, nl).expect("Unable to launch the game");
    
    unsafe { SetWindowLongPtrA(w, GWLP_USERDATA, std::mem::transmute(&mut e)); }
    unsafe { ShowWindow(w, SW_SHOWNORMAL); }

    while process_message_all() { e.do_update(); }
}
extern "system" fn window_callback(w: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    match msg {
        WM_DESTROY => unsafe { PostQuitMessage(0); return 0; },
        WM_SIZE => unsafe {
            let ep = std::mem::transmute::<_, *mut EngineW>(GetWindowLongPtrA(w, GWLP_USERDATA));
            if let Some(ep) = ep.as_mut() {
                let (w, h) = (LOWORD(lparam as _), HIWORD(lparam as _));
                ep.do_resize_backbuffer(peridot::math::Vector2(w as _, h as _)); ep.do_update();
            }
            return 0;
        },
        _ => unsafe { DefWindowProcA(w, msg, wparam, lparam) }
    }
}

fn process_message_all() -> bool {
    let mut msg = unsafe { std::mem::uninitialized() };
    while unsafe { PeekMessageA(&mut msg, std::ptr::null_mut(), 0, 0, PM_REMOVE) != 0 } {
        if msg.message == WM_QUIT { return false; }
        unsafe { TranslateMessage(&mut msg); DispatchMessageA(&mut msg); }
    }
    return true;
}

type GameW = userlib::Game<NativeLink>;
type EngineW = peridot::Engine<GameW, NativeLink>;

use winapi::Interface;
use winapi::um::audioclient::*;
use winapi::um::audiosessiontypes::AUDCLNT_SHAREMODE_SHARED;
use winapi::um::combaseapi::{CoCreateInstance, CLSCTX_ALL, PropVariantClear, CoTaskMemFree};
use winapi::um::mmdeviceapi::*;
use winapi::um::propidl::*;
use winapi::um::propsys::IPropertyStore;
use winapi::um::coml2api::STGM_READ;
use winapi::um::strmif::REFERENCE_TIME;
use winapi::shared::devpkey::*;
use winapi::shared::mmreg::*;
use winapi::shared::ksmedia::KSDATAFORMAT_SUBTYPE_PCM;
use winapi::shared::winerror::{HRESULT, SUCCEEDED};
use std::io::{Result as IOResult, Error as IOError};
use std::os::windows::ffi::OsStringExt;
use std::ffi::OsString;
fn hr_into_result(hr: HRESULT) -> IOResult<()> {
    if SUCCEEDED(hr) { Ok(()) } else { Err(IOError::from_raw_os_error(hr)) }
}
pub struct MMDeviceEnumerator(*mut IMMDeviceEnumerator);
impl MMDeviceEnumerator {
    fn new() -> IOResult<Self> {
        let mut enumerator: *mut IMMDeviceEnumerator = std::ptr::null_mut();
        let hr = unsafe {
            CoCreateInstance(&CLSID_MMDeviceEnumerator, std::ptr::null_mut(),
                CLSCTX_ALL, &IMMDeviceEnumerator::uuidof(), std::mem::transmute(&mut enumerator))
        };

        hr_into_result(hr).map(|_| MMDeviceEnumerator(enumerator))
    }
}
impl Drop for MMDeviceEnumerator { fn drop(&mut self) { unsafe { (*self.0).Release(); } } }
struct MMDevice(*mut IMMDevice);
impl MMDeviceEnumerator {
    fn default_audio_endpoint(&self, dataflow: EDataFlow, role: ERole) -> IOResult<MMDevice> {
        let mut md = std::ptr::null_mut();
        let hr = unsafe {
            (*self.0).GetDefaultAudioEndpoint(dataflow, role, &mut md)
        };

        hr_into_result(hr).map(|_| MMDevice(md))
    }
}
impl Drop for MMDevice { fn drop(&mut self) { unsafe { (*self.0).Release(); } } }

pub struct MMDeviceProperties(*mut IPropertyStore);
impl MMDevice {
    pub fn properties(&self) -> IOResult<MMDeviceProperties> {
        let mut p = std::ptr::null_mut();
        let hr = unsafe { (*self.0).OpenPropertyStore(STGM_READ, &mut p) };

        hr_into_result(hr).map(|_| MMDeviceProperties(p))
    }
}
impl Drop for MMDeviceProperties { fn drop(&mut self) { unsafe { (*self.0).Release(); } } }

pub struct AudioClient(*mut IAudioClient);
impl Drop for AudioClient { fn drop(&mut self) { unsafe { (*self.0).Release(); } } }
impl MMDevice {
    pub fn activate(&self) -> IOResult<AudioClient> {
        let mut p = std::ptr::null_mut();
        let hr = unsafe {
            (*self.0).Activate(&IAudioClient::uuidof(), CLSCTX_ALL,
                std::ptr::null_mut(), &mut p)
        };

        hr_into_result(hr).map(|_| AudioClient(p as *mut _))
    }
}
impl AudioClient {
    pub fn initialize_shared(&self, buffer_duration: REFERENCE_TIME, wfx: &WAVEFORMATEX) -> IOResult<()> {
        let hr = unsafe {
            (*self.0).Initialize(AUDCLNT_SHAREMODE_SHARED, 0,
                buffer_duration, 0, wfx as *const _, std::ptr::null_mut())
        };

        hr_into_result(hr)
    }
    pub fn mix_format(&self) -> IOResult<WAVEFORMATEX> {
        let mut p = std::ptr::null_mut();
        hr_into_result(unsafe { (*self.0).GetMixFormat(&mut p as *mut _) })?;
        let r: WAVEFORMATEX = unsafe { *p };
        unsafe { CoTaskMemFree(p as *mut _) };
        return Ok(r);
    }

    pub fn buffer_size(&self) -> IOResult<usize> {
        let mut us = 0;
        let hr = unsafe { (*self.0).GetBufferSize(&mut us) };

        hr_into_result(hr).map(|_| us as usize)
    }
}

pub struct PropVariant(PROPVARIANT);
impl PropVariant {
    pub fn init() -> Self { unsafe { PropVariant(std::mem::zeroed()) } }
    pub unsafe fn as_osstr_unchecked(&self) -> OsString {
        let ptr = std::mem::transmute::<_, &[*const u16]>(&self.0.data[..])[0];
        let len = (0..).find(|&x| *ptr.offset(x) == 0).expect("Infinite Length");
        let slice = std::slice::from_raw_parts(ptr, len as _);
        
        OsString::from_wide(slice)
    }
}
impl Drop for PropVariant { fn drop(&mut self) { unsafe { PropVariantClear(&mut self.0); } } }
winapi::DEFINE_PROPERTYKEY!(PKEY_DeviceInterface_FriendlyName,
    0x026e516e, 0xb814, 0x414b, 0x83, 0xcd, 0x85, 0x6d, 0x6f, 0xef, 0x48, 0x22, 2);
winapi::DEFINE_PROPERTYKEY!(PKEY_Device_DeviceDesc,
    0xa45c254e, 0xdf1c, 0x4efd, 0x80, 0x20, 0x67, 0xd1, 0x46, 0xa8, 0x50, 0xe0, 2);
winapi::DEFINE_PROPERTYKEY!(PKEY_Device_FriendlyName,
    0xa45c254e, 0xdf1c, 0x4efd, 0x80, 0x20, 0x67, 0xd1, 0x46, 0xa8, 0x50, 0xe0, 14);
impl MMDeviceProperties {
    pub fn interface_friendly_name(&self) -> IOResult<String> {
        let mut pv = PropVariant::init();
        let hr = unsafe { (*self.0).GetValue(&PKEY_DeviceInterface_FriendlyName, &mut pv.0) };
        
        hr_into_result(hr).map(|_| unsafe {
            pv.as_osstr_unchecked().into_string().expect("Decoding FriendlyName")
        })
    }
    pub fn desc(&self) -> IOResult<String> {
        let mut pv = PropVariant::init();
        let hr = unsafe { (*self.0).GetValue(&PKEY_Device_DeviceDesc, &mut pv.0) };

        hr_into_result(hr).map(|_| unsafe {
            pv.as_osstr_unchecked().into_string().expect("Decoding DeviceDesc")
        })
    }
    pub fn friendly_name(&self) -> IOResult<String> {
        let mut pv = PropVariant::init();
        let hr = unsafe { (*self.0).GetValue(&PKEY_Device_FriendlyName, &mut pv.0) };

        hr_into_result(hr).map(|_| unsafe {
            pv.as_osstr_unchecked().into_string().expect("Decoding DeviceDesc")
        })
    }
}

struct NativeSoundEngine {

}
impl NativeSoundEngine {
    pub fn new() -> IOResult<Self> {
        let enumerator = MMDeviceEnumerator::new()?;
        let dev0 = enumerator.default_audio_endpoint(eRender, eConsole)?;
        
        println!("Audio Output Device: {}", dev0.properties()?.friendly_name()?);

        let aclient = dev0.activate().expect("activate");
        let (samples_per_sec, bits_per_sample) = (44100, 32);
        let wfx = WAVEFORMATEXTENSIBLE {
            Format: WAVEFORMATEX {
                wFormatTag: WAVE_FORMAT_EXTENSIBLE, nChannels: 2,
                nSamplesPerSec: samples_per_sec, wBitsPerSample: bits_per_sample,
                nBlockAlign: (bits_per_sample >> 3) * 2,
                nAvgBytesPerSec: samples_per_sec * 2 * (bits_per_sample >> 3) as u32,
                cbSize: std::mem::size_of::<WAVEFORMATEXTENSIBLE>() as _
            },
            Samples: bits_per_sample,
            dwChannelMask: SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT,
            SubFormat: KSDATAFORMAT_SUBTYPE_PCM
        };
        aclient.initialize_shared(10 * 1000 * 10, unsafe { std::mem::transmute(&wfx) }).expect("initialize");

        println!("Processing Buffer Size: {}", aclient.buffer_size()?);

        return Ok(NativeSoundEngine {});
    }
}

use std::rc::Rc;
use bedrock as br;
use std::path::PathBuf;

struct AssetProvider { base: PathBuf }
impl AssetProvider {
    fn new() -> Self {
        #[cfg(feature = "UseExternalAssetPath")] let base = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))] let base = {
            let mut exe = std::env::current_exe().expect("Unable to determine the location of exe file");
            exe.pop(); exe.push("/assets"); exe
        };
        trace!("Asset BaseDirectory={}", base.display());
        AssetProvider { base }
    }
}
impl peridot::PlatformAssetLoader for AssetProvider {
    type Asset = std::fs::File;
    type StreamingAsset = std::fs::File;

    fn get(&self, path: &str, ext: &str) -> std::io::Result<Self::Asset> {
        let mut p = self.base.clone();
        p.push(path.replace('.', "/"));
        p.set_extension(ext);
        return std::fs::File::open(&p);
    }
    fn get_streaming(&self, path: &str, ext: &str) -> std::io::Result<Self::StreamingAsset> {
        let mut p = self.base.clone();
        p.push(path.replace('.', "/"));
        p.set_extension(ext);
        return std::fs::File::open(&p);
    }
}
struct RenderTargetProvider(HWND);
impl peridot::PlatformRenderTarget for RenderTargetProvider {
    fn surface_extension_name(&self) -> &'static str { "VK_KHR_win32_surface" }
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
            -> br::Result<peridot::SurfaceInfo> {
        if !pd.win32_presentation_support(renderer_queue_family) {
            panic!("WindowSubsystem does not support Vulkan rendering");
        }
        let s = br::Surface::new_win32(vi, module_handle(), self.0)?;
        if !pd.surface_support(renderer_queue_family, &s)? {
            panic!("Vulkan does not support this surface to render");
        }
        return peridot::SurfaceInfo::gather_info(pd, s);
    }
    fn current_geometry_extent(&self) -> (usize, usize) { (0, 0) }
}
struct InputHandler(Option<Rc<peridot::InputProcess>>);
impl InputHandler {
    fn new() -> Self {
        InputHandler(None)
    }
}
impl peridot::InputProcessPlugin for InputHandler {
    fn on_start_handle(&mut self, processor: &Rc<peridot::InputProcess>) {
        self.0 = Some(processor.clone());
    }
}
struct NativeLink { al: AssetProvider, prt: RenderTargetProvider, input: InputHandler }
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = AssetProvider;
    type RenderTargetProvider = RenderTargetProvider;
    type InputProcessor = InputHandler;

    fn asset_loader(&self) -> &AssetProvider { &self.al }
    fn render_target_provider(&self) -> &RenderTargetProvider { &self.prt }
    fn input_processor_mut(&mut self) -> &mut InputHandler { &mut self.input }
}
