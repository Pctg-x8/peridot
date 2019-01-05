use winapi::um::winuser::{
    DefWindowProcA, CreateWindowExA, PeekMessageA, DispatchMessageA, TranslateMessage, WNDCLASSEXA, RegisterClassExA,
    AdjustWindowRectEx, WS_OVERLAPPEDWINDOW, WS_EX_APPWINDOW, CW_USEDEFAULT, ShowWindow, SW_SHOWNORMAL,
    PostQuitMessage, PM_REMOVE,
    LoadCursorA, IDC_ARROW
};
use winapi::um::winuser::{WM_DESTROY, WM_QUIT};
use winapi::um::libloaderapi::{GetModuleHandleA};
use winapi::shared::windef::{RECT, HWND};
use winapi::shared::minwindef::{LRESULT, WPARAM, LPARAM, UINT, HINSTANCE};

#[macro_use] extern crate log;
mod userlib;

const LPSZCLASSNAME: &str = concat!(env!("PERIDOT_WINDOWS_APPID"), ".mainWindow\0");

fn module_handle() -> HINSTANCE { unsafe { GetModuleHandleA(std::ptr::null()) } }

fn main() {
    env_logger::init();

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

    let nl = NativeLink {
        al: AssetProvider::new(), prt: RenderTargetProvider(w),
        input: InputHandler::new()
    };
    let mut e = EngineW::launch(GameW::NAME, GameW::VERSION, nl).expect("Unable to launch the game");
    
    unsafe { ShowWindow(w, SW_SHOWNORMAL); }

    while process_message_all() { e.do_update(); }
}
extern "system" fn window_callback(w: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    match msg {
        WM_DESTROY => unsafe { PostQuitMessage(0); return 0; },
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
