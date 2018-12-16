use winapi::um::winuser::{
    DefWindowProcA, CreateWindowExA, GetMessageA, DispatchMessageA, TranslateMessage, WNDCLASSEXA, RegisterClassExA,
    AdjustWindowRectEx, WS_OVERLAPPEDWINDOW, WS_EX_APPWINDOW, CW_USEDEFAULT, ShowWindow, SW_SHOWNORMAL,
    PostQuitMessage, WM_DESTROY
};
use winapi::um::libloaderapi::{GetModuleHandleA};
use winapi::shared::windef::{RECT, HWND};
use winapi::shared::minwindef::{LRESULT, WPARAM, LPARAM, UINT, HINSTANCE};

mod userlib;

const LPSZCLASSNAME: &str = "Peridot::Cradle::MainWindow\0";

fn module_handle() -> HINSTANCE { unsafe { GetModuleHandleA(std::ptr::null()) } }

fn main() {
    let wca = WNDCLASSEXA {
        cbSize: std::mem::size_of::<WNDCLASSEXA>() as _, hInstance: module_handle(),
        lpszClassName: LPSZCLASSNAME.as_ptr() as *const _,
        lpfnWndProc: Some(window_callback),
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

    let mut input_handler = InputHandler::new();
    let e = EngineW::launch(GameW::NAME, GameW::VERSION, RenderTargetProvider(w), AssetProvider, &mut input_handler)
        .expect("Unable to launch the game");
    
    unsafe { ShowWindow(w, SW_SHOWNORMAL); }

    let mut msg = unsafe { std::mem::uninitialized() };
    while unsafe { GetMessageA(&mut msg, std::ptr::null_mut(), 0, 0) > 0 } {
        unsafe {
            TranslateMessage(&mut msg);
            DispatchMessageA(&mut msg);
        }
    }
}
extern "system" fn window_callback(w: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    match msg {
        WM_DESTROY => unsafe { PostQuitMessage(0); return 0; },
        _ => unsafe { DefWindowProcA(w, msg, wparam, lparam) }
    }
}

use std::rc::Rc;
use bedrock as br;

type GameW = userlib::Game<AssetProvider, RenderTargetProvider>;
type EngineW = peridot::Engine<GameW, AssetProvider, RenderTargetProvider>;

// TODO AssetLoaderを実装する
struct AssetProvider;
impl peridot::AssetLoader for AssetProvider {
    type Asset = std::fs::File;
    type StreamingAsset = std::fs::File;

    fn get(&self, path: &str, ext: &str) -> std::io::Result<Self::Asset> {
        unimplemented!("Win32 AssetLoader::get");
    }
    fn get_streaming(&self, path: &str, ext: &str) -> std::io::Result<Self::StreamingAsset> {
        unimplemented!("Win32 AssetLoader::get_streaming");
    }
}
struct RenderTargetProvider(HWND);
impl peridot::PlatformRenderTarget for RenderTargetProvider {
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
