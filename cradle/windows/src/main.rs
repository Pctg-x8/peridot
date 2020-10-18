use winapi::um::winuser::{
    DefWindowProcA, CreateWindowExA, PeekMessageA, DispatchMessageA, TranslateMessage, WNDCLASSEXA, RegisterClassExA,
    AdjustWindowRectEx, WS_OVERLAPPEDWINDOW, WS_EX_APPWINDOW, CW_USEDEFAULT, ShowWindow, SW_SHOWNORMAL, WM_SIZE,
    PostQuitMessage, PM_REMOVE,
    LoadCursorA, IDC_ARROW, SetWindowLongPtrA, GetWindowLongPtrA, GWLP_USERDATA
};
use winapi::um::shellscalingapi::{SetProcessDpiAwareness, PROCESS_SYSTEM_DPI_AWARE};
use winapi::um::winuser::{WM_DESTROY, WM_QUIT};
use winapi::um::libloaderapi::GetModuleHandleA;
use winapi::shared::windef::{RECT, HWND};
use winapi::shared::minwindef::{LRESULT, WPARAM, LPARAM, UINT, HINSTANCE, LOWORD, HIWORD};

use std::mem::MaybeUninit;
#[macro_use] extern crate log;
mod userlib;
use peridot::{EngineEvents, FeatureRequests};

const LPSZCLASSNAME: &str = concat!(env!("PERIDOT_WINDOWS_APPID"), ".mainWindow\0");

fn module_handle() -> HINSTANCE { unsafe { GetModuleHandleA(std::ptr::null()) } }

pub struct GameDriver
{
    base: peridot::Engine<NativeLink>,
    usercode: userlib::Game<NativeLink>,
    current_size: peridot::math::Vector2<usize>
}
impl GameDriver
{
    fn new(window: HWND, init_size: peridot::math::Vector2<usize>) -> Self
    {
        let nl = NativeLink
        {
            al: AssetProvider::new(),
            window,
            input: InputHandler::new()
        };
        let mut base = peridot::Engine::new(
            userlib::Game::<NativeLink>::NAME, userlib::Game::<NativeLink>::VERSION,
            nl, userlib::Game::<NativeLink>::requested_features()
        );
        let usercode = userlib::Game::init(&base);
        base.postinit();

        GameDriver
        {
            base, usercode, current_size: init_size
        }
    }

    fn update(&mut self)
    {
        self.base.do_update(&mut self.usercode);
    }
    fn resize(&mut self, size: peridot::math::Vector2<usize>)
    {
        self.base.do_resize_backbuffer(size, &mut self.usercode);
    }
}

fn main()
{
    env_logger::init();

    unsafe { SetProcessDpiAwareness(PROCESS_SYSTEM_DPI_AWARE); }

    let wca = WNDCLASSEXA {
        cbSize: std::mem::size_of::<WNDCLASSEXA>() as _, hInstance: module_handle(),
        lpszClassName: LPSZCLASSNAME.as_ptr() as *const _,
        lpfnWndProc: Some(window_callback),
        hCursor: unsafe { LoadCursorA(std::ptr::null_mut(), IDC_ARROW as _) },
        .. unsafe { MaybeUninit::zeroed().assume_init() }
    };
    let wcatom = unsafe { RegisterClassExA(&wca) };
    if wcatom <= 0 { panic!("Register Class Failed!"); }

    let wname = format!("{} v{}.{}.{}",
        userlib::Game::<NativeLink>::NAME,
        userlib::Game::<NativeLink>::VERSION.0,
        userlib::Game::<NativeLink>::VERSION.1,
        userlib::Game::<NativeLink>::VERSION.2);
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
    
    let mut driver = GameDriver::new(w, peridot::math::Vector2(640, 480));
    unsafe { SetWindowLongPtrA(w, GWLP_USERDATA, &mut driver as *mut GameDriver as _); }
    unsafe { ShowWindow(w, SW_SHOWNORMAL); }

    while process_message_all() { driver.update(); }
}
extern "system" fn window_callback(w: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM) -> LRESULT
{
    match msg
    {
        WM_DESTROY => unsafe { PostQuitMessage(0); return 0; },
        WM_SIZE => unsafe
        {
            let p = GetWindowLongPtrA(w, GWLP_USERDATA) as *mut GameDriver;
            if let Some(driver) = p.as_mut()
            {
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
        _ => unsafe { DefWindowProcA(w, msg, wparam, lparam) }
    }
}

fn process_message_all() -> bool
{
    let mut msg = MaybeUninit::uninit();
    while unsafe { PeekMessageA(msg.as_mut_ptr(), std::ptr::null_mut(), 0, 0, PM_REMOVE) != 0 }
    {
        if unsafe { (*msg.as_ptr()).message } == WM_QUIT { return false; }
        unsafe { TranslateMessage(msg.as_mut_ptr()); DispatchMessageA(msg.as_mut_ptr()); }
    }
    
    true
}

use std::rc::Rc;
use bedrock as br;
use std::path::PathBuf;

struct AssetProvider { base: PathBuf }
impl AssetProvider
{
    fn new() -> Self
    {
        #[cfg(feature = "UseExternalAssetPath")] let base = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))] let base =
        {
            let mut exe = std::env::current_exe().expect("Unable to determine the location of exe file");
            exe.pop(); exe.push("/assets"); exe
        };
        trace!("Asset BaseDirectory={}", base.display());
        AssetProvider { base }
    }
}
impl peridot::PlatformAssetLoader for AssetProvider
{
    type Asset = std::fs::File;
    type StreamingAsset = std::fs::File;

    fn get(&self, path: &str, ext: &str) -> std::io::Result<Self::Asset>
    {
        let mut p = self.base.clone();
        p.push(path.replace('.', "/"));
        p.set_extension(ext);
        return std::fs::File::open(&p);
    }
    fn get_streaming(&self, path: &str, ext: &str) -> std::io::Result<Self::StreamingAsset>
    {
        let mut p = self.base.clone();
        p.push(path.replace('.', "/"));
        p.set_extension(ext);
        return std::fs::File::open(&p);
    }
}
struct Presenter {
    window: HWND,
    sc: peridot::IntegratedSwapchain
}
impl Presenter {
    fn new(g: &peridot::Graphics, window: HWND) -> Self {
        if !g.adapter().win32_presentation_support(g.graphics_queue_family_index()) {
            panic!("WindowSubsystem does not support Vulkan Rendering");
        }
        let s = br::Surface::new_win32(g.instance(), module_handle(), window).expect("Failed to create Surface");
        let support = g.adapter().surface_support(g.graphics_queue_family_index(), &s)
            .expect("Failed to query Surface Support");
        if !support {
            panic!("Vulkan does not support this surface to render");
        }

        Presenter {
            window,
            sc: peridot::IntegratedSwapchain::new(g, s, peridot::math::Vector2(0, 0))
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat { self.sc.format() }
    fn backbuffer_count(&self) -> usize { self.sc.backbuffer_count() }
    fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> { self.sc.backbuffer(index) }

    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        self.sc.emit_initialize_backbuffer_commands(recorder)
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> { self.sc.acquire_next_backbuffer_index() }
    fn render_and_present<'s>(
        &'s mut self,
        g: &peridot::Graphics,
        last_render_fence: &br::Fence,
        present_queue: &br::Queue,
        backbuffer_index: u32,
        render_submission: br::SubmissionBatch<'s>,
        update_submission: Option<br::SubmissionBatch<'s>>
    ) -> br::Result<()> {
        self.sc.render_and_present(
            g, last_render_fence, present_queue, backbuffer_index, render_submission, update_submission
        )
    }
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }
    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> { peridot::math::Vector2(0, 0) }
}
struct InputHandler(Option<Rc<peridot::InputProcess>>);
impl InputHandler
{
    fn new() -> Self
    {
        InputHandler(None)
    }
}
impl peridot::InputProcessPlugin for InputHandler
{
    fn on_start_handle(&mut self, processor: &Rc<peridot::InputProcess>)
    {
        self.0 = Some(processor.clone());
    }
}
struct NativeLink { al: AssetProvider, window: HWND, input: InputHandler }
impl peridot::NativeLinker for NativeLink
{
    type AssetLoader = AssetProvider;
    type Presenter = Presenter;
    type InputProcessor = InputHandler;

    fn instance_extensions(&self) -> Vec<&str> { vec!["VK_KHR_surface", "VK_KHR_win32_surface"] }
    fn device_extensions(&self) -> Vec<&str> { vec!["VK_KHR_swapchain"] }

    fn asset_loader(&self) -> &AssetProvider { &self.al }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter { Presenter::new(g, self.window) }
    fn input_processor_mut(&mut self) -> &mut InputHandler { &mut self.input }
}
