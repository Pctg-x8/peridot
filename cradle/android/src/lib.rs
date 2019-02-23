//! peridot-cradle for android platform

#[macro_use] extern crate log;
extern crate libc;
extern crate android_logger;
extern crate bedrock; extern crate peridot_vertex_processing_pack;
extern crate android;

use std::ptr::null_mut;

mod userlib;

use peridot;
use self::userlib::Game;
use std::rc::Rc;

struct MainWindow { e: Option<EngineA> }
impl MainWindow {
    fn new() -> Self {
        MainWindow { e: None }
    }
    fn init(&mut self, app: &android::App) {
        let am = unsafe { AssetManager::from_ptr((*app.activity).asset_manager).expect("null assetmanager") };
        let nl = NativeLink {
            al: PlatformAssetLoader::new(am), prt: PlatformWindowHandler(app.window),
            input: PlatformInputProcessPlugin::new()
        };
        self.e = EngineA::launch(GameA::NAME, GameA::VERSION, nl).expect("Failed to initialize the engine").into();
    }
    fn render(&mut self)
    {
        if let Some(e) = self.e.as_mut() { e.do_update(); }
    }
}

use bedrock as br;
struct PlatformWindowHandler(*mut android::ANativeWindow);
impl peridot::PlatformRenderTarget for PlatformWindowHandler {
    fn surface_extension_name(&self) -> &'static str { "VK_KHR_android_surface" }
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
            -> br::Result<peridot::SurfaceInfo> {
        let obj = br::Surface::new_android(vi, self.0)?;
        if !pd.surface_support(renderer_queue_family, &obj)? {
            panic!("Vulkan Surface is not supported by this adapter");
        }
        return peridot::SurfaceInfo::gather_info(&pd, obj);
    }
    fn current_geometry_extent(&self) -> (usize, usize) {
        unsafe { ((*self.0).width() as _, (*self.0).height() as _) }
    }
}

struct PlatformInputProcessPlugin { processor: Option<Rc<peridot::InputProcess>> }
impl PlatformInputProcessPlugin {
    fn new() -> Self {
        PlatformInputProcessPlugin { processor: None }
    }
}
impl peridot::InputProcessPlugin for PlatformInputProcessPlugin {
    fn on_start_handle(&mut self, ip: &Rc<peridot::InputProcess>) {
        self.processor = Some(ip.clone());
        info!("Started Handling Inputs...");
    }
}

use android::{AssetManager, Asset, AASSET_MODE_STREAMING, AASSET_MODE_RANDOM};
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::ffi::CString;
struct PlatformAssetLoader { amgr: AssetManager }
impl PlatformAssetLoader {
    fn new(amgr: AssetManager) -> Self { PlatformAssetLoader { amgr } }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type Asset = Asset;
    type StreamingAsset = Asset;

    fn get(&self, path: &str, ext: &str) -> IOResult<Asset> {
        let mut path_str = path.replace(".", "/"); path_str.push('.'); path_str.push_str(ext);
        let path_str = CString::new(path_str).expect("converting path");
        self.amgr.open(path_str.as_ptr(), AASSET_MODE_RANDOM).ok_or(IOError::new(ErrorKind::NotFound, ""))
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Asset> {
        let mut path_str = path.replace(".", "/"); path_str.push('.'); path_str.push_str(ext);
        let path_str = CString::new(path_str).expect("converting path");
        self.amgr.open(path_str.as_ptr(), AASSET_MODE_STREAMING).ok_or(IOError::new(ErrorKind::NotFound, ""))
    }
}
struct NativeLink {
    al: PlatformAssetLoader, prt: PlatformWindowHandler, input: PlatformInputProcessPlugin
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = PlatformWindowHandler;
    type InputProcessor = PlatformInputProcessPlugin;

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn render_target_provider(&self) -> &PlatformWindowHandler { &self.prt }
    fn input_processor_mut(&mut self) -> &mut PlatformInputProcessPlugin { &mut self.input }
}
type GameA = Game<NativeLink>;
type EngineA = peridot::Engine<GameA, NativeLink>;

#[no_mangle]
pub extern "C" fn android_main(app: *mut android::App) {
    let app = unsafe { app.as_mut().expect("null app") };
    app.on_app_cmd = Some(appcmd_callback);
    let mut mw = MainWindow::new();
    app.user_data = unsafe { std::mem::transmute(&mut mw) };

    android_logger::init_once(
        android_logger::Filter::default()
            .with_min_level(log::Level::Trace)
    );
    info!("Launching NativeActivity: {:p}", app);
    std::panic::set_hook(Box::new(|p| {
        error!("Panicking in app: {}", p);
    }));

    'alp: loop {
        let (mut _outfd, mut events, mut source) = (0, 0, null_mut::<android::PollSource>());
        while android::Looper::poll_all(0, &mut _outfd, &mut events, unsafe { std::mem::transmute(&mut source) }) >= 0 {
            if let Some(sref) = unsafe { source.as_mut() } { sref.process(app); }
            if app.destroy_requested != 0 { break 'alp; }
        }
        mw.render();
    }
}

pub extern "C" fn appcmd_callback(app: *mut android::App, cmd: i32) {
    let app = unsafe { app.as_mut().expect("null app") };
    let mw = unsafe { std::mem::transmute::<_, *mut MainWindow>(app.user_data).as_mut().expect("null window") };

    match cmd {
        android::APP_CMD_INIT_WINDOW => {
            trace!("Initializing Window...");
            mw.init(app);
        },
        android::APP_CMD_TERM_WINDOW => {
            trace!("Terminating Window...");
        },
        e => trace!("Unknown Event: {}", e)
    }
}
