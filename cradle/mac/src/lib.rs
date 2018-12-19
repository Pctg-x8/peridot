#[macro_use]
extern crate objc;
#[macro_use] extern crate log;
extern crate appkit; use appkit::{NSString, NSRect};
extern crate libc; use libc::c_void;

extern crate peridot;
extern crate bedrock as br;
use std::fs::File;
use std::io::Result as IOResult;
use std::rc::Rc;

struct NSLogger;
impl log::Log for NSLogger {
    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            unsafe {
                let mut fmt = NSString::from_str(&format!("[{}] {}", record.level(), record.args())).expect("NSString");
                NSLog(&mut *fmt);
            }
        }
    }
    fn enabled(&self, metadata: &log::Metadata) -> bool { metadata.level() <= log::Level::Info }
    fn flush(&self) {}
}
static LOGGER: NSLogger = NSLogger;
extern "C" {
    fn NSLog(format: *mut NSString, ...);
}

// TODO: AssetLoader実装する
struct PlatformAssetLoader {}
impl PlatformAssetLoader {
    pub fn new() -> Self {
        PlatformAssetLoader {}
    }
}
impl peridot::AssetLoader for PlatformAssetLoader {
    type Asset = File;
    type StreamingAsset = File;

    fn get(&self, _path: &str, _ext: &str) -> IOResult<File> {
        unimplemented!("MacOSAssetLoader::get");
    }
    fn get_streaming(&self, _path: &str, _ext: &str) -> IOResult<File> {
        unimplemented!("MacOSAssetLoader::get_streaming");
    }
}
struct PlatformRenderTargetHandler(*mut c_void);
impl PlatformRenderTargetHandler {
    pub fn new(o: *mut c_void) -> Self {
        PlatformRenderTargetHandler(o)
    }
}
impl peridot::PlatformRenderTarget for PlatformRenderTargetHandler {
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
            -> br::Result<peridot::SurfaceInfo> {
        info!("create_surface: {:p}", self.0);
        let obj = br::Surface::new_macos(vi, self.0 as *const _)?;
        if !pd.surface_support(renderer_queue_family, &obj)? {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }
        return peridot::SurfaceInfo::gather_info(&pd, obj);
    }
    fn current_geometry_extent(&self) -> (usize, usize) {
        let NSRect { size, .. } = unsafe { msg_send![self.0 as *mut objc::runtime::Object, frame] };
        (size.width as _, size.height as _)
    }
}
// TODO: InputProcessPlugin実装する
pub(crate) struct PlatformInputProcessPlugin { processor: Option<Rc<peridot::InputProcess>> }
impl PlatformInputProcessPlugin {
    fn new() -> Self {
        PlatformInputProcessPlugin { processor: None }
    }
}
impl peridot::InputProcessPlugin for PlatformInputProcessPlugin {
    fn on_start_handle(&mut self, ip: &Rc<peridot::InputProcess>) {
        self.processor = Some(ip.clone());
    }
}
pub(self) struct PluginLoader { rt_view: *mut c_void, input: PlatformInputProcessPlugin }
impl PluginLoader {
    pub(self) fn new(rt_view: *mut c_void) -> Self {
        PluginLoader { rt_view, input: PlatformInputProcessPlugin::new() }
    }
}
impl peridot::PlatformPluginLoader for PluginLoader {
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = PlatformRenderTargetHandler;
    type InputProcessor = PlatformInputProcessPlugin;

    fn new_asset_loader(&self) -> PlatformAssetLoader { PlatformAssetLoader::new() }
    fn new_render_target_provider(&self) -> PlatformRenderTargetHandler { PlatformRenderTargetHandler::new(self.rt_view) }
    fn input_processor(&mut self) -> &mut PlatformInputProcessPlugin { &mut self.input }
    fn run_renderloop(&mut self, framerate: Option<f32>) {
        // TODO: CADisplayLinkで回す
    }
}
mod glib;
type Game = glib::Game<PlatformAssetLoader, PlatformRenderTargetHandler>;
type Engine = peridot::Engine<Game, PlatformAssetLoader, PlatformRenderTargetHandler>;

#[allow(dead_code)]
pub struct GameRun {
    plugin_loader: PluginLoader, engine: Engine
}
#[no_mangle]
pub extern "C" fn launch_game(v: *mut libc::c_void) -> *mut GameRun {
    log::set_logger(&LOGGER).expect("Failed to set logger");
    log::set_max_level(log::LevelFilter::Debug);

    let mut plugin_loader = PluginLoader::new(v);
    let engine = Engine::launch(Game::NAME, Game::VERSION, &mut plugin_loader).expect("Failed to launch the game");
    Box::into_raw(Box::new(GameRun { plugin_loader, engine }))
}
#[no_mangle]
pub extern "C" fn terminate_game(gr: *mut GameRun) {
    unsafe { drop(Box::from_raw(gr)); }
}
