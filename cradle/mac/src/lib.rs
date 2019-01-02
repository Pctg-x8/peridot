#[macro_use]
extern crate objc;
#[macro_use] extern crate log;
extern crate appkit; use appkit::{NSString, NSRect, CocoaObject};
extern crate libc; use libc::c_void;

extern crate peridot;
extern crate bedrock as br;
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::io::Cursor;
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
    fn nsbundle_path_for_resource(name: *mut NSString, oftype: *mut NSString) -> *mut objc::runtime::Object;
}

// TODO: AssetLoader実装する
pub struct PlatformAssetLoader {}
impl PlatformAssetLoader {
    fn new() -> Self {
        PlatformAssetLoader {}
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type Asset = Cursor<Vec<u8>>;
    type StreamingAsset = Cursor<Vec<u8>>;

    fn get(&self, path: &str, ext: &str) -> IOResult<Cursor<Vec<u8>>> {
        let mut pathbase = NSString::from_str("assets").expect("NSString for pathbase");
        let mut pathext = NSString::from_str("par").expect("NSString for ext");
        let par_path: CocoaObject<NSString> = unsafe {
            CocoaObject::from_id(nsbundle_path_for_resource(&mut *pathbase, &mut *pathext)).expect("No Return Value")
        };
        let mut arc = peridot::archive::ArchiveRead::from_file(par_path.to_str(), false)?;
        let b = arc.read_bin(&format!("{}.{}", path.replace(".", "/"), ext))?;
        match b {
            None => Err(IOError::new(ErrorKind::NotFound, "not in primary asset package")),
            Some(b) => Ok(Cursor::new(b))
        }
    }
    fn get_streaming(&self, _path: &str, _ext: &str) -> IOResult<Cursor<Vec<u8>>> {
        unimplemented!("MacOSAssetLoader::get_streaming");
    }
}
pub struct PlatformRenderTargetHandler(*mut c_void);
impl PlatformRenderTargetHandler {
    fn new(o: *mut c_void) -> Self {
        PlatformRenderTargetHandler(o)
    }
}
impl peridot::PlatformRenderTarget for PlatformRenderTargetHandler {
    fn surface_extension_name(&self) -> &'static str { "VK_MVK_macos_surface" }
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
impl peridot::PluginLoader for PluginLoader {
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = PlatformRenderTargetHandler;
    type InputProcessor = PlatformInputProcessPlugin;

    fn new_asset_loader(&self) -> PlatformAssetLoader { PlatformAssetLoader::new() }
    fn new_render_target_provider(&self) -> PlatformRenderTargetHandler { PlatformRenderTargetHandler::new(self.rt_view) }
    fn input_processor(&mut self) -> &mut PlatformInputProcessPlugin { &mut self.input }
}
pub struct NativeLink { al: PlatformAssetLoader, prt: PlatformRenderTargetHandler }
impl peridot::PlatformLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = PlatformRenderTargetHandler;

    fn new(al: PlatformAssetLoader, prt: PlatformRenderTargetHandler) -> Self {
        NativeLink { al, prt }
    }
    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn render_target_provider(&self) -> &PlatformRenderTargetHandler { &self.prt }
}
mod userlib;
type Game = userlib::Game<NativeLink>;
type Engine = peridot::Engine<Game, NativeLink>;

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
#[no_mangle]
pub extern "C" fn update_game(gr: *mut GameRun) {
    unsafe { (*gr).engine.do_update(); }
}
