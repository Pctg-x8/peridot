//! peridot-cradle for android platform

#[macro_use] extern crate log;
extern crate libc;
extern crate android_logger;
extern crate bedrock;
extern crate android;

use std::ptr::null_mut;

mod userlib;

use peridot;
use self::userlib::Game;
use std::rc::Rc;

struct MainWindow { e: Option<EngineA>, stopping_render: bool }
impl MainWindow {
    fn new() -> Self {
        MainWindow { e: None, stopping_render: true }
    }
    fn init(&mut self, app: &android::App) {
        let am = unsafe { AssetManager::from_ptr((*app.activity).asset_manager).expect("null assetmanager") };
        let nl = NativeLink {
            al: PlatformAssetLoader::new(am), prt: PlatformWindowHandler(app.window),
            input: PlatformInputProcessPlugin::new()
        };
        self.e = EngineA::launch(GameA::NAME, GameA::VERSION, nl).expect("Failed to initialize the engine").into();
        self.stopping_render = false;
    }
    fn destroy(&mut self)
    {
        self.e = None;
        self.stopping_render = true;
    }
    fn stop_render(&mut self) { self.stopping_render = true; }
    fn render(&mut self)
    {
        if self.stopping_render { return; }

        if let Some(e) = self.e.as_mut() { e.do_update(); }
    }
}

use bedrock as br;
struct PlatformWindowHandler(*mut android::ANativeWindow);
impl peridot::PlatformRenderTarget for PlatformWindowHandler
{
    fn surface_extension_name(&self) -> &'static str { "VK_KHR_android_surface" }
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
            -> br::Result<peridot::SurfaceInfo>
    {
        let obj = br::Surface::new_android(vi, self.0)?;
        if !pd.surface_support(renderer_queue_family, &obj)?
        {
            panic!("Vulkan Surface is not supported by this adapter");
        }
        return peridot::SurfaceInfo::gather_info(&pd, obj);
    }
    fn current_geometry_extent(&self) -> (usize, usize)
    {
        unsafe { ((*self.0).width() as _, (*self.0).height() as _) }
    }
}

struct PlatformInputProcessPlugin { processor: Option<Rc<peridot::InputProcess>> }
impl PlatformInputProcessPlugin
{
    fn new() -> Self
    {
        PlatformInputProcessPlugin { processor: None }
    }
}
impl peridot::InputProcessPlugin for PlatformInputProcessPlugin
{
    fn on_start_handle(&mut self, ip: &Rc<peridot::InputProcess>)
    {
        self.processor = Some(ip.clone());
        info!("Started Handling Inputs...");
    }
}

use android::{AssetManager, Asset, AASSET_MODE_STREAMING, AASSET_MODE_RANDOM};
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::ffi::CString;
struct PlatformAssetLoader { amgr: AssetManager }
impl PlatformAssetLoader
{
    fn new(amgr: AssetManager) -> Self { PlatformAssetLoader { amgr } }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader
{
    type Asset = Asset;
    type StreamingAsset = Asset;

    fn get(&self, path: &str, ext: &str) -> IOResult<Asset>
    {
        let mut path_str = path.replace(".", "/"); path_str.push('.'); path_str.push_str(ext);
        let path_str = CString::new(path_str).expect("converting path");
        self.amgr.open(path_str.as_ptr(), AASSET_MODE_RANDOM).ok_or(IOError::new(ErrorKind::NotFound, ""))
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Asset>
    {
        let mut path_str = path.replace(".", "/"); path_str.push('.'); path_str.push_str(ext);
        let path_str = CString::new(path_str).expect("converting path");
        self.amgr.open(path_str.as_ptr(), AASSET_MODE_STREAMING).ok_or(IOError::new(ErrorKind::NotFound, ""))
    }
}
struct NativeLink
{
    al: PlatformAssetLoader, prt: PlatformWindowHandler, input: PlatformInputProcessPlugin
}
impl peridot::NativeLinker for NativeLink
{
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = PlatformWindowHandler;
    type InputProcessor = PlatformInputProcessPlugin;

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn render_target_provider(&self) -> &PlatformWindowHandler { &self.prt }
    fn input_processor_mut(&mut self) -> &mut PlatformInputProcessPlugin { &mut self.input }
}
type GameA = Game<NativeLink>;
type EngineA = peridot::Engine<GameA, NativeLink>;

// JNI Exports //

use jni::{JNIEnv, objects::{JByteBuffer, JObject, JClass}};

pub struct EngineInstances
{
    e: EngineA
}
#[no_mangle]
pub extern "system" fn Java_com_cterm2_peridot_NativeLibLink_init<'e>(
    env: JNIEnv<'e>, _: JClass,
    surface: JObject, asset_manager: JObject) -> JByteBuffer<'e>
{
    android_logger::init_once(
        android_logger::Filter::default().with_min_level(log::Level::Trace)
    );
    info!("Initializing NativeGameEngine...");

    std::panic::set_hook(Box::new(|p| { error!("Panicking in app! {}", p); }));
    
    let window = unsafe { android::ANativeWindow_fromSurface(env.clone(), surface) };
    let am = unsafe { AssetManager::from_java(env.clone(), asset_manager).expect("null assetmanager") };
    let nl = NativeLink
    {
        al: PlatformAssetLoader::new(am),
        prt: PlatformWindowHandler(window),
        input: PlatformInputProcessPlugin::new()
    };
    let e = EngineA::launch(GameA::NAME, GameA::VERSION, nl).expect("Failed to initialize the engine");
    let instances = Box::new(EngineInstances
    {
        e
    });

    let ptr = Box::into_raw(instances);
    env.new_direct_byte_buffer(unsafe { std::slice::from_raw_parts_mut(ptr as *mut u8, 0) })
        .expect("Creating DirectByteBuffer failed")
}
#[no_mangle]
pub extern "system" fn Java_com_cterm2_peridot_NativeLibLink_fin(e: JNIEnv, _: JClass, obj: JByteBuffer)
{
    info!("Finalizing NativeGameEngine...");
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    let e = unsafe { Box::from_raw(std::mem::transmute::<_, *mut EngineInstances>(bytes.as_ptr())) };
    
    drop(e);
}
#[no_mangle]
pub extern "system" fn Java_com_cterm2_peridot_NativeLibLink_update(e: JNIEnv, _: JClass, obj: JByteBuffer)
{
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    let e = unsafe { (bytes.as_ptr() as *mut EngineInstances).as_mut().expect("null ptr?") };

    e.e.do_update();
}
