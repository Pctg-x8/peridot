//! peridot-cradle for android platform

use log::*;

mod userlib;

use peridot::{EngineEvents, FeatureRequests};
use std::rc::Rc;

struct Game
{
    engine: peridot::Engine<NativeLink>,
    userlib: userlib::Game<NativeLink>
}
impl Game
{
    fn new(asset_manager: AssetManager, window: *mut android::ANativeWindow) -> Self
    {
        let nl = NativeLink
        {
            al: PlatformAssetLoader::new(asset_manager),
            prt: PlatformWindowHandler(window),
            input: PlatformInputProcessPlugin::new()
        };
        let engine = peridot::Engine::new(
            userlib::Game::<NativeLink>::NAME, userlib::Game::<NativeLink>::VERSION,
            nl, userlib::Game::<NativeLink>::requested_features()
        );

        Game
        {
            userlib: userlib::Game::init(&engine),
            engine
        }
    }

    fn update(&mut self) { self.engine.do_update(&mut self.userlib); }
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
        
        peridot::SurfaceInfo::gather_info(&pd, obj)
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

// JNI Exports //

use jni::{JNIEnv, objects::{JByteBuffer, JObject, JClass}};

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
    let e = Game::new(am, window);

    let ptr = Box::into_raw(Box::new(e));
    env.new_direct_byte_buffer(unsafe { std::slice::from_raw_parts_mut(ptr as *mut u8, 0) })
        .expect("Creating DirectByteBuffer failed")
}
#[no_mangle]
pub extern "system" fn Java_com_cterm2_peridot_NativeLibLink_fin(e: JNIEnv, _: JClass, obj: JByteBuffer)
{
    info!("Finalizing NativeGameEngine...");
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    drop(unsafe { Box::from_raw(bytes.as_ptr() as *mut Game) });
}
#[no_mangle]
pub extern "system" fn Java_com_cterm2_peridot_NativeLibLink_update(e: JNIEnv, _: JClass, obj: JByteBuffer)
{
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    let e = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    e.update();
}
