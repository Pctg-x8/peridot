//! Custom Render Target

use libc::{c_char, c_void};
use bedrock as br; use br::traits::*;
use bedrock::vk::{VkSurfaceKHR, VkInstance};

// Interfacing //
#[repr(C)]
struct RenderTargetInfo
{
    vk_surface_extension_name: *const c_char,
    surface_factory_context_ptr: *mut c_void,
    surface_factory: extern "C" fn(context: *mut c_void, instance: VkInstance) -> VkSurfaceKHR,
    get_current_extent: extern "C" fn(context: *mut c_void, width: *mut u32, height: *mut u32)
}
#[repr(C)]
struct RuntimeInfo
{
    render_target_info: *const RenderTargetInfo,
    asset_base_path: *const c_char
}

#[no_mangle]
pub extern "C" fn pecrtInitGame(rt_info: *const RuntimeInfo) -> *mut c_void
{
    let rti_ref = unsafe { rt_info.as_ref().expect("null rt_info") };

    let nlink = NativeLink
    {
        al: CustomAssetLoader::new(unsafe { CStr::from_ptr(rti_ref.asset_base_path) }),
        prt: CustomRenderTargetHandler::new(rti_ref.render_target_info),
        input: PlatformInputProcessPlugin::new()
    };
    let engine = Engine::launch(Game::NAME, Game::VERSION, nlink).expect("Failed to launch the game");
    Box::into_raw(Box::new(GameRun(engine))) as _
}
#[no_mangle]
pub extern "C" fn pecrtEndGame(core: *mut c_void)
{
    drop(Box::from_raw(core as *mut GameRun));
}

use std::io::Result as IOResult;
use std::ffi::CStr;
use std::fs::File;
use std::path::PathBuf;
use std::rc::Rc;
pub struct CustomAssetLoader { base_path: String }
impl CustomAssetLoader
{
    fn new(base_path: &CStr) -> Self
    {
        CustomAssetLoader
        {
            base_path: base_path.to_string_lossy().into_owned()
        }
    }
}
impl peridot::PlatformAssetLoader for CustomAssetLoader
{
    type Asset = File;
    type StreamingAsset = File;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>
    {
        let mut apath = PathBuf::from(self.base_path.clone());
        apath.push(path.replace(".", "/")); apath.set_extension(ext);
        return File::open(apath);
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::Asset> { self.get(path, ext) }
}
pub struct CustomRenderTargetHandler(*const RenderTargetInfo);
impl CustomRenderTargetHandler
{
    fn new(o: *const RenderTargetInfo) -> Self
    {
        CustomRenderTargetHandler(o)
    }
}
impl peridot::PlatformRenderTarget for CustomRenderTargetHandler
{
    fn surface_extension_name(&self) -> &'static str
    {
        unsafe { CStr::from_ptr((*self.0).vk_surface_extension_name).to_str().expect("invalid utf-8 sequence") }
    }
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
        -> br::Result<peridot::SurfaceInfo>
    {
        let obj = unsafe
        {
            br::Surface::from_raw(((*self.0).surface_factory)(
                (*self.0).surface_factory_context_ptr, vi.native_ptr()), vi)
        };
        if !pd.surface_support(renderer_queue_family, &obj)? {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }
        return peridot::SurfaceInfo::gather_info(&pd, obj);
    }
    fn current_geometry_extent(&self) -> (usize, usize)
    {
        let (mut w, mut h) = (0, 0);
        unsafe { ((*self.0).get_current_extent)((*self.0).surface_factory_context_ptr, &mut w, &mut h) };

        (w as _, h as _)
    }
}
// TODO: InputProcessPlugin実装する
pub struct PlatformInputProcessPlugin { processor: Option<Rc<peridot::InputProcess>> }
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

pub struct NativeLink
{
    al: CustomAssetLoader,
    prt: CustomRenderTargetHandler,
    input: PlatformInputProcessPlugin
}
impl peridot::NativeLinker for NativeLink
{
    type AssetLoader = CustomAssetLoader;
    type RenderTargetProvider = CustomRenderTargetHandler;
    type InputProcessor = PlatformInputProcessPlugin;

    fn asset_loader(&self) -> &CustomAssetLoader { &self.al }
    fn render_target_provider(&self) -> &CustomRenderTargetHandler { &self.prt }
    fn input_processor_mut(&mut self) -> &mut PlatformInputProcessPlugin { &mut self.input }
}
mod userlib;
type Game = userlib::Game<NativeLink>;
type Engine = peridot::Engine<Game, NativeLink>;

#[allow(dead_code)]
pub struct GameRun(Engine);
