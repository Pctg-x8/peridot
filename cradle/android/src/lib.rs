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
use std::sync::{Arc, RwLock};
use std::pin::Pin;

struct MainWindow
{
    e: Option<EngineA>,
    snd: Option<NativeAudioEngine>,
    stopping_render: bool
}
impl MainWindow
{
    fn new() -> Self
    {
        MainWindow { e: None, snd: None, stopping_render: true }
    }
    fn init(&mut self, app: &android::App)
    {
        let am = unsafe { AssetManager::from_ptr((*app.activity).asset_manager).expect("null assetmanager") };
        let nl = NativeLink
        {
            al: PlatformAssetLoader::new(am), prt: PlatformWindowHandler(app.window),
            input: PlatformInputProcessPlugin::new()
        };
        let e = EngineA::launch(GameA::NAME, GameA::VERSION, nl).expect("Failed to initialize the engine");
        let snd = NativeAudioEngine::new(e.audio_mixer());
        self.e = Some(e);
        self.snd = Some(snd);
        self.stopping_render = false;
    }
    fn destroy(&mut self)
    {
        if let Some(ref mut s) = self.snd { s.pause(); }
        self.e = None;
        self.snd = None;
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

#[no_mangle]
pub extern "C" fn android_main(app: *mut android::App)
{
    let app = unsafe { app.as_mut().expect("null app") };
    app.on_app_cmd = Some(appcmd_callback);
    let mut mw = MainWindow::new();
    app.user_data = unsafe { std::mem::transmute(&mut mw) };

    android_logger::init_once(
        android_logger::Filter::default().with_min_level(log::Level::Trace)
    );
    info!("Launching NativeActivity: {:p}", app);
    std::panic::set_hook(Box::new(|p| { error!("Panicking in app: {}", p); }));

    'alp: loop
    {
        let (mut _outfd, mut events, mut source) = (0, 0, null_mut::<android::PollSource>());
        while android::Looper::poll_all(0, &mut _outfd, &mut events, unsafe { std::mem::transmute(&mut source) }) >= 0
        {
            if let Some(sref) = unsafe { source.as_mut() } { sref.process(app); }
            if app.destroy_requested != 0 { break 'alp; }
        }
        mw.render();
    }
}

pub extern "C" fn appcmd_callback(app: *mut android::App, cmd: i32)
{
    let app = unsafe { app.as_mut().expect("null app") };
    let mw = unsafe { (app.user_data as *mut MainWindow).as_mut().expect("null window") };

    match cmd
    {
        android::APP_CMD_INIT_WINDOW =>
        {
            trace!("Initializing Window...");
            mw.init(app);
        },
        android::APP_CMD_TERM_WINDOW =>
        {
            trace!("Terminating Window...");
            mw.stop_render();
        },
        android::APP_CMD_DESTROY =>
        {
            trace!("Destroying App...");
            mw.destroy();
            trace!("App Destroyed");
        },
        e => trace!("Unknown Event: {}", e)
    }
}

mod audio_backend;

struct Generator(Arc<RwLock<peridot::audio::Mixer>>);
impl audio_backend::aaudio::DataCallback for Generator
{
    fn callback(&mut self, stream_ptr: *mut audio_backend::aaudio::native::AAudioStream, buf: *mut libc::c_void, frames: usize) -> audio_backend::aaudio::CallbackResult
    {
        let bufslice = unsafe { std::slice::from_raw_parts_mut(buf as *mut f32, frames << 1) };
        for b in bufslice.iter_mut() { *b = 0.0; }
        self.0.write().expect("Mixer Write Failed!").process(bufslice);

        audio_backend::aaudio::CallbackResult::Continue
    }
}
struct NativeAudioEngine
{
    stream: audio_backend::aaudio::Stream,
    generator: Pin<Box<Generator>>
}
impl NativeAudioEngine
{
    pub fn new(mixer: &Arc<RwLock<peridot::audio::Mixer>>) -> Self
    {
        let mut generator = Box::pin(Generator(mixer.clone()));
        let api = audio_backend::aaudio::Api::load().expect("AAudio unsupported?");
        let mut stream = api.new_stream_builder().expect("Failed to create StreamBuilder")
            .as_output()
            // .set_low_latency_mode()
            .use_shared()
            .use_float_format()
            .set_channel_count(2)
            .set_sample_rate(44100)
            .set_data_callback(generator.as_mut())
            .open_stream()
            .expect("Failed to open playback stream");
        stream.request_start().expect("Failed to start playback stream");
        
        NativeAudioEngine
        {
            stream,
            generator
        }
    }

    pub fn pause(&mut self)
    {
        self.stream.request_pause().expect("Failed to pause stream");
    }
}
impl Drop for NativeAudioEngine
{
    fn drop(&mut self)
    {
        self.stream.request_stop();
        trace!("NativeAudioEngine end");
    }
}
