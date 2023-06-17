//! peridot-cradle for android platform

use br::PhysicalDevice;
use log::*;

mod userlib;

use bedrock as br;
use peridot::mthelper::{DynamicMut, DynamicMutabilityProvider, SharedRef};
use peridot::{EngineEvents, FeatureRequests};
use std::pin::Pin;
use std::sync::{Arc, RwLock};

struct Game {
    engine: peridot::Engine<NativeLink>,
    userlib: userlib::Game<NativeLink>,
    snd: NativeAudioEngine,
    stopping_render: bool,
    pos_cache: SharedRef<DynamicMut<TouchPositionCache>>,
}
impl Game {
    fn new(asset_manager: AssetManager, window: *mut android::ANativeWindow) -> Self {
        let nl = NativeLink {
            al: PlatformAssetLoader::new(asset_manager),
            w: window,
        };
        let mut engine = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            userlib::Game::<NativeLink>::requested_features(),
        );
        let snd = NativeAudioEngine::new(engine.audio_mixer());
        let pos_cache = SharedRef::new(DynamicMut::new(TouchPositionCache::new()));
        engine.input_mut().set_nativelink(Box::new(InputNativeLink {
            pos_cache: pos_cache.clone(),
        }));
        engine.postinit();

        Self {
            userlib: userlib::Game::init(&mut engine),
            engine,
            snd,
            stopping_render: false,
            pos_cache,
        }
    }

    fn update(&mut self) {
        self.engine.do_update(&mut self.userlib);
    }
}

struct Presenter {
    window: *mut android::ANativeWindow,
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<peridot::InstanceObject>>,
}
impl Presenter {
    pub fn new(
        g: &peridot::Graphics,
        render_queue_family_index: u32,
        window: *mut android::ANativeWindow,
    ) -> Self {
        let obj = g
            .adapter()
            .new_surface_android(window)
            .expect("Failed to create Surface");
        let supported = g
            .adapter()
            .surface_support(render_queue_family_index, &obj)
            .expect("Failed to query surface availability");
        if !supported {
            panic!("Vulkan Surface is not supported by this adapter");
        }

        Self {
            window,
            sc: peridot::IntegratedSwapchain::new(g, obj, unsafe {
                peridot::math::Vector2((*window).width() as _, (*window).height() as _)
            }),
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    type Backbuffer = br::ImageViewObject<
        br::SwapchainImage<
            SharedRef<
                br::SwapchainObject<
                    peridot::DeviceObject,
                    br::SurfaceObject<peridot::InstanceObject>,
                >,
            >,
        >,
    >;

    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }
    fn backbuffer_count(&self) -> usize {
        self.sc.backbuffer_count()
    }
    fn backbuffer(&self, index: usize) -> Option<SharedRef<Self::Backbuffer>> {
        self.sc.backbuffer(index)
    }

    fn emit_initialize_backbuffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut + ?Sized>,
    ) {
        self.sc.emit_initialize_backbuffer_commands(recorder)
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_backbuffer_index()
    }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_backbuffer_layout()
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut (impl br::Fence + br::VkHandleMut),
        backbuffer_index: u32,
        render_submission: impl br::SubmissionBatch,
        update_submission: Option<impl br::SubmissionBatch>,
    ) -> br::Result<()> {
        self.sc.render_and_present(
            g,
            last_render_fence,
            backbuffer_index,
            render_submission,
            update_submission,
        )
    }
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }

    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> {
        unsafe { peridot::math::Vector2((*self.window).width() as _, (*self.window).height() as _) }
    }
}

use android::{Asset, AssetManager, AASSET_MODE_RANDOM, AASSET_MODE_STREAMING};
use std::ffi::CString;
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
struct PlatformAssetLoader {
    amgr: AssetManager,
}
impl PlatformAssetLoader {
    fn new(amgr: AssetManager) -> Self {
        PlatformAssetLoader { amgr }
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type Asset = Asset;
    type StreamingAsset = Asset;

    fn get(&self, path: &str, ext: &str) -> IOResult<Asset> {
        let mut path_str = path.replace(".", "/");
        path_str.push('.');
        path_str.push_str(ext);
        let path_str = CString::new(path_str).expect("converting path");
        self.amgr
            .open(path_str.as_ptr(), AASSET_MODE_RANDOM)
            .ok_or(IOError::new(ErrorKind::NotFound, ""))
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Asset> {
        let mut path_str = path.replace(".", "/");
        path_str.push('.');
        path_str.push_str(ext);
        let path_str = CString::new(path_str).expect("converting path");
        self.amgr
            .open(path_str.as_ptr(), AASSET_MODE_STREAMING)
            .ok_or(IOError::new(ErrorKind::NotFound, ""))
    }
}

struct NativeLink {
    al: PlatformAssetLoader,
    w: *mut android::ANativeWindow,
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;
    fn instance_extensions(&self) -> Vec<&str> {
        vec!["VK_KHR_surface", "VK_KHR_android_surface"]
    }
    fn device_extensions(&self) -> Vec<&str> {
        vec!["VK_KHR_swapchain"]
    }

    fn asset_loader(&self) -> &PlatformAssetLoader {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(g, g.graphics_queue_family_index(), self.w)
    }
}

struct TouchPositionCache(Vec<(f32, f32)>);
impl TouchPositionCache {
    pub fn new() -> Self {
        TouchPositionCache(Vec::new())
    }
    pub fn query(&self, id: usize) -> Option<&(f32, f32)> {
        self.0.get(id)
    }
    pub fn set(&mut self, id: usize, x: f32, y: f32) {
        if self.0.len() <= id {
            self.0.resize(id + 1, (0.0, 0.0));
        }
        self.0[id] = (x, y);
    }
}

struct InputNativeLink {
    pos_cache: SharedRef<DynamicMut<TouchPositionCache>>,
}
impl peridot::NativeInput for InputNativeLink {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)> {
        self.pos_cache.borrow().query(index as _).copied()
    }
}

// JNI Exports //

use jni::{
    objects::{JByteBuffer, JClass, JObject},
    sys::{jfloat, jint},
    JNIEnv,
};

#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_init<'e>(
    env: JNIEnv<'e>,
    _: JClass,
    surface: JObject,
    asset_manager: JObject,
) -> JByteBuffer<'e> {
    android_logger::init_once(android_logger::Filter::default().with_min_level(log::Level::Trace));
    info!("Initializing NativeGameEngine...");

    std::panic::set_hook(Box::new(|p| {
        error!("Panicking in app! {}", p);
    }));

    let window = unsafe { android::ANativeWindow_fromSurface(env.clone(), surface) };
    let am =
        unsafe { AssetManager::from_java(env.clone(), asset_manager).expect("null assetmanager") };
    let e = Game::new(am, window);

    let ptr = Box::into_raw(Box::new(e));
    env.new_direct_byte_buffer(unsafe { std::slice::from_raw_parts_mut(ptr as *mut u8, 0) })
        .expect("Creating DirectByteBuffer failed")
}
#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_fin(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
) {
    info!("Finalizing NativeGameEngine...");
    let bytes = e
        .get_direct_buffer_address(obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    drop(unsafe { Box::from_raw(bytes.as_ptr() as *mut Game) });
}
#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_update(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
) {
    let bytes = e
        .get_direct_buffer_address(obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let e = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    e.update();
}

mod audio_backend;

struct Generator(Arc<RwLock<peridot::audio::Mixer>>);
impl audio_backend::aaudio::DataCallback for Generator {
    fn callback(
        &mut self,
        stream_ptr: *mut audio_backend::aaudio::native::AAudioStream,
        buf: *mut libc::c_void,
        frames: usize,
    ) -> audio_backend::aaudio::CallbackResult {
        let bufslice = unsafe { std::slice::from_raw_parts_mut(buf as *mut f32, frames << 1) };
        for b in bufslice.iter_mut() {
            *b = 0.0;
        }
        self.0
            .write()
            .expect("Mixer Write Failed!")
            .process(bufslice);

        audio_backend::aaudio::CallbackResult::Continue
    }
}
struct NativeAudioEngine {
    stream: audio_backend::aaudio::Stream,
    generator: Pin<Box<Generator>>,
}
impl NativeAudioEngine {
    pub fn new(mixer: &Arc<RwLock<peridot::audio::Mixer>>) -> Self {
        let mut generator = Box::pin(Generator(mixer.clone()));
        let api = audio_backend::aaudio::Api::load().expect("AAudio unsupported?");
        let mut stream = api
            .new_stream_builder()
            .expect("Failed to create StreamBuilder")
            .as_output()
            // .set_low_latency_mode()
            .use_shared()
            .use_float_format()
            .set_channel_count(2)
            .set_sample_rate(44100)
            .set_data_callback(generator.as_mut())
            .open_stream()
            .expect("Failed to open playback stream");
        stream
            .request_start()
            .expect("Failed to start playback stream");
        generator.0.write().expect("AudioEngine Poisoned").start();

        NativeAudioEngine { stream, generator }
    }

    pub fn pause(&mut self) {
        self.generator
            .0
            .write()
            .expect("AudioEngine Poisoning")
            .stop();
        self.stream.request_pause().expect("Failed to pause stream");
        let mut st = self.stream.state();
        while st != audio_backend::aaudio::native::AAUDIO_STREAM_STATE_PAUSED {
            self.stream
                .wait_for_state_change(st, &mut st, None)
                .expect("Waiting StreamStateChange failed");
        }
        self.stream.request_flush().expect("Failed to pause stream");
    }
}
impl Drop for NativeAudioEngine {
    fn drop(&mut self) {
        self.generator
            .0
            .write()
            .expect("AudioEngine Poisoning")
            .stop();
        self.stream.request_stop();
        trace!("NativeAudioEngine end");
    }
}

#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_processTouchDownEvent(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint,
) {
    let bytes = e
        .get_direct_buffer_address(obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let gd = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    gd.engine
        .input_mut()
        .dispatch_button_event(peridot::NativeButtonInput::Touch(id as _), true);
}
#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_processTouchUpEvent(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint,
) {
    let bytes = e
        .get_direct_buffer_address(obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let gd = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    gd.engine
        .input_mut()
        .dispatch_button_event(peridot::NativeButtonInput::Touch(id as _), false);
}
#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_setTouchPositionAbsolute(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint,
    x: jfloat,
    y: jfloat,
) {
    let bytes = e
        .get_direct_buffer_address(obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let gd = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    gd.pos_cache.borrow_mut().set(id as _, x, y);
    gd.engine.input_mut().dispatch_analog_event(
        peridot::NativeAnalogInput::TouchMoveX(id as _),
        x,
        true,
    );
    gd.engine.input_mut().dispatch_analog_event(
        peridot::NativeAnalogInput::TouchMoveY(id as _),
        y,
        true,
    );
}
