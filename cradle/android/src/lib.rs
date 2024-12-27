//! peridot-cradle for android platform

use async_std::sync::Mutex;
use br::PhysicalDevice;
use log::*;

use android::{Asset, AssetManager, AASSET_MODE_RANDOM, AASSET_MODE_STREAMING};
use std::ffi::CString;
use std::io::{Error as IOError, ErrorKind, Result as IOResult};

mod userlib;

use bedrock as br;
use parking_lot::RwLock;
use peridot::mthelper::{DynamicMut, DynamicMutabilityProvider, SharedRef};
use peridot::{EngineEvents, FeatureRequests};
use std::ffi::CStr;
use std::pin::Pin;
use std::sync::Arc;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

struct Game {
    engine_input: peridot::InputProcess,
    snd: NativeAudioEngine,
    stopping_render: bool,
    pos_cache: SharedRef<DynamicMut<TouchPositionCache>>,
    event_sender: async_std::channel::Sender<peridot::EngineEvent>,
    frame_timing_sender: async_std::channel::Sender<()>,
    usercode_thread: async_std::task::JoinHandle<()>,
}
impl Game {
    fn new(asset_manager: AssetManager, window: *mut android::ANativeWindow) -> Self {
        let (event_sender, event_receiver) = async_std::channel::unbounded();
        let (frame_timing_sender, frame_timing_receiver) = async_std::channel::bounded(1);

        let nl = NativeLink {
            al: PlatformAssetLoader::new(asset_manager),
            w: window,
        };
        let mut engine = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            Default::default(),
            (event_sender.clone(), event_receiver),
            frame_timing_receiver,
        );
        let snd = NativeAudioEngine::new(engine.audio_mixer());
        let pos_cache = SharedRef::new(DynamicMut::new(TouchPositionCache::new()));
        engine.input_mut().set_nativelink(Box::new(InputNativeLink {
            pos_cache: pos_cache.clone(),
        }));
        engine.post_init();

        let engine_input = engine.input().clone();
        let usercode_thread = async_std::task::spawn(async move {
            userlib::game_main(&mut engine).await;
        });

        Self {
            engine_input,
            snd,
            stopping_render: false,
            pos_cache,
            event_sender,
            frame_timing_sender,
            usercode_thread,
        }
    }
}

struct Presenter {
    window: *mut android::ANativeWindow,
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<peridot::InstanceObject>>,
}
unsafe impl Sync for Presenter {}
unsafe impl Send for Presenter {}
impl Presenter {
    pub fn new(
        g: &peridot::Graphics,
        render_queue_family_index: u32,
        window: *mut android::ANativeWindow,
    ) -> Self {
        let obj = unsafe {
            br::SurfaceObject::new(
                g.adapter(),
                &br::vk::VkAndroidSurfaceCreateInfoKHR::new(window),
            )
            .expect("Failed to create Surface")
        };
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
    type BackBuffer = br::ImageViewObject<
        br::SwapchainImage<
            SharedRef<
                br::SurfaceSwapchainObject<
                    peridot::DeviceObject,
                    br::SurfaceObject<peridot::InstanceObject>,
                >,
            >,
        >,
    >;

    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }
    fn back_buffer_count(&self) -> usize {
        self.sc.back_buffer_count()
    }
    fn back_buffer(&self, index: usize) -> Option<&SharedRef<Self::BackBuffer>> {
        self.sc.back_buffer(index)
    }

    fn emit_initialize_back_buffer_commands<
        'r,
        CB: br::CommandBuffer + br::VkHandleMut + ?Sized,
    >(
        &self,
        recorder: br::CmdRecord<'r, CB, peridot::DeviceObject>,
    ) -> br::CmdRecord<'r, CB, peridot::DeviceObject> {
        self.sc.emit_initialize_back_buffer_commands(recorder)
    }
    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_back_buffer_index()
    }
    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::FenceMut,
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
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }

    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        unsafe { peridot::math::Vector2((*self.window).width() as _, (*self.window).height() as _) }
    }
}

use android::{Asset, AssetManager, AASSET_MODE_RANDOM, AASSET_MODE_STREAMING};
use std::ffi::CString;
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
struct PlatformAssetLoader {
    amgr: AssetManager,
}
unsafe impl Sync for PlatformAssetLoader {}
unsafe impl Send for PlatformAssetLoader {}
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
unsafe impl Sync for NativeLink {}
unsafe impl Send for NativeLink {}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;
    fn instance_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_surface", c"VK_KHR_android_surface"]
    }
    fn device_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_swapchain"]
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
    // android_logger::init_once(android_logger::Filter::default().with_min_level(log::Level::Trace));

    std::panic::set_hook(Box::new(|p| {
        log::error!("Panicking in app! {}", p);
    }));

    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().pretty())
        .with(tracing_android::layer("Peridot").expect("Failed to create android tracing layer"))
        .init();

    tracing::info!("Initializing NativeGameEngine...");

    std::panic::set_hook(Box::new(|p| {
        tracing::error!("Panicking in app! {}", p);
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
    let e = unsafe { Box::from_raw(bytes.as_ptr() as *mut Game) };

    async_std::task::block_on(async move {
        if e.event_sender
            .send(peridot::EngineEvent::Shutdown)
            .await
            .is_ok()
        {
            e.usercode_thread.await;
        }
    });
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

    match e.frame_timing_sender.try_send(()) {
        Ok(_) => (),
        Err(async_std::channel::TrySendError::Full(_)) => (),
        Err(async_std::channel::TrySendError::Closed(_)) => {
            tracing::warn!("Frame Timing channel was closed!");
        }
    }
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
        self.0.write().process(bufslice);

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
        generator.0.write().start();

        NativeAudioEngine { stream, generator }
    }

    pub fn pause(&mut self) {
        self.generator.0.write().stop();
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
        self.generator.0.write().stop();
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

    gd.engine_input
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

    gd.engine_input
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
    gd.engine_input
        .dispatch_analog_event(peridot::NativeAnalogInput::TouchMoveX(id as _), x, true);
    gd.engine_input
        .dispatch_analog_event(peridot::NativeAnalogInput::TouchMoveY(id as _), y, true);
}
