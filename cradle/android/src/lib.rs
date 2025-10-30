//! peridot-cradle for android platform

use br::PhysicalDevice;
use log::*;

mod native_wrapper;
#[allow(dead_code)]
mod userlib;

use android::{AASSET_MODE_RANDOM, AASSET_MODE_STREAMING};
use bedrock::{self as br, InstanceChild, SurfaceCreateInfo, VkHandle};
use parking_lot::RwLock;
use peridot::mthelper::{DynamicMut, DynamicMutabilityProvider, SharedRef};
use std::ffi::CStr;
use std::pin::Pin;
use std::sync::Arc;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

fn init_logger() {
    let android_layer = match tracing_android::layer("Peridot") {
        Ok(x) => Some(x),
        Err(_) => {
            unsafe {
                android::__android_log_print(
                    android::ANDROID_LOG_WARN,
                    c"peridot::tracing".as_ptr(),
                    c"Could not create android tracing layer".as_ptr(),
                );
            }
            None
        }
    };

    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().pretty())
        .with(android_layer)
        .init();
    std::panic::set_hook(Box::new(|p| {
        tracing::error!("{p}");
    }));
}

static USERCODE_WAKER_VTABLE: core::task::RawWakerVTable = core::task::RawWakerVTable::new(
    |data| core::task::RawWaker::new(data, &USERCODE_WAKER_VTABLE),
    |data| {},
    |data| {},
    |data| {},
);

fn launch<F: core::future::Future>(
    asset_manager: native_wrapper::AssetManager,
    window: native_wrapper::Window,
    usercode_launcher: impl FnOnce(peridot::Engine<'static, NativeLink>) -> F,
) -> NativeCallData {
    let bgio_worker = peridot::native_io::android::BackgroundIoWorkerPool::spawn();

    let (event_sender, event_receiver) = async_std::channel::unbounded();
    let (frame_timing_sender, frame_timing_receiver) = async_std::channel::bounded(1);

    let event_queue = Box::pin(peridot::EventQueue::new());
    let event_queue_lifetime_extended: &'static peridot::EventQueue =
        unsafe { &*(&*event_queue as *const _) };
    let nl = NativeLink {
        al: PlatformAssetLoader::new(asset_manager),
        w: window,
    };
    let mut engine = peridot::Engine::new(
        userlib::APP_IDENTIFIER,
        userlib::APP_VERSION,
        nl,
        unsafe { core::mem::MaybeUninit::zeroed().assume_init() },
        (event_sender.clone(), event_receiver),
        frame_timing_receiver,
        event_queue_lifetime_extended,
    );
    let snd = NativeAudioEngine::new(engine.audio_mixer());
    let pos_cache = SharedRef::new(DynamicMut::new(TouchPositionCache::new()));
    engine.input_mut().set_nativelink(Box::new(InputNativeLink {
        pos_cache: pos_cache.clone(),
    }));
    engine.post_init();

    let engine_input = engine.input().clone();
    let usercode_thread = Box::pin(usercode_launcher(engine));

    let driver = Box::new(Game {
        engine_input,
        snd,
        stopping_render: false,
        pos_cache,
        event_sender,
        frame_timing_sender,
        event_queue,
        usercode_thread,
        bgio_worker,
        _pinned: core::marker::PhantomPinned,
    });

    extern "C" fn fin<F: core::future::Future>(inst_ptr: *mut core::ffi::c_void) {
        let mut inst = unsafe { Box::from_raw(inst_ptr as *mut Game<F>) };
        inst.event_queue.enqueue(peridot::Event::Shutdown);
        while !inst.step() {}
    }
    extern "C" fn update<F: core::future::Future>(inst_ptr: *mut core::ffi::c_void) {
        let inst = unsafe { &mut *(inst_ptr as *mut Game<F>) };
        inst.event_queue.enqueue(peridot::Event::NextFrame);
        inst.step();
    }
    extern "C" fn process_touch_down_event<F: core::future::Future>(
        inst_ptr: *mut core::ffi::c_void,
        id: u32,
    ) {
        let inst = unsafe { &mut *(inst_ptr as *mut Game<F>) };
        inst.engine_input
            .dispatch_button_event(peridot::NativeButtonInput::Touch(id), true);
    }
    extern "C" fn process_touch_up_event<F: core::future::Future>(
        inst_ptr: *mut core::ffi::c_void,
        id: u32,
    ) {
        let inst = unsafe { &mut *(inst_ptr as *mut Game<F>) };
        inst.engine_input
            .dispatch_button_event(peridot::NativeButtonInput::Touch(id), false);
    }
    extern "C" fn set_touch_position_absolute<F: core::future::Future>(
        inst_ptr: *mut core::ffi::c_void,
        id: u32,
        x: f32,
        y: f32,
    ) {
        let inst = unsafe { &mut *(inst_ptr as *mut Game<F>) };

        inst.pos_cache.borrow_mut().set(id as _, x, y);
        inst.engine_input.dispatch_analog_event(
            peridot::NativeAnalogInput::TouchMoveX(id),
            x,
            true,
        );
        inst.engine_input.dispatch_analog_event(
            peridot::NativeAnalogInput::TouchMoveY(id),
            y,
            true,
        );
    }

    NativeCallData {
        inst_ptr: Box::into_raw(driver) as _,
        finalize: fin::<F>,
        update: update::<F>,
        process_touch_down_event: process_touch_down_event::<F>,
        process_touch_up_event: process_touch_up_event::<F>,
        set_touch_position_absolute: set_touch_position_absolute::<F>,
    }
}

struct Game<F> {
    engine_input: peridot::InputProcess,
    snd: NativeAudioEngine,
    stopping_render: bool,
    pos_cache: SharedRef<DynamicMut<TouchPositionCache>>,
    event_sender: async_std::channel::Sender<peridot::EngineEvent>,
    frame_timing_sender: async_std::channel::Sender<()>,
    event_queue: Pin<Box<peridot::EventQueue>>,
    usercode_thread: Pin<Box<F>>,
    bgio_worker: peridot::native_io::android::BackgroundIoWorkerPool,
    // self-referential struct
    _pinned: core::marker::PhantomPinned,
}
impl<F: core::future::Future> Game<F> {
    fn step(&mut self) -> bool {
        let waker = unsafe {
            core::task::Waker::from_raw(core::task::RawWaker::new(
                core::ptr::null(),
                &USERCODE_WAKER_VTABLE,
            ))
        };

        self.usercode_thread
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&waker))
            .is_ready()
    }
}

struct Surface {
    gfx_device: peridot::VulkanGfx,
    handle: br::vk::VkSurfaceKHR,
}
impl Drop for Surface {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_surface(
                self.gfx_device.instance().native_ptr(),
                self.handle,
                None,
            );
        }
    }
}
impl br::VkHandle for Surface {
    type Handle = br::vk::VkSurfaceKHR;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

struct Presenter {
    window: native_wrapper::Window,
    sc: peridot::IntegratedSwapchain<Surface>,
}
unsafe impl Sync for Presenter {}
unsafe impl Send for Presenter {}
impl Presenter {
    pub fn new(
        g: &peridot::Graphics,
        render_queue_family_index: u32,
        window: native_wrapper::Window,
    ) -> Self {
        let obj = Surface {
            handle: unsafe {
                br::AndroidSurfaceCreateInfo::new(window.as_ptr())
                    .execute(g.device().instance(), None)
                    .expect("Failed to create surface")
            },
            gfx_device: g.device().clone(),
        };
        let supported = g
            .device()
            .surface_support(&obj)
            .expect("Failed to query surface availability");
        if !supported {
            panic!("Vulkan Surface is not supported by this adapter");
        }

        Self {
            sc: peridot::IntegratedSwapchain::new(
                g,
                obj,
                peridot::math::Vector2(window.width() as _, window.height() as _),
            ),
            window,
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    #[inline(always)]
    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }

    #[inline(always)]
    fn back_buffer_count(&self) -> usize {
        self.sc.back_buffer_count()
    }

    #[inline(always)]
    fn back_buffer_size(&self) -> peridot::math::Vector2<u32> {
        self.sc.back_buffer_size()
    }

    #[inline(always)]
    fn back_buffer(&self, index: usize) -> Option<br::VkHandleRef<br::vk::VkImage>> {
        self.sc.back_buffer(index)
    }

    #[inline(always)]
    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: br::CmdRecord<'r>,
    ) -> br::CmdRecord<'r> {
        self.sc.emit_initialize_back_buffer_commands(recorder)
    }

    #[inline(always)]
    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_back_buffer_index()
    }

    #[inline(always)]
    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
    }

    #[inline(always)]
    fn render_and_present<'s, 'r>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        backbuffer_index: u32,
        render_submission: peridot::SubmissionBatchBuilder<'r>,
        update_submission: Option<peridot::SubmissionBatchBuilder<'r>>,
    ) -> br::Result<()>
    where
        's: 'r,
    {
        self.sc.render_and_present(
            g,
            last_render_fence,
            backbuffer_index,
            render_submission,
            update_submission,
        )
    }

    #[inline(always)]
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }

    #[inline(always)]
    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        peridot::math::Vector2(self.window.width() as _, self.window.height() as _)
    }
}

use std::ffi::CString;
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
struct PlatformAssetLoader {
    amgr: RwLock<native_wrapper::AssetManager>,
}
unsafe impl Sync for PlatformAssetLoader {}
unsafe impl Send for PlatformAssetLoader {}
impl PlatformAssetLoader {
    fn new(amgr: native_wrapper::AssetManager) -> Self {
        PlatformAssetLoader {
            amgr: RwLock::new(amgr),
        }
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type Asset<'a> = peridot::native_io::android::BundledAssetRandomReader;
    type AssetBlobAsync<'a> = peridot::native_io::android::BundledAssetAsyncRandomReader;
    type StreamingAsset<'a> = native_wrapper::Asset;

    fn get<'a>(&'a self, path: &str, ext: &str) -> IOResult<Self::Asset<'a>> {
        let mut path_str = path.replace(".", "/");
        path_str.push('.');
        path_str.push_str(ext);
        let path_str = CString::new(path_str).expect("converting path");
        Ok(
            peridot::native_io::android::BundledAssetRandomReader::from_asset_ptr(
                self.amgr
                    .write()
                    .open(&path_str, AASSET_MODE_RANDOM)
                    .ok_or(IOError::new(ErrorKind::NotFound, ""))?
                    .leak(),
            ),
        )
    }

    fn get_async<'a>(
        &'a self,
        path: &str,
        ext: &str,
    ) -> impl core::future::Future<Output = IOResult<Self::AssetBlobAsync<'a>>> {
        async move {
            let mut path_str = path.replace(".", "/");
            path_str.push('.');
            path_str.push_str(ext);
            let path_str = CString::new(path_str).expect("converting path");
            Ok(
                peridot::native_io::android::BundledAssetAsyncRandomReader::from_asset_ptr(
                    self.amgr
                        .write()
                        .open(&path_str, AASSET_MODE_RANDOM)
                        .ok_or(IOError::new(ErrorKind::NotFound, ""))?
                        .leak(),
                ),
            )
        }
    }

    fn get_streaming<'a>(&'a self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset<'a>> {
        let mut path_str = path.replace(".", "/");
        path_str.push('.');
        path_str.push_str(ext);
        let path_str = CString::new(path_str).expect("converting path");
        self.amgr
            .write()
            .open(&path_str, AASSET_MODE_STREAMING)
            .ok_or(IOError::new(ErrorKind::NotFound, ""))
    }
}

struct NativeLink {
    al: PlatformAssetLoader,
    w: native_wrapper::Window,
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
        Presenter::new(g, g.graphics_queue_family_index(), self.w.clone())
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

struct NativeCallData {
    inst_ptr: *mut core::ffi::c_void,
    finalize: extern "C" fn(inst_ptr: *mut core::ffi::c_void),
    update: extern "C" fn(inst_ptr: *mut core::ffi::c_void),
    process_touch_down_event: extern "C" fn(inst_ptr: *mut core::ffi::c_void, id: u32),
    process_touch_up_event: extern "C" fn(inst_ptr: *mut core::ffi::c_void, id: u32),
    set_touch_position_absolute:
        extern "C" fn(inst_ptr: *mut core::ffi::c_void, id: u32, x: f32, y: f32),
}

#[no_mangle]
pub extern "system" fn Java_io_ct2_peridot_NativeLibLink_init<'e>(
    mut env: JNIEnv<'e>,
    _: JClass,
    surface: JObject,
    asset_manager: JObject,
) -> JByteBuffer<'e> {
    init_logger();
    tracing::info!("Initializing NativeGameEngine...");

    let window = native_wrapper::Window::from_surface(&env, &surface)
        .expect("No native window associated to the surface");
    let am = native_wrapper::AssetManager::from_java(&env, &asset_manager)
        .expect("Failed to get AndroidAssetManager native object");
    let ncd = launch(am, window, |mut e| async move {
        userlib::game_main(&mut e).await;
    });

    let ptr = Box::into_raw(Box::new(ncd));
    unsafe {
        env.new_direct_byte_buffer(ptr as *mut u8, core::mem::size_of::<NativeCallData>())
            .expect("Creating DirectByteBuffer failed")
    }
}
#[no_mangle]
pub extern "system" fn Java_io_ct2_peridot_NativeLibLink_fin(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
) {
    info!("Finalizing NativeGameEngine...");
    let bytes = e
        .get_direct_buffer_address(&obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let ncd = unsafe { Box::from_raw(bytes as *mut NativeCallData) };

    (ncd.finalize)(ncd.inst_ptr);
}
#[no_mangle]
pub extern "system" fn Java_io_ct2_peridot_NativeLibLink_update(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
) {
    let bytes = e
        .get_direct_buffer_address(&obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let ncd = unsafe { (bytes as *mut NativeCallData).as_mut().expect("null ptr?") };

    (ncd.update)(ncd.inst_ptr);
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
pub extern "system" fn Java_io_ct2_peridot_NativeLibLink_processTouchDownEvent(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint,
) {
    let bytes = e
        .get_direct_buffer_address(&obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let ncd = unsafe { (bytes as *mut NativeCallData).as_mut().expect("null ptr?") };

    (ncd.process_touch_down_event)(ncd.inst_ptr, id as _);
}
#[no_mangle]
pub extern "system" fn Java_io_ct2_peridot_NativeLibLink_processTouchUpEvent(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint,
) {
    let bytes = e
        .get_direct_buffer_address(&obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let ncd = unsafe { (bytes as *mut NativeCallData).as_mut().expect("null ptr?") };

    (ncd.process_touch_up_event)(ncd.inst_ptr, id as _);
}
#[no_mangle]
pub extern "system" fn Java_io_ct2_peridot_NativeLibLink_setTouchPositionAbsolute(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint,
    x: jfloat,
    y: jfloat,
) {
    let bytes = e
        .get_direct_buffer_address(&obj)
        .expect("Getting Pointer from DirectByteBuffer failed");
    let ncd = unsafe { (bytes as *mut NativeCallData).as_mut().expect("null ptr?") };

    (ncd.set_touch_position_absolute)(ncd.inst_ptr, id as _, x, y);
}
