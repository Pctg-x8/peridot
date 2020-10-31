//! peridot-cradle for android platform

use log::*;

mod userlib;

use bedrock as br;
use peridot::{EngineEvents, FeatureRequests};
use std::rc::Rc;

struct Game {
    engine: peridot::Engine<NativeLink>,
    userlib: userlib::Game<NativeLink>,
    pos_cache: std::rc::Rc<std::cell::RefCell<TouchPositionCache>>
}
impl Game {
    fn new(asset_manager: AssetManager, window: *mut android::ANativeWindow) -> Self {
        let nl = NativeLink {
            al: PlatformAssetLoader::new(asset_manager),
            w: window
        };
        let mut engine = peridot::Engine::new(
            userlib::Game::<NativeLink>::NAME, userlib::Game::<NativeLink>::VERSION,
            nl, userlib::Game::<NativeLink>::requested_features()
        );
        let pos_cache = std::rc::Rc::new(std::cell::RefCell::new(TouchPositionCache::new()));
        engine.input_mut().set_nativelink(Box::new(InputNativeLink { pos_cache: pos_cache.clone() }));
        engine.postinit();

        Game {
            userlib: userlib::Game::init(&mut engine),
            engine,
            pos_cache
        }
    }

    fn update(&mut self) { self.engine.do_update(&mut self.userlib); }
}

struct Presenter {
    window: *mut android::ANativeWindow,
    sc: peridot::IntegratedSwapchain
}
impl Presenter {
    pub fn new(g: &peridot::Graphics, render_queue_family_index: u32, window: *mut android::ANativeWindow) -> Self {
        let obj = br::Surface::new_android(g.instance(), window).expect("Failed to create Surface");
        let supported = g.adapter().surface_support(render_queue_family_index, &obj)
            .expect("Failed to query surface availability");
        if !supported {
            panic!("Vulkan Surface is not supported by this adapter");
        }

        Presenter {
            window,
            sc: peridot::IntegratedSwapchain::new(
                g, obj, unsafe { peridot::math::Vector2((*window).width() as _, (*window).height() as _) }
            )
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat { self.sc.format() }
    fn backbuffer_count(&self) -> usize { self.sc.backbuffer_count() }
    fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> { self.sc.backbuffer(index) }

    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        self.sc.emit_initialize_backbuffer_commands(recorder)
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> { self.sc.acquire_next_backbuffer_index() }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_backbuffer_layout()
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &peridot::Graphics,
        last_render_fence: &br::Fence,
        present_queue: &br::Queue,
        backbuffer_index: u32,
        render_submission: br::SubmissionBatch<'s>,
        update_submission: Option<br::SubmissionBatch<'s>>
    ) -> br::Result<()> {
        self.sc.render_and_present(
            g, last_render_fence, present_queue, backbuffer_index, render_submission, update_submission
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
    al: PlatformAssetLoader, w: *mut android::ANativeWindow
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;
    fn instance_extensions(&self) -> Vec<&str> { vec!["VK_KHR_surface", "VK_KHR_android_surface"] }
    fn device_extensions(&self) -> Vec<&str> { vec!["VK_KHR_swapchain"] }

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(g, g.graphics_queue_family_index(), self.w)
    }
}

struct TouchPositionCache(Vec<(f32, f32)>);
impl TouchPositionCache {
    pub fn new() -> Self { TouchPositionCache(Vec::new()) }
    pub fn query(&self, id: usize) -> Option<&(f32, f32)> {
        self.0.get(id)
    }
    pub fn set(&mut self, id: usize, x: f32, y: f32) {
        if self.0.len() <= id { self.0.resize(id + 1, (0.0, 0.0)); }
        self.0[id] = (x, y);
    }
}

struct InputNativeLink {
    pos_cache: std::rc::Rc<std::cell::RefCell<TouchPositionCache>>
}
impl peridot::NativeInput for InputNativeLink {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)> {
        self.pos_cache.borrow().query(index as _).copied()
    }
}

// JNI Exports //

use jni::{JNIEnv, objects::{JByteBuffer, JObject, JClass}, sys::{jint, jfloat}};

#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_init<'e>(
    env: JNIEnv<'e>,
    _: JClass,
    surface: JObject,
    asset_manager: JObject
) -> JByteBuffer<'e> {
    android_logger::init_once(android_logger::Filter::default().with_min_level(log::Level::Trace));
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
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_fin(e: JNIEnv, _: JClass, obj: JByteBuffer) {
    info!("Finalizing NativeGameEngine...");
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    drop(unsafe { Box::from_raw(bytes.as_ptr() as *mut Game) });
}
#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_update(e: JNIEnv, _: JClass, obj: JByteBuffer) {
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    let e = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    e.update();
}

#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_processTouchDownEvent(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint
) {
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    let gd = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    gd.engine.input().dispatch_button_event(peridot::NativeButtonInput::Touch(id as _), true);
}
#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_processTouchUpEvent(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint
) {
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    let gd = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    gd.engine.input().dispatch_button_event(peridot::NativeButtonInput::Touch(id as _), false);
}
#[no_mangle]
pub extern "system" fn Java_jp_ct2_peridot_NativeLibLink_setTouchPositionAbsolute(
    e: JNIEnv,
    _: JClass,
    obj: JByteBuffer,
    id: jint,
    x: jfloat,
    y: jfloat
) {
    let bytes = e.get_direct_buffer_address(obj).expect("Getting Pointer from DirectByteBuffer failed");
    let gd = unsafe { (bytes.as_ptr() as *mut Game).as_mut().expect("null ptr?") };

    gd.pos_cache.borrow_mut().set(id as _, x, y);
    gd.engine.input().dispatch_analog_event(peridot::NativeAnalogInput::TouchMoveX(id as _), x, true);
    gd.engine.input().dispatch_analog_event(peridot::NativeAnalogInput::TouchMoveY(id as _), y, true);
}
