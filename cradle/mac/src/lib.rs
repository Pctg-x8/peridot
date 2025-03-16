use appkit::{CocoaObject, NSString};
use audio::NativeAudioEngine;
use libc::c_void;
use log::*;
use objc::{msg_send, sel, sel_impl};

use bedrock as br;
use br::PhysicalDevice;
use core::future::Future;
use peridot::mthelper::SharedRef;
use std::ffi::CStr;
use std::io::Cursor;
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
use std::pin::Pin;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use tracing_subscriber::{Layer, Registry};

mod audio;

struct NativeLogStream;
impl std::io::Write for &'_ NativeLogStream {
    fn write(&mut self, buf: &[u8]) -> IOResult<usize> {
        unsafe {
            let mut fmt =
                NSString::from_str(core::str::from_utf8_unchecked(buf)).expect("NSString");
            NSLog(&mut *fmt);
            Ok(buf.len())
        }
    }

    fn flush(&mut self) -> IOResult<()> {
        std::io::stderr().flush()
    }
}
impl<'a> tracing_subscriber::fmt::MakeWriter<'a> for NativeLogStream {
    type Writer = &'a Self;

    fn make_writer(&'a self) -> Self::Writer {
        self
    }
}

struct NSLogger;
impl log::Log for NSLogger {
    fn log(&self, record: &log::Record) {
        if self.enabled(record.metadata()) {
            unsafe {
                let mut fmt =
                    NSString::from_str(&format!("[{}] {}", record.level(), record.args()))
                        .expect("NSString");
                NSLog(&mut *fmt);
            }
        }
    }
    fn enabled(&self, metadata: &log::Metadata) -> bool {
        metadata.level() <= log::Level::Info
    }
    fn flush(&self) {}
}
static LOGGER: NSLogger = NSLogger;
unsafe extern "C" {
    #[allow(improper_ctypes)]
    unsafe fn NSLog(format: *mut NSString, ...);
}

use std::io::prelude::{Read, Seek};
use std::io::SeekFrom;
/// View of a Readable Element
pub struct ReaderView<R: Read + Seek> {
    inner: R,
    offset: u64,
    length: u64,
}
impl<R: Read + Seek> ReaderView<R> {
    pub fn new(mut reader: R, offset: u64, length: u64) -> IOResult<Self> {
        reader.seek(SeekFrom::Start(offset))?;
        return Ok(ReaderView {
            inner: reader,
            offset,
            length,
        });
    }
    fn current(&mut self) -> IOResult<u64> {
        self.inner
            .seek(SeekFrom::Current(0))
            .map(|x| x - self.offset)
    }
    fn left(&mut self) -> IOResult<u64> {
        self.current().map(|c| self.length - c)
    }
}
impl<R: Read + Seek> Read for ReaderView<R> {
    fn read(&mut self, mut buf: &mut [u8]) -> IOResult<usize> {
        let left = self.left()?;
        if buf.len() as u64 > left {
            buf = &mut buf[..left as usize];
        }
        return self.inner.read(buf);
    }
}
impl<R: Read + Seek> Seek for ReaderView<R> {
    fn seek(&mut self, pos: SeekFrom) -> IOResult<u64> {
        let pos_translated = match pos {
            SeekFrom::End(x) => SeekFrom::Start(((self.offset + self.length) as i64 - x) as _),
            SeekFrom::Start(x) => SeekFrom::Start(self.offset + x.min(self.length)),
            SeekFrom::Current(x) => SeekFrom::Current(x.min(self.left()? as i64)),
        };
        return self.inner.seek(pos_translated);
    }
}
pub struct PlatformAssetLoader {
    par_path: CocoaObject<NSString>,
}
impl PlatformAssetLoader {
    fn new() -> Self {
        let mut pathbase = NSString::from_str("assets").expect("NSString for pathbase");
        let mut pathext = NSString::from_str("par").expect("NSString for ext");
        let par_path: CocoaObject<NSString> = unsafe {
            CocoaObject::from_retained_id(nsbundle_path_for_resource(&mut *pathbase, &mut *pathext))
                .expect("No Primary Asset")
        };
        println!("par_path: {}", par_path.to_str());

        PlatformAssetLoader { par_path }
    }
}
use peridot::archive as par;
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type Asset = Cursor<Vec<u8>>;
    type StreamingAsset = ReaderView<par::EitherArchiveReader>;

    fn get(&self, path: &str, ext: &str) -> IOResult<Cursor<Vec<u8>>> {
        let mut arc = peridot::archive::ArchiveRead::from_file(self.par_path.to_str(), false)
            .map_err(|e| match e {
                peridot::archive::ArchiveReadError::IO(e) => e,
                peridot::archive::ArchiveReadError::IntegrityCheckFailed => {
                    error!("PrimaryArchive integrity check failed!");
                    IOError::new(ErrorKind::Other, "PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::SignatureMismatch => {
                    error!("PrimaryArchive signature mismatch!");
                    IOError::new(ErrorKind::Other, "PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::Lz4DecompressError(e) => {
                    error!("lz4 decompress error: {:?}", e);
                    IOError::new(ErrorKind::Other, "PrimaryArchive read error")
                }
                _ => IOError::new(ErrorKind::Other, "PrimaryArchive read error"),
            })?;
        let b = arc.read_bin(&format!("{}.{}", path.replace(".", "/"), ext))?;
        match b {
            None => Err(IOError::new(
                ErrorKind::NotFound,
                "not in primary asset package",
            )),
            Some(b) => Ok(Cursor::new(b)),
        }
    }
    fn get_streaming(
        &self,
        path: &str,
        ext: &str,
    ) -> IOResult<ReaderView<par::EitherArchiveReader>> {
        let arc = peridot::archive::ArchiveRead::from_file(self.par_path.to_str(), false).map_err(
            |e| match e {
                peridot::archive::ArchiveReadError::IO(e) => e,
                peridot::archive::ArchiveReadError::IntegrityCheckFailed => {
                    error!("PrimaryArchive integrity check failed!");
                    IOError::new(ErrorKind::Other, "PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::SignatureMismatch => {
                    error!("PrimaryArchive signature mismatch!");
                    IOError::new(ErrorKind::Other, "PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::Lz4DecompressError(e) => {
                    error!("lz4 decompress error: {:?}", e);
                    IOError::new(ErrorKind::Other, "PrimaryArchive read error")
                }
                _ => IOError::new(ErrorKind::Other, "PrimaryArchive read error"),
            },
        )?;
        let e = arc.find(&format!("{}.{}", path.replace(".", "/"), ext));
        match e {
            None => Err(IOError::new(
                ErrorKind::NotFound,
                "not in primary asset package",
            )),
            Some(b) => ReaderView::new(arc.into_inner_reader(), b.byte_offset, b.byte_length),
        }
    }
}
fn acquire_layer_size(layer: *mut c_void) -> peridot::math::Vector2<u32> {
    let cr: appkit::CGRect =
        unsafe { msg_send![layer as *mut objc::runtime::Object, contentsRect] };

    peridot::math::Vector2(cr.size.width as _, cr.size.height as _)
}
pub struct Presenter {
    layer_ptr: *mut c_void,
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<SharedRef<br::InstanceObject>>>,
}
unsafe impl Sync for Presenter {}
unsafe impl Send for Presenter {}
impl Presenter {
    fn new(layer_ptr: *mut c_void, g: &peridot::Graphics) -> Self {
        let obj = unsafe {
            br::SurfaceObject::new(
                g.adapter(),
                &br::vk::VkMetalSurfaceCreateInfoEXT::new(layer_ptr as *const _),
            )
            .expect("Failed to create Surface")
        };
        let support = g
            .adapter()
            .surface_support(g.graphics_queue_family_index(), &obj)
            .expect("Failed to query Surface Support");
        if !support {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }

        Presenter {
            layer_ptr,
            sc: peridot::IntegratedSwapchain::new(g, obj, acquire_layer_size(layer_ptr)),
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
    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
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
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::FenceMut,
        back_buffer_index: u32,
        render_submission: impl br::SubmissionBatch,
        update_submission: Option<impl br::SubmissionBatch>,
    ) -> br::Result<()> {
        self.sc.render_and_present(
            g,
            last_render_fence,
            back_buffer_index,
            render_submission,
            update_submission,
        )
    }
    /// Returns whether re-initializing is needed for back-buffer resources
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs re-initializing back-buffer resource
        true
    }
    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        acquire_layer_size(self.layer_ptr)
    }
}
pub struct NativeLink {
    rt_view: *mut c_void,
    al: PlatformAssetLoader,
}
unsafe impl Sync for NativeLink {}
unsafe impl Send for NativeLink {}
impl NativeLink {
    pub fn new(rt_view: *mut c_void) -> Self {
        NativeLink {
            al: PlatformAssetLoader::new(),
            rt_view,
        }
    }
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;

    fn instance_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_surface", c"VK_MVK_macos_surface"]
    }
    fn device_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_swapchain"]
    }

    fn asset_loader(&self) -> &PlatformAssetLoader {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(self.rt_view, g)
    }

    fn rendering_precision(&self) -> f32 {
        unsafe { nsscreen_backing_scale_factor() }
    }
}
mod userlib;
type Engine<'q> = peridot::Engine<'q, NativeLink>;

const USERCODE_WAKER_VTABLE: &'static core::task::RawWakerVTable = &core::task::RawWakerVTable::new(
    |ptr| core::task::RawWaker::new(ptr, USERCODE_WAKER_VTABLE),
    |_| {},
    |_| {},
    |_| {},
);

pub struct GameDriver {
    ex_input: peridot::InputProcess,
    frame_timing_sender: async_std::channel::Sender<()>,
    event_sender: async_std::channel::Sender<peridot::EngineEvent>,
    event_queue: peridot::EventQueue,
    _pinned: core::marker::PhantomPinned,
}

pub struct AppInternalState {
    pub event_queue: peridot::EventQueue,
}

fn launch_f<'f, F>(
    initialization_context: *mut core::ffi::c_void,
    v: *mut core::ffi::c_void,
    launch_usercode: impl FnOnce(peridot::Engine<'f, NativeLink>) -> F,
) -> *mut core::ffi::c_void
where
    F: Future<Output = ()> + 'f,
{
    let (event_sender, event_receiver) = async_std::channel::unbounded::<peridot::EngineEvent>();
    let (frame_timing_sender, frame_timing_receiver) = async_std::channel::bounded::<()>(1);

    let state = Box::new(AppInternalState {
        event_queue: peridot::EventQueue::new(),
    });
    let state_ptr = Box::into_raw(state);
    let state_lifetime_extended: &'f AppInternalState = unsafe { &*state_ptr };

    let mut engine = Engine::new(
        userlib::APP_IDENTIFIER,
        userlib::APP_VERSION,
        NativeLink::new(v),
        Default::default(),
        (event_sender.clone(), event_receiver),
        frame_timing_receiver,
        &state_lifetime_extended.event_queue,
    );
    let nih = Box::new(NativeInputHandler::new(v));
    engine.input().set_nativelink(nih);
    let mut nae = NativeAudioEngine::init();
    nae.start(engine.audio_mixer().clone());
    engine.post_init();
    let ex_input = engine.input().clone();

    let usercode_waker =
        unsafe { core::task::Waker::new(core::ptr::null(), &USERCODE_WAKER_VTABLE) };
    let mut usercode = Box::pin(launch_usercode(engine));
    let _ = usercode
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&usercode_waker));

    struct GameDriverContext<F> {
        usercode: Pin<Box<F>>,
        state: Box<AppInternalState>,
    }
    extern "C" fn game_driver_terminate<F: core::future::Future>(ctx: *mut core::ffi::c_void) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.state.event_queue.enqueue(peridot::Event::Shutdown);
        loop {
            let usercode_waker =
                unsafe { core::task::Waker::new(core::ptr::null(), &USERCODE_WAKER_VTABLE) };
            if ctx
                .usercode
                .as_mut()
                .poll(&mut core::task::Context::from_waker(&usercode_waker))
                .is_ready()
            {
                break;
            }
        }

        drop(unsafe { Box::from_raw(ctx) });

        unsafe {
            nsapp_reply_should_terminate();
        }
    }
    extern "C" fn game_driver_update<F: core::future::Future>(ctx: *mut core::ffi::c_void) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.state.event_queue.enqueue(peridot::Event::NextFrame);
        let usercode_waker =
            unsafe { core::task::Waker::new(core::ptr::null(), &USERCODE_WAKER_VTABLE) };
        let _ = ctx
            .usercode
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&usercode_waker));
    }
    extern "C" fn game_driver_resize<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        w: u32,
        h: u32,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.state
            .event_queue
            .enqueue(peridot::Event::Resize(peridot::math::Vector2(w, h)));
        let usercode_waker =
            unsafe { core::task::Waker::new(core::ptr::null(), &USERCODE_WAKER_VTABLE) };
        let _ = ctx
            .usercode
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&usercode_waker));
    }
    let cbs: &'static GameDriverCallbacks = &GameDriverCallbacks {
        terminate: game_driver_terminate::<F>,
        update: game_driver_update::<F>,
        resize: game_driver_resize::<F>,
    };
    unsafe {
        give_game_driver_callbacks(
            initialization_context,
            cbs,
            Box::into_raw(Box::new(GameDriverContext {
                usercode,
                state: Box::from_raw(state_ptr),
            })) as _,
        )
    }

    core::ptr::null_mut()
}

// Swift Linking //

#[repr(C)]
pub struct GameDriverCallbacks {
    terminate: extern "C" fn(*mut core::ffi::c_void),
    update: extern "C" fn(*mut core::ffi::c_void),
    resize: extern "C" fn(*mut core::ffi::c_void, w: u32, h: u32),
}

unsafe extern "C" {
    unsafe fn nsapp_reply_should_terminate();
    #[allow(improper_ctypes)]
    unsafe fn nsbundle_path_for_resource(
        name: *mut NSString,
        oftype: *mut NSString,
    ) -> *mut objc::runtime::Object;
    unsafe fn nsscreen_backing_scale_factor() -> f32;
    unsafe fn obtain_mouse_pointer_position(rt_view: *mut libc::c_void, x: *mut f32, y: *mut f32);

    unsafe fn give_game_driver_callbacks(
        initialization_context: *mut core::ffi::c_void,
        callbacks: *const GameDriverCallbacks,
        aux_ptr: *mut core::ffi::c_void,
    );
}

#[no_mangle]
pub extern "C" fn launch_game(
    initialization_context: *mut core::ffi::c_void,
    v: *mut core::ffi::c_void,
) -> *mut core::ffi::c_void {
    log::set_logger(&LOGGER).expect("Failed to set logger");
    log::set_max_level(log::LevelFilter::Trace);

    let subscriber = Registry::default().with(
        tracing_subscriber::fmt::layer()
            .pretty()
            .with_writer(NativeLogStream)
            .with_filter(tracing_subscriber::filter::EnvFilter::from_default_env()),
    );
    tracing::subscriber::set_global_default(subscriber).expect("Failed to set log subscriber");

    launch_f(initialization_context, v, |mut engine| async move {
        userlib::game_main(&mut engine).await;
    })
}

#[no_mangle]
pub extern "C" fn captionbar_text() -> *mut c_void {
    NSString::from_str(userlib::APP_TITLE)
        .expect("CaptionbarText NSString Allocation")
        .into_id() as *mut _
}

#[no_mangle]
pub extern "C" fn handle_character_keydown(g: *mut GameDriver, character: u8) {
    // trace!("Dispatching Character Down Event: {}", character);
    // unsafe {
    //     (*g).ex_input.dispatch_button_event(
    //         peridot::NativeButtonInput::Character((character as char).to_ascii_uppercase()),
    //         true,
    //     );
    // }
    eprintln!("old function");
}
#[no_mangle]
pub extern "C" fn handle_character_keyup(g: *mut GameDriver, character: u8) {
    // trace!("Dispatching Character Up Event: {}", character);
    // unsafe {
    //     (*g).ex_input.dispatch_button_event(
    //         peridot::NativeButtonInput::Character((character as char).to_ascii_uppercase()),
    //         false,
    //     );
    // }
    eprintln!("old function");
}

const KEYMOD_SHIFT: u8 = 1;
const KEYMOD_OPTION: u8 = 2;
const KEYMOD_CONTROL: u8 = 3;
const KEYMOD_COMMAND: u8 = 4;
const KEYMOD_CAPSLOCK: u8 = 5;
#[no_mangle]
pub extern "C" fn handle_keymod_down(g: *mut GameDriver, code: u8) {
    // trace!("Dispatching Keymod Down Event: {}", code);
    // let code_to_bty = match code {
    //     KEYMOD_SHIFT => peridot::NativeButtonInput::LeftShift,
    //     KEYMOD_OPTION => peridot::NativeButtonInput::LeftAlt,
    //     KEYMOD_CONTROL => peridot::NativeButtonInput::LeftControl,
    //     KEYMOD_COMMAND => peridot::NativeButtonInput::LeftMeta,
    //     KEYMOD_CAPSLOCK => peridot::NativeButtonInput::CapsLock,
    //     _ => return,
    // };
    // unsafe {
    //     (*g).ex_input.dispatch_button_event(code_to_bty, true);
    // }
    eprintln!("old function");
}
#[no_mangle]
pub extern "C" fn handle_keymod_up(g: *mut GameDriver, code: u8) {
    // trace!("Dispatching Keymod Up Event: {}", code);
    // let code_to_bty = match code {
    //     KEYMOD_SHIFT => peridot::NativeButtonInput::LeftShift,
    //     KEYMOD_OPTION => peridot::NativeButtonInput::LeftAlt,
    //     KEYMOD_CONTROL => peridot::NativeButtonInput::LeftControl,
    //     KEYMOD_COMMAND => peridot::NativeButtonInput::LeftMeta,
    //     KEYMOD_CAPSLOCK => peridot::NativeButtonInput::CapsLock,
    //     _ => return,
    // };
    // unsafe {
    //     (*g).ex_input.dispatch_button_event(code_to_bty, false);
    // }
    eprintln!("old function");
}

struct NativeInputHandler {
    rt_view: *mut libc::c_void,
}
unsafe impl Sync for NativeInputHandler {}
unsafe impl Send for NativeInputHandler {}
impl NativeInputHandler {
    fn new(rt_view: *mut libc::c_void) -> Self {
        NativeInputHandler { rt_view }
    }
}
impl peridot::NativeInput for NativeInputHandler {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)> {
        if index == 0 {
            let (mut x, mut y) = (0.0, 0.0);
            unsafe {
                obtain_mouse_pointer_position(self.rt_view, &mut x, &mut y);
            }

            Some((x, y))
        } else {
            None
        }
    }
}

#[no_mangle]
pub extern "C" fn handle_mouse_button_down(g: *mut GameDriver, index: u8) {
    // unsafe {
    //     (*g).ex_input
    //         .dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), true);
    // }
    eprintln!("old function");
}
#[no_mangle]
pub extern "C" fn handle_mouse_button_up(g: *mut GameDriver, index: u8) {
    // unsafe {
    //     (*g).ex_input
    //         .dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), false);
    // }
    eprintln!("old function");
}

#[no_mangle]
pub extern "C" fn report_mouse_move_abs(g: *mut GameDriver, x: f32, y: f32) {
    // unsafe {
    //     let scale = nsscreen_backing_scale_factor();
    //     (*g).ex_input
    //         .dispatch_analog_event(peridot::NativeAnalogInput::MouseX, x * scale, true);
    //     (*g).ex_input
    //         .dispatch_analog_event(peridot::NativeAnalogInput::MouseY, y * scale, true);
    // }
    eprintln!("old function");
}
