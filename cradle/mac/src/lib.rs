use audio::NativeAudioEngine;
use libc::c_void;
use log::*;

use bedrock as br;
use br::{InstanceChild, PhysicalDevice, SurfaceCreateInfo, VkHandle};
use core::future::Future;
use peridot::mthelper::SharedRef;
use std::ffi::CStr;
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
use std::pin::Pin;
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use tracing_subscriber::{Layer, Registry};

mod audio;

struct NativeLogStream;
impl std::io::Write for &'_ NativeLogStream {
    fn write(&mut self, buf: &[u8]) -> IOResult<usize> {
        let fmt = unsafe { core::str::from_utf8_unchecked(buf) };
        unsafe {
            nslog_utf8(fmt.as_ptr(), fmt.len());
        }

        Ok(buf.len())
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
            let fmt = format!("[{}] {}", record.level(), record.args());
            unsafe {
                nslog_utf8(fmt.as_ptr(), fmt.len());
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
    unsafe fn nslog_utf8(bytes: *const u8, length: usize);
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
    par_path: String,
    par: peridot_archive::Archive,
    par_async: Option<peridot_archive::ArchiveAsync>,
}
impl PlatformAssetLoader {
    fn new() -> Self {
        const PAR_PATH: &str = "assets";
        const PAR_EXT: &str = "par";

        let mut par_path_short = [0u8; 256];
        let mut par_path_len = par_path_short.len();
        let par_path = if unsafe {
            nsbundle_path_for_resource(
                PAR_PATH.as_ptr(),
                PAR_PATH.len(),
                PAR_EXT.as_ptr(),
                PAR_EXT.len(),
                par_path_short.as_mut_ptr(),
                &mut par_path_len,
            )
        } {
            unsafe { String::from_utf8_unchecked(par_path_short[..par_path_len].into()) }
        } else {
            let mut buf = Vec::with_capacity(par_path_len);
            unsafe {
                nsbundle_path_for_resource(
                    PAR_PATH.as_ptr(),
                    PAR_PATH.len(),
                    PAR_EXT.as_ptr(),
                    PAR_EXT.len(),
                    buf.spare_capacity_mut().as_mut_ptr().cast(),
                    &mut par_path_len,
                );
            }
            unsafe { String::from_utf8_unchecked(buf) }
        };
        println!("par_path: {par_path}");

        PlatformAssetLoader {
            par: peridot_archive::Archive::new(
                peridot::native_io::PlatformNativeFileReader::open(&par_path)
                    .expect("Failed to open primary asset"),
                false,
            )
            .map_err(|e| match e {
                peridot::archive::ArchiveReadError::IO(e) => e,
                peridot::archive::ArchiveReadError::IntegrityCheckFailed => {
                    error!("PrimaryArchive integrity check failed!");
                    IOError::other("PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::SignatureMismatch => {
                    error!("PrimaryArchive signature mismatch!");
                    IOError::other("PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::Lz4DecompressError(e) => {
                    error!("lz4 decompress error: {:?}", e);
                    IOError::other("PrimaryArchive read error")
                }
                _ => IOError::other("PrimaryArchive read error"),
            })
            .expect("Failed to intiialize primary asset reader"),
            par_path,
            par_async: None,
        }
    }

    async fn post_init(&mut self) {
        self.par_async = Some(
            peridot_archive::ArchiveAsync::new(
                peridot::native_io::PlatformNativeFileReaderAsync::open(&self.par_path)
                    .expect("Failed to open primary asset"),
                false,
            )
            .await
            .map_err(|e| match e {
                peridot::archive::ArchiveReadError::IO(e) => e,
                peridot::archive::ArchiveReadError::IntegrityCheckFailed => {
                    error!("PrimaryArchive integrity check failed!");
                    IOError::other("PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::SignatureMismatch => {
                    error!("PrimaryArchive signature mismatch!");
                    IOError::other("PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::Lz4DecompressError(e) => {
                    error!("lz4 decompress error: {:?}", e);
                    IOError::other("PrimaryArchive read error")
                }
                _ => IOError::other("PrimaryArchive read error"),
            })
            .expect("Failed to intiialize primary asset reader"),
        );
    }
}
use peridot::archive as par;
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type AssetBlob<'a> =
        peridot_archive::ArchiveBinReader<'a, peridot::native_io::PlatformNativeFileReader>;
    type AssetBlobAsync<'a> = peridot_archive::ArchiveBinReaderAsync<
        'a,
        peridot::native_io::PlatformNativeFileReaderAsync,
    >;
    type StreamingAsset<'a> =
        par::ArchiveBinReader<'a, peridot::native_io::PlatformNativeFileReader>;

    fn get<'a>(&'a self, path: &str, ext: &str) -> IOResult<Self::AssetBlob<'a>> {
        let Some(entry) = self.par.find_entry(path, ext) else {
            return Err(IOError::new(
                ErrorKind::NotFound,
                "not in primary asset package",
            ));
        };

        Ok(self.par.read_bin(entry))
    }

    fn get_async<'a>(
        &'a self,
        path: &str,
        ext: &str,
    ) -> impl core::future::Future<Output = IOResult<Self::AssetBlobAsync<'a>>> {
        async move {
            let par_async = unsafe { self.par_async.as_ref().unwrap_unchecked() };

            let Some(entry) = par_async.find_entry(path, ext) else {
                return Err(IOError::new(
                    ErrorKind::NotFound,
                    "not in primary asset package",
                ));
            };

            Ok(par_async.read_bin(entry))
        }
    }

    fn get_streaming<'a>(&'a self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset<'a>> {
        let Some(entry) = self.par.find_entry(path, ext) else {
            return Err(IOError::new(
                ErrorKind::NotFound,
                "not in primary asset package",
            ));
        };

        Ok(self.par.read_bin(entry))
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

pub struct Presenter {
    layer_ptr: *mut c_void,
    sc: peridot::IntegratedSwapchain<Surface>,
}
unsafe impl Sync for Presenter {}
unsafe impl Send for Presenter {}
impl Presenter {
    fn new(layer_ptr: *mut c_void, g: &peridot::Graphics) -> Self {
        let obj = Surface {
            handle: unsafe {
                br::MetalSurfaceCreateInfo::new(layer_ptr as *const _)
                    .execute(g.device().instance(), None)
                    .expect("Failed to create surface")
            },
            gfx_device: g.device().clone(),
        };
        let support = g
            .device()
            .surface_support(&obj)
            .expect("Failed to query Surface Support");
        if !support {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }

        let mut width = core::mem::MaybeUninit::uninit();
        let mut height = core::mem::MaybeUninit::uninit();
        unsafe {
            acquire_layer_size(layer_ptr, width.as_mut_ptr(), height.as_mut_ptr());
        }

        Presenter {
            layer_ptr,
            sc: peridot::IntegratedSwapchain::new(
                g,
                obj,
                peridot::math::Vector2(unsafe { width.assume_init() }, unsafe {
                    height.assume_init()
                }),
            ),
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }

    fn back_buffer_count(&self) -> usize {
        self.sc.back_buffer_count()
    }

    fn back_buffer_size(&self) -> peridot::math::Vector2<u32> {
        self.sc.back_buffer_size()
    }

    fn back_buffer<'a>(&'a self, index: usize) -> Option<br::VkHandleRef<'a, br::vk::VkImage>> {
        self.sc.back_buffer(index)
    }

    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
    }

    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: br::CmdRecord<'r>,
    ) -> br::CmdRecord<'r> {
        self.sc.emit_initialize_back_buffer_commands(recorder)
    }

    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_back_buffer_index()
    }

    fn render_and_present<'s, 'r>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        back_buffer_index: u32,
        render_submission: peridot::SubmissionBatchBuilder<'r>,
        update_submission: Option<peridot::SubmissionBatchBuilder<'r>>,
    ) -> br::Result<()>
    where
        's: 'r,
    {
        self.sc.render_and_present(
            g,
            last_render_fence,
            back_buffer_index,
            render_submission,
            update_submission,
        )
    }

    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs re-initializing back-buffer resource
        true
    }

    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        let mut w = core::mem::MaybeUninit::uninit();
        let mut h = core::mem::MaybeUninit::uninit();
        unsafe {
            acquire_layer_size(self.layer_ptr, w.as_mut_ptr(), h.as_mut_ptr());
        }

        peridot::math::Vector2(unsafe { w.assume_init() }, unsafe { h.assume_init() })
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
    |ptr| /*println!("game_main wake")*/{unsafe { schedule_usercode_task_polling(ptr.cast_mut().cast::<core::ffi::c_void>()); }},
    |ptr| /*println!("game_main wake by ref")*/{ unsafe { schedule_usercode_task_polling(ptr.cast_mut().cast::<core::ffi::c_void>()); } },
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
) where
    F: Future<Output = ()> + 'f,
{
    let (event_sender, event_receiver) = async_std::channel::unbounded::<peridot::EngineEvent>();
    let (_, frame_timing_receiver) = async_std::channel::bounded::<()>(1);

    let state = Box::new(AppInternalState {
        event_queue: peridot::EventQueue::new(),
    });
    let state_ptr = Box::into_raw(state);
    let state_lifetime_extended: &'f AppInternalState = unsafe { &*state_ptr };

    let mut engine = Engine::new(
        userlib::APP_IDENTIFIER,
        userlib::APP_VERSION,
        NativeLink::new(v),
        unsafe { core::mem::MaybeUninit::zeroed().assume_init() },
        (event_sender.clone(), event_receiver),
        frame_timing_receiver,
        &state_lifetime_extended.event_queue,
    );
    let nih = Box::new(NativeInputHandler::new(v));
    engine.input().set_nativelink(nih);
    // let mut nae = NativeAudioEngine::init();
    // nae.start(engine.audio_mixer().clone());
    engine.post_init();
    let input = engine.input().clone();

    let usercode_waker = unsafe {
        core::task::Waker::new(initialization_context.cast::<()>(), &USERCODE_WAKER_VTABLE)
    };
    let usercode = Box::pin(launch_usercode(engine));

    struct GameDriverContext<F> {
        usercode: Pin<Box<F>>,
        usercode_waker: core::task::Waker,
        state: Box<AppInternalState>,
        input: peridot::InputProcess,
    }
    extern "C" fn game_driver_terminate<F: core::future::Future>(ctx: *mut core::ffi::c_void) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.state.event_queue.enqueue(peridot::Event::Shutdown);
        loop {
            if ctx
                .usercode
                .as_mut()
                .poll(&mut core::task::Context::from_waker(&ctx.usercode_waker))
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
    }
    extern "C" fn game_driver_handle_character_keydown<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        character: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.input.dispatch_button_event(
            peridot::NativeButtonInput::Character((character as char).to_ascii_uppercase()),
            true,
        );
    }
    extern "C" fn game_driver_handle_character_keyup<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        character: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.input.dispatch_button_event(
            peridot::NativeButtonInput::Character((character as char).to_ascii_uppercase()),
            false,
        );
    }
    extern "C" fn game_driver_handle_keymod_down<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        code: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        let code_to_bty = match code {
            KEYMOD_SHIFT => peridot::NativeButtonInput::LeftShift,
            KEYMOD_OPTION => peridot::NativeButtonInput::LeftAlt,
            KEYMOD_CONTROL => peridot::NativeButtonInput::LeftControl,
            KEYMOD_COMMAND => peridot::NativeButtonInput::LeftMeta,
            KEYMOD_CAPSLOCK => peridot::NativeButtonInput::CapsLock,
            _ => return,
        };
        ctx.input.dispatch_button_event(code_to_bty, true);
    }
    extern "C" fn game_driver_handle_keymod_up<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        code: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        let code_to_bty = match code {
            KEYMOD_SHIFT => peridot::NativeButtonInput::LeftShift,
            KEYMOD_OPTION => peridot::NativeButtonInput::LeftAlt,
            KEYMOD_CONTROL => peridot::NativeButtonInput::LeftControl,
            KEYMOD_COMMAND => peridot::NativeButtonInput::LeftMeta,
            KEYMOD_CAPSLOCK => peridot::NativeButtonInput::CapsLock,
            _ => return,
        };
        ctx.input.dispatch_button_event(code_to_bty, false);
    }
    extern "C" fn game_driver_handle_mouse_button_down<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        index: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.input
            .dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), true);
    }
    extern "C" fn game_driver_handle_mouse_button_up<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        index: u8,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        ctx.input
            .dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), false);
    }
    extern "C" fn game_driver_report_mouse_move_abs<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
        x: f32,
        y: f32,
    ) {
        let ctx = unsafe { &mut *(ctx as *mut GameDriverContext<F>) };

        let scale = unsafe { nsscreen_backing_scale_factor() };
        ctx.input
            .dispatch_analog_event(peridot::NativeAnalogInput::MouseX, x * scale, true);
        ctx.input
            .dispatch_analog_event(peridot::NativeAnalogInput::MouseY, y * scale, true);
    }
    extern "C" fn game_driver_poll_usercode_task<F: core::future::Future>(
        ctx: *mut core::ffi::c_void,
    ) {
        let ctx = unsafe { &mut *ctx.cast::<GameDriverContext<F>>() };

        let r = ctx
            .usercode
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&ctx.usercode_waker));
        if r.is_ready() {
            tracing::warn!("Usercode task terminated?");
        }
    }
    let cbs: &'static GameDriverCallbacks = &GameDriverCallbacks {
        terminate: game_driver_terminate::<F>,
        update: game_driver_update::<F>,
        resize: game_driver_resize::<F>,
        handle_character_keydown: game_driver_handle_character_keydown::<F>,
        handle_character_keyup: game_driver_handle_character_keyup::<F>,
        handle_keymod_down: game_driver_handle_keymod_down::<F>,
        handle_keymod_up: game_driver_handle_keymod_up::<F>,
        handle_mouse_button_down: game_driver_handle_mouse_button_down::<F>,
        handle_mouse_button_up: game_driver_handle_mouse_button_up::<F>,
        report_mouse_move_abs: game_driver_report_mouse_move_abs::<F>,
        poll_usercode_task: game_driver_poll_usercode_task::<F>,
    };
    let context_ptr = Box::into_raw(Box::new(GameDriverContext {
        usercode,
        usercode_waker,
        state: unsafe { Box::from_raw(state_ptr) },
        input,
    }));
    unsafe { give_game_driver_callbacks(initialization_context, cbs, context_ptr as _) }

    // execute initial process
    game_driver_poll_usercode_task::<F>(context_ptr.cast::<core::ffi::c_void>());
}

// Swift Linking //

const KEYMOD_SHIFT: u8 = 1;
const KEYMOD_OPTION: u8 = 2;
const KEYMOD_CONTROL: u8 = 3;
const KEYMOD_COMMAND: u8 = 4;
const KEYMOD_CAPSLOCK: u8 = 5;

#[repr(C)]
pub struct GameDriverCallbacks {
    terminate: extern "C" fn(*mut core::ffi::c_void),
    update: extern "C" fn(*mut core::ffi::c_void),
    resize: extern "C" fn(*mut core::ffi::c_void, w: u32, h: u32),
    handle_character_keydown: extern "C" fn(*mut core::ffi::c_void, character: u8),
    handle_character_keyup: extern "C" fn(*mut core::ffi::c_void, character: u8),
    handle_keymod_down: extern "C" fn(*mut core::ffi::c_void, code: u8),
    handle_keymod_up: extern "C" fn(*mut core::ffi::c_void, code: u8),
    handle_mouse_button_down: extern "C" fn(*mut core::ffi::c_void, index: u8),
    handle_mouse_button_up: extern "C" fn(*mut core::ffi::c_void, index: u8),
    report_mouse_move_abs: extern "C" fn(*mut core::ffi::c_void, x: f32, y: f32),
    poll_usercode_task: extern "C" fn(*mut core::ffi::c_void),
}

unsafe extern "C" {
    unsafe fn acquire_layer_size(
        layer_ptr: *const core::ffi::c_void,
        width: *mut u32,
        height: *mut u32,
    );

    unsafe fn nsapp_reply_should_terminate();
    #[allow(improper_ctypes)]
    unsafe fn nsbundle_path_for_resource(
        path: *const u8,
        path_length: usize,
        ext: *const u8,
        ext_lenght: usize,
        out_path: *mut u8,
        out_path_length: *mut usize,
    ) -> bool;
    unsafe fn nsscreen_backing_scale_factor() -> f32;
    unsafe fn obtain_mouse_pointer_position(rt_view: *mut libc::c_void, x: *mut f32, y: *mut f32);

    unsafe fn give_game_driver_callbacks(
        initialization_context: *mut core::ffi::c_void,
        callbacks: *const GameDriverCallbacks,
        aux_ptr: *mut core::ffi::c_void,
    );

    fn schedule_usercode_task_polling(initialization_context: *mut core::ffi::c_void);
}

#[no_mangle]
pub extern "C" fn launch_game(
    initialization_context: *mut core::ffi::c_void,
    v: *mut core::ffi::c_void,
) {
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
        engine.internal_native_link_mut().al.post_init().await;
        userlib::game_main(&mut engine).await;
    });
}

#[no_mangle]
pub extern "C" fn captionbar_text(length: *mut usize) -> *const core::ffi::c_char {
    unsafe {
        *length = userlib::APP_TITLE.len();
    }

    userlib::APP_TITLE.as_ptr().cast()
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
