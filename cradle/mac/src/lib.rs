use appkit::{CocoaObject, NSRect, NSString};
use libc::c_void;
use log::*;
use objc::{msg_send, sel, sel_impl};

use bedrock as br;
use br::PhysicalDevice;
use peridot::mthelper::SharedRef;
use peridot::{EngineEvents, FeatureRequests};
use std::io::Cursor;
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
use std::sync::{Arc, RwLock};
use tracing_subscriber::prelude::__tracing_subscriber_SubscriberExt;
use tracing_subscriber::{Layer, Registry};

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
extern "C" {
    fn NSLog(format: *mut NSString, ...);
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
        let par_path = unsafe {
            CocoaObject::from_id(nsbundle_path_for_resource(&mut *pathbase, &mut *pathext))
                .expect("No Primary Asset")
        };

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
fn acquire_view_size(view: *mut c_void) -> peridot::math::Vector2<usize> {
    let NSRect { size, .. } = unsafe { msg_send![view as *mut objc::runtime::Object, frame] };
    debug!("current geometry extent: {}/{}", size.width, size.height);
    peridot::math::Vector2(size.width as _, size.height as _)
}
pub struct Presenter {
    view_ptr: *mut c_void,
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<SharedRef<br::InstanceObject>>>,
}
impl Presenter {
    fn new(view_ptr: *mut c_void, g: &peridot::Graphics) -> Self {
        let obj = g
            .adapter()
            .new_surface_macos(view_ptr as *const _)
            .expect("Failed to create Surface");
        let support = g
            .adapter()
            .surface_support(g.graphics_queue_family_index(), &obj)
            .expect("Failed to query Surface Support");
        if !support {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }

        Presenter {
            view_ptr,
            sc: peridot::IntegratedSwapchain::new(g, obj, acquire_view_size(view_ptr)),
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
    fn back_buffer(&self, index: usize) -> Option<SharedRef<Self::BackBuffer>> {
        self.sc.back_buffer(index)
    }
    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
    }

    fn emit_initialize_back_buffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut + ?Sized>,
    ) {
        self.sc.emit_initialize_back_buffer_commands(recorder);
    }
    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_back_buffer_index()
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut (impl br::Fence + br::VkHandleMut),
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
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs re-initializing back-buffer resource
        true
    }
    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> {
        acquire_view_size(self.view_ptr)
    }
}
pub struct NativeLink {
    rt_view: *mut c_void,
    al: PlatformAssetLoader,
}
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

    fn instance_extensions(&self) -> Vec<&str> {
        vec!["VK_KHR_surface", "VK_MVK_macos_surface"]
    }
    fn device_extensions(&self) -> Vec<&str> {
        vec!["VK_KHR_swapchain"]
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
type Game = userlib::Game<NativeLink>;
type Engine = peridot::Engine<NativeLink>;

pub struct GameDriver {
    engine: Engine,
    usercode: Game,
    #[allow(dead_code)]
    nae: NativeAudioEngine,
}
impl GameDriver {
    pub fn new(rt_view: *mut libc::c_void) -> Self {
        let nl = NativeLink::new(rt_view);
        let mut engine = Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            Game::requested_features(),
        );
        let usercode = Game::init(&mut engine);
        let nih = Box::new(NativeInputHandler::new(rt_view));
        engine.input_mut().set_nativelink(nih);
        let mut nae = NativeAudioEngine::init();
        nae.start(engine.audio_mixer().clone());
        engine.post_init();

        GameDriver {
            engine,
            usercode,
            nae,
        }
    }

    fn update(&mut self) {
        self.engine.do_update(&mut self.usercode);
    }
    fn resize(&mut self, size: peridot::math::Vector2<usize>) {
        self.engine.do_resize_back_buffer(size, &mut self.usercode);
    }
}

// Swift Linking //

extern "C" {
    fn nsbundle_path_for_resource(
        name: *mut NSString,
        oftype: *mut NSString,
    ) -> *mut objc::runtime::Object;
    fn nsscreen_backing_scale_factor() -> f32;
    fn obtain_mouse_pointer_position(rt_view: *mut libc::c_void, x: *mut f32, y: *mut f32);
}

#[no_mangle]
pub extern "C" fn launch_game(v: *mut libc::c_void) -> *mut GameDriver {
    // log::set_logger(&LOGGER).expect("Failed to set logger");
    // log::set_max_level(log::LevelFilter::Trace);

    let subscriber = Registry::default().with(
        tracing_subscriber::fmt::layer()
            .pretty()
            .with_writer(NativeLogStream)
            .with_filter(tracing_subscriber::filter::EnvFilter::from_default_env()),
    );
    tracing::subscriber::set_global_default(subscriber).expect("Failed to set log subscriber");

    Box::into_raw(Box::new(GameDriver::new(v)))
}
#[no_mangle]
pub extern "C" fn terminate_game(g: *mut GameDriver) {
    unsafe {
        drop(Box::from_raw(g));
    }
}
#[no_mangle]
pub extern "C" fn update_game(g: *mut GameDriver) {
    unsafe {
        (*g).update();
    }
}
#[no_mangle]
pub extern "C" fn resize_game(g: *mut GameDriver, w: u32, h: u32) {
    unsafe {
        (*g).resize(peridot::math::Vector2(w as _, h as _));
    }
}
#[no_mangle]
pub extern "C" fn captionbar_text() -> *mut c_void {
    NSString::from_str(userlib::APP_TITLE)
        .expect("CaptionbarText NSString Allocation")
        .into_id() as *mut _
}

pub struct OutputAU(appkit::AudioUnit);
impl OutputAU {
    fn new() -> Self {
        unsafe {
            let d = appkit::AudioComponentDescription {
                component_type: appkit::kAudioUnitType_Output,
                component_subtype: appkit::kAudioUnitSubType_DefaultOutput,
                component_manufacturer: appkit::kAudioUnitManufacturer_Apple,
                component_flags: 0,
                component_flags_mask: 0,
            };

            let c = appkit::AudioComponentFindNext(std::ptr::null_mut(), &d);
            if c.is_null() {
                panic!("No output audio component found");
            }
            let mut au = std::mem::MaybeUninit::uninit();
            if appkit::AudioComponentInstanceNew(c, au.as_mut_ptr()) != 0 {
                panic!("AudioComponentInstanceNew failed");
            }
            let au = au.assume_init();
            appkit::AudioUnitInitialize(au);

            OutputAU(au)
        }
    }

    fn set_stream_format(&self, format: &appkit::AudioStreamBasicDescription) {
        unsafe {
            let r = appkit::AudioUnitSetProperty(
                self.0,
                appkit::kAudioUnitProperty_StreamFormat,
                appkit::kAudioUnitScope_Input,
                0,
                format as *const _ as *const _,
                std::mem::size_of::<appkit::AudioStreamBasicDescription>() as _,
            );
            if r != 0 {
                panic!("Setting StreamFormat Failed: {}", r);
            }
        }
    }
    fn set_render_callback(&self, callback: appkit::AURenderCallback, context: *mut c_void) {
        let cb = appkit::AURenderCallbackStruct {
            input_proc: callback,
            input_proc_ref_con: context,
        };

        unsafe {
            appkit::AudioUnitSetProperty(
                self.0,
                appkit::kAudioUnitProperty_SetRenderCallback,
                appkit::kAudioUnitScope_Input,
                0,
                &cb as *const _ as *const _,
                std::mem::size_of::<appkit::AURenderCallbackStruct>() as _,
            );
        }
    }
    fn start(&self) {
        unsafe {
            appkit::AudioOutputUnitStart(self.0);
        }
    }
}
impl Drop for OutputAU {
    fn drop(&mut self) {
        unsafe {
            appkit::AudioComponentInstanceDispose(self.0);
        }
    }
}

pub struct NativeAudioEngine {
    output: OutputAU,
    amixer: Option<Box<Arc<RwLock<peridot::audio::Mixer>>>>,
}
impl NativeAudioEngine {
    fn init() -> Self {
        let output = OutputAU::new();

        let af = appkit::AudioStreamBasicDescription {
            sample_rate: 44100.0,
            format_id: appkit::kAudioFormatLinearPCM,
            format_flags: appkit::kAudioFormatFlagIsFloat,
            bits_per_channel: 32,
            channels_per_frame: 2,
            bytes_per_frame: 2 * 4,
            frames_per_packet: 1,
            bytes_per_packet: 2 * 4 * 1,
            _reserved: 0,
        };
        output.set_stream_format(&af);

        NativeAudioEngine {
            output,
            amixer: None,
        }
    }
    fn start(&mut self, mixer: Arc<RwLock<peridot::audio::Mixer>>) {
        let mut mixer = Box::new(mixer);
        self.output
            .set_render_callback(Self::render as _, mixer.as_mut() as *mut _ as _);
        self.output.start();
        mixer.write().expect("Poisoned Audio").start();
        self.amixer = Some(mixer);
    }

    extern "C" fn render(
        in_ref_con: *mut c_void,
        _io_action_flags: *mut appkit::AudioUnitRenderActionFlags,
        _in_time_stamp: *const appkit::AudioTimeStamp,
        _in_bus_number: u32,
        in_number_frames: u32,
        io_data: *mut appkit::AudioBufferList,
    ) -> appkit::OSStatus {
        let ctx = unsafe { &mut *(in_ref_con as *mut Arc<RwLock<peridot::audio::Mixer>>) };
        let bufptr = unsafe {
            std::slice::from_raw_parts_mut(
                (*io_data).buffers[0].data as *mut f32,
                (*io_data).buffers[0].number_channels as usize * in_number_frames as usize,
            )
        };
        for v in bufptr.iter_mut() {
            *v = 0.0;
        }
        ctx.write().expect("Processing WriteLock").process(bufptr);
        // trace!("render callback! {:?} {}", unsafe { &(*io_data).buffers[0] }, in_number_frames);
        0
    }
}

#[no_mangle]
pub extern "C" fn handle_character_keydown(g: *mut GameDriver, character: u8) {
    trace!("Dispatching Character Down Event: {}", character);
    unsafe {
        (*g).engine.input_mut().dispatch_button_event(
            peridot::NativeButtonInput::Character((character as char).to_ascii_uppercase()),
            true,
        );
    }
}
#[no_mangle]
pub extern "C" fn handle_character_keyup(g: *mut GameDriver, character: u8) {
    trace!("Dispatching Character Up Event: {}", character);
    unsafe {
        (*g).engine.input_mut().dispatch_button_event(
            peridot::NativeButtonInput::Character((character as char).to_ascii_uppercase()),
            false,
        );
    }
}

const KEYMOD_SHIFT: u8 = 1;
const KEYMOD_OPTION: u8 = 2;
const KEYMOD_CONTROL: u8 = 3;
const KEYMOD_COMMAND: u8 = 4;
const KEYMOD_CAPSLOCK: u8 = 5;
#[no_mangle]
pub extern "C" fn handle_keymod_down(g: *mut GameDriver, code: u8) {
    trace!("Dispatching Keymod Down Event: {}", code);
    let code_to_bty = match code {
        KEYMOD_SHIFT => peridot::NativeButtonInput::LeftShift,
        KEYMOD_OPTION => peridot::NativeButtonInput::LeftAlt,
        KEYMOD_CONTROL => peridot::NativeButtonInput::LeftControl,
        KEYMOD_COMMAND => peridot::NativeButtonInput::LeftMeta,
        KEYMOD_CAPSLOCK => peridot::NativeButtonInput::CapsLock,
        _ => return,
    };
    unsafe {
        (*g).engine
            .input_mut()
            .dispatch_button_event(code_to_bty, true);
    }
}
#[no_mangle]
pub extern "C" fn handle_keymod_up(g: *mut GameDriver, code: u8) {
    trace!("Dispatching Keymod Up Event: {}", code);
    let code_to_bty = match code {
        KEYMOD_SHIFT => peridot::NativeButtonInput::LeftShift,
        KEYMOD_OPTION => peridot::NativeButtonInput::LeftAlt,
        KEYMOD_CONTROL => peridot::NativeButtonInput::LeftControl,
        KEYMOD_COMMAND => peridot::NativeButtonInput::LeftMeta,
        KEYMOD_CAPSLOCK => peridot::NativeButtonInput::CapsLock,
        _ => return,
    };
    unsafe {
        (*g).engine
            .input_mut()
            .dispatch_button_event(code_to_bty, false);
    }
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
    unsafe {
        (*g).engine
            .input_mut()
            .dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), true);
    }
}
#[no_mangle]
pub extern "C" fn handle_mouse_button_up(g: *mut GameDriver, index: u8) {
    unsafe {
        (*g).engine
            .input_mut()
            .dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), false);
    }
}

#[no_mangle]
pub extern "C" fn report_mouse_move_abs(g: *mut GameDriver, x: f32, y: f32) {
    unsafe {
        let scale = nsscreen_backing_scale_factor();
        (*g).engine.input_mut().dispatch_analog_event(
            peridot::NativeAnalogInput::MouseX,
            x * scale,
            true,
        );
        (*g).engine.input_mut().dispatch_analog_event(
            peridot::NativeAnalogInput::MouseY,
            y * scale,
            true,
        );
    }
}
