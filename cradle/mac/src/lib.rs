
use objc::{msg_send, sel, sel_impl};
use log::*;
use appkit::{NSString, NSRect, CocoaObject};
use libc::c_void;

use bedrock as br;
use peridot::{EngineEvents, FeatureRequests};
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::io::Cursor;
use std::rc::Rc;
use std::sync::{Arc, RwLock};

struct NSLogger;
impl log::Log for NSLogger
{
    fn log(&self, record: &log::Record)
    {
        // if self.enabled(record.metadata())
        {
            unsafe
            {
                let mut fmt = NSString::from_str(&format!("[{}] {}", record.level(), record.args()))
                    .expect("NSString");
                NSLog(&mut *fmt);
            }
        }
    }
    fn enabled(&self, metadata: &log::Metadata) -> bool { metadata.level() <= log::Level::Info }
    fn flush(&self) {}
}
static LOGGER: NSLogger = NSLogger;
extern "C"
{
    fn NSLog(format: *mut NSString, ...);
}

use std::io::prelude::{Read, Seek};
use std::io::SeekFrom;
/// View of a Readable Element
pub struct ReaderView<R: Read + Seek> { inner: R, offset: u64, length: u64 }
impl<R: Read + Seek> ReaderView<R>
{
    pub fn new(mut reader: R, offset: u64, length: u64) -> IOResult<Self>
    {
        reader.seek(SeekFrom::Start(offset))?;
        return Ok(ReaderView { inner: reader, offset, length });
    }
    fn current(&mut self) -> IOResult<u64> { self.inner.seek(SeekFrom::Current(0)).map(|x| x - self.offset) }
    fn left(&mut self) -> IOResult<u64> { self.current().map(|c| self.length - c) }
}
impl<R: Read + Seek> Read for ReaderView<R>
{
    fn read(&mut self, mut buf: &mut [u8]) -> IOResult<usize>
    {
        let left = self.left()?;
        if buf.len() as u64 > left { buf = &mut buf[..left as usize]; }
        return self.inner.read(buf);
    }
}
impl<R: Read + Seek> Seek for ReaderView<R>
{
    fn seek(&mut self, pos: SeekFrom) -> IOResult<u64>
    {
        let pos_translated = match pos
        {
            SeekFrom::End(x) => SeekFrom::Start(((self.offset + self.length) as i64 - x) as _),
            SeekFrom::Start(x) => SeekFrom::Start(self.offset + x.min(self.length)),
            SeekFrom::Current(x) => SeekFrom::Current(x.min(self.left()? as i64))
        };
        return self.inner.seek(pos_translated);
    }
}
pub struct PlatformAssetLoader { par_path: CocoaObject<NSString> }
impl PlatformAssetLoader
{
    fn new() -> Self
    {
        let mut pathbase = NSString::from_str("assets").expect("NSString for pathbase");
        let mut pathext = NSString::from_str("par").expect("NSString for ext");
        let par_path = unsafe
        {
            CocoaObject::from_id(nsbundle_path_for_resource(&mut *pathbase, &mut *pathext)).expect("No Primary Asset")
        };

        PlatformAssetLoader { par_path }
    }
}
use peridot::archive as par;
impl peridot::PlatformAssetLoader for PlatformAssetLoader
{
    type Asset = Cursor<Vec<u8>>;
    type StreamingAsset = ReaderView<par::EitherArchiveReader>;

    fn get(&self, path: &str, ext: &str) -> IOResult<Cursor<Vec<u8>>>
    {
        let mut arc = peridot::archive::ArchiveRead::from_file(self.par_path.to_str(), false).map_err(|e| match e
        {
            peridot::archive::ArchiveReadError::IO(e) => e,
            peridot::archive::ArchiveReadError::IntegrityCheckFailed =>
            {
                error!("PrimaryArchive integrity check failed!");
                IOError::new(ErrorKind::Other, "PrimaryArchive read error")
            },
            peridot::archive::ArchiveReadError::SignatureMismatch =>
            {
                error!("PrimaryArchive signature mismatch!");
                IOError::new(ErrorKind::Other, "PrimaryArchive read error")
            },
            peridot::archive::ArchiveReadError::Lz4DecompressError(e) =>
            {
                error!("lz4 decompress error: {:?}", e);
                IOError::new(ErrorKind::Other, "PrimaryArchive read error")
            },
            _ => IOError::new(ErrorKind::Other, "PrimaryArchive read error")
        })?;
        let b = arc.read_bin(&format!("{}.{}", path.replace(".", "/"), ext))?;
        match b
        {
            None => Err(IOError::new(ErrorKind::NotFound, "not in primary asset package")),
            Some(b) => Ok(Cursor::new(b))
        }
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<ReaderView<par::EitherArchiveReader>>
    {
        let arc = peridot::archive::ArchiveRead::from_file(self.par_path.to_str(), false).map_err(|e| match e
        {
            peridot::archive::ArchiveReadError::IO(e) => e,
            peridot::archive::ArchiveReadError::IntegrityCheckFailed =>
            {
                error!("PrimaryArchive integrity check failed!");
                IOError::new(ErrorKind::Other, "PrimaryArchive read error")
            },
            peridot::archive::ArchiveReadError::SignatureMismatch =>
            {
                error!("PrimaryArchive signature mismatch!");
                IOError::new(ErrorKind::Other, "PrimaryArchive read error")
            },
            peridot::archive::ArchiveReadError::Lz4DecompressError(e) =>
            {
                error!("lz4 decompress error: {:?}", e);
                IOError::new(ErrorKind::Other, "PrimaryArchive read error")
            },
            _ => IOError::new(ErrorKind::Other, "PrimaryArchive read error")
        })?;
        let e = arc.find(&format!("{}.{}", path.replace(".", "/"), ext));
        match e
        {
            None => Err(IOError::new(ErrorKind::NotFound, "not in primary asset package")),
            Some(b) => ReaderView::new(arc.into_inner_reader(), b.byte_offset, b.byte_length)
        }
    }
}
pub struct PlatformRenderTargetHandler(*mut c_void);
impl PlatformRenderTargetHandler
{
    fn new(o: *mut c_void) -> Self
    {
        PlatformRenderTargetHandler(o)
    }
}
impl peridot::PlatformRenderTarget for PlatformRenderTargetHandler
{
    fn surface_extension_name(&self) -> &'static str { "VK_MVK_macos_surface" }
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
        -> br::Result<peridot::SurfaceInfo>
    {
        let obj = br::Surface::new_macos(vi, self.0 as *const _)?;
        if !pd.surface_support(renderer_queue_family, &obj)?
        {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }
        return peridot::SurfaceInfo::gather_info(&pd, obj);
    }
    fn current_geometry_extent(&self) -> (usize, usize)
    {
        let NSRect { size, .. } = unsafe { msg_send![self.0 as *mut objc::runtime::Object, frame] };
        debug!("current geometry extent: {}/{}", size.width, size.height);
        (size.width as _, size.height as _)
    }
}
// TODO: InputProcessPlugin実装する
pub struct PlatformInputProcessPlugin { processor: Option<Rc<peridot::InputProcess>> }
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
    }
}
pub struct NativeLink
{
    al: PlatformAssetLoader, prt: PlatformRenderTargetHandler,
    input: PlatformInputProcessPlugin
}
impl NativeLink
{
    pub fn new(rt_view: *mut c_void) -> Self
    {
        NativeLink
        {
            al: PlatformAssetLoader::new(), prt: PlatformRenderTargetHandler::new(rt_view),
            input: PlatformInputProcessPlugin::new()
        }
    }
}
impl peridot::NativeLinker for NativeLink
{
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = PlatformRenderTargetHandler;
    type InputProcessor = PlatformInputProcessPlugin;

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn render_target_provider(&self) -> &PlatformRenderTargetHandler { &self.prt }
    fn input_processor_mut(&mut self) -> &mut PlatformInputProcessPlugin { &mut self.input }

    fn rendering_precision(&self) -> f32 { unsafe { nsscreen_backing_scale_factor() } }
}
mod userlib;
type Game = userlib::Game<NativeLink>;
type Engine = peridot::Engine<NativeLink>;

pub struct GameDriver
{
    engine: Engine,
    usercode: Game,
    nae: NativeAudioEngine
}
impl GameDriver
{
    pub fn new(rt_view: *mut libc::c_void) -> Self
    {
        let nl = NativeLink::new(rt_view);
        let engine = Engine::new(Game::NAME, Game::VERSION, nl, Game::requested_features());
        let usercode = Game::init(&engine);
        engine.postinit();
        let mut nae = NativeAudioEngine::init();
        nae.start(engine.audio_mixer().clone());

        GameDriver { engine, usercode, nae }
    }

    fn update(&mut self)
    {
        self.engine.do_update(&mut self.usercode);
    }
    fn resize(&mut self, size: peridot::math::Vector2<usize>)
    {
        self.engine.do_resize_backbuffer(size, &mut self.usercode);
    }
}

// Swift Linking //

extern "C"
{
    fn nsbundle_path_for_resource(name: *mut NSString, oftype: *mut NSString) -> *mut objc::runtime::Object;
    fn nsscreen_backing_scale_factor() -> f32;
}

#[no_mangle]
pub extern "C" fn launch_game(v: *mut libc::c_void) -> *mut GameDriver
{
    log::set_logger(&LOGGER).expect("Failed to set logger");
    log::set_max_level(log::LevelFilter::Trace);

    Box::into_raw(Box::new(GameDriver::new(v)))
}
#[no_mangle]
pub extern "C" fn terminate_game(g: *mut GameDriver)
{
    unsafe { drop(Box::from_raw(g)); }
}
#[no_mangle]
pub extern "C" fn update_game(g: *mut GameDriver)
{
    unsafe { (*g).update(); }
}
#[no_mangle]
pub extern "C" fn resize_game(g: *mut GameDriver, w: u32, h: u32)
{
    unsafe { (*g).resize(peridot::math::Vector2(w as _, h as _)); }
}
#[no_mangle]
pub extern "C" fn captionbar_text() -> *mut c_void
{
    NSString::from_str(&format!("{} v{}.{}.{}", Game::NAME, Game::VERSION.0, Game::VERSION.1, Game::VERSION.2))
        .expect("CaptionbarText NSString Allocation").into_id() as *mut _
}

pub struct OutputAU(appkit::AudioUnit);
impl OutputAU
{
    fn new() -> Self
    {
        unsafe
        {
            let d = appkit::AudioComponentDescription
            {
                component_type: appkit::kAudioUnitType_Output,
                component_subtype: appkit::kAudioUnitSubType_DefaultOutput,
                component_manufacturer: appkit::kAudioUnitManufacturer_Apple,
                component_flags: 0, component_flags_mask: 0
            };

            let c = appkit::AudioComponentFindNext(std::ptr::null_mut(), &d);
            if c.is_null() { panic!("No output audio component found"); }
            let mut au = std::mem::MaybeUninit::uninit();
            if appkit::AudioComponentInstanceNew(c, au.as_mut_ptr()) != 0
            {
                panic!("AudioComponentInstanceNew failed");
            }
            let au = au.assume_init();
            appkit::AudioUnitInitialize(au);

            OutputAU(au)
        }
    }

    fn set_stream_format(&self, format: &appkit::AudioStreamBasicDescription)
    {
        unsafe
        {
            let r = appkit::AudioUnitSetProperty(self.0,
                appkit::kAudioUnitProperty_StreamFormat, appkit::kAudioUnitScope_Input,
                0, format as *const _ as *const _, std::mem::size_of::<appkit::AudioStreamBasicDescription>() as _);
            if r != 0 { panic!("Setting StreamFormat Failed: {}", r); }
        }
    }
    fn set_render_callback(&self, callback: appkit::AURenderCallback, context: *mut c_void)
    {
        let cb = appkit::AURenderCallbackStruct { input_proc: callback, input_proc_ref_con: context };

        unsafe
        {
            appkit::AudioUnitSetProperty(self.0, appkit::kAudioUnitProperty_SetRenderCallback,
                appkit::kAudioUnitScope_Input, 0, &cb as *const _ as *const _,
                std::mem::size_of::<appkit::AURenderCallbackStruct>() as _);
        }
    }
    fn start(&self)
    {
        unsafe { appkit::AudioOutputUnitStart(self.0); }
    }
}
impl Drop for OutputAU
{
    fn drop(&mut self)
    {
        unsafe { appkit::AudioComponentInstanceDispose(self.0); }
    }
}

pub struct NativeAudioEngine
{
    output: OutputAU, amixer: Option<Box<Arc<RwLock<peridot::audio::Mixer>>>>
}
impl NativeAudioEngine
{
    fn init() -> Self
    {
        let output = OutputAU::new();

        let af = appkit::AudioStreamBasicDescription
        {
            sample_rate: 44100.0,
            format_id: appkit::kAudioFormatLinearPCM,
            format_flags: appkit::kAudioFormatFlagIsFloat,
            bits_per_channel: 32,
            channels_per_frame: 2,
            bytes_per_frame: 2 * 4,
            frames_per_packet: 1,
            bytes_per_packet: 2 * 4 * 1,
            _reserved: 0
        };
        output.set_stream_format(&af);

        NativeAudioEngine { output, amixer: None }
    }
    fn start(&mut self, mixer: Arc<RwLock<peridot::audio::Mixer>>)
    {
        let bptr = Box::into_raw(Box::new(mixer));
        self.amixer = Some(unsafe { Box::from_raw(bptr) });
        self.output.set_render_callback(Self::render as _, bptr as *mut _);
        self.output.start();
        self.amixer.as_ref().expect("no audio?").write().expect("Poisoning Audio").start();
    }

    extern "C" fn render(in_ref_con: *mut c_void,
        _io_action_flags: *mut appkit::AudioUnitRenderActionFlags,
        _in_time_stamp: *const appkit::AudioTimeStamp,
        _in_bus_number: u32,
        in_number_frames: u32,
        io_data: *mut appkit::AudioBufferList) -> appkit::OSStatus
    {
        let ctx = unsafe { &mut *(in_ref_con as *mut Arc<RwLock<peridot::audio::Mixer>>) };
        let bufptr = unsafe
        {
            std::slice::from_raw_parts_mut((*io_data).buffers[0].data as *mut f32,
                (*io_data).buffers[0].number_channels as usize * in_number_frames as usize)
        };
        for v in bufptr.iter_mut() { *v = 0.0; }
        ctx.write().expect("Processing WriteLock").process(bufptr);
        // trace!("render callback! {:?} {}", unsafe { &(*io_data).buffers[0] }, in_number_frames);
        0
    }
}
