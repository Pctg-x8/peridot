
use objc::{msg_send, sel, sel_impl};
use log::*;
use appkit::{NSString, NSRect, CocoaObject};
use libc::c_void;

use bedrock as br;
use peridot::{EngineEvents, FeatureRequests};
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::io::Cursor;
use std::rc::Rc;

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
pub struct NativeLink
{
    al: PlatformAssetLoader, prt: PlatformRenderTargetHandler
}
impl NativeLink
{
    pub fn new(rt_view: *mut c_void) -> Self
    {
        NativeLink
        {
            al: PlatformAssetLoader::new(), prt: PlatformRenderTargetHandler::new(rt_view)
        }
    }
}
impl peridot::NativeLinker for NativeLink
{
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = PlatformRenderTargetHandler;

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn render_target_provider(&self) -> &PlatformRenderTargetHandler { &self.prt }

    fn rendering_precision(&self) -> f32 { unsafe { nsscreen_backing_scale_factor() } }
}
mod userlib;
type Game = userlib::Game<NativeLink>;
type Engine = peridot::Engine<NativeLink>;

pub struct GameDriver
{
    engine: Engine,
    usercode: Game
}
impl GameDriver
{
    pub fn new(rt_view: *mut libc::c_void) -> Self
    {
        let nl = NativeLink::new(rt_view);
        let mut engine = Engine::new(Game::NAME, Game::VERSION, nl, Game::requested_features());
        let usercode = Game::init(&mut engine);
        let nih = Box::new(NativeInputHandler::new(rt_view));
        engine.input_mut().set_nativelink(nih);

        GameDriver { engine, usercode }
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
    fn obtain_mouse_pointer_position(rt_view: *mut libc::c_void, x: *mut f32, y: *mut f32);
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

#[no_mangle]
pub extern "C" fn handle_character_keydown(g: *mut GameDriver, character: u8) {
    trace!("Dispatching Character Down Event: {}", character);
    unsafe { (*g).engine.input().dispatch_button_event(peridot::NativeButtonInput::Character(character as _), true); }
}
#[no_mangle]
pub extern "C" fn handle_character_keyup(g: *mut GameDriver, character: u8) {
    trace!("Dispatching Character Up Event: {}", character);
    unsafe { (*g).engine.input().dispatch_button_event(peridot::NativeButtonInput::Character(character as _), false); }
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
        _ => return
    };
    unsafe { (*g).engine.input().dispatch_button_event(code_to_bty, true); }
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
        _ => return
    };
    unsafe { (*g).engine.input().dispatch_button_event(code_to_bty, false); }
}

struct NativeInputHandler {
    rt_view: *mut libc::c_void
}
impl NativeInputHandler {
    fn new(rt_view: *mut libc::c_void) -> Self {
        NativeInputHandler { rt_view }
    }
}
impl peridot::NativeInput for NativeInputHandler {
    fn get_pointer_position(&self, index: u32) -> Option<(f32, f32)> {
        if index == 0 {
            let (mut x, mut y) = (0.0, 0.0);
            unsafe { obtain_mouse_pointer_position(self.rt_view, &mut x, &mut y); }

            Some((x, y))
        } else {
            None
        }
    }
}

#[no_mangle]
pub extern "C" fn handle_mouse_button_down(g: *mut GameDriver, index: u8) {
    unsafe { (*g).engine.input().dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), true); }
}
#[no_mangle]
pub extern "C" fn handle_mouse_button_up(g: *mut GameDriver, index: u8) {
    unsafe { (*g).engine.input().dispatch_button_event(peridot::NativeButtonInput::Mouse(index as _), false); }
}
