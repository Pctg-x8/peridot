
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
fn acquire_view_size(view: *mut c_void) -> peridot::math::Vector2<usize> {
    let NSRect { size, .. } = unsafe { msg_send![view as *mut objc::runtime::Object, frame] };
    debug!("current geometry extent: {}/{}", size.width, size.height);
    peridot::math::Vector2(size.width as _, size.height as _)
}
pub struct Presenter {
    view_ptr: *mut c_void,
    sc: peridot::IntegratedSwapchain
}
impl Presenter {
    fn new(view_ptr: *mut c_void, g: &peridot::Graphics) -> Self {
        let obj = br::Surface::new_macos(g.instance(), view_ptr as *const _).expect("Failed to create Surface");
        let support = g.adapter().surface_support(g.graphics_queue_family_index(), &obj)
            .expect("Failed to query Surface Support");
        if !support {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }

        Presenter {
            view_ptr,
            sc: peridot::IntegratedSwapchain::new(g, obj, acquire_view_size(view_ptr))
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat { self.sc.format() }
    fn backbuffer_count(&self) -> usize { self.sc.backbuffer_count() }
    fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> { self.sc.backbuffer(index) }

    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        self.sc.emit_initialize_backbuffer_commands(recorder);
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_backbuffer_index()
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
    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> { acquire_view_size(self.view_ptr) }
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
    rt_view: *mut c_void,
    al: PlatformAssetLoader,
    input: PlatformInputProcessPlugin
}
impl NativeLink
{
    pub fn new(rt_view: *mut c_void) -> Self
    {
        NativeLink
        {
            al: PlatformAssetLoader::new(), rt_view,
            input: PlatformInputProcessPlugin::new()
        }
    }
}
impl peridot::NativeLinker for NativeLink
{
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;
    type InputProcessor = PlatformInputProcessPlugin;

    fn instance_extensions(&self) -> Vec<&str> { vec!["VK_KHR_surface", "VK_MVK_macos_surface"] }
    fn device_extensions(&self) -> Vec<&str> { vec!["VK_KHR_swapchain"] }

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter { Presenter::new(self.rt_view, g) }
    fn input_processor_mut(&mut self) -> &mut PlatformInputProcessPlugin { &mut self.input }

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
        let engine = Engine::new(Game::NAME, Game::VERSION, nl, Game::requested_features());
        let usercode = Game::init(&engine);

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
