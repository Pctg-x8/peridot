#[macro_use]
extern crate objc;
#[macro_use] extern crate log;
extern crate appkit; use appkit::{NSString, NSRect, CocoaObject};
extern crate libc; use libc::c_void;

extern crate peridot;
#[macro_use] extern crate peridot_derive;
extern crate bedrock as br;
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::io::Cursor;
use std::rc::Rc;
use std::collections::BTreeMap;

struct NSLogger;
impl log::Log for NSLogger {
    fn log(&self, record: &log::Record) {
        // if self.enabled(record.metadata()) {
            unsafe {
                let mut fmt = NSString::from_str(&format!("[{}] {}", record.level(), record.args()))
                    .expect("NSString");
                NSLog(&mut *fmt);
            }
        // }
    }
    fn enabled(&self, metadata: &log::Metadata) -> bool { metadata.level() <= log::Level::Info }
    fn flush(&self) {}
}
static LOGGER: NSLogger = NSLogger;
extern "C" {
    fn NSLog(format: *mut NSString, ...);
}

use std::io::prelude::{Read, Seek};
use std::io::SeekFrom;
/// View of a Readable Element
pub struct ReaderView<R: Read + Seek> { inner: R, offset: u64, length: u64 }
impl<R: Read + Seek> ReaderView<R> {
    pub fn new(mut reader: R, offset: u64, length: u64) -> IOResult<Self> {
        reader.seek(SeekFrom::Start(offset))?;
        return Ok(ReaderView { inner: reader, offset, length });
    }
    fn current(&mut self) -> IOResult<u64> { self.inner.seek(SeekFrom::Current(0)).map(|x| x - self.offset) }
    fn left(&mut self) -> IOResult<u64> { self.current().map(|c| self.length - c) }
}
impl<R: Read + Seek> Read for ReaderView<R> {
    fn read(&mut self, mut buf: &mut [u8]) -> IOResult<usize> {
        let left = self.left()?;
        if buf.len() as u64 > left { buf = &mut buf[..left as usize]; }
        return self.inner.read(buf);
    }
}
impl<R: Read + Seek> Seek for ReaderView<R> {
    fn seek(&mut self, pos: SeekFrom) -> IOResult<u64> {
        let pos_translated = match pos {
            SeekFrom::End(x) => SeekFrom::Start(((self.offset + self.length) as i64 - x) as _),
            SeekFrom::Start(x) => SeekFrom::Start(self.offset + x.min(self.length)),
            SeekFrom::Current(x) => SeekFrom::Current(x.min(self.left()? as i64))
        };
        return self.inner.seek(pos_translated);
    }
}
pub struct PlatformAssetLoader { par_path: CocoaObject<NSString> }
impl PlatformAssetLoader {
    fn new() -> Self {
        let mut pathbase = NSString::from_str("assets").expect("NSString for pathbase");
        let mut pathext = NSString::from_str("par").expect("NSString for ext");
        let par_path = unsafe {
            CocoaObject::from_id(nsbundle_path_for_resource(&mut *pathbase, &mut *pathext)).expect("No Primary Asset")
        };

        PlatformAssetLoader { par_path }
    }
}
use peridot::archive as par;
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type Asset = Cursor<Vec<u8>>;
    type StreamingAsset = ReaderView<par::EitherArchiveReader>;

    fn get(&self, path: &str, ext: &str) -> IOResult<Cursor<Vec<u8>>> {
        let mut arc = peridot::archive::ArchiveRead::from_file(self.par_path.to_str(), false)?;
        let b = arc.read_bin(&format!("{}.{}", path.replace(".", "/"), ext))?;
        match b {
            None => Err(IOError::new(ErrorKind::NotFound, "not in primary asset package")),
            Some(b) => Ok(Cursor::new(b))
        }
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<ReaderView<par::EitherArchiveReader>> {
        let arc = peridot::archive::ArchiveRead::from_file(self.par_path.to_str(), false)?;
        let e = arc.find(&format!("{}.{}", path.replace(".", "/"), ext));
        match e {
            None => Err(IOError::new(ErrorKind::NotFound, "not in primary asset package")),
            Some(b) => ReaderView::new(arc.into_inner_reader(), b.byte_offset, b.byte_length)
        }
    }
}
pub struct PlatformRenderTargetHandler(*mut c_void);
impl PlatformRenderTargetHandler {
    fn new(o: *mut c_void) -> Self {
        PlatformRenderTargetHandler(o)
    }
}
impl peridot::PlatformRenderTarget for PlatformRenderTargetHandler {
    fn surface_extension_name(&self) -> &'static str { "VK_MVK_macos_surface" }
    fn create_surface(&self, vi: &br::Instance, pd: &br::PhysicalDevice, renderer_queue_family: u32)
            -> br::Result<peridot::SurfaceInfo> {
        info!("create_surface: {:p}", self.0);
        let obj = br::Surface::new_macos(vi, self.0 as *const _)?;
        if !pd.surface_support(renderer_queue_family, &obj)? {
            panic!("Vulkan Rendering is not supported by this adapter.");
        }
        return peridot::SurfaceInfo::gather_info(&pd, obj);
    }
    fn current_geometry_extent(&self) -> (usize, usize) {
        let NSRect { size, .. } = unsafe { msg_send![self.0 as *mut objc::runtime::Object, frame] };
        debug!("current geometry extent: {}/{}", size.width, size.height);
        (size.width as _, size.height as _)
    }
}
pub struct InputProcessor {
    mouse_ax_handler: (Vec<u32>, Vec<u32>), scroll_ax_handler: (Vec<u32>, Vec<u32>), mag_ax_handler: Vec<u32>,
    axe_values: BTreeMap<u32, f32>
}
impl peridot::PlatformInputProcessor for InputProcessor {
    fn link(&mut self, source: peridot::InputEventSource, vinput_num: u32) {
        
    }
    fn link_axis(&mut self, source: peridot::AxisEventSource, vaxe_num: u32) {
        match source {
            peridot::AxisEventSource::MouseMoveHorizontal => self.mouse_ax_handler.0.push(vaxe_num as _),
            peridot::AxisEventSource::MouseMoveVertical => self.mouse_ax_handler.1.push(vaxe_num as _),
            peridot::AxisEventSource::ScrollHorizontal => self.scroll_ax_handler.0.push(vaxe_num as _),
            peridot::AxisEventSource::ScrollVertical => self.scroll_ax_handler.1.push(vaxe_num as _),
            peridot::AxisEventSource::Magnification => self.mag_ax_handler.push(vaxe_num as _),
            _ => ()
        }
    }
    fn query(&self, vinput_num: u32) -> bool { false }
    fn query_axis(&self, vaxe_num: u32) -> f32 { self.axe_values.get(&vaxe_num).cloned().unwrap_or(0.0) }
}
impl InputProcessor {
    pub fn new() -> Self {
        InputProcessor {
            mouse_ax_handler: (Vec::new(), Vec::new()), scroll_ax_handler: (Vec::new(), Vec::new()),
            mag_ax_handler: Vec::new(),
            axe_values: BTreeMap::new()
        }
    }
    pub fn clear_axe_movements(&mut self) {
        for (_, v) in self.axe_values.iter_mut() { *v = 0.0; }
    }
}
pub struct NativeLink {
    al: PlatformAssetLoader, prt: PlatformRenderTargetHandler,
    input: InputProcessor
}
impl NativeLink {
    pub fn new(rt_view: *mut c_void) -> Self {
        NativeLink {
            al: PlatformAssetLoader::new(), prt: PlatformRenderTargetHandler::new(rt_view),
            input: InputProcessor::new()
        }
    }
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type RenderTargetProvider = PlatformRenderTargetHandler;
    type InputProcessor = InputProcessor;

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn render_target_provider(&self) -> &PlatformRenderTargetHandler { &self.prt }
    fn input_processor(&self) -> &InputProcessor { &self.input }
    fn input_processor_mut(&mut self) -> &mut InputProcessor { &mut self.input }

    fn rendering_precision(&self) -> f32 { unsafe { nsscreen_backing_scale_factor() } }
}
mod userlib;
type Game = userlib::Game<NativeLink>;
type Engine = peridot::Engine<Game, NativeLink>;

// Swift Linking //

extern "C" {
    fn nsbundle_path_for_resource(name: *mut NSString, oftype: *mut NSString) -> *mut objc::runtime::Object;
    fn nsscreen_backing_scale_factor() -> f32;
}

#[allow(dead_code)]
pub struct GameRun(Engine);
#[no_mangle]
pub extern "C" fn launch_game(v: *mut libc::c_void) -> *mut GameRun {
    log::set_logger(&LOGGER).expect("Failed to set logger");
    log::set_max_level(log::LevelFilter::Trace);

    let nlink = NativeLink::new(v);
    let engine = Engine::launch(Game::NAME, Game::VERSION, nlink).expect("Failed to launch the game");
    Box::into_raw(Box::new(GameRun(engine)))
}
#[no_mangle]
pub extern "C" fn terminate_game(gr: *mut GameRun) {
    unsafe { drop(Box::from_raw(gr)); }
}
#[no_mangle]
pub extern "C" fn update_game(gr: *mut GameRun) {
    unsafe {
        let ref mut e = (*gr).0;
        e.do_update();
        e.native_link_mut().input.clear_axe_movements();
    }
}
#[no_mangle]
pub extern "C" fn resize_game(gr: *mut GameRun, w: u32, h: u32) {
    unsafe { (*gr).0.do_resize_backbuffer(peridot::math::Vector2(w as _, h as _)); }
}
#[no_mangle]
pub extern "C" fn captionbar_text() -> *mut c_void {
    NSString::from_str(&format!("{} v{}.{}.{}", Game::NAME, Game::VERSION.0, Game::VERSION.1, Game::VERSION.2))
        .expect("CaptionbarText NSString Allocation").into_id() as *mut _
}

#[no_mangle]
pub extern "C" fn on_mousemove(gr: *mut GameRun, x: f32, y: f32) {
    let engine = unsafe { &mut (*gr).0 };
    let ref mut input_mut = engine.native_link_mut().input;
    for &ie in &input_mut.mouse_ax_handler.0 {
        *input_mut.axe_values.entry(ie).or_insert(0.0) += x;
    }
    for &ie in &input_mut.mouse_ax_handler.1 {
        *input_mut.axe_values.entry(ie).or_insert(0.0) += y;
    }
}
#[no_mangle]
pub extern "C" fn on_scroll(gr: *mut GameRun, x: f32, y: f32) {
    let engine = unsafe { &mut (*gr).0 };
    let ref mut input_mut = engine.native_link_mut().input;
    for &ie in &input_mut.scroll_ax_handler.0 {
        *input_mut.axe_values.entry(ie).or_insert(0.0) += x;
    }
    for &ie in &input_mut.scroll_ax_handler.1 {
        *input_mut.axe_values.entry(ie).or_insert(0.0) += y;
    }
}
#[no_mangle]
pub extern "C" fn on_magnification(gr: *mut GameRun, v: f32) {
    let engine = unsafe { &mut (*gr).0 };
    let ref mut input_mut = engine.native_link_mut().input;
    for &ie in &input_mut.mag_ax_handler {
        *input_mut.axe_values.entry(ie).or_insert(0.0) += v;
    }
}