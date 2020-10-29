
#[macro_use] extern crate log;

use std::fs::File;
use std::path::PathBuf;
use std::io::Result as IOResult;
use std::rc::Rc;
use bedrock as br;
use peridot::{EngineEvents, FeatureRequests};

mod userlib;

pub struct PlatformAssetLoader { basedir: PathBuf }
impl PlatformAssetLoader
{
    fn new() -> Self
    {
        #[cfg(feature = "UseExternalAssetPath")] let basedir = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))] let basedir =
        {
            let mut binloc = std::env::current_exe().expect("Getting exe directory");
            binloc.pop(); binloc.push("assets"); binloc
        };

        trace!("Using Assets in {}", basedir.display());
        PlatformAssetLoader { basedir }
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader
{
    type Asset = File;
    type StreamingAsset = File;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>
    {
        let mut apath = self.basedir.clone();
        apath.push(path.replace(".", "/")); apath.set_extension(ext);
        return File::open(apath);
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::Asset> { self.get(path, ext) }
}

pub struct PlatformInputHandler { processor: Option<Rc<peridot::InputProcess>> }
impl PlatformInputHandler
{
    fn new() -> Self
    {
        PlatformInputHandler { processor: None }
    }
}
impl peridot::InputProcessPlugin for PlatformInputHandler
{
    fn on_start_handle(&mut self, processor: &Rc<peridot::InputProcess>)
    {
        self.processor = Some(processor.clone());
    }
}

pub struct WindowHandler { dp: *mut xcb::ffi::xcb_connection_t, vis: xcb::Visualid, wid: xcb::Window }
pub struct Presenter {
    sc: peridot::IntegratedSwapchain
}
impl Presenter {
    fn new(g: &peridot::Graphics, renderer_queue_family: u32, w: &WindowHandler) -> Self {
        if !g.adapter().xcb_presentation_support(renderer_queue_family, w.dp, w.vis) {
            panic!("Vulkan Presentation is not supported!");
        }
        let so = br::Surface::new_xcb(g.instance(), w.dp, w.wid).expect("Failed to create Surface object");
        if !g.adapter().surface_support(renderer_queue_family, &so).expect("Failed to query surface support") {
            panic!("Vulkan Surface is not supported");
        }
        let sc = peridot::IntegratedSwapchain::new(g, so, peridot::math::Vector2(120, 120));

        Presenter { sc }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat { self.sc.format() }
    fn backbuffer_count(&self) -> usize { self.sc.backbuffer_count() }
    fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> { self.sc.backbuffer(index) }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_backbuffer_layout()
    }
    
    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        self.sc.emit_initialize_backbuffer_commands(recorder)
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
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }

    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> {
        peridot::math::Vector2(120, 120)
    }
}

pub struct NativeLink { al: PlatformAssetLoader, wh: WindowHandler, ip: PlatformInputHandler }
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;
    type InputProcessor = PlatformInputHandler;

    fn instance_extensions(&self) -> Vec<&str> { vec!["VK_KHR_surface", "VK_KHR_xcb_surface"] }
    fn device_extensions(&self) -> Vec<&str> { vec!["VK_KHR_swapchain"] }

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(g, g.graphics_queue_family_index(), &self.wh)
    }
    fn input_processor_mut(&mut self) -> &mut PlatformInputHandler { &mut self.ip }
}

#[allow(dead_code)]
struct X11
{
    con: xcb::Connection, wm_protocols: xcb::Atom, wm_delete_window: xcb::Atom, vis: xcb::Visualid,
    mainwnd_id: xcb::Window
}
impl X11
{
    fn init() -> Self {
        let (con, screen_index) = xcb::Connection::connect(None).expect("Connecting with xcb");
        let s0 = con.get_setup().roots().nth(screen_index as _).expect("No screen");
        let vis = s0.root_visual();

        let wm_protocols = xcb::intern_atom_unchecked(&con, false, "WM_PROTOCOLS");
        let wm_delete_window = xcb::intern_atom_unchecked(&con, false, "WM_DELETE_WINDOW");
        con.flush();
        let wm_protocols = wm_protocols.get_reply().expect("No WM_PROTOCOLS").atom();
        let wm_delete_window = wm_delete_window.get_reply().expect("No WM_DELETE_WINDOW").atom();

        let title = format!("{} v{}.{}.{}",
            userlib::Game::<NativeLink>::NAME,
            userlib::Game::<NativeLink>::VERSION.0,
            userlib::Game::<NativeLink>::VERSION.1,
            userlib::Game::<NativeLink>::VERSION.2
        );
        let mainwnd_id = con.generate_id();
        xcb::create_window(&con, s0.root_depth(), mainwnd_id, s0.root(), 0, 0, 640, 480, 0,
            xcb::WINDOW_CLASS_INPUT_OUTPUT as _, vis, &[]);
        xcb::change_property(&con, xcb::PROP_MODE_REPLACE as _,
            mainwnd_id, xcb::ATOM_WM_NAME, xcb::ATOM_STRING, 8, title.as_bytes());
        xcb::change_property(&con, xcb::PROP_MODE_APPEND as _,
            mainwnd_id, wm_protocols, xcb::ATOM_ATOM, 32, &[wm_delete_window]);
        con.flush();

        X11 { con, wm_protocols, wm_delete_window, vis, mainwnd_id }
    }
    fn show(&self) {
        xcb::map_window(&self.con, self.mainwnd_id);
        self.con.flush();
    }
    /// Returns false if application has beed exited
    fn process_all_events(&self) -> bool {
        while let Some(ev) = self.con.poll_for_event()
        {
            if (ev.response_type() & 0x7f) == xcb::CLIENT_MESSAGE
            {
                let e: &xcb::ClientMessageEvent = unsafe { xcb::cast_event(&ev) };
                if e.data().data32()[0] == self.wm_delete_window { return false; }
            }
            else
            {
                debug!("Generic Event: {:?}", ev.response_type());
            }
        }
        return true;
    }
}

pub struct GameDriver {
    engine: peridot::Engine<NativeLink>,
    usercode: userlib::Game<NativeLink>
}
impl GameDriver {
    fn new(wh: WindowHandler) -> Self {
        let nl = NativeLink {
            al: PlatformAssetLoader::new(),
            wh,
            ip: PlatformInputHandler::new()
        };
        let mut engine = peridot::Engine::new(
            userlib::Game::<NativeLink>::NAME, userlib::Game::<NativeLink>::VERSION,
            nl, userlib::Game::<NativeLink>::requested_features()
        );
        let usercode = userlib::Game::init(&engine);
        engine.postinit();

        GameDriver { engine, usercode }
    }

    fn update(&mut self) { self.engine.do_update(&mut self.usercode); }
}

fn main() {
    env_logger::init();
    let x11 = X11::init();

    let mut gd = GameDriver::new(WindowHandler { dp: x11.con.get_raw_conn(), vis: x11.vis, wid: x11.mainwnd_id });

    x11.show();
    while x11.process_all_events() { gd.update(); }
    println!("Terminating Program...");
}
