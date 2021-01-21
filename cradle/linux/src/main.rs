
#[macro_use] extern crate log;

use std::fs::File;
use std::path::PathBuf;
use std::io::Result as IOResult;
use std::rc::Rc;
use std::cell::RefCell;
use bedrock as br;
use peridot::{EngineEvents, FeatureRequests};
use std::os::unix::io::{AsRawFd, RawFd};

mod udev;
mod epoll;
mod kernel_input;
mod input;
mod userlib;

pub struct PlatformAssetLoader {
    basedir: PathBuf,
    #[cfg(feature = "IterationBuild")]
    builtin_asset_base_dir: PathBuf
}
impl PlatformAssetLoader {
    fn new() -> Self {
        #[cfg(feature = "UseExternalAssetPath")] let basedir = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))] let basedir = {
            let mut binloc = std::env::current_exe().expect("Getting exe directory");
            binloc.pop(); binloc.push("assets"); binloc
        };

        trace!("Using Assets in {}", basedir.display());
        PlatformAssetLoader {
            basedir,
            #[cfg(feature = "IterationBuild")]
            builtin_asset_base_dir: PathBuf::from(env!("PERIDOT_BUILTIN_ASSET_DIRECTORY_PATH"))
        }
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type Asset = File;
    type StreamingAsset = File;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset> {
        let path_segments: Vec<_> = path.split('.').collect();
        #[cfg(feature = "IterationBuild")]
        if path_segments.first().map_or(false, |&s| s == "builtin") {
            // Switch to external builtin directory
            let mut apath = self.builtin_asset_base_dir.clone();
            apath.extend(path_segments.into_iter().skip(1));
            apath.set_extension(ext);

            return File::open(apath);
        }
        let mut apath = self.basedir.clone();
        apath.extend(path_segments);
        apath.set_extension(ext);

        File::open(apath)
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::Asset> { self.get(path, ext) }
}
pub struct WindowHandler {
    dp: *mut xcb::ffi::xcb_connection_t, vis: xcb::Visualid, wid: xcb::Window,
    x11_ref: Rc<RefCell<X11>>
}
pub struct Presenter {
    x11_ref: Rc<RefCell<X11>>,
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

        Presenter { sc, x11_ref: w.x11_ref.clone() }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat { self.sc.format() }
    fn backbuffer_count(&self) -> usize { self.sc.backbuffer_count() }
    fn backbuffer(&self, index: usize) -> Option<std::rc::Rc<br::ImageView>> { self.sc.backbuffer(index) }
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
        self.x11_ref.borrow().mainwnd_geometry().clone()
    }
}

pub struct NativeLink { al: PlatformAssetLoader, wh: WindowHandler }
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;

    fn instance_extensions(&self) -> Vec<&str> { vec!["VK_KHR_surface", "VK_KHR_xcb_surface"] }
    fn device_extensions(&self) -> Vec<&str> { vec!["VK_KHR_swapchain"] }

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(g, g.graphics_queue_family_index(), &self.wh)
    }
}

#[allow(dead_code)]
pub struct X11 {
    con: xcb::Connection, wm_protocols: xcb::Atom, wm_delete_window: xcb::Atom, vis: xcb::Visualid,
    mainwnd_id: xcb::Window,
    cached_window_size: peridot::math::Vector2<usize>
}
impl X11 {
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
            xcb::WINDOW_CLASS_INPUT_OUTPUT as _, vis, &[(xcb::CW_EVENT_MASK, xcb::EVENT_MASK_RESIZE_REDIRECT)]);
        xcb::change_property(&con, xcb::PROP_MODE_REPLACE as _,
            mainwnd_id, xcb::ATOM_WM_NAME, xcb::ATOM_STRING, 8, title.as_bytes());
        xcb::change_property(&con, xcb::PROP_MODE_APPEND as _,
            mainwnd_id, wm_protocols, xcb::ATOM_ATOM, 32, &[wm_delete_window]);
        con.flush();

        X11 {
            con, wm_protocols, wm_delete_window, vis, mainwnd_id,
            cached_window_size: peridot::math::Vector2(640, 480)
        }
    }
    fn fd(&self) -> RawFd { self.con.as_raw_fd() }
    fn flush(&self) { if !self.con.flush() { panic!("Failed to flush"); } }
    fn show(&self) {
        xcb::map_window(&self.con, self.mainwnd_id);
        self.con.flush();
    }
    /// Returns false if application has beed exited
    fn process_all_events(&mut self) -> bool {
        while let Some(ev) = self.con.poll_for_event() {
            let event_type = ev.response_type() & 0x7f;
            if event_type == xcb::CLIENT_MESSAGE {
                let e: &xcb::ClientMessageEvent = unsafe { xcb::cast_event(&ev) };
                if e.data().data32()[0] == self.wm_delete_window { return false; }
            } else if event_type == xcb::RESIZE_REQUEST {
                let e: &xcb::ResizeRequestEvent = unsafe { xcb::cast_event(&ev) };
                self.cached_window_size = peridot::math::Vector2(e.width() as _, e.height() as _);
            } else {
                debug!("Generic Event: {:?}", ev.response_type());
            }
        }
        return true;
    }

    fn mainwnd_geometry(&self) -> &peridot::math::Vector2<usize> {
        &self.cached_window_size
    }
}

pub struct GameDriver {
    engine: peridot::Engine<NativeLink>,
    usercode: userlib::Game<NativeLink>
}
impl GameDriver {
    fn new(wh: WindowHandler, x11: &Rc<RefCell<X11>>) -> Self {
        let nl = NativeLink {
            al: PlatformAssetLoader::new(),
            wh
        };
        let mut engine = peridot::Engine::new(
            userlib::Game::<NativeLink>::NAME, userlib::Game::<NativeLink>::VERSION,
            nl, userlib::Game::<NativeLink>::requested_features()
        );
        let usercode = userlib::Game::init(&mut engine);
        engine.input_mut().set_nativelink(Box::new(input::InputNativeLink::new(x11)));
        engine.postinit();

        GameDriver { engine, usercode }
    }

    fn update(&mut self) { self.engine.do_update(&mut self.usercode); }
}

fn main() {
    env_logger::init();
    let x11 = std::rc::Rc::new(RefCell::new(X11::init()));

    let mut gd = GameDriver::new(
        WindowHandler {
            dp: x11.borrow().con.get_raw_conn(), vis: x11.borrow().vis,
            wid: x11.borrow().mainwnd_id, x11_ref: x11.clone()
        },
        &x11
    );

    let ep = epoll::Epoll::new().expect("Failed to create epoll interface");
    ep.add_fd(x11.borrow().fd(), libc::EPOLLIN as _, 0).expect("Failed to add x11 fd");
    let mut input = input::InputSystem::new(&ep, 1, 2);

    x11.borrow().show();
    let mut events = vec![unsafe { std::mem::MaybeUninit::zeroed().assume_init() }; 2 + input.managed_devices_count()];
    'app: loop {
        if events.len() != 2 + input.managed_devices_count() {
            // resize
            events.resize(2 + input.managed_devices_count(), unsafe { std::mem::MaybeUninit::zeroed().assume_init() });
        }

        let count = ep.wait(&mut events, Some(1)).expect("Failed to waiting epoll");
        // FIXME: あとでちゃんと待つ(external_fence_fdでは待てなさそうなので、監視スレッド立てるかしかないか......)
        if count == 0 { gd.update(); }

        for e in &events[..count as usize] {
            if e.u64 == 0 {
                if !x11.borrow_mut().process_all_events() { break 'app; }
            } else if e.u64 == 1 {
                input.process_monitor_event(&ep);
            } else {
                input.process_device_event(gd.engine.input(), e.u64, &x11.borrow());
            }
        }
    }
    info!("Terminating Program...");
}
