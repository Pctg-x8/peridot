#[macro_use]
extern crate log;

use bedrock as br;
use br::PhysicalDevice;
use peridot::{
    mthelper::{DynamicMut, DynamicMutabilityProvider, SharedRef},
    EngineEvents, FeatureRequests,
};
use std::fs::File;
use std::io::Result as IOResult;
use std::os::unix::io::{AsRawFd, RawFd};
use std::path::PathBuf;
use xcb::XidNew;

mod sound_backend;
use sound_backend::NativeAudioEngine;
mod epoll;
mod input;
mod kernel_input;
mod udev;
mod userlib;

pub struct PlatformAssetLoader {
    basedir: PathBuf,
    #[cfg(feature = "IterationBuild")]
    builtin_asset_basedir: PathBuf,
}
impl PlatformAssetLoader {
    fn new() -> Self {
        #[cfg(feature = "UseExternalAssetPath")]
        let basedir = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))]
        let basedir = {
            let mut binloc = std::env::current_exe().expect("Getting exe directory");
            binloc.pop();
            binloc.push("assets");
            binloc
        };

        trace!("Using Assets in {}", basedir.display());
        PlatformAssetLoader {
            basedir,
            #[cfg(feature = "IterationBuild")]
            builtin_asset_basedir: PathBuf::from(env!("PERIDOT_BUILTIN_ASSET_PATH")),
        }
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type Asset = File;
    type StreamingAsset = File;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset> {
        #[allow(unused_mut)]
        let mut path_segments = path.split('.').peekable();

        #[cfg(feature = "IterationBuild")]
        if path_segments.peek().map_or(false, |&s| s == "builtin") {
            // Switch base to external builtin path
            path_segments.next();
            let mut apath = self.builtin_asset_basedir.clone();
            apath.extend(path_segments);
            apath.set_extension(ext);

            return File::open(apath);
        }

        let mut apath = self.basedir.clone();
        apath.extend(path_segments);
        apath.set_extension(ext);

        File::open(apath)
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::Asset> {
        self.get(path, ext)
    }
}

pub struct WindowHandler {
    vis: xcb::x::Visualid,
    wid: xcb::x::Window,
    x11_ref: SharedRef<DynamicMut<X11>>,
}

pub struct Presenter {
    x11_ref: SharedRef<DynamicMut<X11>>,
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<peridot::InstanceObject>>,
}
impl Presenter {
    fn new(g: &peridot::Graphics, renderer_queue_family: u32, w: &WindowHandler) -> Self {
        if !g.adapter().xcb_presentation_support(
            renderer_queue_family,
            w.x11_ref.borrow().con.get_raw_conn(),
            w.vis,
        ) {
            panic!("Vulkan Presentation is not supported!");
        }
        let so = g
            .adapter()
            .new_surface_xcb(w.x11_ref.borrow().con.get_raw_conn(), w.wid)
            .expect("Failed to create Surface object");
        if !g
            .adapter()
            .surface_support(renderer_queue_family, &so)
            .expect("Failed to query surface support")
        {
            panic!("Vulkan Surface is not supported");
        }
        let sc = peridot::IntegratedSwapchain::new(g, so, peridot::math::Vector2(120, 120));

        Presenter {
            sc,
            x11_ref: w.x11_ref.clone(),
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    type Backbuffer = br::ImageViewObject<
        br::SwapchainImage<
            SharedRef<
                br::SwapchainObject<
                    peridot::DeviceObject,
                    br::SurfaceObject<peridot::InstanceObject>,
                >,
            >,
        >,
    >;

    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }
    fn backbuffer_count(&self) -> usize {
        self.sc.backbuffer_count()
    }
    fn backbuffer(&self, index: usize) -> Option<SharedRef<Self::Backbuffer>> {
        self.sc.backbuffer(index)
    }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_backbuffer_layout()
    }

    fn emit_initialize_backbuffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut + ?Sized>,
    ) {
        self.sc.emit_initialize_backbuffer_commands(recorder)
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_backbuffer_index()
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut (impl br::Fence + br::VkHandleMut),
        backbuffer_index: u32,
        render_submission: impl br::SubmissionBatch,
        update_submission: Option<impl br::SubmissionBatch>,
    ) -> br::Result<()> {
        self.sc.render_and_present(
            g,
            last_render_fence,
            backbuffer_index,
            render_submission,
            update_submission,
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

pub struct NativeLink {
    al: PlatformAssetLoader,
    wh: WindowHandler,
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter;

    fn instance_extensions(&self) -> Vec<&str> {
        vec!["VK_KHR_surface", "VK_KHR_xcb_surface"]
    }
    fn device_extensions(&self) -> Vec<&str> {
        vec!["VK_KHR_swapchain"]
    }

    fn asset_loader(&self) -> &PlatformAssetLoader {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter {
        Presenter::new(g, g.graphics_queue_family_index(), &self.wh)
    }
}

#[allow(dead_code)]
pub struct X11 {
    con: xcb::Connection,
    wm_protocols: xcb::x::Atom,
    wm_delete_window: xcb::x::Atom,
    vis: xcb::x::Visualid,
    mainwnd_id: xcb::x::Window,
    cached_window_size: peridot::math::Vector2<usize>,
}
impl X11 {
    fn init() -> Self {
        let (con, screen_index) = xcb::Connection::connect(None).expect("Connecting with xcb");
        let s0 = con
            .get_setup()
            .roots()
            .nth(screen_index as _)
            .expect("No screen");
        let vis = s0.root_visual();

        let wm_protocols = con.send_request(&xcb::x::InternAtom {
            only_if_exists: false,
            name: b"WM_PROTOCOLS\0",
        });
        let wm_delete_window = con.send_request(&xcb::x::InternAtom {
            only_if_exists: false,
            name: b"WM_DELETE_WINDOW\0",
        });
        con.flush().expect("Failed to flush");
        let wm_protocols = con
            .wait_for_reply(wm_protocols)
            .expect("No WM_PROTOCOLS")
            .atom();
        let wm_delete_window = con
            .wait_for_reply(wm_delete_window)
            .expect("No WM_DELETE_WINDOW")
            .atom();

        let mainwnd_id = con.generate_id();
        con.send_request(&xcb::x::CreateWindow {
            wid: mainwnd_id,
            parent: s0.root(),
            x: 0,
            y: 0,
            width: 640,
            height: 480,
            border_width: 0,
            class: xcb::x::WindowClass::InputOutput,
            depth: s0.root_depth(),
            visual: vis,
            value_list: &[xcb::x::Cw::EventMask(xcb::x::EventMask::RESIZE_REDIRECT)],
        });
        con.send_request(&xcb::x::ChangeProperty {
            mode: xcb::x::PropMode::Replace,
            window: mainwnd_id,
            property: xcb::x::ATOM_WM_NAME,
            r#type: xcb::x::ATOM_STRING,
            data: userlib::APP_TITLE.as_bytes(),
        });
        con.send_request(&xcb::x::ChangeProperty {
            mode: xcb::x::PropMode::Append,
            window: mainwnd_id,
            property: wm_protocols,
            r#type: xcb::x::ATOM_ATOM,
            data: &[wm_delete_window],
        });
        con.flush().expect("Failed to flush");

        X11 {
            con,
            wm_protocols,
            wm_delete_window,
            vis,
            mainwnd_id,
            cached_window_size: peridot::math::Vector2(640, 480),
        }
    }
    fn fd(&self) -> RawFd {
        self.con.as_raw_fd()
    }
    fn flush(&self) {
        self.con.flush().expect("Failed to flush");
    }
    fn show(&self) {
        self.con.send_request(&xcb::x::MapWindow {
            window: self.mainwnd_id,
        });
        self.con.flush().expect("Failed to flush");
    }
    /// Returns false if application has beed exited
    fn process_all_events(&mut self) -> bool {
        while let Some(ev) = self
            .con
            .poll_for_event()
            .expect("Failed to poll window system events")
        {
            match ev {
                xcb::Event::X(xcb::x::Event::ClientMessage(e)) => match e.data() {
                    xcb::x::ClientMessageData::Data32(d)
                        if unsafe { xcb::x::Atom::new(d[0]) } == self.wm_delete_window =>
                    {
                        return false;
                    }
                    _ => {}
                },
                xcb::Event::X(xcb::x::Event::ResizeRequest(e)) => {
                    self.cached_window_size =
                        peridot::math::Vector2(e.width() as _, e.height() as _);
                }
                _ => {
                    debug!("Unhandled Event: {ev:?}");
                }
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
    usercode: userlib::Game<NativeLink>,
    _snd: NativeAudioEngine,
}
impl GameDriver {
    fn new(wh: WindowHandler, x11: &SharedRef<DynamicMut<X11>>) -> Self {
        let nl = NativeLink {
            al: PlatformAssetLoader::new(),
            wh,
        };
        let mut engine = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            userlib::Game::<NativeLink>::requested_features(),
        );
        let usercode = userlib::Game::init(&mut engine);
        engine
            .input_mut()
            .set_nativelink(Box::new(input::InputNativeLink::new(x11)));
        engine.postinit();
        let _snd = NativeAudioEngine::new(engine.audio_mixer());

        GameDriver {
            engine,
            usercode,
            _snd,
        }
    }

    fn update(&mut self) {
        self.engine.do_update(&mut self.usercode);
    }
}

fn main() {
    env_logger::init();
    let x11 = SharedRef::new(DynamicMut::new(X11::init()));

    let mut gd = GameDriver::new(
        WindowHandler {
            vis: x11.borrow().vis,
            wid: x11.borrow().mainwnd_id,
            x11_ref: x11.clone(),
        },
        &x11,
    );

    let ep = epoll::Epoll::new().expect("Failed to create epoll interface");
    ep.add_fd(x11.borrow().fd(), libc::EPOLLIN as _, 0)
        .expect("Failed to add x11 fd");
    let mut input = input::InputSystem::new(&ep, 1, 2);

    x11.borrow().show();
    gd.engine
        .audio_mixer()
        .write()
        .expect("Failed to mutate audio mixer")
        .start();
    let mut events = vec![
        unsafe { std::mem::MaybeUninit::zeroed().assume_init() };
        2 + input.managed_devices_count()
    ];
    'app: loop {
        if events.len() != 2 + input.managed_devices_count() {
            // resize
            events.resize(2 + input.managed_devices_count(), unsafe {
                std::mem::MaybeUninit::zeroed().assume_init()
            });
        }

        let count = ep
            .wait(&mut events, Some(1))
            .expect("Failed to waiting epoll");
        // FIXME: あとでちゃんと待つ(external_fence_fdでは待てなさそうなので、監視スレッド立てるかしかないか......)
        if count == 0 {
            gd.update();
        }

        for e in &events[..count as usize] {
            if e.u64 == 0 {
                if !x11.borrow_mut().process_all_events() {
                    break 'app;
                }
            } else if e.u64 == 1 {
                input.process_monitor_event(&ep);
            } else {
                input.process_device_event(
                    &mut gd.engine.input_mut().make_event_receiver(),
                    e.u64,
                    &x11.borrow(),
                );
            }
        }
    }
    info!("Terminating Program...");
}
