use std::{collections::HashMap, path::PathBuf};

use bedrock as br;
use br::PhysicalDevice;
use peridot::{mthelper::SharedRef, EngineEvents, FeatureRequests};
use std::fs::File;
use std::io::Result as IOResult;
use tracing::{info, trace};
use wayland_client::{
    protocol::{
        wl_callback::WlCallback, wl_compositor::WlCompositor, wl_display::WlDisplay,
        wl_registry::WlRegistry, wl_surface::WlSurface,
    },
    Proxy,
};
use wayland_protocols::{
    wp::fractional_scale::v1::client::{
        wp_fractional_scale_manager_v1::WpFractionalScaleManagerV1,
        wp_fractional_scale_v1::WpFractionalScaleV1,
    },
    xdg::{
        decoration::zv1::client::{
            zxdg_decoration_manager_v1::ZxdgDecorationManagerV1,
            zxdg_toplevel_decoration_v1::ZxdgToplevelDecorationV1,
        },
        shell::client::{
            xdg_surface::XdgSurface, xdg_toplevel::XdgToplevel, xdg_wm_base::XdgWmBase,
        },
    },
};
mod userlib;

fn main() {
    tracing_subscriber::fmt::Subscriber::builder().init();

    smol::block_on(run());
}

pub struct RegisteredInterface {
    pub name: u32,
    pub version: u32,
}
impl RegisteredInterface {
    pub fn bind<I, D, S>(
        &self,
        registry: &WlRegistry,
        qhandle: &wayland_client::QueueHandle<D>,
        state: S,
    ) -> I
    where
        D: wayland_client::Dispatch<I, S> + 'static,
        I: wayland_client::Proxy + 'static,
        S: Send + Sync + 'static,
    {
        registry.bind(self.name, self.version, qhandle, state)
    }
}

pub struct CallbackHandler {
    pub interfaces: HashMap<String, RegisteredInterface>,
}
impl CallbackHandler {
    pub fn bind_interface<I, D, S>(
        &self,
        registry: &WlRegistry,
        qhandle: &wayland_client::QueueHandle<D>,
        state: S,
    ) -> Option<I>
    where
        D: wayland_client::Dispatch<I, S> + 'static,
        I: wayland_client::Proxy + 'static,
        S: Send + Sync + 'static,
    {
        self.interfaces
            .get(I::interface().name)
            .map(|ri| ri.bind(registry, qhandle, state))
    }
}
impl wayland_client::Dispatch<WlCallback, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &WlCallback,
        event: <WlCallback as wayland_client::Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            wayland_client::protocol::wl_callback::Event::Done { .. } => {
                info!("callback done");
            }
            _ => (),
        }
    }
}
impl wayland_client::Dispatch<WlRegistry, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &WlRegistry,
        event: <WlRegistry as wayland_client::Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            wayland_client::protocol::wl_registry::Event::Global {
                interface,
                name,
                version,
            } => {
                state
                    .interfaces
                    .insert(interface, RegisteredInterface { name, version });
            }
            wayland_client::protocol::wl_registry::Event::GlobalRemove { name } => {
                state.interfaces.retain(|_, v| v.name != name);
            }
            _ => (),
        }
    }
}
impl wayland_client::Dispatch<WlCompositor, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &WlCompositor,
        event: <WlCompositor as wayland_client::Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
    }
}
impl wayland_client::Dispatch<WlSurface, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &WlSurface,
        event: <WlSurface as wayland_client::Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            wayland_client::protocol::wl_surface::Event::PreferredBufferScale { factor } => {
                info!("Surface Preferred buffer scale: {factor}");
            }
            _ => (),
        }
    }
}
impl wayland_client::Dispatch<XdgWmBase, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &XdgWmBase,
        event: <XdgWmBase as wayland_client::Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            wayland_protocols::xdg::shell::client::xdg_wm_base::Event::Ping { serial } => {
                proxy.pong(serial);
            }
            _ => (),
        }
    }
}
impl wayland_client::Dispatch<XdgSurface, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &XdgSurface,
        event: <XdgSurface as wayland_client::Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            wayland_protocols::xdg::shell::client::xdg_surface::Event::Configure { serial } => {
                println!("xdg_surface configure");
                proxy.ack_configure(serial);
            }
            _ => (),
        }
    }
}
impl wayland_client::Dispatch<XdgToplevel, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &XdgToplevel,
        event: <XdgToplevel as wayland_client::Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            wayland_protocols::xdg::shell::client::xdg_toplevel::Event::Close => {
                info!("close surface");
            }
            _ => (),
        }
    }
}
impl wayland_client::Dispatch<ZxdgDecorationManagerV1, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &ZxdgDecorationManagerV1,
        event: <ZxdgDecorationManagerV1 as Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
    }
}
impl wayland_client::Dispatch<ZxdgToplevelDecorationV1, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &ZxdgToplevelDecorationV1,
        event: <ZxdgToplevelDecorationV1 as Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
    }
}
impl wayland_client::Dispatch<WpFractionalScaleManagerV1, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &WpFractionalScaleManagerV1,
        event: <WpFractionalScaleManagerV1 as Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
    }
}
impl wayland_client::Dispatch<WpFractionalScaleV1, ()> for CallbackHandler {
    fn event(
        state: &mut Self,
        proxy: &WpFractionalScaleV1,
        event: <WpFractionalScaleV1 as Proxy>::Event,
        data: &(),
        conn: &wayland_client::Connection,
        qhandle: &wayland_client::QueueHandle<Self>,
    ) {
        match event {
            wayland_protocols::wp::fractional_scale::v1::client::wp_fractional_scale_v1::Event::PreferredScale { scale } => {
                info!("Preferred Scale received: {scale} ({})", scale as f32 / 120.0);
            },
            _ => ()
        }
    }
}

async fn run() {
    let wl =
        wayland_client::Connection::connect_to_env().expect("Failed to connect to wayland server");
    let mut event_queue = wl.new_event_queue();
    let mut cb_handler = CallbackHandler {
        interfaces: HashMap::new(),
    };
    let registry = wl.display().get_registry(&event_queue.handle(), ());
    event_queue
        .roundtrip(&mut cb_handler)
        .expect("Failed to roundtrip");
    let compositor: WlCompositor = cb_handler
        .bind_interface(&registry, &event_queue.handle(), ())
        .expect("No copmositor found");
    let surface = compositor.create_surface(&event_queue.handle(), ());

    let whs = WindowHandler {
        display: wl.display(),
        surface: surface.clone(),
    };
    let nl = NativeLink {
        al: PlatformAssetLoader::new(),
        wh: whs,
    };
    let mut engine = peridot::Engine::new(
        userlib::APP_IDENTIFIER,
        userlib::APP_VERSION,
        nl,
        userlib::Game::<NativeLink>::requested_features(),
    );
    let mut usercode = userlib::Game::init(&mut engine);
    engine.postinit();

    let xdg_wm_base: XdgWmBase = cb_handler
        .bind_interface(&registry, &event_queue.handle(), ())
        .expect("No xdg extension found");
    let xdg_decoration_manager: ZxdgDecorationManagerV1 = cb_handler
        .bind_interface(&registry, &event_queue.handle(), ())
        .expect("No xdg decoration manager found");
    let xdg_surface = xdg_wm_base.get_xdg_surface(&surface, &event_queue.handle(), ());
    let xdg_toplevel = xdg_surface.get_toplevel(&event_queue.handle(), ());
    xdg_toplevel.set_app_id(String::from(userlib::APP_IDENTIFIER));
    xdg_toplevel.set_title(format!(
        "{} v{}.{}.{}",
        userlib::APP_TITLE,
        userlib::APP_VERSION.0,
        userlib::APP_VERSION.1,
        userlib::APP_VERSION.2
    ));
    let _xdg_toplevel_deco =
        xdg_decoration_manager.get_toplevel_decoration(&xdg_toplevel, &event_queue.handle(), ());
    surface.commit();

    let fractional_scale_manager: WpFractionalScaleManagerV1 = cb_handler
        .bind_interface(&registry, &event_queue.handle(), ())
        .expect("No Fractional Scale manager found");
    let _fractional_scale =
        fractional_scale_manager.get_fractional_scale(&surface, &event_queue.handle(), ());

    event_queue
        .roundtrip(&mut cb_handler)
        .expect("Failed to roundtrip");
    println!("starting wayland cradle");

    loop {
        event_queue
            .roundtrip(&mut cb_handler)
            .expect("Failed to roundtrip");
        engine.do_update(&mut usercode);
    }
}

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
    pub display: WlDisplay,
    pub surface: WlSurface,
}

pub struct Presenter {
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<peridot::InstanceObject>>,
}
impl Presenter {
    fn new(g: &peridot::Graphics, renderer_queue_family: u32, w: &WindowHandler) -> Self {
        if !g
            .adapter()
            .wayland_presentation_support(renderer_queue_family, w.display.id().as_ptr() as _)
        {
            panic!("Vulkan Presentation is not supported!");
        }
        let so = g
            .adapter()
            .new_surface_wayland(w.display.id().as_ptr() as _, w.surface.id().as_ptr() as _)
            .expect("Failed to create Surface object");
        if !g
            .adapter()
            .surface_support(renderer_queue_family, &so)
            .expect("Failed to query surface support")
        {
            panic!("Vulkan Surface is not supported");
        }
        let sc = peridot::IntegratedSwapchain::new(g, so, peridot::math::Vector2(640, 480));

        Presenter { sc }
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
        // self.x11_ref.borrow().mainwnd_geometry().clone()
        // TODO
        peridot::math::Vector2(640, 480)
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
        vec!["VK_KHR_surface", "VK_KHR_wayland_surface"]
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
