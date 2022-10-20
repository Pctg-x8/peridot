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
use std::path::PathBuf;
use ws::{InputSystemBackend, VulkanPresentable};

mod sound_backend;
use sound_backend::NativeAudioEngine;

use crate::ws::WindowSystemBackend;
mod epoll;
mod input;
mod kernel_input;
mod udev;
mod userlib;
mod ws;

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

pub struct Presenter<WindowSystem: WindowSystemBackend> {
    x11_ref: SharedRef<DynamicMut<WindowSystem>>,
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<peridot::InstanceObject>>,
}
impl<WindowSystem: WindowSystemBackend + VulkanPresentable> Presenter<WindowSystem> {
    fn new(
        g: &peridot::Graphics,
        renderer_queue_family: u32,
        w: &SharedRef<DynamicMut<WindowSystem>>,
    ) -> Self {
        if !w
            .borrow()
            .presentation_support(g.adapter(), renderer_queue_family)
        {
            panic!("Vulkan Presentation is not supported!");
        }
        let so = w
            .borrow()
            .create_surface(g.adapter())
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
            x11_ref: w.clone(),
        }
    }
}
impl<WindowBackend: WindowSystemBackend> peridot::PlatformPresenter for Presenter<WindowBackend> {
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
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + ?Sized>,
    ) {
        self.sc.emit_initialize_backbuffer_commands(recorder)
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_backbuffer_index()
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::Fence,
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

pub struct NativeLink<WindowSystem: WindowSystemBackend + VulkanPresentable> {
    al: PlatformAssetLoader,
    ws: SharedRef<DynamicMut<WindowSystem>>,
}
impl<WindowSystem: WindowSystemBackend + VulkanPresentable> peridot::NativeLinker
    for NativeLink<WindowSystem>
{
    type AssetLoader = PlatformAssetLoader;
    type Presenter = Presenter<WindowSystem>;

    fn intercept_instance_builder(&self, builder: &mut br::InstanceBuilder) {
        builder.add_extensions(vec![
            "VK_KHR_surface",
            WindowSystem::REQUIRED_INSTANCE_EXTENSION,
        ]);
    }
    fn intercept_device_builder(&self, builder: &mut br::DeviceBuilder<impl br::PhysicalDevice>) {
        builder.add_extensions(vec!["VK_KHR_swapchain"]);
    }

    fn asset_loader(&self) -> &PlatformAssetLoader {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Self::Presenter {
        Presenter::new(g, g.graphics_queue_family_index(), &self.ws)
    }
}

pub struct GameDriver<
    WindowSystem: WindowSystemBackend + VulkanPresentable + InputSystemBackend + Send + Sync + 'static,
> {
    engine: peridot::Engine<NativeLink<WindowSystem>>,
    usercode: userlib::Game<NativeLink<WindowSystem>>,
    _snd: NativeAudioEngine,
}
impl<
        WindowSystem: WindowSystemBackend + VulkanPresentable + InputSystemBackend + Send + Sync + 'static,
    > GameDriver<WindowSystem>
{
    fn new(ws: &SharedRef<DynamicMut<WindowSystem>>) -> Self {
        let nl = NativeLink {
            al: PlatformAssetLoader::new(),
            ws: ws.clone(),
        };
        let mut engine = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            nl,
            userlib::Game::<NativeLink<WindowSystem>>::requested_features(),
        );
        let usercode = userlib::Game::init(&mut engine);
        engine
            .input_mut()
            .set_nativelink(Box::new(input::InputNativeLink::new(ws)));
        engine.postinit();
        let _snd = NativeAudioEngine::new(engine.audio_mixer());

        Self {
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
    let x11 = SharedRef::new(DynamicMut::new(ws::xlib::WindowSystem::init()));

    let mut gd = GameDriver::new(&x11);

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
                    &*x11.borrow(),
                );
            }
        }
    }
    info!("Terminating Program...");
}
