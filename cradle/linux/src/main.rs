use core::{future::Future, pin::Pin};
use input::PointerPositionProvider;
use parking_lot::RwLock;
use peridot::mthelper::{make_shared_mutable_ref, DynamicMutabilityProvider, SharedMutableRef};
use presenter::PresenterProvider;
use sound_backend::SoundBackend;
use std::{ffi::CStr, path::PathBuf, sync::Arc};
use std::{fs::File, os::fd::AsRawFd};
use std::{io::Result as IOResult, os::fd::RawFd};
use tracing_subscriber::{prelude::__tracing_subscriber_SubscriberExt, util::SubscriberInitExt};

mod sound_backend;

use crate::presenter::{wayland::Wayland, BorrowFd, EventProcessor, WindowBackend};
mod epoll;
mod input;
mod kernel_input;
mod presenter;
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

        tracing::trace!("Using Assets in {}", basedir.display());
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

        let mut apath = self.basedir.clone();
        apath.extend(path_segments);
        apath.set_extension(ext);

        File::open(apath)
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::Asset> {
        self.get(path, ext)
    }
}

pub struct NativeLink<PP: PresenterProvider> {
    al: PlatformAssetLoader,
    pp: PP,
}
impl<PP: PresenterProvider> peridot::NativeLinker for NativeLink<PP> {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = PP::Presenter;

    fn instance_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_surface", PP::SURFACE_EXT_NAME]
    }
    fn device_extensions(&self) -> Vec<&CStr> {
        vec![c"VK_KHR_swapchain"]
    }

    fn asset_loader(&self) -> &PlatformAssetLoader {
        &self.al
    }
    fn new_presenter(&self, g: &peridot::Graphics) -> Self::Presenter {
        self.pp.create(g)
    }
}

static USERCODE_WAKER_VTABLE: core::task::RawWakerVTable = core::task::RawWakerVTable::new(
    |ptr| core::task::RawWaker::new(ptr, &USERCODE_WAKER_VTABLE),
    |_| {},
    |_| {},
    |_| {},
);

pub struct GameDriver<MainF> {
    engine_input: peridot::InputProcess,
    engine_audio: Arc<RwLock<peridot::audio::Mixer>>,
    _snd: Box<dyn SoundBackend>,
    event_sender: async_std::channel::Sender<peridot::EngineEvent>,
    frame_timing_sender: async_std::channel::Sender<()>,
    event_queue: Pin<Box<peridot::EventQueue>>,
    usercode: Pin<Box<MainF>>,
    // self-referential struct
    _pinned: core::marker::PhantomPinned,
}
impl<MainF: Future> GameDriver<MainF> {
    fn new<PP>(
        pp: SharedMutableRef<PP>,
        usercode_launcher: impl FnOnce(
            peridot::Engine<'static, NativeLink<SharedMutableRef<PP>>>,
        ) -> MainF,
    ) -> Self
    where
        PP: PointerPositionProvider + Send + Sync + 'static,
        SharedMutableRef<PP>: PresenterProvider,
    {
        let (event_sender, event_receiver) = async_std::channel::unbounded();
        let (frame_timing_sender, frame_timing_receiver) = async_std::channel::bounded(1);

        let event_queue = Box::pin(peridot::EventQueue::new());
        let event_queue_lifetime_extended: &'static peridot::EventQueue =
            unsafe { &*(&*event_queue as *const _) };
        let mut engine = peridot::Engine::new(
            userlib::APP_IDENTIFIER,
            userlib::APP_VERSION,
            NativeLink {
                al: PlatformAssetLoader::new(),
                pp: pp.clone(),
            },
            unsafe { core::mem::MaybeUninit::zeroed().assume_init() },
            (event_sender.clone(), event_receiver),
            frame_timing_receiver,
            &event_queue_lifetime_extended,
        );
        engine
            .input()
            .set_nativelink(Box::new(input::InputNativeLink::new(pp)));
        engine.post_init();
        let _snd: Box<dyn SoundBackend> =
            if sound_backend::pipewire::NativeAudioEngine::is_available() {
                Box::new(sound_backend::pipewire::NativeAudioEngine::new(
                    engine.audio_mixer(),
                ))
            } else {
                // fallback
                Box::new(sound_backend::pa::NativeAudioEngine::new(
                    engine.audio_mixer(),
                ))
            };

        let engine_input = engine.input().clone();
        let engine_audio = engine.audio_mixer().clone();
        let usercode = Box::pin(usercode_launcher(engine));

        Self {
            engine_input,
            engine_audio,
            _snd,
            event_sender,
            frame_timing_sender,
            event_queue,
            usercode,
            _pinned: core::marker::PhantomPinned,
        }
    }

    /// returns true if usercode coroutine has done
    pub fn step(&mut self) -> bool {
        let usercode_waker =
            unsafe { core::task::Waker::new(core::ptr::null(), &USERCODE_WAKER_VTABLE) };

        self.usercode
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&usercode_waker))
            .is_ready()
    }
}

pub struct EpollTemporaryAddFd<'e> {
    instance: &'e epoll::Epoll,
    fd: RawFd,
}
impl<'e> EpollTemporaryAddFd<'e> {
    pub fn add(
        instance: &'e epoll::Epoll,
        fd: RawFd,
        events: u32,
        extras: u64,
    ) -> std::io::Result<Self> {
        instance.add_fd(fd, events, extras)?;
        Ok(Self { instance, fd })
    }
}
impl Drop for EpollTemporaryAddFd<'_> {
    fn drop(&mut self) {
        self.instance
            .remove_fd(self.fd)
            .expect("Failed to remove fd from epoll instance");
    }
}

fn run_with_window_backend<W>(window_backend: SharedMutableRef<W>)
where
    W: WindowBackend + EventProcessor + PointerPositionProvider + Send + Sync + 'static,
    SharedMutableRef<W>: PresenterProvider,
{
    let mut gd = GameDriver::new(window_backend.clone(), |mut engine| async move {
        userlib::game_main(&mut engine).await;
    });

    let ep = epoll::Epoll::new().expect("Failed to create epoll interface");
    let mut input = input::InputSystem::new(&ep, 1, 2);

    window_backend.borrow_mut().show();
    gd.engine_audio.write().start();
    let mut events = Vec::new();
    let mut last_drawn_geometry = window_backend.borrow().geometry();
    while !window_backend.borrow().has_close_requested() {
        // step usercode before wait
        gd.step();

        if events.len() != 2 + input.managed_devices_count() {
            // resize
            events.resize(2 + input.managed_devices_count(), unsafe {
                std::mem::MaybeUninit::zeroed().assume_init()
            });
        }

        let window_backend_readiness_guard = window_backend.borrow_mut().readiness_guard();
        let window_backend_temporary_epoll = EpollTemporaryAddFd::add(
            &ep,
            window_backend_readiness_guard.borrow_fd().as_raw_fd(),
            libc::EPOLLIN as _,
            0,
        );

        let count = ep
            .wait(&mut events, Some(1))
            .expect("Failed to waiting epoll");
        drop(window_backend_temporary_epoll);

        // TODO: あとでちゃんと待つ(external_fence_fdでは待てなさそうなので、監視スレッド立てるかしかないか......)
        if count == 0 {
            window_backend.borrow_mut().cancel_read();
            drop(window_backend_readiness_guard);
            let current_geometry = window_backend.borrow().geometry();
            if last_drawn_geometry != current_geometry {
                last_drawn_geometry = current_geometry;
                gd.event_queue
                    .enqueue(peridot::Event::Resize(last_drawn_geometry));
            }

            gd.event_queue.enqueue(peridot::Event::NextFrame);
            continue;
        }

        let mut rg = Some(window_backend_readiness_guard);
        for e in &events[..count as usize] {
            if e.u64 == 0 {
                window_backend
                    .borrow_mut()
                    .process_all_events(rg.take().expect("window events signaled twice"));
            } else if e.u64 == 1 {
                input.process_monitor_event(&ep);
            } else {
                let mut input_lock = gd.engine_input.state_write_lock();
                input.process_device_event(
                    &mut input_lock.make_event_receiver(),
                    e.u64,
                    &*window_backend.borrow(),
                );
            }
        }
        if rg.is_some() {
            // no window server events processed
            window_backend.borrow_mut().cancel_read();
        }
    }

    gd.event_queue.enqueue(peridot::Event::Shutdown);
    while !gd.step() {}

    gd.engine_audio.write().stop();
    tracing::trace!("Terminating Program...");
}

fn main() {
    let fmt = tracing_subscriber::fmt::layer().pretty();
    let env_filter = tracing_subscriber::filter::EnvFilter::from_default_env();
    tracing_subscriber::registry()
        .with(fmt)
        .with(env_filter)
        .init();

    if let Ok(backend_name) = std::env::var("PERIDOT_PREFERRED_WINDOW_BACKEND") {
        if backend_name == "wayland" {
            run_with_window_backend(make_shared_mutable_ref(
                Wayland::try_init().expect("Failed to initialize wayland backend"),
            ));
            return;
        }
        #[cfg(feature = "support-xcb")]
        if backend_name == "xcb" {
            run_with_window_backend(make_shared_mutable_ref(
                presenter::xcb::X11::try_init().expect("Failed to initialize xcb backend"),
            ));
            return;
        }

        tracing::warn!(
            { backend = backend_name },
            "unknown backend specified, ignoring"
        );
    }

    if let Some(x) = Wayland::try_init() {
        run_with_window_backend(make_shared_mutable_ref(x));
        return;
    }
    #[cfg(feature = "support-xcb")]
    if let Some(x) = presenter::xcb::X11::try_init() {
        run_with_window_backend(make_shared_mutable_ref(x));
        return;
    }

    panic!("No suitable window backend");
}
