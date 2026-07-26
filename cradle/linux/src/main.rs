use core::{future::Future, pin::Pin};
use input::PointerPositionProvider;
use linux_epoll::{Epoll, EpollEventBits};
use linux_eventfd::{EventFD, EventFDFlags};
use parking_lot::RwLock;
use peridot::mthelper::{make_shared_mutable_ref, DynamicMutabilityProvider, SharedMutableRef};
use presenter::PresenterProvider;
use sound_backend::SoundBackend;
use std::os::fd::AsRawFd;
use std::{ffi::CStr, path::PathBuf, sync::Arc};
use std::{io::Result as IOResult, os::fd::RawFd};
use tracing_subscriber::{prelude::__tracing_subscriber_SubscriberExt, util::SubscriberInitExt};

mod sound_backend;

use crate::presenter::{wayland::Wayland, BorrowFd, EventProcessor, WindowBackend};
mod input;
mod presenter;
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

    #[tracing::instrument(skip(self), ret(level = tracing::Level::DEBUG))]
    fn resolve_asset_realpath(&self, path: &str, ext: &str) -> PathBuf {
        #[allow(unused_mut)]
        let mut path_segments = path.split('.').peekable();

        #[cfg(feature = "IterationBuild")]
        if path_segments.peek().map_or(false, |&s| s == "builtin") {
            let _ = path_segments.next();

            let mut p = self.builtin_asset_basedir.clone();
            p.extend(path_segments);
            p.set_extension(ext);
            return p;
        }

        let mut apath = self.basedir.clone();
        apath.extend(path_segments);
        apath.set_extension(ext);
        apath
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type AssetBlob<'a> = peridot::native_io::linux::NativeFileBlobRandomReader;
    type AssetBlobAsync<'a> = peridot::native_io::linux::NativeFileAsyncBlobRandomReader;
    type StreamingAsset<'a> = peridot::native_io::RandomBlobReadSeekAdapter<
        peridot::native_io::linux::NativeFileBlobRandomReader,
    >;

    fn get<'a>(&'a self, path: &str, ext: &str) -> IOResult<Self::AssetBlob<'a>> {
        peridot::native_io::linux::NativeFileBlobRandomReader::open(
            self.resolve_asset_realpath(path, ext),
        )
    }

    fn get_async<'a>(
        &'a self,
        path: &str,
        ext: &str,
    ) -> impl core::future::Future<Output = IOResult<Self::AssetBlobAsync<'a>>> {
        async move {
            peridot::native_io::linux::NativeFileAsyncBlobRandomReader::open(
                self.resolve_asset_realpath(path, ext),
            )
        }
    }

    fn get_streaming<'a>(&'a self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset<'a>> {
        self.get(path, ext)
            .map(peridot::native_io::RandomBlobReadSeekAdapter::new)
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
    |ptr| unsafe {
        if let Err(e) = (*ptr.cast::<EventFD>()).inc(1) {
            tracing::warn!(reason = ?e, "usercode wake failed");
        }
    },
    |ptr| unsafe {
        if let Err(e) = (*ptr.cast::<EventFD>()).inc(1) {
            tracing::warn!(reason = ?e, "usercode wake failed");
        }
    },
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
    usercode_event: Pin<Box<EventFD>>,
    // self-referential struct
    _pinned: core::marker::PhantomPinned,
}
impl<MainF: Future> GameDriver<MainF> {
    fn new<PP>(
        pp: SharedMutableRef<PP>,
        epoll: &Epoll,
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
            if let Some(init) = sound_backend::pipewire::NativeAudioEngine::try_init() {
                Box::new(sound_backend::pipewire::NativeAudioEngine::new(
                    init,
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
        let usercode_event =
            Box::pin(EventFD::new(0, EventFDFlags::NONBLOCK).expect("EventFD::new"));
        if let Err(e) = epoll.add(
            usercode_event.as_ref().get_ref(),
            EpollEventBits::IN,
            USERCODE_FUTURE_WAKE_EVENT_ID,
        ) {
            tracing::warn!(reason = ?e, "ep.add usercode_event failed");
        }

        Self {
            engine_input,
            engine_audio,
            _snd,
            event_sender,
            frame_timing_sender,
            event_queue,
            usercode,
            usercode_event,
            _pinned: core::marker::PhantomPinned,
        }
    }

    /// returns true if usercode coroutine has done
    pub fn step(&mut self) -> bool {
        let usercode_waker = unsafe {
            core::task::Waker::new(
                self.usercode_event.as_ref().get_ref() as *const _ as _,
                &USERCODE_WAKER_VTABLE,
            )
        };

        self.usercode
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&usercode_waker))
            .is_ready()
    }
}

pub struct EpollTemporaryAddFd<'e> {
    instance: &'e Epoll,
    fd: RawFd,
}
impl<'e> EpollTemporaryAddFd<'e> {
    pub fn add(
        instance: &'e Epoll,
        fd: &(impl AsRawFd + ?Sized),
        events: EpollEventBits,
        extras: u64,
    ) -> std::io::Result<Self> {
        instance.add(fd, events, extras)?;
        Ok(Self {
            instance,
            fd: fd.as_raw_fd(),
        })
    }
}
impl Drop for EpollTemporaryAddFd<'_> {
    fn drop(&mut self) {
        self.instance
            .del(&self.fd)
            .expect("Failed to remove fd from epoll instance");
    }
}

const UDEV_MONITOR_EVENT_ID: u64 = 2;
const UDEV_DEVICE_EVENT_ID_START: u64 = 3;
const FIXED_EVENT_COUNT: usize = 3;
const WINDOW_BACKEND_EVENT_ID: u64 = 0;
const USERCODE_FUTURE_WAKE_EVENT_ID: u64 = 1;

fn run_with_window_backend<W>(window_backend: SharedMutableRef<W>)
where
    W: WindowBackend + EventProcessor + PointerPositionProvider + Send + Sync + 'static,
    SharedMutableRef<W>: PresenterProvider,
{
    let ep = Epoll::new(0).expect("Failed to create epoll interface");
    let mut input = input::InputSystem::new(&ep, UDEV_MONITOR_EVENT_ID, UDEV_DEVICE_EVENT_ID_START);

    let mut gd = GameDriver::new(window_backend.clone(), &ep, |mut engine| async move {
        userlib::game_main(&mut engine).await;
    });

    window_backend.borrow_mut().show();
    gd.engine_audio.write().start();

    // initial uesrcode step
    gd.step();

    let mut events = Vec::with_capacity(8);
    let mut last_drawn_geometry = window_backend.borrow().geometry();
    'app: while !window_backend.borrow().has_close_requested() {
        if events.capacity() > (FIXED_EVENT_COUNT + input.managed_devices_count() * 2).max(8) {
            // reduce memory consumption
            events = Vec::with_capacity((FIXED_EVENT_COUNT + input.managed_devices_count()).max(8));
        } else {
            // resize capacity
            events.reserve(FIXED_EVENT_COUNT + input.managed_devices_count());
        }

        let window_backend_readiness_guard = window_backend.borrow_mut().readiness_guard();
        let window_backend_temporary_epoll = EpollTemporaryAddFd::add(
            &ep,
            &window_backend_readiness_guard.borrow_fd(),
            EpollEventBits::IN,
            WINDOW_BACKEND_EVENT_ID,
        );

        let count = match ep.wait(events.spare_capacity_mut(), Some(1)) {
            Ok(x) => x,
            Err(e) if e.kind() == std::io::ErrorKind::Interrupted => 0,
            Err(e) => {
                tracing::error!(reason = ?e, "epoll_wait failed");
                break;
            }
        };
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

        let mut has_window_events = false;
        let mut has_usercode_wakes = false;
        for e in unsafe { core::slice::from_raw_parts(events.as_ptr(), count as _) } {
            if e.value() == WINDOW_BACKEND_EVENT_ID {
                has_window_events = true;
            } else if e.value() == USERCODE_FUTURE_WAKE_EVENT_ID {
                has_usercode_wakes = true;
            } else if e.value() == UDEV_MONITOR_EVENT_ID {
                input.process_monitor_event(&ep);
            } else {
                let mut input_lock = gd.engine_input.state_write_lock();
                input.process_device_event(
                    &mut input_lock.make_event_receiver(),
                    e.value(),
                    &*window_backend.borrow(),
                );
            }
        }

        if has_window_events {
            window_backend
                .borrow_mut()
                .process_all_events(window_backend_readiness_guard);
        } else {
            // no window server events processed
            // Note: これ先にキャンセルしてあげないとVulkan WSI関係の呼び出しでとまる
            window_backend.borrow_mut().cancel_read();
        }

        if has_usercode_wakes {
            if let Err(e) = gd.usercode_event.take() {
                tracing::warn!(reason = ?e, "usercode_event.take failed");
            }

            if gd.step() {
                break 'app;
            }
        }
    }

    gd.event_queue.enqueue(peridot::Event::Shutdown);
    while !gd.step() {}

    gd.engine_audio.write().stop();
    tracing::trace!("Terminating Program...");
}

fn main() {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().pretty())
        .with(tracing_subscriber::filter::EnvFilter::from_default_env())
        .init();

    let io_reactor_thread = peridot::native_io::linux::IoReactorThread::spawn();

    if let Some(backend_name) = std::env::var_os("PERIDOT_PREFERRED_WINDOW_BACKEND") {
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

        tracing::warn!(backend = ?backend_name, "unknown backend specified, ignoring");
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

    drop(io_reactor_thread);
    panic!("No suitable window backend");
}
