extern crate peridot_marble_editor_model as model;
extern crate peridot_marble_editor_shared as shared;

use bitflags::Flags;
use core::{cell::Cell, pin::Pin};
#[cfg(target_os = "linux")]
use linux_epoll::{Epoll, EpollEventBits};
#[cfg(feature = "wayland")]
use linux_eventfd::{EventFD, EventFDFlags};
use model::{Application, ApplicationMutation, ObjectID, ObjectRenderShape, PreviewEditToolType};
use peridot_math::{One, Zero};
#[cfg(target_os = "linux")]
use peridot_tp_dbus as dbus;
use shared::{Color32, LogicalUnit, NonDropAnyTypeQueue, Point, Rect, Size};
#[cfg(target_os = "linux")]
use std::os::fd::AsRawFd;
#[cfg(not(windows))]
#[cfg(target_os = "linux")]
use std::sync::Arc;
use std::{
    cell::RefCell,
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    path::{Path, PathBuf},
    rc::Rc,
    sync::Mutex,
};
#[cfg(target_os = "macos")]
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use crate::{
    graphics::Graphics,
    input::{
        EventContinueControl, FocusTargetToken, InputEventContext, KeyInputCode,
        KeyInputEventHandler, KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, ModifierKey,
        NativeDesktopSurface, PointerInputManager, PointerInputUnit,
        hittest::{
            CursorShape, HitTestArgs, HitTestTreeActionHandler, HitTestTreeData,
            HitTestTreeManager, HitTestTreeRef, PointerActionArgs, PointerButton,
            PointerButtonActionArgs,
        },
    },
    persistence::{DockState, PersistStateWindowData, WindowGeometryState, WindowState},
    rendering::{
        MainThreadTextureIDIssuer, RenderMessage, RenderMessageSender, RenderThread, RendererSync,
        ShaderTexture, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTexture,
            CompositeTree, CompositeTreeRef, CompositeTreeSyncBuffer, CornerRadius, Gradient,
            GradientRef, TextureMappingMode, TextureType,
        },
        preview::HandlePointing,
        text::{FontID, FontSet, RootFontSet},
    },
    ui::dock::{PaneContentResizeContext, PaneGroupCreateContext},
    uicore::{
        MeasureContext, MountContext, MountTarget, PopupID, PopupManager, RenderContext,
        TeardownContext, TypedViewIdentifier, View, ViewDestructionContext, ViewFeedbackContext,
        ViewFeedbackHandler, ViewFeedbackRegisterable, ViewFeedbackRegistry,
        ViewFeedbackRegistryDelayedOps, ViewGroupID, ViewGroupRegisterable,
        ViewGroupRelationControllable, ViewGroupRelationStore, ViewIdentifier,
        ViewIdentifierAllocator, ViewImmediateRenderable, ViewInitContext,
        ViewInstanceQueryableMut, ViewInstanceStore, ViewLayoutChild, ViewLayoutFlowAlignment,
        ViewLayoutFlowDirection, ViewLayoutFlowJustify, ViewLayoutGridCell, ViewLayoutOverflow,
        ViewLayoutStateStore, ViewRegisterable, ViewRelationControllable, ViewRenderElements,
        ViewRenderQueue, ViewRenderStateStore, ViewRenderer, ViewSize, ViewTreeRelationStore,
    },
    uikit::{
        ContainerView, ContainerViewInit, MenuCommandSelectionHandler, MenuEventHandler, MenuItem,
        MenuItemCommonResources, MenuItemInteractableElement, NumericInputViewIO,
        NumericInputViewInit, RadioButtonView, ScrollContainer, ScrollContainerInit,
        SimpleButtonEventHandler, SimpleButtonViewInit, StaticTextViewInit, TextInputView,
        TextInputViewIO,
    },
    utils::{DummyDebug, NonCloneable, UnsafeMainThreadOnlyOnceCell},
};

#[cfg(windows)]
mod bindgen;
mod graphics;
mod input;
mod persistence;
mod platform;
mod proto;
mod rendering;
mod ui;
mod uicore;
mod uikit;
mod utils;

static APP_WAKER_VTABLE: core::task::RawWakerVTable = core::task::RawWakerVTable::new(
    |data| core::task::RawWaker::new(data, &APP_WAKER_VTABLE),
    |_| {},
    |_| {},
    |_| {},
);

pub fn launch() {
    #[cfg(windows)]
    utils::platform::windows::set_panic_hook();

    #[cfg(target_os = "macos")]
    tracing_subscriber::registry()
        .with(utils::platform::mac::LogLayer)
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    #[cfg(windows)]
    tracing_subscriber::fmt()
        .pretty()
        .with_ansi(false)
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(utils::platform::windows::DebugOutputWriter)
        .init();
    #[cfg(all(not(target_os = "macos"), not(windows)))]
    tracing_subscriber::fmt()
        .pretty()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_thread_names(true)
        .init();

    profiler::init_profiler();

    let mut event_store = VecDeque::new();
    let (rt_sender, rt_receiver) = std::sync::mpsc::channel::<RenderMessage>();
    let fs = FileSystem::new();

    #[cfg(windows)]
    let mut app_context = platform::windows::ApplicationContext::new();
    #[cfg(windows)]
    let dx_context = platform::windows::DxContext::new();

    #[cfg(feature = "wayland")]
    let mut dp_context = platform::unix::wayland::DisplayServerContext::connect();
    #[cfg(feature = "wayland")]
    let static_pixbufs = platform::unix::wayland::StaticPixbufs::new(&dp_context);

    #[cfg(target_os = "linux")]
    let dbus = dbus::Connection::connect_bus(dbus::BusType::Session).expect("dbus.connect");

    let root_font_set = RootFontSet::new();
    let gfx = Graphics::init(&fs);
    #[cfg(windows)]
    assert!(
        gfx.presentation_support(),
        "win32 presentation not supported on graphics queue"
    );
    #[cfg(feature = "wayland")]
    assert!(
        dp_context.check_for_vk(&gfx),
        "wayland presentation not supported on graphics queue"
    );

    let mut main_camera = peridot_math::Camera {
        position: peridot_math::Vector3(1.0, 1.0, -5.0),
        rotation: peridot_math::Quaternion::ONE,
        projection: Some(peridot_math::ProjectionMethod::Physical {
            focal_length: 30.0,
            sensor_size: peridot_math::Vector2(35.0, 24.0),
            screen_fitting: peridot_math::PhysicalScreenFitting::Shrink,
            lens_shift: peridot_math::Vector2(0.0, 0.0),
        }),
        depth_range: 0.1..1000.0,
    };
    main_camera.look_at(
        peridot_math::Vector3::ZERO,
        Some(peridot_math::Vector3::up()),
    );

    let renderer_sync = Mutex::new(RendererSync {
        composite_buffer: CompositeTreeSyncBuffer::new(),
    });
    let preview_state = Mutex::new(rendering::preview::CommittedState {
        viewport_size: Size::new_logical(640.0, 480.0),
        main_camera,
        main_camera_dirtified: false,
        pushed_meshes: Vec::new(),
        dirty_meshes: HashMap::new(),
        removed_meshes: HashSet::new(),
        pushed_render_data: Vec::new(),
        dirty_render_data: HashMap::new(),
        removed_render_data: HashSet::new(),
        handle_shape: None,
        handle_pointing: None,
        handle_to_world_transform: peridot_math::Matrix4::ONE,
        handle_data_dirtified: false,
    });

    let global_time_base = std::time::Instant::now();

    #[cfg(feature = "wayland")]
    let terminate_event = std::sync::Arc::new(
        EventFD::new(0, EventFDFlags::empty()).expect("terminate_event.create"),
    );

    let mut polling = false;
    let empty_dispatcher = LogicFiberEventDispatcher {
        event_store: &mut event_store,
        polling: &mut polling,
        #[allow(invalid_value)]
        poll_fn_ptr: unsafe { core::mem::MaybeUninit::uninit().assume_init() },
        future_ptr: core::ptr::null_mut(),
    };
    let mut app_event_dispatcher = core::pin::pin!(empty_dispatcher.clone());

    #[cfg(feature = "wayland")]
    let mut wl_global_msg = core::pin::pin!(platform::unix::wayland::GlobalMessaging::new(
        &mut dp_context,
        &static_pixbufs,
    ));
    #[cfg(feature = "wayland")]
    dp_context.bind_global_messaging(wl_global_msg.as_mut());

    #[cfg(feature = "enable-profiling")]
    #[cfg(target_os = "linux")]
    let memory_sample_timer_fd = utils::platform::linux::TimerFD::new().expect("timerfd.new");
    #[cfg(feature = "enable-profiling")]
    #[cfg(target_os = "linux")]
    memory_sample_timer_fd
        .set_interval(0, 50_000_000)
        .expect("timerfd.set");

    #[cfg(windows)]
    #[cfg(feature = "enable-profiling")]
    let memory_sample_timer =
        utils::platform::windows::WaitableTimer::new(false).expect("memory_sample_timer.create");
    #[cfg(windows)]
    #[cfg(feature = "enable-profiling")]
    {
        memory_sample_timer
            .set_interval_relative(50)
            .expect("memory_sample_timer.set_interval_relative");
    }

    #[cfg(windows)]
    let pointer_hovering_timer =
        utils::platform::windows::WaitableTimer::new(false).expect("pointer_hovering_timer.create");
    #[cfg(windows)]
    let context_menu_delayed_action_timer = utils::platform::windows::WaitableTimer::new(false)
        .expect("context_menu_delayed_action_timer.create");
    #[cfg(target_os = "linux")]
    let pointer_hovering_timer = utils::platform::linux::TimerFD::new().expect("timerfd.new");
    #[cfg(feature = "wayland")]
    let delayed_action_timer = utils::platform::linux::TimerFD::new().expect("timerfd.create");
    #[cfg(feature = "wayland")]
    let delayed_action_timer_fd = std::os::unix::prelude::AsRawFd::as_raw_fd(&delayed_action_timer);

    let coreloop = core::pin::pin!(CoreLoop::new(
        #[cfg(windows)]
        SystemLink {
            font_set: FontSet::new(root_font_set),
            rt_sender: rt_sender.clone(),
            gfx,
            event_dispatcher: app_event_dispatcher.as_mut().get_mut(),
            app_context,
            pointer_hovering_timer_handle: pointer_hovering_timer.as_handle(),
            flyout_surface_context: platform::windows::flyout_surface::SharedState::new(
                app_context,
                &dx_context,
                &context_menu_delayed_action_timer,
            ),
        },
        #[cfg(not(windows))]
        SystemLink {
            rt_sender: rt_sender.clone(),
            gfx: &gfx,
            font_set: FontSet::new(&root_font_set),
            event_dispatcher: app_event_dispatcher.as_mut().get_mut(),
            #[cfg(target_os = "linux")]
            dbus: &dbus,
            #[cfg(feature = "wayland")]
            display_server: platform::unix::wayland::DisplayServerLink {
                context: &mut dp_context,
                static_pixbufs: &static_pixbufs,
                global_messaging_ptr: unsafe { wl_global_msg.as_mut().get_unchecked_mut() },
            },
            #[cfg(target_os = "linux")]
            terminate_event: terminate_event.clone(),
            #[cfg(target_os = "linux")]
            pointer_hovering_timer: &pointer_hovering_timer,
            #[cfg(feature = "wayland")]
            flyout_surface_context: platform::unix::wayland::flyout_surface::SharedState {
                delayed_action_timer,
            },
            #[cfg(target_os = "macos")]
            flyout_surface_context: platform::mac::flyout_surface::SharedState {
                event_dispatcher: app_event_dispatcher.as_mut().get_mut()
            },
        },
        &fs,
        &global_time_base,
        &renderer_sync,
        &preview_state,
    ));

    profiler::sample_memory!();
    main_wrapper(
        move |cl, eq| run(cl, eq),
        &mut app_event_dispatcher,
        coreloop,
        &mut event_store,
        &global_time_base,
        &renderer_sync,
        &gfx,
        rt_receiver,
        &root_font_set,
        &preview_state,
        #[cfg(windows)]
        &mut app_context,
        #[cfg(windows)]
        &dx_context,
        #[cfg(feature = "wayland")]
        &mut dp_context,
        #[cfg(feature = "wayland")]
        wl_global_msg,
        #[cfg(feature = "wayland")]
        &terminate_event,
        #[cfg(target_os = "linux")]
        &dbus,
        #[cfg(target_os = "linux")]
        &pointer_hovering_timer,
        #[cfg(target_os = "linux")]
        &delayed_action_timer_fd,
        #[cfg(target_os = "linux")]
        #[cfg(feature = "enable-profiling")]
        &memory_sample_timer_fd,
    );

    profiler::fini_profiler();
}

fn main_wrapper<'sys, AppFuture: core::future::Future<Output = ()> + 'sys>(
    run_app: impl FnOnce(Pin<&'sys mut CoreLoop<'static, 'sys>>, EventQueue) -> AppFuture,
    app_event_dispatcher: &mut LogicFiberEventDispatcher,
    mut coreloop: Pin<&'sys mut CoreLoop<'static, 'sys>>,
    event_store: &mut VecDeque<Event>,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
    gfx: &'sys Graphics,
    rt_receiver: std::sync::mpsc::Receiver<RenderMessage>,
    root_font_set: &'sys RootFontSet,
    preview_state: &'sys Mutex<rendering::preview::CommittedState>,
    #[cfg(windows)] app_context: &'sys mut platform::windows::ApplicationContext,
    #[cfg(windows)] dx_context: &'sys platform::windows::DxContext,
    #[cfg(feature = "wayland")] dp_context: &'sys mut platform::unix::wayland::DisplayServerContext,
    #[cfg(feature = "wayland")] mut wl_global_msg: Pin<
        &'sys mut platform::unix::wayland::GlobalMessaging<'sys>,
    >,
    #[cfg(feature = "wayland")] terminate_event: &(impl AsRawFd + ?Sized),
    #[cfg(target_os = "linux")] dbus: &'sys dbus::Connection,
    #[cfg(target_os = "linux")] pointer_hovering_timer: &(impl AsRawFd + ?Sized),
    #[cfg(target_os = "linux")] delayed_action_timer_fd: &(impl AsRawFd + ?Sized),
    #[cfg(target_os = "linux")]
    #[cfg(feature = "enable-profiling")]
    memory_sample_timer_fd: &(impl AsRawFd + ?Sized),
) {
    let cl_ptr = core::ptr::from_mut(unsafe { coreloop.as_mut().get_unchecked_mut() });
    let mut app = core::pin::pin!(run_app(
        unsafe { Pin::new_unchecked(&mut *cl_ptr) },
        EventQueue { event_store }
    ));
    app_event_dispatcher.future_ptr = unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ };
    app_event_dispatcher.poll_fn_ptr =
        unsafe { core::mem::transmute(AppFuture::poll as *const core::ffi::c_void) };
    #[cfg(feature = "wayland")]
    wl_global_msg.as_mut().bind_coreloop(coreloop.as_mut());

    app_event_dispatcher.poll_init();
    unsafe { Pin::new_unchecked(&mut *cl_ptr) }.init();

    let sync_event_bus = SyncEventBus::new(app_event_dispatcher.clone());
    let shutdown = std::sync::atomic::AtomicBool::new(false);
    std::thread::scope(|thread_scope| {
        let render_thread = RenderThread {
            gfx,
            shutdown_signal: &shutdown,
            renderer_sync,
            global_time_base,
            event_bus: &sync_event_bus,
            message_receiver: rt_receiver,
            root_font_set,
            preview_state,
            #[cfg(windows)]
            dx_context,
            #[cfg(windows)]
            d3d12_present_counter: 0,
        };
        let render_thread = std::thread::Builder::new()
            .name("Render".into())
            .spawn_scoped(thread_scope, || render_thread.run())
            .expect("render_thread spawn");

        profiler::sample_memory!();

        #[cfg(target_os = "linux")]
        let epoll = Epoll::new(0).expect("epoll.new");
        #[cfg(feature = "wayland")]
        epoll
            .add(&dp_context.display_fd(), EpollEventBits::IN, 0)
            .expect("epoll.add");
        #[cfg(feature = "wayland")]
        epoll
            .add(terminate_event, EpollEventBits::IN, 1)
            .expect("epoll.add");
        #[cfg(feature = "wayland")]
        epoll
            .add(&sync_event_bus.efd, EpollEventBits::IN, 2)
            .expect("epoll.add");
        #[cfg(target_os = "linux")]
        epoll
            .add(pointer_hovering_timer, EpollEventBits::IN, 3)
            .expect("epoll.add");
        #[cfg(feature = "wayland")]
        epoll
            .add(delayed_action_timer_fd, EpollEventBits::IN, 4)
            .expect("epoll.add");
        #[cfg(feature = "wayland")]
        #[cfg(feature = "enable-profiling")]
        epoll
            .add(memory_sample_timer_fd, EpollEventBits::IN, 5)
            .expect("epoll.add");
        #[cfg(target_os = "linux")]
        let poll_id_to_watch_ref = core::cell::UnsafeCell::new(std::collections::HashMap::new());
        #[cfg(target_os = "linux")]
        dbus.set_watch_functions(Box::new(DBusWatcher {
            epoll: &epoll,
            last_poll_id: 100,
            fd_to_poll_id: std::collections::HashMap::new(),
            poll_id_to_watch_ref: &poll_id_to_watch_ref,
        }));
        #[cfg(target_os = "linux")]
        let mut eventbuf = [const { core::mem::MaybeUninit::uninit() }; 8];
        #[cfg(target_os = "linux")]
        'app: loop {
            #[cfg(feature = "wayland")]
            if dp_context.prepare_read().is_err() {
                break 'app;
            }
            let active_events = epoll.wait(&mut eventbuf, None).expect("epoll.wait");

            let mut wl_display_signal = false;
            let mut terminate_signal = false;
            let mut dbus_signal = false;
            let mut events_signal = false;
            let mut pointer_hovering_timer_signal = false;
            let mut delayed_action_timer_signal = false;
            for n in 0..active_events {
                let e = unsafe { eventbuf[n as usize].assume_init_ref() };
                if e.value() == 0 {
                    wl_display_signal = true;
                    continue;
                }
                if e.value() == 1 {
                    terminate_signal = true;
                    continue;
                }
                if e.value() == 2 {
                    events_signal = true;
                    continue;
                }
                if e.value() == 3 {
                    pointer_hovering_timer_signal = true;
                    continue;
                }
                if e.value() == 4 {
                    delayed_action_timer_signal = true;
                    continue;
                }
                if let Some(&wr) = unsafe { (*poll_id_to_watch_ref.get()).get(&e.value()) } {
                    let mut flags = dbus::WatchFlags::empty();
                    if e.events().contains(EpollEventBits::IN) {
                        flags |= dbus::WatchFlags::READABLE;
                    }
                    if e.events().contains(EpollEventBits::OUT) {
                        flags |= dbus::WatchFlags::WRITABLE;
                    }
                    if e.events().contains(EpollEventBits::ERR) {
                        flags |= dbus::WatchFlags::ERROR;
                    }
                    if e.events().contains(EpollEventBits::HUP) {
                        flags |= dbus::WatchFlags::HANGUP;
                    }

                    if !unsafe { (*wr).handle(flags) } {
                        tracing::error!(?flags, "dbus.watch.handle");
                    }
                    dbus_signal = true;
                    continue;
                }
                #[cfg(feature = "enable-profiling")]
                if e.value() == 5 {
                    let mut b = [core::mem::MaybeUninit::<u8>::uninit(); 8];
                    if unsafe {
                        libc::read(
                            std::os::fd::AsRawFd::as_raw_fd(memory_sample_timer_fd),
                            b.as_mut_ptr().cast(),
                            8,
                        )
                    } < 0
                    {
                        tracing::error!(reason = %std::io::Error::last_os_error(), "read memory_sample_timer_fd failed");
                    }

                    profiler::sample_memory!();
                }
            }

            if wl_display_signal {
                dp_context.process_events();
            } else {
                dp_context.cancel_reading();
            }

            if terminate_signal {
                break 'app;
            }

            if events_signal {
                sync_event_bus.redispatch(&app_event_dispatcher);
            }

            if pointer_hovering_timer_signal {
                coreloop.as_mut().handle_pointer_hover_timeout();
            }

            if delayed_action_timer_signal {
                coreloop.as_mut().perform_menu_delayed_action();
            }

            if dbus_signal {
                while let Some(m) = dbus.pop_message() {
                    coreloop.as_mut().handle_dbus_message(m);
                }
            }
        }

        #[cfg(windows)]
        let handles = [
            sync_event_bus.event_notify.as_handle(),
            pointer_hovering_timer.as_handle(),
            context_menu_delayed_action_timer.as_handle(),
            #[cfg(feature = "enable-profiling")]
            memory_sample_timer.as_handle(),
        ];
        #[cfg(windows)]
        let mut msg = core::mem::MaybeUninit::uninit();
        #[cfg(windows)]
        'app: loop {
            let r = unsafe {
                windows::Win32::UI::WindowsAndMessaging::MsgWaitForMultipleObjectsEx(
                    Some(&handles),
                    windows::Win32::System::Threading::INFINITE,
                    windows::Win32::UI::WindowsAndMessaging::QS_ALLEVENTS,
                    windows::Win32::UI::WindowsAndMessaging::MWMO_INPUTAVAILABLE,
                )
            };
            if r == windows::Win32::Foundation::WAIT_FAILED {
                panic!(
                    "unrecoverable MsgWaitForMultipleObjectsEx error: {}",
                    std::io::Error::last_os_error()
                );
            }

            if let Some(hindex) = r.0.checked_sub(windows::Win32::Foundation::WAIT_OBJECT_0.0)
                && let Some(&handle) = handles.get(hindex as usize)
            {
                // handle signaled
                if handle == sync_event_bus.event_notify.as_handle() {
                    sync_event_bus.redispatch(&app_event_dispatcher);
                    continue;
                }
                if handle == pointer_hovering_timer.as_handle() {
                    app_event_dispatcher.dispatch(Event::PointerHover);
                    continue;
                }
                if handle == context_menu_delayed_action_timer.as_handle() {
                    app_event_dispatcher.dispatch(Event::MenuPerformDelayedAction);
                    continue;
                }
                #[cfg(feature = "enable-profiling")]
                if handle == memory_sample_timer.as_handle() {
                    profiler::profiler().emit_memory_stats();
                    continue;
                }
            }
            if r.0 == windows::Win32::Foundation::WAIT_OBJECT_0.0 + handles.len() as u32 {
                while unsafe {
                    windows::Win32::UI::WindowsAndMessaging::PeekMessageW(
                        msg.as_mut_ptr(),
                        None,
                        0,
                        0,
                        windows::Win32::UI::WindowsAndMessaging::PM_REMOVE,
                    )
                    .as_bool()
                } {
                    let msg = unsafe { msg.assume_init_ref() };
                    if msg.message == windows::Win32::UI::WindowsAndMessaging::WM_QUIT {
                        break 'app;
                    }

                    unsafe {
                        let _ = windows::Win32::UI::WindowsAndMessaging::TranslateMessage(msg);
                        windows::Win32::UI::WindowsAndMessaging::DispatchMessageW(msg);
                    }
                }
                continue;
            }

            tracing::warn!(?r, "unhandled mwmo result");
        }

        #[cfg(target_os = "macos")]
        unsafe {
            platform::mac::bridge::nsapp_run();
        }

        app_event_dispatcher.terminate();
        shutdown.store(true, std::sync::atomic::Ordering::Release);
        render_thread.join().expect("render_thread join");
    });
}

#[derive(Clone, Debug, PartialEq)]
pub enum SyncEvent {
    NewPresentID { id: u64 },
    WindowPostCreateRenderBuffer { window: WindowHandle },
    FlyoutSurfacePostCreateRenderBuffer { target: FlyoutSurfaceHandle },
    PopupUnmount { id: PopupID },
}
impl SyncEvent {
    pub const fn p_name(&self) -> &'static str {
        match self {
            Self::NewPresentID { .. } => "Sync(NewPresentID)",
            Self::WindowPostCreateRenderBuffer { .. } => "Sync(WindowPostResizeRenderBuffer)",
            Self::FlyoutSurfacePostCreateRenderBuffer { .. } => {
                "Sync(ContextMenuPostResizeRenderBuffer)"
            }
            Self::PopupUnmount { .. } => "Sync(PopupUnmount)",
        }
    }
}

#[derive(Clone, Debug)]
pub enum Event {
    Sync(SyncEvent),
    Quit,
    OpenAlertDialog {
        target_window: WindowHandle,
        message: String,
    },
    PopupClose {
        id: PopupID,
    },
    OpenCustomViewFlyout {
        parent: WindowHandle,
        surface_pos: Point<LogicalUnit>,
        view_constructor: NonCloneable<DummyDebug<Box<dyn FlyoutSurfacePresenterConstructor>>>,
    },
    MenuSelectItem {
        depth: usize,
        index: usize,
    },
    MenuDeselectItem {
        depth: usize,
    },
    MenuSelectCommand {
        id: u64,
    },
    DropdownMenuSelectItem {
        id: usize,
        receiver: std::rc::Weak<uikit::dropdown_box::EventHandler>,
    },
    DockMoveSplitter {
        controlling_dock: ui::dock::DockID,
        pos_client: f32,
    },
    DockBeginPreview {
        initiator: WindowHandle,
        pointer: PointerID,
        source_dock: ui::dock::DockID,
        tab_index: usize,
        pane_rect: Rect<LogicalUnit>,
        tab_size: Size<LogicalUnit>,
        client_pos: Point<LogicalUnit>,
    },
    // TODO: これあんまりいい設計じゃないので使わない形にしたい（macOSでのIME入力によるView更新のためだけに必要）
    ScheduleViewRenderExt {
        id: ViewIdentifier,
    },
    #[cfg(windows)]
    CoreTextLayoutRequested {
        ht: HitTestTreeRef,
        request: windows::UI::Text::Core::CoreTextLayoutRequest,
        deferral: Option<windows::Foundation::Deferral>,
    },
    #[cfg(windows)]
    CoreTextTextUpdating {
        ht: HitTestTreeRef,
        e: windows::UI::Text::Core::CoreTextTextUpdatingEventArgs,
        deferral: Option<windows::Foundation::Deferral>,
    },
    #[cfg(windows)]
    CoreTextFormatUpdating {
        ht: HitTestTreeRef,
        e: windows::UI::Text::Core::CoreTextFormatUpdatingEventArgs,
        deferral: Option<windows::Foundation::Deferral>,
    },
}
impl Event {
    /// Profilerに表示するEvent名
    #[cfg(feature = "enable-profiling")]
    pub const fn p_name(&self) -> &'static str {
        match self {
            Self::Sync(e) => e.p_name(),
            Self::Quit => "Quit",
            Self::OpenAlertDialog { .. } => "OpenAlertDialog",
            Self::PopupClose { .. } => "PopupClose",
            Self::OpenCustomViewFlyout { .. } => "OpenCustomViewFlyout",
            Self::MenuSelectItem { .. } => "MenuSelectItem",
            Self::MenuDeselectItem { .. } => "MenuDeselectItem",
            Self::MenuSelectCommand { .. } => "MenuSelectCommand",
            Self::DropdownMenuSelectItem { .. } => "DropdownMenuSelectItem",
            Self::DockMoveSplitter { .. } => "DockMoveSplitter",
            Self::DockBeginPreview { .. } => "DockBeginPreview",
            Self::ScheduleViewRenderExt { .. } => "ScheduleViewRenderExt",
            #[cfg(not(target_os = "macos"))]
            #[cfg(windows)]
            Self::CoreTextLayoutRequested { .. } => "CoreTextLayoutRequested",
            #[cfg(windows)]
            Self::CoreTextTextUpdating { .. } => "CoreTextTextUpdating",
            #[cfg(windows)]
            Self::CoreTextFormatUpdating { .. } => "CoreTextFormatUpdating",
        }
    }
}

struct EventQueue {
    event_store: *mut VecDeque<Event>,
}
impl EventQueue {
    pub async fn next_event(&self) -> Event {
        EventQueueNextEventAwaiter { q: self }.await
    }
}

#[derive(Clone)]
pub struct LogicFiberEventDispatcher {
    event_store: *mut VecDeque<Event>,
    polling: *mut bool,
    poll_fn_ptr: fn(*mut core::ffi::c_void, ctx: &mut core::task::Context) -> core::task::Poll<()>,
    future_ptr: *mut core::ffi::c_void,
}
impl LogicFiberEventDispatcher {
    pub fn poll_init(&self) {
        unsafe {
            self.polling.write_volatile(true);
        }
        let _ = unsafe {
            (self.poll_fn_ptr)(
                self.future_ptr,
                &mut core::task::Context::from_waker(&core::task::Waker::new(
                    &(),
                    &APP_WAKER_VTABLE,
                )),
            )
        };
        unsafe {
            self.polling.write_volatile(false);
        }
    }

    pub fn dispatch(&self, e: Event) {
        unsafe {
            (*self.event_store).push_back(e);
            if !*self.polling {
                *self.polling = true;
                let _ = (self.poll_fn_ptr)(
                    self.future_ptr,
                    &mut core::task::Context::from_waker(&core::task::Waker::new(
                        &(),
                        &APP_WAKER_VTABLE,
                    )),
                );
                *self.polling = false;
            }
        }
    }

    pub fn terminate(&self) {
        unsafe {
            (*self.event_store).push_back(Event::Quit);
        }

        while unsafe {
            (self.poll_fn_ptr)(
                self.future_ptr,
                &mut core::task::Context::from_waker(&core::task::Waker::new(
                    &(),
                    &APP_WAKER_VTABLE,
                )),
            )
        }
        .is_pending()
        {}
    }

    pub fn can_immediate_dispatch(&self) -> bool {
        unsafe { !*self.polling }
    }
}

struct EventQueueNextEventAwaiter<'e> {
    q: &'e EventQueue,
}
impl<'e> core::future::Future for EventQueueNextEventAwaiter<'e> {
    type Output = Event;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match unsafe { (&mut *self.get_mut().q.event_store).pop_front() } {
            None => core::task::Poll::Pending,
            Some(x) => core::task::Poll::Ready(x),
        }
    }
}

pub struct ColorPickerSharedResources {
    ring_tex_id: TextureID,
    alpha_slider_bg_tex_id: TextureID,
}
impl ColorPickerSharedResources {
    pub fn new(
        texid_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &RenderMessageSender,
    ) -> Self {
        let ring_tex_id = texid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterShaderTexture {
                id: ring_tex_id,
                data: ShaderTexture {
                    width: 128.0,
                    height: 128.0,
                    shader_path: "color_picker_ring.spv".into(),
                },
            })
            .expect("rt_sender.send");
        let alpha_slider_bg_tex_id = texid_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterShaderTexture {
                id: alpha_slider_bg_tex_id,
                data: ShaderTexture {
                    width: 16.0,
                    height: 16.0,
                    shader_path: "checkerboard.spv".into(),
                },
            })
            .expect("rt_sender.send");

        Self {
            ring_tex_id,
            alpha_slider_bg_tex_id,
        }
    }
}

static COLOR_PICKER_SHARED_RES: UnsafeMainThreadOnlyOnceCell<ColorPickerSharedResources> =
    UnsafeMainThreadOnlyOnceCell(core::cell::OnceCell::new());

pub struct ColorPickerView {
    hex_text_input_view_id: ViewIdentifier,
    backing_store: std::rc::Weak<dyn ColorPickerBackingStoreEvent>,
    eh: Option<Rc<ColorPickerEventHandler>>,
}
impl Drop for ColorPickerView {
    fn drop(&mut self) {
        if self.eh.is_some() {
            tracing::warn!("ColorPickedView dropped but still rendered");
        }
    }
}
impl ColorPickerView {
    const RING_THICKNESS: f32 = 12.0;
    const GRADIENT_BOX_MARGIN: f32 = 4.0;
    const POINTER_SIZE: f32 = 12.0;
    const ALPHA_SLIDER_THUMB_THICKNESS: f32 = 3.0;

    pub fn new(
        hex_text_input_view_id: ViewIdentifier,
        backing_store: std::rc::Weak<impl ColorPickerBackingStoreEvent + 'static>,
    ) -> Self {
        Self {
            hex_text_input_view_id,
            backing_store: backing_store as _,
            eh: None,
        }
    }
}
impl View for ColorPickerView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.eh {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .apply();
                ctx.ht_manager.get_data_mut(e.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(e.ht_root).top = layout_rect.top;

                e
            }
            None => {
                // first render
                let shared = COLOR_PICKER_SHARED_RES.0.get_or_init(|| {
                    ColorPickerSharedResources::new(
                        ctx.main_thread_texture_id_issuer,
                        ctx.system_link.rt_sender(),
                    )
                });

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(layout_rect.left),
                        AnimatableFloat::Value(layout_rect.top),
                    ],
                    size: [AnimatableFloat::Value(128.0), AnimatableFloat::Value(128.0)],
                    has_bitmap: true,
                    composite_mode: CompositeMode::DirectSourceOver(CompositeTexture {
                        id: shared.ring_tex_id,
                        r#type: TextureType::Color,
                        mapping: TextureMappingMode::Stretch,
                        slice_borders: [0.0; 4],
                    }),
                    ..Default::default()
                });
                let gradient_box_size =
                    2.0 * (64.0 - Self::RING_THICKNESS - Self::GRADIENT_BOX_MARGIN) / 2.0f32.sqrt();
                let ct_sat_light_box = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(-gradient_box_size * 0.5),
                        AnimatableFloat::Value(-gradient_box_size * 0.5),
                    ],
                    relative_offset_adjustment: [0.5, 0.5],
                    size: [
                        AnimatableFloat::Value(gradient_box_size),
                        AnimatableFloat::Value(gradient_box_size),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::ColorPickerGradientBox(AnimatableColor::Value(
                        [1.0, 0.0, 0.0, 1.0],
                    )),
                    ..Default::default()
                });
                let ct_pointer = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
                    size: [
                        AnimatableFloat::Value(Self::POINTER_SIZE),
                        AnimatableFloat::Value(Self::POINTER_SIZE),
                    ],
                    has_bitmap: true,
                    corner_radius: CornerRadius::all(Self::POINTER_SIZE * 0.5),
                    border: Some(Border {
                        thickness: 2.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_pointer_dark = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [AnimatableFloat::Value(2.0), AnimatableFloat::Value(2.0)],
                    size: [
                        AnimatableFloat::Value(Self::POINTER_SIZE - 4.0),
                        AnimatableFloat::Value(Self::POINTER_SIZE - 4.0),
                    ],
                    has_bitmap: true,
                    corner_radius: CornerRadius::all((Self::POINTER_SIZE - 4.0) * 0.5),
                    border: Some(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([0.0, 0.0, 0.0, 0.5]),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let alpha_slider_content_gradient =
                    ctx.composite_tree.create_gradient(Gradient::Linear {
                        start_color: [1.0, 0.0, 0.0, 0.0],
                        end_color: [1.0, 0.0, 0.0, 1.0],
                        start_pos_relative: [0.0, 0.0],
                        end_pos_relative: [1.0, 0.0],
                    });
                let ct_alpha_slider_base = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(0.0),
                        AnimatableFloat::Value(128.0 + 8.0),
                    ],
                    size: [AnimatableFloat::Value(128.0), AnimatableFloat::Value(16.0)],
                    has_bitmap: true,
                    composite_mode: CompositeMode::DirectSourceOver(CompositeTexture {
                        id: shared.alpha_slider_bg_tex_id,
                        r#type: TextureType::Color,
                        mapping: TextureMappingMode::Repeat,
                        slice_borders: [0.0; 4],
                    }),
                    ..Default::default()
                });
                let ct_alpha_slider_content = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillLinearGradient(
                        alpha_slider_content_gradient,
                    ),
                    ..Default::default()
                });
                let ct_alpha_slider_thumb = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(128.0 - Self::ALPHA_SLIDER_THUMB_THICKNESS * 0.5),
                        AnimatableFloat::Value(0.0),
                    ],
                    size: [
                        AnimatableFloat::Value(Self::ALPHA_SLIDER_THUMB_THICKNESS),
                        AnimatableFloat::Value(0.0),
                    ],
                    relative_size_adjustment: [0.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        0.1, 0.1, 0.1, 1.0,
                    ])),
                    border: Some(Border {
                        thickness: 0.5,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_hex_label = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(0.0),
                        AnimatableFloat::Value(128.0 + 32.0 + 16.0),
                    ],
                    size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(20.0)],
                    has_bitmap: false,
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: "HEX".into(),
                            font_id: FontID::UIDefault,
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        }],
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: layout_rect.left,
                    top: layout_rect.top,
                    width: 128.0,
                    height: 128.0,
                    ..Default::default()
                });
                let ht_sat_light_box = ctx.ht_manager.create(HitTestTreeData {
                    left: -gradient_box_size * 0.5,
                    top: -gradient_box_size * 0.5,
                    left_adjustment_factor: 0.5,
                    top_adjustment_factor: 0.5,
                    width: gradient_box_size,
                    height: gradient_box_size,
                    ..Default::default()
                });
                let ht_alpha_slider = ctx.ht_manager.create(HitTestTreeData {
                    left: 0.0,
                    top: 128.0 + 8.0,
                    width: 128.0,
                    height: 16.0,
                    ..Default::default()
                });

                ctx.composite_tree.add_child(ct_root, ct_sat_light_box);
                ctx.composite_tree.add_child(ct_pointer, ct_pointer_dark);
                ctx.composite_tree.add_child(ct_sat_light_box, ct_pointer);
                ctx.composite_tree
                    .add_child(ct_alpha_slider_base, ct_alpha_slider_content);
                ctx.composite_tree
                    .add_child(ct_alpha_slider_base, ct_alpha_slider_thumb);
                ctx.composite_tree.add_child(ct_root, ct_alpha_slider_base);
                ctx.composite_tree.add_child(ct_root, ct_hex_label);
                ctx.ht_manager.add_child(ht_root, ht_sat_light_box);
                ctx.ht_manager.add_child(ht_root, ht_alpha_slider);

                let eh = Rc::new_cyclic(|thisref| ColorPickerEventHandler {
                    backing_store: self.backing_store.clone(),
                    ct_root,
                    ct_sat_light_box,
                    ct_pointer,
                    ct_pointer_dark,
                    ct_alpha_slider_base,
                    ct_alpha_slider_content,
                    ct_alpha_slider_thumb,
                    alpha_slider_content_gradient,
                    ct_hex_label,
                    ht_root,
                    ht_sat_light_box,
                    ht_alpha_slider,
                    sat_light_box_size: Size::new_logical(gradient_box_size, gradient_box_size),
                    ring_selecting: Cell::new(false),
                    box_selecting: Cell::new(false),
                    alpha_sliding: Cell::new(false),
                    current_hue: Cell::new(0.0),
                    current_light: Cell::new(1.0),
                    current_saturation: Cell::new(0.0),
                    current_alpha: Cell::new(1.0),
                    hex_text_input_view: RefCell::new(ColorPickerHexTextInputView::new(
                        self.hex_text_input_view_id,
                        Rect::from_lt_size(
                            Point::new_logical(32.0, 128.0 + 32.0 + 16.0),
                            Size::new_logical(128.0 - 32.0, 20.0),
                        ),
                        thisref.clone(),
                    )),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);
                ctx.ht_manager.set_action_handler(ht_sat_light_box, &eh);
                ctx.ht_manager.set_action_handler(ht_alpha_slider, &eh);

                if let Some(e) = self.backing_store.upgrade() {
                    let v = e.value();

                    eh.set_by_color(v, ctx.composite_tree);
                    eh.hex_text_input_view.borrow().set_value(v);
                }

                &*self.eh.insert(eh)
            }
        };
        // TODO: ViewがViewをもつパターン(これなしにしたほうがいいかも)
        // self.eh.hex_text_input_view.borrow_mut().render(
        //     ctx,
        //     &uikit::RawMountTarget {
        //         ht_root: self.eh.ht_root,
        //         ct_root: self.eh.ct_root,
        //     },
        //     kf_group,
        // );

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(e) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(e.ct_root);
        ctx.ht_manager.free_all(e.ht_root);
        ctx.composite_tree
            .free_gradient(e.alpha_slider_content_gradient);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(128.0, 128.0 + 32.0 + 16.0 + 20.0)
    }
}

struct ColorPickerEventHandler {
    backing_store: std::rc::Weak<dyn ColorPickerBackingStoreEvent>,
    ct_root: CompositeTreeRef,
    ct_sat_light_box: CompositeTreeRef,
    ct_pointer: CompositeTreeRef,
    ct_pointer_dark: CompositeTreeRef,
    ct_alpha_slider_base: CompositeTreeRef,
    ct_alpha_slider_content: CompositeTreeRef,
    ct_alpha_slider_thumb: CompositeTreeRef,
    alpha_slider_content_gradient: GradientRef,
    ct_hex_label: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    ht_sat_light_box: HitTestTreeRef,
    ht_alpha_slider: HitTestTreeRef,
    sat_light_box_size: Size<LogicalUnit>,
    ring_selecting: Cell<bool>,
    box_selecting: Cell<bool>,
    alpha_sliding: Cell<bool>,
    current_hue: Cell<f32>,
    current_light: Cell<f32>,
    current_saturation: Cell<f32>,
    current_alpha: Cell<f32>,
    hex_text_input_view: RefCell<ColorPickerHexTextInputView>,
}
impl HitTestTreeActionHandler for ColorPickerEventHandler {
    fn hittest(&self, target: HitTestTreeRef, args: &HitTestArgs) -> bool {
        if target == self.ht_root {
            let dcenter_x = args.tree_local_x - 64.0;
            let dcenter_y = args.tree_local_y - 64.0;
            let dcenter = (dcenter_x * dcenter_x + dcenter_y * dcenter_y).sqrt();

            return (64.0 - ColorPickerView::RING_THICKNESS) <= dcenter && dcenter <= 64.0;
        }

        true
    }

    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_root {
            // ring
            let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_root,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let dcenter_x = local_x - 64.0;
            let dcenter_y = local_y - 64.0;
            let hue =
                360.0 * (dcenter_y.atan2(dcenter_x) / core::f32::consts::TAU + 0.5) + 360.0 - 90.0;
            self.select_hue(hue, context.composite_tree, context.view_render_queue);
            self.ring_selecting.set(true);

            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }

        if sender == self.ht_sat_light_box {
            let (local_x, local_y, w, h) = context.ht_manager.translate_client_to_tree_local(
                self.ht_sat_light_box,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let local_x = local_x.clamp(0.0, w);
            let local_y = local_y.clamp(0.0, h);
            self.move_cursor(
                local_x,
                local_y,
                context.composite_tree,
                context.view_render_queue,
            );

            self.box_selecting.set(true);
            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }

        if sender == self.ht_alpha_slider {
            let (local_x, _, w, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_alpha_slider,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let new_alpha = local_x.clamp(0.0, w) / w;
            self.current_alpha.set(new_alpha);
            self.color_changed(context.composite_tree, context.view_render_queue);
            context
                .composite_tree
                .get_mut(self.ct_alpha_slider_thumb)
                .offset[0] = AnimatableFloat::Value(
                new_alpha * w - ColorPickerView::ALPHA_SLIDER_THUMB_THICKNESS * 0.5,
            );
            context
                .composite_tree
                .mark_dirty(self.ct_alpha_slider_thumb);

            self.alpha_sliding.set(true);
            return EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT;
        }

        EventContinueControl::empty()
    }

    fn on_pointer_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_root && self.ring_selecting.get() {
            // ring
            let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_root,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let dcenter_x = local_x - 64.0;
            let dcenter_y = local_y - 64.0;
            let hue =
                360.0 * (dcenter_y.atan2(dcenter_x) / core::f32::consts::TAU + 0.5) + 360.0 - 90.0;
            self.select_hue(hue, context.composite_tree, context.view_render_queue);

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_sat_light_box && self.box_selecting.get() {
            let (local_x, local_y, w, h) = context.ht_manager.translate_client_to_tree_local(
                self.ht_sat_light_box,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let local_x = local_x.clamp(0.0, w);
            let local_y = local_y.clamp(0.0, h);
            self.move_cursor(
                local_x,
                local_y,
                context.composite_tree,
                context.view_render_queue,
            );

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_alpha_slider && self.alpha_sliding.get() {
            let (local_x, _, w, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_alpha_slider,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let new_alpha = local_x.clamp(0.0, w) / w;
            self.current_alpha.set(new_alpha);
            self.color_changed(context.composite_tree, context.view_render_queue);
            context
                .composite_tree
                .get_mut(self.ct_alpha_slider_thumb)
                .offset[0] = AnimatableFloat::Value(
                new_alpha * w - ColorPickerView::ALPHA_SLIDER_THUMB_THICKNESS * 0.5,
            );
            context
                .composite_tree
                .mark_dirty(self.ct_alpha_slider_thumb);

            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::empty()
    }

    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_root {
            // ring
            let (local_x, local_y, _, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_root,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let dcenter_x = local_x - 64.0;
            let dcenter_y = local_y - 64.0;
            let hue =
                360.0 * (dcenter_y.atan2(dcenter_x) / core::f32::consts::TAU + 0.5) + 360.0 - 90.0;
            self.select_hue(hue, context.composite_tree, context.view_render_queue);

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_sat_light_box {
            let (local_x, local_y, w, h) = context.ht_manager.translate_client_to_tree_local(
                self.ht_sat_light_box,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let local_x = local_x.clamp(0.0, w);
            let local_y = local_y.clamp(0.0, h);
            self.move_cursor(
                local_x,
                local_y,
                context.composite_tree,
                context.view_render_queue,
            );

            return EventContinueControl::STOP_PROPAGATION;
        }

        if sender == self.ht_alpha_slider {
            let (local_x, _, w, _) = context.ht_manager.translate_client_to_tree_local(
                self.ht_alpha_slider,
                args.client_pos.x,
                args.client_pos.y,
                args.client_size.width,
                args.client_size.height,
            );
            let new_alpha = local_x.clamp(0.0, w) / w;
            self.current_alpha.set(new_alpha);
            self.color_changed(context.composite_tree, context.view_render_queue);
            context
                .composite_tree
                .get_mut(self.ct_alpha_slider_thumb)
                .offset[0] = AnimatableFloat::Value(
                new_alpha * w - ColorPickerView::ALPHA_SLIDER_THUMB_THICKNESS * 0.5,
            );
            context
                .composite_tree
                .mark_dirty(self.ct_alpha_slider_thumb);

            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::empty()
    }

    fn on_pointer_up(
        &self,
        sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if sender == self.ht_root {
            self.ring_selecting.set(false);
            return EventContinueControl::STOP_PROPAGATION
                | EventContinueControl::RELEASE_CAPTURE_ELEMENT;
        }

        if sender == self.ht_sat_light_box {
            self.box_selecting.set(false);
            return EventContinueControl::STOP_PROPAGATION
                | EventContinueControl::RELEASE_CAPTURE_ELEMENT;
        }

        if sender == self.ht_alpha_slider {
            self.alpha_sliding.set(false);
            return EventContinueControl::STOP_PROPAGATION
                | EventContinueControl::RELEASE_CAPTURE_ELEMENT;
        }

        EventContinueControl::empty()
    }
}
impl ColorPickerEventHandler {
    fn move_cursor<E>(
        &self,
        x: f32,
        y: f32,
        composite_tree: &mut CompositeTree<E>,
        view_render_queue: &mut ViewRenderQueue,
    ) {
        self.current_light
            .set(1.0 - y / self.sat_light_box_size.height);
        self.current_saturation
            .set(x / self.sat_light_box_size.width);
        self.color_changed(composite_tree, view_render_queue);

        let ct_pointer = composite_tree.get_mut(self.ct_pointer);
        ct_pointer.offset = [
            AnimatableFloat::Value(x - ColorPickerView::POINTER_SIZE * 0.5),
            AnimatableFloat::Value(y - ColorPickerView::POINTER_SIZE * 0.5),
        ];
        composite_tree.mark_dirty(self.ct_pointer);
    }

    fn select_hue<E>(
        &self,
        hue: f32,
        composite_tree: &mut CompositeTree<E>,
        view_render_queue: &mut ViewRenderQueue,
    ) {
        self.current_hue.set(hue);
        self.color_changed(composite_tree, view_render_queue);

        let r = hue_to_rgb_wave(hue + 120.0);
        let g = hue_to_rgb_wave(hue);
        let b = hue_to_rgb_wave(hue - 120.0);

        composite_tree.get_mut(self.ct_sat_light_box).composite_mode =
            CompositeMode::ColorPickerGradientBox(AnimatableColor::Value([r, g, b, 1.0]));
        composite_tree.mark_dirty(self.ct_sat_light_box);
    }

    fn color_changed<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        view_render_queue: &mut ViewRenderQueue,
    ) {
        const fn lerp(a: f32, b: f32, t: f32) -> f32 {
            a + (b - a) * t
        }

        let r = lerp(
            1.0,
            hue_to_rgb_wave(self.current_hue.get() + 120.0),
            self.current_saturation.get(),
        ) * self.current_light.get();
        let g = lerp(
            1.0,
            hue_to_rgb_wave(self.current_hue.get() - 0.0),
            self.current_saturation.get(),
        ) * self.current_light.get();
        let b = lerp(
            1.0,
            hue_to_rgb_wave(self.current_hue.get() - 120.0),
            self.current_saturation.get(),
        ) * self.current_light.get();
        let rgba = gen_rgba(
            (r * 255.0) as _,
            (g * 255.0) as _,
            (b * 255.0) as _,
            (self.current_alpha.get() * 255.0) as _,
        );

        self.hex_text_input_view.borrow().set_value(rgba);

        composite_tree.set_gradient(
            self.alpha_slider_content_gradient,
            Gradient::Linear {
                start_color: [r, g, b, 0.0],
                end_color: [r, g, b, 1.0],
                start_pos_relative: [0.0, 0.0],
                end_pos_relative: [1.0, 0.0],
            },
        );

        if let Some(e) = self.backing_store.upgrade() {
            e.new_value(rgba, view_render_queue);
        }
    }

    fn set_by_color<E>(&self, color: u32, composite_tree: &mut CompositeTree<E>) {
        let r = color as u8 as f32 / 255.0;
        let g = (color >> 8) as u8 as f32 / 255.0;
        let b = (color >> 16) as u8 as f32 / 255.0;
        let a = (color >> 24) as u8 as f32 / 255.0;

        let max = r.max(g).max(b);
        let min = r.min(g).min(b);
        let d = max - min;
        let hue = if d == 0.0 {
            0.0
        } else if max == r {
            60.0 * (g - b) / d
        } else if max == g {
            120.0 + 60.0 * (b - r) / d
        } else {
            240.0 + 60.0 * (r - g) / d
        };
        let hue = if hue < 0.0 { 360.0 + hue } else { hue };
        let saturation = (max - min) / max;
        let light = max;

        self.current_hue.set(hue);
        self.current_light.set(light);
        self.current_saturation.set(saturation);
        self.current_alpha.set(a);

        composite_tree.get_mut(self.ct_sat_light_box).composite_mode =
            CompositeMode::ColorPickerGradientBox(AnimatableColor::Value([
                hue_to_rgb_wave(hue + 120.0),
                hue_to_rgb_wave(hue),
                hue_to_rgb_wave(hue - 120.0),
                1.0,
            ]));
        composite_tree.mark_dirty(self.ct_sat_light_box);

        let pointer_x = saturation * self.sat_light_box_size.width;
        let pointer_y = (1.0 - light) * self.sat_light_box_size.height;
        let ct_pointer = composite_tree.get_mut(self.ct_pointer);
        ct_pointer.offset = [
            AnimatableFloat::Value(pointer_x - ColorPickerView::POINTER_SIZE * 0.5),
            AnimatableFloat::Value(pointer_y - ColorPickerView::POINTER_SIZE * 0.5),
        ];
        composite_tree.mark_dirty(self.ct_pointer);
        composite_tree.get_mut(self.ct_alpha_slider_thumb).offset[0] =
            AnimatableFloat::Value(a * 128.0 - ColorPickerView::ALPHA_SLIDER_THUMB_THICKNESS * 0.5);
        composite_tree.mark_dirty(self.ct_alpha_slider_thumb);

        composite_tree.set_gradient(
            self.alpha_slider_content_gradient,
            Gradient::Linear {
                start_color: [r, g, b, 0.0],
                end_color: [r, g, b, 1.0],
                start_pos_relative: [0.0, 0.0],
                end_pos_relative: [1.0, 0.0],
            },
        );
    }
}

const fn hue_to_rgb_wave(hue: f32) -> f32 {
    // generate ／￣￣＼＿＿ wave
    let phase = (hue / 60.0) % 6.0;
    match phase {
        0.0..1.0 => phase,
        1.0..3.0 => 1.0,
        3.0..4.0 => 4.0 - phase,
        _ => 0.0,
    }
}

const fn gen_rgba(r: u8, g: u8, b: u8, a: u8) -> u32 {
    r as u32 | ((g as u32) << 8) | ((b as u32) << 16) | ((a as u32) << 24)
}

struct ColorPickerHexTextInputView {
    id: ViewIdentifier,
    eh: Option<Rc<ColorPickerHexTextInputEventHandler>>,
    rect: Rect<LogicalUnit>,
    parent_view_handler: std::rc::Weak<ColorPickerEventHandler>,
}
impl ColorPickerHexTextInputView {
    pub fn new(
        id: ViewIdentifier,
        rect: Rect<LogicalUnit>,
        parent_view_handler: std::rc::Weak<ColorPickerEventHandler>,
    ) -> Self {
        Self {
            id,
            eh: None,
            rect,
            parent_view_handler,
        }
    }

    fn set_value(&self, value: u32) {
        // TODO: render内でやるようにする
        tracing::warn!(value, "todo: set_value");
    }
}
impl View for ColorPickerHexTextInputView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.eh {
            Some(ref e) => {
                // TODO: reflect changes
                e
            }
            None => {
                let kf_token = ctx.keyboard_focus_registry.acquire_token();

                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: self.rect.left,
                    top: self.rect.top,
                    width: self.rect.width,
                    height: self.rect.height,
                    cursor_shape: CursorShape::IBeam,
                    ..Default::default()
                });
                let eh = Rc::new_cyclic(|eh| ColorPickerHexTextInputEventHandler {
                    core: uikit::TextInputViewCore::new(
                        ctx,
                        self.rect.clone(),
                        [0.0; 2],
                        [0.0; 2],
                        self.id,
                        ht_root,
                    ),
                    value_edit: RefCell::new("00000000".into()),
                    value: Cell::new(0),
                    ht_root,
                    token: kf_token,
                    parent_view_handler: self.parent_view_handler.clone(),
                });
                ctx.keyboard_focus_registry.set_event_handler(kf_token, &eh);
                ctx.ht_manager.set_action_handler(ht_root, eh.core.entity());

                &*self.eh.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.core.entity().ct_root()),
            hit_tree: Some(e.core.entity().ht_root()),
            keyboard_focus: Some(e.token),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.keyboard_focus_registry.release_token(entity.token);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }
}

struct ColorPickerHexTextInputEventHandler {
    core: uikit::TextInputViewCore,
    value_edit: RefCell<String>,
    value: Cell<u32>,
    ht_root: HitTestTreeRef,
    token: FocusTargetToken,
    parent_view_handler: std::rc::Weak<ColorPickerEventHandler>,
}
impl KeyInputEventHandler for ColorPickerHexTextInputEventHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        self.core.entity().focus_taken(context)
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        self.core.entity().focus_released(context);
        self.confirm_direct_input(context.composite_tree, context.view_render_queue);
    }

    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        if code == KeyInputCode::Enter {
            // 確定or入力開始
            self.confirm_direct_input(context.composite_tree, context.view_render_queue);
            return;
        }

        if code == KeyInputCode::Esc {
            // 入力キャンセル
            self.cancel_direct_input(context.view_render_queue);
            return;
        }

        self.core.entity().keydown(context, code, modifier);
    }

    #[inline(always)]
    fn r#char(&self, context: &mut InputEventContext, ch: char, modifier: ModifierKey) {
        self.core.entity().r#char(context, ch, modifier);
    }

    #[inline(always)]
    #[cfg(feature = "wayland")]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: Option<&str>,
        new_preedit_string: Option<&str>,
    ) {
        self.core
            .entity()
            .ime_state_changes(context, new_committed_string, new_preedit_string);
    }
}
impl TextInputViewIO for ColorPickerHexTextInputEventHandler {
    fn text(&self, _requester: ViewIdentifier, _app: &Application) -> String {
        self.value_edit.borrow().clone()
    }

    fn set_text(&self, _sender: ViewIdentifier, _app: &mut ApplicationMutation, text: String) {
        self.value_edit.replace(text);
    }
}
impl ColorPickerHexTextInputEventHandler {
    fn parse(text: &str) -> Option<u32> {
        const fn parse_ascii_hexdigit(c: u8) -> Option<u8> {
            match c {
                b'0'..=b'9' => Some(c - b'0'),
                b'A'..=b'F' => Some(c - b'A' + 10),
                b'a'..=b'f' => Some(c - b'a' + 10),
                _ => None,
            }
        }

        match text.as_bytes() {
            // RGB
            &[r, g, b] => {
                let r = parse_ascii_hexdigit(r)?;
                let g = parse_ascii_hexdigit(g)?;
                let b = parse_ascii_hexdigit(b)?;

                Some(gen_rgba(r | r << 4, g | g << 4, b | b << 4, 255))
            }
            // RGBA
            &[r, g, b, a] => {
                let r = parse_ascii_hexdigit(r)?;
                let g = parse_ascii_hexdigit(g)?;
                let b = parse_ascii_hexdigit(b)?;
                let a = parse_ascii_hexdigit(a)?;

                Some(gen_rgba(r | r << 4, g | g << 4, b | b << 4, a | a << 4))
            }
            // RRGGBB
            &[r0, r1, g0, g1, b0, b1] => {
                let r = parse_ascii_hexdigit(r1)? | parse_ascii_hexdigit(r0)? << 4;
                let g = parse_ascii_hexdigit(g1)? | parse_ascii_hexdigit(g0)? << 4;
                let b = parse_ascii_hexdigit(b1)? | parse_ascii_hexdigit(b0)? << 4;

                Some(gen_rgba(r, g, b, 255))
            }
            // RRGGBBAA
            &[r0, r1, g0, g1, b0, b1, a0, a1] => {
                let r = parse_ascii_hexdigit(r1)? | parse_ascii_hexdigit(r0)? << 4;
                let g = parse_ascii_hexdigit(g1)? | parse_ascii_hexdigit(g0)? << 4;
                let b = parse_ascii_hexdigit(b1)? | parse_ascii_hexdigit(b0)? << 4;
                let a = parse_ascii_hexdigit(a1)? | parse_ascii_hexdigit(a0)? << 4;

                Some(gen_rgba(r, g, b, a))
            }
            // unknown
            _ => None,
        }
    }

    fn fmt(rgba: u32) -> String {
        let r = rgba as u8;
        let g = (rgba >> 8) as u8;
        let b = (rgba >> 16) as u8;
        let a = (rgba >> 24) as u8;

        format!("{r:02X}{g:02X}{b:02X}{a:02X}")
    }

    fn confirm_direct_input<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        view_render_queue: &mut ViewRenderQueue,
    ) {
        let current_value = self.value.get();
        let new_value = Self::parse(&self.value_edit.borrow()).unwrap_or(current_value);
        self.value.set(new_value);

        // HitTestTreeへの変更がはいるので遅延させる
        self.core
            .entity()
            .lazy_update_and_schedule(view_render_queue, |e| {
                e.perform_external_state_update(|st| st.set_content(Self::fmt(new_value)))
            });

        if current_value != new_value {
            // notify changed
            if let Some(parent) = self.parent_view_handler.upgrade() {
                parent.set_by_color(new_value, composite_tree);

                if let Some(e) = parent.backing_store.upgrade() {
                    e.new_value(new_value, view_render_queue);
                }
            }
        }
    }

    fn cancel_direct_input(&self, view_render_queue: &mut ViewRenderQueue) {
        self.core
            .entity()
            .lazy_update_and_schedule(view_render_queue, |e| {
                e.perform_external_state_update(|st| st.set_content(Self::fmt(self.value.get())))
            });
    }
}

pub trait ColorPickerBackingStoreEvent {
    fn value(&self) -> u32;
    fn new_value(&self, value: u32, view_render_queue: &mut ViewRenderQueue);
}

pub struct EditableColorButtonView {
    id: TypedViewIdentifier<EditableColorButtonView>,
    eh: Option<Rc<EditableColorButtonEventHandler>>,
    color: u32,
}
impl EditableColorButtonView {
    const COLOR_PREVIEW_MARGIN: f32 = 6.0;

    pub fn new(id: TypedViewIdentifier<EditableColorButtonView>, init_color: u32) -> Self {
        Self {
            id,
            eh: None,
            color: init_color,
        }
    }
}
impl View for EditableColorButtonView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.eh {
            Some(ref e) => {
                ctx.composite_tree
                    .begin_mod_chain(e.ct_color)
                    .composite_mode(CompositeMode::FillColor(AnimatableColor::Value([
                        e.color.get() as u8 as f32 / 255.0,
                        (e.color.get() >> 8) as u8 as f32 / 255.0,
                        (e.color.get() >> 16) as u8 as f32 / 255.0,
                        (e.color.get() >> 24) as u8 as f32 / 255.0,
                    ])))
                    .apply();
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .offset_imm(layout_rect.left, layout_rect.top)
                    .size_imm(layout_rect.width, layout_rect.height)
                    .apply();
                ctx.ht_manager.get_data_mut(e.ht_root).left = layout_rect.left;
                ctx.ht_manager.get_data_mut(e.ht_root).top = layout_rect.top;
                ctx.ht_manager.get_data_mut(e.ht_root).width = layout_rect.width;
                ctx.ht_manager.get_data_mut(e.ht_root).height = layout_rect.height;

                e
            }
            None => {
                // first render
                let shared = COLOR_PICKER_SHARED_RES.0.get_or_init(|| {
                    ColorPickerSharedResources::new(
                        ctx.main_thread_texture_id_issuer,
                        ctx.system_link.rt_sender(),
                    )
                });

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(layout_rect.left),
                        AnimatableFloat::Value(layout_rect.top),
                    ],
                    size: [
                        AnimatableFloat::Value(layout_rect.width),
                        AnimatableFloat::Value(layout_rect.height),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 0.0,
                    ])),
                    border: Some(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    }),
                    corner_radius: CornerRadius::all(8.0),
                    ..Default::default()
                });
                let ct_color_base = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    offset: [
                        AnimatableFloat::Value(Self::COLOR_PREVIEW_MARGIN),
                        AnimatableFloat::Value(Self::COLOR_PREVIEW_MARGIN),
                    ],
                    size: [
                        AnimatableFloat::Value(-Self::COLOR_PREVIEW_MARGIN * 2.0),
                        AnimatableFloat::Value(-Self::COLOR_PREVIEW_MARGIN * 2.0),
                    ],
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::DirectSourceOver(CompositeTexture {
                        id: shared.alpha_slider_bg_tex_id,
                        r#type: TextureType::Color,
                        mapping: TextureMappingMode::Repeat,
                        slice_borders: [0.0; 4],
                    }),
                    ..Default::default()
                });
                let ct_color = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        self.color as u8 as f32 / 255.0,
                        (self.color >> 8) as u8 as f32 / 255.0,
                        (self.color >> 16) as u8 as f32 / 255.0,
                        (self.color >> 24) as u8 as f32 / 255.0,
                    ])),
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    left: layout_rect.left,
                    top: layout_rect.top,
                    width: layout_rect.width,
                    height: layout_rect.height,
                    cursor_shape: CursorShape::Pointer,
                    ..Default::default()
                });

                ctx.composite_tree.add_child(ct_color_base, ct_color);
                ctx.composite_tree.add_child(ct_root, ct_color_base);

                let eh = Rc::new_cyclic(|thisref| EditableColorButtonEventHandler {
                    thisref: thisref.clone(),
                    view_id: self.id,
                    ct_root,
                    ht_root,
                    ct_color_base,
                    ct_color,
                    color: Cell::new(self.color),
                });
                ctx.ht_manager.set_action_handler(ht_root, &eh);

                &*self.eh.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(48.0, 20.0)
    }
}

struct EditableColorButtonEventHandler {
    thisref: std::rc::Weak<EditableColorButtonEventHandler>,
    view_id: TypedViewIdentifier<EditableColorButtonView>,
    ct_root: CompositeTreeRef,
    ct_color_base: CompositeTreeRef,
    ct_color: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    color: Cell<u32>,
}
impl HitTestTreeActionHandler for EditableColorButtonEventHandler {
    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let vc = Box::new(EditableColorButtonPickerFlyoutViewConstructor {
            backing_store: self.thisref.clone(),
        });
        let (gl, gt, gw, gh, _) = context.ht_manager.compute_global_rect_autoroot(sender);
        context
            .system_link
            .dispatch_event(Event::OpenCustomViewFlyout {
                parent: context
                    .ht_manager
                    .query_root_window(sender)
                    .expect("not mounted"),
                surface_pos: Point::new_logical(gl + gw * 0.5 - vc.size().width * 0.5, gt + gh),
                view_constructor: NonCloneable(DummyDebug(vc)),
            });

        EventContinueControl::STOP_PROPAGATION
    }
}
impl ColorPickerBackingStoreEvent for EditableColorButtonEventHandler {
    fn value(&self) -> u32 {
        self.color.get()
    }

    fn new_value(&self, value: u32, view_render_queue: &mut ViewRenderQueue) {
        self.color.set(value);
        view_render_queue.schedule(self.view_id.into_untyped());
    }
}

struct EditableColorButtonPickerFlyoutView(TypedViewIdentifier<ColorPickerView>);
impl EditableColorButtonPickerFlyoutView {
    fn new(
        ctx: &mut ViewInitContext,
        backing_store: &std::rc::Weak<EditableColorButtonEventHandler>,
    ) -> Self {
        let v = ColorPickerView::new(ctx.alloc_view_id_without_instance(), backing_store.clone());
        Self(ctx.construct_view_direct(|_| Box::new(v)))
    }
}
impl FlyoutSurfacePresenter for EditableColorButtonPickerFlyoutView {
    fn root_view_id(&self) -> ViewIdentifier {
        self.0.into_untyped()
    }
}

pub struct EditableColorButtonPickerFlyoutViewConstructor {
    backing_store: std::rc::Weak<EditableColorButtonEventHandler>,
}
impl FlyoutSurfacePresenterConstructor for EditableColorButtonPickerFlyoutViewConstructor {
    fn size(&self) -> Size<LogicalUnit> {
        Size::new_logical(128.0 + 16.0, 128.0 + 32.0 + 16.0 + 20.0 + 16.0)
    }

    fn create(&self, ctx: &mut ViewInitContext) -> Box<dyn FlyoutSurfacePresenter> {
        Box::new(EditableColorButtonPickerFlyoutView::new(
            ctx,
            &self.backing_store,
        ))
    }
}

struct ColorPickerTestBackingStore {
    color: Cell<u32>,
}
impl ColorPickerBackingStoreEvent for ColorPickerTestBackingStore {
    fn value(&self) -> u32 {
        self.color.get()
    }
    fn new_value(&self, value: u32, _view_render_queue: &mut ViewRenderQueue) {
        self.color.set(value);
    }
}

struct UIKitPreviewNumericInputValueStore(Cell<i64>);
impl TextInputViewIO for UIKitPreviewNumericInputValueStore {
    fn text(&self, _requester: ViewIdentifier, _application: &Application) -> String {
        self.0.get().to_string()
    }

    fn set_text(
        &self,
        _sender: ViewIdentifier,
        _application: &mut ApplicationMutation,
        input: String,
    ) {
        let Some(new_value) = input
            .split_once('.')
            .map_or(&input as &str, |x| x.0)
            .parse::<i64>()
            .ok()
        else {
            // invalid input(hold current)
            return;
        };

        self.0.set(new_value);
    }
}
impl NumericInputViewIO for UIKitPreviewNumericInputValueStore {
    fn set_delta(
        &self,
        _sender: ViewIdentifier,
        _application: &mut ApplicationMutation,
        delta: f32,
    ) {
        self.0.update(|x| x + (delta * 0.5).round() as i64)
    }
}

struct UIKitPreviewTextInputValueStore(RefCell<String>);
impl TextInputViewIO for UIKitPreviewTextInputValueStore {
    fn text(&self, _requester: ViewIdentifier, _app: &Application) -> String {
        self.0.borrow().clone()
    }

    fn set_text(&self, _sender: ViewIdentifier, _app: &mut ApplicationMutation, text: String) {
        *self.0.borrow_mut() = text;
    }
}

struct UIKitPreviewDropdownValueStore(Cell<usize>);
impl uikit::dropdown_box::IO for UIKitPreviewDropdownValueStore {
    fn selected_index(&self, _requester: ViewIdentifier, _application: &Application) -> usize {
        self.0.get()
    }

    fn on_selected_index_change(
        &self,
        _sender: ViewIdentifier,
        index: usize,
        _application: &mut ApplicationMutation,
    ) {
        self.0.set(index)
    }
}

pub struct UIKitPreviewPanePresenter {
    kf_group: KeyboardFocusGroupRef,
    scroll_container: TypedViewIdentifier<ScrollContainer>,
    content_view: TypedViewIdentifier<ContainerView>,
    text_input_backing_store1: Rc<UIKitPreviewTextInputValueStore>,
    text_input_backing_store2: Rc<UIKitPreviewTextInputValueStore>,
    color_picker_backing_store: Rc<ColorPickerTestBackingStore>,
    numeric_input_view_backing_store: Rc<UIKitPreviewNumericInputValueStore>,
    dropdown_value_store: Rc<UIKitPreviewDropdownValueStore>,
    rgc1: ViewGroupID,
}
impl UIKitPreviewPanePresenter {
    const ID: &str = internal_pane_identifier!("UIKitPreview");

    #[profiler::instrument("PaneInitialize.UIKitPreview")]
    pub fn new(ctx: &mut ViewInitContext) -> Self {
        // TODO: ペイン内コンテンツのFocusGroupどうするか......(いったんペイン内ローカルでつくる)
        let kf_group = ctx.keyboard_focus_registry.acquire_group();

        let content_view = ctx.construct_view(ContainerViewInit, |_| []);
        {
            let l = ctx.view_layout_mut(content_view).expect("query failed");
            l.width = ViewSize::Fixed(256.0);
            l.padding.set_all(8.0);
            l.child = ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Vertical,
                alignment: ViewLayoutFlowAlignment::Start,
                justify: ViewLayoutFlowJustify::Start,
                overflow: ViewLayoutOverflow::Overflow,
                gap: 8.0,
            };
        }

        struct AlertButtonEventHandler(String);
        impl SimpleButtonEventHandler for AlertButtonEventHandler {
            #[inline(always)]
            fn on_click_event(&self, window: WindowHandle) -> Event {
                Event::OpenAlertDialog {
                    target_window: window,
                    message: self.0.clone(),
                }
            }
        }

        let container = ctx.construct_view(ContainerViewInit, |ctx| {
            let label = ctx.construct_view(
                StaticTextViewInit {
                    content: "Simple Buttons + Alert Dialog".into(),
                    ..Default::default()
                },
                |_| [],
            );

            let button_container = ctx.construct_view(ContainerViewInit, |ctx| {
                const LONG_MESSAGE: &str = "とてもとても長いメッセージで自動折り返しをしてみる ああああああああああああああああああああああああああああああ";

                    [ctx.construct_view(SimpleButtonViewInit {
                         label: "Test Alert".into(),
                                        event_handler: Some(Box::new(AlertButtonEventHandler(
                                            "てすとめっせーじ from button\n改行もしてみる".into(),
                                        ))),
                                    }, |_| []).into_untyped(), ctx.construct_view(SimpleButtonViewInit {
                                        label: "Test Alert 2".into(),
                                        event_handler: Some(Box::new(AlertButtonEventHandler(LONG_MESSAGE.into()))),
                                    }, |_| []).into_untyped()]
            });
            {
                let button_container = ctx.view_layout_mut(button_container).expect("query failed");
                button_container.padding.left = 8.0;
                button_container.width = ViewSize::FillAvailable;
                button_container.child = ViewLayoutChild::Flow {
                    direction: ViewLayoutFlowDirection::Horizontal,
                    alignment: ViewLayoutFlowAlignment::Start,
                    justify: ViewLayoutFlowJustify::Start,
                    overflow: ViewLayoutOverflow::Overflow,
                    gap: 8.0,
                };
            }

            [label.into_untyped(), button_container.into_untyped()]
        });
        {
            let container = ctx.view_layout_mut(container).expect("query failed");
            container.child = ViewLayoutChild::Flow {
                direction: ViewLayoutFlowDirection::Vertical,
                alignment: ViewLayoutFlowAlignment::Start,
                justify: ViewLayoutFlowJustify::Start,
                overflow: ViewLayoutOverflow::Overflow,
                gap: 0.0,
            };
        }
        ctx.view_set_parent(container, content_view);

        let text_input_backing_store1 =
            Rc::new(UIKitPreviewTextInputValueStore(RefCell::new(String::new())));
        let text_input_backing_store2 =
            Rc::new(UIKitPreviewTextInputValueStore(RefCell::new(String::new())));

        let container = ctx.construct_view(ContainerViewInit, |ctx| {
            [
                ctx.construct_view(
                    StaticTextViewInit {
                        content: "Text Input(Single Line)".into(),
                        ..Default::default()
                    },
                    |_| [],
                )
                .into_untyped(),
                {
                    let v = ctx.construct_view(ContainerViewInit, |ctx| {
                        [
                            {
                                let v = ctx.construct_view_direct(|id| {
                                    Box::new(TextInputView::new(
                                        id,
                                        Rc::downgrade(&text_input_backing_store1),
                                    ))
                                });
                                let l = ctx.view_layout_mut(v).expect("query failed");
                                l.width = ViewSize::Fixed(128.0);
                                l.height = ViewSize::Fixed(20.0);
                                v
                            }
                            .into_untyped(),
                            {
                                let v = ctx.construct_view_direct(|id| {
                                    Box::new(TextInputView::new(
                                        id,
                                        Rc::downgrade(&text_input_backing_store2),
                                    ))
                                });
                                let l = ctx.view_layout_mut(v).expect("query failed");
                                l.width = ViewSize::Fixed(128.0);
                                l.height = ViewSize::Fixed(20.0);
                                v
                            }
                            .into_untyped(),
                        ]
                    });
                    let l = ctx.view_layout_mut(v).expect("query failed");
                    l.padding.left = 8.0;
                    l.child = ViewLayoutChild::Flow {
                        direction: ViewLayoutFlowDirection::Vertical,
                        alignment: ViewLayoutFlowAlignment::Start,
                        justify: ViewLayoutFlowJustify::Start,
                        overflow: ViewLayoutOverflow::Overflow,
                        gap: 4.0,
                    };
                    v
                }
                .into_untyped(),
            ]
        });
        ctx.view_layout_mut(container).expect("query failed").child = ViewLayoutChild::Flow {
            direction: ViewLayoutFlowDirection::Vertical,
            alignment: ViewLayoutFlowAlignment::Start,
            justify: ViewLayoutFlowJustify::Start,
            overflow: ViewLayoutOverflow::Overflow,
            gap: 0.0,
        };
        ctx.view_set_parent(container, content_view);

        let container = ctx.construct_view(ContainerViewInit, |ctx| {
            [
                ctx.construct_view(
                    StaticTextViewInit {
                        content: "Text Input (Multiline)".into(),
                        ..Default::default()
                    },
                    |_| [],
                )
                .into_untyped(),
                {
                    let v = ctx.construct_view_direct(|id| {
                        Box::new(uikit::MultilineTextInputView::new(id))
                    });
                    let l = ctx.view_layout_mut(v).expect("query failed");
                    l.width = ViewSize::FillAvailable;
                    l.height = ViewSize::Fixed(100.0);
                    v
                }
                .into_untyped(),
            ]
        });
        ctx.view_layout_mut(container).expect("query failed").child = ViewLayoutChild::Flow {
            direction: ViewLayoutFlowDirection::Vertical,
            alignment: ViewLayoutFlowAlignment::Start,
            justify: ViewLayoutFlowJustify::Start,
            overflow: ViewLayoutOverflow::Overflow,
            gap: 0.0,
        };
        ctx.view_layout_mut(container).expect("query failed").width = ViewSize::FillAvailable;
        ctx.view_set_parent(container, content_view);

        let color_picker_backing_store = Rc::new(ColorPickerTestBackingStore {
            color: Cell::new(0xffffffff),
        });
        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Color Picker(Standalone)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, content_view);
        let color_picker = ColorPickerView::new(
            ctx.alloc_view_id_without_instance(),
            Rc::downgrade(&color_picker_backing_store),
        );
        let color_picker = ctx.construct_view_direct(|_| Box::new(color_picker));
        ctx.view_set_parent(color_picker, content_view);

        let toggle_button =
            ctx.construct_view_direct(|_| Box::new(uikit::ToggleButtonView::new("Toggle".into())));
        ctx.view_set_parent(toggle_button, content_view);

        // inline controls preview
        let container = ctx.construct_view(ContainerViewInit, |_| []);
        ctx.view_set_parent(container, content_view);
        ctx.view_layout_mut(container).expect("query failed").child = ViewLayoutChild::Grid {
            cols: vec![
                ViewLayoutGridCell::Flexible(1.0),
                ViewLayoutGridCell::FixedFitContent,
            ],
            rows: vec![ViewLayoutGridCell::FixedFitContent],
            gap_cols: 4.0,
            gap_rows: 4.0,
        };
        ctx.view_layout_mut(container).expect("query failed").width = ViewSize::FillAvailable;

        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Color Picker(Button Style)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let editable_color_button =
            ctx.construct_view_direct(|id| Box::new(EditableColorButtonView::new(id, 0xffffffff)));
        {
            let l = ctx
                .view_layout_mut(editable_color_button)
                .expect("query failed");
            l.width = ViewSize::Fixed(64.0);
            l.height = ViewSize::Fixed(20.0);
        }
        ctx.view_set_parent(editable_color_button, container);

        let numeric_input_view_backing_store =
            Rc::new(UIKitPreviewNumericInputValueStore(Cell::new(0)));
        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Numeric Input".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let numeric_input_view = ctx.construct_view(
            NumericInputViewInit {
                value: Rc::downgrade(&numeric_input_view_backing_store),
                ..Default::default()
            },
            |_| [],
        );
        {
            let l = ctx
                .view_layout_mut(numeric_input_view)
                .expect("query failed");
            l.width = ViewSize::Fixed(64.0);
            l.height = ViewSize::Fixed(20.0);
        }
        ctx.view_set_parent(numeric_input_view, container);

        let dropdown_value_store = Rc::new(UIKitPreviewDropdownValueStore(Cell::new(0)));
        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Dropdown".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let dropdown_box = ctx.construct_view_direct(|id| {
            Box::new(uikit::dropdown_box::View::new(
                id,
                Rc::downgrade(&dropdown_value_store),
                vec![
                    "DropdownBox Item 1".into(),
                    "DropdownBox Item 2".into(),
                    "DropdownBox Item 3 too long version".into(),
                ],
            ))
        });
        {
            let l = ctx.view_layout_mut(dropdown_box).expect("query failed");
            l.width = ViewSize::Fixed(80.0);
            l.height = ViewSize::Fixed(24.0);
        }
        ctx.view_set_parent(dropdown_box, container);

        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Single Checkbox".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let checkbox = ctx.construct_view_direct(|_| Box::new(uikit::CheckboxView::new()));
        ctx.view_set_parent(checkbox, container);

        let rgc1 = ctx.create_view_group();
        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Radio Button (Group 1)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let radio_button1 = ctx.construct_view_direct(|id| Box::new(RadioButtonView::new(id)));
        ctx.join_view_group(radio_button1, rgc1);
        ctx.view_set_parent(radio_button1, container);
        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Radio Button (Group 1)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let radio_button2 = ctx.construct_view_direct(|id| Box::new(RadioButtonView::new(id)));
        ctx.join_view_group(radio_button2, rgc1);
        ctx.view_set_parent(radio_button2, container);
        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Radio Button (Group 1)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let radio_button3 = ctx.construct_view_direct(|id| Box::new(RadioButtonView::new(id)));
        ctx.join_view_group(radio_button3, rgc1);
        ctx.view_set_parent(radio_button3, container);
        let label = ctx.construct_view(
            StaticTextViewInit {
                content: "Radio Button (No group)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let radio_button4 = ctx.construct_view_direct(|id| Box::new(RadioButtonView::new(id)));
        ctx.view_set_parent(radio_button4, container);

        let scroll_container = ctx.construct_view(ScrollContainerInit::new(content_view), |_| {
            [content_view.into_untyped()]
        });

        Self {
            kf_group,
            scroll_container,
            content_view,
            text_input_backing_store1,
            text_input_backing_store2,
            color_picker_backing_store,
            numeric_input_view_backing_store,
            dropdown_value_store,
            rgc1,
        }
    }
}
impl ui::dock::PaneContentPresenter for UIKitPreviewPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "uikit on stage".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.scroll_container.into_untyped()
    }

    fn resize(&self, new_size: &Size<LogicalUnit>, context: &mut PaneContentResizeContext) {
        // tracing::debug!(?new_size, "resize pane");
        let content_width = new_size.width.max(128.0);
        context
            .view_layout_mut(self.content_view)
            .expect("query failed")
            .width = ViewSize::Fixed(content_width);
        context
            .view_layout_mut(self.scroll_container)
            .expect("query failed")
            .width = ViewSize::Fixed(new_size.width);
        context
            .view_layout_mut(self.scroll_container)
            .expect("query failed")
            .height = ViewSize::Fixed(new_size.height);
        context.schedule_view_render(self.scroll_container);
    }
}

struct TimelinePanePresenter {
    root_view_id: TypedViewIdentifier<ContainerView>,
}
impl TimelinePanePresenter {
    const ID: &str = internal_pane_identifier!("Timeline");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        Self {
            root_view_id: ctx.construct_view_direct(|_| Box::new(ContainerView)),
        }
    }
}
impl ui::dock::PaneContentPresenter for TimelinePanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Timeline".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view_id.into_untyped()
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}

struct ProjectSettingsPanePresenter {
    root_view_id: TypedViewIdentifier<ContainerView>,
}
impl ProjectSettingsPanePresenter {
    const ID: &str = internal_pane_identifier!("ProjectSettings");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        Self {
            root_view_id: ctx.construct_view_direct(|_| Box::new(ContainerView)),
        }
    }
}
impl ui::dock::PaneContentPresenter for ProjectSettingsPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Project Settings".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view_id.into_untyped()
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}

struct AssetPreviewPanePresenter {
    root_view_id: TypedViewIdentifier<ContainerView>,
}
impl AssetPreviewPanePresenter {
    const ID: &str = internal_pane_identifier!("AssetPreview");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        Self {
            root_view_id: ctx.construct_view_direct(|_| Box::new(ContainerView)),
        }
    }
}
impl ui::dock::PaneContentPresenter for AssetPreviewPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Asset Preview".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view_id.into_untyped()
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}

struct PerWindowData {
    screen_reposition_interests: HashSet<HitTestTreeRef>,
    root_view: TypedViewIdentifier<WindowRootView>,
    header: ui::window_header::Component,
    appmenu: Option<TypedViewIdentifier<ui::app_menu_bar::View>>,
    footer: Option<TypedViewIdentifier<ui::window_footer::View>>,
    docking_manager: ui::dock::WindowDockingManager,
}
impl PerWindowData {
    fn compute_content_area(&self, surface_size: Size<LogicalUnit>) -> Rect<LogicalUnit> {
        let top_offset = if self.appmenu.is_some() {
            ui::window_header::View::THICKNESS + ui::app_menu_bar::View::HEIGHT
        } else {
            ui::window_header::View::THICKNESS
        };
        let bottom_offset = if self.footer.is_some() {
            ui::window_footer::View::THICKNESS
        } else {
            0.0
        };

        Rect::from_lt_size(
            Point::new_logical(0.0, top_offset),
            Size::new_logical(
                surface_size.width,
                surface_size.height - top_offset - bottom_offset,
            ),
        )
    }
}

struct WindowRootView {}
impl View for WindowRootView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        _ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        ViewRenderElements::EMPTY
    }

    fn teardown(&mut self, _ctx: &mut TeardownContext) {}

    fn measure_preferred_content_size(&self, ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }
}

struct LaunchArgs<'sys> {
    pub event_queue: EventQueue,
    pub global_time_base: &'sys std::time::Instant,
    pub renderer_sync: &'sys Mutex<RendererSync>,
    pub file_system: &'sys FileSystem,
    pub committed_preview_state: &'sys Mutex<rendering::preview::CommittedState>,
}

profiler::section!(INITIALIZE = "LogicFiber.Initialize");
profiler::section!(PROCESS_EVENT = "LogicFiber.ProcessEvent");
profiler::section!(LOCK_WAIT = "Mutex.LockWait");

pub struct CoreLoop<'h, 'sys> {
    syslink: SystemLink<'sys>,
    fs: &'sys FileSystem,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
    committed_preview_state: &'sys Mutex<rendering::preview::CommittedState>,
    // base model
    application: Application,
    // base functionalities
    composite_tree: CompositeTree<SyncEvent>,
    ht_manager: HitTestTreeManager<'h>,
    keyboard_focus_registry: KeyboardFocusTokenRegistry,
    pointer_input_manager: PointerInputManager,
    texture_id_issuer: MainThreadTextureIDIssuer,
    delayed_render_messages: Vec<RenderMessage>,
    // view management
    view_allocator: ViewIdentifierAllocator,
    view_instance_store: ViewInstanceStore,
    view_tree_relation_store: ViewTreeRelationStore,
    view_group_relation_store: ViewGroupRelationStore,
    view_layout_state_store: ViewLayoutStateStore,
    view_render_state_store: ViewRenderStateStore,
    view_render_queue: ViewRenderQueue,
    view_feedback_registry: ViewFeedbackRegistry,
    view_feedback_registry_delayed_ops: VecDeque<ViewFeedbackRegistryDelayedOps>,
    view_feedback_store: NonDropAnyTypeQueue,
    // view common res
    window_bg_gradient: GradientRef,
    context_menu_common_resources: MenuItemCommonResources,
    // window management
    main_window: WindowHandle,
    sub_windows: HashSet<WindowHandle>,
    // high-level functionalities
    popup_manager: PopupManager,
    dock_store: ui::dock::DockStore,
    menu_open_requests: Vec<MenuOpenRequest>,
    menu_reopen_request: Option<MenuOpenRequest>,
    current_active_menu_session: Option<MenuSession>,
    dropdown_menu_open_requests: Vec<DropdownMenuOpenRequest>,
    current_active_dropdown_menu_session: Option<DropdownMenuSession>,
    custom_view_flyout_session: Option<CustomViewFlyoutSession>,
    docking_preview_state: Option<ui::dock::DockingPreviewState>,
    // preview
    preview_input_state: core::pin::Pin<Box<PreviewInputState>>,
    preview_state: PreviewMainThreadState,
    // should be pinned
    _marker: core::marker::PhantomPinned,
}
impl<'sys> CoreLoop<'static, 'sys> {
    pub fn new(
        syslink: SystemLink<'sys>,
        fs: &'sys FileSystem,
        global_time_base: &'sys std::time::Instant,
        renderer_sync: &'sys Mutex<RendererSync>,
        committed_preview_state: &'sys Mutex<rendering::preview::CommittedState>,
    ) -> Self {
        // いくつかは現時点では初期化できない いまのところどれもdropしない型なので適当な値で埋めておく
        Self {
            syslink,
            fs,
            global_time_base,
            renderer_sync,
            committed_preview_state,
            // base model
            application: Application::new(),
            // base functionalities
            composite_tree: CompositeTree::new(),
            ht_manager: HitTestTreeManager::new(),
            keyboard_focus_registry: KeyboardFocusTokenRegistry::new(),
            pointer_input_manager: PointerInputManager::new(),
            texture_id_issuer: MainThreadTextureIDIssuer::new(),
            delayed_render_messages: Vec::new(),
            // view management
            view_allocator: ViewIdentifierAllocator::new(),
            view_instance_store: ViewInstanceStore::new(),
            view_tree_relation_store: ViewTreeRelationStore::new(),
            view_group_relation_store: ViewGroupRelationStore::new(),
            view_layout_state_store: ViewLayoutStateStore::new(),
            view_render_state_store: ViewRenderStateStore::new(),
            view_render_queue: ViewRenderQueue::new(),
            view_feedback_registry: ViewFeedbackRegistry::new(),
            view_feedback_registry_delayed_ops: VecDeque::new(),
            view_feedback_store: NonDropAnyTypeQueue::new(),
            // view common res
            #[allow(invalid_value)]
            window_bg_gradient: unsafe { core::mem::MaybeUninit::uninit().assume_init() },
            #[allow(invalid_value)]
            context_menu_common_resources: unsafe {
                core::mem::MaybeUninit::uninit().assume_init()
            },
            // window management
            #[allow(invalid_value)]
            main_window: unsafe { core::mem::MaybeUninit::uninit().assume_init() },
            sub_windows: HashSet::new(),
            // high-level functionalities
            popup_manager: PopupManager::new(),
            dock_store: ui::dock::DockStore::new(),
            menu_open_requests: Vec::new(),
            menu_reopen_request: None,
            current_active_menu_session: None,
            dropdown_menu_open_requests: Vec::new(),
            current_active_dropdown_menu_session: None,
            custom_view_flyout_session: None,
            docking_preview_state: None,
            // preview
            preview_input_state: Box::pin(PreviewInputState::new()),
            preview_state: PreviewMainThreadState::new(),
            // should pinned
            _marker: core::marker::PhantomPinned,
        }
    }

    #[profiler::instrument("CoreLoop.Initialize")]
    pub fn init(mut self: core::pin::Pin<&mut Self>) {
        // WindowsではWM_NCHITTESTの返り値の計算に必要なので一旦生ポインタで参照もたせる（実際どうするかはあとで考える）
        #[cfg(windows)]
        unsafe {
            platform::windows::locate_non_client_hittest_managers(
                &self.pointer_input_manager,
                &self.ht_manager,
            );
        }

        let this = unsafe { self.as_mut().get_unchecked_mut() };
        this.context_menu_common_resources = MenuItemCommonResources::new(
            &mut this.composite_tree,
            &mut this.texture_id_issuer,
            this.syslink.rt_sender(),
        );
        this.window_bg_gradient = this.composite_tree.create_gradient(Gradient::Corner {
            right_top: [0.1, 0.1, 0.1, 1.0],
            left_bottom: [0.1, 0.1, 0.1, 1.0],
            right_bottom: [0.05, 0.025, 0.0, 1.0],
        });

        let last_window_state = 'try_restore_last_window_state: {
            let fp = match std::fs::File::open(this.fs.window_state_save_path()) {
                Ok(fp) => fp,
                Err(e) => {
                    tracing::warn!(reason = %e, "persist.open.window_state");
                    break 'try_restore_last_window_state None;
                }
            };
            match PersistStateWindowData::deserialize(&mut std::io::BufReader::new(fp)) {
                Ok(state) => Some(state),
                Err(e) => {
                    tracing::warn!(reason = %e, "persist.restore.window_state");
                    break 'try_restore_last_window_state None;
                }
            }
        };

        let main_window = create_main_window(
            match last_window_state {
                None => MainWindowOpenMode::New,
                Some(ref x) => MainWindowOpenMode::Restore(x.main.geometry.clone()),
            },
            self.as_mut(),
        );
        let this = unsafe { self.as_mut().get_unchecked_mut() };
        this.main_window = main_window;

        let mut view_init_ctx = ViewInitContext {
            mount_context: MountContext {
                composite_tree: &mut this.composite_tree,
                ht_manager: &mut this.ht_manager,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                keyboard_focus_registry: &mut this.keyboard_focus_registry,
            },
            view_allocator: &mut this.view_allocator,
            view_instance_store: &mut this.view_instance_store,
            view_tree_relation_store: &mut this.view_tree_relation_store,
            view_group_relation_store: &mut this.view_group_relation_store,
            view_layout_state_store: &mut this.view_layout_state_store,
            view_render_state_store: &mut this.view_render_state_store,
            view_feedback_subscription_delayed_ops: &mut this.view_feedback_registry_delayed_ops,
            system_link: &this.syslink,
            main_thread_texture_id_issuer: &mut this.texture_id_issuer,
            application: &this.application,
        };

        view_init_ctx
            .composite_tree
            .begin_mod_chain(this.main_window.ct_root())
            .has_bitmap(true)
            .composite_mode(CompositeMode::FillCornerGradient(
                this.window_bg_gradient,
                AnimatableColor::Value([0.0, 0.025, 0.05, 1.0]),
            ))
            .apply();
        let main_window_root_view =
            view_init_ctx.construct_view_direct(|_| Box::new(WindowRootView {}));
        let window_header = ui::window_header::Component::new(
            ui::window_header::Caption::Main,
            ui::window_header::ComponentInit {
                with_system_command_buttons: this.main_window.needs_system_command_buttons(),
            },
            &mut view_init_ctx,
        );
        view_init_ctx.view_set_parent_untyped(
            window_header.root_view(),
            main_window_root_view.into_untyped(),
        );

        let app_menu_view = if this.syslink.needs_app_menu_in_surface() {
            let app_menu_view = view_init_ctx.construct_view_direct(|_| {
                Box::new(ui::app_menu_bar::View::new(
                    ui::window_header::View::THICKNESS,
                    vec![
                        (
                            "ファイル(F)".into(),
                            vec![
                                MenuItem::Command {
                                    label: "新規プロジェクト...".into(),
                                    command_id: 0,
                                },
                                MenuItem::Command {
                                    label: "新規ファイル...".into(),
                                    command_id: 0,
                                },
                                MenuItem::Separator,
                                MenuItem::Command {
                                    label: "プロジェクトを開く...".into(),
                                    command_id: 0,
                                },
                                MenuItem::Command {
                                    label: "保存".into(),
                                    command_id: 0,
                                },
                                MenuItem::Command {
                                    label: "名前をつけて保存...".into(),
                                    command_id: 0,
                                },
                                MenuItem::Separator,
                                MenuItem::Command {
                                    label: "Peridot Marble Editor を終了".into(),
                                    command_id: 1000,
                                },
                            ],
                        ),
                        (
                            "編集(E)".into(),
                            vec![MenuItem::Command {
                                label: "項目2".into(),
                                command_id: 1,
                            }],
                        ),
                        (
                            "ウィンドウ(W)".into(),
                            vec![
                                MenuItem::Command {
                                    label: "項目3".into(),
                                    command_id: 2,
                                },
                                MenuItem::SubMenu {
                                    label: "その他".into(),
                                    items: vec![
                                        MenuItem::Command {
                                            label: "ウィンドウ1".into(),
                                            command_id: 201,
                                        },
                                        MenuItem::Command {
                                            label: "ウィンドウ2".into(),
                                            command_id: 202,
                                        },
                                    ],
                                },
                            ],
                        ),
                        (
                            "ヘルプ(H)".into(),
                            vec![
                                MenuItem::Command {
                                    label: "項目4".into(),
                                    command_id: 3,
                                },
                                MenuItem::Command {
                                    label: "バージョン情報".into(),
                                    command_id: 100,
                                },
                            ],
                        ),
                    ],
                ))
            });
            view_init_ctx.view_set_parent(app_menu_view, main_window_root_view);
            Some(app_menu_view)
        } else {
            None
        };

        let window_footer_view =
            view_init_ctx.construct_view_direct(|_| Box::new(ui::window_footer::View::new()));
        view_init_ctx.view_set_parent(window_footer_view, main_window_root_view);

        let initial_dock_state = initial_dock_state();
        let dock_top_offset = ui::window_header::View::THICKNESS
            + if app_menu_view.is_some() {
                ui::app_menu_bar::View::HEIGHT
            } else {
                0.0
            };
        let main_window_size = this.main_window.client_size();
        this.main_window
            .associate_extra_data(Box::new(PerWindowData {
                screen_reposition_interests: HashSet::new(),
                root_view: main_window_root_view,
                header: window_header,
                appmenu: app_menu_view,
                footer: Some(window_footer_view),
                docking_manager: ui::dock::WindowDockingManager::new(
                    this.main_window,
                    &mut view_init_ctx,
                    &mut this.view_render_queue,
                    Rect::from_lt_size(
                        Point::new_logical(0.0, dock_top_offset),
                        Size::new_logical(
                            main_window_size.width,
                            main_window_size.height
                                - dock_top_offset
                                - ui::window_footer::View::THICKNESS,
                        ),
                    ),
                    &mut this.dock_store,
                    |view_init_ctx, view_render_queue, store| {
                        construct_dock_from_state(
                            match last_window_state {
                                None => &initial_dock_state,
                                Some(ref x) => &x.main.dock,
                            },
                            this.main_window.keyboard_focus_group(),
                            &mut PaneGroupCreateContext {
                                view_init_context: view_init_ctx,
                                view_render_queue,
                            },
                            store,
                            |id, view_init_ctx| match id {
                                // TODO: このへんうまい具合にRegistryつくりたい
                                UIKitPreviewPanePresenter::ID => {
                                    Box::new(UIKitPreviewPanePresenter::new(view_init_ctx))
                                }
                                ui::pane::object_tree::Presenter::ID => {
                                    Box::new(ui::pane::object_tree::Presenter::new(view_init_ctx))
                                }
                                ui::pane::inspector::Presenter::ID => {
                                    Box::new(ui::pane::inspector::Presenter::new(view_init_ctx))
                                }
                                ui::pane::asset_explorer::Presenter::ID => Box::new(
                                    ui::pane::asset_explorer::Presenter::new(view_init_ctx),
                                ),
                                ProjectSettingsPanePresenter::ID => {
                                    Box::new(ProjectSettingsPanePresenter::new(view_init_ctx))
                                }
                                TimelinePanePresenter::ID => {
                                    Box::new(TimelinePanePresenter::new(view_init_ctx))
                                }
                                AssetPreviewPanePresenter::ID => {
                                    Box::new(AssetPreviewPanePresenter::new(view_init_ctx))
                                }
                                PreviewPanePresenter::ID => Box::new(PreviewPanePresenter::new(
                                    view_init_ctx,
                                    this.preview_input_state.as_mut().get_mut(),
                                )),
                                id => todo!("generic pane id handling: {id:?}"),
                            },
                        )
                    },
                ),
            }));

        view_init_ctx.render_view_with_base(
            main_window_root_view.into_untyped(),
            &this.main_window,
            this.main_window.keyboard_focus_group(),
            Rect::from_lt_size(Point::new_logical(0.0, 0.0), this.main_window.client_size()),
        );

        if let Some(ref last_window_state) = last_window_state {
            for sub in last_window_state.sub.iter() {
                let new_window = open_sub_window(
                    SubWindowOpenMode::Restore(sub.geometry.clone()),
                    self.as_mut(),
                    |mut w, coreloop| {
                        let this = unsafe { coreloop.get_unchecked_mut() };
                        this.ht_manager.get_data_mut(w.ht_root()).root_of_window = Some(w);

                        this.composite_tree
                            .begin_mod_chain(w.ct_root())
                            .has_bitmap(true)
                            .composite_mode(CompositeMode::FillCornerGradient(
                                this.window_bg_gradient,
                                AnimatableColor::Value([0.0, 0.025, 0.05, 1.0]),
                            ))
                            .apply();

                        let mut view_init_ctx = ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut this.composite_tree,
                                ht_manager: &mut this.ht_manager,
                                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                                keyboard_focus_registry: &mut this.keyboard_focus_registry,
                            },
                            view_allocator: &mut this.view_allocator,
                            view_instance_store: &mut this.view_instance_store,
                            view_tree_relation_store: &mut this.view_tree_relation_store,
                            view_group_relation_store: &mut this.view_group_relation_store,
                            view_layout_state_store: &mut this.view_layout_state_store,
                            view_render_state_store: &mut this.view_render_state_store,
                            view_feedback_subscription_delayed_ops: &mut this
                                .view_feedback_registry_delayed_ops,
                            system_link: &this.syslink,
                            main_thread_texture_id_issuer: &mut this.texture_id_issuer,
                            application: &this.application,
                        };
                        let root_view =
                            view_init_ctx.construct_view_direct(|_| Box::new(WindowRootView {}));
                        let window_header_view = ui::window_header::Component::new(
                            ui::window_header::Caption::Sub,
                            ui::window_header::ComponentInit {
                                with_system_command_buttons: w.needs_system_command_buttons(),
                            },
                            &mut view_init_ctx,
                        );
                        view_init_ctx.view_set_parent_untyped(
                            window_header_view.root_view(),
                            root_view.into_untyped(),
                        );

                        view_init_ctx.render_view_with_base(
                            root_view.into_untyped(),
                            &w,
                            w.keyboard_focus_group(),
                            Rect::from_lt_size(Point::new_logical(0.0, 0.0), w.client_size()),
                        );

                        w.associate_extra_data(Box::new(PerWindowData {
                            root_view: root_view,
                            screen_reposition_interests: HashSet::new(),
                            header: window_header_view,
                            appmenu: None,
                            footer: None,
                            docking_manager: ui::dock::WindowDockingManager::new(
                                w,
                                &mut view_init_ctx,
                                &mut this.view_render_queue,
                                Rect::from_lt_size(
                                    Point::new_logical(0.0, ui::window_header::View::THICKNESS),
                                    Size::new_logical(320.0, 240.0),
                                ),
                                &mut this.dock_store,
                                |view_init_ctx, view_render_queue, store| {
                                    construct_dock_from_state(
                                        &sub.dock,
                                        w.keyboard_focus_group(),
                                        &mut PaneGroupCreateContext {
                                            view_init_context: view_init_ctx,
                                            view_render_queue,
                                        },
                                        store,
                                        |id, view_init_ctx| match id {
                                            // TODO: このへんうまい具合にRegistryつくりたい
                                            UIKitPreviewPanePresenter::ID => Box::new(
                                                UIKitPreviewPanePresenter::new(view_init_ctx),
                                            ),
                                            ui::pane::object_tree::Presenter::ID => {
                                                Box::new(ui::pane::object_tree::Presenter::new(
                                                    view_init_ctx,
                                                ))
                                            }
                                            ui::pane::inspector::Presenter::ID => Box::new(
                                                ui::pane::inspector::Presenter::new(view_init_ctx),
                                            ),
                                            ui::pane::asset_explorer::Presenter::ID => {
                                                Box::new(ui::pane::asset_explorer::Presenter::new(
                                                    view_init_ctx,
                                                ))
                                            }
                                            ProjectSettingsPanePresenter::ID => Box::new(
                                                ProjectSettingsPanePresenter::new(view_init_ctx),
                                            ),
                                            TimelinePanePresenter::ID => {
                                                Box::new(TimelinePanePresenter::new(view_init_ctx))
                                            }
                                            AssetPreviewPanePresenter::ID => Box::new(
                                                AssetPreviewPanePresenter::new(view_init_ctx),
                                            ),
                                            PreviewPanePresenter::ID => {
                                                Box::new(PreviewPanePresenter::new(
                                                    view_init_ctx,
                                                    this.preview_input_state.as_mut().get_mut(),
                                                ))
                                            }
                                            id => todo!("generic pane id handling: {id:?}"),
                                        },
                                    )
                                },
                            ),
                        }));
                    },
                );
                unsafe { self.as_mut().get_unchecked_mut() }
                    .sub_windows
                    .insert(new_window);
            }
        }

        // process delayed ops before first model sync
        self.as_mut().process_delayed_view_feedback_registry_ops();

        // initial sync model with view
        let this = unsafe { self.as_mut().get_unchecked_mut() };
        this.application.sync(&mut this.view_feedback_store);

        // final sync
        self.as_mut().dispatch_view_feedback();
        self.as_mut().update_view();
        self.as_mut().sync_threads();
        self.as_mut().process_delayed_view_feedback_registry_ops();

        let this = unsafe { self.get_unchecked_mut() };
        this.ht_manager.dump(this.main_window.ht_root());
        this.syslink.prelaunch(this.main_window);
    }

    fn close_sub_window(mut self: core::pin::Pin<&mut Self>, mut target: WindowHandle) {
        let wd = unsafe { target.take_extra_data::<PerWindowData>() };
        struct LocalContext<'a, 'h, 'sys>(ViewInitContext<'a, 'h, 'sys>);
        impl ViewDestructionContext for LocalContext<'_, '_, '_> {
            fn destruct_view_recursive_untyped(&mut self, target: ViewIdentifier) {
                uicore::destruct_view_recursive(
                    target,
                    &mut TeardownContext {
                        composite_tree: &mut self.0.mount_context.composite_tree,
                        ht_manager: &mut self.0.mount_context.ht_manager,
                        keyboard_focus_registry: &mut self.0.mount_context.keyboard_focus_registry,
                        current_sec: self.0.mount_context.current_sec,
                        view_feedback_subscription_delayed_ops: &mut self
                            .0
                            .view_feedback_subscription_delayed_ops,
                    },
                    self.0.view_allocator,
                    self.0.view_instance_store,
                    self.0.view_tree_relation_store,
                    self.0.view_group_relation_store,
                    self.0.view_layout_state_store,
                    self.0.view_render_state_store,
                );
            }
        }

        let this = unsafe { self.as_mut().get_unchecked_mut() };
        wd.docking_manager.teardown(
            &mut this.dock_store,
            &mut LocalContext(ViewInitContext {
                mount_context: MountContext {
                    composite_tree: &mut this.composite_tree,
                    ht_manager: &mut this.ht_manager,
                    current_sec: this.global_time_base.elapsed().as_secs_f32(),
                    keyboard_focus_registry: &mut this.keyboard_focus_registry,
                },
                view_allocator: &mut this.view_allocator,
                view_instance_store: &mut this.view_instance_store,
                view_tree_relation_store: &mut this.view_tree_relation_store,
                view_group_relation_store: &mut this.view_group_relation_store,
                view_layout_state_store: &mut this.view_layout_state_store,
                view_render_state_store: &mut this.view_render_state_store,
                view_feedback_subscription_delayed_ops: &mut this
                    .view_feedback_registry_delayed_ops,
                system_link: &this.syslink,
                main_thread_texture_id_issuer: &mut this.texture_id_issuer,
                application: &this.application,
            }),
        );
        this.sub_windows.remove(&target);
        close_sub_window(target);

        self.as_mut().update_view();
        self.as_mut().process_delayed_view_feedback_registry_ops();
        self.as_mut().sync_threads();
    }

    fn resize_window(
        mut self: core::pin::Pin<&mut Self>,
        target: WindowHandle,
        size: Size<LogicalUnit>,
    ) {
        let this = unsafe { self.as_mut().get_unchecked_mut() };
        let wd = unsafe { target.extra_data_ref::<PerWindowData>() };
        wd.docking_manager.resize(
            wd.compute_content_area(size),
            &mut this.dock_store,
            &mut PaneContentResizeContext {
                view_instance_store: &mut this.view_instance_store,
                view_render_queue: &mut this.view_render_queue,
                composite_tree: &mut this.composite_tree,
                ht_manager: &mut this.ht_manager,
            },
        );

        self.update_view();
    }

    fn handle_window_move(
        self: core::pin::Pin<&mut Self>,
        mut target: WindowHandle,
        pos: Point<LogicalUnit>,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        let wd = unsafe { target.extra_data_mut::<PerWindowData>() };
        let mut input_context = InputEventContext {
            composite_tree: &mut this.composite_tree,
            current_sec: this.global_time_base.elapsed().as_secs_f32(),
            system_link: &mut this.syslink,
            ht_manager: &this.ht_manager,
            dock_store: &mut this.dock_store,
            view_instance_store: &mut this.view_instance_store,
            view_group_relation_store: &this.view_group_relation_store,
            view_render_queue: &mut this.view_render_queue,
            menu_open_requests: &mut this.menu_open_requests,
            menu_reopen_request: &mut this.menu_reopen_request,
            dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
            application: ApplicationMutation {
                state: &mut this.application,
                view_feedbacks: &mut this.view_feedback_store,
            },
        };

        for &ht in wd.screen_reposition_interests.iter() {
            if let Some(e) = this.ht_manager.get_data(ht).screen_reposition_handler() {
                e.on_screen_reposition_required(ht, &mut input_context, pos);
            }
        }

        // ContextMenuはウィンドウ移動で消しちゃう（Explorerもこの挙動っぽい）
        if let Some(c) = this
            .current_active_menu_session
            .take_if(|x| x.parent == target)
        {
            if let Some(ref a) = unsafe { target.extra_data_ref::<PerWindowData>() }.appmenu {
                uicore::view_instance::<ui::app_menu_bar::View>(
                    a.into_untyped(),
                    &this.view_instance_store,
                )
                .expect("query failed")
                .on_close_all(
                    &mut this.composite_tree,
                    this.global_time_base.elapsed().as_secs_f32(),
                );
            }

            c.terminate(
                &this.syslink,
                &mut this.composite_tree,
                &mut this.ht_manager,
                &mut this.keyboard_focus_registry,
            );
        }

        if let Some(mut c) = this
            .current_active_dropdown_menu_session
            .take_if(|x| x.parent == target)
        {
            c.close_all(
                &this.syslink,
                &mut this.composite_tree,
                &mut this.ht_manager,
                &mut this.keyboard_focus_registry,
            );
        }

        if let Some(c) = this
            .custom_view_flyout_session
            .take_if(|x| x.parent == target)
        {
            c.terminate(&mut FlyoutSurfaceSessionTerminateContext {
                syslink: &this.syslink,
                view_allocator: &mut this.view_allocator,
                view_instance_store: &mut this.view_instance_store,
                view_tree_relation_store: &mut this.view_tree_relation_store,
                view_group_relation_store: &mut this.view_group_relation_store,
                view_layout_state_store: &mut this.view_layout_state_store,
                view_render_state_store: &mut this.view_render_state_store,
                teardown_context: TeardownContext {
                    composite_tree: &mut this.composite_tree,
                    ht_manager: &mut this.ht_manager,
                    keyboard_focus_registry: &mut this.keyboard_focus_registry,
                    current_sec: this.global_time_base.elapsed().as_secs_f32(),
                    view_feedback_subscription_delayed_ops: &mut this
                        .view_feedback_registry_delayed_ops,
                },
            });
        }
    }

    fn rescale_popup_of_window(
        self: core::pin::Pin<&mut Self>,
        target: WindowHandle,
        new_scale: f32,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.popup_manager
            .rescale(target, new_scale, &mut this.composite_tree);
    }

    fn handle_window_maximize_state_changes(
        self: core::pin::Pin<&mut Self>,
        target: WindowHandle,
        is_maximized: bool,
    ) {
        struct LocalContext<'a> {
            view_render_queue: &'a mut ViewRenderQueue,
            view_instance_store: &'a mut ViewInstanceStore,
        }
        impl uicore::ViewInstanceQueryableMut for LocalContext<'_> {
            #[inline(always)]
            fn view_instance_mut_of<T: View + 'static>(
                &mut self,
                id: ViewIdentifier,
            ) -> Option<&mut T> {
                uicore::view_instance_mut(id, self.view_instance_store)
            }

            #[inline(always)]
            fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
                uicore::view_set_visibility(id, visible, self.view_instance_store)
            }

            #[inline(always)]
            fn view_layout_mut_untyped(
                &mut self,
                id: ViewIdentifier,
            ) -> Option<&mut uicore::ViewLayout> {
                uicore::view_layout_mut(id, self.view_instance_store)
            }
        }
        impl uicore::ViewRenderer for LocalContext<'_> {
            #[inline(always)]
            fn schedule_view_render_untyped(&mut self, target: ViewIdentifier) {
                self.view_render_queue.schedule(target)
            }
        }

        let this = unsafe { self.get_unchecked_mut() };
        unsafe { target.extra_data_ref::<PerWindowData>() }
            .header
            .set_maximize_state(
                is_maximized,
                &mut LocalContext {
                    view_render_queue: &mut this.view_render_queue,
                    view_instance_store: &mut this.view_instance_store,
                },
            );
    }

    fn handle_window_focus_changed(
        self: core::pin::Pin<&mut Self>,
        mut target: WindowHandle,
        focused: bool,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        let mut input_context = InputEventContext {
            composite_tree: &mut this.composite_tree,
            current_sec: this.global_time_base.elapsed().as_secs_f32(),
            system_link: &mut this.syslink,
            ht_manager: &this.ht_manager,
            dock_store: &mut this.dock_store,
            view_instance_store: &mut this.view_instance_store,
            view_group_relation_store: &this.view_group_relation_store,
            view_render_queue: &mut this.view_render_queue,
            menu_open_requests: &mut this.menu_open_requests,
            menu_reopen_request: &mut this.menu_reopen_request,
            dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
            application: ApplicationMutation {
                state: &mut this.application,
                view_feedbacks: &mut this.view_feedback_store,
            },
        };
        let mgr = target.keyboard_focus_state_mut();

        if focused {
            mgr.notify_window_focus(&mut input_context, &this.keyboard_focus_registry);
        } else {
            mgr.notify_window_lost_focus(&mut input_context, &this.keyboard_focus_registry);
        }

        if !focused
            && let Some(c) = this
                .current_active_menu_session
                .take_if(|x| x.parent == target)
        {
            // フォーカスロストした時もコンテキストメニューを閉じる
            if let Some(ref a) = unsafe { target.extra_data_ref::<PerWindowData>() }.appmenu {
                uicore::view_instance::<ui::app_menu_bar::View>(
                    a.into_untyped(),
                    &this.view_instance_store,
                )
                .expect("query failed")
                .on_close_all(
                    &mut this.composite_tree,
                    this.global_time_base.elapsed().as_secs_f32(),
                );
            }

            c.terminate(
                &this.syslink,
                &mut this.composite_tree,
                &mut this.ht_manager,
                &mut this.keyboard_focus_registry,
            );
        }
    }

    fn handle_window_activation_state_changed(
        self: Pin<&mut Self>,
        target: WindowHandle,
        activated: bool,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        if !activated {
            if let Some(c) = this
                .current_active_menu_session
                .take_if(|x| x.parent == target)
            {
                if let Some(ref a) = unsafe { target.extra_data_ref::<PerWindowData>() }.appmenu {
                    uicore::view_instance::<ui::app_menu_bar::View>(
                        a.into_untyped(),
                        &this.view_instance_store,
                    )
                    .expect("query failed")
                    .on_close_all(
                        &mut this.composite_tree,
                        this.global_time_base.elapsed().as_secs_f32(),
                    );
                }

                c.terminate(
                    &this.syslink,
                    &mut this.composite_tree,
                    &mut this.ht_manager,
                    &mut this.keyboard_focus_registry,
                );
            }
        }
    }

    fn handle_pointer_down(
        self: core::pin::Pin<&mut Self>,
        target: WindowHandle,
        pointer_id: PointerID,
        button: PointerButton,
        key_modifier: ModifierKey,
    ) {
        // #[cfg(target_os = "macos")]
        // drag_preview_popover.bind_position_base_window_link(window);

        let this = unsafe { self.get_unchecked_mut() };
        if let Some(ref a) = unsafe { target.extra_data_ref::<PerWindowData>() }.appmenu {
            uicore::view_instance::<ui::app_menu_bar::View>(
                a.into_untyped(),
                &this.view_instance_store,
            )
            .expect("query failed")
            .on_close_all(
                &mut this.composite_tree,
                this.global_time_base.elapsed().as_secs_f32(),
            );
        }

        if let Some(c) = this.current_active_menu_session.take() {
            if let Some(ref a) = unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu {
                uicore::view_instance::<ui::app_menu_bar::View>(
                    a.into_untyped(),
                    &this.view_instance_store,
                )
                .expect("query failed")
                .on_close_all(
                    &mut this.composite_tree,
                    this.global_time_base.elapsed().as_secs_f32(),
                );
            }

            c.terminate(
                &this.syslink,
                &mut this.composite_tree,
                &mut this.ht_manager,
                &mut this.keyboard_focus_registry,
            );
        }

        if let Some(mut c) = this.current_active_dropdown_menu_session.take() {
            c.close_all(
                &this.syslink,
                &mut this.composite_tree,
                &mut this.ht_manager,
                &mut this.keyboard_focus_registry,
            );
        }

        if let Some(c) = this.custom_view_flyout_session.take() {
            c.terminate(&mut FlyoutSurfaceSessionTerminateContext {
                syslink: &this.syslink,
                view_allocator: &mut this.view_allocator,
                view_instance_store: &mut this.view_instance_store,
                view_tree_relation_store: &mut this.view_tree_relation_store,
                view_group_relation_store: &mut this.view_group_relation_store,
                view_layout_state_store: &mut this.view_layout_state_store,
                view_render_state_store: &mut this.view_render_state_store,
                teardown_context: TeardownContext {
                    composite_tree: &mut this.composite_tree,
                    ht_manager: &mut this.ht_manager,
                    keyboard_focus_registry: &mut this.keyboard_focus_registry,
                    current_sec: this.global_time_base.elapsed().as_secs_f32(),
                    view_feedback_subscription_delayed_ops: &mut this
                        .view_feedback_registry_delayed_ops,
                },
            });
        }

        this.pointer_input_manager.handle_mouse_down(
            pointer_id,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            button,
            key_modifier,
            target.ht_root(),
            &mut this.keyboard_focus_registry,
        );
    }

    fn handle_pointer_move(
        self: core::pin::Pin<&mut Self>,
        target: WindowHandle,
        pointer_id: PointerID,
        client_pos: Point<LogicalUnit>,
        key_modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_mouse_move(
            NativeDesktopSurface::Window(target),
            pointer_id,
            client_pos,
            key_modifier,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            target.ht_root(),
        );

        let cursor_shape = this.pointer_input_manager.cursor_shape(&this.ht_manager);
        this.syslink.set_cursor(&pointer_id, cursor_shape);
    }

    fn handle_pointer_move_relative(
        self: core::pin::Pin<&mut Self>,
        pointer_id: PointerID,
        relative: Point<LogicalUnit>,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_mouse_move_relative(
            pointer_id,
            relative,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
        );
    }

    fn handle_pointer_up(
        self: core::pin::Pin<&mut Self>,
        target: WindowHandle,
        pointer_id: PointerID,
        button: PointerButton,
        key_modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_mouse_up(
            pointer_id,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            button,
            key_modifier,
            target.ht_root(),
        );
    }

    fn handle_pointer_leave_window(self: core::pin::Pin<&mut Self>, pointer_id: PointerID) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_mouse_leave(
            pointer_id,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
        );
    }

    fn handle_pointer_hover_timeout(self: core::pin::Pin<&mut Self>) {
        let this = unsafe { self.get_unchecked_mut() };
        this.syslink.kill_pointer_hovering_timeout();
        this.pointer_input_manager
            .handle_pointer_hover(&mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            });
    }

    fn dispatch_scroll_wheel(
        self: core::pin::Pin<&mut Self>,
        amount: f32,
        key_modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_scroll_wheel(
            amount,
            key_modifier,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
        );
    }

    fn switch_focus_by_key(
        self: core::pin::Pin<&mut Self>,
        mut target: WindowHandle,
        key_modifier: ModifierKey,
    ) {
        let Some(next_focus) = (if key_modifier.contains(ModifierKey::SHIFT) {
            target
                .keyboard_focus_state()
                .prev_focus(&self.keyboard_focus_registry)
        } else {
            target
                .keyboard_focus_state()
                .next_focus(&self.keyboard_focus_registry)
        }) else {
            return;
        };

        let this = unsafe { self.get_unchecked_mut() };
        target.keyboard_focus_state_mut().update_focus_with_event(
            next_focus,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            &this.keyboard_focus_registry,
        );
    }

    fn dispatch_key_down(
        self: core::pin::Pin<&mut Self>,
        target: WindowHandle,
        code: KeyInputCode,
        modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        target.keyboard_focus_state().handle_keydown(
            code,
            modifier,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            &this.keyboard_focus_registry,
        );
    }

    fn dispatch_key_up(
        self: core::pin::Pin<&mut Self>,
        target: WindowHandle,
        code: KeyInputCode,
        modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        target.keyboard_focus_state().handle_keyup(
            code,
            modifier,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            &this.keyboard_focus_registry,
        );
    }

    fn dispatch_key_char(
        self: Pin<&mut Self>,
        target: WindowHandle,
        ch: char,
        modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        target.keyboard_focus_state().handle_char(
            ch,
            modifier,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            &this.keyboard_focus_registry,
        );
    }

    #[cfg(feature = "wayland")]
    fn dispatch_ime_state_changes(
        self: Pin<&mut Self>,
        target: WindowHandle,
        preedit_string: Option<String>,
        committed_string: Option<String>,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        target.keyboard_focus_state().handle_ime_state_changes(
            committed_string.as_deref(),
            preedit_string.as_deref(),
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            &this.keyboard_focus_registry,
        );
    }

    fn close_popup(self: core::pin::Pin<&mut Self>, id: PopupID) {
        let this = unsafe { self.get_unchecked_mut() };
        this.popup_manager.close(
            id,
            &mut RenderContext {
                composite_tree: &mut this.composite_tree,
                ht_manager: &mut this.ht_manager,
                keyboard_focus_registry: &mut this.keyboard_focus_registry,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &this.syslink,
                main_thread_texture_id_issuer: &mut this.texture_id_issuer,
                application: &this.application,
                view_feedback_subscription_delayed_ops: &mut this
                    .view_feedback_registry_delayed_ops,
            },
            &mut this.view_instance_store,
            &this.view_tree_relation_store,
            &mut this.view_layout_state_store,
            &mut this.view_render_state_store,
        );
    }

    fn destroy_popup(self: core::pin::Pin<&mut Self>, id: PopupID) {
        let this = unsafe { self.get_unchecked_mut() };
        this.popup_manager.teardown(
            id,
            &mut this.view_instance_store,
            &mut this.view_tree_relation_store,
            &mut this.view_render_state_store,
            &mut TeardownContext {
                composite_tree: &mut this.composite_tree,
                ht_manager: &mut this.ht_manager,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                keyboard_focus_registry: &mut this.keyboard_focus_registry,
                view_feedback_subscription_delayed_ops: &mut this
                    .view_feedback_registry_delayed_ops,
            },
        );
    }

    fn open_alert_dialog(self: Pin<&mut Self>, target_window: WindowHandle, message: String) {
        let this = unsafe { self.get_unchecked_mut() };
        let opened_id = this.popup_manager.open(
            &mut ViewInitContext {
                mount_context: MountContext {
                    composite_tree: &mut this.composite_tree,
                    ht_manager: &mut this.ht_manager,
                    current_sec: this.global_time_base.elapsed().as_secs_f32(),
                    keyboard_focus_registry: &mut this.keyboard_focus_registry,
                },
                view_allocator: &mut this.view_allocator,
                view_instance_store: &mut this.view_instance_store,
                view_tree_relation_store: &mut this.view_tree_relation_store,
                view_group_relation_store: &mut this.view_group_relation_store,
                view_layout_state_store: &mut this.view_layout_state_store,
                view_render_state_store: &mut this.view_render_state_store,
                view_feedback_subscription_delayed_ops: &mut this
                    .view_feedback_registry_delayed_ops,
                system_link: &this.syslink,
                main_thread_texture_id_issuer: &mut this.texture_id_issuer,
                application: &this.application,
            },
            target_window,
            |id, ctx| uikit::AlertDialogPresenter::new(ctx, id, message, target_window),
        );
        this.popup_manager.post_open_action(
            opened_id,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            &this.keyboard_focus_registry,
        );
    }

    fn open_custom_flyout(
        mut self: Pin<&mut Self>,
        parent: WindowHandle,
        surface_pos: Point<LogicalUnit>,
        view_constructor: Box<dyn FlyoutSurfacePresenterConstructor>,
    ) {
        let custom_view_flyout_session =
            CustomViewFlyoutSession::begin(parent, surface_pos, view_constructor, self.as_mut());
        unsafe { self.get_unchecked_mut() }.custom_view_flyout_session =
            Some(custom_view_flyout_session);
    }

    fn close_all_menus(self: Pin<&mut Self>) {
        let this = unsafe { self.get_unchecked_mut() };
        if let Some(c) = this.current_active_menu_session.take() {
            if let Some(ref a) = unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu {
                uicore::view_instance::<ui::app_menu_bar::View>(
                    a.into_untyped(),
                    &this.view_instance_store,
                )
                .expect("query failed")
                .on_close_all(
                    &mut this.composite_tree,
                    this.global_time_base.elapsed().as_secs_f32(),
                );
            }

            c.terminate(
                &this.syslink,
                &mut this.composite_tree,
                &mut this.ht_manager,
                &mut this.keyboard_focus_registry,
            );
        }
    }

    fn rescale_menu(self: Pin<&mut Self>, new_scale: f32) {
        let this = unsafe { self.get_unchecked_mut() };
        if let Some(ref c) = this.custom_view_flyout_session {
            c.rescale(
                new_scale,
                &mut this.composite_tree,
                &this.ht_manager,
                &this.syslink,
            );
        }
    }

    fn handle_menu_item_selection(self: Pin<&mut Self>, depth: usize, index: usize) {
        let this = unsafe { self.get_unchecked_mut() };
        if let Some(c) = this.current_active_menu_session.as_mut() {
            c.select_item(
                depth,
                index,
                &mut this.composite_tree,
                this.global_time_base.elapsed().as_secs_f32(),
            );

            this.syslink.flyout_surface_context.reserve_delayed_action();
        }
    }

    fn handle_menu_item_deselection(self: Pin<&mut Self>, depth: usize) {
        let this = unsafe { self.get_unchecked_mut() };
        if let Some(c) = this.current_active_menu_session.as_mut() {
            c.deselect_item(
                depth,
                &mut this.composite_tree,
                this.global_time_base.elapsed().as_secs_f32(),
            );

            this.syslink.flyout_surface_context.reserve_delayed_action();
        }
    }

    fn perform_menu_delayed_action(mut self: Pin<&mut Self>) {
        let this = unsafe { self.as_mut().get_unchecked_mut() };
        this.syslink
            .flyout_surface_context
            .unreserve_delayed_action();

        // TODO: ここ生ポインタにして借用関係消さないといけないの微妙なのであとでつくりなおす(というかメニュー関係全部か)
        let thisptr = core::ptr::from_mut(this);
        if let Some(c) = this.current_active_menu_session.as_mut() {
            c.perform_delayed_action(thisptr);
        }
    }

    fn dispatch_menu_pointer_down(
        self: Pin<&mut Self>,
        target: FlyoutSurfaceHandle,
        pointer_id: PointerID,
        button: PointerButton,
        key_modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_mouse_down(
            pointer_id,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            button,
            key_modifier,
            target.ht_root(),
            &mut this.keyboard_focus_registry,
        );
    }

    fn handle_menu_pointer_move(
        self: Pin<&mut Self>,
        target: FlyoutSurfaceHandle,
        pointer_id: PointerID,
        client_pos: Point<PointerInputUnit>,
        key_modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_mouse_move(
            NativeDesktopSurface::ContextMenu(target),
            pointer_id,
            client_pos,
            key_modifier,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            target.ht_root(),
        );

        let cursor_shape = this.pointer_input_manager.cursor_shape(&this.ht_manager);
        this.syslink.set_cursor(&pointer_id, cursor_shape);
    }

    fn dispatch_menu_pointer_up(
        self: Pin<&mut Self>,
        target: FlyoutSurfaceHandle,
        pointer_id: PointerID,
        button: PointerButton,
        key_modifier: ModifierKey,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_mouse_up(
            pointer_id,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
            button,
            key_modifier,
            target.ht_root(),
        );
    }

    fn dispatch_menu_pointer_leave(self: Pin<&mut Self>, pointer_id: PointerID) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.handle_mouse_leave(
            pointer_id,
            &this.ht_manager,
            &mut InputEventContext {
                composite_tree: &mut this.composite_tree,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &mut this.syslink,
                ht_manager: &this.ht_manager,
                dock_store: &mut this.dock_store,
                view_instance_store: &mut this.view_instance_store,
                view_group_relation_store: &this.view_group_relation_store,
                view_render_queue: &mut this.view_render_queue,
                menu_open_requests: &mut this.menu_open_requests,
                menu_reopen_request: &mut this.menu_reopen_request,
                dropdown_menu_open_requests: &mut this.dropdown_menu_open_requests,
                application: ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            },
        );
    }

    fn perform_select_menu_command(self: Pin<&mut Self>, id: u64) {
        let this = unsafe { self.get_unchecked_mut() };
        // コマンド選択したらとじる
        let ch = if let Some(c) = this.current_active_menu_session.take() {
            if let Some(ref a) = unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu {
                uicore::view_instance::<ui::app_menu_bar::View>(
                    a.into_untyped(),
                    &this.view_instance_store,
                )
                .expect("query failed")
                .on_close_all(
                    &mut this.composite_tree,
                    this.global_time_base.elapsed().as_secs_f32(),
                );
            }

            let ch = c.terminate(
                &this.syslink,
                &mut this.composite_tree,
                &mut this.ht_manager,
                &mut this.keyboard_focus_registry,
            );

            this.composite_tree.commit(
                &mut this
                    .renderer_sync
                    .lock()
                    .expect("poisoned")
                    .composite_buffer,
            );

            Some(ch)
        } else {
            None
        };

        if let Some(mut ch) = ch {
            ch.on_select_command(
                id,
                &mut ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
            );
        }
    }

    fn perform_dropdown_menu_select_item(
        self: Pin<&mut Self>,
        id: usize,
        receiver: std::rc::Weak<uikit::dropdown_box::EventHandler>,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        if let Some(r) = receiver.upgrade() {
            struct LocalContext<'env> {
                view_instance_store: &'env mut ViewInstanceStore,
                view_render_queue: &'env mut ViewRenderQueue,
            }
            impl ViewInstanceQueryableMut for LocalContext<'_> {
                #[inline(always)]
                fn view_instance_mut_of<T: View + 'static>(
                    &mut self,
                    id: ViewIdentifier,
                ) -> Option<&mut T> {
                    uicore::view_instance_mut(id, self.view_instance_store)
                }

                #[inline(always)]
                fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
                    uicore::view_set_visibility(id, visible, self.view_instance_store)
                }

                #[inline(always)]
                fn view_layout_mut_untyped(
                    &mut self,
                    id: ViewIdentifier,
                ) -> Option<&mut uicore::ViewLayout> {
                    uicore::view_layout_mut(id, self.view_instance_store)
                }
            }
            impl ViewRenderer for LocalContext<'_> {
                #[inline(always)]
                fn schedule_view_render_untyped(&mut self, target: ViewIdentifier) {
                    self.view_render_queue.schedule(target);
                }
            }
            r.set_selection_id(
                id,
                &mut ApplicationMutation {
                    state: &mut this.application,
                    view_feedbacks: &mut this.view_feedback_store,
                },
                &mut LocalContext {
                    view_instance_store: &mut this.view_instance_store,
                    view_render_queue: &mut this.view_render_queue,
                },
            );
        }

        // 選択したら閉じる
        if let Some(mut c) = this.current_active_dropdown_menu_session.take() {
            c.close_all(
                &this.syslink,
                &mut this.composite_tree,
                &mut this.ht_manager,
                &mut this.keyboard_focus_registry,
            );
        }
    }

    fn move_dock_splitter(self: Pin<&mut Self>, target: ui::dock::DockID, pos_client: f32) {
        let this = unsafe { self.get_unchecked_mut() };
        ui::dock::move_splitter(
            target,
            &mut this.dock_store,
            pos_client,
            &mut PaneContentResizeContext {
                view_instance_store: &mut this.view_instance_store,
                view_render_queue: &mut this.view_render_queue,
                composite_tree: &mut this.composite_tree,
                ht_manager: &mut this.ht_manager,
            },
        );
    }

    fn begin_redock_preview(
        self: Pin<&mut Self>,
        initiator: WindowHandle,
        pointer: PointerID,
        source_dock: ui::dock::DockID,
        tab_index: usize,
        pane_rect: Rect<LogicalUnit>,
        tab_size: Size<LogicalUnit>,
        client_pos: Point<LogicalUnit>,
    ) {
        let (state, popover_rect) = ui::dock::begin_preview(
            pane_rect,
            tab_size,
            &client_pos,
            initiator,
            source_dock,
            tab_index,
        );

        let this = unsafe { self.get_unchecked_mut() };
        this.syslink
            .begin_pane_drag(initiator, &pointer, state.offset, &popover_rect);
        this.docking_preview_state = Some(state);
    }

    fn move_redock_preview(
        self: Pin<&mut Self>,
        dest_window: WindowHandle,
        client_pos_in_dest: Point<LogicalUnit>,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        if let Some(ref mut state) = this.docking_preview_state {
            let popover_rect = ui::dock::move_preview(
                &unsafe { dest_window.extra_data_ref::<PerWindowData>() }.docking_manager,
                &this.dock_store,
                &client_pos_in_dest,
                state,
            );
            this.syslink.update_pane_drag(dest_window, &popover_rect);
        }
    }

    fn confirm_redock(
        mut self: Pin<&mut Self>,
        mut destination_window: WindowHandle,
        client_pos_in_dest: Point<LogicalUnit>,
    ) {
        let this = unsafe { self.as_mut().get_unchecked_mut() };
        if let Some(state) = this.docking_preview_state.take() {
            let dm = &mut unsafe { destination_window.extra_data_mut::<PerWindowData>() }
                .docking_manager;

            tracing::debug!(?client_pos_in_dest, "dock confirm");

            let mut source_window = state.source_window;
            let source_dock = state.source_dock;
            let tab_index = state.tab_index;
            this.syslink.end_pane_drag();
            let (op, suggested_rect) =
                ui::dock::end_preview(dm, &mut this.dock_store, &client_pos_in_dest, state);
            let (diverged_content, undock_result) = dm.redock(
                source_dock,
                &mut this.dock_store,
                tab_index,
                op,
                &suggested_rect,
                &mut ui::dock::RedockingContext {
                    view_init_ctx: ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut this.composite_tree,
                            ht_manager: &mut this.ht_manager,
                            current_sec: this.global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut this.keyboard_focus_registry,
                        },
                        view_allocator: &mut this.view_allocator,
                        view_instance_store: &mut this.view_instance_store,
                        view_tree_relation_store: &mut this.view_tree_relation_store,
                        view_group_relation_store: &mut this.view_group_relation_store,
                        view_layout_state_store: &mut this.view_layout_state_store,
                        view_render_state_store: &mut this.view_render_state_store,
                        view_feedback_subscription_delayed_ops: &mut this
                            .view_feedback_registry_delayed_ops,
                        system_link: &this.syslink,
                        main_thread_texture_id_issuer: &mut this.texture_id_issuer,
                        application: &this.application,
                    },
                    view_render_queue: &mut this.view_render_queue,
                },
            );

            match undock_result {
                ui::dock::UndockResult::Success => {}
                ui::dock::UndockResult::ToBeEmpty => {
                    unsafe {
                        drop(source_window.take_extra_data::<PerWindowData>());
                    }
                    this.sub_windows.remove(&source_window);
                    close_sub_window(source_window);
                }
            }

            if let Some(content) = diverged_content {
                let new_window = open_sub_window(
                    SubWindowOpenMode::DockDiverge {
                        rect: Rect::from_lt_size(
                            Point::new_logical(
                                suggested_rect.left,
                                suggested_rect.top - ui::window_header::View::THICKNESS,
                            ),
                            Size::new_logical(
                                suggested_rect.width,
                                suggested_rect.height + ui::window_header::View::THICKNESS,
                            ),
                        ),
                        position_ref_window: destination_window,
                    },
                    self.as_mut(),
                    |mut w, coreloop| {
                        let this = unsafe { coreloop.get_unchecked_mut() };
                        this.ht_manager.get_data_mut(w.ht_root()).root_of_window = Some(w);

                        this.composite_tree
                            .begin_mod_chain(w.ct_root())
                            .has_bitmap(true)
                            .composite_mode(CompositeMode::FillCornerGradient(
                                this.window_bg_gradient,
                                AnimatableColor::Value([0.0, 0.025, 0.05, 1.0]),
                            ))
                            .apply();

                        let mut view_init_ctx = ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut this.composite_tree,
                                ht_manager: &mut this.ht_manager,
                                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                                keyboard_focus_registry: &mut this.keyboard_focus_registry,
                            },
                            view_allocator: &mut this.view_allocator,
                            view_instance_store: &mut this.view_instance_store,
                            view_tree_relation_store: &mut this.view_tree_relation_store,
                            view_group_relation_store: &mut this.view_group_relation_store,
                            view_layout_state_store: &mut this.view_layout_state_store,
                            view_render_state_store: &mut this.view_render_state_store,
                            view_feedback_subscription_delayed_ops: &mut this
                                .view_feedback_registry_delayed_ops,
                            system_link: &this.syslink,
                            main_thread_texture_id_issuer: &mut this.texture_id_issuer,
                            application: &this.application,
                        };
                        let root_view =
                            view_init_ctx.construct_view_direct(|_| Box::new(WindowRootView {}));
                        let window_header_view = ui::window_header::Component::new(
                            ui::window_header::Caption::Sub,
                            ui::window_header::ComponentInit {
                                with_system_command_buttons: w.needs_system_command_buttons(),
                            },
                            &mut view_init_ctx,
                        );
                        view_init_ctx.view_set_parent_untyped(
                            window_header_view.root_view(),
                            root_view.into_untyped(),
                        );

                        view_init_ctx.render_view_with_base(
                            root_view.into_untyped(),
                            &w,
                            w.keyboard_focus_group(),
                            Rect::from_lt_size(Point::new_logical(0.0, 0.0), w.client_size()),
                        );

                        w.associate_extra_data(Box::new(PerWindowData {
                            root_view,
                            screen_reposition_interests: HashSet::new(),
                            header: window_header_view,
                            appmenu: None,
                            footer: None,
                            docking_manager: ui::dock::WindowDockingManager::new(
                                w,
                                &mut view_init_ctx,
                                &mut this.view_render_queue,
                                Rect::from_lt_size(
                                    Point::new_logical(0.0, ui::window_header::View::THICKNESS),
                                    suggested_rect.size(),
                                ),
                                &mut this.dock_store,
                                |view_init_ctx, view_render_queue, store| {
                                    store.alloc_root(|root_id, store| {
                                        store.alloc_fill(
                                            root_id,
                                            &mut PaneGroupCreateContext {
                                                view_init_context: view_init_ctx,
                                                view_render_queue,
                                            },
                                            |_| vec![content],
                                            0,
                                        )
                                    })
                                },
                            ),
                        }));
                    },
                );
                unsafe { self.as_mut().get_unchecked_mut() }
                    .sub_windows
                    .insert(new_window);
            }
        }
    }

    fn update_preview(self: Pin<&mut Self>) {
        let this = unsafe { self.get_unchecked_mut() };

        this.preview_state.update(
            &mut *profiler::wrap!(
                LOCK_WAIT,
                this.committed_preview_state.lock().expect("poisoned")
            ),
            &mut this.preview_input_state,
            &mut ApplicationMutation {
                state: &mut this.application,
                view_feedbacks: &mut this.view_feedback_store,
            },
        );
    }

    fn perform_drop(
        self: Pin<&mut Self>,
        data: DragData,
        target_window: WindowHandle,
        client_pos: Point<LogicalUnit>,
    ) {
        let this = unsafe { self.get_unchecked_mut() };
        this.pointer_input_manager.perform_drop(
            data,
            client_pos,
            target_window.ht_root(),
            target_window.client_size(),
            &this.ht_manager,
        );
    }

    fn schedule_view_render(self: Pin<&mut Self>, id: ViewIdentifier) {
        let this = unsafe { self.get_unchecked_mut() };
        this.view_render_queue.schedule(id);
    }

    fn perform_menu_opens(mut self: Pin<&mut Self>) {
        let this = unsafe { self.as_mut().get_unchecked_mut() };
        assert!(
            this.menu_open_requests.len() <= 1,
            "more open request in one event?"
        );
        if let Some(req) = this.menu_open_requests.pop() {
            assert!(
                this.current_active_menu_session.is_none(),
                "another menu still active"
            );
            let session = MenuSession::new(
                req.parent,
                req.items,
                req.command_handler,
                req.surface_pos,
                self.as_mut(),
            );
            unsafe { self.as_mut().get_unchecked_mut() }.current_active_menu_session =
                Some(session);
        }

        let this = unsafe { self.as_mut().get_unchecked_mut() };
        if let Some(req) = this.menu_reopen_request.take() {
            if let Some(c) = this.current_active_menu_session.take() {
                c.terminate(
                    &this.syslink,
                    &mut this.composite_tree,
                    &mut this.ht_manager,
                    &mut this.keyboard_focus_registry,
                );
            }

            let session = MenuSession::new(
                req.parent,
                req.items,
                req.command_handler,
                req.surface_pos,
                self.as_mut(),
            );
            unsafe { self.as_mut().get_unchecked_mut() }.current_active_menu_session =
                Some(session);
        }

        let this = unsafe { self.as_mut().get_unchecked_mut() };
        assert!(
            this.dropdown_menu_open_requests.len() <= 1,
            "more dropdown open request in one event?"
        );
        if let Some(req) = this.dropdown_menu_open_requests.pop() {
            assert!(
                this.current_active_dropdown_menu_session.is_none(),
                "another dropdown menu still active"
            );
            let session = DropdownMenuSession::new(
                req.selection_receiver,
                req.parent,
                self.as_mut(),
                req.surface_pos,
                req.min_width,
                req.items,
            );
            unsafe { self.as_mut().get_unchecked_mut() }.current_active_dropdown_menu_session =
                Some(session);
        }
    }

    fn dispatch_view_feedback(self: core::pin::Pin<&mut Self>) {
        if self.view_feedback_store.is_empty() {
            // no view feedbacks
            return;
        }

        let this = unsafe { self.get_unchecked_mut() };
        let mut fb_context = ViewFeedbackContext {
            application: &this.application,
            composite_tree: &mut this.composite_tree,
            ht_manager: &mut this.ht_manager,
            current_sec: this.global_time_base.elapsed().as_secs_f32(),
            keyboard_focus_registry: &mut this.keyboard_focus_registry,
            view_allocator: &mut this.view_allocator,
            view_instance_store: &mut this.view_instance_store,
            view_tree_relation_store: &mut this.view_tree_relation_store,
            view_group_relation_store: &mut this.view_group_relation_store,
            view_layout_state_store: &mut this.view_layout_state_store,
            view_render_state_store: &mut this.view_render_state_store,
            view_feedback_subscription_delayed_ops: &mut this.view_feedback_registry_delayed_ops,
            system_link: &this.syslink,
            main_thread_texture_id_issuer: &mut this.texture_id_issuer,
            view_render_queue: &mut this.view_render_queue,
        };

        for (t, p) in this.view_feedback_store.iter() {
            unsafe {
                this.view_feedback_registry
                    .dispatch_dynamic_unchecked(p, t, &mut fb_context);
            }
        }
        this.view_feedback_store.clear();
        this.view_feedback_registry.perform_atomic(&mut fb_context);
    }

    fn update_view(self: core::pin::Pin<&mut Self>) {
        let this = unsafe { self.get_unchecked_mut() };
        this.view_render_queue.perform(
            &mut RenderContext {
                composite_tree: &mut this.composite_tree,
                ht_manager: &mut this.ht_manager,
                keyboard_focus_registry: &mut this.keyboard_focus_registry,
                current_sec: this.global_time_base.elapsed().as_secs_f32(),
                system_link: &this.syslink,
                main_thread_texture_id_issuer: &mut this.texture_id_issuer,
                application: &this.application,
                view_feedback_subscription_delayed_ops: &mut this
                    .view_feedback_registry_delayed_ops,
            },
            &mut this.view_instance_store,
            &this.view_tree_relation_store,
            &mut this.view_layout_state_store,
            &mut this.view_render_state_store,
        );
    }

    fn process_delayed_view_feedback_registry_ops(self: core::pin::Pin<&mut Self>) {
        let this = unsafe { self.get_unchecked_mut() };
        this.view_feedback_registry
            .perform_delayed(&mut this.view_feedback_registry_delayed_ops);
    }

    fn sync_threads(self: core::pin::Pin<&mut Self>) {
        let this = unsafe { self.get_unchecked_mut() };
        this.composite_tree.commit(
            &mut this
                .renderer_sync
                .lock()
                .expect("poisoned")
                .composite_buffer,
        );
        for msg in this.delayed_render_messages.drain(..) {
            this.syslink.rt_sender().send(msg).expect("rt_sender.send");
        }
    }

    pub fn update_view_all(mut self: Pin<&mut Self>) {
        self.as_mut().perform_menu_opens();
        self.as_mut().dispatch_view_feedback();
        self.as_mut().update_view();
        self.as_mut().process_delayed_view_feedback_registry_ops();
        self.as_mut().sync_threads();
    }

    fn save_window_state(&self) {
        tracing::info!("saving window state");
        let window_state_persist = PersistStateWindowData {
            main: WindowState {
                geometry: self.main_window.geometry_state_snapshot(&self.syslink),
                dock: unsafe { self.main_window.extra_data_ref::<PerWindowData>() }
                    .docking_manager
                    .state_snapshot(&self.dock_store),
            },
            sub: self
                .sub_windows
                .iter()
                .map(|w| WindowState {
                    geometry: w.geometry_state_snapshot(&self.syslink),
                    dock: unsafe { w.extra_data_ref::<PerWindowData>() }
                        .docking_manager
                        .state_snapshot(&self.dock_store),
                })
                .collect(),
        };

        let fp = match std::fs::File::create(self.fs.window_state_save_path()) {
            Ok(fp) => fp,
            Err(e) => {
                tracing::warn!(reason = %e, "persist.create.window_state");
                return;
            }
        };
        if let Err(e) = window_state_persist.serialize(&mut std::io::BufWriter::new(fp)) {
            tracing::warn!(reason = %e, "persist.save.window_state");
        }
    }

    pub fn on_event(mut self: Pin<&mut Self>, e: Event) {
        profiler::scope!(PROCESS_EVENT, str e.p_name());

        match e {
            Event::Quit => unreachable!("could not exit by calling on_event"),
            Event::Sync(SyncEvent::WindowPostCreateRenderBuffer { window }) => {
                #[cfg(feature = "wayland")]
                window.update_manual_scaling();
            }
            Event::Sync(SyncEvent::FlyoutSurfacePostCreateRenderBuffer { target }) => {
                #[cfg(feature = "wayland")]
                target.update_manual_scaling();
            }
            Event::OpenAlertDialog {
                target_window,
                message,
            } => self.as_mut().open_alert_dialog(target_window, message),
            Event::PopupClose { id } => self.as_mut().close_popup(id),
            Event::Sync(SyncEvent::PopupUnmount { id }) => self.as_mut().destroy_popup(id),
            Event::OpenCustomViewFlyout {
                parent,
                surface_pos,
                view_constructor,
            } => self
                .as_mut()
                .open_custom_flyout(parent, surface_pos, view_constructor.0.0),
            Event::MenuSelectItem { depth, index } => {
                self.as_mut().handle_menu_item_selection(depth, index)
            }
            Event::MenuDeselectItem { depth } => self.as_mut().handle_menu_item_deselection(depth),
            Event::MenuSelectCommand { id } => self.as_mut().perform_select_menu_command(id),
            Event::DropdownMenuSelectItem { id, receiver } => self
                .as_mut()
                .perform_dropdown_menu_select_item(id, receiver),
            Event::DockMoveSplitter {
                controlling_dock,
                pos_client,
            } => self
                .as_mut()
                .move_dock_splitter(controlling_dock, pos_client),
            Event::DockBeginPreview {
                initiator,
                pointer,
                source_dock,
                tab_index,
                pane_rect,
                tab_size,
                client_pos,
            } => self.as_mut().begin_redock_preview(
                initiator,
                pointer,
                source_dock,
                tab_index,
                pane_rect,
                tab_size,
                client_pos,
            ),
            Event::Sync(SyncEvent::NewPresentID { .. }) => self.as_mut().update_preview(),
            Event::ScheduleViewRenderExt { id } => self.as_mut().schedule_view_render(id),
            #[cfg(windows)]
            Event::CoreTextLayoutRequested {
                ht,
                request,
                deferral,
            } => {
                if deferral.is_none()
                    || request
                        .IsCanceled()
                        .inspect_err(|e| tracing::error!(reason = %e, "request.is_canceled"))
                        == Ok(false)
                {
                    if let Some(w) = ht_manager
                        .get_data(ht)
                        .native_text_deferrable_event_handler()
                    {
                        if let Err(e) = w.layout(
                            &mut InputEventContext {
                                composite_tree: &mut composite_tree,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                system_link: &mut system_link,
                                ht_manager: &ht_manager,
                                dock_store: &mut dock_store,
                                view_instance_store: &mut view_instance_store,
                                view_group_relation_store: &view_group_relation_store,
                                view_render_queue: &mut view_render_queue,
                                application: ApplicationMutation {
                                    state: &mut application,
                                    view_feedbacks: &mut view_feedback_store,
                                },
                            },
                            &request,
                        ) {
                            tracing::error!(reason = %e, "CoreTextLayoutRequested");
                            if let Some(d) = deferral {
                                if let Err(e) = d.Close() {
                                    tracing::error!(reason = %e, "deferral.close");
                                }
                            }
                        } else {
                            if let Some(d) = deferral {
                                if let Err(e) = d.Complete() {
                                    tracing::error!(reason = %e, "deferral.complete");
                                }
                            }
                        }
                    }
                }
            }
            #[cfg(windows)]
            Event::CoreTextTextUpdating { ht, e, deferral } => {
                if deferral.is_none()
                    || e.IsCanceled()
                        .inspect_err(|e| tracing::error!(reason = %e, "e.is_canceled"))
                        == Ok(false)
                {
                    if let Some(w) = ht_manager
                        .get_data(ht)
                        .native_text_deferrable_event_handler()
                    {
                        if let Err(e) = w.text_updating(
                            &mut InputEventContext {
                                composite_tree: &mut composite_tree,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                system_link: &mut system_link,
                                ht_manager: &ht_manager,
                                dock_store: &mut dock_store,
                                view_instance_store: &mut view_instance_store,
                                view_group_relation_store: &view_group_relation_store,
                                view_render_queue: &mut view_render_queue,
                                application: ApplicationMutation {
                                    state: &mut application,
                                    view_feedbacks: &mut view_feedback_store,
                                },
                            },
                            &e,
                        ) {
                            tracing::error!(reason = %e, "CoreTextTextUpdating");
                            if let Some(d) = deferral {
                                if let Err(e) = d.Close() {
                                    tracing::error!(reason = %e, "deferral.close");
                                }
                            }
                        } else {
                            if let Some(d) = deferral {
                                if let Err(e) = d.Complete() {
                                    tracing::error!(reason = %e, "deferral.complete");
                                }
                            }
                        }
                    }
                }
            }
            #[cfg(windows)]
            Event::CoreTextFormatUpdating { ht, e, deferral } => {
                if deferral.is_none()
                    || e.IsCanceled()
                        .inspect_err(|e| tracing::error!(reason = %e, "e.is_canceled"))
                        == Ok(false)
                {
                    if let Some(w) = ht_manager
                        .get_data(ht)
                        .native_text_deferrable_event_handler()
                    {
                        if let Err(e) = w.format_updating(
                            &mut InputEventContext {
                                composite_tree: &mut composite_tree,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                system_link: &mut system_link,
                                ht_manager: &ht_manager,
                                dock_store: &mut dock_store,
                                view_instance_store: &mut view_instance_store,
                                view_group_relation_store: &view_group_relation_store,
                                view_render_queue: &mut view_render_queue,
                                application: ApplicationMutation {
                                    state: &mut application,
                                    view_feedbacks: &mut view_feedback_store,
                                },
                            },
                            &e,
                        ) {
                            tracing::error!(reason = %e, "CoreTextFormatUpdating");
                            if let Some(d) = deferral {
                                if let Err(e) = d.Close() {
                                    tracing::error!(reason = %e, "deferral.close");
                                }
                            }
                        } else {
                            if let Some(d) = deferral {
                                if let Err(e) = d.Complete() {
                                    tracing::error!(reason = %e, "deferral.complete");
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[cfg(unix)]
    #[tracing::instrument(target = "dbus::loop", skip(self, msg), fields(type = ?msg.r#type(), path = ?msg.path(), interface = ?msg.interface(), member = ?msg.member()))]
    fn handle_dbus_message(self: Pin<&mut Self>, msg: dbus::Message) {
        match msg.r#type() {
            dbus::MessageType::MethodCall
                if msg
                    .path()
                    .is_some_and(|x| x == platform::unix::APPMENU_OBJECT_PATH)
                    && msg.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
                    && msg.member() == Some(c"GetLayout") =>
            {
                let args = proto::dbus_menu::GetLayoutRequest::deserialize(&mut msg.iter());

                tracing::debug!(?args, "com.canonical.dbusmenu.GetLayout");

                // toriaezu
                assert_eq!(args.recursion_depth, 1);

                if args.parent_id == 1 {
                    let mut reply = dbus::Message::new_method_return(&msg)
                        .expect("dbus.message.new_method_return");
                    proto::dbus_menu::GetLayoutReply {
                        revision: 1,
                        layout: proto::dbus_menu::Layout {
                            id: 1,
                            properties: Default::default(),
                            children: &[proto::dbus_menu::Layout {
                                id: 100,
                                properties: proto::dbus_menu::LayoutProperties {
                                    label: Some(c"終了"),
                                    enabled: Some(true),
                                    visible: Some(true),
                                    icon_name: Some(c"window-close"),
                                    shortcut: Some(&[&[c"Alt", c"F4"], &[c"Meta", c"Q"]]),
                                    ..Default::default()
                                },
                                children: &[],
                            }],
                        },
                    }
                    .serialize(&mut reply.iter_append())
                    .expect("dbus_menu.get_layout.serialize_reply");
                    unsafe { &*self.syslink.dbus }
                        .send(&mut reply)
                        .expect("dbus.send");
                } else if args.parent_id == 0 {
                    let mut reply = dbus::Message::new_method_return(&msg)
                        .expect("dbus.message.new_method_return");
                    proto::dbus_menu::GetLayoutReply {
                        revision: 1,
                        layout: proto::dbus_menu::Layout {
                            id: 0,
                            properties: proto::dbus_menu::LayoutProperties {
                                children_display: Some(c"submenu"),
                                ..Default::default()
                            },
                            children: &[proto::dbus_menu::Layout {
                                id: 1,
                                properties: proto::dbus_menu::LayoutProperties {
                                    label: Some(c"ファイル"),
                                    enabled: Some(true),
                                    visible: Some(true),
                                    children_display: Some(c"submenu"),
                                    ..Default::default()
                                },
                                children: &[],
                            }],
                        },
                    }
                    .serialize(&mut reply.iter_append())
                    .expect("dbus_menu.get_layout.serialize_reply");
                    unsafe { &*self.syslink.dbus }
                        .send(&mut reply)
                        .expect("dbus.send");
                } else {
                    unreachable!("unknown menu id");
                }
            }
            dbus::MessageType::MethodCall
                if msg
                    .path()
                    .is_some_and(|x| x == platform::unix::APPMENU_OBJECT_PATH)
                    && msg.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
                    && msg.member() == Some(c"Event") =>
            {
                let mut args_iter = msg.iter();
                let id = args_iter.try_get_i32().expect("id:i");
                args_iter.next();
                let event_id = args_iter.try_get_cstr().expect("event_id:s").to_owned();
                args_iter.next();
                let data_container = args_iter.try_begin_iter_variant_content().expect("data:v");
                args_iter.next();
                let timestamp = args_iter.try_get_u32().expect("timestamp:u");

                tracing::trace!(
                    id,
                    ?event_id,
                    data.signature = ?data_container.signature(),
                    timestamp,
                    "menu event"
                );

                if id == 100 && event_id == c"clicked" {
                    // clicked quit menu item
                    return;
                }
            }
            dbus::MessageType::MethodCall
                if msg
                    .path()
                    .is_some_and(|x| x == platform::unix::APPMENU_OBJECT_PATH)
                    && msg.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
                    && msg.member() == Some(c"AboutToShow") =>
            {
                let mut args_iter = msg.iter();
                let id = args_iter.try_get_i32().expect("id:i");

                let mut reply =
                    dbus::Message::new_method_return(&msg).expect("dbus.message.new_method_return");
                proto::dbus_menu::AboutToShowReply { need_update: false }
                    .serialize(&mut reply.iter_append())
                    .expect("dbus_menu.about_to_show.serialize_reply");
                unsafe { &*self.syslink.dbus }
                    .send(&mut reply)
                    .expect("dbus.send");
            }
            _ => tracing::trace!(target: "dbus::loop", "unknown dbus message"),
        }
    }
}

#[tracing::instrument(target = "peridot_marble_editor::logic_fiber", skip_all)]
async fn run<'sys>(mut inst: Pin<&mut CoreLoop<'static, 'sys>>, event_queue: EventQueue) {
    tracing::info!("app start");

    loop {
        let e = event_queue.next_event().await;
        tracing::trace!(target: "event-trace", event = ?e);
        match e {
            Event::Quit => break,
            e => inst.as_mut().on_event(e),
        }

        // after-input common update phase
        inst.as_mut().update_view_all();
    }

    inst.save_window_state();
    tracing::info!("app finish");
    #[cfg(windows)]
    unsafe {
        platform::windows::unlocate_non_client_hittest_managers();
    }
}

pub trait FlyoutSurfacePresenter {
    fn root_view_id(&self) -> ViewIdentifier;

    #[allow(unused_variables)]
    fn rescale(
        &self,
        new_scale: f32,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &HitTestTreeManager,
        system_link: &SystemLink,
    ) {
    }

    #[allow(unused_variables)]
    fn teardown(&self, ctx: &mut TeardownContext) {}
}
pub trait FlyoutSurfacePresenterConstructor {
    fn size(&self) -> Size<LogicalUnit>;
    fn create(&self, view_init_context: &mut ViewInitContext) -> Box<dyn FlyoutSurfacePresenter>;
}

pub struct FlyoutSurfaceSessionTerminateContext<'a, 'h, 'sys> {
    pub syslink: &'a SystemLink<'sys>,
    pub view_allocator: &'a mut ViewIdentifierAllocator,
    pub view_instance_store: &'a mut ViewInstanceStore,
    pub view_tree_relation_store: &'a mut ViewTreeRelationStore,
    pub view_group_relation_store: &'a mut ViewGroupRelationStore,
    pub view_layout_state_store: &'a mut ViewLayoutStateStore,
    pub view_render_state_store: &'a mut ViewRenderStateStore,
    pub teardown_context: TeardownContext<'a, 'h>,
}
impl ViewDestructionContext for FlyoutSurfaceSessionTerminateContext<'_, '_, '_> {
    #[inline(always)]
    fn destruct_view_recursive_untyped(&mut self, target: ViewIdentifier) {
        uicore::destruct_view_recursive(
            target,
            &mut self.teardown_context,
            self.view_allocator,
            self.view_instance_store,
            self.view_tree_relation_store,
            self.view_group_relation_store,
            self.view_layout_state_store,
            self.view_render_state_store,
        );
    }
}

pub struct CustomViewFlyoutSurface {
    native_surface: FlyoutSurfaceHandle,
    content: Box<dyn FlyoutSurfacePresenter>,
}
pub struct CustomViewFlyoutSession {
    parent: WindowHandle,
    opening_surface: CustomViewFlyoutSurface,
}
impl CustomViewFlyoutSession {
    pub fn begin(
        parent: WindowHandle,
        pos: Point<LogicalUnit>,
        content_ctor: Box<dyn FlyoutSurfacePresenterConstructor>,
        mut cl: Pin<&mut CoreLoop<'static, '_>>,
    ) -> Self {
        let surface = create_flyout_surface(parent, pos, content_ctor.size(), cl.as_mut());

        let cl = unsafe { cl.get_unchecked_mut() };
        let mut view_init_ctx = ViewInitContext {
            mount_context: MountContext {
                composite_tree: &mut cl.composite_tree,
                ht_manager: &mut cl.ht_manager,
                current_sec: cl.global_time_base.elapsed().as_secs_f32(),
                keyboard_focus_registry: &mut cl.keyboard_focus_registry,
            },
            view_allocator: &mut cl.view_allocator,
            view_instance_store: &mut cl.view_instance_store,
            view_tree_relation_store: &mut cl.view_tree_relation_store,
            view_group_relation_store: &mut cl.view_group_relation_store,
            view_layout_state_store: &mut cl.view_layout_state_store,
            view_render_state_store: &mut cl.view_render_state_store,
            view_feedback_subscription_delayed_ops: &mut cl.view_feedback_registry_delayed_ops,
            system_link: &cl.syslink,
            main_thread_texture_id_issuer: &mut cl.texture_id_issuer,
            application: &cl.application,
        };
        let content = content_ctor.create(&mut view_init_ctx);
        view_init_ctx.render_view_with_base(
            content.root_view_id(),
            &surface,
            surface.keyboard_focus_state().root_group(),
            Rect::from_lt_size(Point::new_logical(0.0, 0.0), content_ctor.size()),
        );

        Self {
            parent,
            opening_surface: CustomViewFlyoutSurface {
                native_surface: surface,
                content,
            },
        }
    }

    pub fn rescale(
        &self,
        new_scale: f32,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &HitTestTreeManager,
        system_link: &SystemLink,
    ) {
        self.opening_surface
            .content
            .rescale(new_scale, composite_tree, ht_manager, system_link);
    }

    pub fn terminate<'a, 'h: 'a>(self, env: &mut FlyoutSurfaceSessionTerminateContext) {
        self.opening_surface
            .content
            .teardown(&mut env.teardown_context);
        env.destruct_view_recursive_untyped(self.opening_surface.content.root_view_id());
        self.opening_surface.native_surface.close(
            env.syslink,
            env.teardown_context.composite_tree,
            env.teardown_context.ht_manager,
            env.teardown_context.keyboard_focus_registry,
        );
    }
}

pub struct DropdownMenuOpenRequest {
    pub parent: WindowHandle,
    pub surface_pos: Point<LogicalUnit>,
    pub min_width: f32,
    pub items: Vec<crate::uikit::dropdown_box::MenuItem>,
    pub selection_receiver: std::rc::Weak<crate::uikit::dropdown_box::EventHandler>,
}

pub struct DropdownMenuSurface {
    native_surface: FlyoutSurfaceHandle,
    item_views: Vec<uikit::dropdown_box::MenuItemView>,
}

pub struct DropdownMenuSession {
    parent: WindowHandle,
    opening_surfaces: Vec<DropdownMenuSurface>,
}
impl DropdownMenuSession {
    pub fn new(
        selection_receiver: std::rc::Weak<uikit::dropdown_box::EventHandler>,
        parent: WindowHandle,
        mut cl: Pin<&mut CoreLoop<'static, '_>>,
        pos: Point<LogicalUnit>,
        min_width: f32,
        items: Vec<uikit::dropdown_box::MenuItem>,
    ) -> Self {
        let menu_layout = uikit::dropdown_box::MenuLayout::new(items, cl.syslink.font_set());
        let root_surface = create_flyout_surface(
            parent,
            pos,
            Size::new_logical(
                menu_layout.required_width().max(min_width),
                menu_layout.height(),
            ),
            cl.as_mut(),
        );

        let cl = unsafe { cl.get_unchecked_mut() };
        let mut view_init_ctx = ViewInitContext {
            mount_context: MountContext {
                composite_tree: &mut cl.composite_tree,
                ht_manager: &mut cl.ht_manager,
                current_sec: cl.global_time_base.elapsed().as_secs_f32(),
                keyboard_focus_registry: &mut cl.keyboard_focus_registry,
            },
            view_allocator: &mut cl.view_allocator,
            view_instance_store: &mut cl.view_instance_store,
            view_tree_relation_store: &mut cl.view_tree_relation_store,
            view_group_relation_store: &mut cl.view_group_relation_store,
            view_layout_state_store: &mut cl.view_layout_state_store,
            view_render_state_store: &mut cl.view_render_state_store,
            view_feedback_subscription_delayed_ops: &mut cl.view_feedback_registry_delayed_ops,
            system_link: &cl.syslink,
            main_thread_texture_id_issuer: &mut cl.texture_id_issuer,
            application: &cl.application,
        };
        let item_views = menu_layout
            .instantiate_all(&mut view_init_ctx, selection_receiver, |v, ctx| {
                v.mount(ctx, &root_surface)
            })
            .collect::<Vec<_>>();

        Self {
            parent,
            opening_surfaces: vec![DropdownMenuSurface {
                native_surface: root_surface,
                item_views,
            }],
        }
    }

    pub fn close_all<E>(
        &mut self,
        syslink: &SystemLink,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        for v in self.opening_surfaces.drain(..) {
            v.native_surface
                .close(syslink, composite_tree, ht_manager, keyboard_focus_registry);
        }
    }
}

pub struct MenuOpenRequest {
    pub parent: WindowHandle,
    pub items: Vec<MenuItem>,
    pub surface_pos: Point<LogicalUnit>,
    pub command_handler: Box<dyn MenuCommandSelectionHandler>,
}

pub struct MenuSurface {
    handle: FlyoutSurfaceHandle,
    item_views: Vec<Option<MenuItemInteractableElement>>,
    _event_handler: Rc<MenuEventHandler>,
    parent_path: Vec<usize>,
    current_selecting: Option<usize>,
}
impl MenuSurface {
    fn new(
        mut cl: Pin<&mut CoreLoop<'static, '_>>,
        initiator_window: WindowHandle,
        display_pos: Point<LogicalUnit>,
        depth: usize,
        parent_path: Vec<usize>,
        items: impl Iterator<Item = MenuItem>,
    ) -> Self {
        let layouted_items = crate::uikit::MenuItemLayout::build(items, cl.syslink.font_set());
        let width = crate::uikit::MenuItemLayout::min_width(layouted_items.iter());
        let height = crate::uikit::MenuItemLayout::height(layouted_items.iter());

        let surface = create_flyout_surface(
            initiator_window,
            display_pos,
            Size::new_logical(width.value(), height.value()),
            cl.as_mut(),
        );

        let cl = unsafe { cl.get_unchecked_mut() };
        let (item_views, eh) = crate::uikit::MenuItemLayout::instantiate(
            layouted_items.into_iter(),
            depth,
            &mut ViewInitContext {
                mount_context: MountContext {
                    composite_tree: &mut cl.composite_tree,
                    ht_manager: &mut cl.ht_manager,
                    current_sec: cl.global_time_base.elapsed().as_secs_f32(),
                    keyboard_focus_registry: &mut cl.keyboard_focus_registry,
                },
                view_allocator: &mut cl.view_allocator,
                view_instance_store: &mut cl.view_instance_store,
                view_tree_relation_store: &mut cl.view_tree_relation_store,
                view_group_relation_store: &mut cl.view_group_relation_store,
                view_layout_state_store: &mut cl.view_layout_state_store,
                view_render_state_store: &mut cl.view_render_state_store,
                view_feedback_subscription_delayed_ops: &mut cl.view_feedback_registry_delayed_ops,
                system_link: &cl.syslink,
                main_thread_texture_id_issuer: &mut cl.texture_id_issuer,
                application: &cl.application,
            },
            &cl.context_menu_common_resources,
            &surface,
        );
        cl.ht_manager.set_action_handler(surface.ht_root(), &eh);

        Self {
            handle: surface,
            item_views,
            _event_handler: eh,
            parent_path,
            current_selecting: None,
        }
    }

    pub fn set_current_selecting(
        &mut self,
        new_index: usize,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        if self.current_selecting == Some(new_index) {
            // no changes
            return;
        }

        if let Some(x) = self.current_selecting {
            self.item_views[x]
                .as_ref()
                .expect("not interactable")
                .unlit(composite_tree, current_sec);
        }

        self.current_selecting = Some(new_index);
        self.item_views[new_index]
            .as_ref()
            .expect("not interactable")
            .lit(composite_tree, current_sec);
    }

    pub fn deselect(&mut self, composite_tree: &mut CompositeTree<SyncEvent>, current_sec: f32) {
        if let Some(x) = self.current_selecting {
            self.item_views[x]
                .as_ref()
                .expect("not interactable")
                .unlit(composite_tree, current_sec);
        }

        self.current_selecting = None;
    }
}

pub struct MenuSession {
    parent: WindowHandle,
    items: Vec<MenuItem>,
    command_handler: Box<dyn MenuCommandSelectionHandler>,
    opening_surfaces: Vec<MenuSurface>,
    active_selection: Option<(usize, usize)>,
}
impl MenuSession {
    pub fn new(
        parent: WindowHandle,
        items: Vec<MenuItem>,
        command_handler: Box<dyn MenuCommandSelectionHandler>,
        surface_pos: Point<LogicalUnit>,
        cl: Pin<&mut CoreLoop<'static, '_>>,
    ) -> Self {
        #[cfg(target_os = "macos")]
        view_init_context
            .system_link
            .flyout_surface_context
            .observe_global_click();

        Self {
            opening_surfaces: vec![MenuSurface::new(
                cl,
                parent,
                surface_pos,
                0,
                Vec::new(),
                items.iter().cloned(),
            )],
            parent,
            items,
            command_handler,
            active_selection: None,
        }
    }

    fn close_deeper<E>(
        &mut self,
        target_depth: usize,
        system_link: &SystemLink,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        while self.opening_surfaces.len() > target_depth + 1 {
            self.opening_surfaces.pop().expect("empty?").handle.close(
                system_link,
                composite_tree,
                ht_manager,
                keyboard_focus_registry,
            );
        }
    }

    fn query_submenu<'a>(&'a self, index_path: impl Iterator<Item = usize>) -> &'a [MenuItem] {
        index_path.fold(&self.items[..], |haystack, x| match haystack[x] {
            MenuItem::SubMenu { ref items, .. } => items,
            _ => unreachable!("invalid nesting"),
        })
    }

    pub fn perform_delayed_action(&mut self, mut cl: *mut CoreLoop<'static, '_>) {
        match self.active_selection {
            Some((depth, index)) => {
                let cl_ref = unsafe { &mut *cl };
                self.close_deeper(
                    depth,
                    &cl_ref.syslink,
                    &mut cl_ref.composite_tree,
                    &mut cl_ref.ht_manager,
                    &mut cl_ref.keyboard_focus_registry,
                );
                let latest_surface = self.opening_surfaces.last().expect("root?");

                if let Some(MenuItemInteractableElement::SubMenu(ref submenu)) =
                    latest_surface.item_views[index]
                {
                    // submenu delayed action
                    let pos = latest_surface.handle.submenu_pop_position(submenu);
                    let parent_path = latest_surface
                        .parent_path
                        .iter()
                        .copied()
                        .chain(core::iter::once(index))
                        .collect::<Vec<_>>();
                    let items = self.query_submenu(parent_path.iter().copied());

                    self.opening_surfaces.push(MenuSurface::new(
                        unsafe { Pin::new_unchecked(cl_ref) },
                        self.parent,
                        pos,
                        depth + 1,
                        parent_path,
                        items.iter().cloned(),
                    ));
                }
            }
            None => {
                // 最初のやつだけ表示する
                let cl = unsafe { &mut *cl };
                self.close_deeper(
                    0,
                    &cl.syslink,
                    &mut cl.composite_tree,
                    &mut cl.ht_manager,
                    &mut cl.keyboard_focus_registry,
                );
            }
        }
    }

    pub fn terminate(
        mut self,
        system_link: &SystemLink,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) -> Box<dyn MenuCommandSelectionHandler> {
        while let Some(c) = self.opening_surfaces.pop() {
            c.handle.close(
                system_link,
                composite_tree,
                ht_manager,
                keyboard_focus_registry,
            );
        }

        #[cfg(target_os = "macos")]
        system_link.flyout_surface_context.unobserve_global_click();

        self.command_handler
    }

    pub fn select_item(
        &mut self,
        depth: usize,
        index: usize,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        let parent_path = if let Some(surface) = self.opening_surfaces.get_mut(depth) {
            surface.set_current_selecting(index, composite_tree, current_sec);
            surface.parent_path.clone()
        } else {
            tracing::warn!(depth, "selecting non-displaying depth");
            return;
        };

        // 親（発生元）も選択表示にする
        for (depth, &index) in parent_path.iter().enumerate() {
            self.opening_surfaces[depth].set_current_selecting(index, composite_tree, current_sec);
        }

        self.active_selection = Some((depth, index));
    }

    pub fn deselect_item(
        &mut self,
        depth: usize,
        composite_tree: &mut CompositeTree<SyncEvent>,
        current_sec: f32,
    ) {
        if let Some(surface) = self.opening_surfaces.get_mut(depth) {
            surface.deselect(composite_tree, current_sec);
        }

        self.active_selection = None;
    }
}

/// Main Windowを開く方法
pub enum MainWindowOpenMode {
    /// 新規
    New,
    /// 復元
    Restore(WindowGeometryState),
}

/// Sub Windowを開く方法
pub enum SubWindowOpenMode {
    /// ドックからポップする
    DockDiverge {
        rect: Rect<LogicalUnit>,
        position_ref_window: WindowHandle,
    },
    /// 復元
    Restore(WindowGeometryState),
}

/// Windowの種類
pub enum WindowType {
    /// Main（起動時に必ず1つ存在するWindow 閉じるとアプリ終了）
    Main {
        #[cfg(target_os = "linux")]
        termination_event: std::sync::Arc<linux_eventfd::EventFD>,
    },
    /// Sub（Dockから外したり必要に応じて表示されるWindow）
    Sub,
}

// platform-dependent constants
pub const DRAG_PREVIEW_POPOVER_BG_COLOR: Color32 = Color32 {
    r: 16,
    g: 176,
    b: 255,
    a: 16,
};

#[cfg(not(windows))]
pub struct SystemLink<'sys> {
    gfx: *const Graphics<'sys>,
    rt_sender: RenderMessageSender,
    font_set: FontSet,
    event_dispatcher: *mut LogicFiberEventDispatcher,
    #[cfg(feature = "wayland")]
    display_server: platform::unix::wayland::DisplayServerLink<'sys>,
    #[cfg(target_os = "linux")]
    dbus: *const dbus::Connection,
    #[cfg(target_os = "linux")]
    terminate_event: Arc<linux_eventfd::EventFD>,
    #[cfg(target_os = "linux")]
    pointer_hovering_timer: *const utils::platform::linux::TimerFD,
    #[cfg(feature = "wayland")]
    pub flyout_surface_context: platform::unix::wayland::flyout_surface::SharedState,
    #[cfg(target_os = "macos")]
    pub flyout_surface_context: platform::mac::flyout_surface::SharedState,
}
#[cfg(not(windows))]
impl SystemLink<'_> {
    #[inline(always)]
    pub const fn rt_sender(&self) -> &RenderMessageSender {
        &self.rt_sender
    }

    #[inline(always)]
    pub fn event_dispatcher(&self) -> &LogicFiberEventDispatcher {
        unsafe { &*self.event_dispatcher }
    }

    #[inline(always)]
    pub const fn font_set(&self) -> &FontSet {
        &self.font_set
    }

    #[inline(always)]
    pub fn dispatch_event(&self, event: Event) {
        unsafe { &*self.event_dispatcher }.dispatch(event);
    }
}

#[cfg(target_os = "macos")]
pub use platform::mac::{
    DragPreviewPopoverHandle, PointerID, WindowHandle, WindowPersistentStateNativeGeometryUnit,
    flyout_surface::Handle as FlyoutSurfaceHandle,
};
#[cfg(feature = "wayland")]
pub use platform::unix::wayland::{
    DragData, FlyoutSurfaceHandle, PointerID, ToplevelHandle as WindowHandle,
    WindowPersistentStateNativeGeometryUnit, close_sub_window, create_flyout_surface,
    create_main_window, open_sub_window,
};
#[cfg(windows)]
pub use platform::windows::{
    DragData, PointerID, SystemLink, WindowHandle, WindowPersistentStateNativeGeometryUnit,
    flyout_surface::Handle as FlyoutSurfaceHandle,
};

pub struct SyncEventBus {
    queue: std::sync::Mutex<VecDeque<SyncEvent>>,
    #[cfg(target_os = "linux")]
    efd: linux_eventfd::EventFD,
    #[cfg(windows)]
    event_notify: utils::platform::windows::Event,
    #[cfg(target_os = "macos")]
    redispatch_to: LogicFiberEventDispatcher,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for SyncEventBus {}
#[cfg(target_os = "macos")]
unsafe impl Send for SyncEventBus {}
impl SyncEventBus {
    pub fn new(redispatch_to: LogicFiberEventDispatcher) -> Self {
        Self {
            queue: std::sync::Mutex::new(VecDeque::new()),
            #[cfg(target_os = "linux")]
            efd: linux_eventfd::EventFD::new(0, linux_eventfd::EventFDFlags::empty())
                .expect("app_event_bus.efd.create"),
            #[cfg(windows)]
            event_notify: utils::platform::windows::Event::new(true, false).expect("event.new"),
            #[cfg(target_os = "macos")]
            redispatch_to,
        }
    }

    #[profiler::instrument("SyncEventBus.Push")]
    pub fn push(&self, e: SyncEvent) {
        self.queue.lock().expect("poisoned").push_back(e);
        #[cfg(target_os = "linux")]
        self.efd.inc(1).unwrap();
        #[cfg(windows)]
        self.event_notify.set().expect("event_notify.set");
        #[cfg(target_os = "macos")]
        unsafe {
            extern "C" fn callback(ctx: *mut core::ffi::c_void) {
                let this = unsafe { &*(ctx.cast::<SyncEventBus>()) };
                this.redispatch(&this.redispatch_to);
            }

            platform::mac::bridge::ni_post_unbound_callback_from_thread(
                callback,
                self as *const _ as _,
            );
        }
    }

    fn redispatch(&self, dispatcher: &LogicFiberEventDispatcher) {
        let mut queue = self.queue.lock().expect("poisoned");
        while let Some(event) = queue.pop_front() {
            dispatcher.dispatch(Event::Sync(event));
        }
        if let Err(e) = self.notify_clear() {
            tracing::error!(reason = ?e, "notify_clear");
        };
    }

    fn notify_clear(&self) -> std::io::Result<()> {
        #[cfg(target_os = "linux")]
        return match self.efd.take() {
            // WouldBlock(EAGAIN)はでてきてもOK
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(()),
            Err(e) => Err(e),
            Ok(_) => Ok(()),
        };
        #[cfg(windows)]
        return self.event_notify.reset().map_err(From::from);
        #[cfg(target_os = "macos")]
        {
            // TODO
            Ok(())
        }
    }
}

/// 初期のドッキングレイアウト
fn initial_dock_state() -> DockState {
    DockState::Splitted {
        direction: persistence::DockDirection::Bottom(320.0),
        content: Box::new(DockState::Filled {
            content_ids: vec![ui::pane::asset_explorer::Presenter::ID.into()],
            active_index: 0,
        }),
        rest: Box::new(DockState::Splitted {
            direction: persistence::DockDirection::Right(256.0),
            content: Box::new(DockState::Filled {
                content_ids: vec![
                    ui::pane::inspector::Presenter::ID.into(),
                    UIKitPreviewPanePresenter::ID.into(),
                ],
                active_index: 0,
            }),
            rest: Box::new(DockState::Splitted {
                direction: persistence::DockDirection::Top(120.0),
                content: Box::new(DockState::Filled {
                    content_ids: vec![TimelinePanePresenter::ID.into()],
                    active_index: 0,
                }),
                rest: Box::new(DockState::Splitted {
                    direction: persistence::DockDirection::Left(160.0),
                    content: Box::new(DockState::Filled {
                        content_ids: vec![ui::pane::object_tree::Presenter::ID.into()],
                        active_index: 0,
                    }),
                    rest: Box::new(DockState::Filled {
                        content_ids: vec![
                            PreviewPanePresenter::ID.into(),
                            ProjectSettingsPanePresenter::ID.into(),
                            AssetPreviewPanePresenter::ID.into(),
                        ],
                        active_index: 0,
                    }),
                }),
            }),
        }),
    }
}

fn construct_dock_from_state(
    state: &DockState,
    root_keyboard_focus_group: KeyboardFocusGroupRef,
    create_context: &mut PaneGroupCreateContext,
    store: &mut ui::dock::DockStore,
    mut pane_constructor: impl FnMut(
        &str,
        &mut ViewInitContext,
    ) -> Box<dyn ui::dock::PaneContentPresenter>,
) -> ui::dock::DockID {
    fn rec(
        this: &DockState,
        root_keyboard_focus_group: KeyboardFocusGroupRef,
        create_context: &mut PaneGroupCreateContext,
        store: &mut ui::dock::DockStore,
        parent: ui::dock::DockID,
        pane_constructor: &mut impl FnMut(
            &str,
            &mut ViewInitContext,
        ) -> Box<dyn ui::dock::PaneContentPresenter>,
    ) -> ui::dock::DockID {
        match this {
            &DockState::Filled {
                ref content_ids,
                active_index,
            } => store.alloc_fill(
                parent,
                create_context,
                |view_init_ctx| {
                    content_ids
                        .iter()
                        .map(|x| pane_constructor(x, view_init_ctx))
                        .collect()
                },
                active_index,
            ),
            DockState::Splitted {
                direction,
                content,
                rest,
            } => store.alloc_recurse(|parent1, store| ui::dock::Dock::Splitted {
                parent,
                direction: match direction {
                    &persistence::DockDirection::Left(w) => {
                        ui::dock::DockDirection::ToLeft(Cell::new(w))
                    }
                    &persistence::DockDirection::Right(w) => {
                        ui::dock::DockDirection::ToRight(Cell::new(w))
                    }
                    &persistence::DockDirection::Top(w) => {
                        ui::dock::DockDirection::ToTop(Cell::new(w))
                    }
                    &persistence::DockDirection::Bottom(w) => {
                        ui::dock::DockDirection::ToBottom(Cell::new(w))
                    }
                },
                splitter: create_context.construct_view_direct(|_| {
                    Box::new(ui::dock::DockedPaneSplitterView::new(
                        match direction {
                            persistence::DockDirection::Left(_)
                            | persistence::DockDirection::Right(_) => {
                                ui::dock::DockedPaneSplitDirection::Horizontal
                            }
                            persistence::DockDirection::Top(_)
                            | persistence::DockDirection::Bottom(_) => {
                                ui::dock::DockedPaneSplitDirection::Vertical
                            }
                        },
                        parent1,
                    ))
                }),
                docked: rec(
                    content,
                    root_keyboard_focus_group,
                    create_context,
                    store,
                    parent1,
                    pane_constructor,
                ),
                rest: rec(
                    rest,
                    root_keyboard_focus_group,
                    create_context,
                    store,
                    parent1,
                    pane_constructor,
                ),
            }),
        }
    }

    store.alloc_root(|parent, store| {
        rec(
            state,
            root_keyboard_focus_group,
            create_context,
            store,
            parent,
            &mut pane_constructor,
        )
    })
}

pub struct FileSystem {
    resources_base_path: PathBuf,
    cache_base_path: PathBuf,
    persist_state_base_path: PathBuf,
}
impl FileSystem {
    #[tracing::instrument]
    pub fn new() -> Self {
        // TODO: リリース版だとresourcesの場所はかわる
        #[cfg(not(target_os = "macos"))]
        let resources_base_path = std::env::current_exe()
            .expect("fs.resources_base_path.current_exe")
            .parent()
            .expect("fs.resources_base_path.current_exe.parent")
            .join("../../../core/resources");
        #[cfg(target_os = "macos")] // macのはこれで確定（bundleするときにここにコピーしてる）
        let resources_base_path = std::env::current_exe()
            .expect("fs.resources._base_path.current_exe")
            .parent()
            .expect("fs.resources_base_pat.current_exe.parent")
            .join("../Resources/resources");

        #[cfg(target_os = "macos")]
        let cache_base_path = PathBuf::from(unsafe {
            core::ffi::CStr::from_ptr(
                crate::platform::mac::bridge::ni_query_filesystem_cachedir_path(),
            )
            .to_str()
            .expect("fs.cache_base_path.invalid_str")
        })
        .join("peridot/.editor");
        #[cfg(target_os = "macos")]
        let persist_state_base_path = PathBuf::from(unsafe {
            core::ffi::CStr::from_ptr(
                crate::platform::mac::bridge::ni_query_filesystem_persist_statedir_path(),
            )
            .to_str()
            .expect("fs,persist_state_base_path.invalid_str")
        })
        .join("peridot/.editor");

        #[cfg(unix)]
        let cache_base_path =
            crate::utils::platform::unix::xdg::cache_home().join("io.ct2.peridot.editor");
        #[cfg(unix)]
        let persist_state_base_path =
            crate::utils::platform::unix::xdg::state_home().join("io.ct2.peridot.editor");

        #[cfg(windows)]
        let appdata_base_path =
            crate::utils::platform::windows::local_app_data_dir().join("peridot/.editor");
        #[cfg(windows)]
        let cache_base_path = appdata_base_path.join("cache");
        #[cfg(windows)]
        let persist_state_base_path = appdata_base_path.join("state");

        if let Err(e) = std::fs::create_dir_all(&cache_base_path) {
            tracing::error!(path = ?cache_base_path, reason = %e, "fs.cache_base_path.create_dir_all");
        }

        if let Err(e) = std::fs::create_dir_all(&persist_state_base_path) {
            tracing::error!(path = ?persist_state_base_path, reason = %e, "fs.persist_state_base_path.create_dir_all");
        }

        tracing::info!(
            resources_base_path = %resources_base_path.display(),
            cache_base_path = %cache_base_path.display(),
            persist_state_base_path = %persist_state_base_path.display(),
            "filesystem initialized"
        );

        Self {
            resources_base_path,
            cache_base_path,
            persist_state_base_path,
        }
    }

    #[inline(always)]
    pub fn resolve_resource_path(&self, path: impl AsRef<Path>) -> PathBuf {
        self.resources_base_path.join(path)
    }

    #[inline(always)]
    pub fn resolve_cache_path(&self, path: impl AsRef<Path>) -> PathBuf {
        self.cache_base_path.join(path)
    }

    #[inline(always)]
    pub fn resolve_persist_state_path(&self, path: impl AsRef<Path>) -> PathBuf {
        self.persist_state_base_path.join(path)
    }

    #[inline(always)]
    pub fn window_state_save_path(&self) -> PathBuf {
        self.resolve_persist_state_path("window_state")
    }
}

#[cfg(target_os = "linux")]
struct DBusWatcher<'e> {
    epoll: &'e Epoll,
    last_poll_id: u64,
    fd_to_poll_id: std::collections::HashMap<core::ffi::c_int, u64>,
    poll_id_to_watch_ref:
        &'e core::cell::UnsafeCell<std::collections::HashMap<u64, *mut dbus::WatchRef>>,
}
#[cfg(target_os = "linux")]
impl dbus::WatchFunction for DBusWatcher<'_> {
    #[tracing::instrument(target = "dbus", skip(self, watch), fields(fd = watch.as_raw_fd()))]
    fn add(&mut self, watch: &mut dbus::WatchRef) -> bool {
        if watch.enabled() {
            tracing::trace!("add watch");

            let mut event_type = EpollEventBits::empty();
            let flags = watch.flags();
            if flags.contains(dbus::WatchFlags::READABLE) {
                event_type |= EpollEventBits::IN;
            }
            if flags.contains(dbus::WatchFlags::WRITABLE) {
                event_type |= EpollEventBits::OUT;
            }

            let poll_id = self.last_poll_id;
            self.last_poll_id += 1;
            self.fd_to_poll_id.insert(watch.as_raw_fd(), poll_id);
            unsafe {
                (*self.poll_id_to_watch_ref.get()).insert(poll_id, watch);
            }
            if let Err(e) = self.epoll.add(watch, event_type, poll_id) {
                tracing::error!(reason = %e, "dbus.watcher.epolll.add");
            }
        }

        true
    }

    #[tracing::instrument(target = "dbus", skip(self, watch), fields(fd = watch.as_raw_fd()))]
    fn remove(&mut self, watch: &mut dbus::WatchRef) {
        let Some(poll_id) = self.fd_to_poll_id.remove(&watch.as_raw_fd()) else {
            // not added?
            return;
        };

        tracing::trace!(poll_id, "remove watch");

        unsafe {
            (*self.poll_id_to_watch_ref.get()).remove(&poll_id);
        }
        if poll_id == self.last_poll_id - 1 {
            // できるだけ再利用する
            self.last_poll_id -= 1;
        }

        match self.epoll.del(&watch.as_raw_fd()) {
            // ENOENTは無視
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {}
            Err(e) => {
                tracing::error!(reason = %e, "dbus.watcher.epoll.del");
            }
            Ok(_) => (),
        }
    }

    fn toggled(&mut self, watch: &mut dbus::WatchRef) {
        if watch.enabled() {
            self.add(watch);
        } else {
            self.remove(watch);
        }
    }
}

bitflags::bitflags! {
    #[derive(Clone, Copy)]
    pub struct PreviewKeyInputState : u8 {
        const W = 0x01;
        const A = 0x02;
        const S = 0x04;
        const D = 0x08;
        const SHIFT = 0x10;
        const CONTROL = 0x20;
    }
}

struct PreviewInputState {
    new_viewport_size: Option<Size<LogicalUnit>>,
    scroll_amount: f32,
    grabbing: bool,
    clicked: bool,
    grab_delta: Point<LogicalUnit>,
    key_input: PreviewKeyInputState,
    pointer_pos: Option<Point<LogicalUnit>>,
}
impl PreviewInputState {
    pub fn new() -> Self {
        Self {
            new_viewport_size: None,
            scroll_amount: 0.0,
            grabbing: false,
            clicked: false,
            grab_delta: Point::new_logical(0.0, 0.0),
            key_input: PreviewKeyInputState::empty(),
            pointer_pos: None,
        }
    }
}

enum ManipulationState {
    None,
    Camera,
    Translate {
        pointing: HandlePointing,
        base_object_pos: peridot_math::Vector3F32,
        base_cursor_pos: peridot_math::Vector3F32,
        grab_sum: Point<LogicalUnit>,
    },
    Rotate {
        pointing: HandlePointing,
        base_object_rot: peridot_math::Vector3F32,
        base_cursor_pos: peridot_math::Vector3F32,
        grab_sum: Point<LogicalUnit>,
    },
    Scale {
        pointing: HandlePointing,
        base_object_scale: peridot_math::Vector3F32,
        base_cursor_pos: peridot_math::Vector3F32,
        grab_sum: Point<LogicalUnit>,
    },
}

struct PreviewMainThreadState {
    manipulation_state: ManipulationState,
    latched_key_motion_amplifier: Option<f32>,
    render_shape_to_mesh_id: HashMap<ObjectRenderShape, usize>,
    last_available_mesh_id: usize,
    free_mesh_ids: BTreeSet<usize>,
    last_available_render_id: usize,
    free_render_ids: BTreeSet<usize>,
}
impl PreviewMainThreadState {
    pub fn new() -> Self {
        Self {
            manipulation_state: ManipulationState::None,
            latched_key_motion_amplifier: None,
            render_shape_to_mesh_id: HashMap::new(),
            last_available_mesh_id: 0,
            free_mesh_ids: BTreeSet::new(),
            last_available_render_id: 0,
            free_render_ids: BTreeSet::new(),
        }
    }

    #[profiler::instrument("MainThread.Preview.Update")]
    pub fn update(
        &mut self,
        committed_state: &mut rendering::preview::CommittedState,
        input: &mut PreviewInputState,
        application: &mut ApplicationMutation,
    ) {
        if let Some(new_viewport_size) = input.new_viewport_size.take() {
            committed_state.viewport_size = new_viewport_size;
        }

        let scroll_amount = core::mem::replace(&mut input.scroll_amount, 0.0);
        let grab_delta = core::mem::replace(&mut input.grab_delta, Point::new_logical(0.0, 0.0));
        let clicked = core::mem::replace(&mut input.clicked, false);

        loop {
            match self.manipulation_state {
                ManipulationState::None => {
                    if scroll_amount != 0.0 {
                        // move by scroll
                        let amplifier =
                            5.0f32.powf(if committed_state.main_camera.position.1 == 0.0 {
                                0.0
                            } else {
                                committed_state.main_camera.position.1.abs().log10().floor()
                            });
                        committed_state.main_camera.position = committed_state.main_camera.position
                            + committed_state.main_camera.forward()
                                * 0.25
                                * amplifier
                                * scroll_amount;
                        committed_state.main_camera_dirtified = true;
                    }

                    if clicked && let Some(pointer_pos) = input.pointer_pos {
                        // TODO: 必要なら最適化する

                        let ray = committed_state.main_camera.viewport_point_to_world_ray(
                            peridot_math::Vector2(
                                pointer_pos.x / committed_state.viewport_size.width,
                                pointer_pos.y / committed_state.viewport_size.height,
                            ),
                            committed_state.viewport_size.width
                                / committed_state.viewport_size.height,
                        );
                        let mut selected_oid = None;
                        for (oid, o) in application.state.objects.iter().enumerate() {
                            if o.hittest_ray(&ray) {
                                selected_oid = Some(ObjectID::from_array_index(oid));
                                break;
                            }
                        }

                        match selected_oid {
                            Some(oid) => {
                                model::select_object(application, oid);
                            }
                            None => {
                                model::object_deselect_all(application);
                            }
                        }
                    }

                    if input.grabbing {
                        // grab start on this frame

                        if let Some(&selected) = application.selected_objects.iter().next()
                            && let Some(pointer_pos) = input.pointer_pos
                        {
                            let current_handle_shape =
                                match model::preview_edit_tool_type(application) {
                                    PreviewEditToolType::Translate => {
                                        rendering::preview::HandleShape::Translation
                                    }
                                    PreviewEditToolType::Rotate => {
                                        rendering::preview::HandleShape::Rotation
                                    }
                                    PreviewEditToolType::Scale => {
                                        rendering::preview::HandleShape::Scale
                                    }
                                };

                            let handle_matrix =
                                &application.objects[selected.into_array_index()].world_matrix;
                            let handle_pos = peridot_math::Vector3(
                                handle_matrix.0[3],
                                handle_matrix.1[3],
                                handle_matrix.2[3],
                            );

                            let ray = committed_state.main_camera.viewport_point_to_world_ray(
                                peridot_math::Vector2(
                                    pointer_pos.x / committed_state.viewport_size.width,
                                    pointer_pos.y / committed_state.viewport_size.height,
                                ),
                                committed_state.viewport_size.width
                                    / committed_state.viewport_size.height,
                            );

                            let handle_scale =
                                (committed_state.main_camera.position - handle_pos).len();
                            if let Some(pointing) = Self::hittest_with_handle(
                                current_handle_shape,
                                handle_scale,
                                &handle_pos,
                                &ray,
                            ) {
                                self.manipulation_state = match current_handle_shape {
                                    rendering::preview::HandleShape::Translation => {
                                        ManipulationState::Translate {
                                            pointing,
                                            base_object_pos: application.objects
                                                [selected.into_array_index()]
                                            .local_position,
                                            base_cursor_pos: committed_state
                                                .main_camera
                                                .viewport_point_to_world_point(
                                                    peridot_math::Vector2(
                                                        pointer_pos.x
                                                            / committed_state.viewport_size.width,
                                                        pointer_pos.y
                                                            / committed_state.viewport_size.height,
                                                    ),
                                                    committed_state.viewport_size.width
                                                        / committed_state.viewport_size.height,
                                                ),
                                            grab_sum: pointer_pos,
                                        }
                                    }
                                    rendering::preview::HandleShape::Rotation => {
                                        ManipulationState::Rotate {
                                            pointing,
                                            base_object_rot: application.objects
                                                [selected.into_array_index()]
                                            .local_rotation_euler,
                                            base_cursor_pos: committed_state
                                                .main_camera
                                                .viewport_point_to_world_point(
                                                    peridot_math::Vector2(
                                                        pointer_pos.x
                                                            / committed_state.viewport_size.width,
                                                        pointer_pos.y
                                                            / committed_state.viewport_size.height,
                                                    ),
                                                    committed_state.viewport_size.width
                                                        / committed_state.viewport_size.height,
                                                ),
                                            grab_sum: pointer_pos,
                                        }
                                    }
                                    rendering::preview::HandleShape::Scale => {
                                        ManipulationState::Scale {
                                            pointing,
                                            base_object_scale: application.objects
                                                [selected.into_array_index()]
                                            .local_scale,
                                            base_cursor_pos: committed_state
                                                .main_camera
                                                .viewport_point_to_world_point(
                                                    peridot_math::Vector2(
                                                        pointer_pos.x
                                                            / committed_state.viewport_size.width,
                                                        pointer_pos.y
                                                            / committed_state.viewport_size.height,
                                                    ),
                                                    committed_state.viewport_size.width
                                                        / committed_state.viewport_size.height,
                                                ),
                                            grab_sum: pointer_pos,
                                        }
                                    }
                                };
                                break;
                            }
                        }

                        self.manipulation_state = ManipulationState::Camera;
                        continue;
                    } else {
                        break;
                    }
                }
                ManipulationState::Camera => {
                    if grab_delta.x != 0.0 || grab_delta.y != 0.0 {
                        // rotate by grab
                        committed_state.main_camera.rotation = committed_state.main_camera.rotation
                            * peridot_math::Quaternion::new(
                                grab_delta.y * 0.5f32.to_radians(),
                                peridot_math::Matrix3::from(committed_state.main_camera.rotation)
                                    * peridot_math::Vector3::left(),
                            )
                            * peridot_math::Quaternion::new(
                                grab_delta.x * 0.5f32.to_radians(),
                                peridot_math::Vector3::down(),
                            );
                        committed_state.main_camera_dirtified = true;
                    }

                    if scroll_amount != 0.0 {
                        // move by scroll
                        let amplifier =
                            5.0f32.powf(if committed_state.main_camera.position.1 == 0.0 {
                                0.0
                            } else {
                                committed_state.main_camera.position.1.abs().log10().floor()
                            });
                        committed_state.main_camera.position = committed_state.main_camera.position
                            + committed_state.main_camera.forward()
                                * 0.25
                                * amplifier
                                * scroll_amount;
                        committed_state.main_camera_dirtified = true;
                    }

                    if input.grabbing {
                        let mut key_forwards = 0.0f32;
                        let mut key_rights = 0.0f32;
                        let mut key_y_motions = 0.0f32;
                        if input.key_input.contains(PreviewKeyInputState::W) {
                            key_forwards += 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::S) {
                            key_forwards -= 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::D) {
                            key_rights += 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::A) {
                            key_rights -= 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::SHIFT) {
                            key_y_motions += 1.0;
                        }
                        if input.key_input.contains(PreviewKeyInputState::CONTROL) {
                            key_y_motions -= 1.0;
                        }

                        if key_forwards != 0.0 || key_rights != 0.0 || key_y_motions != 0.0 {
                            // move by key
                            let amplifier =
                                *self.latched_key_motion_amplifier.get_or_insert_with(|| {
                                    2.5f32.powf(if committed_state.main_camera.position.1 == 0.0 {
                                        0.0
                                    } else {
                                        committed_state.main_camera.position.1.abs().log10().floor()
                                    })
                                });
                            committed_state.main_camera.position = committed_state
                                .main_camera
                                .position
                                + committed_state.main_camera.forward()
                                    * (0.25 * amplifier * key_forwards)
                                + committed_state.main_camera.right()
                                    * (0.25 * amplifier * key_rights)
                                + peridot_math::Vector3(0.0, key_y_motions * 0.25 * amplifier, 0.0);
                            committed_state.main_camera_dirtified = true;
                        } else {
                            self.latched_key_motion_amplifier = None;
                        }
                    } else {
                        self.latched_key_motion_amplifier = None;
                        self.manipulation_state = ManipulationState::None;
                    }

                    break;
                }
                ManipulationState::Translate {
                    pointing,
                    base_object_pos,
                    base_cursor_pos,
                    ref mut grab_sum,
                } => {
                    const SENSITIVITY: f32 = 25.0;

                    if !input.grabbing {
                        self.manipulation_state = ManipulationState::None;
                        continue;
                    }

                    *grab_sum = grab_sum.with_offset(grab_delta);
                    let cursor_pos = committed_state.main_camera.viewport_point_to_world_point(
                        peridot_math::Vector2(
                            grab_sum.x / committed_state.viewport_size.width,
                            grab_sum.y / committed_state.viewport_size.height,
                        ),
                        committed_state.viewport_size.width / committed_state.viewport_size.height,
                    );
                    let move_delta = (cursor_pos - base_cursor_pos) * SENSITIVITY;

                    match pointing {
                        HandlePointing::X => {
                            model::set_selected_object_local_translate_x(
                                application,
                                base_object_pos.0 + move_delta.0,
                            );
                        }
                        HandlePointing::Y => {
                            model::set_selected_object_local_translate_y(
                                application,
                                base_object_pos.1 + move_delta.1,
                            );
                        }
                        HandlePointing::Z => {
                            model::set_selected_object_local_translate_z(
                                application,
                                base_object_pos.2 + move_delta.2,
                            );
                        }
                        HandlePointing::All => {
                            // nop for translate
                        }
                    }

                    break;
                }
                ManipulationState::Rotate {
                    pointing,
                    base_object_rot,
                    base_cursor_pos,
                    ref mut grab_sum,
                } => {
                    if !input.grabbing {
                        self.manipulation_state = ManipulationState::None;
                        continue;
                    }
                    const SENSITIVITY: f32 = 90.0;

                    *grab_sum = grab_sum.with_offset(grab_delta);
                    let cursor_pos = committed_state.main_camera.viewport_point_to_world_point(
                        peridot_math::Vector2(
                            grab_sum.x / committed_state.viewport_size.width,
                            grab_sum.y / committed_state.viewport_size.height,
                        ),
                        committed_state.viewport_size.width / committed_state.viewport_size.height,
                    );
                    let move_delta = (cursor_pos - base_cursor_pos) * SENSITIVITY;

                    // TODO: ここ見る軸はこれであってるか？
                    match pointing {
                        HandlePointing::X => {
                            model::set_selected_object_local_rotation_x(
                                application,
                                base_object_rot.0 - move_delta.1,
                            );
                        }
                        HandlePointing::Y => {
                            model::set_selected_object_local_rotation_y(
                                application,
                                base_object_rot.1 + move_delta.0,
                            );
                        }
                        HandlePointing::Z => {
                            model::set_selected_object_local_rotation_z(
                                application,
                                base_object_rot.2 - move_delta.1,
                            );
                        }
                        HandlePointing::All => {
                            // nop for rotation
                        }
                    }

                    break;
                }
                ManipulationState::Scale {
                    pointing,
                    base_object_scale,
                    base_cursor_pos,
                    ref mut grab_sum,
                } => {
                    if !input.grabbing {
                        self.manipulation_state = ManipulationState::None;
                        continue;
                    }
                    const SENSITIVITY: f32 = 25.0;

                    *grab_sum = grab_sum.with_offset(grab_delta);
                    let cursor_pos = committed_state.main_camera.viewport_point_to_world_point(
                        peridot_math::Vector2(
                            grab_sum.x / committed_state.viewport_size.width,
                            grab_sum.y / committed_state.viewport_size.height,
                        ),
                        committed_state.viewport_size.width / committed_state.viewport_size.height,
                    );
                    let move_delta = (cursor_pos - base_cursor_pos) * SENSITIVITY;

                    match pointing {
                        HandlePointing::X => {
                            model::set_selected_object_local_scale_x(
                                application,
                                base_object_scale.0 + move_delta.0,
                            );
                        }
                        HandlePointing::Y => {
                            model::set_selected_object_local_scale_y(
                                application,
                                base_object_scale.1 + move_delta.1,
                            );
                        }
                        HandlePointing::Z => {
                            model::set_selected_object_local_scale_z(
                                application,
                                base_object_scale.2 + move_delta.2,
                            );
                        }
                        HandlePointing::All => {
                            let scale_all = move_delta.len();
                            model::set_selected_object_local_scale(
                                application,
                                base_object_scale
                                    + peridot_math::Vector3(scale_all, scale_all, scale_all),
                            );
                        }
                    }

                    break;
                }
            }
        }

        let mut process_stack = Vec::new();
        process_stack.extend(application.world_matrix_recompute_targets.iter().copied());
        while let Some(id) = process_stack.pop() {
            match application.objects[id.into_array_index()].parent {
                None => {
                    // this is root object: compute direct matrix
                    let o = &mut application.state.objects[id.into_array_index()];
                    o.world_matrix = o.compute_local_matrix();
                    o.render_dirty = true;
                }
                Some(parent_id) => {
                    if application
                        .state
                        .world_matrix_recompute_targets
                        .contains(&parent_id)
                    {
                        // parent is scheduled to be updated the world matrix
                        continue;
                    }

                    let parent_matrix = application.state.objects[parent_id.into_array_index()]
                        .world_matrix
                        .clone();
                    let o = &mut application.state.objects[id.into_array_index()];
                    o.world_matrix = parent_matrix * o.compute_local_matrix();
                    o.render_dirty = true;
                }
            }

            application.state.world_matrix_recompute_targets.remove(&id);
            process_stack.extend(
                application.state.objects[id.into_array_index()]
                    .children
                    .iter()
                    .copied(),
            );
        }

        for o in application.state.removed_object_render_ids.drain(..) {
            committed_state.removed_render_data.insert(o);
        }

        for o in application.state.objects.iter_mut() {
            if core::mem::replace(&mut o.render_dirty, false) {
                // update object render data
                if !o.render_enabled {
                    if let Some(current_render_id) = o.render_id.take() {
                        committed_state
                            .removed_render_data
                            .insert(current_render_id);
                        self.free_mesh_ids.insert(current_render_id);
                    }
                } else {
                    let mesh_id = *self
                        .render_shape_to_mesh_id
                        .entry(o.render_shape)
                        .or_insert_with(|| {
                            if let Some(rid) = self.free_mesh_ids.pop_first() {
                                committed_state
                                    .dirty_meshes
                                    .insert(rid, mesh_data_for_render_shape(o.render_shape));
                                return rid;
                            }

                            let rid = self.last_available_mesh_id;
                            self.last_available_mesh_id += 1;
                            committed_state
                                .pushed_meshes
                                .push(mesh_data_for_render_shape(o.render_shape));
                            rid
                        });

                    match o.render_id {
                        None => {
                            // first render
                            o.render_id =
                                Some(if let Some(rid) = self.free_render_ids.pop_first() {
                                    committed_state.dirty_render_data.insert(
                                        rid,
                                        rendering::preview::CommittedRenderData {
                                            object_to_world: o.world_matrix.clone(),
                                            mesh_id,
                                        },
                                    );
                                    rid
                                } else {
                                    let rid = self.last_available_render_id;
                                    self.last_available_render_id += 1;
                                    committed_state.pushed_render_data.push(
                                        rendering::preview::CommittedRenderData {
                                            object_to_world: o.world_matrix.clone(),
                                            mesh_id,
                                        },
                                    );
                                    rid
                                });
                        }
                        Some(rid) => {
                            // update existing
                            committed_state.dirty_render_data.insert(
                                rid,
                                rendering::preview::CommittedRenderData {
                                    object_to_world: o.world_matrix.clone(),
                                    mesh_id,
                                },
                            );
                        }
                    }
                }
            }
        }

        let current_handle_shape;
        // TODO: handle for multiple selected?(中心に置くとかになるかな)
        if let Some(&selected) = application.selected_objects.iter().next() {
            let handle_matrix = application.objects[selected.into_array_index()]
                .world_matrix
                .clone();
            let handle_pos =
                peridot_math::Vector3(handle_matrix.0[3], handle_matrix.1[3], handle_matrix.2[3]);
            let handle_matrix = peridot_math::Matrix4::translation(handle_pos);
            if handle_matrix != committed_state.handle_to_world_transform {
                committed_state.handle_to_world_transform = handle_matrix;
                committed_state.handle_data_dirtified = true;
            }

            current_handle_shape = Some(match model::preview_edit_tool_type(application) {
                PreviewEditToolType::Translate => rendering::preview::HandleShape::Translation,
                PreviewEditToolType::Rotate => rendering::preview::HandleShape::Rotation,
                PreviewEditToolType::Scale => rendering::preview::HandleShape::Scale,
            });

            if !input.grabbing {
                let current_handle_pointing = if let Some(pointer_pos) = input.pointer_pos {
                    let ray = committed_state.main_camera.viewport_point_to_world_ray(
                        peridot_math::Vector2(
                            pointer_pos.x / committed_state.viewport_size.width,
                            pointer_pos.y / committed_state.viewport_size.height,
                        ),
                        committed_state.viewport_size.width / committed_state.viewport_size.height,
                    );

                    let handle_scale = (committed_state.main_camera.position - handle_pos).len();
                    Self::hittest_with_handle(
                        unsafe { current_handle_shape.unwrap_unchecked() },
                        handle_scale,
                        &handle_pos,
                        &ray,
                    )
                } else {
                    None
                };

                if current_handle_pointing != committed_state.handle_pointing {
                    committed_state.handle_pointing = current_handle_pointing;
                    committed_state.handle_data_dirtified = true;
                }
            }
        } else {
            current_handle_shape = None;
        }
        if current_handle_shape != committed_state.handle_shape {
            committed_state.handle_shape = current_handle_shape;
            committed_state.handle_data_dirtified = true;
        }
    }

    fn hittest_with_handle(
        shape: rendering::preview::HandleShape,
        scale: f32,
        pos: &peridot_math::Vector3F32,
        ray: &peridot_math::Ray3<f32>,
    ) -> Option<rendering::preview::HandlePointing> {
        match shape {
            rendering::preview::HandleShape::Translation => {
                let scale = peridot_math::Vector3(scale, scale, scale);
                let bbox_x = rendering::preview::handle::TRANSLATE_HANDLE_HITBOX_X
                    .scale(&scale)
                    .translate(pos);
                let bbox_y = rendering::preview::handle::TRANSLATE_HANDLE_HITBOX_Y
                    .scale(&scale)
                    .translate(pos);
                let bbox_z = rendering::preview::handle::TRANSLATE_HANDLE_HITBOX_Z
                    .scale(&scale)
                    .translate(pos);

                if bbox_x.intersect(ray).is_some() {
                    Some(rendering::preview::HandlePointing::X)
                } else if bbox_y.intersect(ray).is_some() {
                    Some(rendering::preview::HandlePointing::Y)
                } else if bbox_z.intersect(ray).is_some() {
                    Some(rendering::preview::HandlePointing::Z)
                } else {
                    None
                }
            }
            rendering::preview::HandleShape::Rotation => {
                let hit_sphere = rendering::preview::handle::ROTATION_HANDLE_HITSPHERE
                    .scale(scale)
                    .translate(pos);
                if let Some(tr) = hit_sphere.intersect(ray) {
                    const SENSIBLE_WIDTH: f32 = 0.02;
                    let p = ray.point(tr.start) - *pos;
                    if -SENSIBLE_WIDTH * scale <= p.0 && p.0 <= SENSIBLE_WIDTH * scale {
                        Some(rendering::preview::HandlePointing::X)
                    } else if -SENSIBLE_WIDTH * scale <= p.1 && p.1 <= SENSIBLE_WIDTH * scale {
                        Some(rendering::preview::HandlePointing::Y)
                    } else if -SENSIBLE_WIDTH * scale <= p.2 && p.2 <= SENSIBLE_WIDTH * scale {
                        Some(rendering::preview::HandlePointing::Z)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            rendering::preview::HandleShape::Scale => {
                let scale = peridot_math::Vector3(scale, scale, scale);
                let bbox_x = rendering::preview::handle::SCALE_HANDLE_HITBOX_X
                    .scale(&scale)
                    .translate(pos);
                let bbox_y = rendering::preview::handle::SCALE_HANDLE_HITBOX_Y
                    .scale(&scale)
                    .translate(pos);
                let bbox_z = rendering::preview::handle::SCALE_HANDLE_HITBOX_Z
                    .scale(&scale)
                    .translate(pos);
                let bbox_center = rendering::preview::handle::SCALE_HANDLE_HITBOX_CENTER
                    .scale(&scale)
                    .translate(pos);

                if bbox_x.intersect(ray).is_some() {
                    Some(rendering::preview::HandlePointing::X)
                } else if bbox_y.intersect(ray).is_some() {
                    Some(rendering::preview::HandlePointing::Y)
                } else if bbox_z.intersect(ray).is_some() {
                    Some(rendering::preview::HandlePointing::Z)
                } else if bbox_center.intersect(ray).is_some() {
                    Some(rendering::preview::HandlePointing::All)
                } else {
                    None
                }
            }
        }
    }
}

const PLANE_VERTICES: &[[peridot_math::Vector4F32; 2]] = &[
    [
        peridot_math::Vector4(-0.5, 0.0, -0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, 0.0, -0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, 0.0, 0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.0, 0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
];
const PLANE_INDICES: &[u16] = &[0, 1, 2, 2, 3, 0];

const CUBE_VERTICES: &[[peridot_math::Vector4F32; 2]] = &[
    // +x
    [
        peridot_math::Vector4(0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
    ],
    // -x
    [
        peridot_math::Vector4(-0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
    ],
    // +y
    [
        peridot_math::Vector4(0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
    ],
    // -y
    [
        peridot_math::Vector4(0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
    ],
    // +z
    [
        peridot_math::Vector4(0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, 0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
    ],
    // -z
    [
        peridot_math::Vector4(0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, 0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
    ],
    [
        peridot_math::Vector4(0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
    ],
    [
        peridot_math::Vector4(-0.5, -0.5, -0.5, 1.0),
        peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
    ],
];
const CUBE_INDICES: &[u16] = &[
    0, 1, 2, 2, 1, 3, // +x
    4, 5, 6, 6, 5, 7, // -x
    8, 9, 10, 10, 9, 11, // +y
    12, 13, 14, 14, 13, 15, // -y
    16, 17, 18, 18, 17, 19, // +z
    20, 21, 22, 22, 21, 23, // -z
];

fn mesh_data_for_render_shape(shape: ObjectRenderShape) -> rendering::preview::CommittedMeshData {
    match shape {
        ObjectRenderShape::Plane => {
            let mut vbuf_bytes = vec![0u8; size_of_val(PLANE_VERTICES)];
            let mut ibuf_bytes = vec![0u8; size_of_val(PLANE_INDICES)];
            unsafe {
                vbuf_bytes.as_mut_ptr().copy_from_nonoverlapping(
                    PLANE_VERTICES.as_ptr().cast(),
                    size_of_val(PLANE_VERTICES),
                );
                ibuf_bytes.as_mut_ptr().copy_from_nonoverlapping(
                    PLANE_INDICES.as_ptr().cast(),
                    size_of_val(PLANE_INDICES),
                );
            }

            rendering::preview::CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: rendering::preview::IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(0..6)]),
            }
        }
        ObjectRenderShape::Cube => {
            let mut vbuf_bytes = vec![0u8; size_of_val(CUBE_VERTICES)];
            let mut ibuf_bytes = vec![0u8; size_of_val(CUBE_INDICES)];
            unsafe {
                vbuf_bytes.as_mut_ptr().copy_from_nonoverlapping(
                    CUBE_VERTICES.as_ptr().cast(),
                    size_of_val(CUBE_VERTICES),
                );
                ibuf_bytes.as_mut_ptr().copy_from_nonoverlapping(
                    CUBE_INDICES.as_ptr().cast(),
                    size_of_val(CUBE_INDICES),
                );
            }

            rendering::preview::CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: rendering::preview::IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(0..36)]),
            }
        }
        ObjectRenderShape::Sphere => {
            const HDIV: usize = 20;
            const VDIV: usize = 10;

            let vertex_count = HDIV * (VDIV + 1);
            let index_count = (HDIV * VDIV) * 6;
            let mut vbuf_bytes =
                vec![0u8; size_of::<[peridot_math::Vector4F32; 2]>() * vertex_count];
            let mut ibuf_bytes = vec![0u8; size_of::<u16>() * index_count];
            tracing::debug!(vertex_count, index_count);
            unsafe {
                let vt = vbuf_bytes
                    .as_mut_ptr()
                    .cast::<[peridot_math::Vector4F32; 2]>();
                let ix = ibuf_bytes.as_mut_ptr().cast::<u16>();

                // TODO: v = 0とv = VDIV - 1を特殊処理したほうがよさそう(形状がfanになる)
                for v in 0..=VDIV {
                    for h in 0..HDIV {
                        let ix_base = (h + v * HDIV) * 6;

                        let (y, yc) =
                            (core::f32::consts::PI * (v as f32 / VDIV as f32 - 0.5)).sin_cos();
                        let (x, z) = (core::f32::consts::TAU * h as f32 / HDIV as f32).sin_cos();
                        let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();

                        vt.add(h + v * HDIV).write_unaligned([
                            peridot_math::Vector4(x * yc * 0.5, y * 0.5, z * yc * 0.5, 1.0),
                            n.clone().with_w(0.0),
                        ]);
                        if v < VDIV {
                            let v0 = v;
                            let v1 = v + 1;
                            let h0 = h;
                            let h1 = (h + 1) % HDIV;
                            ix.add(ix_base + 0).write_unaligned((h0 + v0 * HDIV) as _);
                            ix.add(ix_base + 2).write_unaligned((h1 + v0 * HDIV) as _);
                            ix.add(ix_base + 1).write_unaligned((h1 + v1 * HDIV) as _);
                            ix.add(ix_base + 3).write_unaligned((h0 + v0 * HDIV) as _);
                            ix.add(ix_base + 5).write_unaligned((h1 + v1 * HDIV) as _);
                            ix.add(ix_base + 4).write_unaligned((h0 + v1 * HDIV) as _);
                        }
                    }
                }
            }

            rendering::preview::CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: rendering::preview::IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(
                    0..index_count as u32,
                )]),
            }
        }
        ObjectRenderShape::Cylinder => {
            const DIV_COUNT: usize = 16;

            let vertex_count = 2 + DIV_COUNT * 2 + DIV_COUNT * 2;
            let index_count = (DIV_COUNT * 3) * 2 + (DIV_COUNT * 6);
            let mut vbuf_bytes =
                vec![0u8; size_of::<[peridot_math::Vector4F32; 2]>() * vertex_count];
            let mut ibuf_bytes = vec![0u8; size_of::<u16>() * index_count];
            unsafe {
                let v = vbuf_bytes
                    .as_mut_ptr()
                    .cast::<[peridot_math::Vector4F32; 2]>();
                let i = ibuf_bytes.as_mut_ptr().cast::<u16>();

                // top/bottom center point
                v.add(0).write_unaligned([
                    peridot_math::Vector4(0.0, 0.5, 0.0, 1.0),
                    peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                ]);
                v.add(1).write_unaligned([
                    peridot_math::Vector4(0.0, -0.5, 0.0, 1.0),
                    peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                ]);

                let top_plane_vts_index_base = 2;
                let top_plane_ix_base = 0;
                let bottom_plane_vts_index_base = top_plane_vts_index_base + DIV_COUNT;
                let bottom_plane_ix_base = top_plane_ix_base + DIV_COUNT * 3;
                let side_plane_vts_index_base = bottom_plane_vts_index_base + DIV_COUNT;
                let side_plane_ix_base = bottom_plane_ix_base + DIV_COUNT * 3;
                for n in 0..DIV_COUNT {
                    let th = core::f32::consts::TAU * n as f32 / DIV_COUNT as f32;
                    let (s, c) = th.sin_cos();

                    // top/bottom plane
                    v.add(top_plane_vts_index_base + n).write_unaligned([
                        peridot_math::Vector4(s * 0.5, 0.5, c * 0.5, 1.0),
                        peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                    ]);
                    v.add(bottom_plane_vts_index_base + n).write_unaligned([
                        peridot_math::Vector4(s * 0.5, -0.5, c * 0.5, 1.0),
                        peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                    ]);
                    i.add(top_plane_ix_base + n * 3 + 0).write_unaligned(0);
                    i.add(top_plane_ix_base + n * 3 + 1)
                        .write_unaligned((top_plane_vts_index_base + (n + 1) % DIV_COUNT) as _);
                    i.add(top_plane_ix_base + n * 3 + 2)
                        .write_unaligned((top_plane_vts_index_base + n) as _);
                    i.add(bottom_plane_ix_base + n * 3 + 0).write_unaligned(1);
                    i.add(bottom_plane_ix_base + n * 3 + 1)
                        .write_unaligned((bottom_plane_vts_index_base + n) as _);
                    i.add(bottom_plane_ix_base + n * 3 + 2)
                        .write_unaligned((bottom_plane_vts_index_base + (n + 1) % DIV_COUNT) as _);

                    // side plane
                    v.add(side_plane_vts_index_base + n * 2 + 0)
                        .write_unaligned([
                            peridot_math::Vector4(s * 0.5, 0.5, c * 0.5, 1.0),
                            peridot_math::Vector4(s, 0.0, c, 0.0),
                        ]);
                    v.add(side_plane_vts_index_base + n * 2 + 1)
                        .write_unaligned([
                            peridot_math::Vector4(s * 0.5, -0.5, c * 0.5, 1.0),
                            peridot_math::Vector4(s, 0.0, c, 0.0),
                        ]);
                    i.add(side_plane_ix_base + n * 6 + 0)
                        .write_unaligned((side_plane_vts_index_base + n * 2 + 0) as _);
                    i.add(side_plane_ix_base + n * 6 + 1).write_unaligned(
                        (side_plane_vts_index_base + ((n + 1) % DIV_COUNT) * 2 + 0) as _,
                    );
                    i.add(side_plane_ix_base + n * 6 + 2)
                        .write_unaligned((side_plane_vts_index_base + n * 2 + 1) as _);
                    i.add(side_plane_ix_base + n * 6 + 3).write_unaligned(
                        (side_plane_vts_index_base + ((n + 1) % DIV_COUNT) * 2 + 0) as _,
                    );
                    i.add(side_plane_ix_base + n * 6 + 4).write_unaligned(
                        (side_plane_vts_index_base + ((n + 1) % DIV_COUNT) * 2 + 1) as _,
                    );
                    i.add(side_plane_ix_base + n * 6 + 5)
                        .write_unaligned((side_plane_vts_index_base + n * 2 + 1) as _);
                }
            }

            rendering::preview::CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: rendering::preview::IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(
                    0..index_count as u32,
                )]),
            }
        }
        ObjectRenderShape::Capsule => {
            const HDIV: usize = 20;
            const VDIV: usize = 3;

            let vertex_count = 2 + (HDIV * VDIV) * 2;
            let index_count = HDIV * 6 + (HDIV * VDIV) * 12 + HDIV * 6;
            let mut vbuf_bytes =
                vec![0u8; size_of::<[peridot_math::Vector4F32; 2]>() * vertex_count];
            let mut ibuf_bytes = vec![0u8; size_of::<u16>() * index_count];
            tracing::debug!(vertex_count, index_count);
            unsafe {
                let vt = vbuf_bytes
                    .as_mut_ptr()
                    .cast::<[peridot_math::Vector4F32; 2]>();
                let ix = ibuf_bytes.as_mut_ptr().cast::<u16>();

                // peaks
                vt.write_unaligned([
                    peridot_math::Vector4(0.0, 0.5, 0.0, 1.0),
                    peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                ]);
                vt.add(1).write_unaligned([
                    peridot_math::Vector4(0.0, -0.5, 0.0, 1.0),
                    peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                ]);

                // first v layer(v 0 -> 1)
                let v_base = 2;
                for h in 0..HDIV {
                    let (y, yc) = (core::f32::consts::PI * (0.5 / VDIV as f32 - 0.5)).sin_cos();
                    let (x, z) = (core::f32::consts::TAU * h as f32 / HDIV as f32).sin_cos();
                    let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();
                    vt.add(v_base + h * 2 + 0).write_unaligned([
                        peridot_math::Vector4(x * yc * 0.25, y * 0.25 - 0.25, z * yc * 0.25, 1.0),
                        n.with_w(0.0),
                    ]);
                    let (y, yc) = (core::f32::consts::PI * (-0.5 / VDIV as f32 + 0.5)).sin_cos();
                    let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();
                    vt.add(v_base + h * 2 + 1).write_unaligned([
                        peridot_math::Vector4(x * yc * 0.25, y * 0.25 + 0.25, z * yc * 0.25, 1.0),
                        n.with_w(0.0),
                    ]);

                    ix.add(h * 6 + 0).write_unaligned(1);
                    ix.add(h * 6 + 1).write_unaligned((v_base + h * 2 + 0) as _);
                    ix.add(h * 6 + 2)
                        .write_unaligned((v_base + ((h + 1) % HDIV) * 2 + 0) as _);
                    ix.add(h * 6 + 3).write_unaligned(0);
                    ix.add(h * 6 + 5).write_unaligned((v_base + h * 2 + 1) as _);
                    ix.add(h * 6 + 4)
                        .write_unaligned((v_base + ((h + 1) % HDIV) * 2 + 1) as _);
                }

                // middle v layers
                for v in 2..=VDIV {
                    for h in 0..HDIV {
                        let ix_base = (h + (v - 1) * HDIV) * 12;

                        let (y, yc) = (core::f32::consts::PI
                            * (0.5 * v as f32 / VDIV as f32 - 0.5))
                            .sin_cos();
                        let (x, z) = (core::f32::consts::TAU * h as f32 / HDIV as f32).sin_cos();
                        let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();

                        vt.add(v_base + (h + (v - 1) * HDIV) * 2 + 0)
                            .write_unaligned([
                                peridot_math::Vector4(
                                    x * yc * 0.25,
                                    y * 0.25 - 0.25,
                                    z * yc * 0.25,
                                    1.0,
                                ),
                                n.clone().with_w(0.0),
                            ]);
                        let v0 = v - 2;
                        let v1 = v - 1;
                        let h0 = h;
                        let h1 = (h + 1) % HDIV;
                        ix.add(ix_base + 0)
                            .write_unaligned((v_base + (h0 + v0 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 2)
                            .write_unaligned((v_base + (h1 + v0 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 1)
                            .write_unaligned((v_base + (h1 + v1 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 3)
                            .write_unaligned((v_base + (h0 + v0 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 5)
                            .write_unaligned((v_base + (h1 + v1 * HDIV) * 2 + 0) as _);
                        ix.add(ix_base + 4)
                            .write_unaligned((v_base + (h0 + v1 * HDIV) * 2 + 0) as _);

                        let (y, yc) = (core::f32::consts::PI
                            * (-0.5 * v as f32 / VDIV as f32 + 0.5))
                            .sin_cos();
                        let n = peridot_math::Vector3(x * yc, y, z * yc).normalize();

                        vt.add(v_base + (h + (v - 1) * HDIV) * 2 + 1)
                            .write_unaligned([
                                peridot_math::Vector4(
                                    x * yc * 0.25,
                                    y * 0.25 + 0.25,
                                    z * yc * 0.25,
                                    1.0,
                                ),
                                n.clone().with_w(0.0),
                            ]);
                        let v0 = v - 2;
                        let v1 = v - 1;
                        let h0 = h;
                        let h1 = (h + 1) % HDIV;
                        ix.add(ix_base + 6)
                            .write_unaligned((v_base + (h0 + v0 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 7)
                            .write_unaligned((v_base + (h1 + v0 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 8)
                            .write_unaligned((v_base + (h1 + v1 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 9)
                            .write_unaligned((v_base + (h0 + v0 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 10)
                            .write_unaligned((v_base + (h1 + v1 * HDIV) * 2 + 1) as _);
                        ix.add(ix_base + 11)
                            .write_unaligned((v_base + (h0 + v1 * HDIV) * 2 + 1) as _);
                    }
                }

                // side planes
                for h in 0..HDIV {
                    let ix_base = HDIV * 6 + (HDIV * VDIV) * 12 + h * 6;
                    let v_base0 = v_base + (HDIV * (VDIV - 1) + h) * 2;
                    let v_base1 = v_base + (HDIV * (VDIV - 1) + (h + 1) % HDIV) * 2;

                    ix.add(ix_base + 0).write_unaligned((v_base0 + 0) as _);
                    ix.add(ix_base + 1).write_unaligned((v_base0 + 1) as _);
                    ix.add(ix_base + 2).write_unaligned((v_base1 + 0) as _);
                    ix.add(ix_base + 3).write_unaligned((v_base1 + 0) as _);
                    ix.add(ix_base + 5).write_unaligned((v_base1 + 1) as _);
                    ix.add(ix_base + 4).write_unaligned((v_base0 + 1) as _);
                }
            }

            rendering::preview::CommittedMeshData {
                vertices: std::sync::Arc::from(vbuf_bytes),
                vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
                indices: std::sync::Arc::from(ibuf_bytes),
                index_type: rendering::preview::IndexType::U16,
                sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(
                    0..index_count as u32,
                )]),
            }
        }
    }
}

struct PreviewToolSelectorButtonView {
    round_top: bool,
    round_bottom: bool,
    pos: Point<LogicalUnit>,
    label: String,
    bound_tool_type: PreviewEditToolType,
    entity: Option<Rc<PreviewToolSelectorButtonViewEntity>>,
    selecting: bool,
}
impl PreviewToolSelectorButtonView {
    const SIZE: f32 = 24.0;
    const ROUNDING: f32 = 8.0;
    const SELECTING_COLOR: [f32; 4] = [0.25, 0.5, 1.0, 0.5];
    const DESELECTING_COLOR: [f32; 4] = [0.25, 0.25, 0.25, 0.5];

    fn new(
        round_top: bool,
        round_bottom: bool,
        pos: Point<LogicalUnit>,
        label: String,
        bound_tool_type: PreviewEditToolType,
    ) -> Self {
        Self {
            round_top,
            round_bottom,
            pos,
            label,
            bound_tool_type,
            entity: None,
            selecting: false,
        }
    }

    fn set_selecting(&mut self, selecting: bool) {
        self.selecting = selecting;
    }
}
impl View for PreviewToolSelectorButtonView {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                if self.selecting != e.selecting.replace(self.selecting) {
                    // TODO: reflect selecting
                    ctx.composite_tree
                        .begin_mod_chain(e.ct_root)
                        .composite_mode(CompositeMode::FillColorBackdropBlur(
                            AnimatableColor::Animated {
                                from_value: if self.selecting {
                                    Self::DESELECTING_COLOR
                                } else {
                                    Self::SELECTING_COLOR
                                },
                                to_value: if self.selecting {
                                    Self::SELECTING_COLOR
                                } else {
                                    Self::DESELECTING_COLOR
                                },
                                curve: AnimationCurve::Linear,
                                event_on_complete: None,
                                sec_duration: (ctx.current_sec..ctx.current_sec + 0.1).into(),
                            },
                            AnimatableFloat::Value(3.0),
                        ))
                        .apply();
                }

                e
            }
            None => {
                // first render
                let rounding = match (self.round_top, self.round_bottom) {
                    (false, false) => CornerRadius::all(0.0),
                    (true, false) => CornerRadius {
                        left_top: [Self::ROUNDING, Self::ROUNDING],
                        right_top: [Self::ROUNDING, Self::ROUNDING],
                        left_bottom: [0.0, 0.0],
                        right_bottom: [0.0, 0.0],
                    },
                    (false, true) => CornerRadius {
                        left_top: [0.0, 0.0],
                        right_top: [0.0, 0.0],
                        left_bottom: [Self::ROUNDING, Self::ROUNDING],
                        right_bottom: [Self::ROUNDING, Self::ROUNDING],
                    },
                    (true, true) => CornerRadius {
                        left_top: [Self::ROUNDING, Self::ROUNDING],
                        right_top: [Self::ROUNDING, Self::ROUNDING],
                        left_bottom: [Self::ROUNDING, Self::ROUNDING],
                        right_bottom: [Self::ROUNDING, Self::ROUNDING],
                    },
                };

                let ct_root = ctx.composite_tree.create(CompositeRect {
                    scale_factor: CompositeRectScaleFactor::UI,
                    size: [
                        AnimatableFloat::Value(Self::SIZE),
                        AnimatableFloat::Value(Self::SIZE),
                    ],
                    offset: [
                        AnimatableFloat::Value(self.pos.x),
                        AnimatableFloat::Value(self.pos.y),
                    ],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColorBackdropBlur(
                        AnimatableColor::Value(if self.selecting {
                            Self::SELECTING_COLOR
                        } else {
                            Self::DESELECTING_COLOR
                        }),
                        AnimatableFloat::Value(3.0),
                    ),
                    corner_radius: rounding.clone(),
                    border: Some(Border {
                        thickness: 1.0,
                        color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                        ..Default::default()
                    }),
                    text: Some(CompositeRectText {
                        runs: vec![CompositeRectTextRun {
                            content: self.label.clone(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        }],
                        horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                        vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                        ..Default::default()
                    }),
                    ..Default::default()
                });
                let ct_hover_lit = ctx.composite_tree.create(CompositeRect {
                    relative_size_adjustment: [1.0, 1.0],
                    has_bitmap: true,
                    composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                        1.0, 1.0, 1.0, 0.0,
                    ])),
                    corner_radius: rounding,
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width: Self::SIZE,
                    height: Self::SIZE,
                    left: self.pos.x,
                    top: self.pos.y,
                    cursor_shape: CursorShape::Pointer,
                    ..Default::default()
                });
                ctx.composite_tree.add_child(ct_root, ct_hover_lit);

                let entity = Rc::new(PreviewToolSelectorButtonViewEntity {
                    ct_root,
                    ct_hover_lit,
                    ht_root,
                    bound_tool_type: self.bound_tool_type,
                    selecting: Cell::new(self.selecting),
                });
                ctx.ht_manager.set_action_handler(ht_root, &entity);

                &*self.entity.insert(entity)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendering
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(Self::SIZE, Self::SIZE)
    }
}

struct PreviewToolSelectorButtonViewEntity {
    ct_root: CompositeTreeRef,
    ct_hover_lit: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    bound_tool_type: PreviewEditToolType,
    selecting: Cell<bool>,
}
impl HitTestTreeActionHandler for PreviewToolSelectorButtonViewEntity {
    fn on_pointer_enter(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_hover_lit)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.1],
                curve: AnimationCurve::Linear,
                event_on_complete: None,
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_hover_lit)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.1],
                to_value: [1.0, 1.0, 1.0, 0.0],
                curve: AnimationCurve::Linear,
                event_on_complete: None,
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_start(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        model::set_preview_edit_tool_type(context, self.bound_tool_type);

        EventContinueControl::STOP_PROPAGATION
    }
}

pub struct PreviewPanePresenter {
    root_view_id: TypedViewIdentifier<PreviewView>,
    feedback_receiver: Rc<PreviewPaneFeedbackReceiver>,
}
impl PreviewPanePresenter {
    const ID: &str = internal_pane_identifier!("Preview");

    fn new(ctx: &mut ViewInitContext, input_state: *mut PreviewInputState) -> Self {
        let root_view = ctx.construct_view_direct(|_| Box::new(PreviewView::new(input_state)));
        let translate_control_button = ctx.construct_view_direct(|_| {
            Box::new(PreviewToolSelectorButtonView::new(
                true,
                false,
                Point::new_logical(8.0, 8.0),
                "T".into(),
                PreviewEditToolType::Translate,
            ))
        });
        let rotate_control_button = ctx.construct_view_direct(|_| {
            Box::new(PreviewToolSelectorButtonView::new(
                false,
                false,
                Point::new_logical(8.0, 8.0 + 24.0 - 1.0),
                "R".into(),
                PreviewEditToolType::Rotate,
            ))
        });
        let scale_control_button = ctx.construct_view_direct(|_| {
            Box::new(PreviewToolSelectorButtonView::new(
                false,
                true,
                Point::new_logical(8.0, 8.0 + 48.0 - 2.0),
                "S".into(),
                PreviewEditToolType::Scale,
            ))
        });
        ctx.view_set_parent(translate_control_button, root_view);
        ctx.view_set_parent(rotate_control_button, root_view);
        ctx.view_set_parent(scale_control_button, root_view);

        let feedback_receiver = Rc::new(PreviewPaneFeedbackReceiver {
            translate_tool_button_view_id: translate_control_button,
            rotate_tool_button_view_id: rotate_control_button,
            scale_tool_button_view_id: scale_control_button,
        });
        ctx.subscribe_view_feedback::<model::ViewFeedbackPreviewEditToolTypeChanged>(
            &feedback_receiver,
        );

        Self {
            root_view_id: root_view,
            feedback_receiver,
        }
    }
}
impl ui::dock::PaneContentPresenter for PreviewPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Preview".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view_id.into_untyped()
    }

    fn resize(&self, new_size: &Size<LogicalUnit>, context: &mut PaneContentResizeContext) {
        unsafe {
            &mut *context
                .view_instance_mut(self.root_view_id)
                .expect("query failed")
                .input_state
        }
        .new_viewport_size = Some(new_size.clone());
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<model::ViewFeedbackPreviewEditToolTypeChanged>(
            &self.feedback_receiver,
        );
    }
}

pub struct PreviewPaneFeedbackReceiver {
    translate_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
    rotate_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
    scale_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
}
impl ViewFeedbackHandler<model::ViewFeedbackPreviewEditToolTypeChanged>
    for PreviewPaneFeedbackReceiver
{
    fn accept_feedback<'a, 'h, 'sys>(
        &self,
        _feedback: &model::ViewFeedbackPreviewEditToolTypeChanged,
        context: &mut ViewFeedbackContext<'a, 'h, 'sys>,
    ) {
        let is_selecting = model::preview_edit_tool_type(context) == PreviewEditToolType::Translate;
        context
            .view_instance_mut(self.translate_tool_button_view_id)
            .expect("query failed")
            .set_selecting(is_selecting);
        context.schedule_view_render(self.translate_tool_button_view_id);

        let is_selecting = model::preview_edit_tool_type(context) == PreviewEditToolType::Rotate;
        context
            .view_instance_mut(self.rotate_tool_button_view_id)
            .expect("query failed")
            .set_selecting(is_selecting);
        context.schedule_view_render(self.rotate_tool_button_view_id);

        let is_selecting = model::preview_edit_tool_type(context) == PreviewEditToolType::Scale;
        context
            .view_instance_mut(self.scale_tool_button_view_id)
            .expect("query failed")
            .set_selecting(is_selecting);
        context.schedule_view_render(self.scale_tool_button_view_id);
    }
}

struct PreviewView {
    input_state: *mut PreviewInputState,
    entity: Option<Rc<PreviewViewEntity>>,
}
impl PreviewView {
    pub fn new(input_state: *mut PreviewInputState) -> Self {
        Self {
            input_state,
            entity: None,
        }
    }
}
impl View for PreviewView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                // first render
                let kf_token = ctx.keyboard_focus_registry.acquire_token();
                let ct_root = ctx.composite_tree.create(CompositeRect {
                    // has_bitmap: true,
                    custom_render_token: Some(rendering::PREVIEW_COMPOSITE),
                    relative_size_adjustment: [1.0, 1.0],
                    ..Default::default()
                });
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width_adjustment_factor: 1.0,
                    height_adjustment_factor: 1.0,
                    keyboard_focus: Some(kf_token),
                    ..Default::default()
                });

                let entity = Rc::new(PreviewViewEntity {
                    ct_root,
                    ht_root,
                    kf_token,
                    input_state: self.input_state,
                });
                ctx.ht_manager.set_action_handler(ht_root, &entity);
                ctx.keyboard_focus_registry
                    .set_event_handler(kf_token, &entity);

                &*self.entity.insert(entity)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            keyboard_focus: Some(e.kf_token),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free(entity.ct_root);
        ctx.ht_manager.free(entity.ht_root);
        ctx.keyboard_focus_registry.release_token(entity.kf_token);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }

    fn create_new_layout_layer(&self) -> bool {
        true
    }
}

struct PreviewViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    kf_token: FocusTargetToken,
    input_state: *mut PreviewInputState,
}
impl HitTestTreeActionHandler for PreviewViewEntity {
    fn on_scroll_wheel(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        args: &input::hittest::ScrollWheelActionArgs,
    ) -> input::hittest::ScrollWheelActionResponse {
        unsafe { &mut *self.input_state }.scroll_amount += args.amount;

        input::hittest::ScrollWheelActionResponse {
            left_amount: 0.0,
            continue_flags: EventContinueControl::STOP_PROPAGATION,
        }
    }

    fn on_pointer_leave(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.pointer_pos = None;
        EventContinueControl::empty()
    }

    fn on_pointer_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        let (x, y, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        unsafe { &mut *self.input_state }.pointer_pos = Some(Point::new_logical(x, y));

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_start(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.grabbing = true;
        EventContinueControl::GRAB_POINTER | EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_end(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.grabbing = false;
        EventContinueControl::RELEASE_CAPTURE_ELEMENT | EventContinueControl::STOP_PROPAGATION
    }

    fn grab_delta_move(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        args: &input::hittest::GrabDeltaMoveActionArgs,
    ) -> EventContinueControl {
        let st = unsafe { &mut *self.input_state };
        st.grab_delta.x += args.delta.x;
        st.grab_delta.y += args.delta.y;

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.clicked = true;

        EventContinueControl::STOP_PROPAGATION
    }
}
impl KeyInputEventHandler for PreviewViewEntity {
    fn focus_released(&self, _context: &mut InputEventContext) {
        unsafe { &mut *self.input_state }.key_input.clear();
    }

    fn keydown(
        &self,
        _context: &mut InputEventContext,
        code: KeyInputCode,
        _modifier: ModifierKey,
    ) {
        match code {
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'w') => {
                self.set_key(PreviewKeyInputState::W);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'a') => {
                self.set_key(PreviewKeyInputState::A);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'s') => {
                self.set_key(PreviewKeyInputState::S);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'d') => {
                self.set_key(PreviewKeyInputState::D);
            }
            KeyInputCode::RightShift | KeyInputCode::LeftShift => {
                self.set_key(PreviewKeyInputState::SHIFT);
            }
            KeyInputCode::RightControl | KeyInputCode::LeftControl => {
                self.set_key(PreviewKeyInputState::CONTROL);
            }
            _ => (),
        }
    }

    fn keyup(&self, _context: &mut InputEventContext, code: KeyInputCode, _modifier: ModifierKey) {
        tracing::debug!(?code, "keyup");
        match code {
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'w') => {
                self.unset_key(PreviewKeyInputState::W);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'a') => {
                self.unset_key(PreviewKeyInputState::A);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'s') => {
                self.unset_key(PreviewKeyInputState::S);
            }
            KeyInputCode::Character(c) if c.eq_ignore_ascii_case(&'d') => {
                self.unset_key(PreviewKeyInputState::D);
            }
            KeyInputCode::RightShift | KeyInputCode::LeftShift => {
                self.unset_key(PreviewKeyInputState::SHIFT);
            }
            KeyInputCode::RightControl | KeyInputCode::LeftControl => {
                self.unset_key(PreviewKeyInputState::CONTROL);
            }
            _ => (),
        }
    }
}
impl PreviewViewEntity {
    fn set_key(&self, key: PreviewKeyInputState) {
        unsafe { &mut *self.input_state }.key_input.insert(key);
    }

    fn unset_key(&self, key: PreviewKeyInputState) {
        unsafe { &mut *self.input_state }.key_input.remove(key);
    }
}
