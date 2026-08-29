use bitflags::Flags;
use core::cell::Cell;
#[cfg(target_os = "linux")]
use linux_epoll::{Epoll, EpollEventBits};
#[cfg(feature = "wayland")]
use linux_eventfd::{EventFD, EventFDFlags};
use peridot_math::{One, Zero};
#[cfg(target_os = "linux")]
use peridot_tp_dbus as dbus;
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
    model::{
        Application, ApplicationMutation, ObjectID, ObjectRenderShape, PreviewEditToolType,
        ViewFeedbackPreviewEditToolTypeChanged,
    },
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
        text::{FontID, FontSet, RootFontSet, TextLayout},
    },
    ui::dock::{PaneContentResizeContext, PaneGroupCreateContext},
    uikit::{
        ContainerView, ContainerViewInit, MenuEventHandler, MenuItem, MenuItemCommonResources,
        MenuItemInteractableElement, MountContext, MountTarget, NumericInputView,
        NumericInputViewIO, NumericInputViewInit, PopupID, PopupManager, RadioButtonView,
        RenderContext, ScrollContainer, SimpleButtonEventHandler, SimpleButtonViewInit,
        StaticTextViewInit, TeardownContext, TextInputView, TextInputViewIO, TypedViewIdentifier,
        View, ViewDestructionContext, ViewFeedbackContext, ViewFeedbackHandler,
        ViewFeedbackRegisterable, ViewFeedbackRegistry, ViewGroupID, ViewGroupRegisterable,
        ViewGroupRelationControllable, ViewGroupRelationStore, ViewIdentifier,
        ViewIdentifierAllocator, ViewImmediateRenderable, ViewInitContext,
        ViewInstanceQueryableMut, ViewInstanceStore, ViewLayoutChild, ViewLayoutFlowAlignment,
        ViewLayoutFlowDirection, ViewLayoutFlowJustify, ViewLayoutGridCell, ViewLayoutOverflow,
        ViewLayoutStateStore, ViewRegisterable, ViewRelationControllable, ViewRenderQueue,
        ViewRenderStateStore, ViewRenderer, ViewSize, ViewTreeRelationStore,
    },
    utils::{
        Color32, DummyDebug, LogicalUnit, NonCloneable, Point, Rect, Size,
        UnsafeMainThreadOnlyOnceCell,
    },
};

#[cfg(windows)]
mod bindgen;
mod graphics;
mod input;
mod model;
mod platform;
mod proto;
mod rendering;
mod ui;
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
    let vk_device = Graphics::init(&fs);
    #[cfg(windows)]
    assert!(
        vk_device.presentation_support(),
        "win32 presentation not supported on graphics queue"
    );
    #[cfg(feature = "wayland")]
    assert!(
        dp_context.check_for_vk(&vk_device),
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
    main_wrapper(
        move |args, system_link| run(args, system_link),
        &mut event_store,
        &global_time_base,
        &Mutex::new(RendererSync {
            composite_buffer: CompositeTreeSyncBuffer::new(),
        }),
        &fs,
        &vk_device,
        rt_sender,
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
        &static_pixbufs,
        #[cfg(target_os = "linux")]
        &dbus,
    );

    profiler::fini_profiler();
}

fn main_wrapper<'sys, AppFuture: core::future::Future<Output = ()> + 'sys>(
    run_app: impl FnOnce(LaunchArgs<'sys>, SystemLink<'sys>) -> AppFuture,
    event_store: &mut VecDeque<Event>,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
    fs: &'sys FileSystem,
    gfx: &'sys Graphics,
    rt_sender: RenderMessageSender,
    rt_receiver: std::sync::mpsc::Receiver<RenderMessage>,
    root_font_set: &'sys RootFontSet,
    preview_state: &'sys Mutex<rendering::preview::CommittedState>,
    #[cfg(windows)] app_context: &'sys mut platform::windows::ApplicationContext,
    #[cfg(windows)] dx_context: &'sys platform::windows::DxContext,
    #[cfg(feature = "wayland")] dp_context: &'sys mut platform::unix::wayland::DisplayServerContext,
    #[cfg(feature = "wayland")] static_pixbufs: &'sys platform::unix::wayland::StaticPixbufs,
    #[cfg(target_os = "linux")] dbus: &'sys dbus::Connection,
) {
    profiler::sample_memory!();

    #[cfg(feature = "wayland")]
    let terminate_event = std::sync::Arc::new(
        EventFD::new(0, EventFDFlags::empty()).expect("terminate_event.create"),
    );

    let mut polling = false;
    let empty_dispatcher = LogicFiberEventDispatcher {
        event_store,
        polling: &mut polling,
        poll_fn_ptr: unsafe { core::mem::transmute(AppFuture::poll as *const core::ffi::c_void) },
        future_ptr: core::ptr::null_mut(),
    };
    let mut app_event_dispatcher = core::pin::pin!(empty_dispatcher.clone());

    #[cfg(feature = "wayland")]
    let mut wl_global_msg = core::pin::pin!(platform::unix::wayland::GlobalMessaging::new(
        dp_context,
        static_pixbufs,
        empty_dispatcher.clone()
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
    let mut app = core::pin::pin!(run_app(
        LaunchArgs {
            event_queue: EventQueue { event_store },
            global_time_base,
            renderer_sync,
            file_system: fs,
            committed_preview_state: preview_state
        },
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
            gfx,
            font_set: FontSet::new(root_font_set),
            event_dispatcher: app_event_dispatcher.as_mut().get_mut(),
            #[cfg(target_os = "linux")]
            dbus,
            #[cfg(feature = "wayland")]
            display_server: platform::unix::DisplayServerLink {
                context: dp_context,
                static_pixbufs,
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
    ));

    app_event_dispatcher.future_ptr = unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ };
    #[cfg(feature = "wayland")]
    wl_global_msg
        .as_mut()
        .reset_event_dispatcher(app_event_dispatcher.clone());

    // initial poll
    unsafe {
        core::ptr::write_volatile(&mut polling, true);
    }
    let _ = app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }));
    unsafe {
        core::ptr::write_volatile(&mut polling, false);
    }

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
            .add(&terminate_event, EpollEventBits::IN, 1)
            .expect("epoll.add");
        #[cfg(feature = "wayland")]
        epoll
            .add(&sync_event_bus.efd, EpollEventBits::IN, 2)
            .expect("epoll.add");
        #[cfg(target_os = "linux")]
        epoll
            .add(&pointer_hovering_timer, EpollEventBits::IN, 3)
            .expect("epoll.add");
        #[cfg(feature = "wayland")]
        epoll
            .add(&delayed_action_timer_fd, EpollEventBits::IN, 4)
            .expect("epoll.add");
        #[cfg(feature = "wayland")]
        #[cfg(feature = "enable-profiling")]
        epoll
            .add(&memory_sample_timer_fd, EpollEventBits::IN, 5)
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
                            std::os::fd::AsRawFd::as_raw_fd(&memory_sample_timer_fd),
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
                app_event_dispatcher.dispatch(Event::PointerHover);
            }

            if delayed_action_timer_signal {
                app_event_dispatcher.dispatch(Event::MenuPerformDelayedAction);
            }

            if dbus_signal {
                while let Some(m) = dbus.pop_message() {
                    let span = tracing::info_span!(target: "dbus::loop", "dbus message recv", r#type = ?m.r#type(), path = ?m.path(), interface = ?m.interface(), member = ?m.member());
                    let _enter = span.enter();
                    match m.r#type() {
                        dbus::MessageType::MethodCall
                            if m.path()
                                .is_some_and(|x| x == platform::unix::APPMENU_OBJECT_PATH)
                                && m.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
                                && m.member() == Some(c"GetLayout") =>
                        {
                            let args =
                                proto::dbus_menu::GetLayoutRequest::deserialize(&mut m.iter());

                            tracing::debug!(?args, "com.canonical.dbusmenu.GetLayout");

                            // toriaezu
                            assert_eq!(args.recursion_depth, 1);

                            if args.parent_id == 1 {
                                let mut reply = dbus::Message::new_method_return(&m)
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
                                                shortcut: Some(&[
                                                    &[c"Alt", c"F4"],
                                                    &[c"Meta", c"Q"],
                                                ]),
                                                ..Default::default()
                                            },
                                            children: &[],
                                        }],
                                    },
                                }
                                .serialize(&mut reply.iter_append())
                                .expect("dbus_menu.get_layout.serialize_reply");
                                dbus.send(&mut reply).expect("dbus.send");
                            } else if args.parent_id == 0 {
                                let mut reply = dbus::Message::new_method_return(&m)
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
                                dbus.send(&mut reply).expect("dbus.send");
                            } else {
                                unreachable!("unknown menu id");
                            }
                        }
                        dbus::MessageType::MethodCall
                            if m.path()
                                .is_some_and(|x| x == platform::unix::APPMENU_OBJECT_PATH)
                                && m.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
                                && m.member() == Some(c"Event") =>
                        {
                            let mut args_iter = m.iter();
                            let id = args_iter.try_get_i32().expect("id:i");
                            args_iter.next();
                            let event_id = args_iter.try_get_cstr().expect("event_id:s").to_owned();
                            args_iter.next();
                            let data_container =
                                args_iter.try_begin_iter_variant_content().expect("data:v");
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
                                break 'app;
                            }
                        }
                        dbus::MessageType::MethodCall
                            if m.path()
                                .is_some_and(|x| x == platform::unix::APPMENU_OBJECT_PATH)
                                && m.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
                                && m.member() == Some(c"AboutToShow") =>
                        {
                            let mut args_iter = m.iter();
                            let id = args_iter.try_get_i32().expect("id:i");

                            let mut reply = dbus::Message::new_method_return(&m)
                                .expect("dbus.message.new_method_return");
                            proto::dbus_menu::AboutToShowReply { need_update: false }
                                .serialize(&mut reply.iter_append())
                                .expect("dbus_menu.about_to_show.serialize_reply");
                            dbus.send(&mut reply).expect("dbus.send");
                        }
                        _ => tracing::trace!(target: "dbus::loop", "unknown dbus message"),
                    }
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
    PointerDown {
        window: WindowHandle,
        pointer_id: PointerID,
        button: PointerButton,
        key_modifier: ModifierKey,
    },
    PointerMove {
        pointer_id: PointerID,
        window: WindowHandle,
        client_pos: Point<PointerInputUnit>,
        key_modifier: ModifierKey,
    },
    PointerMoveRelative {
        pointer_id: PointerID,
        window: WindowHandle,
        relative: Point<PointerInputUnit>,
    },
    PointerUp {
        window: WindowHandle,
        pointer_id: PointerID,
        button: PointerButton,
        key_modifier: ModifierKey,
    },
    PointerLeaveWindow {
        window: WindowHandle,
        pointer_id: PointerID,
    },
    PointerHover,
    ScrollWheel {
        amount: f32,
        key_modifier: ModifierKey,
    },
    KeyDown {
        window: WindowHandle,
        code: KeyInputCode,
        modifier: ModifierKey,
    },
    KeyUp {
        window: WindowHandle,
        code: KeyInputCode,
        modifier: ModifierKey,
    },
    KeyChar {
        window: WindowHandle,
        ch: char,
        modifier: ModifierKey,
    },
    IMEStateChanges {
        window: WindowHandle,
        committed_string: Option<String>,
        preedit_string: Option<String>,
    },
    WindowMove {
        window: WindowHandle,
        pos: Point<PointerInputUnit>,
    },
    WindowResize {
        window: WindowHandle,
        size: Size<PointerInputUnit>,
    },
    WindowRescaleUI {
        window: WindowHandle,
        new_scale: f32,
    },
    WindowMaximizeStateChanged {
        window: WindowHandle,
        is_maximized: bool,
    },
    WindowFocusChanged {
        window: WindowHandle,
        focused: bool,
    },
    WindowActivatingStateChanged {
        window: WindowHandle,
        activated: bool,
    },
    SubWindowClose {
        window: WindowHandle,
    },
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
    MenuOpen {
        parent: WindowHandle,
        items: Vec<MenuItem>,
        surface_pos: Point<LogicalUnit>,
    },
    MenuReopen {
        parent: WindowHandle,
        items: Vec<MenuItem>,
        surface_pos: Point<LogicalUnit>,
    },
    DropdownMenuOpen {
        parent: WindowHandle,
        surface_pos: Point<LogicalUnit>,
        min_width: f32,
        items: Vec<crate::uikit::dropdown_box::MenuItem>,
        selection_receiver: std::rc::Weak<crate::uikit::dropdown_box::EventHandler>,
    },
    MenuCloseAll,
    MenuRescale {
        scale: f32,
    },
    MenuSelectItem {
        depth: usize,
        index: usize,
    },
    MenuDeselectItem {
        depth: usize,
    },
    MenuOpenSubmenu {
        depth: usize,
        index: usize,
    },
    MenuPerformDelayedAction,
    MenuPointerDown {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
        button: PointerButton,
        key_modifier: ModifierKey,
    },
    MenuPointerMove {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
        client_pos: Point<PointerInputUnit>,
        key_modifier: ModifierKey,
    },
    MenuPointerUp {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
        button: PointerButton,
        key_modifier: ModifierKey,
    },
    MenuPointerLeave {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
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
    DockMovePreview {
        dest_window: WindowHandle,
        client_pos_in_dest: Point<LogicalUnit>,
    },
    DockConfirm {
        pointer: PointerID,
        destination_window: WindowHandle,
        client_pos_in_dest: Point<LogicalUnit>,
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
    #[cfg(feature = "enable-profiling")]
    pub const fn p_name(&self) -> &'static str {
        match self {
            Self::Sync(e) => e.p_name(),
            Self::Quit => "Quit",
            Self::PointerDown { .. } => "PointerDown",
            Self::PointerMove { .. } => "PointerMove",
            Self::PointerMoveRelative { .. } => "PointerMoveRelative",
            Self::PointerUp { .. } => "PointerUp",
            Self::PointerLeaveWindow { .. } => "PointerLeaveWindow",
            Self::PointerHover => "PointerHover",
            Self::ScrollWheel { .. } => "ScrollWheel",
            Self::KeyDown { .. } => "KeyDown",
            Self::KeyUp { .. } => "KeyUp",
            Self::KeyChar { .. } => "KeyChar",
            Self::IMEStateChanges { .. } => "IMEStateChanges",
            Self::WindowMove { .. } => "WindowMove",
            Self::WindowResize { .. } => "WindowResize",
            Self::WindowRescaleUI { .. } => "WindowRescaleUI",
            Self::WindowMaximizeStateChanged { .. } => "WindowMaximizeStateChanged",
            Self::WindowFocusChanged { .. } => "WindowFocusChanged",
            Self::WindowActivatingStateChanged { .. } => "WindowActivatingStateChanged",
            Self::SubWindowClose { .. } => "SubWindowClose",
            Self::OpenAlertDialog { .. } => "OpenAlertDialog",
            Self::PopupClose { .. } => "PopupClose",
            Self::OpenCustomViewFlyout { .. } => "OpenCustomViewFlyout",
            Self::MenuOpen { .. } => "MenuOpen",
            Self::MenuReopen { .. } => "MenuReopen",
            Self::DropdownMenuOpen { .. } => "DropdownMenuOpen",
            Self::MenuCloseAll => "MenuCloseAll",
            Self::MenuRescale { .. } => "MenuRescale",
            Self::MenuSelectItem { .. } => "MenuSelectItem",
            Self::MenuDeselectItem { .. } => "MenuDeselectItem",
            Self::MenuOpenSubmenu { .. } => "MenuOpenSubmenu",
            Self::MenuPerformDelayedAction => "MenuPerformDelayedAction",
            Self::MenuPointerDown { .. } => "MenuPointerDown",
            Self::MenuPointerMove { .. } => "MenuPointerMove",
            Self::MenuPointerUp { .. } => "MenuPointerUp",
            Self::MenuPointerLeave { .. } => "MenuPointerLeave",
            Self::MenuSelectCommand { .. } => "MenuSelectCommand",
            Self::DropdownMenuSelectItem { .. } => "DropdownMenuSelectItem",
            Self::DockMoveSplitter { .. } => "DockMoveSplitter",
            Self::DockBeginPreview { .. } => "DockBeginPreview",
            Self::DockMovePreview { .. } => "DockMovePreview",
            Self::DockConfirm { .. } => "DockConfirm",
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

pub const MENU_COMMAND_ID_OBJECT_CREATE_PLANE: u64 = 1;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CUBE: u64 = 2;
pub const MENU_COMMAND_ID_OBJECT_CREATE_SPHERE: u64 = 3;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER: u64 = 4;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE: u64 = 5;
pub const MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN: u64 = 10;
pub const MENU_COMMAND_ID_OBJECT_DESTROY_SELECTED: u64 = 20;
pub const MENU_COMMAND_ID_OBJECT_DUPLICATE_SELECTED: u64 = 21;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CHILD_PLANE: u64 = 31;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CHILD_CUBE: u64 = 32;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CHILD_SPHERE: u64 = 33;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CHILD_CYLINDER: u64 = 34;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CHILD_CAPSULE: u64 = 35;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CHILD_SP_TERRAIN: u64 = 310;

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
    ) -> uikit::ViewRenderElements {
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

        uikit::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..uikit::ViewRenderElements::EMPTY
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

    fn measure_preferred_content_size(
        &self,
        _ctx: &mut uikit::MeasureContext,
    ) -> Size<LogicalUnit> {
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
    ) -> uikit::ViewRenderElements {
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

        uikit::ViewRenderElements {
            composite_tree: Some(e.core.entity().ct_root()),
            hit_tree: Some(e.core.entity().ht_root()),
            keyboard_focus: Some(e.token),
            ..uikit::ViewRenderElements::EMPTY
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

    fn measure_preferred_content_size(&self, ctx: &mut uikit::MeasureContext) -> Size<LogicalUnit> {
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
    ) -> uikit::ViewRenderElements {
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

        uikit::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..uikit::ViewRenderElements::EMPTY
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

    fn measure_preferred_content_size(&self, ctx: &mut uikit::MeasureContext) -> Size<LogicalUnit> {
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
        Self(ctx.construct_view(|_| Box::new(v)))
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

        let content_view = ctx.construct_view(|id| Box::new(ContainerView));
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

        let mut ytop = 8.0;
        let mut content_width = 8.0f32;

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

        let container = ctx.construct_view2(ContainerViewInit, |ctx| {
            let label = ctx.construct_view2(
                StaticTextViewInit {
                    content: "Simple Buttons + Alert Dialog".into(),
                    ..Default::default()
                },
                |_| [],
            );

            let button_container = ctx.construct_view2(ContainerViewInit, |ctx| {
                const LONG_MESSAGE: &str = "とてもとても長いメッセージで自動折り返しをしてみる ああああああああああああああああああああああああああああああ";

                    [ctx.construct_view2(SimpleButtonViewInit {
                         label: "Test Alert".into(),
                                        event_handler: Some(Box::new(AlertButtonEventHandler(
                                            "てすとめっせーじ from button\n改行もしてみる".into(),
                                        ))),
                                    }, |_| []).into_untyped(), ctx.construct_view2(SimpleButtonViewInit {
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

        let container = ctx.construct_view2(ContainerViewInit, |ctx| {
            [
                ctx.construct_view2(
                    StaticTextViewInit {
                        content: "Text Input(Single Line)".into(),
                        ..Default::default()
                    },
                    |_| [],
                )
                .into_untyped(),
                {
                    let v = ctx.construct_view2(ContainerViewInit, |ctx| {
                        [
                            {
                                let v = ctx.construct_view(|id| {
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
                                let v = ctx.construct_view(|id| {
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

        let container = ctx.construct_view2(ContainerViewInit, |ctx| {
            [
                ctx.construct_view2(
                    StaticTextViewInit {
                        content: "Text Input (Multiline)".into(),
                        ..Default::default()
                    },
                    |_| [],
                )
                .into_untyped(),
                {
                    let v =
                        ctx.construct_view(|id| Box::new(uikit::MultilineTextInputView::new(id)));
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
        let label = ctx.construct_view2(
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
        let color_picker = ctx.construct_view(|_| Box::new(color_picker));
        ctx.view_set_parent(color_picker, content_view);

        let toggle_button =
            ctx.construct_view(|_| Box::new(uikit::ToggleButtonView::new("Toggle".into())));
        ctx.view_set_parent(toggle_button, content_view);

        // inline controls preview
        let container = ctx.construct_view2(ContainerViewInit, |_| []);
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

        let label = ctx.construct_view2(
            StaticTextViewInit {
                content: "Color Picker(Button Style)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let editable_color_button =
            ctx.construct_view(|id| Box::new(EditableColorButtonView::new(id, 0xffffffff)));
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
        let label = ctx.construct_view2(
            StaticTextViewInit {
                content: "Numeric Input".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let numeric_input_view = ctx.construct_view(|id| {
            Box::new(NumericInputView::new(
                id,
                NumericInputViewInit {
                    value: Rc::downgrade(&numeric_input_view_backing_store),
                    ..Default::default()
                },
            ))
        });
        {
            let l = ctx
                .view_layout_mut(numeric_input_view)
                .expect("query failed");
            l.width = ViewSize::Fixed(64.0);
            l.height = ViewSize::Fixed(20.0);
        }
        ctx.view_set_parent(numeric_input_view, container);

        let dropdown_value_store = Rc::new(UIKitPreviewDropdownValueStore(Cell::new(0)));
        let label = ctx.construct_view2(
            StaticTextViewInit {
                content: "Dropdown".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let dropdown_box = ctx.construct_view(|id| {
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

        let label = ctx.construct_view2(
            StaticTextViewInit {
                content: "Single Checkbox".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let checkbox = ctx.construct_view(|_| Box::new(uikit::CheckboxView::new()));
        ctx.view_set_parent(checkbox, container);

        let rgc1 = ctx.create_view_group();
        let label = ctx.construct_view2(
            StaticTextViewInit {
                content: "Radio Button (Group 1)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let radio_button1 = ctx.construct_view(|id| Box::new(RadioButtonView::new(id)));
        ctx.join_view_group(radio_button1, rgc1);
        ctx.view_set_parent(radio_button1, container);
        let label = ctx.construct_view2(
            StaticTextViewInit {
                content: "Radio Button (Group 1)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let radio_button2 = ctx.construct_view(|id| Box::new(RadioButtonView::new(id)));
        ctx.join_view_group(radio_button2, rgc1);
        ctx.view_set_parent(radio_button2, container);
        let label = ctx.construct_view2(
            StaticTextViewInit {
                content: "Radio Button (Group 1)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let radio_button3 = ctx.construct_view(|id| Box::new(RadioButtonView::new(id)));
        ctx.join_view_group(radio_button3, rgc1);
        ctx.view_set_parent(radio_button3, container);
        let label = ctx.construct_view2(
            StaticTextViewInit {
                content: "Radio Button (No group)".into(),
                ..Default::default()
            },
            |_| [],
        );
        ctx.view_set_parent(label, container);
        let radio_button4 = ctx.construct_view(|id| Box::new(RadioButtonView::new(id)));
        ctx.view_set_parent(radio_button4, container);

        let scroll_container = ctx.construct_view(|id| {
            Box::new(ScrollContainer::new(
                id,
                Rect::from_lt_size(
                    Point::new_logical(0.0, 0.0),
                    Size::new_logical(256.0, 128.0),
                ),
                content_view.into_untyped(),
            ))
        });
        ctx.view_set_parent(content_view, scroll_container);

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
        context
            .view_instance_mut(self.scroll_container)
            .expect("query failed")
            .resize(new_size.clone());
        let content_width = new_size.width.max(128.0);
        context
            .view_layout_mut(self.content_view)
            .expect("query failed")
            .width = ViewSize::Fixed(content_width);
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
            root_view_id: ctx.construct_view(|_| Box::new(ContainerView)),
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

struct AssetExplorerPanePresenter {
    root_view_id: TypedViewIdentifier<AssetExplorerFileListView>,
}
impl AssetExplorerPanePresenter {
    const ID: &str = internal_pane_identifier!("AssetExplorer");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        Self {
            root_view_id: ctx.construct_view(|_| Box::new(AssetExplorerFileListView::new())),
        }
    }
}
impl ui::dock::PaneContentPresenter for AssetExplorerPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Asset Explorer".into()
    }

    fn root_view_id(&self) -> ViewIdentifier {
        self.root_view_id.into_untyped()
    }
}

pub struct AssetExplorerFileListView {
    entity: Option<Rc<AssetExplorerFileListViewEntity>>,
}
impl AssetExplorerFileListView {
    pub fn new() -> Self {
        Self { entity: None }
    }
}
impl View for AssetExplorerFileListView {
    fn render(
        &mut self,
        _layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> uikit::ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => e,
            None => {
                let ct_root = CompositeRect::build()
                    .expand_full()
                    .create(ctx.composite_tree);
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    width_adjustment_factor: 1.0,
                    height_adjustment_factor: 1.0,
                    ..Default::default()
                });

                let element = AssetExplorerTiledElementSubView::new(
                    ctx.composite_tree,
                    ctx.ht_manager,
                    ctx.system_link,
                    "toolongelementname.asset".into(),
                );
                ctx.composite_tree.add_child(ct_root, element.ct_root);
                ctx.ht_manager.add_child(ht_root, element.ht_root);

                let entity = Rc::new(AssetExplorerFileListViewEntity {
                    ct_root,
                    ht_root,
                    elements: vec![element],
                });

                &*self.entity.insert(entity)
            }
        };

        uikit::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..uikit::ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        _ctx: &mut uikit::MeasureContext,
    ) -> Size<LogicalUnit> {
        Size::new_logical(0.0, 0.0)
    }
}

struct AssetExplorerFileListViewEntity {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    elements: Vec<AssetExplorerTiledElementSubView>,
}

struct AssetExplorerTiledElementSubView {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl AssetExplorerTiledElementSubView {
    const MARGIN: f32 = 8.0;
    const ICON_TEXT_MARGIN: f32 = 2.0;
    const TEXT_WIDTH_MAX: f32 = 64.0;

    pub fn new<E>(
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        syslink: &SystemLink,
        label: String,
    ) -> Self {
        let label_metric = TextLayout::new_single(
            &label,
            FontID::UIDefault,
            syslink.font_set(),
            CompositeRectTextHorizontalAlignment::Middle,
            Some(Self::TEXT_WIDTH_MAX),
            Some(2),
        )
        .size();

        let ct_root = CompositeRect::build()
            .use_ui_scale()
            .size_imm(
                Self::TEXT_WIDTH_MAX + Self::MARGIN * 2.0,
                32.0 + Self::MARGIN * 2.0 + label_metric.height + Self::ICON_TEXT_MARGIN,
            )
            .composite_fill_color_imm([0.0; 4])
            .border(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            })
            .corner_radius(CornerRadius::all(4.0))
            .create(composite_tree);
        let ct_icon = CompositeRect::build()
            .use_ui_scale()
            .composite_fill_color_imm([1.0, 1.0, 1.0, 0.5])
            .size_imm(32.0, 32.0)
            .relative_offset_adjustment(0.5, 0.0)
            .offset_imm(-16.0, Self::MARGIN)
            .create(composite_tree);
        let ct_label = CompositeRect::build()
            .text(
                CompositeRectText::build()
                    .run(CompositeRectTextRun::build(label).color_imm([1.0, 1.0, 1.0, 1.0]))
                    .horizontal_middle()
                    .allow_wrapping()
                    .limit_lines(2),
            )
            .size_imm(Self::TEXT_WIDTH_MAX, 0.0)
            .offset_imm(Self::MARGIN, Self::MARGIN + 32.0 + Self::ICON_TEXT_MARGIN)
            .create(composite_tree);
        let ht_root = ht_manager.create(HitTestTreeData {
            width: Self::TEXT_WIDTH_MAX + Self::MARGIN * 2.0,
            height: 32.0 + Self::MARGIN * 2.0 + label_metric.height + Self::ICON_TEXT_MARGIN,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });

        composite_tree.add_child(ct_root, ct_icon);
        composite_tree.add_child(ct_root, ct_label);

        Self { ct_root, ht_root }
    }
}

struct ProjectSettingsPanePresenter {
    root_view_id: TypedViewIdentifier<ContainerView>,
}
impl ProjectSettingsPanePresenter {
    const ID: &str = internal_pane_identifier!("ProjectSettings");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        Self {
            root_view_id: ctx.construct_view(|_| Box::new(ContainerView)),
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
            root_view_id: ctx.construct_view(|_| Box::new(ContainerView)),
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
    docking_manager: ui::dock::DockingManager,
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
    ) -> uikit::ViewRenderElements {
        uikit::ViewRenderElements::EMPTY
    }

    fn teardown(&mut self, _ctx: &mut TeardownContext) {}

    fn measure_preferred_content_size(&self, ctx: &mut uikit::MeasureContext) -> Size<LogicalUnit> {
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

#[tracing::instrument(target = "peridot_marble_editor::logic_fiber", skip_all)]
async fn run<'sys>(
    LaunchArgs {
        event_queue,
        global_time_base,
        renderer_sync,
        file_system,
        committed_preview_state,
    }: LaunchArgs<'sys>,
    mut system_link: SystemLink<'sys>,
) {
    tracing::info!("app start");
    profiler::begin!(perf = INITIALIZE);

    let mut application = Application::new();
    let mut view_feedback_store = VecDeque::new();
    let mut view_feedback_registry_delayed_ops = VecDeque::new();

    let mut composite_tree = CompositeTree::new();
    let mut ht_manager = HitTestTreeManager::new();
    let mut keyboard_focus_registry = KeyboardFocusTokenRegistry::new();
    let mut pointer_input_manager = PointerInputManager::new();
    let mut view_allocator = ViewIdentifierAllocator::new();
    let mut view_instance_store = ViewInstanceStore::new();
    let mut view_tree_relation_store = ViewTreeRelationStore::new();
    let mut view_group_relation_store = ViewGroupRelationStore::new();
    let mut view_layout_state_store = ViewLayoutStateStore::new();
    let mut view_render_state_store = ViewRenderStateStore::new();
    let mut view_feedback_registry = ViewFeedbackRegistry::new();
    let mut view_render_queue = ViewRenderQueue::new();
    let mut dock_store = ui::dock::DockStore::new();
    let mut texture_id_issuer = MainThreadTextureIDIssuer::new();
    let mut popup_manager = PopupManager::new();

    // WindowsではWM_NCHITTESTの返り値の計算に必要なので一旦生ポインタで参照もたせる（実際どうするかはあとで考える）
    #[cfg(windows)]
    unsafe {
        platform::windows::locate_non_client_hittest_managers(&pointer_input_manager, &ht_manager);
    }

    let context_menu_common_resources = MenuItemCommonResources::new(
        &mut composite_tree,
        &mut texture_id_issuer,
        system_link.rt_sender(),
    );
    let mut current_active_menu_session = None::<MenuSession>;
    let mut current_active_dropdown_menu_session = None::<DropdownMenuSession>;
    let mut custom_view_flyout_session = None::<CustomViewFlyoutSession>;

    let mut delayed_render_messages = Vec::new();
    let mut docking_preview_state = None;

    let mut preview_input_state = PreviewInputState::new();
    let mut preview_state = PreviewMainThreadState::new();

    let last_window_state = 'try_restore_last_window_state: {
        let fp = match std::fs::File::open(file_system.window_state_save_path()) {
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

    let window_bg_gradient = composite_tree.create_gradient(Gradient::Corner {
        right_top: [0.1, 0.1, 0.1, 1.0],
        left_bottom: [0.1, 0.1, 0.1, 1.0],
        right_bottom: [0.05, 0.025, 0.0, 1.0],
    });

    let mut sub_windows = HashSet::new();
    let mut main_window = system_link.create_main_window(
        match last_window_state {
            None => MainWindowOpenMode::New,
            Some(ref x) => MainWindowOpenMode::Restore(x.main.geometry.clone()),
        },
        &mut composite_tree,
        &mut ht_manager,
        &mut keyboard_focus_registry,
        &mut delayed_render_messages,
    );

    let mut view_init_ctx = ViewInitContext {
        mount_context: MountContext {
            composite_tree: &mut composite_tree,
            ht_manager: &mut ht_manager,
            current_sec: global_time_base.elapsed().as_secs_f32(),
            keyboard_focus_registry: &mut keyboard_focus_registry,
        },
        view_allocator: &mut view_allocator,
        view_instance_store: &mut view_instance_store,
        view_tree_relation_store: &mut view_tree_relation_store,
        view_group_relation_store: &mut view_group_relation_store,
        view_layout_state_store: &mut view_layout_state_store,
        view_render_state_store: &mut view_render_state_store,
        view_feedback_subscription_delayed_ops: &mut view_feedback_registry_delayed_ops,
        system_link: &system_link,
        main_thread_texture_id_issuer: &mut texture_id_issuer,
        application: &application,
    };

    view_init_ctx
        .composite_tree
        .begin_mod_chain(main_window.ct_root())
        .has_bitmap(true)
        .composite_mode(CompositeMode::FillCornerGradient(
            window_bg_gradient,
            AnimatableColor::Value([0.0, 0.025, 0.05, 1.0]),
        ))
        .apply();
    let main_window_root_view = view_init_ctx.construct_view(|_| Box::new(WindowRootView {}));
    let window_header = ui::window_header::Component::new(
        ui::window_header::Caption::Main {
            project_name: "New Project".into(),
        },
        ui::window_header::ComponentInit {
            with_system_command_buttons: main_window.needs_system_command_buttons(),
        },
        &mut view_init_ctx,
    );
    view_init_ctx.view_set_parent_untyped(
        window_header.root_view(),
        main_window_root_view.into_untyped(),
    );

    let app_menu_view = if system_link.needs_app_menu_in_surface() {
        let app_menu_view = view_init_ctx.construct_view(|_| {
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
        view_init_ctx.construct_view(|_| Box::new(ui::window_footer::View::new()));
    view_init_ctx.view_set_parent(window_footer_view, main_window_root_view);

    let initial_dock_state = DockState::Splitted {
        direction: DockDirection::Bottom(320.0),
        content: Box::new(DockState::Filled {
            content_ids: vec![AssetExplorerPanePresenter::ID.into()],
            active_index: 0,
        }),
        rest: Box::new(DockState::Splitted {
            direction: DockDirection::Right(256.0),
            content: Box::new(DockState::Filled {
                content_ids: vec![
                    ui::pane::inspector::Presenter::ID.into(),
                    UIKitPreviewPanePresenter::ID.into(),
                ],
                active_index: 0,
            }),
            rest: Box::new(DockState::Splitted {
                direction: DockDirection::Top(120.0),
                content: Box::new(DockState::Filled {
                    content_ids: vec![TimelinePanePresenter::ID.into()],
                    active_index: 0,
                }),
                rest: Box::new(DockState::Splitted {
                    direction: DockDirection::Left(160.0),
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
    };

    let dock_top_offset = ui::window_header::View::THICKNESS
        + if app_menu_view.is_some() {
            ui::app_menu_bar::View::HEIGHT
        } else {
            0.0
        };
    let main_window_size = main_window.client_size();
    main_window.associate_extra_data(Box::new(PerWindowData {
        screen_reposition_interests: HashSet::new(),
        root_view: main_window_root_view,
        header: window_header,
        appmenu: app_menu_view,
        footer: Some(window_footer_view),
        docking_manager: ui::dock::DockingManager::new(
            main_window,
            &mut view_init_ctx,
            &mut view_render_queue,
            Rect::from_lt_size(
                Point::new_logical(0.0, dock_top_offset),
                Size::new_logical(
                    main_window_size.width,
                    main_window_size.height - dock_top_offset - ui::window_footer::View::THICKNESS,
                ),
            ),
            &mut dock_store,
            |view_init_ctx, view_render_queue, store| {
                match last_window_state {
                    None => &initial_dock_state,
                    Some(ref x) => &x.main.dock,
                }
                .construct(
                    main_window.keyboard_focus_group(),
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
                        AssetExplorerPanePresenter::ID => {
                            Box::new(AssetExplorerPanePresenter::new(view_init_ctx))
                        }
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
                            &mut preview_input_state,
                        )),
                        id => todo!("generic pane id handling: {id:?}"),
                    },
                )
            },
        ),
    }));

    view_init_ctx.render_view_with_base(
        main_window_root_view.into_untyped(),
        &main_window,
        main_window.keyboard_focus_group(),
        Rect::from_lt_size(Point::new_logical(0.0, 0.0), main_window.client_size()),
    );

    if let Some(ref last_window_state) = last_window_state {
        for sub in last_window_state.sub.iter() {
            let new_window = system_link.open_window(
                SubWindowOpenMode::Restore(sub.geometry.clone()),
                &mut composite_tree,
                &mut ht_manager,
                &mut keyboard_focus_registry,
                &mut delayed_render_messages,
                |mut w, composite_tree, ht_manager, keyboard_focus_registry, system_link| {
                    ht_manager.get_data_mut(w.ht_root()).root_of_window = Some(w);

                    composite_tree
                        .begin_mod_chain(w.ct_root())
                        .has_bitmap(true)
                        .composite_mode(CompositeMode::FillCornerGradient(
                            window_bg_gradient,
                            AnimatableColor::Value([0.0, 0.025, 0.05, 1.0]),
                        ))
                        .apply();

                    let mut view_feedback_registry_delayed_ops = VecDeque::new();
                    let mut view_init_ctx = ViewInitContext {
                        mount_context: MountContext {
                            composite_tree,
                            ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry,
                        },
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    };
                    let root_view = view_init_ctx.construct_view(|_| Box::new(WindowRootView {}));
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
                        docking_manager: ui::dock::DockingManager::new(
                            w,
                            &mut view_init_ctx,
                            &mut view_render_queue,
                            Rect::from_lt_size(
                                Point::new_logical(0.0, ui::window_header::View::THICKNESS),
                                Size::new_logical(320.0, 240.0),
                            ),
                            &mut dock_store,
                            |view_init_ctx, view_render_queue, store| {
                                sub.dock.construct(
                                    w.keyboard_focus_group(),
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
                                        ui::pane::object_tree::Presenter::ID => Box::new(
                                            ui::pane::object_tree::Presenter::new(view_init_ctx),
                                        ),
                                        ui::pane::inspector::Presenter::ID => Box::new(
                                            ui::pane::inspector::Presenter::new(view_init_ctx),
                                        ),
                                        AssetExplorerPanePresenter::ID => {
                                            Box::new(AssetExplorerPanePresenter::new(view_init_ctx))
                                        }
                                        ProjectSettingsPanePresenter::ID => Box::new(
                                            ProjectSettingsPanePresenter::new(view_init_ctx),
                                        ),
                                        TimelinePanePresenter::ID => {
                                            Box::new(TimelinePanePresenter::new(view_init_ctx))
                                        }
                                        AssetPreviewPanePresenter::ID => {
                                            Box::new(AssetPreviewPanePresenter::new(view_init_ctx))
                                        }
                                        PreviewPanePresenter::ID => {
                                            Box::new(PreviewPanePresenter::new(
                                                view_init_ctx,
                                                &mut preview_input_state,
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
            sub_windows.insert(new_window);
        }
    }

    view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);

    // initial sync model with view
    application.sync(&mut view_feedback_store);
    let mut fb_context = ViewFeedbackContext {
        application: &application,
        composite_tree: &mut composite_tree,
        ht_manager: &mut ht_manager,
        current_sec: global_time_base.elapsed().as_secs_f32(),
        keyboard_focus_registry: &mut keyboard_focus_registry,
        view_allocator: &mut view_allocator,
        view_instance_store: &mut view_instance_store,
        view_tree_relation_store: &mut view_tree_relation_store,
        view_group_relation_store: &mut view_group_relation_store,
        view_layout_state_store: &mut view_layout_state_store,
        view_render_state_store: &mut view_render_state_store,
        view_feedback_subscription_delayed_ops: &mut view_feedback_registry_delayed_ops,
        system_link: &system_link,
        main_thread_texture_id_issuer: &mut texture_id_issuer,

        view_render_queue: &mut view_render_queue,
    };

    for x in view_feedback_store.drain(..) {
        x.dispatch(&view_feedback_registry, &mut fb_context);
    }

    view_feedback_registry.perform_atomic(&mut fb_context);

    view_render_queue.perform(
        &mut RenderContext {
            composite_tree: &mut composite_tree,
            ht_manager: &mut ht_manager,
            keyboard_focus_registry: &mut keyboard_focus_registry,
            current_sec: global_time_base.elapsed().as_secs_f32(),
            system_link: &system_link,
            main_thread_texture_id_issuer: &mut texture_id_issuer,
            application: &application,
            view_feedback_subscription_delayed_ops: &mut view_feedback_registry_delayed_ops,
        },
        &mut view_instance_store,
        &view_tree_relation_store,
        &mut view_layout_state_store,
        &mut view_render_state_store,
    );

    composite_tree.commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
    ht_manager.dump(main_window.ht_root());
    for msg in delayed_render_messages.drain(..) {
        system_link.rt_sender().send(msg).expect("rt_sender.send");
    }
    view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);

    system_link.prelaunch(main_window);
    profiler::end!(perf);

    loop {
        let e = event_queue.next_event().await;
        tracing::trace!(target: "event-trace", event = ?e);
        profiler::scope!(PROCESS_EVENT, str e.p_name());
        match e {
            Event::Quit => break,
            Event::SubWindowClose { mut window } => {
                let wd = unsafe { window.take_extra_data::<PerWindowData>() };
                struct LocalContext<'a, 'h>(ViewInitContext<'a, 'h>);
                impl ViewDestructionContext for LocalContext<'_, '_> {
                    fn destruct_view_recursive_untyped(&mut self, target: ViewIdentifier) {
                        crate::uikit::destruct_view_recursive(
                            target,
                            &mut TeardownContext {
                                composite_tree: &mut self.0.mount_context.composite_tree,
                                ht_manager: &mut self.0.mount_context.ht_manager,
                                keyboard_focus_registry: &mut self
                                    .0
                                    .mount_context
                                    .keyboard_focus_registry,
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
                wd.docking_manager.teardown(
                    &mut dock_store,
                    &mut LocalContext(ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    }),
                );
                sub_windows.remove(&window);
                system_link.close_window(
                    window,
                    &mut composite_tree,
                    &mut ht_manager,
                    &mut keyboard_focus_registry,
                );
            }
            Event::WindowResize { window, size } => {
                let wd = unsafe { window.extra_data_ref::<PerWindowData>() };
                wd.docking_manager.resize(
                    wd.compute_content_area(size),
                    &mut dock_store,
                    &mut PaneContentResizeContext {
                        view_instance_store: &mut view_instance_store,
                        view_render_queue: &mut view_render_queue,
                        composite_tree: &mut composite_tree,
                        ht_manager: &mut ht_manager,
                    },
                );
            }
            Event::Sync(SyncEvent::WindowPostCreateRenderBuffer { window }) => {
                #[cfg(feature = "wayland")]
                window.update_manual_scaling();
            }
            Event::Sync(SyncEvent::FlyoutSurfacePostCreateRenderBuffer { target }) => {
                #[cfg(feature = "wayland")]
                target.update_manual_scaling();
            }
            Event::WindowMove { mut window, pos } => {
                let wd = unsafe { window.extra_data_mut::<PerWindowData>() };
                let mut input_context = InputEventContext {
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
                };

                for &ht in wd.screen_reposition_interests.iter() {
                    if let Some(e) = ht_manager.get_data(ht).screen_reposition_handler() {
                        e.on_screen_reposition_required(ht, &mut input_context, pos);
                    }
                }

                // ContextMenuはウィンドウ移動で消しちゃう（Explorerもこの挙動っぽい）
                if let Some(c) = current_active_menu_session.take_if(|x| x.parent == window) {
                    if let Some(ref a) = unsafe { window.extra_data_ref::<PerWindowData>() }.appmenu
                    {
                        crate::uikit::view_instance::<ui::app_menu_bar::View>(
                            a.into_untyped(),
                            &view_instance_store,
                        )
                        .expect("query failed")
                        .on_close_all(
                            &mut composite_tree,
                            global_time_base.elapsed().as_secs_f32(),
                        );
                    }

                    c.terminate(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }

                if let Some(mut c) =
                    current_active_dropdown_menu_session.take_if(|x| x.parent == window)
                {
                    c.close_all(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }

                if let Some(c) = custom_view_flyout_session.take_if(|x| x.parent == window) {
                    c.terminate(&mut FlyoutSurfaceSessionTerminateContext {
                        syslink: &system_link,
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        teardown_context: TeardownContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                        },
                    });
                }
            }
            Event::WindowRescaleUI { window, new_scale } => {
                popup_manager.rescale(window, new_scale, &mut composite_tree);
            }
            Event::WindowMaximizeStateChanged {
                window,
                is_maximized,
            } => unsafe {
                struct LocalContext<'a> {
                    view_render_queue: &'a mut ViewRenderQueue,
                    view_instance_store: &'a mut ViewInstanceStore,
                }
                impl crate::uikit::ViewInstanceQueryableMut for LocalContext<'_> {
                    #[inline(always)]
                    fn view_instance_mut_of<T: View + 'static>(
                        &mut self,
                        id: ViewIdentifier,
                    ) -> Option<&mut T> {
                        crate::uikit::view_instance_mut(id, self.view_instance_store)
                    }

                    #[inline(always)]
                    fn view_set_visibility_untyped(&mut self, id: ViewIdentifier, visible: bool) {
                        crate::uikit::view_set_visibility(id, visible, self.view_instance_store)
                    }

                    #[inline(always)]
                    fn view_layout_mut_untyped(
                        &mut self,
                        id: ViewIdentifier,
                    ) -> Option<&mut crate::uikit::ViewLayout> {
                        crate::uikit::view_layout_mut(id, self.view_instance_store)
                    }
                }
                impl crate::uikit::ViewRenderer for LocalContext<'_> {
                    #[inline(always)]
                    fn schedule_view_render_untyped(&mut self, target: ViewIdentifier) {
                        self.view_render_queue.schedule(target)
                    }
                }
                window
                    .extra_data_ref::<PerWindowData>()
                    .header
                    .set_maximize_state(
                        is_maximized,
                        &mut LocalContext {
                            view_render_queue: &mut view_render_queue,
                            view_instance_store: &mut view_instance_store,
                        },
                    );
            },
            Event::WindowFocusChanged {
                mut window,
                focused,
            } => {
                let mut input_context = InputEventContext {
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
                };
                let mgr = window.keyboard_focus_state_mut();

                if focused {
                    mgr.notify_window_focus(&mut input_context, &keyboard_focus_registry);
                } else {
                    mgr.notify_window_lost_focus(&mut input_context, &keyboard_focus_registry);
                }

                if !focused
                    && let Some(c) = current_active_menu_session.take_if(|x| x.parent == window)
                {
                    // フォーカスロストした時もコンテキストメニューを閉じる
                    if let Some(ref a) = unsafe { window.extra_data_ref::<PerWindowData>() }.appmenu
                    {
                        crate::uikit::view_instance::<ui::app_menu_bar::View>(
                            a.into_untyped(),
                            &view_instance_store,
                        )
                        .expect("query failed")
                        .on_close_all(
                            &mut composite_tree,
                            global_time_base.elapsed().as_secs_f32(),
                        );
                    }

                    c.terminate(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }
            }
            Event::WindowActivatingStateChanged { window, activated } => {
                if !activated {
                    if let Some(c) = current_active_menu_session.take_if(|x| x.parent == window) {
                        if let Some(ref a) =
                            unsafe { window.extra_data_ref::<PerWindowData>() }.appmenu
                        {
                            crate::uikit::view_instance::<ui::app_menu_bar::View>(
                                a.into_untyped(),
                                &view_instance_store,
                            )
                            .expect("query failed")
                            .on_close_all(
                                &mut composite_tree,
                                global_time_base.elapsed().as_secs_f32(),
                            );
                        }

                        c.terminate(
                            &system_link,
                            &mut composite_tree,
                            &mut ht_manager,
                            &mut keyboard_focus_registry,
                        );
                    }
                }
            }
            Event::PointerDown {
                window,
                pointer_id,
                button,
                key_modifier,
            } => {
                // #[cfg(target_os = "macos")]
                // drag_preview_popover.bind_position_base_window_link(window);

                if let Some(ref a) = unsafe { window.extra_data_ref::<PerWindowData>() }.appmenu {
                    crate::uikit::view_instance::<ui::app_menu_bar::View>(
                        a.into_untyped(),
                        &view_instance_store,
                    )
                    .expect("query failed")
                    .on_close_all(
                        &mut composite_tree,
                        global_time_base.elapsed().as_secs_f32(),
                    );
                }

                if let Some(c) = current_active_menu_session.take() {
                    if let Some(ref a) =
                        unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu
                    {
                        crate::uikit::view_instance::<ui::app_menu_bar::View>(
                            a.into_untyped(),
                            &view_instance_store,
                        )
                        .expect("query failed")
                        .on_close_all(
                            &mut composite_tree,
                            global_time_base.elapsed().as_secs_f32(),
                        );
                    }

                    c.terminate(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }

                if let Some(mut c) = current_active_dropdown_menu_session.take() {
                    c.close_all(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }

                if let Some(c) = custom_view_flyout_session.take() {
                    c.terminate(&mut FlyoutSurfaceSessionTerminateContext {
                        syslink: &system_link,
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        teardown_context: TeardownContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                        },
                    });
                }

                pointer_input_manager.handle_mouse_down(
                    pointer_id,
                    &ht_manager,
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
                    button,
                    key_modifier,
                    window.ht_root(),
                    &mut keyboard_focus_registry,
                );
            }
            Event::PointerMove {
                pointer_id,
                window,
                client_pos,
                key_modifier,
            } => {
                pointer_input_manager.handle_mouse_move(
                    NativeDesktopSurface::Window(window),
                    pointer_id,
                    client_pos,
                    key_modifier,
                    &ht_manager,
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
                    window.ht_root(),
                );

                let cursor_shape = pointer_input_manager.cursor_shape(&ht_manager);
                system_link.set_cursor(&pointer_id, cursor_shape);
            }
            Event::PointerMoveRelative {
                pointer_id,
                window,
                relative,
            } => {
                pointer_input_manager.handle_mouse_move_relative(
                    pointer_id,
                    relative,
                    &ht_manager,
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
                );
            }
            Event::PointerUp {
                window,
                pointer_id,
                button,
                key_modifier,
            } => {
                pointer_input_manager.handle_mouse_up(
                    pointer_id,
                    &ht_manager,
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
                    button,
                    key_modifier,
                    window.ht_root(),
                );
            }
            Event::PointerLeaveWindow { window, pointer_id } => {
                pointer_input_manager.handle_mouse_leave(
                    pointer_id,
                    &ht_manager,
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
                );
            }
            Event::PointerHover => {
                system_link.kill_pointer_hovering_timeout();
                pointer_input_manager.handle_pointer_hover(&mut InputEventContext {
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
                });
            }
            Event::ScrollWheel {
                amount,
                key_modifier,
            } => {
                pointer_input_manager.handle_scroll_wheel(
                    amount,
                    key_modifier,
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
                );
            }
            Event::KeyDown {
                mut window,
                code,
                modifier,
            } if code == KeyInputCode::Character('\t') || code == KeyInputCode::Tab => {
                if let Some(next_focus) = if modifier.contains(ModifierKey::SHIFT) {
                    window
                        .keyboard_focus_state()
                        .prev_focus(&keyboard_focus_registry)
                } else {
                    window
                        .keyboard_focus_state()
                        .next_focus(&keyboard_focus_registry)
                } {
                    window.keyboard_focus_state_mut().update_focus_with_event(
                        next_focus,
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
                        &keyboard_focus_registry,
                    );
                }
            }
            Event::KeyDown {
                window,
                code,
                modifier,
            } => {
                window.keyboard_focus_state().handle_keydown(
                    code,
                    modifier,
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
                    &keyboard_focus_registry,
                );
            }
            Event::KeyUp {
                window,
                code,
                modifier,
            } => {
                window.keyboard_focus_state().handle_keyup(
                    code,
                    modifier,
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
                    &keyboard_focus_registry,
                );
            }
            Event::KeyChar {
                window,
                ch,
                modifier,
            } => {
                window.keyboard_focus_state().handle_char(
                    ch,
                    modifier,
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
                    &keyboard_focus_registry,
                );
            }
            Event::IMEStateChanges {
                window,
                committed_string,
                preedit_string,
            } => {
                window.keyboard_focus_state().handle_ime_state_changes(
                    committed_string.as_deref(),
                    preedit_string.as_deref(),
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
                    &keyboard_focus_registry,
                );
            }
            Event::OpenAlertDialog {
                target_window,
                message,
            } => {
                let opened_id = popup_manager.open(
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    target_window,
                    |id, ctx| uikit::AlertDialogPresenter::new(ctx, id, message, target_window),
                );
                popup_manager.post_open_action(
                    opened_id,
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
                    &keyboard_focus_registry,
                );
            }
            Event::PopupClose { id } => {
                popup_manager.close(
                    id,
                    &mut RenderContext {
                        composite_tree: &mut composite_tree,
                        ht_manager: &mut ht_manager,
                        keyboard_focus_registry: &mut keyboard_focus_registry,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                    },
                    &mut view_instance_store,
                    &view_tree_relation_store,
                    &mut view_layout_state_store,
                    &mut view_render_state_store,
                );
            }
            Event::Sync(SyncEvent::PopupUnmount { id }) => {
                popup_manager.teardown(
                    id,
                    &mut view_instance_store,
                    &mut view_tree_relation_store,
                    &mut view_render_state_store,
                    &mut TeardownContext {
                        composite_tree: &mut composite_tree,
                        ht_manager: &mut ht_manager,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        keyboard_focus_registry: &mut keyboard_focus_registry,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                    },
                );
            }
            Event::OpenCustomViewFlyout {
                parent,
                surface_pos,
                view_constructor,
            } => {
                custom_view_flyout_session = Some(CustomViewFlyoutSession::begin(
                    parent,
                    surface_pos,
                    view_constructor.0.0,
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    &mut delayed_render_messages,
                ));
            }
            Event::MenuOpen {
                parent,
                items,
                surface_pos,
            } => {
                current_active_menu_session = Some(MenuSession::new(
                    parent,
                    items,
                    surface_pos,
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    &mut delayed_render_messages,
                    &context_menu_common_resources,
                ));
            }
            Event::MenuReopen {
                parent,
                items,
                surface_pos,
            } => {
                if let Some(c) = current_active_menu_session.take() {
                    c.terminate(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }

                current_active_menu_session = Some(MenuSession::new(
                    parent,
                    items,
                    surface_pos,
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    &mut delayed_render_messages,
                    &context_menu_common_resources,
                ));
            }
            Event::DropdownMenuOpen {
                parent,
                surface_pos,
                min_width,
                items,
                selection_receiver,
            } => {
                current_active_dropdown_menu_session = Some(DropdownMenuSession::new(
                    selection_receiver,
                    parent,
                    &system_link,
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_allocator: &mut view_allocator,
                        view_instance_store: &mut view_instance_store,
                        view_tree_relation_store: &mut view_tree_relation_store,
                        view_group_relation_store: &mut view_group_relation_store,
                        view_layout_state_store: &mut view_layout_state_store,
                        view_render_state_store: &mut view_render_state_store,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    &mut delayed_render_messages,
                    surface_pos,
                    min_width,
                    items,
                ));
            }
            Event::MenuCloseAll => {
                if let Some(c) = current_active_menu_session.take() {
                    if let Some(ref a) =
                        unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu
                    {
                        crate::uikit::view_instance::<ui::app_menu_bar::View>(
                            a.into_untyped(),
                            &view_instance_store,
                        )
                        .expect("query failed")
                        .on_close_all(
                            &mut composite_tree,
                            global_time_base.elapsed().as_secs_f32(),
                        );
                    }

                    c.terminate(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }
            }
            Event::MenuRescale { scale } => {
                if let Some(ref c) = custom_view_flyout_session {
                    c.rescale(scale, &mut composite_tree, &ht_manager, &system_link);
                }
            }
            Event::MenuSelectItem { depth, index } => {
                if let Some(c) = current_active_menu_session.as_mut() {
                    c.select_item(
                        depth,
                        index,
                        &mut composite_tree,
                        global_time_base.elapsed().as_secs_f32(),
                    );

                    system_link.flyout_surface_context.reserve_delayed_action();
                }
            }
            Event::MenuDeselectItem { depth } => {
                if let Some(c) = current_active_menu_session.as_mut() {
                    c.deselect_item(
                        depth,
                        &mut composite_tree,
                        global_time_base.elapsed().as_secs_f32(),
                    );

                    system_link.flyout_surface_context.reserve_delayed_action();
                }
            }
            Event::MenuOpenSubmenu { depth, index } => {
                /* if let Some(c) = current_active_context_menu_session.as_mut() {
                    c.open_submenu(
                        depth,
                        index,
                        &system_link,
                        &mut ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                            },
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                            ui_scale_factor: 1.0, // updated later
                            system_link: &system_link,
                        },
                        &context_menu_common_resources,
                        &typing_context,
                    );

                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }*/
            }
            Event::MenuPerformDelayedAction => {
                system_link
                    .flyout_surface_context
                    .unreserve_delayed_action();

                if let Some(c) = current_active_menu_session.as_mut() {
                    c.perform_delayed_action(
                        &system_link,
                        &mut ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_allocator: &mut view_allocator,
                            view_instance_store: &mut view_instance_store,
                            view_tree_relation_store: &mut view_tree_relation_store,
                            view_group_relation_store: &mut view_group_relation_store,
                            view_layout_state_store: &mut view_layout_state_store,
                            view_render_state_store: &mut view_render_state_store,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                        &mut delayed_render_messages,
                        &context_menu_common_resources,
                    );
                }
            }
            Event::MenuPointerDown {
                pointer_id,
                target,
                button,
                key_modifier,
            } => {
                pointer_input_manager.handle_mouse_down(
                    pointer_id,
                    &ht_manager,
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
                    button,
                    key_modifier,
                    target.ht_root(),
                    &mut keyboard_focus_registry,
                );
            }
            Event::MenuPointerMove {
                pointer_id,
                target,
                client_pos,
                key_modifier,
            } => {
                pointer_input_manager.handle_mouse_move(
                    NativeDesktopSurface::ContextMenu(target),
                    pointer_id,
                    client_pos,
                    key_modifier,
                    &ht_manager,
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
                    target.ht_root(),
                );

                let cursor_shape = pointer_input_manager.cursor_shape(&ht_manager);
                system_link.set_cursor(&pointer_id, cursor_shape);
            }
            Event::MenuPointerUp {
                pointer_id,
                target,
                button,
                key_modifier,
            } => {
                pointer_input_manager.handle_mouse_up(
                    pointer_id,
                    &ht_manager,
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
                    button,
                    key_modifier,
                    target.ht_root(),
                );
            }
            Event::MenuPointerLeave { pointer_id, .. } => {
                pointer_input_manager.handle_mouse_leave(
                    pointer_id,
                    &ht_manager,
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
                );
            }
            Event::MenuSelectCommand { id } => {
                tracing::debug!(id, "ContextMenuSelectCommand");

                // コマンド選択したらとじる
                if let Some(c) = current_active_menu_session.take() {
                    if let Some(ref a) =
                        unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu
                    {
                        crate::uikit::view_instance::<ui::app_menu_bar::View>(
                            a.into_untyped(),
                            &view_instance_store,
                        )
                        .expect("query failed")
                        .on_close_all(
                            &mut composite_tree,
                            global_time_base.elapsed().as_secs_f32(),
                        );
                    }

                    c.terminate(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );

                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }

                match id {
                    MENU_COMMAND_ID_OBJECT_CREATE_PLANE => {
                        crate::model::object_create_of_shape(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Plane".into(),
                            ObjectRenderShape::Plane,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CUBE => {
                        crate::model::object_create_of_shape(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Cube".into(),
                            ObjectRenderShape::Cube,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_SPHERE => {
                        crate::model::object_create_of_shape(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Sphere".into(),
                            ObjectRenderShape::Sphere,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER => {
                        crate::model::object_create_of_shape(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Cylinder".into(),
                            ObjectRenderShape::Cylinder,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE => {
                        crate::model::object_create_of_shape(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Capsule".into(),
                            ObjectRenderShape::Capsule,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN => {
                        crate::model::object_create_of_shape(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Terrain".into(),
                            // TODO: terrain support
                            ObjectRenderShape::Plane,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CHILD_PLANE => {
                        crate::model::object_create_of_shape_children_of_selected(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Plane".into(),
                            ObjectRenderShape::Plane,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CHILD_CUBE => {
                        crate::model::object_create_of_shape_children_of_selected(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Cube".into(),
                            ObjectRenderShape::Cube,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CHILD_SPHERE => {
                        crate::model::object_create_of_shape_children_of_selected(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Sphere".into(),
                            ObjectRenderShape::Sphere,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CHILD_CYLINDER => {
                        crate::model::object_create_of_shape_children_of_selected(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Cylinder".into(),
                            ObjectRenderShape::Cylinder,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CHILD_CAPSULE => {
                        crate::model::object_create_of_shape_children_of_selected(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Capsule".into(),
                            ObjectRenderShape::Capsule,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CHILD_SP_TERRAIN => {
                        crate::model::object_create_of_shape_children_of_selected(
                            &mut ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                            "New Terrain".into(),
                            // TODO: terrain support
                            ObjectRenderShape::Plane,
                        );
                    }
                    MENU_COMMAND_ID_OBJECT_DESTROY_SELECTED => {
                        crate::model::object_destroy_selected(&mut ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        });
                    }
                    MENU_COMMAND_ID_OBJECT_DUPLICATE_SELECTED => {
                        crate::model::object_duplicate_selected(&mut ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        });
                    }
                    _ => {
                        tracing::warn!(id, "unhandled menu command");
                    }
                }
            }
            Event::DropdownMenuSelectItem { id, receiver } => {
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
                            uikit::view_instance_mut(id, self.view_instance_store)
                        }

                        #[inline(always)]
                        fn view_set_visibility_untyped(
                            &mut self,
                            id: ViewIdentifier,
                            visible: bool,
                        ) {
                            uikit::view_set_visibility(id, visible, self.view_instance_store)
                        }

                        #[inline(always)]
                        fn view_layout_mut_untyped(
                            &mut self,
                            id: ViewIdentifier,
                        ) -> Option<&mut uikit::ViewLayout> {
                            uikit::view_layout_mut(id, self.view_instance_store)
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
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                        &mut LocalContext {
                            view_instance_store: &mut view_instance_store,
                            view_render_queue: &mut view_render_queue,
                        },
                    );
                }

                // 選択したら閉じる
                if let Some(mut c) = current_active_dropdown_menu_session.take() {
                    c.close_all(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }
            }
            Event::DockMoveSplitter {
                controlling_dock,
                pos_client,
            } => {
                ui::dock::move_splitter(
                    controlling_dock,
                    &mut dock_store,
                    pos_client,
                    &mut PaneContentResizeContext {
                        view_instance_store: &mut view_instance_store,
                        view_render_queue: &mut view_render_queue,
                        composite_tree: &mut composite_tree,
                        ht_manager: &mut ht_manager,
                    },
                );
            }
            Event::DockBeginPreview {
                initiator,
                pointer,
                source_dock,
                tab_index,
                pane_rect,
                tab_size,
                client_pos,
            } => {
                let (state, popover_rect) = ui::dock::begin_preview(
                    pane_rect,
                    tab_size,
                    &client_pos,
                    initiator,
                    source_dock,
                    tab_index,
                );

                system_link.begin_pane_drag(initiator, &pointer, state.offset, &popover_rect);
                docking_preview_state = Some(state);
            }
            Event::DockMovePreview {
                dest_window,
                client_pos_in_dest,
            } => {
                if let Some(ref mut state) = docking_preview_state {
                    let popover_rect = ui::dock::move_preview(
                        &unsafe { dest_window.extra_data_ref::<PerWindowData>() }.docking_manager,
                        &dock_store,
                        &client_pos_in_dest,
                        state,
                    );
                    system_link.update_pane_drag(dest_window, &popover_rect);
                }
            }
            Event::DockConfirm {
                pointer,
                mut destination_window,
                client_pos_in_dest,
            } => {
                if let Some(state) = docking_preview_state.take() {
                    let dm = &mut unsafe { destination_window.extra_data_mut::<PerWindowData>() }
                        .docking_manager;

                    tracing::debug!(?client_pos_in_dest, "dock confirm");

                    let mut view_feedback_registry_delayed_ops = VecDeque::new();
                    let mut source_window = state.source_window;
                    let source_dock = state.source_dock;
                    let tab_index = state.tab_index;
                    system_link.end_pane_drag();
                    let (op, suggested_rect) =
                        ui::dock::end_preview(dm, &mut dock_store, &client_pos_in_dest, state);
                    let (diverged_content, undock_result) = dm.redock(
                        source_dock,
                        &mut dock_store,
                        tab_index,
                        op,
                        &suggested_rect,
                        &mut ui::dock::RedockingContext {
                            view_init_ctx: ViewInitContext {
                                mount_context: MountContext {
                                    composite_tree: &mut composite_tree,
                                    ht_manager: &mut ht_manager,
                                    current_sec: global_time_base.elapsed().as_secs_f32(),
                                    keyboard_focus_registry: &mut keyboard_focus_registry,
                                },
                                view_allocator: &mut view_allocator,
                                view_instance_store: &mut view_instance_store,
                                view_tree_relation_store: &mut view_tree_relation_store,
                                view_group_relation_store: &mut view_group_relation_store,
                                view_layout_state_store: &mut view_layout_state_store,
                                view_render_state_store: &mut view_render_state_store,
                                view_feedback_subscription_delayed_ops:
                                    &mut view_feedback_registry_delayed_ops,
                                system_link: &system_link,
                                main_thread_texture_id_issuer: &mut texture_id_issuer,
                                application: &application,
                            },
                            view_render_queue: &mut view_render_queue,
                        },
                    );

                    match undock_result {
                        ui::dock::UndockResult::Success => {}
                        ui::dock::UndockResult::ToBeEmpty => {
                            unsafe {
                                drop(source_window.take_extra_data::<PerWindowData>());
                            }
                            sub_windows.remove(&source_window);
                            system_link.close_window(
                                source_window,
                                &mut composite_tree,
                                &mut ht_manager,
                                &mut keyboard_focus_registry,
                            );
                        }
                    }

                    if let Some(content) = diverged_content {
                        let new_window = system_link.open_window(
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
                            &mut composite_tree,
                            &mut ht_manager,
                            &mut keyboard_focus_registry,
                            &mut delayed_render_messages,
                            |mut w,
                             composite_tree,
                             ht_manager,
                             keyboard_focus_registry,
                             system_link| {
                                ht_manager.get_data_mut(w.ht_root()).root_of_window = Some(w);

                                composite_tree
                                    .begin_mod_chain(w.ct_root())
                                    .has_bitmap(true)
                                    .composite_mode(CompositeMode::FillCornerGradient(
                                        window_bg_gradient,
                                        AnimatableColor::Value([0.0, 0.025, 0.05, 1.0]),
                                    ))
                                    .apply();

                                let mut view_init_ctx = ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree,
                                        ht_manager,
                                        current_sec: global_time_base.elapsed().as_secs_f32(),
                                        keyboard_focus_registry,
                                    },
                                    view_allocator: &mut view_allocator,
                                    view_instance_store: &mut view_instance_store,
                                    view_tree_relation_store: &mut view_tree_relation_store,
                                    view_group_relation_store: &mut view_group_relation_store,
                                    view_layout_state_store: &mut view_layout_state_store,
                                    view_render_state_store: &mut view_render_state_store,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                };
                                let root_view =
                                    view_init_ctx.construct_view(|_| Box::new(WindowRootView {}));
                                let window_header_view = ui::window_header::Component::new(
                                    ui::window_header::Caption::Sub,
                                    ui::window_header::ComponentInit {
                                        with_system_command_buttons: w
                                            .needs_system_command_buttons(),
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
                                    Rect::from_lt_size(
                                        Point::new_logical(0.0, 0.0),
                                        w.client_size(),
                                    ),
                                );

                                w.associate_extra_data(Box::new(PerWindowData {
                                    root_view,
                                    screen_reposition_interests: HashSet::new(),
                                    header: window_header_view,
                                    appmenu: None,
                                    footer: None,
                                    docking_manager: ui::dock::DockingManager::new(
                                        w,
                                        &mut view_init_ctx,
                                        &mut view_render_queue,
                                        Rect::from_lt_size(
                                            Point::new_logical(
                                                0.0,
                                                ui::window_header::View::THICKNESS,
                                            ),
                                            suggested_rect.size(),
                                        ),
                                        &mut dock_store,
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
                        sub_windows.insert(new_window);
                    }
                }
            }
            Event::Sync(SyncEvent::NewPresentID { .. }) => {
                // vsync update period
                preview_state.update(
                    &mut *profiler::wrap!(
                        LOCK_WAIT,
                        committed_preview_state.lock().expect("poisoned")
                    ),
                    &mut preview_input_state,
                    &mut ApplicationMutation {
                        state: &mut application,
                        view_feedbacks: &mut view_feedback_store,
                    },
                );
            }
            Event::ScheduleViewRenderExt { id } => {
                view_render_queue.schedule(id);
            }
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

        // after-input common update phase
        if !view_feedback_store.is_empty() {
            let mut fb_context = ViewFeedbackContext {
                application: &application,
                composite_tree: &mut composite_tree,
                ht_manager: &mut ht_manager,
                current_sec: global_time_base.elapsed().as_secs_f32(),
                keyboard_focus_registry: &mut keyboard_focus_registry,
                view_allocator: &mut view_allocator,
                view_instance_store: &mut view_instance_store,
                view_tree_relation_store: &mut view_tree_relation_store,
                view_group_relation_store: &mut view_group_relation_store,
                view_layout_state_store: &mut view_layout_state_store,
                view_render_state_store: &mut view_render_state_store,
                view_feedback_subscription_delayed_ops: &mut view_feedback_registry_delayed_ops,
                system_link: &system_link,
                main_thread_texture_id_issuer: &mut texture_id_issuer,
                view_render_queue: &mut view_render_queue,
            };

            for x in view_feedback_store.drain(..) {
                x.dispatch(&view_feedback_registry, &mut fb_context);
            }

            view_feedback_registry.perform_atomic(&mut fb_context);
        }

        view_render_queue.perform(
            &mut RenderContext {
                composite_tree: &mut composite_tree,
                ht_manager: &mut ht_manager,
                keyboard_focus_registry: &mut keyboard_focus_registry,
                current_sec: global_time_base.elapsed().as_secs_f32(),
                system_link: &system_link,
                main_thread_texture_id_issuer: &mut texture_id_issuer,
                application: &application,
                view_feedback_subscription_delayed_ops: &mut view_feedback_registry_delayed_ops,
            },
            &mut view_instance_store,
            &view_tree_relation_store,
            &mut view_layout_state_store,
            &mut view_render_state_store,
        );

        composite_tree.commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
        view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);

        for msg in delayed_render_messages.drain(..) {
            system_link.rt_sender().send(msg).expect("rt_sender.send");
        }
    }

    tracing::info!("saving window state");
    let window_state_persist = PersistStateWindowData {
        main: WindowState {
            geometry: main_window.geometry_state_snapshot(&system_link),
            dock: unsafe { main_window.extra_data_ref::<PerWindowData>() }
                .docking_manager
                .state_snapshot(&dock_store),
        },
        sub: sub_windows
            .iter()
            .map(|w| WindowState {
                geometry: w.geometry_state_snapshot(&system_link),
                dock: unsafe { w.extra_data_ref::<PerWindowData>() }
                    .docking_manager
                    .state_snapshot(&dock_store),
            })
            .collect(),
    };
    'try_save_window_state: {
        let fp = match std::fs::File::create(file_system.window_state_save_path()) {
            Ok(fp) => fp,
            Err(e) => {
                tracing::warn!(reason = %e, "persist.create.window_state");
                break 'try_save_window_state;
            }
        };
        if let Err(e) = window_state_persist.serialize(&mut std::io::BufWriter::new(fp)) {
            tracing::warn!(reason = %e, "persist.save.window_state");
        }
    }

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

pub struct FlyoutSurfaceSessionTerminateContext<'a, 'h> {
    pub syslink: &'a SystemLink<'a>,
    pub view_allocator: &'a mut ViewIdentifierAllocator,
    pub view_instance_store: &'a mut ViewInstanceStore,
    pub view_tree_relation_store: &'a mut ViewTreeRelationStore,
    pub view_group_relation_store: &'a mut ViewGroupRelationStore,
    pub view_layout_state_store: &'a mut ViewLayoutStateStore,
    pub view_render_state_store: &'a mut ViewRenderStateStore,
    pub teardown_context: TeardownContext<'a, 'h>,
}
impl ViewDestructionContext for FlyoutSurfaceSessionTerminateContext<'_, '_> {
    #[inline(always)]
    fn destruct_view_recursive_untyped(&mut self, target: ViewIdentifier) {
        crate::uikit::destruct_view_recursive(
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
        view_init_context: &mut ViewInitContext,
        delayed_render_messages: &mut Vec<RenderMessage>,
    ) -> Self {
        let surface = view_init_context.system_link.new_flyout_surface(
            parent,
            pos,
            content_ctor.size(),
            view_init_context.mount_context.composite_tree,
            view_init_context.mount_context.ht_manager,
            view_init_context.mount_context.keyboard_focus_registry,
            delayed_render_messages,
        );
        let content = content_ctor.create(view_init_context);
        view_init_context.render_view_with_base(
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
        syslink: &SystemLink,
        view_init_context: &mut ViewInitContext,
        delayed_render_messages: &mut Vec<RenderMessage>,
        pos: Point<LogicalUnit>,
        min_width: f32,
        items: Vec<uikit::dropdown_box::MenuItem>,
    ) -> Self {
        let menu_layout = uikit::dropdown_box::MenuLayout::new(items, syslink.font_set());
        let root_surface = syslink.new_flyout_surface(
            parent,
            pos,
            Size::new_logical(
                menu_layout.required_width().max(min_width),
                menu_layout.height(),
            ),
            view_init_context.mount_context.composite_tree,
            view_init_context.mount_context.ht_manager,
            view_init_context.mount_context.keyboard_focus_registry,
            delayed_render_messages,
        );

        let item_views = menu_layout
            .instantiate_all(view_init_context, selection_receiver, |v, ctx| {
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

pub struct MenuSurface {
    handle: FlyoutSurfaceHandle,
    item_views: Vec<Option<MenuItemInteractableElement>>,
    _event_handler: Rc<MenuEventHandler>,
    parent_path: Vec<usize>,
    current_selecting: Option<usize>,
}
impl MenuSurface {
    fn new(
        ctx: &mut ViewInitContext,
        delayed_render_messages: &mut Vec<RenderMessage>,
        common_res: &crate::uikit::MenuItemCommonResources,
        initiator_window: WindowHandle,
        display_pos: Point<LogicalUnit>,
        depth: usize,
        parent_path: Vec<usize>,
        items: impl Iterator<Item = MenuItem>,
    ) -> Self {
        let layouted_items = crate::uikit::MenuItemLayout::build(items, ctx.system_link.font_set());
        let width = crate::uikit::MenuItemLayout::min_width(layouted_items.iter());
        let height = crate::uikit::MenuItemLayout::height(layouted_items.iter());

        let surface = ctx.system_link.new_flyout_surface(
            initiator_window,
            display_pos,
            Size::new_logical(width.value(), height.value()),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
            ctx.mount_context.keyboard_focus_registry,
            delayed_render_messages,
        );

        let (item_views, eh) = crate::uikit::MenuItemLayout::instantiate(
            layouted_items.into_iter(),
            depth,
            ctx,
            common_res,
            &surface,
        );
        ctx.ht_manager.set_action_handler(surface.ht_root(), &eh);

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
    opening_surfaces: Vec<MenuSurface>,
    active_selection: Option<(usize, usize)>,
}
impl MenuSession {
    pub fn new(
        parent: WindowHandle,
        items: Vec<MenuItem>,
        surface_pos: Point<LogicalUnit>,
        view_init_context: &mut ViewInitContext,
        delayed_render_messages: &mut Vec<RenderMessage>,
        common_res: &MenuItemCommonResources,
    ) -> Self {
        #[cfg(target_os = "macos")]
        view_init_context
            .system_link
            .flyout_surface_context
            .observe_global_click();

        Self {
            opening_surfaces: vec![MenuSurface::new(
                view_init_context,
                delayed_render_messages,
                common_res,
                parent,
                surface_pos,
                0,
                Vec::new(),
                items.iter().cloned(),
            )],
            parent,
            items,
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

    pub fn perform_delayed_action(
        &mut self,
        system_link: &SystemLink,
        view_init_context: &mut ViewInitContext,
        delayed_render_messages: &mut Vec<RenderMessage>,
        common_res: &MenuItemCommonResources,
    ) {
        match self.active_selection {
            Some((depth, index)) => {
                self.close_deeper(
                    depth,
                    system_link,
                    view_init_context.mount_context.composite_tree,
                    view_init_context.mount_context.ht_manager,
                    view_init_context.mount_context.keyboard_focus_registry,
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
                        view_init_context,
                        delayed_render_messages,
                        common_res,
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
                self.close_deeper(
                    0,
                    system_link,
                    view_init_context.mount_context.composite_tree,
                    view_init_context.mount_context.ht_manager,
                    view_init_context.mount_context.keyboard_focus_registry,
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
    ) {
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
    #[cfg(all(unix, not(target_os = "macos")))]
    display_server: platform::unix::DisplayServerLink,
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

    #[cfg(feature = "wayland")]
    #[inline(always)]
    pub fn new_flyout_surface<E>(
        &self,
        parent: WindowHandle,
        pos: Point<LogicalUnit>,
        size: Size<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
        delayed_render_messages: &mut Vec<RenderMessage>,
    ) -> FlyoutSurfaceHandle {
        platform::unix::wayland::flyout_surface::new_surface(
            parent,
            pos,
            size,
            self,
            composite_tree,
            ht_manager,
            keyboard_focus_registry,
            delayed_render_messages,
            parent.ui_scale_factor(),
        )
    }
}

#[cfg(target_os = "macos")]
pub use platform::mac::{
    DragPreviewPopoverHandle, PointerID, WindowHandle, WindowPersistentStateNativeGeometryUnit,
    flyout_surface::Handle as FlyoutSurfaceHandle,
};
#[cfg(feature = "wayland")]
pub use platform::unix::wayland::{
    FlyoutSurfaceHandle, PointerID, ToplevelHandle as WindowHandle,
    WindowPersistentStateNativeGeometryUnit,
};
#[cfg(windows)]
pub use platform::windows::{
    PointerID, SystemLink, WindowHandle, WindowPersistentStateNativeGeometryUnit,
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

#[derive(thiserror::Error, Debug)]
pub enum PersistStateDeserializeError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("persist_state_deserialize_error.invalid_format")]
    InvalidFormat,
}

#[derive(Debug)]
pub enum DockDirection {
    Left(f32),
    Right(f32),
    Top(f32),
    Bottom(f32),
}
impl DockDirection {
    pub fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        match self {
            &Self::Left(x) => {
                w.write_all(&[0x01])?;
                w.write_all(&f32::to_ne_bytes(x))?;
            }
            &Self::Right(x) => {
                w.write_all(&[0x02])?;
                w.write_all(&f32::to_ne_bytes(x))?;
            }
            &Self::Top(x) => {
                w.write_all(&[0x03])?;
                w.write_all(&f32::to_ne_bytes(x))?;
            }
            &Self::Bottom(x) => {
                w.write_all(&[0x04])?;
                w.write_all(&f32::to_ne_bytes(x))?;
            }
        }

        Ok(())
    }

    pub fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError> {
        let mut buf = [0u8; 1];
        r.read_exact(&mut buf)?;
        match buf[0] {
            0x01 => {
                let mut buf = [0u8; size_of::<f32>()];
                r.read_exact(&mut buf)?;
                Ok(Self::Left(f32::from_ne_bytes(buf)))
            }
            0x02 => {
                let mut buf = [0u8; size_of::<f32>()];
                r.read_exact(&mut buf)?;
                Ok(Self::Right(f32::from_ne_bytes(buf)))
            }
            0x03 => {
                let mut buf = [0u8; size_of::<f32>()];
                r.read_exact(&mut buf)?;
                Ok(Self::Top(f32::from_ne_bytes(buf)))
            }
            0x04 => {
                let mut buf = [0u8; size_of::<f32>()];
                r.read_exact(&mut buf)?;
                Ok(Self::Bottom(f32::from_ne_bytes(buf)))
            }
            _ => Err(PersistStateDeserializeError::InvalidFormat),
        }
    }
}
#[derive(Debug)]
pub enum DockState {
    Filled {
        content_ids: Vec<String>,
        active_index: usize,
    },
    Splitted {
        direction: DockDirection,
        content: Box<DockState>,
        rest: Box<DockState>,
    },
}
impl DockState {
    pub fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::Filled {
                content_ids,
                active_index,
            } => {
                w.write_all(&[0x01])?;
                w.write_all(&usize::to_ne_bytes(content_ids.len()))?;
                for id in content_ids {
                    w.write_all(&usize::to_ne_bytes(id.len()))?;
                    w.write_all(id.as_bytes())?;
                }
                w.write_all(&usize::to_ne_bytes(*active_index))?;
            }
            Self::Splitted {
                direction,
                content,
                rest,
            } => {
                w.write_all(&[0x02])?;
                direction.serialize(w)?;
                content.serialize(w)?;
                rest.serialize(w)?;
            }
        }

        Ok(())
    }

    pub fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError> {
        let mut buf = [0u8; 1];
        r.read_exact(&mut buf)?;
        match buf[0] {
            0x01 => {
                let mut content_count = 0usize;
                r.read_exact(unsafe {
                    core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut content_count)
                })?;
                let mut content_ids = Vec::with_capacity(content_count);
                for _ in 0..content_count {
                    let mut id_length = 0usize;
                    r.read_exact(unsafe {
                        core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut id_length)
                    })?;
                    let mut id = Vec::with_capacity(id_length);
                    r.read_exact(unsafe { core::mem::transmute(id.spare_capacity_mut()) })?;
                    unsafe {
                        id.set_len(id_length);
                    }
                    content_ids.push(unsafe { String::from_utf8_unchecked(id) });
                }
                let mut active_index = 0usize;
                r.read_exact(unsafe {
                    core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut active_index)
                })?;

                Ok(Self::Filled {
                    content_ids,
                    active_index,
                })
            }
            0x02 => {
                let direction = DockDirection::deserialize(r)?;
                let content = Self::deserialize(r)?;
                let rest = Self::deserialize(r)?;

                Ok(Self::Splitted {
                    direction,
                    content: Box::new(content),
                    rest: Box::new(rest),
                })
            }
            _ => Err(PersistStateDeserializeError::InvalidFormat),
        }
    }

    pub fn construct(
        &self,
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
                        &DockDirection::Left(w) => ui::dock::DockDirection::ToLeft(Cell::new(w)),
                        &DockDirection::Right(w) => ui::dock::DockDirection::ToRight(Cell::new(w)),
                        &DockDirection::Top(w) => ui::dock::DockDirection::ToTop(Cell::new(w)),
                        &DockDirection::Bottom(w) => {
                            ui::dock::DockDirection::ToBottom(Cell::new(w))
                        }
                    },
                    splitter: create_context.construct_view(|_| {
                        Box::new(ui::dock::DockedPaneSplitterView::new(
                            match direction {
                                DockDirection::Left(_) | DockDirection::Right(_) => {
                                    ui::dock::DockedPaneSplitDirection::Horizontal
                                }
                                DockDirection::Top(_) | DockDirection::Bottom(_) => {
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
                self,
                root_keyboard_focus_group,
                create_context,
                store,
                parent,
                &mut pane_constructor,
            )
        })
    }
}

trait PersistStateSerializable: Sized {
    fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()>;
    fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError>;
}
impl PersistStateSerializable for Rect<crate::utils::LogicalUnit> {
    fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&f32::to_ne_bytes(self.left))?;
        w.write_all(&f32::to_ne_bytes(self.top))?;
        w.write_all(&f32::to_ne_bytes(self.width))?;
        w.write_all(&f32::to_ne_bytes(self.height))?;
        Ok(())
    }

    fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError> {
        let mut x = 0f32;
        let mut y = 0f32;
        let mut width = 0f32;
        let mut height = 0f32;
        r.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; size_of::<f32>()]>(&mut x) })?;
        r.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; size_of::<f32>()]>(&mut y) })?;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<f32>()]>(&mut width)
        })?;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<f32>()]>(&mut height)
        })?;

        Ok(Self::from_lt_size(
            Point::new_logical(x, y),
            Size::new_logical(width, height),
        ))
    }
}
impl PersistStateSerializable for Rect<crate::utils::PixelsUnit> {
    fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&i32::to_ne_bytes(self.left))?;
        w.write_all(&i32::to_ne_bytes(self.top))?;
        w.write_all(&u32::to_ne_bytes(self.width))?;
        w.write_all(&u32::to_ne_bytes(self.height))?;
        Ok(())
    }

    fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError> {
        let mut left = 0i32;
        let mut top = 0i32;
        let mut width = 0u32;
        let mut height = 0u32;
        r.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; size_of::<i32>()]>(&mut left) })?;
        r.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; size_of::<i32>()]>(&mut top) })?;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<u32>()]>(&mut width)
        })?;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<u32>()]>(&mut height)
        })?;

        Ok(Self::from_lt_size(
            Point::new_pixels(left, top),
            Size::new_pixels(width, height),
        ))
    }
}

#[derive(Debug, Clone)]
pub enum WindowGeometryState {
    Maximized {
        monitor_index: usize,
    },
    Restored {
        rect: Rect<WindowPersistentStateNativeGeometryUnit>,
    },
}
impl WindowGeometryState {
    fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::Maximized { monitor_index } => {
                w.write_all(&[0x01])?;
                w.write_all(&usize::to_ne_bytes(*monitor_index))?;
            }
            Self::Restored { rect } => {
                w.write_all(&[0x02])?;
                rect.serialize(w)?;
            }
        }

        Ok(())
    }

    fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError> {
        let mut buf = [0u8; 1];
        r.read_exact(&mut buf)?;
        match buf[0] {
            0x01 => {
                let mut monitor_index = 0usize;
                r.read_exact(unsafe {
                    core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut monitor_index)
                })?;
                Ok(Self::Maximized { monitor_index })
            }
            0x02 => {
                let rect = PersistStateSerializable::deserialize(r)?;

                Ok(Self::Restored { rect })
            }
            _ => Err(PersistStateDeserializeError::InvalidFormat),
        }
    }
}
#[derive(Debug)]
pub struct WindowState {
    geometry: WindowGeometryState,
    dock: DockState,
}
impl WindowState {
    fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        self.geometry.serialize(w)?;
        self.dock.serialize(w)?;

        Ok(())
    }

    fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError> {
        let geometry = WindowGeometryState::deserialize(r)?;
        let dock = DockState::deserialize(r)?;

        Ok(Self { geometry, dock })
    }
}
#[derive(Debug)]
pub struct PersistStateWindowData {
    main: WindowState,
    sub: Vec<WindowState>,
}
impl PersistStateWindowData {
    pub fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()> {
        self.main.serialize(w)?;
        w.write_all(&usize::to_ne_bytes(self.sub.len()))?;
        for sub in &self.sub {
            sub.serialize(w)?;
        }

        Ok(())
    }

    pub fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError> {
        let main = WindowState::deserialize(r)?;
        let mut sub_len = 0usize;
        r.read_exact(unsafe {
            core::mem::transmute::<_, &mut [u8; size_of::<usize>()]>(&mut sub_len)
        })?;
        let mut sub = Vec::with_capacity(sub_len);
        for _ in 0..sub_len {
            sub.push(WindowState::deserialize(r)?);
        }
        Ok(Self { main, sub })
    }
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

        #[cfg(target_os = "linux")]
        let cache_base_path = 'cache_base_path: {
            if let Some(p) = std::env::var_os("XDG_CACHE_HOME") {
                break 'cache_base_path PathBuf::from(p).join("io.ct2.peridot.editor");
            }

            if let Some(p) = std::env::var_os("HOME") {
                break 'cache_base_path PathBuf::from(p).join(".cache/io.ct2.peridot.editor");
            }

            tracing::warn!(
                "neither XDG_CACHE_HOME nor HOME is set, generating cache into current working directory"
            );
            std::env::current_dir()
                .expect("fs.cache_base_path.current_dir")
                .join(".cache/io.ct2.peridot.editor")
        };
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

        #[cfg(target_os = "linux")]
        let persist_state_base_path = 'persist_state_base_path: {
            if let Some(p) = std::env::var_os("XDG_DATA_HOME") {
                break 'persist_state_base_path PathBuf::from(p).join("io.ct2.peridot.editor");
            }

            if let Some(p) = std::env::var_os("HOME") {
                break 'persist_state_base_path PathBuf::from(p)
                    .join(".local/share/io.ct2.peridot.editor");
            }

            tracing::warn!(
                "neither XDG_DATA_HOME nor HOME is set, generating persisted-state data into current working directory"
            );
            std::env::current_dir()
                .expect("fs.persist_state_base_path.current_dir")
                .join(".persist-state/io.ct2.peridot.editor")
        };

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
                                crate::model::select_object(application, oid);
                            }
                            None => {
                                crate::model::object_deselect_all(application);
                            }
                        }
                    }

                    if input.grabbing {
                        // grab start on this frame

                        if let Some(&selected) = application.selected_objects.iter().next()
                            && let Some(pointer_pos) = input.pointer_pos
                        {
                            let current_handle_shape =
                                match crate::model::preview_edit_tool_type(application) {
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
                            crate::model::set_selected_object_local_translate_x(
                                application,
                                base_object_pos.0 + move_delta.0,
                            );
                        }
                        HandlePointing::Y => {
                            crate::model::set_selected_object_local_translate_y(
                                application,
                                base_object_pos.1 + move_delta.1,
                            );
                        }
                        HandlePointing::Z => {
                            crate::model::set_selected_object_local_translate_z(
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
                            crate::model::set_selected_object_local_rotation_x(
                                application,
                                base_object_rot.0 - move_delta.1,
                            );
                        }
                        HandlePointing::Y => {
                            crate::model::set_selected_object_local_rotation_y(
                                application,
                                base_object_rot.1 + move_delta.0,
                            );
                        }
                        HandlePointing::Z => {
                            crate::model::set_selected_object_local_rotation_z(
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
                            crate::model::set_selected_object_local_scale_x(
                                application,
                                base_object_scale.0 + move_delta.0,
                            );
                        }
                        HandlePointing::Y => {
                            crate::model::set_selected_object_local_scale_y(
                                application,
                                base_object_scale.1 + move_delta.1,
                            );
                        }
                        HandlePointing::Z => {
                            crate::model::set_selected_object_local_scale_z(
                                application,
                                base_object_scale.2 + move_delta.2,
                            );
                        }
                        HandlePointing::All => {
                            let scale_all = move_delta.len();
                            crate::model::set_selected_object_local_scale(
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

            current_handle_shape = Some(match crate::model::preview_edit_tool_type(application) {
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
    ) -> uikit::ViewRenderElements {
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

        uikit::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..uikit::ViewRenderElements::EMPTY
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

    fn measure_preferred_content_size(&self, ctx: &mut uikit::MeasureContext) -> Size<LogicalUnit> {
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
        crate::model::set_preview_edit_tool_type(context, self.bound_tool_type);

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
        let root_view = ctx.construct_view(|_| Box::new(PreviewView::new(input_state)));
        let translate_control_button = ctx.construct_view(|_| {
            Box::new(PreviewToolSelectorButtonView::new(
                true,
                false,
                Point::new_logical(8.0, 8.0),
                "T".into(),
                PreviewEditToolType::Translate,
            ))
        });
        let rotate_control_button = ctx.construct_view(|_| {
            Box::new(PreviewToolSelectorButtonView::new(
                false,
                false,
                Point::new_logical(8.0, 8.0 + 24.0 - 1.0),
                "R".into(),
                PreviewEditToolType::Rotate,
            ))
        });
        let scale_control_button = ctx.construct_view(|_| {
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
        ctx.subscribe_view_feedback::<ViewFeedbackPreviewEditToolTypeChanged>(&feedback_receiver);

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
        ctx.unsubscribe_view_feedback::<ViewFeedbackPreviewEditToolTypeChanged>(
            &self.feedback_receiver,
        );
    }
}

pub struct PreviewPaneFeedbackReceiver {
    translate_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
    rotate_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
    scale_tool_button_view_id: TypedViewIdentifier<PreviewToolSelectorButtonView>,
}
impl ViewFeedbackHandler<ViewFeedbackPreviewEditToolTypeChanged> for PreviewPaneFeedbackReceiver {
    fn accept_feedback<'a, 'h>(
        &self,
        feedback: &ViewFeedbackPreviewEditToolTypeChanged,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let is_selecting =
            crate::model::preview_edit_tool_type(context) == PreviewEditToolType::Translate;
        context
            .view_instance_mut(self.translate_tool_button_view_id)
            .expect("query failed")
            .set_selecting(is_selecting);
        context.schedule_view_render(self.translate_tool_button_view_id);

        let is_selecting =
            crate::model::preview_edit_tool_type(context) == PreviewEditToolType::Rotate;
        context
            .view_instance_mut(self.rotate_tool_button_view_id)
            .expect("query failed")
            .set_selecting(is_selecting);
        context.schedule_view_render(self.rotate_tool_button_view_id);

        let is_selecting =
            crate::model::preview_edit_tool_type(context) == PreviewEditToolType::Scale;
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
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> uikit::ViewRenderElements {
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

        uikit::ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            keyboard_focus: Some(e.kf_token),
            ..uikit::ViewRenderElements::EMPTY
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

    fn measure_preferred_content_size(&self, ctx: &mut uikit::MeasureContext) -> Size<LogicalUnit> {
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
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
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
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.grabbing = true;
        EventContinueControl::GRAB_POINTER | EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_end(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
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
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
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
