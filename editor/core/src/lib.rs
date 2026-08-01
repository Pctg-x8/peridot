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
    num::NonZeroUsize,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Mutex,
};
#[cfg(target_os = "macos")]
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

#[cfg(not(windows))]
#[cfg(feature = "wayland")]
use crate::uikit::MenuItemLayout;
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
    rendering::{
        MainThreadTextureIDIssuer, RenderMessage, RenderMessageSender, RenderThread, RendererSync,
        ShaderTexture, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTexture, CompositeTree, CompositeTreeRef,
            CompositeTreeSyncBuffer, CornerRadius, Gradient, GradientRef, TextureMappingMode,
            TextureType,
        },
        text::{FontID, FontSet, TextLayout},
    },
    uikit::{
        CheckboxView, MenuBaseSurfaceEventHandler, MenuItem, MenuItemCommonResources, MenuItemView,
        MountContext, MountTarget, NumericInputView, NumericInputViewBackingStore, PopupID,
        PopupManager, RawMountTarget, RenderContext, ScrollContainer, SimpleButtonEventHandler,
        SimpleButtonView, StaticTextView, TeardownContext, TextInputView, ViewElementSize,
        ViewEventHandler, ViewFeedbackContext, ViewFeedbackHandler, ViewFeedbackPerformAtomic,
        ViewFeedbackRegistry, ViewIdentifier, ViewInitContext, ViewLocation, ViewPlacement,
        ViewRegistry, ViewUpdateContext,
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
mod perf;
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
        .init();

    crate::perf::init_profiler();

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

    let root_font_set = FontSet::new();
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
        root_font_set,
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

    crate::perf::fini_profiler();
}

fn main_wrapper<'sys, AppFuture: core::future::Future<Output = ()> + 'sys>(
    run_app: impl FnOnce(LaunchArgs<'sys>, SystemLink<'sys>) -> AppFuture,
    event_store: &mut VecDeque<Event>,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
    fs: &'sys FileSystem,
    vk_device: &'sys Graphics,
    rt_sender: RenderMessageSender,
    rt_receiver: std::sync::mpsc::Receiver<RenderMessage>,
    root_font_set: FontSet,
    preview_state: &'sys Mutex<rendering::preview::CommittedState>,
    #[cfg(windows)] app_context: &'sys mut platform::windows::ApplicationContext,
    #[cfg(windows)] dx_context: &'sys platform::windows::DxContext,
    #[cfg(feature = "wayland")] dp_context: &'sys mut platform::unix::wayland::DisplayServerContext,
    #[cfg(feature = "wayland")] static_pixbufs: &'sys platform::unix::wayland::StaticPixbufs,
    #[cfg(target_os = "linux")] dbus: &'sys dbus::Connection,
) {
    perf_sample_memory!();

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
    let pointer_hovering_timer = core::pin::pin!(
        utils::platform::windows::WaitableTimer::new(false).expect("pointer_hovering_timer.create")
    );
    #[cfg(windows)]
    let context_menu_delayed_action_timer = core::pin::pin!(
        utils::platform::windows::WaitableTimer::new(false)
            .expect("context_menu_delayed_action_timer.create")
    );
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
            font_set: &root_font_set,
            rt_sender: rt_sender.clone(),
            vk_device,
            event_dispatcher: app_event_dispatcher.as_mut().get_mut(),
            app_context: app_context,
            pointer_hovering_timer: pointer_hovering_timer.as_ref().get_ref(),
            flyout_surface_context: platform::windows::flyout_surface::SharedState::new(
                app_context,
                &dx_context,
                context_menu_delayed_action_timer.as_ref(),
            ),
        },
        #[cfg(not(windows))]
        SystemLink {
            rt_sender: rt_sender.clone(),
            vk_device,
            font_set: &root_font_set,
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
            gfx: vk_device,
            shutdown_signal: &shutdown,
            renderer_sync,
            global_time_base,
            event_bus: &sync_event_bus,
            message_receiver: rt_receiver,
            font_set: &root_font_set,
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

        perf_sample_memory!();

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

                    crate::perf_sample_memory!();
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

            if r.0 == windows::Win32::Foundation::WAIT_OBJECT_0.0 {
                sync_event_bus.redispatch(&app_event_dispatcher);
                continue;
            }
            if r.0 == windows::Win32::Foundation::WAIT_OBJECT_0.0 + 1 {
                app_event_dispatcher.dispatch(Event::PointerHover);
                continue;
            }
            if r.0 == windows::Win32::Foundation::WAIT_OBJECT_0.0 + 2 {
                app_event_dispatcher.dispatch(Event::MenuPerformDelayedAction);
                continue;
            }
            #[cfg(feature = "enable-profiling")]
            if r.0 == windows::Win32::Foundation::WAIT_OBJECT_0.0 + 3 {
                crate::perf::profiler().emit_memory_stats();
                continue;
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

        event_store.push_back(Event::Quit);
        while app
            .as_mut()
            .poll(&mut core::task::Context::from_waker(&unsafe {
                core::task::Waker::new(&(), &APP_WAKER_VTABLE)
            }))
            .is_pending()
        {}

        shutdown.store(true, std::sync::atomic::Ordering::Release);
        render_thread.join().expect("render_thread join");
    });
}

#[derive(Clone, Debug)]
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
        view_constructor: NonCloneable<DummyDebug<Box<dyn FlyoutSurfaceViewConstructor>>>,
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
    UpdateView {
        id: ViewIdentifier,
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
            Self::UpdateView { .. } => "UpdateView",
            Self::DockMoveSplitter { .. } => "DockMoveSplitter",
            Self::DockBeginPreview { .. } => "DockBeginPreview",
            Self::DockMovePreview { .. } => "DockMovePreview",
            Self::DockConfirm { .. } => "DockConfirm",
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

pub struct RadioButtonView {
    eh: Rc<RadioButtonEventHandler>,
}
impl RadioButtonView {
    pub fn new(
        ctx: &mut ViewInitContext,
        rect: Rect<LogicalUnit>,
        group_controller: &Rc<RadioButtonGroupController>,
    ) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(rect.left),
                AnimatableFloat::Value(rect.top),
            ],
            size: [
                AnimatableFloat::Value(rect.width),
                AnimatableFloat::Value(rect.height),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            border: Some(Border {
                thickness: 0.5,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.5]),
                ..Default::default()
            }),
            corner_radius: CornerRadius::all(8.0),
            ..Default::default()
        });
        let ct_mark = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(4.0), AnimatableFloat::Value(4.0)],
            size: [
                AnimatableFloat::Value(rect.width - 8.0),
                AnimatableFloat::Value(rect.height - 8.0),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            corner_radius: CornerRadius::all(4.0),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            left: rect.left,
            top: rect.top,
            width: rect.width,
            height: rect.height,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_root, ct_mark);

        let eh = Rc::new_cyclic(|thisref| RadioButtonEventHandler {
            thisref: thisref.clone(),
            ct_root,
            ct_mark,
            ht_root,
            group_controller: Rc::downgrade(group_controller),
            current: Cell::new(false),
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);
        unsafe { &mut *group_controller.views.get() }.push(Rc::downgrade(&eh));

        Self { eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }
}

struct RadioButtonEventHandler {
    thisref: std::rc::Weak<RadioButtonEventHandler>,
    ct_root: CompositeTreeRef,
    ct_mark: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    group_controller: std::rc::Weak<RadioButtonGroupController>,
    current: Cell<bool>,
}
impl HitTestTreeActionHandler for RadioButtonEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .get_mut(self.ct_root)
            .border
            .as_mut()
            .expect("no border?")
            .color = AnimatableColor::Animated {
            from_value: [1.0, 1.0, 1.0, 0.5],
            to_value: [1.0, 1.0, 1.0, 1.0],
            sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .get_mut(self.ct_root)
            .border
            .as_mut()
            .expect("no border?")
            .color = AnimatableColor::Animated {
            from_value: [1.0, 1.0, 1.0, 1.0],
            to_value: [1.0, 1.0, 1.0, 0.5],
            sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
            curve: AnimationCurve::Linear,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if let Some(ref x) = self.group_controller.upgrade() {
            x.select(&self.thisref, context.composite_tree, context.current_sec);
        }

        EventContinueControl::STOP_PROPAGATION
    }
}
impl RadioButtonEventHandler {
    fn update_mark<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.current.get() {
            composite_tree.get_mut(self.ct_mark).opacity = AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 1.0,
                sec_duration: (current_sec..current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            };
        } else {
            composite_tree.get_mut(self.ct_mark).opacity = AnimatableFloat::Animated {
                from_value: 1.0,
                to_value: 0.0,
                sec_duration: (current_sec..current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            };
        }
        composite_tree.mark_dirty(self.ct_mark);
    }

    fn set_current<E>(&self, value: bool, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.current.replace(value) != value {
            // changed
            self.update_mark(composite_tree, current_sec);
        }
    }
}

pub struct RadioButtonGroupController {
    views: core::cell::UnsafeCell<Vec<std::rc::Weak<RadioButtonEventHandler>>>,
}
impl RadioButtonGroupController {
    pub fn new() -> Self {
        Self {
            views: core::cell::UnsafeCell::new(Vec::new()),
        }
    }

    fn select<E>(
        &self,
        target: &std::rc::Weak<RadioButtonEventHandler>,
        composite_tree: &mut CompositeTree<E>,
        current_sec: f32,
    ) {
        for x in unsafe { &*self.views.get() }.iter() {
            if let Some(ref x1) = x.upgrade() {
                x1.set_current(
                    std::rc::Weak::ptr_eq(x, target),
                    composite_tree,
                    current_sec,
                );
            }
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
    eh: Rc<ColorPickerEventHandler>,
    first_rendered: bool,
}
impl ColorPickerView {
    const RING_THICKNESS: f32 = 12.0;
    const GRADIENT_BOX_MARGIN: f32 = 4.0;
    const POINTER_SIZE: f32 = 12.0;
    const ALPHA_SLIDER_THUMB_THICKNESS: f32 = 3.0;

    pub fn new(
        ctx: &mut ViewInitContext,
        lt: Point<LogicalUnit>,
        backing_store: &std::rc::Weak<impl ColorPickerBackingStoreEvent + 'static>,
    ) -> Self {
        let shared = COLOR_PICKER_SHARED_RES.0.get_or_init(|| {
            ColorPickerSharedResources::new(
                ctx.main_thread_texture_id_issuer,
                ctx.system_link.rt_sender(),
            )
        });

        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(lt.x), AnimatableFloat::Value(lt.y)],
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
        let ct_sat_light_box = ctx.mount_context.composite_tree.create(CompositeRect {
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
            composite_mode: CompositeMode::ColorPickerGradientBox(AnimatableColor::Value([
                1.0, 0.0, 0.0, 1.0,
            ])),
            ..Default::default()
        });
        let ct_pointer = ctx.mount_context.composite_tree.create(CompositeRect {
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
        let ct_pointer_dark = ctx.mount_context.composite_tree.create(CompositeRect {
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
            ctx.mount_context
                .composite_tree
                .create_gradient(Gradient::Linear {
                    start_color: [1.0, 0.0, 0.0, 0.0],
                    end_color: [1.0, 0.0, 0.0, 1.0],
                    start_pos_relative: [0.0, 0.0],
                    end_pos_relative: [1.0, 0.0],
                });
        let ct_alpha_slider_base = ctx.mount_context.composite_tree.create(CompositeRect {
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
        let ct_alpha_slider_content = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillLinearGradient(alpha_slider_content_gradient),
            ..Default::default()
        });
        let ct_alpha_slider_thumb = ctx.mount_context.composite_tree.create(CompositeRect {
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
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.1, 0.1, 0.1, 1.0])),
            border: Some(Border {
                thickness: 0.5,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_hex_label = ctx.mount_context.composite_tree.create(CompositeRect {
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
            left: lt.x,
            top: lt.y,
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
            backing_store: backing_store.clone(),
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
            hex_text_input_view: ColorPickerHexTextInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(32.0, 128.0 + 32.0 + 16.0),
                    Size::new_logical(128.0 - 32.0, 20.0),
                ),
                thisref,
            ),
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);
        ctx.ht_manager.set_action_handler(ht_sat_light_box, &eh);
        ctx.ht_manager.set_action_handler(ht_alpha_slider, &eh);

        if let Some(e) = backing_store.upgrade() {
            let v = e.value();

            eh.set_by_color(v, ctx.composite_tree);
            eh.hex_text_input_view.set_value(v, ctx.system_link);
        }

        Self {
            eh,
            first_rendered: false,
        }
    }

    pub fn render(&mut self, ctx: &mut RenderContext, target: &(impl MountTarget + ?Sized)) {
        if !self.first_rendered {
            // first rende
            ctx.composite_tree
                .add_child(target.ct_root(), self.eh.ct_root);
            ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
        }

        self.first_rendered = true;
        // これだけ遅延させる必要がある（ScreenPositionInterestsどうしようか......）
        self.eh.hex_text_input_view.render(
            ctx,
            &uikit::RawMountTarget {
                ht_root: self.eh.ht_root,
                ct_root: self.eh.ct_root,
            },
        );
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
    hex_text_input_view: ColorPickerHexTextInputView,
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
            self.select_hue(hue, context.composite_tree, context.system_link);
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
                context.system_link,
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
            self.color_changed(context.system_link, context.composite_tree);
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
            self.select_hue(hue, context.composite_tree, context.system_link);

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
                context.system_link,
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
            self.color_changed(context.system_link, context.composite_tree);
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
            self.select_hue(hue, context.composite_tree, context.system_link);

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
                context.system_link,
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
            self.color_changed(context.system_link, context.composite_tree);
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
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
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
        syslink: &SystemLink,
    ) {
        self.current_light
            .set(1.0 - y / self.sat_light_box_size.height);
        self.current_saturation
            .set(x / self.sat_light_box_size.width);
        self.color_changed(syslink, composite_tree);

        let ct_pointer = composite_tree.get_mut(self.ct_pointer);
        ct_pointer.offset = [
            AnimatableFloat::Value(x - ColorPickerView::POINTER_SIZE * 0.5),
            AnimatableFloat::Value(y - ColorPickerView::POINTER_SIZE * 0.5),
        ];
        composite_tree.mark_dirty(self.ct_pointer);
    }

    fn select_hue<E>(&self, hue: f32, composite_tree: &mut CompositeTree<E>, syslink: &SystemLink) {
        self.current_hue.set(hue);
        self.color_changed(syslink, composite_tree);

        let r = hue_to_rgb_wave(hue + 120.0);
        let g = hue_to_rgb_wave(hue);
        let b = hue_to_rgb_wave(hue - 120.0);

        composite_tree.get_mut(self.ct_sat_light_box).composite_mode =
            CompositeMode::ColorPickerGradientBox(AnimatableColor::Value([r, g, b, 1.0]));
        composite_tree.mark_dirty(self.ct_sat_light_box);
    }

    fn color_changed<E>(&self, syslink: &SystemLink, composite_tree: &mut CompositeTree<E>) {
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

        self.hex_text_input_view.set_value(rgba, syslink);

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
            e.new_value(rgba, syslink.event_dispatcher());
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
    eh: Rc<ColorPickerHexTextInputEventHandler>,
}
impl ColorPickerHexTextInputView {
    pub fn new(
        ctx: &mut ViewInitContext,
        rect: Rect<LogicalUnit>,
        parent_view_handler: &std::rc::Weak<ColorPickerEventHandler>,
    ) -> Self {
        let view_id = ctx.view_registry.alloc();
        let kf_token = ctx.keyboard_focus_registry.acquire_token();
        let raw = uikit::RawTextInputView::new(
            ctx,
            rect,
            "00000000".into(),
            kf_token,
            uikit::RawTextInputViewCreateFlags::NON_DELEGATED_HT,
            view_id,
        );
        let eh = Rc::new(ColorPickerHexTextInputEventHandler {
            value: Cell::new(0),
            raw: RefCell::new(raw),
            id: view_id,
            token: kf_token,
            parent_view_handler: parent_view_handler.clone(),
        });
        ctx.keyboard_focus_registry.set_event_handler(kf_token, &eh);
        ctx.view_registry.set_event_handler(eh.id, &eh);

        Self { eh }
    }

    pub fn render(&self, ctx: &mut RenderContext, parent: &(impl MountTarget + ?Sized)) {
        self.eh.raw.borrow_mut().render(ctx, parent);
    }

    pub fn set_keyboard_focus_group(
        &self,
        group: KeyboardFocusGroupRef,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        keyboard_focus_registry.join_group(group, self.eh.token);
    }

    fn set_value(&self, value: u32, syslink: &SystemLink) {
        self.eh.value.set(value);
        self.eh
            .raw
            .borrow()
            .set_content_lazy(ColorPickerHexTextInputEventHandler::fmt(value));
        syslink.dispatch_event(Event::UpdateView { id: self.eh.id });
    }
}

struct ColorPickerHexTextInputEventHandler {
    value: Cell<u32>,
    raw: RefCell<uikit::RawTextInputView>,
    id: ViewIdentifier,
    token: FocusTargetToken,
    parent_view_handler: std::rc::Weak<ColorPickerEventHandler>,
}
impl ViewEventHandler for ColorPickerHexTextInputEventHandler {
    #[inline(always)]
    fn update(&self, context: &mut ViewUpdateContext) {
        self.raw.borrow().fwd_view_update(context);
    }
}
impl KeyInputEventHandler for ColorPickerHexTextInputEventHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        // HitTestTreeへの変更がはいるので遅延させる
        self.raw.borrow().set_focus_lazy(context.ht_manager);
        context
            .system_link
            .dispatch_event(Event::UpdateView { id: self.id });
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        self.raw.borrow().release_focus_lazy(context.ht_manager);
        self.confirm_direct_input(context.system_link, context.composite_tree);
    }

    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode, modifier: ModifierKey) {
        if code == KeyInputCode::Enter {
            // 確定or入力開始
            self.confirm_direct_input(context.system_link, context.composite_tree);
            return;
        }

        if code == KeyInputCode::Esc {
            // 入力キャンセル
            self.cancel_direct_input(context.system_link);
            return;
        }

        self.raw.borrow().fwd_keydown(context, code, modifier);
    }

    #[inline(always)]
    fn r#char(&self, context: &mut InputEventContext, ch: char, _modifier: ModifierKey) {
        self.raw.borrow().fwd_char(context, ch);
    }

    #[inline(always)]
    #[cfg(feature = "wayland")]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: Option<&str>,
        new_preedit_string: Option<&str>,
    ) {
        self.raw
            .borrow()
            .fwd_ime_state_changes(context, new_committed_string, new_preedit_string);
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

    fn confirm_direct_input<E>(&self, syslink: &SystemLink, composite_tree: &mut CompositeTree<E>) {
        let current_value = self.value.get();
        let new_value = Self::parse(&self.raw.borrow().content()).unwrap_or(current_value);
        self.value.set(new_value);

        // HitTestTreeへの変更がはいるので遅延させる
        self.raw.borrow().set_content_lazy(Self::fmt(new_value));
        syslink.dispatch_event(Event::UpdateView { id: self.id });

        if current_value != new_value {
            // notify changed
            if let Some(parent) = self.parent_view_handler.upgrade() {
                parent.set_by_color(new_value, composite_tree);

                if let Some(e) = parent.backing_store.upgrade() {
                    e.new_value(new_value, syslink.event_dispatcher());
                }
            }
        }
    }

    fn cancel_direct_input(&self, syslink: &SystemLink) {
        self.raw
            .borrow()
            .set_content_lazy(Self::fmt(self.value.get()));
        syslink.dispatch_event(Event::UpdateView { id: self.id });
    }
}

pub trait ColorPickerBackingStoreEvent {
    fn value(&self) -> u32;
    fn new_value(&self, value: u32, event_dispatcher: &LogicFiberEventDispatcher);
}

pub struct EditableColorButtonView {
    eh: Rc<EditableColorButtonEventHandler>,
}
impl EditableColorButtonView {
    const COLOR_PREVIEW_MARGIN: f32 = 6.0;

    pub fn new(ctx: &mut ViewInitContext, rect: Rect<LogicalUnit>, init_color: u32) -> Self {
        let shared = COLOR_PICKER_SHARED_RES.0.get_or_init(|| {
            ColorPickerSharedResources::new(
                ctx.main_thread_texture_id_issuer,
                ctx.system_link.rt_sender(),
            )
        });

        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(rect.left),
                AnimatableFloat::Value(rect.top),
            ],
            size: [
                AnimatableFloat::Value(rect.width),
                AnimatableFloat::Value(rect.height),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            border: Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }),
            corner_radius: CornerRadius::all(8.0),
            ..Default::default()
        });
        let ct_color_base = ctx.mount_context.composite_tree.create(CompositeRect {
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
        let ct_color = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                init_color as u8 as f32 / 255.0,
                (init_color >> 8) as u8 as f32 / 255.0,
                (init_color >> 16) as u8 as f32 / 255.0,
                (init_color >> 24) as u8 as f32 / 255.0,
            ])),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            left: rect.left,
            top: rect.top,
            width: rect.width,
            height: rect.height,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_color_base, ct_color);
        ctx.composite_tree.add_child(ct_root, ct_color_base);

        let eh = Rc::new_cyclic(|thisref| EditableColorButtonEventHandler {
            thisref: thisref.clone(),
            view_id: ctx.view_registry.alloc(),
            ct_root,
            ht_root,
            ct_color_base,
            ct_color,
            color: Cell::new(init_color),
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);
        ctx.view_registry.set_event_handler(eh.view_id, &eh);

        Self { eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }
}

struct EditableColorButtonEventHandler {
    thisref: std::rc::Weak<EditableColorButtonEventHandler>,
    view_id: ViewIdentifier,
    ct_root: CompositeTreeRef,
    ct_color_base: CompositeTreeRef,
    ct_color: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    color: Cell<u32>,
}
impl ViewEventHandler for EditableColorButtonEventHandler {
    fn update(&self, context: &mut ViewUpdateContext) {
        context.composite_tree.get_mut(self.ct_color).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Value([
                self.color.get() as u8 as f32 / 255.0,
                (self.color.get() >> 8) as u8 as f32 / 255.0,
                (self.color.get() >> 16) as u8 as f32 / 255.0,
                (self.color.get() >> 24) as u8 as f32 / 255.0,
            ]));
        context.composite_tree.mark_dirty(self.ct_color);
    }
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

    fn new_value(&self, value: u32, event_dispatcher: &LogicFiberEventDispatcher) {
        self.color.set(value);
        event_dispatcher.dispatch(Event::UpdateView { id: self.view_id });
    }
}

struct EditableColorButtonPickerFlyoutView {
    inner_view: ColorPickerView,
}
impl EditableColorButtonPickerFlyoutView {
    fn new(
        ctx: &mut ViewInitContext,
        backing_store: &std::rc::Weak<EditableColorButtonEventHandler>,
    ) -> Self {
        Self {
            inner_view: ColorPickerView::new(ctx, Point::new_logical(8.0, 8.0), backing_store),
        }
    }
}
impl FlyoutSurfaceView for EditableColorButtonPickerFlyoutView {
    fn render(&mut self, ctx: &mut RenderContext, surface: FlyoutSurfaceHandle) {
        self.inner_view.render(ctx, &surface);
    }
}

pub struct EditableColorButtonPickerFlyoutViewConstructor {
    backing_store: std::rc::Weak<EditableColorButtonEventHandler>,
}
impl FlyoutSurfaceViewConstructor for EditableColorButtonPickerFlyoutViewConstructor {
    fn size(&self) -> Size<LogicalUnit> {
        Size::new_logical(128.0 + 16.0, 128.0 + 32.0 + 16.0 + 20.0 + 16.0)
    }

    fn create(&self, ctx: &mut ViewInitContext) -> Box<dyn FlyoutSurfaceView> {
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
    fn new_value(&self, value: u32, _event_dispatcher: &LogicFiberEventDispatcher) {
        self.color.set(value);
    }
}

macro_rules! internal_pane_identifier {
    ($name: literal) => {
        concat!("io.ct2.peridot.editor.internal.pane.", $name)
    };
}

struct UIKitPreviewNumericInputValueStore(Cell<i64>);
impl NumericInputViewBackingStore for UIKitPreviewNumericInputValueStore {
    fn display_value(&self, _requester: ViewIdentifier, _application: &Application) -> String {
        self.0.get().to_string()
    }

    fn set_delta(
        &self,
        _sender: ViewIdentifier,
        _application: &mut ApplicationMutation,
        delta: f32,
    ) {
        self.0.update(|x| x + (delta * 0.5).round() as i64)
    }

    fn set_from_string(
        &self,
        _sender: ViewIdentifier,
        _application: &mut ApplicationMutation,
        input: &str,
    ) {
        let Some(new_value) = input
            .split_once('.')
            .map_or(input, |x| x.0)
            .parse::<i64>()
            .ok()
        else {
            // invalid input(hold current)
            return;
        };

        self.0.set(new_value);
    }
}

crate::perf_section!(PANE_INIT_UIKIT_PREVIEW = "PaneInitialize.UIKitPreview");

pub struct UIKitPreviewPanePresenter {
    kf_group: KeyboardFocusGroupRef,
    scroll_container: ScrollContainer,
    test_alert_btn: SimpleButtonView,
    test_alert_btn2: SimpleButtonView,
    text_input_view: TextInputView,
    text_input_view2: TextInputView,
    ml_text_editor_view: uikit::MultilineTextInputView,
    color_picker_backing_store: Rc<ColorPickerTestBackingStore>,
    color_picker: ColorPickerView,
    editable_color_button: EditableColorButtonView,
    numeric_input_view_backing_store: Rc<UIKitPreviewNumericInputValueStore>,
    numeric_input_view: NumericInputView,
    dropdown_box: uikit::dropdown_box::View,
    toggle_button: uikit::ToggleButtonView,
    checkbox: uikit::CheckboxView,
    rgc1: Rc<RadioButtonGroupController>,
    rgc2: Rc<RadioButtonGroupController>,
    radio_button1: RadioButtonView,
    radio_button2: RadioButtonView,
    radio_button3: RadioButtonView,
    radio_button4: RadioButtonView,
}
impl UIKitPreviewPanePresenter {
    const ID: &str = internal_pane_identifier!("UIKitPreview");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        crate::perf_scope!(PANE_INIT_UIKIT_PREVIEW);

        // TODO: ペイン内コンテンツのFocusGroupどうするか......(いったんペイン内ローカルでつくる)
        let kf_group = ctx.keyboard_focus_registry.acquire_group();

        let scroll_container = ScrollContainer::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(0.0, 0.0),
                Size::new_logical(128.0, 128.0),
            ),
        );

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

        let mut label = StaticTextView::new(
            "Simple Buttons + Alert Dialog".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(8.0, ytop),
                size: ViewElementSize::Automatic,
            },
        );
        ytop += label.compute_size_without_render(ctx.system_link).height;
        let mut test_alert_btn = SimpleButtonView::new(
            ctx,
            "Test Alert".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(16.0, ytop),
                size: ViewElementSize::Automatic,
            },
            Some(Box::new(AlertButtonEventHandler(
                "てすとめっせーじ from button\n改行もしてみる".into(),
            ))),
        );
        let mut test_alert_btn2 = SimpleButtonView::new(
            ctx,
            "Test Alert 2".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(88.0, ytop),
                size: ViewElementSize::Fixed(Size::new_logical(96.0, 24.0))
            },
            Some(Box::new(AlertButtonEventHandler("とてもとても長いメッセージで自動折り返しをしてみる ああああああああああああああああああああああああああああああ".into()))),
        );
        ytop += 24.0;

        label.render(&mut ctx.make_render_context(), &scroll_container);
        test_alert_btn.render(&mut ctx.make_render_context(), &scroll_container, kf_group);
        test_alert_btn2.render(&mut ctx.make_render_context(), &scroll_container, kf_group);

        ytop += 8.0;

        let mut label = StaticTextView::new(
            "Text Input(Single Line)".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(8.0, ytop),
                size: ViewElementSize::Automatic,
            },
        );
        ytop += label.compute_size_without_render(ctx.system_link).height;
        let mut text_input_view = TextInputView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0, ytop),
                Size::new_logical(128.0, 20.0),
            ),
        );
        ytop += 24.0;
        let mut text_input_view2 = TextInputView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0, ytop),
                Size::new_logical(128.0, 20.0),
            ),
        );
        ytop += 24.0;

        label.render(&mut ctx.make_render_context(), &scroll_container);
        text_input_view.render(&mut ctx.make_render_context(), &scroll_container, kf_group);
        text_input_view2.render(&mut ctx.make_render_context(), &scroll_container, kf_group);

        ytop += 8.0;

        let mut label = StaticTextView::new(
            "Text Input (Multiline)".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(8.0, ytop),
                size: ViewElementSize::Automatic,
            },
        );
        label.render(&mut ctx.make_render_context(), &scroll_container);
        ytop += label.compute_size_without_render(ctx.system_link).height;
        let ml_text_kf_token = ctx.keyboard_focus_registry.acquire_token();
        let ml_text_editor_view = uikit::MultilineTextInputView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0, ytop),
                Size::new_logical(160.0, 100.0),
            ),
            "".into(),
            ml_text_kf_token,
            uikit::RawTextInputViewCreateFlags::NON_DELEGATED_HT,
        );
        ml_text_editor_view.mount(ctx, &scroll_container);
        ytop += 100.0;

        ytop += 8.0;

        let color_picker_backing_store = Rc::new(ColorPickerTestBackingStore {
            color: Cell::new(0xffffffff),
        });
        let mut label = StaticTextView::new(
            "Color Picker(Standalone)".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(8.0, ytop),
                size: ViewElementSize::Automatic,
            },
        );
        label.render(&mut ctx.make_render_context(), &scroll_container);
        ytop += label.compute_size_without_render(ctx.system_link).height;
        let mut color_picker = ColorPickerView::new(
            ctx,
            Point::new_logical(16.0, ytop),
            &Rc::downgrade(&color_picker_backing_store),
        );
        color_picker.render(&mut ctx.make_render_context(), &scroll_container);
        ytop += 128.0 + 32.0 + 16.0 + 20.0;

        ytop += 8.0;

        let mut label = StaticTextView::new(
            "Color Picker(Button Style)".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(8.0, ytop),
                size: ViewElementSize::Automatic,
            },
        );
        label.render(&mut ctx.make_render_context(), &scroll_container);
        let label_width = label.compute_size_without_render(ctx.system_link).width;
        let editable_color_button = EditableColorButtonView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0 + label_width, ytop - 2.0),
                Size::new_logical(64.0, 20.0),
            ),
            0xffffffff,
        );
        editable_color_button.mount(ctx, &scroll_container);
        ytop += 20.0;
        content_width = content_width.max(label_width + 16.0 + 64.0 + 8.0);

        let numeric_input_view_backing_store =
            Rc::new(UIKitPreviewNumericInputValueStore(Cell::new(0)));
        let mut label = StaticTextView::new(
            "Numeric Input".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(8.0, ytop),
                size: ViewElementSize::Automatic,
            },
        );
        label.render(&mut ctx.make_render_context(), &scroll_container);
        let label_width = label.compute_size_without_render(ctx.system_link).width;
        let mut numeric_input_view = NumericInputView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0 + label_width, ytop - 2.0),
                Size::new_logical(64.0, 20.0),
            ),
            Rc::downgrade(&numeric_input_view_backing_store),
        );
        numeric_input_view.post_init(ctx);
        numeric_input_view.render(&mut ctx.make_render_context(), &scroll_container, kf_group);
        ytop += 20.0;

        let mut label = StaticTextView::new(
            "Dropdown".into(),
            ViewPlacement {
                location: ViewLocation::new_left_top(8.0, ytop + 4.0),
                size: ViewElementSize::Automatic,
            },
        );
        label.render(&mut ctx.make_render_context(), &scroll_container);
        let label_width = label.compute_size_without_render(ctx.system_link).width;
        let mut dropdown_box = uikit::dropdown_box::View::new(
            ViewPlacement {
                location: ViewLocation::new_left_top(label_width + 16.0, ytop),
                size: ViewElementSize::Fixed(Size::new_logical(128.0, 24.0)),
            },
            vec![
                "DropdownBox Item 1".into(),
                "DropdownBox Item 2".into(),
                "DropdownBox Item 3 too long version".into(),
            ],
        );
        dropdown_box.render(&mut ctx.make_render_context(), &scroll_container);
        ytop += 28.0;

        let mut toggle_button = uikit::ToggleButtonView::new(
            ViewPlacement {
                location: ViewLocation::new_left_top(8.0, ytop),
                size: ViewElementSize::Automatic,
            },
            "Toggle / Checkbox".into(),
        );
        toggle_button.render(&mut ctx.make_render_context(), &scroll_container);

        let mut checkbox = uikit::CheckboxView::new(ViewPlacement {
            location: ViewLocation::new_left_top(144.0, ytop + 4.0),
            size: ViewElementSize::Automatic,
        });
        checkbox.render(&mut ctx.make_render_context(), &scroll_container);
        ytop += 24.0;

        ytop += 8.0;

        let label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(8.0), AnimatableFloat::Value(ytop)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "Radio Buttons/Groups".into(),
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                ..Default::default()
            }),
            ..Default::default()
        });
        ctx.composite_tree
            .add_child(scroll_container.ct_root(), label);
        ytop += 16.0;

        let rgc1 = Rc::new(RadioButtonGroupController::new());
        let rgc2 = Rc::new(RadioButtonGroupController::new());
        let radio_button1 = RadioButtonView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0, ytop),
                Size::new_logical(16.0, 16.0),
            ),
            &rgc1,
        );
        radio_button1.mount(ctx, &scroll_container);
        let radio_button2 = RadioButtonView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(36.0, ytop),
                Size::new_logical(16.0, 16.0),
            ),
            &rgc1,
        );
        radio_button2.mount(ctx, &scroll_container);
        let radio_button3 = RadioButtonView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(56.0, ytop),
                Size::new_logical(16.0, 16.0),
            ),
            &rgc1,
        );
        radio_button3.mount(ctx, &scroll_container);
        let radio_button4 = RadioButtonView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(76.0, ytop),
                Size::new_logical(16.0, 16.0),
            ),
            &rgc2,
        );
        radio_button4.mount(ctx, &scroll_container);
        ytop += 24.0;

        scroll_container.set_content_size(
            Size::new_logical(content_width, ytop + 8.0),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );
        Self {
            kf_group,
            scroll_container,
            test_alert_btn,
            test_alert_btn2,
            text_input_view,
            text_input_view2,
            ml_text_editor_view,
            color_picker_backing_store,
            color_picker,
            editable_color_button,
            numeric_input_view_backing_store,
            numeric_input_view,
            dropdown_box,
            toggle_button,
            checkbox,
            rgc1,
            rgc2,
            radio_button1,
            radio_button2,
            radio_button3,
            radio_button4,
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

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {
        self.scroll_container.mount(ctx, target);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        self.scroll_container.unmount(ctx);
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {}

    fn resize(
        &self,
        new_size: &Size<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        self.scroll_container
            .resize(new_size.clone(), composite_tree, ht_manager);
    }
}

struct TimelinePanePresenter {}
impl TimelinePanePresenter {
    const ID: &str = internal_pane_identifier!("Timeline");
}
impl ui::dock::PaneContentPresenter for TimelinePanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Timeline".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {}

    fn unmount(&self, ctx: &mut MountContext) {}

    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}

struct ObjectTreePanePresenter {
    eh: Rc<ObjectTreePaneEventHandler>,
}
impl ObjectTreePanePresenter {
    const ID: &str = internal_pane_identifier!("ObjectTree");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });

        let ht_context_menu_receiver = ctx.ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });

        let eh = Rc::new(ObjectTreePaneEventHandler {
            ct_root,
            ht_root,
            ht_context_menu_receiver,
            object_tree_changed: Cell::new(false),
            object_selection_changed: Cell::new(false),
            row_views: RefCell::new(Vec::new()),
        });
        ctx.ht_manager
            .set_action_handler(eh.ht_context_menu_receiver, &eh);
        ctx.subscribe_view_feedback::<ViewFeedbackPerformAtomic>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectTreeChanged>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&eh);

        ctx.ht_manager
            .add_child(eh.ht_root, eh.ht_context_menu_receiver);

        Self { eh }
    }
}
impl ui::dock::PaneContentPresenter for ObjectTreePanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Object Tree".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.eh.ct_root);
        ctx.ht_manager.remove_child(self.eh.ht_root);
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectTreeChanged>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&self.eh);

        ctx.mount_context.composite_tree.free_all(self.eh.ct_root);
        ctx.mount_context.ht_manager.free_all(self.eh.ht_root)
    }
}

pub const MENU_COMMAND_ID_OBJECT_CREATE_CUBE: u64 = 1;
pub const MENU_COMMAND_ID_OBJECT_CREATE_SPHERE: u64 = 2;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER: u64 = 3;
pub const MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE: u64 = 4;
pub const MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN: u64 = 10;

struct ObjectTreePaneEventHandler {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    ht_context_menu_receiver: HitTestTreeRef,
    object_tree_changed: Cell<bool>,
    object_selection_changed: Cell<bool>,
    row_views: RefCell<Vec<ObjectTreeObjectRowView>>,
}
impl HitTestTreeActionHandler for ObjectTreePaneEventHandler {
    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button == PointerButton::Secondary {
            context.system_link.dispatch_event(Event::MenuOpen {
                parent: context
                    .ht_manager
                    .query_root_window(self.ht_context_menu_receiver)
                    .expect("not mounted"),
                items: vec![
                    MenuItem::Heading {
                        label: "Create Object".into(),
                    },
                    MenuItem::Command {
                        label: "Cube".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CUBE,
                    },
                    MenuItem::Command {
                        label: "Sphere".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_SPHERE,
                    },
                    MenuItem::Command {
                        label: "Cylinder".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER,
                    },
                    MenuItem::Command {
                        label: "Capsule".into(),
                        command_id: MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE,
                    },
                    MenuItem::SubMenu {
                        label: "Special".into(),
                        items: vec![MenuItem::Command {
                            label: "Terrain".into(),
                            command_id: MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN,
                        }],
                    },
                ],
                surface_pos: args.client_pos,
            });

            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::empty()
    }
}
impl ViewFeedbackHandler<ViewFeedbackPerformAtomic> for ObjectTreePaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackPerformAtomic,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let object_tree_changed = self.object_tree_changed.replace(false);
        let object_selection_changed = self.object_selection_changed.replace(false);

        if object_tree_changed {
            let mut row_views = self.row_views.borrow_mut();
            for x in row_views.drain(..) {
                x.unmount(&mut context.view_init_context);
                x.teardown(&mut context.view_init_context.make_teardown_context());
            }
            for (n, &x) in context.application.root_objects.iter().enumerate() {
                let o = context.application.object(x);
                let rv = ObjectTreeObjectRowView::new(
                    &mut context.view_init_context,
                    x,
                    o.name.clone(),
                    n as f32 * ObjectTreeObjectRowView::ITEM_HEIGHT,
                    context.application.object_is_selected(x),
                );
                rv.mount(
                    &mut context.view_init_context,
                    &RawMountTarget {
                        ht_root: self.ht_root,
                        ct_root: self.ct_root,
                    },
                );
                row_views.push(rv);
            }
        }

        if object_selection_changed {
            for x in self.row_views.borrow().iter() {
                x.eh.update_selected(
                    context.application.object_is_selected(x.eh.assigned_object),
                    context.view_init_context.mount_context.composite_tree,
                    context.view_init_context.current_sec,
                );
            }
        }
    }
}
impl ViewFeedbackHandler<ViewFeedbackObjectTreeChanged> for ObjectTreePaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectTreeChanged,
        _context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.object_tree_changed.set(true);
    }
}
impl ViewFeedbackHandler<ViewFeedbackObjectSelectionChanged> for ObjectTreePaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectSelectionChanged,
        _context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.object_selection_changed.set(true);
    }
}

struct ObjectTreeObjectRowView {
    eh: Rc<ObjectTreeObjectRowEventHandler>,
}
impl ObjectTreeObjectRowView {
    const ITEM_HEIGHT: f32 = 20.0;

    fn new(
        ctx: &mut ViewInitContext,
        assigned_object: ObjectID,
        init_label: String,
        init_y: f32,
        init_selected: bool,
    ) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(init_y)],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(Self::ITEM_HEIGHT),
            ],
            relative_size_adjustment: [1.0, 0.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                0.0,
                0.25,
                1.0,
                if init_selected { 1.0 } else { 0.0 },
            ])),
            ..Default::default()
        });
        let ct_label_hover = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            relative_size_adjustment: [1.0, 1.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0; 4])),
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: init_label,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            top: init_y,
            height: Self::ITEM_HEIGHT,
            width_adjustment_factor: 1.0,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });

        let eh = Rc::new(ObjectTreeObjectRowEventHandler {
            assigned_object,
            selection_lit: Cell::new(init_selected),
            ct_root,
            ct_label_hover,
            ht_root,
        });
        ctx.ht_manager.set_action_handler(eh.ht_root, &eh);

        ctx.composite_tree.add_child(eh.ct_root, eh.ct_label_hover);

        Self { eh }
    }

    fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.eh.ct_root);
        ctx.ht_manager.remove_child(self.eh.ht_root);
    }

    fn teardown(self, ctx: &mut TeardownContext) {
        ctx.mount_context.composite_tree.free_all(self.eh.ct_root);
        ctx.mount_context.ht_manager.free_all(self.eh.ht_root);
    }
}

struct ObjectTreeObjectRowEventHandler {
    assigned_object: ObjectID,
    selection_lit: Cell<bool>,
    ct_root: CompositeTreeRef,
    ct_label_hover: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl HitTestTreeActionHandler for ObjectTreeObjectRowEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_label_hover)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.125],
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        context
            .composite_tree
            .begin_mod_chain(self.ct_label_hover)
            .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.125],
                to_value: [1.0, 1.0, 1.0, 0.0],
                sec_duration: (context.current_sec..context.current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            }))
            .apply();

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button == PointerButton::Primary {
            if args.key_modifier.contains(ModifierKey::CONTROL) {
                context
                    .application
                    .toggle_object_selection_additive(self.assigned_object);
            } else {
                context.application.select_object(self.assigned_object);
            }

            return EventContinueControl::STOP_PROPAGATION;
        }

        EventContinueControl::empty()
    }
}
impl ObjectTreeObjectRowEventHandler {
    fn update_selected<E>(
        &self,
        selected: bool,
        composite_tree: &mut CompositeTree<E>,
        current_sec: f32,
    ) {
        if self.selection_lit.replace(selected) == selected {
            // not changed
            return;
        }

        if selected {
            composite_tree
                .begin_mod_chain(self.ct_root)
                .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [0.0, 0.25, 1.0, 0.0],
                    to_value: [0.0, 0.25, 1.0, 1.0],
                    curve: AnimationCurve::EASE_OUT,
                    event_on_complete: None,
                    sec_duration: (current_sec..current_sec + 0.1).into(),
                }))
                .apply();
        } else {
            composite_tree
                .begin_mod_chain(self.ct_root)
                .composite_mode(CompositeMode::FillColor(AnimatableColor::Animated {
                    from_value: [0.0, 0.25, 1.0, 1.0],
                    to_value: [0.0, 0.25, 1.0, 0.0],
                    curve: AnimationCurve::EASE_OUT,
                    event_on_complete: None,
                    sec_duration: (current_sec..current_sec + 0.1).into(),
                }))
                .apply();
        }
    }
}

struct InspectorPanePresenter {
    eh: Rc<InspectorPaneEventHandler>,
}
impl InspectorPanePresenter {
    const ID: &str = internal_pane_identifier!("Inspector");

    pub fn new(ctx: &mut ViewInitContext) -> Self {
        // TODO: PaneのKeyboardFocusGroupどうするか
        let kf_group = ctx.keyboard_focus_registry.acquire_group();

        let root_container_view = ScrollContainer::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(0.0, 0.0),
                Size::new_logical(128.0, 128.0),
            ),
        );
        let ct_selected_object_label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(8.0), AnimatableFloat::Value(8.0)],
            size: [AnimatableFloat::Value(-16.0), AnimatableFloat::Value(12.0)],
            relative_size_adjustment: [1.0, 0.0],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "No selection".into(),
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_selected_object_name_label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(8.0),
                AnimatableFloat::Value(8.0 + 12.0),
            ],
            size: [AnimatableFloat::Value(-16.0), AnimatableFloat::Value(12.0)],
            relative_size_adjustment: [1.0, 0.0],
            ..Default::default()
        });

        let eh = Rc::new_cyclic(|eh| {
            let items_container_view = ScrollContainer::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(0.0, 8.0 + 12.0 + 12.0),
                    Size::new_logical(128.0, 128.0),
                ),
            );

            let mut label = StaticTextView::new(
                "POSITION".into(),
                ViewPlacement {
                    location: ViewLocation {
                        offset: Point::new_logical(8.0, 8.0),
                        anchor: [0.0, 0.0],
                        parent_anchor: [0.0, 0.0],
                    },
                    size: ViewElementSize::Automatic,
                },
            );
            label.set_font(FontID::UIFormLiftedLabel);
            label.render(&mut ctx.make_render_context(), &items_container_view);
            let mut local_position_x_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0, 8.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            let mut local_position_y_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0 + 40.0, 8.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            let mut local_position_z_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0 + 40.0 + 40.0, 8.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            local_position_x_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );
            local_position_y_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );
            local_position_z_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );

            let mut label = StaticTextView::new(
                "ROTATION".into(),
                ViewPlacement {
                    location: ViewLocation {
                        offset: Point::new_logical(8.0, 8.0 + 12.0 + 16.0),
                        anchor: [0.0, 0.0],
                        parent_anchor: [0.0, 0.0],
                    },
                    size: ViewElementSize::Automatic,
                },
            );
            label.set_font(FontID::UIFormLiftedLabel);
            label.render(&mut ctx.make_render_context(), &items_container_view);
            let mut local_rotation_x_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0, 8.0 + 12.0 + 16.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            let mut local_rotation_y_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0 + 40.0, 8.0 + 12.0 + 16.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            let mut local_rotation_z_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0 + 40.0 + 40.0, 8.0 + 12.0 + 16.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            local_rotation_x_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );
            local_rotation_y_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );
            local_rotation_z_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );

            let mut label = StaticTextView::new(
                "SCALE".into(),
                ViewPlacement {
                    location: ViewLocation {
                        offset: Point::new_logical(8.0, 8.0 + 12.0 + 16.0 + 12.0 + 16.0),
                        anchor: [0.0, 0.0],
                        parent_anchor: [0.0, 0.0],
                    },
                    size: ViewElementSize::Automatic,
                },
            );
            label.set_font(FontID::UIFormLiftedLabel);
            label.render(&mut ctx.make_render_context(), &items_container_view);
            let mut local_scale_x_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0, 8.0 + 12.0 + 16.0 + 12.0 + 16.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            let mut local_scale_y_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0 + 40.0, 8.0 + 12.0 + 16.0 + 12.0 + 16.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            let mut local_scale_z_input_view = NumericInputView::new(
                ctx,
                Rect::from_lt_size(
                    Point::new_logical(8.0 + 40.0 + 40.0, 8.0 + 12.0 + 16.0 + 12.0 + 16.0 + 12.0),
                    Size::new_logical(32.0, 16.0),
                ),
                eh.clone(),
            );
            local_scale_x_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );
            local_scale_y_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );
            local_scale_z_input_view.render(
                &mut ctx.make_render_context(),
                &items_container_view,
                kf_group,
            );

            let render_section_top = 8.0 + 12.0 + 16.0 + 12.0 + 16.0 + 12.0 + 16.0 + 8.0;
            let mut render_checkbox = CheckboxView::new(ViewPlacement {
                location: ViewLocation::new_left_top(8.0, render_section_top),
                size: ViewElementSize::Automatic,
            });
            render_checkbox.render(&mut ctx.make_render_context(), &items_container_view);
            let mut section_label = StaticTextView::new(
                "Render".into(),
                ViewPlacement {
                    location: ViewLocation {
                        offset: Point::new_logical(8.0 + 24.0, render_section_top),
                        anchor: [0.0, 0.0],
                        parent_anchor: [0.0, 0.0],
                    },
                    size: ViewElementSize::Automatic,
                },
            );
            section_label.render(&mut ctx.make_render_context(), &items_container_view);

            let mut label = StaticTextView::new(
                "SHAPE".into(),
                ViewPlacement {
                    location: ViewLocation {
                        offset: Point::new_logical(8.0, render_section_top + 24.0),
                        anchor: [0.0, 0.0],
                        parent_anchor: [0.0, 0.0],
                    },
                    size: ViewElementSize::Automatic,
                },
            );
            label.set_font(FontID::UIFormLiftedLabel);
            label.render(&mut ctx.make_render_context(), &items_container_view);
            let mut shape_selector = uikit::dropdown_box::View::new(
                ViewPlacement {
                    location: ViewLocation {
                        parent_anchor: [0.0, 0.0],
                        anchor: [0.0, 0.0],
                        offset: Point::new_logical(8.0, render_section_top + 24.0 + 12.0),
                    },
                    size: ViewElementSize::Automatic,
                },
                vec![
                    "Cube".into(),
                    "Sphere".into(),
                    "Cylinder".into(),
                    "Capsule".into(),
                ],
            );
            shape_selector.render(&mut ctx.make_render_context(), &items_container_view);

            items_container_view.set_content_size(
                Size::new_logical(128.0 + 16.0, render_section_top + 24.0 + 12.0 + 24.0),
                ctx.mount_context.composite_tree,
                ctx.mount_context.ht_manager,
            );

            InspectorPaneEventHandler {
                object_selection_changed: Cell::new(false),
                items_container_mounted: Cell::new(false),
                root_container_view,
                ct_selected_object_label,
                ct_selected_object_name_label,
                items_container_view,
                numeric_input_views: vec![
                    local_position_x_input_view,
                    local_position_y_input_view,
                    local_position_z_input_view,
                    local_rotation_x_input_view,
                    local_rotation_y_input_view,
                    local_rotation_z_input_view,
                    local_scale_x_input_view,
                    local_scale_y_input_view,
                    local_scale_z_input_view,
                ],
                checkboxes: vec![render_checkbox],
                dropdowns: vec![shape_selector],
            }
        });
        ctx.subscribe_view_feedback::<ViewFeedbackPerformAtomic>(&eh);
        ctx.subscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&eh);

        ctx.composite_tree.add_child(
            eh.root_container_view.ct_root(),
            eh.ct_selected_object_label,
        );
        ctx.composite_tree.add_child(
            eh.root_container_view.ct_root(),
            eh.ct_selected_object_name_label,
        );
        eh.root_container_view.set_content_size(
            Size::new_logical(128.0, 8.0 + 12.0),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );

        for x in eh.numeric_input_views.iter() {
            x.post_init(ctx);
        }

        Self { eh }
    }
}
impl ui::dock::PaneContentPresenter for InspectorPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Inspector".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {
        self.eh.root_container_view.mount(ctx, target);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        self.eh.root_container_view.unmount(ctx);
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.unsubscribe_view_feedback::<ViewFeedbackPerformAtomic>(&self.eh);
        ctx.unsubscribe_view_feedback::<ViewFeedbackObjectSelectionChanged>(&self.eh);
    }

    fn resize(
        &self,
        new_size: &Size<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        self.eh
            .root_container_view
            .resize(*new_size, composite_tree, ht_manager);
        self.eh.items_container_view.resize(
            Size::new_logical(new_size.width, new_size.height - 8.0 - 12.0 - 12.0),
            composite_tree,
            ht_manager,
        );
    }
}

struct InspectorPaneEventHandler {
    object_selection_changed: Cell<bool>,
    items_container_mounted: Cell<bool>,
    root_container_view: ScrollContainer,
    ct_selected_object_label: CompositeTreeRef,
    ct_selected_object_name_label: CompositeTreeRef,
    items_container_view: ScrollContainer,
    numeric_input_views: Vec<NumericInputView>,
    checkboxes: Vec<CheckboxView>,
    dropdowns: Vec<uikit::dropdown_box::View>,
}
impl ViewFeedbackHandler<ViewFeedbackPerformAtomic> for InspectorPaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackPerformAtomic,
        context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        let object_selection_changed = self.object_selection_changed.replace(false);

        if object_selection_changed {
            match context.application.selected_objects.len() {
                0 => {
                    context
                        .view_init_context
                        .composite_tree
                        .begin_mod_chain(self.ct_selected_object_label)
                        .text_run(CompositeRectTextRun {
                            content: "No selection".into(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        })
                        .apply();
                    context
                        .view_init_context
                        .composite_tree
                        .begin_mod_chain(self.ct_selected_object_name_label)
                        .rm_text()
                        .apply();
                    self.root_container_view.set_content_size(
                        Size::new_logical(128.0, 8.0 + 12.0),
                        context.view_init_context.mount_context.composite_tree,
                        context.view_init_context.mount_context.ht_manager,
                    );

                    if self.items_container_mounted.replace(false) {
                        self.items_container_view
                            .unmount(&mut context.view_init_context);
                    }
                }
                1 => {
                    let id = *unsafe {
                        context
                            .application
                            .selected_objects
                            .iter()
                            .next()
                            .unwrap_unchecked()
                    };
                    context
                        .view_init_context
                        .composite_tree
                        .begin_mod_chain(self.ct_selected_object_label)
                        .text_run(CompositeRectTextRun {
                            content: format!("Object {id}"),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        })
                        .apply();
                    context
                        .view_init_context
                        .composite_tree
                        .begin_mod_chain(self.ct_selected_object_name_label)
                        .text(CompositeRectText {
                            runs: vec![CompositeRectTextRun {
                                content: format!("Name: {}", context.application.object(id).name),
                                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                                ..Default::default()
                            }],
                            ..Default::default()
                        })
                        .apply();
                    self.root_container_view.set_content_size(
                        Size::new_logical(128.0, 8.0 + 12.0 + 12.0),
                        context.view_init_context.mount_context.composite_tree,
                        context.view_init_context.mount_context.ht_manager,
                    );

                    if !self.items_container_mounted.replace(true) {
                        self.items_container_view
                            .mount(&mut context.view_init_context, &self.root_container_view);
                    }

                    for x in self.numeric_input_views.iter() {
                        x.revalidate(
                            &context.application,
                            context.view_init_context.mount_context.composite_tree,
                            context.view_init_context.system_link,
                            context.view_init_context.mount_context.ht_manager,
                            context.view_init_context.current_sec,
                        );
                    }
                }
                _ => {
                    context
                        .view_init_context
                        .composite_tree
                        .begin_mod_chain(self.ct_selected_object_label)
                        .text_run(CompositeRectTextRun {
                            content: "Multiple selection".into(),
                            color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                            ..Default::default()
                        })
                        .apply();
                    context
                        .view_init_context
                        .composite_tree
                        .begin_mod_chain(self.ct_selected_object_name_label)
                        .rm_text()
                        .apply();
                    self.root_container_view.set_content_size(
                        Size::new_logical(128.0, 8.0 + 12.0),
                        context.view_init_context.mount_context.composite_tree,
                        context.view_init_context.mount_context.ht_manager,
                    );

                    if self.items_container_mounted.replace(false) {
                        self.items_container_view
                            .unmount(&mut context.view_init_context);
                    }
                }
            }
        }
    }
}
impl ViewFeedbackHandler<ViewFeedbackObjectSelectionChanged> for InspectorPaneEventHandler {
    fn accept_feedback<'a, 'h>(
        &self,
        _feedback: &ViewFeedbackObjectSelectionChanged,
        _context: &mut ViewFeedbackContext<'a, 'h>,
    ) {
        self.object_selection_changed.set(true);
    }
}
impl NumericInputViewBackingStore for InspectorPaneEventHandler {
    fn display_value(&self, requester: ViewIdentifier, application: &Application) -> String {
        // TODO: multi-select
        let Some(&selected) = application.selected_objects.iter().next() else {
            return "-".into();
        };

        if requester == self.numeric_input_views[0].id() {
            // pos x
            format!("{:.3}", application.object(selected).local_position.0)
        } else if requester == self.numeric_input_views[1].id() {
            // pos y
            format!("{:.3}", application.object(selected).local_position.1)
        } else if requester == self.numeric_input_views[2].id() {
            // pos z
            format!("{:.3}", application.object(selected).local_position.2)
        } else if requester == self.numeric_input_views[3].id() {
            // rotate x
            format!("{:.3}", application.object(selected).local_rotation_euler.0)
        } else if requester == self.numeric_input_views[4].id() {
            // rotate y
            format!("{:.3}", application.object(selected).local_rotation_euler.1)
        } else if requester == self.numeric_input_views[5].id() {
            // rotate z
            format!("{:.3}", application.object(selected).local_rotation_euler.2)
        } else if requester == self.numeric_input_views[6].id() {
            // scale x
            format!("{:.3}", application.object(selected).local_scale.0)
        } else if requester == self.numeric_input_views[7].id() {
            // scale y
            format!("{:.3}", application.object(selected).local_scale.1)
        } else if requester == self.numeric_input_views[8].id() {
            // scale z
            format!("{:.3}", application.object(selected).local_scale.2)
        } else {
            "-".into()
        }
    }

    fn set_delta(&self, sender: ViewIdentifier, application: &mut ApplicationMutation, delta: f32) {
        // TODO: multi-select
        let Some(&selected) = application.selected_objects.iter().next() else {
            return;
        };

        if sender == self.numeric_input_views[0].id() {
            // pos x
            application.object_modify_data(selected, |o| o.local_position.0 += delta * 0.1);
        } else if sender == self.numeric_input_views[1].id() {
            // pos y
            application.object_modify_data(selected, |o| o.local_position.1 += delta * 0.1);
        } else if sender == self.numeric_input_views[2].id() {
            // pos z
            application.object_modify_data(selected, |o| o.local_position.2 += delta * 0.1);
        } else if sender == self.numeric_input_views[3].id() {
            // rotate x
            application.object_modify_data(selected, |o| o.local_rotation_euler.0 += delta);
        } else if sender == self.numeric_input_views[4].id() {
            // rotate y
            application.object_modify_data(selected, |o| o.local_rotation_euler.1 += delta);
        } else if sender == self.numeric_input_views[5].id() {
            // rotate z
            application.object_modify_data(selected, |o| o.local_rotation_euler.2 += delta);
        } else if sender == self.numeric_input_views[6].id() {
            // scale x
            application.object_modify_data(selected, |o| o.local_scale.0 += delta * 0.1);
        } else if sender == self.numeric_input_views[7].id() {
            // scale y
            application.object_modify_data(selected, |o| o.local_scale.1 += delta * 0.1);
        } else if sender == self.numeric_input_views[8].id() {
            // scale z
            application.object_modify_data(selected, |o| o.local_scale.2 += delta * 0.1);
        }
    }

    fn set_from_string(
        &self,
        sender: ViewIdentifier,
        application: &mut ApplicationMutation,
        input: &str,
    ) {
        // TODO: multi-select
        let Some(&selected) = application.selected_objects.iter().next() else {
            return;
        };

        if sender == self.numeric_input_views[0].id() {
            // pos x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.0 = v);
        } else if sender == self.numeric_input_views[1].id() {
            // pos y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.1 = v);
        } else if sender == self.numeric_input_views[2].id() {
            // pos z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_position.2 = v);
        } else if sender == self.numeric_input_views[3].id() {
            // rotate x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.0 = v);
        } else if sender == self.numeric_input_views[4].id() {
            // rotate y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.1 = v);
        } else if sender == self.numeric_input_views[5].id() {
            // rotate z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_rotation_euler.2 = v);
        } else if sender == self.numeric_input_views[6].id() {
            // scale x
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_scale.0 = v);
        } else if sender == self.numeric_input_views[7].id() {
            // scale y
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_scale.1 = v);
        } else if sender == self.numeric_input_views[8].id() {
            // scale z
            let Some(v) = input.parse::<f32>().ok() else {
                // invalid input
                return;
            };
            application.object_modify_data(selected, |o| o.local_scale.2 = v);
        }
    }
}

struct AssetExplorerPanePresenter {}
impl AssetExplorerPanePresenter {
    const ID: &str = internal_pane_identifier!("AssetExplorer");
}
impl ui::dock::PaneContentPresenter for AssetExplorerPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Asset Explorer".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {}

    fn unmount(&self, ctx: &mut MountContext) {}

    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}

struct ProjectSettingsPanePresenter {}
impl ProjectSettingsPanePresenter {
    const ID: &str = internal_pane_identifier!("ProjectSettings");
}
impl ui::dock::PaneContentPresenter for ProjectSettingsPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Project Settings".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {}

    fn unmount(&self, ctx: &mut MountContext) {}

    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}

struct AssetPreviewPanePresenter {}
impl AssetPreviewPanePresenter {
    const ID: &str = internal_pane_identifier!("AssetPreview");
}
impl ui::dock::PaneContentPresenter for AssetPreviewPanePresenter {
    fn id(&self) -> String {
        Self::ID.into()
    }

    fn name(&self) -> String {
        "Asset Preview".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {}

    fn unmount(&self, ctx: &mut MountContext) {}

    fn teardown(&mut self, ctx: &mut TeardownContext) {}
}

pub enum ViewFeedback {
    ObjectTreeChanged(ViewFeedbackObjectTreeChanged),
    ObjectSelectionChanged(ViewFeedbackObjectSelectionChanged),
    ObjectDataChanged(ViewFeedbackObjectDataChanged),
}
impl ViewFeedback {
    pub const fn object_tree_changed() -> Self {
        Self::ObjectTreeChanged(ViewFeedbackObjectTreeChanged)
    }

    pub const fn object_selection_changed() -> Self {
        Self::ObjectSelectionChanged(ViewFeedbackObjectSelectionChanged)
    }

    pub const fn object_data_changed(object_id: ObjectID) -> Self {
        Self::ObjectDataChanged(ViewFeedbackObjectDataChanged(object_id))
    }

    pub fn dispatch(self, registry: &ViewFeedbackRegistry, context: &mut ViewFeedbackContext) {
        match self {
            Self::ObjectTreeChanged(o) => registry.dispatch(o, context),
            Self::ObjectSelectionChanged(o) => registry.dispatch(o, context),
            Self::ObjectDataChanged(o) => registry.dispatch(o, context),
        }
    }
}

#[derive(Clone)]
pub struct ViewFeedbackObjectTreeChanged;

pub struct ViewFeedbackObjectSelectionChanged;

pub struct ViewFeedbackObjectDataChanged(pub ObjectID);

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ObjectID(NonZeroUsize);
impl core::fmt::Display for ObjectID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0.get())
    }
}
impl ObjectID {
    const fn from_array_index(v: usize) -> Self {
        Self(unsafe { NonZeroUsize::new_unchecked(v.checked_add(1).expect("too many objects!")) })
    }

    const fn into_array_index(self) -> usize {
        self.0.get() - 1
    }
}

pub enum ObjectRenderShape {
    Cube,
    Sphere,
    Cylinder,
    Capsule,
}

pub struct Object {
    parent: Option<ObjectID>,
    children: Vec<ObjectID>,
    name: String,
    local_position: peridot_math::Vector3F32,
    local_rotation_euler: peridot_math::Vector3F32,
    local_scale: peridot_math::Vector3F32,
    world_matrix: peridot_math::Matrix4F32,
    render_enabled: bool,
    render_shape: ObjectRenderShape,
}
impl Object {
    fn new(name: String) -> Self {
        Self {
            parent: None,
            children: Vec::new(),
            name,
            local_position: peridot_math::Vector3(0.0, 0.0, 0.0),
            local_rotation_euler: peridot_math::Vector3(0.0, 0.0, 0.0),
            local_scale: peridot_math::Vector3(1.0, 1.0, 1.0),
            world_matrix: peridot_math::Matrix4F32::ONE,
            render_enabled: false,
            render_shape: ObjectRenderShape::Cube,
        }
    }

    fn reset(&mut self) {
        self.name = String::new();
        self.children = Vec::new();
        self.parent = None;
    }
}

/// Logical Application Model
pub struct Application {
    objects: Vec<Object>,
    free_object_indices: BTreeSet<usize>,
    root_objects: Vec<ObjectID>,
    selected_objects: HashSet<ObjectID>,
}
impl Application {
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            free_object_indices: BTreeSet::new(),
            root_objects: Vec::new(),
            selected_objects: HashSet::new(),
        }
    }

    fn alloc_object(&mut self, o: Object) -> ObjectID {
        if let Some(index) = self.free_object_indices.pop_first() {
            self.objects[index] = o;
            self.root_objects.push(ObjectID::from_array_index(index));
            return ObjectID::from_array_index(index);
        }

        let index = self.objects.len();
        self.objects.push(o);
        self.root_objects.push(ObjectID::from_array_index(index));
        ObjectID::from_array_index(index)
    }

    fn free_object(&mut self, id: ObjectID) {
        // detach from registry
        match self.objects[id.into_array_index()].parent.take() {
            Some(parent) => {
                self.objects[parent.into_array_index()]
                    .children
                    .retain(|&oid| oid != id);
            }
            None => {
                self.root_objects.retain(|&oid| oid != id);
            }
        }

        self.free_object_indices.insert(id.into_array_index());
        self.objects[id.into_array_index()].reset();

        // TODO: compactionの頻度を減らすかはあとで検討
        self.compaction_objects();
    }

    fn compaction_objects(&mut self) {
        // objectsのうしろにいるfreeを解放
        while self.free_object_indices.remove(&(self.objects.len() - 1)) {
            self.objects.pop();
        }

        self.objects.shrink_to_fit();
    }

    pub fn object(&self, id: ObjectID) -> &Object {
        &self.objects[id.into_array_index()]
    }

    pub fn object_is_selected(&self, id: ObjectID) -> bool {
        self.selected_objects.contains(&id)
    }
}
pub struct ApplicationMutation<'a> {
    state: &'a mut Application,
    view_feedbacks: &'a mut VecDeque<ViewFeedback>,
}
impl core::ops::Deref for ApplicationMutation<'_> {
    type Target = Application;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.state
    }
}
impl ApplicationMutation<'_> {
    pub fn object_create(&mut self, name: String) -> ObjectID {
        let id = self.state.alloc_object(Object::new(name));
        self.view_feedbacks
            .push_back(ViewFeedback::object_tree_changed());
        id
    }

    pub fn object_destroy(&mut self, id: ObjectID) {
        self.state.free_object(id);
        self.view_feedbacks
            .push_back(ViewFeedback::object_tree_changed());
    }

    pub fn object_set_parent(&mut self, id: ObjectID, parent: ObjectID) {
        match self.state.objects[id.into_array_index()]
            .parent
            .replace(parent)
        {
            None => {
                // detach from root
                self.state.root_objects.retain(|&oid| oid != id);
            }
            Some(old_parent) if old_parent == parent => {
                // already linked
                return;
            }
            Some(old_parent) => {
                // detach from old parent
                self.state.objects[old_parent.into_array_index()]
                    .children
                    .retain(|&oid| oid != id);
            }
        }

        self.state.objects[parent.into_array_index()]
            .children
            .push(id);
        self.view_feedbacks
            .push_back(ViewFeedback::object_tree_changed());
    }

    pub fn object_detach_parent(&mut self, child: ObjectID) {
        let Some(parent) = self.state.objects[child.into_array_index()].parent.take() else {
            // already on root
            return;
        };

        self.state.objects[parent.into_array_index()]
            .children
            .retain(|&id| id != child);
        self.state.root_objects.push(parent);

        self.view_feedbacks
            .push_back(ViewFeedback::object_tree_changed());
    }

    pub fn object_modify_data(&mut self, id: ObjectID, updater: impl FnOnce(&mut Object)) {
        updater(&mut self.state.objects[id.into_array_index()]);
        self.view_feedbacks
            .push_back(ViewFeedback::object_data_changed(id));
    }

    pub fn select_object(&mut self, id: ObjectID) {
        if self.state.selected_objects.len() == 1
            && self
                .state
                .selected_objects
                .iter()
                .next()
                .is_some_and(|&x| x == id)
        {
            // already selected
            return;
        }

        self.state.selected_objects.clear();
        self.state.selected_objects.insert(id);
        self.view_feedbacks
            .push_back(ViewFeedback::object_selection_changed());
    }

    pub fn toggle_object_selection_additive(&mut self, id: ObjectID) {
        if !self.state.selected_objects.insert(id) {
            // selecting
            self.state.selected_objects.remove(&id);
        }

        self.view_feedbacks
            .push_back(ViewFeedback::object_selection_changed());
    }

    pub fn clear_selection(&mut self) {
        if self.state.selected_objects.is_empty() {
            // already cleared
            return;
        }

        self.state.selected_objects.clear();
        self.view_feedbacks
            .push_back(ViewFeedback::object_selection_changed());
    }
}

struct PerWindowData {
    screen_reposition_interests: HashSet<HitTestTreeRef>,
    header: ui::window_header::View,
    appmenu: Option<ui::app_menu_bar::View>,
    footer: Option<ui::window_footer::View>,
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

struct LaunchArgs<'sys> {
    pub event_queue: EventQueue,
    pub global_time_base: &'sys std::time::Instant,
    pub renderer_sync: &'sys Mutex<RendererSync>,
    pub file_system: &'sys FileSystem,
    pub committed_preview_state: &'sys Mutex<rendering::preview::CommittedState>,
}

crate::perf_section!(INITIALIZE = "LogicFiber.Initialize");
crate::perf_section!(PROCESS_EVENT = "LogicFiber.ProcessEvent");
crate::perf_section!(LOCK_WAIT = "Mutex.LockWait");

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
    crate::perf_begin!(perf = INITIALIZE);

    let mut application = Application::new();
    let mut view_feedback_store = VecDeque::new();
    let mut view_feedback_registry_delayed_ops = VecDeque::new();

    let mut composite_tree = CompositeTree::new();
    let mut ht_manager = HitTestTreeManager::new();
    let mut keyboard_focus_registry = KeyboardFocusTokenRegistry::new();
    let mut pointer_input_manager = PointerInputManager::new();
    let mut view_registry = ViewRegistry::new();
    let mut view_feedback_registry = ViewFeedbackRegistry::new();
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

    let mut preview_input_state = PreviewInputState {
        new_viewport_size: None,
        scroll_amount: 0.0,
        grabbing: false,
        grab_delta: Point::new_logical(0.0, 0.0),
        key_input: PreviewKeyInputState::empty(),
    };
    // preview local states
    let mut preview_latched_key_motion_amplifier = None::<f32>;

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
        view_registry: &mut view_registry,
        view_feedback_subscription_delayed_ops: &mut view_feedback_registry_delayed_ops,
        ui_scale_factor: main_window.ui_scale_factor(),
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

    let window_header_view = ui::window_header::View::new(
        &mut view_init_ctx,
        ui::window_header::Caption::Main {
            project_name: "New Project".into(),
        },
        main_window.needs_system_command_buttons(),
    );
    window_header_view.mount(&mut view_init_ctx, &main_window);

    let app_menu_view = if system_link.needs_app_menu_in_surface() {
        let app_menu_view = ui::app_menu_bar::View::new(
            &mut view_init_ctx,
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
        );
        app_menu_view.mount(&mut view_init_ctx, &main_window);
        Some(app_menu_view)
    } else {
        None
    };

    let window_footer_view = ui::window_footer::View::new(&mut view_init_ctx);
    window_footer_view.mount(&mut view_init_ctx, &main_window);

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
                    InspectorPanePresenter::ID.into(),
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
                        content_ids: vec![ObjectTreePanePresenter::ID.into()],
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
        header: window_header_view,
        appmenu: app_menu_view,
        footer: Some(window_footer_view),
        docking_manager: ui::dock::DockingManager::new(
            main_window,
            &mut view_init_ctx,
            Rect::from_lt_size(
                Point::new_logical(0.0, dock_top_offset),
                Size::new_logical(
                    main_window_size.width,
                    main_window_size.height - dock_top_offset - ui::window_footer::View::THICKNESS,
                ),
            ),
            &mut dock_store,
            |view_init_ctx, store| {
                match last_window_state {
                    None => &initial_dock_state,
                    Some(ref x) => &x.main.dock,
                }
                .construct(view_init_ctx, store, |id, view_init_ctx| match id {
                    // TODO: このへんうまい具合にRegistryつくりたい
                    UIKitPreviewPanePresenter::ID => {
                        Box::new(UIKitPreviewPanePresenter::new(view_init_ctx))
                    }
                    ObjectTreePanePresenter::ID => {
                        Box::new(ObjectTreePanePresenter::new(view_init_ctx))
                    }
                    InspectorPanePresenter::ID => {
                        Box::new(InspectorPanePresenter::new(view_init_ctx))
                    }
                    AssetExplorerPanePresenter::ID => Box::new(AssetExplorerPanePresenter {}),
                    ProjectSettingsPanePresenter::ID => Box::new(ProjectSettingsPanePresenter {}),
                    TimelinePanePresenter::ID => Box::new(TimelinePanePresenter {}),
                    AssetPreviewPanePresenter::ID => Box::new(AssetPreviewPanePresenter {}),
                    PreviewPanePresenter::ID => Box::new(PreviewPanePresenter::new(
                        view_init_ctx,
                        &mut preview_input_state,
                    )),
                    id => todo!("generic pane id handling: {id:?}"),
                })
            },
        ),
    }));

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
                        view_registry: &mut view_registry,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        ui_scale_factor: w.ui_scale_factor(),
                        system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    };
                    let window_header_view = ui::window_header::View::new(
                        &mut view_init_ctx,
                        ui::window_header::Caption::Sub,
                        w.needs_system_command_buttons(),
                    );
                    window_header_view.mount(&mut view_init_ctx, &w);

                    w.associate_extra_data(Box::new(PerWindowData {
                        screen_reposition_interests: HashSet::new(),
                        header: window_header_view,
                        appmenu: None,
                        footer: None,
                        docking_manager: ui::dock::DockingManager::new(
                            w,
                            &mut view_init_ctx,
                            Rect::from_lt_size(
                                Point::new_logical(0.0, ui::window_header::View::THICKNESS),
                                Size::new_logical(320.0, 240.0),
                            ),
                            &mut dock_store,
                            |view_init_ctx, store| {
                                sub.dock
                                    .construct(view_init_ctx, store, |id, view_init_ctx| match id {
                                        // TODO: このへんうまい具合にRegistryつくりたい
                                        UIKitPreviewPanePresenter::ID => {
                                            Box::new(UIKitPreviewPanePresenter::new(view_init_ctx))
                                        }
                                        ObjectTreePanePresenter::ID => {
                                            Box::new(ObjectTreePanePresenter::new(view_init_ctx))
                                        }
                                        InspectorPanePresenter::ID => {
                                            Box::new(InspectorPanePresenter::new(view_init_ctx))
                                        }
                                        AssetExplorerPanePresenter::ID => {
                                            Box::new(AssetExplorerPanePresenter {})
                                        }
                                        ProjectSettingsPanePresenter::ID => {
                                            Box::new(ProjectSettingsPanePresenter {})
                                        }
                                        TimelinePanePresenter::ID => {
                                            Box::new(TimelinePanePresenter {})
                                        }
                                        AssetPreviewPanePresenter::ID => {
                                            Box::new(AssetPreviewPanePresenter {})
                                        }
                                        PreviewPanePresenter::ID => {
                                            Box::new(PreviewPanePresenter::new(
                                                view_init_ctx,
                                                &mut preview_input_state,
                                            ))
                                        }
                                        id => todo!("generic pane id handling: {id:?}"),
                                    })
                            },
                        ),
                    }));
                },
            );
            sub_windows.insert(new_window);
        }
    }

    composite_tree.commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
    ht_manager.dump(main_window.ht_root());
    for msg in delayed_render_messages.drain(..) {
        system_link.rt_sender().send(msg).expect("rt_sender.send");
    }
    view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);

    // initial push test
    let mut preview_state =
        crate::perf_wrap!(LOCK_WAIT, committed_preview_state.lock().expect("poisoned"));
    let mut vbuf_bytes = vec![0u8; size_of::<[peridot_math::Vector4F32; 2]>() * 24];
    let mut ibuf_bytes = vec![0u8; size_of::<u16>() * 36];
    unsafe {
        const VERTICES: &[[peridot_math::Vector4F32; 2]] = &[
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
        const INDICES: &[u16] = &[
            0, 1, 2, 2, 1, 3, // +x
            4, 5, 6, 6, 5, 7, // -x
            8, 9, 10, 10, 9, 11, // +y
            12, 13, 14, 14, 13, 15, // -y
            16, 17, 18, 18, 17, 19, // +z
            20, 21, 22, 22, 21, 23, // -z
        ];

        vbuf_bytes
            .as_mut_ptr()
            .cast::<[peridot_math::Vector4F32; 2]>()
            .copy_from_nonoverlapping(VERTICES.as_ptr(), VERTICES.len());
        ibuf_bytes
            .as_mut_ptr()
            .cast::<u16>()
            .copy_from_nonoverlapping(INDICES.as_ptr(), INDICES.len());
    }
    preview_state
        .pushed_meshes
        .push(rendering::preview::CommittedMeshData {
            vertices: std::sync::Arc::from(vbuf_bytes),
            vertex_stride: size_of::<[peridot_math::Vector4F32; 2]>(),
            indices: std::sync::Arc::from(ibuf_bytes),
            index_type: rendering::preview::IndexType::U16,
            sub_mesh_ranges: std::sync::Arc::new([core::range::Range::from(0..36)]),
        });
    preview_state
        .pushed_render_data
        .push(rendering::preview::CommittedRenderData {
            object_to_world: peridot_math::Matrix4F32::ONE,
            mesh_id: 0,
        });
    drop(preview_state);

    system_link.prelaunch(main_window);
    crate::perf_end!(perf);
    loop {
        let e = event_queue.next_event().await;
        tracing::trace!(target: "event-trace", event = ?e);
        crate::perf_scope!(PROCESS_EVENT, str e.p_name());
        match e {
            Event::Quit => break,
            Event::SubWindowClose { mut window } => {
                unsafe {
                    drop(window.take_extra_data::<PerWindowData>());
                }
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
                    &mut composite_tree,
                    &mut ht_manager,
                );

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                        a.on_close_all(
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
                    c.terminate(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::WindowRescaleUI { window, new_scale } => {
                popup_manager.rescale(window, new_scale, &mut composite_tree);

                let mut renderer_sync = renderer_sync.lock().expect("poisoned");
                composite_tree.commit(&mut renderer_sync.composite_buffer);
            }
            Event::WindowMaximizeStateChanged {
                window,
                is_maximized,
            } => unsafe {
                window
                    .extra_data_ref::<PerWindowData>()
                    .header
                    .set_maximize_state(is_maximized, &mut composite_tree, &mut ht_manager);
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
                        a.on_close_all(
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

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::WindowActivatingStateChanged { window, activated } => {
                if !activated {
                    if let Some(c) = current_active_menu_session.take_if(|x| x.parent == window) {
                        if let Some(ref a) =
                            unsafe { window.extra_data_ref::<PerWindowData>() }.appmenu
                        {
                            a.on_close_all(
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
                    a.on_close_all(
                        &mut composite_tree,
                        global_time_base.elapsed().as_secs_f32(),
                    );
                }

                if let Some(c) = current_active_menu_session.take() {
                    if let Some(ref a) =
                        unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu
                    {
                        a.on_close_all(
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
                    c.terminate(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );
                }

                pointer_input_manager.handle_mouse_down(
                    pointer_id,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        ht_manager: &ht_manager,
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

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    window.ht_root(),
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);

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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    button,
                    key_modifier,
                    window.ht_root(),
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::PointerHover => {
                system_link.kill_pointer_hovering_timeout();
                pointer_input_manager.handle_pointer_hover(&mut InputEventContext {
                    composite_tree: &mut composite_tree,
                    current_sec: global_time_base.elapsed().as_secs_f32(),
                    system_link: &mut system_link,
                    ht_manager: &ht_manager,
                    application: ApplicationMutation {
                        state: &mut application,
                        view_feedbacks: &mut view_feedback_store,
                    },
                });

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: 1.0, // TODO: これどうするか...
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // TODO: これどうするか...
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: 1.0, // TODO: これどうするか...
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // TODO: これどうするか...
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                            application: ApplicationMutation {
                                state: &mut application,
                                view_feedbacks: &mut view_feedback_store,
                            },
                        },
                        &keyboard_focus_registry,
                    );

                    if !view_feedback_store.is_empty() {
                        let fb_time = global_time_base.elapsed().as_secs_f32();
                        for x in view_feedback_store.drain(..) {
                            x.dispatch(
                                &view_feedback_registry,
                                &mut ViewFeedbackContext {
                                    application: &application,
                                    view_init_context: ViewInitContext {
                                        mount_context: MountContext {
                                            composite_tree: &mut composite_tree,
                                            ht_manager: &mut ht_manager,
                                            current_sec: fb_time,
                                            keyboard_focus_registry: &mut keyboard_focus_registry,
                                        },
                                        view_registry: &mut view_registry,
                                        view_feedback_subscription_delayed_ops:
                                            &mut view_feedback_registry_delayed_ops,
                                        ui_scale_factor: window.ui_scale_factor(),
                                        system_link: &system_link,
                                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                                        application: &application,
                                    },
                                },
                            );
                        }

                        view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                            application: &application,
                            view_init_context: ViewInitContext {
                                mount_context: MountContext {
                                    composite_tree: &mut composite_tree,
                                    ht_manager: &mut ht_manager,
                                    current_sec: fb_time,
                                    keyboard_focus_registry: &mut keyboard_focus_registry,
                                },
                                view_registry: &mut view_registry,
                                view_feedback_subscription_delayed_ops:
                                    &mut view_feedback_registry_delayed_ops,
                                ui_scale_factor: window.ui_scale_factor(),
                                system_link: &system_link,
                                main_thread_texture_id_issuer: &mut texture_id_issuer,
                                application: &application,
                            },
                        });
                    }
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                    view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    &keyboard_focus_registry,
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    &keyboard_focus_registry,
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    &keyboard_focus_registry,
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    &keyboard_focus_registry,
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: window.ui_scale_factor(),
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: window.ui_scale_factor(),
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::OpenAlertDialog {
                target_window,
                message,
            } => {
                let mut view_feedback_registry_delayed_ops = VecDeque::new();
                let opened_id = popup_manager.open(
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_registry: &mut view_registry,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        ui_scale_factor: target_window.ui_scale_factor(),
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    &keyboard_focus_registry,
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: 1.0, // TODO: これどうするか...
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // TODO: これどうするか...
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::PopupClose { id } => {
                if popup_manager.close(
                    id,
                    &mut RenderContext {
                        composite_tree: &mut composite_tree,
                        ht_manager: &mut ht_manager,
                        keyboard_focus_registry: &mut keyboard_focus_registry,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                    },
                ) {
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }
            }
            Event::Sync(SyncEvent::PopupUnmount { id }) => {
                if popup_manager.teardown(
                    &mut TeardownContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_registry: &mut view_registry,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                    },
                    id,
                ) {
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }
            }
            Event::OpenCustomViewFlyout {
                parent,
                surface_pos,
                view_constructor,
            } => {
                let mut view_feedback_registry_delayed_ops = VecDeque::new();
                custom_view_flyout_session = Some(CustomViewFlyoutSession::new(
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
                        view_registry: &mut view_registry,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        ui_scale_factor: parent.ui_scale_factor(),
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    &mut delayed_render_messages,
                ));

                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::MenuOpen {
                parent,
                items,
                surface_pos,
            } => {
                let mut view_feedback_registry_delayed_ops = VecDeque::new();
                current_active_menu_session = Some(MenuSession::new(
                    parent,
                    items,
                    &system_link,
                    surface_pos,
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_registry: &mut view_registry,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        ui_scale_factor: 1.0, // updated later
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    &mut delayed_render_messages,
                    &context_menu_common_resources,
                ));

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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

                let mut view_feedback_registry_delayed_ops = VecDeque::new();
                current_active_menu_session = Some(MenuSession::new(
                    parent,
                    items,
                    &system_link,
                    surface_pos,
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_registry: &mut view_registry,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        ui_scale_factor: 1.0, // updated later
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    &mut delayed_render_messages,
                    &context_menu_common_resources,
                ));

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::DropdownMenuOpen {
                parent,
                surface_pos,
                min_width,
                items,
                selection_receiver,
            } => {
                let mut view_feedback_registry_delayed_ops = VecDeque::new();
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
                        view_registry: &mut view_registry,
                        view_feedback_subscription_delayed_ops:
                            &mut view_feedback_registry_delayed_ops,
                        ui_scale_factor: 1.0, // updated later
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                        application: &application,
                    },
                    &mut delayed_render_messages,
                    surface_pos,
                    min_width,
                    items,
                ));

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::MenuCloseAll => {
                if let Some(c) = current_active_menu_session.take() {
                    if let Some(ref a) =
                        unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu
                    {
                        a.on_close_all(
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
            }
            Event::MenuRescale { scale } => {
                let mut should_commit_ct = false;

                if let Some(ref c) = custom_view_flyout_session {
                    c.rescale(scale, &mut composite_tree, &ht_manager, &system_link);
                    should_commit_ct = true;
                }

                if should_commit_ct {
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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

                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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

                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                    let mut view_feedback_registry_delayed_ops = VecDeque::new();
                    c.perform_delayed_action(
                        &system_link,
                        &mut ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // updated later
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                        &mut delayed_render_messages,
                        &context_menu_common_resources,
                    );

                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                    view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: 1.0, // TODO: これどうするか...
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // TODO: これどうするか...
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    target.ht_root(),
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: 1.0, // TODO: これどうするか...
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // TODO: これどうするか...
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);

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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                    button,
                    key_modifier,
                    target.ht_root(),
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: 1.0, // TODO: これどうするか...
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // TODO: これどうするか...
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
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
                        application: ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        },
                    },
                );

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: 1.0, // TODO: これどうするか...
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // TODO: これどうするか...
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::MenuSelectCommand { id } => {
                tracing::debug!(id, "ContextMenuSelectCommand");

                // コマンド選択したらとじる
                if let Some(c) = current_active_menu_session.take() {
                    if let Some(ref a) =
                        unsafe { c.parent.extra_data_ref::<PerWindowData>() }.appmenu
                    {
                        a.on_close_all(
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
                    MENU_COMMAND_ID_OBJECT_CREATE_CUBE => {
                        ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        }
                        .object_create("New Cube".into());
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_SPHERE => {
                        ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        }
                        .object_create("New Sphere".into());
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CYLINDER => {
                        ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        }
                        .object_create("New Cylinder".into());
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_CAPSULE => {
                        ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        }
                        .object_create("New Capsule".into());
                    }
                    MENU_COMMAND_ID_OBJECT_CREATE_SP_TERRAIN => {
                        ApplicationMutation {
                            state: &mut application,
                            view_feedbacks: &mut view_feedback_store,
                        }
                        .object_create("New Terrain".into());
                    }
                    _ => (),
                }

                if !view_feedback_store.is_empty() {
                    let fb_time = global_time_base.elapsed().as_secs_f32();
                    for x in view_feedback_store.drain(..) {
                        x.dispatch(
                            &view_feedback_registry,
                            &mut ViewFeedbackContext {
                                application: &application,
                                view_init_context: ViewInitContext {
                                    mount_context: MountContext {
                                        composite_tree: &mut composite_tree,
                                        ht_manager: &mut ht_manager,
                                        current_sec: fb_time,
                                        keyboard_focus_registry: &mut keyboard_focus_registry,
                                    },
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: 1.0, // TODO: これどうするか...
                                    system_link: &system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                },
                            },
                        );
                    }

                    view_feedback_registry.perform_atomic(&mut ViewFeedbackContext {
                        application: &application,
                        view_init_context: ViewInitContext {
                            mount_context: MountContext {
                                composite_tree: &mut composite_tree,
                                ht_manager: &mut ht_manager,
                                current_sec: fb_time,
                                keyboard_focus_registry: &mut keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            view_feedback_subscription_delayed_ops:
                                &mut view_feedback_registry_delayed_ops,
                            ui_scale_factor: 1.0, // TODO: これどうするか...
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                            application: &application,
                        },
                    });
                }

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
            }
            Event::DropdownMenuSelectItem { id, receiver } => {
                let mut should_commit_ct = false;

                if let Some(r) = receiver.upgrade() {
                    r.set_selection_id(id, &mut composite_tree);
                    should_commit_ct = true;
                }

                // 選択したら閉じる
                if let Some(mut c) = current_active_dropdown_menu_session.take() {
                    c.close_all(
                        &system_link,
                        &mut composite_tree,
                        &mut ht_manager,
                        &mut keyboard_focus_registry,
                    );

                    should_commit_ct = true;
                }

                if should_commit_ct {
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }
            }
            Event::UpdateView { id } => {
                view_registry.call_update(
                    id,
                    &mut ViewUpdateContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                        },
                        system_link: &system_link,
                    },
                );

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::DockMoveSplitter {
                controlling_dock,
                pos_client,
            } => {
                ui::dock::move_splitter(
                    controlling_dock,
                    &mut dock_store,
                    pos_client,
                    &mut composite_tree,
                    &mut ht_manager,
                );

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                    let mount_target = destination_window;
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
                                view_registry: &mut view_registry,
                                view_feedback_subscription_delayed_ops:
                                    &mut view_feedback_registry_delayed_ops,
                                ui_scale_factor: 1.0, // updated later
                                system_link: &system_link,
                                main_thread_texture_id_issuer: &mut texture_id_issuer,
                                application: &application,
                            },
                        },
                        &mount_target,
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
                                    view_registry: &mut view_registry,
                                    view_feedback_subscription_delayed_ops:
                                        &mut view_feedback_registry_delayed_ops,
                                    ui_scale_factor: w.ui_scale_factor(),
                                    system_link,
                                    main_thread_texture_id_issuer: &mut texture_id_issuer,
                                    application: &application,
                                };
                                let window_header_view = ui::window_header::View::new(
                                    &mut view_init_ctx,
                                    ui::window_header::Caption::Sub,
                                    w.needs_system_command_buttons(),
                                );
                                window_header_view.mount(&mut view_init_ctx, &w);

                                w.associate_extra_data(Box::new(PerWindowData {
                                    screen_reposition_interests: HashSet::new(),
                                    header: window_header_view,
                                    appmenu: None,
                                    footer: None,
                                    docking_manager: ui::dock::DockingManager::new(
                                        w,
                                        &mut view_init_ctx,
                                        Rect::from_lt_size(
                                            Point::new_logical(
                                                0.0,
                                                ui::window_header::View::THICKNESS,
                                            ),
                                            suggested_rect.size(),
                                        ),
                                        &mut dock_store,
                                        |view_init_ctx, store| {
                                            store.alloc_root(|root_id, store| {
                                                store.alloc_fill(
                                                    root_id,
                                                    view_init_ctx,
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

                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                    view_feedback_registry.perform_delayed(&mut view_feedback_registry_delayed_ops);
                }
            }
            Event::Sync(SyncEvent::NewPresentID { id }) => {
                // vsync update period
                let mut preview_state =
                    crate::perf_wrap!(LOCK_WAIT, committed_preview_state.lock().expect("poisoned"));

                if let Some(new_viewport_size) = preview_input_state.new_viewport_size.take() {
                    preview_state.viewport_size = new_viewport_size;
                }

                let scroll_amount = core::mem::replace(&mut preview_input_state.scroll_amount, 0.0);
                let grab_delta = core::mem::replace(
                    &mut preview_input_state.grab_delta,
                    Point::new_logical(0.0, 0.0),
                );

                if grab_delta.x != 0.0 || grab_delta.y != 0.0 {
                    // rotate by grab
                    preview_state.main_camera.rotation = preview_state.main_camera.rotation
                        * peridot_math::Quaternion::new(
                            grab_delta.y * 0.5f32.to_radians(),
                            peridot_math::Matrix3::from(preview_state.main_camera.rotation)
                                * peridot_math::Vector3::left(),
                        )
                        * peridot_math::Quaternion::new(
                            grab_delta.x * 0.5f32.to_radians(),
                            peridot_math::Vector3::down(),
                        );
                    preview_state.main_camera_dirtified = true;
                }

                if scroll_amount != 0.0 {
                    // move by scroll
                    let amplifier = 5.0f32.powf(if preview_state.main_camera.position.1 == 0.0 {
                        0.0
                    } else {
                        preview_state.main_camera.position.1.abs().log10().floor()
                    });
                    preview_state.main_camera.position = preview_state.main_camera.position
                        + preview_state.main_camera.forward() * 0.25 * amplifier * scroll_amount;
                    preview_state.main_camera_dirtified = true;
                }

                if preview_input_state.grabbing {
                    let mut key_forwards = 0.0f32;
                    let mut key_rights = 0.0f32;
                    let mut key_y_motions = 0.0f32;
                    if preview_input_state
                        .key_input
                        .contains(PreviewKeyInputState::W)
                    {
                        key_forwards += 1.0;
                    }
                    if preview_input_state
                        .key_input
                        .contains(PreviewKeyInputState::S)
                    {
                        key_forwards -= 1.0;
                    }
                    if preview_input_state
                        .key_input
                        .contains(PreviewKeyInputState::D)
                    {
                        key_rights += 1.0;
                    }
                    if preview_input_state
                        .key_input
                        .contains(PreviewKeyInputState::A)
                    {
                        key_rights -= 1.0;
                    }
                    if preview_input_state
                        .key_input
                        .contains(PreviewKeyInputState::SHIFT)
                    {
                        key_y_motions += 1.0;
                    }
                    if preview_input_state
                        .key_input
                        .contains(PreviewKeyInputState::CONTROL)
                    {
                        key_y_motions -= 1.0;
                    }

                    if key_forwards != 0.0 || key_rights != 0.0 || key_y_motions != 0.0 {
                        // move by key
                        let amplifier =
                            *preview_latched_key_motion_amplifier.get_or_insert_with(|| {
                                2.5f32.powf(if preview_state.main_camera.position.1 == 0.0 {
                                    0.0
                                } else {
                                    preview_state.main_camera.position.1.abs().log10().floor()
                                })
                            });
                        preview_state.main_camera.position = preview_state.main_camera.position
                            + preview_state.main_camera.forward()
                                * (0.25 * amplifier * key_forwards)
                            + preview_state.main_camera.right() * (0.25 * amplifier * key_rights)
                            + peridot_math::Vector3(0.0, key_y_motions * 0.25 * amplifier, 0.0);
                        preview_state.main_camera_dirtified = true;
                    } else {
                        preview_latched_key_motion_amplifier = None;
                    }
                } else {
                    preview_latched_key_motion_amplifier = None;
                }
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

pub trait FlyoutSurfaceView {
    fn render(&mut self, ctx: &mut RenderContext, surface: FlyoutSurfaceHandle);

    #[allow(unused_variables)]
    fn rescale(
        &self,
        new_scale: f32,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &HitTestTreeManager,
        system_link: &SystemLink,
    ) {
    }
}
pub trait FlyoutSurfaceViewConstructor {
    fn size(&self) -> Size<LogicalUnit>;
    fn create(&self, view_init_context: &mut ViewInitContext) -> Box<dyn FlyoutSurfaceView>;
}
pub struct CustomViewFlyoutSurface {
    native_surface: FlyoutSurfaceHandle,
    view: Box<dyn FlyoutSurfaceView>,
}
pub struct CustomViewFlyoutSession {
    parent: WindowHandle,
    opening_surface: CustomViewFlyoutSurface,
}
impl CustomViewFlyoutSession {
    pub fn new(
        parent: WindowHandle,
        pos: Point<LogicalUnit>,
        view_constructor: Box<dyn FlyoutSurfaceViewConstructor>,
        view_init_context: &mut ViewInitContext,
        delayed_render_messages: &mut Vec<RenderMessage>,
    ) -> Self {
        let surface = view_init_context.system_link.new_flyout_surface(
            parent,
            pos,
            view_constructor.size(),
            view_init_context.mount_context.composite_tree,
            view_init_context.mount_context.ht_manager,
            view_init_context.mount_context.keyboard_focus_registry,
            delayed_render_messages,
        );
        view_init_context.ui_scale_factor = surface.render_scale();
        let mut view = view_constructor.create(view_init_context);
        view.render(&mut view_init_context.make_render_context(), surface);

        Self {
            parent,
            opening_surface: CustomViewFlyoutSurface {
                native_surface: surface,
                view,
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
            .view
            .rescale(new_scale, composite_tree, ht_manager, system_link);
    }

    pub fn terminate(
        self,
        syslink: &SystemLink,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        self.opening_surface.native_surface.close(
            syslink,
            composite_tree,
            ht_manager,
            keyboard_focus_registry,
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
        view_init_context.ui_scale_factor = root_surface.render_scale();

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
    item_views: Vec<MenuItemView>,
    _base_event_handler: Rc<MenuBaseSurfaceEventHandler>,
    parent_path: Vec<usize>,
    current_selecting: Option<usize>,
}
impl MenuSurface {
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
            self.item_views[x].unlit(composite_tree, current_sec);
        }

        self.current_selecting = Some(new_index);
        self.item_views[new_index].lit(composite_tree, current_sec);
    }

    pub fn deselect(&mut self, composite_tree: &mut CompositeTree<SyncEvent>, current_sec: f32) {
        if let Some(x) = self.current_selecting {
            self.item_views[x].unlit(composite_tree, current_sec);
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
        system_link: &SystemLink,
        surface_pos: Point<LogicalUnit>,
        view_init_context: &mut ViewInitContext,
        delayed_render_messages: &mut Vec<RenderMessage>,
        common_res: &MenuItemCommonResources,
    ) -> Self {
        #[cfg(target_os = "macos")]
        system_link.flyout_surface_context.observe_global_click();

        let (root_surface, base_event_handler, item_views) = system_link.pop_context_menu(
            parent,
            view_init_context,
            0,
            surface_pos,
            |render_scale| {
                crate::uikit::MenuItemLayout::build(items.iter().cloned(), system_link.font_set())
            },
            delayed_render_messages,
            |layout, h, view_init_ctx| {
                view_init_ctx.ui_scale_factor = h.render_scale();
                let views = crate::uikit::MenuItemLayout::instantiate(
                    layout.into_iter(),
                    0,
                    view_init_ctx,
                    common_res,
                );
                for x in views.iter() {
                    x.mount(view_init_ctx, &h);
                }

                views
            },
        );

        Self {
            parent,
            items,
            opening_surfaces: vec![MenuSurface {
                handle: root_surface,
                item_views,
                _base_event_handler: base_event_handler,
                parent_path: Vec::new(),
                current_selecting: None,
            }],
            active_selection: None,
        }
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
                while self.opening_surfaces.len() > depth + 1 {
                    self.opening_surfaces.pop().expect("empty?").handle.close(
                        system_link,
                        view_init_context.mount_context.composite_tree,
                        view_init_context.mount_context.ht_manager,
                        view_init_context.mount_context.keyboard_focus_registry,
                    );
                }
                let latest_surface = self.opening_surfaces.last().expect("root?");

                if let MenuItemView::SubMenu(ref submenu) = latest_surface.item_views[index] {
                    let pos = latest_surface.handle.submenu_pop_position(submenu);
                    let parent_path = latest_surface
                        .parent_path
                        .iter()
                        .copied()
                        .chain(core::iter::once(index))
                        .collect::<Vec<_>>();
                    let items = parent_path.iter().fold(&self.items[..], |haystack, &x| {
                        match haystack[x] {
                            MenuItem::SubMenu { ref items, .. } => items,
                            _ => unreachable!("invalid nesting"),
                        }
                    });

                    let (surface, base_event_handler, item_views) = system_link.pop_context_menu(
                        self.parent,
                        view_init_context,
                        depth + 1,
                        pos,
                        |render_scale| {
                            crate::uikit::MenuItemLayout::build(
                                items.into_iter().cloned(),
                                system_link.font_set(),
                            )
                        },
                        delayed_render_messages,
                        |layout, h, view_init_ctx| {
                            view_init_ctx.ui_scale_factor = h.render_scale();
                            let views = crate::uikit::MenuItemLayout::instantiate(
                                layout.into_iter(),
                                depth + 1,
                                view_init_ctx,
                                common_res,
                            );
                            for x in views.iter() {
                                x.mount(view_init_ctx, &h);
                            }

                            views
                        },
                    );

                    self.opening_surfaces.push(MenuSurface {
                        handle: surface,
                        item_views,
                        _base_event_handler: base_event_handler,
                        parent_path,
                        current_selecting: None,
                    });
                }
            }
            None => {
                // 最初のやつだけ表示する
                while self.opening_surfaces.len() > 1 {
                    self.opening_surfaces.pop().expect("empty?").handle.close(
                        system_link,
                        view_init_context.mount_context.composite_tree,
                        view_init_context.mount_context.ht_manager,
                        view_init_context.mount_context.keyboard_focus_registry,
                    );
                }
            }
        }
    }

    pub fn open_submenu(
        &mut self,
        depth: usize,
        index: usize,
        system_link: &SystemLink,
        view_init_context: &mut ViewInitContext,
        delayed_render_messages: &mut Vec<RenderMessage>,
        common_res: &MenuItemCommonResources,
    ) {
        while self.opening_surfaces.len() > depth + 1 {
            self.opening_surfaces.pop().expect("empty?").handle.close(
                system_link,
                view_init_context.mount_context.composite_tree,
                view_init_context.mount_context.ht_manager,
                view_init_context.mount_context.keyboard_focus_registry,
            );
        }

        let target_surface = self.opening_surfaces.last().expect("root?");
        let MenuItemView::SubMenu(ref view) = target_surface.item_views[index] else {
            panic!("not a submenu");
        };
        let display_pos = target_surface.handle.submenu_pop_position(view);
        let parent_path = target_surface
            .parent_path
            .iter()
            .copied()
            .chain(core::iter::once(index))
            .collect::<Vec<_>>();
        let items = parent_path
            .iter()
            .fold(&self.items[..], |haystack, &x| match haystack[x] {
                MenuItem::SubMenu { ref items, .. } => items,
                _ => unreachable!("invalid nesting"),
            });

        let (surface, base_event_handler, item_views) = system_link.pop_context_menu(
            self.parent,
            view_init_context,
            depth + 1,
            display_pos,
            |render_scale| {
                crate::uikit::MenuItemLayout::build(
                    items.into_iter().cloned(),
                    system_link.font_set(),
                )
            },
            delayed_render_messages,
            |layout, h, view_init_ctx| {
                view_init_ctx.ui_scale_factor = h.render_scale();
                let views = crate::uikit::MenuItemLayout::instantiate(
                    layout.into_iter(),
                    depth + 1,
                    view_init_ctx,
                    common_res,
                );
                for x in views.iter() {
                    x.mount(view_init_ctx, &h);
                }

                views
            },
        );

        self.opening_surfaces.push(MenuSurface {
            handle: surface,
            item_views,
            _base_event_handler: base_event_handler,
            parent_path,
            current_selecting: None,
        });
    }

    pub fn terminate(
        mut self,
        system_link: &SystemLink,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        tracing::debug!("context menu terminate");
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

pub enum MainWindowOpenMode {
    New,
    Restore(WindowGeometryState),
}

pub enum SubWindowOpenMode {
    DockDiverge {
        rect: Rect<LogicalUnit>,
        position_ref_window: WindowHandle,
    },
    Restore(WindowGeometryState),
}

#[cfg(windows)]
pub type SystemLink<'sys> = platform::windows::SystemLink<'sys>;

#[cfg(not(windows))]
pub struct SystemLink<'sys> {
    vk_device: *const Graphics<'sys>,
    rt_sender: RenderMessageSender,
    font_set: *const FontSet,
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
        unsafe { &*self.font_set }
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

    #[cfg(feature = "wayland")]
    pub fn pop_context_menu(
        &self,
        parent: WindowHandle,
        view_init_context: &mut ViewInitContext,
        depth: usize,
        surface_pos: Point<LogicalUnit>,
        layouted_items: impl FnOnce(f32) -> Vec<MenuItemLayout>,
        delayed_render_messages: &mut Vec<RenderMessage>,
        setup_contents: impl FnOnce(
            Vec<MenuItemLayout>,
            FlyoutSurfaceHandle,
            &mut ViewInitContext,
        ) -> Vec<MenuItemView>,
    ) -> (
        FlyoutSurfaceHandle,
        Rc<MenuBaseSurfaceEventHandler>,
        Vec<MenuItemView>,
    ) {
        let layouted_items = layouted_items(view_init_context.ui_scale_factor);
        let width = MenuItemLayout::min_width(layouted_items.iter());
        let height = MenuItemLayout::height(layouted_items.iter());
        tracing::debug!(%width, %height, "pop context menu");

        let handle = self.new_flyout_surface(
            parent,
            surface_pos,
            Size::new_logical(width.value(), height.value()),
            view_init_context.mount_context.composite_tree,
            view_init_context.mount_context.ht_manager,
            view_init_context.mount_context.keyboard_focus_registry,
            delayed_render_messages,
        );

        let base_surface_event_handler = Rc::new(MenuBaseSurfaceEventHandler::new(depth));
        view_init_context
            .ht_manager
            .set_action_handler(handle.ht_root(), &base_surface_event_handler);
        let views = setup_contents(layouted_items, handle, view_init_context);

        (handle, base_surface_event_handler, views)
    }
}

pub enum WindowType {
    Main {
        #[cfg(target_os = "linux")]
        termination_event: std::sync::Arc<linux_eventfd::EventFD>,
    },
    Sub,
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
    PointerID, WindowHandle, WindowPersistentStateNativeGeometryUnit,
    flyout_surface::Handle as FlyoutSurfaceHandle,
};

crate::perf_section!(SYNC_EVENT_BUS_PUSH = "SyncEventBus.Push");

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

    pub fn push(&self, e: SyncEvent) {
        crate::perf_scope!(SYNC_EVENT_BUS_PUSH);

        self.queue.lock().expect("poisoned").push_back(e);
        #[cfg(target_os = "linux")]
        self.efd.inc(1).unwrap();
        #[cfg(windows)]
        {
            self.event_notify.set().expect("event_notify.set");
        }
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
        {
            self.event_notify.reset().map_err(From::from)
        }
        #[cfg(target_os = "macos")]
        {
            // TODO
            Ok(())
        }
    }
}

// platform-dependent constants
pub const DRAG_PREVIEW_POPOVER_BG_COLOR: Color32 = Color32 {
    r: 16,
    g: 176,
    b: 255,
    a: 16,
};

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
        view_init_ctx: &mut ViewInitContext,
        store: &mut ui::dock::DockStore,
        mut pane_constructor: impl FnMut(
            &str,
            &mut ViewInitContext,
        ) -> Box<dyn ui::dock::PaneContentPresenter>,
    ) -> ui::dock::DockID {
        fn rec(
            this: &DockState,
            view_init_ctx: &mut ViewInitContext,
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
                    view_init_ctx,
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
                    splitter: ui::dock::DockedPaneSplitterView::new(
                        view_init_ctx,
                        match direction {
                            DockDirection::Left(_) | DockDirection::Right(_) => {
                                ui::dock::DockedPaneSplitDirection::Horizontal
                            }
                            DockDirection::Top(_) | DockDirection::Bottom(_) => {
                                ui::dock::DockedPaneSplitDirection::Vertical
                            }
                        },
                        parent1,
                    ),
                    docked: rec(content, view_init_ctx, store, parent1, pane_constructor),
                    rest: rec(rest, view_init_ctx, store, parent1, pane_constructor),
                }),
            }
        }

        store.alloc_root(|parent, store| {
            rec(self, view_init_ctx, store, parent, &mut pane_constructor)
        })
    }
}

trait PersistStateFormat: Sized {
    fn serialize(&self, w: &mut (impl std::io::Write + ?Sized)) -> std::io::Result<()>;
    fn deserialize(
        r: &mut (impl std::io::Read + ?Sized),
    ) -> Result<Self, PersistStateDeserializeError>;
}
impl PersistStateFormat for Rect<crate::utils::LogicalUnit> {
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
impl PersistStateFormat for Rect<crate::utils::PixelsUnit> {
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
                let rect = PersistStateFormat::deserialize(r)?;

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
            PathBuf::from(std::env::var_os("LOCALAPPDATA").expect("fs.appdata_base_path.no_env"))
                .join("peridot/.editor");
        #[cfg(windows)]
        let cache_base_path = appdata_base_path.join("cache");
        #[cfg(windows)]
        let persist_state_base_path = appdata_base_path.join("state");

        if let Err(e) = std::fs::create_dir_all(&cache_base_path) {
            tracing::error!(reason = %e, "fs.cache_base_path.create_dir_all");
        }

        if let Err(e) = std::fs::create_dir_all(&persist_state_base_path) {
            tracing::error!(reason = %e, "fs.persist_state_base_path.create_dir_all");
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
    grab_delta: Point<LogicalUnit>,
    key_input: PreviewKeyInputState,
}

pub struct PreviewPanePresenter {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    kf_token: FocusTargetToken,
    input_handler: Rc<PreviewInputHandler>,
}
impl PreviewPanePresenter {
    const ID: &str = internal_pane_identifier!("Preview");

    fn new(ctx: &mut ViewInitContext, input_state: *mut PreviewInputState) -> Self {
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

        let input_handler = Rc::new(PreviewInputHandler { input_state });
        ctx.ht_manager.set_action_handler(ht_root, &input_handler);
        ctx.keyboard_focus_registry
            .set_event_handler(kf_token, &input_handler);

        Self {
            ct_root,
            ht_root,
            kf_token,
            input_handler,
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

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {
        ctx.composite_tree.add_child(target.ct_root, self.ct_root);
        ctx.ht_manager.add_child(target.ht_root, self.ht_root);
    }

    fn resize(
        &self,
        new_size: &Size<LogicalUnit>,
        _composite_tree: &mut CompositeTree<SyncEvent>,
        _ht_manager: &mut HitTestTreeManager,
    ) {
        unsafe { &mut *self.input_handler.input_state }.new_viewport_size = Some(new_size.clone());
    }

    fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.ct_root);
        ctx.ht_manager.remove_child(self.ht_root);
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        ctx.mount_context.composite_tree.free(self.ct_root);
        ctx.mount_context.ht_manager.free(self.ht_root);
        ctx.mount_context
            .keyboard_focus_registry
            .release_token(self.kf_token);
    }
}

struct PreviewInputHandler {
    input_state: *mut PreviewInputState,
}
impl HitTestTreeActionHandler for PreviewInputHandler {
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

    fn on_pointer_down(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.grabbing = true;

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::GRAB_POINTER
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        unsafe { &mut *self.input_state }.grabbing = false;

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::RELEASE_CAPTURE_ELEMENT
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
}
impl KeyInputEventHandler for PreviewInputHandler {
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
impl PreviewInputHandler {
    fn set_key(&self, key: PreviewKeyInputState) {
        unsafe { &mut *self.input_state }.key_input.insert(key);
    }

    fn unset_key(&self, key: PreviewKeyInputState) {
        unsafe { &mut *self.input_state }.key_input.remove(key);
    }
}
