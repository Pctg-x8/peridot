use core::cell::Cell;
#[cfg(target_os = "linux")]
use linux_epoll::{Epoll, EpollEventBits};
#[cfg(feature = "wayland")]
use linux_eventfd::{EventFD, EventFDFlags};
#[cfg(target_os = "linux")]
use peridot_tp_dbus as dbus;
#[cfg(target_os = "linux")]
use std::os::fd::AsRawFd;
#[cfg(not(windows))]
#[cfg(target_os = "linux")]
use std::sync::Arc;
use std::{
    cell::RefCell,
    collections::{BTreeSet, HashSet, VecDeque},
    path::{Path, PathBuf},
    rc::Rc,
    sync::Mutex,
};
#[cfg(target_os = "macos")]
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use crate::{
    graphics::VulkanDevice,
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
        MainThreadTextureIDIssuer, Normalized2DStaticMeshTexture, RenderMessage,
        RenderMessageSender, RenderThread, RendererSync, ShaderTexture, TextureID,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectScaleFactor, CompositeRectText,
            CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTexture, CompositeTree, CompositeTreeRef,
            CompositeTreeSyncBuffer, CornerRadius, Gradient, GradientRef, TextureMappingMode,
            TextureType,
        },
        text::{FontID, FontSet, TextLayout},
    },
    uikit::{
        MenuBaseSurfaceEventHandler, MenuItem, MenuItemCommonResources, MenuItemLayout,
        MenuItemView, MountContext, MountTarget, NumericInputView, OverlayPopupBasicFrameView,
        OverlayPopupBasicMaskView, Popup, PopupID, PopupManager, Positioning, RawMountTarget,
        ScrollContainer, SimpleButtonConstantEventHandler, SimpleButtonEventHandler,
        SimpleButtonView, TextInputView, ViewEventHandler, ViewIdentifier, ViewInitContext,
        ViewRegistry, ViewUpdateContext,
    },
    utils::{
        Color32, DummyDebug, LogicalUnit, NonCloneable, Point, Rect, SafeF32, Size,
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
    let app_context = platform::windows::ApplicationContext::new();
    #[cfg(windows)]
    let dx_context = platform::windows::DxContext::new();

    #[cfg(feature = "wayland")]
    let mut dp_context = platform::unix::wayland::DisplayServerContext::connect();
    #[cfg(feature = "wayland")]
    let static_pixbufs = platform::unix::wayland::StaticPixbufs::new(&dp_context);

    #[cfg(target_os = "linux")]
    let dbus = dbus::Connection::connect_bus(dbus::BusType::Session).expect("dbus.connect");

    #[cfg(feature = "freetype")]
    let ft = crate::rendering::text::FreeType::init().expect("freetype.init");
    let root_font_set = FontSet::new(
        #[cfg(feature = "freetype")]
        &ft,
    );

    let vk_device = VulkanDevice::new(&fs);
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
        #[cfg(windows)]
        &app_context,
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
    vk_device: &'sys VulkanDevice,
    rt_sender: RenderMessageSender,
    rt_receiver: std::sync::mpsc::Receiver<RenderMessage>,
    root_font_set: FontSet,
    #[cfg(windows)] app_context: &'sys platform::windows::ApplicationContext,
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
        },
        #[cfg(windows)]
        SystemLink {
            font_set: &root_font_set,
            rt_sender: rt_sender.clone(),
            vk_device,
            event_dispatcher: app_event_dispatcher.as_mut().get_mut(),
            app_context_ptr: app_context,
            pointer_hovering_timer: pointer_hovering_timer.as_ref().get_ref(),
            flyout_surface_context: platform::windows::flyout_surface::SharedState::new(
                app_context,
                &dx_context,
                context_menu_delayed_action_timer.as_ref(),
            )
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
                global_messaging_ptr: wl_global_msg.as_ref().get_ref() as *const _,
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
            flyout_surface_context: platform::mac::context_menu::SharedState {
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
            vk_device,
            shutdown_signal: &shutdown,
            renderer_sync,
            global_time_base,
            event_bus: &sync_event_bus,
            message_receiver: rt_receiver,
            font_set: &root_font_set,
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
        let evdevs = (0..32)
            .filter_map(|x| {
                linux_input::EventDevice::open(
                    &std::ffi::CString::new(format!("/dev/input/event{x}")).expect("invalid str"),
                )
                .ok()
            })
            .collect::<Vec<_>>();
        #[cfg(target_os = "linux")]
        for (n, e) in evdevs.iter().enumerate() {
            epoll
                .add(e, EpollEventBits::IN, (10 + n) as _)
                .expect("epoll.add");
        }
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
            let mut global_mouse_clicked = false;
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
                if e.value() >= 10 && e.value() < 10 + 32 {
                    let ed = evdevs[(e.value() - 10) as usize]
                        .read()
                        .expect("evdev.read");
                    if ed.type_ == linux_input::EventType::Key as u16
                        && ed.value == 1
                        && ed.code >= linux_input::Key::MouseLeft as u16
                        && ed.code < linux_input::Key::Joystick as u16
                    {
                        // tracing::debug!("mouse button input");
                        global_mouse_clicked = true;
                    }
                    // tracing::debug!(n = e.value() - 10, ?ed, "evdev");
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

            if global_mouse_clicked {
                app_event_dispatcher.dispatch(Event::GlobalMouseClicked);
            }

            if dbus_signal {
                while let Some(m) = dbus.pop_message() {
                    let span = tracing::info_span!(target: "dbus::loop", "dbus message recv", r#type = ?m.r#type(), path = ?m.path(), interface = ?m.interface(), member = ?m.member());
                    let _enter = span.enter();
                    match m.r#type() {
                        dbus::MessageType::MethodCall
                            if m.path().is_some_and(|x| {
                                x == platform::unix::wayland::APPMENU_OBJECT_PATH
                            }) && m.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
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
                                                shortcut: Some(&[&[c"Alt", c"F4"]]),
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
                            if m.path().is_some_and(|x| {
                                x == platform::unix::wayland::APPMENU_OBJECT_PATH
                            }) && m.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
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
                            if m.path().is_some_and(|x| {
                                x == platform::unix::wayland::APPMENU_OBJECT_PATH
                            }) && m.interface() == Some(proto::dbus_menu::INTERFACE_NAME)
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
                    if msg.message == windows::Win32::UI::WindowsAndMessaging::WM_LBUTTONDOWN
                        || msg.message == windows::Win32::UI::WindowsAndMessaging::WM_RBUTTONDOWN
                        || msg.message == windows::Win32::UI::WindowsAndMessaging::WM_MBUTTONDOWN
                    {
                        app_event_dispatcher.dispatch(Event::GlobalMouseClicked);
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
    WindowPostCreateRenderBuffer { window: WindowHandle },
    FlyoutSurfacePostCreateRenderBuffer { target: FlyoutSurfaceHandle },
    PopupUnmount { id: PopupID },
}
impl SyncEvent {
    pub const fn p_name(&self) -> &'static str {
        match self {
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
        #[cfg(feature = "wayland")]
        event_id: platform::unix::wayland::PointerEventID,
    },
    PointerMove {
        pointer_id: PointerID,
        window: WindowHandle,
        client_pos: Point<PointerInputUnit>,
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
    IMEStateChanges {
        window: WindowHandle,
        committed_string: String,
        preedit_string: String,
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
    SubWindowOpen,
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
        #[cfg(feature = "wayland")]
        event_id: platform::unix::wayland::PointerEventID,
    },
    MenuPointerMove {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
        client_pos: Point<PointerInputUnit>,
    },
    MenuPointerUp {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
        button: PointerButton,
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
        window: WindowHandle,
        controlling_dock: DockID,
        pos_client: f32,
    },
    DockBeginPreview {
        window: WindowHandle,
        pane_rect: Rect<LogicalUnit>,
        client_pos: Point<LogicalUnit>,
    },
    DockMovePreview {
        window: WindowHandle,
        client_pos: Point<LogicalUnit>,
    },
    DockConfirm {
        window: WindowHandle,
        source_dock: DockID,
        tab_index: usize,
        client_pos: Point<LogicalUnit>,
    },
    #[cfg(not(target_os = "macos"))]
    GlobalMouseClicked,
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
            Self::IMEStateChanges { .. } => "IMEStateChanges",
            Self::WindowMove { .. } => "WindowMove",
            Self::WindowResize { .. } => "WindowResize",
            Self::WindowRescaleUI { .. } => "WindowRescaleUI",
            Self::WindowMaximizeStateChanged { .. } => "WindowMaximizeStateChanged",
            Self::WindowFocusChanged { .. } => "WindowFocusChanged",
            Self::WindowActivatingStateChanged { .. } => "WindowActivatingStateChanged",
            Self::SubWindowOpen => "SubWindowOpen",
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
            Self::GlobalMouseClicked => "GlobalMouseClicked",
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

pub struct AlertDialogPresenter {
    id: PopupID,
    mask: OverlayPopupBasicMaskView,
    frame: OverlayPopupBasicFrameView,
    ct_message: CompositeTreeRef,
    confirm_button: SimpleButtonView,
}
impl AlertDialogPresenter {
    const AROUND_PADDING: f32 = 16.0;
    const MESSAGE_BUTTON_SPACING: f32 = 12.0;

    pub fn new(
        ctx: &mut ViewInitContext,
        popup_id: PopupID,
        message: String,
        owner_window: WindowHandle,
    ) -> Self {
        let tl = TextLayout::new_single(
            &message,
            FontID::UIDefault,
            ctx.system_link.font_set(),
            CompositeRectTextHorizontalAlignment::Middle,
            Some(owner_window.client_size().width * 0.8),
        );
        let text_width = tl
            .visual_width(ctx.system_link.font_set())
            .max(64.0)
            .min(owner_window.client_size().width * 0.8);

        let mask = OverlayPopupBasicMaskView::new(ctx);
        let frame = OverlayPopupBasicFrameView::new(
            ctx,
            Size::new_logical(
                text_width + Self::AROUND_PADDING * 2.0,
                tl.height() + Self::MESSAGE_BUTTON_SPACING + 24.0 + Self::AROUND_PADDING * 2.0,
            ),
        );
        let confirm_button = SimpleButtonView::new(
            ctx,
            "OK".into(),
            Size::new_logical(64.0, 24.0),
            Some(Box::new(SimpleButtonConstantEventHandler(
                Event::PopupClose { id: popup_id },
            ))),
        );
        let ct_message = ctx.mount_context.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [
                AnimatableFloat::Value(text_width),
                AnimatableFloat::Value(16.0),
            ],
            relative_offset_adjustment: [0.5, 0.0],
            offset: [
                AnimatableFloat::Value(-text_width * 0.5),
                AnimatableFloat::Value(Self::AROUND_PADDING),
            ],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    font_id: FontID::UIDefault,
                    content: message,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    spacing_inline_start: 0.0,
                }],
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                vertical_alignment: CompositeRectTextVerticalAlignment::Start,
                allow_wrapping: true,
                ..Default::default()
            }),
            ..Default::default()
        });

        confirm_button.locate(
            &Positioning {
                parent_anchor: [0.5, 1.0],
                anchor: [0.5, 1.0],
                offset: [0.0, -Self::AROUND_PADDING],
            },
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );
        ctx.composite_tree.add_child(frame.ct_root(), ct_message);
        confirm_button.mount(ctx, &frame);
        frame.mount(ctx, &mask);

        Self {
            id: popup_id,
            mask,
            frame,
            ct_message,
            confirm_button,
        }
    }
}
impl Popup for AlertDialogPresenter {
    fn mount(&self, ctx: &mut MountContext, parent: &RawMountTarget) {
        self.mask.mount(ctx, parent);
        self.mask
            .play_open_animation(ctx.composite_tree, ctx.current_sec);
        self.frame
            .play_open_animation(ctx.composite_tree, ctx.current_sec);
    }

    fn set_keyboard_focus_group(
        &self,
        group: KeyboardFocusGroupRef,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        self.confirm_button
            .set_keyboard_focus_group(group, keyboard_focus_registry);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        self.mask.unmount(ctx);
    }

    fn close(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        current_sec: f32,
    ) {
        // disable button interaction while animating
        self.confirm_button.set_interactive(false, ht_manager);

        self.mask.play_close_animation(composite_tree, current_sec);
        self.frame.play_close_animation(
            composite_tree,
            current_sec,
            SyncEvent::PopupUnmount { id: self.id },
        );
    }

    fn terminate(&mut self, ctx: &mut MountContext) {
        self.confirm_button.unmount(ctx);
        self.confirm_button.terminate(ctx);

        ctx.composite_tree.free_all(self.mask.ct_root());
        ctx.ht_manager.free_all(self.mask.ht_root());
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
            start_sec: context.current_sec,
            end_sec: context.current_sec + 0.1,
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
            start_sec: context.current_sec,
            end_sec: context.current_sec + 0.1,
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
                start_sec: current_sec,
                end_sec: current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            };
        } else {
            composite_tree.get_mut(self.ct_mark).opacity = AnimatableFloat::Animated {
                from_value: 1.0,
                to_value: 0.0,
                start_sec: current_sec,
                end_sec: current_sec + 0.1,
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

        Self { eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);

        // これだけ遅延させる必要がある（ScreenPositionInterestsどうしようか......）
        self.eh.hex_text_input_view.mount(
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
        let kf_token = ctx.keyboard_focus_registry.acquire_token();
        let raw = uikit::RawTextInputView::new(
            ctx,
            rect,
            "00000000".into(),
            kf_token,
            uikit::RawTextInputViewCreateFlags::NON_DELEGATED_HT,
        );
        let eh = Rc::new(ColorPickerHexTextInputEventHandler {
            value: Cell::new(0),
            raw,
            id: ctx.view_registry.alloc(),
            token: kf_token,
            parent_view_handler: parent_view_handler.clone(),
        });
        ctx.keyboard_focus_registry.set_event_handler(kf_token, &eh);
        ctx.view_registry.set_event_handler(eh.id, &eh);

        Self { eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        self.eh.raw.mount(ctx, parent);
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
            .set_content_lazy(ColorPickerHexTextInputEventHandler::fmt(value));
        syslink.dispatch_event(Event::UpdateView { id: self.eh.id });
    }
}

struct ColorPickerHexTextInputEventHandler {
    value: Cell<u32>,
    raw: uikit::RawTextInputView,
    id: ViewIdentifier,
    token: FocusTargetToken,
    parent_view_handler: std::rc::Weak<ColorPickerEventHandler>,
}
impl ViewEventHandler for ColorPickerHexTextInputEventHandler {
    #[inline(always)]
    fn update(&self, context: &mut ViewUpdateContext) {
        self.raw.fwd_view_update(context);
    }
}
impl KeyInputEventHandler for ColorPickerHexTextInputEventHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        // HitTestTreeへの変更がはいるので遅延させる
        self.raw.set_focus_lazy();
        context
            .system_link
            .dispatch_event(Event::UpdateView { id: self.id });
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        self.raw.release_focus_lazy();
        self.confirm_direct_input(context.system_link, context.composite_tree);
    }

    #[inline(always)]
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

        self.raw.fwd_keydown(context, code, modifier);
    }

    #[inline(always)]
    #[cfg(feature = "wayland")]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: &str,
        new_preedit_string: &str,
    ) {
        self.raw
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
        let new_value = Self::parse(&self.raw.content()).unwrap_or(current_value);
        self.value.set(new_value);

        // HitTestTreeへの変更がはいるので遅延させる
        self.raw.set_content_lazy(Self::fmt(new_value));
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
        self.raw.set_content_lazy(Self::fmt(self.value.get()));
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
    fn mount(&self, mount_context: &mut MountContext, surface: FlyoutSurfaceHandle) {
        self.inner_view.mount(mount_context, &surface);
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

pub trait PaneView {
    fn name(&self) -> String;
    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget);
    fn unmount(&self, ctx: &mut MountContext);

    #[allow(unused_variables)]
    #[inline(always)]
    fn resize(
        &self,
        new_size: &Size<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
    }
}

pub struct TestPane1View {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl TestPane1View {
    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [AnimatableFloat::Value(128.0), AnimatableFloat::Value(128.0)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "test content1".into(),
                    ..Default::default()
                }],
                ..Default::default()
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            ..Default::default()
        });

        Self { ct_root, ht_root }
    }
}
impl PaneView for TestPane1View {
    fn name(&self) -> String {
        "test".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {
        ctx.composite_tree.add_child(target.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.ht_root);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.ct_root);
        ctx.ht_manager.remove_child(self.ht_root);
    }
}

pub struct TestPane2View {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl TestPane2View {
    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [AnimatableFloat::Value(128.0), AnimatableFloat::Value(128.0)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "test content2".into(),
                    color: AnimatableColor::Value([1.0, 0.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                ..Default::default()
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            ..Default::default()
        });

        Self { ct_root, ht_root }
    }
}
impl PaneView for TestPane2View {
    fn name(&self) -> String {
        "test2".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {
        ctx.composite_tree.add_child(target.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.ht_root);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.ct_root);
        ctx.ht_manager.remove_child(self.ht_root);
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DockID(u32);
impl DockID {
    #[inline(always)]
    const fn store_index(&self) -> usize {
        self.0 as _
    }
}

pub struct DockStore {
    docks: Vec<Option<Dock>>,
    computed_states: Vec<ComputedDockState>,
    free_id: BTreeSet<DockID>,
}
impl DockStore {
    pub fn new() -> Self {
        Self {
            docks: Vec::new(),
            computed_states: Vec::new(),
            free_id: BTreeSet::new(),
        }
    }

    pub fn alloc(&mut self, dock: impl FnOnce(DockID) -> Dock) -> DockID {
        if let Some(id) = self.free_id.pop_first() {
            self.docks[id.store_index()] = Some(dock(id));
            return id;
        }

        let id = DockID(self.docks.len().try_into().expect("too many docks"));
        self.docks.push(Some(dock(id)));
        self.computed_states.push(ComputedDockState {
            rect: Rect::from_lt_size(Point::new_logical(0.0, 0.0), Size::new_logical(0.0, 0.0)),
        });
        id
    }

    pub fn alloc_recurse(&mut self, dock: impl FnOnce(DockID, &mut Self) -> Dock) -> DockID {
        if let Some(id) = self.free_id.pop_first() {
            let dock = dock(id, self);
            self.docks[id.store_index()] = Some(dock);
            return id;
        }

        let id = DockID(self.docks.len().try_into().expect("too many docks"));
        self.docks.push(None);
        self.computed_states.push(ComputedDockState {
            rect: Rect::from_lt_size(Point::new_logical(0.0, 0.0), Size::new_logical(0.0, 0.0)),
        });

        let dock = dock(id, self);
        self.docks[id.store_index()] = Some(dock);

        id
    }

    pub fn free(&mut self, id: DockID) -> Dock {
        if id.store_index() == self.docks.len() + 1 {
            // tail freed
            self.computed_states.pop();
            return self
                .docks
                .pop()
                .expect("returned to empty")
                .expect("already freed?");
        }

        self.free_id.insert(id);
        self.docks[id.store_index()].take().expect("already freed?")
    }

    pub fn replace(&mut self, id: DockID, new_dock: Dock) -> Dock {
        core::mem::replace(
            self.docks[id.store_index()]
                .as_mut()
                .expect("already freed?"),
            new_dock,
        )
    }

    pub fn replace_by(&mut self, id: DockID, f: impl FnOnce(Dock) -> Dock) {
        let x = unsafe {
            core::ptr::read(
                self.docks[id.store_index()]
                    .as_ref()
                    .expect("already freed?"),
            )
        };
        core::mem::forget(core::mem::replace(
            self.docks[id.store_index()]
                .as_mut()
                .expect("already freed?"),
            f(x),
        ));
    }

    pub fn get(&self, id: DockID) -> &Dock {
        self.docks[id.store_index()]
            .as_ref()
            .expect("already freed?")
    }

    pub fn get_mut(&mut self, id: DockID) -> &mut Dock {
        self.docks[id.store_index()]
            .as_mut()
            .expect("already freed?")
    }

    pub fn get_computed_state(&self, id: DockID) -> &ComputedDockState {
        &self.computed_states[id.store_index()]
    }

    pub fn get_computed_state_mut(&mut self, id: DockID) -> &mut ComputedDockState {
        &mut self.computed_states[id.store_index()]
    }

    #[tracing::instrument(skip(self, root))]
    pub fn dump(&self, root: DockID) {
        fn rec(store: &DockStore, id: DockID, level: usize, sink: &mut String) {
            use core::fmt::Write;

            sink.extend(core::iter::repeat_n(' ', level * 2));
            writeln!(sink, "#{} {:?}", id.0, store.docks[id.store_index()]).unwrap();

            match store.docks[id.store_index()] {
                Some(Dock::Fill { .. }) => {}
                Some(Dock::ToLeft { left, right, .. })
                | Some(Dock::ToRight { left, right, .. }) => {
                    rec(store, left, level + 1, sink);
                    rec(store, right, level + 1, sink);
                }
                Some(Dock::ToTop { top, bottom, .. })
                | Some(Dock::ToBottom { top, bottom, .. }) => {
                    rec(store, top, level + 1, sink);
                    rec(store, bottom, level + 1, sink);
                }
                None => {}
            }
        }

        let mut sink = String::new();
        rec(self, root, 0, &mut sink);
        tracing::debug!("{}", sink);
    }
}

pub struct ComputedDockState {
    rect: Rect<LogicalUnit>,
}

pub enum Dock {
    Fill {
        parent: Option<DockID>,
        group_view: PaneGroupView,
    },
    ToLeft {
        parent: Option<DockID>,
        left: DockID,
        right: DockID,
        splitter: DockedPaneSplitterView,
        width: Cell<f32>,
    },
    ToRight {
        parent: Option<DockID>,
        left: DockID,
        right: DockID,
        splitter: DockedPaneSplitterView,
        width: Cell<f32>,
    },
    ToTop {
        parent: Option<DockID>,
        top: DockID,
        bottom: DockID,
        splitter: DockedPaneSplitterView,
        height: Cell<f32>,
    },
    ToBottom {
        parent: Option<DockID>,
        top: DockID,
        bottom: DockID,
        splitter: DockedPaneSplitterView,
        height: Cell<f32>,
    },
}
impl core::fmt::Debug for Dock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Fill { parent, .. } => f
                .debug_struct("Dock::Fill")
                .field("parent", parent)
                .finish_non_exhaustive(),
            Self::ToLeft {
                parent,
                left,
                right,
                ..
            } => f
                .debug_struct("Dock::ToLeft")
                .field("parent", parent)
                .field("left", left)
                .field("right", right)
                .finish_non_exhaustive(),
            Self::ToRight {
                parent,
                left,
                right,
                ..
            } => f
                .debug_struct("Dock::ToRight")
                .field("parent", parent)
                .field("left", left)
                .field("right", right)
                .finish_non_exhaustive(),
            Self::ToTop {
                parent,
                top,
                bottom,
                ..
            } => f
                .debug_struct("Dock::ToTop")
                .field("parent", parent)
                .field("top", top)
                .field("bottom", bottom)
                .finish_non_exhaustive(),
            Self::ToBottom {
                parent,
                top,
                bottom,
                ..
            } => f
                .debug_struct("Dock::ToBottom")
                .field("parent", parent)
                .field("top", top)
                .field("bottom", bottom)
                .finish_non_exhaustive(),
        }
    }
}
impl Dock {
    const PARENT_DOCK_THRESHOLD: f32 = 8.0;

    pub fn teardown(
        self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        match self {
            Self::Fill { group_view, .. } => {
                group_view.teardown(composite_tree, ht_manager);
            }
            Self::ToLeft { splitter, .. } => {
                splitter.teardown(composite_tree, ht_manager);
            }
            Self::ToRight { splitter, .. } => {
                splitter.teardown(composite_tree, ht_manager);
            }
            Self::ToTop { splitter, .. } => {
                splitter.teardown(composite_tree, ht_manager);
            }
            Self::ToBottom { splitter, .. } => {
                splitter.teardown(composite_tree, ht_manager);
            }
        }
    }

    fn mount(&self, ctx: &mut MountContext, mount_target: &(impl MountTarget + ?Sized)) {
        match self {
            Dock::Fill { group_view, .. } => group_view.mount(ctx, mount_target),
            Dock::ToLeft { splitter, .. } => {
                splitter.mount(ctx, mount_target);
            }
            Dock::ToRight { splitter, .. } => {
                splitter.mount(ctx, mount_target);
            }
            Dock::ToTop { splitter, .. } => {
                splitter.mount(ctx, mount_target);
            }
            Dock::ToBottom { splitter, .. } => {
                splitter.mount(ctx, mount_target);
            }
        }
    }

    pub const fn parent(&self) -> Option<DockID> {
        match self {
            &Self::Fill { parent, .. } => parent,
            &Self::ToLeft { parent, .. } => parent,
            &Self::ToRight { parent, .. } => parent,
            &Self::ToTop { parent, .. } => parent,
            &Self::ToBottom { parent, .. } => parent,
        }
    }

    pub fn maintain_dock_id_relation(&self, id: DockID) {
        match self {
            Self::Fill { group_view, .. } => {
                group_view.rebind_dock(id);
            }
            Self::ToLeft { splitter, .. } => {
                splitter.rebind_controlling_dock(id);
            }
            Self::ToRight { splitter, .. } => {
                splitter.rebind_controlling_dock(id);
            }
            Self::ToTop { splitter, .. } => {
                splitter.rebind_controlling_dock(id);
            }
            Self::ToBottom { splitter, .. } => {
                splitter.rebind_controlling_dock(id);
            }
        }
    }

    pub fn reparent(self, new_parent: Option<DockID>) -> Self {
        match self {
            Self::Fill { group_view, .. } => Self::Fill {
                group_view,
                parent: new_parent,
            },
            Self::ToLeft {
                left,
                right,
                splitter,
                width,
                ..
            } => Self::ToLeft {
                left,
                right,
                splitter,
                width,
                parent: new_parent,
            },
            Self::ToRight {
                left,
                right,
                splitter,
                width,
                ..
            } => Self::ToRight {
                left,
                right,
                splitter,
                width,
                parent: new_parent,
            },
            Self::ToTop {
                top,
                bottom,
                splitter,
                height,
                ..
            } => Self::ToTop {
                top,
                bottom,
                splitter,
                height,
                parent: new_parent,
            },
            Self::ToBottom {
                top,
                bottom,
                splitter,
                height,
                ..
            } => Self::ToBottom {
                top,
                bottom,
                splitter,
                height,
                parent: new_parent,
            },
        }
    }

    fn replace_matching_child(&mut self, target: DockID, new: DockID) {
        match self {
            Dock::Fill { .. } => unreachable!("fill cannot be nested"),
            Dock::ToLeft { left, right, .. } if *left == target => {
                *left = new;
            }
            Dock::ToLeft { left, right, .. } if *right == target => {
                *right = new;
            }
            Dock::ToRight { left, right, .. } if *left == target => {
                *left = new;
            }
            Dock::ToRight { left, right, .. } if *right == target => {
                *right = new;
            }
            Dock::ToTop { top, bottom, .. } if *top == target => {
                *top = new;
            }
            Dock::ToTop { top, bottom, .. } if *bottom == target => {
                *bottom = new;
            }
            Dock::ToBottom { top, bottom, .. } if *top == target => {
                *top = new;
            }
            Dock::ToBottom { top, bottom, .. } if *bottom == target => {
                *bottom = new;
            }
            t => unreachable!("invalid structure {t:?}"),
        }
    }

    pub fn compute_recommended_operation(
        this: DockID,
        store: &DockStore,
        source_rect: Rect<LogicalUnit>,
        available_rect: Rect<LogicalUnit>,
        pos: Point<LogicalUnit>,
    ) -> (DockingOperation, Rect<LogicalUnit>) {
        fn try_parent_dock(
            this: DockID,
            source_rect: &Rect<LogicalUnit>,
            available_rect: &Rect<LogicalUnit>,
            pos: &Point<LogicalUnit>,
        ) -> Option<(DockingOperation, Rect<LogicalUnit>)> {
            if pos.x <= available_rect.left + Dock::PARENT_DOCK_THRESHOLD {
                return Some((
                    DockingOperation::SplitToLeft(this),
                    available_rect.slice_left(source_rect.width.min(available_rect.width * 0.7)),
                ));
            }
            if pos.x >= available_rect.right() - Dock::PARENT_DOCK_THRESHOLD {
                return Some((
                    DockingOperation::SplitToRight(this),
                    available_rect.slice_right(source_rect.width.min(available_rect.width * 0.7)),
                ));
            }
            if pos.y <= available_rect.top + Dock::PARENT_DOCK_THRESHOLD {
                return Some((
                    DockingOperation::SplitToTop(this),
                    available_rect.slice_top(source_rect.height.min(available_rect.height * 0.7)),
                ));
            }
            if pos.y >= available_rect.bottom() - Dock::PARENT_DOCK_THRESHOLD {
                return Some((
                    DockingOperation::SplitToBottom(this),
                    available_rect
                        .slice_bottom(source_rect.height.min(available_rect.height * 0.7)),
                ));
            }

            None
        }

        if !available_rect.point_in_inclusive(&pos) {
            // not hit to the rect
            return (DockingOperation::Diverge, source_rect);
        }

        match store.get(this) {
            Self::Fill { .. } => {
                let dl = pos.x - available_rect.left;
                let dr = available_rect.right() - pos.x;
                let dt = pos.y - available_rect.top;
                let db = available_rect.bottom() - pos.y;
                if dl.min(dr) < dt.min(db) {
                    if dl <= available_rect.width * 0.3 {
                        return (
                            DockingOperation::SplitToLeft(this),
                            available_rect
                                .slice_left(source_rect.width.min(available_rect.width * 0.7)),
                        );
                    }
                    if dr <= available_rect.width * 0.3 {
                        return (
                            DockingOperation::SplitToRight(this),
                            available_rect
                                .slice_right(source_rect.width.min(available_rect.width * 0.7)),
                        );
                    }
                } else {
                    if dt <= available_rect.height * 0.3 {
                        return (
                            DockingOperation::SplitToTop(this),
                            available_rect
                                .slice_top(source_rect.height.min(available_rect.height * 0.7)),
                        );
                    }
                    if db <= available_rect.height * 0.3 {
                        return (
                            DockingOperation::SplitToBottom(this),
                            available_rect
                                .slice_bottom(source_rect.height.min(available_rect.height * 0.7)),
                        );
                    }
                }

                return (DockingOperation::Merge(this), available_rect);
            }
            &Self::ToLeft {
                left,
                right,
                ref width,
                ..
            } => {
                if let Some(op) = try_parent_dock(this, &source_rect, &available_rect, &pos) {
                    return op;
                }

                let width = width.get();
                let r = available_rect.slice_left(width);
                if pos.x <= r.right() {
                    return Self::compute_recommended_operation(left, store, source_rect, r, pos);
                }
                let r = available_rect
                    .slice_right(available_rect.width - width - DockedPaneSplitterView::THICKNESS);
                if pos.x >= r.left {
                    return Self::compute_recommended_operation(right, store, source_rect, r, pos);
                }
            }
            &Self::ToRight {
                left,
                right,
                ref width,
                ..
            } => {
                if let Some(op) = try_parent_dock(this, &source_rect, &available_rect, &pos) {
                    return op;
                }

                let width = width.get();
                let r = available_rect
                    .slice_left(available_rect.width - width - DockedPaneSplitterView::THICKNESS);
                if pos.x <= r.right() {
                    return Self::compute_recommended_operation(left, store, source_rect, r, pos);
                }
                let r = available_rect.slice_right(width);
                if pos.x >= r.left {
                    return Self::compute_recommended_operation(right, store, source_rect, r, pos);
                }
            }
            &Self::ToTop {
                top,
                bottom,
                ref height,
                ..
            } => {
                if let Some(op) = try_parent_dock(this, &source_rect, &available_rect, &pos) {
                    return op;
                }

                let height = height.get();
                let r = available_rect.slice_top(height);
                if pos.y <= r.bottom() {
                    return Self::compute_recommended_operation(top, store, source_rect, r, pos);
                }
                let r = available_rect.slice_bottom(
                    available_rect.height - height - DockedPaneSplitterView::THICKNESS,
                );
                if pos.y >= r.top {
                    return Self::compute_recommended_operation(bottom, store, source_rect, r, pos);
                }
            }
            &Self::ToBottom {
                top,
                bottom,
                ref height,
                ..
            } => {
                if let Some(op) = try_parent_dock(this, &source_rect, &available_rect, &pos) {
                    return op;
                }

                let height = height.get();
                let r = available_rect
                    .slice_top(available_rect.height - height - DockedPaneSplitterView::THICKNESS);
                if pos.y <= r.bottom() {
                    return Self::compute_recommended_operation(top, store, source_rect, r, pos);
                }
                let r = available_rect.slice_bottom(height);
                if pos.y >= r.top {
                    return Self::compute_recommended_operation(bottom, store, source_rect, r, pos);
                }
            }
        }

        return (DockingOperation::Diverge, source_rect);
    }
}

#[derive(Debug)]
pub enum DockingOperation {
    Merge(DockID),
    SplitToLeft(DockID),
    SplitToRight(DockID),
    SplitToTop(DockID),
    SplitToBottom(DockID),
    Diverge,
}

pub struct DockingPreviewState {
    control_rect: Rect<LogicalUnit>,
    original_rect: Rect<LogicalUnit>,
    offset: Point<LogicalUnit>,
}

pub struct DockingManager {
    bound_window: WindowHandle,
    max_rect: Rect<LogicalUnit>,
    store: DockStore,
    root_id: DockID,
    preview_state: Option<DockingPreviewState>,
}
impl DockingManager {
    #[tracing::instrument(skip(bound_window, ctx, dock_ctor))]
    pub fn new(
        bound_window: WindowHandle,
        ctx: &mut ViewInitContext,
        max_rect: Rect<LogicalUnit>,
        dock_ctor: impl FnOnce(&mut ViewInitContext, &mut DockStore) -> DockID,
    ) -> Self {
        let mut store = DockStore::new();
        let root_id = dock_ctor(ctx, &mut store);
        Self::mount_recursive(root_id, &store, ctx, &bound_window);
        Self::relayout_dock(
            root_id,
            &mut store,
            max_rect.clone(),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );

        store.dump(root_id);

        Self {
            bound_window,
            max_rect,
            root_id,
            store,
            preview_state: None,
        }
    }

    fn mount_recursive(
        target: DockID,
        store: &DockStore,
        ctx: &mut MountContext,
        mount_target: &(impl MountTarget + ?Sized),
    ) {
        match store.get(target) {
            Dock::Fill { group_view, .. } => group_view.mount(ctx, mount_target),
            &Dock::ToLeft {
                left,
                right,
                ref splitter,
                ..
            } => {
                Self::mount_recursive(left, store, ctx, mount_target);
                Self::mount_recursive(right, store, ctx, mount_target);
                splitter.mount(ctx, mount_target);
            }
            &Dock::ToRight {
                left,
                right,
                ref splitter,
                ..
            } => {
                Self::mount_recursive(left, store, ctx, mount_target);
                Self::mount_recursive(right, store, ctx, mount_target);
                splitter.mount(ctx, mount_target);
            }
            &Dock::ToTop {
                top,
                bottom,
                ref splitter,
                ..
            } => {
                Self::mount_recursive(top, store, ctx, mount_target);
                Self::mount_recursive(bottom, store, ctx, mount_target);
                splitter.mount(ctx, mount_target);
            }
            &Dock::ToBottom {
                top,
                bottom,
                ref splitter,
                ..
            } => {
                Self::mount_recursive(top, store, ctx, mount_target);
                Self::mount_recursive(bottom, store, ctx, mount_target);
                splitter.mount(ctx, mount_target);
            }
        }
    }

    fn relayout_dock(
        target: DockID,
        store: &mut DockStore,
        available_rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        store.get_computed_state_mut(target).rect = available_rect.clone();

        match store.get(target) {
            Dock::Fill { group_view, .. } => {
                group_view.set_rect(available_rect, composite_tree, ht_manager)
            }
            &Dock::ToLeft {
                left,
                right,
                ref splitter,
                ref width,
                ..
            } => {
                let width = width.get();
                let l_rect = Rect::from_lt_size(
                    Point::new_logical(available_rect.left, available_rect.top),
                    Size::new_logical(width, available_rect.height),
                );
                let s_rect = Rect::from_lt_size(
                    Point::new_logical(l_rect.right(), available_rect.top),
                    Size::new_logical(DockedPaneSplitterView::THICKNESS, available_rect.height),
                );
                let r_rect = Rect::from_lt_size(
                    Point::new_logical(s_rect.right(), available_rect.top),
                    Size::new_logical(
                        available_rect.width - s_rect.width - l_rect.width,
                        available_rect.height,
                    ),
                );

                splitter.resize(s_rect, composite_tree, ht_manager);
                Self::relayout_dock(left, store, l_rect, composite_tree, ht_manager);
                Self::relayout_dock(right, store, r_rect, composite_tree, ht_manager);
            }
            &Dock::ToRight {
                left,
                right,
                ref splitter,
                ref width,
                ..
            } => {
                let width = width.get();
                let l_rect = Rect::from_lt_size(
                    Point::new_logical(available_rect.left, available_rect.top),
                    Size::new_logical(
                        available_rect.width - width - DockedPaneSplitterView::THICKNESS,
                        available_rect.height,
                    ),
                );
                let s_rect = Rect::from_lt_size(
                    Point::new_logical(l_rect.right(), available_rect.top),
                    Size::new_logical(DockedPaneSplitterView::THICKNESS, available_rect.height),
                );
                let r_rect = Rect::from_lt_size(
                    Point::new_logical(s_rect.right(), available_rect.top),
                    Size::new_logical(width, available_rect.height),
                );

                splitter.resize(s_rect, composite_tree, ht_manager);
                Self::relayout_dock(left, store, l_rect, composite_tree, ht_manager);
                Self::relayout_dock(right, store, r_rect, composite_tree, ht_manager);
            }
            &Dock::ToTop {
                top,
                bottom,
                ref splitter,
                ref height,
                ..
            } => {
                let height = height.get();
                let t_rect = Rect::from_lt_size(
                    Point::new_logical(available_rect.left, available_rect.top),
                    Size::new_logical(available_rect.width, height),
                );
                let s_rect = Rect::from_lt_size(
                    Point::new_logical(available_rect.left, t_rect.bottom()),
                    Size::new_logical(available_rect.width, DockedPaneSplitterView::THICKNESS),
                );
                let b_rect = Rect::from_lt_size(
                    Point::new_logical(available_rect.left, s_rect.bottom()),
                    Size::new_logical(
                        available_rect.width,
                        available_rect.height - s_rect.height - t_rect.height,
                    ),
                );

                splitter.resize(s_rect, composite_tree, ht_manager);
                Self::relayout_dock(top, store, t_rect, composite_tree, ht_manager);
                Self::relayout_dock(bottom, store, b_rect, composite_tree, ht_manager);
            }
            &Dock::ToBottom {
                top,
                bottom,
                ref splitter,
                ref height,
                ..
            } => {
                let height = height.get();
                let t_rect = Rect::from_lt_size(
                    Point::new_logical(available_rect.left, available_rect.top),
                    Size::new_logical(
                        available_rect.width,
                        available_rect.height - height - DockedPaneSplitterView::THICKNESS,
                    ),
                );
                let s_rect = Rect::from_lt_size(
                    Point::new_logical(available_rect.left, t_rect.bottom()),
                    Size::new_logical(available_rect.width, DockedPaneSplitterView::THICKNESS),
                );
                let b_rect = Rect::from_lt_size(
                    Point::new_logical(available_rect.left, s_rect.bottom()),
                    Size::new_logical(available_rect.width, height),
                );

                splitter.resize(s_rect, composite_tree, ht_manager);
                Self::relayout_dock(top, store, t_rect, composite_tree, ht_manager);
                Self::relayout_dock(bottom, store, b_rect, composite_tree, ht_manager);
            }
        }
    }

    pub fn move_splitter(
        &mut self,
        target: DockID,
        new_splitter_client_pos: f32,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        let self_rect = &self.store.get_computed_state(target).rect;
        match self.store.get(target) {
            Dock::Fill { .. } => unreachable!("fill does not have any splitters!"),
            Dock::ToLeft { width, .. } => {
                let new_fixed_size =
                    (new_splitter_client_pos - self_rect.left).clamp(10.0, self_rect.width - 10.0);
                width.set(new_fixed_size);
            }
            Dock::ToRight { width, .. } => {
                let new_fixed_size = (self_rect.right()
                    - (new_splitter_client_pos + DockedPaneSplitterView::THICKNESS))
                    .clamp(10.0, self_rect.width - 10.0);
                width.set(new_fixed_size);
            }
            Dock::ToTop { height, .. } => {
                let new_fixed_size =
                    (new_splitter_client_pos - self_rect.top).clamp(10.0, self_rect.height - 10.0);
                height.set(new_fixed_size);
            }
            Dock::ToBottom { height, .. } => {
                let new_fixed_size = (self_rect.bottom()
                    - (new_splitter_client_pos + DockedPaneSplitterView::THICKNESS))
                    .clamp(10.0, self_rect.height - 10.0);
                height.set(new_fixed_size);
            }
        }

        let self_rect = self_rect.clone();
        Self::relayout_dock(
            target,
            &mut self.store,
            self_rect,
            composite_tree,
            ht_manager,
        );
    }

    #[tracing::instrument(skip(self, composite_tree, ht_manager))]
    fn undock(
        &mut self,
        target: DockID,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        self.store.dump(self.root_id);

        let old_dock = self.store.free(target);

        match old_dock {
            Dock::Fill { parent: None, .. } => todo!("empty docking manager support"),
            Dock::Fill {
                parent: Some(parent),
                group_view,
            } => {
                group_view.teardown(composite_tree, ht_manager);
                let (remain_dock, parent_parent) = match self.store.get(parent) {
                    Dock::Fill { .. } => unreachable!("fill cannot be nested"),
                    &Dock::ToLeft {
                        left,
                        right,
                        parent: parent_parent,
                        ..
                    } if left == target => (right, parent_parent),
                    &Dock::ToLeft {
                        left,
                        right,
                        parent: parent_parent,
                        ..
                    } if right == target => (left, parent_parent),
                    &Dock::ToRight {
                        left,
                        right,
                        parent: parent_parent,
                        ..
                    } if left == target => (right, parent_parent),
                    &Dock::ToRight {
                        left,
                        right,
                        parent: parent_parent,
                        ..
                    } if right == target => (left, parent_parent),
                    &Dock::ToTop {
                        top,
                        bottom,
                        parent: parent_parent,
                        ..
                    }
                    | &Dock::ToBottom {
                        top,
                        bottom,
                        parent: parent_parent,
                        ..
                    } if top == target => (bottom, parent_parent),
                    &Dock::ToTop {
                        top,
                        bottom,
                        parent: parent_parent,
                        ..
                    }
                    | &Dock::ToBottom {
                        top,
                        bottom,
                        parent: parent_parent,
                        ..
                    } if bottom == target => (top, parent_parent),
                    t => unreachable!("invalid structure {t:?}"),
                };

                let new_parent = self.store.free(remain_dock).reparent(parent_parent);
                new_parent.maintain_dock_id_relation(parent);
                match new_parent {
                    Dock::Fill { .. } => (),
                    Dock::ToLeft { left, right, .. } | Dock::ToRight { left, right, .. } => {
                        self.store.replace_by(left, |x| x.reparent(Some(parent)));
                        self.store.replace_by(right, |x| x.reparent(Some(parent)));
                    }
                    Dock::ToTop { top, bottom, .. } | Dock::ToBottom { top, bottom, .. } => {
                        self.store.replace_by(top, |x| x.reparent(Some(parent)));
                        self.store.replace_by(bottom, |x| x.reparent(Some(parent)));
                    }
                }
                self.store
                    .replace(parent, new_parent)
                    .teardown(composite_tree, ht_manager);
                let relayout_base = parent_parent.unwrap_or(self.root_id);
                let relayout_base_rect = self.store.get_computed_state(relayout_base).rect.clone();
                Self::relayout_dock(
                    relayout_base,
                    &mut self.store,
                    relayout_base_rect,
                    composite_tree,
                    ht_manager,
                );
            }
            _ => todo!(),
        }

        self.store.dump(self.root_id);
    }

    #[tracing::instrument(skip(self, view_init_ctx, mount_target))]
    fn redock(
        &mut self,
        source: DockID,
        index: usize,
        op: DockingOperation,
        suggested_rect: &Rect<LogicalUnit>,
        view_init_ctx: &mut ViewInitContext,
        mount_target: &(impl MountTarget + ?Sized),
    ) -> Option<Box<dyn PaneView>> {
        self.store.dump(self.root_id);

        let Dock::Fill {
            group_view: source_group_view,
            ..
        } = self.store.get(source)
        else {
            unreachable!("merge from non-fill dock");
        };
        let content = source_group_view.remove_content(index, view_init_ctx);
        let should_undock_source = !source_group_view.has_contents();

        let diverged_contents = match op {
            // ウィンドウのオープンが必要なので内容物だけ返してLogicFiber側でやる
            DockingOperation::Diverge => Some(content),
            DockingOperation::Merge(target) => {
                let Dock::Fill {
                    group_view: target_group_view,
                    ..
                } = self.store.get(target)
                else {
                    unreachable!("merge into non-fill dock");
                };

                target_group_view.add_content(content, view_init_ctx, true);
                None
            }
            DockingOperation::SplitToLeft(target) => {
                let target_parent = self.store.get(target).parent();
                let new_dock = self.store.alloc_recurse(|parent_id, store| {
                    let d = Dock::ToLeft {
                        parent: target_parent,
                        left: store.alloc(|id| {
                            let d = Dock::Fill {
                                parent: Some(parent_id),
                                group_view: PaneGroupView::new(view_init_ctx, vec![content], id),
                            };
                            d.mount(view_init_ctx, mount_target);
                            d
                        }),
                        right: target,
                        splitter: DockedPaneSplitterView::new(
                            view_init_ctx,
                            DockedPaneSplitDirection::Vertical,
                            parent_id,
                        ),
                        width: Cell::new(suggested_rect.width),
                    };
                    d.mount(view_init_ctx, mount_target);
                    d
                });

                self.store
                    .replace_by(target, |x| x.reparent(Some(new_dock)));
                if let Some(target_parent) = target_parent {
                    self.store
                        .get_mut(target_parent)
                        .replace_matching_child(target, new_dock);
                } else {
                    assert_eq!(self.root_id, target);
                    self.root_id = new_dock;
                }

                let relayout_base = target_parent.unwrap_or(self.root_id);
                let relayout_base_rect = target_parent
                    .map_or(&self.max_rect, |x| &self.store.get_computed_state(x).rect)
                    .clone();
                Self::relayout_dock(
                    relayout_base,
                    &mut self.store,
                    relayout_base_rect,
                    view_init_ctx.mount_context.composite_tree,
                    view_init_ctx.mount_context.ht_manager,
                );
                None
            }
            DockingOperation::SplitToRight(target) => {
                let target_parent = self.store.get(target).parent();
                let new_dock = self.store.alloc_recurse(|parent_id, store| {
                    let d = Dock::ToRight {
                        parent: target_parent,
                        left: target,
                        right: store.alloc(|id| {
                            let d = Dock::Fill {
                                parent: Some(parent_id),
                                group_view: PaneGroupView::new(view_init_ctx, vec![content], id),
                            };
                            d.mount(view_init_ctx, mount_target);
                            d
                        }),
                        splitter: DockedPaneSplitterView::new(
                            view_init_ctx,
                            DockedPaneSplitDirection::Vertical,
                            parent_id,
                        ),
                        width: Cell::new(suggested_rect.width),
                    };
                    d.mount(view_init_ctx, mount_target);
                    d
                });

                self.store
                    .replace_by(target, |x| x.reparent(Some(new_dock)));
                if let Some(target_parent) = target_parent {
                    self.store
                        .get_mut(target_parent)
                        .replace_matching_child(target, new_dock);
                } else {
                    assert_eq!(self.root_id, target);
                    self.root_id = new_dock;
                }

                let relayout_base = target_parent.unwrap_or(self.root_id);
                let relayout_base_rect = target_parent
                    .map_or(&self.max_rect, |x| &self.store.get_computed_state(x).rect)
                    .clone();
                Self::relayout_dock(
                    relayout_base,
                    &mut self.store,
                    relayout_base_rect,
                    view_init_ctx.mount_context.composite_tree,
                    view_init_ctx.mount_context.ht_manager,
                );
                None
            }
            DockingOperation::SplitToTop(target) => {
                let target_parent = self.store.get(target).parent();
                let new_dock = self.store.alloc_recurse(|parent_id, store| {
                    let d = Dock::ToTop {
                        parent: target_parent,
                        top: store.alloc(|id| {
                            let d = Dock::Fill {
                                parent: Some(parent_id),
                                group_view: PaneGroupView::new(view_init_ctx, vec![content], id),
                            };
                            d.mount(view_init_ctx, mount_target);
                            d
                        }),
                        bottom: target,
                        splitter: DockedPaneSplitterView::new(
                            view_init_ctx,
                            DockedPaneSplitDirection::Horizontal,
                            parent_id,
                        ),
                        height: Cell::new(suggested_rect.height),
                    };
                    d.mount(view_init_ctx, mount_target);
                    d
                });

                self.store
                    .replace_by(target, |x| x.reparent(Some(new_dock)));
                if let Some(target_parent) = target_parent {
                    self.store
                        .get_mut(target_parent)
                        .replace_matching_child(target, new_dock);
                } else {
                    assert_eq!(self.root_id, target);
                    self.root_id = new_dock;
                }

                let relayout_base = target_parent.unwrap_or(self.root_id);
                let relayout_base_rect = target_parent
                    .map_or(&self.max_rect, |x| &self.store.get_computed_state(x).rect)
                    .clone();
                Self::relayout_dock(
                    relayout_base,
                    &mut self.store,
                    relayout_base_rect,
                    view_init_ctx.mount_context.composite_tree,
                    view_init_ctx.mount_context.ht_manager,
                );
                None
            }
            DockingOperation::SplitToBottom(target) => {
                let target_parent = self.store.get(target).parent();
                let new_dock = self.store.alloc_recurse(|parent_id, store| {
                    let d = Dock::ToBottom {
                        parent: target_parent,
                        top: target,
                        bottom: store.alloc(|id| {
                            let d = Dock::Fill {
                                parent: Some(parent_id),
                                group_view: PaneGroupView::new(view_init_ctx, vec![content], id),
                            };
                            d.mount(view_init_ctx, mount_target);
                            d
                        }),
                        splitter: DockedPaneSplitterView::new(
                            view_init_ctx,
                            DockedPaneSplitDirection::Horizontal,
                            parent_id,
                        ),
                        height: Cell::new(suggested_rect.height),
                    };
                    d.mount(view_init_ctx, mount_target);
                    d
                });

                self.store
                    .replace_by(target, |x| x.reparent(Some(new_dock)));
                if let Some(target_parent) = target_parent {
                    self.store
                        .get_mut(target_parent)
                        .replace_matching_child(target, new_dock);
                } else {
                    assert_eq!(self.root_id, target);
                    self.root_id = new_dock;
                }

                let relayout_base = target_parent.unwrap_or(self.root_id);
                let relayout_base_rect = target_parent
                    .map_or(&self.max_rect, |x| &self.store.get_computed_state(x).rect)
                    .clone();
                Self::relayout_dock(
                    relayout_base,
                    &mut self.store,
                    relayout_base_rect,
                    view_init_ctx.mount_context.composite_tree,
                    view_init_ctx.mount_context.ht_manager,
                );
                None
            }
        };

        if should_undock_source {
            self.undock(
                source,
                view_init_ctx.mount_context.composite_tree,
                view_init_ctx.mount_context.ht_manager,
            );
        }

        self.store.dump(self.root_id);
        diverged_contents
    }

    fn begin_preview(
        &mut self,
        pane_rect: Rect<LogicalUnit>,
        client_pos: &Point<LogicalUnit>,
        popover: &DragPreviewPopoverHandle,
    ) {
        popover.show(
            &Point::new_logical(pane_rect.left, pane_rect.top),
            &Size::new_logical(pane_rect.width, pane_rect.height),
        );
        self.preview_state = Some(DockingPreviewState {
            offset: Point::new_logical(pane_rect.left - client_pos.x, pane_rect.top - client_pos.y),
            control_rect: self.max_rect.clone(),
            original_rect: pane_rect,
        });
    }

    fn move_preview(&self, client_pos: Point<LogicalUnit>, popover: &DragPreviewPopoverHandle) {
        let Some(ref state) = self.preview_state else {
            return;
        };

        match Dock::compute_recommended_operation(
            self.root_id,
            &self.store,
            state.original_rect.clone(),
            state.control_rect.clone(),
            client_pos,
        ) {
            (DockingOperation::Merge(_), r) => {
                popover.set_rect(&r);
            }
            (DockingOperation::SplitToLeft(_), r) => {
                popover.set_rect(&r);
            }
            (DockingOperation::SplitToRight(_), r) => {
                popover.set_rect(&r);
            }
            (DockingOperation::SplitToTop(_), r) => {
                popover.set_rect(&r);
            }
            (DockingOperation::SplitToBottom(_), r) => {
                popover.set_rect(&r);
            }
            (DockingOperation::Diverge, _) => {
                popover.set_rect(&Rect::from_lt_size(
                    Point::new_logical(
                        client_pos.x + state.offset.x,
                        client_pos.y + state.offset.y,
                    ),
                    state.original_rect.size(),
                ));
            }
        }
    }

    fn end_preview(
        &mut self,
        client_pos: Point<LogicalUnit>,
        popover: &DragPreviewPopoverHandle,
    ) -> (DockingOperation, Rect<LogicalUnit>) {
        let state = self.preview_state.take().expect("not docking");

        popover.hide();
        Dock::compute_recommended_operation(
            self.root_id,
            &self.store,
            state.original_rect.clone(),
            state.control_rect.clone(),
            client_pos,
        )
    }
}

enum DockedPaneSplitDirection {
    Horizontal,
    Vertical,
}

pub struct DockedPaneSplitterView {
    eh: Rc<DockedPaneSplitterEventHandler>,
}
impl DockedPaneSplitterView {
    const THICKNESS: f32 = 4.0;

    fn new(
        ctx: &mut ViewInitContext,
        dir: DockedPaneSplitDirection,
        controlling_dock: DockID,
    ) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                1.0, 1.0, 1.0, 0.125,
            ])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            cursor_shape: match dir {
                DockedPaneSplitDirection::Horizontal => CursorShape::ResizeVertical,
                DockedPaneSplitDirection::Vertical => CursorShape::ResizeHorizontal,
            },
            ..Default::default()
        });

        let eh = Rc::new(DockedPaneSplitterEventHandler {
            view_id: ctx.view_registry.alloc(),
            dir,
            controlling_dock: Cell::new(controlling_dock),
            ct_root,
            ht_root,
            pressing: Cell::new(false),
            pending_relayout: Cell::new(None),
            drag_delta: Cell::new(0.0),
        });
        ctx.ht_manager.set_action_handler(eh.ht_root, &eh);
        ctx.view_registry.set_event_handler(eh.view_id, &eh);

        Self { eh }
    }

    pub fn teardown(
        self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        ht_manager.remove_child(self.eh.ht_root);
        composite_tree.remove_child(self.eh.ct_root);

        ht_manager.free_all(self.eh.ht_root);
        composite_tree.free_all(self.eh.ct_root);
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }

    #[inline(always)]
    fn resize<E>(
        &self,
        rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        self.eh.perform_relayout(rect, composite_tree, ht_manager);
    }

    #[inline(always)]
    fn rebind_controlling_dock(&self, dock: DockID) {
        self.eh.controlling_dock.set(dock);
    }
}

struct DockedPaneSplitterEventHandler {
    view_id: ViewIdentifier,
    dir: DockedPaneSplitDirection,
    controlling_dock: Cell<DockID>,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    pressing: Cell<bool>,
    pending_relayout: Cell<Option<Rect<LogicalUnit>>>,
    drag_delta: Cell<f32>,
}
impl ViewEventHandler for DockedPaneSplitterEventHandler {
    fn update(&self, context: &mut ViewUpdateContext) {
        if let Some(new_rect) = self.pending_relayout.take() {
            self.perform_relayout(
                new_rect,
                context.mount_context.composite_tree,
                context.mount_context.ht_manager,
            );
        }
    }
}
impl HitTestTreeActionHandler for DockedPaneSplitterEventHandler {
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.drag_delta.set(match self.dir {
            DockedPaneSplitDirection::Horizontal => {
                args.client_pos.y - context.ht_manager.compute_global_rect_autoroot(sender).1
            }
            DockedPaneSplitDirection::Vertical => {
                args.client_pos.x - context.ht_manager.compute_global_rect_autoroot(sender).0
            }
        });
        self.pressing.set(true);

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT
    }

    fn on_pointer_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        if !self.pressing.get() {
            return EventContinueControl::empty();
        }

        self.r#move(
            &args.client_pos,
            context.ht_manager,
            context.system_link.event_dispatcher(),
        );
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.r#move(
            &args.client_pos,
            context.ht_manager,
            context.system_link.event_dispatcher(),
        );
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.pressing.set(false);

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::RELEASE_CAPTURE_ELEMENT
    }
}
impl DockedPaneSplitterEventHandler {
    fn r#move(
        &self,
        client_pos: &Point<PointerInputUnit>,
        ht_manager: &HitTestTreeManager,
        event_dispatcher: &LogicFiberEventDispatcher,
    ) {
        let pos = match self.dir {
            DockedPaneSplitDirection::Horizontal => client_pos.y + self.drag_delta.get(),
            DockedPaneSplitDirection::Vertical => client_pos.x + self.drag_delta.get(),
        };
        event_dispatcher.dispatch(Event::DockMoveSplitter {
            window: ht_manager
                .query_root_window(self.ht_root)
                .expect("not mounted"),
            controlling_dock: self.controlling_dock.get(),
            pos_client: pos,
        });
    }

    fn perform_relayout<E>(
        &self,
        new_rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        composite_tree.get_mut(self.ct_root).offset = [
            AnimatableFloat::Value(new_rect.left),
            AnimatableFloat::Value(new_rect.top),
        ];
        composite_tree.get_mut(self.ct_root).size = [
            AnimatableFloat::Value(new_rect.width),
            AnimatableFloat::Value(new_rect.height),
        ];
        composite_tree.mark_dirty(self.ct_root);
        ht_manager.get_data_mut(self.ht_root).left = new_rect.left;
        ht_manager.get_data_mut(self.ht_root).top = new_rect.top;
        ht_manager.get_data_mut(self.ht_root).width = new_rect.width;
        ht_manager.get_data_mut(self.ht_root).height = new_rect.height;
    }
}

static PANE_GROUP_TAB_ACTIVE_GRADIENT: UnsafeMainThreadOnlyOnceCell<GradientRef> =
    UnsafeMainThreadOnlyOnceCell(std::cell::OnceCell::new());
fn pane_group_tab_active_gradient<E>(composite_tree: &mut CompositeTree<E>) -> GradientRef {
    *PANE_GROUP_TAB_ACTIVE_GRADIENT.0.get_or_init(|| {
        composite_tree.create_gradient(Gradient::Linear {
            start_color: [0.0, 0.1, 0.5, 0.0],
            end_color: [0.0, 0.1, 0.5, 1.0],
            start_pos_relative: [0.0, 0.8],
            end_pos_relative: [0.0, 1.0],
        })
    })
}

pub struct PaneGroupView {
    controller: Rc<PaneGroupViewController>,
}
impl PaneGroupView {
    pub fn new(ctx: &mut ViewInitContext, contents: Vec<Box<dyn PaneView>>, dock: DockID) -> Self {
        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.5])),
            clip_child: Some(ClipConfig {
                left_softness: SafeF32::ZERO,
                top_softness: SafeF32::ZERO,
                right_softness: SafeF32::ZERO,
                bottom_softness: SafeF32::ZERO,
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            ..Default::default()
        });

        let ct_content_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            relative_size_adjustment: [1.0, 1.0],
            offset: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(16.0 + PaneGroupTabView::PADDING_Y * 2.0),
            ],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(-16.0 - PaneGroupTabView::PADDING_Y * 2.0),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([
                0.01, 0.01, 0.01, 1.0,
            ])),
            ..Default::default()
        });
        let ht_content_root = ctx.ht_manager.create(HitTestTreeData {
            top: 16.0 + PaneGroupTabView::PADDING_Y * 2.0,
            height: -16.0 - PaneGroupTabView::PADDING_Y * 2.0,
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_root, ct_content_root);
        ctx.ht_manager.add_child(ht_root, ht_content_root);

        let controller = Rc::new_cyclic(|wgc| PaneGroupViewController {
            view_id: ctx.view_registry.alloc(),
            ct_root,
            ht_root,
            ct_content_root,
            ht_content_root,
            dock: Cell::new(dock),
            contents: RefCell::new(
                contents
                    .into_iter()
                    .map(|c| {
                        let tv = PaneGroupTabView::new(ctx, c.name(), wgc.clone());
                        tv.mount(ctx, &RawMountTarget { ct_root, ht_root });
                        (c, tv)
                    })
                    .collect(),
            ),
            current_active_index: Cell::new(0),
            pending_active_changes: Cell::new(None),
            pending_set_rect: Cell::new(None),
        });
        ctx.view_registry
            .set_event_handler(controller.view_id, &controller);

        Self::relocate_tabs(
            controller.contents.borrow().iter().map(|(_, t)| t),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );
        controller.activate(0, ctx);

        Self { controller }
    }

    pub fn teardown(
        self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        composite_tree.remove_child(self.controller.ct_root);
        ht_manager.remove_child(self.controller.ht_root);

        composite_tree.free_all(self.controller.ct_root);
        ht_manager.free_all(self.controller.ht_root);
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.controller.ct_root);
        ctx.ht_manager
            .add_child(target.ht_root(), self.controller.ht_root);
    }

    #[inline(always)]
    pub fn set_rect(
        &self,
        rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        self.controller
            .perform_set_rect(rect, composite_tree, ht_manager);
    }

    #[inline(always)]
    fn rebind_dock(&self, dock: DockID) {
        self.controller.dock.set(dock);
    }

    fn add_content(
        &self,
        content: Box<dyn PaneView>,
        ctx: &mut ViewInitContext,
        with_activate: bool,
    ) {
        let tab_view = PaneGroupTabView::new(ctx, content.name(), Rc::downgrade(&self.controller));
        tab_view.mount(
            ctx,
            &RawMountTarget {
                ct_root: self.controller.ct_root,
                ht_root: self.controller.ht_root,
            },
        );
        self.controller
            .contents
            .borrow_mut()
            .push((content, tab_view));
        Self::relocate_tabs(
            self.controller.contents.borrow().iter().map(|(_, t)| t),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );

        if with_activate {
            self.controller
                .perform_change_active(self.controller.contents.borrow().len() - 1, ctx);
        }
    }

    fn remove_content(&self, index: usize, ctx: &mut MountContext) -> Box<dyn PaneView> {
        let is_active = self.controller.current_active_index.get() == index;
        let (content, tab) = self.controller.contents.borrow_mut().remove(index);
        if is_active {
            content.unmount(ctx);

            if !self.controller.contents.borrow().is_empty() {
                let new_active = self
                    .controller
                    .current_active_index
                    .get()
                    .clamp(0, self.controller.contents.borrow().len() - 1);
                self.controller.activate(new_active, ctx);
                self.controller.current_active_index.set(new_active);
            }
        }
        // TODO: ここのアクティブインデックスのメンテ処理が不十分なので直す（最後のタブがアクティブのときに他のタブをグループから外すと次に切り替えしたときに範囲外参照になる）
        // アクティブタブをインデックスで持つんじゃなくてインスタンスアドレスで持つ形にするのがよさそうかも

        Self::relocate_tabs(
            self.controller.contents.borrow().iter().map(|(_, t)| t),
            ctx.composite_tree,
            ctx.ht_manager,
        );

        tab.teardown(ctx);
        content
    }

    #[inline(always)]
    fn has_contents(&self) -> bool {
        !self.controller.contents.borrow().is_empty()
    }

    fn relocate_tabs<'t, E>(
        tabs: impl Iterator<Item = &'t PaneGroupTabView>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        let mut left_offset = 0.0;
        for t in tabs {
            t.place(
                Point::new_logical(left_offset, 0.0),
                composite_tree,
                ht_manager,
            );
            left_offset += t.size.width;
        }
    }
}

struct PaneGroupViewController {
    view_id: ViewIdentifier,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    ct_content_root: CompositeTreeRef,
    ht_content_root: HitTestTreeRef,
    dock: Cell<DockID>,
    contents: RefCell<Vec<(Box<dyn PaneView>, PaneGroupTabView)>>,
    current_active_index: Cell<usize>,
    pending_active_changes: Cell<Option<usize>>,
    pending_set_rect: Cell<Option<Rect<LogicalUnit>>>,
}
impl ViewEventHandler for PaneGroupViewController {
    fn update(&self, context: &mut ViewUpdateContext) {
        if let Some(index) = self.pending_active_changes.take() {
            self.perform_change_active(index, context);
        }

        if let Some(rect) = self.pending_set_rect.take() {
            self.perform_set_rect(
                rect,
                context.mount_context.composite_tree,
                context.mount_context.ht_manager,
            );
        }
    }
}
impl PaneGroupViewController {
    #[inline(always)]
    fn tab_index(&self, tab: &PaneGroupTabEventHandler) -> Option<usize> {
        self.contents
            .borrow()
            .iter()
            .position(|x| core::ptr::addr_eq(x.1.eh.as_ref(), tab))
    }

    fn select_tab(&self, tab: &PaneGroupTabEventHandler, e: &LogicFiberEventDispatcher) {
        let Some(index) = self.tab_index(tab) else {
            tracing::warn!("no tab found");
            return;
        };

        self.pending_active_changes.set(Some(index));
        e.dispatch(Event::UpdateView { id: self.view_id });
    }

    fn perform_change_active(&self, new_active_index: usize, ctx: &mut MountContext) {
        let old_active = self.current_active_index.replace(new_active_index);
        if old_active != new_active_index {
            let old_active = &self.contents.borrow()[old_active];
            old_active.0.unmount(ctx);
            old_active
                .1
                .eh
                .set_active(false, ctx.composite_tree, ctx.current_sec);

            self.activate(new_active_index, ctx);
        }
    }

    fn activate(&self, index: usize, context: &mut MountContext) {
        let target = &self.contents.borrow()[index];
        target.0.mount(
            context,
            &RawMountTarget {
                ct_root: self.ct_content_root,
                ht_root: self.ht_content_root,
            },
        );
        target
            .1
            .eh
            .set_active(true, context.composite_tree, context.current_sec);
    }

    fn perform_set_rect(
        &self,
        rect: Rect<LogicalUnit>,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        composite_tree.get_mut(self.ct_root).offset = [
            AnimatableFloat::Value(rect.left),
            AnimatableFloat::Value(rect.top),
        ];
        composite_tree.get_mut(self.ct_root).size = [
            AnimatableFloat::Value(rect.width),
            AnimatableFloat::Value(rect.height),
        ];
        composite_tree.mark_dirty(self.ct_root);
        ht_manager.get_data_mut(self.ht_root).left = rect.left;
        ht_manager.get_data_mut(self.ht_root).top = rect.top;
        ht_manager.get_data_mut(self.ht_root).width = rect.width;
        ht_manager.get_data_mut(self.ht_root).height = rect.height;

        let content_size = Size::new_logical(
            rect.width,
            rect.height - 16.0 - PaneGroupTabView::PADDING_Y * 2.0,
        );
        for (c, _) in self.contents.borrow().iter() {
            c.resize(&content_size, composite_tree, ht_manager);
        }
    }
}

struct PaneGroupTabView {
    ht_root: HitTestTreeRef,
    size: Size<LogicalUnit>,
    eh: Rc<PaneGroupTabEventHandler>,
}
impl PaneGroupTabView {
    const PADDING_X: f32 = 8.0;
    const PADDING_Y: f32 = 4.0;
    const ROUNDING: f32 = 8.0;

    fn new(
        ctx: &mut ViewInitContext,
        label: String,
        group_controller: std::rc::Weak<PaneGroupViewController>,
    ) -> Self {
        let active_gradient = pane_group_tab_active_gradient(ctx.composite_tree);
        let tw =
            TextLayout::measure_visual_width(&label, FontID::UIDefault, ctx.system_link.font_set());
        let size = Size::new_logical(tw + Self::PADDING_X * 2.0, 16.0 + Self::PADDING_Y * 2.0);

        let ct_root = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            size: [
                AnimatableFloat::Value(size.width),
                AnimatableFloat::Value(size.height),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            corner_radius: CornerRadius::all(Self::ROUNDING),
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: label,
                    ..Default::default()
                }],
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_underline = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            relative_size_adjustment: [1.0, 1.0],
            relative_offset_adjustment: [0.0, 0.0],
            has_bitmap: true,
            composite_mode: CompositeMode::FillLinearGradient(active_gradient),
            corner_radius: CornerRadius::all(Self::ROUNDING),
            scale_x: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            width: size.width,
            height: size.height,
            cursor_shape: CursorShape::Pointer,
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_root, ct_underline);

        let eh = Rc::new(PaneGroupTabEventHandler {
            ct_root,
            ct_underline,
            active: Cell::new(false),
            group_controller,
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        Self { ht_root, size, eh }
    }

    pub fn teardown(self, ctx: &mut MountContext) {
        self.unmount(ctx);
        ctx.composite_tree.free_all(self.eh.ct_root);
        ctx.ht_manager.free_all(self.ht_root);
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.ht_root);
    }

    pub fn unmount(&self, ctx: &mut MountContext) {
        ctx.composite_tree.remove_child(self.eh.ct_root);
        ctx.ht_manager.remove_child(self.ht_root);
    }

    pub fn place<E>(
        &self,
        pos: Point<LogicalUnit>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        composite_tree.get_mut(self.eh.ct_root).offset =
            [AnimatableFloat::Value(pos.x), AnimatableFloat::Value(pos.y)];
        composite_tree.mark_dirty(self.eh.ct_root);
        ht_manager.get_data_mut(self.ht_root).left = pos.x;
        ht_manager.get_data_mut(self.ht_root).top = pos.y;
    }
}

struct PaneGroupTabEventHandler {
    ct_root: CompositeTreeRef,
    ct_underline: CompositeTreeRef,
    active: Cell<bool>,
    group_controller: std::rc::Weak<PaneGroupViewController>,
}
impl HitTestTreeActionHandler for PaneGroupTabEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        context.composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.5],
                start_sec: context.current_sec,
                end_sec: context.current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            });
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        context.composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.5],
                to_value: [1.0, 1.0, 1.0, 0.0],
                start_sec: context.current_sec,
                end_sec: context.current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            });
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if args.button == PointerButton::Primary {
            if let Some(gc) = self.group_controller.upgrade() {
                gc.select_tab(self, context.system_link.event_dispatcher());
            }
        } else {
            context.system_link.dispatch_event(Event::MenuOpen {
                parent: context
                    .ht_manager
                    .query_root_window(sender)
                    .expect("not mounted"),
                items: vec![
                    crate::uikit::MenuItem::Command {
                        label: "Entry1".into(),
                        command_id: 0,
                    },
                    crate::uikit::MenuItem::Command {
                        label: "Entry2".into(),
                        command_id: 1,
                    },
                    crate::uikit::MenuItem::Separator,
                    crate::uikit::MenuItem::Command {
                        label: "Entry3".into(),
                        command_id: 2,
                    },
                    crate::uikit::MenuItem::Heading {
                        label: "Head".into(),
                    },
                    crate::uikit::MenuItem::SubMenu {
                        label: "Sub".into(),
                        items: vec![crate::uikit::MenuItem::Command {
                            label: "SubEntry1".into(),
                            command_id: 4,
                        }],
                    },
                    crate::uikit::MenuItem::Command {
                        label: "Entry4".into(),
                        command_id: 3,
                    },
                ],
                surface_pos: args.client_pos,
            });
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_start(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> input::EventContinueControl {
        if args.button != PointerButton::Primary {
            return input::EventContinueControl::empty();
        }

        if let Some(gc) = self.group_controller.upgrade() {
            let (x, y, w, h, _) = context.ht_manager.compute_global_rect_autoroot(gc.ht_root);

            context.system_link.dispatch_event(Event::DockBeginPreview {
                window: context
                    .ht_manager
                    .query_root_window(sender)
                    .expect("not mounted"),
                pane_rect: Rect::from_lt_size(Point::new_logical(x, y), Size::new_logical(w, h)),
                client_pos: args.client_pos,
            });
        }

        input::EventContinueControl::CAPTURE_ELEMENT | input::EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> input::EventContinueControl {
        context.system_link.dispatch_event(Event::DockMovePreview {
            window: context
                .ht_manager
                .query_root_window(sender)
                .expect("not mounted"),
            client_pos: args.client_pos,
        });

        input::EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_end(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> input::EventContinueControl {
        if args.button != PointerButton::Primary {
            return input::EventContinueControl::empty();
        }

        let Some(gc) = self.group_controller.upgrade() else {
            return input::EventContinueControl::empty();
        };

        context.system_link.dispatch_event(Event::DockConfirm {
            window: context
                .ht_manager
                .query_root_window(sender)
                .expect("not mounted"),
            client_pos: args.client_pos,
            source_dock: gc.dock.get(),
            tab_index: gc.tab_index(self).expect("not in any group"),
        });

        input::EventContinueControl::RELEASE_CAPTURE_ELEMENT
            | input::EventContinueControl::STOP_PROPAGATION
    }
}
impl PaneGroupTabEventHandler {
    fn set_active<E>(&self, active: bool, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.active.replace(active) == active {
            // active not changed
            return;
        }

        if active {
            composite_tree.get_mut(self.ct_underline).scale_x = AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 1.0,
                start_sec: current_sec,
                end_sec: current_sec + 0.2,
                curve: AnimationCurve::EASE_OUT_HARD,
                event_on_complete: None,
            };
            composite_tree.get_mut(self.ct_underline).opacity = AnimatableFloat::Animated {
                from_value: 0.0,
                to_value: 1.0,
                start_sec: current_sec,
                end_sec: current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            };
            composite_tree.mark_dirty(self.ct_underline);
        } else {
            composite_tree.get_mut(self.ct_underline).scale_x = AnimatableFloat::Animated {
                from_value: 1.0,
                to_value: 0.0,
                start_sec: current_sec,
                end_sec: current_sec + 0.2,
                curve: AnimationCurve::EASE_IN,
                event_on_complete: None,
            };
            composite_tree.mark_dirty(self.ct_underline);
        }
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

pub struct UIKitPreviewPaneView {
    scroll_container: ScrollContainer,
    test_alert_btn: SimpleButtonView,
    test_alert_btn2: SimpleButtonView,
    text_input_view: TextInputView,
    text_input_view2: TextInputView,
    ml_text_editor_view: uikit::MultilineTextInputView,
    color_picker_backing_store: Rc<ColorPickerTestBackingStore>,
    color_picker: ColorPickerView,
    editable_color_button: EditableColorButtonView,
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
impl UIKitPreviewPaneView {
    pub fn new(ctx: &mut ViewInitContext) -> Self {
        let scroll_container = ScrollContainer::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(0.0, 0.0),
                Size::new_logical(128.0, 128.0),
            ),
        );

        let mut ytop = 8.0;
        let mut content_width = 8.0f32;
        let label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(8.0), AnimatableFloat::Value(ytop)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "Simple Buttons + Alert Dialog".into(),
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

        let test_alert_btn = SimpleButtonView::new(
            ctx,
            "Test Alert".into(),
            Size::new_logical(64.0, 24.0),
            Some(Box::new(AlertButtonEventHandler(
                "てすとめっせーじ from button\n改行もしてみる".into(),
            ))),
        );
        test_alert_btn.locate(
            &Positioning {
                parent_anchor: [0.0, 0.0],
                anchor: [0.0, 0.0],
                offset: [16.0, ytop],
            },
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );
        test_alert_btn.mount(ctx, &scroll_container);

        let test_alert_btn2 = SimpleButtonView::new(
            ctx,
            "Test Alert 2".into(),
            Size::new_logical(96.0, 24.0),
            Some(Box::new(AlertButtonEventHandler("とてもとても長いメッセージで自動折り返しをしてみる ああああああああああああああああああああああああああああああ".into()))),
        );
        test_alert_btn2.locate(
            &Positioning {
                parent_anchor: [0.0, 0.0],
                anchor: [0.0, 0.0],
                offset: [88.0, ytop],
            },
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );
        test_alert_btn2.mount(ctx, &scroll_container);

        ytop += 24.0;
        ytop += 8.0;

        let label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(8.0), AnimatableFloat::Value(ytop)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "Text Input(Single Line)".into(),
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

        let text_input_view = TextInputView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0, ytop),
                Size::new_logical(128.0, 20.0),
            ),
        );
        text_input_view.mount(ctx, &scroll_container);
        // text_input_view.set_keyboard_focus_group(
        //     main_window.keyboard_focus_group(),
        //     view_init_ctx.keyboard_focus_registry,
        // );
        ytop += 24.0;

        let text_input_view2 = TextInputView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0, ytop),
                Size::new_logical(128.0, 20.0),
            ),
        );
        text_input_view2.mount(ctx, &scroll_container);
        // text_input_view2.set_keyboard_focus_group(
        //     main_window.keyboard_focus_group(),
        //     view_init_ctx.keyboard_focus_registry,
        // );
        ytop += 24.0;
        ytop += 8.0;

        let label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(8.0), AnimatableFloat::Value(ytop)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "Text Input(Multiline)".into(),
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

        let label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(8.0), AnimatableFloat::Value(ytop)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "Color Picker(Standalone)".into(),
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

        let color_picker_backing_store = Rc::new(ColorPickerTestBackingStore {
            color: Cell::new(0xffffffff),
        });
        let color_picker = ColorPickerView::new(
            ctx,
            Point::new_logical(16.0, ytop),
            &Rc::downgrade(&color_picker_backing_store),
        );
        color_picker.mount(ctx, &scroll_container);
        ytop += 128.0 + 32.0 + 16.0 + 20.0;
        ytop += 8.0;

        let label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(8.0), AnimatableFloat::Value(ytop)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "Color Picker(Button Style)".into(),
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                ..Default::default()
            }),
            ..Default::default()
        });
        ctx.composite_tree
            .add_child(scroll_container.ct_root(), label);
        let label_width = TextLayout::measure_visual_width(
            "Color Picker(Button Style)",
            FontID::UIDefault,
            ctx.system_link.font_set(),
        );

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

        let label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(8.0), AnimatableFloat::Value(ytop)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "Numeric Input".into(),
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                ..Default::default()
            }),
            ..Default::default()
        });
        ctx.composite_tree
            .add_child(scroll_container.ct_root(), label);
        let label_width = TextLayout::measure_visual_width(
            "Numeric Input",
            FontID::UIDefault,
            ctx.system_link.font_set(),
        );

        let numeric_input_view = NumericInputView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(16.0 + label_width, ytop - 2.0),
                Size::new_logical(64.0, 20.0),
            ),
        );
        numeric_input_view.mount(ctx, &scroll_container);
        // numeric_input_view.set_keyboard_focus_group(
        //     main_window.keyboard_focus_group(),
        //     view_init_ctx.keyboard_focus_registry,
        // );
        ytop += 20.0;

        let label = ctx.composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [
                AnimatableFloat::Value(8.0),
                AnimatableFloat::Value(ytop + 4.0),
            ],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: "Dropdown".into(),
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                ..Default::default()
            }),
            ..Default::default()
        });
        ctx.composite_tree
            .add_child(scroll_container.ct_root(), label);
        let label_width = TextLayout::measure_visual_width(
            "Dropdown",
            FontID::UIDefault,
            ctx.system_link.font_set(),
        );

        let dropdown_box = uikit::dropdown_box::View::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(label_width + 16.0, ytop),
                Size::new_logical(128.0, 24.0),
            ),
            vec![
                "DropdownBox Item 1".into(),
                "DropdownBox Item 2".into(),
                "DropdownBox Item 3 too long version".into(),
            ],
        );
        dropdown_box.mount(ctx, &scroll_container);
        ytop += 28.0;

        let toggle_button = uikit::ToggleButtonView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(8.0, ytop),
                Size::new_logical(128.0, 24.0),
            ),
            "Toggle / Checkbox".into(),
        );
        toggle_button.mount(ctx, &scroll_container);

        let checkbox = uikit::CheckboxView::new(
            ctx,
            Rect::from_lt_size(
                Point::new_logical(144.0, ytop + 4.0),
                Size::new_logical(16.0, 16.0),
            ),
        );
        checkbox.mount(ctx, &scroll_container);
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

        // test_alert_btn.set_keyboard_focus_group(
        //     main_window.keyboard_focus_group(),
        //     ctx.keyboard_focus_registry,
        // );
        // test_alert_btn2.set_keyboard_focus_group(
        //     main_window.keyboard_focus_group(),
        //     ctx.keyboard_focus_registry,
        // );

        scroll_container.set_content_size(
            Size::new_logical(content_width, ytop + 8.0),
            ctx.mount_context.composite_tree,
            ctx.mount_context.ht_manager,
        );
        Self {
            scroll_container,
            test_alert_btn,
            test_alert_btn2,
            text_input_view,
            text_input_view2,
            ml_text_editor_view,
            color_picker_backing_store,
            color_picker,
            editable_color_button,
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
impl PaneView for UIKitPreviewPaneView {
    fn name(&self) -> String {
        "uikit on stage".into()
    }

    fn mount(&self, ctx: &mut MountContext, target: &RawMountTarget) {
        self.scroll_container.mount(ctx, target);
    }

    fn unmount(&self, ctx: &mut MountContext) {
        self.scroll_container.unmount(ctx);
    }

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

struct PerWindowData {
    screen_reposition_interests: HashSet<HitTestTreeRef>,
    header: ui::window_header::View,
    docking_manager: DockingManager,
}

struct LaunchArgs<'sys> {
    pub event_queue: EventQueue,
    pub global_time_base: &'sys std::time::Instant,
    pub renderer_sync: &'sys Mutex<RendererSync>,
}

crate::perf_section!(INITIALIZE = "LogicFiber.Initialize");
crate::perf_section!(PROCESS_EVENT = "LogicFiber.ProcessEvent");

#[tracing::instrument(target = "peridot_marble_editor::logic_fiber", skip_all)]
async fn run<'sys>(
    LaunchArgs {
        event_queue,
        global_time_base,
        renderer_sync,
    }: LaunchArgs<'sys>,
    mut system_link: SystemLink<'sys>,
) {
    tracing::info!("app start");
    crate::perf_begin!(perf = INITIALIZE);

    let drag_preview_popover = DragPreviewPopoverHandle::new(&system_link);

    let mut composite_tree = CompositeTree::new();
    let mut ht_manager = HitTestTreeManager::new();

    let mut keyboard_focus_registry = KeyboardFocusTokenRegistry::new();
    let mut pointer_input_manager = PointerInputManager::new();

    // WindowsではWM_NCHITTESTの返り値の計算に必要なので一旦生ポインタで参照もたせる（実際どうするかはあとで考える）
    #[cfg(windows)]
    unsafe {
        platform::windows::locate_non_client_hittest_managers(&pointer_input_manager, &ht_manager);
    }

    let mut view_registry = ViewRegistry::new();

    let mut texture_id_issuer = MainThreadTextureIDIssuer::new();
    let mut popup_manager = PopupManager::new();
    let context_menu_common_resources = MenuItemCommonResources::new(
        &mut composite_tree,
        &mut texture_id_issuer,
        system_link.rt_sender(),
    );
    let mut current_active_menu_session = None::<MenuSession>;
    let mut current_active_dropdown_menu_session = None::<DropdownMenuSession>;
    let mut custom_view_flyout_session = None::<CustomViewFlyoutSession>;
    let mut delayed_render_messages = Vec::new();

    let mut main_window = system_link.create_main_window(
        &mut composite_tree,
        &mut ht_manager,
        &mut keyboard_focus_registry,
        &mut delayed_render_messages,
    );

    composite_tree.get_mut(main_window.ct_root()).composite_mode =
        CompositeMode::FillColor(AnimatableColor::Value([0.1, 0.2, 0.3, 1.0]));
    composite_tree.get_mut(main_window.ct_root()).has_bitmap = true;
    composite_tree.mark_dirty(main_window.ct_root());

    let mut view_init_ctx = ViewInitContext {
        mount_context: MountContext {
            composite_tree: &mut composite_tree,
            ht_manager: &mut ht_manager,
            current_sec: global_time_base.elapsed().as_secs_f32(),
            keyboard_focus_registry: &mut keyboard_focus_registry,
        },
        view_registry: &mut view_registry,
        ui_scale_factor: main_window.ui_scale_factor(),
        system_link: &system_link,
        main_thread_texture_id_issuer: &mut texture_id_issuer,
    };
    let window_header_view = ui::window_header::View::new(
        &mut view_init_ctx,
        ui::window_header::Caption::Main {
            project_name: "New Project".into(),
        },
        main_window.needs_system_command_buttons(),
    );
    window_header_view.mount(&mut view_init_ctx, &main_window);

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

    main_window.associate_extra_data(Box::new(PerWindowData {
        screen_reposition_interests: HashSet::new(),
        header: window_header_view,
        docking_manager: DockingManager::new(
            main_window,
            &mut view_init_ctx,
            Rect::from_lt_size(
                Point::new_logical(8.0, 460.0),
                Size::new_logical(320.0, 256.0),
            ),
            |view_init_ctx, store| {
                store.alloc_recurse(|parent_id, store| Dock::ToRight {
                    parent: None,
                    left: store.alloc(|id| {
                        let pane_group_view_contents: Vec<Box<dyn PaneView>> = vec![
                            Box::new(UIKitPreviewPaneView::new(view_init_ctx)),
                            Box::new(TestPane1View::new(view_init_ctx)),
                        ];

                        Dock::Fill {
                            group_view: PaneGroupView::new(
                                view_init_ctx,
                                pane_group_view_contents,
                                id,
                            ),
                            parent: Some(parent_id),
                        }
                    }),
                    right: store.alloc(|id| {
                        let pane_group_view_contents: Vec<Box<dyn PaneView>> = vec![
                            Box::new(TestPane1View::new(view_init_ctx)),
                            Box::new(TestPane2View::new(view_init_ctx)),
                        ];

                        Dock::Fill {
                            group_view: PaneGroupView::new(
                                view_init_ctx,
                                pane_group_view_contents,
                                id,
                            ),
                            parent: Some(parent_id),
                        }
                    }),
                    splitter: DockedPaneSplitterView::new(
                        view_init_ctx,
                        DockedPaneSplitDirection::Vertical,
                        parent_id,
                    ),
                    width: Cell::new(96.0),
                })
            },
        ),
    }));

    composite_tree.commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
    ht_manager.dump(main_window.ht_root());
    for msg in delayed_render_messages.drain(..) {
        system_link.rt_sender().send(msg).expect("rt_sender.send");
    }

    system_link.prelaunch(main_window);
    crate::perf_end!(perf);
    loop {
        let e = event_queue.next_event().await;
        tracing::trace!(target: "event-trace", event = ?e);
        crate::perf_scope!(PROCESS_EVENT, str e.p_name());
        match e {
            Event::Quit => break,
            Event::SubWindowOpen => {
                system_link.open_window(
                    Size::new_logical(320.0, 240.0),
                    main_window.ui_scale_factor(),
                    &mut composite_tree,
                    &mut ht_manager,
                    &mut keyboard_focus_registry,
                    &mut delayed_render_messages,
                    |mut w, composite_tree, ht_manager, keyboard_focus_registry, system_link| {
                        ht_manager.get_data_mut(w.ht_root()).root_of_window = Some(w);

                        composite_tree.get_mut(w.ct_root()).has_bitmap = true;
                        composite_tree.get_mut(w.ct_root()).composite_mode =
                            CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.1, 0.2, 1.0]));
                        composite_tree.mark_dirty(w.ct_root());

                        let mut view_init_ctx = ViewInitContext {
                            mount_context: MountContext {
                                composite_tree,
                                ht_manager,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                keyboard_focus_registry,
                            },
                            view_registry: &mut view_registry,
                            ui_scale_factor: w.ui_scale_factor(),
                            system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
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
                            docking_manager: DockingManager::new(
                                w,
                                &mut view_init_ctx,
                                Rect::from_lt_size(
                                    Point::new_logical(8.0, ui::window_header::View::THICKNESS),
                                    Size::new_logical(320.0, 256.0),
                                ),
                                |view_init_ctx, store| {
                                    store.alloc(|id| {
                                        let pane_group_view_contents: Vec<Box<dyn PaneView>> =
                                            vec![Box::new(TestPane1View::new(view_init_ctx))];

                                        Dock::Fill {
                                            group_view: PaneGroupView::new(
                                                view_init_ctx,
                                                pane_group_view_contents,
                                                id,
                                            ),
                                            parent: None,
                                        }
                                    })
                                },
                            ),
                        }));
                    },
                );

                let mut renderer_sync = renderer_sync.lock().expect("poisoned");
                composite_tree.commit(&mut renderer_sync.composite_buffer);
            }
            Event::SubWindowClose { mut window } => {
                tracing::trace!("subWindowClose");
                unsafe {
                    drop(window.take_extra_data::<PerWindowData>());
                }
                system_link.close_window(
                    window,
                    &mut composite_tree,
                    &mut ht_manager,
                    &mut keyboard_focus_registry,
                );
            }
            Event::WindowResize { window, size } => {
                // pointer_input_manager.set_client_size(window, size);
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
                    drag_preview_popover: &drag_preview_popover,
                    ht_manager: &ht_manager,
                };

                for &ht in wd.screen_reposition_interests.iter() {
                    if let Some(e) = ht_manager.get_data(ht).screen_reposition_handler() {
                        e.on_screen_reposition_required(ht, &mut input_context, pos);
                    }
                }

                // ContextMenuはウィンドウ移動で消しちゃう（Explorerもこの挙動っぽい）
                if let Some(c) = current_active_menu_session.take_if(|x| x.parent == window) {
                    if window == main_window {
                        app_menu_view.on_close_all(
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

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                    drag_preview_popover: &drag_preview_popover,
                    ht_manager: &ht_manager,
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
                    if window == main_window {
                        app_menu_view.on_close_all(
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

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::WindowActivatingStateChanged { window, activated } => {
                if !activated {
                    if let Some(c) = current_active_menu_session.take_if(|x| x.parent == window) {
                        if window == main_window {
                            app_menu_view.on_close_all(
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
                #[cfg(feature = "wayland")]
                event_id,
            } => {
                #[cfg(feature = "wayland")]
                drag_preview_popover.bind_parent_window(window);
                #[cfg(windows)]
                drag_preview_popover.bind_position_base_window(window);
                #[cfg(target_os = "macos")]
                drag_preview_popover.bind_position_base_window_link(window);

                // waylandの場合はここでTitleBarロールの判定をする
                // 他PFではシステム側でやってくれる/ウィンドウコールバック内でないといけない
                #[cfg(feature = "wayland")]
                if pointer_input_manager.role_focus(&ht_manager)
                    == Some(input::hittest::Role::TitleBar)
                {
                    window.begin_drag(event_id);
                }

                pointer_input_manager.handle_mouse_down(
                    pointer_id,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    button,
                    window.ht_root(),
                    &mut keyboard_focus_registry,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::PointerMove {
                pointer_id,
                window,
                client_pos,
            } => {
                pointer_input_manager.handle_mouse_move(
                    NativeDesktopSurface::Window(window),
                    pointer_id,
                    client_pos,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    window.ht_root(),
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);

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
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::PointerUp {
                window,
                pointer_id,
                button,
            } => {
                pointer_input_manager.handle_mouse_up(
                    pointer_id,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    button,
                    window.ht_root(),
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::PointerLeaveWindow { pointer_id, .. } => {
                pointer_input_manager.handle_mouse_leave(
                    pointer_id,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                );

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::PointerHover => {
                system_link.kill_pointer_hovering_timeout();
                pointer_input_manager.handle_pointer_hover(&mut InputEventContext {
                    composite_tree: &mut composite_tree,
                    current_sec: global_time_base.elapsed().as_secs_f32(),
                    system_link: &mut system_link,
                    drag_preview_popover: &drag_preview_popover,
                    ht_manager: &ht_manager,
                });

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                );

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                            drag_preview_popover: &drag_preview_popover,
                            ht_manager: &ht_manager,
                        },
                        &keyboard_focus_registry,
                    );
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    &keyboard_focus_registry,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    &keyboard_focus_registry,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::IMEStateChanges {
                window,
                committed_string,
                preedit_string,
            } => {
                window.keyboard_focus_state().handle_ime_state_changes(
                    &committed_string,
                    &preedit_string,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    &keyboard_focus_registry,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                        view_registry: &mut view_registry,
                        ui_scale_factor: main_window.ui_scale_factor(),
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                    },
                    target_window,
                    |id, ctx| AlertDialogPresenter::new(ctx, id, message, target_window),
                );
                popup_manager.post_open_action(
                    opened_id,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    &keyboard_focus_registry,
                );

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::PopupClose { id } => {
                if popup_manager.close(
                    id,
                    &mut composite_tree,
                    &mut ht_manager,
                    global_time_base.elapsed().as_secs_f32(),
                ) {
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }
            }
            Event::Sync(SyncEvent::PopupUnmount { id }) => {
                if popup_manager.unmount(
                    &mut MountContext {
                        composite_tree: &mut composite_tree,
                        ht_manager: &mut ht_manager,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        keyboard_focus_registry: &mut keyboard_focus_registry,
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
                        ui_scale_factor: main_window.ui_scale_factor(),
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
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
                        ui_scale_factor: 1.0, // updated later
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                    },
                    &mut delayed_render_messages,
                    &context_menu_common_resources,
                ));

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                        ui_scale_factor: 1.0, // updated later
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                    },
                    &mut delayed_render_messages,
                    &context_menu_common_resources,
                ));

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                        view_registry: &mut view_registry,
                        ui_scale_factor: 1.0, // updated later
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                    },
                    &mut delayed_render_messages,
                    surface_pos,
                    min_width,
                    items,
                ));

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::MenuCloseAll => {
                if let Some(c) = current_active_menu_session.take() {
                    if c.parent == main_window {
                        app_menu_view.on_close_all(
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

                if let Some(ref c) = current_active_dropdown_menu_session {
                    should_commit_ct = true;
                }

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
                            ui_scale_factor: 1.0, // updated later
                            system_link: &system_link,
                            main_thread_texture_id_issuer: &mut texture_id_issuer,
                        },
                        &mut delayed_render_messages,
                        &context_menu_common_resources,
                    );

                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }
            }
            Event::MenuPointerDown {
                pointer_id,
                target,
                button,
                #[cfg(feature = "wayland")]
                event_id,
            } => {
                // waylandの場合はここでTitleBarロールの判定をする
                // 他PFではシステム側でやってくれる/ウィンドウコールバック内でないといけない
                // TODO: Flyoutの要素としてTitleBarが必要になったときに対応
                /*#[cfg(feature = "wayland")]
                if pointer_input_manager.role_focus(&ht_manager)
                    == Some(input::hittest::Role::TitleBar)
                {
                    target.begin_drag(event_id);
                }*/

                pointer_input_manager.handle_mouse_down(
                    pointer_id,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    button,
                    target.ht_root(),
                    &mut keyboard_focus_registry,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::MenuPointerMove {
                pointer_id,
                target,
                client_pos,
            } => {
                pointer_input_manager.handle_mouse_move(
                    NativeDesktopSurface::ContextMenu(target),
                    pointer_id,
                    client_pos,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    target.ht_root(),
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);

                let cursor_shape = pointer_input_manager.cursor_shape(&ht_manager);
                system_link.set_cursor(&pointer_id, cursor_shape);
            }
            Event::MenuPointerUp {
                pointer_id,
                target,
                button,
            } => {
                pointer_input_manager.handle_mouse_up(
                    pointer_id,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                    button,
                    target.ht_root(),
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::MenuPointerLeave { pointer_id, .. } => {
                pointer_input_manager.handle_mouse_leave(
                    pointer_id,
                    &ht_manager,
                    &mut InputEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        system_link: &mut system_link,
                        drag_preview_popover: &drag_preview_popover,
                        ht_manager: &ht_manager,
                    },
                );

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::MenuSelectCommand { id } => {
                tracing::debug!(id, "ContextMenuSelectCommand");

                // コマンド選択したらとじる
                if let Some(c) = current_active_menu_session.take() {
                    if c.parent == main_window {
                        app_menu_view.on_close_all(
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
            #[cfg(not(target_os = "macos"))]
            Event::GlobalMouseClicked => {
                let mut should_commit_ct = false;

                if !system_link.any_pointer_on_context_menu() {
                    if let Some(c) = current_active_menu_session.take() {
                        if c.parent == main_window {
                            app_menu_view.on_global_mouse_click(
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

                        should_commit_ct = true;
                    }
                }

                if !system_link.any_pointer_on_dropdown_menu() {
                    if let Some(mut c) = current_active_dropdown_menu_session.take() {
                        c.close_all(
                            &system_link,
                            &mut composite_tree,
                            &mut ht_manager,
                            &mut keyboard_focus_registry,
                        );

                        should_commit_ct = true;
                    }
                }

                if !system_link.any_pointer_on_dropdown_menu() {
                    if let Some(c) = custom_view_flyout_session.take() {
                        c.terminate(
                            &system_link,
                            &mut composite_tree,
                            &mut ht_manager,
                            &mut keyboard_focus_registry,
                        );

                        should_commit_ct = true;
                    }
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
                mut window,
                controlling_dock,
                pos_client,
            } => {
                unsafe { window.extra_data_mut::<PerWindowData>() }
                    .docking_manager
                    .move_splitter(
                        controlling_dock,
                        pos_client,
                        &mut composite_tree,
                        &mut ht_manager,
                    );

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::DockBeginPreview {
                mut window,
                pane_rect,
                client_pos,
            } => {
                unsafe { window.extra_data_mut::<PerWindowData>() }
                    .docking_manager
                    .begin_preview(pane_rect, &client_pos, &drag_preview_popover);
            }
            Event::DockMovePreview {
                mut window,
                client_pos,
            } => {
                unsafe { window.extra_data_mut::<PerWindowData>() }
                    .docking_manager
                    .move_preview(client_pos, &drag_preview_popover);
            }
            Event::DockConfirm {
                mut window,
                source_dock,
                tab_index,
                client_pos,
            } => {
                let mount_target = window;
                let dm = &mut unsafe { window.extra_data_mut::<PerWindowData>() }.docking_manager;

                let (op, suggested_rect) = dm.end_preview(client_pos, &drag_preview_popover);
                let diverged_content = dm.redock(
                    source_dock,
                    tab_index,
                    op,
                    &suggested_rect,
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                        },
                        view_registry: &mut view_registry,
                        ui_scale_factor: 1.0, // updated later
                        system_link: &system_link,
                        main_thread_texture_id_issuer: &mut texture_id_issuer,
                    },
                    &mount_target,
                );

                if let Some(content) = diverged_content {
                    system_link.open_window(
                        suggested_rect.size(),
                        window.ui_scale_factor(),
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

                            composite_tree.get_mut(w.ct_root()).has_bitmap = true;
                            composite_tree.get_mut(w.ct_root()).composite_mode =
                                CompositeMode::FillColor(AnimatableColor::Value([
                                    0.0, 0.1, 0.2, 1.0,
                                ]));
                            composite_tree.mark_dirty(w.ct_root());

                            let mut view_init_ctx = ViewInitContext {
                                mount_context: MountContext {
                                    composite_tree,
                                    ht_manager,
                                    current_sec: global_time_base.elapsed().as_secs_f32(),
                                    keyboard_focus_registry,
                                },
                                view_registry: &mut view_registry,
                                ui_scale_factor: w.ui_scale_factor(),
                                system_link,
                                main_thread_texture_id_issuer: &mut texture_id_issuer,
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
                                docking_manager: DockingManager::new(
                                    w,
                                    &mut view_init_ctx,
                                    Rect::from_lt_size(
                                        Point::new_logical(8.0, ui::window_header::View::THICKNESS),
                                        Size::new_logical(320.0, 256.0),
                                    ),
                                    |view_init_ctx, store| {
                                        store.alloc(|id| Dock::Fill {
                                            group_view: PaneGroupView::new(
                                                view_init_ctx,
                                                vec![content],
                                                id,
                                            ),
                                            parent: None,
                                        })
                                    },
                                ),
                            }));
                        },
                    );
                }

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                                drag_preview_popover: &drag_preview_popover,
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
                                drag_preview_popover: &drag_preview_popover,
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
                                drag_preview_popover: &drag_preview_popover,
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

    tracing::info!("app finish");
    #[cfg(windows)]
    unsafe {
        platform::windows::unlocate_non_client_hittest_managers();
    }
}

pub trait FlyoutSurfaceView {
    fn mount(&self, mount_context: &mut MountContext, surface: FlyoutSurfaceHandle);

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
        let view = view_constructor.create(view_init_context);
        view.mount(view_init_context, surface);

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
        system_link.context_menu.observe_global_click();

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
        system_link.context_menu.unobserve_global_click();
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

#[cfg(windows)]
pub type SystemLink<'sys> = platform::windows::SystemLink<'sys>;

#[cfg(not(windows))]
pub struct SystemLink<'sys> {
    vk_device: *const VulkanDevice<'sys>,
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
    pub context_menu: platform::mac::context_menu::SharedState,
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
pub type PointerID = platform::mac::PointerID;

#[cfg(target_os = "macos")]
pub type DragPreviewPopoverHandle = platform::mac::DragPreviewPopoverHandle;

#[cfg(windows)]
pub use platform::windows::{
    DragPreviewPopoverHandle, PointerID, WindowHandle,
    flyout_surface::Handle as FlyoutSurfaceHandle,
};
#[cfg(target_os = "macos")]
pub type WindowHandle = platform::mac::WindowHandle;
#[cfg(feature = "wayland")]
pub use platform::unix::wayland::{
    DragPreviewPopoverHandle, FlyoutSurfaceHandle, PointerID, ToplevelHandle as WindowHandle,
};

#[cfg(target_os = "macos")]
pub type FlyoutSurfaceHandle = platform::mac::context_menu::Handle;

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
        match self.efd.take() {
            // WouldBlock(EAGAIN)はでてきてもOK
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => Ok(()),
            Err(e) => Err(e),
            Ok(_) => Ok(()),
        }
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
impl DragPreviewPopoverHandle {
    pub const BG_COLOR: Color32 = Color32 {
        r: 16,
        g: 176,
        b: 255,
        a: 16,
    };
}

pub struct FileSystem {
    resources_base_path: PathBuf,
    cache_base_path: PathBuf,
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
        #[cfg(windows)]
        let cache_base_path = {
            let base =
                PathBuf::from(std::env::var_os("LOCALAPPDATA").expect("fs.cache_base_path.no_env"));
            let p = base.join("peridot/.editor");

            p
        };

        if let Err(e) = std::fs::create_dir_all(&cache_base_path) {
            tracing::error!(reason = %e, "fs.cache_base_path.create_dir_all");
        }

        tracing::info!(
            resources_base_path = %resources_base_path.display(),
            cache_base_path = %cache_base_path.display(),
            "filesystem initialized"
        );

        Self {
            resources_base_path,
            cache_base_path,
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
