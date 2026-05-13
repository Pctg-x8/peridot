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
    collections::{HashSet, VecDeque},
    path::{Path, PathBuf},
    rc::Rc,
    sync::Mutex,
};
#[cfg(target_os = "macos")]
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

use crate::{
    graphics::VulkanDevice,
    input::{
        EventContinueControl, InputEventContext, KeyInputCode, KeyboardFocusGroupRef,
        KeyboardFocusTokenRegistry, ModifierKey, NativeDesktopSurface, PointerInputManager,
        PointerInputUnit,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager,
            HitTestTreeRef, PointerActionArgs, PointerButton, PointerButtonActionArgs,
            ScrollWheelActionArgs, ScrollWheelActionResponse,
        },
    },
    rendering::{
        MainThreadTextureIDIssuer, RenderMessage, RenderThread, RendererSync,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTree,
            CompositeTreeRef, CompositeTreeSyncBuffer, CornerRadius, Gradient,
        },
        text::{FontID, FontSet, TextLayout},
    },
    uikit::{
        MenuBaseSurfaceEventHandler, MenuItem, MenuItemCommonResources, MenuItemLayout,
        MenuItemView, MountContext, MountTarget, OverlayPopupBasicFrameView,
        OverlayPopupBasicMaskView, Popup, PopupID, PopupManager, Positioning, RawMountTarget,
        ScrollContainer, SimpleButtonView, TextInputView, ViewEventHandler, ViewIdentifier,
        ViewInitContext, ViewRegistry, ViewUpdateContext,
    },
    utils::{Color32, InteriorMutableLogicalUnit, LogicalUnit, Point, Rect, SafeF32, Size},
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
    rt_sender: std::sync::mpsc::Sender<RenderMessage>,
    rt_receiver: std::sync::mpsc::Receiver<RenderMessage>,
    root_font_set: FontSet,
    #[cfg(windows)] app_context: &'sys platform::windows::ApplicationContext,
    #[cfg(windows)] dx_context: &'sys platform::windows::DxContext,
    #[cfg(feature = "wayland")] dp_context: &'sys mut platform::unix::wayland::DisplayServerContext,
    #[cfg(feature = "wayland")] static_pixbufs: &'sys platform::unix::wayland::StaticPixbufs,
    #[cfg(target_os = "linux")] dbus: &'sys dbus::Connection,
) {
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

    #[cfg(windows)]
    let mut pointer_hovering_timer_id = 0;
    #[cfg(windows)]
    let mut context_menu_delayed_action_timer_id = core::pin::pin!(0);
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
            pointer_hovering_timer_id: &mut pointer_hovering_timer_id,
            flyout_surface_context: platform::windows::flyout_surface::SharedState::new(
                app_context,
                &dx_context,
                context_menu_delayed_action_timer_id.as_mut(),
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
            context_menu: platform::unix::wayland::flyout_surface::SharedState {
                delayed_action_timer,
            },
            #[cfg(target_os = "macos")]
            context_menu: platform::mac::context_menu::SharedState {
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
                } else if e.value() == 1 {
                    terminate_signal = true;
                } else if e.value() == 2 {
                    events_signal = true;
                } else if e.value() == 3 {
                    pointer_hovering_timer_signal = true;
                } else if e.value() == 4 {
                    delayed_action_timer_signal = true;
                } else if e.value() >= 10 && e.value() < 10 + 32 {
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
                } else if let Some(&wr) = unsafe { (*poll_id_to_watch_ref.get()).get(&e.value()) } {
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
                app_event_dispatcher.dispatch(Event::ContextMenuPerformDelayedAction);
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
        let handles = [sync_event_bus.event_notify];
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

            if r == windows::Win32::Foundation::WAIT_OBJECT_0 {
                sync_event_bus.redispatch(&app_event_dispatcher);
            } else if r.0 == windows::Win32::Foundation::WAIT_OBJECT_0.0 + handles.len() as u32 {
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
                    if windows::Win32::UI::WindowsAndMessaging::WM_LBUTTONDOWN <= msg.message
                        && msg.message <= windows::Win32::UI::WindowsAndMessaging::WM_MBUTTONDBLCLK
                    {
                        app_event_dispatcher.dispatch(Event::GlobalMouseClicked);
                    }
                    if msg.message == windows::Win32::UI::WindowsAndMessaging::WM_TIMER
                        && msg.wParam.0 == pointer_hovering_timer_id
                    {
                        app_event_dispatcher.dispatch(Event::PointerHover);
                        continue;
                    }
                    if msg.message == windows::Win32::UI::WindowsAndMessaging::WM_TIMER
                        && msg.wParam.0 == *context_menu_delayed_action_timer_id
                    {
                        app_event_dispatcher.dispatch(Event::ContextMenuPerformDelayedAction);
                        continue;
                    }

                    unsafe {
                        let _ = windows::Win32::UI::WindowsAndMessaging::TranslateMessage(msg);
                        windows::Win32::UI::WindowsAndMessaging::DispatchMessageW(msg);
                    }
                }
            } else if r == windows::Win32::Foundation::WAIT_FAILED {
                panic!(
                    "unrecoverable MsgWaitForMultipleObjectsEx error: {}",
                    std::io::Error::last_os_error()
                );
            } else {
                tracing::warn!(?r, "unhandled mwmo result");
            }
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
    WindowPostResizeRenderBuffer { window: WindowHandle },
    ContextMenuPostResizeRenderBuffer { target: FlyoutSurfaceHandle },
    PopupUnmount { id: PopupID },
}
impl SyncEvent {
    pub const fn p_name(&self) -> &'static str {
        match self {
            Self::WindowPostResizeRenderBuffer { .. } => "Sync(WindowPostResizeRenderBuffer)",
            Self::ContextMenuPostResizeRenderBuffer { .. } => {
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
    ContextMenuOpen {
        parent: WindowHandle,
        items: Vec<MenuItem>,
        surface_pos: Point<LogicalUnit>,
    },
    DropdownMenuOpen {
        parent: WindowHandle,
        surface_pos: Point<LogicalUnit>,
        min_width: f32,
        items: Vec<DropdownMenuItem>,
        selection_receiver: std::rc::Weak<DropdownBoxEventHandler>,
    },
    ContextMenuCloseAll,
    ContextMenuRescale {
        scale: f32,
    },
    ContextMenuSelectItem {
        depth: usize,
        index: usize,
    },
    ContextMenuDeselectItem {
        depth: usize,
    },
    ContextMenuOpenSubmenu {
        depth: usize,
        index: usize,
    },
    ContextMenuPerformDelayedAction,
    ContextMenuPointerDown {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
        button: PointerButton,
        #[cfg(feature = "wayland")]
        event_id: platform::unix::wayland::PointerEventID,
    },
    ContextMenuPointerMove {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
        client_pos: Point<PointerInputUnit>,
    },
    ContextMenuPointerUp {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
        button: PointerButton,
    },
    ContextMenuPointerLeave {
        pointer_id: PointerID,
        target: FlyoutSurfaceHandle,
    },
    ContextMenuSelectCommand {
        id: u64,
    },
    DropdownMenuSelectItem {
        id: usize,
        receiver: std::rc::Weak<DropdownBoxEventHandler>,
    },
    UpdateView {
        id: ViewIdentifier,
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
            Self::ContextMenuOpen { .. } => "ContextMenuOpen",
            Self::DropdownMenuOpen { .. } => "DropdownMenuOpen",
            Self::ContextMenuCloseAll => "ContextMenuCloseAll",
            Self::ContextMenuRescale { .. } => "ContextMenuRescale",
            Self::ContextMenuSelectItem { .. } => "ContextMenuSelectItem",
            Self::ContextMenuDeselectItem { .. } => "ContextMenuDeselectItem",
            Self::ContextMenuOpenSubmenu { .. } => "ContextMenuOpenSubmenu",
            Self::ContextMenuPerformDelayedAction => "ContextMenuPerformDelayedAction",
            Self::ContextMenuPointerDown { .. } => "ContextMenuPointerDown",
            Self::ContextMenuPointerMove { .. } => "ContextMenuPointerMove",
            Self::ContextMenuPointerUp { .. } => "ContextMenuPointerUp",
            Self::ContextMenuPointerLeave { .. } => "ContextMenuPointerLeave",
            Self::ContextMenuSelectCommand { .. } => "ContextMenuSelectCommand",
            Self::DropdownMenuSelectItem { .. } => "DropdownMenuSelectItem",
            Self::UpdateView { .. } => "UpdateView",
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
    pub fn new(ctx: &mut ViewInitContext, popup_id: PopupID, message: String) -> Self {
        let mask = OverlayPopupBasicMaskView::new(ctx);
        // TODO: サイズをmessageの長さにあわせる必要がある どう計測したものか......(あるいは固定サイズにして折り返させるか？)
        let frame = OverlayPopupBasicFrameView::new(ctx, Size::new_logical(200.0, 88.0));
        let confirm_button = SimpleButtonView::new(
            ctx,
            "OK".into(),
            Size::new_logical(64.0, 24.0),
            Some(Event::PopupClose { id: popup_id }),
        );
        let ct_message = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(64.0), AnimatableFloat::Value(16.0)],
            relative_offset_adjustment: [0.5, 0.0],
            offset: [AnimatableFloat::Value(-32.0), AnimatableFloat::Value(16.0)],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    font_id: FontID::UIDefault,
                    content: message,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    spacing_inline_start: 0.0,
                }],
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });

        confirm_button.locate(
            &Positioning {
                parent_anchor: [0.5, 1.0],
                anchor: [0.5, 1.0],
                offset: [0.0, -16.0],
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

    fn rescale(&self, scale: f32, composite_tree: &mut CompositeTree<SyncEvent>) {
        self.frame.rescale(scale, composite_tree);
        self.confirm_button.rescale(scale, composite_tree);
        composite_tree.get_mut(self.ct_message).base_scale_factor = scale;
        composite_tree.mark_dirty_all(self.ct_message);
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

pub struct DropdownBoxViewSharedResources {
    down_arrow_tex: usize,
}
impl DropdownBoxViewSharedResources {
    const DOWN_ARROW_TEX_SIZE: f32 = 16.0;
    const DOWN_ARROW_TEX_VERTICES: &'static [[f32; 2]] =
        &[[0.25, 0.375], [0.75, 0.375], [0.5, 0.625]];
    const DOWN_ARROW_TEX_INDICES: &'static [u16] = &[0, 1, 2];

    pub fn new(
        id_issuer: &mut MainThreadTextureIDIssuer,
        rt_sender: &std::sync::mpsc::Sender<RenderMessage>,
    ) -> Self {
        let down_arrow_tex = id_issuer.issue();
        rt_sender
            .send(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                id: down_arrow_tex,
                vertices: Self::DOWN_ARROW_TEX_VERTICES,
                indices: Self::DOWN_ARROW_TEX_INDICES,
                width: Self::DOWN_ARROW_TEX_SIZE,
                height: Self::DOWN_ARROW_TEX_SIZE,
            })
            .expect("rt_sender.send");

        Self { down_arrow_tex }
    }
}

pub struct DropdownBoxView {
    eh: Rc<DropdownBoxEventHandler>,
    ct_text_clip: CompositeTreeRef,
}
impl DropdownBoxView {
    pub fn new(
        ctx: &mut ViewInitContext,
        shared_res: &DropdownBoxViewSharedResources,
        items: Vec<String>,
    ) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            offset: [AnimatableFloat::Value(200.0), AnimatableFloat::Value(24.0)],
            size: [AnimatableFloat::Value(128.0), AnimatableFloat::Value(24.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            corner_radius: CornerRadius::all(4.0),
            border: Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_text_clip = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            relative_size_adjustment: [1.0, 1.0],
            size: [AnimatableFloat::Value(-12.0), AnimatableFloat::Value(0.0)],
            clip_child: Some(ClipConfig {
                left_softness: SafeF32::ZERO,
                right_softness: unsafe { SafeF32::new_unchecked(12.0) },
                top_softness: SafeF32::ZERO,
                bottom_softness: SafeF32::ZERO,
            }),
            ..Default::default()
        });
        let ct_text = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            relative_size_adjustment: [1.0, 1.0],
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: if items.is_empty() {
                        ""
                    } else {
                        items[0].as_str()
                    }
                    .into(),
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Start,
                offset: [4.0, 0.0],
                ..Default::default()
            }),
            ..Default::default()
        });
        let ct_down_arrow = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            offset: [AnimatableFloat::Value(-20.0), AnimatableFloat::Value(-8.0)],
            relative_offset_adjustment: [1.0, 0.5],
            size: [AnimatableFloat::Value(16.0), AnimatableFloat::Value(16.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::ColorTint(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            texatlas_rect_id: Some(shared_res.down_arrow_tex),
            ..Default::default()
        });
        ctx.composite_tree.add_child(ct_text_clip, ct_text);
        ctx.composite_tree.add_child(ct_root, ct_text_clip);
        ctx.composite_tree.add_child(ct_root, ct_down_arrow);

        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            left: 200.0,
            top: 24.0,
            width: 128.0,
            height: 24.0,
            ..Default::default()
        });

        let eh = Rc::new_cyclic(|w| DropdownBoxEventHandler {
            this_weakref: w.clone(),
            ct_root,
            ct_text,
            ct_down_arrow,
            ht_root,
            items,
            current_selected: core::cell::Cell::new(0),
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        Self { eh, ct_text_clip }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }

    pub fn rescale<E>(&self, new_scale: f32, composite_tree: &mut CompositeTree<E>) {
        composite_tree.get_mut(self.eh.ct_root).base_scale_factor = new_scale;
        composite_tree.get_mut(self.ct_text_clip).base_scale_factor = new_scale;
        composite_tree.get_mut(self.eh.ct_text).base_scale_factor = new_scale;
        composite_tree
            .get_mut(self.eh.ct_down_arrow)
            .base_scale_factor = new_scale;

        composite_tree.mark_dirty(self.eh.ct_root);
        composite_tree.mark_dirty_all(self.ct_text_clip);
        composite_tree.mark_dirty_all(self.eh.ct_text);
        composite_tree.mark_dirty(self.eh.ct_down_arrow);
    }
}

pub struct DropdownBoxEventHandler {
    this_weakref: std::rc::Weak<DropdownBoxEventHandler>,
    ct_root: CompositeTreeRef,
    ct_text: CompositeTreeRef,
    ct_down_arrow: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    items: Vec<String>,
    current_selected: core::cell::Cell<usize>,
}
impl HitTestTreeActionHandler for DropdownBoxEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        context.composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.0625],
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
                from_value: [1.0, 1.0, 1.0, 0.0625],
                to_value: [1.0, 1.0, 1.0, 0.0],
                start_sec: context.current_sec,
                end_sec: context.current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            });
        context.composite_tree.mark_dirty(self.ct_root);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        context.composite_tree.get_mut(self.ct_down_arrow).offset[1] = AnimatableFloat::Animated {
            from_value: -8.0,
            to_value: -7.0,
            start_sec: context.current_sec,
            end_sec: context.current_sec + 0.1,
            curve: AnimationCurve::EASE_OUT,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_down_arrow);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        context.composite_tree.get_mut(self.ct_down_arrow).offset[1] = AnimatableFloat::Animated {
            from_value: -7.0,
            to_value: -8.0,
            start_sec: context.current_sec,
            end_sec: context.current_sec + 0.1,
            curve: AnimationCurve::EASE_OUT,
            event_on_complete: None,
        };
        context.composite_tree.mark_dirty(self.ct_down_arrow);

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        let w = context
            .ht_manager
            .query_root_window(self.ht_root)
            .expect("not mounted");
        let (x, y) = context.ht_manager.translate_tree_local_to_root(
            self.ht_root,
            0.0,
            24.0,
            w.client_size().width,
            w.client_size().height,
        );

        context.system_link.dispatch_event(Event::DropdownMenuOpen {
            parent: w,
            surface_pos: Point::new_logical(x, y),
            min_width: 128.0,
            items: self
                .items
                .iter()
                .enumerate()
                .map(|(n, c)| DropdownMenuItem {
                    content: c.into(),
                    id: n,
                })
                .collect(),
            selection_receiver: self.this_weakref.clone(),
        });

        EventContinueControl::STOP_PROPAGATION
    }
}
impl DropdownBoxEventHandler {
    pub fn set_selection_id<E>(&self, id: usize, composite_tree: &mut CompositeTree<E>) {
        self.current_selected.set(id);
        self.update_text(composite_tree);
    }

    fn update_text<E>(&self, composite_tree: &mut CompositeTree<E>) {
        let content = if self.items.is_empty() {
            ""
        } else {
            self.items[self.current_selected.get()].as_str()
        };
        composite_tree
            .get_mut(self.ct_text)
            .text
            .as_mut()
            .expect("no text set?")
            .runs[0]
            .content = content.into();
        composite_tree.mark_text_layout_dirty(self.ct_text);
    }
}

struct PerWindowData {
    screen_reposition_interests: HashSet<HitTestTreeRef>,
    header: ui::window_header::View,
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
    let texture_id_set = ui::window_header::SystemCommandTextureIDSet::new(
        &mut texture_id_issuer,
        system_link.rt_sender(),
    );
    let mut popup_manager = PopupManager::new();
    let context_menu_common_resources = MenuItemCommonResources::new(
        &mut composite_tree,
        &mut texture_id_issuer,
        system_link.rt_sender(),
    );
    let mut current_active_context_menu_session = None::<ContextMenuSession>;
    let mut current_active_dropdown_menu_session = None::<DropdownMenuSession>;

    let mut main_window = system_link.create_main_window(
        &mut composite_tree,
        &mut ht_manager,
        &mut keyboard_focus_registry,
    );

    composite_tree
        .get_mut(main_window.composite_root())
        .composite_mode = CompositeMode::FillColor(AnimatableColor::Value([0.1, 0.2, 0.3, 1.0]));
    composite_tree
        .get_mut(main_window.composite_root())
        .has_bitmap = true;
    composite_tree.mark_dirty(main_window.composite_root());

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
    };
    let window_header_view = ui::window_header::View::new(
        &mut view_init_ctx,
        ui::window_header::Caption::Main {
            project_name: "New Project".into(),
        },
        &texture_id_set,
        main_window.needs_system_command_buttons(),
    );
    window_header_view.mount(&mut view_init_ctx, &main_window);

    main_window.associate_extra_data(Box::new(PerWindowData {
        screen_reposition_interests: HashSet::new(),
        header: window_header_view,
    }));

    // tab view
    let tab_main = view_init_ctx.composite_tree.create(CompositeRect {
        has_bitmap: true,
        base_scale_factor: main_window.ui_scale_factor(),
        composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
        size: [AnimatableFloat::Value(100.0), AnimatableFloat::Value(36.0)],
        offset: [AnimatableFloat::Value(100.0), AnimatableFloat::Value(100.0)],
        text: Some(CompositeRectText {
            runs: vec![CompositeRectTextRun {
                font_id: FontID::UIDefault,
                content: "tab".into(),
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }],
            horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
            vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
            ..Default::default()
        }),
        ..Default::default()
    });
    let tab_bg_grad = view_init_ctx
        .composite_tree
        .create_gradient(Gradient::Radial {
            start_color: [1.0, 1.0, 1.0, 1.0],
            end_color: [1.0, 1.0, 1.0, 0.0],
            center_relative: [0.5, 0.5],
            radius: [0.5, 0.1],
        });
    let tab_bg = view_init_ctx.composite_tree.create(CompositeRect {
        base_scale_factor: main_window.ui_scale_factor(),
        relative_size_adjustment: [1.0, 1.0],
        has_bitmap: true,
        composite_mode: CompositeMode::FillRadialGradient(tab_bg_grad),
        ..Default::default()
    });
    view_init_ctx.composite_tree.add_child(tab_main, tab_bg);
    view_init_ctx
        .composite_tree
        .add_child(main_window.composite_root(), tab_main);
    let ht_tab_main = view_init_ctx.ht_manager.create(HitTestTreeData {
        left: 100.0,
        top: 100.0,
        width: 100.0,
        height: 36.0,
        cursor_shape: CursorShape::Pointer,
        ..Default::default()
    });
    view_init_ctx
        .ht_manager
        .add_child(main_window.ht_root(), ht_tab_main);

    struct TabHitAction {
        ct: CompositeTreeRef,
        ht: HitTestTreeRef,
    }
    impl HitTestTreeActionHandler for TabHitAction {
        fn on_pointer_enter(
            &self,
            sender: HitTestTreeRef,
            context: &mut InputEventContext,
            args: &PointerActionArgs,
        ) -> input::EventContinueControl {
            context.composite_tree.get_mut(self.ct).composite_mode =
                CompositeMode::FillColor(AnimatableColor::Animated {
                    start_sec: context.current_sec,
                    end_sec: context.current_sec + 0.1,
                    from_value: [1.0, 1.0, 1.0, 0.0],
                    to_value: [1.0, 1.0, 1.0, 0.25],
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                });
            context.composite_tree.mark_dirty(self.ct);

            input::EventContinueControl::STOP_PROPAGATION
        }

        fn on_pointer_leave(
            &self,
            sender: HitTestTreeRef,
            context: &mut InputEventContext,
            args: &PointerActionArgs,
        ) -> input::EventContinueControl {
            context.composite_tree.get_mut(self.ct).composite_mode =
                CompositeMode::FillColor(AnimatableColor::Animated {
                    start_sec: context.current_sec,
                    end_sec: context.current_sec + 0.1,
                    from_value: [1.0, 1.0, 1.0, 0.25],
                    to_value: [1.0, 1.0, 1.0, 0.0],
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                });
            context.composite_tree.mark_dirty(self.ct);

            input::EventContinueControl::STOP_PROPAGATION
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

            context
                .drag_preview_popover
                .show(&args.client_pos, &Size::new_logical(128.0, 128.0));

            input::EventContinueControl::CAPTURE_ELEMENT
                | input::EventContinueControl::STOP_PROPAGATION
        }

        fn on_drag_move(
            &self,
            sender: HitTestTreeRef,
            context: &mut InputEventContext,
            args: &PointerActionArgs,
        ) -> input::EventContinueControl {
            context.drag_preview_popover.r#move(&args.client_pos);

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

            context.drag_preview_popover.hide();

            input::EventContinueControl::RELEASE_CAPTURE_ELEMENT
                | input::EventContinueControl::STOP_PROPAGATION
        }

        fn on_click(
            &self,
            sender: HitTestTreeRef,
            context: &mut InputEventContext,
            args: &PointerButtonActionArgs,
        ) -> input::EventContinueControl {
            if args.button == PointerButton::Primary {
                context.system_link.dispatch_event(Event::SubWindowOpen);

                input::EventContinueControl::STOP_PROPAGATION
            } else {
                context.system_link.dispatch_event(Event::ContextMenuOpen {
                    parent: context
                        .ht_manager
                        .query_root_window(self.ht)
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

                input::EventContinueControl::STOP_PROPAGATION
            }
        }
    }
    let ht_action_handler = std::rc::Rc::new(TabHitAction {
        ct: tab_main,
        ht: ht_tab_main,
    });
    view_init_ctx
        .ht_manager
        .set_action_handler(ht_tab_main, &ht_action_handler);

    let test_alert_btn = SimpleButtonView::new(
        &mut view_init_ctx,
        "Test Alert".into(),
        Size::new_logical(64.0, 24.0),
        Some(Event::OpenAlertDialog {
            target_window: main_window,
            message: "てすとめっせーじ from button".into(),
        }),
    );
    test_alert_btn.locate(
        &Positioning {
            parent_anchor: [0.0, 0.0],
            anchor: [0.0, 0.0],
            offset: [200.0, 64.0],
        },
        &mut view_init_ctx.mount_context.composite_tree,
        &mut view_init_ctx.mount_context.ht_manager,
    );
    test_alert_btn.mount(&mut view_init_ctx, &main_window);
    test_alert_btn.set_keyboard_focus_group(
        main_window.keyboard_focus_group(),
        view_init_ctx.keyboard_focus_registry,
    );

    let text_input_view = TextInputView::new(&mut view_init_ctx, Point::new_logical(200.0, 300.0));
    text_input_view.mount(&mut view_init_ctx, &main_window);
    text_input_view.set_keyboard_focus_group(
        main_window.keyboard_focus_group(),
        view_init_ctx.keyboard_focus_registry,
    );

    let text_input_view2 = TextInputView::new(&mut view_init_ctx, Point::new_logical(200.0, 324.0));
    text_input_view2.mount(&mut view_init_ctx, &main_window);
    text_input_view2.set_keyboard_focus_group(
        main_window.keyboard_focus_group(),
        view_init_ctx.keyboard_focus_registry,
    );

    let scroll_container = ScrollContainer::new(
        &mut view_init_ctx,
        Rect::from_lt_size(
            Point::new_logical(500.0, 200.0),
            Size::new_logical(128.0, 128.0),
        ),
    );
    scroll_container.mount(&mut view_init_ctx, &main_window);

    let text_input_view3 = TextInputView::new(&mut view_init_ctx, Point::new_logical(8.0, 8.0));
    text_input_view3.mount(&mut view_init_ctx, &scroll_container);
    text_input_view3.set_keyboard_focus_group(
        main_window.keyboard_focus_group(),
        view_init_ctx.keyboard_focus_registry,
    );

    scroll_container.set_content_size(
        Size::new_logical(100.0, 400.0),
        view_init_ctx.mount_context.composite_tree,
        view_init_ctx.mount_context.ht_manager,
    );

    let dropdown_box_view_shared_res =
        DropdownBoxViewSharedResources::new(&mut texture_id_issuer, system_link.rt_sender());
    let dropdown_box = DropdownBoxView::new(
        &mut view_init_ctx,
        &dropdown_box_view_shared_res,
        vec![
            "DropdownBox Item 1".into(),
            "DropdownBox Item 2".into(),
            "DropdownBox Item 3 too long version".into(),
        ],
    );
    dropdown_box.mount(&mut view_init_ctx, &main_window);

    composite_tree.commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
    ht_manager.dump(main_window.ht_root());

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
                    &mut composite_tree,
                    &mut ht_manager,
                    &mut keyboard_focus_registry,
                    |mut w, composite_tree, ht_manager, keyboard_focus_registry, system_link| {
                        ht_manager.get_data_mut(w.ht_root()).root_of_window = Some(w);

                        composite_tree.get_mut(w.composite_root()).has_bitmap = true;
                        composite_tree.get_mut(w.composite_root()).composite_mode =
                            CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.1, 0.2, 1.0]));
                        composite_tree.mark_dirty(w.composite_root());

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
                        };
                        let window_header_view = ui::window_header::View::new(
                            &mut view_init_ctx,
                            ui::window_header::Caption::Sub,
                            &texture_id_set,
                            w.needs_system_command_buttons(),
                        );
                        window_header_view.mount(&mut view_init_ctx, &w);

                        w.associate_extra_data(Box::new(PerWindowData {
                            screen_reposition_interests: HashSet::new(),
                            header: window_header_view,
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
            Event::Sync(SyncEvent::WindowPostResizeRenderBuffer { window }) => {
                #[cfg(feature = "wayland")]
                window.update_manual_scaling();
            }
            Event::Sync(SyncEvent::ContextMenuPostResizeRenderBuffer { target }) => {
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
                if let Some(c) = current_active_context_menu_session.take_if(|x| x.parent == window)
                {
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

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::WindowRescaleUI {
                mut window,
                new_scale,
            } => {
                let wd = unsafe { window.extra_data_mut::<PerWindowData>() };
                wd.header
                    .rescale(new_scale, &mut composite_tree, &texture_id_set);

                popup_manager.rescale(window, new_scale, &mut composite_tree);

                if window == main_window {
                    // TODO: このへんそろそろいい感じに自動でやりたい スクロールコンテナとかは子Viewを含むのでまた木構造を組む必要がある
                    composite_tree.get_mut(tab_main).base_scale_factor = new_scale;
                    composite_tree.mark_dirty_all(tab_main);
                    test_alert_btn.rescale(new_scale, &mut composite_tree);
                    text_input_view.rescale(
                        &mut composite_tree,
                        window,
                        &system_link,
                        &ht_manager,
                        new_scale,
                    );
                    text_input_view2.rescale(
                        &mut composite_tree,
                        window,
                        &system_link,
                        &ht_manager,
                        new_scale,
                    );
                    dropdown_box.rescale(new_scale, &mut composite_tree);
                }

                let mut renderer_sync = renderer_sync.lock().expect("poisoned");
                composite_tree.commit(&mut renderer_sync.composite_buffer);
                system_link.notify_ui_scale_changes_to_render(window, new_scale);
            }
            Event::WindowMaximizeStateChanged {
                window,
                is_maximized,
            } => unsafe {
                window
                    .extra_data_ref::<PerWindowData>()
                    .header
                    .set_maximize_state(
                        is_maximized,
                        &mut composite_tree,
                        &mut ht_manager,
                        &texture_id_set,
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
                    drag_preview_popover: &drag_preview_popover,
                    ht_manager: &ht_manager,
                };
                let mgr = window.keyboard_focus_state_mut();

                if focused {
                    mgr.notify_window_focus(&mut input_context, &keyboard_focus_registry);
                } else {
                    mgr.notify_window_lost_focus(&mut input_context, &keyboard_focus_registry);
                }

                if !focused && let Some(c) = current_active_context_menu_session.take() {
                    // フォーカスロストした時もコンテキストメニューを閉じる
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
                    if let Some(c) =
                        current_active_context_menu_session.take_if(|x| x.parent == window)
                    {
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
                    },
                    target_window,
                    |id, ctx| AlertDialogPresenter::new(ctx, id, message),
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
            Event::ContextMenuOpen {
                parent,
                items,
                surface_pos,
            } => {
                current_active_context_menu_session = Some(ContextMenuSession::new(
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
                    },
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
                    },
                    surface_pos,
                    min_width,
                    items,
                ));

                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::ContextMenuCloseAll => {
                if let Some(c) = current_active_context_menu_session.take() {
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
            Event::ContextMenuRescale { scale } => {
                let mut should_commit_ct = false;

                if let Some(ref c) = current_active_context_menu_session {
                    c.rescale(scale, &mut composite_tree);
                    should_commit_ct = true;
                }

                if let Some(ref c) = current_active_dropdown_menu_session {
                    c.rescale(scale, &mut composite_tree);
                    should_commit_ct = true;
                }

                if should_commit_ct {
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }
            }
            Event::ContextMenuSelectItem { depth, index } => {
                if let Some(c) = current_active_context_menu_session.as_mut() {
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
            Event::ContextMenuDeselectItem { depth } => {
                if let Some(c) = current_active_context_menu_session.as_mut() {
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
            Event::ContextMenuOpenSubmenu { depth, index } => {
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
            Event::ContextMenuPerformDelayedAction => {
                system_link
                    .flyout_surface_context
                    .unreserve_delayed_action();

                if let Some(c) = current_active_context_menu_session.as_mut() {
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
                        },
                        &context_menu_common_resources,
                    );

                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }
            }
            Event::ContextMenuPointerDown {
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
            Event::ContextMenuPointerMove {
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
            Event::ContextMenuPointerUp {
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
            Event::ContextMenuPointerLeave { pointer_id, .. } => {
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
            Event::ContextMenuSelectCommand { id } => {
                tracing::debug!(id, "ContextMenuSelectCommand");

                // コマンド選択したらとじる
                if let Some(c) = current_active_context_menu_session.take() {
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
                    if let Some(c) = current_active_context_menu_session.take() {
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

                if should_commit_ct {
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
                }
            }
            Event::UpdateView { id } => {
                view_registry.call_update(
                    id,
                    &mut ViewUpdateContext {
                        composite_tree: &mut composite_tree,
                        ht_manager: &mut ht_manager,
                        keyboard_focus_registry: &keyboard_focus_registry,
                        system_link: &system_link,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                    },
                );

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
    }

    tracing::info!("app finish");
    #[cfg(windows)]
    unsafe {
        platform::windows::unlocate_non_client_hittest_managers();
    }
}

pub struct ContextMenuSurface {
    handle: FlyoutSurfaceHandle,
    item_views: Vec<MenuItemView>,
    _base_event_handler: Rc<MenuBaseSurfaceEventHandler>,
    parent_path: Vec<usize>,
    current_selecting: Option<usize>,
}
impl ContextMenuSurface {
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

#[derive(Debug, Clone)]
pub struct DropdownMenuItem {
    pub content: String,
    pub id: usize,
}

pub struct DropdownMenuItemView {
    eh: Rc<DropdownMenuItemEventHandler>,
}
impl DropdownMenuItemView {
    const ITEM_HEIGHT: f32 = 24.0;

    pub fn new(
        ctx: &mut ViewInitContext,
        selection_receiver: std::rc::Weak<DropdownBoxEventHandler>,
        item: DropdownMenuItem,
        y_pos: f32,
    ) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(y_pos)],
            relative_size_adjustment: [1.0, 0.0],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(Self::ITEM_HEIGHT),
            ],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    content: item.content,
                    color: AnimatableColor::Value([1.0; 4]),
                    ..Default::default()
                }],
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                offset: [4.0, 0.0],
                ..Default::default()
            }),
            ..Default::default()
        });
        let ht_root = ctx.mount_context.ht_manager.create(HitTestTreeData {
            top: y_pos,
            width_adjustment_factor: 1.0,
            height: Self::ITEM_HEIGHT,
            ..Default::default()
        });

        let eh = Rc::new(DropdownMenuItemEventHandler {
            ct_root,
            ht_root,
            id: item.id,
            receiver: selection_receiver,
        });
        ctx.ht_manager.set_action_handler(ht_root, &eh);

        Self { eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(target.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.eh.ht_root);
    }

    pub fn rescale<E>(&self, new_scale: f32, composite_tree: &mut CompositeTree<E>) {
        composite_tree.get_mut(self.eh.ct_root).base_scale_factor = new_scale;
        composite_tree.mark_dirty_all(self.eh.ct_root);
    }
}

pub struct DropdownMenuItemEventHandler {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    id: usize,
    receiver: std::rc::Weak<DropdownBoxEventHandler>,
}
impl HitTestTreeActionHandler for DropdownMenuItemEventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        context.composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.0625],
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
                from_value: [1.0, 1.0, 1.0, 0.0625],
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
        context
            .system_link
            .dispatch_event(Event::DropdownMenuSelectItem {
                id: self.id,
                receiver: self.receiver.clone(),
            });

        EventContinueControl::STOP_PROPAGATION
    }
}

pub struct DropdownMenuSurface {
    native_surface: FlyoutSurfaceHandle,
    item_views: Vec<DropdownMenuItemView>,
}

pub struct DropdownMenuSession {
    parent: WindowHandle,
    opening_surfaces: Vec<DropdownMenuSurface>,
}
impl DropdownMenuSession {
    pub fn new(
        selection_receiver: std::rc::Weak<DropdownBoxEventHandler>,
        parent: WindowHandle,
        syslink: &SystemLink,
        view_init_context: &mut ViewInitContext,
        pos: Point<LogicalUnit>,
        min_width: f32,
        items: Vec<DropdownMenuItem>,
    ) -> Self {
        let mut width = min_width;
        for v in items.iter() {
            width = width.max(
                TextLayout::measure_visual_width(
                    &v.content,
                    FontID::UIDefault,
                    syslink.font_set(),
                    1.0,
                ) + 4.0
                    + 4.0,
            );
        }

        let root_surface = syslink.new_flyout_surface(
            parent,
            pos,
            Size::new_logical(
                width,
                items.len() as f32 * DropdownMenuItemView::ITEM_HEIGHT,
            ),
            view_init_context.mount_context.composite_tree,
            view_init_context.mount_context.ht_manager,
            view_init_context.mount_context.keyboard_focus_registry,
        );
        view_init_context.ui_scale_factor = root_surface.render_scale();

        let mut item_views = Vec::with_capacity(items.len());
        for (n, v) in items.into_iter().enumerate() {
            let v = DropdownMenuItemView::new(
                view_init_context,
                selection_receiver.clone(),
                v,
                n as f32 * DropdownMenuItemView::ITEM_HEIGHT,
            );
            v.mount(view_init_context, &root_surface);
            item_views.push(v);
        }

        Self {
            parent,
            opening_surfaces: vec![DropdownMenuSurface {
                native_surface: root_surface,
                item_views,
            }],
        }
    }

    pub fn rescale<E>(&self, new_scale: f32, composite_tree: &mut CompositeTree<E>) {
        for x in self.opening_surfaces.iter() {
            for x in x.item_views.iter() {
                x.rescale(new_scale, composite_tree);
            }
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

pub struct ContextMenuSession {
    parent: WindowHandle,
    items: Vec<MenuItem>,
    opening_surfaces: Vec<ContextMenuSurface>,
    active_selection: Option<(usize, usize)>,
}
impl ContextMenuSession {
    pub fn new(
        parent: WindowHandle,
        items: Vec<MenuItem>,
        system_link: &SystemLink,
        surface_pos: Point<LogicalUnit>,
        view_init_context: &mut ViewInitContext,
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
                crate::uikit::MenuItemLayout::build(
                    items.iter().cloned(),
                    system_link.font_set(),
                    render_scale,
                )
            },
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
            opening_surfaces: vec![ContextMenuSurface {
                handle: root_surface,
                item_views,
                _base_event_handler: base_event_handler,
                parent_path: Vec::new(),
                current_selecting: None,
            }],
            active_selection: None,
        }
    }

    pub fn rescale<E>(&self, scale: f32, composite_tree: &mut CompositeTree<E>) {
        #[cfg(not(target_os = "macos"))]
        for s in self.opening_surfaces.iter() {
            for v in s.item_views.iter() {
                v.rescale(scale, composite_tree);
            }
        }
    }

    pub fn perform_delayed_action(
        &mut self,
        system_link: &SystemLink,
        view_init_context: &mut ViewInitContext,
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
                                render_scale,
                            )
                        },
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

                    self.opening_surfaces.push(ContextMenuSurface {
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
                    render_scale,
                )
            },
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

        self.opening_surfaces.push(ContextMenuSurface {
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
    rt_sender: std::sync::mpsc::Sender<RenderMessage>,
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
    pub context_menu: platform::unix::wayland::flyout_surface::SharedState,
    #[cfg(target_os = "macos")]
    pub context_menu: platform::mac::context_menu::SharedState,
}
#[cfg(not(windows))]
impl SystemLink<'_> {
    #[inline(always)]
    pub const fn rt_sender(&self) -> &std::sync::mpsc::Sender<RenderMessage> {
        &self.rt_sender
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
        ref_scale_factor: f32,
    ) -> FlyoutSurfaceHandle {
        platform::unix::wayland::flyout_surface::new_surface(
            parent,
            pos,
            size,
            self,
            composite_tree,
            ht_manager,
            keyboard_focus_registry,
            ref_scale_factor,
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
            view_init_context.ui_scale_factor,
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

#[cfg(windows)]
pub type PointerID = platform::windows::PointerID;
#[cfg(target_os = "macos")]
pub type PointerID = platform::mac::PointerID;
#[cfg(feature = "wayland")]
pub type PointerID = platform::unix::wayland::PointerID;

#[cfg(windows)]
pub type DragPreviewPopoverHandle = platform::windows::DragPreviewPopoverHandle;
#[cfg(target_os = "macos")]
pub type DragPreviewPopoverHandle = platform::mac::DragPreviewPopoverHandle;
#[cfg(feature = "wayland")]
pub type DragPreviewPopoverHandle = platform::unix::wayland::DragPreviewPopoverHandle;

#[cfg(windows)]
pub type WindowHandle = platform::windows::WindowHandle;
#[cfg(target_os = "macos")]
pub type WindowHandle = platform::mac::WindowHandle;
#[cfg(feature = "wayland")]
pub type WindowHandle = platform::unix::wayland::WindowHandle;

#[cfg(windows)]
pub type FlyoutSurfaceHandle = platform::windows::flyout_surface::Handle;
#[cfg(target_os = "macos")]
pub type FlyoutSurfaceHandle = platform::mac::context_menu::Handle;
#[cfg(feature = "wayland")]
pub type FlyoutSurfaceHandle = platform::unix::wayland::flyout_surface::Handle;

pub struct SyncEventBus {
    queue: std::sync::Mutex<VecDeque<SyncEvent>>,
    #[cfg(target_os = "linux")]
    efd: linux_eventfd::EventFD,
    #[cfg(windows)]
    event_notify: windows::Win32::Foundation::HANDLE,
    #[cfg(target_os = "macos")]
    redispatch_to: LogicFiberEventDispatcher,
}
#[cfg(any(windows, target_os = "macos"))]
unsafe impl Sync for SyncEventBus {}
#[cfg(any(windows, target_os = "macos"))]
unsafe impl Send for SyncEventBus {}
impl Drop for SyncEventBus {
    fn drop(&mut self) {
        #[cfg(windows)]
        unsafe {
            if let Err(e) = windows::Win32::Foundation::CloseHandle(self.event_notify) {
                tracing::error!(reason = ?e, "event_notify.close");
            }
        }
    }
}
impl SyncEventBus {
    pub fn new(redispatch_to: LogicFiberEventDispatcher) -> Self {
        Self {
            queue: std::sync::Mutex::new(VecDeque::new()),
            #[cfg(target_os = "linux")]
            efd: linux_eventfd::EventFD::new(0, linux_eventfd::EventFDFlags::empty())
                .expect("app_event_bus.efd.create"),
            #[cfg(windows)]
            event_notify: unsafe {
                windows::Win32::System::Threading::CreateEventW(None, true, false, None)
                    .expect("event_notify.create")
            },
            #[cfg(target_os = "macos")]
            redispatch_to,
        }
    }

    pub fn push(&self, e: SyncEvent) {
        self.queue.lock().expect("poisoned").push_back(e);
        #[cfg(target_os = "linux")]
        self.efd.inc(1).unwrap();
        #[cfg(windows)]
        unsafe {
            windows::Win32::System::Threading::SetEvent(self.event_notify)
                .expect("event_notify.set");
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
        unsafe {
            windows::Win32::System::Threading::ResetEvent(self.event_notify).map_err(From::from)
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
