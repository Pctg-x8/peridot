#[cfg(target_os = "linux")]
use linux_epoll::{Epoll, EpollEventBits};
#[cfg(feature = "wayland")]
use linux_eventfd::{EventFD, EventFDFlags};
#[cfg(target_os = "linux")]
use peridot_tp_dbus as dbus;
#[cfg(feature = "wayland")]
use peridot_tp_wayland as wl;
#[cfg(target_os = "linux")]
use peridot_tp_xkbcommon as xkbcommon;
#[cfg(target_os = "linux")]
use std::os::fd::AsRawFd;
use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
    rc::Rc,
    sync::Mutex,
};

use crate::{
    graphics::VulkanDevice,
    input::{
        FocusTargetToken, InputEventContext, KeyInputCode, KeyInputEventHandler,
        KeyboardFocusTokenRegistry, PointerInputManager, PointerInputUnit,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeCreate, HitTestTreeData,
            HitTestTreeManager, HitTestTreeRef, PointerActionArgs,
        },
    },
    rendering::{
        MainThreadTextureIDIssuer, RenderMessage, RenderThread, RendererSync,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, Border, ClipConfig, CompositeMode,
            CompositeRect, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTree,
            CompositeTreeRef, CompositeTreeSyncBuffer,
        },
        text::{FontID, PerWindowFontSet, RootFontSet, TextLayout, ThreadLocalTypingContext},
    },
    uikit::{
        MountContext, MountTarget, OverlayPopupBasicFrameView, OverlayPopupBasicMaskView, Popup,
        PopupID, PopupManager, Positioning, RawMountTarget, SimpleButtonView, ViewInitContext,
    },
    utils::{Color32, LogicalUnit, Point, Rect, SafeF32, Size},
};
#[cfg(target_os = "macos")]
use crate::{input::PerWindowKeyboardFocusState, utils::PixelsUnit};

#[cfg(windows)]
mod bindgen;
mod graphics;
mod input;
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
    tracing_subscriber::fmt()
        .with_ansi(false)
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    #[cfg(windows)]
    tracing_subscriber::fmt()
        .with_ansi(false)
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(utils::platform::windows::DebugOutputWriter)
        .init();
    #[cfg(all(not(target_os = "macos"), not(windows)))]
    tracing_subscriber::fmt()
        .pretty()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let mut event_store = VecDeque::new();
    let event_queue = EventQueue {
        event_store: &mut event_store,
    };
    let global_time_base = std::time::Instant::now();
    let fs = FileSystem::new();
    main_wrapper(
        move |global_time_base,
              renderer_sync,
              composite_tree,
              ht_manager,
              main_window,
              system_link| {
            run(
                event_queue,
                global_time_base,
                renderer_sync,
                composite_tree,
                ht_manager,
                main_window,
                system_link,
            )
        },
        &mut event_store,
        &global_time_base,
        &Mutex::new(RendererSync {
            composite_buffer: CompositeTreeSyncBuffer::new(),
        }),
        &fs,
    );
}

fn main_wrapper<'sys, AppFuture: core::future::Future<Output = ()> + 'sys>(
    run_app: impl FnOnce(
        &'sys std::time::Instant,
        &'sys Mutex<RendererSync>,
        CompositeTree<SyncEvent>,
        HitTestTreeManager<'sys>,
        WindowHandle,
        SystemLink<'sys>,
    ) -> AppFuture,
    event_store: &mut VecDeque<Event>,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
    fs: &'sys FileSystem,
) {
    let events = SyncEventBus {
        queue: std::sync::Mutex::new(VecDeque::new()),
        #[cfg(target_os = "linux")]
        efd: linux_eventfd::EventFD::new(0, linux_eventfd::EventFDFlags::empty())
            .expect("app_event_bus.efd.create"),
        #[cfg(windows)]
        event_notify: unsafe {
            windows::Win32::System::Threading::CreateEventW(None, true, false, None)
                .expect("event_notify.create")
        },
    };
    let vk_device = VulkanDevice::new(&fs);
    let (rt_sender, rt_receiver) = std::sync::mpsc::channel::<RenderMessage>();
    #[cfg(windows)]
    assert!(
        vk_device.presentation_support(),
        "win32 presentation not supported on graphics queue"
    );

    #[cfg(windows)]
    let app_context = platform::windows::ApplicationContext::new();

    #[cfg(target_os = "linux")]
    let dbus = dbus::Connection::connect_bus(dbus::BusType::Session).expect("dbus.connect");

    #[cfg(feature = "wayland")]
    let terminate_event = std::sync::Arc::new(
        EventFD::new(0, EventFDFlags::empty()).expect("terminate_event.create"),
    );

    #[cfg(feature = "wayland")]
    let mut wl_display = wl::Display::connect().expect("wl_display connect");
    #[cfg(feature = "wayland")]
    assert!(
        vk_device.presentation_support(&wl_display),
        "wayland presentation not supported on graphics queue"
    );
    #[cfg(feature = "wayland")]
    let mut wl_interfaces = platform::unix::wayland::GlobalInterfaces::collect_sync(&wl_display)
        .expect("wl_interfaces.collect_sync");
    #[cfg(feature = "wayland")]
    let mut window_registry = platform::unix::wayland::WindowRegistry::new();

    #[cfg(windows)]
    let drag_preview_popover = DragPreviewPopoverHandle::new(&app_context);

    #[cfg(feature = "wayland")]
    let popover_buf_shm_bytes = if wl_interfaces.single_pixel_buffer_manager.is_some() {
        0
    } else {
        4
    };
    #[cfg(feature = "wayland")]
    let window_decoration_pixbuf_offset = utils::rup2(
        popover_buf_shm_bytes,
        platform::unix::wayland::WindowDecorationPixbuf::REQUIRED_BYTE_ALIGNMENT,
    );
    #[cfg(feature = "wayland")]
    let shm_total_byte_length = window_decoration_pixbuf_offset
        + if platform::unix::wayland::Window::should_client_decoration(&wl_interfaces) {
            platform::unix::wayland::WindowDecorationPixbuf::REQUIRED_BYTE_LENGTH
        } else {
            0
        };
    #[cfg(feature = "wayland")]
    let shm_pair = if shm_total_byte_length > 0 {
        let shm_region = utils::platform::unix::TemporalSharedMemory::new_unique(
            c"/pme_shm",
            libc::O_RDWR,
            0o0600,
        )
        .expect("buf.shm.create")
        .expect("buf.shm.create.non_unique");
        unsafe {
            utils::platform::unix::ftruncate(&shm_region, shm_total_byte_length as _)
                .expect("buf.shm.resize");
        }

        let mapped = utils::platform::unix::MappedMemory::new(
            None,
            shm_total_byte_length,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            &shm_region,
            0,
        )
        .expect("buf.mmap");

        let shmp = wl_interfaces
            .shm
            .create_pool(&shm_region, shm_total_byte_length as _)
            .expect("shmp.create.popup");

        Some((shmp, shm_region, mapped))
    } else {
        None
    };
    #[cfg(feature = "wayland")]
    let popover_buf = if let Some(ref spb) = wl_interfaces.single_pixel_buffer_manager {
        let c = DragPreviewPopoverHandle::BG_COLOR.premultiplied();
        let b = spb
            .create_u32_rgba_buffer(c.r_u32(), c.g_u32(), c.b_u32(), c.a_u32())
            .expect("popup_buf.create.single_pixel_buffer");

        platform::unix::wayland::DragPreviewPopoverBuffer::SinglePixel(b)
    } else {
        // traditional shm-based single pixel buffer
        let (shm, _, mapped) = shm_pair.as_ref().expect("no shm");

        let buf = shm
            .create_buffer(0, 1, 1, 4, wl::ShmFormat::ARGB8888)
            .expect("buf.create.popup");
        unsafe {
            core::ptr::write(
                mapped.as_ptr().cast::<u32>(),
                DragPreviewPopoverHandle::BG_COLOR
                    .premultiplied()
                    .argb8888(),
            );
        }

        platform::unix::wayland::DragPreviewPopoverBuffer::Shm { buf }
    };
    #[cfg(feature = "wayland")]
    let window_decoration_pixbuf = core::pin::pin!(
        if platform::unix::wayland::Window::should_client_decoration(&wl_interfaces) {
            let (shm, _, mapped) = shm_pair.as_ref().expect("no shm");

            platform::unix::wayland::WindowDecorationPixbuf::generate_content(unsafe {
                mapped.as_ptr().byte_add(window_decoration_pixbuf_offset)
            });
            Some(platform::unix::wayland::WindowDecorationPixbuf::new(
                shm,
                window_decoration_pixbuf_offset,
            ))
        } else {
            None
        }
    );

    #[cfg(feature = "wayland")]
    let drag_preview_popover = DragPreviewPopoverHandle {
        display: &mut wl_display,
        wl_interfaces: &wl_interfaces as *const _ as _,
        root_window: core::cell::Cell::new(core::ptr::null_mut()),
        buf: popover_buf,
        popup: core::cell::UnsafeCell::new(None),
    };

    #[cfg(target_os = "macos")]
    let drag_preview_popover = DragPreviewPopoverHandle {
        position_base_window_link: core::cell::Cell::new(core::ptr::null_mut()),
    };

    let mut composite_tree = CompositeTree::new();
    let mut ht_manager = HitTestTreeManager::new();
    let mut polling = false;
    let empty_dispatcher = LogicFiberEventDispatcher {
        event_store,
        polling: &mut polling,
        poll_fn_ptr: unsafe { core::mem::transmute(AppFuture::poll as *const core::ffi::c_void) },
        future_ptr: core::ptr::null_mut(),
    };

    #[cfg(windows)]
    let main_window_handle = SystemLink::init_main_window(
        &vk_device,
        empty_dispatcher.clone(),
        &mut composite_tree,
        &mut ht_manager,
        &rt_sender,
        &app_context,
    );

    #[cfg(feature = "wayland")]
    let main_window_handle = SystemLink::init_main_window(
        &wl_display,
        &wl_interfaces,
        &mut window_registry,
        window_decoration_pixbuf.as_ref().get_ref().as_ref(),
        &dbus,
        &mut composite_tree,
        &mut ht_manager,
        empty_dispatcher.clone(),
        &terminate_event,
        &vk_device,
        &rt_sender,
    );

    #[cfg(target_os = "macos")]
    let mut w = MacWindow::new(
        WindowType::Main {},
        platform::mac::bridge::WindowCreationFlags::MAIN,
        empty_dispatcher.clone(),
        composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            ..Default::default()
        }),
        ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        }),
    );
    #[cfg(target_os = "macos")]
    let main_window_handle = w.make_handle();
    #[cfg(target_os = "macos")]
    w.make_primary_window();

    #[cfg(target_os = "macos")]
    let vk_surface = graphics::VulkanSurface::new(&vk_device, unsafe {
        bedrock::SurfaceCreateInfo::execute(
            &bedrock::MetalSurfaceCreateInfo::new(w.metal_layer()),
            bedrock::InstanceChild::instance(&vk_device),
            None,
        )
        .expect("vk_surface.create")
    });

    #[cfg(target_os = "macos")]
    rt_sender
        .send(RenderMessage::NewWindow(rendering::NewWindowData {
            #[cfg(target_os = "macos")]
            init_scale: SafeF32::new(
                *w.dispatcher()
                    .state
                    .active_buffer_scale
                    .lock()
                    .expect("poisoned"),
            )
            .expect("invalid scale"),
            #[cfg(target_os = "macos")]
            latest_ui_scale_changes: utils::UnboundedRef::new(
                &w.dispatcher().state.latest_ui_scale_changes,
            ),
            key: main_window_handle,
            vk_surface: rendering::NewWindowVulkanSurface(vk_surface.unbound().1),
        }))
        .expect("rt_sender.send");

    let root_font_set = RootFontSet::new();

    #[cfg(feature = "wayland")]
    let mut wl_global_msg = core::pin::pin!(platform::unix::wayland::GlobalMessaging {
        text_input_manager: wl_interfaces.text_input_manager.as_ptr(),
        xkb_context: xkbcommon::Context::new(xkbcommon::ContextFlags::NO_FLAGS)
            .expect("xkb_context.create"),
        keyboard: None,
        pointer: None,
        cursor_shape_manager: wl_interfaces
            .cursor_shape_manager
            .as_ref()
            .map(|x| x.as_ptr()),
        event_dispatcher: empty_dispatcher.clone(),
        ime_pending_state: platform::unix::wayland::IMEPendingState {
            committed_text: String::new(),
            preedit_text: String::new(),
        },
        _pinned: core::marker::PhantomPinned,
    });

    let mut app_event_dispatcher = core::pin::pin!(empty_dispatcher.clone());
    let mut app = core::pin::pin!(run_app(
        global_time_base,
        renderer_sync,
        composite_tree,
        ht_manager,
        main_window_handle,
        #[cfg(windows)]
        SystemLink {
            drag_preview_popover,
            root_font_set: &root_font_set,
            rt_sender: rt_sender.clone(),
            vk_device: &vk_device,
            event_dispatcher: app_event_dispatcher.as_mut().get_mut(),
            app_context_ptr: &app_context,
        },
        #[cfg(not(windows))]
        SystemLink {
            drag_preview_popover,
            rt_sender: rt_sender.clone(),
            vk_device: &vk_device,
            root_font_set: &root_font_set,
            event_dispatcher: app_event_dispatcher.as_mut().get_mut(),
            #[cfg(target_os = "linux")]
            dbus: &dbus,
            #[cfg(feature = "wayland")]
            display_server: platform::unix::DisplayServerLink {
                wl_display: &mut wl_display,
                wl_global_interfaces: &wl_interfaces,
                pointer_state_ref: unsafe {
                    &mut wl_global_msg.as_mut().get_unchecked_mut().pointer as *mut _
                },
                window_registry: &mut window_registry,
                decoration_pixbuf: window_decoration_pixbuf
                    .as_ref()
                    .get_ref()
                    .as_ref()
                    .map_or_else(core::ptr::null, |x| x as *const _),
                global_messaging_ptr: wl_global_msg.as_ref().get_ref() as *const _,
            }
        }
    ));

    app_event_dispatcher.future_ptr = unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ };
    #[cfg(feature = "wayland")]
    unsafe {
        wl_global_msg
            .as_mut()
            .get_unchecked_mut()
            .event_dispatcher
            .future_ptr = app.as_mut().get_unchecked_mut() as *mut _ as _;
    }
    #[cfg(feature = "wayland")]
    window_registry
        .get_mut(main_window_handle)
        .expect("no window")
        .rebind_event_dispatcher(LogicFiberEventDispatcher {
            event_store,
            polling: &mut polling,
            poll_fn_ptr: unsafe {
                core::mem::transmute(AppFuture::poll as *const core::ffi::c_void)
            },
            future_ptr: unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ },
        });
    #[cfg(windows)]
    SystemLink::postinit_main_window(
        main_window_handle,
        LogicFiberEventDispatcher {
            event_store,
            polling: &mut polling,
            poll_fn_ptr: unsafe {
                core::mem::transmute(AppFuture::poll as *const core::ffi::c_void)
            },
            future_ptr: unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ },
        },
    );
    #[cfg(target_os = "macos")]
    w.rebind_event_dispatcher(LogicFiberEventDispatcher {
        event_store,
        polling: &mut polling,
        poll_fn_ptr: unsafe { core::mem::transmute(AppFuture::poll as *const core::ffi::c_void) },
        future_ptr: unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ },
    });

    #[cfg(feature = "wayland")]
    wl_interfaces
        .xdg_wm_base
        .set_listener(unsafe { wl_global_msg.as_mut().get_unchecked_mut() })
        .into_result()
        .expect("xdg_wm_base set_listener");
    #[cfg(feature = "wayland")]
    wl_interfaces
        .seat
        .set_listener(unsafe { wl_global_msg.as_mut().get_unchecked_mut() })
        .into_result()
        .expect("seat set_listener");

    // initial poll
    let _ = app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }));

    SystemLink::prelaunch(main_window_handle);
    #[cfg(feature = "wayland")]
    wl_display.roundtrip().expect("roundtrip");

    let shutdown = std::sync::atomic::AtomicBool::new(false);
    std::thread::scope(|thread_scope| {
        let render_thread = RenderThread {
            vk_device: &vk_device,
            shutdown_signal: &shutdown,
            renderer_sync: &renderer_sync,
            global_time_base: &global_time_base,
            event_bus: &events,
            message_receiver: rt_receiver,
            root_font_set: &root_font_set,
        };
        let render_thread = std::thread::Builder::new()
            .name("Render".into())
            .spawn_scoped(thread_scope, || render_thread.run())
            .expect("render_thread spawn");

        #[cfg(target_os = "linux")]
        let epoll = Epoll::new(0).expect("epoll.new");
        #[cfg(feature = "wayland")]
        epoll
            .add(&wl_display, EpollEventBits::IN, 0)
            .expect("epoll.add.wl_display");
        #[cfg(feature = "wayland")]
        epoll
            .add(&terminate_event, EpollEventBits::IN, 1)
            .expect("epoll.add.terminate_event");
        #[cfg(feature = "wayland")]
        epoll
            .add(&events.efd, EpollEventBits::IN, 2)
            .expect("epoll.add.events_efd");
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
            if platform::unix::wayland::dp_prepare_read(&mut wl_display).is_err() {
                break 'app;
            }
            let active_events = epoll.wait(&mut eventbuf, None).expect("epoll.wait");

            let mut wl_display_signal = false;
            let mut terminate_signal = false;
            let mut dbus_signal = false;
            let mut events_signal = false;
            for n in 0..active_events {
                let e = unsafe { eventbuf[n as usize].assume_init_ref() };
                if e.value() == 0 {
                    wl_display_signal = true;
                } else if e.value() == 1 {
                    terminate_signal = true;
                } else if e.value() == 2 {
                    events_signal = true;
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
                wl_display.read_events().expect("wl_display.read_events");
                wl_display
                    .dispatch_pending()
                    .expect("wl_display.dispatch_pending");
            } else {
                wl_display.cancel_read();
            }

            if terminate_signal {
                break 'app;
            }

            if events_signal {
                events.redispatch(&app_event_dispatcher);
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
        let handles = [events.event_notify];
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
                events.redispatch(&app_event_dispatcher);
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

#[derive(Clone)]
pub enum SyncEvent {
    WindowPostResizeRenderBuffer { window: WindowHandle },
    PopupUnmount { id: PopupID },
}

#[derive(Clone)]
pub enum Event {
    Sync(SyncEvent),
    Quit,
    PointerDown {
        window: WindowHandle,
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
    },
    KeyDown {
        window: WindowHandle,
        code: KeyInputCode,
    },
    KeyUp {
        window: WindowHandle,
        code: KeyInputCode,
    },
    IMEStateChanges {
        window: WindowHandle,
        committed_string: String,
        preedit_string: String,
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
}

struct TextInputViewEventHandler {
    token: FocusTargetToken,
    ct_root: CompositeTreeRef,
    ct_text: CompositeTreeRef,
    ct_cursor: CompositeTreeRef,
    ct_preedit_underline: CompositeTreeRef,
    ct_selection_bg: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    content_h_offset: core::cell::Cell<f32>,
    content_visible_width: f32,
    content: core::cell::RefCell<String>,
    cursor_pos_bytes: core::cell::Cell<usize>,
    preedit_range_start_bytes: core::cell::Cell<usize>,
    preedit_range_end_bytes: core::cell::Cell<usize>,
    selection_begin_bytes: core::cell::Cell<usize>,
    dragging: core::cell::Cell<bool>,
    #[cfg(windows)]
    native_text_input_context: platform::windows::NativeTextInputContext,
}
impl KeyInputEventHandler for TextInputViewEventHandler {
    fn focus_taken(&self, context: &mut InputEventContext) {
        self.update(context);
        #[cfg(windows)]
        self.native_text_input_context.notify_focus_enter();
    }

    fn focus_released(&self, context: &mut InputEventContext) {
        self.update(context);
        #[cfg(windows)]
        self.native_text_input_context.notify_focus_leave();

        // clear selection
        self.selection_begin_bytes.set(self.cursor_pos_bytes.get());
        self.update_selection(context.composite_tree, context.sender_window);
        self.sync_selection_native();
    }

    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode) {
        tracing::debug!(?code, "keydown");

        match code {
            KeyInputCode::LeftArrow => {
                let mut new_cursor_pos = self.cursor_pos_bytes.get().saturating_sub(1);
                while new_cursor_pos > 0 {
                    if self.content.borrow().is_char_boundary(new_cursor_pos) {
                        break;
                    }

                    new_cursor_pos -= 1;
                }
                self.cursor_pos_bytes.set(new_cursor_pos);
                self.selection_begin_bytes.set(new_cursor_pos); // 選択を解除
                self.update_cursor_position(
                    context.composite_tree,
                    context.sender_window,
                    context.system_link,
                    context.ht_manager,
                    context.sender_window.client_size(),
                );
                self.update_selection(context.composite_tree, context.sender_window);
                self.sync_selection_native();
            }
            KeyInputCode::RightArrow => {
                let mut new_cursor_pos = self
                    .cursor_pos_bytes
                    .get()
                    .saturating_add(1)
                    .min(self.content.borrow().len());
                while new_cursor_pos < self.content.borrow().len() {
                    if self.content.borrow().is_char_boundary(new_cursor_pos) {
                        break;
                    }

                    new_cursor_pos += 1;
                }
                self.cursor_pos_bytes.set(new_cursor_pos);
                self.selection_begin_bytes.set(new_cursor_pos); // 選択を解除
                self.update_cursor_position(
                    context.composite_tree,
                    context.sender_window,
                    context.system_link,
                    context.ht_manager,
                    context.sender_window.client_size(),
                );
                self.update_selection(context.composite_tree, context.sender_window);
                self.sync_selection_native();
            }
            KeyInputCode::Home => {
                self.cursor_pos_bytes.set(0);
                self.selection_begin_bytes.set(0); // 選択を解除
                self.update_cursor_position(
                    context.composite_tree,
                    context.sender_window,
                    context.system_link,
                    context.ht_manager,
                    context.sender_window.client_size(),
                );
                self.update_selection(context.composite_tree, context.sender_window);
                self.sync_selection_native();
            }
            KeyInputCode::End => {
                self.cursor_pos_bytes.set(self.content.borrow().len());
                self.selection_begin_bytes.set(self.content.borrow().len()); // 選択を解除
                self.update_cursor_position(
                    context.composite_tree,
                    context.sender_window,
                    context.system_link,
                    context.ht_manager,
                    context.sender_window.client_size(),
                );
                self.update_selection(context.composite_tree, context.sender_window);
                self.sync_selection_native();
            }
            KeyInputCode::Insert => {
                // TODO: insert mode
            }
            KeyInputCode::Character(c) if c == '\n' => (/* ignore enter key */),
            KeyInputCode::Character(c) if c == '\x08' => {
                // bksp
                let selection_range = self.selection_range();
                if selection_range.is_empty() {
                    // single character removal

                    let mut remove_to = self.cursor_pos_bytes.get().saturating_sub(1);
                    while remove_to > 0 {
                        if self.content.borrow().is_char_boundary(remove_to) {
                            break;
                        }

                        remove_to -= 1;
                    }
                    if remove_to != self.cursor_pos_bytes.get() {
                        self.content
                            .borrow_mut()
                            .replace_range(remove_to..self.cursor_pos_bytes.get(), "");
                        self.cursor_pos_bytes.set(remove_to);
                        self.selection_begin_bytes.set(remove_to);

                        self.update_text(context.composite_tree);
                        self.update_cursor_position(
                            context.composite_tree,
                            context.sender_window,
                            context.system_link,
                            context.ht_manager,
                            context.sender_window.client_size(),
                        );
                        self.sync_selection_native();
                    }
                } else {
                    // remove selection
                    self.content
                        .borrow_mut()
                        .replace_range(selection_range.clone(), "");
                    self.cursor_pos_bytes.set(selection_range.start);
                    self.selection_begin_bytes.set(selection_range.start);

                    self.update_text(context.composite_tree);
                    self.update_cursor_position(
                        context.composite_tree,
                        context.sender_window,
                        context.system_link,
                        context.ht_manager,
                        context.sender_window.client_size(),
                    );
                    self.update_selection(context.composite_tree, context.sender_window);
                    self.sync_selection_native();
                }
            }
            KeyInputCode::Character(c) if c == '\x7f' => {
                // del
                let selection_range = self.selection_range();
                if selection_range.is_empty() {
                    // single character removal
                    if self.cursor_pos_bytes.get() < self.content.borrow().len() {
                        let remove_to = self.cursor_pos_bytes.get();
                        let remove_to = remove_to
                            + self.content.borrow()[remove_to..]
                                .chars()
                                .next()
                                .expect("no char")
                                .len_utf8();

                        self.content
                            .borrow_mut()
                            .replace_range(self.cursor_pos_bytes.get()..remove_to, "");
                        self.update_text(context.composite_tree);
                        self.sync_selection_native();
                    }
                } else {
                    // remove selection
                    self.content
                        .borrow_mut()
                        .replace_range(selection_range.clone(), "");
                    self.update_text(context.composite_tree);

                    self.cursor_pos_bytes.set(selection_range.start);
                    self.selection_begin_bytes.set(selection_range.start);
                    self.update_cursor_position(
                        context.composite_tree,
                        context.sender_window,
                        context.system_link,
                        context.ht_manager,
                        context.sender_window.client_size(),
                    );
                    self.update_selection(context.composite_tree, context.sender_window);
                    self.sync_selection_native();
                }
            }
            KeyInputCode::Character(c) if !c.is_control() => {
                let selection_range = self.selection_range();
                if selection_range.is_empty() {
                    // just insert
                    self.content
                        .borrow_mut()
                        .insert(self.cursor_pos_bytes.get(), c);
                    self.cursor_pos_bytes
                        .set(self.cursor_pos_bytes.get() + c.len_utf8());
                } else {
                    // replace selection
                    self.content
                        .borrow_mut()
                        .replace_range(selection_range.clone(), &c.to_string());
                    self.cursor_pos_bytes
                        .set(selection_range.start + c.len_utf8());
                }

                self.selection_begin_bytes.set(self.cursor_pos_bytes.get());
                self.update_text(context.composite_tree);
                self.update_cursor_position(
                    context.composite_tree,
                    context.sender_window,
                    context.system_link,
                    context.ht_manager,
                    context.sender_window.client_size(),
                );
                self.update_selection(context.composite_tree, context.sender_window);
                self.sync_selection_native();
            }
            _ => (),
        }
    }

    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: &str,
        new_preedit_string: &str,
    ) {
        let selection_range = self.selection_range();
        if !selection_range.is_empty() {
            // remove selection first
            self.content
                .borrow_mut()
                .replace_range(selection_range.clone(), "");
            self.cursor_pos_bytes.set(selection_range.start);
            self.selection_begin_bytes.set(selection_range.start);
        }

        // TODO: waylandのText Input v3はこの順序で処理しろと書いてある https://wayland.app/protocols/text-input-unstable-v3#zwp_text_input_v3:event:done
        // 他PFではどうなのかは不明
        let has_preedit_text =
            self.preedit_range_start_bytes.get() != self.preedit_range_end_bytes.get();

        if has_preedit_text {
            if !new_preedit_string.is_empty() {
                // replace preedit
                self.content.borrow_mut().replace_range(
                    self.preedit_range_start_bytes.get()..self.preedit_range_end_bytes.get(),
                    new_preedit_string,
                );
                self.preedit_range_start_bytes
                    .set(self.preedit_range_start_bytes.get());
                self.preedit_range_end_bytes
                    .set(self.preedit_range_start_bytes.get() + new_preedit_string.len());
                self.cursor_pos_bytes
                    .set(self.preedit_range_end_bytes.get());
            } else {
                // clear preedit
                self.content.borrow_mut().replace_range(
                    self.preedit_range_start_bytes.get()..self.preedit_range_end_bytes.get(),
                    "",
                );
                self.preedit_range_start_bytes
                    .set(self.preedit_range_start_bytes.get());
                self.preedit_range_end_bytes
                    .set(self.preedit_range_start_bytes.get());
                self.cursor_pos_bytes
                    .set(self.preedit_range_start_bytes.get());
            }
        }

        if !new_committed_string.is_empty() {
            // insert committed
            self.content
                .borrow_mut()
                .insert_str(self.cursor_pos_bytes.get(), new_committed_string);
            self.cursor_pos_bytes
                .set(self.cursor_pos_bytes.get() + new_committed_string.len());
        }

        if !has_preedit_text && !new_preedit_string.is_empty() {
            // insert preedit
            self.content
                .borrow_mut()
                .insert_str(self.cursor_pos_bytes.get(), new_preedit_string);
            self.preedit_range_start_bytes
                .set(self.cursor_pos_bytes.get());
            self.preedit_range_end_bytes
                .set(self.cursor_pos_bytes.get() + new_preedit_string.len());
            self.cursor_pos_bytes
                .set(self.preedit_range_end_bytes.get());
        }

        // no selection in editing
        self.selection_begin_bytes.set(self.cursor_pos_bytes.get());

        self.update_text(context.composite_tree);
        self.update_preedit_underline(context.composite_tree, context.sender_window);
        self.update_cursor_position(
            context.composite_tree,
            context.sender_window,
            context.system_link,
            context.ht_manager,
            context.sender_window.client_size(),
        );
        self.update_selection(context.composite_tree, context.sender_window);
        self.sync_selection_native();
    }
}
impl HitTestTreeActionHandler for TextInputViewEventHandler {
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> input::EventContinueControl {
        let (local_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );

        let cursor_rect = context.composite_tree.get_mut(self.ct_cursor);
        // TextLayoutはPixels座標系なのでscaleをかけておく
        let (_, bytes) = TextLayout::find_nearest_position_with_bytes(
            (local_x - 2.0 - self.content_h_offset.get()) * cursor_rect.base_scale_factor,
            &self.content.borrow(),
            FontID::UIDefault,
            unsafe {
                &context
                    .sender_window
                    .extra_data_ref::<PerWindowData>()
                    .font_set
            },
            context.sender_window.ui_scale_factor(),
        );
        self.cursor_pos_bytes.set(bytes);
        self.selection_begin_bytes.set(bytes); // 最初は同じところ(=範囲選択なし)
        self.update_cursor_position(
            context.composite_tree,
            context.sender_window,
            context.system_link,
            context.ht_manager,
            args.client_size,
        );
        self.update_selection(context.composite_tree, context.sender_window);
        self.sync_selection_native();
        self.dragging.set(true);

        input::EventContinueControl::STOP_PROPAGATION | input::EventContinueControl::CAPTURE_ELEMENT
    }

    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> input::EventContinueControl {
        if !self.dragging.get() {
            // not dragging
            return input::EventContinueControl::STOP_PROPAGATION;
        }

        let (local_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );

        let cursor_rect = context.composite_tree.get_mut(self.ct_cursor);
        // TextLayoutはPixels座標系なのでscaleをかけておく
        let (_, bytes) = TextLayout::find_nearest_position_with_bytes(
            (local_x - 2.0 - self.content_h_offset.get()) * cursor_rect.base_scale_factor,
            &self.content.borrow(),
            FontID::UIDefault,
            unsafe {
                &context
                    .sender_window
                    .extra_data_ref::<PerWindowData>()
                    .font_set
            },
            context.sender_window.ui_scale_factor(),
        );
        self.cursor_pos_bytes.set(bytes);
        self.update_cursor_position(
            context.composite_tree,
            context.sender_window,
            context.system_link,
            context.ht_manager,
            args.client_size,
        );
        self.update_selection(context.composite_tree, context.sender_window);
        self.sync_selection_native();

        input::EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> input::EventContinueControl {
        let (local_x, _, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x,
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );

        let cursor_rect = context.composite_tree.get_mut(self.ct_cursor);
        // TextLayoutはPixels座標系なのでscaleをかけておく
        let (_, bytes) = TextLayout::find_nearest_position_with_bytes(
            (local_x - 2.0 - self.content_h_offset.get()) * cursor_rect.base_scale_factor,
            &self.content.borrow(),
            FontID::UIDefault,
            unsafe {
                &context
                    .sender_window
                    .extra_data_ref::<PerWindowData>()
                    .font_set
            },
            context.sender_window.ui_scale_factor(),
        );
        self.cursor_pos_bytes.set(bytes);
        self.update_cursor_position(
            context.composite_tree,
            context.sender_window,
            context.system_link,
            context.ht_manager,
            args.client_size,
        );
        self.update_selection(context.composite_tree, context.sender_window);
        self.sync_selection_native();
        self.dragging.set(false);

        input::EventContinueControl::STOP_PROPAGATION
            | input::EventContinueControl::RELEASE_CAPTURE_ELEMENT
    }

    fn on_double_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> input::EventContinueControl {
        #[cfg(windows)]
        let user_language = windows::System::UserProfile::GlobalizationPreferences::Languages()
            .expect("globalization_preferences.languages")
            .First()
            .expect("vector_view.first")
            .Current()
            .expect("iterator.current");
        #[cfg(windows)]
        let word_segmenter =
            windows::Data::Text::WordsSegmenter::CreateWithLanguage(&user_language)
                .expect("words_segmenter.create");
        #[cfg(windows)]
        let ws = word_segmenter
            .GetTokenAt(
                &windows_core::HSTRING::from_wide(&{
                    let mut u16s = Vec::new();
                    for c in self.content.borrow().chars() {
                        let mut b = [0; 2];
                        u16s.extend_from_slice(c.encode_utf16(&mut b));
                    }
                    u16s
                }),
                self.content
                    .borrow()
                    .char_indices()
                    .take_while(|&(i, _)| i < self.cursor_pos_bytes.get())
                    .count() as _,
            )
            .expect("word_segmenter.get_token_at");
        #[cfg(windows)]
        let text_segment = ws
            .SourceTextSegment()
            .expect("word_segment.source_text_segment");
        #[cfg(windows)]
        self.selection_begin_bytes.set(
            self.content
                .borrow()
                .chars()
                .take(text_segment.StartPosition as _)
                .map(|c| c.len_utf8())
                .sum(),
        );
        #[cfg(windows)]
        self.cursor_pos_bytes.set(
            self.content
                .borrow()
                .chars()
                .take((text_segment.StartPosition + text_segment.Length) as _)
                .map(|c| c.len_utf8())
                .sum(),
        );

        #[cfg(not(windows))]
        use unicode_segmentation::UnicodeSegmentation;
        #[cfg(not(windows))]
        let mut words = Vec::new();
        #[cfg(not(windows))]
        let content = self.content.borrow();
        #[cfg(not(windows))]
        let mut chars = content.chars();
        #[cfg(not(windows))]
        let mut is_budou_cluster = false;
        #[cfg(not(windows))]
        let mut same_cluster_range = 0..0;
        #[cfg(not(windows))]
        let mut cb = 0;
        #[cfg(not(windows))]
        while let Some(c) = chars.next() {
            let is_budou_cluster_c = peridot_tp_unicode_properties::script::is_hiragana(c)
                || peridot_tp_unicode_properties::script::is_katakana(c)
                || peridot_tp_unicode_properties::script::is_han(c)
                || peridot_tp_unicode_properties::script::is_thai(c)
                // 一部Commonにあるらしいので特別対応
                || c as u32 == 0x30fc || c as u32 == 0xff70;
            if is_budou_cluster_c != is_budou_cluster {
                if !same_cluster_range.is_empty() {
                    if !is_budou_cluster {
                        words.extend(
                            content[same_cluster_range.clone()]
                                .split_word_bounds()
                                .map(|x| x.to_owned()),
                        )
                    } else {
                        words.extend(
                            peridot_tp_budoux::parse(
                                &peridot_tp_budoux::embedded::ja_knbc::MODEL,
                                &content[same_cluster_range.clone()],
                            )
                            .into_iter()
                            .map(|x| x.to_owned()),
                        )
                    }
                }

                is_budou_cluster = is_budou_cluster_c;
                same_cluster_range = cb..cb;
            }

            same_cluster_range.end += c.len_utf8();
            cb += c.len_utf8();
        }
        #[cfg(not(windows))]
        if !same_cluster_range.is_empty() {
            if !is_budou_cluster {
                words.extend(
                    content[same_cluster_range.clone()]
                        .split_word_bounds()
                        .map(|x| x.to_owned()),
                )
            } else {
                words.extend(
                    peridot_tp_budoux::parse(
                        &peridot_tp_budoux::embedded::ja_knbc::MODEL,
                        &content[same_cluster_range.clone()],
                    )
                    .into_iter()
                    .map(|x| x.to_owned()),
                )
            }
        }

        #[cfg(not(windows))]
        tracing::debug!(?words, "double click");

        // TODO: LTR前提 最適化はあとで
        #[cfg(not(windows))]
        let (sx, _, _, _) = context.ht_manager.translate_client_to_tree_local(
            sender,
            args.client_pos.x - 2.0 - self.content_h_offset.get(),
            args.client_pos.y,
            args.client_size.width,
            args.client_size.height,
        );
        #[cfg(not(windows))]
        let target_x_pixels = sx * context.composite_tree.get(self.ct_text).base_scale_factor;
        #[cfg(not(windows))]
        let mut measure_range = 0..0;
        #[cfg(not(windows))]
        let mut select_range = 0..content.len();
        #[cfg(not(windows))]
        for w in words {
            let starting_byte = measure_range.end;
            measure_range.end += w.len();
            let tw = TextLayout::measure_total_advances(
                &content[measure_range.clone()],
                FontID::UIDefault,
                unsafe {
                    &context
                        .sender_window
                        .extra_data_ref::<PerWindowData>()
                        .font_set
                },
                context.sender_window.ui_scale_factor(),
            );

            if target_x_pixels <= tw {
                select_range = starting_byte..measure_range.end;
                break;
            }
        }

        #[cfg(not(windows))]
        self.cursor_pos_bytes.set(select_range.end);
        #[cfg(not(windows))]
        self.selection_begin_bytes.set(select_range.start);

        self.update_cursor_position(
            context.composite_tree,
            context.sender_window,
            context.system_link,
            context.ht_manager,
            args.client_size,
        );
        self.update_selection(context.composite_tree, context.sender_window);
        self.sync_selection_native();

        input::EventContinueControl::STOP_PROPAGATION
    }
}
impl TextInputViewEventHandler {
    fn update(&self, context: &mut InputEventContext) {
        if context
            .sender_window
            .keyboard_focus_state()
            .has_focus(&self.token)
        {
            context.composite_tree.get_mut(self.ct_root).border = Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 0.5],
                    to_value: [1.0, 1.0, 1.0, 1.0],
                    start_sec: context.current_sec,
                    end_sec: context.current_sec + 0.1,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                },
            });
            context.composite_tree.get_mut(self.ct_cursor).opacity = AnimatableFloat::Value(1.0);
        } else {
            context.composite_tree.get_mut(self.ct_root).border = Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Animated {
                    from_value: [1.0, 1.0, 1.0, 1.0],
                    to_value: [1.0, 1.0, 1.0, 0.5],
                    start_sec: context.current_sec,
                    end_sec: context.current_sec + 0.1,
                    curve: AnimationCurve::Linear,
                    event_on_complete: None,
                },
            });
            context.composite_tree.get_mut(self.ct_cursor).opacity = AnimatableFloat::Value(0.0);
        }

        context.composite_tree.mark_dirty(self.ct_root);
        context.composite_tree.mark_dirty(self.ct_cursor);
    }

    fn update_text(&self, composite_tree: &mut CompositeTree<SyncEvent>) {
        composite_tree.get_mut(self.ct_text).text = Some(CompositeRectText {
            runs: vec![CompositeRectTextRun {
                font_id: FontID::UIDefault,
                content: self.content.borrow().clone(),
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }],
            horizontal_alignment: CompositeRectTextHorizontalAlignment::Start,
            vertical_alignment: CompositeRectTextVerticalAlignment::Start,
            ..Default::default()
        });
        composite_tree.mark_text_layout_dirty(self.ct_text);
    }

    fn update_cursor_position(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        window: WindowHandle,
        system_link: &SystemLink,
        ht_manager: &HitTestTreeManager,
        client_size: Size<LogicalUnit>,
    ) {
        let tw = TextLayout::measure_total_advances(
            &self.content.borrow()[..self.cursor_pos_bytes.get()],
            FontID::UIDefault,
            unsafe { &window.extra_data_ref::<PerWindowData>().font_set },
            window.ui_scale_factor(),
        );

        let mut text_scroll_occured = false;
        let cursor_rect = composite_tree.get_mut(self.ct_cursor);
        // base_scale_factorがかかるのであらかじめわっておく
        let mut cursor_display_x = tw / cursor_rect.base_scale_factor + self.content_h_offset.get();
        if cursor_display_x < 0.0 {
            // 範囲外になる(左すぎ cursor_display_xが0になるようにスクロール量を調整)
            self.content_h_offset
                .set(self.content_h_offset.get() - cursor_display_x);
            text_scroll_occured = true;
            cursor_display_x = 0.0;
        } else if self.content_visible_width - 2.0 < cursor_display_x {
            // 範囲外になる(右すぎ cursor_display_xがcontent_visible_widthになるようにスクロール量を調整)
            self.content_h_offset.set(
                self.content_h_offset.get()
                    - (cursor_display_x - (self.content_visible_width - 2.0)),
            );
            text_scroll_occured = true;
            cursor_display_x = self.content_visible_width - 2.0;
        }
        cursor_rect.offset[0] = AnimatableFloat::Value(cursor_display_x);

        let (sx, sy) = ht_manager.translate_tree_local_to_root(
            self.ht_root,
            2.0 + cursor_display_x,
            2.0,
            client_size.width,
            client_size.height,
        );
        #[cfg(feature = "wayland")]
        system_link.set_ime_cursor_rect(Rect::from_lt_size(
            Point::new_logical(sx, sy),
            Size::new_logical(2.0, 16.0),
        ));
        #[cfg(feature = "wayland")]
        system_link.ime_set_surrounding_text(
            &self.content.borrow(),
            self.cursor_pos_bytes.get(),
            self.selection_begin_bytes.get(),
        );
        #[cfg(feature = "wayland")]
        system_link.ime_commit();

        composite_tree.mark_dirty(self.ct_cursor);

        if text_scroll_occured {
            composite_tree.get_mut(self.ct_text).offset[0] =
                AnimatableFloat::Value(self.content_h_offset.get());
            composite_tree.mark_dirty(self.ct_text);
            self.update_preedit_underline(composite_tree, window);
            self.update_selection(composite_tree, window);
        }
    }

    fn update_preedit_underline(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        window: WindowHandle,
    ) {
        let preedit_range =
            self.preedit_range_start_bytes.get()..self.preedit_range_end_bytes.get();
        if preedit_range.is_empty() {
            // no preedit
            composite_tree.get_mut(self.ct_preedit_underline).opacity = AnimatableFloat::Value(0.0);
            composite_tree.mark_dirty(self.ct_preedit_underline);
            return;
        }

        let o = TextLayout::measure_total_advances(
            &self.content.borrow()[..preedit_range.start],
            FontID::UIDefault,
            unsafe { &window.extra_data_ref::<PerWindowData>().font_set },
            window.ui_scale_factor(),
        );
        let tw = TextLayout::measure_total_advances(
            &self.content.borrow()[preedit_range],
            FontID::UIDefault,
            unsafe { &window.extra_data_ref::<PerWindowData>().font_set },
            window.ui_scale_factor(),
        );

        let underline_rect = composite_tree.get_mut(self.ct_preedit_underline);
        underline_rect.offset[0] = AnimatableFloat::Value(
            o / underline_rect.base_scale_factor + self.content_h_offset.get(),
        );
        underline_rect.size[0] = AnimatableFloat::Value(tw / underline_rect.base_scale_factor);
        underline_rect.opacity = AnimatableFloat::Value(1.0);

        composite_tree.mark_dirty(self.ct_preedit_underline);
    }

    fn update_selection(
        &self,
        composite_tree: &mut CompositeTree<SyncEvent>,
        window: WindowHandle,
    ) {
        let selection_range = self.selection_range();
        if selection_range.is_empty() {
            // no selection
            composite_tree.get_mut(self.ct_selection_bg).size[0] = AnimatableFloat::Value(0.0);
            composite_tree.mark_dirty(self.ct_selection_bg);
            return;
        }

        let o = TextLayout::measure_total_advances(
            &self.content.borrow()[..selection_range.start],
            FontID::UIDefault,
            unsafe { &window.extra_data_ref::<PerWindowData>().font_set },
            window.ui_scale_factor(),
        );
        let tw = TextLayout::measure_total_advances(
            &self.content.borrow()[selection_range],
            FontID::UIDefault,
            unsafe { &window.extra_data_ref::<PerWindowData>().font_set },
            window.ui_scale_factor(),
        );

        let ct = composite_tree.get_mut(self.ct_selection_bg);
        ct.offset[0] =
            AnimatableFloat::Value(o / ct.base_scale_factor + self.content_h_offset.get());
        ct.size[0] = AnimatableFloat::Value(tw / ct.base_scale_factor);

        composite_tree.mark_dirty(self.ct_selection_bg);
    }

    fn sync_selection_native(&self) {
        #[cfg(windows)]
        let selection_begin_bytes = self.selection_begin_bytes.get();
        #[cfg(windows)]
        let cursor_pos_bytes = self.cursor_pos_bytes.get();
        #[cfg(windows)]
        let selection_begin_acp = self
            .content
            .borrow()
            .char_indices()
            .take_while(|&(i, _)| i < selection_begin_bytes)
            .count();
        #[cfg(windows)]
        let cursor_pos_acp = self
            .content
            .borrow()
            .char_indices()
            .take_while(|&(i, _)| i < cursor_pos_bytes)
            .count();
        #[cfg(windows)]
        self.native_text_input_context.notify_selection_changed(
            selection_begin_acp.min(cursor_pos_acp) as _,
            selection_begin_acp.max(cursor_pos_acp) as _,
        );
    }

    fn selection_range(&self) -> core::ops::Range<usize> {
        match (
            self.cursor_pos_bytes.get(),
            self.selection_begin_bytes.get(),
        ) {
            (a, b) if a <= b => a..b,
            (a, b) => b..a,
        }
    }
}
#[cfg(windows)]
impl platform::windows::TextProvider for TextInputViewEventHandler {
    fn text(
        &self,
        range: windows::UI::Text::Core::CoreTextRange,
    ) -> windows_core::Result<windows_core::HSTRING> {
        let mut u16s = Vec::with_capacity((range.EndCaretPosition - range.StartCaretPosition) as _);
        for c in self
            .content
            .borrow()
            .chars()
            .skip(range.StartCaretPosition as _)
            .take((range.EndCaretPosition - range.StartCaretPosition) as _)
        {
            let mut buf = [0; 2];
            u16s.extend_from_slice(c.encode_utf16(&mut buf));
        }

        Ok(windows_core::HSTRING::from_wide(&u16s))
    }

    fn selection(
        &self,
        req: &windows::UI::Text::Core::CoreTextSelectionRequest,
    ) -> windows_core::Result<()> {
        let selection_begin_bytes = self.selection_begin_bytes.get();
        let cursor_pos_bytes = self.cursor_pos_bytes.get();
        let selection_begin_acp = self
            .content
            .borrow()
            .char_indices()
            .take_while(|&(i, _)| i < selection_begin_bytes)
            .count();
        let cursor_pos_acp = self
            .content
            .borrow()
            .char_indices()
            .take_while(|&(i, _)| i < cursor_pos_bytes)
            .count();

        req.SetSelection(windows::UI::Text::Core::CoreTextRange {
            StartCaretPosition: selection_begin_acp.min(cursor_pos_acp) as _,
            EndCaretPosition: selection_begin_acp.max(cursor_pos_acp) as _,
        })
    }
}
#[cfg(windows)]
impl platform::windows::CoreTextDeferrableEventHandler for TextInputViewEventHandler {
    fn layout(
        &self,
        ctx: &mut InputEventContext,
        req: &windows::UI::Text::Core::CoreTextLayoutRequest,
    ) -> windows_core::Result<()> {
        let range = req.Range()?;
        tracing::trace!(
            req.range = ?range,
            "edit_context.layout_requested"
        );

        let start_bytes = self
            .content
            .borrow()
            .chars()
            .take(range.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let end_bytes = self
            .content
            .borrow()
            .chars()
            .take(range.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        let window = ctx
            .ht_manager
            .query_root_window(self.ht_root)
            .expect("no window bound");
        let r = ctx.ht_manager.compute_screen_rect_pixels_with_insets(
            self.ht_root,
            Point::new_logical(2.0, 2.0),
            Point::new_logical(2.0, 2.0),
        );
        let o = TextLayout::measure_total_advances(
            &self.content.borrow()[..start_bytes],
            FontID::UIDefault,
            unsafe { &window.extra_data_ref::<PerWindowData>().font_set },
            window.ui_scale_factor(),
        ) + self.content_h_offset.get() * window.ui_scale_factor();
        let w = TextLayout::measure_total_advances(
            &self.content.borrow()[start_bytes..end_bytes],
            FontID::UIDefault,
            unsafe { &window.extra_data_ref::<PerWindowData>().font_set },
            window.ui_scale_factor(),
        );

        req.LayoutBounds()?
            .SetTextBounds(windows::Foundation::Rect {
                X: r.left as f32 + o,
                Y: r.top as _,
                Width: w,
                Height: r.height as _,
            })
    }

    fn text_updating(
        &self,
        ctx: &mut InputEventContext,
        e: &windows::UI::Text::Core::CoreTextTextUpdatingEventArgs,
    ) -> windows_core::Result<()> {
        let range = e.Range()?;
        let text = e.Text()?.to_string_lossy();
        let new_selection = e.NewSelection()?;
        tracing::trace!(
            ?new_selection,
            ?range,
            ?text,
            current = &self.content.borrow() as &str,
            "edit_context.text_updating"
        );

        let replace_start_bytes = self
            .content
            .borrow()
            .chars()
            .take(range.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let replace_end_bytes = self
            .content
            .borrow()
            .chars()
            .take(range.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        self.content
            .borrow_mut()
            .replace_range(replace_start_bytes..replace_end_bytes, &text);

        let new_cursor_start_bytes = self
            .content
            .borrow()
            .chars()
            .take(new_selection.StartCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());
        let new_cursor_end_bytes = self
            .content
            .borrow()
            .chars()
            .take(new_selection.EndCaretPosition as _)
            .fold(0, |a, c| a + c.len_utf8());

        let window = ctx
            .ht_manager
            .query_root_window(self.ht_root)
            .expect("no root window");
        self.selection_begin_bytes.set(new_cursor_start_bytes);
        self.cursor_pos_bytes.set(new_cursor_end_bytes);

        self.update_text(ctx.composite_tree);
        self.update_cursor_position(
            ctx.composite_tree,
            window,
            ctx.system_link,
            ctx.ht_manager,
            window.client_size(),
        );
        self.update_selection(ctx.composite_tree, window);

        e.SetResult(windows::UI::Text::Core::CoreTextTextUpdatingResult::Succeeded)?;
        Ok(())
    }

    fn format_updating(
        &self,
        ctx: &mut InputEventContext,
        e: &windows::UI::Text::Core::CoreTextFormatUpdatingEventArgs,
    ) -> windows_core::Result<()> {
        let underline_type = e.UnderlineType()?.Value()?;
        let range = e.Range()?;
        let reason = e.Reason()?;
        tracing::trace!(
            background_color = ?e.BackgroundColor(),
            ?range,
            ?reason,
            text_color = ?e.TextColor(),
            underline_color = ?e.UnderlineColor(),
            ?underline_type,
            "edit_context.format_updating"
        );

        // TODO: Windowsの場合は複数下線要素ができる場合がある（部分的に変換する場合など）
        let window = ctx
            .ht_manager
            .query_root_window(self.ht_root)
            .expect("no root window");
        if underline_type == windows::UI::Text::UnderlineType::None {
            self.preedit_range_start_bytes.set(0);
            self.preedit_range_end_bytes.set(0);
        } else {
            self.preedit_range_start_bytes.set(
                self.content
                    .borrow()
                    .chars()
                    .take(range.StartCaretPosition as _)
                    .map(|x| x.len_utf8())
                    .sum(),
            );
            self.preedit_range_end_bytes.set(
                self.content
                    .borrow()
                    .chars()
                    .take(range.EndCaretPosition as _)
                    .map(|x| x.len_utf8())
                    .sum(),
            );
        }

        self.update_preedit_underline(ctx.composite_tree, window);

        Ok(())
    }
}

pub struct TextInputView {
    ct_text_clip: CompositeTreeRef,
    eh: Rc<TextInputViewEventHandler>,
}
impl TextInputView {
    pub fn new(ctx: &mut ViewInitContext, window: WindowHandle, syslink: &SystemLink) -> Self {
        let kf_token = ctx.keyboard_focus_registry.acquire_token();
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(128.0), AnimatableFloat::Value(20.0)],
            offset: [AnimatableFloat::Value(200.0), AnimatableFloat::Value(300.0)],
            has_bitmap: true,
            border: Some(Border {
                thickness: 1.0,
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 0.5]),
            }),
            ..Default::default()
        });
        let ct_text_clip = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [
                AnimatableFloat::Value(128.0 - 4.0),
                AnimatableFloat::Value(20.0 - 4.0),
            ],
            offset: [AnimatableFloat::Value(2.0), AnimatableFloat::Value(2.0)],
            clip_child: Some(ClipConfig {
                left_softness: unsafe { SafeF32::new_unchecked(1.0) },
                right_softness: unsafe { SafeF32::new_unchecked(1.0) },
                top_softness: unsafe { SafeF32::new_unchecked(1.0) },
                bottom_softness: unsafe { SafeF32::new_unchecked(1.0) },
            }),
            ..Default::default()
        });
        let ct_text = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            has_bitmap: true,
            ..Default::default()
        });
        let ct_cursor = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(2.0), AnimatableFloat::Value(16.0)],
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(0.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_preedit_underline = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(1.0), AnimatableFloat::Value(1.0)],
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(14.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 1.0])),
            opacity: AnimatableFloat::Value(0.0),
            ..Default::default()
        });
        let ct_selection_bg = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            size: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(16.0)],
            has_bitmap: true,
            composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.2, 0.4, 1.0, 0.25])),
            ..Default::default()
        });
        let ht_root = ctx.mount_context.ht_manager.create(HitTestTreeData {
            width: 128.0,
            height: 20.0,
            left: 200.0,
            top: 300.0,
            cursor_shape: CursorShape::IBeam,
            keyboard_focus: Some(kf_token),
            ..Default::default()
        });

        ctx.composite_tree.add_child(ct_text_clip, ct_selection_bg);
        ctx.composite_tree.add_child(ct_text_clip, ct_text);
        ctx.composite_tree.add_child(ct_text_clip, ct_cursor);
        ctx.composite_tree
            .add_child(ct_text_clip, ct_preedit_underline);
        ctx.composite_tree.add_child(ct_root, ct_text_clip);

        let eh = Rc::new(TextInputViewEventHandler {
            token: kf_token,
            ct_root,
            ct_text,
            ct_cursor,
            ct_preedit_underline,
            ct_selection_bg,
            ht_root,
            content_h_offset: core::cell::Cell::new(0.0),
            content_visible_width: 128.0 - 4.0,
            content: core::cell::RefCell::new("aaa".into()),
            cursor_pos_bytes: core::cell::Cell::new(0),
            preedit_range_start_bytes: core::cell::Cell::new(0),
            preedit_range_end_bytes: core::cell::Cell::new(0),
            selection_begin_bytes: core::cell::Cell::new(0),
            dragging: core::cell::Cell::new(false),
            #[cfg(windows)]
            native_text_input_context: platform::windows::NativeTextInputContext::new(
                ctx.system_link,
            ),
        });
        ctx.keyboard_focus_registry.set_event_handler(kf_token, &eh);
        ctx.ht_manager.set_action_handler(ht_root, &eh);
        #[cfg(windows)]
        ctx.ht_manager
            .set_native_text_deferrable_event_handler(ht_root, &eh);
        #[cfg(windows)]
        eh.native_text_input_context
            .bind_action(ctx.system_link, &eh, ht_root);

        eh.update_text(ctx.composite_tree);

        Self { ct_text_clip, eh }
    }

    pub fn mount(&self, ctx: &mut MountContext, parent: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree
            .add_child(parent.ct_root(), self.eh.ct_root);
        ctx.ht_manager.add_child(parent.ht_root(), self.eh.ht_root);
    }

    pub fn rescale(
        &self,
        ct: &mut CompositeTree<SyncEvent>,
        window: WindowHandle,
        syslink: &SystemLink,
        ht_manager: &HitTestTreeManager,
        new_scale: f32,
    ) {
        ct.get_mut(self.eh.ct_root).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_root);
        ct.get_mut(self.eh.ct_text).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_text);
        ct.get_mut(self.eh.ct_cursor).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_cursor);
        ct.get_mut(self.eh.ct_preedit_underline).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_preedit_underline);
        ct.get_mut(self.ct_text_clip).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.ct_text_clip);
        ct.get_mut(self.eh.ct_selection_bg).base_scale_factor = new_scale;
        ct.mark_dirty_all(self.eh.ct_selection_bg);

        self.eh
            .update_cursor_position(ct, window, syslink, ht_manager, window.client_size());
        self.eh.update_preedit_underline(ct, window);
    }
}

struct PerWindowData {
    font_set: PerWindowFontSet<'static>,
    header: ui::window_header::View,
}

#[tracing::instrument(target = "peridot_marble_editor::logic_fiber", skip_all)]
async fn run<'sys>(
    event_queue: EventQueue,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
    mut composite_tree: CompositeTree<SyncEvent>,
    mut ht_manager: HitTestTreeManager<'sys>,
    mut main_window: WindowHandle,
    system_link: SystemLink<'sys>,
) {
    tracing::info!("app start");

    let typing_context = ThreadLocalTypingContext {
        #[cfg(feature = "freetype")]
        ft_lib: rendering::text::FreeType::init().expect("freetype.init"),
    };
    let mut keyboard_focus_registry = KeyboardFocusTokenRegistry::new();
    let mut pointer_input_manager = PointerInputManager::new();
    pointer_input_manager.set_client_size(main_window, main_window.client_size());

    // WindowsではWM_NCHITTESTの返り値の計算に必要なので一旦生ポインタで参照もたせる（実際どうするかはあとで考える）
    #[cfg(windows)]
    unsafe {
        platform::windows::locate_non_client_hittest_managers(&pointer_input_manager, &ht_manager);
    }

    let mut texture_id_issuer = MainThreadTextureIDIssuer::new();
    let init_scale = main_window.ui_scale_factor();
    let texture_id_set = ui::window_header::SystemCommandTextureIDSet::new(
        &mut texture_id_issuer,
        system_link.rt_sender(),
    );
    let mut popup_manager = PopupManager::new();

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
        },
        keyboard_focus_registry: &mut keyboard_focus_registry,
        ui_scale_factor: init_scale,
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
        font_set: unsafe {
            PerWindowFontSet::new(system_link.root_font_set(), &typing_context).lifetime_unbound()
        },
        header: window_header_view,
    }));

    // tab view
    let tab_main = view_init_ctx.composite_tree.create(CompositeRect {
        has_bitmap: true,
        base_scale_factor: init_scale,
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
            args: &PointerActionArgs,
        ) -> input::EventContinueControl {
            context
                .drag_preview
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
            context.drag_preview.r#move(&args.client_pos);

            input::EventContinueControl::STOP_PROPAGATION
        }

        fn on_drag_end(
            &self,
            sender: HitTestTreeRef,
            context: &mut InputEventContext,
            args: &PointerActionArgs,
        ) -> input::EventContinueControl {
            context.drag_preview.hide();

            input::EventContinueControl::RELEASE_CAPTURE_ELEMENT
                | input::EventContinueControl::STOP_PROPAGATION
        }

        fn on_click(
            &self,
            sender: HitTestTreeRef,
            context: &mut InputEventContext,
            args: &PointerActionArgs,
        ) -> input::EventContinueControl {
            context.system_link.dispatch_event(Event::SubWindowOpen);

            input::EventContinueControl::STOP_PROPAGATION
        }
    }
    let ht_action_handler = std::rc::Rc::new(TabHitAction { ct: tab_main });
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

    let text_input_view = TextInputView::new(&mut view_init_ctx, main_window, &system_link);
    text_input_view.mount(&mut view_init_ctx, &main_window);

    composite_tree.commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
    ht_manager.dump(main_window.ht_root());

    loop {
        match event_queue.next_event().await {
            Event::Quit => break,
            Event::SubWindowOpen => {
                system_link.open_window(
                    &mut composite_tree,
                    &mut ht_manager,
                    |mut w, composite_tree, ht_manager| {
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
                            },
                            keyboard_focus_registry: &mut keyboard_focus_registry,
                            ui_scale_factor: init_scale,
                            system_link: &system_link,
                        };
                        let window_header_view = ui::window_header::View::new(
                            &mut view_init_ctx,
                            ui::window_header::Caption::Sub,
                            &texture_id_set,
                            w.needs_system_command_buttons(),
                        );
                        window_header_view.mount(&mut view_init_ctx, &w);

                        w.associate_extra_data(Box::new(PerWindowData {
                            font_set: unsafe {
                                PerWindowFontSet::new(system_link.root_font_set(), &typing_context)
                                    .lifetime_unbound()
                            },
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
                system_link.close_window(window, &mut composite_tree, &mut ht_manager);
            }
            Event::WindowResize { window, size } => {
                pointer_input_manager.set_client_size(window, size);
            }
            Event::Sync(SyncEvent::WindowPostResizeRenderBuffer { window }) => {
                #[cfg(feature = "wayland")]
                window.update_manual_scaling();
            }
            Event::WindowRescaleUI {
                mut window,
                new_scale,
            } => {
                let wd = unsafe { window.extra_data_mut::<PerWindowData>() };
                #[cfg(feature = "freetype")]
                wd.font_set.rescale((new_scale * 72.0) as _);
                wd.header
                    .rescale(new_scale, &mut composite_tree, &texture_id_set);

                popup_manager.rescale(window, new_scale, &mut composite_tree);

                if window == main_window {
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
                let mut ht_create_only_access = ht_manager.derive_create_only_access();
                let mut input_context = InputEventContext {
                    sender_window: window,
                    composite_tree: &mut composite_tree,
                    current_sec: global_time_base.elapsed().as_secs_f32(),
                    drag_preview: system_link.drag_preview_popover(),
                    system_link: &system_link,
                    ht_create_only_access: &mut ht_create_only_access,
                    ht_manager: &ht_manager,
                };
                let mgr = window.keyboard_focus_state_mut();

                if focused {
                    mgr.notify_window_focus(&mut input_context, &keyboard_focus_registry);
                } else {
                    mgr.notify_window_lost_focus(&mut input_context, &keyboard_focus_registry);
                }
            }
            Event::PointerDown {
                window,
                #[cfg(feature = "wayland")]
                event_id,
            } => {
                #[cfg(feature = "wayland")]
                system_link
                    .drag_preview_popover()
                    .bind_parent_window(window);
                #[cfg(windows)]
                system_link
                    .drag_preview_popover()
                    .bind_position_base_window(window);
                #[cfg(target_os = "macos")]
                system_link
                    .drag_preview_popover()
                    .bind_position_base_window_link(window);

                // waylandの場合はここでTitleBarロールの判定をする
                // 他PFではシステム側でやってくれる/ウィンドウコールバック内でないといけない
                #[cfg(feature = "wayland")]
                if pointer_input_manager.role_focus(&ht_manager)
                    == Some(input::hittest::Role::TitleBar)
                {
                    window.begin_drag(event_id);
                }

                let mut ht_create_only_access = ht_manager.derive_create_only_access();
                pointer_input_manager.handle_mouse_left_down(
                    &window,
                    &ht_manager,
                    &mut InputEventContext {
                        sender_window: window,
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
                        ht_manager: &ht_manager,
                    },
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
                let mut ht_create_only_access = ht_manager.derive_create_only_access();
                pointer_input_manager.handle_mouse_move(
                    window,
                    client_pos,
                    &window,
                    &ht_manager,
                    &mut InputEventContext {
                        sender_window: window,
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
                        ht_manager: &ht_manager,
                    },
                    window.ht_root(),
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);

                let cursor_shape = pointer_input_manager.cursor_shape(&ht_manager);
                system_link.set_cursor(&pointer_id, cursor_shape);
            }
            Event::PointerUp { window } => {
                let mut ht_create_only_access = ht_manager.derive_create_only_access();
                pointer_input_manager.handle_mouse_left_up(
                    &window,
                    &ht_manager,
                    &mut InputEventContext {
                        sender_window: window,
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
                        ht_manager: &ht_manager,
                    },
                    window.ht_root(),
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::KeyDown { window, code } => {
                let mut ht_create_only_access = ht_manager.derive_create_only_access();
                window.keyboard_focus_state().handle_keydown(
                    code,
                    &mut InputEventContext {
                        sender_window: window,
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
                        ht_manager: &ht_manager,
                    },
                    &keyboard_focus_registry,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::KeyUp { window, code } => {
                let mut ht_create_only_access = ht_manager.derive_create_only_access();
                window.keyboard_focus_state().handle_keyup(
                    code,
                    &mut InputEventContext {
                        sender_window: window,
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
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
                let mut ht_create_only_access = ht_manager.derive_create_only_access();
                window.keyboard_focus_state().handle_ime_state_changes(
                    &committed_string,
                    &preedit_string,
                    &mut InputEventContext {
                        sender_window: window,
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
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
                popup_manager.open(
                    &mut ViewInitContext {
                        mount_context: MountContext {
                            composite_tree: &mut composite_tree,
                            ht_manager: &mut ht_manager,
                            current_sec: global_time_base.elapsed().as_secs_f32(),
                        },
                        keyboard_focus_registry: &mut keyboard_focus_registry,
                        ui_scale_factor: main_window.ui_scale_factor(),
                        system_link: &system_link,
                    },
                    target_window,
                    |id, ctx| AlertDialogPresenter::new(ctx, id, message),
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
                    },
                    id,
                ) {
                    composite_tree
                        .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
                        let mut ht_create_only_access = ht_manager.derive_create_only_access();
                        if let Err(e) = w.layout(
                            &mut InputEventContext {
                                sender_window: main_window,
                                composite_tree: &mut composite_tree,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                drag_preview: system_link.drag_preview_popover(),
                                system_link: &system_link,
                                ht_create_only_access: &mut ht_create_only_access,
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
                        let mut ht_create_only_access = ht_manager.derive_create_only_access();
                        if let Err(e) = w.text_updating(
                            &mut InputEventContext {
                                sender_window: main_window,
                                composite_tree: &mut composite_tree,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                drag_preview: system_link.drag_preview_popover(),
                                system_link: &system_link,
                                ht_create_only_access: &mut ht_create_only_access,
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
                        let mut ht_create_only_access = ht_manager.derive_create_only_access();
                        if let Err(e) = w.format_updating(
                            &mut InputEventContext {
                                sender_window: main_window,
                                composite_tree: &mut composite_tree,
                                current_sec: global_time_base.elapsed().as_secs_f32(),
                                drag_preview: system_link.drag_preview_popover(),
                                system_link: &system_link,
                                ht_create_only_access: &mut ht_create_only_access,
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

#[cfg(windows)]
pub type SystemLink<'sys> = platform::windows::SystemLink<'sys>;

#[cfg(not(windows))]
pub struct SystemLink<'sys> {
    drag_preview_popover: DragPreviewPopoverHandle,
    vk_device: *const VulkanDevice<'sys>,
    rt_sender: std::sync::mpsc::Sender<RenderMessage>,
    root_font_set: *const RootFontSet,
    event_dispatcher: *mut LogicFiberEventDispatcher,
    #[cfg(all(unix, not(target_os = "macos")))]
    display_server: platform::unix::DisplayServerLink,
    #[cfg(target_os = "linux")]
    dbus: *const dbus::Connection,
}
#[cfg(not(windows))]
impl SystemLink<'_> {
    #[inline(always)]
    pub const fn rt_sender(&self) -> &std::sync::mpsc::Sender<RenderMessage> {
        &self.rt_sender
    }

    #[inline(always)]
    pub fn drag_preview_popover(&self) -> &DragPreviewPopoverHandle {
        &self.drag_preview_popover
    }

    #[inline(always)]
    pub const fn root_font_set(&self) -> &RootFontSet {
        unsafe { &*self.root_font_set }
    }

    #[inline(always)]
    pub fn dispatch_event(&self, event: Event) {
        unsafe { &*self.event_dispatcher }.dispatch(event);
    }

    #[cfg(target_os = "macos")]
    pub fn open_window<'h, HT: HitTestTreeCreate<'h> + ?Sized>(
        &self,
        composite_tree: &mut CompositeTree<Event>,
        hit_tree: &mut HT,
        setup_content: impl FnOnce(WindowHandle, &mut CompositeTree<Event>, &mut HT),
    ) -> WindowHandle {
        let mut w = MacWindow::new(
            WindowType::Sub,
            platform::mac::bridge::WindowCreationFlags::empty(),
            unsafe { (*self.event_dispatcher).clone() },
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            hit_tree.create(HitTestTreeData {
                width_adjustment_factor: 1.0,
                height_adjustment_factor: 1.0,
                ..Default::default()
            }),
        );
        let handle = w.make_handle();
        w.show();
        // notify resize on show(register to pointer input manager)
        let mut width = core::mem::MaybeUninit::uninit();
        let mut height = core::mem::MaybeUninit::uninit();
        unsafe {
            platform::mac::bridge::ni_get_size_logical(
                w.native_ptr,
                width.as_mut_ptr(),
                height.as_mut_ptr(),
            )
        }
        unsafe { &*self.event_dispatcher }.dispatch(Event::WindowResize {
            window: handle,
            size: Size::new_logical(unsafe { width.assume_init() as _ }, unsafe {
                height.assume_init() as _
            }),
        });

        let vk_surface = graphics::VulkanSurface::new(unsafe { &*self.vk_device }, unsafe {
            bedrock::SurfaceCreateInfo::execute(
                &bedrock::MetalSurfaceCreateInfo::new(w.metal_layer()),
                bedrock::InstanceChild::instance(&*self.vk_device),
                None,
            )
            .expect("vk_surface.create")
        });
        self.rt_sender
            .send(RenderMessage::NewWindow(rendering::NewWindowData {
                init_scale: SafeF32::new(
                    *w.dispatcher()
                        .state
                        .active_buffer_scale
                        .lock()
                        .expect("poisoned"),
                )
                .expect("invalid scale"),
                latest_ui_scale_changes: utils::UnboundedRef::new(
                    &w.dispatcher().state.latest_ui_scale_changes,
                ),
                key: handle,
                vk_surface: rendering::NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");

        setup_content(handle, composite_tree, hit_tree);
        handle
    }

    #[cfg(target_os = "macos")]
    pub fn close_window(&self, window_handle: WindowHandle) {
        let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
        self.rt_sender
            .send(RenderMessage::DestroyWindow(
                window_handle,
                done_event_sender,
            ))
            .expect("rt_sender.send.destroy_window");
        let tpctx = unsafe { platform::mac::bridge::ni_degreade_thread_priroity_temporarily() };
        done_event_receiver
            .recv()
            .expect("done_event_receiver.recv");
        unsafe {
            platform::mac::bridge::ni_restore_thread_priority(tpctx);
        }

        unsafe {
            platform::mac::bridge::ni_release_window(window_handle.0);
        }
    }

    #[cfg(target_os = "macos")]
    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        unsafe {
            platform::mac::bridge::ni_set_cursor_shape(match cursor {
                CursorShape::Default => platform::mac::bridge::CursorShape::Arrow as _,
                CursorShape::Pointer => platform::mac::bridge::CursorShape::Pointer as _,
                CursorShape::IBeam => platform::mac::bridge::CursorShape::IBeam as _,
                CursorShape::ResizeHorizontal => {
                    platform::mac::bridge::CursorShape::ResizeHorizontal as _
                }
            })
        }
    }

    #[cfg(target_os = "macos")]
    pub fn notify_ui_scale_changes_to_render(&self, _window: WindowHandle, _new_scale: f32) {
        // TODO: これmacでやることあるのか？（起こらない気がする）
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
#[cfg(feature = "wayland")]
pub type PointerID = platform::unix::wayland::PointerID;

#[cfg(windows)]
pub type DragPreviewPopoverHandle = platform::windows::DragPreviewPopoverHandle;
#[cfg(feature = "wayland")]
pub type DragPreviewPopoverHandle = platform::unix::wayland::DragPreviewPopoverHandle;

#[cfg(windows)]
pub type WindowHandle = platform::windows::WindowHandle;
#[cfg(feature = "wayland")]
pub type WindowHandle = platform::unix::wayland::WindowHandle;

#[cfg(target_os = "macos")]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct WindowHandle(*mut crate::platform::mac::bridge::WindowLink);
#[cfg(target_os = "macos")]
unsafe impl Sync for WindowHandle {}
#[cfg(target_os = "macos")]
unsafe impl Send for WindowHandle {}
#[cfg(target_os = "macos")]
impl WindowHandle {
    #[inline(always)]
    pub fn state(&self) -> &MacWindowState {
        unsafe {
            &(*crate::platform::mac::bridge::ni_get_window_callback_context(self.0)
                .cast::<MacWindowDispatcher>())
            .state
        }
    }

    #[inline(always)]
    fn state_mut(&mut self) -> &mut MacWindowState {
        unsafe {
            &mut (*crate::platform::mac::bridge::ni_get_window_callback_context(self.0)
                .cast::<MacWindowDispatcher>())
            .state
        }
    }

    #[inline(always)]
    pub fn associate_extra_data<T>(&mut self, data: Box<T>) {
        self.state_mut().extra_data = Box::into_raw(data) as _;
    }

    #[inline(always)]
    pub unsafe fn extra_data_ref<T>(&self) -> &T {
        unsafe { &*self.state().extra_data.cast() }
    }

    #[inline(always)]
    pub unsafe fn extra_data_mut<T>(&mut self) -> &mut T {
        unsafe { &mut *self.state_mut().extra_data.cast() }
    }

    #[inline(always)]
    pub unsafe fn take_extra_data<T>(&mut self) -> Box<T> {
        let r = unsafe { Box::from_raw(self.state_mut().extra_data.cast()) };
        self.state_mut().extra_data = core::ptr::null_mut();

        r
    }

    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        let state = self.state();

        state
            .active_rt_size
            .lock()
            .expect("poisoned")
            .to_logical(*state.active_buffer_scale.lock().expect("poisoned"))
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        *self.state().active_buffer_scale.lock().expect("poisoned")
    }

    #[inline(always)]
    pub fn composite_root(&self) -> CompositeTreeRef {
        self.state().composite_root
    }

    #[inline(always)]
    pub fn ht_root(&self) -> HitTestTreeRef {
        self.state().ht_root
    }

    #[inline(always)]
    pub fn keyboard_focus_state(&self) -> &PerWindowKeyboardFocusState {
        &self.state().keyboard_focus_state
    }

    #[inline(always)]
    pub fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        &mut self.state_mut().keyboard_focus_state
    }
}
#[cfg(target_os = "macos")]
impl input::ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {}

    #[inline(always)]
    fn release_pointer(&self) {}
}

pub struct SyncEventBus {
    queue: std::sync::Mutex<VecDeque<SyncEvent>>,
    #[cfg(target_os = "linux")]
    efd: linux_eventfd::EventFD,
    #[cfg(windows)]
    event_notify: windows::Win32::Foundation::HANDLE,
}
#[cfg(windows)]
unsafe impl Sync for SyncEventBus {}
#[cfg(windows)]
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
    pub fn push(&self, e: SyncEvent) {
        self.queue.lock().expect("poisoned").push_back(e);
        #[cfg(target_os = "linux")]
        self.efd.inc(1).unwrap();
        #[cfg(windows)]
        unsafe {
            windows::Win32::System::Threading::SetEvent(self.event_notify)
                .expect("event_notify.set");
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

#[cfg(target_os = "macos")]
pub struct DragPreviewPopoverHandle {
    position_base_window_link: core::cell::Cell<*mut platform::mac::bridge::WindowLink>,
}
#[cfg(target_os = "macos")]
impl DragPreviewPopoverHandle {
    #[inline(always)]
    pub fn bind_position_base_window_link(&self, w: WindowHandle) {
        self.position_base_window_link.set(w.0);
    }

    pub fn show(&self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
        unsafe {
            // macの場合はスクリーン座標が必要
            let mut x = pos.x as f64;
            let mut y = pos.y as f64;
            platform::mac::bridge::ni_convert_point_to_screen(
                self.position_base_window_link.get(),
                &mut x,
                &mut y,
            );

            platform::mac::bridge::ni_show_drag_preview(x, y, size.width as _, size.height as _);
        }
    }

    pub fn r#move(&self, pos: &Point<PointerInputUnit>) {
        unsafe {
            // macの場合はスクリーン座標が必要
            let mut x = pos.x as f64;
            let mut y = pos.y as f64;
            platform::mac::bridge::ni_convert_point_to_screen(
                self.position_base_window_link.get(),
                &mut x,
                &mut y,
            );

            platform::mac::bridge::ni_move_drag_preview(x, y);
        }
    }

    pub fn hide(&self) {
        unsafe {
            platform::mac::bridge::ni_hide_drag_preview();
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
        });
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

#[cfg(target_os = "macos")]
#[derive(Clone, Copy)]
pub struct PointerID();

#[cfg(target_os = "macos")]
pub struct MacWindow {
    native_ptr: *mut platform::mac::bridge::WindowLink,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindow {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindow {}
#[cfg(target_os = "macos")]
impl MacWindow {
    pub fn new(
        window_type: WindowType,
        flags: platform::mac::bridge::WindowCreationFlags,
        event_dispatcher: LogicFiberEventDispatcher,
        composite_root: CompositeTreeRef,
        ht_root: HitTestTreeRef,
    ) -> Self {
        let native_ptr = unsafe { platform::mac::bridge::ni_create_window(flags.bits()) };
        let init_scale = unsafe { platform::mac::bridge::ni_get_content_scale(native_ptr) };
        let dispatcher = Box::new(MacWindowDispatcher {
            event_dispatcher,
            window_type,
            state: MacWindowState {
                wlink: native_ptr,
                swapchain_externally_invalidation_signal: std::sync::Arc::new(
                    std::sync::atomic::AtomicBool::new(false),
                ),
                latest_ui_scale_changes: Mutex::new(None),
                active_size: std::sync::Mutex::new(Size::new_logical(960.0, 540.0)),
                active_rt_size: std::sync::Mutex::new(
                    Size::new_logical(960.0, 540.0).to_pixels_ceil(init_scale),
                ),
                active_buffer_scale: std::sync::Mutex::new(init_scale),
                composite_root,
                ht_root,
            },
        });
        let callbacks: &'static platform::mac::bridge::WindowLinkCallbacks =
            &platform::mac::bridge::WindowLinkCallbacks {
                destructor: MacWindowDispatcher::destructor,
                on_window_close: MacWindowDispatcher::on_window_close,
                on_resize: MacWindowDispatcher::on_resize,
                on_pointer_down: MacWindowDispatcher::on_pointer_down,
                on_pointer_move: MacWindowDispatcher::on_pointer_move,
                on_pointer_up: MacWindowDispatcher::on_pointer_up,
            };
        unsafe {
            platform::mac::bridge::ni_set_window_callbacks(
                native_ptr,
                callbacks,
                Box::into_raw(dispatcher) as _,
            );
        }

        Self { native_ptr }
    }

    #[inline(always)]
    pub const fn make_handle(&self) -> WindowHandle {
        WindowHandle(self.native_ptr)
    }

    #[inline(always)]
    pub fn dispatcher(&self) -> &MacWindowDispatcher {
        unsafe { &*platform::mac::bridge::ni_get_window_callback_context(self.native_ptr).cast() }
    }

    #[inline(always)]
    pub fn dispatcher_mut(&mut self) -> &mut MacWindowDispatcher {
        unsafe {
            &mut *platform::mac::bridge::ni_get_window_callback_context(self.native_ptr).cast()
        }
    }

    #[inline(always)]
    pub fn rebind_event_dispatcher(&mut self, dispatcher: LogicFiberEventDispatcher) {
        self.dispatcher_mut().event_dispatcher = dispatcher;
    }

    #[inline(always)]
    pub fn make_primary_window(&mut self) {
        unsafe {
            platform::mac::bridge::ni_make_primary_window(self.native_ptr);
        }
    }

    #[inline(always)]
    pub fn show(&mut self) {
        unsafe {
            platform::mac::bridge::ni_show_window(self.native_ptr);
        }
    }

    #[inline(always)]
    pub fn metal_layer(&self) -> *mut core::ffi::c_void {
        unsafe { platform::mac::bridge::ni_get_metal_layer(self.native_ptr) }
    }

    #[inline(always)]
    pub fn manual_capture_begin(&self) {
        unsafe {
            platform::mac::bridge::manual_capture_begin(self.native_ptr);
        }
    }
}

#[cfg(target_os = "macos")]
struct MacWindowDispatcher {
    event_dispatcher: LogicFiberEventDispatcher,
    window_type: WindowType,
    state: MacWindowState,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindowDispatcher {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindowDispatcher {}
#[cfg(target_os = "macos")]
impl MacWindowDispatcher {
    extern "C" fn destructor(this: *mut core::ffi::c_void) {
        tracing::trace!(?this, "window_dispatcher.destruct");
        drop(unsafe { Box::from_raw(this.cast::<Self>()) });
    }

    extern "C" fn on_window_close(
        caller_context: *mut core::ffi::c_void,
        window: *mut platform::mac::bridge::WindowLink,
    ) {
        let this = unsafe { &*caller_context.cast::<Self>() };
        if let WindowType::Sub = this.window_type {
            this.event_dispatcher.dispatch(Event::SubWindowClose {
                window: WindowHandle(window),
            });
        }
    }

    extern "C" fn on_resize(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        width: f64,
        height: f64,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        let new_size = Size::new_logical(width as _, height as _);
        let mut active_size_locked = this.state.active_size.lock().expect("poisoned");
        if new_size != *active_size_locked {
            *active_size_locked = new_size;
            *this.state.active_rt_size.lock().expect("poisoned") =
                new_size.to_pixels_ceil(*this.state.active_buffer_scale.lock().expect("poisoned"));
            this.state
                .swapchain_externally_invalidation_signal
                .store(true, std::sync::atomic::Ordering::Relaxed);
            this.event_dispatcher.dispatch(Event::WindowResize {
                window: WindowHandle(window),
                size: new_size,
            });
        }
    }

    extern "C" fn on_pointer_down(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        x: f64,
        y: f64,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // tracing::info!(x, y, "pointer down");
        this.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(window),
            client_pos: Point::new_logical(x as _, y as _),
        });
        this.event_dispatcher.dispatch(Event::PointerDown {
            window: WindowHandle(window),
        });
    }

    extern "C" fn on_pointer_move(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
        x: f64,
        y: f64,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // tracing::trace!(x, y, "pointer move");
        this.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(window),
            client_pos: Point::new_logical(x as _, y as _),
        });
    }

    extern "C" fn on_pointer_up(
        caller_context: *mut core::ffi::c_void,
        window: *mut crate::platform::mac::bridge::WindowLink,
    ) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // tracing::info!("pointer up");
        this.event_dispatcher.dispatch(Event::PointerUp {
            window: WindowHandle(window),
        });
    }
}

#[cfg(target_os = "macos")]
struct MacWindowState {
    wlink: *mut platform::mac::bridge::WindowLink,
    extra_data: *mut core::ffi::c_void,
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    latest_ui_scale_changes: Mutex<Option<f32>>,
    active_size: std::sync::Mutex<Size<LogicalUnit>>,
    active_rt_size: std::sync::Mutex<Size<PixelsUnit>>,
    active_buffer_scale: std::sync::Mutex<f32>,
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    keyboard_focus_state: PerWindowKeyboardFocusState,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindowState {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindowState {}
