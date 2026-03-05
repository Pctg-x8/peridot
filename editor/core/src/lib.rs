use bedrock::{self as br, InstanceChild, PhysicalDevice, SurfaceCreateInfo};
use core::pin::Pin;
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
#[cfg(feature = "wayland")]
use std::{collections::HashMap, sync::RwLock};
use std::{
    collections::VecDeque,
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
};
#[cfg(windows)]
use windows::Win32::{
    System::WinRT::{
        CreateDispatcherQueueController, DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT,
        DispatcherQueueOptions,
    },
    UI::WindowsAndMessaging::{DispatchMessageW, GetMessageW, TranslateMessage},
};

use crate::{
    graphics::{VulkanDevice, VulkanSurface},
    hittest::{
        CursorShape, HitTestTreeActionHandler, HitTestTreeCreate, HitTestTreeData,
        HitTestTreeManager, HitTestTreeRef,
    },
    input::{KeyboardFocusManager, PointerInputManager, PointerInputUnit, ShellPointerActions},
    rendering::{
        NewWindowData, NewWindowVulkanSurface, RenderMessage, RenderThread,
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef,
            CompositeTreeSyncBuffer,
        },
        text::FontID,
    },
    utils::{Color32, LogicalUnit, PixelsUnit, Point, SafeF32, Size, UnboundedRef},
};

#[cfg(windows)]
mod bindgen;
mod graphics;
mod hittest;
mod input;
mod platform;
mod proto;
mod rendering;
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

    let mut event_store = core::pin::pin!(VecDeque::new());
    let event_queue = EventQueue {
        event_store: event_store.as_mut().get_mut(),
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
        event_store,
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
        CompositeTree<Event>,
        HitTestTreeManager<'sys>,
        WindowHandle,
        SystemLink<'sys>,
    ) -> AppFuture,
    mut event_store: Pin<&mut VecDeque<Event>>,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
    fs: &'sys FileSystem,
) {
    let events = AppEventBus {
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
    let hinstance = utils::platform::windows::current_instance_handle();
    #[cfg(windows)]
    let wc_set = platform::windows::WindowClassSet::register(hinstance);
    #[cfg(windows)]
    let app_runtime = utils::platform::windows::WindowsAppRuntimeBootstrap::init();
    #[cfg(windows)]
    let _dispatcher_queue = unsafe {
        CreateDispatcherQueueController(DispatcherQueueOptions {
            dwSize: core::mem::size_of::<DispatcherQueueOptions>() as _,
            threadType: DQTYPE_THREAD_CURRENT,
            apartmentType: DQTAT_COM_ASTA,
        })
        .expect("dispatchqueuecontroller.create")
    };
    #[cfg(windows)]
    let native_compositor =
        windows::UI::Composition::Compositor::new().expect("win.compositor.create");

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
    let drag_preview_popover = DragPreviewPopoverHandle::new(hinstance, &native_compositor);

    #[cfg(feature = "wayland")]
    let popover_buf = if let Some(ref spb) = wl_interfaces.single_pixel_buffer_manager {
        let c = DragPreviewPopoverHandle::BG_COLOR.premultiplied();
        let b = spb
            .create_u32_rgba_buffer(c.r_u32(), c.g_u32(), c.b_u32(), c.a_u32())
            .expect("popup_buf.create.single_pixel_buffer");

        platform::unix::wayland::DragPreviewPopoverBuffer::SinglePixel(b)
    } else {
        // traditional shm-based single pixel buffer
        let shm_region = utils::platform::unix::TemporalSharedMemory::new_unique(
            c"/pme_shm",
            libc::O_RDWR,
            0o0600,
        )
        .expect("buf.shm.create")
        .expect("buf.shm.create.non_unique");
        unsafe {
            utils::platform::unix::ftruncate(&shm_region, 4).expect("buf.shm.resize");
        }

        let mapped = utils::platform::unix::MappedMemory::new(
            None,
            4,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            &shm_region,
            0,
        )
        .expect("buf.mmap");
        unsafe {
            core::ptr::write(
                mapped.as_ptr().cast::<u32>(),
                DragPreviewPopoverHandle::BG_COLOR
                    .premultiplied()
                    .argb8888(),
            );
        }

        let shmp = wl_interfaces
            .shm
            .create_pool(&shm_region, 4)
            .expect("shmp.create.popup");
        let buf = shmp
            .create_buffer(0, 1, 1, 4, wl::ShmFormat::ARGB8888)
            .expect("buf.create.popup");

        platform::unix::wayland::DragPreviewPopoverBuffer::Shm {
            shm_region,
            mapped,
            shm_pool: shmp,
            buf,
        }
    };

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
    let mut polling = core::pin::pin!(false);
    let empty_dispatcher = LogicFiberEventDispatcher {
        event_store: event_store.as_mut().get_mut() as *mut _ as _,
        polling: polling.as_mut().get_mut(),
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
        &wc_set,
    );

    #[cfg(feature = "wayland")]
    let main_window_handle = SystemLink::init_main_window(
        &wl_display,
        &wl_interfaces,
        &mut window_registry,
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
    let vk_surface = VulkanSurface::new(&vk_device, unsafe {
        br::MetalSurfaceCreateInfo::new(w.metal_layer())
            .execute(vk_device.instance(), None)
            .expect("vk_surface.create")
    });

    #[cfg(target_os = "macos")]
    rt_sender
        .send(RenderMessage::NewWindow(NewWindowData {
            #[cfg(target_os = "macos")]
            init_scale: SafeF32::new(
                *w.dispatcher
                    .state
                    .active_buffer_scale
                    .lock()
                    .expect("poisoned"),
            )
            .expect("invalid scale"),
            #[cfg(target_os = "macos")]
            latest_ui_scale_changes: UnboundedRef::new(&w.dispatcher.state.latest_ui_scale_changes),
            key: main_window_handle,
            vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            #[cfg(target_os = "macos")]
            composite_root: w.dispatcher.state.composite_root,
        }))
        .expect("rt_sender.send");

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
            rt_sender: rt_sender.clone(),
            vk_device: &vk_device,
            event_dispatcher: app_event_dispatcher.as_mut().get_mut(),
            window_class_set: &wc_set,
        },
        #[cfg(not(windows))]
        SystemLink {
            drag_preview_popover,
            rt_sender: rt_sender.clone(),
            vk_device: &vk_device,
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
            event_store: event_store.as_mut().get_mut() as *mut _ as _,
            polling: polling.as_mut().get_mut(),
            poll_fn_ptr: unsafe {
                core::mem::transmute(AppFuture::poll as *const core::ffi::c_void)
            },
            future_ptr: unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ },
        });
    #[cfg(windows)]
    SystemLink::postinit_main_window(
        main_window_handle,
        LogicFiberEventDispatcher {
            event_store: event_store.as_mut().get_mut() as *mut _ as _,
            polling: polling.as_mut().get_mut(),
            poll_fn_ptr: unsafe {
                core::mem::transmute(AppFuture::poll as *const core::ffi::c_void)
            },
            future_ptr: unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ },
        },
    );
    #[cfg(target_os = "macos")]
    w.rebind_event_dispatcher(LogicFiberEventDispatcher {
        event_store: event_store.as_mut().get_mut() as *mut _ as _,
        polling: polling.as_mut().get_mut(),
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

    let _ = app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }));

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
        #[cfg(target_os = "linux")]
        let poll_id_to_watch_ref = core::cell::UnsafeCell::new(HashMap::new());
        #[cfg(target_os = "linux")]
        dbus.set_watch_functions(Box::new(DBusWatcher {
            epoll: &epoll,
            last_poll_id: 100,
            fd_to_poll_id: HashMap::new(),
            poll_id_to_watch_ref: &poll_id_to_watch_ref,
        }));
        #[cfg(target_os = "linux")]
        let mut events = [const { core::mem::MaybeUninit::uninit() }; 8];
        #[cfg(target_os = "linux")]
        'app: loop {
            #[cfg(feature = "wayland")]
            if platform::unix::wayland::dp_prepare_read(&mut wl_display).is_err() {
                break 'app;
            }
            let active_events = epoll.wait(&mut events, None).expect("epoll.wait");

            let mut wl_display_signal = false;
            let mut terminate_signal = false;
            let mut dbus_signal = false;
            for n in 0..active_events {
                let e = unsafe { events[n as usize].assume_init_ref() };
                if e.value() == 0 {
                    wl_display_signal = true;
                } else if e.value() == 1 {
                    terminate_signal = true;
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

            #[cfg(target_os = "linux")]
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
        let mut msg = core::mem::MaybeUninit::uninit();
        #[cfg(windows)]
        'app: loop {
            match unsafe { GetMessageW(msg.as_mut_ptr(), None, 0, 0) } {
                windows_core::BOOL(0) => break 'app,
                windows_core::BOOL(-1) => {
                    Err::<(), _>(std::io::Error::last_os_error()).expect("GetMessageW")
                }
                _ => unsafe {
                    let msg = msg.assume_init_ref();
                    let _ = TranslateMessage(msg);
                    DispatchMessageW(msg);
                },
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

        #[cfg(windows)]
        app_runtime.shutdown();
    });
}

struct RendererSync {
    pub composite_buffer: CompositeTreeSyncBuffer<Event>,
}

#[derive(Clone)]
pub enum Event {
    Quit,
    PointerDown {
        window: WindowHandle,
    },
    PointerMove {
        pointer_id: PointerID,
        window: WindowHandle,
        client_pos: Point<PointerInputUnit>,
    },
    PointerUp {
        window: WindowHandle,
    },
    WindowResize {
        window: WindowHandle,
        size: Size<PointerInputUnit>,
    },
    WindowRescaleUI {
        window: WindowHandle,
        new_scale: f32,
    },
    SubWindowClose {
        window: WindowHandle,
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

#[tracing::instrument(target = "peridot_marble_editor::logic_fiber", skip_all)]
async fn run<'sys>(
    event_queue: EventQueue,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
    mut composite_tree: CompositeTree<Event>,
    mut ht_manager: HitTestTreeManager<'sys>,
    main_window: WindowHandle,
    system_link: SystemLink<'sys>,
) {
    tracing::info!("app start");

    let mut keyboard_focus_manager = KeyboardFocusManager::new();
    let mut pointer_input_manager = PointerInputManager::new();
    pointer_input_manager.set_client_size(main_window, main_window.client_size());

    // WindowsではWM_NCHITTESTの返り値の計算に必要なので一旦生ポインタで参照もたせる（実際どうするかはあとで考える）
    #[cfg(windows)]
    unsafe {
        platform::windows::locate_non_client_hittest_managers(&pointer_input_manager, &ht_manager);
    }

    composite_tree
        .get_mut(main_window.composite_root())
        .composite_mode = CompositeMode::FillColor(AnimatableColor::Value([0.1, 0.2, 0.3, 1.0]));
    composite_tree
        .get_mut(main_window.composite_root())
        .has_bitmap = true;
    composite_tree.mark_dirty(main_window.composite_root());

    let init_scale = main_window.ui_scale_factor();

    // app title view
    #[cfg(target_os = "macos")]
    let title_bar_thickness = 32.0;
    #[cfg(not(target_os = "macos"))]
    let title_bar_thickness = 24.0;
    let app_title = composite_tree.create(CompositeRect {
        has_bitmap: true,
        base_scale_factor: init_scale,
        composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.125])),
        relative_size_adjustment: [1.0, 0.0],
        size: [
            AnimatableFloat::Value(0.0),
            AnimatableFloat::Value(title_bar_thickness),
        ],
        text: Some(CompositeRectText {
            runs: vec![
                CompositeRectTextRun {
                    font_id: FontID::UIDefault,
                    content: "Peridot Marble Editor".into(),
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                },
                CompositeRectTextRun {
                    font_id: FontID::UITitleProjectName,
                    content: "New Project".into(),
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    spacing_inline_start: 4.0,
                    ..Default::default()
                },
            ],
            horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
            vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
            ..Default::default()
        }),
        ..Default::default()
    });
    composite_tree.add_child(main_window.composite_root(), app_title);
    let ht_caption_bar = ht_manager.create(HitTestTreeData {
        width_adjustment_factor: 1.0,
        height: title_bar_thickness,
        role: Some(crate::hittest::Role::TitleBar),
        ..Default::default()
    });
    ht_manager.add_child(main_window.ht_root(), ht_caption_bar);

    // tab view
    let tab_main = composite_tree.create(CompositeRect {
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
    composite_tree.add_child(main_window.composite_root(), tab_main);
    let ht_tab_main = ht_manager.create(HitTestTreeData {
        left: 100.0,
        top: 100.0,
        width: 100.0,
        height: 36.0,
        cursor_shape: hittest::CursorShape::Pointer,
        ..Default::default()
    });
    ht_manager.add_child(main_window.ht_root(), ht_tab_main);

    struct TabHitAction {
        ct: CompositeTreeRef,
    }
    impl HitTestTreeActionHandler for TabHitAction {
        fn on_pointer_enter(
            &self,
            sender: hittest::HitTestTreeRef,
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
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
            sender: hittest::HitTestTreeRef,
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
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
            sender: hittest::HitTestTreeRef,
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
        ) -> input::EventContinueControl {
            context
                .drag_preview
                .show(&args.client_pos, &Size::new_logical(128.0, 128.0));

            input::EventContinueControl::CAPTURE_ELEMENT
                | input::EventContinueControl::STOP_PROPAGATION
        }

        fn on_drag_move(
            &self,
            sender: hittest::HitTestTreeRef,
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
        ) -> input::EventContinueControl {
            context.drag_preview.r#move(&args.client_pos);

            input::EventContinueControl::STOP_PROPAGATION
        }

        fn on_drag_end(
            &self,
            sender: hittest::HitTestTreeRef,
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
        ) -> input::EventContinueControl {
            context.drag_preview.hide();

            input::EventContinueControl::RELEASE_CAPTURE_ELEMENT
                | input::EventContinueControl::STOP_PROPAGATION
        }

        fn on_click(
            &self,
            sender: HitTestTreeRef,
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
        ) -> input::EventContinueControl {
            context
                .system_link
                .open_window(context.composite_tree, context.ht_create_only_access);

            input::EventContinueControl::STOP_PROPAGATION
        }
    }
    let ht_action_handler = std::rc::Rc::new(TabHitAction { ct: tab_main });
    ht_manager.set_action_handler(ht_tab_main, &ht_action_handler);

    let ct_alert_btn = composite_tree.create(CompositeRect {
        base_scale_factor: init_scale,
        size: [AnimatableFloat::Value(64.0), AnimatableFloat::Value(24.0)],
        offset: [AnimatableFloat::Value(200.0), AnimatableFloat::Value(64.0)],
        text: Some(CompositeRectText {
            runs: vec![CompositeRectTextRun {
                font_id: FontID::UIDefault,
                content: "Test Alert".into(),
                color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                ..Default::default()
            }],
            horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
            vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
            ..Default::default()
        }),
        has_bitmap: true,
        composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.0])),
        ..Default::default()
    });
    let ht_alert_btn = ht_manager.create(HitTestTreeData {
        left: 200.0,
        top: 64.0,
        width: 64.0,
        height: 24.0,
        cursor_shape: CursorShape::Pointer,
        ..Default::default()
    });
    composite_tree.add_child(main_window.composite_root(), ct_alert_btn);
    ht_manager.add_child(main_window.ht_root(), ht_alert_btn);

    struct AlertButtonActionHandler {
        ct: CompositeTreeRef,
    }
    impl HitTestTreeActionHandler for AlertButtonActionHandler {
        fn on_pointer_enter(
            &self,
            sender: HitTestTreeRef,
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
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
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
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

        fn on_click(
            &self,
            sender: HitTestTreeRef,
            context: &mut hittest::HitTestEventContext,
            args: &hittest::PointerActionArgs,
        ) -> input::EventContinueControl {
            tracing::debug!("todo: show alert popup");

            input::EventContinueControl::STOP_PROPAGATION
        }
    }
    let ht_alert_btn_action_handler =
        std::rc::Rc::new(AlertButtonActionHandler { ct: ct_alert_btn });
    ht_manager.set_action_handler(ht_alert_btn, &ht_alert_btn_action_handler);

    composite_tree.commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
    ht_manager.dump(main_window.ht_root());

    loop {
        match event_queue.next_event().await {
            Event::Quit => break,
            Event::WindowResize { window, size } => {
                pointer_input_manager.set_client_size(window, size);
            }
            Event::SubWindowClose { window } => {
                system_link.close_window(window);
            }
            Event::WindowRescaleUI { window, new_scale } => {
                if window == main_window {
                    composite_tree.get_mut(app_title).base_scale_factor = new_scale;
                    composite_tree.mark_dirty_all(app_title);
                    composite_tree.get_mut(tab_main).base_scale_factor = new_scale;
                    composite_tree.mark_dirty_all(tab_main);
                    composite_tree.get_mut(ct_alert_btn).base_scale_factor = new_scale;
                    composite_tree.mark_dirty_all(ct_alert_btn);

                    let mut renderer_sync = renderer_sync.lock().expect("poisoned");
                    composite_tree.commit(&mut renderer_sync.composite_buffer);
                }

                system_link.notify_ui_scale_changes_to_render(window, new_scale);
            }
            Event::PointerDown { window } => {
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

                let mut ht_create_only_access = ht_manager.derive_create_only_access();
                pointer_input_manager.handle_mouse_left_down(
                    &window,
                    &ht_manager,
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
                    },
                    window.ht_root(),
                    &mut keyboard_focus_manager,
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
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
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
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                        system_link: &system_link,
                        ht_create_only_access: &mut ht_create_only_access,
                    },
                    window.ht_root(),
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
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
struct SystemLink<'sys> {
    drag_preview_popover: DragPreviewPopoverHandle,
    vk_device: *const VulkanDevice<'sys>,
    rt_sender: std::sync::mpsc::Sender<RenderMessage>,
    event_dispatcher: *mut LogicFiberEventDispatcher,
    #[cfg(unix)]
    display_server: platform::unix::DisplayServerLink,
    #[cfg(target_os = "linux")]
    dbus: *const dbus::Connection,
}
#[cfg(not(windows))]
impl SystemLink<'_> {
    #[inline(always)]
    pub fn drag_preview_popover(&self) -> &DragPreviewPopoverHandle {
        &self.drag_preview_popover
    }

    #[cfg(target_os = "macos")]
    pub fn open_window<'h>(
        &self,
        composite_tree: &mut CompositeTree<Event>,
        hit_tree: &mut (impl HitTestTreeCreate<'h> + ?Sized),
    ) -> WindowHandle {
        let mut w = MacWindow::new(
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

        let vk_surface = VulkanSurface::new(unsafe { &*self.vk_device }, unsafe {
            br::MetalSurfaceCreateInfo::new(w.metal_layer())
                .execute((*self.vk_device).instance(), None)
                .expect("vk_surface.create")
        });
        self.rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                init_scale: SafeF32::new(
                    *w.dispatcher
                        .state
                        .active_buffer_scale
                        .lock()
                        .expect("poisoned"),
                )
                .expect("invalid scale"),
                composite_root: w.dispatcher.state.composite_root,
                latest_ui_scale_changes: UnboundedRef::new(
                    &w.dispatcher.state.latest_ui_scale_changes,
                ),
                key: handle,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");

        handle
    }

    #[cfg(windows)]
    pub fn open_window<'h>(
        &self,
        composite_tree: &mut CompositeTree<Event>,
        hit_tree: &mut (impl HitTestTreeCreate<'h> + ?Sized),
    ) -> WindowHandle {
        let w = platform::windows::NativeWindow::new(
            unsafe { &*self.window_class_set },
            WindowType::Sub,
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            hit_tree.create(HitTestTreeData {
                width_adjustment_factor: 1.0,
                height_adjustment_factor: 1.0,
                ..Default::default()
            }),
            unsafe { &*self.event_dispatcher }.clone(),
        );
        let h = w.make_handle();
        w.show(windows::Win32::UI::WindowsAndMessaging::SW_SHOW);

        let vk_surface = w.create_vk_surface(unsafe { &*self.vk_device });
        self.rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                key: h,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
                latest_ui_scale_changes: UnboundedRef::new(&w.state_ref().latest_ui_scale_changes),
                init_scale: SafeF32::new(w.dpi() as f32 / 96.0).expect("invalid scale"),
                composite_root: w.state_ref().composite_root,
            }))
            .expect("rt_sender.send");

        h
    }

    #[cfg(target_os = "macos")]
    pub fn close_window(&self, window_handle: WindowHandle) {
        unsafe {
            platform::mac::bridge::ni_release_window(window_handle.0);
        }
    }

    #[cfg(windows)]
    pub fn close_window(&self, mut window_handle: WindowHandle) {
        let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
        self.rt_sender
            .send(RenderMessage::DestroyWindow(
                window_handle,
                done_event_sender,
            ))
            .expect("rt_sender.send.destroy_window");
        done_event_receiver
            .recv()
            .expect("done_event_receiver.recv");

        window_handle.destroy();
    }

    #[cfg(windows)]
    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        unsafe {
            // TODO: 必要そうならキャッシュする
            windows::Win32::UI::WindowsAndMessaging::SetCursor(match cursor {
                CursorShape::Default => Some(
                    windows::Win32::UI::WindowsAndMessaging::LoadCursorW(
                        None,
                        windows::Win32::UI::WindowsAndMessaging::IDC_ARROW,
                    )
                    .expect("load_cursor.default"),
                ),
                CursorShape::Pointer => Some(
                    windows::Win32::UI::WindowsAndMessaging::LoadCursorW(
                        None,
                        windows::Win32::UI::WindowsAndMessaging::IDC_HAND,
                    )
                    .expect("load_cursor.default"),
                ),
                CursorShape::IBeam => Some(
                    windows::Win32::UI::WindowsAndMessaging::LoadCursorW(
                        None,
                        windows::Win32::UI::WindowsAndMessaging::IDC_IBEAM,
                    )
                    .expect("load_cursor.default"),
                ),
                CursorShape::ResizeHorizontal => Some(
                    windows::Win32::UI::WindowsAndMessaging::LoadCursorW(
                        None,
                        windows::Win32::UI::WindowsAndMessaging::IDC_SIZEWE,
                    )
                    .expect("load_cursor.default"),
                ),
            });
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
        termination_event: Arc<linux_eventfd::EventFD>,
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
    pub unsafe fn query_state(&self) -> &MacWindowState {
        unsafe {
            &(*crate::platform::mac::bridge::ni_get_window_callback_context(self.0)
                .cast::<MacWindowDispatcher>())
            .state
        }
    }

    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        let state = unsafe { self.query_state() };

        state
            .active_rt_size
            .lock()
            .expect("poisoned")
            .to_logical(*state.active_buffer_scale.lock().expect("poisoned"))
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        unsafe {
            *self
                .query_state()
                .active_buffer_scale
                .lock()
                .expect("poisoned")
        }
    }

    #[inline(always)]
    pub fn composite_root(&self) -> CompositeTreeRef {
        unsafe { self.query_state() }.composite_root
    }

    #[inline(always)]
    pub fn ht_root(&self) -> HitTestTreeRef {
        unsafe { self.query_state() }.ht_root
    }
}
#[cfg(target_os = "macos")]
impl ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {}

    #[inline(always)]
    fn release_pointer(&self) {}
}

// async logicむけにあとで作りなおすかも いったんsprite-atlas-visualizerから雑にコピー
pub struct AppEventBus {
    queue: std::sync::Mutex<VecDeque<Event>>,
    #[cfg(target_os = "linux")]
    efd: linux_eventfd::EventFD,
    #[cfg(windows)]
    event_notify: windows::Win32::Foundation::HANDLE,
}
#[cfg(windows)]
unsafe impl Sync for AppEventBus {}
#[cfg(windows)]
unsafe impl Send for AppEventBus {}
impl Drop for AppEventBus {
    fn drop(&mut self) {
        #[cfg(windows)]
        unsafe {
            if let Err(e) = windows::Win32::Foundation::CloseHandle(self.event_notify) {
                tracing::error!(reason = ?e, "event_notify.close");
            }
        }
    }
}
impl AppEventBus {
    pub fn push(&self, e: Event) {
        self.queue.lock().expect("poisoned").push_back(e);
        #[cfg(target_os = "linux")]
        self.efd.inc(1).unwrap();
        #[cfg(windows)]
        unsafe {
            windows::Win32::System::Threading::SetEvent(self.event_notify)
                .expect("event_notify.set");
        }
    }

    fn pop(&self) -> Option<Event> {
        self.queue.lock().expect("poisoned").pop_front()
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

#[cfg(target_os = "linux")]
struct DBusWatcher<'e> {
    epoll: &'e Epoll,
    last_poll_id: u64,
    fd_to_poll_id: HashMap<core::ffi::c_int, u64>,
    poll_id_to_watch_ref: &'e core::cell::UnsafeCell<HashMap<u64, *mut dbus::WatchRef>>,
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

#[cfg(target_os = "macos")]
#[derive(Clone, Copy)]
pub struct PointerID();

#[cfg(target_os = "macos")]
pub struct MacWindow {
    native_ptr: *mut platform::mac::bridge::WindowLink,
    dispatcher: Pin<Box<MacWindowDispatcher>>,
}
#[cfg(target_os = "macos")]
impl Drop for MacWindow {
    fn drop(&mut self) {
        unsafe {
            platform::mac::bridge::ni_unset_window_callbacks(self.native_ptr);
            platform::mac::bridge::ni_release_window(self.native_ptr);
        }
    }
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindow {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindow {}
#[cfg(target_os = "macos")]
impl MacWindow {
    pub fn new(
        flags: platform::mac::bridge::WindowCreationFlags,
        event_dispatcher: LogicFiberEventDispatcher,
        composite_root: CompositeTreeRef,
        ht_root: HitTestTreeRef,
    ) -> Self {
        let native_ptr = unsafe { platform::mac::bridge::ni_create_window(flags.bits()) };
        let init_scale = unsafe { platform::mac::bridge::ni_get_content_scale(native_ptr) };
        let mut dispatcher = Box::pin(MacWindowDispatcher {
            event_dispatcher,
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
                on_resize: MacWindowDispatcher::on_resize,
                on_pointer_down: MacWindowDispatcher::on_pointer_down,
                on_pointer_move: MacWindowDispatcher::on_pointer_move,
                on_pointer_up: MacWindowDispatcher::on_pointer_up,
            };
        unsafe {
            platform::mac::bridge::ni_set_window_callbacks(
                native_ptr,
                callbacks,
                dispatcher.as_mut().get_mut() as *mut _ as _,
            );
        }

        Self {
            native_ptr,
            dispatcher,
        }
    }

    #[inline(always)]
    pub const fn make_handle(&self) -> WindowHandle {
        WindowHandle(self.native_ptr)
    }

    #[inline(always)]
    pub fn rebind_event_dispatcher(&mut self, dispatcher: LogicFiberEventDispatcher) {
        self.dispatcher.event_dispatcher = dispatcher;
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
    state: MacWindowState,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindowDispatcher {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindowDispatcher {}
#[cfg(target_os = "macos")]
impl MacWindowDispatcher {
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
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    latest_ui_scale_changes: Mutex<Option<f32>>,
    active_size: std::sync::Mutex<Size<LogicalUnit>>,
    active_rt_size: std::sync::Mutex<Size<PixelsUnit>>,
    active_buffer_scale: std::sync::Mutex<f32>,
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindowState {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindowState {}
