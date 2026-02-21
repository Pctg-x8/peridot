use bedrock::{self as br, Device, InstanceChild, PhysicalDevice, QueueMut, SurfaceCreateInfo};
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
    sync::{Arc, Mutex},
};
#[cfg(windows)]
use windows::Win32::{
    System::WinRT::{
        CreateDispatcherQueueController, DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT,
        DispatcherQueueOptions,
    },
    UI::WindowsAndMessaging::{DispatchMessageW, GetMessageW, SW_SHOWNORMAL, TranslateMessage},
};

use crate::{
    composite::{
        AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
        CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
        CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef,
        CompositeTreeSyncBuffer,
    },
    graphics::VulkanDevice,
    hittest::{CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager},
    input::{KeyboardFocusManager, PointerInputManager, PointerInputUnit, ShellPointerActions},
    renderer::RenderThread,
    text::FontID,
    utils::{Color32, LogicalUnit, PixelsUnit, Point, SafeF32, Size},
};
#[cfg(feature = "wayland")]
use crate::{graphics::VulkanSurface, hittest::HitTestTreeRef};

mod atlas;
#[cfg(windows)]
mod bindgen;
mod composite;
mod graphics;
mod hittest;
mod input;
mod platform;
mod proto;
mod renderer;
mod text;
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

    let mut event_store = core::pin::pin!(None);
    let event_queue = EventQueue {
        event_store: event_store.as_mut().get_mut(),
    };
    let global_time_base = std::time::Instant::now();
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
    );
}

fn main_wrapper<'sys, AppFuture: core::future::Future<Output = ()> + 'sys>(
    run_app: impl FnOnce(
        &'sys std::time::Instant,
        &'sys Mutex<RendererSync>,
        CompositeTree<Event>,
        HitTestTreeManager<'sys>,
        WindowHandle,
        SystemLink,
    ) -> AppFuture,
    mut event_store: Pin<&mut Option<Event>>,
    global_time_base: &'sys std::time::Instant,
    renderer_sync: &'sys Mutex<RendererSync>,
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

    #[cfg(windows)]
    let hinstance = utils::platform::windows::current_instance_handle();
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

    #[cfg(windows)]
    let drag_preview_popover = DragPreviewPopoverHandle::new(hinstance, &native_compositor);

    #[cfg(feature = "wayland")]
    let terminate_event = std::sync::Arc::new(
        EventFD::new(0, EventFDFlags::empty()).expect("terminate_event.create"),
    );

    #[cfg(feature = "wayland")]
    let mut wl_display = wl::Display::connect().expect("wl_display connect");
    #[cfg(feature = "wayland")]
    let mut wl_interfaces =
        WaylandGlobalInterfaces::collect_sync(&wl_display).expect("wl_interfaces.collect_sync");

    #[cfg(feature = "wayland")]
    let popover_buf = if let Some(ref spb) = wl_interfaces.single_pixel_buffer_manager {
        let c = DragPreviewPopoverHandle::BG_COLOR.premultiplied();
        let b = spb
            .create_u32_rgba_buffer(c.r_u32(), c.g_u32(), c.b_u32(), c.a_u32())
            .expect("popup_buf.create.single_pixel_buffer");

        DragPreviewPopoverBuffer::SinglePixel(b)
    } else {
        // traditional shm-based single pixel buffer
        let shm_region = utils::platform::linux::TemporalSharedMemory::new_unique(
            c"/pme_shm",
            libc::O_RDWR,
            0o0600,
        )
        .expect("buf.shm.create")
        .expect("buf.shm.create.non_unique");
        unsafe {
            utils::platform::linux::ftruncate(&shm_region, 4).expect("buf.shm.resize");
        }

        let mapped = utils::platform::linux::MappedMemory::new(
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

        DragPreviewPopoverBuffer::Shm {
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
        bound_window_link: core::ptr::null_mut(),
    };

    let mut composite_tree = CompositeTree::new();
    let main_window_composite_root = composite_tree.create(CompositeRect {
        relative_size_adjustment: [1.0, 1.0],
        ..Default::default()
    });
    let mut ht_manager = HitTestTreeManager::new();
    let main_window_ht_root = ht_manager.create(HitTestTreeData {
        width_adjustment_factor: 1.0,
        height_adjustment_factor: 1.0,
        ..Default::default()
    });

    #[cfg(windows)]
    let wc_set = platform::windows::WindowClassSet::register::<AppFuture>(hinstance);
    #[cfg(windows)]
    let mut w = platform::windows::NativeWindow::new(
        &wc_set,
        main_window_composite_root,
        main_window_ht_root,
        LogicFiberEventDispatcher {
            event_store: event_store.as_mut().get_mut() as *mut _ as _,
            future: core::ptr::null_mut(),
        },
    );

    #[cfg(feature = "wayland")]
    let mut w = WaylandWindow::new(
        &wl_interfaces,
        &dbus,
        terminate_event.clone(),
        LogicFiberEventDispatcher {
            event_store: event_store.as_mut().get_mut() as *mut _ as _,
            future: core::ptr::null_mut(),
        },
        main_window_composite_root,
        main_window_ht_root,
    );

    #[cfg(target_os = "macos")]
    let mut w = MacWindow::new(LogicFiberEventDispatcher::<AppFuture> {
        event_store: event_store.as_mut().get_mut() as *mut _,
        future: core::ptr::null_mut(),
    });
    #[cfg(target_os = "macos")]
    w.make_primary_window();

    let vk_device = VulkanDevice::new();

    #[cfg(windows)]
    if !vk_device
        .primary_adapter_ref()
        .win32_presentation_support(vk_device.present_queue_family_index())
    {
        panic!("win32 presentation not supported on graphics queue");
    }
    #[cfg(windows)]
    let vk_surface = w.create_vk_surface(&vk_device);

    #[cfg(feature = "wayland")]
    if !unsafe {
        vk_device
            .primary_adapter_ref()
            .wayland_presentation_support(
                vk_device.present_queue_family_index(),
                wl_display.as_raw().cast(),
            )
    } {
        panic!("wayland presentation not supported on graphics queue");
    }
    #[cfg(feature = "wayland")]
    let vk_surface = VulkanSurface::new(&vk_device, unsafe {
        br::WaylandSurfaceCreateInfo::new(wl_display.as_raw().cast(), w.surface.as_raw().cast())
            .execute(vk_device.instance(), None)
            .expect("vk_surface.create")
    });

    #[cfg(target_os = "macos")]
    let vk_surface = VulkanSurface {
        handle: unsafe {
            br::MetalSurfaceCreateInfo::new(w.metal_layer())
                .execute(vk_device.instance(), None)
                .expect("vk_surface.create")
        },
        device: &vk_device,
    };

    #[cfg(feature = "wayland")]
    let mut wl_global_msg = core::pin::pin!(WaylandGlobalMessaging {
        text_input_manager: wl_interfaces.text_input_manager.as_ptr(),
        xkb_context: xkbcommon::Context::new(xkbcommon::ContextFlags::NO_FLAGS)
            .expect("xkb_context.create"),
        keyboard: None,
        pointer: None,
        cursor_shape_manager: wl_interfaces
            .cursor_shape_manager
            .as_ref()
            .map(|x| x.as_ptr()),
        event_dispatcher: LogicFiberEventDispatcher {
            event_store: event_store.as_mut().get_mut() as *mut _ as _,
            future: core::ptr::null_mut()
        },
        _pinned: core::marker::PhantomPinned,
    });

    let mut app = core::pin::pin!(run_app(
        global_time_base,
        renderer_sync,
        composite_tree,
        ht_manager,
        #[cfg(feature = "wayland")]
        WindowHandle(w.surface.as_ptr()),
        #[cfg(windows)]
        w.make_handle(),
        SystemLink {
            drag_preview_popover,
            #[cfg(feature = "wayland")]
            pointer_state_ref: unsafe {
                &mut wl_global_msg.as_mut().get_unchecked_mut().pointer as *mut _
            },
        } /*
          #[cfg(target_os = "macos")]
          WindowHandle {
              state_ref: &mut w.dispatcher.state as *mut _
          },
          drag_preview_popover*/
    ));

    #[cfg(feature = "wayland")]
    unsafe {
        wl_global_msg
            .as_mut()
            .get_unchecked_mut()
            .event_dispatcher
            .future = app.as_mut().get_unchecked_mut() as *mut _;
    }
    #[cfg(feature = "wayland")]
    w.rebind_event_dispatcher(LogicFiberEventDispatcher {
        event_store: event_store.as_mut().get_mut() as *mut _ as _,
        future: unsafe { app.as_mut().get_unchecked_mut() as *mut _ },
    });
    #[cfg(target_os = "macos")]
    unsafe {
        w.dispatcher.event_dispatcher.future = app.as_mut().get_unchecked_mut() as *mut _;
    }
    #[cfg(windows)]
    w.rebind_event_dispatcher(LogicFiberEventDispatcher {
        event_store: event_store.as_mut().get_mut() as *mut _ as _,
        future: unsafe { app.as_mut().get_unchecked_mut() as *mut _ },
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
    w.commit();
    #[cfg(feature = "wayland")]
    wl_display.roundtrip().expect("roundtrip");

    #[cfg(feature = "wayland")]
    let main_window_init_scale = SafeF32::new(
        w.event_listener
            .state
            .committed_state
            .get_mut()
            .expect("poisoned")
            .active_buffer_scale,
    )
    .expect("invalid scale");
    #[cfg(feature = "wayland")]
    let main_window_state = &w.event_listener.state;

    let shutdown = std::sync::atomic::AtomicBool::new(false);
    std::thread::scope(|thread_scope| {
        let render_thread = RenderThread {
            vk_device: &vk_device,
            shutdown_signal: &shutdown,
            renderer_sync: &renderer_sync,
            global_time_base: &global_time_base,
            event_bus: &events,
            #[cfg(windows)]
            main_window_handle: w.make_sendable(),
            #[cfg(windows)]
            main_window_state: w.state_ref(),
            #[cfg(windows)]
            main_window_init_scale: SafeF32::new(w.dpi() as f32 / 96.0).expect("invalid scale"),
            main_window_vk_surface: vk_surface,
        };
        let render_thread = std::thread::Builder::new()
            .name("Render".into())
            .spawn_scoped(thread_scope, || {
                render_thread.run();
            })
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
            'prepare_loop: loop {
                match wl_display.prepare_read() {
                    Ok(_) => break 'prepare_loop,
                    Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                        wl_display
                            .dispatch_pending()
                            .expect("wl_display.dispatch_pending");
                    }
                    Err(e) => {
                        tracing::error!(reason = ?e, "wl_display.prepare_read");
                        break 'app;
                    }
                }
            }
            #[cfg(feature = "wayland")]
            wl_display.flush().expect("wl_display.flush");
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
                            if m.path().is_some_and(|x| x == WL_APPMENU_OBJECT_PATH)
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
                            if m.path().is_some_and(|x| x == WL_APPMENU_OBJECT_PATH)
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
                            if m.path().is_some_and(|x| x == WL_APPMENU_OBJECT_PATH)
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
        w.show(SW_SHOWNORMAL);

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

        *event_store = Some(Event::Quit);
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
        #[cfg(target_os = "macos")]
        active_window: *mut platform::mac::bridge::WindowLink,
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
}
#[cfg(any(feature = "wayland", windows, target_os = "macos"))]
unsafe impl Sync for Event {}
#[cfg(any(feature = "wayland", windows, target_os = "macos"))]
unsafe impl Send for Event {}

struct EventQueue {
    event_store: *mut Option<Event>,
}
impl EventQueue {
    pub async fn next_event(&self) -> Event {
        EventQueueNextEventAwaiter { q: self }.await
    }
}

pub struct LogicFiberEventDispatcher<AppFuture> {
    event_store: *mut Option<Event>,
    future: *mut AppFuture,
}
impl<AppFuture: core::future::Future> LogicFiberEventDispatcher<AppFuture> {
    pub fn dispatch(&self, e: Event) {
        unsafe {
            (*self.event_store) = Some(e);
            let _ = core::pin::Pin::new_unchecked(&mut *self.future).poll(
                &mut core::task::Context::from_waker(&core::task::Waker::new(
                    &(),
                    &APP_WAKER_VTABLE,
                )),
            );
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
        match unsafe { (&mut *self.get_mut().q.event_store).take() } {
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
    system_link: SystemLink,
) {
    tracing::info!("app start");

    let mut keyboard_focus_manager = KeyboardFocusManager::new();
    let mut pointer_input_manager = PointerInputManager::new();
    pointer_input_manager.set_client_size(main_window, main_window.client_size());

    // WindowsではWM_NCHITTESTの返り値の計算に必要なので一旦生ポインタで参照もたせる（実際どうするかはあとで考える）
    #[cfg(windows)]
    main_window.bind_hittest_managers(&pointer_input_manager, &ht_manager);

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
    }
    let ht_action_handler = std::rc::Rc::new(TabHitAction { ct: tab_main });
    ht_manager.set_action_handler(ht_tab_main, &ht_action_handler);

    composite_tree.commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
    ht_manager.dump(main_window.ht_root());

    loop {
        match event_queue.next_event().await {
            Event::Quit => break,
            Event::WindowResize { window, size } => {
                pointer_input_manager.set_client_size(window, size);
            }
            Event::WindowRescaleUI { window, new_scale } => {
                composite_tree.get_mut(app_title).base_scale_factor = new_scale;
                composite_tree.mark_dirty_all(app_title);
                composite_tree.get_mut(tab_main).base_scale_factor = new_scale;
                composite_tree.mark_dirty_all(tab_main);

                let mut renderer_sync = renderer_sync.lock().expect("poisoned");
                composite_tree.commit(&mut renderer_sync.composite_buffer);
                system_link.notify_ui_scale_changes_to_render(window, new_scale);
            }
            Event::PointerDown {
                window,
                #[cfg(target_os = "macos")]
                active_window,
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
                {
                    drag_preview_popover.bound_window_link = active_window;
                }

                pointer_input_manager.handle_mouse_left_down(
                    &window,
                    &ht_manager,
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
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
                pointer_input_manager.handle_mouse_move(
                    window,
                    client_pos,
                    &window,
                    &ht_manager,
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
                    },
                    window.ht_root(),
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);

                let cursor_shape = pointer_input_manager.cursor_shape(&ht_manager);
                system_link.set_cursor(&pointer_id, cursor_shape);

                /*
                #[cfg(target_os = "macos")]
                unsafe {
                    platform::mac::bridge::ni_set_cursor_shape(match cursor_shape {
                        hittest::CursorShape::Default => {
                            platform::mac::bridge::CursorShape::Arrow as _
                        }
                        hittest::CursorShape::Pointer => {
                            platform::mac::bridge::CursorShape::Pointer as _
                        }
                        hittest::CursorShape::IBeam => {
                            platform::mac::bridge::CursorShape::IBeam as _
                        }
                        hittest::CursorShape::ResizeHorizontal => {
                            platform::mac::bridge::CursorShape::ResizeHorizontal as _
                        }
                    })
                }*/
            }
            Event::PointerUp { window } => {
                pointer_input_manager.handle_mouse_left_up(
                    &window,
                    &ht_manager,
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: system_link.drag_preview_popover(),
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
    main_window.unbind_hittest_managers();
}

struct SystemLink {
    drag_preview_popover: DragPreviewPopoverHandle,
    #[cfg(feature = "wayland")]
    pointer_state_ref: *const Option<WaylandPointerState>,
}
impl SystemLink {
    #[inline(always)]
    pub fn drag_preview_popover(&self) -> &DragPreviewPopoverHandle {
        &self.drag_preview_popover
    }

    #[cfg(feature = "wayland")]
    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        if let Some(&WaylandPointerState {
            enter_state: Some(WaylandPointerEnterState { serial, .. }),
            cursor: Some(ref shape_device),
            ..
        }) = unsafe { (*self.pointer_state_ref).as_ref() }
        {
            shape_device
                .set_shape(serial, cursor.as_wayland())
                .expect("cursor_shape_device.set_cursor");
        }
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

    #[cfg(feature = "wayland")]
    pub fn notify_ui_scale_changes_to_render(&self, window: WindowHandle, new_scale: f32) {
        *window
            .state()
            .latest_ui_scale_changes
            .lock()
            .expect("poisoned") = Some(new_scale);
    }
}

#[cfg(windows)]
pub type PointerID = platform::windows::PointerID;
#[cfg(windows)]
pub type DragPreviewPopoverHandle = platform::windows::DragPreviewPopoverHandle;
#[cfg(windows)]
pub type WindowHandle = platform::windows::WindowHandle;

#[cfg(feature = "wayland")]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct WindowHandle(*mut wl::Surface);
#[cfg(feature = "wayland")]
impl WindowHandle {
    #[inline(always)]
    fn state(&self) -> &WaylandWindowState {
        unsafe { &*(*self.0).user_data().cast() }
    }

    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        self.state()
            .committed_state
            .lock()
            .expect("poisoned")
            .active_size_logical
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        self.state()
            .committed_state
            .lock()
            .expect("poisoned")
            .active_buffer_scale
    }

    #[inline(always)]
    pub fn composite_root(&self) -> CompositeTreeRef {
        self.state().composite_root
    }

    #[inline(always)]
    pub fn ht_root(&self) -> HitTestTreeRef {
        self.state().ht_root
    }
}
#[cfg(feature = "wayland")]
impl ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {
        // Waylandはなし(勝手にキャプチャ状態になってるらしい)
    }

    #[inline(always)]
    fn release_pointer(&self) {
        // Waylandはなし(勝手にキャプチャ状態になってるらしい)
    }
}

#[cfg(target_os = "macos")]
pub struct WindowHandle {
    state_ref: *mut MacWindowState,
}
#[cfg(target_os = "macos")]
impl WindowHandle {
    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        let state = unsafe { &*self.state_ref };

        state
            .active_rt_size
            .lock()
            .expect("poisoned")
            .to_logical(*state.active_buffer_scale.lock().expect("poisoned"))
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        unsafe {
            *(*self.state_ref)
                .active_buffer_scale
                .lock()
                .expect("poisoned")
        }
    }
}
#[cfg(target_os = "macos")]
impl ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {}

    #[inline(always)]
    fn release_pointer(&self) {}
}

#[cfg(feature = "wayland")]
#[derive(Clone, Copy)]
pub struct PointerID();
#[cfg(feature = "wayland")]
pub struct CursorShaping {
    pointer_state_ref: *mut Option<WaylandPointerState>,
}
#[cfg(feature = "wayland")]
impl CursorShaping {
    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        if let Some(&WaylandPointerState {
            enter_state: Some(WaylandPointerEnterState { serial, .. }),
            cursor: Some(ref shape_device),
            ..
        }) = unsafe { (*self.pointer_state_ref).as_ref() }
        {
            shape_device
                .set_shape(serial, cursor.as_wayland())
                .expect("cursor_shape_device.set_cursor");
        }
    }
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

#[cfg(feature = "wayland")]
struct WaylandGlobalInterfaces {
    outputs: Vec<wl::Owned<wl::Output>>,
    compositor: wl::Owned<wl::Compositor>,
    xdg_wm_base: wl::Owned<wl::XdgWmBase>,
    seat: wl::Owned<wl::Seat>,
    shm: wl::Owned<wl::Shm>,
    viewporter: wl::Owned<wl::WpViewporter>,
    text_input_manager: wl::Owned<wl::ZwpTextInputManagerV3>,
    // optional requirements
    single_pixel_buffer_manager: Option<wl::Owned<wl::WpSinglePixelBufferManagerV1>>,
    kde_blur_manager: Option<wl::Owned<wl::OrgKdeKwinBlurManager>>,
    kde_appmenu_manager: Option<wl::Owned<wl::OrgKdeKwinAppmenuManager>>,
    zxdg_decoration_manager: Option<wl::Owned<wl::ZxdgDecorationManagerV1>>,
    cursor_shape_manager: Option<wl::Owned<wl::WpCursorShapeManagerV1>>,
    fractional_scale_manager: Option<wl::Owned<wl::WpFractionalScaleManagerV1>>,
}
#[cfg(feature = "wayland")]
impl WaylandGlobalInterfaces {
    pub fn collect_sync(display: &wl::Display) -> std::io::Result<Self> {
        let mut wl_registry = display.get_registry()?;
        let mut rl = RegistryListener::default();
        wl_registry
            .set_listener(&mut rl)
            .into_result()
            .expect("wl_registry.set_listener");
        display.roundtrip()?;

        Ok(Self {
            outputs: rl.outputs,
            compositor: rl.compositor.expect("no compositor"),
            xdg_wm_base: rl.xdg_wm_base.expect("no xdg-shell"),
            seat: rl.seat.expect("no seat"),
            shm: rl.shm.expect("no shm"),
            viewporter: rl.viewporter.expect("no viewporter"),
            text_input_manager: rl.text_input_manager.expect("no text-input"),
            single_pixel_buffer_manager: rl.single_pixel_buffer_manager,
            kde_blur_manager: rl.kde_blur_manager,
            kde_appmenu_manager: rl.kde_appmenu_manager,
            zxdg_decoration_manager: rl.zxdg_decoration_manager,
            cursor_shape_manager: rl.cursor_shape_manager,
            fractional_scale_manager: rl.fractional_scale_manager,
        })
    }
}
#[cfg(feature = "wayland")]
#[derive(Default)]
struct RegistryListener {
    compositor: Option<wl::Owned<wl::Compositor>>,
    outputs: Vec<wl::Owned<wl::Output>>,
    xdg_wm_base: Option<wl::Owned<wl::XdgWmBase>>,
    seat: Option<wl::Owned<wl::Seat>>,
    shm: Option<wl::Owned<wl::Shm>>,
    viewporter: Option<wl::Owned<wl::WpViewporter>>,
    text_input_manager: Option<wl::Owned<wl::ZwpTextInputManagerV3>>,
    single_pixel_buffer_manager: Option<wl::Owned<wl::WpSinglePixelBufferManagerV1>>,
    kde_blur_manager: Option<wl::Owned<wl::OrgKdeKwinBlurManager>>,
    kde_appmenu_manager: Option<wl::Owned<wl::OrgKdeKwinAppmenuManager>>,
    zxdg_decoration_manager: Option<wl::Owned<wl::ZxdgDecorationManagerV1>>,
    cursor_shape_manager: Option<wl::Owned<wl::WpCursorShapeManagerV1>>,
    fractional_scale_manager: Option<wl::Owned<wl::WpFractionalScaleManagerV1>>,
}
#[cfg(feature = "wayland")]
impl wl::RegistryListener for RegistryListener {
    fn global(
        &mut self,
        registry: &mut peridot_tp_wayland::Registry,
        name: u32,
        interface: &core::ffi::CStr,
        version: u32,
    ) {
        tracing::info!(target: "wl::diag::global_interface", name, ?interface, version);

        if interface == c"wl_compositor" {
            self.compositor = Some(registry.bind(name, version).expect("bind compositor"));
        } else if interface == c"wl_output" {
            self.outputs
                .push(registry.bind(name, version).expect("bind output"));
        } else if interface == c"xdg_wm_base" {
            self.xdg_wm_base = Some(registry.bind(name, version).expect("bind xdg_wm_base"));
        } else if interface == c"wl_seat" {
            assert!(self.seat.is_none(), "multiple seat?");
            self.seat = Some(registry.bind(name, version).expect("bind seat"));
        } else if interface == c"wl_shm" {
            self.shm = Some(registry.bind(name, version).expect("bind shm"));
        } else if interface == c"wp_viewporter" {
            self.viewporter = Some(registry.bind(name, version).expect("bind viewporter"));
        } else if interface == c"wp_single_pixel_buffer_manager_v1" {
            self.single_pixel_buffer_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind single_pixel_buffer_manager"),
            );
        } else if interface == c"org_kde_kwin_blur_manager" {
            self.kde_blur_manager =
                Some(registry.bind(name, version).expect("bind kde_blur_manager"));
        } else if interface == c"org_kde_kwin_appmenu_manager" {
            self.kde_appmenu_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind kde_appmenu_manager"),
            );
        } else if interface == c"zxdg_decoration_manager_v1" {
            self.zxdg_decoration_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind zxdg_decoration_manager"),
            );
        } else if interface == c"zwp_text_input_manager_v3" {
            self.text_input_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind text_input_manager"),
            );
        } else if interface == c"wp_cursor_shape_manager_v1" {
            self.cursor_shape_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind cursor_shape_manager"),
            );
        } else if interface == c"wp_fractional_scale_manager_v1" {
            self.fractional_scale_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind fractional_scale_manager"),
            );
        }
    }

    fn global_remove(&mut self, _registry: &mut peridot_tp_wayland::Registry, name: u32) {
        tracing::info!(target: "wl::diag", name, "wl interface remove");
    }
}

#[cfg(feature = "wayland")]
#[allow(dead_code)]
enum DragPreviewPopoverBuffer {
    SinglePixel(wl::Owned<wl::Buffer>),
    Shm {
        shm_region: utils::platform::linux::TemporalSharedMemory,
        mapped: utils::platform::linux::MappedMemory,
        shm_pool: wl::Owned<wl::ShmPool>,
        buf: wl::Owned<wl::Buffer>,
    },
}
#[cfg(feature = "wayland")]
impl DragPreviewPopoverBuffer {
    #[inline(always)]
    pub fn buffer(&self) -> &wl::Buffer {
        match self {
            Self::SinglePixel(x) => x,
            Self::Shm { buf, .. } => buf,
        }
    }
}

#[cfg(feature = "wayland")]
struct DragPreviewPopoverHandle {
    pub display: *mut wl::Display,
    pub wl_interfaces: *const WaylandGlobalInterfaces,
    pub root_window: core::cell::Cell<*mut wl::XdgSurface>,
    pub buf: DragPreviewPopoverBuffer,
    pub popup: core::cell::UnsafeCell<
        Option<(
            Option<wl::Owned<wl::OrgKdeKwinBlur>>,
            wl::Owned<wl::XdgPopup>,
            wl::Owned<wl::XdgSurface>,
            wl::Owned<wl::WpViewport>,
            wl::Owned<wl::Surface>,
            Box<WaylandPopupState>,
        )>,
    >,
}
#[cfg(feature = "wayland")]
impl DragPreviewPopoverHandle {
    pub fn bind_parent_window(&self, window: WindowHandle) {
        self.root_window.set(window.state().xdg_surface_ptr);
    }

    pub fn show(&self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
        let wl_popup_surface = unsafe {
            (*self.wl_interfaces)
                .compositor
                .create_surface()
                .expect("wl_popup_surface.create")
        };
        let mut xdg_popup_surface = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .get_xdg_surface(&wl_popup_surface)
                .expect("xdg_popup_surface.create")
        };

        let positioner = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .create_positioner()
                .expect("pos.create")
        };
        positioner
            .set_size(size.width as _, size.height as _)
            .expect("pos.set_size");
        positioner
            .set_offset(pos.x as _, pos.y as _)
            .expect("pos.set_offset");
        positioner
            .set_anchor(wl::XdgPositionerAnchor::TopLeft)
            .expect("pos.set_anchor");
        positioner
            .set_anchor_rect(0, 0, 1, 1)
            .expect("pos.set_anchor_rect");
        positioner
            .set_gravity(wl::XdgPositionerGravity::BottomRight)
            .expect("pos.set_gravity");
        positioner
            .set_constraint_adjustment(wl::XdgPositionerConstraintAdjustment::None)
            .expect("pos.set_constraint_adjustment");
        let mut pp = unsafe {
            xdg_popup_surface
                .get_popup(Some(&*self.root_window.get()), &positioner)
                .expect("pop.create")
        };
        let mut popup_state = Box::new(WaylandPopupState {
            surface_ptr: wl_popup_surface.as_ptr(),
        });
        xdg_popup_surface
            .set_listener(&mut *popup_state)
            .into_result()
            .expect("xdg_popup_surface.set_listener");
        pp.set_listener(&mut *popup_state)
            .into_result()
            .expect("pop.set_listener");
        wl_popup_surface.commit().expect("wl_popup_surface.commit");
        unsafe {
            // process configure event...(Kwinとかはconfigureくるまえにattachするとエラーが出ておちる)
            (*self.display).roundtrip().expect("roundtrip");
        }

        wl_popup_surface
            .attach(Some(self.buf.buffer()), 0, 0)
            .expect("wl_popup_surface.attach");
        wl_popup_surface
            .damage(0, 0, -1, -1)
            .expect("wl_popup_surface.damage");
        let viewport = unsafe {
            (*self.wl_interfaces)
                .viewporter
                .get_viewport(&wl_popup_surface)
                .expect("popup_viewport.create")
        };
        viewport
            .set_source(
                wl::Fixed::from_f32_lossy(0.0),
                wl::Fixed::from_f32_lossy(0.0),
                wl::Fixed::from_f32_lossy(1.0),
                wl::Fixed::from_f32_lossy(1.0),
            )
            .expect("viewport.set_source");
        viewport
            .set_destination(size.width as _, size.height as _)
            .expect("viewport.set_destination");

        let blur = if let Some(bm) = unsafe { (*self.wl_interfaces).kde_blur_manager.as_ref() } {
            let blur = bm.create(&wl_popup_surface).expect("blur.create");
            blur.commit().expect("blur.commit");

            Some(blur)
        } else {
            None
        };

        wl_popup_surface.commit().expect("wl_popup_surface.commit");

        unsafe {
            (*self.popup.get()) = Some((
                blur,
                pp,
                xdg_popup_surface,
                viewport,
                wl_popup_surface,
                popup_state,
            ));
        }
    }

    pub fn r#move(&self, p: &Point<PointerInputUnit>) {
        let Some((_, pp, _, _, _, _)) = (unsafe { &*self.popup.get() }) else {
            return;
        };

        let pos = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .create_positioner()
                .expect("pos.create")
        };
        pos.set_offset(p.x as _, p.y as _).expect("pos.set_offset");
        pp.reposition(&pos, 0).expect("pp.reposition");
    }

    pub fn hide(&self) {
        unsafe {
            (*self.popup.get()) = None;
        }
    }
}

#[cfg(feature = "wayland")]
struct WaylandPopupState {
    surface_ptr: *mut wl::Surface,
}
#[cfg(feature = "wayland")]
impl wl::XdgSurfaceEventListener for WaylandPopupState {
    #[tracing::instrument(skip(self, sender))]
    fn configure(&mut self, sender: &mut peridot_tp_wayland::XdgSurface, serial: u32) {
        tracing::trace!("popup.surface.configure");
        sender.ack_configure(serial).expect("popup.ack_configure");

        unsafe {
            (*self.surface_ptr).commit().expect("popup.surface.commit");
        }
    }
}
#[cfg(feature = "wayland")]
impl wl::XdgPopupEventListener for WaylandPopupState {
    #[tracing::instrument(skip(self, sender))]
    fn configure(
        &mut self,
        sender: &mut peridot_tp_wayland::XdgPopup,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) {
        tracing::trace!("popup.configure");
    }

    #[tracing::instrument(skip(self, sender))]
    fn popup_done(&mut self, sender: &mut peridot_tp_wayland::XdgPopup) {
        tracing::trace!("popup.popup_done");
    }

    #[tracing::instrument(skip(self, sender))]
    fn repositioned(&mut self, sender: &mut peridot_tp_wayland::XdgPopup, token: u32) {
        tracing::trace!("popup.repositioned");
    }
}

#[cfg(target_os = "macos")]
pub struct DragPreviewPopoverHandle {
    bound_window_link: *mut platform::mac::bridge::WindowLink,
}
#[cfg(target_os = "macos")]
impl DragPreviewPopoverHandle {
    pub fn show(&mut self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
        unsafe {
            // macの場合はスクリーン座標が必要
            let mut x = pos.x as f64;
            let mut y = pos.y as f64;
            platform::mac::bridge::ni_convert_point_to_screen(
                self.bound_window_link,
                &mut x,
                &mut y,
            );

            platform::mac::bridge::ni_show_drag_preview(x, y, size.width as _, size.height as _);
        }
    }

    pub fn r#move(&mut self, pos: &Point<PointerInputUnit>) {
        unsafe {
            // macの場合はスクリーン座標が必要
            let mut x = pos.x as f64;
            let mut y = pos.y as f64;
            platform::mac::bridge::ni_convert_point_to_screen(
                self.bound_window_link,
                &mut x,
                &mut y,
            );

            platform::mac::bridge::ni_move_drag_preview(x, y);
        }
    }

    pub fn hide(&mut self) {
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

#[cfg(feature = "wayland")]
const WL_APPMENU_OBJECT_PATH: &core::ffi::CStr = c"/AppMenu";

#[cfg(feature = "wayland")]
struct WaylandPointerEnterState {
    pub surface: *mut wl::Surface,
    pub serial: u32,
}

#[cfg(feature = "wayland")]
struct WaylandPointerState {
    _wl_object: wl::Owned<wl::Pointer>,
    cursor: Option<wl::Owned<wl::WpCursorShapeDeviceV1>>,
    pos: Point<LogicalUnit>,
    enter_state: Option<WaylandPointerEnterState>,
}

#[cfg(feature = "wayland")]
struct WaylandKeyboardState {
    _wl_object: wl::Owned<wl::Keyboard>,
    xkb_keymap: Option<xkbcommon::Keymap>,
    xkb_state: Option<xkbcommon::State>,
    _text_input: Option<wl::Owned<wl::ZwpTextInputV3>>,
}

#[cfg(feature = "wayland")]
struct WaylandGlobalMessaging<AppFuture: core::future::Future<Output = ()>> {
    pub text_input_manager: *mut wl::ZwpTextInputManagerV3,
    pub xkb_context: xkbcommon::Context,
    pub keyboard: Option<WaylandKeyboardState>,
    pub pointer: Option<WaylandPointerState>,
    pub cursor_shape_manager: Option<*mut wl::WpCursorShapeManagerV1>,
    pub event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
    _pinned: core::marker::PhantomPinned,
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::XdgWmBaseEventListener
    for WaylandGlobalMessaging<AppFuture>
{
    #[inline(always)]
    fn ping(&mut self, sender: &mut peridot_tp_wayland::XdgWmBase, serial: u32) {
        sender.pong(serial).expect("xdg_wm_base pong");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::SeatEventListener
    for WaylandGlobalMessaging<AppFuture>
{
    fn capabilities(
        &mut self,
        seat: &mut peridot_tp_wayland::Seat,
        capabilities: wl::SeatCapability,
    ) {
        tracing::trace!(?capabilities, "seat::capabilities");

        if capabilities.contains(wl::SeatCapability::POINTER) {
            // pointer
            let mut p = seat.get_pointer().expect("seat.get_pointer");
            p.set_listener(self)
                .into_result()
                .expect("pointer.set_listener");
            let c = if let Some(mgr) = self.cursor_shape_manager {
                Some(unsafe {
                    (*mgr)
                        .get_pointer(&p)
                        .expect("cursor_shape_manager.get_pointer")
                })
            } else {
                None
            };

            self.pointer = Some(WaylandPointerState {
                _wl_object: p,
                cursor: c,
                pos: Point::new_logical(0.0, 0.0),
                enter_state: None,
            });
        } else {
            // remove pointer
            self.pointer = None;
        }

        if capabilities.contains(wl::SeatCapability::KEYBOARD) {
            let mut k = seat.get_keyboard().expect("seat.get_keyboard");
            k.set_listener(self)
                .into_result()
                .expect("keyboard.set_listener");
            let mut ti = unsafe {
                (*self.text_input_manager)
                    .get_text_input(seat)
                    .expect("text_input_manager.get_text_input")
            };
            ti.set_listener(self)
                .into_result()
                .expect("text_input.set_listener");

            self.keyboard = Some(WaylandKeyboardState {
                _wl_object: k,
                xkb_keymap: None,
                xkb_state: None,
                _text_input: Some(ti),
            });
        } else {
            // remove keyboard
            self.keyboard = None;
        }
    }

    fn name(&mut self, _seat: &mut peridot_tp_wayland::Seat, name: &core::ffi::CStr) {
        tracing::trace!(?name, "seat::name");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::PointerEventListener
    for WaylandGlobalMessaging<AppFuture>
{
    #[tracing::instrument(skip(self, _pointer, surface), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn enter(
        &mut self,
        _pointer: &mut wl::Pointer,
        serial: u32,
        surface: &mut wl::Surface,
        surface_x: wl::Fixed,
        surface_y: wl::Fixed,
    ) {
        let state = self.pointer.as_mut().expect("no pointer state initialized");

        state.enter_state = Some(WaylandPointerEnterState {
            surface: surface as *mut _,
            serial,
        });
        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());

        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(surface as *mut _),
            client_pos: state.pos,
        });
    }

    #[tracing::instrument(skip(self, _pointer, _surface))]
    fn leave(&mut self, _pointer: &mut wl::Pointer, serial: u32, _surface: &mut wl::Surface) {
        let state = self.pointer.as_mut().expect("no pointer state initialized");

        state.enter_state = None;
    }

    #[tracing::instrument(skip(self, _pointer), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn motion(
        &mut self,
        _pointer: &mut wl::Pointer,
        time: u32,
        surface_x: wl::Fixed,
        surface_y: wl::Fixed,
    ) {
        let state = self.pointer.as_mut().expect("no pointer state initialized");
        let Some(ref enter_state) = state.enter_state else {
            return;
        };

        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());
        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(enter_state.surface),
            client_pos: state.pos,
        });
    }

    #[tracing::instrument(skip(self, _pointer), fields(state = state as u32))]
    fn button(
        &mut self,
        _pointer: &mut wl::Pointer,
        serial: u32,
        time: u32,
        button: u32,
        state: wl::PointerButtonState,
    ) {
        let pointer_state = self.pointer.as_ref().expect("no pointer state initialized");
        let Some(ref enter_state) = pointer_state.enter_state else {
            return;
        };

        if state == wl::PointerButtonState::Pressed {
            self.event_dispatcher.dispatch(Event::PointerDown {
                window: WindowHandle(enter_state.surface),
            });
        } else if state == wl::PointerButtonState::Released {
            self.event_dispatcher.dispatch(Event::PointerUp {
                window: WindowHandle(enter_state.surface),
            });
        }
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis(&mut self, _pointer: &mut wl::Pointer, time: u32, axis: u32, value: wl::Fixed) {
        tracing::trace!("pointer.axis");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn frame(&mut self, _pointer: &mut wl::Pointer) {
        // tracing::trace!("pointer.frame");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_source(&mut self, _pointer: &mut wl::Pointer, axis_source: u32) {
        tracing::trace!("pointer.axis_source");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_stop(&mut self, _pointer: &mut wl::Pointer, time: u32, axis: u32) {
        tracing::trace!("pointer.axis_stop");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_discrete(&mut self, _pointer: &mut wl::Pointer, axis: u32, discrete: i32) {
        tracing::trace!("pointer.axis_discrete");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_value120(&mut self, _pointer: &mut wl::Pointer, axis: u32, value120: i32) {
        tracing::trace!("pointer.axis_value120");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_relative_direction(&mut self, _pointer: &mut wl::Pointer, axis: u32, direction: u32) {
        tracing::trace!("pointer.axis_relative_direction");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::KeyboardEventListener
    for WaylandGlobalMessaging<AppFuture>
{
    #[tracing::instrument(skip(self, _sender))]
    fn keymap(
        &mut self,
        _sender: &mut wl::Keyboard,
        format: wl::KeyboardKeymapFormat,
        fd: i32,
        size: u32,
    ) {
        let state = self.keyboard.as_mut().expect("keyboard_state.uninit");
        if format != wl::KeyboardKeymapFormat::XkbV1 {
            unimplemented!("unknown keymap format: {format:?}");
        }

        let mapped = utils::platform::linux::MappedMemory::new(
            None,
            size as _,
            libc::PROT_READ,
            libc::MAP_PRIVATE,
            &fd,
            0,
        )
        .expect("keyboard.keymap.mmap");
        let content = unsafe {
            core::ffi::CStr::from_bytes_with_nul(core::slice::from_raw_parts(
                mapped.as_ptr().cast::<u8>(),
                size as _,
            ))
            .expect("invalid content")
            .to_str()
            .expect("invalid content")
        };
        let keymap = xkbcommon::Keymap::from_buffer(
            &self.xkb_context,
            unsafe { core::slice::from_raw_parts(content.as_ptr(), size as _) },
            xkbcommon::KeymapFormat::TextV1,
            xkbcommon::KeymapCompileFlags::NO_FLAGS,
        )
        .expect("xkb_keymap.create");
        let xkb_state = xkbcommon::State::new(&keymap).expect("xkb_state.create");

        state.xkb_keymap = Some(keymap);
        state.xkb_state = Some(xkb_state);
    }

    #[tracing::instrument(skip(self, _sender, _surface))]
    fn enter(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        _surface: &mut wl::Surface,
        keys: &[u32],
    ) {
        tracing::trace!("keyboard::enter");
    }

    #[tracing::instrument(skip(self, _sender, _surface))]
    fn leave(&mut self, _sender: &mut wl::Keyboard, serial: u32, _surface: &mut wl::Surface) {
        tracing::trace!("keyboard::leave");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn key(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        time: u32,
        key: u32,
        state: wl::KeyboardKeyState,
    ) {
        let state = self.keyboard.as_mut().expect("keyboard_state.uninit");
        tracing::trace!("keyboard::key");

        if let Some(ref mut x) = state.xkb_state {
            let mut buf = Vec::with_capacity(32);
            // evdevのスキャンコードでくるので、xkbのスキャンコードにする(8を足せばいいらしい: https://wayland-book.com/seat/keyboard.html)
            let mut alen = x.key_get_utf8(key + 8, buf.spare_capacity_mut());
            if alen > buf.capacity() {
                buf.reserve(alen - buf.capacity());
                alen = x.key_get_utf8(key + 8, buf.spare_capacity_mut());
            }
            unsafe {
                buf.set_len(alen);
            }
            tracing::trace!(
                alen,
                text = unsafe { core::str::from_utf8_unchecked(&buf) },
                "keyboard translated"
            );
        }
    }

    #[tracing::instrument(skip(self, _sender))]
    fn modifiers(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        mods_depressed: u32,
        mods_latched: u32,
        mods_locked: u32,
        group: u32,
    ) {
        let state = self.keyboard.as_mut().expect("keyboard_state.uninit");
        tracing::trace!("keyboard::modifiers");

        if let Some(ref mut x) = state.xkb_state {
            x.update_mask(
                mods_depressed,
                mods_latched,
                mods_locked,
                group,
                group,
                group,
            );
        }
    }

    #[tracing::instrument(skip(self, _sender))]
    fn repeat_info(&mut self, _sender: &mut wl::Keyboard, rate: i32, delay: i32) {
        tracing::trace!("keyboard::repeat_info");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::ZwpTextInputV3EventListener
    for WaylandGlobalMessaging<AppFuture>
{
    #[tracing::instrument(skip(self, sender, _surface))]
    fn enter(&mut self, sender: &mut wl::ZwpTextInputV3, _surface: &mut wl::Surface) {
        tracing::trace!("textinputv3::enter");
        sender.enable().expect("text_input.enable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(skip(self, sender, _surface))]
    fn leave(&mut self, sender: &mut wl::ZwpTextInputV3, _surface: &mut wl::Surface) {
        tracing::trace!("textinputv3::leave");
        sender.disable().expect("text_input.disable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn preedit_string(
        &mut self,
        _sender: &mut wl::ZwpTextInputV3,
        text: Option<&core::ffi::CStr>,
        cursor_begin: i32,
        cursor_end: i32,
    ) {
        tracing::trace!("textinputv3::preedit_string");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn commit_string(&mut self, _sender: &mut wl::ZwpTextInputV3, text: Option<&core::ffi::CStr>) {
        tracing::trace!("textinputv3::commit_string");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn delete_surrounding_text(
        &mut self,
        _sender: &mut wl::ZwpTextInputV3,
        before_length: u32,
        after_length: u32,
    ) {
        tracing::trace!("textinputv3::delete_surrounding_text");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn done(&mut self, _sender: &mut wl::ZwpTextInputV3, serial: u32) {
        tracing::trace!("textinputv3::done");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::ZwlrLayerSurfaceV1EventListener
    for WaylandGlobalMessaging<AppFuture>
{
    #[tracing::instrument(skip(self, sender))]
    fn configure(
        &mut self,
        sender: &mut wl::ZwlrLayerSurfaceV1,
        serial: u32,
        width: u32,
        height: u32,
    ) {
        tracing::trace!("layer surface configure");
        sender
            .ack_configure(serial)
            .expect("layer_surface.ack_configure");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn closed(&mut self, _sender: &mut wl::ZwlrLayerSurfaceV1) {
        tracing::trace!("layer surface closed");
    }
}

#[cfg(feature = "wayland")]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct WaylandSurfaceKey(*mut wl::Surface);
#[cfg(feature = "wayland")]
unsafe impl Sync for WaylandSurfaceKey {}
#[cfg(feature = "wayland")]
unsafe impl Send for WaylandSurfaceKey {}

#[cfg(feature = "wayland")]
pub struct WaylandWindow<AppFuture: core::future::Future<Output = ()>> {
    surface: wl::Owned<wl::Surface>,
    xdg_surface: wl::Owned<wl::XdgSurface>,
    xdg_toplevel: wl::Owned<wl::XdgToplevel>,
    deco: Option<wl::Owned<wl::ZxdgToplevelDecorationV1>>,
    fractional_scale: Option<wl::Owned<wl::WpFractionalScaleV1>>,
    _appmenu: Option<wl::Owned<wl::OrgKdeKwinAppmenu>>,
    event_listener: Pin<Box<WaylandWindowEventListener<AppFuture>>>,
}
#[cfg(feature = "wayland")]
unsafe impl<AppFuture: core::future::Future<Output = ()>> Sync for WaylandWindow<AppFuture> {}
#[cfg(feature = "wayland")]
unsafe impl<AppFuture: core::future::Future<Output = ()>> Send for WaylandWindow<AppFuture> {}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> WaylandWindow<AppFuture> {
    fn new(
        wl_interfaces: &WaylandGlobalInterfaces,
        dbus: &dbus::Connection,
        terminate_event: Arc<EventFD>,
        event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
        composite_root: CompositeTreeRef,
        ht_root: HitTestTreeRef,
    ) -> Self {
        let mut surface = wl_interfaces
            .compositor
            .create_surface()
            .expect("wl_surface create");
        let mut xdg_surface = wl_interfaces
            .xdg_wm_base
            .get_xdg_surface(&surface)
            .expect("xdg_surface create");
        let mut xdg_toplevel = xdg_surface.get_toplevel().expect("xdg_toplevel create");
        xdg_toplevel
            .set_title(c"Peridot Marble Editor")
            .expect("xdg_toplevel.set_title");
        xdg_surface
            .set_window_geometry(0, 0, 640, 480)
            .expect("xdg_surface.set_window_geometry");

        let appmenu = if let Some(ref am) = wl_interfaces.kde_appmenu_manager {
            let a = am.create(&surface).expect("appmenu.create");
            a.set_address(dbus.unique_name().expect("no name"), WL_APPMENU_OBJECT_PATH)
                .expect("appmenu.set_address");

            Some(a)
        } else {
            None
        };

        let mut deco = if let Some(ref dm) = wl_interfaces.zxdg_decoration_manager {
            let d = dm
                .get_toplevel_decoration(&xdg_toplevel)
                .expect("decoration.get_toplevel");
            d.set_mode(wl::ZxdgToplevelDecorationV1Mode::ClientSide)
                .expect("decoration.set_mode");

            Some(d)
        } else {
            None
        };

        let mut fractional_scale = if let Some(ref fs) = wl_interfaces.fractional_scale_manager {
            let f = fs
                .get_fractional_scale(&surface)
                .expect("fractional_scale.create");

            Some(f)
        } else {
            None
        };

        let mut event_listener = Box::pin(WaylandWindowEventListener {
            state: WaylandWindowState {
                surface_ptr: surface.as_ptr(),
                xdg_surface_ptr: xdg_surface.as_ptr(),
                composite_root,
                ht_root,
                committed_state: Mutex::new(WaylandWindowCommittedState {
                    active_buffer_scale: 1.0,
                    active_size: Size::new_pixels(640, 480),
                    active_size_logical: Size::new_logical(640.0, 480.0),
                }),
                swapchain_externally_invalidation_signal: std::sync::Arc::new(
                    std::sync::atomic::AtomicBool::new(false),
                ),
                latest_ui_scale_changes: std::sync::Arc::new(Mutex::new(None)),
            },
            has_fractional_scale_support: fractional_scale.is_some(),
            pending_configure_size: (None, None),
            pending_configure_buffer_scale: None,
            terminate_event,
            event_dispatcher,
        });
        surface
            .set_listener(event_listener.as_mut().get_mut())
            .into_result()
            .expect("wl_surface set listener");
        xdg_surface
            .set_listener(event_listener.as_mut().get_mut())
            .into_result()
            .expect("xdg_surface set listener");
        xdg_toplevel
            .set_listener(event_listener.as_mut().get_mut())
            .into_result()
            .expect("xdg_toplevel set listener");
        if let Some(ref mut x) = deco {
            x.set_listener(event_listener.as_mut().get_mut())
                .into_result()
                .expect("zxdg_toplevel_decoration_v1.set_listener");
        }
        if let Some(ref mut x) = fractional_scale {
            x.set_listener(event_listener.as_mut().get_mut())
                .into_result()
                .expect("wp_fractional_scale_v1.set_listener");
        }

        // commits initial state
        surface.commit().expect("wl_surface.commit");

        Self {
            surface,
            xdg_surface,
            xdg_toplevel,
            _appmenu: appmenu,
            deco,
            fractional_scale,
            event_listener,
        }
    }

    pub const fn as_key(&self) -> WaylandSurfaceKey {
        WaylandSurfaceKey(self.surface.as_ptr())
    }

    pub fn rebind_event_dispatcher(
        &mut self,
        event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
    ) {
        self.event_listener.event_dispatcher = event_dispatcher;
    }

    fn commit(&self) {
        self.surface.commit().expect("wl_surface.commit");
    }
}

#[cfg(feature = "wayland")]
struct WaylandWindowCommittedState {
    active_buffer_scale: f32,
    active_size: Size<PixelsUnit>,
    active_size_logical: Size<LogicalUnit>,
}

#[cfg(feature = "wayland")]
struct WaylandWindowState {
    surface_ptr: *mut wl::Surface,
    xdg_surface_ptr: *mut wl::XdgSurface,
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    committed_state: Mutex<WaylandWindowCommittedState>,
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    latest_ui_scale_changes: std::sync::Arc<Mutex<Option<f32>>>,
}
#[cfg(feature = "wayland")]
unsafe impl Sync for WaylandWindowState {}
#[cfg(feature = "wayland")]
unsafe impl Send for WaylandWindowState {}

#[cfg(feature = "wayland")]
#[repr(C)] // place state at 0 always: WaylandWindowEventListener can be reinterpreted as WaylandWindowState
struct WaylandWindowEventListener<AppFuture: core::future::Future<Output = ()>> {
    state: WaylandWindowState,
    has_fractional_scale_support: bool,
    pending_configure_size: (Option<i32>, Option<i32>),
    pending_configure_buffer_scale: Option<f32>,
    terminate_event: std::sync::Arc<EventFD>,
    event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::SurfaceEventListener
    for WaylandWindowEventListener<AppFuture>
{
    #[tracing::instrument(skip(self, _surface, _output))]
    fn enter(&mut self, _surface: &mut wl::Surface, _output: &mut wl::Output) {}

    #[tracing::instrument(skip(self, _surface, _output))]
    fn leave(&mut self, _surface: &mut wl::Surface, _output: &mut wl::Output) {}

    #[tracing::instrument(skip(self, _surface))]
    fn preferred_buffer_scale(&mut self, _surface: &mut wl::Surface, factor: i32) {
        tracing::trace!(
            has_fractional_scale = self.has_fractional_scale_support,
            "perferred buffer scale"
        );
        if self.has_fractional_scale_support {
            // fractional_scaleがある場合はこっちは処理しなくていい
            return;
        }

        self.pending_configure_buffer_scale = Some(factor as _);
    }

    #[tracing::instrument(skip(self, _surface))]
    fn preferred_buffer_transform(&mut self, _surface: &mut wl::Surface, transform: u32) {
        tracing::trace!("preferred buffer transform");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::XdgSurfaceEventListener
    for WaylandWindowEventListener<AppFuture>
{
    #[tracing::instrument(skip(self, sender))]
    fn configure(&mut self, sender: &mut wl::XdgSurface, serial: u32) {
        tracing::trace!("xdg surface configure");

        self.commit();
        sender
            .ack_configure(serial)
            .expect("xdg_surface.ack_configure");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::XdgToplevelEventListener
    for WaylandWindowEventListener<AppFuture>
{
    #[tracing::instrument(skip(self, sender))]
    fn close(&mut self, sender: &mut wl::XdgToplevel) {
        tracing::trace!("xdg toplevel close");
        self.terminate_event.inc(1).expect("terminate_event.inc");
    }

    #[tracing::instrument(skip(self, sender), fields(states = ?unsafe { states.as_slice::<u32>() }))]
    fn configure(
        &mut self,
        sender: &mut wl::XdgToplevel,
        width: i32,
        height: i32,
        states: &mut wl::ffi::Array,
    ) {
        tracing::trace!("xdg toplevel configure");

        self.pending_configure_size = (
            if width == 0 {
                self.pending_configure_size.0
            } else {
                Some(width)
            },
            if height == 0 {
                self.pending_configure_size.1
            } else {
                Some(height)
            },
        );
    }

    fn configure_bounds(&mut self, _sender: &mut wl::XdgToplevel, _width: i32, _height: i32) {}

    fn wm_capabilities(
        &mut self,
        _sender: &mut wl::XdgToplevel,
        _capabilities: &mut wl::ffi::Array,
    ) {
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::ZxdgToplevelDecorationV1EventListener
    for WaylandWindowEventListener<AppFuture>
{
    fn configure(
        &mut self,
        _sender: &mut wl::ZxdgToplevelDecorationV1,
        mode: wl::ZxdgToplevelDecorationV1Mode,
    ) {
        match mode {
            wl::ZxdgToplevelDecorationV1Mode::ClientSide => {
                tracing::warn!("TODO: client side decoration impl");
            }
            wl::ZxdgToplevelDecorationV1Mode::ServerSide => {
                tracing::warn!("server side decoration?");
            }
        }
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::WpFractionalScaleV1EventListener
    for WaylandWindowEventListener<AppFuture>
{
    #[tracing::instrument(skip(self, _sender))]
    fn preferred_scale(&mut self, _sender: &mut wl::WpFractionalScaleV1, scale: u32) {
        tracing::trace!("fractional scale");
        self.pending_configure_buffer_scale = Some(scale as f32 / 120.0);
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> WaylandWindowEventListener<AppFuture> {
    fn commit(&mut self) {
        let mut delayed_event_queue = Vec::with_capacity(2);

        {
            let mut committed_state_ref = self.state.committed_state.lock().expect("poisoned");
            let mut rescaled = false;
            if let Some(s) = self.pending_configure_buffer_scale.take() {
                if self.has_fractional_scale_support {
                    // fractional scaleでは1固定にする必要がある
                    unsafe { &*self.state.surface_ptr }
                        .set_buffer_scale(1)
                        .expect("wl_surface.set_buffer_scale");
                } else {
                    unsafe { &*self.state.surface_ptr }
                        .set_buffer_scale(s as _)
                        .expect("wl_surface.set_buffer_scale");
                }

                committed_state_ref.active_buffer_scale = s;
                delayed_event_queue.push(Event::WindowRescaleUI {
                    window: WindowHandle(self.state.surface_ptr),
                    new_scale: s,
                });
                rescaled = true;
            }

            let (w, h) = (
                self.pending_configure_size.0.take(),
                self.pending_configure_size.1.take(),
            );
            if rescaled || w.is_some() || h.is_some() {
                // potentially size changes
                let logical_size = Size::new_logical(
                    w.map_or(committed_state_ref.active_size_logical.width, |x| x as _),
                    h.map_or(committed_state_ref.active_size_logical.height, |y| y as _),
                );
                let pixels_size =
                    logical_size.to_pixels_ceil(committed_state_ref.active_buffer_scale);
                if pixels_size != committed_state_ref.active_size {
                    committed_state_ref.active_size = pixels_size;
                    committed_state_ref.active_size_logical = logical_size;
                    self.state
                        .swapchain_externally_invalidation_signal
                        .store(true, std::sync::atomic::Ordering::Relaxed);

                    delayed_event_queue.push(Event::WindowResize {
                        window: WindowHandle(self.state.surface_ptr),
                        size: logical_size,
                    });
                }
            }
        }

        for x in delayed_event_queue {
            self.event_dispatcher.dispatch(x);
        }
    }
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

#[cfg(target_os = "macos")]
pub struct MacWindow<AppFuture> {
    native_ptr: *mut platform::mac::bridge::WindowLink,
    dispatcher: Pin<Box<MacWindowDispatcher<AppFuture>>>,
}
#[cfg(target_os = "macos")]
impl<AppFuture> Drop for MacWindow<AppFuture> {
    fn drop(&mut self) {
        unsafe {
            platform::mac::bridge::ni_unset_window_callbacks(self.native_ptr);
            platform::mac::bridge::ni_release_window(self.native_ptr);
        }
    }
}
#[cfg(target_os = "macos")]
unsafe impl<AppFuture> Sync for MacWindow<AppFuture> {}
#[cfg(target_os = "macos")]
unsafe impl<AppFuture> Send for MacWindow<AppFuture> {}
#[cfg(target_os = "macos")]
impl<AppFuture: core::future::Future<Output = ()>> MacWindow<AppFuture> {
    pub fn new(event_dispatcher: LogicFiberEventDispatcher<AppFuture>) -> Self {
        let native_ptr = unsafe { platform::mac::bridge::ni_create_window() };
        let init_scale = unsafe { platform::mac::bridge::ni_get_content_scale(native_ptr) };
        let mut dispatcher = Box::pin(MacWindowDispatcher {
            event_dispatcher,
            state: MacWindowState {
                wlink: native_ptr,
                swapchain_externally_invalidation_signal: std::sync::Arc::new(
                    std::sync::atomic::AtomicBool::new(false),
                ),
                active_size: std::sync::Mutex::new(Size::new_logical(960.0, 540.0)),
                active_rt_size: std::sync::Mutex::new(
                    Size::new_logical(960.0, 540.0).to_pixels_ceil(init_scale),
                ),
                active_buffer_scale: std::sync::Mutex::new(init_scale),
            },
        });
        let callbacks: &'static platform::mac::bridge::WindowLinkCallbacks =
            &platform::mac::bridge::WindowLinkCallbacks {
                on_resize: MacWindowDispatcher::<AppFuture>::on_resize,
                on_pointer_down: MacWindowDispatcher::<AppFuture>::on_pointer_down,
                on_pointer_move: MacWindowDispatcher::<AppFuture>::on_pointer_move,
                on_pointer_up: MacWindowDispatcher::<AppFuture>::on_pointer_up,
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
    pub fn make_primary_window(&mut self) {
        unsafe {
            platform::mac::bridge::ni_make_primary_window(self.native_ptr);
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
struct MacWindowDispatcher<AppFuture> {
    event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
    state: MacWindowState,
}
#[cfg(target_os = "macos")]
unsafe impl<AppFuture> Sync for MacWindowDispatcher<AppFuture> {}
#[cfg(target_os = "macos")]
unsafe impl<AppFuture> Send for MacWindowDispatcher<AppFuture> {}
#[cfg(target_os = "macos")]
impl<AppFuture: core::future::Future<Output = ()>> MacWindowDispatcher<AppFuture> {
    extern "C" fn on_resize(caller_context: *mut core::ffi::c_void, width: f64, height: f64) {
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
            this.event_dispatcher
                .dispatch(Event::WindowResize(new_size));
        }
    }

    extern "C" fn on_pointer_down(caller_context: *mut core::ffi::c_void, x: f64, y: f64) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // tracing::info!(x, y, "pointer down");
        this.event_dispatcher.dispatch(Event::PointerMove {
            client_pos: Point::new_logical(x as _, y as _),
        });
        this.event_dispatcher.dispatch(Event::PointerDown {
            active_window: this.state.wlink,
        });
    }

    extern "C" fn on_pointer_move(caller_context: *mut core::ffi::c_void, x: f64, y: f64) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // tracing::trace!(x, y, "pointer move");
        this.event_dispatcher.dispatch(Event::PointerMove {
            client_pos: Point::new_logical(x as _, y as _),
        });
    }

    extern "C" fn on_pointer_up(caller_context: *mut core::ffi::c_void) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        // tracing::info!("pointer up");
        this.event_dispatcher.dispatch(Event::PointerUp);
    }
}

#[cfg(target_os = "macos")]
struct MacWindowState {
    wlink: *mut platform::mac::bridge::WindowLink,
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    active_size: std::sync::Mutex<Size<LogicalUnit>>,
    active_rt_size: std::sync::Mutex<Size<PixelsUnit>>,
    active_buffer_scale: std::sync::Mutex<f32>,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindowState {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindowState {}
