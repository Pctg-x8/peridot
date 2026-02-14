use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Device, DeviceMemoryMut, Fence, FenceMut,
    ImageChild, InstanceChild, MemoryBound, PhysicalDevice, QueueMut, RenderPass, ShaderModule,
    SurfaceCreateInfo, Swapchain, VkHandle, VkHandleMut,
};
use core::pin::Pin;
#[cfg(target_os = "linux")]
use linux_epoll::{Epoll, EpollEventBits};
#[cfg(feature = "wayland")]
use linux_eventfd::{EventFD, EventFDFlags};
#[cfg(target_os = "linux")]
use peridot_tp_dbus as dbus;
#[cfg(feature = "fontconfig")]
use peridot_tp_fontconfig as fc;
#[cfg(feature = "freetype")]
use peridot_tp_freetype as ft;
#[cfg(feature = "wayland")]
use peridot_tp_wayland as wl;
#[cfg(target_os = "linux")]
use peridot_tp_xkbcommon as xkbcommon;
#[cfg(target_os = "linux")]
use std::os::fd::AsRawFd;
#[cfg(feature = "wayland")]
use std::sync::RwLock;
use std::{
    collections::VecDeque,
    sync::{Arc, Mutex},
};
#[cfg(windows)]
use windows::{
    UI::{
        Composition::CompositionEffectSourceParameter,
        Text::Core::{CoreTextEditContext, CoreTextServicesManager},
    },
    Win32::{
        Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
        Graphics::{
            DirectWrite::{DWRITE_FACTORY_TYPE_SHARED, DWriteCreateFactory, IDWriteFactory},
            Gdi::HBRUSH,
        },
        System::{
            LibraryLoader::GetModuleHandleW,
            WinRT::{
                Composition::ICompositorDesktopInterop, CreateDispatcherQueueController,
                DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT, DispatcherQueueOptions,
            },
        },
        UI::WindowsAndMessaging::{
            CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, DispatchMessageW, GetClientRect,
            GetMessageW, GetWindowLongPtrW, HCURSOR, HICON, IDI_APPLICATION, LoadIconW,
            NCCALCSIZE_PARAMS, PostQuitMessage, RegisterClassExW, SHOW_WINDOW_CMD, SW_HIDE,
            SW_SHOWNOACTIVATE, SW_SHOWNORMAL, SetWindowLongPtrW, ShowWindow, TranslateMessage,
            WINDOW_LONG_PTR_INDEX, WM_DESTROY, WNDCLASS_STYLES, WNDCLASSEXW, WS_EX_APPWINDOW,
            WS_EX_NOACTIVATE, WS_EX_NOREDIRECTIONBITMAP, WS_EX_TOPMOST, WS_EX_TRANSPARENT,
            WS_OVERLAPPEDWINDOW, WS_POPUP,
        },
    },
};

#[cfg(windows)]
use windows_core::*;
#[cfg(windows)]
use windows_numerics::{Vector2, Vector3};

#[cfg(windows)]
use crate::{
    bindgen::Microsoft::Graphics::Canvas::Effects::{EffectOptimization, GaussianBlurEffect},
    text::FontSet,
};
use crate::{
    composite::{
        AnimatableColor, AnimatableFloat, AnimationCurve, BoundCompositeRenderer, CompositeMode,
        CompositeRect, CompositeRectText, CompositeRectTextHorizontalAlignment,
        CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeRenderingData,
        CompositeStreamingData, CompositeTree, CompositeTreeRef, CompositeTreeRender,
        CompositeTreeSyncBuffer, VectorRasterizationState,
    },
    graphics::{VG_COLOR_FORMAT, VG_STENCIL_FORMAT, VulkanDevice},
    hittest::{CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager},
    input::{KeyboardFocusManager, PointerInputManager, PointerInputUnit, ShellPointerActions},
    text::{FontID, GlyphAtlas},
    utils::{Color32, LogicalUnit, PixelsUnit, Point, Size},
};

mod atlas;
#[cfg(windows)]
mod bindgen;
mod composite;
mod graphics;
mod helper_types;
mod hittest;
mod input;
mod mathext;
mod platform;
mod proto;
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
    platform::windows::set_panic_hook();

    #[cfg(target_os = "macos")]
    tracing_subscriber::fmt()
        .with_ansi(false)
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    #[cfg(windows)]
    tracing_subscriber::fmt()
        .with_ansi(false)
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(platform::windows::DebugOutputWriter)
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
              cursor_shaping,
              main_window,
              drag_preview_popover| {
            run(
                event_queue,
                global_time_base,
                renderer_sync,
                cursor_shaping,
                main_window,
                drag_preview_popover,
            )
        },
        event_store,
        &global_time_base,
        &Mutex::new(RendererSync {
            composite_buffer: CompositeTreeSyncBuffer::new(),
            latest_ui_scale_changes: None,
        }),
    );
}

fn main_wrapper<'sys, AppFuture: core::future::Future<Output = ()> + 'sys>(
    run_app: impl FnOnce(
        &'sys std::time::Instant,
        &'sys Mutex<RendererSync>,
        CursorShaping,
        WindowHandle,
        DragPreviewPopoverHandle,
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
    let app_runtime = platform::windows::WindowsAppRuntimeBootstrap::init();
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

    #[cfg(feature = "freetype")]
    let ft = FreeType::init().expect("FreeType.init");

    #[cfg(windows)]
    let hinstance: HINSTANCE = unsafe { GetModuleHandleW(None).expect("GetModuleHandleW").into() };
    #[cfg(windows)]
    let atom = unsafe {
        register_class(&WNDCLASSEXW {
            cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
            style: WNDCLASS_STYLES(0),
            cbClsExtra: 0,
            cbWndExtra: core::mem::size_of::<[usize; 3]>() as _,
            lpfnWndProc: Some(WindowState::<AppFuture>::handle_messages),
            hInstance: hinstance,
            hIcon: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
            hCursor: HCURSOR(core::ptr::null_mut()),
            hbrBackground: HBRUSH(core::ptr::null_mut()),
            lpszMenuName: PCWSTR::null(),
            lpszClassName: w!("MainWindow"),
            hIconSm: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
        })
        .expect("register_class.main")
    };
    #[cfg(windows)]
    let w = unsafe {
        CreateWindowExW(
            WS_EX_APPWINDOW,
            PCWSTR(core::ptr::without_provenance(atom as _)),
            w!("Peridot Marble Editor"),
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            None,
            None,
            Some(hinstance),
            None,
        )
        .expect("CreateWindowExW")
    };
    #[cfg(windows)]
    let w = Win32Window(w);

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
        let shm_region =
            platform::linux::TemporalSharedMemory::new_unique(c"/pme_shm", libc::O_RDWR, 0o0600)
                .expect("buf.shm.create")
                .expect("buf.shm.create.non_unique");
        unsafe {
            platform::linux::ftruncate(&shm_region, 4).expect("buf.shm.resize");
        }

        let mapped = platform::linux::MappedMemory::new(
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
        root_window: core::ptr::null_mut(),
        buf: popover_buf,
        popup: None,
    };

    #[cfg(target_os = "macos")]
    let drag_preview_popover = DragPreviewPopoverHandle {
        bound_window_link: core::ptr::null_mut(),
    };

    #[cfg(feature = "wayland")]
    let mut w = WaylandWindow::new(&wl_interfaces, &dbus);

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
    let vk_surface = Surface {
        handle: unsafe {
            br::Win32SurfaceCreateInfo::new(
                core::mem::transmute(hinstance),
                core::mem::transmute(w.0),
            )
            .execute(vk_device.instance(), None)
            .expect("vk_surface.create")
        },
        device: &vk_device,
    };

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
    let vk_surface = Surface {
        handle: unsafe {
            br::WaylandSurfaceCreateInfo::new(wl_display.as_raw().cast(), w.surface.as_raw().cast())
                .execute(vk_device.instance(), None)
                .expect("vk_surface.create")
        },
        device: &vk_device,
    };

    #[cfg(target_os = "macos")]
    let vk_surface = Surface {
        handle: unsafe {
            br::MetalSurfaceCreateInfo::new(w.metal_layer())
                .execute(vk_device.instance(), None)
                .expect("vk_surface.create")
        },
        device: &vk_device,
    };

    match vk_device
        .primary_adapter_ref()
        .surface_support(vk_device.present_queue_family_index(), &vk_surface)
    {
        Ok(true) => (),
        Ok(false) => {
            panic!("surface not supported on graphics queue");
        }
        Err(e) => Err(e).expect("surface_support"),
    };

    #[cfg(feature = "wayland")]
    let surface_states = Mutex::new(HashMap::new());
    #[cfg(feature = "wayland")]
    let mut wl_global_msg = core::pin::pin!(WaylandGlobalMessaging {
        surface_states: &surface_states,
        xdg_surface_to_surface: HashMap::new(),
        surface_to_xdg_surface: HashMap::new(),
        xdg_toplevel_to_surface: HashMap::new(),
        fractional_scale_to_surface: HashMap::new(),
        text_input_manager: wl_interfaces.text_input_manager.as_ptr(),
        xkb_context: xkbcommon::Context::new(xkbcommon::ContextFlags::NO_FLAGS)
            .expect("xkb_context.create"),
        keyboard: None,
        pointer: None,
        cursor_shape_manager: wl_interfaces
            .cursor_shape_manager
            .as_ref()
            .map(|x| x.as_ptr()),
        has_fractional_scale_support: wl_interfaces.fractional_scale_manager.is_some(),
        event_dispatcher: LogicFiberEventDispatcher {
            event_store: event_store.as_mut().get_mut() as *mut _ as _,
            future: core::ptr::null_mut()
        },
        _pinned: core::marker::PhantomPinned,
    });
    #[cfg(feature = "wayland")]
    w.bind_global_messaging(wl_global_msg.as_mut(), terminate_event.clone());

    let mut app = core::pin::pin!(run_app(
        global_time_base,
        renderer_sync,
        #[cfg(feature = "wayland")]
        CursorShaping {
            pointer_state_ref: unsafe {
                &mut wl_global_msg.as_mut().get_unchecked_mut().pointer as *mut _
            },
        },
        #[cfg(windows)]
        CursorShaping {},
        #[cfg(windows)]
        WindowHandle { hwnd: w.0 },
        #[cfg(feature = "wayland")]
        WindowHandle {
            wl_surface: w.surface.as_ptr(),
            wl_surface_to_state: &surface_states,
        },
        #[cfg(target_os = "macos")]
        WindowHandle {
            state_ref: &mut w.dispatcher.state as *mut _
        },
        drag_preview_popover
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
    #[cfg(target_os = "macos")]
    unsafe {
        w.dispatcher.event_dispatcher.future = app.as_mut().get_unchecked_mut() as *mut _;
    }

    let _ = app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }));

    #[cfg(feature = "wayland")]
    w.commit();
    #[cfg(feature = "wayland")]
    wl_display.roundtrip().expect("roundtrip");

    #[cfg(windows)]
    unsafe {
        WindowState::set_for_window(
            &w,
            Box::new(WindowState {
                content_scale: windows::Win32::UI::HiDpi::GetDpiForWindow(w.0) as f32 / 96.0,
                event_dispatcher: LogicFiberEventDispatcher {
                    event_store: event_store.as_mut().get_mut() as *mut _ as _,
                    future: app.as_mut().get_unchecked_mut() as *mut _ as _,
                },
                text_services_mgr: None,
                edit_context: None,
            }),
        );
    }

    let shutdown = std::sync::atomic::AtomicBool::new(false);
    std::thread::scope(|thread_scope| {
        let render_thread = std::thread::Builder::new()
            .name("Render".into())
            .spawn_scoped(thread_scope, || {
                tracing::info!("Starting RenderThread...");
                let mut render_queue = vk_device.queue(vk_device.present_queue_family_index(), 0);

                let mut composite_tree = CompositeTreeRender::new();

                #[cfg(windows)]
                let dw_factory: IDWriteFactory = unsafe {
                    DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED).expect("dwrite.factory.create")
                };

                let surface_present_modes = vk_device
                    .primary_adapter_ref()
                    .surface_present_modes_alloc(&vk_surface)
                    .expect("vk_surface.present_modes");
                let surface_caps = vk_device
                    .primary_adapter_ref()
                    .surface_capabilities(&vk_surface)
                    .expect("vk_surface.capabilities");
                let surface_formats = vk_device
                    .primary_adapter_ref()
                    .surface_formats_alloc(&vk_surface)
                    .expect("vk_surface.formats");
                let mut surface_ext = if surface_caps.currentExtent.width == 0xffffffff
                    || surface_caps.currentExtent.height == 0xffffffff
                {
                    #[cfg(windows)]
                    let client_size = w.pixels_client_size();
                    #[cfg(feature = "wayland")]
                    let client_size =
                        surface_states.lock().expect("poisoned")[&w.as_key()].active_size;
                    #[cfg(target_os = "macos")]
                    let client_size = *w.dispatcher.state.active_rt_size.lock().expect("poisoned");

                    br::Extent2D {
                        width: if surface_caps.currentExtent.width == 0xffffffff {
                            client_size.width
                        } else {
                            surface_caps.currentExtent.width
                        },
                        height: if surface_caps.currentExtent.height == 0xffffffff {
                            client_size.height
                        } else {
                            surface_caps.currentExtent.height
                        },
                    }
                } else {
                    surface_caps.currentExtent
                };
                let surface_format = surface_formats
                    .iter()
                    .find(|f| {
                        f.colorSpace == br::vk::VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
                            && f.format == br::vk::VK_FORMAT_B8G8R8A8_SRGB
                    })
                    .copied()
                    .expect("no suitable surface format");
                let surface_present_mode = surface_present_modes
                    .iter()
                    .find(|&&x| x == br::PresentMode::FIFO)
                    .copied()
                    .expect("no suitable present mode");
                tracing::trace!(?surface_ext, "swapchain.create");
                let mut vk_swapchain = br::SwapchainBuilder::new(
                    &vk_surface,
                    surface_caps.minImageCount.max(2),
                    surface_format,
                    surface_ext,
                    br::ImageUsageFlags::COLOR_ATTACHMENT,
                )
                .present_mode(surface_present_mode)
                .pre_transform(br::SurfaceTransformFlags::IDENTITY.bits())
                .composite_alpha(br::CompositeAlphaFlags::OPAQUE.bits())
                .create(&vk_device)
                .expect("swapchain create");
                let mut backbuffer_images = vk_swapchain
                    .images_alloc()
                    .expect("backbuffer images")
                    .into_iter()
                    .map(|x| x.unmanage().0)
                    .collect::<Vec<_>>();
                let mut backbuffer_image_views = backbuffer_images
                    .iter()
                    .map(|b| LocalImageView {
                        handle: unsafe {
                            br::vkfn_wrapper::create_image_view(
                                vk_device.native_ptr(),
                                &br::ImageViewCreateInfo::new(
                                    br::VkHandleRef::from_raw_ref(b),
                                    br::ImageSubresourceRange::new(
                                        br::AspectMask::COLOR,
                                        0..1,
                                        0..1,
                                    ),
                                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                    surface_format.format,
                                ),
                                None,
                            )
                            .expect("backbuffer image view create")
                        },
                        device: &vk_device,
                    })
                    .collect::<Vec<_>>();

                let vk_render_pass = br::RenderPassObject::new(
                    &vk_device,
                    &br::RenderPassCreateInfo2::new(
                        &[br::AttachmentDescription2::new(surface_format.format)
                            .color_memory_op(br::LoadOp::Load, br::StoreOp::Store)
                            .layout_transition(
                                br::ImageLayout::PresentSrc,
                                br::ImageLayout::PresentSrc,
                            )],
                        &[br::SubpassDescription2::new()
                            .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                        &[br::SubpassDependency2::new(
                            br::SubpassIndex::Internal(0),
                            br::SubpassIndex::External,
                        )
                        .by_region()
                        .of_memory(
                            br::AccessFlags::COLOR_ATTACHMENT.write,
                            br::AccessFlags::MEMORY.read,
                        )
                        .of_execution(
                            br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                            br::PipelineStageFlags(0),
                        )],
                    ),
                )
                .expect("render pass create");
                let mut vk_framebuffers = backbuffer_image_views
                    .iter()
                    .map(|bb| {
                        br::FramebufferObject::new(
                            &vk_device,
                            &br::FramebufferCreateInfo::new(
                                &vk_render_pass,
                                &[bb.as_transparent_ref()],
                                surface_ext.width,
                                surface_ext.height,
                            ),
                        )
                        .expect("framebuffer create")
                    })
                    .collect::<Vec<_>>();

                let dpi = 108;
                let mut glyph_atlas = GlyphAtlas::new(&vk_device);
                #[cfg(feature = "freetype")]
                let font_set = FontSet::new(&ft, dpi);
                #[cfg(target_os = "macos")]
                let font_set = FontSet::new();
                #[cfg(windows)]
                let font_set = FontSet::new(dw_factory);

                #[derive(br::SpecializationConstants)]
                struct FillShaderVertexConstants {
                    #[constant_id = 0]
                    target_texture_width: f32,
                    #[constant_id = 1]
                    target_texture_height: f32,
                }
                #[derive(br::SpecializationConstants)]
                struct CurveShaderVertexConstants {
                    #[constant_id = 0]
                    target_texture_width: f32,
                    #[constant_id = 1]
                    target_texture_height: f32,
                }
                let fill_shader_module = vk_device.require_shader("vg-fill.spv");
                let curve_shader_module = vk_device.require_shader("vg-curve.spv");
                let vec_tri_fill_shader_module = vk_device.require_shader("vec-tri-fill.spv");

                let vector_render_pass = br::RenderPassObject::new(
                    &vk_device,
                    &br::RenderPassCreateInfo2::new(
                        &[
                            br::AttachmentDescription2::new(br::vk::VK_FORMAT_S8_UINT)
                                .stencil_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
                                .layout_transition(
                                    br::ImageLayout::Undefined,
                                    br::ImageLayout::DepthStencilReadOnlyOpt,
                                )
                                .samples(GlyphAtlas::MULTISAMPLE_LEVEL),
                            br::AttachmentDescription2::new(br::vk::VK_FORMAT_R8_UNORM)
                                .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)
                                .layout_transition(
                                    br::ImageLayout::Undefined,
                                    br::ImageLayout::TransferSrcOpt,
                                )
                                .samples(GlyphAtlas::MULTISAMPLE_LEVEL),
                        ],
                        &[
                            br::SubpassDescription2::new().depth_stencil(
                                &br::AttachmentReference2::depth_stencil_attachment_opt(0),
                            ),
                            br::SubpassDescription2::new()
                                .depth_stencil(
                                    &br::AttachmentReference2::depth_stencil_readonly_opt(0),
                                )
                                .colors(&[br::AttachmentReference2::color_attachment_opt(1)]),
                        ],
                        &[
                            br::SubpassDependency2::new(
                                br::SubpassIndex::Internal(0),
                                br::SubpassIndex::Internal(1),
                            )
                            .by_region()
                            .of_memory(
                                br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
                                br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.read,
                            )
                            .of_execution(
                                br::PipelineStageFlags::LATE_FRAGMENT_TESTS,
                                br::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
                            ),
                            br::SubpassDependency2::new(
                                br::SubpassIndex::Internal(1),
                                br::SubpassIndex::External,
                            )
                            .by_region()
                            .of_memory(
                                br::AccessFlags::COLOR_ATTACHMENT.write,
                                br::AccessFlags::TRANSFER.read,
                            )
                            .of_execution(
                                br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                                br::PipelineStageFlags::TRANSFER,
                            ),
                        ],
                    ),
                )
                .expect("vector render pass create");

                let pipeline_layout = br::PipelineLayoutObject::new(
                    &vk_device,
                    &br::PipelineLayoutCreateInfo::new(&[], &[]),
                )
                .expect("vector pipeline layout create");
                let [triangle_fans_pipeline, curve_pipeline, colorize_pipeline] = vk_device
                    .new_graphics_pipeline_array(
                        &[
                            br::GraphicsPipelineCreateInfo::new(
                                &pipeline_layout,
                                vector_render_pass.subpass(0),
                                &[
                                    fill_shader_module
                                        .on_stage(br::ShaderStage::Vertex, c"vertMain")
                                        .with_specialization_info(&br::SpecializationInfo::new(
                                            &FillShaderVertexConstants {
                                                target_texture_width: glyph_atlas.size().width as _,
                                                target_texture_height: glyph_atlas.size().height
                                                    as _,
                                            },
                                        )),
                                    fill_shader_module
                                        .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                                ],
                                &br::PipelineVertexInputStateCreateInfo::new(
                                    &[br::VertexInputBindingDescription::per_vertex_typed::<
                                        [f32; 2],
                                    >(0)],
                                    &[br::VertexInputAttributeDescription {
                                        location: 0,
                                        binding: 0,
                                        offset: 0,
                                        format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                                    }],
                                ),
                                &br::PipelineInputAssemblyStateCreateInfo::new(
                                    br::PrimitiveTopology::TriangleList,
                                ),
                                &br::PipelineViewportStateCreateInfo::new(
                                    &[glyph_atlas
                                        .size()
                                        .into_rect(br::Offset2D::ZERO)
                                        .make_viewport(0.0..1.0)],
                                    &[glyph_atlas.size().into_rect(br::Offset2D::ZERO)],
                                ),
                                &br::PipelineRasterizationStateCreateInfo::new(
                                    br::PolygonMode::Fill,
                                    br::CullModeFlags::NONE,
                                    br::FrontFace::CounterClockwise,
                                ),
                                &br::PipelineColorBlendStateCreateInfo::new(&[
                                    br::vk::VkPipelineColorBlendAttachmentState::NOBLEND,
                                ]),
                            )
                            .set_multisample_state(
                                &br::PipelineMultisampleStateCreateInfo::new()
                                    .rasterization_samples(GlyphAtlas::MULTISAMPLE_LEVEL as _),
                            )
                            .set_depth_stencil_state(
                                &br::PipelineDepthStencilStateCreateInfo::new()
                                    .stencil_test(true)
                                    .stencil_state_front(
                                        br::vk::VkStencilOpState::always_forall(
                                            br::StencilOp::Invert,
                                        )
                                        .write_mask(0x01),
                                    )
                                    .stencil_state_back(
                                        br::vk::VkStencilOpState::always_forall(
                                            br::StencilOp::Invert,
                                        )
                                        .write_mask(0x01),
                                    ),
                            ),
                            br::GraphicsPipelineCreateInfo::new(
                                &pipeline_layout,
                                vector_render_pass.subpass(0),
                                &[
                                    curve_shader_module
                                        .on_stage(br::ShaderStage::Vertex, c"vertMain")
                                        .with_specialization_info(&br::SpecializationInfo::new(
                                            &CurveShaderVertexConstants {
                                                target_texture_width: glyph_atlas.size().width as _,
                                                target_texture_height: glyph_atlas.size().height
                                                    as _,
                                            },
                                        )),
                                    curve_shader_module
                                        .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                                ],
                                &br::PipelineVertexInputStateCreateInfo::new(
                                    &[br::VertexInputBindingDescription::per_vertex_typed::<
                                        [f32; 4],
                                    >(0)],
                                    &[
                                        br::VertexInputAttributeDescription {
                                            location: 0,
                                            binding: 0,
                                            offset: 0,
                                            format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                                        },
                                        br::VertexInputAttributeDescription {
                                            location: 1,
                                            binding: 0,
                                            offset: core::mem::size_of::<[f32; 2]>() as _,
                                            format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                                        },
                                    ],
                                ),
                                &br::PipelineInputAssemblyStateCreateInfo::new(
                                    br::PrimitiveTopology::TriangleList,
                                ),
                                &br::PipelineViewportStateCreateInfo::new(
                                    &[glyph_atlas
                                        .size()
                                        .into_rect(br::Offset2D::ZERO)
                                        .make_viewport(0.0..1.0)],
                                    &[glyph_atlas.size().into_rect(br::Offset2D::ZERO)],
                                ),
                                &br::PipelineRasterizationStateCreateInfo::new(
                                    br::PolygonMode::Fill,
                                    br::CullModeFlags::NONE,
                                    br::FrontFace::CounterClockwise,
                                ),
                                &br::PipelineColorBlendStateCreateInfo::new(&[
                                    br::vk::VkPipelineColorBlendAttachmentState::NOBLEND,
                                ]),
                            )
                            .set_multisample_state(
                                &br::PipelineMultisampleStateCreateInfo::new()
                                    .rasterization_samples(GlyphAtlas::MULTISAMPLE_LEVEL as _),
                            )
                            .set_depth_stencil_state(
                                &br::PipelineDepthStencilStateCreateInfo::new()
                                    .stencil_test(true)
                                    .stencil_state_front(
                                        br::StencilOpState::always_forall(br::StencilOp::Invert)
                                            .write_mask(0x01),
                                    )
                                    .stencil_state_back(
                                        br::StencilOpState::always_forall(br::StencilOp::Invert)
                                            .write_mask(0x01),
                                    ),
                            ),
                            br::GraphicsPipelineCreateInfo::new(
                                &pipeline_layout,
                                vector_render_pass.subpass(1),
                                &[
                                    vec_tri_fill_shader_module
                                        .on_stage(br::ShaderStage::Vertex, c"vertMain"),
                                    vec_tri_fill_shader_module
                                        .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                                ],
                                &br::PipelineVertexInputStateCreateInfo::new(&[], &[]),
                                &br::PipelineInputAssemblyStateCreateInfo::new(
                                    br::PrimitiveTopology::TriangleList,
                                ),
                                &br::PipelineViewportStateCreateInfo::new(
                                    &[glyph_atlas
                                        .size()
                                        .into_rect(br::Offset2D::ZERO)
                                        .make_viewport(0.0..1.0)],
                                    &[glyph_atlas.size().into_rect(br::Offset2D::ZERO)],
                                ),
                                &br::PipelineRasterizationStateCreateInfo::new(
                                    br::PolygonMode::Fill,
                                    br::CullModeFlags::NONE,
                                    br::FrontFace::CounterClockwise,
                                ),
                                &br::PipelineColorBlendStateCreateInfo::new(&[
                                    br::vk::VkPipelineColorBlendAttachmentState::NOBLEND,
                                ]),
                            )
                            .set_multisample_state(
                                &br::PipelineMultisampleStateCreateInfo::new()
                                    .rasterization_samples(GlyphAtlas::MULTISAMPLE_LEVEL as _),
                            )
                            .set_depth_stencil_state(
                                &br::PipelineDepthStencilStateCreateInfo::new()
                                    .stencil_test(true)
                                    .stencil_state_front(br::StencilOpState::NOP.set_compare(
                                        br::CompareOp::Equal,
                                        0x01,
                                        0x01,
                                    ))
                                    .stencil_state_back(br::StencilOpState::NOP.set_compare(
                                        br::CompareOp::Equal,
                                        0x01,
                                        0x01,
                                    )),
                            ),
                        ],
                        None::<&br::PipelineCacheObject<&br::DeviceObject<&br::InstanceObject>>>,
                    )
                    .expect("create vector rasterize pipelines");

                let mut composite_renderer = BoundCompositeRenderer::new(
                    &vk_device,
                    glyph_atlas.view(),
                    surface_format.format,
                    surface_ext,
                    &backbuffer_image_views,
                );

                let mut init_cp = br::CommandPoolObject::new(
                    &vk_device,
                    &br::CommandPoolCreateInfo::new(vk_device.present_queue_family_index()),
                )
                .expect("init_cp.create");
                let [mut init_cb] = br::CommandBufferObject::alloc_array(
                    &vk_device,
                    &br::CommandBufferFixedCountAllocateInfo::new(
                        &mut init_cp,
                        br::CommandBufferLevel::Primary,
                    ),
                )
                .expect("init_cb.create");
                unsafe {
                    init_cb
                        .begin(&br::CommandBufferBeginInfo::new())
                        .expect("init_cb.begin")
                }
                .inject(|r| {
                    vk_device.cmd_pipeline_barrier(
                        r,
                        &br::DependencyInfo::new(
                            &[],
                            &[],
                            &[br::ImageMemoryBarrier2::new(
                                &glyph_atlas.image(),
                                glyph_atlas.image_range_entire(),
                            )
                            .transit_to(br::ImageLayout::TransferDestOpt.from_undefined())],
                        ),
                    )
                })
                .clear_color_image(
                    &glyph_atlas.image(),
                    br::ImageLayout::TransferDestOpt,
                    &[br::ClearColorValue::from([0.0; 4])],
                    &[br::ImageSubresourceRange::new(
                        br::AspectMask::COLOR,
                        0..1,
                        0..1,
                    )],
                )
                .inject(|r| {
                    vk_device.cmd_pipeline_barrier(
                        r,
                        &br::DependencyInfo::new(
                            &[],
                            &[],
                            &[br::ImageMemoryBarrier2::new(
                                &glyph_atlas.image(),
                                glyph_atlas.image_range_entire(),
                            )
                            .transit_to(
                                br::ImageLayout::ShaderReadOnlyOpt
                                    .from(br::ImageLayout::TransferDestOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::CLEAR,
                                br::AccessFlags2::TRANSFER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::FRAGMENT_SHADER,
                                br::AccessFlags2::SHADER.read,
                            )],
                        ),
                    )
                })
                .end()
                .expect("init_cb.end");
                unsafe {
                    render_queue
                        .submit_raw(
                            &[br::SubmitInfo::new(
                                &[],
                                &[],
                                &[init_cb.as_transparent_ref()],
                                &[],
                            )],
                            None,
                        )
                        .expect("init_cb.submit");
                    render_queue.wait().expect("init_cb.wait");
                }

                let mut update_cp = br::CommandPoolObject::new(
                    &vk_device,
                    &br::CommandPoolCreateInfo::new(vk_device.present_queue_family_index()),
                )
                .expect("update_cp.create");
                let [mut update_cb] = br::CommandBufferObject::alloc_array(
                    &vk_device,
                    &br::CommandBufferFixedCountAllocateInfo::new(
                        &mut update_cp,
                        br::CommandBufferLevel::Primary,
                    ),
                )
                .expect("update_cb.create");
                unsafe {
                    update_cb
                        .begin(&br::CommandBufferBeginInfo::new())
                        .expect("update_cb.begin")
                        .end()
                        .expect("update_cb.end");
                }
                let mut update_completion_fence =
                    br::FenceObject::new(&vk_device, &br::FenceCreateInfo::new(0))
                        .expect("update_completion_fence.create");
                let update_completion_semaphore =
                    br::SemaphoreObject::new(&vk_device, &br::SemaphoreCreateInfo::new())
                        .expect("update_completion_semaphore.create");
                let mut updating = false;

                let mut render_cp = br::CommandPoolObject::new(
                    &vk_device,
                    &br::CommandPoolCreateInfo::new(vk_device.present_queue_family_index()),
                )
                .expect("command pool create");
                let mut render_commands = br::CommandBufferObject::alloc(
                    &vk_device,
                    &br::CommandBufferAllocateInfo::new(
                        &mut render_cp,
                        vk_framebuffers.len() as _,
                        br::CommandBufferLevel::Primary,
                    ),
                )
                .expect("command buffer alloc");
                let mut main_cb_invalid = true;

                let present_ready_semaphores = (0..vk_framebuffers.len())
                    .map(|_| {
                        br::SemaphoreObject::new(&vk_device, &br::SemaphoreCreateInfo::new())
                            .expect("rendering_timeline_semaphore create")
                    })
                    .collect::<Vec<_>>();
                let mut backbuffer_ready_fence =
                    br::FenceObject::new(&vk_device, &br::FenceCreateInfo::new(0))
                        .expect("last render completion fence create");
                let mut swapchain_invalidated = false;
                let mut last_composite_render_data = CompositeRenderingData {
                    instructions: Vec::new(),
                    render_passes: Vec::new(),
                    required_backdrop_buffer_count: 0,
                };
                let mut vector_raster_state = VectorRasterizationState::new();
                'lp: while !shutdown.load(std::sync::atomic::Ordering::Acquire) {
                    // unsafe {
                    //     w.manual_capture_begin();
                    // }

                    #[cfg(feature = "wayland")]
                    if surface_states.lock().expect("poisoned")[&w.as_key()]
                        .swapchain_externally_invalidation_signal
                        .compare_exchange_weak(
                            true,
                            false,
                            std::sync::atomic::Ordering::Relaxed,
                            std::sync::atomic::Ordering::Relaxed,
                        )
                        .is_ok()
                    {
                        swapchain_invalidated = true;
                    }
                    #[cfg(target_os = "macos")]
                    if w.dispatcher
                        .state
                        .swapchain_externally_invalidation_signal
                        .compare_exchange_weak(
                            true,
                            false,
                            std::sync::atomic::Ordering::Relaxed,
                            std::sync::atomic::Ordering::Relaxed,
                        )
                        == Ok(true)
                    {
                        swapchain_invalidated = true;
                    }

                    if swapchain_invalidated {
                        let x = std::time::Instant::now();
                        render_queue.wait().expect("waiting pending queue works");
                        tracing::trace!(elapsed = ?x.elapsed(), "queue waiting time during resize");

                        if shutdown.load(std::sync::atomic::Ordering::Acquire) {
                            // already shut down
                            break 'lp;
                        }

                        if !main_cb_invalid {
                            unsafe {
                                render_cp
                                    .reset(br::CommandPoolResetFlags::EMPTY)
                                    .expect("reset render cp");
                            }
                            main_cb_invalid = true;
                        }
                        drop(vk_framebuffers);
                        drop(backbuffer_image_views);
                        drop(backbuffer_images);

                        let surface_caps = vk_device
                            .primary_adapter_ref()
                            .surface_capabilities(&vk_surface)
                            .expect("vk_surface.capabilities");
                        surface_ext = if surface_caps.currentExtent.width == 0xffffffff
                            || surface_caps.currentExtent.height == 0xffffffff
                        {
                            #[cfg(windows)]
                            let client_size = w.pixels_client_size();
                            #[cfg(feature = "wayland")]
                            let client_size =
                                surface_states.lock().expect("poisoned")[&w.as_key()].active_size;
                            #[cfg(target_os = "macos")]
                            let client_size =
                                *w.dispatcher.state.active_rt_size.lock().expect("poisoned");

                            br::Extent2D {
                                width: if surface_caps.currentExtent.width == 0xffffffff {
                                    client_size.width
                                } else {
                                    surface_caps.currentExtent.width
                                },
                                height: if surface_caps.currentExtent.height == 0xffffffff {
                                    client_size.height
                                } else {
                                    surface_caps.currentExtent.height
                                },
                            }
                        } else {
                            surface_caps.currentExtent
                        };

                        tracing::trace!(?surface_ext, "swapchain.create");
                        vk_swapchain = br::SwapchainBuilder::new(
                            &vk_surface,
                            surface_caps.minImageCount.max(2),
                            surface_format,
                            surface_ext,
                            br::ImageUsageFlags::COLOR_ATTACHMENT,
                        )
                        .present_mode(surface_present_mode)
                        .pre_transform(br::SurfaceTransformFlags::IDENTITY.bits())
                        .composite_alpha(br::CompositeAlphaFlags::OPAQUE.bits())
                        .enable_clip()
                        .old_swapchain(&vk_swapchain)
                        .create(&vk_device)
                        .expect("swapchain create");
                        backbuffer_images = vk_swapchain
                            .images_alloc()
                            .expect("backbuffer images")
                            .into_iter()
                            .map(|x| x.unmanage().0)
                            .collect::<Vec<_>>();
                        backbuffer_image_views = backbuffer_images
                            .iter()
                            .map(|b| LocalImageView {
                                handle: unsafe {
                                    br::vkfn_wrapper::create_image_view(
                                        vk_device.native_ptr(),
                                        &br::ImageViewCreateInfo::new(
                                            br::VkHandleRef::from_raw_ref(b),
                                            br::ImageSubresourceRange::new(
                                                br::AspectMask::COLOR,
                                                0..1,
                                                0..1,
                                            ),
                                            br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                            surface_format.format,
                                        ),
                                        None,
                                    )
                                    .expect("backbuffer image view create")
                                },
                                device: &vk_device,
                            })
                            .collect::<Vec<_>>();
                        vk_framebuffers = backbuffer_image_views
                            .iter()
                            .map(|bb| {
                                br::FramebufferObject::new(
                                    &vk_device,
                                    &br::FramebufferCreateInfo::new(
                                        &vk_render_pass,
                                        &[bb.as_transparent_ref()],
                                        surface_ext.width,
                                        surface_ext.height,
                                    ),
                                )
                                .expect("framebuffer create")
                            })
                            .collect::<Vec<_>>();

                        let mut descriptor_writes = Vec::new();
                        composite_renderer.recreate_rt_resources(
                            &vk_device,
                            surface_format.format,
                            &backbuffer_image_views,
                            surface_ext,
                            &mut descriptor_writes,
                        );
                        vk_device.update_descriptor_sets(&descriptor_writes, &[]);

                        swapchain_invalidated = false;
                    }

                    let backbuffer_index = match vk_swapchain.acquire_next(
                        None,
                        br::CompletionHandlerMut::Host(
                            backbuffer_ready_fence.as_transparent_ref_mut(),
                        ),
                    ) {
                        Ok(x) => x,
                        Err(e) if e == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                            swapchain_invalidated = true;
                            continue 'lp;
                        }
                        Err(e) => Err(e).expect("acquire next"),
                    };
                    backbuffer_ready_fence
                        .wait()
                        .expect("last render completion fence wait");
                    backbuffer_ready_fence
                        .reset()
                        .expect("last render completion fence reset");

                    let current_t = global_time_base.elapsed();
                    vector_raster_state.clear();
                    {
                        let mut renderer_sync = renderer_sync.lock().expect("poisoned");
                        if let Some(_) = renderer_sync.latest_ui_scale_changes.take() {
                            glyph_atlas.clear();
                        }
                        renderer_sync.composite_buffer.clean(&mut composite_tree);
                    }
                    let composite_render_data = composite_renderer.update(
                        &vk_device,
                        &mut composite_tree,
                        surface_ext,
                        &font_set,
                        &mut glyph_atlas,
                        &mut vector_raster_state,
                        |e| events.push(e),
                        current_t.as_secs_f32(),
                    );
                    if !vector_raster_state.is_empty() {
                        // TODO: 最適化はあとで
                        let filltri_points_offset = 0;
                        let filltri_indices_offset = filltri_points_offset
                            + core::mem::size_of_val(&vector_raster_state.fill_tri_points[..]);
                        let curve_triangles_offset = (filltri_indices_offset
                            + core::mem::size_of_val(&vector_raster_state.fill_tri_indices[..])
                            + (core::mem::size_of::<[f32; 4]>() - 1))
                            & !(core::mem::size_of::<[f32; 4]>() - 1);
                        let vector_draw_buffer_total_size = curve_triangles_offset
                            + core::mem::size_of_val(&vector_raster_state.curve_tris[..]);
                        let mut vector_draw_buffer = br::BufferObject::new(
                            &vk_device,
                            &br::BufferCreateInfo::new(
                                vector_draw_buffer_total_size,
                                br::BufferUsage::VERTEX_BUFFER
                                    | br::BufferUsage::INDEX_BUFFER
                                    | br::BufferUsage::TRANSFER_DEST,
                            ),
                        )
                        .expect("vector_draw_buffer create");
                        let vector_draw_buffer_memreq = vector_draw_buffer.requirements();
                        let vector_draw_buffer_memory = br::DeviceMemoryObject::new(
                            &vk_device,
                            &br::MemoryAllocateInfo::new(
                                vector_draw_buffer_memreq.size,
                                vk_device
                                    .find_device_local_memory_index(
                                        vector_draw_buffer_memreq.memoryTypeBits,
                                    )
                                    .expect("no suitable memory"),
                            ),
                        )
                        .expect("vector_draw_buffer malloc");
                        vector_draw_buffer
                            .bind(&vector_draw_buffer_memory, 0)
                            .expect("vector_draw_buffer bind");

                        let mut vector_draw_init_buffer = br::BufferObject::new(
                            &vk_device,
                            &br::BufferCreateInfo::new(
                                vector_draw_buffer_total_size,
                                br::BufferUsage::TRANSFER_SRC,
                            ),
                        )
                        .expect("vector_draw_init_buffer create");
                        let vector_draw_init_buffer_memreq = vector_draw_init_buffer.requirements();
                        let vector_draw_init_buffer_memindex = vk_device
                            .find_host_visible_memory_index(
                                vector_draw_init_buffer_memreq.memoryTypeBits,
                            )
                            .expect("no suitable memory");
                        let mut vector_draw_init_buffer_memory = br::DeviceMemoryObject::new(
                            &vk_device,
                            &br::MemoryAllocateInfo::new(
                                vector_draw_init_buffer_memreq.size,
                                vector_draw_init_buffer_memindex,
                            ),
                        )
                        .expect("vector_draw_init_buffer malloc");
                        vector_draw_init_buffer
                            .bind(&vector_draw_init_buffer_memory, 0)
                            .expect("vector_draw_init_buffer bind");
                        let p = vector_draw_init_buffer_memory
                            .map(0..vector_draw_buffer_total_size)
                            .expect("vector_draw_init_buffer_memory map");
                        unsafe {
                            core::ptr::copy_nonoverlapping(
                                vector_raster_state.fill_tri_points.as_ptr(),
                                p.ptr().byte_add(filltri_points_offset).cast(),
                                vector_raster_state.fill_tri_points.len(),
                            );
                            core::ptr::copy_nonoverlapping(
                                vector_raster_state.fill_tri_indices.as_ptr(),
                                p.ptr().byte_add(filltri_indices_offset).cast(),
                                vector_raster_state.fill_tri_indices.len(),
                            );
                            core::ptr::copy_nonoverlapping(
                                vector_raster_state.curve_tris.as_ptr(),
                                p.ptr().byte_add(curve_triangles_offset).cast(),
                                vector_raster_state.curve_tris.len(),
                            );
                        }
                        if !vk_device.is_coherent_memory(vector_draw_init_buffer_memindex) {
                            unsafe {
                                vk_device
                                    .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new(
                                        &vector_draw_init_buffer_memory,
                                        0..vector_draw_buffer_total_size as u64,
                                    )])
                                    .expect("flush_mapped_memory_ranges");
                            }
                        }
                        unsafe {
                            vector_draw_init_buffer_memory.unmap();
                        }

                        let mut vector_color_ms_buffer = br::ImageObject::new(
                            &vk_device,
                            &br::ImageCreateInfo::new(*glyph_atlas.size(), VG_COLOR_FORMAT)
                                .set_usage(
                                    br::ImageUsageFlags::COLOR_ATTACHMENT
                                        | br::ImageUsageFlags::TRANSFER_SRC,
                                )
                                .sample_counts(GlyphAtlas::MULTISAMPLE_LEVEL),
                        )
                        .expect("vector color_ms buffer create");
                        vk_device.dbg_set_name(&vector_color_ms_buffer, c"Vector::color_ms_buffer");
                        let mut vector_stencil_buffer = br::ImageObject::new(
                            &vk_device,
                            &br::ImageCreateInfo::new(*glyph_atlas.size(), VG_STENCIL_FORMAT)
                                .set_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT)
                                .sample_counts(GlyphAtlas::MULTISAMPLE_LEVEL),
                        )
                        .expect("vector stencil buffer create");
                        vk_device.dbg_set_name(&vector_stencil_buffer, c"Vector::stencil_buffer");
                        let vector_color_ms_buffer_memreq = vector_color_ms_buffer.requirements();
                        let vector_stencil_buffer_memreq = vector_stencil_buffer.requirements();
                        tracing::debug!(
                            ?vector_color_ms_buffer_memreq,
                            ?vector_stencil_buffer_memreq
                        );
                        let vector_color_ms_buffer_mem = br::DeviceMemoryObject::new(
                            &vk_device,
                            &br::MemoryAllocateInfo::new(
                                vector_color_ms_buffer_memreq.size,
                                vk_device
                                    .find_lazily_allocatable_device_local_memory_index(
                                        vector_color_ms_buffer_memreq.memoryTypeBits,
                                    )
                                    .expect("no suitable memory"),
                            ),
                        )
                        .expect("vector color_ms buffer malloc");
                        vector_color_ms_buffer
                            .bind(&vector_color_ms_buffer_mem, 0)
                            .expect("vector color_ms buffer bind");
                        let vector_color_ms_buffer = br::ImageViewBuilder::new(
                            vector_color_ms_buffer,
                            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        )
                        .create()
                        .expect("vector color_ms buffer imageview create");
                        let vector_stencil_buffer_mem = br::DeviceMemoryObject::new(
                            &vk_device,
                            &br::MemoryAllocateInfo::new(
                                vector_stencil_buffer_memreq.size,
                                vk_device
                                    .find_lazily_allocatable_device_local_memory_index(
                                        vector_stencil_buffer_memreq.memoryTypeBits,
                                    )
                                    .expect("no suitable memory"),
                            ),
                        )
                        .expect("vector stencil buffer malloc");
                        vector_stencil_buffer
                            .bind(&vector_stencil_buffer_mem, 0)
                            .expect("vector stencil buffer bind");
                        let vector_stencil_buffer = br::ImageViewBuilder::new(
                            vector_stencil_buffer,
                            br::ImageSubresourceRange::new(br::AspectMask::STENCIL, 0..1, 0..1),
                        )
                        .create()
                        .expect("vector stencil buffer imageview create");
                        let vector_framebuffer = br::FramebufferObject::new(
                            &vk_device,
                            &br::FramebufferCreateInfo::new(
                                &vector_render_pass,
                                &[
                                    vector_stencil_buffer.as_transparent_ref(),
                                    vector_color_ms_buffer.as_transparent_ref(),
                                ],
                                glyph_atlas.size().width,
                                glyph_atlas.size().height,
                            ),
                        )
                        .expect("vector framebuffer create");

                        let mut cp = br::CommandPoolObject::new(
                            &vk_device,
                            &br::CommandPoolCreateInfo::new(vk_device.present_queue_family_index()),
                        )
                        .expect("cp init");
                        let mut cb = br::CommandBufferObject::alloc(
                            &vk_device,
                            &br::CommandBufferAllocateInfo::new(
                                &mut cp,
                                1,
                                br::CommandBufferLevel::Primary,
                            ),
                        )
                        .expect("alloc cb");
                        unsafe {
                            cb[0]
                                .begin(&br::CommandBufferBeginInfo::new())
                                .expect("cb begin")
                        }
                        .copy_buffer(
                            &vector_draw_init_buffer,
                            &vector_draw_buffer,
                            &[br::BufferCopy::mirror(
                                0,
                                vector_draw_buffer_total_size as _,
                            )],
                        )
                        .inject(|r| {
                            vk_device.cmd_pipeline_barrier(
                                r,
                                &br::DependencyInfo::new(
                                    &[br::MemoryBarrier2::new()
                                        .from(
                                            br::PipelineStageFlags2::COPY,
                                            br::AccessFlags2::TRANSFER.write,
                                        )
                                        .to(
                                            br::PipelineStageFlags2::VERTEX_INPUT,
                                            br::AccessFlags2::VERTEX_ATTRIBUTE_READ
                                                | br::AccessFlags2::INDEX_READ,
                                        )],
                                    &[],
                                    &[],
                                ),
                            )
                        })
                        .begin_render_pass(
                            &br::RenderPassBeginInfo::new(
                                &vector_render_pass,
                                &vector_framebuffer,
                                glyph_atlas.size().into_rect(br::Offset2D::ZERO),
                                &[
                                    br::ClearValue::depth_stencil(1.0, 0),
                                    br::ClearValue::color_f32([0.0; 4]),
                                ],
                            ),
                            br::SubpassContents::Inline,
                        )
                        .bind_pipeline(br::PipelineBindPoint::Graphics, &triangle_fans_pipeline)
                        .bind_vertex_buffer_array(
                            0,
                            &[vector_draw_buffer.as_transparent_ref()],
                            &[filltri_points_offset as _],
                        )
                        .bind_index_buffer(
                            &vector_draw_buffer,
                            filltri_indices_offset,
                            br::IndexType::U16,
                        )
                        .draw_indexed(vector_raster_state.fill_tri_indices.len() as _, 1, 0, 0, 0)
                        .bind_pipeline(br::PipelineBindPoint::Graphics, &curve_pipeline)
                        .bind_vertex_buffer_array(
                            0,
                            &[vector_draw_buffer.as_transparent_ref()],
                            &[curve_triangles_offset as _],
                        )
                        .draw(vector_raster_state.curve_tris.len() as _, 1, 0, 0)
                        .next_subpass(br::SubpassContents::Inline)
                        .bind_pipeline(br::PipelineBindPoint::Graphics, &colorize_pipeline)
                        .draw(3, 1, 0, 0)
                        .end_render_pass()
                        .inject(|r| {
                            vk_device.cmd_pipeline_barrier(
                                r,
                                &br::DependencyInfo::new(
                                    &[],
                                    &[],
                                    &[br::ImageMemoryBarrier2::new(
                                        &glyph_atlas.image(),
                                        br::ImageSubresourceRange::new(
                                            br::AspectMask::COLOR,
                                            0..1,
                                            0..1,
                                        ),
                                    )
                                    .from(
                                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                                        br::AccessFlags2::SHADER.read,
                                    )
                                    .to(
                                        br::PipelineStageFlags2::RESOLVE,
                                        br::AccessFlags2::TRANSFER.read,
                                    )
                                    .transferring_layout(
                                        br::ImageLayout::ShaderReadOnlyOpt,
                                        br::ImageLayout::TransferDestOpt,
                                    )],
                                ),
                            )
                        })
                        .resolve_image(
                            vector_color_ms_buffer.image(),
                            br::ImageLayout::TransferSrcOpt,
                            &glyph_atlas.image(),
                            br::ImageLayout::TransferDestOpt,
                            &vector_raster_state
                                .updated_rects
                                .iter()
                                .map(|r| br::vk::VkImageResolve {
                                    srcSubresource: br::ImageSubresourceLayers::new(
                                        br::AspectMask::COLOR,
                                        0,
                                        0..1,
                                    ),
                                    srcOffset: r.offset.with_z(0),
                                    dstSubresource: br::ImageSubresourceLayers::new(
                                        br::AspectMask::COLOR,
                                        0,
                                        0..1,
                                    ),
                                    dstOffset: r.offset.with_z(0),
                                    extent: r.extent.with_depth(1),
                                })
                                .collect::<Vec<_>>(),
                        )
                        .inject(|r| {
                            vk_device.cmd_pipeline_barrier(
                                r,
                                &br::DependencyInfo::new(
                                    &[],
                                    &[],
                                    &[br::ImageMemoryBarrier2::new(
                                        &glyph_atlas.image(),
                                        br::ImageSubresourceRange::new(
                                            br::AspectMask::COLOR,
                                            0..1,
                                            0..1,
                                        ),
                                    )
                                    .from(
                                        br::PipelineStageFlags2::RESOLVE,
                                        br::AccessFlags2::TRANSFER.read,
                                    )
                                    .to(
                                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                                        br::AccessFlags2::SHADER.read,
                                    )
                                    .transferring_layout(
                                        br::ImageLayout::TransferDestOpt,
                                        br::ImageLayout::ShaderReadOnlyOpt,
                                    )],
                                ),
                            )
                        })
                        .end()
                        .expect("cb end");
                        unsafe {
                            render_queue
                                .submit_raw(
                                    &[br::SubmitInfo::new(
                                        &[],
                                        &[],
                                        &[cb[0].as_transparent_ref()],
                                        &[],
                                    )],
                                    None,
                                )
                                .expect("vector render submit");
                        }
                        render_queue.wait().expect("vector render wait");
                    }
                    if composite_render_data != last_composite_render_data {
                        // requires repopulate render commands
                        if !main_cb_invalid {
                            unsafe {
                                render_cp
                                    .reset(br::CommandPoolResetFlags::EMPTY)
                                    .expect("render_cp.reset");
                            }
                            main_cb_invalid = true;
                        }

                        composite_renderer.prepare_input_backdrop_descriptor_sets(
                            &vk_device,
                            composite_render_data.required_backdrop_buffer_count,
                        );

                        last_composite_render_data = composite_render_data;
                    }

                    composite_renderer.update_streaming_data(
                        &vk_device,
                        CompositeStreamingData {
                            current_sec: current_t.as_secs_f32(),
                        },
                    );

                    let needs_update = composite_renderer.update_backdrop_resources(
                        &vk_device,
                        surface_format.format,
                        surface_ext,
                        last_composite_render_data.required_backdrop_buffer_count == 0,
                    );
                    // TODO: いったんめんどうなので毎回更新
                    if true || needs_update {
                        if updating {
                            update_completion_fence
                                .wait()
                                .expect("update_completion_fence.wait");
                            update_completion_fence
                                .reset()
                                .expect("update_completion_fence.reset");
                        }

                        unsafe {
                            update_cp
                                .reset(br::CommandPoolResetFlags::EMPTY)
                                .expect("update_cp.reset");
                        }
                        unsafe {
                            update_cb
                                .begin(&br::CommandBufferBeginInfo::new())
                                .expect("update_cb.begin")
                        }
                        .inject(|r| composite_renderer.sync_buffer(r))
                        .end()
                        .expect("update_cb.end");

                        unsafe {
                            render_queue
                                .submit_raw(
                                    &[br::SubmitInfo::new(
                                        &[],
                                        &[],
                                        &[update_cb.as_transparent_ref()],
                                        &[update_completion_semaphore.as_transparent_ref()],
                                    )],
                                    Some(update_completion_fence.as_transparent_ref_mut()),
                                )
                                .expect("gfx.update.submit");
                        }
                        updating = true;
                    }

                    if main_cb_invalid {
                        for (n, cb) in render_commands.iter_mut().enumerate() {
                            unsafe {
                                cb.begin(&br::CommandBufferBeginInfo::new())
                                    .expect("command buffer begin")
                            }
                            .inject(|r| {
                                composite_renderer.populate_commands(
                                    r,
                                    &vk_device,
                                    &last_composite_render_data,
                                    surface_ext,
                                    br::VkHandleRef::from_raw_ref(&backbuffer_images[n]),
                                    n,
                                    |_, r| r,
                                )
                            })
                            .inject(|r| vk_device.cmd_end_render_pass(r))
                            .end()
                            .expect("command buffer end");
                        }

                        main_cb_invalid = false;
                    }

                    let mut render_wait_semaphores = Vec::with_capacity(1);
                    let mut render_wait_stages = Vec::with_capacity(1);
                    if needs_update {
                        render_wait_semaphores
                            .push(update_completion_semaphore.as_transparent_ref());
                        render_wait_stages.push(br::PipelineStageFlags::VERTEX_INPUT);
                    }

                    unsafe {
                        render_queue
                            .submit_raw(
                                &[br::SubmitInfo::new(
                                    &render_wait_semaphores,
                                    &render_wait_stages,
                                    &[render_commands[backbuffer_index as usize]
                                        .as_transparent_ref()],
                                    &[present_ready_semaphores[backbuffer_index as usize]
                                        .as_transparent_ref()],
                                )],
                                None,
                            )
                            .expect("queue submit")
                    };
                    let mut results = [br::vk::VK_SUCCESS];
                    match render_queue.present(&br::PresentInfo::new(
                        &[
                            present_ready_semaphores[backbuffer_index as usize]
                                .as_transparent_ref(),
                        ],
                        &[vk_swapchain.as_transparent_ref()],
                        &[backbuffer_index],
                        &mut results,
                    )) {
                        Ok(_) => (),
                        Err(e) if e == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                            swapchain_invalidated = true;
                            continue 'lp;
                        }
                        Err(e) => Err::<(), _>(e).expect("queue present"),
                    }

                    // unsafe {
                    //     manual_capture_end();
                    // }
                }

                unsafe {
                    vk_device.wait().expect("device wait");
                    glyph_atlas.drop(&vk_device);
                }
                tracing::info!("RenderThread terminated");
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
                BOOL(0) => break 'app,
                BOOL(-1) => Err::<(), _>(std::io::Error::last_os_error()).expect("GetMessageW"),
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
    // TODO: multi-window support
    pub latest_ui_scale_changes: Option<f32>,
}

#[derive(Clone)]
pub enum Event {
    Quit,
    PointerDown {
        #[cfg(feature = "wayland")]
        root_window: core::ptr::NonNull<wl::XdgSurface>,
        #[cfg(windows)]
        active_window: HWND,
        #[cfg(target_os = "macos")]
        active_window: *mut platform::mac::bridge::WindowLink,
    },
    PointerMove {
        pointer_id: PointerID,
        client_pos: Point<PointerInputUnit>,
    },
    PointerUp,
    WindowResize(Size<PointerInputUnit>),
    WindowRescaleUI {
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
    cursor_shaping: CursorShaping,
    main_window: WindowHandle,
    mut drag_preview_popover: DragPreviewPopoverHandle,
) {
    tracing::info!("app start");

    let mut keyboard_focus_manager = KeyboardFocusManager::new();
    // TODO: マルチウィンドウ対応
    let mut pointer_input_manager = PointerInputManager::new(main_window.client_size());

    let mut ht_manager = HitTestTreeManager::new();
    let mut composite_tree = CompositeTree::new();
    #[cfg(windows)]
    unsafe {
        // WindowsではWM_NCHITTESTの返り値の計算に必要なので一旦生ポインタで参照もたせる（実際どうするかはあとで考える）
        SetWindowLongPtrW(
            main_window.hwnd,
            WINDOW_LONG_PTR_INDEX((core::mem::size_of::<usize>() * 1) as _),
            &pointer_input_manager as *const _ as _,
        );
        SetWindowLongPtrW(
            main_window.hwnd,
            WINDOW_LONG_PTR_INDEX((core::mem::size_of::<usize>() * 2) as _),
            &ht_manager as *const _ as _,
        );
    }

    composite_tree
        .get_mut(CompositeTree::<Event>::ROOT)
        .composite_mode = CompositeMode::FillColor(AnimatableColor::Value([0.1, 0.2, 0.3, 1.0]));
    composite_tree
        .get_mut(CompositeTree::<Event>::ROOT)
        .has_bitmap = true;
    composite_tree.mark_dirty(CompositeTree::<Event>::ROOT);

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
            layout_dirty: true,
            ..Default::default()
        }),
        ..Default::default()
    });
    composite_tree.add_child(CompositeTree::<Event>::ROOT, app_title);
    let ht_caption_bar = ht_manager.create(HitTestTreeData {
        width_adjustment_factor: 1.0,
        height: title_bar_thickness,
        role: Some(crate::hittest::Role::TitleBar),
        ..Default::default()
    });
    ht_manager.add_child(HitTestTreeManager::ROOT, ht_caption_bar);

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
            layout_dirty: true,
            ..Default::default()
        }),
        ..Default::default()
    });
    composite_tree.add_child(CompositeTree::<Event>::ROOT, tab_main);
    let ht_tab_main = ht_manager.create(HitTestTreeData {
        left: 100.0,
        top: 100.0,
        width: 100.0,
        height: 36.0,
        cursor_shape: hittest::CursorShape::Pointer,
        ..Default::default()
    });
    ht_manager.add_child(HitTestTreeManager::ROOT, ht_tab_main);

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
    ht_manager.dump(HitTestTreeManager::ROOT);

    loop {
        match event_queue.next_event().await {
            Event::Quit => break,
            Event::WindowResize(new) => {
                pointer_input_manager.set_client_size(new);
            }
            Event::WindowRescaleUI { new_scale } => {
                composite_tree.get_mut(app_title).base_scale_factor = new_scale;
                composite_tree.mark_dirty_all(app_title);
                composite_tree.get_mut(tab_main).base_scale_factor = new_scale;
                composite_tree.mark_dirty_all(tab_main);

                let mut renderer_sync = renderer_sync.lock().expect("poisoned");
                composite_tree.commit(&mut renderer_sync.composite_buffer);
                renderer_sync.latest_ui_scale_changes = Some(new_scale);
            }
            Event::PointerDown {
                #[cfg(feature = "wayland")]
                root_window,
                #[cfg(windows)]
                active_window,
                #[cfg(target_os = "macos")]
                active_window,
            } => {
                #[cfg(feature = "wayland")]
                {
                    drag_preview_popover.root_window = root_window.as_ptr();
                }
                #[cfg(windows)]
                {
                    drag_preview_popover.base_window_handle = active_window;
                }
                #[cfg(target_os = "macos")]
                {
                    drag_preview_popover.bound_window_link = active_window;
                }

                pointer_input_manager.handle_mouse_left_down(
                    &main_window,
                    &mut ht_manager,
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: &mut drag_preview_popover,
                    },
                    HitTestTreeManager::ROOT,
                    &mut keyboard_focus_manager,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
            Event::PointerMove {
                pointer_id,
                client_pos,
            } => {
                pointer_input_manager.handle_mouse_move(
                    client_pos,
                    &main_window,
                    &mut ht_manager,
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: &mut drag_preview_popover,
                    },
                    HitTestTreeManager::ROOT,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);

                let cursor_shape = pointer_input_manager.cursor_shape(&ht_manager);
                cursor_shaping.set_cursor(&pointer_id, cursor_shape);

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
            Event::PointerUp => {
                pointer_input_manager.handle_mouse_left_up(
                    &main_window,
                    &mut ht_manager,
                    &mut crate::hittest::HitTestEventContext {
                        composite_tree: &mut composite_tree,
                        current_sec: global_time_base.elapsed().as_secs_f32(),
                        drag_preview: &mut drag_preview_popover,
                    },
                    HitTestTreeManager::ROOT,
                );
                composite_tree
                    .commit(&mut renderer_sync.lock().expect("poisoned").composite_buffer);
            }
        }
    }

    tracing::info!("app finish");
    #[cfg(windows)]
    unsafe {
        SetWindowLongPtrW(
            main_window.hwnd,
            WINDOW_LONG_PTR_INDEX((core::mem::size_of::<usize>() * 1) as _),
            0,
        );
        SetWindowLongPtrW(
            main_window.hwnd,
            WINDOW_LONG_PTR_INDEX((core::mem::size_of::<usize>() * 2) as _),
            0,
        );
    }
}

#[derive(Clone, Copy)]
#[cfg(windows)]
pub struct WindowHandle {
    hwnd: windows::Win32::Foundation::HWND,
}
#[cfg(windows)]
impl WindowHandle {
    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        let mut rc = core::mem::MaybeUninit::uninit();
        if let Err(e) = unsafe {
            windows::Win32::UI::WindowsAndMessaging::GetClientRect(self.hwnd, rc.as_mut_ptr())
        } {
            tracing::error!(reason = %e, "get_client_rect");
            return Size::new_logical(0.0, 0.0);
        }

        let rc = unsafe { rc.assume_init_ref() };
        Size::new_pixels(rc.right as _, rc.bottom as _).to_logical(unsafe {
            windows::Win32::UI::HiDpi::GetDpiForWindow(self.hwnd) as f32 / 96.0
        })
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        unsafe { windows::Win32::UI::HiDpi::GetDpiForWindow(self.hwnd) as f32 / 96.0 }
    }
}
#[cfg(windows)]
impl ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {
        unsafe {
            windows::Win32::UI::Input::KeyboardAndMouse::SetCapture(self.hwnd);
        }
    }

    #[inline(always)]
    fn release_pointer(&self) {
        if let Err(e) = unsafe { windows::Win32::UI::Input::KeyboardAndMouse::ReleaseCapture() } {
            tracing::error!(reason = %e, "release_capture");
        }
    }
}

#[derive(Clone, Copy)]
#[cfg(feature = "wayland")]
pub struct WindowHandle {
    wl_surface: *mut wl::Surface,
    wl_surface_to_state: *const Mutex<HashMap<WaylandSurfaceKey, WaylandWindowState>>,
}
#[cfg(feature = "wayland")]
impl WindowHandle {
    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        let wl_surface_to_state = unsafe { (*self.wl_surface_to_state).lock().expect("poisoned") };
        let state = &wl_surface_to_state[&WaylandSurfaceKey(self.wl_surface)];

        state.active_size.to_logical(state.active_buffer_scale)
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        unsafe {
            (*self.wl_surface_to_state).lock().expect("poisoned")
                [&WaylandSurfaceKey(self.wl_surface)]
                .active_buffer_scale
        }
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

#[cfg(windows)]
#[derive(Clone, Copy)]
pub struct PointerID();
#[cfg(windows)]
pub struct CursorShaping {}
#[cfg(windows)]
impl CursorShaping {
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
}

struct Surface<'d> {
    handle: br::vk::VkSurfaceKHR,
    device: &'d VulkanDevice,
}
impl Drop for Surface<'_> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_surface(
                self.device.instance().native_ptr(),
                self.handle,
                None,
            );
        }
    }
}
impl br::VkHandle for Surface<'_> {
    type Handle = br::vk::VkSurfaceKHR;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
unsafe impl Sync for Surface<'_> {}
unsafe impl Send for Surface<'_> {}
impl br::InstanceChild for Surface<'_> {
    type ConcreteInstance = <VulkanDevice as br::InstanceChild>::ConcreteInstance;

    #[inline(always)]
    fn instance(&self) -> &Self::ConcreteInstance {
        self.device.instance()
    }
}
impl br::Surface for Surface<'_> {}

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

struct LocalImageView<'d, Device: br::Device + ?Sized + 'd> {
    handle: br::vk::VkImageView,
    device: &'d Device,
}
impl<Device: br::Device + ?Sized> Drop for LocalImageView<'_, Device> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(self.device.native_ptr(), self.handle, None);
        }
    }
}
impl<Device: br::Device + ?Sized> br::VkHandle for LocalImageView<'_, Device> {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
impl<Device: br::Device + ?Sized> br::ImageView for LocalImageView<'_, Device> {}

#[cfg(windows)]
#[repr(transparent)]
pub struct Win32Window(HWND);
#[cfg(windows)]
unsafe impl Sync for Win32Window {}
#[cfg(windows)]
unsafe impl Send for Win32Window {}
#[cfg(windows)]
impl Win32Window {
    #[inline(always)]
    pub fn pixels_client_size(&self) -> Size<PixelsUnit> {
        let mut rect = core::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(self.0, rect.as_mut_ptr()).expect("GetClientRect");
        }
        let rect = unsafe { rect.assume_init_ref() };
        Size::new_pixels(rect.right as _, rect.bottom as _)
    }

    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        self.pixels_client_size()
            .to_logical(unsafe { windows::Win32::UI::HiDpi::GetDpiForWindow(self.0) as f32 / 96.0 })
    }

    #[inline(always)]
    pub fn show(&self, cmd: SHOW_WINDOW_CMD) {
        let _ = unsafe { ShowWindow(self.0, cmd) };
    }

    #[inline(always)]
    pub unsafe fn set_long_ptr(&mut self, index: WINDOW_LONG_PTR_INDEX, value: isize) -> isize {
        unsafe { SetWindowLongPtrW(self.0, index, value) }
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
        shm_region: platform::linux::TemporalSharedMemory,
        mapped: platform::linux::MappedMemory,
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
    pub root_window: *mut wl::XdgSurface,
    pub buf: DragPreviewPopoverBuffer,
    pub popup: Option<(
        Option<wl::Owned<wl::OrgKdeKwinBlur>>,
        wl::Owned<wl::XdgPopup>,
        wl::Owned<wl::XdgSurface>,
        wl::Owned<wl::WpViewport>,
        wl::Owned<wl::Surface>,
        Box<WaylandPopupState>,
    )>,
}
#[cfg(feature = "wayland")]
impl DragPreviewPopoverHandle {
    pub fn show(&mut self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
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
                .get_popup(Some(&*self.root_window), &positioner)
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

        self.popup = Some((
            blur,
            pp,
            xdg_popup_surface,
            viewport,
            wl_popup_surface,
            popup_state,
        ));
    }

    pub fn r#move(&mut self, p: &Point<PointerInputUnit>) {
        let Some((_, ref pp, _, _, _, _)) = self.popup else {
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

    pub fn hide(&mut self) {
        self.popup = None;
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

#[cfg(windows)]
pub struct DragPreviewPopoverHandle {
    w: HWND,
    base_window_handle: HWND,
    _composition_target: windows::UI::Composition::Desktop::DesktopWindowTarget,
    root_visual: windows::UI::Composition::SpriteVisual,
}
#[cfg(windows)]
impl Drop for DragPreviewPopoverHandle {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { windows::Win32::UI::WindowsAndMessaging::DestroyWindow(self.w) } {
            tracing::error!(reason = %e, "dragPreviewPopover.destroyNative");
        }
    }
}
#[cfg(windows)]
impl DragPreviewPopoverHandle {
    pub fn new(
        hinstance: HINSTANCE,
        native_compositor: &windows::UI::Composition::Compositor,
    ) -> Self {
        let atom_drag_floating = unsafe {
            register_class(&WNDCLASSEXW {
                cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
                style: WNDCLASS_STYLES(0),
                cbClsExtra: 0,
                cbWndExtra: 0,
                lpfnWndProc: Some(Self::wndproc),
                hInstance: hinstance,
                hIcon: HICON(core::ptr::null_mut()),
                hCursor: HCURSOR(core::ptr::null_mut()),
                hbrBackground: HBRUSH(core::ptr::null_mut()),
                lpszMenuName: PCWSTR::null(),
                lpszClassName: w!("DragFloatingWindow"),
                hIconSm: HICON(core::ptr::null_mut()),
            })
            .expect("register_class.drag")
        };
        let w = unsafe {
            use windows::Win32::UI::WindowsAndMessaging::WS_EX_LAYERED;

            CreateWindowExW(
                WS_EX_TRANSPARENT
                    | WS_EX_LAYERED
                    | WS_EX_NOACTIVATE
                    | WS_EX_TOPMOST
                    | WS_EX_NOREDIRECTIONBITMAP,
                PCWSTR(core::ptr::without_provenance(atom_drag_floating as _)),
                w!(""),
                WS_POPUP,
                100,
                100,
                128,
                128,
                None,
                None,
                Some(hinstance),
                None,
            )
            .expect("CreateWindowExW")
        };

        let fx = GaussianBlurEffect::new().expect("drag.fx.create");
        fx.SetSource(
            &CompositionEffectSourceParameter::Create(h!("source"))
                .expect("compositioneffectsourceparameter.create"),
        )
        .expect("drag.fx.set_source");
        fx.SetBlurAmount(16.0).expect("drag.fx.set_blur_amount");
        fx.SetOptimization(EffectOptimization::Speed)
            .expect("drag.fx.set_optimization");
        let effect_factory = native_compositor
            .CreateEffectFactory(&fx)
            .expect("drag.fx.create_factory");
        let backdrop_brush = native_compositor
            .CreateBackdropBrush()
            .expect("drag.backdrop_brush.create");
        let blur_brush = effect_factory.CreateBrush().expect("drag.fx_brush.create");
        blur_brush
            .SetSourceParameter(h!("Source"), &backdrop_brush)
            .expect("drag.fx.set_blur_source");
        let blur_visual = native_compositor
            .CreateSpriteVisual()
            .expect("drag.visual.blur.create");
        blur_visual
            .SetCenterPoint(Vector3::new(0.5, 0.5, 0.5))
            .expect("drag.visual.blur.set_center_point");
        blur_visual
            .SetAnchorPoint(Vector2::new(0.5, 0.5))
            .expect("drag.visual.blur.set_anchor_point");
        blur_visual
            .SetRelativeOffsetAdjustment(Vector3::new(0.5, 0.5, 0.0))
            .expect("drag.visual.blur.set_relative_offset_adjustment");
        blur_visual
            .SetBrush(&blur_brush)
            .expect("drag.visual.blur.set_brush");
        blur_visual
            .SetShadow(&{
                let x = native_compositor
                    .CreateDropShadow()
                    .expect("drag.visual.shadow.create");
                x.SetBlurRadius(32.0)
                    .expect("drag.visual.shadow.set_blur_radius");
                x.SetOffset(Vector3::new(0.0, 16.0, 0.0))
                    .expect("drag.visual.shadow.set_offset");
                x.SetOpacity(0.3).expect("drag.visual.shadow.set_opacity");
                x
            })
            .expect("drag.visual.set_shadow");
        let color_tint_visual = native_compositor
            .CreateSpriteVisual()
            .expect("drag.visual.color_tint.create");
        color_tint_visual
            .SetBrush(
                &native_compositor
                    .CreateColorBrushWithColor(
                        DragPreviewPopoverHandle::BG_COLOR.windows_native_color(),
                    )
                    .expect("drag.visual.color_tint.brush.create"),
            )
            .expect("drag.visual.color_tint.set_brush");
        color_tint_visual
            .SetRelativeOffsetAdjustment(Vector3::zero())
            .expect("drag.visual.color_tint.set_relative_offset_adjustment");
        color_tint_visual
            .SetRelativeSizeAdjustment(Vector2::one())
            .expect("drag.visual.color_tint.set_relative_size_adjustment");
        blur_visual
            .Children()
            .expect("drag.visual.get_children")
            .InsertAtTop(&color_tint_visual)
            .expect("drag.visual.add_child");

        let composition_target = unsafe {
            native_compositor
                .cast::<ICompositorDesktopInterop>()
                .expect("native_compositor.cast.desktop_interop")
                .CreateDesktopWindowTarget(w, true)
                .expect("drag.composition_target.create")
        };
        composition_target
            .SetRoot(&blur_visual)
            .expect("drag.visual.set_root");
        blur_visual
            .SetSize(Vector2::new(128.0 - 32.0, 128.0 - 32.0))
            .expect("drag.visual.set_size");

        Self {
            w,
            base_window_handle: HWND(core::ptr::null_mut()),
            _composition_target: composition_target,
            root_visual: blur_visual,
        }
    }

    pub fn show(&mut self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
        unsafe {
            use windows::Win32::{
                Foundation::POINT,
                Graphics::Gdi::MapWindowPoints,
                UI::{
                    HiDpi::GetDpiForWindow,
                    WindowsAndMessaging::{SWP_NOACTIVATE, SWP_NOZORDER, SetWindowPos},
                },
            };

            // デスクトップ座標で指定になるので置き換え
            let scale = GetDpiForWindow(self.base_window_handle) as f32 / 96.0;
            let pos = pos.to_pixels_round(scale);
            let size = size.to_pixels_ceil(scale);
            let mut p = [POINT { x: pos.x, y: pos.y }];
            MapWindowPoints(Some(self.base_window_handle), None, &mut p);
            let [POINT { x, y }] = p;

            // 影のぶんだけ余分に設定する
            SetWindowPos(
                self.w,
                None,
                x - 32,
                y - 32,
                (size.width + 64) as _,
                (size.height + 64) as _,
                SWP_NOZORDER | SWP_NOACTIVATE,
            )
            .expect("setwindowpos");
            self.root_visual
                .SetSize(Vector2::new(size.width as _, size.height as _))
                .expect("drag.visual.set_size");
            let _ = ShowWindow(self.w, SW_SHOWNOACTIVATE);
        }
    }

    pub fn r#move(&mut self, pos: &Point<PointerInputUnit>) {
        unsafe {
            use windows::Win32::{
                Foundation::POINT,
                Graphics::Gdi::MapWindowPoints,
                UI::{
                    HiDpi::GetDpiForWindow,
                    WindowsAndMessaging::{SWP_NOACTIVATE, SWP_NOSIZE, SWP_NOZORDER, SetWindowPos},
                },
            };

            // デスクトップ座標で指定になるので置き換え
            let scale = GetDpiForWindow(self.base_window_handle) as f32 / 96.0;
            let pos = pos.to_pixels_round(scale);
            let mut p = [POINT { x: pos.x, y: pos.y }];
            MapWindowPoints(Some(self.base_window_handle), None, &mut p);
            let [POINT { x, y }] = p;

            // 影のぶんだけずらして設定する
            SetWindowPos(
                self.w,
                None,
                x - 32,
                y - 32,
                0,
                0,
                SWP_NOZORDER | SWP_NOACTIVATE | SWP_NOSIZE,
            )
            .expect("setwindowpos");
        }
    }

    pub fn hide(&mut self) {
        unsafe {
            let _ = ShowWindow(self.w, SW_HIDE);
        }
    }

    extern "system" fn wndproc(hwnd: HWND, msg: u32, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
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
    pub xdg_surface: *mut wl::XdgSurface,
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
struct WaylandGlobalMessaging<'sys, AppFuture: core::future::Future<Output = ()>> {
    pub surface_states: &'sys Mutex<HashMap<WaylandSurfaceKey, WaylandWindowState>>,
    pub xdg_surface_to_surface: HashMap<*mut wl::XdgSurface, *mut wl::Surface>,
    pub surface_to_xdg_surface: HashMap<*mut wl::Surface, *mut wl::XdgSurface>,
    pub xdg_toplevel_to_surface: HashMap<*mut wl::XdgToplevel, *mut wl::Surface>,
    pub fractional_scale_to_surface: HashMap<*mut wl::WpFractionalScaleV1, *mut wl::Surface>,
    pub text_input_manager: *mut wl::ZwpTextInputManagerV3,
    pub xkb_context: xkbcommon::Context,
    pub keyboard: Option<WaylandKeyboardState>,
    pub pointer: Option<WaylandPointerState>,
    pub cursor_shape_manager: Option<*mut wl::WpCursorShapeManagerV1>,
    pub event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
    pub has_fractional_scale_support: bool,
    _pinned: core::marker::PhantomPinned,
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::XdgWmBaseEventListener
    for WaylandGlobalMessaging<'_, AppFuture>
{
    #[inline(always)]
    fn ping(&mut self, sender: &mut peridot_tp_wayland::XdgWmBase, serial: u32) {
        sender.pong(serial).expect("xdg_wm_base pong");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::SeatEventListener
    for WaylandGlobalMessaging<'_, AppFuture>
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
    for WaylandGlobalMessaging<'_, AppFuture>
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
            xdg_surface: self.surface_to_xdg_surface[&(surface as *mut _)],
            serial,
        });
        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());

        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
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
        let Some(ref mut enter_state) = state.enter_state else {
            return;
        };

        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());
        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
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
                root_window: unsafe { core::ptr::NonNull::new_unchecked(enter_state.xdg_surface) },
            });
        } else if state == wl::PointerButtonState::Released {
            self.event_dispatcher.dispatch(Event::PointerUp);
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
    for WaylandGlobalMessaging<'_, AppFuture>
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

        let mapped = platform::linux::MappedMemory::new(
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
    for WaylandGlobalMessaging<'_, AppFuture>
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
    for WaylandGlobalMessaging<'_, AppFuture>
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
impl<AppFuture: core::future::Future<Output = ()>> wl::SurfaceEventListener
    for WaylandGlobalMessaging<'_, AppFuture>
{
    #[tracing::instrument(skip(self, surface, output))]
    fn enter(&mut self, surface: &mut wl::Surface, output: &mut wl::Output) {}

    #[tracing::instrument(skip(self, surface, output))]
    fn leave(&mut self, surface: &mut wl::Surface, output: &mut wl::Output) {}

    #[tracing::instrument(skip(self, surface))]
    fn preferred_buffer_scale(&mut self, surface: &mut wl::Surface, factor: i32) {
        tracing::trace!(
            has_fractional_scale = self.has_fractional_scale_support,
            "perferred buffer scale"
        );
        if self.has_fractional_scale_support {
            // fractional_scaleがある場合はこっちは処理しなくていい
            return;
        }

        surface
            .set_buffer_scale(factor)
            .expect("wl_surface set_buffer_scale");
        self.set_scale(WaylandSurfaceKey(surface as *mut _), factor as _);
    }

    #[tracing::instrument(skip(self, surface))]
    fn preferred_buffer_transform(&mut self, surface: &mut wl::Surface, transform: u32) {
        tracing::trace!("preferred buffer transform");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::XdgSurfaceEventListener
    for WaylandGlobalMessaging<'_, AppFuture>
{
    #[tracing::instrument(skip(self, sender))]
    fn configure(&mut self, sender: &mut wl::XdgSurface, serial: u32) {
        tracing::trace!("xdg surface configure");

        let mut states_lock = self.surface_states.lock().expect("poisoned");
        let state = states_lock
            .get_mut(&WaylandSurfaceKey(
                self.xdg_surface_to_surface[&(sender as *mut _)],
            ))
            .expect("no surface registered");

        if let Some((w, h)) = state.pending_configure_size.take() {
            self.event_dispatcher
                .dispatch(Event::WindowResize(Size::new_logical(w as _, h as _)));

            let w: u32 = (u32::try_from(w).expect("negative window size") as f32
                * state.active_buffer_scale)
                .ceil() as _;
            let h: u32 = (u32::try_from(h).expect("negative window size") as f32
                * state.active_buffer_scale)
                .ceil() as _;
            if w != state.active_size.width || h != state.active_size.height {
                state.active_size = Size::new_pixels(w, h);
                state
                    .swapchain_externally_invalidation_signal
                    .store(true, std::sync::atomic::Ordering::Relaxed);
            }
        }

        sender
            .ack_configure(serial)
            .expect("xdg_surface.ack_configure");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::XdgToplevelEventListener
    for WaylandGlobalMessaging<'_, AppFuture>
{
    #[tracing::instrument(skip(self, sender))]
    fn close(&mut self, sender: &mut wl::XdgToplevel) {
        tracing::trace!("xdg toplevel close");
        self.surface_states
            .lock()
            .expect("poisoned")
            .get(&WaylandSurfaceKey(
                self.xdg_toplevel_to_surface[&(sender as *mut _)],
            ))
            .expect("no surface registered")
            .terminate_event
            .inc(1)
            .expect("terminate_event.inc");
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

        let mut states_lock = self.surface_states.lock().expect("poisoned");
        let state = states_lock
            .get_mut(&WaylandSurfaceKey(
                self.xdg_toplevel_to_surface[&(sender as *mut _)],
            ))
            .expect("no surface registered");

        state.pending_configure_size = Some((
            if width == 0 {
                state.active_size.width as _
            } else {
                width
            },
            if height == 0 {
                state.active_size.height as _
            } else {
                height
            },
        ));
    }

    fn configure_bounds(&mut self, sender: &mut wl::XdgToplevel, width: i32, height: i32) {}

    fn wm_capabilities(&mut self, sender: &mut wl::XdgToplevel, capabilities: &mut wl::ffi::Array) {
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::ZxdgToplevelDecorationV1EventListener
    for WaylandGlobalMessaging<'_, AppFuture>
{
    fn configure(
        &mut self,
        sender: &mut wl::ZxdgToplevelDecorationV1,
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
    for WaylandGlobalMessaging<'_, AppFuture>
{
    #[tracing::instrument(skip(self, sender))]
    fn preferred_scale(&mut self, sender: &mut wl::WpFractionalScaleV1, scale: u32) {
        tracing::trace!("fractional scale");
        // fractional scaleでは1固定にする必要がある
        unsafe { &*self.fractional_scale_to_surface[&(sender as *mut _)] }
            .set_buffer_scale(1)
            .expect("wl_surface.set_buffer_scale");
        self.set_scale(
            WaylandSurfaceKey(self.fractional_scale_to_surface[&(sender as *mut _)]),
            scale as f32 / 120.0,
        );
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> WaylandGlobalMessaging<'_, AppFuture> {
    fn set_scale(&self, surface: WaylandSurfaceKey, scale: f32) {
        self.surface_states
            .lock()
            .expect("poisoned")
            .get_mut(&surface)
            .expect("surface_states.none")
            .active_buffer_scale = scale;
        self.event_dispatcher
            .dispatch(Event::WindowRescaleUI { new_scale: scale });
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
pub struct WaylandWindow {
    surface: wl::Owned<wl::Surface>,
    xdg_surface: wl::Owned<wl::XdgSurface>,
    xdg_toplevel: wl::Owned<wl::XdgToplevel>,
    deco: Option<wl::Owned<wl::ZxdgToplevelDecorationV1>>,
    fractional_scale: Option<wl::Owned<wl::WpFractionalScaleV1>>,
    _appmenu: Option<wl::Owned<wl::OrgKdeKwinAppmenu>>,
}
#[cfg(feature = "wayland")]
unsafe impl Sync for WaylandWindow {}
#[cfg(feature = "wayland")]
unsafe impl Send for WaylandWindow {}
#[cfg(feature = "wayland")]
impl WaylandWindow {
    fn new(wl_interfaces: &WaylandGlobalInterfaces, dbus: &dbus::Connection) -> Self {
        let surface = wl_interfaces
            .compositor
            .create_surface()
            .expect("wl_surface create");
        let xdg_surface = wl_interfaces
            .xdg_wm_base
            .get_xdg_surface(&surface)
            .expect("xdg_surface create");
        let xdg_toplevel = xdg_surface.get_toplevel().expect("xdg_toplevel create");
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

        let deco = if let Some(ref dm) = wl_interfaces.zxdg_decoration_manager {
            let d = dm
                .get_toplevel_decoration(&xdg_toplevel)
                .expect("decoration.get_toplevel");
            d.set_mode(wl::ZxdgToplevelDecorationV1Mode::ClientSide)
                .expect("decoration.set_mode");

            Some(d)
        } else {
            None
        };

        let fractional_scale = if let Some(ref fs) = wl_interfaces.fractional_scale_manager {
            let f = fs
                .get_fractional_scale(&surface)
                .expect("fractional_scale.create");

            Some(f)
        } else {
            None
        };

        // commits initial state
        surface.commit().expect("wl_surface.commit");

        Self {
            surface,
            xdg_surface,
            xdg_toplevel,
            _appmenu: appmenu,
            deco,
            fractional_scale,
        }
    }

    pub const fn as_key(&self) -> WaylandSurfaceKey {
        WaylandSurfaceKey(self.surface.as_ptr())
    }

    fn bind_global_messaging(
        &mut self,
        mut global_messaging: core::pin::Pin<
            &mut WaylandGlobalMessaging<impl core::future::Future<Output = ()>>,
        >,
        terminate_event: std::sync::Arc<EventFD>,
    ) {
        unsafe { global_messaging.as_mut().get_unchecked_mut() }
            .surface_states
            .lock()
            .expect("poisoned")
            .insert(
                WaylandSurfaceKey(self.surface.as_ptr()),
                WaylandWindowState {
                    pending_configure_size: None,
                    active_buffer_scale: 1.0,
                    active_size: Size::new_pixels(640, 480),
                    swapchain_externally_invalidation_signal: std::sync::Arc::new(
                        std::sync::atomic::AtomicBool::new(false),
                    ),
                    terminate_event,
                },
            );
        unsafe { global_messaging.as_mut().get_unchecked_mut() }
            .xdg_surface_to_surface
            .insert(self.xdg_surface.as_ptr(), self.surface.as_ptr());
        unsafe { global_messaging.as_mut().get_unchecked_mut() }
            .surface_to_xdg_surface
            .insert(self.surface.as_ptr(), self.xdg_surface.as_ptr());
        unsafe { global_messaging.as_mut().get_unchecked_mut() }
            .xdg_toplevel_to_surface
            .insert(self.xdg_toplevel.as_ptr(), self.surface.as_ptr());
        if let Some(ref f) = self.fractional_scale {
            unsafe { global_messaging.as_mut().get_unchecked_mut() }
                .fractional_scale_to_surface
                .insert(f.as_ptr(), self.surface.as_ptr());
        }

        self.surface
            .set_listener(unsafe { global_messaging.as_mut().get_unchecked_mut() })
            .into_result()
            .expect("wl_surface set listener");
        self.xdg_surface
            .set_listener(unsafe { global_messaging.as_mut().get_unchecked_mut() })
            .into_result()
            .expect("xdg_surface set listener");
        self.xdg_toplevel
            .set_listener(unsafe { global_messaging.as_mut().get_unchecked_mut() })
            .into_result()
            .expect("xdg_toplevel set listener");
        if let Some(ref mut x) = self.deco {
            x.set_listener(unsafe { global_messaging.as_mut().get_unchecked_mut() })
                .into_result()
                .expect("zxdg_toplevel_decoration_v1.set_listener");
        }
        if let Some(ref mut x) = self.fractional_scale {
            x.set_listener(unsafe { global_messaging.as_mut().get_unchecked_mut() })
                .into_result()
                .expect("wp_fractional_scale_v1.set_listener");
        }
    }

    fn commit(&self) {
        self.surface.commit().expect("wl_surface.commit");
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

#[cfg(feature = "wayland")]
struct WaylandWindowState {
    pending_configure_size: Option<(i32, i32)>,
    active_buffer_scale: f32,
    active_size: Size<PixelsUnit>,
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    terminate_event: std::sync::Arc<EventFD>,
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

#[cfg(windows)]
#[inline(always)]
unsafe fn register_class(x: &WNDCLASSEXW) -> std::io::Result<u16> {
    match unsafe { RegisterClassExW(x) } {
        r if r == 0 => Err(std::io::Error::last_os_error()),
        r => Ok(r),
    }
}

#[cfg(windows)]
pub struct WindowMessageHandlingContext {
    hwnd: HWND,
}
#[cfg(windows)]
impl crate::input::ShellPointerActions for WindowMessageHandlingContext {
    #[inline(always)]
    fn capture_pointer(&self) {
        unsafe {
            windows::Win32::UI::Input::KeyboardAndMouse::SetCapture(self.hwnd);
        }
    }

    #[inline(always)]
    fn release_pointer(&self) {
        if let Err(e) = unsafe { windows::Win32::UI::Input::KeyboardAndMouse::ReleaseCapture() } {
            tracing::error!(reason = %e, "release_capture");
        }
    }
}

#[cfg(windows)]
struct WindowState<AppFuture: core::future::Future<Output = ()>> {
    content_scale: f32,
    event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
    text_services_mgr: Option<CoreTextServicesManager>,
    edit_context: Option<CoreTextEditContext>,
}
#[cfg(windows)]
impl<AppFuture: core::future::Future<Output = ()>> Drop for WindowState<AppFuture> {
    fn drop(&mut self) {
        unsafe {
            // TODO: detect main window
            PostQuitMessage(0);
        }
    }
}
#[cfg(windows)]
impl<AppFuture: core::future::Future<Output = ()>> WindowState<AppFuture> {
    #[inline(always)]
    fn set_for_window(w: &Win32Window, this: Box<Self>) {
        unsafe {
            SetWindowLongPtrW(
                w.0,
                WINDOW_LONG_PTR_INDEX(0),
                Box::into_raw(this) as *mut _ as _,
            );
        }
    }

    #[inline(always)]
    fn get_for_window<'a>(w: HWND) -> &'a mut Self {
        unsafe {
            &mut *core::ptr::with_exposed_provenance_mut::<Self>(
                GetWindowLongPtrW(w, WINDOW_LONG_PTR_INDEX(0)).cast_unsigned(),
            )
        }
    }

    #[inline(always)]
    fn try_get_for_window<'a>(w: HWND) -> Option<&'a mut Self> {
        unsafe {
            core::ptr::with_exposed_provenance_mut::<Self>(
                GetWindowLongPtrW(w, WINDOW_LONG_PTR_INDEX(0)).cast_unsigned(),
            )
            .as_mut()
        }
    }

    fn compute_client_rect(params: &mut NCCALCSIZE_PARAMS) {
        // remove non-client area
        let w = unsafe {
            use windows::Win32::UI::WindowsAndMessaging::{GetSystemMetrics, SM_CXSIZEFRAME};
            GetSystemMetrics(SM_CXSIZEFRAME)
        };
        let h = unsafe {
            use windows::Win32::UI::WindowsAndMessaging::{GetSystemMetrics, SM_CYSIZEFRAME};
            GetSystemMetrics(SM_CYSIZEFRAME)
        };
        params.rgrc[0].left += w;
        params.rgrc[0].right -= w;
        params.rgrc[0].bottom -= h;
        // topはいじらない（topいじるともとのタイトルバーが一部表示される 他アプリもそんな感じなのでtopは自前で当たり判定組んでリサイズ判定する）
    }

    #[tracing::instrument(skip(self))]
    fn dpi_changed(
        &mut self,
        hwnd: HWND,
        new_scale: f32,
        new_rect: &windows::Win32::Foundation::RECT,
    ) {
        tracing::trace!("dpi changed");

        unsafe {
            use windows::Win32::UI::WindowsAndMessaging::{SWP_NOZORDER, SetWindowPos};

            if let Err(e) = SetWindowPos(
                hwnd,
                None,
                new_rect.left,
                new_rect.top,
                new_rect.right - new_rect.left,
                new_rect.bottom - new_rect.top,
                SWP_NOZORDER,
            ) {
                tracing::error!(reason = %e, "dpi_changed.set_window_pos");
            }
        }

        self.content_scale = new_scale;
        self.event_dispatcher
            .dispatch(Event::WindowRescaleUI { new_scale });
    }

    #[tracing::instrument(skip(self))]
    fn resize(&mut self, new_size: Size<PixelsUnit>) {
        tracing::trace!(?new_size);

        self.event_dispatcher
            .dispatch(Event::WindowResize(new_size.to_logical(self.content_scale)));
    }

    #[tracing::instrument(skip(self))]
    fn mouse_move(&mut self, client_pos: Point<PixelsUnit>) {
        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            client_pos: client_pos.to_logical(self.content_scale),
        });
    }

    #[tracing::instrument(skip(self))]
    fn left_button_down(&mut self, hwnd: HWND, client_pos: Point<PixelsUnit>) {
        // move then down
        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            client_pos: client_pos.to_logical(self.content_scale),
        });
        self.event_dispatcher.dispatch(Event::PointerDown {
            active_window: hwnd,
        });
    }

    #[tracing::instrument(skip(self))]
    fn left_button_up(&mut self) {
        self.event_dispatcher.dispatch(Event::PointerUp);
    }

    fn non_client_hittest(&self, hwnd: HWND, screen_pos: Point<PixelsUnit>) -> Option<u32> {
        use windows::Win32::UI::WindowsAndMessaging::{
            HTCAPTION, HTCLIENT, HTCLOSE, HTMAXBUTTON, HTMINBUTTON,
        };

        let mut p = [screen_pos.to_win32()];
        unsafe {
            use windows::Win32::Graphics::Gdi::MapWindowPoints;
            MapWindowPoints(None, Some(hwnd), &mut p);
        }
        let client_pos = Point::from_win32(p[0]);

        let mut client_size = core::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(hwnd, client_size.as_mut_ptr()).expect("getclientsize");
        }
        let client_size = unsafe { client_size.assume_init() };

        if 0 > client_pos.x
            || client_pos.x > client_size.right
            || 0 > client_pos.y
            || client_pos.y > client_size.bottom
        {
            // ウィンドウ範囲外
            return None;
        }

        let resize_h = unsafe {
            use windows::Win32::UI::WindowsAndMessaging::{GetSystemMetrics, SM_CYSIZEFRAME};
            GetSystemMetrics(SM_CYSIZEFRAME)
        };
        if client_pos.y < resize_h {
            return Some(windows::Win32::UI::WindowsAndMessaging::HTTOP);
        }

        let pointer_input_manager_ptr = unsafe {
            core::ptr::with_exposed_provenance_mut::<PointerInputManager>(GetWindowLongPtrW(
                hwnd,
                WINDOW_LONG_PTR_INDEX((core::mem::size_of::<usize>() * 1) as _),
            ) as _)
        };
        let ht_manager_ptr = unsafe {
            core::ptr::with_exposed_provenance_mut::<HitTestTreeManager>(GetWindowLongPtrW(
                hwnd,
                WINDOW_LONG_PTR_INDEX((core::mem::size_of::<usize>() * 2) as _),
            ) as _)
        };

        if pointer_input_manager_ptr.is_null() {
            // unlinked from logic fiber
            return Some(HTCLIENT);
        }

        let pointer_input_manager = unsafe { &*pointer_input_manager_ptr };
        match pointer_input_manager.role(
            &client_pos.to_logical(self.content_scale),
            &Size::new_pixels(
                (client_size.right - client_size.left) as _,
                (client_size.bottom - client_size.top) as _,
            )
            .to_logical(self.content_scale),
            unsafe { &*ht_manager_ptr },
            HitTestTreeManager::ROOT,
        ) {
            None => Some(HTCLIENT),
            Some(crate::hittest::Role::TitleBar) => Some(HTCAPTION),
            Some(crate::hittest::Role::ForceClient) => Some(HTCLIENT),
            Some(crate::hittest::Role::CloseButton) => Some(HTCLOSE),
            Some(crate::hittest::Role::MaximizeButton) => Some(HTMAXBUTTON),
            Some(crate::hittest::Role::MinimizeButton) => Some(HTMINBUTTON),
            // Windowsだと同じ位置にあるので同じものを返す
            Some(crate::hittest::Role::RestoreButton) => Some(HTMAXBUTTON),
        }
    }

    extern "system" fn handle_messages(
        hwnd: HWND,
        msg: u32,
        wparam: WPARAM,
        lparam: LPARAM,
    ) -> LRESULT {
        use windows::Win32::UI::WindowsAndMessaging::{
            WA_ACTIVE, WA_CLICKACTIVE, WM_ACTIVATE, WM_CHAR, WM_CREATE, WM_DPICHANGED,
            WM_KILLFOCUS, WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_NCCALCSIZE, WM_NCHITTEST,
            WM_SETFOCUS, WM_SIZE,
        };

        if msg == WM_DESTROY {
            unsafe {
                drop(Box::from_raw(Self::get_for_window(hwnd)));
            }

            return LRESULT(0);
        }

        if msg == WM_CREATE {
            unsafe {
                // notify frame change
                use windows::Win32::UI::WindowsAndMessaging::{
                    SWP_FRAMECHANGED, SWP_NOACTIVATE, SWP_NOMOVE, SWP_NOSIZE, SWP_NOZORDER,
                    SetWindowPos,
                };

                SetWindowPos(
                    hwnd,
                    None,
                    0,
                    0,
                    0,
                    0,
                    SWP_NOMOVE | SWP_NOSIZE | SWP_NOZORDER | SWP_FRAMECHANGED | SWP_NOACTIVATE,
                )
                .expect("create.swp.framechange");
            }

            return LRESULT(0);
        }

        if msg == WM_ACTIVATE && (wparam.0 == WA_ACTIVE as _ || wparam.0 == WA_CLICKACTIVE as _) {
            let state = Self::get_for_window(hwnd);

            if state.text_services_mgr.is_none() {
                // first time activation
                use windows::{
                    Foundation::TypedEventHandler,
                    UI::Text::Core::{
                        CoreTextCompositionCompletedEventArgs, CoreTextCompositionStartedEventArgs,
                        CoreTextFormatUpdatingEventArgs, CoreTextLayoutRequestedEventArgs,
                        CoreTextSelectionRequestedEventArgs, CoreTextSelectionUpdatingEventArgs,
                        CoreTextServicesManager, CoreTextTextRequestedEventArgs,
                        CoreTextTextUpdatingEventArgs,
                    },
                };

                let text_services_mgr = CoreTextServicesManager::GetForCurrentView()
                    .expect("coretextservicesmanager.get");
                let edit_context = text_services_mgr
                    .CreateEditContext()
                    .expect("edit_context.create");
                edit_context
                    .LayoutRequested(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextLayoutRequestedEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        let req = e.Request().expect("layout_requested.event_args.request");
                        tracing::trace!(
                            req.is_canceled = ?req.IsCanceled(),
                            req.range = ?req.Range(),
                            "edit_context.layout_requested"
                        );

                        req.LayoutBounds()
                        .expect("layout_requested.event_args.request.layout_bounds")
                        .SetControlBounds(windows::Foundation::Rect {
                            X: 0.0,
                            Y: 0.0,
                            Width: 100.0,
                            Height: 20.0,
                        })
                        .expect(
                            "layout_requested.event_args.request.layout_bounds.set_control_bounds",
                        );
                        req.LayoutBounds()
                            .expect("layout_requested.event_args.request.layout_bounds")
                            .SetTextBounds(windows::Foundation::Rect {
                                X: 0.0,
                                Y: 0.0,
                                Width: 100.0,
                                Height: 20.0,
                            })
                            .expect(
                                "layout_requested.event_args.request.layout_bounds.set_text_bounds",
                            );

                        Ok(())
                    }))
                    .expect("edit_context.layout_requested");
                edit_context
                    .TextRequested(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextTextRequestedEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        let req = e.Request().expect("text_requested.event_args.request");
                        tracing::trace!(
                            req.is_canceled = ?req.IsCanceled(),
                            req.range = ?req.Range(),
                            req.text = ?req.Text(),
                            "edit_context.text_requested"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.text_requested");
                edit_context
                    .TextUpdating(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextTextUpdatingEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        tracing::trace!(
                            input_language = ?e.InputLanguage(),
                            is_canceled = ?e.IsCanceled(),
                            new_selection = ?e.NewSelection(),
                            range = ?e.Range(),
                            text = ?e.Text().map(|x| x.to_string_lossy()),
                            "edit_context.text_updating"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.text_updating");
                edit_context
                    .CompositionStarted(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextCompositionStartedEventArgs,
                    >::new(|sender, e| {
                        tracing::trace!("composition_started");
                        let e = e.ok().expect("event_args.null");
                        tracing::trace!(
                            is_canceled = ?e.IsCanceled(),
                            "edit_context.composition_started"
                        );
                        Ok(())
                    }))
                    .expect("edit_context.composition_started");
                edit_context.CompositionCompleted(&TypedEventHandler::<
                    CoreTextEditContext,
                    CoreTextCompositionCompletedEventArgs,
                >::new(move |sender, e| {
                    let e = e.ok().expect("event_args.null");
                    tracing::trace!(
                        composition_segments = ?e.CompositionSegments(),
                        composition_segments.len = ?e.CompositionSegments().and_then(|x| x.Size()),
                        is_canceled = ?e.IsCanceled(),
                        "edit_context.composition_completed"
                    );

                    for segment in e.CompositionSegments().expect("edit_context.composition_copmleted.composition_segments") {
                        tracing::trace!(
                            preconversion_string = ?segment.PreconversionString().map(|x| x.to_string_lossy()),
                            range = ?segment.Range(),
                            "edit_context.composition_completed.segment"
                        );
                    }

                    Ok(())
                }))
                .expect("edit_context.composition_completed");
                edit_context
                    .FormatUpdating(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextFormatUpdatingEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        tracing::trace!(
                            background_color = ?e.BackgroundColor(),
                            is_canceled = ?e.IsCanceled(),
                            range = ?e.Range(),
                            reason = ?e.Reason(),
                            text_color = ?e.TextColor(),
                            underline_color = ?e.UnderlineColor(),
                            underline_type = ?e.UnderlineType(),
                            "edit_context.format_updating"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.format_updating");
                edit_context
                    .FocusRemoved(&TypedEventHandler::<
                        CoreTextEditContext,
                        windows_core::IInspectable,
                    >::new(|sender, e| {
                        tracing::trace!(e = ?e.ok(), "edit_context.focus_removed");

                        Ok(())
                    }))
                    .expect("edit_context.focus_removed");
                edit_context
                    .NotifyFocusLeaveCompleted(&TypedEventHandler::<
                        CoreTextEditContext,
                        windows_core::IInspectable,
                    >::new(|sender, e| {
                        tracing::trace!(e = ?e.ok(), "edit_context.notify_focus_leave_completed");

                        Ok(())
                    }))
                    .expect("edit_context.notify_focus_leave_completed");
                edit_context
                    .SelectionRequested(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextSelectionRequestedEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        let req = e
                            .Request()
                            .expect("edit_context.selection_requested.event_args.request");
                        tracing::trace!(
                            req.is_canceled = ?req.IsCanceled(),
                            req.selection = ?req.Selection(),
                            "edit_context.selection_requested"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.selection_requested");
                edit_context
                    .SelectionUpdating(&TypedEventHandler::<
                        CoreTextEditContext,
                        CoreTextSelectionUpdatingEventArgs,
                    >::new(|sender, e| {
                        let e = e.ok().expect("event_args.null");
                        tracing::trace!(
                            is_canceled = ?e.IsCanceled(),
                            selection = ?e.Selection(),
                            "edit_context.selection_updating"
                        );

                        Ok(())
                    }))
                    .expect("edit_context.selection_updating");

                state.text_services_mgr = Some(text_services_mgr);
                state.edit_context = Some(edit_context);
            }
        }

        if msg == WM_DPICHANGED {
            Self::get_for_window(hwnd).dpi_changed(
                hwnd,
                (wparam.0 & 0xffff) as u16 as f32 / 96.0,
                unsafe { &*core::ptr::without_provenance(lparam.0.cast_unsigned()) },
            );

            return LRESULT(0);
        }

        if msg == WM_SETFOCUS {
            let state = Self::get_for_window(hwnd);

            state
                .edit_context
                .as_ref()
                .expect("not activated?")
                .NotifyFocusEnter()
                .expect("edit_context.notify_focus_enter");

            return LRESULT(0);
        }

        if msg == WM_KILLFOCUS {
            let state = Self::get_for_window(hwnd);

            state
                .edit_context
                .as_ref()
                .expect("not activated?")
                .NotifyFocusLeave()
                .expect("edit_context.notify_focus_leave");

            return LRESULT(0);
        }

        if msg == WM_CHAR {
            tracing::trace!(keycode = wparam.0, "char input");

            return LRESULT(0);
        }

        if msg == WM_SIZE {
            if let Some(state) = Self::try_get_for_window(hwnd) {
                state.resize(Size::new_pixels(
                    (lparam.0 & 0xffff) as u16 as _,
                    ((lparam.0 >> 16) & 0xffff) as u16 as _,
                ));
            }

            return LRESULT(0);
        }

        if msg == WM_NCCALCSIZE {
            if wparam.0 == 1 {
                Self::compute_client_rect(unsafe {
                    &mut *core::ptr::without_provenance_mut(lparam.0.cast_unsigned())
                });

                return LRESULT(0);
            }
        }

        if msg == WM_NCHITTEST {
            let Some(state) = Self::try_get_for_window(hwnd) else {
                // 初期化完了前にきた
                return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
            };
            let Some(result) = state.non_client_hittest(
                hwnd,
                Point::new_pixels(
                    (lparam.0 & 0xffff) as i16 as _,
                    ((lparam.0 >> 16) & 0xffff) as i16 as _,
                ),
            ) else {
                // よくわからん(アプリウィンドウ範囲外)のでデフォルトに任せる
                return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
            };

            return LRESULT(result as _);
        }

        if msg == WM_LBUTTONDOWN {
            Self::get_for_window(hwnd).left_button_down(
                hwnd,
                Point::new_pixels(
                    (lparam.0 & 0xffff) as i16 as _,
                    ((lparam.0 >> 16) & 0xffff) as i16 as _,
                ),
            );

            return LRESULT(0);
        }

        if msg == WM_MOUSEMOVE {
            Self::get_for_window(hwnd).mouse_move(Point::new_pixels(
                (lparam.0 & 0xffff) as i16 as _,
                ((lparam.0 >> 16) & 0xffff) as i16 as _,
            ));

            return LRESULT(0);
        }

        if msg == WM_LBUTTONUP {
            Self::get_for_window(hwnd).left_button_up();

            return LRESULT(0);
        }

        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
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
