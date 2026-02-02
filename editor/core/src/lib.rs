use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Device, DeviceMemoryMut, Fence, FenceMut,
    ImageChild, InstanceChild, MemoryBound, PhysicalDevice, QueueMut, RenderPass, ShaderModule,
    SurfaceCreateInfo, Swapchain, VkHandle, VkHandleMut, VkObject,
};
use core::pin::Pin;
#[cfg(target_os = "linux")]
use linux_epoll::{Epoll, EpollEventBits};
#[cfg(feature = "wayland")]
use linux_eventfd::{EventFD, EventFDFlags};
#[cfg(target_os = "linux")]
use peridot_tp_dbus::{self as dbus, MessageIterAppendLike};
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
use std::{
    collections::{HashMap, VecDeque},
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
            DirectWrite::{
                DWRITE_FACTORY_TYPE_SHARED, DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_WEIGHT_NORMAL, DWriteCreateFactory, IDWriteFactory, IDWriteTextFormat,
            },
            Gdi::HBRUSH,
        },
        System::{
            LibraryLoader::GetModuleHandleW,
            WinRT::{
                Composition::{ICompositorDesktopInterop, ICompositorInterop},
                CreateDispatcherQueueController, DQTAT_COM_ASTA, DQTYPE_THREAD_CURRENT,
                DispatcherQueueOptions,
            },
        },
        UI::WindowsAndMessaging::{
            CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, DispatchMessageW, GetClientRect,
            GetMessageW, GetWindowLongPtrW, HCURSOR, HICON, IDI_APPLICATION, LoadIconW,
            PostQuitMessage, RegisterClassExW, SHOW_WINDOW_CMD, SW_HIDE, SW_SHOW,
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
use crate::bindgen::Microsoft::Graphics::Canvas::Effects::{
    EffectOptimization, GaussianBlurEffect,
};
use crate::{
    composite::{
        AnimatableColor, AnimatableFloat, BoundCompositeRenderer, CompositeMode, CompositeRect,
        CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
        CompositeRectTextVerticalAlignment, CompositeRenderingData, CompositeStreamingData,
        CompositeTree, CompositeTreeRender, CompositeTreeSyncBuffer, FontID,
        VectorRasterizationState,
    },
    graphics::{VG_COLOR_FORMAT, VG_STENCIL_FORMAT, VulkanDevice},
    hittest::{HitTestTreeData, HitTestTreeManager},
    input::{KeyboardFocusManager, PointerInputManager, ShellPointerActions},
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

static APP_WAKER_VTABLE: core::task::RawWakerVTable = core::task::RawWakerVTable::new(
    |data| core::task::RawWaker::new(data, &APP_WAKER_VTABLE),
    |_| {},
    |_| {},
    |_| {},
);

#[cfg(windows)]
struct WindowsDebugOutputWriter;
#[cfg(windows)]
impl<'a> tracing_subscriber::fmt::MakeWriter<'a> for WindowsDebugOutputWriter {
    type Writer = &'a Self;

    fn make_writer(&'a self) -> Self::Writer {
        self
    }
}
#[cfg(windows)]
impl std::io::Write for &'_ WindowsDebugOutputWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let zero_terminated = unsafe {
            core::str::from_utf8_unchecked(buf)
                .encode_utf16()
                .chain(core::iter::once(0))
                .collect::<Vec<_>>()
        };

        unsafe {
            windows::Win32::System::Diagnostics::Debug::OutputDebugStringW(windows::core::PCWSTR(
                zero_terminated.as_ptr(),
            ));
        }

        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub fn launch() {
    #[cfg(windows)]
    std::panic::set_hook(Box::new(|panic| unsafe {
        let panic_msg = match std::ffi::CString::new(panic.to_string()) {
            Ok(x) => x,
            Err(_) => c"<<Could not convert panic message!>>".into(),
        };

        windows::Win32::System::Diagnostics::Debug::OutputDebugStringA(windows::core::PCSTR(
            panic_msg.as_ptr().cast(),
        ));

        windows::Win32::UI::WindowsAndMessaging::MessageBoxA(
            None,
            windows::core::PCSTR(panic_msg.as_ptr().cast()),
            windows::core::PCSTR(c"Program panic!".as_ptr().cast()),
            windows::Win32::UI::WindowsAndMessaging::MB_OK
                | windows::Win32::UI::WindowsAndMessaging::MB_ICONERROR,
        );
        std::process::abort();
    }));

    #[cfg(all(not(target_os = "macos"), not(windows)))]
    tracing_subscriber::fmt()
        .pretty()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    #[cfg(target_os = "macos")]
    tracing_subscriber::fmt()
        .with_ansi(false)
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();
    #[cfg(windows)]
    tracing_subscriber::fmt()
        .with_ansi(false)
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .with_writer(WindowsDebugOutputWriter)
        .init();

    let mut event_store = core::pin::pin!(None);
    let event_queue = EventQueue {
        event_store: event_store.as_mut().get_mut(),
    };
    let composite_sync_buffer = Mutex::new(CompositeTreeSyncBuffer::new());
    main_wrapper(
        move |composite_tree_sync_buffer, main_window, drag_preview_popover| {
            run(
                event_queue,
                composite_tree_sync_buffer,
                main_window,
                drag_preview_popover,
            )
        },
        event_store,
        &composite_sync_buffer,
    );
}

fn main_wrapper<'sys, AppFuture: core::future::Future<Output = ()> + 'sys>(
    run_app: impl FnOnce(
        &'sys Mutex<CompositeTreeSyncBuffer>,
        WindowHandle,
        DragPreviewPopoverHandle,
    ) -> AppFuture,
    mut event_store: Pin<&mut Option<Event>>,
    composite_sync_buffer: &'sys Mutex<CompositeTreeSyncBuffer>,
) {
    let global_time_base = std::time::Instant::now();
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

    #[cfg(feature = "wayland")]
    let mut surface_to_xdg_surface = HashMap::new();

    #[cfg(feature = "wayland")]
    let mut w = WaylandWindow::new(&wl_interfaces, &dbus, terminate_event.clone());
    #[cfg(feature = "wayland")]
    surface_to_xdg_surface.insert(w.surface.as_ptr(), w.xdg_surface.as_ptr());

    #[cfg(target_os = "macos")]
    let mut w = MacWindow::new();
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

    let mut app = core::pin::pin!(run_app(
        &composite_sync_buffer,
        #[cfg(windows)]
        WindowHandle { hwnd: w.0 },
        #[cfg(feature = "wayland")]
        WindowHandle {
            window_state: w.state.as_ref() as *const _
        },
        drag_preview_popover
    ));
    let _ = app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }));
    #[cfg(feature = "wayland")]
    let mut wl_global_msg = core::pin::pin!(WaylandGlobalMessaging {
        text_input_manager: wl_interfaces.text_input_manager.as_ptr(),
        text_input: None,
        keyboard: None,
        xkb_context: xkbcommon::Context::new(xkbcommon::ContextFlags::NO_FLAGS)
            .expect("xkb_context.create"),
        xkb_keymap: None,
        xkb_state: None,
        pointer: None,
        pointer_pos: (0.0, 0.0),
        pointer_active_surface: None,
        surface_to_xdg_surface,
        event_dispatcher: LogicFiberEventDispatcher {
            event_store: event_store.as_mut().get_mut() as *mut _ as _,
            future: unsafe { app.as_mut().get_unchecked_mut() as *mut _ as _ }
        },
        _pinned: core::marker::PhantomPinned,
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

    #[cfg(feature = "wayland")]
    wl_display.roundtrip().expect("roundtrip");

    #[cfg(windows)]
    unsafe {
        WindowState::set_for_window(
            &w,
            Box::new(WindowState {
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
                    #[cfg(not(target_os = "macos"))]
                    let (cw, ch) = w.client_size();
                    #[cfg(target_os = "macos")]
                    let (cw, ch) = *w.state.active_rt_size.lock().expect("poisoned");

                    br::Extent2D {
                        width: if surface_caps.currentExtent.width == 0xffffffff {
                            cw
                        } else {
                            surface_caps.currentExtent.width
                        },
                        height: if surface_caps.currentExtent.height == 0xffffffff {
                            ch
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

                let dpi = 168;
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
                                                target_texture_width: glyph_atlas
                                                    .space_mgr
                                                    .max
                                                    .width
                                                    as _,
                                                target_texture_height: glyph_atlas
                                                    .space_mgr
                                                    .max
                                                    .height
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
                                        .space_mgr
                                        .max
                                        .into_rect(br::Offset2D::ZERO)
                                        .make_viewport(0.0..1.0)],
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO)],
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
                                                target_texture_width: glyph_atlas
                                                    .space_mgr
                                                    .max
                                                    .width
                                                    as _,
                                                target_texture_height: glyph_atlas
                                                    .space_mgr
                                                    .max
                                                    .height
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
                                        .space_mgr
                                        .max
                                        .into_rect(br::Offset2D::ZERO)
                                        .make_viewport(0.0..1.0)],
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO)],
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
                                        .space_mgr
                                        .max
                                        .into_rect(br::Offset2D::ZERO)
                                        .make_viewport(0.0..1.0)],
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO)],
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
                    if w.state
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
                    if w.state
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
                            #[cfg(not(target_os = "macos"))]
                            let (cw, ch) = w.client_size();
                            #[cfg(target_os = "macos")]
                            let (cw, ch) = *w.state.active_rt_size.lock().expect("poisoned");

                            br::Extent2D {
                                width: if surface_caps.currentExtent.width == 0xffffffff {
                                    cw
                                } else {
                                    surface_caps.currentExtent.width
                                },
                                height: if surface_caps.currentExtent.height == 0xffffffff {
                                    ch
                                } else {
                                    surface_caps.currentExtent.height
                                },
                            }
                        } else {
                            surface_caps.currentExtent
                        };

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
                    composite_sync_buffer
                        .lock()
                        .expect("poisoned")
                        .clean(&mut composite_tree);
                    let composite_render_data = composite_renderer.update(
                        &vk_device,
                        &mut composite_tree,
                        surface_ext,
                        &font_set,
                        &mut glyph_atlas,
                        &mut vector_raster_state,
                        &events,
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
                            &br::ImageCreateInfo::new(glyph_atlas.space_mgr.max, VG_COLOR_FORMAT)
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
                            &br::ImageCreateInfo::new(glyph_atlas.space_mgr.max, VG_STENCIL_FORMAT)
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
                                glyph_atlas.space_mgr.max.width,
                                glyph_atlas.space_mgr.max.height,
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
                                glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO),
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
                    if needs_update {
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
                                && m.interface() == Some(c"com.canonical.dbusmenu")
                                && m.member() == Some(c"GetLayout") =>
                        {
                            let mut args_iter = m.iter();
                            let parent_id = args_iter.try_get_i32().expect("parent:i");
                            args_iter.next();
                            let recursion_depth =
                                args_iter.try_get_i32().expect("recursionDepth:i");
                            args_iter.next();
                            let mut property_names_iter = args_iter
                                .try_begin_iter_array_content()
                                .expect("propertyNames:as");
                            let mut property_names = Vec::new();
                            while property_names_iter.has_next() {
                                property_names.push(
                                    property_names_iter
                                        .try_get_cstr()
                                        .expect("propertyNames[]:s")
                                        .to_owned(),
                                );
                                property_names_iter.next();
                            }

                            tracing::debug!(
                                parent_id,
                                recursion_depth,
                                ?property_names,
                                "com.canonical.dbusmenu.GetLayout"
                            );

                            // toriaezu
                            assert_eq!(recursion_depth, 1);
                            assert!(property_names.is_empty());

                            if parent_id == 1 {
                                let mut reply = dbus::Message::new_method_return(&m)
                                    .expect("dbus.message.new_method_return");
                                let mut reply_iter = reply.iter_append();
                                reply_iter
                                    .append_u32(1)
                                    .expect("dbus.message.append.getlayout.revision");
                                let mut layout_root_struct_iter = reply_iter
                                    .open_struct_container()
                                    .expect("dbus.message.open_struct_container.getlayout.layout");
                                layout_root_struct_iter
                                    .append_i32(0)
                                    .expect("dbus.message.struct.append.getlayout.layout.0");
                                let mut layout_root_property_array_iter = layout_root_struct_iter
                                .open_array_container(c"{sv}")
                                .expect(
                                    "dbus.message.open_array_container.getlayout.layout.properties",
                                );
                                layout_root_property_array_iter
                                    .close()
                                    .expect("dbus.message.array.close.getlayout.layout.properties");
                                let mut layout_children_array_iter =
                                layout_root_struct_iter.open_array_container(c"v").expect(
                                    "dbus.message.open_array_container.getlayout.layout.children",
                                );
                                let mut layout_child_variant_iter = layout_children_array_iter
                                .open_variant_container(c"(ia{sv}av)")
                                .expect(
                                    "dbus.message.open_variant_container.getlayout.layout.child",
                                );
                                let mut layout_child_struct_iter =
                                    layout_child_variant_iter.open_struct_container().expect(
                                        "dbus.message.open_struct_container.getlayout.layout.child",
                                    );
                                layout_child_struct_iter.append_i32(100).expect(
                                    "dbus.message.array.append_i32.getlayout.layout.child.file",
                                );
                                let mut layout_child_properties_iter = layout_child_struct_iter.open_array_container(c"{sv}").expect("dbus.message.open_array_container.getlayout.layout.child.properties");
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"label").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"s").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.append_cstr(c"終了").expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"enabled").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"b").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.append_bool(true).expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"visible").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"b").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.append_bool(true).expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"icon-name").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"s").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.append_cstr(c"window-close").expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"shortcut").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"aas").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                let mut shortcut_array_iter = layout_child_property_value_iter.open_array_container(c"as").expect("dbus.message.open_array_container.getlayout.layout.child.property.value");
                                let mut shortcut_entry_iter = shortcut_array_iter.open_array_container(c"s").expect("dbus.message.open_array_container.getlayout.layout.child.property.value");
                                shortcut_entry_iter.append_cstr(c"Alt").expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                shortcut_entry_iter.append_cstr(c"F4").expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                shortcut_entry_iter.close().expect("dbus.message.array.close.getlayout.layout.child.property.value");
                                shortcut_array_iter.close().expect("dbus.message.array.close.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                layout_child_properties_iter.close().expect(
                                    "dbus.message.array.close.getlayout.layout.child.properties",
                                );
                                let mut layout_child_children_iter =
                                    layout_child_struct_iter.open_array_container(c"v").expect(
                                        "dbus.message.open_array_container.getlayout.layout.child",
                                    );
                                layout_child_children_iter.close().expect(
                                    "dbus.message.array.close.getlayout.layout.child.children",
                                );
                                layout_child_struct_iter
                                    .close()
                                    .expect("dbus.message.struct.close.getlayout.layout.child");
                                layout_child_variant_iter
                                    .close()
                                    .expect("dbus.message.variant.close.getlayout.layout.child");
                                layout_children_array_iter
                                    .close()
                                    .expect("dbus.message.array.close.getlayout.layout.children");
                                layout_root_struct_iter
                                    .close()
                                    .expect("dbus.message.struct.close.getlayout.layout");

                                dbus.send(&mut reply).expect("dbus.send");
                            } else if parent_id == 0 {
                                let mut reply = dbus::Message::new_method_return(&m)
                                    .expect("dbus.message.new_method_return");
                                let mut reply_iter = reply.iter_append();
                                reply_iter
                                    .append_u32(1)
                                    .expect("dbus.message.append.getlayout.revision");
                                let mut layout_root_struct_iter = reply_iter
                                    .open_struct_container()
                                    .expect("dbus.message.open_struct_container.getlayout.layout");
                                layout_root_struct_iter
                                    .append_i32(0)
                                    .expect("dbus.message.struct.append.getlayout.layout.0");
                                let mut layout_root_property_array_iter = layout_root_struct_iter
                                .open_array_container(c"{sv}")
                                .expect(
                                    "dbus.message.open_array_container.getlayout.layout.properties",
                                );
                                let mut layout_root_property_entry_iter = layout_root_property_array_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.properties.element");
                                layout_root_property_entry_iter.append_cstr(c"children-display").expect("dbus.message.dict_entry.append_cstr.getlayout.layout.properties.element");
                                let mut layout_root_property_entry_value_iter = layout_root_property_entry_iter.open_variant_container(c"s").expect("dbus.message.dict_entry.open_variant_container.getlayout.layout.properties.element");
                                layout_root_property_entry_value_iter.append_cstr(c"submenu").expect("dbus.message.variant.append_cstr.getlayout.layout.properties.element");
                                layout_root_property_entry_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.properties.element",
                            );
                                layout_root_property_entry_iter.close().expect(
                                "dbus.message.dict_entry.close.getlayout.layout.properties.element",
                            );
                                layout_root_property_array_iter
                                    .close()
                                    .expect("dbus.message.array.close.getlayout.layout.properties");
                                let mut layout_children_array_iter =
                                layout_root_struct_iter.open_array_container(c"v").expect(
                                    "dbus.message.open_array_container.getlayout.layout.children",
                                );
                                let mut layout_child_variant_iter = layout_children_array_iter
                                .open_variant_container(c"(ia{sv}av)")
                                .expect(
                                    "dbus.message.open_variant_container.getlayout.layout.child",
                                );
                                let mut layout_child_struct_iter =
                                    layout_child_variant_iter.open_struct_container().expect(
                                        "dbus.message.open_struct_container.getlayout.layout.child",
                                    );
                                layout_child_struct_iter.append_i32(1).expect(
                                    "dbus.message.array.append_i32.getlayout.layout.child.file",
                                );
                                let mut layout_child_properties_iter = layout_child_struct_iter.open_array_container(c"{sv}").expect("dbus.message.open_array_container.getlayout.layout.child.properties");
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"label").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"s").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.append_cstr(c"ファイル").expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"enabled").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"b").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.append_bool(true).expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"visible").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"b").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.append_bool(true).expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                let mut layout_child_property_iter = layout_child_properties_iter.open_dict_entry_container().expect("dbus.message.open_dict_entry_container.getlayout.layout.child.property");
                                layout_child_property_iter.append_cstr(c"children-display").expect("dbus.message.array.append_string.getlayout.layout.child.property.label");
                                let mut layout_child_property_value_iter = layout_child_property_iter.open_variant_container(c"s").expect("dbus.message.open_variant_container.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.append_cstr(c"submenu").expect("dbus.message.array.append_string.getlayout.layout.child.property.value");
                                layout_child_property_value_iter.close().expect(
                                "dbus.message.variant.close.getlayout.layout.child.property.value",
                            );
                                layout_child_property_iter.close().expect(
                                    "dbus.message.dict_entry.close.getlayout.layout.child.property",
                                );
                                layout_child_properties_iter.close().expect(
                                    "dbus.message.array.close.getlayout.layout.child.properties",
                                );
                                let mut layout_child_children_iter =
                                    layout_child_struct_iter.open_array_container(c"v").expect(
                                        "dbus.message.open_array_container.getlayout.layout.child",
                                    );
                                layout_child_children_iter.close().expect(
                                    "dbus.message.array.close.getlayout.layout.child.children",
                                );
                                layout_child_struct_iter
                                    .close()
                                    .expect("dbus.message.struct.close.getlayout.layout.child");
                                layout_child_variant_iter
                                    .close()
                                    .expect("dbus.message.variant.close.getlayout.layout.child");
                                layout_children_array_iter
                                    .close()
                                    .expect("dbus.message.array.close.getlayout.layout.children");
                                layout_root_struct_iter
                                    .close()
                                    .expect("dbus.message.struct.close.getlayout.layout");

                                dbus.send(&mut reply).expect("dbus.send");
                            } else {
                                unreachable!("unknown menu id");
                            }
                        }
                        dbus::MessageType::MethodCall
                            if m.path().is_some_and(|x| x == WL_APPMENU_OBJECT_PATH)
                                && m.interface() == Some(c"com.canonical.dbusmenu")
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
                                && m.interface() == Some(c"com.canonical.dbusmenu")
                                && m.member() == Some(c"AboutToShow") =>
                        {
                            let mut args_iter = m.iter();
                            let id = args_iter.try_get_i32().expect("id:i");

                            let mut reply = dbus::Message::new_method_return(&m)
                                .expect("dbus.message.new_method_return");
                            let mut reply_iter = reply.iter_append();
                            reply_iter
                                .append_bool(false)
                                .expect("dbus.message.append_bool");

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
                    TranslateMessage(msg);
                    DispatchMessageW(msg);
                },
            }
        }

        #[cfg(target_os = "macos")]
        unsafe {
            nsapp_run();
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

#[derive(Clone)]
pub enum Event {
    Quit,
    PointerDown {
        #[cfg(feature = "wayland")]
        root_window: core::ptr::NonNull<wl::XdgSurface>,
        #[cfg(windows)]
        active_window: HWND,
        client_x: f32,
        client_y: f32,
    },
    PointerMove {
        #[cfg(windows)]
        active_window: HWND,
        client_x: f32,
        client_y: f32,
    },
    PointerUp,
    WindowResize {
        new_width: u32,
        new_height: u32,
    },
}
#[cfg(any(feature = "wayland", windows))]
unsafe impl Sync for Event {}
#[cfg(any(feature = "wayland", windows))]
unsafe impl Send for Event {}

struct EventQueue {
    event_store: *mut Option<Event>,
}
impl EventQueue {
    pub async fn next_event(&self) -> Event {
        EventQueueNextEventAwaiter { q: self }.await
    }
}

struct LogicFiberEventDispatcher<AppFuture> {
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
    composite_tree_sync_buffer: &'sys Mutex<CompositeTreeSyncBuffer>,
    main_window: WindowHandle,
    mut drag_preview_popover: DragPreviewPopoverHandle,
) {
    tracing::info!("app start");

    // TODO: マルチウィンドウ対応
    let main_client_size = main_window.client_size();
    let mut keyboard_focus_manager = KeyboardFocusManager::new();
    let mut pointer_input_manager =
        PointerInputManager::new(main_client_size.0 as _, main_client_size.1 as _);

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

    composite_tree.get_mut(CompositeTree::ROOT).composite_mode =
        CompositeMode::FillColor(AnimatableColor::Value([0.1, 0.2, 0.3, 1.0]));
    composite_tree.get_mut(CompositeTree::ROOT).has_bitmap = true;
    composite_tree.mark_dirty(CompositeTree::ROOT);

    // app title view
    let app_title = composite_tree.create(CompositeRect {
        has_bitmap: true,
        composite_mode: CompositeMode::FillColor(AnimatableColor::Value([1.0, 1.0, 1.0, 0.125])),
        relative_size_adjustment: [1.0, 0.0],
        size: [
            AnimatableFloat::Value(0.0),
            AnimatableFloat::Value(24.0 * 2.0),
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
    composite_tree.add_child(CompositeTree::ROOT, app_title);
    let ht_caption_bar = ht_manager.create(HitTestTreeData {
        width_adjustment_factor: 1.0,
        height: 24.0 * 2.0,
        role: Some(crate::hittest::Role::TitleBar),
        ..Default::default()
    });
    ht_manager.add_child(HitTestTreeManager::ROOT, ht_caption_bar);

    composite_tree.commit(&mut composite_tree_sync_buffer.lock().expect("poisoned"));
    ht_manager.dump(HitTestTreeManager::ROOT);

    loop {
        match event_queue.next_event().await {
            Event::Quit => break,
            Event::WindowResize {
                new_width,
                new_height,
            } => {
                pointer_input_manager.set_client_size(new_width as _, new_height as _);
            }
            Event::PointerDown {
                #[cfg(feature = "wayland")]
                root_window,
                #[cfg(windows)]
                active_window,
                mut client_x,
                mut client_y,
            } => {
                pointer_input_manager.handle_mouse_left_down(
                    &main_window,
                    &mut ht_manager,
                    &mut crate::hittest::HitTestEventContext {},
                    HitTestTreeManager::ROOT,
                    &mut keyboard_focus_manager,
                );

                // DragPreviewの動作確認用のダミー処理
                main_window.capture_pointer();

                #[cfg(feature = "wayland")]
                {
                    drag_preview_popover.root_window = root_window.as_ptr();
                }
                #[cfg(windows)]
                {
                    // Windowsはグローバル座標を渡す必要があるのでここで変換する
                    let mut p = [windows::Win32::Foundation::POINT {
                        x: client_x as _,
                        y: client_y as _,
                    }];
                    unsafe {
                        windows::Win32::Graphics::Gdi::MapWindowPoints(
                            Some(active_window),
                            None,
                            &mut p,
                        );
                    }
                    client_x = p[0].x as _;
                    client_y = p[0].y as _;
                }
                drag_preview_popover.show(&DesktopRect {
                    left: client_x as _,
                    top: client_y as _,
                    width: 128,
                    height: 128,
                });
            }
            Event::PointerMove {
                #[cfg(windows)]
                active_window,
                mut client_x,
                mut client_y,
            } => {
                pointer_input_manager.handle_mouse_move(
                    client_x as _,
                    client_y as _,
                    &mut ht_manager,
                    &mut crate::hittest::HitTestEventContext {},
                    HitTestTreeManager::ROOT,
                );

                // DragPreviewの動作確認用のダミー処理
                #[cfg(windows)]
                {
                    // Windowsはグローバル座標を渡す必要があるのでここで変換する
                    let mut p = [windows::Win32::Foundation::POINT {
                        x: client_x as _,
                        y: client_y as _,
                    }];
                    unsafe {
                        windows::Win32::Graphics::Gdi::MapWindowPoints(
                            Some(active_window),
                            None,
                            &mut p,
                        );
                    }
                    client_x = p[0].x as _;
                    client_y = p[0].y as _;
                }

                drag_preview_popover.r#move(client_x as _, client_y as _);
            }
            Event::PointerUp => {
                pointer_input_manager.handle_mouse_left_up(
                    &main_window,
                    &mut ht_manager,
                    &mut crate::hittest::HitTestEventContext {},
                    HitTestTreeManager::ROOT,
                );

                // DragPreviewの動作確認用のダミー処理
                drag_preview_popover.hide();
                main_window.release_pointer();
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
    pub fn client_size(&self) -> (u32, u32) {
        let mut rc = core::mem::MaybeUninit::uninit();
        if let Err(e) = unsafe {
            windows::Win32::UI::WindowsAndMessaging::GetClientRect(self.hwnd, rc.as_mut_ptr())
        } {
            tracing::error!(reason = %e, "get_client_rect");
            return (0, 0);
        }

        let rc = unsafe { rc.assume_init_ref() };
        (rc.right as _, rc.bottom as _)
    }
}
#[cfg(windows)]
impl crate::input::ShellPointerActions for WindowHandle {
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
    window_state: *const WaylandWindowState,
}
#[cfg(feature = "wayland")]
impl WindowHandle {
    #[inline(always)]
    pub fn client_size(&self) -> (u32, u32) {
        unsafe { (*self.window_state).active_size }
    }
}
#[cfg(feature = "wayland")]
impl crate::input::ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {
        // Waylandはなし(勝手にキャプチャ状態になってるらしい)
    }

    #[inline(always)]
    fn release_pointer(&self) {
        // Waylandはなし(勝手にキャプチャ状態になってるらしい)
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

#[cfg(feature = "freetype")]
pub struct FreeType(ft::Library);
#[cfg(feature = "freetype")]
impl Drop for FreeType {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { ft::done_freetype(self.0) } {
            tracing::error!(reason = ?e, "FreeType.done");
        }
    }
}
#[cfg(feature = "freetype")]
unsafe impl Sync for FreeType {}
#[cfg(feature = "freetype")]
unsafe impl Send for FreeType {}
#[cfg(feature = "freetype")]
impl FreeType {
    #[inline(always)]
    pub fn init() -> ft::Result<Self> {
        ft::init_freetype().map(Self)
    }
}

pub struct FontSet {
    #[cfg(target_os = "macos")]
    ui_default: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(target_os = "macos")]
    ui_title_project_name: apple_sdk_port::Owned<apple_sdk_port::text::Font>,
    #[cfg(feature = "freetype")]
    ui_default: ft::Face,
    #[cfg(feature = "freetype")]
    ui_title_project_name: ft::Face,
    #[cfg(feature = "harfbuzz")]
    ui_default_shaping: core::ptr::NonNull<peridot_tp_harfbuzz::ffi::hb_font_t>,
    #[cfg(feature = "harfbuzz")]
    ui_title_project_name_shaping: core::ptr::NonNull<peridot_tp_harfbuzz::ffi::hb_font_t>,
    #[cfg(windows)]
    dw_factory: IDWriteFactory,
    #[cfg(windows)]
    ui_default: IDWriteTextFormat,
    #[cfg(windows)]
    ui_title_project_name: IDWriteTextFormat,
}
#[cfg(not(windows))]
impl Drop for FontSet {
    fn drop(&mut self) {
        #[cfg(feature = "harfbuzz")]
        unsafe {
            peridot_tp_harfbuzz::ffi::hb_font_destroy(self.ui_default_shaping.as_ptr());
            peridot_tp_harfbuzz::ffi::hb_font_destroy(self.ui_title_project_name_shaping.as_ptr());
        }
        #[cfg(feature = "freetype")]
        {
            if let Err(e) = unsafe { ft::done_face(self.ui_title_project_name) } {
                tracing::error!(reason = %e, "ui_title_project_name.done_face");
            }
            if let Err(e) = unsafe { ft::done_face(self.ui_default) } {
                tracing::error!(reason = %e, "ui_default.done_face");
            }
        }
    }
}
impl FontSet {
    #[cfg(windows)]
    pub fn new(dw: IDWriteFactory) -> Self {
        use windows::Win32::Globalization::GetUserDefaultLocaleName;

        let mut locale_name = [const { core::mem::MaybeUninit::uninit() }; 32];
        let len = unsafe {
            GetUserDefaultLocaleName(core::mem::transmute::<
                &mut [core::mem::MaybeUninit<u16>; 32],
                &mut [u16; 32],
            >(&mut locale_name))
        };
        let locale_name = if len == 0 {
            // fallback to en_US
            let e = std::io::Error::last_os_error();
            tracing::warn!(reason = ?e, "GetUserDefaultLocaleName.fallback");

            &[b'e' as u16, b'n' as _, b'_' as _, b'U' as _, b'S' as _, 0]
        } else {
            unsafe {
                core::mem::transmute::<&[core::mem::MaybeUninit<u16>], &[u16]>(
                    &locale_name[..len as usize],
                )
            }
        };

        let ui_default = unsafe {
            dw.CreateTextFormat(
                w!("Inter Display"),
                None,
                DWRITE_FONT_WEIGHT_NORMAL,
                DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_STRETCH_NORMAL,
                12.0,
                PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_default")
        };
        let ui_title_project_name = unsafe {
            dw.CreateTextFormat(
                w!("Inter Display"),
                None,
                DWRITE_FONT_WEIGHT_NORMAL,
                DWRITE_FONT_STYLE_NORMAL,
                DWRITE_FONT_STRETCH_NORMAL,
                10.0,
                PCWSTR(locale_name.as_ptr()),
            )
            .expect("dwrite.textformat.create.ui_title_project_name")
        };

        Self {
            dw_factory: dw,
            ui_default,
            ui_title_project_name,
        }
    }

    #[cfg(feature = "freetype")]
    pub fn new(lib: &FreeType, dpi: u32) -> Self {
        use peridot_tp_freetype::FractionalExt;

        #[cfg(feature = "fontconfig")]
        let (font_file_path, face_index) = unsafe {
            fc::init().expect("FontConfig.init");
            let mut pat = fc::Pattern::new().expect("FcPattern.create");
            pat.as_mut()
                .add(fc::Pattern::KEY_FAMILY, c"Inter Display")
                .expect("FcPattern.add.family");
            pat.as_mut()
                .add(fc::Pattern::KEY_WEIGHT, &fc::raw::FC_WEIGHT_REGULAR)
                .expect("FcPattern.add.weight");
            pat.as_mut()
                .add(fc::Pattern::KEY_SIZE, &(12.0 as core::ffi::c_double))
                .expect("FcPattern.add.size");
            fc::Config::current()
                .unwrap_unchecked()
                .as_mut()
                .substitute(pat.as_mut(), fc::MatchKind::Pattern)
                .expect("FcConfig.substitute");
            pat.as_mut().default_substitute();
            let fonts = fc::sort(
                fc::Config::current().unwrap_unchecked().as_mut(),
                pat.as_mut(),
                false,
                None,
            )
            .expect("FontConfig.sort");
            // for n in 0..(*fonts).nfont {
            //     let f = *(*fonts).fonts.add(n as usize);
            //     fontconfig::FcPatternPrint(f);
            // }

            let mut font = fonts.as_ref().fonts_slice()[0];
            let file: &core::ffi::CStr = font
                .as_mut()
                .get(fc::Pattern::KEY_FILE)
                .expect("FcPattern.get.file")
                .expect("FcPattern.get.not_exist.file");
            let file = file.to_owned();
            let index: core::ffi::c_int = font
                .as_mut()
                .get(fc::Pattern::KEY_INDEX)
                .expect("FcPattern.get.index")
                .expect("FcPattern.get.not_exist.index");

            (file, index)
        };

        let ui_default = unsafe {
            ft::new_face(lib.0, &font_file_path, face_index as _)
                .expect("FreeType.new_face.ui_default")
        };
        unsafe {
            ft::set_char_size(ui_default, 0, 12.0f32.to_f26dot6_lossy(), 0, dpi)
                .expect("FreeType.set_char_size.ui_default")
        }
        let ui_title_project_name = unsafe {
            ft::new_face(lib.0, &font_file_path, face_index as _)
                .expect("FreeType.Face.new.ui_title_project_name")
        };
        unsafe {
            ft::set_char_size(ui_title_project_name, 0, 10.0f32.to_f26dot6_lossy(), 0, dpi)
                .expect("FreeType.set_char_size.ui_title_project_name")
        }

        #[cfg(feature = "harfbuzz")]
        let ui_default_shaping = core::ptr::NonNull::new(unsafe {
            peridot_tp_harfbuzz::ffi::hb_ft_font_create_referenced(ui_default)
        })
        .expect("hb_ft_font_create_referenced.ui_default");
        #[cfg(feature = "harfbuzz")]
        let ui_title_project_name_shaping = core::ptr::NonNull::new(unsafe {
            peridot_tp_harfbuzz::ffi::hb_ft_font_create_referenced(ui_title_project_name)
        })
        .expect("hb_ft_font_create_referenced.ui_title_project_name");

        Self {
            ui_default,
            ui_title_project_name,
            #[cfg(feature = "harfbuzz")]
            ui_default_shaping,
            #[cfg(feature = "harfbuzz")]
            ui_title_project_name_shaping,
        }
    }

    #[cfg(target_os = "macos")]
    pub fn new() -> Self {
        let ui_default = apple_sdk_port::text::Font::new_ui(
            apple_sdk_port::text::UIFontType::System,
            12.0,
            None,
        );
        let ui_title_project_name = apple_sdk_port::text::Font::new_ui(
            apple_sdk_port::text::UIFontType::System,
            10.0,
            None,
        );

        Self {
            ui_default,
            ui_title_project_name,
        }
    }

    #[cfg(target_os = "macos")]
    #[inline]
    pub fn select(&self, category: FontID) -> &apple_sdk_port::text::Font {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
        }
    }

    #[cfg(feature = "freetype")]
    #[inline]
    pub fn select(&self, category: FontID) -> ft::Face {
        match category {
            FontID::UIDefault => self.ui_default,
            FontID::UITitleProjectName => self.ui_title_project_name,
        }
    }

    #[cfg(feature = "harfbuzz")]
    #[inline]
    pub fn select_shaping(&self, category: FontID) -> *mut peridot_tp_harfbuzz::ffi::hb_font_t {
        match category {
            FontID::UIDefault => self.ui_default_shaping.as_ptr(),
            FontID::UITitleProjectName => self.ui_title_project_name_shaping.as_ptr(),
        }
    }

    #[cfg(windows)]
    #[inline(always)]
    pub const fn native_factory(&self) -> &IDWriteFactory {
        &self.dw_factory
    }

    #[cfg(windows)]
    #[inline]
    pub fn select(&self, category: FontID) -> &IDWriteTextFormat {
        match category {
            FontID::UIDefault => &self.ui_default,
            FontID::UITitleProjectName => &self.ui_title_project_name,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GlyphRect {
    pub left: u32,
    pub top: u32,
    pub width: u32,
    pub height: u32,
}

struct Skyline {
    pub y: u32,
    pub width: u32,
}

struct GlyphAtlasSpaceManager {
    // skyline method
    max: br::Extent2D,
    skylines: Vec<Skyline>,
}
impl GlyphAtlasSpaceManager {
    const SPACING: u32 = 1;

    pub fn new(max: br::Extent2D) -> Self {
        Self {
            skylines: vec![Skyline {
                y: 0,
                width: max.width,
            }],
            max,
        }
    }

    pub fn acquire(&mut self, width: u32, height: u32) -> Option<GlyphRect> {
        let cons_width = width + Self::SPACING;
        let cons_height = height + Self::SPACING;

        let mut fit_left_top = None;
        let mut left = 0;
        let mut n = 0;
        while n < self.skylines.len() && left + cons_width <= self.max.width {
            let skyline = &self.skylines[n];
            let skyline_height = self.max.height - skyline.y;
            if skyline_height >= cons_height && fit_left_top.is_none_or(|(_, t, _)| skyline.y < t) {
                let mut y = skyline.y;

                // potentially overlapping skylines at right
                let mut l1 = left + skyline.width;
                let mut m = n + 1;
                while m < self.skylines.len() && l1 <= left + cons_width {
                    let skyline2 = &self.skylines[m];

                    y = y.max(skyline2.y);
                    l1 += skyline2.width;
                    m += 1;
                }

                // recompute whether it fits
                let skyline_height = self.max.height - y;
                if skyline_height >= cons_height && fit_left_top.is_none_or(|(_, t, _)| y < t) {
                    fit_left_top = Some((left, y, n));
                }
            }

            left += skyline.width;
            n += 1;
        }

        let Some((left, top, left_skyline_point)) = fit_left_top else {
            // no available rects
            return None;
        };

        // update skyline
        let mut left_w = cons_width;
        let mut skyline_point_index = left_skyline_point;
        while left_w > 0 {
            let skyline = &self.skylines[skyline_point_index];

            if skyline.width > left_w {
                // needs splitting(and finishes at this step)
                if skyline_point_index > 0
                    && self.skylines[skyline_point_index - 1].y == top + cons_height
                {
                    // fuse with previous
                    self.skylines[skyline_point_index - 1].width += left_w;
                    self.skylines[skyline_point_index].width -= left_w;
                } else {
                    let org_skyline_y = skyline.y;
                    let right_skyline_width = skyline.width - left_w;
                    self.skylines[skyline_point_index] = Skyline {
                        y: top + cons_height,
                        width: left_w,
                    };
                    self.skylines.insert(
                        skyline_point_index + 1,
                        Skyline {
                            y: org_skyline_y,
                            width: right_skyline_width,
                        },
                    );
                }

                break;
            }

            let sw = skyline.width;
            if skyline_point_index > 0
                && self.skylines[skyline_point_index - 1].y == top + cons_height
            {
                // fuse with previous
                self.skylines[skyline_point_index - 1].width += sw;
                self.skylines.remove(skyline_point_index);
                skyline_point_index -= 1;
            } else {
                // just move this skyline
                self.skylines[left_skyline_point].y = top + cons_height;
            }

            left_w -= sw.min(left_w);
            skyline_point_index += 1;
        }

        Some(GlyphRect {
            left,
            top,
            width,
            height,
        })
    }
}

struct GlyphAtlas {
    res: br::vk::VkImage,
    mem: br::vk::VkDeviceMemory,
    view: br::vk::VkImageView,
    acquired_rects: HashMap<(usize, u16), GlyphRect>,
    space_mgr: GlyphAtlasSpaceManager,
}
impl GlyphAtlas {
    const MULTISAMPLE_LEVEL: u32 = 4;

    pub unsafe fn drop(&mut self, gfx: &VulkanDevice) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(gfx.native_ptr(), self.view, None);
            br::vkfn_wrapper::destroy_image(gfx.native_ptr(), self.res, None);
            br::vkfn_wrapper::free_memory(gfx.native_ptr(), self.mem, None);
        }
    }

    pub fn new(gfx: &VulkanDevice) -> Self {
        let size = br::Extent2D::spread1(4096);

        let mut res = br::ImageObject::new(
            gfx,
            &br::ImageCreateInfo::new(size, br::vk::VK_FORMAT_R8_UNORM).set_usage(
                br::ImageUsageFlags::SAMPLED
                    | br::ImageUsageFlags::COLOR_ATTACHMENT
                    | br::ImageUsageFlags::TRANSFER_DEST,
            ),
        )
        .expect("res create");
        let memory_requirements = res.requirements();
        let mem = br::DeviceMemoryObject::new(
            gfx,
            &br::MemoryAllocateInfo::new(
                memory_requirements.size,
                gfx.find_device_local_memory_index(memory_requirements.memoryTypeBits)
                    .expect("no suitable memory"),
            ),
        )
        .expect("res malloc");
        res.bind(&mem, 0).expect("res mem bind");
        let view = br::ImageViewBuilder::new(
            res,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("res view create");

        view.image()
            .set_name(Some(c"Glyph Atlas"))
            .expect("res set name");
        mem.set_name(Some(c"Glyph Atlas [Backing]"))
            .expect("mem set name");
        view.set_name(Some(c"Glyph Atlas [View]"))
            .expect("view set name");

        let (view, res) = view.unmanage();
        let (res, _, _, _, _) = res.unmanage();
        let (mem, _) = mem.unmanage();
        Self {
            res,
            mem,
            view,
            acquired_rects: HashMap::new(),
            space_mgr: GlyphAtlasSpaceManager::new(size),
        }
    }

    pub fn acquire(&mut self, key: (usize, u16), width: u32, height: u32) -> (GlyphRect, bool) {
        match self.acquired_rects.entry(key) {
            std::collections::hash_map::Entry::Vacant(x) => (
                x.insert(
                    self.space_mgr
                        .acquire(width, height)
                        .expect("no space left"),
                )
                .clone(),
                true,
            ),
            std::collections::hash_map::Entry::Occupied(x) => (x.get().clone(), false),
        }
    }

    #[inline(always)]
    pub const fn image<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImage> {
        unsafe { br::VkHandleRef::dangling(self.res) }
    }

    #[inline(always)]
    pub const fn image_range_entire(&self) -> br::ImageSubresourceRange {
        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1)
    }

    #[inline(always)]
    pub const fn view<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.view) }
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
    pub fn client_size(&self) -> (u32, u32) {
        let mut rect = core::mem::MaybeUninit::uninit();
        unsafe {
            GetClientRect(self.0, rect.as_mut_ptr()).expect("GetClientRect");
        }
        let rect = unsafe { rect.assume_init_ref() };
        (rect.right as _, rect.bottom as _)
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

pub struct DesktopRect {
    pub left: i32,
    pub top: i32,
    pub width: u32,
    pub height: u32,
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
        tracing::info!(target: "wl::diag", name, ?interface, version, "wl interface");

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
    pub fn show(&mut self, rect: &DesktopRect) {
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

        let pos = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .create_positioner()
                .expect("pos.create")
        };
        pos.set_size(rect.width as _, rect.height as _)
            .expect("pos.set_size");
        pos.set_offset(rect.left as _, rect.top as _)
            .expect("pos.set_offset");
        pos.set_anchor(wl::XdgPositionerAnchor::TopLeft)
            .expect("pos.set_anchor");
        pos.set_anchor_rect(0, 0, 1, 1)
            .expect("pos.set_anchor_rect");
        pos.set_gravity(wl::XdgPositionerGravity::BottomRight)
            .expect("pos.set_gravity");
        pos.set_constraint_adjustment(wl::XdgPositionerConstraintAdjustment::None)
            .expect("pos.set_constraint_adjustment");
        let mut pp = unsafe {
            xdg_popup_surface
                .get_popup(Some(&*self.root_window), &pos)
                .expect("pop.create")
        };
        let mut popup_state = Box::new(WaylandPopupState {});
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
            .set_destination(rect.width as _, rect.height as _)
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

    pub fn r#move(&mut self, x: i32, y: i32) {
        let Some((_, ref pp, _, _, _, _)) = self.popup else {
            return;
        };

        let pos = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .create_positioner()
                .expect("pos.create")
        };
        pos.set_offset(x as _, y as _).expect("pos.set_offset");
        pp.reposition(&pos, 0).expect("pp.reposition");
    }

    pub fn hide(&mut self) {
        self.popup = None;
    }
}

#[cfg(target_os = "macos")]
pub struct DragPreviewPopoverHandle;
#[cfg(target_os = "macos")]
impl DragPreviewPopoverHandle {
    pub fn show(&mut self, rect: &DesktopRect) {
        unsafe {
            ni_show_drag_preview();
            // macはleft,bottomが0,0なのでその分を考慮して計算してleft,topを合わせる
            ni_move_drag_preview(
                rect.left as _,
                (rect.top - rect.height as i32) as _,
                rect.width as _,
                rect.height as _,
            );
        }
    }

    pub fn hide(&mut self) {
        unsafe {
            ni_hide_drag_preview();
        }
    }
}

#[cfg(windows)]
pub struct DragPreviewPopoverHandle {
    w: HWND,
    _composition_target: windows::UI::Composition::Desktop::DesktopWindowTarget,
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
            _composition_target: composition_target,
        }
    }

    pub fn show(&mut self, rect: &DesktopRect) {
        unsafe {
            use windows::Win32::UI::WindowsAndMessaging::{
                SWP_NOACTIVATE, SWP_NOZORDER, SetWindowPos,
            };

            // 影のぶんだけ余分に設定する
            SetWindowPos(
                self.w,
                None,
                rect.left - 32,
                rect.top - 32,
                (rect.width + 32) as _,
                (rect.height + 32) as _,
                SWP_NOZORDER | SWP_NOACTIVATE,
            )
            .expect("setwindowpos");
            let _ = ShowWindow(self.w, SW_SHOWNOACTIVATE);
        }
    }

    pub fn r#move(&mut self, x: i32, y: i32) {
        unsafe {
            use windows::Win32::UI::WindowsAndMessaging::{
                SWP_NOACTIVATE, SWP_NOSIZE, SWP_NOZORDER, SetWindowPos,
            };

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

pub struct Color32 {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}
impl Color32 {
    pub const fn premultiplied(&self) -> Self {
        Self {
            r: (self.r as f32 * self.a as f32 / 255.0).round() as u8,
            g: (self.g as f32 * self.a as f32 / 255.0).round() as u8,
            b: (self.b as f32 * self.a as f32 / 255.0).round() as u8,
            a: self.a,
        }
    }

    pub const fn argb8888(&self) -> u32 {
        ((self.a as u32) << 24) | ((self.r as u32) << 16) | ((self.g as u32) << 8) | (self.b as u32)
    }

    pub const fn r_u32(&self) -> u32 {
        (0xffffffffu32 as f32 * (self.r as f32 / 255.0).min(1.0)) as u32
    }

    pub const fn g_u32(&self) -> u32 {
        (0xffffffffu32 as f32 * (self.g as f32 / 255.0).min(1.0)) as u32
    }

    pub const fn b_u32(&self) -> u32 {
        (0xffffffffu32 as f32 * (self.b as f32 / 255.0).min(1.0)) as u32
    }

    pub const fn a_u32(&self) -> u32 {
        (0xffffffffu32 as f32 * (self.a as f32 / 255.0).min(1.0)) as u32
    }

    #[cfg(windows)]
    pub const fn windows_native_color(&self) -> windows::UI::Color {
        windows::UI::Color {
            A: self.a,
            R: self.r,
            G: self.g,
            B: self.b,
        }
    }
}

#[cfg(feature = "wayland")]
const WL_APPMENU_OBJECT_PATH: &core::ffi::CStr = c"/AppMenu";
#[cfg(feature = "wayland")]
struct WaylandGlobalMessaging<AppFuture: core::future::Future<Output = ()>> {
    pub text_input_manager: *mut wl::ZwpTextInputManagerV3,
    pub text_input: Option<wl::Owned<wl::ZwpTextInputV3>>,
    pub keyboard: Option<wl::Owned<wl::Keyboard>>,
    pub xkb_context: xkbcommon::Context,
    pub xkb_keymap: Option<xkbcommon::Keymap>,
    pub xkb_state: Option<xkbcommon::State>,
    pub pointer: Option<wl::Owned<wl::Pointer>>,
    pub pointer_pos: (f32, f32),
    pub pointer_active_surface: Option<core::ptr::NonNull<wl::XdgSurface>>,
    pub surface_to_xdg_surface: HashMap<*mut wl::Surface, *mut wl::XdgSurface>,
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

            self.pointer = Some(p);
        } else {
            // no pointer
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

            self.keyboard = Some(k);
            self.text_input = Some(ti);
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
        _pointer: &mut peridot_tp_wayland::Pointer,
        serial: u32,
        surface: &mut peridot_tp_wayland::Surface,
        surface_x: peridot_tp_wayland::Fixed,
        surface_y: peridot_tp_wayland::Fixed,
    ) {
        self.pointer_active_surface = Some(unsafe {
            core::ptr::NonNull::new_unchecked(self.surface_to_xdg_surface[&(surface as *mut _)])
        });
        self.pointer_pos = (surface_x.to_f32(), surface_y.to_f32());
    }

    #[tracing::instrument(skip(self, _pointer, _surface))]
    fn leave(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        serial: u32,
        _surface: &mut peridot_tp_wayland::Surface,
    ) {
        self.pointer_active_surface = None;
    }

    #[tracing::instrument(skip(self, _pointer), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn motion(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        time: u32,
        surface_x: peridot_tp_wayland::Fixed,
        surface_y: peridot_tp_wayland::Fixed,
    ) {
        self.pointer_pos = (surface_x.to_f32(), surface_y.to_f32());
        self.event_dispatcher.dispatch(Event::PointerMove {
            client_x: self.pointer_pos.0,
            client_y: self.pointer_pos.1,
        });
    }

    #[tracing::instrument(skip(self, _pointer), fields(state = state as u32))]
    fn button(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        serial: u32,
        time: u32,
        button: u32,
        state: peridot_tp_wayland::PointerButtonState,
    ) {
        let Some(pointer_active_surface) = self.pointer_active_surface else {
            return;
        };

        if state == wl::PointerButtonState::Pressed {
            self.event_dispatcher.dispatch(Event::PointerDown {
                root_window: pointer_active_surface,
                client_x: self.pointer_pos.0,
                client_y: self.pointer_pos.1,
            });
        } else if state == wl::PointerButtonState::Released {
            self.event_dispatcher.dispatch(Event::PointerUp);
        }
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        time: u32,
        axis: u32,
        value: peridot_tp_wayland::Fixed,
    ) {
        tracing::trace!("pointer.axis");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn frame(&mut self, _pointer: &mut peridot_tp_wayland::Pointer) {
        // tracing::trace!("pointer.frame");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_source(&mut self, _pointer: &mut peridot_tp_wayland::Pointer, axis_source: u32) {
        tracing::trace!("pointer.axis_source");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_stop(&mut self, _pointer: &mut peridot_tp_wayland::Pointer, time: u32, axis: u32) {
        tracing::trace!("pointer.axis_stop");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_discrete(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        axis: u32,
        discrete: i32,
    ) {
        tracing::trace!("pointer.axis_discrete");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_value120(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        axis: u32,
        value120: i32,
    ) {
        tracing::trace!("pointer.axis_value120");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_relative_direction(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        axis: u32,
        direction: u32,
    ) {
        tracing::trace!("pointer.axis_relative_direction");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::KeyboardEventListener
    for WaylandGlobalMessaging<AppFuture>
{
    #[tracing::instrument(skip(self, sender))]
    fn keymap(
        &mut self,
        sender: &mut peridot_tp_wayland::Keyboard,
        format: peridot_tp_wayland::KeyboardKeymapFormat,
        fd: i32,
        size: u32,
    ) {
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
        let state = xkbcommon::State::new(&keymap).expect("xkb_state.create");

        tracing::trace!("keyboard::keymap\n{content}");
        self.xkb_keymap = Some(keymap);
        self.xkb_state = Some(state);
    }

    #[tracing::instrument(skip(self, sender, surface))]
    fn enter(
        &mut self,
        sender: &mut peridot_tp_wayland::Keyboard,
        serial: u32,
        surface: &mut peridot_tp_wayland::Surface,
        keys: &[u32],
    ) {
        tracing::trace!("keyboard::enter");
    }

    #[tracing::instrument(skip(self, sender, surface))]
    fn leave(
        &mut self,
        sender: &mut peridot_tp_wayland::Keyboard,
        serial: u32,
        surface: &mut peridot_tp_wayland::Surface,
    ) {
        tracing::trace!("keyboard::leave");
    }

    #[tracing::instrument(skip(self, sender))]
    fn key(
        &mut self,
        sender: &mut peridot_tp_wayland::Keyboard,
        serial: u32,
        time: u32,
        key: u32,
        state: peridot_tp_wayland::KeyboardKeyState,
    ) {
        tracing::trace!("keyboard::key");

        if let Some(ref mut x) = self.xkb_state {
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

    #[tracing::instrument(skip(self, sender))]
    fn modifiers(
        &mut self,
        sender: &mut peridot_tp_wayland::Keyboard,
        serial: u32,
        mods_depressed: u32,
        mods_latched: u32,
        mods_locked: u32,
        group: u32,
    ) {
        tracing::trace!("keyboard::modifiers");

        if let Some(ref mut x) = self.xkb_state {
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

    #[tracing::instrument(skip(self, sender))]
    fn repeat_info(&mut self, sender: &mut peridot_tp_wayland::Keyboard, rate: i32, delay: i32) {
        tracing::trace!("keyboard::repeat_info");
    }
}
#[cfg(feature = "wayland")]
impl<AppFuture: core::future::Future<Output = ()>> wl::ZwpTextInputV3EventListener
    for WaylandGlobalMessaging<AppFuture>
{
    #[tracing::instrument(skip(self, sender, surface))]
    fn enter(
        &mut self,
        sender: &mut peridot_tp_wayland::ZwpTextInputV3,
        surface: &mut peridot_tp_wayland::Surface,
    ) {
        tracing::trace!("textinputv3::enter");
        sender.enable().expect("text_input.enable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(skip(self, sender, surface))]
    fn leave(
        &mut self,
        sender: &mut peridot_tp_wayland::ZwpTextInputV3,
        surface: &mut peridot_tp_wayland::Surface,
    ) {
        tracing::trace!("textinputv3::leave");
        sender.disable().expect("text_input.disable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(skip(self, sender))]
    fn preedit_string(
        &mut self,
        sender: &mut peridot_tp_wayland::ZwpTextInputV3,
        text: Option<&core::ffi::CStr>,
        cursor_begin: i32,
        cursor_end: i32,
    ) {
        tracing::trace!("textinputv3::preedit_string");
    }

    #[tracing::instrument(skip(self, sender))]
    fn commit_string(
        &mut self,
        sender: &mut peridot_tp_wayland::ZwpTextInputV3,
        text: Option<&core::ffi::CStr>,
    ) {
        tracing::trace!("textinputv3::commit_string");
    }

    #[tracing::instrument(skip(self, sender))]
    fn delete_surrounding_text(
        &mut self,
        sender: &mut peridot_tp_wayland::ZwpTextInputV3,
        before_length: u32,
        after_length: u32,
    ) {
        tracing::trace!("textinputv3::delete_surrounding_text");
    }

    #[tracing::instrument(skip(self, sender))]
    fn done(&mut self, sender: &mut peridot_tp_wayland::ZwpTextInputV3, serial: u32) {
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
        sender: &mut peridot_tp_wayland::ZwlrLayerSurfaceV1,
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
    fn closed(&mut self, _sender: &mut peridot_tp_wayland::ZwlrLayerSurfaceV1) {
        tracing::trace!("layer surface closed");
    }
}

#[cfg(feature = "wayland")]
pub struct WaylandWindow {
    surface: wl::Owned<wl::Surface>,
    xdg_surface: wl::Owned<wl::XdgSurface>,
    xdg_toplevel: wl::Owned<wl::XdgToplevel>,
    _deco: Option<wl::Owned<wl::ZxdgToplevelDecorationV1>>,
    _appmenu: Option<wl::Owned<wl::OrgKdeKwinAppmenu>>,
    state: Box<WaylandWindowState>,
}
#[cfg(feature = "wayland")]
unsafe impl Sync for WaylandWindow {}
#[cfg(feature = "wayland")]
unsafe impl Send for WaylandWindow {}
#[cfg(feature = "wayland")]
impl WaylandWindow {
    fn new(
        wl_interfaces: &WaylandGlobalInterfaces,
        dbus: &dbus::Connection,
        terminate_event: std::sync::Arc<EventFD>,
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

        let mut state = Box::new(WaylandWindowState {
            pending_configure_size: None,
            active_buffer_scale: 1.0,
            active_size: (640, 480),
            swapchain_externally_invalidation_signal: std::sync::Arc::new(
                std::sync::atomic::AtomicBool::new(false),
            ),
            terminate_event,
        });
        surface
            .set_listener(&mut *state.as_mut())
            .into_result()
            .expect("wl_surface set listener");
        xdg_surface
            .set_listener(&mut *state.as_mut())
            .into_result()
            .expect("xdg_surface set listener");
        xdg_toplevel
            .set_listener(&mut *state.as_mut())
            .into_result()
            .expect("xdg_toplevel set listener");
        if let Some(ref mut x) = deco {
            x.set_listener(&mut *state.as_mut())
                .into_result()
                .expect("zxdg_toplevel_decoration_v1.set_listener");
        }

        // commits initial state
        surface.commit().expect("wl_surface.commit");

        Self {
            surface,
            xdg_surface,
            xdg_toplevel,
            _appmenu: appmenu,
            _deco: deco,
            state,
        }
    }

    pub fn client_size(&self) -> (u32, u32) {
        self.state.active_size
    }
}

#[cfg(feature = "wayland")]
struct WaylandPopupState {}
#[cfg(feature = "wayland")]
impl wl::XdgSurfaceEventListener for WaylandPopupState {
    #[tracing::instrument(skip(self, sender))]
    fn configure(&mut self, sender: &mut peridot_tp_wayland::XdgSurface, serial: u32) {
        tracing::trace!("popup.surface.configure");
        sender.ack_configure(serial).expect("popup.ack_configure");
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
    active_size: (u32, u32),
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    terminate_event: std::sync::Arc<EventFD>,
}
#[cfg(feature = "wayland")]
impl wl::SurfaceEventListener for WaylandWindowState {
    #[tracing::instrument(skip(self, surface, output))]
    fn enter(
        &mut self,
        surface: &mut peridot_tp_wayland::Surface,
        output: &mut peridot_tp_wayland::Output,
    ) {
    }

    #[tracing::instrument(skip(self, surface, output))]
    fn leave(
        &mut self,
        surface: &mut peridot_tp_wayland::Surface,
        output: &mut peridot_tp_wayland::Output,
    ) {
    }

    #[tracing::instrument(skip(self, surface))]
    fn preferred_buffer_scale(&mut self, surface: &mut peridot_tp_wayland::Surface, factor: i32) {
        tracing::trace!("perferred buffer scale");
        surface
            .set_buffer_scale(factor)
            .expect("wl_surface set_buffer_scale");
        self.active_buffer_scale = factor as _;
    }

    #[tracing::instrument(skip(self, surface))]
    fn preferred_buffer_transform(
        &mut self,
        surface: &mut peridot_tp_wayland::Surface,
        transform: u32,
    ) {
        tracing::trace!("preferred buffer transform");
    }
}
#[cfg(feature = "wayland")]
impl wl::XdgSurfaceEventListener for WaylandWindowState {
    #[tracing::instrument(skip(self, sender))]
    fn configure(&mut self, sender: &mut peridot_tp_wayland::XdgSurface, serial: u32) {
        tracing::trace!("xdg surface configure");

        if let Some((w, h)) = self.pending_configure_size.take() {
            let w: u32 = (u32::try_from(w).expect("negative window size") as f32
                * self.active_buffer_scale)
                .ceil() as _;
            let h: u32 = (u32::try_from(h).expect("negative window size") as f32
                * self.active_buffer_scale)
                .ceil() as _;
            if w != self.active_size.0 || h != self.active_size.1 {
                self.active_size = (w, h);
                self.swapchain_externally_invalidation_signal
                    .store(true, std::sync::atomic::Ordering::Relaxed);
            }
        }

        sender
            .ack_configure(serial)
            .expect("xdg_surface.ack_configure");
    }
}
#[cfg(feature = "wayland")]
impl wl::XdgToplevelEventListener for WaylandWindowState {
    #[tracing::instrument(skip(self, sender))]
    fn close(&mut self, sender: &mut peridot_tp_wayland::XdgToplevel) {
        tracing::trace!("xdg toplevel close");
        self.terminate_event.inc(1).expect("terminate_event.inc");
    }

    #[tracing::instrument(skip(self, sender), fields(states = ?unsafe { states.as_slice::<u32>() }))]
    fn configure(
        &mut self,
        sender: &mut peridot_tp_wayland::XdgToplevel,
        width: i32,
        height: i32,
        states: &mut peridot_tp_wayland::ffi::Array,
    ) {
        tracing::trace!("xdg toplevel configure");

        self.pending_configure_size = Some((
            if width == 0 {
                self.active_size.0 as _
            } else {
                width
            },
            if height == 0 {
                self.active_size.1 as _
            } else {
                height
            },
        ));
    }

    fn configure_bounds(
        &mut self,
        sender: &mut peridot_tp_wayland::XdgToplevel,
        width: i32,
        height: i32,
    ) {
    }

    fn wm_capabilities(
        &mut self,
        sender: &mut peridot_tp_wayland::XdgToplevel,
        capabilities: &mut peridot_tp_wayland::ffi::Array,
    ) {
    }
}
#[cfg(feature = "wayland")]
impl wl::ZxdgToplevelDecorationV1EventListener for WaylandWindowState {
    fn configure(
        &mut self,
        sender: &mut peridot_tp_wayland::ZxdgToplevelDecorationV1,
        mode: peridot_tp_wayland::ZxdgToplevelDecorationV1Mode,
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
    event_dispatcher: LogicFiberEventDispatcher<AppFuture>,
    text_services_mgr: Option<CoreTextServicesManager>,
    edit_context: Option<CoreTextEditContext>,
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

    extern "system" fn handle_messages(
        hwnd: HWND,
        msg: u32,
        wparam: WPARAM,
        lparam: LPARAM,
    ) -> LRESULT {
        use windows::Win32::UI::WindowsAndMessaging::{
            WA_ACTIVE, WA_CLICKACTIVE, WM_ACTIVATE, WM_CHAR, WM_CREATE, WM_KILLFOCUS,
            WM_LBUTTONDOWN, WM_LBUTTONUP, WM_MOUSEMOVE, WM_NCCALCSIZE, WM_NCHITTEST, WM_SETFOCUS,
            WM_SIZE,
        };

        if msg == WM_DESTROY {
            unsafe {
                drop(Box::from_raw(Self::get_for_window(hwnd)));
                PostQuitMessage(0);
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

        if msg == WM_NCCALCSIZE {
            if wparam.0 == 1 {
                // remove non-client area

                let params = unsafe {
                    use windows::Win32::UI::WindowsAndMessaging::NCCALCSIZE_PARAMS;

                    &mut *core::ptr::without_provenance_mut::<NCCALCSIZE_PARAMS>(
                        lparam.0.cast_unsigned(),
                    )
                };
                let w = unsafe {
                    use windows::Win32::UI::WindowsAndMessaging::{
                        GetSystemMetrics, SM_CXSIZEFRAME,
                    };
                    GetSystemMetrics(SM_CXSIZEFRAME)
                };
                let h = unsafe {
                    use windows::Win32::UI::WindowsAndMessaging::{
                        GetSystemMetrics, SM_CYSIZEFRAME,
                    };
                    GetSystemMetrics(SM_CYSIZEFRAME)
                };
                params.rgrc[0].left += w;
                params.rgrc[0].right -= w;
                params.rgrc[0].bottom -= h;
                // topはいじらない（topいじるともとのタイトルバーが一部表示される 他アプリもそんな感じなのでtopは自前で当たり判定組んでリサイズ判定する）

                return LRESULT(0);
            }
        }

        if msg == WM_SIZE {
            let w = (lparam.0 & 0xffff) as u16;
            let h = ((lparam.0 >> 16) & 0xffff) as u16;
            tracing::trace!(w, h, "WM_SIZE");

            if let Some(state) = Self::try_get_for_window(hwnd) {
                state.event_dispatcher.dispatch(Event::WindowResize {
                    new_width: w as _,
                    new_height: h as _,
                });
            }

            return LRESULT(0);
        }

        if msg == WM_NCHITTEST {
            use windows::Win32::UI::WindowsAndMessaging::{
                HTCAPTION, HTCLIENT, HTCLOSE, HTMAXBUTTON, HTMINBUTTON,
            };

            let x = (lparam.0 & 0xffff) as i16;
            let y = ((lparam.0 >> 16) & 0xffff) as i16;
            let mut p = [windows::Win32::Foundation::POINT {
                x: x as _,
                y: y as _,
            }];
            unsafe {
                use windows::Win32::Graphics::Gdi::MapWindowPoints;
                MapWindowPoints(None, Some(hwnd), &mut p);
            }
            let [windows::Win32::Foundation::POINT { x, y }] = p;

            let mut client_size = core::mem::MaybeUninit::uninit();
            unsafe {
                GetClientRect(hwnd, client_size.as_mut_ptr()).expect("getclientsize");
            }
            let client_size = unsafe { client_size.assume_init() };

            if 0 > x || x > client_size.right || 0 > y || y > client_size.bottom {
                // ウィンドウ範囲外はシステムにおまかせ
                return unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) };
            }

            let resize_h = unsafe {
                use windows::Win32::UI::WindowsAndMessaging::{GetSystemMetrics, SM_CYSIZEFRAME};
                GetSystemMetrics(SM_CYSIZEFRAME)
            };
            if y < resize_h {
                return LRESULT(windows::Win32::UI::WindowsAndMessaging::HTTOP as _);
            }

            let pointer_input_manager_ptr = unsafe {
                core::ptr::with_exposed_provenance_mut::<PointerInputManager>(GetWindowLongPtrW(
                    hwnd,
                    WINDOW_LONG_PTR_INDEX((core::mem::size_of::<usize>() * 1) as _),
                )
                    as _)
            };
            let ht_manager_ptr = unsafe {
                core::ptr::with_exposed_provenance_mut::<HitTestTreeManager>(GetWindowLongPtrW(
                    hwnd,
                    WINDOW_LONG_PTR_INDEX((core::mem::size_of::<usize>() * 2) as _),
                ) as _)
            };

            if pointer_input_manager_ptr.is_null() {
                // unlinked from logic fiber
                return LRESULT(HTCLIENT as _);
            }

            let pointer_input_manager = unsafe { &*pointer_input_manager_ptr };
            let ui_scale_factor = 1.0; // TODO: dpi
            return match pointer_input_manager.role(
                x as f32 / ui_scale_factor,
                y as f32 / ui_scale_factor,
                (client_size.right - client_size.left) as f32 / ui_scale_factor,
                (client_size.bottom - client_size.top) as f32 / ui_scale_factor,
                unsafe { &*ht_manager_ptr },
                HitTestTreeManager::ROOT,
            ) {
                None => LRESULT(HTCLIENT as _),
                Some(crate::hittest::Role::TitleBar) => LRESULT(HTCAPTION as _),
                Some(crate::hittest::Role::ForceClient) => LRESULT(HTCLIENT as _),
                Some(crate::hittest::Role::CloseButton) => LRESULT(HTCLOSE as _),
                Some(crate::hittest::Role::MaximizeButton) => LRESULT(HTMAXBUTTON as _),
                Some(crate::hittest::Role::MinimizeButton) => LRESULT(HTMINBUTTON as _),
                // Windowsだと同じ位置にあるので同じものを返す
                Some(crate::hittest::Role::RestoreButton) => LRESULT(HTMAXBUTTON as _),
            };
        }

        if msg == WM_LBUTTONDOWN {
            Self::get_for_window(hwnd)
                .event_dispatcher
                .dispatch(Event::PointerDown {
                    active_window: hwnd,
                    client_x: (lparam.0 & 0xffff) as i16 as _,
                    client_y: ((lparam.0 >> 16) & 0xffff) as i16 as _,
                });

            return LRESULT(0);
        }

        if msg == WM_MOUSEMOVE {
            Self::get_for_window(hwnd)
                .event_dispatcher
                .dispatch(Event::PointerMove {
                    active_window: hwnd,
                    client_x: (lparam.0 & 0xffff) as i16 as _,
                    client_y: ((lparam.0 >> 16) & 0xffff) as i16 as _,
                });

            return LRESULT(0);
        }

        if msg == WM_LBUTTONUP {
            Self::get_for_window(hwnd)
                .event_dispatcher
                .dispatch(Event::PointerUp);

            return LRESULT(0);
        }

        unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
    }
}

#[cfg(target_os = "macos")]
pub struct MacWindow {
    native_ptr: *mut core::ffi::c_void,
    state: Pin<Box<MacWindowState>>,
}
#[cfg(target_os = "macos")]
impl Drop for MacWindow {
    fn drop(&mut self) {
        unsafe {
            ni_unset_window_callbacks(self.native_ptr);
            ni_release_window(self.native_ptr);
        }
    }
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindow {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindow {}
#[cfg(target_os = "macos")]
impl MacWindow {
    pub fn new() -> Self {
        let native_ptr = unsafe { ni_create_window() };
        let mut state = Box::pin(MacWindowState {
            wlink: native_ptr,
            drag_preview: MacDragPreviewPopupHandle,
            swapchain_externally_invalidation_signal: std::sync::Arc::new(
                std::sync::atomic::AtomicBool::new(false),
            ),
            active_rt_size: std::sync::Mutex::new((960, 540)),
        });
        let callbacks: &'static WindowLinkCallbacks = &WindowLinkCallbacks {
            on_resize: MacWindowState::on_resize,
            on_pointer_down: MacWindowState::on_pointer_down,
            on_pointer_up: MacWindowState::on_pointer_up,
        };
        unsafe {
            ni_set_window_callbacks(
                native_ptr,
                callbacks,
                state.as_mut().get_mut() as *mut _ as _,
            );
        }

        Self { native_ptr, state }
    }

    #[inline(always)]
    pub fn make_primary_window(&mut self) {
        unsafe {
            ni_make_primary_window(self.native_ptr);
        }
    }

    #[inline(always)]
    pub fn metal_layer(&self) -> *mut core::ffi::c_void {
        unsafe { ni_get_metal_layer(self.native_ptr) }
    }

    #[inline(always)]
    pub fn manual_capture_begin(&self) {
        unsafe {
            manual_capture_begin(self.native_ptr);
        }
    }
}

#[cfg(target_os = "macos")]
struct MacWindowState {
    wlink: *mut core::ffi::c_void,
    drag_preview: MacDragPreviewPopupHandle,
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    active_rt_size: std::sync::Mutex<(u32, u32)>,
}
#[cfg(target_os = "macos")]
unsafe impl Sync for MacWindowState {}
#[cfg(target_os = "macos")]
unsafe impl Send for MacWindowState {}
#[cfg(target_os = "macos")]
impl MacWindowState {
    extern "C" fn on_resize(caller_context: *mut core::ffi::c_void, width: u32, height: u32) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        let mut active_rt_size_locked = this.active_rt_size.lock().expect("poisoned");
        if width != active_rt_size_locked.0 || height != active_rt_size_locked.1 {
            *active_rt_size_locked = (width, height);
            this.swapchain_externally_invalidation_signal
                .store(true, std::sync::atomic::Ordering::Relaxed);
        }
    }

    extern "C" fn on_pointer_down(caller_context: *mut core::ffi::c_void, mut x: f64, mut y: f64) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        tracing::info!(x, y, "pointer down");
        unsafe {
            ni_convert_point_to_screen(this.wlink, &mut x, &mut y);
        }
        this.drag_preview.show(&DesktopRect {
            left: x as _,
            top: y as _,
            width: 128,
            height: 128,
        });
    }

    extern "C" fn on_pointer_up(caller_context: *mut core::ffi::c_void) {
        let this = unsafe { &mut *caller_context.cast::<Self>() };

        tracing::info!("pointer up");
        this.drag_preview.hide();
    }
}

#[cfg(target_os = "macos")]
#[repr(C)]
pub struct WindowLinkCallbacks {
    pub on_resize: extern "C" fn(caller_context: *mut core::ffi::c_void, width: u32, height: u32),
    pub on_pointer_down: extern "C" fn(caller_context: *mut core::ffi::c_void, x: f64, y: f64),
    pub on_pointer_up: extern "C" fn(caller_context: *mut core::ffi::c_void),
}

#[cfg(target_os = "macos")]
unsafe extern "C" {
    fn nsapp_run();

    fn ni_create_window() -> *mut core::ffi::c_void;
    fn ni_release_window(window_link: *mut core::ffi::c_void);
    fn ni_make_primary_window(window_link: *mut core::ffi::c_void);
    fn ni_set_window_callbacks(
        window_link: *mut core::ffi::c_void,
        callbacks: *const WindowLinkCallbacks,
        caller_context: *mut core::ffi::c_void,
    );
    fn ni_unset_window_callbacks(window_link: *mut core::ffi::c_void);
    fn ni_get_metal_layer(window_link: *mut core::ffi::c_void) -> *mut core::ffi::c_void;
    fn ni_convert_point_to_screen(
        window_link: *mut core::ffi::c_void,
        x: *mut core::ffi::c_double,
        y: *mut core::ffi::c_double,
    );

    fn ni_show_drag_preview();
    fn ni_hide_drag_preview();
    fn ni_move_drag_preview(
        x: core::ffi::c_double,
        y: core::ffi::c_double,
        width: core::ffi::c_double,
        height: core::ffi::c_double,
    );

    fn manual_capture_begin(window_link: *mut core::ffi::c_void);
    fn manual_capture_end();
}
