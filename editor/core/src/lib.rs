use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Device, DeviceMemoryMut, Fence, FenceMut,
    ImageChild, InstanceChild, MemoryBound, PhysicalDevice, QueueMut, RenderPass, ShaderModule,
    SurfaceCreateInfo, Swapchain, VkHandle, VkHandleMut, VkObject,
};
use core::pin::Pin;
#[cfg(feature = "wayland")]
use linux_epoll::{Epoll, EpollEventBits};
#[cfg(feature = "wayland")]
use linux_eventfd::{EventFD, EventFDFlags};
#[cfg(feature = "fontconfig")]
use peridot_tp_fontconfig as fc;
#[cfg(feature = "freetype")]
use peridot_tp_freetype as ft;
#[cfg(feature = "wayland")]
use peridot_tp_wayland as wl;
use std::collections::{HashMap, VecDeque};
#[cfg(windows)]
use windows::Win32::{
    Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
    Graphics::{
        Direct2D::Common::{
            D2D1_FIGURE_BEGIN_FILLED, D2D1_FIGURE_END_CLOSED, D2D1_FILL_MODE_WINDING,
            ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySink_Impl,
        },
        DirectWrite::{
            DWRITE_FACTORY_TYPE_SHARED, DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_WEIGHT_NORMAL, DWRITE_GLYPH_METRICS, DWriteCreateFactory, IDWriteFactory,
            IDWritePixelSnapping_Impl, IDWriteTextFormat, IDWriteTextRenderer,
            IDWriteTextRenderer_Impl,
        },
        Gdi::HBRUSH,
    },
    System::LibraryLoader::GetModuleHandleW,
    UI::WindowsAndMessaging::{
        CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, DispatchMessageW, GetClientRect,
        GetMessageW, GetWindowLongPtrW, HCURSOR, IDI_APPLICATION, LoadIconW, PostQuitMessage,
        RegisterClassExW, SHOW_WINDOW_CMD, SW_SHOWNORMAL, SetWindowLongPtrW, ShowWindow,
        WINDOW_LONG_PTR_INDEX, WM_DESTROY, WNDCLASS_STYLES, WNDCLASSEXW, WS_EX_APPWINDOW,
        WS_OVERLAPPEDWINDOW,
    },
};
#[cfg(windows)]
use windows_core::*;

use crate::{
    composite::{
        AnimatableColor, AnimatableFloat, BoundCompositeRenderer, CompositeMode, CompositeRect,
        CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
        CompositeRectTextVerticalAlignment, CompositeRenderingData, CompositeStreamingData,
        CompositeTree, FontID, VectorRasterizationState,
    },
    graphics::{VG_COLOR_FORMAT, VG_STENCIL_FORMAT, VulkanDevice},
};

mod atlas;
mod composite;
mod graphics;
mod helper_types;
mod mathext;

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
        let mut zero_terminated = Vec::with_capacity(buf.len() + 1);
        zero_terminated.extend(buf);
        zero_terminated.push(0);

        unsafe {
            windows::Win32::System::Diagnostics::Debug::OutputDebugStringA(windows::core::PCSTR(
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
    let mut app = core::pin::pin!(run(EventQueue {
        event_store: event_store.as_mut().get_mut(),
    }));
    main_wrapper(app.as_mut(), event_store);
}

fn main_wrapper<AppFuture: core::future::Future<Output = ()>>(
    mut app: Pin<&mut AppFuture>,
    mut event_store: Pin<&mut Option<Event>>,
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

    #[cfg(feature = "freetype")]
    let ft = FreeType::init().expect("FreeType.init");

    let _ = app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }));

    #[cfg(windows)]
    let hinstance: HINSTANCE = unsafe { GetModuleHandleW(None).expect("GetModuleHandleW").into() };
    #[cfg(windows)]
    let atom = unsafe {
        RegisterClassExW(&WNDCLASSEXW {
            cbSize: core::mem::size_of::<WNDCLASSEXW>() as _,
            style: WNDCLASS_STYLES(0),
            cbClsExtra: 0,
            cbWndExtra: core::mem::size_of::<[usize; 2]>() as _,
            lpfnWndProc: Some(wndproc::<AppFuture>),
            hInstance: hinstance,
            hIcon: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
            hCursor: HCURSOR(core::ptr::null_mut()),
            hbrBackground: HBRUSH(core::ptr::null_mut()),
            lpszMenuName: PCWSTR::null(),
            lpszClassName: w!("MainWindow"),
            hIconSm: LoadIconW(None, IDI_APPLICATION).expect("LoadIconW"),
        })
    };
    #[cfg(windows)]
    if atom == 0 {
        Err::<(), _>(std::io::Error::last_os_error()).expect("RegisterClassExW");
    }

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
    let mut w = Win32Window(w);

    #[cfg(windows)]
    unsafe {
        w.set_long_ptr(
            WINDOW_LONG_PTR_INDEX(0),
            app.as_mut().get_unchecked_mut() as *mut _ as _,
        );
        w.set_long_ptr(
            WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _),
            event_store.as_mut().get_mut() as *mut _ as _,
        );
    }

    #[cfg(feature = "wayland")]
    let mut wl_display = wl::Display::connect().expect("wl_display connect");
    #[cfg(feature = "wayland")]
    let mut wl_registry = wl_display.get_registry().expect("wl_registry get");
    #[cfg(feature = "wayland")]
    struct RegistryListener {
        compositor: Option<wl::Owned<wl::Compositor>>,
        outputs: Vec<wl::Owned<wl::Output>>,
        xdg_wm_base: Option<wl::Owned<wl::XdgWmBase>>,
        seat: Option<wl::Owned<wl::Seat>>,
        shm: Option<wl::Owned<wl::Shm>>,
        layer_shell: Option<wl::Owned<wl::ZwlrLayerShellV1>>,
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
            } else if interface == c"zwlr_layer_shell_v1" {
                self.layer_shell = Some(registry.bind(name, version).expect("bind layer_shell"));
            }
        }

        fn global_remove(&mut self, _registry: &mut peridot_tp_wayland::Registry, name: u32) {
            tracing::info!(target: "wl::diag", name, "wl interface remove");
        }
    }
    #[cfg(feature = "wayland")]
    let mut rl = RegistryListener {
        compositor: None,
        outputs: Vec::new(),
        xdg_wm_base: None,
        seat: None,
        shm: None,
        layer_shell: None,
    };
    #[cfg(feature = "wayland")]
    wl_registry
        .set_listener(&mut rl)
        .into_result()
        .expect("wl_registry set_listener");
    #[cfg(feature = "wayland")]
    wl_display.roundtrip().expect("wl_display roundtrip");
    #[cfg(feature = "wayland")]
    drop(wl_registry);
    #[cfg(feature = "wayland")]
    let wl_compositor = rl.compositor.expect("no compositor");
    #[cfg(feature = "wayland")]
    let mut xdg_wm_base = rl.xdg_wm_base.expect("no xdg-shell");
    #[cfg(feature = "wayland")]
    let mut seat = rl.seat.expect("no seat");
    #[cfg(feature = "wayland")]
    let mut shm = rl.shm.expect("no shm");
    #[cfg(feature = "wayland")]
    let layer_shell = rl.layer_shell.expect("no layer_shell");
    #[cfg(feature = "wayland")]
    let outputs = rl.outputs;

    #[cfg(feature = "wayland")]
    let mut wl_global_msg = WaylandGlobalMessaging {
        pointer: None,
        pointer_pos: (0.0, 0.0),
        compositor: unsafe { wl_compositor.copy_ptr().as_ptr() },
        wm_base: unsafe { xdg_wm_base.copy_ptr().as_ptr() },
        root_window: core::ptr::null_mut(),
        popup_buf: core::ptr::null_mut(),
        popup: None,
        display: &mut wl_display,
        _pinned: core::marker::PhantomPinned,
    };
    #[cfg(feature = "wayland")]
    xdg_wm_base
        .set_listener(&mut wl_global_msg)
        .into_result()
        .expect("xdg_wm_base set_listener");
    #[cfg(feature = "wayland")]
    seat.set_listener(&mut wl_global_msg)
        .into_result()
        .expect("seat set_listener");

    #[cfg(feature = "wayland")]
    let wl_surface = wl_compositor.create_surface().expect("wl_surface create");
    #[cfg(feature = "wayland")]
    let wl_xdg_surface = xdg_wm_base
        .get_xdg_surface(&wl_surface)
        .expect("xdg_surface create");
    #[cfg(feature = "wayland")]
    let wl_xdg_toplevel = wl_xdg_surface.get_toplevel().expect("xdg_toplevel create");
    #[cfg(feature = "wayland")]
    wl_xdg_toplevel
        .set_title(c"Peridot Marble Editor")
        .expect("xdg_toplevel.set_title");
    #[cfg(feature = "wayland")]
    wl_xdg_surface
        .set_window_geometry(0, 0, 640, 480)
        .expect("xdg_surface.set_window_geometry");

    #[cfg(feature = "wayland")]
    let terminate_event = std::sync::Arc::new(
        EventFD::new(0, EventFDFlags::empty()).expect("terminate_event.create"),
    );

    #[cfg(feature = "wayland")]
    let mut w = WaylandWindow {
        surface: wl_surface,
        xdg_surface: wl_xdg_surface,
        xdg_toplevel: wl_xdg_toplevel,
        state: Box::new(WaylandWindowState {
            pending_configure_size: None,
            active_buffer_scale: 1.0,
            active_size: (640, 480),
            swapchain_externally_invalidation_signal: std::sync::Arc::new(
                std::sync::atomic::AtomicBool::new(false),
            ),
            terminate_event: terminate_event.clone(),
        }),
    };
    #[cfg(feature = "wayland")]
    w.initialize();
    #[cfg(feature = "wayland")]
    w.surface.commit().expect("wl_surface.commit");
    #[cfg(feature = "wayland")]
    wl_display.roundtrip().expect("roundtrip");

    let fd = unsafe {
        loop {
            let mut nambuf = b"/wl_shm-000000\x00".clone();
            let mut ts = core::mem::MaybeUninit::uninit();
            libc::clock_gettime(libc::CLOCK_REALTIME, ts.as_mut_ptr());
            let mut r = ts.assume_init_ref().tv_nsec;
            for n in 0..6 {
                nambuf[8 + n] = (b'A' as i64 + (r & 15) + (r & 16) * 2) as _;
                r >>= 5;
            }

            let fd = libc::shm_open(
                nambuf.as_ptr().cast(),
                libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
                0o600,
            );
            if fd == -1 {
                continue;
            }

            if libc::ftruncate(fd, 1024 * 1024 * 4) < 0 {
                panic!("ftruncate failed");
            }
            break fd;
        }
    };
    #[cfg(feature = "wayland")]
    let a = unsafe {
        let a = libc::mmap(
            std::ptr::null_mut(),
            1024 * 1024 * 4,
            libc::PROT_READ | libc::PROT_WRITE,
            libc::MAP_SHARED,
            fd,
            0,
        );
        if a == libc::MAP_FAILED {
            panic!("mmap failed");
        }

        a
    };
    #[cfg(feature = "wayland")]
    let shmp = shm
        .create_pool(&fd, 1024 * 1024 * 4)
        .expect("shm.create_pool");
    #[cfg(feature = "wayland")]
    let popup_buf = shmp
        .create_buffer(0, 1024, 1024, 1024 * 4, wl::ShmFormat::ARGB8888)
        .expect("shmp.create_buffer");
    #[cfg(feature = "wayland")]
    unsafe {
        for n in 0..1024 * 1024 {
            core::ptr::write(a.cast::<u32>().add(n), 0x80000000);
        }
    }
    #[cfg(feature = "wayland")]
    {
        wl_global_msg.root_window = unsafe { w.xdg_surface.copy_ptr().as_ptr() };
        wl_global_msg.popup_buf = unsafe { popup_buf.copy_ptr().as_ptr() };
    }

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

    let mut composite_tree = CompositeTree::new();
    composite_tree.get_mut(CompositeTree::ROOT).composite_mode =
        CompositeMode::FillColor(AnimatableColor::Value([0.1, 0.2, 0.3, 1.0]));
    composite_tree.get_mut(CompositeTree::ROOT).has_bitmap = true;
    composite_tree.mark_dirty(CompositeTree::ROOT);

    // app title view
    let app_title = composite_tree.register(CompositeRect {
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

    let shutdown = std::sync::atomic::AtomicBool::new(false);
    std::thread::scope(|thread_scope| {
        let render_thread = std::thread::Builder::new()
            .name("Render".into())
            .spawn_scoped(thread_scope, || {
                tracing::info!("Starting RenderThread...");
                let mut render_queue = vk_device.queue(vk_device.present_queue_family_index(), 0);

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

        #[cfg(feature = "wayland")]
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
        let mut events = [const { core::mem::MaybeUninit::uninit() }; 8];
        #[cfg(feature = "wayland")]
        'app: loop {
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
            wl_display.flush().expect("wl_display.flush");
            let active_events = epoll.wait(&mut events, None).expect("epoll.wait");

            let mut wl_display_signal = false;
            let mut terminate_signal = false;
            for n in 0..active_events {
                let e = unsafe { events[n as usize].assume_init_ref() };
                if e.value() == 0 {
                    wl_display_signal = true;
                } else if e.value() == 1 {
                    terminate_signal = true;
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
    });
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
        unsafe {
            ft::done_face(self.ui_title_project_name);
            ft::done_face(self.ui_default);
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

struct BoxInstance {
    posst: [f32; 4],
    uvst: [f32; 4],
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

#[cfg(windows)]
#[implement(IDWriteTextRenderer)]
pub struct AtlasTextRenderer {
    box_instances: *mut Vec<BoxInstance>,
    atlas: *mut GlyphAtlas,
    new_filltri_points: *mut Vec<[f32; 2]>,
    new_filltri_indices: *mut Vec<u16>,
    new_curve_triangles: *mut Vec<[f32; 4]>,
}
#[cfg(windows)]
impl IDWritePixelSnapping_Impl for AtlasTextRenderer_Impl {
    fn GetCurrentTransform(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
        transform: *mut windows::Win32::Graphics::DirectWrite::DWRITE_MATRIX,
    ) -> windows_core::Result<()> {
        unsafe {
            *transform = windows::Win32::Graphics::DirectWrite::DWRITE_MATRIX {
                m11: 1.0,
                m12: 0.0,
                m21: 0.0,
                m22: 1.0,
                dx: 0.0,
                dy: 0.0,
            };
        }

        Ok(())
    }

    fn GetPixelsPerDip(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
    ) -> windows_core::Result<f32> {
        Ok(168.0 / 96.0)
    }

    fn IsPixelSnappingDisabled(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
    ) -> windows_core::Result<windows_core::BOOL> {
        Ok(BOOL(0))
    }
}
#[cfg(windows)]
impl IDWriteTextRenderer_Impl for AtlasTextRenderer_Impl {
    fn DrawGlyphRun(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
        mut baselineoriginx: f32,
        baselineoriginy: f32,
        measuringmode: windows::Win32::Graphics::DirectWrite::DWRITE_MEASURING_MODE,
        glyphrun: *const windows::Win32::Graphics::DirectWrite::DWRITE_GLYPH_RUN,
        glyphrundescription: *const windows::Win32::Graphics::DirectWrite::DWRITE_GLYPH_RUN_DESCRIPTION,
        clientdrawingeffect: windows_core::Ref<windows_core::IUnknown>,
    ) -> windows_core::Result<()> {
        let dip_to_pixels_scaling = 168.0f32 / 96.0;

        let glyphrun = unsafe { &*glyphrun };
        println!(
            "DrawGlyphRun {baselineoriginx} {baselineoriginy} {measuringmode:?} {:?}",
            glyphrun.fontFace
        );
        let font_face = glyphrun.fontFace.as_ref().expect("no font face");
        let mut font_metrics = core::mem::MaybeUninit::uninit();
        unsafe { font_face.GetMetrics(font_metrics.as_mut_ptr()) };
        let font_metrics = unsafe { font_metrics.assume_init_ref() };
        let design_unit = font_metrics.designUnitsPerEm;
        let mut glyph_metrics: Vec<DWRITE_GLYPH_METRICS> =
            Vec::with_capacity(glyphrun.glyphCount as _);
        unsafe {
            font_face
                .GetDesignGlyphMetrics(
                    glyphrun.glyphIndices,
                    glyphrun.glyphCount,
                    glyph_metrics.spare_capacity_mut().as_mut_ptr() as _,
                    glyphrun.isSideways.as_bool(),
                )
                .expect("GetDesignGlyphMetrics");
            glyph_metrics.set_len(glyphrun.glyphCount as _);
        }
        for n in 0..glyphrun.glyphCount as usize {
            let glyph_width = (glyph_metrics[n].advanceWidth as i32
                - glyph_metrics[n].leftSideBearing
                - glyph_metrics[n].rightSideBearing) as f32
                * glyphrun.fontEmSize
                * dip_to_pixels_scaling
                / design_unit as f32;
            let glyph_height = (glyph_metrics[n].advanceHeight as i32
                - glyph_metrics[n].topSideBearing
                - glyph_metrics[n].bottomSideBearing) as f32
                * glyphrun.fontEmSize
                * dip_to_pixels_scaling
                / design_unit as f32;

            let (r, is_new) = unsafe {
                (*self.atlas).acquire(
                    (FontID::UIDefault as _, *glyphrun.glyphIndices.add(n)),
                    glyph_width.ceil() as _,
                    glyph_height.ceil() as _,
                )
            };
            println!(
                "DrawGlyphRun.Glyph {} {} {:?} {:?} {glyph_width} {glyph_height} {r:?} {is_new}",
                unsafe { *glyphrun.glyphAdvances.add(n) },
                unsafe { *glyphrun.glyphIndices.add(n) },
                unsafe { *glyphrun.glyphOffsets.add(n) },
                glyph_metrics[n],
            );

            unsafe {
                (*self.box_instances).push(BoxInstance {
                    posst: [
                        glyph_width,
                        glyph_height,
                        (baselineoriginx
                            + glyph_metrics[n].leftSideBearing as f32 * glyphrun.fontEmSize
                                / design_unit as f32)
                            * dip_to_pixels_scaling,
                        (baselineoriginy
                            - (glyph_metrics[n].verticalOriginY as f32
                                - glyph_metrics[n].topSideBearing as f32)
                                * glyphrun.fontEmSize
                                / design_unit as f32)
                            * dip_to_pixels_scaling,
                    ],
                    uvst: [
                        r.width as f32 / (*self.atlas).space_mgr.max.width as f32,
                        r.height as f32 / (*self.atlas).space_mgr.max.height as f32,
                        r.left as f32 / (*self.atlas).space_mgr.max.width as f32,
                        r.top as f32 / (*self.atlas).space_mgr.max.height as f32,
                    ],
                });
            }
            if is_new {
                // render font here
                let mut current_figure_state = None;
                let sink = ID2D1SimplifiedGeometrySink::from(GlyphOutlineSink {
                    translate: windows_numerics::Vector2 {
                        X: r.left as f32
                            - glyph_metrics[n].leftSideBearing as f32
                                * glyphrun.fontEmSize
                                * dip_to_pixels_scaling
                                / design_unit as f32,
                        Y: r.top as f32
                            - (glyph_metrics[n].verticalOriginY as f32
                                - glyph_metrics[n].topSideBearing as f32)
                                * glyphrun.fontEmSize
                                * dip_to_pixels_scaling
                                / design_unit as f32,
                    },
                    dip_to_pixels_scale: dip_to_pixels_scaling,
                    current_figure_state: &mut current_figure_state,
                    filltri_points: self.new_filltri_points,
                    filltri_indices: self.new_filltri_indices,
                    curve_triangles: self.new_curve_triangles,
                });
                unsafe {
                    font_face
                        .GetGlyphRunOutline(
                            glyphrun.fontEmSize,
                            glyphrun.glyphIndices.add(n),
                            None,
                            None,
                            1,
                            glyphrun.isSideways.as_bool(),
                            false,
                            &sink,
                        )
                        .expect("GetGlyphRunOutline");
                }
                assert!(current_figure_state.is_none());
            }

            baselineoriginx += unsafe { *glyphrun.glyphAdvances.add(n) };
        }

        Ok(())
    }

    fn DrawInlineObject(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
        originx: f32,
        originy: f32,
        inlineobject: windows_core::Ref<windows::Win32::Graphics::DirectWrite::IDWriteInlineObject>,
        issideways: windows_core::BOOL,
        isrighttoleft: windows_core::BOOL,
        clientdrawingeffect: windows_core::Ref<windows_core::IUnknown>,
    ) -> windows_core::Result<()> {
        unimplemented!();
    }

    fn DrawStrikethrough(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
        baselineoriginx: f32,
        baselineoriginy: f32,
        strikethrough: *const windows::Win32::Graphics::DirectWrite::DWRITE_STRIKETHROUGH,
        clientdrawingeffect: windows_core::Ref<windows_core::IUnknown>,
    ) -> windows_core::Result<()> {
        unimplemented!();
    }

    fn DrawUnderline(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
        baselineoriginx: f32,
        baselineoriginy: f32,
        underline: *const windows::Win32::Graphics::DirectWrite::DWRITE_UNDERLINE,
        clientdrawingeffect: windows_core::Ref<windows_core::IUnknown>,
    ) -> windows_core::Result<()> {
        unimplemented!();
    }
}

#[cfg(windows)]
#[implement(ID2D1SimplifiedGeometrySink)]
struct GlyphOutlineSink {
    translate: windows_numerics::Vector2,
    dip_to_pixels_scale: f32,
    current_figure_state: *mut Option<(windows_numerics::Vector2, u16)>,
    filltri_points: *mut Vec<[f32; 2]>,
    filltri_indices: *mut Vec<u16>,
    curve_triangles: *mut Vec<[f32; 4]>,
}
#[cfg(windows)]
impl ID2D1SimplifiedGeometrySink_Impl for GlyphOutlineSink_Impl {
    fn BeginFigure(
        &self,
        startpoint: &windows_numerics::Vector2,
        figurebegin: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_BEGIN,
    ) {
        assert_eq!(figurebegin, D2D1_FIGURE_BEGIN_FILLED, "not filled figure");

        unsafe {
            (*self.current_figure_state) = Some((*startpoint, (*self.filltri_points).len() as _));
            (*self.filltri_points).push([
                startpoint.X * self.dip_to_pixels_scale + self.translate.X,
                -startpoint.Y * self.dip_to_pixels_scale + self.translate.Y,
            ]);
        }
    }

    fn EndFigure(&self, figureend: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_END) {
        let (start_point, filltri_index0) = unsafe {
            (*self.current_figure_state)
                .take()
                .expect("no figure started?")
        };

        if figureend == D2D1_FIGURE_END_CLOSED {
            // line to start
            unsafe {
                let filltri_point1 = (*self.filltri_points).len() - 1;
                (*self.filltri_points).push([
                    start_point.X * self.dip_to_pixels_scale + self.translate.X,
                    -start_point.Y * self.dip_to_pixels_scale + self.translate.Y,
                ]);
                (*self.filltri_indices).extend([
                    filltri_index0,
                    filltri_point1 as u16,
                    (*self.filltri_points).len() as u16 - 1,
                ]);
            }
        }
    }

    fn AddLines(&self, points: *const windows_numerics::Vector2, pointscount: u32) {
        let &(_, filltri_index0) = unsafe {
            (*self.current_figure_state)
                .as_ref()
                .expect("no figure started?")
        };

        for p in unsafe { core::slice::from_raw_parts(points, pointscount as _) } {
            unsafe {
                let filltri_point1 = (*self.filltri_points).len() - 1;
                (*self.filltri_points).push([
                    p.X * self.dip_to_pixels_scale + self.translate.X,
                    -p.Y * self.dip_to_pixels_scale + self.translate.Y,
                ]);
                (*self.filltri_indices).extend([
                    filltri_index0,
                    filltri_point1 as u16,
                    (*self.filltri_points).len() as u16 - 1,
                ]);
            }
        }
    }

    fn AddBeziers(
        &self,
        beziers: *const windows::Win32::Graphics::Direct2D::Common::D2D1_BEZIER_SEGMENT,
        bezierscount: u32,
    ) {
        let &(_, filltri_index0) = unsafe {
            (*self.current_figure_state)
                .as_ref()
                .expect("no figure started?")
        };

        for p in unsafe { core::slice::from_raw_parts(beziers, bezierscount as _) } {
            let from_p = unsafe { (*self.filltri_points).last().expect("no points emitted") };
            let bez = lyon_geom::CubicBezierSegment {
                from: lyon_geom::point(from_p[0], from_p[1]),
                ctrl1: lyon_geom::point(
                    p.point1.X * self.dip_to_pixels_scale + self.translate.X,
                    -p.point1.Y * self.dip_to_pixels_scale + self.translate.Y,
                ),
                ctrl2: lyon_geom::point(
                    p.point2.X * self.dip_to_pixels_scale + self.translate.X,
                    -p.point2.Y * self.dip_to_pixels_scale + self.translate.Y,
                ),
                to: lyon_geom::point(
                    p.point3.X * self.dip_to_pixels_scale + self.translate.X,
                    -p.point3.Y * self.dip_to_pixels_scale + self.translate.Y,
                ),
            };

            bez.for_each_quadratic_bezier(0.1, &mut |q| unsafe {
                let filltri_point1 = (*self.filltri_points).len() - 1;
                (*self.filltri_points).push([q.to.x, q.to.y]);
                (*self.filltri_indices).extend([
                    filltri_index0,
                    filltri_point1 as u16,
                    (*self.filltri_points).len() as u16 - 1,
                ]);

                (*self.curve_triangles).extend([
                    [q.from.x, q.from.y, 0.0, 0.0],
                    [q.ctrl.x, q.ctrl.y, 0.5, 0.0],
                    [q.to.x, q.to.y, 1.0, 1.0],
                ]);
            });
        }
    }

    fn Close(&self) -> windows_core::Result<()> {
        let &(ref start_point, filltri_index0) = unsafe {
            (*self.current_figure_state)
                .as_ref()
                .expect("no figure started?")
        };

        // line to start
        unsafe {
            let filltri_point1 = (*self.filltri_points).len() - 1;
            (*self.filltri_points).push([
                start_point.X * self.dip_to_pixels_scale + self.translate.X,
                start_point.Y * self.dip_to_pixels_scale + self.translate.Y,
            ]);
            (*self.filltri_indices).extend([
                filltri_index0,
                filltri_point1 as u16,
                (*self.filltri_points).len() as u16 - 1,
            ]);
        }

        Ok(())
    }

    fn SetFillMode(&self, fillmode: windows::Win32::Graphics::Direct2D::Common::D2D1_FILL_MODE) {
        if fillmode != D2D1_FILL_MODE_WINDING {
            tracing::warn!("not winding fill mode specified");
        }
    }

    fn SetSegmentFlags(
        &self,
        vertexflags: windows::Win32::Graphics::Direct2D::Common::D2D1_PATH_SEGMENT,
    ) {
        unimplemented!("SetSegmentFlags {vertexflags:?}")
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

#[cfg(feature = "wayland")]
struct WaylandGlobalMessaging {
    pub pointer: Option<wl::Owned<wl::Pointer>>,
    pub pointer_pos: (f32, f32),
    pub compositor: *mut wl::Compositor,
    pub wm_base: *mut wl::XdgWmBase,
    pub root_window: *mut wl::XdgSurface,
    pub popup_buf: *mut wl::Buffer,
    pub popup: Option<(
        wl::Owned<wl::XdgPopup>,
        wl::Owned<wl::XdgSurface>,
        wl::Owned<wl::Surface>,
        Box<WaylandPopupState>,
    )>,
    pub display: *mut wl::Display,
    _pinned: core::marker::PhantomPinned,
}
#[cfg(feature = "wayland")]
impl wl::XdgWmBaseEventListener for WaylandGlobalMessaging {
    #[inline(always)]
    fn ping(&mut self, sender: &mut peridot_tp_wayland::XdgWmBase, serial: u32) {
        sender.pong(serial).expect("xdg_wm_base pong");
    }
}
#[cfg(feature = "wayland")]
impl wl::SeatEventListener for WaylandGlobalMessaging {
    fn capabilities(&mut self, seat: &mut peridot_tp_wayland::Seat, capabilities: u32) {
        tracing::trace!(capabilities, "seat::capabilities");

        if (capabilities & 0x01) != 0 {
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
    }

    fn name(&mut self, seat: &mut peridot_tp_wayland::Seat, name: &core::ffi::CStr) {
        tracing::trace!(?name, "seat::name");
    }
}
#[cfg(feature = "wayland")]
impl wl::PointerEventListener for WaylandGlobalMessaging {
    #[tracing::instrument(skip(self, _pointer, surface), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn enter(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        serial: u32,
        surface: &mut peridot_tp_wayland::Surface,
        surface_x: peridot_tp_wayland::Fixed,
        surface_y: peridot_tp_wayland::Fixed,
    ) {
        tracing::trace!("pointer.enter");

        self.pointer_pos = (surface_x.to_f32(), surface_y.to_f32());
    }

    #[tracing::instrument(skip(self, _pointer, surface))]
    fn leave(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        serial: u32,
        surface: &mut peridot_tp_wayland::Surface,
    ) {
        tracing::trace!("pointer.leave");
    }

    #[tracing::instrument(skip(self, _pointer), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn motion(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        time: u32,
        surface_x: peridot_tp_wayland::Fixed,
        surface_y: peridot_tp_wayland::Fixed,
    ) {
        tracing::trace!("pointer.motion");

        self.pointer_pos = (surface_x.to_f32(), surface_y.to_f32());
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
        tracing::trace!("pointer.button");

        if state == wl::PointerButtonState::Pressed {
            let wl_popup_surface = unsafe {
                (*self.compositor)
                    .create_surface()
                    .expect("wl_popup_surface.create")
            };
            let mut xdg_popup_surface = unsafe {
                (*self.wm_base)
                    .get_xdg_surface(&wl_popup_surface)
                    .expect("xdg_popup_surface.create")
            };

            let pos = unsafe { (*self.wm_base).create_positioner().expect("pos.create") };
            pos.set_size(128, 128).expect("pos.set_size");
            pos.set_offset(self.pointer_pos.0 as _, self.pointer_pos.1 as _)
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
                // process configure event...
                (*self.display).roundtrip().expect("roundtrip");
            }
            unsafe {
                wl_popup_surface
                    .attach(Some(&*self.popup_buf), 0, 0)
                    .expect("wl_popup_surface.attach");
                wl_popup_surface
                    .damage(0, 0, -1, -1)
                    .expect("wl_popup_surface.damage");
                wl_popup_surface.commit().expect("wl_popup_surface.commit");
            }

            self.popup = Some((pp, xdg_popup_surface, wl_popup_surface, popup_state));
        } else if state == wl::PointerButtonState::Released {
            self.popup = None;
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
        tracing::trace!("pointer.frame");
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
impl wl::ZwlrLayerSurfaceV1EventListener for WaylandGlobalMessaging {
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

    #[tracing::instrument(skip(self, sender))]
    fn closed(&mut self, sender: &mut peridot_tp_wayland::ZwlrLayerSurfaceV1) {
        tracing::trace!("layer surface closed");
    }
}

#[cfg(feature = "wayland")]
pub struct WaylandWindow {
    surface: wl::Owned<wl::Surface>,
    xdg_surface: wl::Owned<wl::XdgSurface>,
    xdg_toplevel: wl::Owned<wl::XdgToplevel>,
    state: Box<WaylandWindowState>,
}
#[cfg(feature = "wayland")]
unsafe impl Sync for WaylandWindow {}
#[cfg(feature = "wayland")]
unsafe impl Send for WaylandWindow {}
#[cfg(feature = "wayland")]
impl WaylandWindow {
    pub fn initialize(&mut self) {
        self.surface
            .set_listener(&mut *self.state.as_mut())
            .into_result()
            .expect("wl_surface set listener");
        self.xdg_surface
            .set_listener(&mut *self.state.as_mut())
            .into_result()
            .expect("xdg_surface set listener");
        self.xdg_toplevel
            .set_listener(&mut *self.state.as_mut())
            .into_result()
            .expect("xdg_toplevel set listener");
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

pub enum Event {
    Quit,
}

struct EventQueue {
    event_store: *mut Option<Event>,
}
impl EventQueue {
    pub async fn next_event(&self) -> Event {
        EventQueueNextEventAwaiter { q: self }.await
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

async fn run(event_queue: EventQueue) {
    tracing::info!("app start");

    loop {
        match event_queue.next_event().await {
            Event::Quit => break,
        }
    }

    tracing::info!("app finish");
}

#[cfg(windows)]
extern "system" fn wndproc<AppFuture: core::future::Future<Output = ()>>(
    hwnd: HWND,
    msg: u32,
    wparam: WPARAM,
    lparam: LPARAM,
) -> LRESULT {
    let app_future = unsafe { GetWindowLongPtrW(hwnd, WINDOW_LONG_PTR_INDEX(0)) };
    let event_store = unsafe {
        GetWindowLongPtrW(
            hwnd,
            WINDOW_LONG_PTR_INDEX(core::mem::size_of::<usize>() as _),
        )
    };

    if msg == WM_DESTROY {
        unsafe {
            PostQuitMessage(0);
        }

        return LRESULT(0);
    }

    unsafe { DefWindowProcW(hwnd, msg, wparam, lparam) }
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
            swapchain_externally_invalidation_signal: std::sync::Arc::new(
                std::sync::atomic::AtomicBool::new(false),
            ),
            active_rt_size: std::sync::Mutex::new((960, 540)),
        });
        let callbacks: &'static WindowLinkCallbacks = &WindowLinkCallbacks {
            on_resize: MacWindowState::on_resize,
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
    swapchain_externally_invalidation_signal: std::sync::Arc<std::sync::atomic::AtomicBool>,
    active_rt_size: std::sync::Mutex<(u32, u32)>,
}
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
}

#[cfg(target_os = "macos")]
#[repr(C)]
pub struct WindowLinkCallbacks {
    pub on_resize: extern "C" fn(caller_context: *mut core::ffi::c_void, width: u32, height: u32),
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

    fn manual_capture_begin(window_link: *mut core::ffi::c_void);
    fn manual_capture_end();
}
