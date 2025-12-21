use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Device, Fence, FenceMut, Instance,
    PhysicalDevice, QueueMut, Swapchain, TypedVulkanStructure, VkHandle, VkHandleMut,
};
use core::pin::Pin;
use windows::{
    Win32::{
        Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
        Graphics::Gdi::HBRUSH,
        System::LibraryLoader::GetModuleHandleW,
        UI::WindowsAndMessaging::{
            CW_USEDEFAULT, CreateWindowExW, DefWindowProcW, DispatchMessageW, GetClientRect,
            GetMessageW, GetWindowLongPtrW, HCURSOR, IDI_APPLICATION, LoadIconW, PostQuitMessage,
            RegisterClassExW, SHOW_WINDOW_CMD, SW_SHOWNORMAL, SetWindowLongPtrW, ShowWindow,
            WINDOW_LONG_PTR_INDEX, WM_DESTROY, WNDCLASS_STYLES, WNDCLASSEXW, WS_EX_APPWINDOW,
            WS_OVERLAPPEDWINDOW,
        },
    },
    core::{BOOL, PCWSTR, w},
};

static APP_WAKER_VTABLE: core::task::RawWakerVTable = core::task::RawWakerVTable::new(
    |data| core::task::RawWaker::new(data, &APP_WAKER_VTABLE),
    |_| {},
    |_| {},
    |_| {},
);

fn main() {
    tracing_subscriber::fmt()
        .pretty()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
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
    let _ = app
        .as_mut()
        .poll(&mut core::task::Context::from_waker(&unsafe {
            core::task::Waker::new(&(), &APP_WAKER_VTABLE)
        }));

    let hinstance: HINSTANCE = unsafe { GetModuleHandleW(None).expect("GetModuleHandleW").into() };
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
    if atom == 0 {
        Err::<(), _>(std::io::Error::last_os_error()).expect("RegisterClassExW");
    }

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
    let mut w = Win32Window(w);

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

    if let Some(xs) = br::instance_extension_properties_cstr_alloc(None)
        .inspect_err(
            |e| tracing::error!(reason = ?e, "Failed to enumerate vulkan instance extensions"),
        )
        .ok()
    {
        for x in xs {
            tracing::info!(name = ?x.extensionName.as_cstr(), version = x.specVersion, "vulkan instance extension");
        }
    }

    if let Some(xs) = br::enumerate_layer_properties_alloc()
        .inspect_err(|e| tracing::error!(reason = ?e, "Failed to enumerate vulkan instance layers"))
        .ok()
    {
        for x in xs {
            tracing::info!(name = ?x.layerName.as_cstr(), version.impl = x.implementationVersion, version.spec = %br::Version::from_raw(x.specVersion), "vulkan instance layer");

            if let Some(ys) = x.layerName.as_cstr().ok().and_then(|ln| br::instance_extension_properties_cstr_alloc(Some(ln)).inspect_err(|e| tracing::error!(reason = ?e, "Failed to enumerate vulkan instance extensions for layer")).ok()) {
                for y in ys {
                    tracing::info!(name = ?y.extensionName.as_cstr(), version = y.specVersion, "vulkan instance extension on layer");
                }
            }
        }
    }

    let mut instance_extensions = vec![c"VK_KHR_surface".into(), c"VK_EXT_debug_utils".into()];
    #[cfg(windows)]
    instance_extensions.push(c"VK_KHR_win32_surface".into());
    let vk_instance = br::InstanceObject::new(&br::InstanceCreateInfo::new(
        &br::ApplicationInfo::new(
            c"Peridot Marble Editor",
            br::Version::new(0, 0, 0, 1),
            c"InHouse",
            br::Version::new(0, 0, 0, 1),
        )
        .api_version(br::Version::new(0, 1, 4, 0)),
        &[],
        &instance_extensions,
    ))
    .expect("vkInstance create");
    let vk_adapter = vk_instance
        .iter_physical_devices()
        .expect("iter_physical_devices")
        .next()
        .expect("no physical devices");

    if let Some(xs) = vk_adapter
        .enumerate_extension_properties_cstr_alloc(None)
        .inspect_err(
            |e| tracing::error!(reason = ?e, "Failed to enumerate vulkan device extensions"),
        )
        .ok()
    {
        for x in xs {
            tracing::info!(name = ?x.extensionName.as_cstr(), version = x.specVersion, "vulkan device extension");
        }
    }

    if let Some(xs) = vk_adapter
        .enumerate_layer_properties_alloc()
        .inspect_err(|e| tracing::error!(reason = ?e, "Failed to enumerate vulkan device layers"))
        .ok()
    {
        for x in xs {
            tracing::info!(name = ?x.layerName.as_cstr(), version.impl = x.implementationVersion, version.spec = %br::Version::from_raw(x.specVersion), "vulkan device layer");

            if let Some(ys) = x.layerName.as_cstr().ok().and_then(|ln| vk_adapter.enumerate_extension_properties_cstr_alloc(Some(ln)).inspect_err(|e| tracing::error!(reason = ?e, "Failed to enumerate vulkan instance extensions for layer")).ok()) {
                for y in ys {
                    tracing::info!(name = ?y.extensionName.as_cstr(), version = y.specVersion, "vulkan device extension on layer");
                }
            }
        }
    }

    let vk_adapter_queue_family_properties = vk_adapter.queue_family_properties_alloc();
    let graphics_queue_family_index = vk_adapter_queue_family_properties
        .find_matching_index(br::QueueFlags::GRAPHICS)
        .expect("no graphics queue");
    let vk_device = br::DeviceObject::new(
        &vk_adapter,
        &br::DeviceCreateInfo::new(
            &[br::DeviceQueueCreateInfo::new(
                graphics_queue_family_index,
                &[0.0],
            )],
            &[],
            &[
                c"VK_KHR_swapchain".into(),
                c"VK_KHR_timeline_semaphore".into(),
            ],
        )
        .with_next(
            &br::PhysicalDeviceFeatures2::new(unsafe {
                core::mem::MaybeUninit::<br::PhysicalDeviceFeatures>::zeroed().assume_init()
            })
            .with_next(
                &mut br::PhysicalDeviceSynchronization2Features::new(true).with_next(
                    &mut br::vk::VkPhysicalDeviceTimelineSemaphoreFeaturesKHR {
                        sType: br::vk::VkPhysicalDeviceTimelineSemaphoreFeaturesKHR::TYPE,
                        pNext: core::ptr::null_mut(),
                        timelineSemaphore: 1,
                    },
                ),
            ),
        ),
    )
    .expect("vk_device create");

    if !vk_adapter.win32_presentation_support(graphics_queue_family_index) {
        panic!("win32 presentation not supported on graphics queue");
    }
    let vk_surface = unsafe {
        br::SurfaceObject::new(
            &vk_adapter,
            &br::Win32SurfaceCreateInfo::new(
                core::mem::transmute(hinstance),
                core::mem::transmute(w.0),
            ),
        )
        .expect("vk_surface create")
    };
    if !vk_adapter
        .surface_support(graphics_queue_family_index, &vk_surface)
        .expect("surface_support")
    {
        panic!("surface not supported on graphics queue");
    }

    std::thread::scope({
        let w = &w;
        move |thread_scope| {
            let shutdown = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));

            let render_thread = std::thread::Builder::new()
                .name("Render".into())
                .spawn_scoped(thread_scope, {
                    let shutdown = shutdown.clone();

                    move || {
                        let mut render_queue = vk_device.queue(graphics_queue_family_index, 0);

                        let present_modes = vk_adapter
                            .surface_present_modes_alloc(&vk_surface)
                            .expect("surface_present_modes");
                        let surface_caps = vk_adapter
                            .surface_capabilities(&vk_surface)
                            .expect("surface_capabilities");
                        let surface_formats = vk_adapter
                            .surface_formats_alloc(&vk_surface)
                            .expect("surface_formats");
                        let mut surface_ext = if surface_caps.currentExtent.width == 0xffffffff
                            || surface_caps.currentExtent.height == 0xffffffff
                        {
                            let (cw, ch) = w.client_size();

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
                        let surface_present_mode = present_modes
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
                        let mut backbuffer_image_views = vk_swapchain
                            .images_alloc()
                            .expect("backbuffer images")
                            .into_iter()
                            .map(|b| LocalImageView {
                                handle: unsafe {
                                    br::vkfn_wrapper::create_image_view(
                                        vk_device.native_ptr(),
                                        &br::ImageViewCreateInfo::new(
                                            &b,
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
                                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)
                                    .layout_transition(
                                        br::ImageLayout::Undefined,
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

                        let mut render_cp = br::CommandPoolObject::new(
                            &vk_device,
                            &br::CommandPoolCreateInfo::new(graphics_queue_family_index),
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
                        for (cb, fb) in render_commands.iter_mut().zip(vk_framebuffers.iter()) {
                            unsafe {
                                cb.begin(&br::CommandBufferBeginInfo::new())
                                    .expect("command buffer begin")
                            }
                            .begin_render_pass(
                                &br::RenderPassBeginInfo::new(
                                    &vk_render_pass,
                                    fb,
                                    surface_ext.into_rect(br::Offset2D::ZERO),
                                    &[br::ClearValue::color_f32([0.1, 0.2, 0.3, 1.0])],
                                ),
                                br::SubpassContents::Inline,
                            )
                            .end_render_pass()
                            .end()
                            .expect("command buffer end");
                        }

                        let present_ready_semaphores = (0..vk_framebuffers.len())
                            .map(|_| {
                                br::SemaphoreObject::new(
                                    &vk_device,
                                    &br::SemaphoreCreateInfo::new(),
                                )
                                .expect("rendering_timeline_semaphore create")
                            })
                            .collect::<Vec<_>>();
                        let mut backbuffer_ready_fence =
                            br::FenceObject::new(&vk_device, &br::FenceCreateInfo::new(0))
                                .expect("last render completion fence create");
                        let mut swapchain_invalidated = false;

                        'lp: while !shutdown.load(std::sync::atomic::Ordering::Acquire) {
                            if swapchain_invalidated {
                                let x = std::time::Instant::now();
                                render_queue.wait().expect("waiting pending queue works");
                                tracing::trace!(elapsed = ?x.elapsed(), "queue waiting time during resize");

                                unsafe {
                                    render_cp
                                        .reset(br::CommandPoolResetFlags::EMPTY)
                                        .expect("reset render cp");
                                }
                                drop(vk_framebuffers);
                                drop(backbuffer_image_views);

                                let surface_caps = vk_adapter
                                    .surface_capabilities(&vk_surface)
                                    .expect("surface_capabilities");
                                surface_ext = if surface_caps.currentExtent.width == 0xffffffff
                                    || surface_caps.currentExtent.height == 0xffffffff
                                {
                                    let (cw, ch) = w.client_size();

                                    br::Extent2D {
                                        width: if surface_caps.currentExtent.width == 0xffffffff
                                        {
                                            cw
                                        } else {
                                            surface_caps.currentExtent.width
                                        },
                                        height: if surface_caps.currentExtent.height
                                            == 0xffffffff
                                        {
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
                                backbuffer_image_views = vk_swapchain
                                    .images_alloc()
                                    .expect("backbuffer images")
                                    .into_iter()
                                    .map(|b| LocalImageView {
                                        handle: unsafe {
                                            br::vkfn_wrapper::create_image_view(
                                                vk_device.native_ptr(),
                                                &br::ImageViewCreateInfo::new(
                                                    &b,
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

                                for (cb, fb) in
                                    render_commands.iter_mut().zip(vk_framebuffers.iter())
                                {
                                    unsafe {
                                        cb.begin(&br::CommandBufferBeginInfo::new())
                                            .expect("command buffer begin")
                                    }
                                    .begin_render_pass(
                                        &br::RenderPassBeginInfo::new(
                                            &vk_render_pass,
                                            fb,
                                            surface_ext.into_rect(br::Offset2D::ZERO),
                                            &[br::ClearValue::color_f32([0.1, 0.2, 0.3, 1.0])],
                                        ),
                                        br::SubpassContents::Inline,
                                    )
                                    .end_render_pass()
                                    .end()
                                    .expect("command buffer end");
                                }

                                swapchain_invalidated =false;
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

                            unsafe {
                                render_queue
                                    .submit_raw(
                                        &[br::SubmitInfo::new(
                                            &[],
                                            &[],
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
                                &[present_ready_semaphores[backbuffer_index as usize]
                                    .as_transparent_ref()],
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
                        }

                        unsafe {
                            vk_device.wait().expect("device wait");
                        }
                    }
                })
                .expect("render_thread spawn");

            w.show(SW_SHOWNORMAL);

            let mut msg = core::mem::MaybeUninit::uninit();
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
        }
    });
}

struct LocalImageView<'d, 'i> {
    handle: br::vk::VkImageView,
    device: &'d br::DeviceObject<&'i br::InstanceObject>,
}
impl Drop for LocalImageView<'_, '_> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(self.device.native_ptr(), self.handle, None);
        }
    }
}
impl br::VkHandle for LocalImageView<'_, '_> {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

#[repr(transparent)]
pub struct Win32Window(HWND);
unsafe impl Sync for Win32Window {}
unsafe impl Send for Win32Window {}
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
