use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, DescriptorPoolMut, Device, DeviceMemoryMut, Fence, FenceMut, ImageChild, Instance, MemoryBound, PhysicalDevice, QueueMut, RenderPass, ShaderModule, Swapchain, TypedVulkanStructure, VkHandle, VkHandleMut, VkObject
};
use core::pin::Pin;
use std::{cell::UnsafeCell, collections::HashMap};
use windows::Win32::{
    Foundation::{HINSTANCE, HWND, LPARAM, LRESULT, WPARAM},
    Graphics::{
        Direct2D::Common::{D2D1_FIGURE_BEGIN_FILLED, D2D1_FIGURE_END_CLOSED, D2D1_FILL_MODE, D2D1_FILL_MODE_WINDING, ID2D1SimplifiedGeometrySink, ID2D1SimplifiedGeometrySink_Impl}, DirectWrite::{
            DWRITE_FACTORY_TYPE_SHARED, DWRITE_FONT_STRETCH_NORMAL, DWRITE_FONT_STYLE_NORMAL,
            DWRITE_FONT_WEIGHT_NORMAL, DWRITE_GLYPH_METRICS, DWriteCreateFactory, IDWriteFactory,
            IDWritePixelSnapping_Impl, IDWriteTextRenderer, IDWriteTextRenderer_Impl,
        }, Gdi::HBRUSH
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
use windows_core::*;

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
            tracing::info!(
                name = ?x.layerName.as_cstr(),
                version.impl = x.implementationVersion,
                version.spec = %br::Version::from_raw(x.specVersion),
                "vulkan instance layer"
            );

            if let Some(ys) = x.layerName.as_cstr().ok().and_then(|ln| {
                br::instance_extension_properties_cstr_alloc(Some(ln))
                    .inspect_err(|e| {
                        tracing::error!(
                            reason = ?e,
                            "Failed to enumerate vulkan instance extensions for layer"
                        )
                    })
                    .ok()
            }) {
                for y in ys {
                    tracing::info!(
                        name = ?y.extensionName.as_cstr(),
                        version = y.specVersion,
                        "vulkan instance extension on layer"
                    );
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
            tracing::info!(
                name = ?x.layerName.as_cstr(),
                version.impl = x.implementationVersion,
                version.spec = %br::Version::from_raw(x.specVersion),
                "vulkan device layer"
            );

            if let Some(ys) = x.layerName.as_cstr().ok().and_then(|ln| {
                vk_adapter
                    .enumerate_extension_properties_cstr_alloc(Some(ln))
                    .inspect_err(|e| {
                        tracing::error!(
                            reason = ?e,
                            "Failed to enumerate vulkan instance extensions for layer"
                        )
                    })
                    .ok()
            }) {
                for y in ys {
                    tracing::info!(
                        name = ?y.extensionName.as_cstr(),
                        version = y.specVersion,
                        "vulkan device extension on layer"
                    );
                }
            }
        }
    }

    let vk_adapter_memory_properties = vk_adapter.memory_properties();
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

                        let mut glyph_atlas = GlyphAtlas::new(&vk_device, &vk_adapter_memory_properties);

                        let dwfactory: IDWriteFactory = unsafe { DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED).expect("DWriteCreateFactory") };
                        let ui_text_format = unsafe { dwfactory.CreateTextFormat(w!("system-ui"), None, DWRITE_FONT_WEIGHT_NORMAL, DWRITE_FONT_STYLE_NORMAL, DWRITE_FONT_STRETCH_NORMAL, 10.0, w!("ja-JP")).expect("CreateTextFormat ui") };

                        let title_layout = unsafe { dwfactory.CreateTextLayout(&"Peridot Marble Editor".encode_utf16().collect::<Vec<_>>(), &ui_text_format, f32::MAX, f32::MAX).expect("CreateTextLayout title") };
                        let mut box_instances = Vec::new();
                        let mut new_filltri_points = Vec::new();
                        let mut new_filltri_indices = Vec::new();
                        let mut new_curve_triangles = Vec::new();
                        let renderer = IDWriteTextRenderer::from(AtlasTextRenderer {
                            box_instances: &mut box_instances,
                            atlas: &mut glyph_atlas,
                            new_filltri_points: &mut new_filltri_points,
                            new_filltri_indices: &mut new_filltri_indices,
                            new_curve_triangles: &mut new_curve_triangles,
                        });
                        unsafe { title_layout.Draw(None, &renderer, 0.0, 0.0).expect("title_layout.Draw"); }

                        #[derive(br::SpecializationConstants)]
                        struct FillShaderVertexConstants {
                            #[constant_id = 0]
                            target_texture_width: f32,
                            #[constant_id = 1]
                            target_texture_height: f32
                        }
                        let fill_shader_binary1 = std::fs::read("./resources/vg-fill.spv").expect("vg-fill load");
                        let mut fill_shader_binary = Vec::with_capacity(fill_shader_binary1.len() >> 2);
                        unsafe { core::ptr::copy_nonoverlapping(fill_shader_binary1.as_ptr(), fill_shader_binary.spare_capacity_mut().as_mut_ptr().cast::<u8>(), fill_shader_binary1.len()); }
                        unsafe { fill_shader_binary.set_len(fill_shader_binary1.len() >> 2); }
                        let fill_shader_module = br::ShaderModuleObject::new(&vk_device, &br::ShaderModuleCreateInfo::new(&fill_shader_binary)).expect("fill_shader module create");

                        #[derive(br::SpecializationConstants)]
                        struct CurveShaderVertexConstants {
                            #[constant_id = 0]
                            target_texture_width: f32,
                            #[constant_id = 1]
                            target_texture_height: f32
                        }
                        let curve_shader_binary1 = std::fs::read("./resources/vg-curve.spv").expect("vg-curve load");
                        let mut curve_shader_binary = Vec::with_capacity(curve_shader_binary1.len() >> 2);
                        unsafe { core::ptr::copy_nonoverlapping(curve_shader_binary1.as_ptr(), curve_shader_binary.spare_capacity_mut().as_mut_ptr().cast::<u8>(), curve_shader_binary1.len()); }
                        unsafe { curve_shader_binary.set_len(curve_shader_binary1.len() >> 2); }
                        let curve_shader_module = br::ShaderModuleObject::new(&vk_device, &br::ShaderModuleCreateInfo::new(&curve_shader_binary)).expect("curve_shader module create");

                        let vec_tri_fill_shader_binary1 = std::fs::read("./resources/vec-tri-fill.spv").expect("vec-tri-fill load");
                        let mut vec_tri_fill_shader_binary = Vec::with_capacity(vec_tri_fill_shader_binary1.len() >> 2);
                        unsafe {
                            core::ptr::copy_nonoverlapping(vec_tri_fill_shader_binary1.as_ptr(), vec_tri_fill_shader_binary.spare_capacity_mut().as_mut_ptr().cast::<u8>(), vec_tri_fill_shader_binary1.len());
                            vec_tri_fill_shader_binary.set_len(vec_tri_fill_shader_binary1.len() >> 2);
                        }
                        let vec_tri_fill_shader_module = br::ShaderModuleObject::new(&vk_device, &br::ShaderModuleCreateInfo::new(&vec_tri_fill_shader_binary)).expect("vec_tri_fill_shader module create");

                        let vector_render_pass = br::RenderPassObject::new(&vk_device, &br::RenderPassCreateInfo2::new(
                            &[
                                br::AttachmentDescription2::new(br::vk::VK_FORMAT_R8_UNORM)
                                    .color_memory_op(br::LoadOp::Load, br::StoreOp::Store)
                                    .layout_transition(br::ImageLayout::ShaderReadOnlyOpt, br::ImageLayout::ShaderReadOnlyOpt),
                                br::AttachmentDescription2::new(br::vk::VK_FORMAT_S8_UINT)
                                    .stencil_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
                                    .layout_transition(br::ImageLayout::Undefined, br::ImageLayout::DepthStencilReadOnlyOpt)
                                    .samples(GlyphAtlas::MULTISAMPLE_LEVEL),
                                br::AttachmentDescription2::new(br::vk::VK_FORMAT_R8_UNORM)
                                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)
                                    .layout_transition(br::ImageLayout::Undefined, br::ImageLayout::ColorAttachmentOpt)
                                    .samples(GlyphAtlas::MULTISAMPLE_LEVEL),
                            ],
                            &[
                                br::SubpassDescription2::new()
                                    .depth_stencil(&br::AttachmentReference2::depth_stencil_attachment_opt(1)),
                                br::SubpassDescription2::new()
                                    .depth_stencil(&br::AttachmentReference2::depth_stencil_readonly_opt(1))
                                    .colors(&[br::AttachmentReference2::color_attachment_opt(2)])
                                    .color_resolves(&[br::AttachmentReference2::color_attachment_opt(0)])
                            ],
                            &[
                                br::SubpassDependency2::new(br::SubpassIndex::Internal(0), br::SubpassIndex::Internal(1))
                                    .by_region()
                                    .of_memory(br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write, br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.read)
                                    .of_execution(br::PipelineStageFlags::LATE_FRAGMENT_TESTS, br::PipelineStageFlags::EARLY_FRAGMENT_TESTS),
                                br::SubpassDependency2::new(br::SubpassIndex::Internal(1), br::SubpassIndex::External)
                                    .by_region()
                                    .of_memory(br::AccessFlags::COLOR_ATTACHMENT.write, br::AccessFlags::SHADER.read)
                                    .of_execution(br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT, br::PipelineStageFlags::FRAGMENT_SHADER)
                            ]
                        )).expect("vector render pass create");

                        let pipeline_layout = br::PipelineLayoutObject::new(&vk_device, &br::PipelineLayoutCreateInfo::new(&[], &[])).expect("vector pipeline layout create");
                        let [triangle_fans_pipeline, curve_pipeline, colorize_pipeline] = vk_device.new_graphics_pipeline_array(&[
                            br::GraphicsPipelineCreateInfo::new(
                                &pipeline_layout,
                                vector_render_pass.subpass(0),
                                &[
                                    fill_shader_module.on_stage(br::ShaderStage::Vertex, c"vertMain")
                                        .with_specialization_info(&br::SpecializationInfo::new(&FillShaderVertexConstants {
                                            target_texture_width: glyph_atlas.space_mgr.max.width as _,
                                            target_texture_height: glyph_atlas.space_mgr.max.height as _
                                        })),
                                    fill_shader_module.on_stage(br::ShaderStage::Fragment, c"fragMain")
                                ],
                                &br::PipelineVertexInputStateCreateInfo::new(
                                    &[br::VertexInputBindingDescription::per_vertex_typed::<[f32; 2]>(0)],
                                    &[br::VertexInputAttributeDescription {
                                        location: 0,
                                        binding: 0,
                                        offset: 0,
                                        format: br::vk::VK_FORMAT_R32G32_SFLOAT
                                    }]
                                ),
                                &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleList),
                                &br::PipelineViewportStateCreateInfo::new(
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO).make_viewport(0.0..1.0)],
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO)]
                                ),
                                &br::PipelineRasterizationStateCreateInfo::new(
                                    br::PolygonMode::Fill, br::CullModeFlags::NONE, br::FrontFace::CounterClockwise
                                ),
                                &br::PipelineColorBlendStateCreateInfo::new(
                                    &[br::vk::VkPipelineColorBlendAttachmentState::NOBLEND]
                                )
                            ).set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new().rasterization_samples(GlyphAtlas::MULTISAMPLE_LEVEL as _))
                            .set_depth_stencil_state(
                                &br::PipelineDepthStencilStateCreateInfo::new()
                                    .stencil_test(true)
                                    .stencil_state_front(br::vk::VkStencilOpState::always_forall(br::StencilOp::Invert).write_mask(0x01))
                                    .stencil_state_back(br::vk::VkStencilOpState::always_forall(br::StencilOp::Invert).write_mask(0x01))
                            ),
                            br::GraphicsPipelineCreateInfo::new(
                                &pipeline_layout,
                                vector_render_pass.subpass(0),
                                &[
                                    curve_shader_module.on_stage(br::ShaderStage::Vertex, c"vertMain")
                                        .with_specialization_info(&br::SpecializationInfo::new(&CurveShaderVertexConstants {
                                            target_texture_width: glyph_atlas.space_mgr.max.width as _,
                                            target_texture_height: glyph_atlas.space_mgr.max.height as _
                                        })),
                                    curve_shader_module.on_stage(br::ShaderStage::Fragment, c"fragMain")
                                ],
                                &br::PipelineVertexInputStateCreateInfo::new(
                                    &[br::VertexInputBindingDescription::per_vertex_typed::<[f32; 4]>(0)],
                                    &[
                                        br::VertexInputAttributeDescription {
                                            location: 0,
                                            binding: 0,
                                            offset: 0,
                                            format: br::vk::VK_FORMAT_R32G32_SFLOAT
                                        },
                                        br::VertexInputAttributeDescription {
                                            location: 1,
                                            binding: 0,
                                            offset: core::mem::size_of::<[f32; 2]>() as _,
                                            format: br::vk::VK_FORMAT_R32G32_SFLOAT
                                        }
                                    ]
                                ),
                                &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleList),
                                &br::PipelineViewportStateCreateInfo::new(
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO).make_viewport(0.0..1.0)],
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO)]
                                ),
                                &br::PipelineRasterizationStateCreateInfo::new(
                                    br::PolygonMode::Fill, br::CullModeFlags::NONE, br::FrontFace::CounterClockwise
                                ),
                                &br::PipelineColorBlendStateCreateInfo::new(
                                    &[br::vk::VkPipelineColorBlendAttachmentState::NOBLEND]
                                )
                            ).set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new().rasterization_samples(GlyphAtlas::MULTISAMPLE_LEVEL as _))
                            .set_depth_stencil_state(
                                &br::PipelineDepthStencilStateCreateInfo::new()
                                    .stencil_test(true)
                                    .stencil_state_front(br::StencilOpState::always_forall(br::StencilOp::Invert).write_mask(0x01))
                                    .stencil_state_back(br::StencilOpState::always_forall(br::StencilOp::Invert).write_mask(0x01))
                            ),
                            br::GraphicsPipelineCreateInfo::new(
                                &pipeline_layout,
                                vector_render_pass.subpass(1),
                                &[
                                    vec_tri_fill_shader_module.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                                    vec_tri_fill_shader_module.on_stage(br::ShaderStage::Fragment, c"fragMain")
                                ],
                                &br::PipelineVertexInputStateCreateInfo::new(&[], &[]),
                                &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleList),
                                &br::PipelineViewportStateCreateInfo::new(
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO).make_viewport(0.0..1.0)],
                                    &[glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO)]
                                ),
                                &br::PipelineRasterizationStateCreateInfo::new(
                                    br::PolygonMode::Fill, br::CullModeFlags::NONE, br::FrontFace::CounterClockwise
                                ),
                                &br::PipelineColorBlendStateCreateInfo::new(
                                    &[br::vk::VkPipelineColorBlendAttachmentState::NOBLEND]
                                )
                            ).set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new().rasterization_samples(GlyphAtlas::MULTISAMPLE_LEVEL as _))
                            .set_depth_stencil_state(
                                &br::PipelineDepthStencilStateCreateInfo::new()
                                    .stencil_test(true)
                                    .stencil_state_front(br::StencilOpState::NOP.set_compare(br::CompareOp::Equal, 0x01, 0x01))
                                    .stencil_state_back(br::StencilOpState::NOP.set_compare(br::CompareOp::Equal, 0x01, 0x01))
                            )
                        ], None::<&br::PipelineCacheObject<&br::DeviceObject<&br::InstanceObject>>>).expect("create vector rasterize pipelines");

                        let filltri_points_offset = 0;
                        let filltri_indices_offset = filltri_points_offset + core::mem::size_of_val(&new_filltri_points[..]);
                        let curve_triangles_offset = (filltri_indices_offset + core::mem::size_of_val(&new_filltri_indices[..]) + (core::mem::size_of::<[f32; 4]>() - 1)) & !(core::mem::size_of::<[f32; 4]>() - 1);
                        let vector_draw_buffer_total_size = curve_triangles_offset + core::mem::size_of_val(&new_curve_triangles[..]);
                        let mut vector_draw_buffer = br::BufferObject::new(&vk_device, &br::BufferCreateInfo::new(vector_draw_buffer_total_size, br::BufferUsage::VERTEX_BUFFER | br::BufferUsage::INDEX_BUFFER | br::BufferUsage::TRANSFER_DEST)).expect("vector_draw_buffer create");
                        let vector_draw_buffer_memreq = vector_draw_buffer.requirements();
                        let vector_draw_buffer_memory = br::DeviceMemoryObject::new(&vk_device, &br::MemoryAllocateInfo::new(vector_draw_buffer_memreq.size, vk_adapter_memory_properties.find_device_local_index(vector_draw_buffer_memreq.memoryTypeBits).expect("no suitable memory"))).expect("vector_draw_buffer malloc");
                        vector_draw_buffer.bind(&vector_draw_buffer_memory, 0).expect("vector_draw_buffer bind");

                        let mut vector_draw_init_buffer = br::BufferObject::new(&vk_device, &br::BufferCreateInfo::new(vector_draw_buffer_total_size, br::BufferUsage::TRANSFER_SRC)).expect("vector_draw_init_buffer create");
                        let vector_draw_init_buffer_memreq = vector_draw_init_buffer.requirements();
                        let vector_draw_init_buffer_memindex = vk_adapter_memory_properties.find_host_visible_index(vector_draw_init_buffer_memreq.memoryTypeBits).expect("no suitable memory");
                        let mut vector_draw_init_buffer_memory = br::DeviceMemoryObject::new(&vk_device, &br::MemoryAllocateInfo::new(vector_draw_init_buffer_memreq.size, vector_draw_init_buffer_memindex)).expect("vector_draw_init_buffer malloc");
                        vector_draw_init_buffer.bind(&vector_draw_init_buffer_memory, 0).expect("vector_draw_init_buffer bind");
                        let p = vector_draw_init_buffer_memory.map(0..vector_draw_buffer_total_size).expect("vector_draw_init_buffer_memory map");
                        unsafe {
                            core::ptr::copy_nonoverlapping(new_filltri_points.as_ptr(), p.ptr().byte_add(filltri_points_offset).cast(), new_filltri_points.len());
                            core::ptr::copy_nonoverlapping(new_filltri_indices.as_ptr(), p.ptr().byte_add(filltri_indices_offset).cast(), new_filltri_indices.len());
                            core::ptr::copy_nonoverlapping(new_curve_triangles.as_ptr(), p.ptr().byte_add(curve_triangles_offset).cast(), new_curve_triangles.len());
                        }
                        if !vk_adapter_memory_properties.is_coherent(vector_draw_init_buffer_memindex) {
                            unsafe {
                                vk_device.flush_mapped_memory_ranges(&[br::MappedMemoryRange::new(&vector_draw_init_buffer_memory, 0..vector_draw_buffer_total_size as u64)]).expect("flush_mapped_memory_ranges");
                            }
                        }
                        unsafe { vector_draw_init_buffer_memory.unmap(); }

                        let mut vector_color_ms_buffer = br::ImageObject::new(&vk_device, &br::ImageCreateInfo::new(glyph_atlas.space_mgr.max, br::vk::VK_FORMAT_R8_UNORM).set_usage(br::ImageUsageFlags::COLOR_ATTACHMENT | br::ImageUsageFlags::TRANSIENT_ATTACHMENT).sample_counts(GlyphAtlas::MULTISAMPLE_LEVEL)).expect("vector color_ms buffer create");
                        let vector_color_ms_buffer_memreq = vector_color_ms_buffer.requirements();
                        let vector_color_ms_buffer_mem = br::DeviceMemoryObject::new(&vk_device, &br::MemoryAllocateInfo::new(vector_color_ms_buffer_memreq.size, vk_adapter_memory_properties.find_lazily_allocated_device_local_index(vector_color_ms_buffer_memreq.memoryTypeBits).or_else(|| vk_adapter_memory_properties.find_device_local_index(vector_color_ms_buffer_memreq.memoryTypeBits)).expect("no suitable memory"))).expect("vector color_ms buffer malloc");
                        vector_color_ms_buffer.bind(&vector_color_ms_buffer_mem, 0).expect("vector color_ms buffer bind");
                        let vector_color_ms_buffer = br::ImageViewBuilder::new(vector_color_ms_buffer, br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1)).create().expect("vector color_ms buffer imageview create");
                        let mut vector_stencil_buffer = br::ImageObject::new(&vk_device, &br::ImageCreateInfo::new(glyph_atlas.space_mgr.max, br::vk::VK_FORMAT_S8_UINT).set_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT | br::ImageUsageFlags::TRANSIENT_ATTACHMENT).sample_counts(GlyphAtlas::MULTISAMPLE_LEVEL)).expect("vector stencil buffer create");
                        let vector_stencil_buffer_memreq = vector_stencil_buffer.requirements();
                        let vector_stencil_buffer_mem = br::DeviceMemoryObject::new(&vk_device, &br::MemoryAllocateInfo::new(vector_stencil_buffer_memreq.size, vk_adapter_memory_properties.find_lazily_allocated_device_local_index(vector_stencil_buffer_memreq.memoryTypeBits).or_else(|| vk_adapter_memory_properties.find_device_local_index(vector_stencil_buffer_memreq.memoryTypeBits)).expect("no suitable memory"))).expect("vector stencil buffer malloc");
                        vector_stencil_buffer.bind(&vector_stencil_buffer_mem, 0).expect("vector stencil buffer bind");
                        let vector_stencil_buffer = br::ImageViewBuilder::new(vector_stencil_buffer, br::ImageSubresourceRange::new(br::AspectMask::STENCIL, 0..1, 0..1)).create().expect("vector stencil buffer imageview create");
                        let vector_framebuffer = br::FramebufferObject::new(&vk_device, &br::FramebufferCreateInfo::new(&vector_render_pass, &[glyph_atlas.view(), vector_stencil_buffer.as_transparent_ref(), vector_color_ms_buffer.as_transparent_ref()], glyph_atlas.space_mgr.max.width, glyph_atlas.space_mgr.max.height)).expect("vector framebuffer create");

                        let mut cp = br::CommandPoolObject::new(&vk_device, &br::CommandPoolCreateInfo::new(graphics_queue_family_index)).expect("cp init");
                        let mut cb = br::CommandBufferObject::alloc(&vk_device, &br::CommandBufferAllocateInfo::new(&mut cp, 1, br::CommandBufferLevel::Primary)).expect("alloc cb");
                        unsafe { cb[0].begin(&br::CommandBufferBeginInfo::new()).expect("cb begin") }
                            .pipeline_barrier(
                                br::PipelineStageFlags(0),
                                br::PipelineStageFlags::TRANSFER,
                                0,
                                &[],
                                &[],
                                &[
                                    br::ImageMemoryBarrier::new(&glyph_atlas.image(), br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1), br::ImageLayout::TransferDestOpt.from_undefined()),
                                ]
                            )
                            .copy_buffer(&vector_draw_init_buffer, &vector_draw_buffer, &[
                                br::BufferCopy::mirror(0, vector_draw_buffer_total_size as _)
                            ])
                            .clear_color_image(&glyph_atlas.image(), br::ImageLayout::TransferDestOpt, &[br::ClearColorValue::from([0.0; 4])], &[br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1)])
                            .pipeline_barrier(
                                br::PipelineStageFlags::TRANSFER,
                                br::PipelineStageFlags::VERTEX_INPUT | br::PipelineStageFlags::FRAGMENT_SHADER,
                                0,
                                &[br::vk::VkMemoryBarrier {
                                    sType: br::vk::VkMemoryBarrier::TYPE,
                                    pNext: core::ptr::null(),
                                    srcAccessMask: br::AccessFlags::TRANSFER.write,
                                    dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::INDEX_READ
                                }],
                                &[],
                                &[
                                    br::ImageMemoryBarrier::new(&glyph_atlas.image(), br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1), br::ImageLayout::TransferDestOpt.to(br::ImageLayout::ShaderReadOnlyOpt)),
                                ]
                            )
                            .begin_render_pass(&br::RenderPassBeginInfo::new(
                                &vector_render_pass,
                                &vector_framebuffer,
                                glyph_atlas.space_mgr.max.into_rect(br::Offset2D::ZERO),
                                &[br::ClearValue::color_f32([0.0; 4]), br::ClearValue::depth_stencil(1.0, 0), br::ClearValue::color_f32([0.0; 4])]
                            ), br::SubpassContents::Inline)
                            .bind_pipeline(br::PipelineBindPoint::Graphics, &triangle_fans_pipeline)
                            .bind_vertex_buffer_array(0, &[vector_draw_buffer.as_transparent_ref()], &[filltri_points_offset as _])
                            .bind_index_buffer(&vector_draw_buffer, filltri_indices_offset, br::IndexType::U16)
                            .draw_indexed(new_filltri_indices.len() as _, 1, 0, 0, 0)
                            .bind_pipeline(br::PipelineBindPoint::Graphics, &curve_pipeline)
                            .bind_vertex_buffer_array(0, &[vector_draw_buffer.as_transparent_ref()], &[curve_triangles_offset as _])
                            .draw(new_curve_triangles.len() as _, 1, 0, 0)
                            .next_subpass(br::SubpassContents::Inline)
                            .bind_pipeline(br::PipelineBindPoint::Graphics, &colorize_pipeline)
                            .draw(3, 1, 0, 0)
                            .end_render_pass()
                        .end().expect("cb end");
                        unsafe { render_queue.submit_raw(&[br::SubmitInfo::new(&[], &[], &[cb[0].as_transparent_ref()], &[])], None).expect("vector render submit"); }
                        render_queue.wait().expect("vector render wait");

                        let vertex_offset = 0;
                        let instance_data_offset = vertex_offset + core::mem::size_of::<[[f32; 4]; 4]>();
                        let total_size = instance_data_offset + core::mem::size_of::<BoxInstance>() * box_instances.len();
                        let mut draw_buffer = br::BufferObject::new(&vk_device, &br::BufferCreateInfo::new(total_size, br::BufferUsage::VERTEX_BUFFER | br::BufferUsage::TRANSFER_DEST)).expect("draw_buffer create");
                        let draw_buffer_memory_requirements = draw_buffer.requirements();
                        let draw_buffer_memory = br::DeviceMemoryObject::new(&vk_device, &br::MemoryAllocateInfo::new(draw_buffer_memory_requirements.size, vk_adapter_memory_properties.find_device_local_index(draw_buffer_memory_requirements.memoryTypeBits).expect("no suitable memory"))).expect("draw_buffer memalloc");
                        draw_buffer.bind(&draw_buffer_memory, 0).expect("draw_buffer bind memory");

                        struct DrawBufferInitContent {
                            pos01: [[f32; 4]; 4],
                            instance_data: [BoxInstance; 0]
                        }
                        let init_size = core::mem::offset_of!(DrawBufferInitContent, instance_data) + core::mem::size_of::<BoxInstance>() * box_instances.len();
                        let mut init_draw_buffer = br::BufferObject::new(&vk_device, &br::BufferCreateInfo::new(init_size, br::BufferUsage::TRANSFER_SRC)).expect("init_draw_buffer create");
                        let init_draw_buffer_memory_requirements = init_draw_buffer.requirements();
                        let init_draw_buffer_memory_index = vk_adapter_memory_properties.find_host_visible_index(init_draw_buffer_memory_requirements.memoryTypeBits).expect("no suitable memory");
                        let mut init_draw_buffer_memory = br::DeviceMemoryObject::new(&vk_device, &br::MemoryAllocateInfo::new(init_draw_buffer_memory_requirements.size, init_draw_buffer_memory_index)).expect("init_draw_buffer memalloc");
                        init_draw_buffer.bind(&init_draw_buffer_memory, 0).expect("init_draw_buffer bind memory");
                        let p = init_draw_buffer_memory.map(0..init_size).expect("init_draw_buffer_memory map");
                        unsafe {
                            let content = p.ptr().cast::<DrawBufferInitContent>();
                            (*content).pos01[0] = [0.0, 0.0, 0.0, 1.0];
                            (*content).pos01[1] = [1.0, 0.0, 0.0, 1.0];
                            (*content).pos01[2] = [0.0, 1.0, 0.0, 1.0];
                            (*content).pos01[3] = [1.0, 1.0, 0.0, 1.0];
                            core::ptr::copy_nonoverlapping(box_instances.as_ptr(), (*content).instance_data.as_mut_ptr(), box_instances.len());
                        }
                        drop(p);
                        if !vk_adapter_memory_properties.is_coherent(init_draw_buffer_memory_index) {
                            unsafe {
                                vk_device.flush_mapped_memory_ranges(&[br::MappedMemoryRange::new(&init_draw_buffer_memory, 0..init_size as u64)]).expect("flush_mapped_memory_ranges");
                            }
                        }
                        unsafe { init_draw_buffer_memory.unmap(); }

                        let mut init_cp = br::CommandPoolObject::new(&vk_device, &br::CommandPoolCreateInfo::new(graphics_queue_family_index)).expect("init command pool create");
                        let mut init_cb = br::CommandBufferObject::alloc(&vk_device, &br::CommandBufferAllocateInfo::new(&mut init_cp, 1, br::CommandBufferLevel::Primary)).expect("init command buffer alloc");
                        unsafe { init_cb[0].begin(&br::CommandBufferBeginInfo::new()).expect("begin init cb") }
                        .copy_buffer(&init_draw_buffer, &draw_buffer, &[
                            br::BufferCopy::copy_data::<[[f32; 4]; 4]>(core::mem::offset_of!(DrawBufferInitContent, pos01) as _, 0),
                            br::BufferCopy {
                                srcOffset: core::mem::offset_of!(DrawBufferInitContent, instance_data) as _,
                                dstOffset: instance_data_offset as _,
                                size: (core::mem::size_of::<BoxInstance>() * box_instances.len()) as _
                            }
                        ])
                        .pipeline_barrier(br::PipelineStageFlags::TRANSFER, br::PipelineStageFlags::VERTEX_INPUT, 0, &[
                            br::vk::VkMemoryBarrier {
                                sType: br::vk::VkMemoryBarrier::TYPE,
                                pNext: core::ptr::null(),
                                srcAccessMask: br::AccessFlags::TRANSFER.write,
                                dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ
                            }
                        ], &[], &[])
                            .end().expect("error in init cb");
                        unsafe { render_queue.submit_raw(&[br::SubmitInfo::new(&[], &[], &[init_cb[0].as_transparent_ref()], &[])], None).expect("submit init"); }
                        render_queue.wait().expect("wait init commands");

                        let dsl_test = br::DescriptorSetLayoutObject::new(&vk_device, &br::DescriptorSetLayoutCreateInfo::new(
                            &[br::DescriptorType::CombinedImageSampler.make_binding(0, 1)]
                        )).expect("dsl_test create");

                        let shader_binary1 = std::fs::read("./resources/test.spv").expect("no shader");
                        let mut shader_binary = Vec::with_capacity(shader_binary1.len() >> 2);
                        unsafe { core::ptr::copy_nonoverlapping(shader_binary1.as_ptr(), shader_binary.spare_capacity_mut().as_mut_ptr().cast::<u8>(), shader_binary1.len()); }
                        unsafe { shader_binary.set_len(shader_binary1.len() >> 2); }
                        let shader_module = br::ShaderModuleObject::new(&vk_device, &br::ShaderModuleCreateInfo::new(&shader_binary)).expect("shader module create");
                        let pipeline_layout = br::PipelineLayoutObject::new(&vk_device, &br::PipelineLayoutCreateInfo::new(
                            &[dsl_test.as_transparent_ref()], &[br::PushConstantRange::new(br::vk::VK_SHADER_STAGE_VERTEX_BIT, 0..core::mem::size_of::<[f32; 2]>() as u32)]
                        )).expect("pipeline layout create");
                        let [mut pipeline] = vk_device.new_graphics_pipeline_array(&[
                            br::GraphicsPipelineCreateInfo::new(&pipeline_layout, vk_render_pass.subpass(0), &[
                                shader_module.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                                shader_module.on_stage(br::ShaderStage::Fragment, c"fragMain")
                            ], &br::PipelineVertexInputStateCreateInfo::new(&[
                                br::VertexInputBindingDescription::per_vertex_typed::<[f32; 4]>(0),
                                br::VertexInputBindingDescription::per_instance_typed::<BoxInstance>(1)
                            ], &[
                                br::VertexInputAttributeDescription {
                                    location: 0,
                                    binding: 0,
                                    offset: 0,
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT
                                },
                                br::VertexInputAttributeDescription {
                                    location: 1,
                                    binding: 1,
                                    offset: core::mem::offset_of!(BoxInstance, posst) as _,
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT
                                },
                                br::VertexInputAttributeDescription {
                                    location: 2,
                                    binding: 1,
                                    offset: core::mem::offset_of!(BoxInstance, uvst) as _,
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT
                                }
                            ]), &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleStrip),
                        &br::PipelineViewportStateCreateInfo::new(&[surface_ext.into_rect(br::Offset2D::ZERO).make_viewport(0.0..1.0)], &[surface_ext.into_rect(br::Offset2D::ZERO)]),
                    &br::PipelineRasterizationStateCreateInfo::new(br::PolygonMode::Fill, br::CullModeFlags::NONE, br::FrontFace::CounterClockwise),
                &br::PipelineColorBlendStateCreateInfo::new(&[br::vk::VkPipelineColorBlendAttachmentState::PREMULTIPLIED])).set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
                        ], None::<&br::PipelineCacheObject::<&br::DeviceObject<&br::InstanceObject>>>).expect("pipeline create");

                        let smp = br::SamplerObject::new(&vk_device, &br::SamplerCreateInfo::new()).expect("smp create");
                        let mut dp = br::DescriptorPoolObject::new(&vk_device, &br::DescriptorPoolCreateInfo::new(1, &[br::DescriptorType::CombinedImageSampler.make_size(1)])).expect("dp create");
                        let [ds_test] = dp.alloc_array(&[dsl_test.as_transparent_ref()]).expect("dp alloc");
                        vk_device.update_descriptor_sets(&[
                            ds_test.binding_at(0).write(br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageInfo::new(&glyph_atlas.view(), br::ImageLayout::ShaderReadOnlyOpt).with_sampler(&smp)]))
                        ], &[]);

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
                            .bind_pipeline(br::PipelineBindPoint::Graphics, &pipeline)
                            .push_constant_slice(&pipeline_layout, br::vk::VK_SHADER_STAGE_VERTEX_BIT, 0, &[surface_ext.width as f32, surface_ext.height as f32])
                            .bind_descriptor_sets(br::PipelineBindPoint::Graphics, &pipeline_layout, 0, &[ds_test], &[])
                            .bind_vertex_buffer_array(0, &[draw_buffer.as_transparent_ref(), draw_buffer.as_transparent_ref()], &[vertex_offset as _, instance_data_offset as _])
                            .draw(4, box_instances.len() as _, 0, 0)
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

                                if shutdown.load(std::sync::atomic::Ordering::Acquire) {
                                    // already shut down
                                    break 'lp;
                                }

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
                                
                        let [pipeline1] = vk_device.new_graphics_pipeline_array(&[
                            br::GraphicsPipelineCreateInfo::new(&pipeline_layout, vk_render_pass.subpass(0), &[
                                shader_module.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                                shader_module.on_stage(br::ShaderStage::Fragment, c"fragMain")
                            ], &br::PipelineVertexInputStateCreateInfo::new(&[
                                br::VertexInputBindingDescription::per_vertex_typed::<[f32; 4]>(0),
                                br::VertexInputBindingDescription::per_instance_typed::<BoxInstance>(1)
                            ], &[
                                br::VertexInputAttributeDescription {
                                    location: 0,
                                    binding: 0,
                                    offset: 0,
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT
                                },
                                br::VertexInputAttributeDescription {
                                    location: 1,
                                    binding: 1,
                                    offset: core::mem::offset_of!(BoxInstance, posst) as _,
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT
                                },
                                br::VertexInputAttributeDescription {
                                    location: 2,
                                    binding: 1,
                                    offset: core::mem::offset_of!(BoxInstance, uvst) as _,
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT
                                }
                            ]), &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleStrip),
                        &br::PipelineViewportStateCreateInfo::new(&[surface_ext.into_rect(br::Offset2D::ZERO).make_viewport(0.0..1.0)], &[surface_ext.into_rect(br::Offset2D::ZERO)]),
                    &br::PipelineRasterizationStateCreateInfo::new(br::PolygonMode::Fill, br::CullModeFlags::NONE, br::FrontFace::CounterClockwise),
                &br::PipelineColorBlendStateCreateInfo::new(&[br::vk::VkPipelineColorBlendAttachmentState::PREMULTIPLIED])).set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
                        ], None::<&br::PipelineCacheObject::<&br::DeviceObject<&br::InstanceObject>>>).expect("pipeline create");
                                pipeline = pipeline1;

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
                            .bind_pipeline(br::PipelineBindPoint::Graphics, &pipeline)
                            .push_constant_slice(&pipeline_layout, br::vk::VK_SHADER_STAGE_VERTEX_BIT, 0, &[surface_ext.width as f32, surface_ext.height as f32])
                            .bind_descriptor_sets(br::PipelineBindPoint::Graphics, &pipeline_layout, 0, &[ds_test], &[])
                            .bind_vertex_buffer_array(0, &[draw_buffer.as_transparent_ref(), draw_buffer.as_transparent_ref()], &[vertex_offset as _, instance_data_offset as _])
                            .draw(4, box_instances.len() as _, 0, 0)
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
                            glyph_atlas.drop(&vk_device);
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
    skylines: Vec<Skyline>
}
impl GlyphAtlasSpaceManager {
    const SPACING: u32 = 1;

    pub fn new(max: br::Extent2D) -> Self {
        Self {
            skylines: vec![Skyline { y: 0, width: max.width }],
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
            if skyline_point_index > 0 && self.skylines[skyline_point_index - 1].y == top + cons_height {
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

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct SafeF32(f32);
impl Eq for SafeF32 {
}
impl Ord for SafeF32 {
    #[inline(always)]
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        unsafe { self.0.partial_cmp(&other.0).unwrap_unchecked() }
    }
}
impl core::hash::Hash for SafeF32 {
    #[inline(always)]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        f32::to_ne_bytes(self.0).hash(state)
    }
}
impl SafeF32 {
    pub const fn new(v: f32) -> Option<Self> {
        if v.is_nan() {
             None
        } else {
            Some(Self(v))
        }
    }

    pub const unsafe fn new_unchecked(v: f32) -> Self {
        Self(v)
    }

    pub const fn value(&self) -> f32 {
        self.0
    }
}

struct GlyphAtlas {
    res: br::vk::VkImage,
    mem: br::vk::VkDeviceMemory,
    view: br::vk::VkImageView,
    acquired_rects: HashMap<(usize, SafeF32, u16), GlyphRect>,
    space_mgr: GlyphAtlasSpaceManager,
}
impl GlyphAtlas {
    const MULTISAMPLE_LEVEL: u32 = 8;

    pub unsafe fn drop(&mut self, device: &(impl br::VkHandle<Handle = br::vk::VkDevice> + ?Sized)) {
        unsafe { 
            br::vkfn_wrapper::destroy_image_view(device.native_ptr(), self.view, None);
            br::vkfn_wrapper::destroy_image(device.native_ptr(), self.res, None);
            br::vkfn_wrapper::free_memory(device.native_ptr(), self.mem, None);
        }
    }

    pub fn new(device: &(impl br::Device<ConcreteInstance: br::InstanceDebugUtilsExtension> + ?Sized), adapter_memory_props: &br::MemoryProperties,) -> Self {
        let size = br::Extent2D::spread1(4096);

        let mut res = br::ImageObject::new(device, &br::ImageCreateInfo::new(size, br::vk::VK_FORMAT_R8_UNORM).set_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT | br::ImageUsageFlags::TRANSFER_DEST)).expect("res create");
        let memory_requirements = res.requirements();
        let mem = br::DeviceMemoryObject::new(device, &br::MemoryAllocateInfo::new(memory_requirements.size, adapter_memory_props.find_device_local_index(memory_requirements.memoryTypeBits).expect("no suitable memory"))).expect("res malloc");
        res.bind(&mem, 0).expect("res mem bind");
        let view = br::ImageViewBuilder::new(res, br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1)).create().expect("res view create");

        view.image().set_name(Some(c"Glyph Atlas")).expect("res set name");
        mem.set_name(Some(c"Glyph Atlas [Backing]")).expect("mem set name");
        view.set_name(Some(c"Glyph Atlas [View]")).expect("view set name");

        let (view, res) = view.unmanage();
        let (res, _, _, _, _) = res.unmanage();
        let (mem, _) = mem.unmanage();
        Self {
            res,
            mem,
            view,
            acquired_rects: HashMap::new(),
            space_mgr: GlyphAtlasSpaceManager::new(size)
        }
    }

    pub fn acquire(&mut self, key: (usize, SafeF32, u16), width: u32, height: u32) -> (GlyphRect, bool) {
        match self.acquired_rects.entry(key) {
            std::collections::hash_map::Entry::Vacant(x) => {
                (x.insert(self.space_mgr.acquire(width, height).expect("no space left")).clone(), true)
            }
            std::collections::hash_map::Entry::Occupied(x) => {
                (x.get().clone(), false)
            }
        }
    }

    #[inline(always)]
    pub const fn image<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImage> {
        unsafe { br::VkHandleRef::dangling(self.res) }
    }

    #[inline(always)]
    pub const fn view<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.view) }
    }
}

#[implement(IDWriteTextRenderer)]
pub struct AtlasTextRenderer {
    box_instances: *mut Vec<BoxInstance>,
    atlas: *mut GlyphAtlas,
    new_filltri_points: *mut Vec<[f32; 2]>,
    new_filltri_indices: *mut Vec<u16>,
    new_curve_triangles: *mut Vec<[f32; 4]>,
}
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
        Ok(1.0)
    }

    fn IsPixelSnappingDisabled(
        &self,
        clientdrawingcontext: *const core::ffi::c_void,
    ) -> windows_core::Result<windows_core::BOOL> {
        Ok(BOOL(1))
    }
}
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
        let dip_to_pixels_scaling = 168.0f32 / 72.0;

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
                * glyphrun.fontEmSize * dip_to_pixels_scaling
                / design_unit as f32;
            let glyph_height = (glyph_metrics[n].advanceHeight as i32
                - glyph_metrics[n].topSideBearing
                - glyph_metrics[n].bottomSideBearing) as f32
                * glyphrun.fontEmSize * dip_to_pixels_scaling
                / design_unit as f32;

            let (r, is_new) = unsafe { (*self.atlas).acquire((0, SafeF32::new_unchecked(glyphrun.fontEmSize), *glyphrun.glyphIndices.add(n)), glyph_width.ceil() as _, glyph_height.ceil() as _) };
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
                        (baselineoriginx + glyph_metrics[n].leftSideBearing as f32 * glyphrun.fontEmSize / design_unit as f32) * dip_to_pixels_scaling,
                        (baselineoriginy - (glyph_metrics[n].verticalOriginY as f32 - glyph_metrics[n].topSideBearing as f32) * glyphrun.fontEmSize / design_unit as f32) * dip_to_pixels_scaling
                    ],
                    uvst: [r.width as f32 / (*self.atlas).space_mgr.max.width as f32, r.height as f32 / (*self.atlas).space_mgr.max.height as f32, r.left as f32 / (*self.atlas).space_mgr.max.width as f32, r.top as f32 / (*self.atlas).space_mgr.max.height as f32],
                });
            }
            if is_new {
                // render font here
                let mut current_figure_state = None;
                let sink = ID2D1SimplifiedGeometrySink::from(GlyphOutlineSink {
                    translate: windows_numerics::Vector2 {
                        X: r.left as f32 - (glyph_metrics[n].leftSideBearing as f32) * glyphrun.fontEmSize * dip_to_pixels_scaling / design_unit as f32,
                        Y: r.top as f32 - (glyph_metrics[n].advanceHeight as f32 - glyph_metrics[n].topSideBearing as f32 - glyph_metrics[n].bottomSideBearing as f32) * glyphrun.fontEmSize * dip_to_pixels_scaling / design_unit as f32,
                    },
                    dip_to_pixels_scale: dip_to_pixels_scaling,
                    current_figure_state: &mut current_figure_state,
                    filltri_points: self.new_filltri_points,
                    filltri_indices: self.new_filltri_indices,
                    curve_triangles: self.new_curve_triangles,
                });
                unsafe { font_face.GetGlyphRunOutline(glyphrun.fontEmSize, glyphrun.glyphIndices.add(n), None, None, 1, glyphrun.isSideways.as_bool(), false, &sink).expect("GetGlyphRunOutline"); }
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

#[implement(ID2D1SimplifiedGeometrySink)]
struct GlyphOutlineSink {
    translate: windows_numerics::Vector2,
    dip_to_pixels_scale: f32,
    current_figure_state: *mut Option<(windows_numerics::Vector2, u16)>,
    filltri_points: *mut Vec<[f32; 2]>,
    filltri_indices: *mut Vec<u16>,
    curve_triangles: *mut Vec<[f32; 4]>
}
impl ID2D1SimplifiedGeometrySink_Impl for GlyphOutlineSink_Impl {
    fn BeginFigure(&self, startpoint: &windows_numerics::Vector2, figurebegin: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_BEGIN) {
        assert_eq!(figurebegin, D2D1_FIGURE_BEGIN_FILLED, "not filled figure");

        unsafe {
            (*self.current_figure_state) = Some((*startpoint, (*self.filltri_points).len() as _));
            (*self.filltri_points).push([startpoint.X * self.dip_to_pixels_scale + self.translate.X, -startpoint.Y * self.dip_to_pixels_scale + self.translate.Y]);
        }
    }

    fn EndFigure(&self, figureend: windows::Win32::Graphics::Direct2D::Common::D2D1_FIGURE_END) {
        let (start_point, filltri_index0) = unsafe { (*self.current_figure_state).take().expect("no figure started?") };

        if figureend == D2D1_FIGURE_END_CLOSED {
            // line to start
            unsafe {
                let filltri_point1 = (*self.filltri_points).len() - 1;
                (*self.filltri_points).push([start_point.X * self.dip_to_pixels_scale + self.translate.X, -start_point.Y * self.dip_to_pixels_scale + self.translate.Y]);
                (*self.filltri_indices).extend([filltri_index0, filltri_point1 as u16, (*self.filltri_points).len() as u16 - 1]);
            }
        }
    }

    fn AddLines(&self, points: *const windows_numerics::Vector2, pointscount: u32) {
        let &(_, filltri_index0) = unsafe { (*self.current_figure_state).as_ref().expect("no figure started?") };

        for p in unsafe { core::slice::from_raw_parts(points, pointscount as _) } {
            unsafe {
                let filltri_point1 = (*self.filltri_points).len() - 1;
                (*self.filltri_points).push([p.X * self.dip_to_pixels_scale + self.translate.X, -p.Y * self.dip_to_pixels_scale + self.translate.Y]);
                (*self.filltri_indices).extend([filltri_index0, filltri_point1 as u16, (*self.filltri_points).len() as u16 - 1]);
            }
        }
    }

    fn AddBeziers(&self, beziers: *const windows::Win32::Graphics::Direct2D::Common::D2D1_BEZIER_SEGMENT, bezierscount: u32) {
        let &(_, filltri_index0) = unsafe { (*self.current_figure_state).as_ref().expect("no figure started?") };

        for p in unsafe { core::slice::from_raw_parts(beziers, bezierscount as _) } {
            let from_p = unsafe { (*self.filltri_points).last().expect("no points emitted") };
            let bez = lyon_geom::CubicBezierSegment {
                from: lyon_geom::point(from_p[0], from_p[1]),
                ctrl1: lyon_geom::point(p.point1.X * self.dip_to_pixels_scale + self.translate.X, -p.point1.Y * self.dip_to_pixels_scale + self.translate.Y),
                ctrl2: lyon_geom::point(p.point2.X * self.dip_to_pixels_scale + self.translate.X, -p.point2.Y * self.dip_to_pixels_scale + self.translate.Y),
                to: lyon_geom::point(p.point3.X * self.dip_to_pixels_scale + self.translate.X, -p.point3.Y * self.dip_to_pixels_scale + self.translate.Y)
            };

            bez.for_each_quadratic_bezier(0.1, &mut |q| {
                unsafe {
                    let filltri_point1 = (*self.filltri_points).len() - 1;
                    (*self.filltri_points).push([q.to.x, q.to.y]);
                    (*self.filltri_indices).extend([filltri_index0, filltri_point1 as u16, (*self.filltri_points).len() as u16 - 1]);

                    (*self.curve_triangles).extend([
                        [q.from.x, q.from.y, 0.0, 0.0],
                        [q.ctrl.x, q.ctrl.y, 0.5, 0.0],
                        [q.to.x, q.to.y, 1.0, 1.0]
                    ]);
                }
            });
        }
    }

    fn Close(&self) -> windows_core::Result<()> {
        let &(ref start_point, filltri_index0) = unsafe { (*self.current_figure_state).as_ref().expect("no figure started?") };

        // line to start
        unsafe {
            let filltri_point1 = (*self.filltri_points).len() - 1;
            (*self.filltri_points).push([start_point.X * self.dip_to_pixels_scale + self.translate.X, start_point.Y * self.dip_to_pixels_scale + self.translate.Y]);
            (*self.filltri_indices).extend([filltri_index0, filltri_point1 as u16, (*self.filltri_points).len() as u16 - 1]);
        }
        
        Ok(())
    }

    fn SetFillMode(&self, fillmode: windows::Win32::Graphics::Direct2D::Common::D2D1_FILL_MODE) {
        if fillmode != D2D1_FILL_MODE_WINDING {
            tracing::warn!("not winding fill mode specified");
        }
    }

    fn SetSegmentFlags(&self, vertexflags: windows::Win32::Graphics::Direct2D::Common::D2D1_PATH_SEGMENT) {
        unimplemented!("SetSegmentFlags {vertexflags:?}")
    }
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
