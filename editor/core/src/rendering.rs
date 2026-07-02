use core::num::NonZeroUsize;
use std::{
    collections::HashMap,
    sync::{Mutex, atomic::AtomicBool},
};

use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, DescriptorPoolMut, Device, DeviceMemoryMut,
    Fence, FenceMut, ImageChild, MemoryBound, QueueMut, RenderPass, ShaderModule,
    SpecializationConstants, SwapchainMut, VkHandle, VkHandleMut,
};
use peridot_math::Matrix4;

use crate::{
    FlyoutSurfaceHandle, SyncEvent, SyncEventBus, WindowHandle,
    graphics::{
        BLEND_STATE_SINGLE_NONE, BLEND_STATE_SINGLE_PREMULTIPLIED, IA_STATE_TRILIST,
        IA_STATE_TRISTRIP, MS_STATE_EMPTY, RASTER_STATE_DEFAULT_FILL_NOCULL, UnboundVulkanSurface,
        VI_STATE_EMPTY, VulkanDevice, VulkanSurface, VulkanSwapchain,
    },
    rendering::{
        atlas::{AtlasRect, ColorTextureAtlas, TextureAtlas},
        composite::{
            BoundCompositeRenderer, CompositeRenderingData, CompositeSharedBuffers,
            CompositeStreamingData, CompositeTreeRef, CompositeTreeRender, CompositeTreeSyncBuffer,
            CustomRenderContext, CustomRenderHandler, CustomRenderHandlerFn, CustomRenderToken,
            EmptyCustomRenderHandler,
        },
        text::FontSet,
        vg::VectorRasterizationState,
    },
    uikit::MountTarget,
    utils::{PixelsUnit, SafeF32, Size},
};

pub mod atlas;
pub mod composite;
pub mod text;
pub mod vg;

#[repr(transparent)]
pub struct NewWindowVulkanSurface(pub UnboundVulkanSurface);
unsafe impl Sync for NewWindowVulkanSurface {}
unsafe impl Send for NewWindowVulkanSurface {}

pub struct NewWindowData {
    pub key: WindowHandle,
    pub vk_surface: NewWindowVulkanSurface,
}

pub struct NewContextMenuData {
    pub w: FlyoutSurfaceHandle,
    #[cfg(not(windows))]
    pub vk_surface: NewWindowVulkanSurface,
    #[cfg(windows)]
    pub swapchain: windows::Win32::Graphics::Dxgi::IDXGISwapChain3,
    pub composite_root: CompositeTreeRef,
}

#[derive(Clone)]
pub struct Normalized2DStaticMeshTexture {
    pub vertices: &'static [[f32; 2]],
    pub indices: &'static [u16],
    pub width: f32,
    pub height: f32,
}

#[derive(Clone)]
pub struct ShaderTexture {
    pub width: f32,
    pub height: f32,
    pub shader_path: String,
}

pub enum RenderMessage {
    NewWindow(NewWindowData),
    DestroyWindow(WindowHandle, std::sync::mpsc::Sender<()>),
    NewContextMenu(NewContextMenuData),
    DestroyContextMenu(FlyoutSurfaceHandle, std::sync::mpsc::Sender<()>),
    RegisterNormalized2DStaticMeshTexture {
        id: TextureID,
        data: Normalized2DStaticMeshTexture,
    },
    RegisterShaderTexture {
        id: TextureID,
        data: ShaderTexture,
    },
}

pub type RenderMessageSender = std::sync::mpsc::Sender<RenderMessage>;

pub struct RendererSync {
    pub composite_buffer: CompositeTreeSyncBuffer<SyncEvent>,
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextureID(NonZeroUsize);
impl TextureID {
    #[inline(always)]
    pub(self) const fn rect_index(&self) -> usize {
        self.0.get() - 1
    }
}

pub struct MainThreadTextureIDIssuer {
    next_id: NonZeroUsize,
}
impl MainThreadTextureIDIssuer {
    pub fn new() -> Self {
        Self {
            next_id: unsafe { NonZeroUsize::new_unchecked(1) },
        }
    }

    pub fn issue(&mut self) -> TextureID {
        let id = self.next_id;
        self.next_id = self.next_id.checked_add(1).expect("too many textures");
        TextureID(id)
    }
}

crate::perf_event!(SYNCPOINT = "RenderSyncPoint");
crate::perf_section!(INITIALIZATION = "RenderThread.Initialize");
crate::perf_section!(RENDERLOOP = "RenderLoop");
crate::perf_section!(PROCESS_MESSAGE = "RenderLoop.ProcessMessage");
crate::perf_section!(UPDATE_GRADIENT = "RenderLoop.UpdateGradient");
crate::perf_section!(UPDATE_WINDOW = "RenderLoop.UpdateWindow");
crate::perf_section!(UPDATE_CONTEXT_MENU = "RenderLoop.UpdateContextMenu");
crate::perf_section!(RENDER_VG_MASK = "RenderLoop.RenderVGMask");
crate::perf_section!(POST_QUEUE = "RenderLoop.PostQueue");
crate::perf_section!(WAIT_QUEUE = "RenderLoop.WaitQueue");
#[cfg(windows)]
crate::perf_section!(WIN32_DX_PRESENT = "RenderLoop.Win32.DirectXPresent");

pub const PREVIEW_COMPOSITE: CustomRenderToken = CustomRenderToken(0);

pub struct CommittedPreviewState {
    pub viewport_size: Size<PixelsUnit>,
}

pub struct RenderThread<'main> {
    pub vk_device: &'main VulkanDevice<'main>,
    pub shutdown_signal: &'main AtomicBool,
    pub renderer_sync: &'main Mutex<RendererSync>,
    pub global_time_base: &'main std::time::Instant,
    pub event_bus: &'main SyncEventBus,
    pub message_receiver: std::sync::mpsc::Receiver<RenderMessage>,
    pub font_set: &'main FontSet,
    pub preview_state: &'main Mutex<CommittedPreviewState>,
    #[cfg(windows)]
    pub dx_context: &'main crate::platform::windows::DxContext,
    #[cfg(windows)]
    pub d3d12_present_counter: u64,
}
impl<'main> RenderThread<'main> {
    #[allow(unused_mut)]
    pub fn run(mut self) {
        tracing::info!("Starting RenderThread...");

        crate::perf_begin!(perf = INITIALIZATION);

        let mut render_queue = self
            .vk_device
            .queue(self.vk_device.present_queue_family_index(), 0);

        #[cfg(windows)]
        let d3d12_present_fence: windows::Win32::Graphics::Direct3D12::ID3D12Fence = unsafe {
            self.dx_context
                .d3d12_device
                .CreateFence(
                    0,
                    windows::Win32::Graphics::Direct3D12::D3D12_FENCE_FLAG_NONE,
                )
                .expect("d3d12_device.CreateFence")
        };
        #[cfg(windows)]
        let d3d12_present_fence_event = unsafe {
            windows::Win32::System::Threading::CreateEventW(None, false, false, None)
                .expect("CreateEvent")
        };

        let mut composite_tree = CompositeTreeRender::new();
        let composite_shared_buffers = CompositeSharedBuffers::new(self.vk_device);
        let vg_render_formats = GlyphAtlasRenderingFormats {
            color: br::vk::VK_FORMAT_R8_UNORM,
            stencil: br::vk::VK_FORMAT_S8_UINT,
        };
        let glyph_atlas_manager_common_resources =
            GlyphAtlasManagerCommonResources::new(self.vk_device, &vg_render_formats);
        struct GlyphAtlasDataPerDpi<'d> {
            manager: MaskTextureAtlasManager<'d>,
            color_manager: ColorTextureAtlasManager<'d>,
            atlas_rects: Vec<AtlasRect>,
            vector_raster_state: VectorRasterizationState,
            shader_texture_rasterize_requests: Vec<(AtlasRect, ShaderTexture)>,
            ref_count: u64,
        }
        impl<'d> GlyphAtlasDataPerDpi<'d> {
            pub fn new(
                common_res: &GlyphAtlasManagerCommonResources<'d>,
                render_queue: &mut (impl br::QueueMut + ?Sized),
            ) -> Self {
                Self {
                    manager: MaskTextureAtlasManager::new(
                        common_res,
                        render_queue,
                        common_res.device.present_queue_family_index(),
                    ),
                    color_manager: ColorTextureAtlasManager::new(
                        common_res,
                        render_queue,
                        common_res.device.present_queue_family_index(),
                    ),
                    atlas_rects: Vec::new(),
                    vector_raster_state: VectorRasterizationState::new(),
                    shader_texture_rasterize_requests: Vec::new(),
                    ref_count: 0,
                }
            }

            pub fn ready_for_reuse(&mut self) {
                self.manager.clear();
                self.color_manager.clear();
                self.atlas_rects.clear();
                self.ref_count = 0;
            }
        }
        let mut glyph_atlas_per_scale: HashMap<SafeF32, GlyphAtlasDataPerDpi> = HashMap::new();
        let mut windows: HashMap<WindowHandle, WindowRenderer> = HashMap::new();
        let mut context_menus: HashMap<FlyoutSurfaceHandle, ContextMenuRenderer> = HashMap::new();
        let mut normalized_2d_static_mesh_textures: HashMap<
            TextureID,
            Normalized2DStaticMeshTexture,
        > = HashMap::new();
        let mut shader_textures = HashMap::<TextureID, ShaderTexture>::new();

        let mut shared_update_cp = br::CommandPoolObject::new(
            self.vk_device,
            &br::CommandPoolCreateInfo::new(self.vk_device.present_queue_family_index()),
        )
        .expect("shared_update_cp.create");
        let [mut shared_update_commands] = br::CommandBufferObject::alloc_array(
            self.vk_device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut shared_update_cp,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("shared_update_commands.alloc");
        unsafe {
            shared_update_commands
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("shared_update_commands.begin")
                .end()
                .expect("shared_update_commands.end");
        }

        let mut present_id = 0;
        self.event_bus
            .push(SyncEvent::NewPresentID { id: present_id });

        struct PreviewRenderTargetResource<'d> {
            device: &'d VulkanDevice<'d>,
            memory: br::vk::VkDeviceMemory,
            image: br::vk::VkImage,
            image_view: br::vk::VkImageView,
        }
        impl Drop for PreviewRenderTargetResource<'_> {
            fn drop(&mut self) {
                unsafe {
                    br::vkfn_wrapper::destroy_image_view(
                        self.device.as_transparent_ref(),
                        br::VkHandleRefMut::dangling(self.image_view),
                        None,
                    );
                    br::vkfn_wrapper::destroy_image(
                        self.device.as_transparent_ref(),
                        br::VkHandleRefMut::dangling(self.image),
                        None,
                    );
                    br::vkfn_wrapper::free_memory(
                        self.device.as_transparent_ref(),
                        br::VkHandleRefMut::dangling(self.memory),
                        None,
                    );
                }
            }
        }
        let preview_init_state = self.preview_state.lock().expect("poisoned");
        let mut preview_rt_image = br::ImageObject::new(
            self.vk_device,
            &br::ImageCreateInfo::new(
                preview_init_state.viewport_size.to_vk(),
                br::vk::VK_FORMAT_R8G8B8A8_UNORM,
            )
            .set_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT),
        )
        .expect("preview_rt.image.create");
        let preview_rt_image_memreq = preview_rt_image.requirements();
        let preview_rt_memory = br::DeviceMemoryObject::new(
            self.vk_device,
            &br::MemoryAllocateInfo::new(
                preview_rt_image_memreq.size,
                self.vk_device
                    .find_device_local_memory_index(preview_rt_image_memreq.memoryTypeBits)
                    .expect("preview_rt.memory.index"),
            ),
        )
        .expect("preview_rt.memory.alloc");
        preview_rt_image
            .bind(&preview_rt_memory, 0)
            .expect("preview_rt.image.bind");
        let preview_rt_image_view = br::ImageViewBuilder::new(
            preview_rt_image,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("preview_rt.image_view.create");
        let (preview_rt_image_view, preview_rt_image) = preview_rt_image_view.unmanage();
        let (preview_rt_image, _, _, _, _) = preview_rt_image.unmanage();
        let (preview_rt_memory, _) = preview_rt_memory.unmanage();
        let mut preview_rt_resource = PreviewRenderTargetResource {
            device: self.vk_device,
            memory: preview_rt_memory,
            image: preview_rt_image,
            image_view: preview_rt_image_view,
        };

        #[repr(C)]
        struct PreviewStreamingBufferContent {
            current_sec: f32,
        }
        let mut preview_streaming_buffer = br::BufferObject::new(
            self.vk_device,
            &br::BufferCreateInfo::new_for_type::<PreviewStreamingBufferContent>(
                br::BufferUsage::UNIFORM_BUFFER,
            ),
        )
        .expect("preview.streaming_buffer.create");
        let preview_streaming_buffer_memreq = preview_streaming_buffer.requirements();
        let preview_streaming_memory_index = self
            .vk_device
            .find_direct_memory_index(preview_streaming_buffer_memreq.memoryTypeBits)
            .expect("preview.streaming_memory.index");
        let preview_streaming_memory_should_flush = !self
            .vk_device
            .is_coherent_memory(preview_streaming_memory_index);
        let mut preview_streaming_memory = br::DeviceMemoryObject::new(
            self.vk_device,
            &br::MemoryAllocateInfo::new(
                preview_streaming_buffer_memreq.size,
                preview_streaming_memory_index,
            ),
        )
        .expect("preview.streaming_memory.alloc");
        preview_streaming_buffer
            .bind(&preview_streaming_memory, 0)
            .expect("preview_streaming_buffer.bind");

        let preview_render_pass = br::RenderPassObject::new(
            self.vk_device,
            &br::RenderPassCreateInfo2::new(
                &[
                    br::AttachmentDescription2::new(br::vk::VK_FORMAT_R8G8B8A8_UNORM)
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)
                        .with_layout_to(br::ImageLayout::ShaderReadOnlyOpt.from_undefined()),
                ],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .by_region()
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::SHADER.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::FRAGMENT_SHADER,
                )],
            ),
        )
        .expect("preview.render_pass.create");
        let preview_framebuffer = br::FramebufferObject::new(
            self.vk_device,
            &br::FramebufferCreateInfo::new(
                &preview_render_pass,
                &[unsafe { br::VkHandleRef::dangling(preview_rt_resource.image_view) }],
                preview_init_state.viewport_size.width,
                preview_init_state.viewport_size.height,
            ),
        )
        .expect("preview.framebuffer.create");

        let preview_render_descriptor_set_layout = br::DescriptorSetLayoutObject::new(
            self.vk_device,
            &br::DescriptorSetLayoutCreateInfo::new(&[
                br::DescriptorType::UniformBuffer.make_binding(0, 1)
            ]),
        )
        .expect("preview.render.descriptor_set_layout.create");
        let mut preview_render_descriptor_pool = br::DescriptorPoolObject::new(
            self.vk_device,
            &br::DescriptorPoolCreateInfo::new(
                1,
                &[br::DescriptorType::UniformBuffer.make_size(1)],
            ),
        )
        .expect("preview.render.descriptor_pool.create");
        let [mut preview_render_common_descriptor_set] = preview_render_descriptor_pool
            .alloc_array(&[preview_render_descriptor_set_layout.as_transparent_ref()])
            .expect("preview_render.descriptor.alloc");
        self.vk_device.update_descriptor_sets(
            &[preview_render_common_descriptor_set.binding_at(0).write(
                br::DescriptorContents::uniform_buffer(
                    &preview_streaming_buffer,
                    0..size_of::<PreviewStreamingBufferContent>() as _,
                ),
            )],
            &[],
        );
        let preview_render_pipeline_layout = br::PipelineLayoutObject::new(
            self.vk_device,
            &br::PipelineLayoutCreateInfo::new(
                &[preview_render_descriptor_set_layout.as_transparent_ref()],
                &[],
            ),
        )
        .expect("preview.render.pipeline_layout.create");
        let preview_render_shader = self.vk_device.require_shader("preview/test.spv");
        let [mut preview_render_pipeline] = self
            .vk_device
            .create_graphics_pipelines_array(&[br::GraphicsPipelineCreateInfo::new(
                &preview_render_pipeline_layout,
                preview_render_pass.subpass(0),
                &[
                    preview_render_shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                    preview_render_shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
                ],
                VI_STATE_EMPTY,
                IA_STATE_TRILIST,
                &br::PipelineViewportStateCreateInfo::new(
                    &[preview_init_state
                        .viewport_size
                        .to_vk()
                        .into_rect(br::Offset2D::ZERO)
                        .make_viewport(0.0..1.0)],
                    &[preview_init_state
                        .viewport_size
                        .to_vk()
                        .into_rect(br::Offset2D::ZERO)],
                ),
                RASTER_STATE_DEFAULT_FILL_NOCULL,
                BLEND_STATE_SINGLE_PREMULTIPLIED,
            )
            .set_multisample_state(MS_STATE_EMPTY)])
            .expect("preview.render.pipelines.create");

        let mut preview_command_pool = br::CommandPoolObject::new(
            self.vk_device,
            &br::CommandPoolCreateInfo::new(self.vk_device.present_queue_family_index()),
        )
        .expect("preview.command_pool.create");
        let [mut preview_command_buffer] = br::CommandBufferObject::alloc_array(
            self.vk_device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut preview_command_pool,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("preview.command_buffer.create");
        unsafe {
            preview_command_buffer
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("preview.command_buffer.begin")
        }
        .begin_render_pass(
            &br::RenderPassBeginInfo::new(
                &preview_render_pass,
                &preview_framebuffer,
                preview_init_state
                    .viewport_size
                    .to_vk()
                    .into_rect(br::Offset2D::ZERO),
                &[br::ClearValue::color_f32([0.0, 0.1, 0.0, 1.0])],
            ),
            br::SubpassContents::Inline,
        )
        .bind_pipeline(br::PipelineBindPoint::Graphics, &preview_render_pipeline)
        .bind_descriptor_sets(
            br::PipelineBindPoint::Graphics,
            &preview_render_pipeline_layout,
            0,
            &[preview_render_common_descriptor_set],
            &[],
        )
        .draw(3, 1, 0, 0)
        .end_render_pass()
        .end()
        .expect("preview.command_buffer.end");

        let dep_semaphore_preview =
            br::SemaphoreObject::new(self.vk_device, &br::SemaphoreCreateInfo::new())
                .expect("dep_semaphore_preview.create");
        let linear_sampler = br::SamplerObject::new(self.vk_device, &br::SamplerCreateInfo::new())
            .expect("linear_sampler.create");
        let mut preview_composite = PreviewComposite::new(
            self.vk_device,
            br::VkHandleRef::from_raw_ref(&preview_rt_resource.image_view),
            &linear_sampler,
            preview_render_pass.subpass(0),
            br::Extent2D {
                width: 640,
                height: 480,
            },
        );

        crate::perf_end!(perf);

        let mut any_swapchain_invalidated = false;
        'lp: while !self
            .shutdown_signal
            .load(std::sync::atomic::Ordering::Acquire)
        {
            crate::perf_scope!(RENDERLOOP);
            // unsafe {
            //     w.manual_capture_begin();
            // }

            loop {
                crate::perf_scope!(PROCESS_MESSAGE);
                match self.message_receiver.try_recv() {
                    Ok(RenderMessage::NewWindow(wd)) => {
                        let init_scale =
                            SafeF32::new(wd.key.ui_scale_factor()).expect("invalid scale");

                        let window_glyph_atlas = match glyph_atlas_per_scale.entry(init_scale) {
                            // use existing
                            std::collections::hash_map::Entry::Occupied(x) => x.into_mut(),
                            // create new one
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert(GlyphAtlasDataPerDpi::new(
                                    &glyph_atlas_manager_common_resources,
                                    &mut render_queue,
                                ))
                            }
                        };
                        window_glyph_atlas.ref_count += 1;
                        windows.insert(
                            wd.key,
                            WindowRenderer::new(
                                self.vk_device,
                                &composite_shared_buffers,
                                wd,
                                init_scale,
                                window_glyph_atlas.manager.atlas(),
                                window_glyph_atlas.color_manager.atlas(),
                                self.event_bus,
                            ),
                        );
                    }
                    Ok(RenderMessage::DestroyWindow(window_handle, done_event_bus)) => {
                        if let Some(x) = windows.remove(&window_handle) {
                            let current = glyph_atlas_per_scale
                                .get_mut(&x.active_scale)
                                .expect("invalid state");
                            current.ref_count -= 1;
                            if current.ref_count == 0 {
                                // no references
                                glyph_atlas_per_scale.remove(&x.active_scale);
                            }
                        }

                        if let Err(e) = done_event_bus.send(()) {
                            tracing::error!(reason = %e, "done_event_bus.send");
                        };
                    }
                    Ok(RenderMessage::NewContextMenu(create_data)) => {
                        let init_scale =
                            SafeF32::new(create_data.w.render_scale()).expect("invalid scale");

                        let window_glyph_atlas = match glyph_atlas_per_scale.entry(init_scale) {
                            // use existing
                            std::collections::hash_map::Entry::Occupied(x) => x.into_mut(),
                            // create new one
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert(GlyphAtlasDataPerDpi::new(
                                    &glyph_atlas_manager_common_resources,
                                    &mut render_queue,
                                ))
                            }
                        };
                        window_glyph_atlas.ref_count += 1;
                        context_menus.insert(
                            create_data.w,
                            ContextMenuRenderer::new(
                                self.vk_device,
                                &composite_shared_buffers,
                                create_data,
                                init_scale,
                                window_glyph_atlas.manager.atlas(),
                                window_glyph_atlas.color_manager.atlas(),
                                self.event_bus,
                                #[cfg(windows)]
                                self.dx_context,
                            ),
                        );
                    }
                    Ok(RenderMessage::DestroyContextMenu(handle, done_event_bus)) => {
                        if let Some(x) = context_menus.remove(&handle) {
                            let current = glyph_atlas_per_scale
                                .get_mut(&x.active_scale)
                                .expect("invalid state");
                            current.ref_count -= 1;
                            if current.ref_count == 0 {
                                // no references
                                glyph_atlas_per_scale.remove(&x.active_scale);
                            }
                        }

                        if let Err(e) = done_event_bus.send(()) {
                            tracing::error!(reason = %e, "done_event_bus.send");
                        }
                    }
                    Ok(RenderMessage::RegisterNormalized2DStaticMeshTexture { id, data }) => {
                        normalized_2d_static_mesh_textures.insert(id, data);
                    }
                    Ok(RenderMessage::RegisterShaderTexture { id, data }) => {
                        shader_textures.insert(id, data);
                    }
                    Err(std::sync::mpsc::TryRecvError::Empty) => {
                        break;
                    }
                    Err(e) => {
                        tracing::error!(reason = %e, "message_receiver.try_recv");
                        break;
                    }
                }
            }

            // TODO: 必要なら後で最適化する
            crate::perf_begin!(perf = UPDATE_GRADIENT);
            composite_tree.update_gradients(self.vk_device, &composite_shared_buffers);
            unsafe {
                shared_update_cp
                    .reset(br::CommandPoolResetFlags::EMPTY)
                    .expect("shared_update_cp.reset");
            }
            unsafe {
                shared_update_commands
                    .begin(&br::CommandBufferBeginInfo::new())
                    .expect("shared_update_commands.begin")
            }
            .inject(|r| composite_shared_buffers.sync_buffer(r))
            .end()
            .expect("shared_update_commands.end");
            render_queue
                .submit(
                    &[br::SubmitInfo::new_array(
                        &[],
                        &[],
                        &[shared_update_commands.as_transparent_ref()],
                        &[],
                    )],
                    None,
                )
                .expect("shared_update.submit");
            render_queue.wait().expect("shared_update.wait");
            crate::perf_end!(perf);

            for x in windows.values_mut() {
                if x.take_swapchain_externally_invalidation_signal() {
                    x.invalidate_swapchain();
                    any_swapchain_invalidated = true;
                }
            }
            #[cfg(not(windows))]
            for x in context_menus.values_mut() {
                if x.take_swapchain_externally_invalidation_signal() {
                    tracing::debug!("ContextMenuSwapchainInvalidation");
                    x.invalidate_swapchain();
                    any_swapchain_invalidated = true;
                }
            }

            if any_swapchain_invalidated {
                let x = std::time::Instant::now();
                render_queue.wait().expect("waiting pending queue works");
                tracing::trace!(elapsed = ?x.elapsed(), "queue waiting time during resize");

                if self
                    .shutdown_signal
                    .load(std::sync::atomic::Ordering::Acquire)
                {
                    // already shut down while waiting queue completion
                    break 'lp;
                }

                let mut descriptor_writes = Vec::new();
                for x in windows.values_mut() {
                    x.validate_swapchain(&mut descriptor_writes, self.event_bus);
                }
                for x in context_menus.values_mut() {
                    x.validate_swapchain(&mut descriptor_writes, self.event_bus);
                }
                self.vk_device
                    .update_descriptor_sets(&descriptor_writes, &[]);

                any_swapchain_invalidated = false;
            }

            // flush synchronizing buffers
            crate::perf_emit!(SYNCPOINT);
            {
                let mut renderer_sync = self.renderer_sync.lock().expect("poisoned");
                renderer_sync.composite_buffer.clean(&mut composite_tree);
            }

            for x in glyph_atlas_per_scale.values_mut() {
                x.vector_raster_state.clear();
            }

            let current_t = self.global_time_base.elapsed();
            composite_tree.update_shared(current_t.as_secs_f32());

            // いったん最適化とかは考えないで直列でまわす(パフォーマンス気になったらそのとき考える)
            struct SubmitParameters<'x> {
                render_wait_semaphores: Vec<br::VkHandleRef<'x, br::vk::VkSemaphore>>,
                render_wait_stages: Vec<br::PipelineStageFlags>,
                render_commands: Vec<br::VkHandleRef<'x, br::vk::VkCommandBuffer>>,
                render_signal_semaphores: Vec<br::VkHandleRef<'x, br::vk::VkSemaphore>>,
            }
            enum PresentKey {
                Window(WindowHandle),
                ContextMenu(FlyoutSurfaceHandle),
            }
            struct VkPresentParameters<'x> {
                key: PresentKey,
                swapchain_ref: br::VkHandleRef<'x, br::vk::VkSwapchainKHR>,
                render_signal_semaphores: Vec<br::VkHandleRef<'x, br::vk::VkSemaphore>>,
                backbuffer_index: u32,
            }
            let mut submit_parameters = Vec::with_capacity(windows.len() + context_menus.len());
            let mut present_parameters = Vec::with_capacity(windows.len() + context_menus.len());
            #[cfg(windows)]
            let mut present_swapchains = Vec::with_capacity(context_menus.len());
            let mut preview_composition_required = false;
            for (k, x) in windows.iter_mut() {
                crate::perf_scope!(UPDATE_WINDOW);

                let backbuffer_index = match x.acquire_backbuffer_with_wait() {
                    Ok(x) => x,
                    Err(e) if e.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                        x.invalidate_swapchain();
                        any_swapchain_invalidated = true;
                        continue;
                    }
                    Err(e) => Err(e).expect("acquire next"),
                };

                let new_ui_scale = x.take_latest_ui_scale_changes();

                if let Some(scale) = new_ui_scale {
                    let scale = SafeF32::new(scale).expect("scale.invalid");

                    let current = glyph_atlas_per_scale
                        .get_mut(&x.active_scale)
                        .expect("invalid state");
                    current.ref_count -= 1;
                    let removed = if current.ref_count == 0 {
                        // no references
                        glyph_atlas_per_scale.remove(&x.active_scale)
                    } else {
                        None
                    };

                    let new_atlas_mgr = match glyph_atlas_per_scale.entry(scale) {
                        // reuse existing
                        std::collections::hash_map::Entry::Occupied(o) => o.into_mut(),
                        std::collections::hash_map::Entry::Vacant(v) => match removed {
                            // reuse existing with clear
                            Some(mut data) => {
                                data.ready_for_reuse();
                                v.insert(data)
                            }
                            // new one
                            None => v.insert(GlyphAtlasDataPerDpi::new(
                                &glyph_atlas_manager_common_resources,
                                &mut render_queue,
                            )),
                        },
                    };
                    new_atlas_mgr.ref_count += 1;

                    x.active_scale = scale;
                    x.invalidate_render_commands(); // DescriptorSetをかえるときは再度つくりなおす必要がある
                    let mut descriptor_writes = Vec::with_capacity(1);
                    x.composite_renderer.rebind_glyph_atlas(
                        new_atlas_mgr.manager.atlas().as_image_view(),
                        new_atlas_mgr.color_manager.atlas().as_image_view(),
                        &mut descriptor_writes,
                    );
                    self.vk_device
                        .update_descriptor_sets(&descriptor_writes, &[]);
                }

                let glyph_atlas_mgr = glyph_atlas_per_scale
                    .get_mut(&x.active_scale)
                    .expect("invalid state");
                // TODO: ここも重いようならあとで改善する
                for (&id, e) in &normalized_2d_static_mesh_textures {
                    let rect_index = id.rect_index();
                    if glyph_atlas_mgr
                        .atlas_rects
                        .get(rect_index)
                        .is_none_or(|x| x == &AtlasRect::EMPTY)
                    {
                        tracing::trace!(?id, "rasterize mesh");
                        let rect = glyph_atlas_mgr.manager.acquire(
                            (e.width * x.active_scale.value()).ceil() as _,
                            (e.height * x.active_scale.value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= rect_index {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(rect_index + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[rect_index] = rect;
                        if glyph_atlas_mgr
                            .vector_raster_state
                            .normalized_2d_mesh_requests
                            .insert(id, (rect.left, rect.top))
                            .is_none()
                        {
                            // new entry
                            glyph_atlas_mgr
                                .vector_raster_state
                                .updated_rects
                                .push(rect.vk_rect());
                        }
                    }
                }
                for (&id, e) in &shader_textures {
                    let rect_index = id.rect_index();
                    if glyph_atlas_mgr.atlas_rects.get(rect_index).is_none_or(|x| {
                        x == &AtlasRect {
                            left: 0,
                            top: 0,
                            right: 0,
                            bottom: 0,
                        }
                    }) {
                        let rect = glyph_atlas_mgr.color_manager.acquire(
                            (e.width * x.active_scale.value()).ceil() as _,
                            (e.height * x.active_scale.value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= rect_index {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(rect_index + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[rect_index] = rect;
                        tracing::trace!(?id, ?rect, "reserve rasterize shader tex");
                        // TODO: clone負荷が気になるようであればそのときに改修する
                        glyph_atlas_mgr
                            .shader_texture_rasterize_requests
                            .push((rect, e.clone()));
                    }
                }
                let needs_update_command = x.update(
                    current_t.as_secs_f32(),
                    &mut composite_tree,
                    &mut glyph_atlas_mgr.manager,
                    &glyph_atlas_mgr.color_manager,
                    &mut glyph_atlas_mgr.atlas_rects,
                    &mut glyph_atlas_mgr.vector_raster_state,
                    self.event_bus,
                    self.font_set,
                    &mut preview_composite,
                );

                let mut render_wait_semaphores = Vec::with_capacity(3);
                let mut render_wait_stages = Vec::with_capacity(3);

                // TODO: いったんめんどうなので毎回更新
                if true || needs_update_command {
                    x.submit_update_commands(&mut render_queue);

                    render_wait_semaphores.push(x.update_completion_semaphore_ref());
                    render_wait_stages.push(br::PipelineStageFlags::VERTEX_INPUT);
                }

                if x.render_requires_preview_composition {
                    render_wait_semaphores.push(dep_semaphore_preview.as_transparent_ref());
                    render_wait_stages.push(br::PipelineStageFlags::FRAGMENT_SHADER);
                    preview_composition_required = true;
                }

                render_wait_semaphores.push(x.backbuffer_ready_semaphore.as_transparent_ref());
                render_wait_stages.push(br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT);

                submit_parameters.push(SubmitParameters {
                    render_wait_semaphores,
                    render_wait_stages,
                    render_commands: vec![x.primary_render_commands_ref(backbuffer_index)],
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                });
                present_parameters.push(VkPresentParameters {
                    key: PresentKey::Window(*k),
                    swapchain_ref: x.swapchain_ref(),
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                    backbuffer_index,
                });
            }
            for (k, x) in context_menus.iter_mut() {
                crate::perf_scope!(UPDATE_CONTEXT_MENU);

                let backbuffer_index = match x.acquire_backbuffer_with_wait() {
                    Ok(x) => x,
                    Err(e) if e.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                        x.invalidate_swapchain();
                        any_swapchain_invalidated = true;
                        continue;
                    }
                    Err(e) => Err(e).expect("acquire next"),
                };

                #[cfg(feature = "wayland")]
                let new_ui_scale = x.take_latest_ui_scale_changes();
                #[cfg(feature = "wayland")]
                if let Some(scale) = new_ui_scale {
                    tracing::debug!(scale, "ContextMenuRescaleRender");
                    let scale = SafeF32::new(scale).expect("scale.invalid");

                    let current = glyph_atlas_per_scale
                        .get_mut(&x.active_scale)
                        .expect("invalid state");
                    current.ref_count -= 1;
                    let removed = if current.ref_count == 0 {
                        // no references
                        glyph_atlas_per_scale.remove(&x.active_scale)
                    } else {
                        None
                    };

                    let new_atlas_mgr = match glyph_atlas_per_scale.entry(scale) {
                        // reuse existing
                        std::collections::hash_map::Entry::Occupied(o) => o.into_mut(),
                        std::collections::hash_map::Entry::Vacant(v) => match removed {
                            // reuse existing with clear
                            Some(mut data) => {
                                data.ready_for_reuse();
                                v.insert(data)
                            }
                            // new one
                            None => v.insert(GlyphAtlasDataPerDpi::new(
                                &glyph_atlas_manager_common_resources,
                                &mut render_queue,
                            )),
                        },
                    };
                    new_atlas_mgr.ref_count += 1;

                    x.rescale(scale);
                    x.invalidate_render_commands(); // DescriptorSetをかえるときは再度つくりなおす必要がある
                    let mut descriptor_writes = Vec::with_capacity(1);
                    x.composite_renderer.rebind_glyph_atlas(
                        new_atlas_mgr.manager.atlas().as_image_view(),
                        new_atlas_mgr.color_manager.atlas().as_image_view(),
                        &mut descriptor_writes,
                    );
                    self.vk_device
                        .update_descriptor_sets(&descriptor_writes, &[]);
                }

                let glyph_atlas_mgr = glyph_atlas_per_scale
                    .get_mut(&x.active_scale)
                    .expect("invalid state");
                // TODO: ここも重いようならあとで改善する
                for (&id, e) in &normalized_2d_static_mesh_textures {
                    let rect_index = id.rect_index();
                    if glyph_atlas_mgr.atlas_rects.get(rect_index).is_none_or(|x| {
                        x == &AtlasRect {
                            left: 0,
                            top: 0,
                            right: 0,
                            bottom: 0,
                        }
                    }) {
                        tracing::trace!(?id, "rasterize mesh");
                        let rect = glyph_atlas_mgr.manager.acquire(
                            (e.width * x.active_scale.value()).ceil() as _,
                            (e.height * x.active_scale.value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= rect_index {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(rect_index + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[rect_index] = rect;
                        if glyph_atlas_mgr
                            .vector_raster_state
                            .normalized_2d_mesh_requests
                            .insert(id, (rect.left, rect.top))
                            .is_none()
                        {
                            // new entry
                            glyph_atlas_mgr
                                .vector_raster_state
                                .updated_rects
                                .push(rect.vk_rect());
                        }
                    }
                }
                for (&id, e) in &shader_textures {
                    let rect_index = id.rect_index();
                    if glyph_atlas_mgr.atlas_rects.get(rect_index).is_none_or(|x| {
                        x == &AtlasRect {
                            left: 0,
                            top: 0,
                            right: 0,
                            bottom: 0,
                        }
                    }) {
                        let rect = glyph_atlas_mgr.color_manager.acquire(
                            (e.width * x.active_scale.value()).ceil() as _,
                            (e.height * x.active_scale.value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= rect_index {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(rect_index + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[rect_index] = rect;
                        tracing::trace!(?id, ?rect, "reserve rasterize shader tex");
                        // TODO: clone負荷が気になるようであればそのときに改修する
                        glyph_atlas_mgr
                            .shader_texture_rasterize_requests
                            .push((rect, e.clone()));
                    }
                }
                let needs_update_command = x.update(
                    current_t.as_secs_f32(),
                    &mut composite_tree,
                    &mut glyph_atlas_mgr.manager,
                    &glyph_atlas_mgr.color_manager,
                    &mut glyph_atlas_mgr.atlas_rects,
                    &mut glyph_atlas_mgr.vector_raster_state,
                    self.event_bus,
                    self.font_set,
                );

                let mut render_wait_semaphores = Vec::with_capacity(1);
                let mut render_wait_stages = Vec::with_capacity(1);

                // TODO: いったんめんどうなので毎回更新
                if true || needs_update_command {
                    x.submit_update_commands(&mut render_queue);

                    render_wait_semaphores.push(x.update_completion_semaphore_ref());
                    render_wait_stages.push(br::PipelineStageFlags::VERTEX_INPUT);
                }

                #[cfg(not(windows))]
                render_wait_semaphores.push(x.backbuffer_ready_semaphore.as_transparent_ref());
                #[cfg(not(windows))]
                render_wait_stages.push(br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT);

                submit_parameters.push(SubmitParameters {
                    render_wait_semaphores,
                    render_wait_stages,
                    render_commands: vec![x.primary_render_commands_ref(backbuffer_index)],
                    #[cfg(windows)]
                    render_signal_semaphores: vec![],
                    #[cfg(not(windows))]
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                });
                #[cfg(not(windows))]
                present_parameters.push(VkPresentParameters {
                    key: PresentKey::ContextMenu(*k),
                    swapchain_ref: x.swapchain.as_transparent_ref(),
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                    backbuffer_index,
                });
                #[cfg(windows)]
                {
                    present_swapchains.push(x.swapchain.clone());
                }
            }

            for (s, x) in glyph_atlas_per_scale.iter_mut() {
                if !x.vector_raster_state.is_empty() {
                    // vector rasterization required
                    crate::perf_scope!(RENDER_VG_MASK);
                    x.manager.perform_render(
                        &x.vector_raster_state,
                        &vg_render_formats,
                        &glyph_atlas_manager_common_resources,
                        &mut render_queue,
                        &normalized_2d_static_mesh_textures,
                        s.value(),
                    );
                    x.vector_raster_state.clear();
                }

                if !x.shader_texture_rasterize_requests.is_empty() {
                    x.color_manager.perform_render(
                        &x.shader_texture_rasterize_requests,
                        s.value(),
                        self.vk_device,
                        &mut render_queue,
                    );
                    x.shader_texture_rasterize_requests.clear();
                }
            }

            crate::perf_begin!(perf = POST_QUEUE);
            if preview_composition_required {
                let ptr = preview_streaming_memory
                    .map(0..size_of::<PreviewStreamingBufferContent>() as _)
                    .expect("preview.streaming_memory.map");
                unsafe {
                    let ptr = ptr.ptr().cast::<PreviewStreamingBufferContent>();
                    core::ptr::write(
                        core::ptr::addr_of_mut!((*ptr).current_sec),
                        self.global_time_base.elapsed().as_secs_f32(),
                    );
                }
                if preview_streaming_memory_should_flush {
                    unsafe {
                        self.vk_device
                            .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new(
                                &preview_streaming_memory,
                                0..size_of::<PreviewStreamingBufferContent>() as _,
                            )])
                            .expect("preview.streaming_memory.flush");
                    }
                }
                unsafe {
                    preview_streaming_memory.unmap();
                }

                // 別でsubmitしないといけないらしい？
                render_queue
                    .submit(
                        &[br::SubmitInfo::new(
                            &[],
                            &[],
                            &[preview_command_buffer.as_transparent_ref()],
                            &[dep_semaphore_preview.as_transparent_ref()],
                        )],
                        None,
                    )
                    .expect("queue submit");
            }
            if !submit_parameters.is_empty() {
                render_queue
                    .submit(
                        &submit_parameters
                            .iter()
                            .map(|x| {
                                br::SubmitInfo::new(
                                    &x.render_wait_semaphores,
                                    &x.render_wait_stages,
                                    &x.render_commands,
                                    &x.render_signal_semaphores,
                                )
                            })
                            .collect::<Vec<_>>(),
                        None,
                    )
                    .expect("queue submit");
            }

            if !present_parameters.is_empty() {
                let mut results = present_parameters
                    .iter()
                    .map(|_| br::vk::VK_SUCCESS)
                    .collect::<Vec<_>>();
                match render_queue.present(&br::PresentInfo::new(
                    &present_parameters
                        .iter()
                        .map(|x| x.render_signal_semaphores[0])
                        .collect::<Vec<_>>(),
                    &present_parameters
                        .iter()
                        .map(|x| x.swapchain_ref)
                        .collect::<Vec<_>>(),
                    &present_parameters
                        .iter()
                        .map(|x| x.backbuffer_index)
                        .collect::<Vec<_>>(),
                    &mut results,
                )) {
                    Ok(_) => (),
                    Err(e) if e.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => (/* handled later */),
                    Err(e) => Err::<(), _>(e).expect("queue present"),
                }

                let present_keys = present_parameters
                    .into_iter()
                    .map(|x| x.key)
                    .collect::<Vec<_>>();
                for (r, p) in results.into_iter().zip(present_keys.into_iter()) {
                    if r == br::vk::VK_ERROR_OUT_OF_DATE_KHR {
                        match p {
                            PresentKey::Window(w) => windows
                                .get_mut(&w)
                                .expect("invalid entry")
                                .invalidate_swapchain(),
                            PresentKey::ContextMenu(w) => context_menus
                                .get_mut(&w)
                                .expect("invalid entry")
                                .invalidate_swapchain(),
                        }
                        any_swapchain_invalidated = true;
                    }
                }
            }
            crate::perf_end!(perf);

            crate::perf_begin!(perf = WAIT_QUEUE);
            render_queue.wait().expect("render_queue.wait");
            crate::perf_end!(perf);

            present_id += 1;
            self.event_bus
                .push(SyncEvent::NewPresentID { id: present_id });

            #[cfg(windows)]
            crate::perf_begin!(perf = WIN32_DX_PRESENT);
            #[cfg(windows)]
            if !present_swapchains.is_empty() {
                for x in present_swapchains {
                    unsafe {
                        x.Present(0, windows::Win32::Graphics::Dxgi::DXGI_PRESENT(0))
                            .ok()
                            .expect("swaphain.Present");
                    }
                }

                self.d3d12_present_counter += 1;
                let wait_for_counter = self.d3d12_present_counter;
                unsafe {
                    d3d12_present_fence
                        .SetEventOnCompletion(wait_for_counter, d3d12_present_fence_event)
                        .expect("d3d12_present_fence.SetEventOnCompletion");
                    self.dx_context
                        .d3d12_cq
                        .Signal(&d3d12_present_fence, wait_for_counter)
                        .expect("d3d12_cq.Wait");
                    windows::Win32::System::Threading::WaitForSingleObject(
                        d3d12_present_fence_event,
                        windows::Win32::System::Threading::INFINITE,
                    );
                }
            }
            #[cfg(windows)]
            crate::perf_end!(perf);

            // unsafe {
            //     manual_capture_end();
            // }
        }

        unsafe {
            // TODO: これdanglingつかわない方法にしたい スレッド専用のDeviceをつくるか......？
            br::vkfn_wrapper::device_wait_idle(br::VkHandleRefMut::dangling(
                self.vk_device.native_ptr(),
            ))
            .expect("device wait");
        }
        #[cfg(windows)]
        if let Err(e) =
            unsafe { windows::Win32::Foundation::CloseHandle(d3d12_present_fence_event) }
        {
            tracing::error!(reason = %e, "CloseHandle");
        }

        unsafe {
            preview_composite.drop(self.vk_device);
            composite_shared_buffers.drop(self.vk_device);
        }

        tracing::info!("RenderThread terminated");
    }
}

#[cfg(windows)]
struct CompositionSwapchainBuffer<'d> {
    vk_device: &'d VulkanDevice<'d>,
    d3d12_resource: windows::Win32::Graphics::Direct3D12::ID3D12Resource,
    shared_handle: windows::Win32::Foundation::HANDLE,
    vk_device_memory: br::vk::VkDeviceMemory,
    vk_image: br::vk::VkImage,
    vk_image_view: br::vk::VkImageView,
}
#[cfg(windows)]
impl<'d> Drop for CompositionSwapchainBuffer<'d> {
    fn drop(&mut self) {
        drop(unsafe {
            br::ImageViewObject::manage(
                self.vk_image_view,
                br::ImageObject::manage(
                    self.vk_image,
                    self.vk_device,
                    // 以下のデータはdropでは使われないので適当に埋める
                    br::vk::VK_IMAGE_TYPE_2D,
                    br::vk::VK_FORMAT_UNDEFINED,
                    br::Extent3D::spread1(1),
                ),
            )
        });
        drop(unsafe { br::DeviceMemoryObject::manage(self.vk_device_memory, self.vk_device) });
    }
}

struct ContextMenuRenderer<'d> {
    w: FlyoutSurfaceHandle,
    active_scale: SafeF32,
    vk_device: &'d VulkanDevice<'d>,
    composite_root: CompositeTreeRef,
    composite_renderer: BoundCompositeRenderer<'d>,
    last_composite_render_data: CompositeRenderingData,
    update_cp: br::CommandPoolObject<&'d VulkanDevice<'d>>,
    update_cb: br::CommandBufferObject<&'d VulkanDevice<'d>>,
    update_completion_fence: br::FenceObject<&'d VulkanDevice<'d>>,
    update_completion_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    updating: bool,
    render_cp: br::CommandPoolObject<&'d VulkanDevice<'d>>,
    render_cb: Vec<br::CommandBufferObject<&'d VulkanDevice<'d>>>,
    render_cb_invalid: bool,
    present_ready_semaphores: Vec<br::SemaphoreObject<&'d VulkanDevice<'d>>>,
    backbuffer_ready_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    swapchain_invalidated: bool,
    #[cfg(not(windows))]
    swapchain: VulkanSwapchain<'d, 'd>,
    #[cfg(not(windows))]
    surface: VulkanSurface<'d, 'd>,
    #[cfg(windows)]
    swapchain: windows::Win32::Graphics::Dxgi::IDXGISwapChain3,
    #[cfg(windows)]
    presentation_buffers: Vec<CompositionSwapchainBuffer<'d>>,
    #[cfg(windows)]
    presentation_size: br::Extent2D,
}
impl<'d> ContextMenuRenderer<'d> {
    fn new(
        device: &'d VulkanDevice<'d>,
        shared_buffers: &CompositeSharedBuffers,
        create_data: NewContextMenuData,
        init_scale: SafeF32,
        glyph_atlas: &TextureAtlas,
        color_atlas: &ColorTextureAtlas,
        event_bus: &SyncEventBus,
        #[cfg(windows)] dx_context: &crate::platform::windows::DxContext,
    ) -> Self {
        #[cfg(not(windows))]
        let surface = unsafe { create_data.vk_surface.0.bound(device) };
        #[cfg(not(windows))]
        let vk_swapchain = VulkanSwapchain::new(&surface, || create_data.w.pixels_size());

        #[cfg(windows)]
        let presentation_size = create_data.w.pixels_size().to_vk();
        #[cfg(windows)]
        let mut presentation_buffers = Vec::with_capacity(2);
        #[cfg(windows)]
        for n in 0..2 {
            use bedrock::{
                DeviceExternalMemoryWin32Extension, TypedVulkanSinkStructure, VulkanStructure,
            };

            let d3d12_resource: windows::Win32::Graphics::Direct3D12::ID3D12Resource = unsafe {
                create_data
                    .swapchain
                    .GetBuffer(n)
                    .expect("swapchain.GetBuffer")
            };
            let shared_handle = unsafe {
                dx_context
                    .d3d12_device
                    .CreateSharedHandle(
                        &d3d12_resource,
                        None,
                        windows::Win32::Foundation::GENERIC_ALL.0,
                        None,
                    )
                    .expect("d3d12_device.CreateSharedHandle")
            };

            let mut vk_image = br::ImageObject::new(
                device,
                &br::ImageCreateInfo::new(presentation_size, br::vk::VK_FORMAT_B8G8R8A8_UNORM)
                    .set_usage(br::ImageUsageFlags::COLOR_ATTACHMENT)
                    .with_next(&br::ExternalMemoryImageCreateInfo::new(
                        br::ExternalMemoryHandleTypeWin32::D3D12Resource as _,
                    )),
            )
            .expect("vk_image.create");
            let mut mreq_dedicated = br::vk::VkMemoryDedicatedRequirementsKHR::uninit_sink();
            let mut mreq = br::vk::VkMemoryRequirements2KHR::uninit_sink();
            unsafe {
                core::ptr::write(
                    core::ptr::addr_of_mut!((*mreq.as_mut_ptr()).pNext),
                    mreq_dedicated.as_mut_ptr().cast(),
                );
            }
            vk_image.requirements2().query(&mut mreq);
            let mreq = unsafe { mreq.assume_init_ref() };
            let mreq_dedicated = unsafe { mreq_dedicated.assume_init_ref() };
            let dedicated_allocation = mreq_dedicated.requiresDedicatedAllocation != 0;
            let mut import_props = br::vk::VkMemoryWin32HandlePropertiesKHR::uninit_sink();
            unsafe {
                device
                    .memory_win32_handle_properties(
                        br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                        core::mem::transmute(shared_handle),
                        &mut import_props,
                    )
                    .expect("device.memory_win32_handle_properties")
            };
            let import_props = unsafe { import_props.assume_init() };
            let memindex = device
                .find_device_local_memory_index(
                    mreq.memoryRequirements.memoryTypeBits & import_props.memoryTypeBits,
                )
                .expect("find_device_local_memory_index");
            let mut malloc_info_ext = br::ImportMemoryWin32HandleInfo::new(
                br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                unsafe { core::mem::transmute(shared_handle) },
                None,
            );
            let mut malloc_info_d =
                dedicated_allocation.then(|| br::MemoryDedicatedAllocateInfo::for_image(&vk_image));
            let mut malloc_info = br::MemoryAllocateInfo::new(1, memindex);
            br::chain_structures(
                [
                    malloc_info.as_generic_mut(),
                    malloc_info_ext.as_generic_mut(),
                ]
                .into_iter()
                .chain(
                    malloc_info_d
                        .iter_mut()
                        .map(br::VulkanStructure::as_generic_mut),
                ),
            );
            let vk_device_memory =
                br::DeviceMemoryObject::new(device, &malloc_info).expect("vk_device_memory.create");
            br::bind_memory(&mut vk_image, &vk_device_memory, 0).expect("vk_image.bind");
            let vk_image_view = br::ImageViewBuilder::new(
                vk_image,
                br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
            )
            .create()
            .expect("vk_image_view.create");

            device.dbg_set_name(&vk_image_view, c"ContextMenu.InteropImage.View");
            device.dbg_set_name(vk_image_view.image(), c"ContextMenu.InteropImage");
            device.dbg_set_name(&vk_device_memory, c"ContextMenu.InteropImage.Memory");

            let (vk_image_view, vk_image) = vk_image_view.unmanage();
            let vk_image = vk_image.unmanage().0;
            let vk_device_memory = vk_device_memory.unmanage().0;

            presentation_buffers.push(CompositionSwapchainBuffer {
                vk_device: device,
                d3d12_resource,
                shared_handle,
                // presentation_buffer,
                vk_device_memory,
                vk_image,
                vk_image_view,
            });
        }

        let mut update_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("update_cp.create");
        let [mut update_cb] = br::CommandBufferObject::alloc_array(
            device,
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

        let mut render_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("command pool create");
        let render_cb = br::CommandBufferObject::alloc(
            device,
            &br::CommandBufferAllocateInfo::new(
                &mut render_cp,
                #[cfg(not(windows))]
                {
                    vk_swapchain.image_count() as _
                },
                #[cfg(windows)]
                {
                    2
                },
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("command buffer alloc");
        for (n, x) in render_cb.iter().enumerate() {
            device.dbg_set_name(x, &unsafe {
                std::ffi::CString::from_vec_unchecked(
                    format!("Window Render Commands #{n}").into_bytes(),
                )
            });
        }

        let composite_renderer = BoundCompositeRenderer::new(
            device,
            shared_buffers,
            glyph_atlas.view(),
            color_atlas.view(),
            #[cfg(not(windows))]
            surface.format(),
            #[cfg(windows)]
            br::vk::VK_FORMAT_B8G8R8A8_UNORM,
            #[cfg(not(windows))]
            vk_swapchain.size(),
            #[cfg(windows)]
            presentation_size,
            #[cfg(not(windows))]
            vk_swapchain.image_view_refs(),
            #[cfg(windows)]
            presentation_buffers
                .iter()
                .map(|b| unsafe { br::VkHandleRef::dangling(b.vk_image_view) }),
        );

        event_bus.push(SyncEvent::FlyoutSurfacePostCreateRenderBuffer {
            target: create_data.w,
        });

        Self {
            w: create_data.w,
            active_scale: init_scale,

            vk_device: device,
            composite_root: create_data.composite_root,
            composite_renderer,
            last_composite_render_data: CompositeRenderingData::EMPTY,
            update_cp,
            update_cb,
            update_completion_fence: br::FenceObject::new(device, &br::FenceCreateInfo::new(0))
                .expect("update_completion_fence.create"),
            update_completion_semaphore: br::SemaphoreObject::new(
                device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("update_completion_semaphore.create"),
            updating: false,
            render_cp,
            render_cb,
            render_cb_invalid: true,
            #[cfg(not(windows))]
            present_ready_semaphores: (0..vk_swapchain.image_count())
                .map(|_| {
                    br::SemaphoreObject::new(device, &br::SemaphoreCreateInfo::new())
                        .expect("rendering_timeline_semaphore create")
                })
                .collect::<Vec<_>>(),
            #[cfg(windows)]
            present_ready_semaphores: (0..2)
                .map(|_| {
                    br::SemaphoreObject::new(device, &br::SemaphoreCreateInfo::new())
                        .expect("rendering_timeline_semaphore create")
                })
                .collect::<Vec<_>>(),
            backbuffer_ready_semaphore: br::SemaphoreObject::new(
                device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("backbuffer_ready_semaphore.create"),
            #[cfg(not(windows))]
            surface,
            #[cfg(not(windows))]
            swapchain: vk_swapchain,
            // #[cfg(windows)]
            // presentation_surface,
            #[cfg(windows)]
            swapchain: create_data.swapchain,
            #[cfg(windows)]
            presentation_buffers,
            #[cfg(windows)]
            presentation_size,
            swapchain_invalidated: false,
        }
    }

    #[cfg(feature = "wayland")]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        self.w.take_swapchain_externally_invalidation_signal()
    }
    #[cfg(not(feature = "wayland"))]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        false
    }

    // waylandのときだけRenderScaleがあとからくるので変更を受け付ける必要がある
    #[cfg(feature = "wayland")]
    pub fn take_latest_ui_scale_changes(&self) -> Option<f32> {
        self.w.take_latest_ui_scale_change()
    }

    #[cfg(not(windows))]
    pub fn rescale(&mut self, scale: SafeF32) {
        self.active_scale = scale;
    }

    pub fn update(
        &mut self,
        current_sec: f32,
        composite_tree: &mut CompositeTreeRender<SyncEvent>,
        glyph_atlas: &mut MaskTextureAtlasManager,
        color_atlas: &ColorTextureAtlasManager,
        mask_atlas_rects: &[AtlasRect],
        vector_raster_state: &mut VectorRasterizationState,
        events: &SyncEventBus,
        font_set: &FontSet,
    ) -> bool {
        let composite_render_data = self.composite_renderer.update(
            self.vk_device,
            composite_tree,
            self.composite_root,
            #[cfg(not(windows))]
            self.swapchain.size(),
            #[cfg(windows)]
            self.presentation_size,
            self.w.render_scale(),
            font_set,
            glyph_atlas,
            color_atlas,
            mask_atlas_rects,
            vector_raster_state,
            |e| events.push(e),
            current_sec,
        );
        if composite_render_data != self.last_composite_render_data {
            // requires repopulate render commands
            self.invalidate_render_commands();

            self.composite_renderer
                .prepare_input_backdrop_descriptor_sets(
                    self.vk_device,
                    composite_render_data.required_backdrop_buffer_count,
                );

            self.last_composite_render_data = composite_render_data;
        }

        self.composite_renderer
            .update_streaming_data(self.vk_device, CompositeStreamingData { current_sec });
        let needs_update_commands = self.composite_renderer.update_backdrop_resources(
            self.vk_device,
            #[cfg(not(windows))]
            self.surface.format(),
            #[cfg(windows)]
            br::vk::VK_FORMAT_B8G8R8A8_UNORM,
            #[cfg(not(windows))]
            self.swapchain.size(),
            #[cfg(windows)]
            self.presentation_size,
            self.last_composite_render_data
                .required_backdrop_buffer_count
                == 0,
        );

        // update_backdrop_resourcesでDescriptorSetの更新がはしるのでここでやる
        self.validate_render_commands();

        needs_update_commands
    }

    pub fn invalidate_swapchain(&mut self) {
        self.swapchain_invalidated = true;
    }

    pub fn validate_swapchain<'s>(
        &'s mut self,
        descriptor_writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        event_bus: &SyncEventBus,
    ) {
        if !self.swapchain_invalidated {
            // already valid
            return;
        }

        self.invalidate_render_commands();

        #[cfg(not(windows))]
        self.surface.refresh_caps();
        #[cfg(not(windows))]
        self.swapchain
            .recreate(&self.surface, || self.w.pixels_size());

        // recrease rt resources
        #[cfg(not(windows))]
        self.composite_renderer.recreate_rt_resources(
            self.vk_device,
            self.surface.format(),
            self.swapchain.image_view_refs(),
            self.swapchain.size(),
            descriptor_writes,
        );
        #[cfg(windows)]
        todo!("revalidate composition swapchain");

        event_bus.push(SyncEvent::FlyoutSurfacePostCreateRenderBuffer { target: self.w });
        self.swapchain_invalidated = false;
    }

    pub fn acquire_backbuffer_with_wait(&mut self) -> br::Result<u32> {
        #[cfg(windows)]
        {
            Ok(unsafe { self.swapchain.GetCurrentBackBufferIndex() })
        }

        #[cfg(not(windows))]
        {
            let backbuffer_index = self.swapchain.acquire_next(
                None,
                br::CompletionHandlerMut::Queue(
                    self.backbuffer_ready_semaphore.as_transparent_ref_mut(),
                ),
            )?;
            /*self.backbuffer_ready_fence
                .wait()
                .expect("last render completion fence wait");
            self.backbuffer_ready_fence
                .reset()
                .expect("last render completion fence reset");*/

            Ok(backbuffer_index)
        }
    }

    pub fn invalidate_render_commands(&mut self) {
        if self.render_cb_invalid {
            // already invalid
            return;
        }

        unsafe {
            self.render_cp
                .reset(br::CommandPoolResetFlags::EMPTY)
                .expect("render_cp.reset");
        }
        self.render_cb_invalid = true;
    }

    pub fn validate_render_commands(&mut self) {
        if !self.render_cb_invalid {
            // already valid
            return;
        }

        for (n, cb) in self.render_cb.iter_mut().enumerate() {
            unsafe {
                cb.begin(&br::CommandBufferBeginInfo::new())
                    .expect("command buffer begin")
            }
            .inject(|r| {
                self.composite_renderer.populate_commands(
                    r,
                    self.vk_device,
                    &self.last_composite_render_data,
                    #[cfg(windows)]
                    self.presentation_size,
                    #[cfg(not(windows))]
                    self.swapchain.size(),
                    #[cfg(windows)]
                    br::VkHandleRef::from_raw_ref(&self.presentation_buffers[n].vk_image),
                    #[cfg(not(windows))]
                    &self.swapchain.image_ref(n),
                    n,
                    &mut EmptyCustomRenderHandler,
                )
            })
            .inject(|r| self.vk_device.cmd_end_render_pass(r))
            .end()
            .expect("command buffer end");
        }

        self.render_cb_invalid = false;
    }

    pub fn wait_for_last_update_completion(&mut self) {
        if !self.updating {
            // no updating work
            return;
        }

        self.update_completion_fence
            .wait()
            .expect("update_completion_fence.wait");
        self.update_completion_fence
            .reset()
            .expect("update_completion_fence.reset");
        self.updating = false;
    }

    pub fn repopulate_update_commands(&mut self) {
        unsafe {
            self.update_cp
                .reset(br::CommandPoolResetFlags::EMPTY)
                .expect("update_cp.reset");
        }
        unsafe {
            self.update_cb
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("update_cb.begin")
        }
        .inject(|r| self.composite_renderer.sync_buffer(r))
        .end()
        .expect("update_cb.end");
    }

    pub fn submit_update_commands(&mut self, device_queue: &mut br::QueueObject<&'d VulkanDevice>) {
        self.wait_for_last_update_completion();
        self.repopulate_update_commands();

        device_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[self.update_cb.as_transparent_ref()],
                    &[self.update_completion_semaphore.as_transparent_ref()],
                )],
                Some(self.update_completion_fence.as_transparent_ref_mut()),
            )
            .expect("gfx.update.submit");
        self.updating = true;
    }

    /*pub fn swapchain_ref<'x>(&'x self) -> br::VkHandleRef<'x, br::vk::VkSwapchainKHR> {
        self.swapchain.as_transparent_ref()
    }*/

    pub fn update_completion_semaphore_ref<'x>(
        &'x self,
    ) -> br::VkHandleRef<'x, br::vk::VkSemaphore> {
        self.update_completion_semaphore.as_transparent_ref()
    }

    pub fn primary_render_commands_ref<'x>(
        &'x self,
        backbuffer_index: u32,
    ) -> br::VkHandleRef<'x, br::vk::VkCommandBuffer> {
        self.render_cb[backbuffer_index as usize].as_transparent_ref()
    }

    pub fn present_ready_semaphore_ref<'x>(
        &'x self,
        backbuffer_index: u32,
    ) -> br::VkHandleRef<'x, br::vk::VkSemaphore> {
        self.present_ready_semaphores[backbuffer_index as usize].as_transparent_ref()
    }
}

struct WindowRenderer<'d> {
    w: crate::WindowHandle,
    active_scale: SafeF32,
    latest_ui_scale_changes: *const Mutex<Option<f32>>,
    vk_device: &'d VulkanDevice<'d>,
    swapchain_invalidated: bool,
    composite_root: CompositeTreeRef,
    composite_renderer: BoundCompositeRenderer<'d>,
    corner_cutout_renderer: Option<CornerCutoutRenderer<'d, 'd>>,
    last_composite_render_data: CompositeRenderingData,
    update_cp: br::CommandPoolObject<&'d VulkanDevice<'d>>,
    update_cb: br::CommandBufferObject<&'d VulkanDevice<'d>>,
    update_completion_fence: br::FenceObject<&'d VulkanDevice<'d>>,
    update_completion_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    updating: bool,
    render_cp: br::CommandPoolObject<&'d VulkanDevice<'d>>,
    render_cb: Vec<br::CommandBufferObject<&'d VulkanDevice<'d>>>,
    render_cb_invalid: bool,
    render_requires_preview_composition: bool,
    present_ready_semaphores: Vec<br::SemaphoreObject<&'d VulkanDevice<'d>>>,
    backbuffer_ready_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    swapchain: VulkanSwapchain<'d, 'd>,
    surface: VulkanSurface<'d, 'd>,
}
impl<'d> WindowRenderer<'d> {
    fn new(
        device: &'d VulkanDevice<'d>,
        shared_buffers: &CompositeSharedBuffers,
        create_data: NewWindowData,
        init_scale: SafeF32,
        glyph_atlas: &TextureAtlas,
        color_atlas: &ColorTextureAtlas,
        event_bus: &SyncEventBus,
    ) -> Self {
        let surface = unsafe { create_data.vk_surface.0.bound(device) };
        let vk_swapchain = VulkanSwapchain::new(&surface, || create_data.key.pixels_client_size());

        let mut update_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("update_cp.create");
        let [mut update_cb] = br::CommandBufferObject::alloc_array(
            device,
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

        let mut render_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("command pool create");
        let render_cb = br::CommandBufferObject::alloc(
            device,
            &br::CommandBufferAllocateInfo::new(
                &mut render_cp,
                vk_swapchain.image_count() as _,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("command buffer alloc");
        for (n, x) in render_cb.iter().enumerate() {
            device.dbg_set_name(x, &unsafe {
                std::ffi::CString::from_vec_unchecked(
                    format!("Window Render Commands #{n}").into_bytes(),
                )
            });
        }

        let composite_renderer = BoundCompositeRenderer::new(
            device,
            shared_buffers,
            glyph_atlas.view(),
            color_atlas.view(),
            surface.format(),
            vk_swapchain.size(),
            vk_swapchain.image_view_refs(),
        );
        let corner_cutout_renderer = if create_data.key.needs_corner_cutout_rendering() {
            Some(CornerCutoutRenderer::new(
                device,
                composite_renderer.subpass_final(),
                composite_renderer.subpass_continue_final(),
            ))
        } else {
            None
        };

        event_bus.push(SyncEvent::WindowPostCreateRenderBuffer {
            window: create_data.key,
        });

        Self {
            w: create_data.key,
            active_scale: init_scale,
            latest_ui_scale_changes: create_data.key.latest_ui_scale_changes(),
            vk_device: device,
            composite_root: create_data.key.ct_root(),
            composite_renderer,
            corner_cutout_renderer,
            last_composite_render_data: CompositeRenderingData::EMPTY,
            update_cp,
            update_cb,
            update_completion_fence: br::FenceObject::new(device, &br::FenceCreateInfo::new(0))
                .expect("update_completion_fence.create"),
            update_completion_semaphore: br::SemaphoreObject::new(
                device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("update_completion_semaphore.create"),
            updating: false,
            render_cp,
            render_cb,
            render_cb_invalid: true,
            render_requires_preview_composition: false,
            present_ready_semaphores: (0..vk_swapchain.image_count())
                .map(|_| {
                    br::SemaphoreObject::new(device, &br::SemaphoreCreateInfo::new())
                        .expect("rendering_timeline_semaphore create")
                })
                .collect::<Vec<_>>(),
            backbuffer_ready_semaphore: br::SemaphoreObject::new(
                device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("backbuffer_ready_semaphore.create"),
            surface,
            swapchain: vk_swapchain,
            swapchain_invalidated: false,
        }
    }

    pub fn take_latest_ui_scale_changes(&self) -> Option<f32> {
        unsafe { &(*self.latest_ui_scale_changes) }
            .lock()
            .expect("poisoned")
            .take()
    }

    pub fn update(
        &mut self,
        current_sec: f32,
        composite_tree: &mut CompositeTreeRender<SyncEvent>,
        glyph_atlas: &mut MaskTextureAtlasManager,
        color_atlas: &ColorTextureAtlasManager,
        mask_atlas_rects: &[AtlasRect],
        vector_raster_state: &mut VectorRasterizationState,
        events: &SyncEventBus,
        font_set: &FontSet,
        preview_composite: &mut PreviewComposite,
    ) -> bool {
        let composite_render_data = self.composite_renderer.update(
            self.vk_device,
            composite_tree,
            self.composite_root,
            self.swapchain.size(),
            self.w.ui_scale_factor(),
            font_set,
            glyph_atlas,
            color_atlas,
            mask_atlas_rects,
            vector_raster_state,
            |e| events.push(e),
            current_sec,
        );
        if composite_render_data != self.last_composite_render_data {
            // requires repopulate render commands
            self.invalidate_render_commands();

            self.composite_renderer
                .prepare_input_backdrop_descriptor_sets(
                    self.vk_device,
                    composite_render_data.required_backdrop_buffer_count,
                );

            self.last_composite_render_data = composite_render_data;
        }

        self.composite_renderer
            .update_streaming_data(self.vk_device, CompositeStreamingData { current_sec });
        let needs_update_commands = self.composite_renderer.update_backdrop_resources(
            self.vk_device,
            self.surface.format(),
            self.swapchain.size(),
            self.last_composite_render_data
                .required_backdrop_buffer_count
                == 0,
        );

        // update_backdrop_resourcesでDescriptorSetの更新がはしるのでここでやる
        self.validate_render_commands(preview_composite);

        needs_update_commands
    }

    #[cfg(any(feature = "wayland", target_os = "macos"))]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        self.w.take_swapchain_externally_invalidation_signal()
    }
    #[cfg(not(any(feature = "wayland", target_os = "macos")))]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        false
    }

    pub fn invalidate_swapchain(&mut self) {
        self.swapchain_invalidated = true;
    }

    pub fn validate_swapchain<'s>(
        &'s mut self,
        descriptor_writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        event_bus: &SyncEventBus,
    ) {
        if !self.swapchain_invalidated {
            // already valid
            return;
        }

        self.invalidate_render_commands();

        self.surface.refresh_caps();
        self.swapchain
            .recreate(&self.surface, || self.w.pixels_client_size());

        // recrease rt resources
        self.composite_renderer.recreate_rt_resources(
            self.vk_device,
            self.surface.format(),
            self.swapchain.image_view_refs(),
            self.swapchain.size(),
            descriptor_writes,
        );

        event_bus.push(SyncEvent::WindowPostCreateRenderBuffer { window: self.w });
        self.swapchain_invalidated = false;
    }

    pub fn acquire_backbuffer_with_wait(&mut self) -> br::Result<u32> {
        let backbuffer_index = self.swapchain.acquire_next(
            None,
            br::CompletionHandlerMut::Queue(
                self.backbuffer_ready_semaphore.as_transparent_ref_mut(),
            ),
        )?;
        /*self.backbuffer_ready_fence
            .wait()
            .expect("last render completion fence wait");
        self.backbuffer_ready_fence
            .reset()
            .expect("last render completion fence reset");*/

        Ok(backbuffer_index)
    }

    pub fn invalidate_render_commands(&mut self) {
        if self.render_cb_invalid {
            // already invalid
            return;
        }

        unsafe {
            self.render_cp
                .reset(br::CommandPoolResetFlags::EMPTY)
                .expect("render_cp.reset");
        }
        self.render_cb_invalid = true;
    }

    pub fn validate_render_commands(&mut self, preview_composite: &mut PreviewComposite) {
        if !self.render_cb_invalid {
            // already valid
            return;
        }

        self.render_requires_preview_composition = false;
        for (n, cb) in self.render_cb.iter_mut().enumerate() {
            unsafe {
                cb.begin(&br::CommandBufferBeginInfo::new())
                    .expect("command buffer begin")
            }
            .inject(|r| {
                self.composite_renderer.populate_commands(
                    r,
                    self.vk_device,
                    &self.last_composite_render_data,
                    self.swapchain.size(),
                    &self.swapchain.image_ref(n),
                    n,
                    &mut CustomRenderHandlerFn(|t, s, pm, ctx, r| match t {
                        PREVIEW_COMPOSITE => {
                            self.render_requires_preview_composition = true;
                            preview_composite.update(
                                self.vk_device,
                                ctx.rt_size,
                                ctx.active_render_pass,
                                ctx.active_subpass_index,
                            );

                            r.bind_pipeline(
                                br::PipelineBindPoint::Graphics,
                                br::VkHandleRef::from_raw_ref(&preview_composite.pipeline),
                            )
                            .push_constant(
                                br::VkHandleRef::from_raw_ref(&preview_composite.pipeline_layout),
                                br::vk::VK_SHADER_STAGE_VERTEX_BIT
                                    | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                                0,
                                &pm.clone().transpose(),
                            )
                            .push_constant_slice(
                                br::VkHandleRef::from_raw_ref(&preview_composite.pipeline_layout),
                                br::vk::VK_SHADER_STAGE_VERTEX_BIT
                                    | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                                size_of::<[f32; 16]>() as _,
                                &[
                                    s.width as f32,
                                    s.height as f32,
                                    ctx.rt_size.width as f32,
                                    ctx.rt_size.height as f32,
                                ],
                            )
                            .bind_descriptor_sets(
                                br::PipelineBindPoint::Graphics,
                                &br::VkHandleRef::from_raw_ref(&preview_composite.pipeline_layout),
                                0,
                                &[preview_composite.descriptor_set],
                                &[],
                            )
                            .draw(4, 1, 0, 0)
                        }
                        _ => r,
                    }),
                )
            })
            .inject(|r| {
                if self.w.is_maximized() {
                    // no curout rendering when maximized
                    return r;
                }
                let Some(ref renderer) = self.corner_cutout_renderer else {
                    // no corner cutout rendering required
                    return r;
                };

                let rt_pixel_size = self.w.pixels_client_size();
                let rt_logical_size = self.w.client_size();

                let r = if self
                    .last_composite_render_data
                    .render_passes
                    .last()
                    .is_some_and(|x| x.continued)
                {
                    // use continued pass
                    r.bind_pipeline(br::PipelineBindPoint::Graphics, &renderer.pipeline_cont)
                } else {
                    r.bind_pipeline(br::PipelineBindPoint::Graphics, &renderer.pipeline)
                };

                r.set_viewport(
                    0,
                    &[br::Viewport {
                        x: 0.0,
                        y: 0.0,
                        width: rt_pixel_size.width as _,
                        height: rt_pixel_size.height as _,
                        minDepth: 0.0,
                        maxDepth: 1.0,
                    }],
                )
                .set_scissor(
                    0,
                    &[br::Rect2D {
                        offset: br::Offset2D::ZERO,
                        extent: br::Extent2D {
                            width: rt_pixel_size.width,
                            height: rt_pixel_size.height,
                        },
                    }],
                )
                .push_constant(
                    &renderer.pipeline_layout,
                    br::vk::VK_SHADER_STAGE_ALL_GRAPHICS,
                    0,
                    &CornerCutoutPushConstants {
                        screen_size: [rt_logical_size.width as _, rt_logical_size.height as _],
                    },
                )
                .draw(4, 4, 0, 0)
            })
            .inject(|r| self.vk_device.cmd_end_render_pass(r))
            .end()
            .expect("command buffer end");
        }

        self.render_cb_invalid = false;
    }

    pub fn wait_for_last_update_completion(&mut self) {
        if !self.updating {
            // no updating work
            return;
        }

        self.update_completion_fence
            .wait()
            .expect("update_completion_fence.wait");
        self.update_completion_fence
            .reset()
            .expect("update_completion_fence.reset");
        self.updating = false;
    }

    pub fn repopulate_update_commands(&mut self) {
        unsafe {
            self.update_cp
                .reset(br::CommandPoolResetFlags::EMPTY)
                .expect("update_cp.reset");
        }
        unsafe {
            self.update_cb
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("update_cb.begin")
        }
        .inject(|r| self.composite_renderer.sync_buffer(r))
        .end()
        .expect("update_cb.end");
    }

    pub fn submit_update_commands(&mut self, device_queue: &mut br::QueueObject<&'d VulkanDevice>) {
        self.wait_for_last_update_completion();
        self.repopulate_update_commands();

        device_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[self.update_cb.as_transparent_ref()],
                    &[self.update_completion_semaphore.as_transparent_ref()],
                )],
                Some(self.update_completion_fence.as_transparent_ref_mut()),
            )
            .expect("gfx.update.submit");
        self.updating = true;
    }

    pub fn swapchain_ref<'x>(&'x self) -> br::VkHandleRef<'x, br::vk::VkSwapchainKHR> {
        self.swapchain.as_transparent_ref()
    }

    pub fn update_completion_semaphore_ref<'x>(
        &'x self,
    ) -> br::VkHandleRef<'x, br::vk::VkSemaphore> {
        self.update_completion_semaphore.as_transparent_ref()
    }

    pub fn primary_render_commands_ref<'x>(
        &'x self,
        backbuffer_index: u32,
    ) -> br::VkHandleRef<'x, br::vk::VkCommandBuffer> {
        self.render_cb[backbuffer_index as usize].as_transparent_ref()
    }

    pub fn present_ready_semaphore_ref<'x>(
        &'x self,
        backbuffer_index: u32,
    ) -> br::VkHandleRef<'x, br::vk::VkSemaphore> {
        self.present_ready_semaphores[backbuffer_index as usize].as_transparent_ref()
    }
}

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

#[derive(Clone)]
pub struct GlyphAtlasRenderingFormats {
    pub color: br::Format,
    pub stencil: br::Format,
}

pub struct GlyphAtlasManagerCommonResources<'d> {
    device: &'d VulkanDevice<'d>,
    fill_shader_module: br::ShaderModuleObject<&'d VulkanDevice<'d>>,
    curve_shader_module: br::ShaderModuleObject<&'d VulkanDevice<'d>>,
    vec_tri_fill_shader_module: br::ShaderModuleObject<&'d VulkanDevice<'d>>,
    fill_tri_white_shader_module: br::ShaderModuleObject<&'d VulkanDevice<'d>>,
    render_pass: br::RenderPassObject<&'d VulkanDevice<'d>>,
    pipeline_layout: br::PipelineLayoutObject<&'d VulkanDevice<'d>>,
}
impl<'d> GlyphAtlasManagerCommonResources<'d> {
    pub fn new(vk_device: &'d VulkanDevice, formats: &GlyphAtlasRenderingFormats) -> Self {
        let fill_shader_module = vk_device.require_shader("vg-fill.spv");
        let curve_shader_module = vk_device.require_shader("vg-curve.spv");
        let vec_tri_fill_shader_module = vk_device.require_shader("vec-tri-fill.spv");
        let fill_tri_white_shader_module = vk_device.require_shader("tri-fill-white.spv");

        let render_pass = br::RenderPassObject::new(
            vk_device,
            &br::RenderPassCreateInfo2::new(
                &[
                    br::AttachmentDescription2::new(formats.stencil)
                        .stencil_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
                        .layout_transition(
                            br::ImageLayout::Undefined,
                            br::ImageLayout::DepthStencilReadOnlyOpt,
                        )
                        .samples(TextureAtlas::MULTISAMPLE_LEVEL),
                    br::AttachmentDescription2::new(formats.color)
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)
                        .layout_transition(
                            br::ImageLayout::Undefined,
                            br::ImageLayout::TransferSrcOpt,
                        )
                        .samples(TextureAtlas::MULTISAMPLE_LEVEL),
                ],
                &[
                    br::SubpassDescription2::new()
                        .depth_stencil(&br::AttachmentReference2::depth_stencil_attachment_opt(0)),
                    br::SubpassDescription2::new()
                        .depth_stencil(&br::AttachmentReference2::depth_stencil_readonly_opt(0))
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
        .expect("render_pass.create");
        let pipeline_layout =
            br::PipelineLayoutObject::new(vk_device, &br::PipelineLayoutCreateInfo::new(&[], &[]))
                .expect("pipeline_layout.create");

        vk_device.dbg_set_name(&render_pass, c"GlyphAtlasManager.VgRasterize.RenderPass");
        vk_device.dbg_set_name(
            &pipeline_layout,
            c"GlyphAtlasManager.VgRasterize.PipelineLayout",
        );

        Self {
            device: vk_device,
            fill_shader_module,
            curve_shader_module,
            vec_tri_fill_shader_module,
            fill_tri_white_shader_module,
            render_pass,
            pipeline_layout,
        }
    }
}

#[repr(C)]
struct CornerCutoutPushConstants {
    pub screen_size: [f32; 2],
}

struct CornerCutoutRenderer<'d, 'fs> {
    _shader: br::ShaderModuleObject<&'d VulkanDevice<'fs>>,
    pipeline_layout: br::PipelineLayoutObject<&'d VulkanDevice<'fs>>,
    pipeline: br::PipelineObject<&'d VulkanDevice<'fs>>,
    pipeline_cont: br::PipelineObject<&'d VulkanDevice<'fs>>,
}
impl<'d, 'fs> CornerCutoutRenderer<'d, 'fs> {
    fn new(
        device: &'d VulkanDevice<'fs>,
        rendered_pass: br::SubpassRef<impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized>,
        rendered_pass_cont: br::SubpassRef<
            impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized,
        >,
    ) -> Self {
        let shader = device.require_shader("corner-cutout.spv");
        let pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[],
                &[
                    br::PushConstantRange::for_type::<CornerCutoutPushConstants>(
                        br::vk::VK_SHADER_STAGE_ALL_GRAPHICS,
                        0,
                    ),
                ],
            ),
        )
        .expect("pipeline_layout.create");
        let blending = br::PipelineColorBlendStateCreateInfo::new(&[
            br::PipelineColorBlendAttachmentState(br::vk::VkPipelineColorBlendAttachmentState {
                blendEnable: true as _,
                srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_ZERO,
                dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
                colorBlendOp: br::vk::VK_BLEND_OP_ADD,
                srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ONE,
                dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
                alphaBlendOp: br::vk::VK_BLEND_OP_ADD,
                colorWriteMask: br::vk::VK_COLOR_COMPONENT_R_BIT
                    | br::vk::VK_COLOR_COMPONENT_G_BIT
                    | br::vk::VK_COLOR_COMPONENT_B_BIT
                    | br::vk::VK_COLOR_COMPONENT_A_BIT,
            }),
            // br::vk::VkPipelineColorBlendAttachmentState::PREMULTIPLIED,
        ]);
        let [pipeline, pipeline_cont] = device
            .create_graphics_pipelines_array(&[
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    rendered_pass,
                    &[
                        shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                        shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRISTRIP,
                    &br::PipelineViewportStateCreateInfo::new_dynamic(1),
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    &blending,
                )
                .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
                .set_dynamic_state(&br::PipelineDynamicStateCreateInfo::new(&[
                    br::vk::VK_DYNAMIC_STATE_VIEWPORT,
                    br::vk::VK_DYNAMIC_STATE_SCISSOR,
                ])),
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    rendered_pass_cont,
                    &[
                        shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                        shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRISTRIP,
                    &br::PipelineViewportStateCreateInfo::new_dynamic(1),
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    &blending,
                )
                .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
                .set_dynamic_state(&br::PipelineDynamicStateCreateInfo::new(&[
                    br::vk::VK_DYNAMIC_STATE_VIEWPORT,
                    br::vk::VK_DYNAMIC_STATE_SCISSOR,
                ])),
            ])
            .expect("pipeline.create");

        Self {
            _shader: shader,
            pipeline_layout,
            pipeline,
            pipeline_cont,
        }
    }
}

pub struct MaskTextureAtlasManager<'d> {
    device: &'d VulkanDevice<'d>,
    atlas: TextureAtlas,
    acquired_glyph_rects: HashMap<(usize, u16), AtlasRect>,
    rounded_fill_rects_by_radius: HashMap<SafeF32, AtlasRect>,
    triangle_fans_pipeline: br::PipelineObject<&'d VulkanDevice<'d>>,
    curve_pipeline: br::PipelineObject<&'d VulkanDevice<'d>>,
    colorize_pipeline: br::PipelineObject<&'d VulkanDevice<'d>>,
    fill_tri_white_pipeline: br::PipelineObject<&'d VulkanDevice<'d>>,
}
impl Drop for MaskTextureAtlasManager<'_> {
    fn drop(&mut self) {
        unsafe {
            self.atlas.drop(self.device);
        }
    }
}
impl<'d> MaskTextureAtlasManager<'d> {
    const VI_STATE_FOR_TRIS: &'static br::PipelineVertexInputStateCreateInfo<'static> =
        &br::PipelineVertexInputStateCreateInfo::new(
            &[br::VertexInputBindingDescription::per_vertex_typed::<
                [f32; 2],
            >(0)],
            &[br::VertexInputAttributeDescription(
                br::vk::VkVertexInputAttributeDescription {
                    location: 0,
                    binding: 0,
                    offset: 0,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                },
            )],
        );
    const VI_STATE_FOR_CURVE: &'static br::PipelineVertexInputStateCreateInfo<'static> =
        &br::PipelineVertexInputStateCreateInfo::new(
            &[br::VertexInputBindingDescription::per_vertex_typed::<
                [f32; 4],
            >(0)],
            &[
                br::VertexInputAttributeDescription(br::vk::VkVertexInputAttributeDescription {
                    location: 0,
                    binding: 0,
                    offset: 0,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                }),
                br::VertexInputAttributeDescription(br::vk::VkVertexInputAttributeDescription {
                    location: 1,
                    binding: 0,
                    offset: core::mem::size_of::<[f32; 2]>() as _,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                }),
            ],
        );
    const STENCIL_MASK: u32 = 0x01;
    const STENCIL_STATE_INVERT: &'static br::PipelineDepthStencilStateCreateInfo =
        &br::PipelineDepthStencilStateCreateInfo::new()
            .stencil_test(true)
            .stencil_state_front(
                br::StencilOpState::always_forall(br::StencilOp::Invert)
                    .write_mask(Self::STENCIL_MASK),
            )
            .stencil_state_back(
                br::StencilOpState::always_forall(br::StencilOp::Invert)
                    .write_mask(Self::STENCIL_MASK),
            );
    const STENCIL_STATE_FILTER_EQ_ONLY: &'static br::PipelineDepthStencilStateCreateInfo =
        &br::PipelineDepthStencilStateCreateInfo::new()
            .stencil_test(true)
            .stencil_state_front(br::StencilOpState::NOP.set_compare(
                br::CompareOp::Equal,
                Self::STENCIL_MASK,
                Self::STENCIL_MASK,
            ))
            .stencil_state_back(br::StencilOpState::NOP.set_compare(
                br::CompareOp::Equal,
                Self::STENCIL_MASK,
                Self::STENCIL_MASK,
            ));

    pub fn new(
        common_res: &GlyphAtlasManagerCommonResources<'d>,
        init_worker_queue: &mut (impl br::QueueMut + ?Sized),
        init_worker_queue_family_index: u32,
    ) -> Self {
        let atlas = TextureAtlas::new(common_res.device);

        let viewports = [atlas
            .size()
            .into_rect(br::Offset2D::ZERO)
            .make_viewport(0.0..1.0)];
        let scissors = [atlas.size().into_rect(br::Offset2D::ZERO)];
        let vp_state = br::PipelineViewportStateCreateInfo::new(&viewports, &scissors);
        let ms_state = br::PipelineMultisampleStateCreateInfo::new()
            .rasterization_samples(TextureAtlas::MULTISAMPLE_LEVEL as _);
        let [
            triangle_fans_pipeline,
            curve_pipeline,
            colorize_pipeline,
            fill_tri_white_pipeline,
        ] = common_res
            .device
            .create_graphics_pipelines_array(&[
                br::GraphicsPipelineCreateInfo::new(
                    &common_res.pipeline_layout,
                    common_res.render_pass.subpass(0),
                    &[
                        common_res
                            .fill_shader_module
                            .on_stage(br::ShaderStage::Vertex, c"vertMain")
                            .with_specialization_info(&br::SpecializationInfo::new(
                                &FillShaderVertexConstants {
                                    target_texture_width: atlas.size().width as _,
                                    target_texture_height: atlas.size().height as _,
                                },
                            )),
                        common_res
                            .fill_shader_module
                            .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    Self::VI_STATE_FOR_TRIS,
                    IA_STATE_TRILIST,
                    &vp_state,
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(&ms_state)
                .set_depth_stencil_state(Self::STENCIL_STATE_INVERT),
                br::GraphicsPipelineCreateInfo::new(
                    &common_res.pipeline_layout,
                    common_res.render_pass.subpass(0),
                    &[
                        common_res
                            .curve_shader_module
                            .on_stage(br::ShaderStage::Vertex, c"vertMain")
                            .with_specialization_info(&br::SpecializationInfo::new(
                                &CurveShaderVertexConstants {
                                    target_texture_width: atlas.size().width as _,
                                    target_texture_height: atlas.size().height as _,
                                },
                            )),
                        common_res
                            .curve_shader_module
                            .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    Self::VI_STATE_FOR_CURVE,
                    IA_STATE_TRILIST,
                    &vp_state,
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(&ms_state)
                .set_depth_stencil_state(Self::STENCIL_STATE_INVERT),
                br::GraphicsPipelineCreateInfo::new(
                    &common_res.pipeline_layout,
                    common_res.render_pass.subpass(1),
                    &[
                        common_res
                            .vec_tri_fill_shader_module
                            .on_stage(br::ShaderStage::Vertex, c"vertMain"),
                        common_res
                            .vec_tri_fill_shader_module
                            .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRILIST,
                    &vp_state,
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(&ms_state)
                .set_depth_stencil_state(Self::STENCIL_STATE_FILTER_EQ_ONLY),
                br::GraphicsPipelineCreateInfo::new(
                    &common_res.pipeline_layout,
                    common_res.render_pass.subpass(1),
                    &[
                        common_res
                            .fill_tri_white_shader_module
                            .on_stage(br::ShaderStage::Vertex, c"vertMain"),
                        common_res
                            .fill_tri_white_shader_module
                            .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    Self::VI_STATE_FOR_TRIS,
                    IA_STATE_TRILIST,
                    &vp_state,
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(&ms_state),
            ])
            .expect("create vector rasterize pipelines");

        let mut init_cp = br::CommandPoolObject::new(
            common_res.device,
            &br::CommandPoolCreateInfo::new(init_worker_queue_family_index),
        )
        .expect("init_cp.create");
        let [mut init_cb] = br::CommandBufferObject::alloc_array(
            common_res.device,
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
            common_res.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        br::ImageMemoryBarrier2::new(&atlas.image(), atlas.image_range_entire())
                            .transit_to(br::ImageLayout::TransferDestOpt.from_undefined()),
                    ],
                ),
            )
        })
        .clear_color_image(
            &atlas.image(),
            br::ImageLayout::TransferDestOpt,
            &[br::ClearColorValue::from([0.0; 4])],
            &[br::ImageSubresourceRange::new(
                br::AspectMask::COLOR,
                0..1,
                0..1,
            )],
        )
        .inject(|r| {
            common_res.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        br::ImageMemoryBarrier2::new(&atlas.image(), atlas.image_range_entire())
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
                            ),
                    ],
                ),
            )
        })
        .end()
        .expect("init_cb.end");
        init_worker_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[init_cb.as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("init_cb.submit");
        init_worker_queue.wait().expect("init_cb.wait");

        Self {
            device: common_res.device,
            atlas,
            acquired_glyph_rects: HashMap::new(),
            rounded_fill_rects_by_radius: HashMap::new(),
            triangle_fans_pipeline,
            curve_pipeline,
            colorize_pipeline,
            fill_tri_white_pipeline,
        }
    }

    #[inline(always)]
    pub fn acquire(&mut self, width: u32, height: u32) -> AtlasRect {
        self.atlas.acquire(width, height)
    }

    pub fn acquire_for_glyph(
        &mut self,
        key: (usize, u16),
        width: u32,
        height: u32,
    ) -> (AtlasRect, bool) {
        match self.acquired_glyph_rects.entry(key) {
            std::collections::hash_map::Entry::Vacant(x) => {
                (x.insert(self.atlas.acquire(width, height)).clone(), true)
            }
            std::collections::hash_map::Entry::Occupied(x) => (x.get().clone(), false),
        }
    }

    pub fn acquire_for_rounded_fill_rect(&mut self, radius: SafeF32) -> (AtlasRect, bool) {
        match self.rounded_fill_rects_by_radius.entry(radius) {
            std::collections::hash_map::Entry::Vacant(x) => (
                x.insert(self.atlas.acquire(
                    (radius.value() * 2.0 + 1.0).ceil() as _,
                    (radius.value() * 2.0 + 1.0).ceil() as _,
                ))
                .clone(),
                true,
            ),
            std::collections::hash_map::Entry::Occupied(x) => (x.get().clone(), false),
        }
    }

    fn perform_render(
        &self,
        state: &VectorRasterizationState,
        formats: &GlyphAtlasRenderingFormats,
        common_res: &GlyphAtlasManagerCommonResources,
        render_worker_queue: &mut (impl br::QueueMut + ?Sized),
        normalized_2d_static_mesh_textures: &HashMap<TextureID, Normalized2DStaticMeshTexture>,
        scale: f32,
    ) {
        // TODO: 最適化はあとで
        let mut normalized_2d_static_mesh_vertices_fused = Vec::new();
        let mut normalized_2d_static_mesh_indices_fused = Vec::new();
        for (x, &(ox, oy)) in state.normalized_2d_mesh_requests.iter() {
            let e = &normalized_2d_static_mesh_textures[x];
            let v_offset = normalized_2d_static_mesh_vertices_fused.len();

            normalized_2d_static_mesh_vertices_fused.extend(e.vertices.iter().map(|&[x, y]| {
                [
                    2.0 * (x * e.width * scale + ox as f32) / self.atlas.size().width as f32 - 1.0,
                    2.0 * (y * e.height * scale + oy as f32) / self.atlas.size().height as f32
                        - 1.0,
                ]
            }));
            normalized_2d_static_mesh_indices_fused
                .extend(e.indices.iter().map(|i| *i + v_offset as u16));
        }

        let filltri_points_offset = 0;
        let filltri_indices_offset =
            filltri_points_offset + core::mem::size_of_val(&state.fill_tri_points[..]);
        let curve_triangles_offset = (filltri_indices_offset
            + core::mem::size_of_val(&state.fill_tri_indices[..])
            + (core::mem::size_of::<[f32; 4]>() - 1))
            & !(core::mem::size_of::<[f32; 4]>() - 1);
        let normalized_2d_static_mesh_vertices_offset =
            curve_triangles_offset + core::mem::size_of_val(&state.curve_tris[..]);
        let normalized_2d_static_mesh_indices_offset = normalized_2d_static_mesh_vertices_offset
            + core::mem::size_of_val(&normalized_2d_static_mesh_vertices_fused[..]);

        let vector_draw_buffer_total_size = normalized_2d_static_mesh_indices_offset
            + core::mem::size_of_val(&normalized_2d_static_mesh_indices_fused[..]);
        let mut vector_draw_buffer = br::BufferObject::new(
            self.device,
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
            self.device,
            &br::MemoryAllocateInfo::new(
                vector_draw_buffer_memreq.size,
                self.device
                    .find_device_local_memory_index(vector_draw_buffer_memreq.memoryTypeBits)
                    .expect("no suitable memory"),
            ),
        )
        .expect("vector_draw_buffer malloc");
        vector_draw_buffer
            .bind(&vector_draw_buffer_memory, 0)
            .expect("vector_draw_buffer bind");

        let mut vector_draw_init_buffer = br::BufferObject::new(
            self.device,
            &br::BufferCreateInfo::new(
                vector_draw_buffer_total_size,
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("vector_draw_init_buffer create");
        let vector_draw_init_buffer_memreq = vector_draw_init_buffer.requirements();
        let vector_draw_init_buffer_memindex = self
            .device
            .find_host_visible_memory_index(vector_draw_init_buffer_memreq.memoryTypeBits)
            .expect("no suitable memory");
        let mut vector_draw_init_buffer_memory = br::DeviceMemoryObject::new(
            self.device,
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
                state.fill_tri_points.as_ptr(),
                p.ptr().byte_add(filltri_points_offset).cast(),
                state.fill_tri_points.len(),
            );
            core::ptr::copy_nonoverlapping(
                state.fill_tri_indices.as_ptr(),
                p.ptr().byte_add(filltri_indices_offset).cast(),
                state.fill_tri_indices.len(),
            );
            core::ptr::copy_nonoverlapping(
                state.curve_tris.as_ptr(),
                p.ptr().byte_add(curve_triangles_offset).cast(),
                state.curve_tris.len(),
            );
            core::ptr::copy_nonoverlapping(
                normalized_2d_static_mesh_vertices_fused.as_ptr(),
                p.ptr()
                    .byte_add(normalized_2d_static_mesh_vertices_offset)
                    .cast(),
                normalized_2d_static_mesh_vertices_fused.len(),
            );
            core::ptr::copy_nonoverlapping(
                normalized_2d_static_mesh_indices_fused.as_ptr(),
                p.ptr()
                    .byte_add(normalized_2d_static_mesh_indices_offset)
                    .cast(),
                normalized_2d_static_mesh_indices_fused.len(),
            );
        }
        if !self
            .device
            .is_coherent_memory(vector_draw_init_buffer_memindex)
        {
            unsafe {
                self.device
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
            self.device,
            &br::ImageCreateInfo::new(*self.atlas.size(), formats.color)
                .set_usage(
                    br::ImageUsageFlags::COLOR_ATTACHMENT | br::ImageUsageFlags::TRANSFER_SRC,
                )
                .sample_counts(TextureAtlas::MULTISAMPLE_LEVEL),
        )
        .expect("vector color_ms buffer create");
        self.device
            .dbg_set_name(&vector_color_ms_buffer, c"Vector::color_ms_buffer");
        let mut vector_stencil_buffer = br::ImageObject::new(
            self.device,
            &br::ImageCreateInfo::new(*self.atlas.size(), formats.stencil)
                .set_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT)
                .sample_counts(TextureAtlas::MULTISAMPLE_LEVEL),
        )
        .expect("vector stencil buffer create");
        self.device
            .dbg_set_name(&vector_stencil_buffer, c"Vector::stencil_buffer");
        let vector_color_ms_buffer_memreq = vector_color_ms_buffer.requirements();
        let vector_stencil_buffer_memreq = vector_stencil_buffer.requirements();
        tracing::debug!(
            ?vector_color_ms_buffer_memreq,
            ?vector_stencil_buffer_memreq
        );
        let vector_color_ms_buffer_mem = br::DeviceMemoryObject::new(
            self.device,
            &br::MemoryAllocateInfo::new(
                vector_color_ms_buffer_memreq.size,
                self.device
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
            self.device,
            &br::MemoryAllocateInfo::new(
                vector_stencil_buffer_memreq.size,
                self.device
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
            self.device,
            &br::FramebufferCreateInfo::new(
                &common_res.render_pass,
                &[
                    vector_stencil_buffer.as_transparent_ref(),
                    vector_color_ms_buffer.as_transparent_ref(),
                ],
                self.atlas.size().width,
                self.atlas.size().height,
            ),
        )
        .expect("vector framebuffer create");

        let mut cp = br::CommandPoolObject::new(
            self.device,
            &br::CommandPoolCreateInfo::new(self.device.present_queue_family_index()),
        )
        .expect("cp init");
        let mut cb = br::CommandBufferObject::alloc(
            self.device,
            &br::CommandBufferAllocateInfo::new(&mut cp, 1, br::CommandBufferLevel::Primary),
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
            self.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .from(
                            br::PipelineStageFlags2::COPY,
                            br::AccessFlags2::TRANSFER.write,
                        )
                        .to(
                            br::PipelineStageFlags2::VERTEX_INPUT,
                            br::AccessFlags2::VERTEX_ATTRIBUTE_READ | br::AccessFlags2::INDEX_READ,
                        )],
                    &[],
                    &[],
                ),
            )
        })
        .begin_render_pass(
            &br::RenderPassBeginInfo::new(
                &common_res.render_pass,
                &vector_framebuffer,
                self.atlas.size().into_rect(br::Offset2D::ZERO),
                &[
                    br::ClearValue::depth_stencil(1.0, 0),
                    br::ClearValue::color_f32([0.0; 4]),
                ],
            ),
            br::SubpassContents::Inline,
        )
        .bind_pipeline(
            br::PipelineBindPoint::Graphics,
            &self.triangle_fans_pipeline,
        )
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
        .draw_indexed(state.fill_tri_indices.len() as _, 1, 0, 0, 0)
        .bind_pipeline(br::PipelineBindPoint::Graphics, &self.curve_pipeline)
        .bind_vertex_buffer_array(
            0,
            &[vector_draw_buffer.as_transparent_ref()],
            &[curve_triangles_offset as _],
        )
        .draw(state.curve_tris.len() as _, 1, 0, 0)
        .next_subpass(br::SubpassContents::Inline)
        .bind_pipeline(br::PipelineBindPoint::Graphics, &self.colorize_pipeline)
        .draw(3, 1, 0, 0)
        .inject(|r| {
            if normalized_2d_static_mesh_vertices_fused.is_empty() {
                // no render requested
                return r;
            }

            r.bind_pipeline(
                br::PipelineBindPoint::Graphics,
                &self.fill_tri_white_pipeline,
            )
            .bind_vertex_buffer_array(
                0,
                &[vector_draw_buffer.as_transparent_ref()],
                &[normalized_2d_static_mesh_vertices_offset as _],
            )
            .bind_index_buffer(
                &vector_draw_buffer,
                normalized_2d_static_mesh_indices_offset as _,
                br::IndexType::U16,
            )
            .draw_indexed(
                normalized_2d_static_mesh_indices_fused.len() as _,
                1,
                0,
                0,
                0,
            )
        })
        .end_render_pass()
        .inject(|r| {
            self.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[],
                    &[],
                    &[br::ImageMemoryBarrier2::new(
                        &self.atlas.image(),
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
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
            &self.atlas.image(),
            br::ImageLayout::TransferDestOpt,
            &state
                .updated_rects
                .iter()
                .map(|r| br::vk::VkImageResolve {
                    srcSubresource: br::ImageSubresourceLayers::new(br::AspectMask::COLOR, 0, 0..1)
                        .0,
                    srcOffset: r.offset.with_z(0),
                    dstSubresource: br::ImageSubresourceLayers::new(br::AspectMask::COLOR, 0, 0..1)
                        .0,
                    dstOffset: r.offset.with_z(0),
                    extent: r.extent.with_depth(1),
                })
                .collect::<Vec<_>>(),
        )
        .inject(|r| {
            self.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[],
                    &[],
                    &[br::ImageMemoryBarrier2::new(
                        &self.atlas.image(),
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    )
                    .from(
                        br::PipelineStageFlags2::RESOLVE,
                        br::AccessFlags2::TRANSFER.write,
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

        render_worker_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[cb[0].as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("vector render submit");
        render_worker_queue.wait().expect("vector render wait");
    }

    pub const fn atlas(&self) -> &TextureAtlas {
        &self.atlas
    }

    pub fn atlas_mut(&mut self) -> &mut TextureAtlas {
        &mut self.atlas
    }

    pub fn clear(&mut self) {
        self.atlas.clear();
        self.acquired_glyph_rects.clear();
    }
}

pub struct ColorTextureAtlasManager<'d> {
    device: &'d VulkanDevice<'d>,
    atlas: ColorTextureAtlas,
}
impl Drop for ColorTextureAtlasManager<'_> {
    fn drop(&mut self) {
        unsafe {
            self.atlas.drop(self.device);
        }
    }
}
impl<'d> ColorTextureAtlasManager<'d> {
    pub fn new(
        common_res: &GlyphAtlasManagerCommonResources<'d>,
        init_worker_queue: &mut (impl br::QueueMut + ?Sized),
        init_worker_queue_family_index: u32,
    ) -> Self {
        let atlas = ColorTextureAtlas::new(common_res.device);

        let init_rp = br::RenderPassObject::new(
            common_res.device,
            &br::RenderPassCreateInfo::new(
                &[br::AttachmentDescription::new(
                    atlas.format(),
                    br::ImageLayout::Undefined,
                    br::ImageLayout::ShaderReadOnlyOpt,
                )
                .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)],
                &[br::SubpassDescription::new().color_attachments(
                    &[br::AttachmentReference::new(
                        0,
                        br::ImageLayout::ColorAttachmentOpt,
                    )],
                    &[],
                )],
                &[br::vk::VkSubpassDependency {
                    srcSubpass: 0,
                    dstSubpass: br::vk::VK_SUBPASS_EXTERNAL,
                    srcAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
                    dstAccessMask: br::AccessFlags::SHADER.read,
                    srcStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
                    dstStageMask: br::PipelineStageFlags::FRAGMENT_SHADER.0,
                    dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT,
                }],
            ),
        )
        .expect("init_rp.create");
        let init_fb = br::FramebufferObject::new(
            common_res.device,
            &br::FramebufferCreateInfo::new(
                &init_rp,
                &[atlas.as_image_view().as_transparent_ref()],
                atlas.size().width,
                atlas.size().height,
            ),
        )
        .expect("init_fb.create");

        let mut init_cp = br::CommandPoolObject::new(
            common_res.device,
            &br::CommandPoolCreateInfo::new(init_worker_queue_family_index),
        )
        .expect("init_cp.create");
        let [mut init_cb] = br::CommandBufferObject::alloc_array(
            common_res.device,
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
        .begin_render_pass(
            &br::RenderPassBeginInfo::new(
                &init_rp,
                &init_fb,
                atlas.size().into_rect(br::Offset2D::ZERO),
                &[br::ClearValue::color_f32([0.0; 4])],
            ),
            br::SubpassContents::Inline,
        )
        .end_render_pass()
        .end()
        .expect("init_cb.end");
        init_worker_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[init_cb.as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("init_cb.submit");
        init_worker_queue.wait().expect("init_cb.wait");

        Self {
            device: common_res.device,
            atlas,
        }
    }

    #[inline(always)]
    pub fn acquire(&mut self, width: u32, height: u32) -> AtlasRect {
        self.atlas.acquire(width, height)
    }

    fn perform_render(
        &self,
        shader_texture_rasterize_requests: &[(AtlasRect, ShaderTexture)],
        render_scale: f32,
        vk_device: &VulkanDevice,
        render_worker_queue: &mut (impl br::QueueMut + ?Sized),
    ) {
        #[derive(SpecializationConstants)]
        struct SpecConstantStorage {
            #[constant_id = 0]
            render_width_px: f32,
            #[constant_id = 1]
            render_height_px: f32,
        }

        // TODO: 最適化はあとで
        let pipeline_layout =
            br::PipelineLayoutObject::new(vk_device, &br::PipelineLayoutCreateInfo::new(&[], &[]))
                .expect("pipeline_layout.create");
        let render_pass = br::RenderPassObject::new(
            vk_device,
            &br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(self.atlas.format())
                    .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store)
                    .with_layout_to(br::ImageLayout::ShaderReadOnlyOpt.from_undefined())],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .by_region()
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::SHADER.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::FRAGMENT_SHADER,
                )],
            ),
        )
        .expect("render_pass.create");
        let fb = br::FramebufferObject::new(
            vk_device,
            &br::FramebufferCreateInfo::new(
                &render_pass,
                &[self.atlas.as_image_view().as_transparent_ref()],
                self.atlas.size().width,
                self.atlas.size().height,
            ),
        )
        .expect("fb.create");
        let render_resources = shader_texture_rasterize_requests
            .iter()
            .map(|(r, s)| {
                let shader = vk_device.require_shader(&s.shader_path);
                let viewport = [r.vk_rect().make_viewport(0.0..1.0)];
                let scissor = [r.vk_rect()];
                let spec_const = SpecConstantStorage {
                    render_width_px: r.width() as _,
                    render_height_px: r.height() as _,
                };
                let [pipeline] = vk_device
                    .create_graphics_pipelines_array(&[br::GraphicsPipelineCreateInfo::new(
                        &pipeline_layout,
                        render_pass.subpass(0),
                        &[
                            shader
                                .on_stage(br::ShaderStage::Vertex, c"vertMain")
                                .with_specialization_info(&br::SpecializationInfo::new(
                                    &spec_const,
                                )),
                            shader
                                .on_stage(br::ShaderStage::Fragment, c"fragMain")
                                .with_specialization_info(&br::SpecializationInfo::new(
                                    &spec_const,
                                )),
                        ],
                        VI_STATE_EMPTY,
                        IA_STATE_TRILIST,
                        &br::PipelineViewportStateCreateInfo::new(&viewport, &scissor),
                        RASTER_STATE_DEFAULT_FILL_NOCULL,
                        BLEND_STATE_SINGLE_NONE,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)])
                    .expect("pipeline.create");

                (shader, pipeline)
            })
            .collect::<Vec<_>>();

        let mut cp = br::CommandPoolObject::new(
            vk_device,
            &br::CommandPoolCreateInfo::new(vk_device.present_queue_family_index()).transient(),
        )
        .expect("cp.create");
        let [mut cb] = br::CommandBufferObject::alloc_array(
            vk_device,
            &br::CommandBufferFixedCountAllocateInfo::new(&mut cp, br::CommandBufferLevel::Primary),
        )
        .expect("cb.alloc");
        unsafe {
            cb.begin(&br::CommandBufferBeginInfo::new().onetime_submit())
                .expect("cb.begin")
        }
        .inject(|r| {
            vk_device.cmd_begin_render_pass(
                r,
                &br::RenderPassBeginInfo::new(
                    &render_pass,
                    &fb,
                    self.atlas.size().into_rect(br::Offset2D::ZERO),
                    &[],
                ),
            )
        })
        .inject(|r| {
            render_resources
                .iter()
                .zip(shader_texture_rasterize_requests.iter())
                .fold(r, |r, (res, tex)| {
                    tracing::debug!(path = ?tex.1.shader_path, rect = ?tex.0, "raster shader tex");
                    r.bind_pipeline(br::PipelineBindPoint::Graphics, &res.1)
                        .draw(3, 1, 0, 0)
                })
        })
        .inject(|r| vk_device.cmd_end_render_pass(r))
        .end()
        .expect("cb.end");

        render_worker_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[cb.as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("vector render submit");
        render_worker_queue.wait().expect("vector render wait");
    }

    pub const fn atlas(&self) -> &ColorTextureAtlas {
        &self.atlas
    }

    pub fn atlas_mut(&mut self) -> &mut ColorTextureAtlas {
        &mut self.atlas
    }

    pub fn clear(&mut self) {
        self.atlas.clear();
    }
}

struct PreviewComposite {
    descriptor_set_layout: br::vk::VkDescriptorSetLayout,
    descriptor_pool: br::vk::VkDescriptorPool,
    descriptor_set: br::DescriptorSet,
    pipeline_layout: br::vk::VkPipelineLayout,
    shader: br::vk::VkShaderModule,
    pipeline_target_rt_size: br::Extent2D,
    pipeline_target_render_pass_handle: br::vk::VkRenderPass,
    pipeline_target_subpass: u32,
    pipeline: br::vk::VkPipeline,
}
impl PreviewComposite {
    unsafe fn drop(self, vk_device: &VulkanDevice) {
        drop(unsafe { br::PipelineObject::manage(self.pipeline, vk_device) });
        drop(unsafe { br::DescriptorPoolObject::manage(self.descriptor_pool, vk_device) });
        drop(unsafe { br::ShaderModuleObject::manage(self.shader, vk_device) });
        drop(unsafe { br::PipelineLayoutObject::manage(self.pipeline_layout, vk_device) });
        drop(unsafe {
            br::DescriptorSetLayoutObject::manage(self.descriptor_set_layout, vk_device)
        });
    }

    fn new(
        vk_device: &VulkanDevice,
        init_render_tex: &(impl br::VkHandle<Handle = br::vk::VkImageView> + ?Sized),
        smp: &(impl br::VkHandle<Handle = br::vk::VkSampler> + ?Sized),
        target_pass: br::SubpassRef<impl br::RenderPass + ?Sized>,
        init_screen_size: br::Extent2D,
    ) -> Self {
        let descriptor_set_layout = br::DescriptorSetLayoutObject::new(
            vk_device,
            &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::CombinedImageSampler
                .make_binding(0, 1)
                .with_immutable_samplers(&[smp.as_transparent_ref()])]),
        )
        .expect("preview_composite.descriptor_set_layout.create");
        let pipeline_layout = br::PipelineLayoutObject::new(
            vk_device,
            &br::PipelineLayoutCreateInfo::new(
                &[descriptor_set_layout.as_transparent_ref()],
                &[br::PushConstantRange::for_type::<[f32; 16 + 4]>(
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                )],
            ),
        )
        .expect("preview_composite.pipeline_layout.create");
        let shader = vk_device.require_shader("simple_blit.spv");

        let mut descriptor_pool = br::DescriptorPoolObject::new(
            vk_device,
            &br::DescriptorPoolCreateInfo::new(
                1,
                &[br::DescriptorType::CombinedImageSampler.make_size(1)],
            ),
        )
        .expect("preview_composite.descriptor_pool.create");
        let [descriptor_set] = descriptor_pool
            .alloc_array(&[descriptor_set_layout.as_transparent_ref()])
            .expect("preview_composite.descriptor_set.alloc");
        vk_device.update_descriptor_sets(
            &[descriptor_set
                .binding_at(0)
                .write(br::DescriptorContents::combined_image_sampler(
                    init_render_tex,
                    br::ImageLayout::ShaderReadOnlyOpt,
                ))],
            &[],
        );

        let [pipeline] = vk_device
            .create_graphics_pipelines_array(&[br::GraphicsPipelineCreateInfo::new(
                &pipeline_layout,
                target_pass,
                &[
                    shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                    shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
                ],
                VI_STATE_EMPTY,
                IA_STATE_TRISTRIP,
                &br::PipelineViewportStateCreateInfo::new(
                    &[init_screen_size
                        .into_rect(br::Offset2D::ZERO)
                        .make_viewport(0.0..1.0)],
                    &[init_screen_size.into_rect(br::Offset2D::ZERO)],
                ),
                RASTER_STATE_DEFAULT_FILL_NOCULL,
                BLEND_STATE_SINGLE_NONE,
            )
            .set_multisample_state(MS_STATE_EMPTY)])
            .expect("preview_composite.pipeline.create");

        let (pipeline, _) = pipeline.unmanage();
        let (descriptor_pool, _) = descriptor_pool.unmanage();
        let (shader, _) = shader.unmanage();
        let (pipeline_layout, _) = pipeline_layout.unmanage();
        let (descriptor_set_layout, _) = descriptor_set_layout.unmanage();
        Self {
            descriptor_set_layout,
            descriptor_pool,
            descriptor_set,
            pipeline_layout,
            shader,
            pipeline_target_rt_size: init_screen_size,
            pipeline_target_render_pass_handle: target_pass.0.native_ptr(),
            pipeline_target_subpass: target_pass.1,
            pipeline,
        }
    }

    pub fn update(
        &mut self,
        device: &VulkanDevice,
        new_rt_size: br::Extent2D,
        new_target_render_pass_handle: br::vk::VkRenderPass,
        new_target_subpass: u32,
    ) {
        if self.pipeline_target_rt_size != new_rt_size
            || self.pipeline_target_render_pass_handle != new_target_render_pass_handle
            || self.pipeline_target_subpass != new_target_subpass
        {
            drop(unsafe { br::PipelineObject::manage(self.pipeline, device) });
            let [pipeline] = device
                .create_graphics_pipelines_array(&[br::GraphicsPipelineCreateInfo::new(
                    br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                    br::SubpassRef(
                        br::VkHandleRef::from_raw_ref(&new_target_render_pass_handle),
                        new_target_subpass,
                    ),
                    &[
                        br::PipelineShaderStage::new(
                            br::ShaderStage::Vertex,
                            br::VkHandleRef::from_raw_ref(&self.shader),
                            c"vertMain",
                        ),
                        br::PipelineShaderStage::new(
                            br::ShaderStage::Fragment,
                            br::VkHandleRef::from_raw_ref(&self.shader),
                            c"fragMain",
                        ),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRISTRIP,
                    &br::PipelineViewportStateCreateInfo::new(
                        &[new_rt_size
                            .into_rect(br::Offset2D::ZERO)
                            .make_viewport(0.0..1.0)],
                        &[new_rt_size.into_rect(br::Offset2D::ZERO)],
                    ),
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(MS_STATE_EMPTY)])
                .expect("preview_composite.pipeline.create");

            self.pipeline = pipeline.unmanage().0;
            self.pipeline_target_rt_size = new_rt_size;
            self.pipeline_target_render_pass_handle = new_target_render_pass_handle;
            self.pipeline_target_subpass = new_target_subpass;
        }
    }
}
