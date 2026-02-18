#[cfg(feature = "wayland")]
use std::{collections::HashMap, sync::Mutex};

use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Fence, FenceMut, QueueMut, Swapchain, VkHandle,
    VkHandleMut,
};

use crate::{
    AppEventBus, Event, RendererSync,
    composite::{
        BoundCompositeRenderer, CompositeRenderingData, CompositeStreamingData,
        CompositeTreeRender, VectorRasterizationState,
    },
    graphics::{VulkanDevice, VulkanSurface, VulkanSwapchain},
    text::FontSet,
};
#[cfg(feature = "wayland")]
use crate::{WaylandSurfaceKey, WaylandWindow, WaylandWindowState, text::GlyphAtlas};

pub struct WindowRenderer<'d> {
    #[cfg(feature = "wayland")]
    w: WaylandWindow,
    vk_device: &'d VulkanDevice,
    surface: VulkanSurface<'d>,
    swapchain: VulkanSwapchain<'d>,
    swapchain_invalidated: bool,
    primary_render_pass: br::RenderPassObject<&'d VulkanDevice>,
    primary_framebuffers: Vec<br::FramebufferObject<'d, &'d VulkanDevice>>,
    composite_tree: CompositeTreeRender<Event>,
    composite_renderer: BoundCompositeRenderer<'d>,
    last_composite_render_data: CompositeRenderingData,
    update_cp: br::CommandPoolObject<&'d VulkanDevice>,
    update_cb: br::CommandBufferObject<&'d VulkanDevice>,
    update_completion_fence: br::FenceObject<&'d VulkanDevice>,
    update_completion_semaphore: br::SemaphoreObject<&'d VulkanDevice>,
    updating: bool,
    render_cp: br::CommandPoolObject<&'d VulkanDevice>,
    render_cb: Vec<br::CommandBufferObject<&'d VulkanDevice>>,
    render_cb_invalid: bool,
    present_ready_semaphores: Vec<br::SemaphoreObject<&'d VulkanDevice>>,
    backbuffer_ready_fence: br::FenceObject<&'d VulkanDevice>,
}
impl<'d> WindowRenderer<'d> {
    #[cfg(feature = "wayland")]
    pub fn new(
        w: WaylandWindow,
        surface_states: &Mutex<HashMap<WaylandSurfaceKey, WaylandWindowState>>,
        surface: VulkanSurface<'d>,
        vk_device: &'d VulkanDevice,
        glyph_atlas: &GlyphAtlas,
    ) -> Self {
        let vk_swapchain = VulkanSwapchain::new(
            &surface,
            #[cfg(windows)]
            || w.pixels_client_size(),
            #[cfg(feature = "wayland")]
            || surface_states.lock().expect("poisoned")[&w.as_key()].active_size,
            #[cfg(target_os = "macos")]
            || *w.dispatcher.state.active_rt_size.lock().expect("poisoned"),
        );

        // TODO: 同じ構造のものがあれば使い回したい
        let vk_render_pass = br::RenderPassObject::new(
            vk_device,
            &br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(surface.format())
                    .color_memory_op(br::LoadOp::Load, br::StoreOp::Store)
                    .layout_transition(br::ImageLayout::PresentSrc, br::ImageLayout::PresentSrc)],
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
        let vk_framebuffers = vk_swapchain
            .image_view_refs()
            .map(|bb| {
                br::FramebufferObject::new(
                    vk_device,
                    &br::FramebufferCreateInfo::new(
                        &vk_render_pass,
                        &[bb],
                        vk_swapchain.size().width,
                        vk_swapchain.size().height,
                    ),
                )
                .expect("framebuffer create")
            })
            .collect::<Vec<_>>();

        let mut update_cp = br::CommandPoolObject::new(
            vk_device,
            &br::CommandPoolCreateInfo::new(vk_device.present_queue_family_index()),
        )
        .expect("update_cp.create");
        let [mut update_cb] = br::CommandBufferObject::alloc_array(
            vk_device,
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
            vk_device,
            &br::CommandPoolCreateInfo::new(vk_device.present_queue_family_index()),
        )
        .expect("command pool create");
        let render_cb = br::CommandBufferObject::alloc(
            vk_device,
            &br::CommandBufferAllocateInfo::new(
                &mut render_cp,
                vk_framebuffers.len() as _,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("command buffer alloc");
        for (n, x) in render_cb.iter().enumerate() {
            vk_device.dbg_set_name(x, &unsafe {
                std::ffi::CString::from_vec_unchecked(
                    format!("Window Render Commands #{n}").into_bytes(),
                )
            });
        }

        Self {
            w,
            vk_device,
            composite_tree: CompositeTreeRender::new(),
            composite_renderer: BoundCompositeRenderer::new(
                &vk_device,
                glyph_atlas.view(),
                surface.format(),
                vk_swapchain.size(),
                vk_swapchain.image_view_refs(),
            ),
            last_composite_render_data: CompositeRenderingData {
                instructions: Vec::new(),
                render_passes: Vec::new(),
                required_backdrop_buffer_count: 0,
            },
            update_cp,
            update_cb,
            update_completion_fence: br::FenceObject::new(vk_device, &br::FenceCreateInfo::new(0))
                .expect("update_completion_fence.create"),
            update_completion_semaphore: br::SemaphoreObject::new(
                vk_device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("update_completion_semaphore.create"),
            updating: false,
            render_cp,
            render_cb,
            render_cb_invalid: true,
            present_ready_semaphores: (0..vk_framebuffers.len())
                .map(|_| {
                    br::SemaphoreObject::new(vk_device, &br::SemaphoreCreateInfo::new())
                        .expect("rendering_timeline_semaphore create")
                })
                .collect::<Vec<_>>(),
            backbuffer_ready_fence: br::FenceObject::new(vk_device, &br::FenceCreateInfo::new(0))
                .expect("last render completion fence create"),
            surface,
            swapchain: vk_swapchain,
            swapchain_invalidated: false,
            primary_render_pass: vk_render_pass,
            primary_framebuffers: vk_framebuffers,
        }
    }

    #[cfg(feature = "wayland")]
    pub const fn window_key(&self) -> WaylandSurfaceKey {
        self.w.as_key()
    }

    pub fn update(
        &mut self,
        current_sec: f32,
        renderer_sync: &Mutex<RendererSync>,
        glyph_atlas: &mut GlyphAtlas,
        font_set: &mut FontSet,
        vector_raster_state: &mut VectorRasterizationState,
        events: &AppEventBus,
    ) -> bool {
        {
            let mut renderer_sync = renderer_sync.lock().expect("poisoned");
            if let Some(scale) = renderer_sync.latest_ui_scale_changes.take() {
                glyph_atlas.clear();
                #[cfg(feature = "freetype")]
                font_set.rescale((scale * 72.0) as _);
            }
            renderer_sync
                .composite_buffer
                .clean(&mut self.composite_tree);
        }
        let composite_render_data = self.composite_renderer.update(
            self.vk_device,
            &mut self.composite_tree,
            self.swapchain.size(),
            &font_set,
            glyph_atlas,
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
        self.validate_render_commands();

        needs_update_commands
    }

    pub fn invalidate_swapchain(&mut self) {
        self.swapchain_invalidated = true;
    }

    pub fn validate_swapchain<'s>(
        &'s mut self,
        descriptor_writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        #[cfg(feature = "wayland")] surface_states: &Mutex<
            HashMap<WaylandSurfaceKey, WaylandWindowState>,
        >,
    ) {
        if !self.swapchain_invalidated {
            // already valid
            return;
        }

        self.invalidate_render_commands();
        self.primary_framebuffers.clear();

        self.surface.refresh_caps();
        self.swapchain.recreate(
            &self.surface,
            #[cfg(windows)]
            || w.pixels_client_size(),
            #[cfg(feature = "wayland")]
            || surface_states.lock().expect("poisoned")[&self.w.as_key()].active_size,
            #[cfg(target_os = "macos")]
            || *w.dispatcher.state.active_rt_size.lock().expect("poisoned"),
        );

        // recrease rt resources
        self.primary_framebuffers
            .extend(self.swapchain.image_view_refs().map(|bb| {
                br::FramebufferObject::new(
                    self.vk_device,
                    &br::FramebufferCreateInfo::new(
                        &self.primary_render_pass,
                        &[bb],
                        self.swapchain.size().width,
                        self.swapchain.size().height,
                    ),
                )
                .expect("framebuffer create")
            }));
        self.composite_renderer.recreate_rt_resources(
            self.vk_device,
            self.surface.format(),
            self.swapchain.image_view_refs(),
            self.swapchain.size(),
            descriptor_writes,
        );

        self.swapchain_invalidated = false;
    }

    pub fn acquire_backbuffer_with_wait(&mut self) -> br::Result<u32> {
        let backbuffer_index = self.swapchain.acquire_next(
            None,
            br::CompletionHandlerMut::Host(self.backbuffer_ready_fence.as_transparent_ref_mut()),
        )?;
        self.backbuffer_ready_fence
            .wait()
            .expect("last render completion fence wait");
        self.backbuffer_ready_fence
            .reset()
            .expect("last render completion fence reset");

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
                    self.swapchain.size(),
                    &self.swapchain.image_ref(n),
                    n,
                    |_, r| r,
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

        unsafe {
            device_queue
                .submit_raw(
                    &[br::SubmitInfo::new(
                        &[],
                        &[],
                        &[self.update_cb.as_transparent_ref()],
                        &[self.update_completion_semaphore.as_transparent_ref()],
                    )],
                    Some(self.update_completion_fence.as_transparent_ref_mut()),
                )
                .expect("gfx.update.submit");
        }
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
