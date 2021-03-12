//! Platform Presenter(Swapchain Abstraction)

use bedrock as br;
use br::VkHandle;
use std::rc::Rc;

pub trait PlatformPresenter {
    fn format(&self) -> br::vk::VkFormat;
    fn backbuffer_count(&self) -> usize;
    fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>>;

    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord);
    fn next_backbuffer_index(&mut self) -> br::Result<u32>;
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags);
    fn render_and_present<'s>(
        &'s mut self,
        g: &crate::Graphics,
        last_render_fence: &br::Fence,
        present_queue: &br::Queue,
        backbuffer_index: u32,
        submissions: crate::QueueSubmissions<'s>
    ) -> br::Result<()>;
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &crate::Graphics, new_size: peridot_math::Vector2<usize>) -> bool;
    fn current_geometry_extent(&self) -> peridot_math::Vector2<usize>;
}

struct IntegratedSwapchainObject {
    swapchain: br::Swapchain,
    backbuffer_images: Vec<Rc<br::ImageView>>
}
impl IntegratedSwapchainObject {
    pub fn new(
        g: &crate::Graphics, surface_info: &crate::SurfaceInfo, default_extent: peridot_math::Vector2<usize>
    ) -> Self {
        let si = g.adapter.surface_capabilities(&surface_info.obj).expect("Failed to query Surface Capabilities");
        let ew = if si.currentExtent.width == 0xffff_ffff { default_extent.0 as _ } else { si.currentExtent.width };
        let eh = if si.currentExtent.height == 0xffff_ffff { default_extent.1 as _ } else { si.currentExtent.height };
        let ew = ew.max(si.minImageExtent.width).min(si.maxImageExtent.width);
        let eh = eh.max(si.minImageExtent.height).min(si.maxImageExtent.height);
        let ext = br::Extent2D(ew, eh);
        let buffer_count = 2.max(si.minImageCount).min(si.maxImageCount);
        let pre_transform =
            if br::SurfaceTransform::Identity.contains(si.supportedTransforms) {
                br::SurfaceTransform::Identity
            } else {
                br::SurfaceTransform::Inherit
            };
        let chain = br::SwapchainBuilder::new(
            &surface_info.obj, buffer_count, &surface_info.fmt, &ext, br::ImageUsage::COLOR_ATTACHMENT
        )
        .present_mode(surface_info.pres_mode)
        .composite_alpha(surface_info.available_composite_alpha)
        .pre_transform(pre_transform)
        .create(&g.device)
        .expect("Failed to create Swapchain");
        
        let isr_c0 = br::ImageSubresourceRange::color(0, 0);
        let backbuffer_images = chain.get_images().expect("Failed to get backbuffer images").into_iter()
            .map(|bb|
                bb.create_view(None, None, &Default::default(), &isr_c0)
                    .expect("Failed to create ImageView for Backbuffer")
                    .into()
            ).collect();
        
        IntegratedSwapchainObject { swapchain: chain, backbuffer_images }
    }
}

/// WSI Swapchain implementation for PlatformPresenter
pub struct IntegratedSwapchain {
    surface_info: crate::SurfaceInfo,
    swapchain: crate::Discardable<IntegratedSwapchainObject>,
    rendering_order: br::Semaphore,
    buffer_ready_order: br::Semaphore,
    present_order: br::Semaphore,
    sparse_binding_order: br::Semaphore
}
impl IntegratedSwapchain {
    pub fn new(g: &crate::Graphics, surface: br::Surface, default_extent: peridot_math::Vector2<usize>) -> Self {
        let surface_info = crate::SurfaceInfo::gather_info(&g.adapter, surface).expect("Failed to gather surface info");
        
        IntegratedSwapchain {
            swapchain: crate::Discardable::from(IntegratedSwapchainObject::new(g, &surface_info, default_extent)),
            surface_info,
            rendering_order: br::Semaphore::new(&g.device).expect("Failed to create Rendering Order Semaphore"),
            buffer_ready_order: br::Semaphore::new(&g.device).expect("Failed to create BufferReady Order Semaphore"),
            present_order: br::Semaphore::new(&g.device).expect("Failed to create Present Order Semaphore"),
            sparse_binding_order: br::Semaphore::new(&g.device)
                .expect("Failed to create Sparse Binding Order Semaphore")
        }
    }

    pub fn format(&self) -> br::vk::VkFormat { self.surface_info.format() }
    pub fn backbuffer_count(&self) -> usize { self.swapchain.get().backbuffer_images.len() }
    pub fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> {
        self.swapchain.get().backbuffer_images.get(index).cloned()
    }

    pub fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        let image_barriers = self.swapchain.get().backbuffer_images.iter()
            .map(|v| br::ImageMemoryBarrier::new(
                &br::ImageSubref::color(v, 0, 0),
                br::ImageLayout::Undefined, br::ImageLayout::PresentSrc
            ))
            .collect::<Vec<_>>();
        
        recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE, br::PipelineStageFlags::BOTTOM_OF_PIPE, false,
            &[], &[], &image_barriers
        );
    }
    pub fn acquire_next_backbuffer_index(&self) -> br::Result<u32> {
        self.swapchain.get().swapchain.acquire_next(None, br::CompletionHandler::from(&self.rendering_order))
    }
    pub fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (br::ImageLayout::PresentSrc, br::PipelineStageFlags::TOP_OF_PIPE)
    }
    pub fn render_and_present<'s>(
        &'s mut self,
        g: &crate::Graphics,
        last_render_fence: &br::Fence,
        q: &br::Queue,
        bb_index: u32,
        mut submissions: crate::QueueSubmissions<'s>
    ) -> br::Result<()> {
        let wait_sparse_binding = if let Some(mut b) = submissions.sparse_binding {
            b.signal_semaphores.to_mut().push(&self.sparse_binding_order);
            q.bind_sparse(&[b], None).expect("Failed to submit sparse binding ops");
            true
        } else {
            false
        };

        if let Some(mut cs) = submissions.updating {
            // copy -> render
            cs.signal_semaphores.to_mut().push(&self.buffer_ready_order);
            if wait_sparse_binding {
                cs.wait_semaphores.to_mut().push((&self.sparse_binding_order, br::PipelineStageFlags::TRANSFER));
            }
            submissions.rendering.wait_semaphores.to_mut().extend(vec![
                (&self.rendering_order, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT),
                (&self.buffer_ready_order, br::PipelineStageFlags::VERTEX_INPUT)
            ]);
            submissions.rendering.signal_semaphores.to_mut().push(&self.present_order);
            g.submit_buffered_commands(&[cs, submissions.rendering], last_render_fence)
                .expect("Failed to submit render and update commands");
        } else {
            // render only (old logic)
            submissions.rendering.signal_semaphores.to_mut().push(&self.present_order);
            submissions.rendering.wait_semaphores.to_mut().push(
                (&self.rendering_order, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT)
            );
            if wait_sparse_binding {
                submissions.rendering.wait_semaphores.to_mut().push(
                    (&self.sparse_binding_order, br::PipelineStageFlags::VERTEX_INPUT)
                );
            }
            g.submit_buffered_commands(&[submissions.rendering], last_render_fence)
                .expect("Failed to submit render commands");
        }

        self.swapchain.get().swapchain.queue_present(q, bb_index, &[&self.present_order])
    }

    pub fn resize(&mut self, g: &crate::Graphics, new_size: peridot_math::Vector2<usize>) {
        self.swapchain.discard_lw();
        self.swapchain.set_lw(IntegratedSwapchainObject::new(g, &self.surface_info, new_size));
    }
}
