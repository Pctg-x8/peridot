//! Platform Presenter(Swapchain Abstraction)

use bedrock as br;
#[cfg(feature = "debug")]
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
        render_submission: br::SubmissionBatch<'s>,
        update_submission: Option<br::SubmissionBatch<'s>>,
    ) -> br::Result<()>;
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &crate::Graphics, new_size: peridot_math::Vector2<usize>) -> bool;
    fn current_geometry_extent(&self) -> peridot_math::Vector2<usize>;
}

struct IntegratedSwapchainObject {
    swapchain: br::Swapchain,
    backbuffer_images: Vec<Rc<br::ImageView>>,
}
impl IntegratedSwapchainObject {
    pub fn new(
        g: &crate::Graphics,
        surface_info: &crate::SurfaceInfo,
        default_extent: peridot_math::Vector2<usize>,
    ) -> Self {
        let si = g
            .adapter
            .surface_capabilities(&surface_info.obj)
            .expect("Failed to query Surface Capabilities");
        let ew = if si.currentExtent.width == 0xffff_ffff {
            default_extent.0 as _
        } else {
            si.currentExtent.width
        };
        let eh = if si.currentExtent.height == 0xffff_ffff {
            default_extent.1 as _
        } else {
            si.currentExtent.height
        };
        let ew = ew.max(si.minImageExtent.width).min(si.maxImageExtent.width);
        let eh = eh
            .max(si.minImageExtent.height)
            .min(si.maxImageExtent.height);
        let ext = br::vk::VkExtent2D {
            width: ew,
            height: eh,
        };
        let buffer_count = 2.max(si.minImageCount).min(si.maxImageCount);
        let pre_transform = if br::SurfaceTransform::Identity.contains(si.supportedTransforms) {
            br::SurfaceTransform::Identity
        } else {
            br::SurfaceTransform::Inherit
        };
        let chain = br::SwapchainBuilder::new(
            &surface_info.obj,
            buffer_count,
            &surface_info.fmt,
            &ext,
            br::ImageUsage::COLOR_ATTACHMENT,
        )
        .present_mode(surface_info.pres_mode)
        .composite_alpha(surface_info.available_composite_alpha)
        .pre_transform(pre_transform)
        .create(&g.device)
        .expect("Failed to create Swapchain");
        #[cfg(feature = "debug")]
        chain
            .set_name(Some(unsafe {
                std::ffi::CStr::from_bytes_with_nul_unchecked(
                    b"Peridot-Default Presentor-Swapchain\0",
                )
            }))
            .expect("Failed to set swapchain name");

        let isr_c0 = br::ImageSubresourceRange::color(0, 0);
        let backbuffer_images: Vec<Rc<_>> = chain
            .get_images()
            .expect("Failed to get backbuffer images")
            .into_iter()
            .map(|bb| {
                bb.create_view(None, None, &Default::default(), &isr_c0)
                    .expect("Failed to create ImageView for Backbuffer")
                    .into()
            })
            .collect();

        #[cfg(feature = "debug")]
        for (n, v) in backbuffer_images.iter().enumerate() {
            v.set_name(Some(
                &std::ffi::CString::new(format!(
                    "Peridot-Default Presentor-Backbuffer View #{}",
                    n
                ))
                .expect("invalid sequence?"),
            ))
            .expect("Failed to set backbuffer view name");
        }

        IntegratedSwapchainObject {
            swapchain: chain,
            backbuffer_images,
        }
    }
}

/// WSI Swapchain implementation for PlatformPresenter
pub struct IntegratedSwapchain {
    surface_info: crate::SurfaceInfo,
    swapchain: crate::Discardable<IntegratedSwapchainObject>,
    rendering_order: br::Semaphore,
    buffer_ready_order: br::Semaphore,
    present_order: br::Semaphore,
}
impl IntegratedSwapchain {
    pub fn new(
        g: &crate::Graphics,
        surface: br::Surface,
        default_extent: peridot_math::Vector2<usize>,
    ) -> Self {
        let surface_info = crate::SurfaceInfo::gather_info(&g.adapter, surface)
            .expect("Failed to gather surface info");

        let rendering_order =
            br::Semaphore::new(&g.device).expect("Failed to create Rendering Order Semaphore");
        let buffer_ready_order =
            br::Semaphore::new(&g.device).expect("Failed to create BufferReady Order Semaphore");
        let present_order =
            br::Semaphore::new(&g.device).expect("Failed to create Present Order Semaphore");
        #[cfg(feature = "debug")]
        {
            rendering_order
                .set_name(Some(unsafe {
                    std::ffi::CStr::from_bytes_with_nul_unchecked(
                        b"Peridot-Default Presentor-Rendering Order Semaphore\0",
                    )
                }))
                .expect("Failed to set Rendering Order Semaphore name");
            buffer_ready_order
                .set_name(Some(unsafe {
                    std::ffi::CStr::from_bytes_with_nul_unchecked(
                        b"Peridot-Default Presentor-BufferReady Order Semaphore\0",
                    )
                }))
                .expect("Failed to set BufferReady Order Semaphore name");
            present_order
                .set_name(Some(unsafe {
                    std::ffi::CStr::from_bytes_with_nul_unchecked(
                        b"Peridot-Default Presentor-Present Order Semaphore\0",
                    )
                }))
                .expect("Failed to set Present Order Semaphore name");
        }

        IntegratedSwapchain {
            swapchain: crate::Discardable::from(IntegratedSwapchainObject::new(
                g,
                &surface_info,
                default_extent,
            )),
            surface_info,
            rendering_order,
            buffer_ready_order,
            present_order,
        }
    }

    pub fn format(&self) -> br::vk::VkFormat {
        self.surface_info.format()
    }
    pub fn backbuffer_count(&self) -> usize {
        self.swapchain.get().backbuffer_images.len()
    }
    pub fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> {
        self.swapchain.get().backbuffer_images.get(index).cloned()
    }

    pub fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        let image_barriers = self
            .swapchain
            .get()
            .backbuffer_images
            .iter()
            .map(|v| {
                br::ImageMemoryBarrier::new(
                    &br::ImageSubref::color(v, 0, 0),
                    br::ImageLayout::Undefined,
                    br::ImageLayout::PresentSrc,
                )
            })
            .collect::<Vec<_>>();

        recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            false,
            &[],
            &[],
            &image_barriers,
        );
    }
    pub fn acquire_next_backbuffer_index(&self) -> br::Result<u32> {
        self.swapchain
            .get()
            .swapchain
            .acquire_next(None, br::CompletionHandler::from(&self.rendering_order))
    }
    pub fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (
            br::ImageLayout::PresentSrc,
            br::PipelineStageFlags::TOP_OF_PIPE,
        )
    }
    pub fn render_and_present<'s>(
        &'s mut self,
        g: &mut crate::Graphics,
        last_render_fence: &mut br::Fence,
        q: &br::Queue,
        bb_index: u32,
        mut render_submission: br::SubmissionBatch<'s>,
        update_submission: Option<br::SubmissionBatch<'s>>,
    ) -> br::Result<()> {
        if let Some(mut cs) = update_submission {
            // copy -> render
            cs.signal_semaphores.to_mut().push(&self.buffer_ready_order);
            render_submission.wait_semaphores.to_mut().extend(vec![
                (
                    &self.rendering_order,
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                ),
                (
                    &self.buffer_ready_order,
                    br::PipelineStageFlags::VERTEX_INPUT,
                ),
            ]);
            render_submission
                .signal_semaphores
                .to_mut()
                .push(&self.present_order);
            g.submit_buffered_commands(&[cs, render_submission], last_render_fence)
                .expect("Failed to submit render and update commands");
        } else {
            // render only (old logic)
            render_submission
                .signal_semaphores
                .to_mut()
                .push(&self.present_order);
            render_submission.wait_semaphores.to_mut().push((
                &self.rendering_order,
                br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            ));
            g.submit_buffered_commands(&[render_submission], last_render_fence)
                .expect("Failed to submit render commands");
        }

        self.swapchain
            .get()
            .swapchain
            .queue_present(q, bb_index, &[&self.present_order])
    }

    pub fn resize(&mut self, g: &crate::Graphics, new_size: peridot_math::Vector2<usize>) {
        self.swapchain.discard_lw();
        self.swapchain.set_lw(IntegratedSwapchainObject::new(
            g,
            &self.surface_info,
            new_size,
        ));
    }
}
