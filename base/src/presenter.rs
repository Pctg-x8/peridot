//! Platform Presenter(Swapchain Abstraction)

use bedrock as br;
#[cfg(feature = "debug")]
use br::VkObject;
use br::{ImageSubresourceSlice, PhysicalDevice, SubmissionBatch, Swapchain};

use crate::{mthelper::SharedRef, DeviceObject};

pub trait PlatformPresenter {
    type BackBuffer: br::ImageView
        + br::ImageChild
        + br::DeviceChild<ConcreteDevice = DeviceObject>
        + 'static;

    fn format(&self) -> br::vk::VkFormat;
    fn back_buffer_count(&self) -> usize;
    fn back_buffer(&self, index: usize) -> Option<SharedRef<Self::BackBuffer>>;

    fn emit_initialize_back_buffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut + ?Sized>,
    );
    fn next_back_buffer_index(&mut self) -> br::Result<u32>;
    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags);
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut crate::Graphics,
        last_render_fence: &mut (impl br::Fence + br::VkHandleMut),
        back_buffer_index: u32,
        render_submission: impl br::SubmissionBatch,
        update_submission: Option<impl br::SubmissionBatch>,
    ) -> br::Result<()>;
    /// Returns whether re-initializing is needed for back-buffer resources
    fn resize(&mut self, g: &crate::Graphics, new_size: peridot_math::Vector2<usize>) -> bool;
    fn current_geometry_extent(&self) -> peridot_math::Vector2<usize>;
}

type SharedSwapchainObject<Device, Surface> =
    SharedRef<br::SurfaceSwapchainObject<Device, Surface>>;
struct IntegratedSwapchainObject<Device: br::Device, Surface: br::Surface> {
    swapchain: SharedSwapchainObject<Device, Surface>,
    back_buffer_images: Vec<
        SharedRef<br::ImageViewObject<br::SwapchainImage<SharedSwapchainObject<Device, Surface>>>>,
    >,
}
impl<Surface: br::Surface> IntegratedSwapchainObject<DeviceObject, Surface> {
    pub fn new(
        g: &crate::Graphics,
        surface: Surface,
        surface_info: &crate::SurfaceInfo,
        default_extent: peridot_math::Vector2<usize>,
    ) -> Self {
        let si = g
            .adapter
            .surface_capabilities(&surface)
            .expect("Failed to query Surface Capabilities");
        let ew = if si.currentExtent.width == u32::MAX {
            default_extent.0 as _
        } else {
            si.currentExtent.width
        };
        let eh = if si.currentExtent.height == u32::MAX {
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
            surface,
            buffer_count,
            surface_info.fmt.clone(),
            ext,
            br::ImageUsage::COLOR_ATTACHMENT,
        )
        .present_mode(surface_info.pres_mode)
        .composite_alpha(surface_info.available_composite_alpha)
        .pre_transform(pre_transform)
        .create(g.device.clone())
        .expect("Failed to create Swapchain");
        let chain = SharedRef::new(chain);
        #[cfg(feature = "debug")]
        chain
            .set_name(Some(unsafe {
                std::ffi::CStr::from_bytes_with_nul_unchecked(
                    b"Peridot-Default Presenter-Swapchain\0",
                )
            }))
            .expect("Failed to set swapchain name");

        let back_buffer_images: Vec<SharedRef<_>> = chain
            .get_images()
            .expect("Failed to get back-buffer images")
            .into_iter()
            .map(|bb| {
                bb.clone_parent()
                    .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                    .view_builder()
                    .create()
                    .expect("Failed to create ImageView for Back-Buffer")
                    .into()
            })
            .collect();

        #[cfg(feature = "debug")]
        for (n, v) in back_buffer_images.iter().enumerate() {
            v.set_name(Some(
                &std::ffi::CString::new(format!(
                    "Peridot-Default Presenter-BackBuffer View #{}",
                    n
                ))
                .expect("invalid sequence?"),
            ))
            .expect("Failed to set back-buffer view name");
        }

        Self {
            swapchain: chain,
            back_buffer_images,
        }
    }
}

/// WSI Swapchain implementation for PlatformPresenter
pub struct IntegratedSwapchain<Surface: br::Surface> {
    surface_info: crate::SurfaceInfo,
    swapchain: crate::Discardable<IntegratedSwapchainObject<DeviceObject, Surface>>,
    rendering_order: br::SemaphoreObject<DeviceObject>,
    buffer_ready_order: br::SemaphoreObject<DeviceObject>,
    present_order: br::SemaphoreObject<DeviceObject>,
}
impl<Surface: br::Surface> IntegratedSwapchain<Surface> {
    pub fn new(
        g: &crate::Graphics,
        surface: Surface,
        default_extent: peridot_math::Vector2<usize>,
    ) -> Self {
        let surface_info = crate::SurfaceInfo::gather_info(&g.adapter, &surface)
            .expect("Failed to gather surface info");

        let rendering_order = br::SemaphoreBuilder::new()
            .create(g.device.clone())
            .expect("Failed to create Rendering Order Semaphore");
        let buffer_ready_order = br::SemaphoreBuilder::new()
            .create(g.device.clone())
            .expect("Failed to create BufferReady Order Semaphore");
        let present_order = br::SemaphoreBuilder::new()
            .create(g.device.clone())
            .expect("Failed to create Present Order Semaphore");
        #[cfg(feature = "debug")]
        {
            rendering_order
                .set_name(Some(unsafe {
                    std::ffi::CStr::from_bytes_with_nul_unchecked(
                        b"Peridot-Default Presenter-Rendering Order Semaphore\0",
                    )
                }))
                .expect("Failed to set Rendering Order Semaphore name");
            buffer_ready_order
                .set_name(Some(unsafe {
                    std::ffi::CStr::from_bytes_with_nul_unchecked(
                        b"Peridot-Default Presenter-BufferReady Order Semaphore\0",
                    )
                }))
                .expect("Failed to set BufferReady Order Semaphore name");
            present_order
                .set_name(Some(unsafe {
                    std::ffi::CStr::from_bytes_with_nul_unchecked(
                        b"Peridot-Default Presenter-Present Order Semaphore\0",
                    )
                }))
                .expect("Failed to set Present Order Semaphore name");
        }

        Self {
            swapchain: crate::Discardable::from(IntegratedSwapchainObject::new(
                g,
                surface,
                &surface_info,
                default_extent,
            )),
            surface_info,
            rendering_order,
            buffer_ready_order,
            present_order,
        }
    }

    #[inline]
    pub const fn format(&self) -> br::vk::VkFormat {
        self.surface_info.format()
    }

    #[inline]
    pub fn back_buffer_count(&self) -> usize {
        self.swapchain.get().back_buffer_images.len()
    }

    #[inline]
    pub fn back_buffer(
        &self,
        index: usize,
    ) -> Option<
        SharedRef<
            br::ImageViewObject<br::SwapchainImage<SharedSwapchainObject<DeviceObject, Surface>>>,
        >,
    > {
        self.swapchain.get().back_buffer_images.get(index).cloned()
    }

    pub fn emit_initialize_back_buffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut + ?Sized>,
    ) {
        let image_barriers = self
            .swapchain
            .get()
            .back_buffer_images
            .iter()
            .map(|v| {
                br::ImageMemoryBarrier::new(
                    &***v,
                    br::vk::VkImageSubresourceRange {
                        aspectMask: br::AspectMask::COLOR.0,
                        baseMipLevel: 0,
                        levelCount: 1,
                        baseArrayLayer: 0,
                        layerCount: 1,
                    },
                    br::ImageLayout::Undefined,
                    br::ImageLayout::PresentSrc,
                )
            })
            .collect::<Vec<_>>();

        let _ = recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            false,
            &[],
            &[],
            &image_barriers,
        );
    }

    #[inline]
    pub fn acquire_next_back_buffer_index(&mut self) -> br::Result<u32> {
        self.swapchain.get_mut_lw().swapchain.acquire_next(
            None,
            br::CompletionHandler::<br::FenceObject<DeviceObject>, _>::Queue(&self.rendering_order),
        )
    }

    #[inline]
    pub const fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (
            br::ImageLayout::PresentSrc,
            br::PipelineStageFlags::TOP_OF_PIPE,
        )
    }

    pub fn render_and_present<'s>(
        &'s mut self,
        g: &mut crate::Graphics,
        last_render_fence: &mut (impl br::Fence + br::VkHandleMut),
        bb_index: u32,
        render_submission: impl br::SubmissionBatch,
        update_submission: Option<impl br::SubmissionBatch>,
    ) -> br::Result<()> {
        if let Some(cs) = update_submission {
            // copy -> render
            let update_signal = &[&self.buffer_ready_order];
            let render_waits = &[
                (
                    &self.rendering_order,
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                ),
                (
                    &self.buffer_ready_order,
                    br::PipelineStageFlags::VERTEX_INPUT,
                ),
            ];
            let render_signal = &[&self.present_order];

            let update_submission = cs.with_signal_semaphores(update_signal);
            let render_submission = render_submission
                .with_wait_semaphores(render_waits)
                .with_signal_semaphores(render_signal);

            g.submit_buffered_commands(
                &[
                    Box::new(update_submission) as Box<dyn br::SubmissionBatch>,
                    Box::new(render_submission),
                ],
                last_render_fence,
            )?;
        } else {
            // render only (old logic)
            let render_signal = &[&self.present_order];
            let render_waits = &[(
                &self.rendering_order,
                br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
            )];

            let render_submission = render_submission
                .with_signal_semaphores(render_signal)
                .with_wait_semaphores(render_waits);

            g.submit_buffered_commands(&[render_submission], last_render_fence)?;
        }

        self.swapchain.get_mut_lw().swapchain.queue_present(
            g.graphics_queue.q.get_mut(),
            bb_index,
            &[&self.present_order],
        )
    }

    pub fn resize(&mut self, g: &crate::Graphics, new_size: peridot_math::Vector2<usize>) {
        if let Some(mut old) = self.swapchain.take_lw() {
            old.back_buffer_images.clear();
            let (_, s) = SharedRef::try_unwrap(old.swapchain)
                .unwrap_or_else(|refs| {
                    panic!(
                        "there are some references of swapchain left: strong={} weak={}",
                        SharedRef::strong_count(&refs),
                        SharedRef::weak_count(&refs)
                    )
                })
                .deconstruct();
            self.swapchain.set_lw(IntegratedSwapchainObject::new(
                g,
                s,
                &self.surface_info,
                new_size,
            ));
        }
    }
}
