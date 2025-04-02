//! Platform Presenter(Swapchain Abstraction)

use bedrock::{self as br, VkRawHandle};

use crate::{graphics::VulkanGfx, mthelper::SharedRef};

pub trait PlatformPresenter {
    fn format(&self) -> br::vk::VkFormat;
    fn back_buffer_size(&self) -> peridot_math::Vector2<u32>;
    fn back_buffer_count(&self) -> usize;
    fn back_buffer<'a>(&'a self, index: usize) -> Option<br::VkHandleRef<'a, br::vk::VkImage>>;

    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: br::CmdRecord<'r, VulkanGfx>,
    ) -> br::CmdRecord<'r, VulkanGfx>;
    fn next_back_buffer_index(&mut self) -> br::Result<u32>;
    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags);
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut crate::Graphics,
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        back_buffer_index: u32,
        render_submission: SubmissionBatchBuilder,
        update_submission: Option<SubmissionBatchBuilder>,
    ) -> br::Result<()>;
    /// Returns whether re-initializing is needed for back-buffer resources
    fn resize(&mut self, g: &crate::Graphics, new_size: peridot_math::Vector2<u32>) -> bool;
    fn current_geometry_extent(&self) -> peridot_math::Vector2<u32>;
}

struct IntegratedSwapchainObjectCore<Surface> {
    surface: Surface,
    handle: br::vk::VkSwapchainKHR,
    device: VulkanGfx,
}
impl<Surface> Drop for IntegratedSwapchainObjectCore<Surface> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_swapchain(self.device.0.device, self.handle, None);
        }
    }
}
impl<Surface> br::VkObject for IntegratedSwapchainObjectCore<Surface> {
    const TYPE: br::vk::VkObjectType = br::vk::VkSwapchainKHR::OBJECT_TYPE;
}
impl<Surface> br::VkHandle for IntegratedSwapchainObjectCore<Surface> {
    type Handle = br::vk::VkSwapchainKHR;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
impl<Surface> br::VkHandleMut for IntegratedSwapchainObjectCore<Surface> {
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.handle
    }
}
impl<Surface> IntegratedSwapchainObjectCore<Surface> {
    pub fn unwrap_surface(self) -> Surface {
        let surface = unsafe { core::ptr::read(&self.surface) };
        let handle = unsafe { core::ptr::read(&self.handle) };
        let device = unsafe { core::ptr::read(&self.device) };
        unsafe {
            br::vkfn_wrapper::destroy_swapchain(device.0.device, handle, None);
        }
        core::mem::forget(self);

        surface
    }
}

#[derive(Clone)]
pub struct IntegratedSwapchainObjectBackbufferRef<Surface> {
    handle: br::vk::VkImage,
    _source: SharedRef<IntegratedSwapchainObjectCore<Surface>>,
}
impl<Surface> br::VkHandle for IntegratedSwapchainObjectBackbufferRef<Surface> {
    type Handle = br::vk::VkImage;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

struct IntegratedSwapchainObject<Surface> {
    core: SharedRef<IntegratedSwapchainObjectCore<Surface>>,
    buffer_size: peridot_math::Vector2<u32>,
    back_buffer_image_handles: Vec<br::vk::VkImage>,
}
impl<Surface: br::VkHandle<Handle = br::vk::VkSurfaceKHR>> IntegratedSwapchainObject<Surface> {
    pub fn new(
        g: &crate::Graphics,
        surface: Surface,
        surface_info: &crate::SurfaceInfo,
        default_extent: peridot_math::Vector2<u32>,
    ) -> Self {
        let si = match g.gfx_device.surface_capabilities(&surface) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to query surface capabilities");
                std::process::abort();
            }
        };
        let ew = if si.currentExtent.width == 0xffff_ffff {
            default_extent.0
        } else {
            si.currentExtent.width
        };
        let eh = if si.currentExtent.height == 0xffff_ffff {
            default_extent.1
        } else {
            si.currentExtent.height
        };
        let ew = ew.clamp(si.minImageExtent.width, si.maxImageExtent.width);
        let eh = eh.clamp(si.minImageExtent.height, si.maxImageExtent.height);
        let ext = br::Extent2D {
            width: ew,
            height: eh,
        };
        let buffer_count = 2.clamp(si.minImageCount, si.maxImageCount);
        let pre_transform =
            if (si.supportedTransforms & br::vk::VK_SURFACE_TRANSFORM_IDENTITY_BIT_KHR) != 0 {
                br::SurfaceTransformFlags::IDENTITY
            } else {
                br::SurfaceTransformFlags::INHERIT
            };
        let chain = unsafe {
            br::vkfn_wrapper::create_swapchain(
                g.gfx_device.0.device,
                &br::SwapchainCreateInfo::new(
                    &surface,
                    buffer_count,
                    surface_info.fmt.clone(),
                    ext,
                    br::ImageUsageFlags::COLOR_ATTACHMENT,
                )
                .present_mode(surface_info.pres_mode)
                .composite_alpha(surface_info.available_composite_alpha)
                .pre_transform(pre_transform),
                None,
            )
        };
        let chain = SharedRef::new(IntegratedSwapchainObjectCore {
            surface,
            handle: match chain {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(cause = ?e, "Failed to create swapchain");
                    std::process::abort();
                }
            },
            device: g.gfx_device.clone(),
        });
        #[cfg(feature = "debug")]
        if let Err(e) = g
            .gfx_device
            .set_object_name(&chain, c"Peridot-Default Presenter-Swapchain")
        {
            tracing::warn!(cause = ?e, "Failed to set swapchain name");
        }

        let n = match unsafe {
            br::vkfn_wrapper::get_swapchain_image_count(g.gfx_device.0.device, chain.handle)
        } {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to acquire swapchain images");
                std::process::abort();
            }
        };
        let mut back_buffer_image_handles = Vec::with_capacity(n as _);
        unsafe {
            back_buffer_image_handles.set_len(back_buffer_image_handles.capacity());
        }
        if let Err(e) = unsafe {
            br::vkfn_wrapper::get_swapchain_images(
                g.gfx_device.0.device,
                chain.handle,
                &mut back_buffer_image_handles,
            )
        } {
            tracing::error!(cause = ?e, "Failed to acquire swapchain images");
            std::process::abort();
        }

        #[cfg(feature = "debug")]
        for (n, v) in back_buffer_image_handles.iter().enumerate() {
            let name = unsafe {
                std::ffi::CString::from_vec_unchecked(
                    format!("Peridot-Default Presenter-BackBuffer #{n}").into_bytes(),
                )
            };
            if let Err(e) = unsafe {
                g.gfx_device
                    .set_object_name_raw(br::vk::VkImage::OBJECT_TYPE, v, &name)
            } {
                tracing::warn!(cause = ?e, "Failed to set swapchain backbuffer image name");
            }
        }

        Self {
            core: chain,
            buffer_size: ext.into(),
            back_buffer_image_handles,
        }
    }

    pub fn nth_backbuffer(&self, index: usize) -> IntegratedSwapchainObjectBackbufferRef<Surface> {
        IntegratedSwapchainObjectBackbufferRef {
            handle: self.back_buffer_image_handles[index],
            _source: self.core.clone(),
        }
    }
}

pub struct SubmissionBatchBuilder<'d> {
    wait_semaphores: Vec<br::VkHandleRef<'d, br::vk::VkSemaphore>>,
    wait_dst_stages: Vec<br::PipelineStageFlags>,
    command_buffers: Vec<br::VkHandleRef<'d, br::vk::VkCommandBuffer>>,
    signal_semaphores: Vec<br::VkHandleRef<'d, br::vk::VkSemaphore>>,
}
impl<'d> SubmissionBatchBuilder<'d> {
    pub fn new() -> Self {
        Self {
            wait_semaphores: Vec::new(),
            wait_dst_stages: Vec::new(),
            command_buffers: Vec::new(),
            signal_semaphores: Vec::new(),
        }
    }

    pub fn add_wait_semaphores(
        &mut self,
        wait_semaphores_with_dst_stages: impl IntoIterator<
            Item = (
                br::VkHandleRef<'d, br::vk::VkSemaphore>,
                br::PipelineStageFlags,
            ),
        >,
    ) -> &mut Self {
        let iter = wait_semaphores_with_dst_stages.into_iter();
        let (lb, ub) = iter.size_hint();
        let ext = ub.unwrap_or(lb);
        let _ = self.wait_semaphores.try_reserve(ext);
        let _ = self.wait_dst_stages.try_reserve(ext);
        for (o, s) in iter {
            self.wait_semaphores.push(o);
            self.wait_dst_stages.push(s);
        }

        self
    }

    pub fn add_command_buffers(
        &mut self,
        command_buffers: impl IntoIterator<Item = br::VkHandleRef<'d, br::vk::VkCommandBuffer>>,
    ) -> &mut Self {
        self.command_buffers.extend(command_buffers);

        self
    }

    pub fn add_signal_semaphores(
        &mut self,
        semaphores: impl IntoIterator<Item = br::VkHandleRef<'d, br::vk::VkSemaphore>>,
    ) -> &mut Self {
        self.signal_semaphores.extend(semaphores);

        self
    }

    fn build(&self) -> br::SubmitInfo {
        br::SubmitInfo::new(
            &self.wait_semaphores,
            &self.wait_dst_stages,
            &self.command_buffers,
            &self.signal_semaphores,
        )
    }
}

/// WSI Swapchain implementation for PlatformPresenter
pub struct IntegratedSwapchain<Surface: br::VkHandle<Handle = br::vk::VkSurfaceKHR>> {
    surface_info: crate::SurfaceInfo,
    swapchain: crate::Discardable1<IntegratedSwapchainObject<Surface>>,
    rendering_order: br::vk::VkSemaphore,
    buffer_ready_order: br::vk::VkSemaphore,
    present_order: br::vk::VkSemaphore,
    gfx_device: VulkanGfx,
}
impl<Surface: br::VkHandle<Handle = br::vk::VkSurfaceKHR>> Drop for IntegratedSwapchain<Surface> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_semaphore(self.gfx_device.0.device, self.present_order, None);
            br::vkfn_wrapper::destroy_semaphore(
                self.gfx_device.0.device,
                self.buffer_ready_order,
                None,
            );
            br::vkfn_wrapper::destroy_semaphore(
                self.gfx_device.0.device,
                self.rendering_order,
                None,
            );
        }
    }
}
impl<Surface: br::VkHandle<Handle = br::vk::VkSurfaceKHR>> IntegratedSwapchain<Surface> {
    pub fn new(
        g: &crate::Graphics,
        surface: Surface,
        default_extent: peridot_math::Vector2<u32>,
    ) -> Self {
        let surface_info = match crate::SurfaceInfo::gather_info(&g.gfx_device, &surface) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to gather surface info");
                std::process::abort();
            }
        };

        let rendering_order = unsafe {
            br::vkfn_wrapper::create_semaphore(
                g.gfx_device.0.device,
                &br::SemaphoreCreateInfo::new(),
                None,
            )
        };
        let buffer_ready_order = unsafe {
            br::vkfn_wrapper::create_semaphore(
                g.gfx_device.0.device,
                &br::SemaphoreCreateInfo::new(),
                None,
            )
        };
        let present_order = unsafe {
            br::vkfn_wrapper::create_semaphore(
                g.gfx_device.0.device,
                &br::SemaphoreCreateInfo::new(),
                None,
            )
        };
        let (rendering_order, buffer_ready_order, present_order) = match (
            rendering_order,
            buffer_ready_order,
            present_order,
        ) {
            (Ok(a), Ok(b), Ok(c)) => (a, b, c),
            _ => {
                if let Err(e) = rendering_order {
                    tracing::error!(cause = ?e, "Failed to create rendering order semaphore");
                }
                if let Err(e) = buffer_ready_order {
                    tracing::error!(cause = ?e, "Failed to create buffer ready order semaphore");
                }
                if let Err(e) = present_order {
                    tracing::error!(cause = ?e, "Failed to create present order semaphore");
                }

                std::process::abort();
            }
        };

        #[cfg(feature = "debug")]
        {
            if let Err(e) = unsafe {
                g.gfx_device.set_object_name_raw(
                    br::vk::VK_OBJECT_TYPE_SEMAPHORE,
                    &rendering_order,
                    c"Peridot-Default Presenter-Rendering Order Semaphore",
                )
            } {
                tracing::warn!(cause = ?e, "Failed to set rendering order semaphore name");
            }
            if let Err(e) = unsafe {
                g.gfx_device.set_object_name_raw(
                    br::vk::VK_OBJECT_TYPE_SEMAPHORE,
                    &buffer_ready_order,
                    c"Peridot-Default Presenter-BufferReady Order Semaphore",
                )
            } {
                tracing::warn!(cause = ?e, "Failed to set buffer ready order semaphore name");
            }
            if let Err(e) = unsafe {
                g.gfx_device.set_object_name_raw(
                    br::vk::VK_OBJECT_TYPE_SEMAPHORE,
                    &present_order,
                    c"Peridot-Default Presenter-Present Order Semaphore",
                )
            } {
                tracing::warn!(cause = ?e, "Failed to set present order semaphore name");
            }
        }

        Self {
            swapchain: crate::Discardable1::from(IntegratedSwapchainObject::new(
                g,
                surface,
                &surface_info,
                default_extent,
            )),
            surface_info,
            rendering_order,
            buffer_ready_order,
            present_order,
            gfx_device: g.gfx_device.clone(),
        }
    }

    #[inline]
    pub const fn format(&self) -> br::vk::VkFormat {
        self.surface_info.format()
    }

    #[inline]
    pub fn back_buffer_count(&self) -> usize {
        self.swapchain.get().back_buffer_image_handles.len()
    }

    #[inline]
    pub fn back_buffer<'s>(&'s self, index: usize) -> Option<br::VkHandleRef<'s, br::vk::VkImage>> {
        self.swapchain
            .get()
            .back_buffer_image_handles
            .get(index)
            .map(|&x| unsafe { br::VkHandleRef::dangling(x) })
    }

    #[inline]
    pub fn back_buffer_mut<'s>(
        &'s mut self,
        index: usize,
    ) -> Option<br::VkHandleRefMut<'s, br::vk::VkImage>> {
        self.swapchain
            .get()
            .back_buffer_image_handles
            .get(index)
            .map(|&x| unsafe { br::VkHandleRefMut::dangling(x) })
    }

    // TODO: undefined -> anyが無条件に許可される環境だったらこれいらない気がする synchronization2拡張が有効じゃないとダメとかあったかもしれないのであとでVulkanの仕様をあたる
    pub fn emit_initialize_back_buffer_commands<'r, E: 'r + ?Sized>(
        &self,
        recorder: br::CmdRecord<'r, E>,
    ) -> br::CmdRecord<'r, E> {
        let image_barriers = self
            .swapchain
            .get()
            .back_buffer_image_handles
            .iter()
            .map(|&v| {
                br::ImageMemoryBarrier::new(
                    unsafe { &br::VkHandleRef::dangling(v) },
                    br::vk::VkImageSubresourceRange {
                        aspectMask: br::AspectMask::COLOR.bits(),
                        baseMipLevel: 0,
                        levelCount: 1,
                        baseArrayLayer: 0,
                        layerCount: 1,
                    },
                    br::ImageLayout::PresentSrc.from_undefined(),
                )
            })
            .collect::<Vec<_>>();

        recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            0,
            &[],
            &[],
            &image_barriers,
        )
    }

    #[inline]
    pub fn acquire_next_back_buffer_index(&mut self) -> br::Result<u32> {
        unsafe {
            br::vkfn_wrapper::acquire_next_image(
                self.gfx_device.0.device,
                br::VkHandleRefMut::dangling(self.swapchain.get_mut().core.handle),
                u64::MAX,
                Some(br::VkHandleRefMut::dangling(self.rendering_order)),
                None,
            )
        }
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
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        bb_index: u32,
        mut render_submission: SubmissionBatchBuilder,
        mut update_submission: Option<SubmissionBatchBuilder>,
    ) -> br::Result<()> {
        if let Some(ref mut cs) = update_submission {
            // copy -> render
            cs.add_signal_semaphores([unsafe {
                br::VkHandleRef::dangling(self.buffer_ready_order)
            }]);
            render_submission
                .add_wait_semaphores([
                    (
                        unsafe { br::VkHandleRef::dangling(self.rendering_order) },
                        br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    ),
                    (
                        unsafe { br::VkHandleRef::dangling(self.buffer_ready_order) },
                        br::PipelineStageFlags::VERTEX_INPUT,
                    ),
                ])
                .add_signal_semaphores([unsafe { br::VkHandleRef::dangling(self.present_order) }]);

            g.submit_buffered_commands_raw(
                &[cs.build(), render_submission.build()],
                last_render_fence,
            )?;
        } else {
            // render only (old logic)
            render_submission
                .add_wait_semaphores([(
                    unsafe { br::VkHandleRef::dangling(self.rendering_order) },
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                )])
                .add_signal_semaphores([unsafe { br::VkHandleRef::dangling(self.present_order) }]);

            g.submit_buffered_commands_raw(&[render_submission.build()], last_render_fence)?;
        }

        unsafe {
            br::vkfn_wrapper::queue_present(
                g.graphics_queue.q,
                &br::PresentInfo::new(
                    &[br::VkHandleRef::dangling(self.present_order)],
                    &[br::VkHandleRef::dangling(self.swapchain.get().core.handle)],
                    &[bb_index],
                    &mut [br::vk::VK_SUCCESS],
                ),
            )
            .map(drop)
        }
    }

    pub fn resize(&mut self, g: &crate::Graphics, new_size: peridot_math::Vector2<u32>) {
        if let Some(mut old) = self.swapchain.take() {
            old.back_buffer_image_handles.clear();
            let s = match SharedRef::try_unwrap(old.core) {
                Ok(x) => x.unwrap_surface(),
                Err(refs) => {
                    tracing::error!(
                        "there are some references of swapchain left: strong={} weak={}",
                        SharedRef::strong_count(&refs),
                        SharedRef::weak_count(&refs)
                    );
                    std::process::abort();
                }
            };
            self.swapchain.set(IntegratedSwapchainObject::new(
                g,
                s,
                &self.surface_info,
                new_size,
            ));
        }
    }

    pub fn back_buffer_size(&self) -> peridot_math::Vector2<u32> {
        self.swapchain.get().buffer_size
    }
}
