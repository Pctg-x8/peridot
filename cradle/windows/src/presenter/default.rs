#[cfg(feature = "debug")]
use br::VkObject;
use br::{Device, Image, PhysicalDevice, SubmissionBatch, Swapchain, VulkanStructure};
use br::{Extendable, VkHandle};
use peridot::{mthelper::SharedRef, DeviceObject, Discardable, Graphics, SurfaceInfo};
use winapi::shared::windef::HMONITOR;

use crate::ThreadsafeWindowOps;
use bedrock as br;

pub struct Presenter {
    _window: SharedRef<ThreadsafeWindowOps>,
    sc: ControlledSwapchain<br::SurfaceObject<peridot::InstanceObject>>,
}
impl Presenter {
    pub fn new(
        g: &peridot::Graphics,
        window: SharedRef<ThreadsafeWindowOps>,
        fullscreen_target_monitor: Option<HMONITOR>,
    ) -> Self {
        if !g
            .adapter()
            .win32_presentation_support(g.graphics_queue_family_index())
        {
            panic!("WindowSubsystem does not support Vulkan Rendering");
        }
        let s = g
            .adapter()
            .new_surface_win32(crate::module_handle(), window.0)
            .expect("Failed to create Surface");
        let support = g
            .adapter()
            .surface_support(g.graphics_queue_family_index(), &s)
            .expect("Failed to query Surface Support");
        if !support {
            panic!("Vulkan does not support this surface to render");
        }

        if let Some(mon) = fullscreen_target_monitor {
            let fsw: br::vk::VkSurfaceFullScreenExclusiveWin32InfoEXT =
                br::FullScreenExclusiveWin32InfoEXT::new(mon).into();
            let req = br::vk::VkPhysicalDeviceSurfaceInfo2KHR {
                sType: br::vk::VkPhysicalDeviceSurfaceInfo2KHR::TYPE,
                surface: s.native_ptr(),
                pNext: &fsw as *const _ as _,
            };
            let mut fs = std::mem::MaybeUninit::<br::vk::VkSurfaceCapabilitiesFullScreenExclusiveEXT>::uninit();
            let mut caps2 = std::mem::MaybeUninit::<br::vk::VkSurfaceCapabilities2KHR>::uninit();
            unsafe {
                (*fs.as_mut_ptr()).sType =
                    br::vk::VkSurfaceCapabilitiesFullScreenExclusiveEXT::TYPE;
                (*fs.as_mut_ptr()).pNext = std::ptr::null_mut();
                (*caps2.as_mut_ptr()).sType = br::vk::VkSurfaceCapabilities2KHR::TYPE;
                (*caps2.as_mut_ptr()).pNext = fs.as_mut_ptr() as _;
            }
            g.adapter()
                .surface_capabilities2(&req, unsafe { &mut *caps2.as_mut_ptr() })
                .expect("Failed to query surface extra capabilities");
            let fs = unsafe { fs.assume_init_ref() };
            println!(
                "fsex: {:?}",
                fs.fullScreenExclusiveSupported == br::vk::VK_TRUE
            );

            let pres_modes = g
                .adapter()
                .surface_present_modes2(&req)
                .expect("Failed to get present modes 2");
            for p in pres_modes {
                println!("pres mode 2: {p}");
            }
        }

        Self {
            _window: window,
            sc: ControlledSwapchain::new(
                g,
                s,
                peridot::math::Vector2(0, 0),
                fullscreen_target_monitor,
            ),
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    type Backbuffer = br::ImageViewObject<
        br::SwapchainImage<
            SharedRef<
                br::SwapchainObject<
                    peridot::DeviceObject,
                    br::SurfaceObject<peridot::InstanceObject>,
                >,
            >,
        >,
    >;

    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }
    fn backbuffer_count(&self) -> usize {
        self.sc.backbuffer_count()
    }
    fn backbuffer(&self, index: usize) -> Option<SharedRef<Self::Backbuffer>> {
        self.sc.backbuffer(index)
    }

    fn emit_initialize_backbuffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + ?Sized>,
    ) {
        self.sc.emit_initialize_backbuffer_commands(recorder)
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_backbuffer_index()
    }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_backbuffer_layout()
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::Fence,
        backbuffer_index: u32,
        render_submission: impl br::SubmissionBatch,
        update_submission: Option<impl br::SubmissionBatch>,
    ) -> br::Result<()> {
        self.sc.render_and_present(
            g,
            last_render_fence,
            backbuffer_index,
            render_submission,
            update_submission,
        )
    }
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }
    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> {
        peridot::math::Vector2(0, 0)
    }
}

type SharedSwapchainObject<Device, Surface> = SharedRef<br::SwapchainObject<Device, Surface>>;
struct IntegratedSwapchainObject<Device: br::Device, Surface: br::Surface> {
    swapchain: SharedSwapchainObject<Device, Surface>,
    backbuffer_images: Vec<
        SharedRef<br::ImageViewObject<br::SwapchainImage<SharedSwapchainObject<Device, Surface>>>>,
    >,
}
impl<Surface: br::Surface> IntegratedSwapchainObject<DeviceObject, Surface> {
    pub fn new(
        g: &Graphics,
        surface: Surface,
        surface_info: &SurfaceInfo,
        default_extent: peridot::math::Vector2<usize>,
        fullscreen_target_monitor: Option<HMONITOR>,
    ) -> Self {
        let si = g
            .adapter()
            .surface_capabilities(&surface)
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
        let mut cb = br::SwapchainBuilder::new(
            surface,
            buffer_count,
            &surface_info.fmt,
            &ext,
            br::ImageUsage::COLOR_ATTACHMENT,
        );
        cb.present_mode(surface_info.pres_mode)
            .composite_alpha(surface_info.available_composite_alpha)
            .pre_transform(pre_transform);
        let chain = if let Some(mon) = fullscreen_target_monitor {
            g.device()
                .clone()
                .new_swapchain(
                    cb.extends(br::FullScreenExclusiveInfoEXT::new(
                        br::FullScreenExclusiveEXT::ApplicationControlled,
                    ))
                    .extends(br::FullScreenExclusiveWin32InfoEXT::new(mon)),
                )
                .expect("Failed to create Swapchain")
        } else {
            g.device()
                .clone()
                .new_swapchain(cb)
                .expect("Failed to create Swapchain")
        };
        let chain = SharedRef::new(chain);
        #[cfg(feature = "debug")]
        chain
            .set_name(Some(unsafe {
                std::ffi::CStr::from_bytes_with_nul_unchecked(
                    b"Peridot-Default Presentor-Swapchain\0",
                )
            }))
            .expect("Failed to set swapchain name");

        if fullscreen_target_monitor.is_some() {
            chain
                .acquire_full_screen_exclusive_mode()
                .expect("Failed to acquire fullscreen mode");
        }

        let isr_c0 = br::ImageSubresourceRange::color(0, 0);
        let backbuffer_images: Vec<SharedRef<_>> = chain
            .get_images()
            .expect("Failed to get backbuffer images")
            .into_iter()
            .map(|bb| {
                bb.clone_parent()
                    .create_view(None, None, &Default::default(), &isr_c0)
                    .expect("Failed to create ImageView for Backbuffer")
                    .into()
            })
            .collect();

        #[cfg(feature = "debug")]
        for (n, v) in backbuffer_images.iter().enumerate() {
            v.set_name(Some(
                &std::ffi::CString::new(format!("Peridot-Default Presentor-Backbuffer View #{n}"))
                    .expect("invalid sequence?"),
            ))
            .expect("Failed to set backbuffer view name");
        }

        Self {
            swapchain: chain,
            backbuffer_images,
        }
    }
}

struct ControlledSwapchain<Surface: br::Surface> {
    surface_info: SurfaceInfo,
    swapchain: Discardable<IntegratedSwapchainObject<DeviceObject, Surface>>,
    rendering_order: br::SemaphoreObject<DeviceObject>,
    buffer_ready_order: br::SemaphoreObject<DeviceObject>,
    present_order: br::SemaphoreObject<DeviceObject>,
    fullscreen_target_monitor: Option<HMONITOR>,
}
impl<Surface: br::Surface> ControlledSwapchain<Surface> {
    pub fn new(
        g: &Graphics,
        surface: Surface,
        default_extent: peridot::math::Vector2<usize>,
        fullscreen_target_monitor: Option<HMONITOR>,
    ) -> Self {
        let surface_info =
            SurfaceInfo::gather_info(g.adapter(), &surface).expect("Failed to gather surface info");

        let rendering_order = g
            .device()
            .clone()
            .new_semaphore()
            .expect("Failed to create Rendering Order Semaphore");
        let buffer_ready_order = g
            .device()
            .clone()
            .new_semaphore()
            .expect("Failed to create BufferReady Order Semaphore");
        let present_order = g
            .device()
            .clone()
            .new_semaphore()
            .expect("Failed to create Present Order Semaphore");
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

        Self {
            swapchain: Discardable::from(IntegratedSwapchainObject::new(
                g,
                surface,
                &surface_info,
                default_extent,
                fullscreen_target_monitor,
            )),
            surface_info,
            rendering_order,
            buffer_ready_order,
            present_order,
            fullscreen_target_monitor,
        }
    }

    #[inline]
    pub const fn format(&self) -> br::vk::VkFormat {
        self.surface_info.format()
    }

    #[inline]
    pub fn backbuffer_count(&self) -> usize {
        self.swapchain.get().backbuffer_images.len()
    }

    #[inline]
    pub fn backbuffer(
        &self,
        index: usize,
    ) -> Option<
        SharedRef<
            br::ImageViewObject<br::SwapchainImage<SharedSwapchainObject<DeviceObject, Surface>>>,
        >,
    > {
        self.swapchain.get().backbuffer_images.get(index).cloned()
    }

    pub fn emit_initialize_backbuffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + ?Sized>,
    ) {
        let image_barriers = self
            .swapchain
            .get()
            .backbuffer_images
            .iter()
            .map(|v| {
                br::ImageMemoryBarrier::new(
                    &***v,
                    br::ImageSubresourceRange::color(0, 0),
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

    #[inline]
    pub fn acquire_next_backbuffer_index(&mut self) -> br::Result<u32> {
        self.swapchain.get_mut_lw().swapchain.acquire_next(
            None,
            br::CompletionHandler::<br::FenceObject<DeviceObject>, _>::Queue(&self.rendering_order),
        )
    }

    #[inline]
    pub const fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (
            br::ImageLayout::PresentSrc,
            br::PipelineStageFlags::TOP_OF_PIPE,
        )
    }

    pub fn render_and_present<'s>(
        &'s mut self,
        g: &mut Graphics,
        last_render_fence: &mut impl br::Fence,
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
            g.graphics_queue_mut().get_mut(),
            bb_index,
            &[&self.present_order],
        )
    }

    pub fn resize(&mut self, g: &Graphics, new_size: peridot::math::Vector2<usize>) {
        if let Some(mut old) = self.swapchain.take_lw() {
            old.backbuffer_images.clear();
            let (_, s) = SharedRef::try_unwrap(old.swapchain)
                .unwrap_or_else(|_| panic!("there are some references of swapchain left"))
                .deconstruct();
            self.swapchain.set_lw(IntegratedSwapchainObject::new(
                g,
                s,
                &self.surface_info,
                new_size,
                self.fullscreen_target_monitor,
            ));
        }
    }
}
