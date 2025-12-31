use std::path::Path;

use bedrock::{
    self as br, Device, Instance, PhysicalDevice, ResolverInterface, TypedVulkanStructure,
    VkHandle, VkRawHandle,
};

pub const VI_STATE_EMPTY: &br::PipelineVertexInputStateCreateInfo =
    &br::PipelineVertexInputStateCreateInfo::new(&[], &[]);

pub const IA_STATE_TRILIST: &br::PipelineInputAssemblyStateCreateInfo =
    &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleList);

pub const RASTER_STATE_DEFAULT_FILL_NOCULL: &br::PipelineRasterizationStateCreateInfo =
    &br::PipelineRasterizationStateCreateInfo::new(
        br::PolygonMode::Fill,
        br::CullModeFlags::NONE,
        br::FrontFace::CounterClockwise,
    );

pub const BLEND_STATE_SINGLE_NONE: &br::PipelineColorBlendStateCreateInfo =
    &br::PipelineColorBlendStateCreateInfo::new(&[
        br::vk::VkPipelineColorBlendAttachmentState::NOBLEND,
    ]);

pub const MS_STATE_EMPTY: &br::PipelineMultisampleStateCreateInfo =
    &br::PipelineMultisampleStateCreateInfo::new();

pub struct VulkanDevice {
    native: br::vk::VkDevice,
    adapter: br::vk::VkPhysicalDevice,
    parent: br::InstanceObject,
    memory_properties: br::MemoryProperties,
    graphics_queue: br::vk::VkQueue,
    graphics_queue_family_index: u32,
    fp_cmd_pipeline_barrier2: br::vk::PFN_vkCmdPipelineBarrier2KHR,
    fp_create_render_pass2: br::vk::PFN_vkCreateRenderPass2KHR,
    fp_cmd_begin_render_pass2: br::vk::PFN_vkCmdBeginRenderPass2KHR,
    fp_cmd_end_render_pass2: br::vk::PFN_vkCmdEndRenderPass2KHR,
    fp_debug_utils_set_object_name: br::vk::PFN_vkSetDebugUtilsObjectNameEXT,
}
unsafe impl Sync for VulkanDevice {}
unsafe impl Send for VulkanDevice {}
impl Drop for VulkanDevice {
    fn drop(&mut self) {
        unsafe {
            br::vkfn::destroy_device(self.native, core::ptr::null());
        }
    }
}
impl br::VkHandle for VulkanDevice {
    type Handle = br::vk::VkDevice;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.native
    }
}
impl br::InstanceChild for VulkanDevice {
    type ConcreteInstance = br::InstanceObject;

    fn instance(&self) -> &Self::ConcreteInstance {
        &self.parent
    }
}
impl br::Device for VulkanDevice {}
impl VulkanDevice {
    pub fn new() -> Self {
        let api_version = match br::instance_version() {
            Ok(v) => {
                tracing::info!(version = %v, "Vulkan");
                v
            }
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to get vulkan version");
                br::Version::new(0, 1, 0, 0)
            }
        };

        if let Some(xs) = br::instance_extension_properties_cstr_alloc(None)
            .inspect_err(
                |e| tracing::error!(reason = ?e, "Failed to enumerate vulkan instance extensions"),
            )
            .ok()
        {
            for x in xs {
                tracing::info!(
                    target: "vk::diag::instance",
                    name = ?x.extensionName.as_cstr(),
                    version = x.specVersion,
                    "vulkan instance extension"
                );
            }
        }

        if let Some(xs) = br::enumerate_layer_properties_alloc()
            .inspect_err(
                |e| tracing::error!(reason = ?e, "Failed to enumerate vulkan instance layers"),
            )
            .ok()
        {
            for x in xs {
                tracing::info!(
                    target: "vk::diag::instance",
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
                            target: "vk::diag::instance",
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
        #[cfg(feature = "wayland")]
        instance_extensions.push(c"VK_KHR_wayland_surface".into());
        #[cfg(target_os = "macos")]
        instance_extensions.push(c"VK_EXT_metal_surface".into());
        #[cfg(target_os = "macos")]
        instance_extensions.push(c"VK_KHR_portability_enumeration".into());

        let app_info = br::ApplicationInfo::new(
            c"Peridot Marble Editor",
            br::Version::new(0, 0, 0, 1),
            c"InHouse",
            br::Version::new(0, 0, 0, 1),
        )
        .api_version(api_version.clone());
        let inst_info = br::InstanceCreateInfo::new(&app_info, &[], &instance_extensions);
        #[cfg(target_os = "macos")]
        let inst_info = inst_info.flags(br::InstanceCreateFlags::ENUMERATE_PORTABILITY);
        let vk_instance = br::InstanceObject::new(&inst_info).expect("vkInstance create");
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
                tracing::info!(
                    target: "vk::diag::device",
                    name = ?x.extensionName.as_cstr(),
                    version = x.specVersion,
                    "vulkan device extension"
                );
            }
        }

        if let Some(xs) = vk_adapter
            .enumerate_layer_properties_alloc()
            .inspect_err(
                |e| tracing::error!(reason = ?e, "Failed to enumerate vulkan device layers"),
            )
            .ok()
        {
            for x in xs {
                tracing::info!(
                    target: "vk::diag::device",
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
                            target: "vk::diag::device",
                            name = ?y.extensionName.as_cstr(),
                            version = y.specVersion,
                            "vulkan device extension on layer"
                        );
                    }
                }
            }
        }

        let device_extensions = [
            c"VK_KHR_swapchain",
            c"VK_KHR_timeline_semaphore",
            c"VK_KHR_synchronization2",
            c"VK_KHR_create_renderpass2",
        ];
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
                &device_extensions
                    .iter()
                    .map(|&x| x.into())
                    .collect::<Vec<_>>(),
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

        let graphics_queue = vk_device.queue(graphics_queue_family_index, 0).unmanage().0;

        Self {
            fp_create_render_pass2: unsafe {
                vk_device.native_ptr().load_function_unconstrainted()
            },
            fp_cmd_begin_render_pass2: unsafe {
                vk_device.native_ptr().load_function_unconstrainted()
            },
            fp_cmd_end_render_pass2: unsafe {
                vk_device.native_ptr().load_function_unconstrainted()
            },
            fp_debug_utils_set_object_name: unsafe {
                vk_device.native_ptr().load_function_unconstrainted()
            },
            fp_cmd_pipeline_barrier2: unsafe {
                vk_device.native_ptr().load_function_unconstrainted()
            },
            memory_properties: vk_adapter_memory_properties,
            graphics_queue_family_index,
            graphics_queue,
            native: vk_device.unmanage().0,
            adapter: vk_adapter.unmanage().0,
            parent: vk_instance,
        }
    }

    #[inline(always)]
    pub const fn primary_adapter_ref<'s>(&'s self) -> VulkanDeviceAdapterRef<'s> {
        VulkanDeviceAdapterRef(self.adapter, self)
    }

    #[inline(always)]
    pub const fn present_queue_family_index(&self) -> u32 {
        self.graphics_queue_family_index
    }

    pub fn create_render_pass(
        &self,
        info: &br::RenderPassCreateInfo2,
    ) -> br::Result<br::RenderPassObject<&Self>> {
        let mut h = core::mem::MaybeUninit::uninit();
        unsafe {
            (self.fp_create_render_pass2.0)(
                self.native,
                info as *const _ as _,
                core::ptr::null(),
                h.as_mut_ptr(),
            )
            .into_result()?;
        }

        Ok(unsafe { br::RenderPassObject::manage(h.assume_init(), self) })
    }

    #[tracing::instrument(skip(self), fields(path = ?path.as_ref()))]
    pub fn require_shader(&self, path: impl AsRef<Path>) -> br::ShaderModuleObject<&Self> {
        let bin = std::fs::read(path)
            .inspect_err(|e| tracing::error!(reason = ?e, "missing required shader"))
            .expect("require_shader");
        let mut aligned_bin = Vec::with_capacity(bin.len() >> 2);
        unsafe {
            core::ptr::copy_nonoverlapping(
                bin.as_ptr(),
                aligned_bin.spare_capacity_mut().as_mut_ptr().cast(),
                bin.len(),
            );
            aligned_bin.set_len(bin.len() >> 2);
        }

        br::ShaderModuleObject::new(self, &br::ShaderModuleCreateInfo::new(&aligned_bin))
            .inspect_err(|e| tracing::error!(reason = ?e, "shader instantiation failed"))
            .expect("require_shader")
    }

    pub fn create_graphics_pipelines_array<const N: usize>(
        &self,
        infos: &[br::GraphicsPipelineCreateInfo; N],
    ) -> br::Result<[br::PipelineObject<&Self>; N]> {
        self.new_graphics_pipeline_array(infos, None::<&br::PipelineCacheObject<&Self>>)
    }

    pub fn create_graphics_pipelines(
        &self,
        infos: &[br::GraphicsPipelineCreateInfo],
    ) -> br::Result<Vec<br::PipelineObject<&Self>>> {
        self.new_graphics_pipelines(infos, None::<&br::PipelineCacheObject<&Self>>)
    }

    pub fn find_lazily_allocatable_device_local_memory_index(
        &self,
        type_index_mask: u32,
    ) -> Option<u32> {
        self.memory_properties
            .find_type_index(
                br::MemoryPropertyFlags::DEVICE_LOCAL | br::MemoryPropertyFlags::LAZILY_ALLOCATED,
                br::MemoryPropertyFlags::EMPTY,
                type_index_mask,
            )
            .or_else(|| {
                self.memory_properties.find_type_index(
                    br::MemoryPropertyFlags::DEVICE_LOCAL,
                    br::MemoryPropertyFlags::EMPTY,
                    type_index_mask,
                )
            })
    }

    pub fn find_device_local_memory_index(&self, type_index_mask: u32) -> Option<u32> {
        self.memory_properties
            .find_device_local_index(type_index_mask)
    }

    pub fn find_host_visible_memory_index(&self, type_index_mask: u32) -> Option<u32> {
        self.memory_properties
            .find_host_visible_index(type_index_mask)
    }

    pub fn find_direct_memory_index(&self, type_index_mask: u32) -> Option<u32> {
        self.memory_properties.find_type_index(
            br::MemoryPropertyFlags::DEVICE_LOCAL | br::MemoryPropertyFlags::HOST_VISIBLE,
            br::MemoryPropertyFlags::EMPTY,
            type_index_mask,
        )
    }

    #[inline(always)]
    pub fn is_coherent_memory(&self, index: u32) -> bool {
        self.memory_properties.is_coherent(index)
    }

    #[tracing::instrument(skip(self), fields(memory_type_index))]
    pub fn alloc_device_local_memory(
        &self,
        size: br::DeviceSize,
        memory_type_index_mask: u32,
    ) -> br::DeviceMemoryObject<&Self> {
        let Some(memindex) = self.find_device_local_memory_index(memory_type_index_mask) else {
            tracing::error!("no suitable memory");
            std::process::exit(1);
        };
        tracing::Span::current().record("memory_type_index", memindex);

        match br::DeviceMemoryObject::new(self, &br::MemoryAllocateInfo::new(size, memindex)) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to allocate device local memory");
                std::process::exit(1);
            }
        }
    }

    #[inline(always)]
    pub fn alloc_device_local_memory_for_requirements(
        &self,
        req: &br::vk::VkMemoryRequirements,
    ) -> br::DeviceMemoryObject<&Self> {
        self.alloc_device_local_memory(req.size, req.memoryTypeBits)
    }

    #[tracing::instrument(skip(self, infos, fence), err(Display))]
    pub unsafe fn bind_sparse_raw(
        &self,
        infos: &[br::vk::VkBindSparseInfo],
        fence: Option<br::VkHandleRefMut<br::vk::VkFence>>,
    ) -> br::Result<()> {
        unsafe {
            br::vkfn_wrapper::queue_bind_sparse(
                br::VkHandleRefMut::dangling(self.graphics_queue),
                infos,
                fence,
            )
        }
    }

    pub fn cmd_pipeline_barrier<'r>(
        &self,
        mut cmd: br::CmdRecord<'r>,
        dep: &br::DependencyInfo,
    ) -> br::CmdRecord<'r> {
        unsafe {
            (self.fp_cmd_pipeline_barrier2.0)(
                cmd.raw_command_buffer_handle_mut().native_ptr(),
                dep as *const _ as _,
            );
        }

        cmd
    }

    pub fn cmd_begin_render_pass<'r>(
        &self,
        mut cmd: br::CmdRecord<'r>,
        info: &br::RenderPassBeginInfo,
    ) -> br::CmdRecord<'r> {
        unsafe {
            (self.fp_cmd_begin_render_pass2.0)(
                cmd.raw_command_buffer_handle_mut().native_ptr(),
                info.as_ref(),
                &br::SubpassBeginInfo::new(br::SubpassContents::Inline),
            );
        }

        cmd
    }

    pub fn cmd_end_render_pass<'r>(&self, mut cmd: br::CmdRecord<'r>) -> br::CmdRecord<'r> {
        unsafe {
            (self.fp_cmd_end_render_pass2.0)(
                cmd.raw_command_buffer_handle_mut().native_ptr(),
                &br::SubpassEndInfo::new(),
            );
        }

        cmd
    }

    pub fn dbg_set_name<H: br::VkObject<Handle: br::VkRawHandle> + ?Sized>(
        &self,
        obj: &H,
        name: &core::ffi::CStr,
    ) {
        let r = unsafe {
            (self.fp_debug_utils_set_object_name.0)(
                self.native,
                core::mem::transmute(&br::DebugUtilsObjectNameInfo::new(obj, Some(name))),
            )
            .into_result()
        };
        if let Err(r) = r {
            tracing::warn!(
                reason = ?r,
                obj.id = obj.native_ptr().raw_handle_value(),
                obj.type = H::TYPE,
                ?name,
                "Failed to set vulkan object name(for debugging)"
            );
        }
    }
}

pub struct VulkanDeviceAdapterRef<'d>(br::vk::VkPhysicalDevice, &'d VulkanDevice);
unsafe impl Sync for VulkanDeviceAdapterRef<'_> {}
unsafe impl Send for VulkanDeviceAdapterRef<'_> {}
impl br::VkHandle for VulkanDeviceAdapterRef<'_> {
    type Handle = br::vk::VkPhysicalDevice;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.0
    }
}
impl br::InstanceChild for VulkanDeviceAdapterRef<'_> {
    type ConcreteInstance = <VulkanDevice as br::InstanceChild>::ConcreteInstance;

    #[inline(always)]
    fn instance(&self) -> &Self::ConcreteInstance {
        self.1.instance()
    }
}
impl br::PhysicalDevice for VulkanDeviceAdapterRef<'_> {}
