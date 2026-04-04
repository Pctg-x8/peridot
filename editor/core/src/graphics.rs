use std::path::{Path, PathBuf};

use bedrock::{
    self as br, Device, Instance, InstanceChild, PhysicalDevice, ResolverInterface, Swapchain,
    VkHandle, VkRawHandle, VulkanStructure,
};

use crate::{
    FileSystem,
    utils::{PixelsUnit, Size},
};

pub const VI_STATE_EMPTY: &br::PipelineVertexInputStateCreateInfo =
    &br::PipelineVertexInputStateCreateInfo::new(&[], &[]);

pub const IA_STATE_TRILIST: &br::PipelineInputAssemblyStateCreateInfo =
    &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleList);
pub const IA_STATE_TRISTRIP: &br::PipelineInputAssemblyStateCreateInfo =
    &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleStrip);

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

pub struct VulkanDevice<'fs> {
    fs: &'fs FileSystem,
    native: br::vk::VkDevice,
    adapter: br::vk::VkPhysicalDevice,
    parent: br::InstanceObject,
    memory_properties: br::MemoryProperties,
    graphics_queue: br::vk::VkQueue,
    graphics_queue_family_index: u32,
    pipeline_cache_path: PathBuf,
    pipeline_cache: br::vk::VkPipelineCache,
    fp_cmd_pipeline_barrier2: br::vk::PFN_vkCmdPipelineBarrier2KHR,
    fp_create_render_pass2: br::vk::PFN_vkCreateRenderPass2KHR,
    fp_cmd_begin_render_pass2: br::vk::PFN_vkCmdBeginRenderPass2KHR,
    fp_cmd_end_render_pass2: br::vk::PFN_vkCmdEndRenderPass2KHR,
    fp_debug_utils_set_object_name: br::vk::PFN_vkSetDebugUtilsObjectNameEXT,
    #[cfg(windows)]
    fp_get_memory_win32_handle_properties: br::vk::PFN_vkGetMemoryWin32HandlePropertiesKHR,
}
unsafe impl Sync for VulkanDevice<'_> {}
unsafe impl Send for VulkanDevice<'_> {}
impl Drop for VulkanDevice<'_> {
    fn drop(&mut self) {
        // writeback pipeline cache for next launch
        self.writeback_pipeline_cache();

        unsafe {
            br::vkfn::destroy_pipeline_cache(self.native, self.pipeline_cache, core::ptr::null());
            br::vkfn::destroy_device(self.native, core::ptr::null());
        }
    }
}
impl br::VkHandle for VulkanDevice<'_> {
    type Handle = br::vk::VkDevice;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.native
    }
}
impl br::InstanceChild for VulkanDevice<'_> {
    type ConcreteInstance = br::InstanceObject;

    fn instance(&self) -> &Self::ConcreteInstance {
        &self.parent
    }
}
impl br::Device for VulkanDevice<'_> {}
#[cfg(windows)]
impl br::DeviceExternalMemoryWin32Extension for VulkanDevice<'_> {
    #[inline(always)]
    fn get_memory_win32_handle_khr_fn(&self) -> br::vk::PFN_vkGetMemoryWin32HandleKHR {
        unimplemented!("not planned to use")
    }

    #[inline(always)]
    fn get_memory_win32_handle_properties_khr_fn(
        &self,
    ) -> br::vk::PFN_vkGetMemoryWin32HandlePropertiesKHR {
        self.fp_get_memory_win32_handle_properties
    }
}
impl<'fs> VulkanDevice<'fs> {
    pub fn new(fs: &'fs FileSystem) -> Self {
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

        for x in br::instance_extension_properties_cstr_alloc(None).unwrap_or_else(|e| {
            tracing::error!(reason = ?e, "Failed to enumerate vulkan instance extensions");
            Vec::new()
        }) {
            tracing::info!(
                target: "vk::diag::instance",
                name = ?x.extensionName.as_cstr(),
                version = x.specVersion,
                "vulkan instance extension"
            );
        }

        for x in br::enumerate_layer_properties_alloc().unwrap_or_else(|e| {
            tracing::error!(reason = ?e, "Failed to enumerate vulkan instance layers");
            Vec::new()
        }) {
            tracing::info!(
                target: "vk::diag::instance",
                name = ?x.layerName.as_cstr(),
                version.impl = x.implementationVersion,
                version.spec = %br::Version::from_raw(x.specVersion),
                "vulkan instance layer"
            );

            if let Some(ln) = x.layerName.as_cstr().ok() {
                for y in
                    br::instance_extension_properties_cstr_alloc(Some(ln)).unwrap_or_else(|e| {
                        tracing::error!(
                            reason = ?e,
                            "Failed to enumerate vulkan instance extensions for layer"
                        );
                        Vec::new()
                    })
                {
                    tracing::info!(
                        target: "vk::diag::instance",
                        name = ?y.extensionName.as_cstr(),
                        version = y.specVersion,
                        "vulkan instance extension on layer"
                    );
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

        #[cfg(windows)]
        extern "system" fn cb_win(
            severity: br::vk::VkDebugUtilsMessageSeverityFlagBitsEXT,
            r#type: br::vk::VkDebugUtilsMessageTypeFlagsEXT,
            data: *const br::vk::VkDebugUtilsMessengerCallbackDataEXT,
            user_data: *mut core::ffi::c_void,
        ) -> br::vk::VkBool32 {
            let sev_str = match severity {
                v if v == br::vk::VK_DEBUG_UTILS_MESSAGE_SEVERITY_VERBOSE_BIT_EXT => {
                    "VERBOSE".into()
                }
                v if v == br::vk::VK_DEBUG_UTILS_MESSAGE_SEVERITY_INFO_BIT_EXT => "INFO".into(),
                v if v == br::vk::VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT => {
                    "WARNING".into()
                }
                v if v == br::vk::VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT => "ERROR".into(),
                _ => format!("{severity}"),
            };
            unsafe {
                windows::Win32::System::Diagnostics::Debug::OutputDebugStringA(
                    windows_core::PCSTR(
                        std::ffi::CString::new(format!(
                            "VK DIAG[{sev_str}]: {}\n",
                            core::ffi::CStr::from_ptr((*data).pMessage)
                                .to_str()
                                .expect("invalid msg")
                        ))
                        .expect("invalid fmt")
                        .as_ptr()
                        .cast(),
                    ),
                );
            }
            false as _
        }
        #[cfg(windows)]
        let eh = br::DebugUtilsMessengerObject::new(
            &vk_instance,
            &br::DebugUtilsMessengerCreateInfo::new(
                br::DebugUtilsMessageSeverityFlags::ERROR
                    | br::DebugUtilsMessageSeverityFlags::WARNING
                    | br::DebugUtilsMessageSeverityFlags::INFO,
                br::DebugUtilsMessageTypeFlags::GENERAL
                    | br::DebugUtilsMessageTypeFlags::PERFORMANCE
                    | br::DebugUtilsMessageTypeFlags::VALIDATION,
                cb_win,
            ),
        )
        .expect("cb")
        .unmanage();

        for x in vk_adapter
            .enumerate_extension_properties_cstr_alloc(None)
            .unwrap_or_else(|e| {
                tracing::error!(reason = ?e, "Failed to enumerate vulkan device extensions");
                Vec::new()
            })
        {
            tracing::info!(
                target: "vk::diag::device",
                name = ?x.extensionName.as_cstr(),
                version = x.specVersion,
                "vulkan device extension"
            );
        }

        for x in vk_adapter
            .enumerate_layer_properties_alloc()
            .unwrap_or_else(|e| {
                tracing::error!(reason = ?e, "Failed to enumerate vulkan device layers");
                Vec::new()
            })
        {
            tracing::info!(
                target: "vk::diag::device",
                name = ?x.layerName.as_cstr(),
                version.impl = x.implementationVersion,
                version.spec = %br::Version::from_raw(x.specVersion),
                "vulkan device layer"
            );

            if let Some(ln) = x.layerName.as_cstr().ok() {
                for y in vk_adapter
                    .enumerate_extension_properties_cstr_alloc(Some(ln))
                    .unwrap_or_else(|e| {
                        tracing::error!(
                            reason = ?e,
                            "Failed to enumerate vulkan instance extensions for layer"
                        );
                        Vec::new()
                    })
                {
                    tracing::info!(
                        target: "vk::diag::device",
                        name = ?y.extensionName.as_cstr(),
                        version = y.specVersion,
                        "vulkan device extension on layer"
                    );
                }
            }
        }

        let mut device_extensions = vec![
            c"VK_KHR_swapchain",
            c"VK_KHR_timeline_semaphore",
            c"VK_KHR_synchronization2",
            c"VK_KHR_create_renderpass2",
        ];
        #[cfg(windows)]
        device_extensions.push(c"VK_KHR_external_memory_win32");
        let vk_adapter_memory_properties = vk_adapter.memory_properties();
        let vk_adapter_queue_family_properties = vk_adapter.queue_family_properties_alloc();
        let graphics_queue_family_index = vk_adapter_queue_family_properties
            .find_matching_index(br::QueueFlags::GRAPHICS)
            .expect("no graphics queue");

        let mut device_features = br::PhysicalDeviceFeatures2::new(Default::default());
        let mut device_sync2_features = br::PhysicalDeviceSynchronization2Features::new(true);
        let mut device_timeline_semaphore_features =
            br::PhysicalDeviceTimelineSemaphoreFeatures::new(true);
        let mut vk11_features = (api_version >= br::Version::new(0, 1, 2, 0)).then(|| {
            br::PhysicalDeviceVulkan11Features {
                shaderDrawParameters: true as _,
                ..Default::default()
            }
        });
        br::chain_structures(
            [
                Some(device_features.as_generic_mut()),
                vk11_features.as_mut().map(|x| x.as_generic_mut()),
                Some(device_sync2_features.as_generic_mut()),
                Some(device_timeline_semaphore_features.as_generic_mut()),
            ]
            .into_iter()
            .flatten(),
        );
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
            .with_next(&device_features),
        )
        .expect("vk_device create");

        let graphics_queue = vk_device.queue(graphics_queue_family_index, 0).unmanage().0;

        let pipeline_cache_path = fs.resolve_cache_path("vk-pipeline-cache");
        let pipeline_cache = 'try_load_pipeline_cache_binary: {
            let exists = pipeline_cache_path.try_exists()
                .inspect_err(|e| tracing::warn!(path = ?pipeline_cache_path, reason = %e, "pipeline_cache.try_exists"))
                .unwrap_or(false);
            if !exists {
                // not found
                tracing::info!(path = ?pipeline_cache_path, "pipeline_cache.not_found");
                break 'try_load_pipeline_cache_binary None;
            }

            let content = std::fs::read(&pipeline_cache_path)
                .inspect_err(|e| tracing::warn!(path = ?pipeline_cache_path, reason = %e, "pipeline_cache.read"))
                .ok();
            let Some(content) = content else {
                // failed to read file
                break 'try_load_pipeline_cache_binary None;
            };

            br::PipelineCacheObject::new(&vk_device, &br::PipelineCacheCreateInfo::new(&content))
                .inspect_err(|e| tracing::warn!(path = ?pipeline_cache_path, reason = %e, "pipeline_cache.new"))
                .ok()
        }.unwrap_or_else(|| br::PipelineCacheObject::new(&vk_device, &br::PipelineCacheCreateInfo::new(&[])).expect("pipeline_cache.new_empty"));

        Self {
            fs,
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
            #[cfg(windows)]
            fp_get_memory_win32_handle_properties: unsafe {
                vk_device.native_ptr().load_function_unconstrainted()
            },
            pipeline_cache: pipeline_cache.unmanage().0,
            pipeline_cache_path,
            memory_properties: vk_adapter_memory_properties,
            graphics_queue_family_index,
            graphics_queue,
            native: vk_device.unmanage().0,
            adapter: vk_adapter.unmanage().0,
            parent: vk_instance,
        }
    }

    #[inline(always)]
    pub const fn primary_adapter_ref<'s>(&'s self) -> VulkanDeviceAdapterRef<'s, 'fs> {
        VulkanDeviceAdapterRef(self.adapter, self)
    }

    #[inline(always)]
    pub const fn present_queue_family_index(&self) -> u32 {
        self.graphics_queue_family_index
    }

    #[cfg(feature = "wayland")]
    pub fn presentation_support(&self, dp: &peridot_tp_wayland::Display) -> bool {
        unsafe {
            self.primary_adapter_ref()
                .wayland_presentation_support(self.present_queue_family_index(), dp.as_raw().cast())
        }
    }

    #[cfg(windows)]
    #[inline]
    pub fn presentation_support(&self) -> bool {
        self.primary_adapter_ref()
            .win32_presentation_support(self.present_queue_family_index())
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

    #[tracing::instrument(skip(self), fields(path = ?path.as_ref(), resolved_path))]
    pub fn require_shader(&self, path: impl AsRef<Path>) -> br::ShaderModuleObject<&Self> {
        let resolved_path = self.fs.resolve_resource_path(&path);
        tracing::Span::current().record("resolved_path", tracing::field::debug(&resolved_path));
        let bin = std::fs::read(resolved_path)
            .inspect_err(|e| tracing::error!(reason = %e, "require_shader.read"))
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

        let o = br::ShaderModuleObject::new(self, &br::ShaderModuleCreateInfo::new(&aligned_bin))
            .inspect_err(|e| tracing::error!(reason = %e, "require_shader.instantiate"))
            .expect("require_shader");
        if let Some(n) = std::ffi::CString::new(path.as_ref().to_string_lossy().into_owned())
            .inspect_err(|e| tracing::warn!(reason = %e, "require_shader.generate_name"))
            .ok()
        {
            self.dbg_set_name(&o, &n);
        }
        o
    }

    pub fn create_graphics_pipelines_array<const N: usize>(
        &self,
        infos: &[br::GraphicsPipelineCreateInfo; N],
    ) -> br::Result<[br::PipelineObject<&Self>; N]> {
        self.new_graphics_pipeline_array(
            infos,
            Some(br::VkHandleRef::from_raw_ref(&self.pipeline_cache)),
        )
    }

    pub fn create_graphics_pipelines(
        &self,
        infos: &[br::GraphicsPipelineCreateInfo],
    ) -> br::Result<Vec<br::PipelineObject<&Self>>> {
        self.new_graphics_pipelines(
            infos,
            Some(br::VkHandleRef::from_raw_ref(&self.pipeline_cache)),
        )
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

    #[tracing::instrument(skip(self))]
    fn writeback_pipeline_cache(&self) {
        let data_length = unsafe {
            br::vkfn_wrapper::get_pipeline_cache_data_byte_length(self.native, self.pipeline_cache)
                .inspect_err(|e| tracing::warn!(reason = %e, "pipeline_cache.get_data_byte_length"))
                .ok()
        };
        let Some(data_length) = data_length else {
            return;
        };

        let mut data = Vec::with_capacity(data_length);
        if let Err(e) = unsafe {
            br::vkfn_wrapper::get_pipeline_cache_data(
                self.native,
                self.pipeline_cache,
                data.spare_capacity_mut(),
            )
        } {
            tracing::warn!(reason = %e, "pipeline_cache.get_data");
            return;
        }
        unsafe {
            data.set_len(data_length);
        }

        if let Err(e) = std::fs::write(&self.pipeline_cache_path, &data) {
            tracing::warn!(path = ?self.pipeline_cache_path, reason = %e, "pipeline_cache.write_file");
        }
    }
}

pub struct VulkanDeviceAdapterRef<'d, 'fs>(br::vk::VkPhysicalDevice, &'d VulkanDevice<'fs>);
unsafe impl Sync for VulkanDeviceAdapterRef<'_, '_> {}
unsafe impl Send for VulkanDeviceAdapterRef<'_, '_> {}
impl br::VkHandle for VulkanDeviceAdapterRef<'_, '_> {
    type Handle = br::vk::VkPhysicalDevice;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.0
    }
}
impl<'fs> br::InstanceChild for VulkanDeviceAdapterRef<'_, 'fs> {
    type ConcreteInstance = <VulkanDevice<'fs> as br::InstanceChild>::ConcreteInstance;

    #[inline(always)]
    fn instance(&self) -> &Self::ConcreteInstance {
        self.1.instance()
    }
}
impl br::PhysicalDevice for VulkanDeviceAdapterRef<'_, '_> {}

pub struct VulkanSwapchain<'d, 'fs> {
    device: &'d VulkanDevice<'fs>,
    handle: br::vk::VkSwapchainKHR,
    ext: br::Extent2D,
    images: Vec<br::vk::VkImage>,
    image_views: Vec<br::vk::VkImageView>,
}
impl Drop for VulkanSwapchain<'_, '_> {
    fn drop(&mut self) {
        unsafe {
            for x in self.image_views.drain(..) {
                br::vkfn_wrapper::destroy_image_view(self.device.native_ptr(), x, None);
            }
            br::vkfn_wrapper::destroy_swapchain(self.device.native_ptr(), self.handle, None);
        }
    }
}
impl br::VkHandle for VulkanSwapchain<'_, '_> {
    type Handle = br::vk::VkSwapchainKHR;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
impl br::DeviceChildHandle for VulkanSwapchain<'_, '_> {
    #[inline(always)]
    fn device_handle(&self) -> bedrock::vk::VkDevice {
        self.device.native_ptr()
    }
}
impl<'fs> br::DeviceChild for VulkanSwapchain<'_, 'fs> {
    type ConcreteDevice = VulkanDevice<'fs>;

    #[inline(always)]
    fn device(&self) -> &Self::ConcreteDevice {
        self.device
    }
}
impl br::Swapchain for VulkanSwapchain<'_, '_> {}
impl<'d, 'fs> VulkanSwapchain<'d, 'fs> {
    pub fn new(
        surface: &VulkanSurface<'d, 'fs>,
        query_window_extent: impl FnOnce() -> Size<PixelsUnit>,
    ) -> Self {
        let ext = if surface.unbound.caps.currentExtent.width == 0xffffffff
            || surface.unbound.caps.currentExtent.height == 0xffffffff
        {
            let window_ext = query_window_extent();

            br::Extent2D {
                width: if surface.unbound.caps.currentExtent.width == 0xffffffff {
                    window_ext.width
                } else {
                    surface.unbound.caps.currentExtent.width
                },
                height: if surface.unbound.caps.currentExtent.height == 0xffffffff {
                    window_ext.height
                } else {
                    surface.unbound.caps.currentExtent.height
                },
            }
        } else {
            surface.unbound.caps.currentExtent
        };

        tracing::trace!(?ext, "swapchain.create");
        let o = br::SwapchainWithSurfaceBuilder::new(
            surface,
            surface.unbound.caps.minImageCount.max(2),
            surface.unbound.selected_format,
            ext,
            br::ImageUsageFlags::COLOR_ATTACHMENT,
        )
        .present_mode(surface.unbound.selected_present_mode)
        .pre_transform(br::SurfaceTransformFlags::IDENTITY)
        .composite_alpha(surface.unbound.selected_composite_alpha)
        .create(surface.device)
        .expect("swapchain create");
        let image_count = o.image_count().expect("swapchain.get_image_count");
        let mut images = Vec::with_capacity(image_count as _);
        o.images(images.spare_capacity_mut())
            .expect("swapchain.get_images");
        unsafe {
            images.set_len(images.capacity());
        }
        let image_views = images
            .iter()
            .map(|b| unsafe {
                br::vkfn_wrapper::create_image_view(
                    surface.device.native_ptr(),
                    &br::ImageViewCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(b),
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        surface.unbound.selected_format.format,
                    ),
                    None,
                )
                .expect("backbuffer image view create")
            })
            .collect::<Vec<_>>();

        let (device, _, handle) = o.unmanage();
        Self {
            device,
            handle,
            ext,
            images,
            image_views,
        }
    }

    pub fn recreate(
        &mut self,
        surface: &VulkanSurface<'d, 'fs>,
        query_window_extent: impl FnOnce() -> Size<PixelsUnit>,
    ) {
        // release pre-created resources
        for x in self.image_views.drain(..) {
            unsafe {
                br::vkfn_wrapper::destroy_image_view(self.device.native_ptr(), x, None);
            }
        }
        self.images.clear();

        self.ext = if surface.unbound.caps.currentExtent.width == 0xffffffff
            || surface.unbound.caps.currentExtent.height == 0xffffffff
        {
            let window_ext = query_window_extent();

            br::Extent2D {
                width: if surface.unbound.caps.currentExtent.width == 0xffffffff {
                    window_ext.width
                } else {
                    surface.unbound.caps.currentExtent.width
                },
                height: if surface.unbound.caps.currentExtent.height == 0xffffffff {
                    window_ext.height
                } else {
                    surface.unbound.caps.currentExtent.height
                },
            }
        } else {
            surface.unbound.caps.currentExtent
        };

        tracing::trace!(ext = ?self.ext, "swapchain.recreate");
        let o = br::SwapchainWithSurfaceBuilder::new(
            surface,
            surface.unbound.caps.minImageCount.max(2),
            surface.unbound.selected_format,
            self.ext,
            br::ImageUsageFlags::COLOR_ATTACHMENT,
        )
        .present_mode(surface.unbound.selected_present_mode)
        .pre_transform(br::SurfaceTransformFlags::IDENTITY)
        .composite_alpha(surface.unbound.selected_composite_alpha)
        .enable_clip()
        .old_swapchain(br::VkHandleRef::from_raw_ref(&self.handle))
        .create(self.device)
        .expect("swapchain create");
        unsafe {
            br::vkfn_wrapper::destroy_swapchain(self.device.native_ptr(), self.handle, None);
        }
        let image_count = o.image_count().expect("swapchain.recreate.get_image_count");
        let _ = self.images.try_reserve(image_count as _);
        o.images(self.images.spare_capacity_mut())
            .expect("swapchain.recreate.get_images");
        unsafe {
            self.images.set_len(image_count as _);
        }
        self.image_views.extend(self.images.iter().map(|b| unsafe {
            br::vkfn_wrapper::create_image_view(
                self.device.native_ptr(),
                &br::ImageViewCreateInfo::new(
                    br::VkHandleRef::from_raw_ref(b),
                    br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                    surface.unbound.selected_format.format,
                ),
                None,
            )
            .expect("backbuffer image view create")
        }));

        let (_, _, handle) = o.unmanage();
        self.handle = handle;
    }

    #[inline(always)]
    pub const fn size(&self) -> br::Extent2D {
        self.ext
    }

    #[inline(always)]
    pub fn image_count(&self) -> usize {
        self.images.len()
    }

    #[inline(always)]
    pub fn image_ref<'a>(&'a self, index: usize) -> br::VkHandleRef<'a, br::vk::VkImage> {
        unsafe { br::VkHandleRef::dangling(self.images[index]) }
    }

    #[inline(always)]
    pub fn image_view_refs<'a>(
        &'a self,
    ) -> impl Iterator<Item = br::VkHandleRef<'a, br::vk::VkImageView>> + use<'a> {
        self.image_views
            .iter()
            .map(|&x| unsafe { br::VkHandleRef::dangling(x) })
    }
}

pub struct UnboundVulkanSurface {
    pub handle: br::vk::VkSurfaceKHR,
    pub selected_format: br::SurfaceFormat,
    pub selected_present_mode: br::PresentMode,
    pub selected_composite_alpha: br::CompositeAlphaFlags,
    pub caps: br::SurfaceCapabilities,
}
impl UnboundVulkanSurface {
    pub unsafe fn drop(
        &mut self,
        instance: &(impl VkHandle<Handle = br::vk::VkInstance> + ?Sized),
    ) {
        unsafe {
            br::vkfn_wrapper::destroy_surface(instance.native_ptr(), self.handle, None);
        }
    }

    pub const unsafe fn bound<'d, 'fs>(
        self,
        device: &'d VulkanDevice<'fs>,
    ) -> VulkanSurface<'d, 'fs> {
        VulkanSurface {
            device,
            unbound: self,
        }
    }

    pub unsafe fn refresh_caps(
        &mut self,
        adapter: &(impl VkHandle<Handle = br::vk::VkPhysicalDevice> + ?Sized),
    ) {
        let mut sink = core::mem::MaybeUninit::uninit();
        unsafe {
            br::vkfn_wrapper::get_physical_device_surface_capabilities(
                adapter.native_ptr(),
                self.handle,
                &mut sink,
            )
            .expect("vk.surface.refresh_caps");
        }
        self.caps = unsafe { sink.assume_init() };
        self.selected_composite_alpha = if self
            .caps
            .supported_composite_alpha()
            .has_all(br::CompositeAlphaFlags::PRE_MULTIPLIED)
        {
            br::CompositeAlphaFlags::PRE_MULTIPLIED
        } else {
            br::CompositeAlphaFlags::INHERIT
        };
    }

    #[inline(always)]
    pub const fn format(&self) -> br::Format {
        self.selected_format.format
    }
}

pub struct VulkanSurface<'d, 'fs> {
    device: &'d VulkanDevice<'fs>,
    unbound: UnboundVulkanSurface,
}
impl Drop for VulkanSurface<'_, '_> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            self.unbound.drop(self.device.instance());
        }
    }
}
impl br::VkHandle for VulkanSurface<'_, '_> {
    type Handle = br::vk::VkSurfaceKHR;

    #[inline(always)]
    fn native_ptr(&self) -> Self::Handle {
        self.unbound.handle
    }
}
unsafe impl Sync for VulkanSurface<'_, '_> {}
unsafe impl Send for VulkanSurface<'_, '_> {}
impl<'fs> br::InstanceChild for VulkanSurface<'_, 'fs> {
    type ConcreteInstance = <VulkanDevice<'fs> as br::InstanceChild>::ConcreteInstance;

    #[inline(always)]
    fn instance(&self) -> &Self::ConcreteInstance {
        self.device.instance()
    }
}
impl br::Surface for VulkanSurface<'_, '_> {}
impl<'d, 'fs> VulkanSurface<'d, 'fs> {
    pub fn new(device: &'d VulkanDevice<'fs>, handle: br::vk::VkSurfaceKHR) -> Self {
        match unsafe {
            br::vkfn_wrapper::get_physical_device_surface_support(
                device.primary_adapter_ref().native_ptr(),
                device.present_queue_family_index(),
                handle,
            )
        } {
            Ok(true) => (),
            Ok(false) => {
                panic!("surface not supported on graphics queue");
            }
            Err(e) => Err(e).expect("surface_support"),
        };

        let present_mode_count = unsafe {
            br::vkfn_wrapper::get_physical_device_surface_present_mode_count(
                device.primary_adapter_ref().native_ptr(),
                handle,
            )
            .expect("vk.surface.get_present_mode_count")
        };
        let mut present_modes = Vec::with_capacity(present_mode_count as _);
        unsafe {
            br::vkfn_wrapper::get_physical_device_surface_present_modes(
                device.primary_adapter_ref().native_ptr(),
                handle,
                present_modes.spare_capacity_mut(),
            )
            .expect("vk.surface.get_present_modes")
        };
        unsafe {
            present_modes.set_len(present_modes.capacity());
        }

        let format_count = unsafe {
            br::vkfn_wrapper::get_physical_device_surface_format_count(
                device.primary_adapter_ref().native_ptr(),
                handle,
            )
            .expect("vk.surface.get_format_count")
        };
        let mut formats = Vec::with_capacity(format_count as _);
        unsafe {
            br::vkfn_wrapper::get_physical_device_surface_formats(
                device.primary_adapter_ref().native_ptr(),
                handle,
                formats.spare_capacity_mut(),
            )
            .expect("vk.surface.get_formats");
        }
        unsafe {
            formats.set_len(formats.capacity());
        }

        let mut caps = core::mem::MaybeUninit::uninit();
        unsafe {
            br::vkfn_wrapper::get_physical_device_surface_capabilities(
                device.primary_adapter_ref().native_ptr(),
                handle,
                &mut caps,
            )
            .expect("vk.surface.get_capabilities");
        }
        let caps = unsafe { caps.assume_init() };

        Self {
            device,
            unbound: UnboundVulkanSurface {
                handle,
                selected_format: formats
                    .iter()
                    .find(|f| {
                        f.colorSpace == br::vk::VK_COLOR_SPACE_SRGB_NONLINEAR_KHR
                            && (f.format == br::vk::VK_FORMAT_B8G8R8A8_SRGB
                                || f.format == br::vk::VK_FORMAT_R8G8B8A8_SRGB)
                    })
                    .copied()
                    .expect("no suitable surface format"),
                selected_present_mode: present_modes
                    .iter()
                    .find(|&&x| x == br::PresentMode::FIFO)
                    .copied()
                    .expect("no suitable present mode"),
                selected_composite_alpha: if caps
                    .supported_composite_alpha()
                    .has_all(br::CompositeAlphaFlags::PRE_MULTIPLIED)
                {
                    br::CompositeAlphaFlags::PRE_MULTIPLIED
                } else {
                    br::CompositeAlphaFlags::INHERIT
                },
                caps,
            },
        }
    }

    pub const fn unbound(self) -> (&'d VulkanDevice<'fs>, UnboundVulkanSurface) {
        let device = unsafe { core::ptr::read(&self.device) };
        let unbound = unsafe { core::ptr::read(&self.unbound) };
        core::mem::forget(self);

        (device, unbound)
    }

    #[inline(always)]
    pub fn refresh_caps(&mut self) {
        unsafe {
            self.unbound
                .refresh_caps(&self.device.primary_adapter_ref());
        }
    }

    #[inline(always)]
    pub const fn format(&self) -> br::Format {
        self.unbound.format()
    }
}
