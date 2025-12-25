use bedrock as br;

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
    fp_create_render_pass2: br::vk::PFN_vkCreateRenderPass2KHR,
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
impl VulkanDevice {
    pub fn new(
        native: br::vk::VkDevice,
        api_version: br::Version,
        device_extensions: &[&core::ffi::CStr],
    ) -> Self {
        Self {
            fp_create_render_pass2: unsafe { br::vk::PFN_vkCreateRenderPass2KHR::get(native) },
            fp_debug_utils_set_object_name: unsafe {
                br::vk::PFN_vkSetDebugUtilsObjectNameEXT::get(native)
            },
            native,
        }
    }

    pub fn create_render_pass(
        &self,
        info: &br::RenderPassCreateInfo2,
    ) -> br::Result<br::RenderPassObject<&Self>> {
        let mut h = core::mem::MaybeUninit::uninit();
        unsafe {
            (self.fp_create_render_pass2.0)(self.native, &info, core::ptr::null(), h.as_mut_ptr())
                .into_result()?;
        }

        Ok(unsafe { br::RenderPassObject::manage(h.assume_init(), self) })
    }

    pub fn dbg_set_name<H: br::VkObject + ?Sized>(&self, obj: &H, name: &core::ffi::CStr) {
        let r = unsafe {
            (self.fp_debug_utils_set_object_name.0)(
                self.native,
                &br::DebugUtilsObjectNameInfo::new(obj, Some(name)),
            )
            .into_result()
        };
        if let Err(r) = r {
            tracing::warn!(
                reason = ?r,
                obj.id = obj.native_ptr(),
                obj.type = H::TYPE,
                ?name,
                "Failed to set vulkan object name(for debugging)"
            );
        }
    }
}
