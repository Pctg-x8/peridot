//! Image Resource Helper

use bedrock::{self as br};

use crate::graphics::VulkanGfx;
#[allow(unused_imports)]
use crate::mthelper::DynamicMutabilityProvider;

use super::SharedMemoryBlock;

/// An image object that unbounded with any memory objects.
pub struct UnboundedStandaloneImage {
    pub(crate) handle: br::vk::VkImage,
    pub(crate) format: br::Format,
    pub(crate) size: br::Extent3D,
    pub(crate) image_type: br::vk::VkImageViewType,
    pub(crate) gfx_device: VulkanGfx,
}
impl Drop for UnboundedStandaloneImage {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image(
                self.gfx_device.native_device_ref(),
                br::VkHandleRefMut::dangling(self.handle),
                None,
            );
        }
    }
}
impl br::VkHandle for UnboundedStandaloneImage {
    type Handle = br::vk::VkImage;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

/// An image object bound with a memory object.
pub struct Image {
    handle: br::vk::VkImage,
    format: br::Format,
    size: br::Extent3D,
    image_type: br::vk::VkImageViewType,
    memory: SharedMemoryBlock,
    memory_offset: u64,
}
impl Drop for Image {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image(
                self.memory.lock_shared().device.native_device_ref(),
                br::VkHandleRefMut::dangling(self.handle),
                None,
            );
        }
    }
}
impl Image {
    pub fn bound(
        r: UnboundedStandaloneImage,
        mem: &SharedMemoryBlock,
        offset: u64,
    ) -> br::Result<Self> {
        let UnboundedStandaloneImage {
            handle,
            format,
            size,
            image_type,
            ..
        } = r;
        unsafe {
            br::vkfn_wrapper::bind_image_memory(
                mem.lock_shared().device.native_device_ref(),
                br::VkHandleRefMut::dangling(handle),
                br::VkHandleRef::dangling(mem.lock_shared().handle),
                offset as _,
            )?;
        }

        Ok(Self {
            handle,
            format,
            size,
            image_type,
            memory: mem.clone(),
            memory_offset: offset,
        })
    }

    pub const fn format(&self) -> super::PixelFormat {
        unsafe { core::mem::transmute(self.format) }
    }

    pub const fn offset_on_memory(&self) -> u64 {
        self.memory_offset
    }
}
impl Image {
    /// Reference to a memory object bound with this object.
    #[inline]
    pub const fn memory(&self) -> &SharedMemoryBlock {
        &self.memory
    }
}
impl br::VkHandle for Image {
    type Handle = br::vk::VkImage;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
impl br::DeviceChildHandle for Image {
    fn device_handle(&self) -> bedrock::vk::VkDevice {
        self.memory.lock_shared().device.0.device
    }
}
impl br::Image for Image {
    fn format(&self) -> br::Format {
        self.format
    }

    fn size(&self) -> &br::Extent3D {
        &self.size
    }

    fn dimension(&self) -> br::vk::VkImageViewType {
        self.image_type
    }
}
