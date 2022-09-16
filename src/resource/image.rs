//! Image Resource Helper

use bedrock as br;
use br::MemoryBound;

#[allow(unused_imports)]
use crate::mthelper::DynamicMutabilityProvider;
use crate::mthelper::SharedRef;

/// A refcounted image object bound with a memory object.
#[derive(Clone)]
pub struct Image<Backend: br::Image, DeviceMemory: br::DeviceMemory>(
    SharedRef<Backend>,
    DeviceMemory,
    u64,
);
impl<Backend: br::Image, DeviceMemory: br::DeviceMemory> Image<Backend, DeviceMemory> {
    pub fn bound(mut r: Backend, mem: DeviceMemory, offset: u64) -> br::Result<Self> {
        r.bind(&mem.borrow(), offset as _)
            .map(move |_| Self(r.into(), mem, offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &DeviceMemory {
        &self.1
    }

    pub fn format(&self) -> super::PixelFormat {
        unsafe { std::mem::transmute(self.0.format()) }
    }
}
impl<Backend: br::Image, DeviceMemory: br::DeviceMemory> std::ops::Deref
    for Image<Backend, DeviceMemory>
{
    type Target = Backend;

    fn deref(&self) -> &Backend {
        &self.0
    }
}
impl<Backend: br::Image, DeviceMemory: br::DeviceMemory> br::VkHandle
    for Image<Backend, DeviceMemory>
{
    type Handle = <Backend as br::VkHandle>::Handle;

    fn native_ptr(&self) -> Self::Handle {
        self.0.native_ptr()
    }
}
impl<Backend: br::Image, Memory: br::DeviceMemory> br::DeviceChild for Image<Backend, Memory> {
    type ConcreteDevice = Backend::ConcreteDevice;

    fn device(&self) -> &Self::ConcreteDevice {
        self.0.device()
    }
}
impl<Backend: br::Image, DeviceMemory: br::DeviceMemory> br::Image
    for Image<Backend, DeviceMemory>
{
    fn format(&self) -> br::vk::VkFormat {
        self.0.format()
    }

    fn size(&self) -> &br::vk::VkExtent3D {
        self.0.size()
    }

    fn dimension(&self) -> br::vk::VkImageViewType {
        self.0.dimension()
    }
}
