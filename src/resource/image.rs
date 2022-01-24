//! Image Resource Helper

use super::memory::memory_borrow;

use bedrock as br;
use br::MemoryBound;

#[cfg(not(feature = "mt"))]
use std::rc::Rc as SharedPtr;
#[cfg(feature = "mt")]
use std::sync::Arc as SharedPtr;

/// A refcounted image object bound with a memory object.
#[derive(Clone)]
pub struct Image(SharedPtr<br::Image>, super::Memory, u64);
impl Image {
    pub fn bound(mut r: br::Image, mem: &super::Memory, offset: u64) -> br::Result<Self> {
        r.bind(&memory_borrow(&mem), offset as _)
            .map(|_| Image(r.into(), mem.clone(), offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &super::Memory {
        &self.1
    }

    pub fn format(&self) -> super::PixelFormat {
        unsafe { std::mem::transmute(self.0.format()) }
    }
}
impl std::ops::Deref for Image {
    type Target = br::Image;
    fn deref(&self) -> &br::Image {
        &self.0
    }
}
impl br::VkHandle for Image {
    type Handle = <br::Image as br::VkHandle>::Handle;
    const TYPE: br::vk::VkObjectType = <br::Image as br::VkHandle>::TYPE;

    fn native_ptr(&self) -> Self::Handle {
        self.0.native_ptr()
    }
}
