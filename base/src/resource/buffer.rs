//! Buffer Resource Helpers

use super::{AutocloseMappedMemoryRange, ExclusiveLockedSharedMemoryBlock, SharedMemoryBlock};

use bedrock::{self as br, DeviceMemoryMut};
use num::Integer;

use crate::graphics::VulkanGfx;
#[allow(unused_imports)]
use crate::mthelper::DynamicMutabilityProvider;

/// A buffer object that unbounded with any memory objects.
pub struct UnboundedStandaloneBuffer(br::vk::VkBuffer, VulkanGfx);
impl Drop for UnboundedStandaloneBuffer {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_buffer(self.1 .0.device, self.0, None);
        }
    }
}
impl br::VkHandle for UnboundedStandaloneBuffer {
    type Handle = br::vk::VkBuffer;

    fn native_ptr(&self) -> Self::Handle {
        self.0
    }
}

/// A buffer object bound with a memory object.
pub struct Buffer(br::vk::VkBuffer, SharedMemoryBlock, u64);
impl Drop for Buffer {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_buffer(self.1.lock_shared().device.0.device, self.0, None)
        }
    }
}
impl Buffer {
    pub fn bound(
        b: UnboundedStandaloneBuffer,
        mem: &SharedMemoryBlock,
        offset: u64,
    ) -> br::Result<Self> {
        let UnboundedStandaloneBuffer(handle, _) = b;
        unsafe {
            br::vkfn_wrapper::bind_buffer_memory(
                mem.lock_shared().device.0.device,
                handle,
                mem.lock_shared().handle,
                offset as _,
            )?;
        }

        Ok(Self(handle, mem.clone(), offset))
    }

    pub fn guard_map<R>(
        &self,
        range: std::ops::Range<u64>,
        f: impl FnOnce(&br::MappedMemory<ExclusiveLockedSharedMemoryBlock>) -> R,
    ) -> br::Result<R> {
        let mut mem = self.1.lock_exclusive();
        let mapped_range = AutocloseMappedMemoryRange(
            mem.map((self.2 + range.start) as _..(self.2 + range.end) as _)?
                .into(),
        );

        Ok(f(&mapped_range))
    }
}
impl Buffer {
    /// Reference to a memory object bound with this object.
    #[inline]
    pub const fn memory(&self) -> &SharedMemoryBlock {
        &self.1
    }
}
impl br::VkHandle for Buffer {
    type Handle = br::vk::VkBuffer;

    fn native_ptr(&self) -> Self::Handle {
        self.0
    }
}
impl br::DeviceChildHandle for Buffer {
    #[inline(always)]
    fn device_handle(&self) -> br::vk::VkDevice {
        self.1.lock_shared().device.0.device
    }
}
impl br::Buffer for Buffer {}

/// A view of the buffer.
#[derive(Clone, Copy)]
pub struct BufferView<Buffer> {
    pub buffer: Buffer,
    pub offset: usize,
}
impl Buffer {
    pub const fn with_offset(self, offset: usize) -> BufferView<Self> {
        BufferView {
            buffer: self,
            offset,
        }
    }

    pub const fn with_offset_ref(&self, offset: usize) -> BufferView<&Self> {
        BufferView {
            buffer: self,
            offset,
        }
    }
}
impl BufferView<Buffer> {
    pub fn with_offset(self, offset: usize) -> Self {
        Self {
            buffer: self.buffer,
            offset: self.offset + offset,
        }
    }

    pub const fn head_range(&self, bytes: usize) -> core::ops::Range<usize> {
        self.offset..self.offset + bytes
    }
}
/// Conversion for Bedrock bind_vertex_buffers form
impl<Buffer> From<BufferView<Buffer>> for (Buffer, usize) {
    fn from(v: BufferView<Buffer>) -> Self {
        (v.buffer, v.offset)
    }
}

/// a view of the buffer in GPU Address.
#[derive(Clone, Copy)]
pub struct DeviceBufferView<Buffer> {
    pub buffer: Buffer,
    pub offset: br::DeviceSize,
}
impl Buffer {
    pub const fn with_dev_offset(self, offset: br::DeviceSize) -> DeviceBufferView<Self> {
        DeviceBufferView {
            buffer: self,
            offset,
        }
    }

    pub const fn with_dev_offset_ref(&self, offset: br::DeviceSize) -> DeviceBufferView<&Self> {
        DeviceBufferView {
            buffer: self,
            offset,
        }
    }
}
impl<Buffer> DeviceBufferView<Buffer> {
    pub fn with_offset(self, offset: br::DeviceSize) -> Self {
        Self {
            buffer: self.buffer,
            offset: self.offset + offset,
        }
    }

    pub const fn head_range(&self, bytes: br::DeviceSize) -> core::ops::Range<br::DeviceSize> {
        self.offset..self.offset + bytes
    }
}

/// (size, align)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferContent {
    Vertex(u64, u64),
    Index(u64, u64),
    Uniform(u64, u64),
    Raw(u64, u64),
    UniformTexel(u64, u64),
    Storage(u64, u64),
    StorageTexel(u64, u64),
}
impl BufferContent {
    fn usage(&self, src: br::BufferUsage) -> br::BufferUsage {
        use self::BufferContent::*;

        match *self {
            Vertex(_, _) => src.vertex_buffer(),
            Index(_, _) => src.index_buffer(),
            Uniform(_, _) => src.uniform_buffer(),
            Raw(_, _) => src,
            UniformTexel(_, _) => src.uniform_texel_buffer(),
            Storage(_, _) => src.storage_buffer(),
            StorageTexel(_, _) => src.storage_texel_buffer(),
        }
    }

    fn alignment(&self, gfx: &VulkanGfx) -> u64 {
        use self::BufferContent::*;

        match *self {
            Vertex(_, a) | Index(_, a) | Raw(_, a) => a,
            Uniform(_, a) | UniformTexel(_, a) => u64::lcm(
                &gfx.adapter_limits().minUniformBufferOffsetAlignment as _,
                &a,
            ),
            Storage(_, a) | StorageTexel(_, a) => u64::lcm(
                &gfx.adapter_limits().minStorageBufferOffsetAlignment as _,
                &a,
            ),
        }
    }

    const fn size(&self) -> u64 {
        use self::BufferContent::*;

        match *self {
            Vertex(v, _)
            | Index(v, _)
            | Uniform(v, _)
            | Raw(v, _)
            | UniformTexel(v, _)
            | Storage(v, _)
            | StorageTexel(v, _) => v,
        }
    }

    /// Generic Shorthands
    pub const fn vertex<T>() -> Self {
        BufferContent::Vertex(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub const fn vertices<T>(count: usize) -> Self {
        BufferContent::Vertex(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }
    pub const fn vertices_for<T>(slice: &[T]) -> Self {
        Self::vertices::<T>(slice.len())
    }

    pub const fn index<T>() -> Self {
        BufferContent::Index(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub const fn indices<T>(count: usize) -> Self {
        BufferContent::Index(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub const fn uniform<T>() -> Self {
        BufferContent::Uniform(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub const fn uniform_dynarray<T>(count: usize) -> Self {
        BufferContent::Uniform(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub const fn uniform_texel<T>() -> Self {
        BufferContent::UniformTexel(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub const fn uniform_texel_dynarray<T>(count: usize) -> Self {
        BufferContent::UniformTexel(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub const fn storage<T>() -> Self {
        BufferContent::Storage(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub const fn storage_dynarray<T>(count: usize) -> Self {
        BufferContent::Storage(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub const fn storage_texel<T>() -> Self {
        BufferContent::StorageTexel(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub const fn storage_texel_dynarray<T>(count: usize) -> Self {
        BufferContent::StorageTexel(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub const fn raw<T>() -> Self {
        BufferContent::Raw(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub const fn raw_dynarray<T>(count: usize) -> Self {
        BufferContent::Raw(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub const fn raw_for_slice<T>(slice: &[T]) -> Self {
        Self::Raw(
            (core::mem::size_of::<T>() * slice.len()) as _,
            core::mem::align_of::<T>() as _,
        )
    }
}
#[derive(Clone)]
pub struct BufferPrealloc<'g> {
    g: &'g crate::Graphics,
    usage: br::BufferUsage,
    offsets: Vec<u64>,
    total: u64,
    common_align: u64,
}
impl<'g> BufferPrealloc<'g> {
    pub const fn new(g: &'g crate::Graphics) -> Self {
        Self {
            g,
            usage: br::BufferUsage(0),
            offsets: Vec::new(),
            total: 0,
            common_align: 1,
        }
    }

    pub fn build_desc(&self) -> br::BufferCreateInfo {
        br::BufferCreateInfo::new(self.total as _, self.usage)
    }

    /// this ignores usage flags from appended contents
    pub fn build_desc_custom_usage(&self, usage: br::BufferUsage) -> br::BufferCreateInfo {
        br::BufferCreateInfo::new(self.total as _, usage)
    }

    pub fn build(&self) -> br::Result<UnboundedStandaloneBuffer> {
        let handle = unsafe {
            br::vkfn_wrapper::create_buffer(
                self.g.gfx_device.0.device,
                &br::BufferCreateInfo::new(self.total as _, self.usage),
                None,
            )?
        };

        Ok(UnboundedStandaloneBuffer(handle, self.g.gfx_device.clone()))
    }

    pub fn build_transferred(&self) -> br::Result<UnboundedStandaloneBuffer> {
        let handle = unsafe {
            br::vkfn_wrapper::create_buffer(
                self.g.gfx_device.0.device,
                &br::BufferCreateInfo::new(self.total as _, self.usage.transfer_dest()),
                None,
            )?
        };

        Ok(UnboundedStandaloneBuffer(handle, self.g.gfx_device.clone()))
    }

    pub fn build_upload(&self) -> br::Result<UnboundedStandaloneBuffer> {
        let handle = unsafe {
            br::vkfn_wrapper::create_buffer(
                self.g.gfx_device.0.device,
                &br::BufferCreateInfo::new(self.total as _, self.usage.transfer_src()),
                None,
            )?
        };

        Ok(UnboundedStandaloneBuffer(handle, self.g.gfx_device.clone()))
    }

    pub fn build_custom_usage(
        &self,
        usage: br::BufferUsage,
    ) -> br::Result<UnboundedStandaloneBuffer> {
        let handle = unsafe {
            br::vkfn_wrapper::create_buffer(
                self.g.gfx_device.0.device,
                &br::BufferCreateInfo::new(self.total as _, self.usage | usage),
                None,
            )?
        };

        Ok(UnboundedStandaloneBuffer(handle, self.g.gfx_device.clone()))
    }

    pub fn add(&mut self, content: BufferContent) -> u64 {
        self.usage = content.usage(self.usage);
        let content_align = content.alignment(&self.g.gfx_device);
        self.common_align = self.common_align.lcm(&content_align);
        let offs = super::align2!(self.total, content_align);
        self.total = offs + content.size() as u64;
        self.offsets.push(offs);

        offs
    }
    pub const fn total_size(&self) -> u64 {
        self.total
    }

    /// Returns first offset of merged(other's) prealloc-ed block
    pub fn merge(&mut self, other: &Self) -> u64 {
        self.common_align = self.common_align.lcm(&other.common_align);
        let offs = super::align2!(self.total, other.common_align);
        self.usage |= other.usage;
        self.total = offs + other.total;
        self.offsets.extend(other.offsets.iter().map(|&o| o + offs));

        offs
    }
}
