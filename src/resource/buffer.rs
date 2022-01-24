//! Buffer Resource Helpers

use super::memory::memory_borrow;
use super::memory::memory_borrow_mut;

use super::AutocloseMappedMemoryRange;

use super::Memory;
use bedrock as br;
use br::MemoryBound;
use num::Integer;

#[cfg(not(feature = "mt"))]
use std::rc::Rc as SharedPtr;
#[cfg(feature = "mt")]
use std::sync::Arc as SharedPtr;

/// A refcounted buffer object bound with a memory object.
#[derive(Clone)]
pub struct Buffer(SharedPtr<br::Buffer>, Memory, u64);
impl Buffer {
    pub fn bound(mut b: br::Buffer, mem: &Memory, offset: u64) -> br::Result<Self> {
        b.bind(&memory_borrow(mem), offset as _)
            .map(|_| Buffer(b.into(), mem.clone(), offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &Memory {
        &self.1
    }

    pub fn guard_map<R>(
        &mut self,
        range: std::ops::Range<u64>,
        f: impl FnOnce(&br::MappedMemoryRange) -> R,
    ) -> br::Result<R> {
        let mut mem = memory_borrow_mut(&self.1);
        let mapped_range = AutocloseMappedMemoryRange(
            mem.map((self.2 + range.start) as _..(self.2 + range.end) as _)?
                .into(),
        );

        Ok(f(&mapped_range))
    }
}
impl std::ops::Deref for Buffer {
    type Target = br::Buffer;
    fn deref(&self) -> &br::Buffer {
        &self.0
    }
}
impl br::VkHandle for Buffer {
    type Handle = <br::Buffer as br::VkHandle>::Handle;
    const TYPE: br::vk::VkObjectType = <br::Buffer as br::VkHandle>::TYPE;

    fn native_ptr(&self) -> Self::Handle {
        self.0.native_ptr()
    }
}

/// A view of the buffer.
#[derive(Clone, Copy)]
pub struct BufferView<'b> {
    pub buffer: &'b Buffer,
    pub offset: usize,
}
impl Buffer {
    pub fn with_offset(&self, offset: usize) -> BufferView {
        BufferView {
            buffer: self,
            offset,
        }
    }
}
impl BufferView<'_> {
    pub fn with_offset(self, offset: usize) -> Self {
        BufferView {
            buffer: self.buffer,
            offset: self.offset + offset,
        }
    }
    pub fn range(&self, bytes: usize) -> std::ops::Range<usize> {
        self.offset..self.offset + bytes
    }
}
/// Conversion for Bedrock bind_vertex_buffers form
impl<'b> From<BufferView<'b>> for (&'b Buffer, usize) {
    fn from(v: BufferView<'b>) -> Self {
        (v.buffer, v.offset)
    }
}

/// a view of the buffer in GPU Address.
#[derive(Clone, Copy)]
pub struct DeviceBufferView<'b> {
    pub buffer: &'b Buffer,
    pub offset: br::vk::VkDeviceSize,
}
impl Buffer {
    pub fn with_dev_offset(&self, offset: br::vk::VkDeviceSize) -> DeviceBufferView {
        DeviceBufferView {
            buffer: self,
            offset,
        }
    }
}
impl DeviceBufferView<'_> {
    pub fn with_offset(&self, offset: br::vk::VkDeviceSize) -> Self {
        DeviceBufferView {
            buffer: self.buffer,
            offset: self.offset + offset,
        }
    }
    pub fn range(&self, bytes: br::vk::VkDeviceSize) -> std::ops::Range<br::vk::VkDeviceSize> {
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
    fn alignment(&self, pd: &br::PhysicalDevice) -> u64 {
        use self::BufferContent::*;

        match *self {
            Vertex(_, a) | Index(_, a) | Raw(_, a) => a,
            Uniform(_, a) | UniformTexel(_, a) => u64::lcm(
                &pd.properties().limits.minUniformBufferOffsetAlignment as _,
                &a,
            ),
            Storage(_, a) | StorageTexel(_, a) => u64::lcm(
                &pd.properties().limits.minStorageBufferOffsetAlignment as _,
                &a,
            ),
        }
    }
    fn size(&self) -> u64 {
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
    pub fn vertex<T>() -> Self {
        BufferContent::Vertex(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub fn vertices<T>(count: usize) -> Self {
        BufferContent::Vertex(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub fn index<T>() -> Self {
        BufferContent::Index(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub fn indices<T>(count: usize) -> Self {
        BufferContent::Index(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub fn uniform<T>() -> Self {
        BufferContent::Uniform(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub fn uniform_dynarray<T>(count: usize) -> Self {
        BufferContent::Uniform(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub fn uniform_texel<T>() -> Self {
        BufferContent::UniformTexel(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub fn uniform_texel_dynarray<T>(count: usize) -> Self {
        BufferContent::UniformTexel(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub fn storage<T>() -> Self {
        BufferContent::Storage(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub fn storage_dynarray<T>(count: usize) -> Self {
        BufferContent::Storage(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
        )
    }

    pub fn storage_texel<T>() -> Self {
        BufferContent::StorageTexel(
            std::mem::size_of::<T>() as _,
            std::mem::align_of::<T>() as _,
        )
    }
    pub fn storage_texel_dynarray<T>(count: usize) -> Self {
        BufferContent::StorageTexel(
            std::mem::size_of::<T>() as u64 * count as u64,
            std::mem::align_of::<T>() as _,
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
    pub fn new(g: &'g crate::Graphics) -> Self {
        BufferPrealloc {
            g,
            usage: br::BufferUsage(0),
            offsets: Vec::new(),
            total: 0,
            common_align: 1,
        }
    }
    pub fn build(&self) -> br::Result<br::Buffer> {
        br::BufferDesc::new(self.total as _, self.usage).create(&self.g.device)
    }
    pub fn build_transferred(&self) -> br::Result<br::Buffer> {
        br::BufferDesc::new(self.total as _, self.usage.transfer_dest()).create(&self.g.device)
    }
    pub fn build_upload(&self) -> br::Result<br::Buffer> {
        br::BufferDesc::new(self.total as _, self.usage.transfer_src()).create(&self.g.device)
    }
    pub fn build_custom_usage(&self, usage: br::BufferUsage) -> br::Result<br::Buffer> {
        br::BufferDesc::new(self.total as _, self.usage | usage).create(&self.g.device)
    }

    pub fn add(&mut self, content: BufferContent) -> u64 {
        self.usage = content.usage(self.usage);
        let content_align = content.alignment(&self.g.adapter);
        self.common_align = self.common_align.lcm(&content_align);
        let offs = super::align2!(self.total, content_align);
        self.total = offs + content.size() as u64;
        self.offsets.push(offs);
        return offs;
    }
    pub fn total_size(&self) -> u64 {
        self.total
    }

    /// Returns first offset of merged(other's) prealloc-ed block
    pub fn merge(&mut self, other: &Self) -> u64 {
        self.common_align = self.common_align.lcm(&other.common_align);
        let offs = super::align2!(self.total, other.common_align);
        self.usage |= other.usage;
        self.total = offs + other.total;
        self.offsets.extend(other.offsets.iter().map(|&o| o + offs));
        return offs;
    }
}
