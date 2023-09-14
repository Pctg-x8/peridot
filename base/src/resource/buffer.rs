//! Buffer Resource Helpers

use super::AutocloseMappedMemoryRange;

use bedrock as br;
use num::Integer;

#[allow(unused_imports)]
use crate::mthelper::DynamicMutabilityProvider;
use crate::{
    mthelper::{DynamicMut, SharedRef},
    DeviceObject,
};

/// A refcounted buffer object bound with a memory object.
#[derive(Clone)]
pub struct Buffer<Backend: br::Buffer, Memory: br::DeviceMemory>(
    Backend,
    SharedRef<DynamicMut<Memory>>,
    u64,
);
impl<
        Backend: br::Buffer + br::MemoryBound + br::VkHandleMut,
        Memory: br::DeviceMemory + br::VkHandleMut,
    > Buffer<Backend, Memory>
{
    pub fn bound(
        mut b: Backend,
        mem: &SharedRef<DynamicMut<Memory>>,
        offset: u64,
    ) -> br::Result<Self> {
        b.bind(&*mem.borrow(), offset as _)
            .map(move |_| Self(b, mem.clone(), offset))
    }

    pub fn guard_map<R>(
        &mut self,
        range: std::ops::Range<u64>,
        f: impl FnOnce(&br::MappedMemoryRange<Memory>) -> R,
    ) -> br::Result<R> {
        let mut mem = self.1.borrow_mut();
        let mapped_range = AutocloseMappedMemoryRange(
            mem.map((self.2 + range.start) as _..(self.2 + range.end) as _)?
                .into(),
        );

        Ok(f(&mapped_range))
    }
}
impl<Backend: br::Buffer, Memory: br::DeviceMemory> Buffer<Backend, Memory> {
    /// Reference to a memory object bound with this object.
    #[inline]
    pub const fn memory(&self) -> &SharedRef<DynamicMut<Memory>> {
        &self.1
    }
}
impl<Backend: br::Buffer, Memory: br::DeviceMemory> std::ops::Deref for Buffer<Backend, Memory> {
    type Target = Backend;

    fn deref(&self) -> &Backend {
        &self.0
    }
}
impl<Backend: br::Buffer, Memory: br::DeviceMemory> br::VkHandle for Buffer<Backend, Memory> {
    type Handle = <Backend as br::VkHandle>::Handle;

    fn native_ptr(&self) -> Self::Handle {
        self.0.native_ptr()
    }
}
impl<Backend: br::Buffer, Memory: br::DeviceMemory> br::DeviceChild for Buffer<Backend, Memory> {
    type ConcreteDevice = Backend::ConcreteDevice;

    fn device(&self) -> &Self::ConcreteDevice {
        self.0.device()
    }
}
impl<Backend: br::Buffer, Memory: br::DeviceMemory> br::Buffer for Buffer<Backend, Memory> {}

/// A view of the buffer.
#[derive(Clone, Copy)]
pub struct BufferView<Buffer: br::Buffer> {
    pub buffer: Buffer,
    pub offset: usize,
}
impl<Backend: br::Buffer, Memory: br::DeviceMemory> Buffer<Backend, Memory> {
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
impl<Buffer: br::Buffer> BufferView<Buffer> {
    pub fn with_offset(self, offset: usize) -> Self {
        Self {
            buffer: self.buffer,
            offset: self.offset + offset,
        }
    }

    pub const fn range(&self, bytes: usize) -> std::ops::Range<usize> {
        self.offset..self.offset + bytes
    }
}
/// Conversion for Bedrock bind_vertex_buffers form
impl<Buffer: br::Buffer> From<BufferView<Buffer>> for (Buffer, usize) {
    fn from(v: BufferView<Buffer>) -> Self {
        (v.buffer, v.offset)
    }
}

/// a view of the buffer in GPU Address.
#[derive(Clone, Copy)]
pub struct DeviceBufferView<Buffer> {
    pub buffer: Buffer,
    pub offset: br::vk::VkDeviceSize,
}
impl<Backend: br::Buffer, Memory: br::DeviceMemory> Buffer<Backend, Memory> {
    pub const fn with_dev_offset(self, offset: br::vk::VkDeviceSize) -> DeviceBufferView<Self> {
        DeviceBufferView {
            buffer: self,
            offset,
        }
    }

    pub const fn with_dev_offset_ref(
        &self,
        offset: br::vk::VkDeviceSize,
    ) -> DeviceBufferView<&Self> {
        DeviceBufferView {
            buffer: self,
            offset,
        }
    }
}
impl<Buffer> DeviceBufferView<Buffer> {
    pub fn with_offset(self, offset: br::vk::VkDeviceSize) -> Self {
        Self {
            buffer: self.buffer,
            offset: self.offset + offset,
        }
    }

    pub const fn range(
        &self,
        bytes: br::vk::VkDeviceSize,
    ) -> std::ops::Range<br::vk::VkDeviceSize> {
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

    fn alignment(&self, pd: &impl br::PhysicalDevice) -> u64 {
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

    pub fn build_desc(&self) -> br::BufferDesc {
        br::BufferDesc::new(self.total as _, self.usage)
    }

    /// this ignores usage flags from appended contents
    pub fn build_desc_custom_usage(&self, usage: br::BufferUsage) -> br::BufferDesc {
        br::BufferDesc::new(self.total as _, usage)
    }

    pub fn build(&self) -> br::Result<br::BufferObject<DeviceObject>> {
        br::BufferDesc::new(self.total as _, self.usage).create(self.g.device.clone())
    }

    pub fn build_transferred(&self) -> br::Result<br::BufferObject<DeviceObject>> {
        br::BufferDesc::new(self.total as _, self.usage.transfer_dest())
            .create(self.g.device.clone())
    }

    pub fn build_upload(&self) -> br::Result<br::BufferObject<DeviceObject>> {
        br::BufferDesc::new(self.total as _, self.usage.transfer_src())
            .create(self.g.device.clone())
    }

    pub fn build_custom_usage(
        &self,
        usage: br::BufferUsage,
    ) -> br::Result<br::BufferObject<DeviceObject>> {
        br::BufferDesc::new(self.total as _, self.usage | usage).create(self.g.device.clone())
    }

    pub fn add(&mut self, content: BufferContent) -> u64 {
        self.usage = content.usage(self.usage);
        let content_align = content.alignment(&self.g.adapter);
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
