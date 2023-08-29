use std::ops::Range;

use bedrock as br;
use br::ImageSubresourceSlice;

#[cfg(feature = "memory_manager_interop")]
use peridot_memory_manager as pmm;

use crate::{BufferUsage, BufferUsageTransitionBarrier, CopyBuffer, GraphicsCommand};

pub struct RangedBuffer<B: br::Buffer>(pub B, pub Range<u64>);
impl<B: br::Buffer> RangedBuffer<B> {
    pub const fn from_offset_length(buffer: B, offset: u64, length: usize) -> Self {
        Self(buffer, offset..offset + length as u64)
    }

    pub const fn for_type<T>(buffer: B, offset: u64) -> Self {
        Self::from_offset_length(buffer, offset, std::mem::size_of::<T>())
    }

    pub const fn offset(&self) -> u64 {
        self.1.start
    }

    pub const fn byte_length(&self) -> u64 {
        self.1.end - self.1.start
    }

    pub fn make_ref<'s>(&'s self) -> RangedBuffer<&'s B> {
        RangedBuffer(&self.0, self.1.clone())
    }

    pub fn make_descriptor_buffer_ref(&self) -> br::DescriptorBufferRef {
        br::DescriptorBufferRef::new(&self.0, self.1.clone())
    }

    pub fn usage_barrier(
        &self,
        from_usage: BufferUsage,
        to_usage: BufferUsage,
    ) -> BufferUsageTransitionBarrier<B> {
        BufferUsageTransitionBarrier {
            buffer: &self,
            from_usage,
            to_usage,
        }
    }

    pub fn usage_barrier3(
        &self,
        first_usage: BufferUsage,
        intermedial_usage: BufferUsage,
        last_usage: BufferUsage,
    ) -> [BufferUsageTransitionBarrier<B>; 2] {
        [
            self.usage_barrier(first_usage, intermedial_usage),
            self.usage_barrier(intermedial_usage, last_usage),
        ]
    }

    pub fn usage_barrier3_switching(
        &self,
        first_usage: BufferUsage,
        intermedial_usage: BufferUsage,
    ) -> [BufferUsageTransitionBarrier<B>; 2] {
        self.usage_barrier3(first_usage, intermedial_usage, first_usage)
    }

    pub fn inner_ref(&self) -> &B {
        &self.0
    }

    /// generates mirroring command from self to dest.
    ///
    /// both buffer length must be equal.
    pub fn mirror_to<'s>(
        &'s self,
        dest: &'s RangedBuffer<impl br::Buffer>,
    ) -> impl GraphicsCommand + 's {
        assert_eq!(self.byte_length(), dest.byte_length());

        CopyBuffer::new(&self.0, &dest.0).with_mirroring(0, self.byte_length() as _)
    }

    /// generates mirroring command from src to self. (reversing mirror_to arguments)
    ///
    /// both buffer length must be equal.
    pub fn mirror_from<'s>(
        &'s self,
        src: &'s RangedBuffer<impl br::Buffer>,
    ) -> impl GraphicsCommand + 's {
        src.mirror_to(self)
    }
}
impl<B: br::Buffer + Clone> RangedBuffer<&'_ B> {
    pub fn clone_inner_ref(&self) -> RangedBuffer<B> {
        RangedBuffer(self.0.clone(), self.1.clone())
    }
}
impl<B: br::Buffer + br::MemoryBound + br::VkHandleMut, M: br::DeviceMemory + br::VkHandleMut>
    RangedBuffer<peridot::Buffer<B, M>>
{
    pub fn guard_map<R>(
        &mut self,
        action: impl FnOnce(&br::MappedMemoryRange<M>) -> R,
    ) -> br::Result<R> {
        self.0.guard_map(self.1.clone(), action)
    }
}
#[cfg(feature = "memory_manager_interop")]
impl From<pmm::Buffer> for RangedBuffer<pmm::Buffer> {
    fn from(value: pmm::Buffer) -> Self {
        let length = value.byte_length();

        Self::from_offset_length(value, 0, length)
    }
}
#[cfg(feature = "memory_manager_interop")]
impl<'s> From<&'s pmm::Buffer> for RangedBuffer<&'s pmm::Buffer> {
    fn from(value: &'s pmm::Buffer) -> Self {
        let length = value.byte_length();

        Self::from_offset_length(value, 0, length)
    }
}

pub struct RangedImage<R: br::Image>(br::ImageSubresourceRange<R>);
impl<R: br::Image> RangedImage<R> {
    pub fn single_color_plane(resource: R) -> Self {
        Self(resource.subresource_range(br::AspectMask::COLOR, 0..1, 0..1))
    }

    pub fn single_depth_stencil_plane(resource: R) -> Self {
        Self(resource.subresource_range(br::AspectMask::DEPTH.stencil(), 0..1, 0..1))
    }

    pub fn single_stencil_plane(resource: R) -> Self {
        Self(resource.subresource_range(br::AspectMask::STENCIL, 0..1, 0..1))
    }

    pub fn barrier(
        &self,
        from_layout: br::ImageLayout,
        to_layout: br::ImageLayout,
    ) -> br::ImageMemoryBarrier {
        self.0.make_ref().memory_barrier(from_layout, to_layout)
    }

    pub fn barrier3(
        &self,
        first_layout: br::ImageLayout,
        intermedial_layout: br::ImageLayout,
        last_layout: br::ImageLayout,
    ) -> [br::ImageMemoryBarrier; 2] {
        [
            self.barrier(first_layout, intermedial_layout),
            self.barrier(intermedial_layout, last_layout),
        ]
    }
}
