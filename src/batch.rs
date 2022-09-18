//! Batched Operation Helpers

use crate::mthelper::SharedRef;
use bedrock as br;
use br::{vk::VkBufferCopy, VkHandle};
use log::*;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{BTreeMap, HashMap};
use std::hash::{Hash, Hasher};
use std::ops::Range;

use crate::PixelFormat;

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ResourceKey<T: VkHandle>(T);
impl<T: br::VkHandle> PartialEq for ResourceKey<T>
where
    T::Handle: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.0.native_ptr().eq(&other.0.native_ptr())
    }
}
impl<T: br::VkHandle> PartialOrd for ResourceKey<T>
where
    T::Handle: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.native_ptr().partial_cmp(&other.0.native_ptr())
    }
}
impl<T: br::VkHandle> Eq for ResourceKey<T> where T::Handle: Eq {}
impl<T: br::VkHandle> Ord for ResourceKey<T>
where
    T::Handle: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl<T: br::VkHandle> Hash for ResourceKey<T>
where
    T::Handle: Hash,
{
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.native_ptr().hash(hasher)
    }
}

pub struct ReadyResourceBarriers<Buffer: br::Buffer, Image: br::Image> {
    buffer: Vec<(Buffer, Range<u64>, br::vk::VkAccessFlags)>,
    image: Vec<(Image, br::ImageSubresourceRange, br::ImageLayout)>,
}
impl<Buffer: br::Buffer, Image: br::Image> ReadyResourceBarriers<Buffer, Image> {
    const fn new() -> Self {
        Self {
            buffer: Vec::new(),
            image: Vec::new(),
        }
    }
}

/// Batching Manager for Transferring Operations.
pub struct TransferBatch<Device: br::Device> {
    barrier_range_src: BTreeMap<
        ResourceKey<SharedRef<dyn br::Buffer<ConcreteDevice = Device>>>,
        Range<br::vk::VkDeviceSize>,
    >,
    barrier_range_dst: BTreeMap<
        ResourceKey<SharedRef<dyn br::Buffer<ConcreteDevice = Device>>>,
        Range<br::vk::VkDeviceSize>,
    >,
    org_layout_src:
        BTreeMap<ResourceKey<SharedRef<dyn br::Image<ConcreteDevice = Device>>>, br::ImageLayout>,
    org_layout_dst:
        BTreeMap<ResourceKey<SharedRef<dyn br::Image<ConcreteDevice = Device>>>, br::ImageLayout>,
    copy_buffers: HashMap<
        (
            ResourceKey<SharedRef<dyn br::Buffer<ConcreteDevice = Device>>>,
            ResourceKey<SharedRef<dyn br::Buffer<ConcreteDevice = Device>>>,
        ),
        Vec<VkBufferCopy>,
    >,
    init_images: BTreeMap<
        ResourceKey<SharedRef<dyn br::Image<ConcreteDevice = Device>>>,
        (
            SharedRef<dyn br::Buffer<ConcreteDevice = Device>>,
            br::vk::VkDeviceSize,
        ),
    >,
    ready_barriers: BTreeMap<
        br::PipelineStageFlags,
        ReadyResourceBarriers<
            SharedRef<dyn br::Buffer<ConcreteDevice = Device>>,
            SharedRef<dyn br::Image<ConcreteDevice = Device>>,
        >,
    >,
}
impl<Device: br::Device> TransferBatch<Device> {
    pub fn new() -> Self {
        Self {
            barrier_range_src: BTreeMap::new(),
            barrier_range_dst: BTreeMap::new(),
            org_layout_src: BTreeMap::new(),
            org_layout_dst: BTreeMap::new(),
            copy_buffers: HashMap::new(),
            init_images: BTreeMap::new(),
            ready_barriers: BTreeMap::new(),
        }
    }

    /// Add copying operation between buffers.
    pub fn add_copying_buffer(
        &mut self,
        src: crate::DeviceBufferView<SharedRef<impl br::Buffer<ConcreteDevice = Device> + 'static>>,
        dst: crate::DeviceBufferView<SharedRef<impl br::Buffer<ConcreteDevice = Device> + 'static>>,
        bytes: br::vk::VkDeviceSize,
    ) {
        trace!(
            "Registering COPYING-BUFFER: ({}, {}) -> {bytes} bytes",
            src.offset,
            dst.offset
        );

        let sr = src.range(bytes);
        let dr = dst.range(bytes);
        let (sk, dk) = (
            ResourceKey(src.buffer as SharedRef<dyn br::Buffer<ConcreteDevice = Device>>),
            ResourceKey(dst.buffer as SharedRef<dyn br::Buffer<ConcreteDevice = Device>>),
        );
        Self::update_barrier_range_for(&mut self.barrier_range_src, sk.clone(), sr);
        Self::update_barrier_range_for(&mut self.barrier_range_dst, dk.clone(), dr);
        self.copy_buffers
            .entry((sk, dk))
            .or_insert_with(Vec::new)
            .push(VkBufferCopy {
                srcOffset: src.offset,
                dstOffset: dst.offset,
                size: bytes,
            });
    }

    /// Add copying operation between buffers.
    /// Shorthand for copying operation that both BufferViews have same offset.
    #[inline]
    pub fn add_mirroring_buffer(
        &mut self,
        src: SharedRef<impl br::Buffer<ConcreteDevice = Device> + 'static>,
        dst: SharedRef<impl br::Buffer<ConcreteDevice = Device> + 'static>,
        offset: br::vk::VkDeviceSize,
        bytes: br::vk::VkDeviceSize,
    ) {
        self.add_copying_buffer(
            super::DeviceBufferView {
                buffer: src,
                offset,
            },
            super::DeviceBufferView {
                buffer: dst,
                offset,
            },
            bytes,
        );
    }

    /// Add image content initializing operation, from the buffer.
    pub fn init_image_from(
        &mut self,
        dest: SharedRef<impl br::Image<ConcreteDevice = Device> + 'static>,
        src: crate::DeviceBufferView<SharedRef<impl br::Buffer<ConcreteDevice = Device> + 'static>>,
    ) {
        let size = (dest.size().width * dest.size().height) as u64
            * (PixelFormat::from(dest.format()).bpp() >> 3) as u64;
        let rk_dest = ResourceKey(dest as SharedRef<dyn br::Image<ConcreteDevice = Device>>);

        self.init_images
            .insert(rk_dest.clone(), (src.buffer.clone(), src.offset));
        let sr = src.range(size);
        Self::update_barrier_range_for(&mut self.barrier_range_src, ResourceKey(src.buffer), sr);
        self.org_layout_dst
            .insert(rk_dest, br::ImageLayout::Preinitialized);
    }

    /// Add ready barrier for buffers.
    pub fn add_buffer_graphics_ready(
        &mut self,
        dest_stage: br::PipelineStageFlags,
        res: SharedRef<impl br::Buffer<ConcreteDevice = Device> + 'static>,
        byterange: Range<br::vk::VkDeviceSize>,
        access_grants: br::vk::VkAccessFlags,
    ) {
        self.ready_barriers
            .entry(dest_stage)
            .or_insert_with(ReadyResourceBarriers::new)
            .buffer
            .push((res, byterange, access_grants));
    }

    /// Add ready barrier for images.
    pub fn add_image_graphics_ready(
        &mut self,
        dest_stage: br::PipelineStageFlags,
        res: SharedRef<impl br::Image<ConcreteDevice = Device> + 'static>,
        layout: br::ImageLayout,
    ) {
        self.ready_barriers
            .entry(dest_stage)
            .or_insert_with(ReadyResourceBarriers::new)
            .image
            .push((res, br::ImageSubresourceRange::color(0, 0), layout));
    }

    /// Have add_copying_buffer, add_mirroring_buffer or init_image_from been called?
    pub fn has_copy_ops(&self) -> bool {
        !self.copy_buffers.is_empty() || !self.init_images.is_empty()
    }

    /// Have add_buffer_graphics_ready or add_image_graphics_ready been called?
    pub fn has_ready_barrier_ops(&self) -> bool {
        !self.ready_barriers.is_empty()
    }

    #[inline]
    fn update_barrier_range_for(
        map: &mut BTreeMap<
            ResourceKey<SharedRef<dyn br::Buffer<ConcreteDevice = Device> + 'static>>,
            Range<br::vk::VkDeviceSize>,
        >,
        k: ResourceKey<SharedRef<dyn br::Buffer<ConcreteDevice = Device> + 'static>>,
        new_range: Range<br::vk::VkDeviceSize>,
    ) {
        let r = map.entry(k).or_insert_with(|| new_range.clone());
        r.start = r.start.min(new_range.start);
        r.end = r.end.max(new_range.end);
    }
}
/// Sinking Commands into CommandBuffers
impl<Device: br::Device> TransferBatch<Device> {
    pub fn sink_transfer_commands(&self, r: &mut br::CmdRecord<impl br::CommandBuffer>) {
        let src_barriers = self.barrier_range_src.iter().map(|(b, r)| {
            br::BufferMemoryBarrier::new(
                &b.0,
                r.clone(),
                br::AccessFlags::HOST.write,
                br::AccessFlags::TRANSFER.read,
            )
        });
        let dst_barriers = self.barrier_range_dst.iter().map(|(b, r)| {
            br::BufferMemoryBarrier::new(&b.0, r.clone(), 0, br::AccessFlags::TRANSFER.write)
        });
        let barriers: Vec<_> = src_barriers.chain(dst_barriers).collect();
        let src_barriers_i = self.org_layout_src.iter().map(|(b, &l0)| {
            br::ImageMemoryBarrier::new(
                &b.0,
                br::ImageSubresourceRange::color(0, 0),
                l0,
                br::ImageLayout::TransferSrcOpt,
            )
        });
        let dst_barriers_i = self.org_layout_dst.iter().map(|(b, &l0)| {
            br::ImageMemoryBarrier::new(
                &b.0,
                br::ImageSubresourceRange::color(0, 0),
                l0,
                br::ImageLayout::TransferDestOpt,
            )
        });
        let barriers_i: Vec<_> = src_barriers_i.chain(dst_barriers_i).collect();

        r.pipeline_barrier(
            br::PipelineStageFlags::HOST,
            br::PipelineStageFlags::TRANSFER,
            false,
            &[],
            &barriers,
            &barriers_i,
        );
        for (&(ref s, ref d), ref rs) in &self.copy_buffers {
            r.copy_buffer(&s.0, &d.0, &rs);
        }
        for (d, s) in &self.init_images {
            trace!(
                "Copying Image: extent={:?}",
                br::vk::VkExtent3D::from(d.0.size().clone())
            );
            r.copy_buffer_to_image(
                &s.0,
                &d.0,
                br::ImageLayout::TransferDestOpt,
                &[br::vk::VkBufferImageCopy {
                    bufferOffset: s.1,
                    bufferRowLength: 0,
                    bufferImageHeight: 0,
                    imageSubresource: br::vk::VkImageSubresourceLayers::default(),
                    imageOffset: br::vk::VkOffset3D { x: 0, y: 0, z: 0 },
                    imageExtent: d.0.size().clone().into(),
                }],
            );
        }
    }

    pub fn sink_graphics_ready_commands(&self, r: &mut br::CmdRecord<impl br::CommandBuffer>) {
        for (
            &stg,
            &ReadyResourceBarriers {
                ref buffer,
                ref image,
                ..
            },
        ) in &self.ready_barriers
        {
            let buf_barriers: Vec<_> = buffer
                .iter()
                .map(|&(ref r, ref br, a)| {
                    br::BufferMemoryBarrier::new(
                        &r,
                        br.start as _..br.end as _,
                        br::AccessFlags::TRANSFER.read,
                        a,
                    )
                })
                .collect();
            let img_barriers: Vec<_> = image
                .iter()
                .map(|(r, range, l)| {
                    br::ImageMemoryBarrier::new(
                        r,
                        range.clone(),
                        br::ImageLayout::TransferDestOpt,
                        *l,
                    )
                })
                .collect();
            r.pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                stg,
                false,
                &[],
                &buf_barriers,
                &img_barriers,
            );
        }
    }
}

/// Batching mechanism for Updating Descriptor Sets
pub struct DescriptorSetUpdateBatch(
    Vec<br::DescriptorSetWriteInfo>,
    Vec<br::DescriptorSetCopyInfo>,
);
impl DescriptorSetUpdateBatch {
    /// Create an Empty batch
    pub fn new() -> Self {
        DescriptorSetUpdateBatch(Vec::new(), Vec::new())
    }

    /// Write an information to bound index and array index in destination.
    pub fn write_index(
        &mut self,
        dest: impl Into<br::vk::VkDescriptorSet>,
        bound: u32,
        array: u32,
        info: br::DescriptorUpdateInfo,
    ) -> &mut Self {
        self.0
            .push(br::DescriptorSetWriteInfo(dest.into(), bound, array, info));
        return self;
    }
    /// Write an information to bound index in destination.
    pub fn write(
        &mut self,
        dest: impl Into<br::vk::VkDescriptorSet>,
        bound: u32,
        info: br::DescriptorUpdateInfo,
    ) -> &mut Self {
        self.write_index(dest, bound, 0, info)
    }

    /// Submit entire batches
    pub fn submit(&self, d: &impl br::Device) {
        d.update_descriptor_sets(&self.0, &self.1);
    }
}
