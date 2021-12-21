//! Batched Operation Helpers

use bedrock as br;
use br::{vk::VkBufferCopy, VkHandle};
use log::*;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{BTreeMap, HashMap};
use std::hash::{Hash, Hasher};
use std::ops::Range;

#[repr(transparent)]
#[derive(Clone)]
pub struct ResourceKey<T: VkHandle>(T);
impl PartialEq for ResourceKey<crate::Buffer> {
    fn eq(&self, other: &Self) -> bool {
        (self.0.native_ptr() as u64).eq(&(other.0.native_ptr() as u64))
    }
}
impl PartialEq for ResourceKey<crate::Image> {
    fn eq(&self, other: &Self) -> bool {
        (self.0.native_ptr() as u64).eq(&(other.0.native_ptr() as u64))
    }
}
impl PartialOrd for ResourceKey<crate::Buffer> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.0.native_ptr() as u64).partial_cmp(&(other.0.native_ptr() as u64))
    }
}
impl PartialOrd for ResourceKey<crate::Image> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.0.native_ptr() as u64).partial_cmp(&(other.0.native_ptr() as u64))
    }
}
impl Eq for ResourceKey<crate::Buffer> {}
impl Ord for ResourceKey<crate::Buffer> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl Eq for ResourceKey<crate::Image> {}
impl Ord for ResourceKey<crate::Image> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl Hash for ResourceKey<crate::Buffer> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.native_ptr().hash(hasher)
    }
}
impl Hash for ResourceKey<crate::Image> {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.0.native_ptr().hash(hasher)
    }
}
pub struct ReadyResourceBarriers {
    buffer: Vec<(crate::Buffer, Range<u64>, br::vk::VkAccessFlags)>,
    image: Vec<(crate::Image, br::ImageSubresourceRange, br::ImageLayout)>,
}
impl ReadyResourceBarriers {
    fn new() -> Self {
        ReadyResourceBarriers {
            buffer: Vec::new(),
            image: Vec::new(),
        }
    }
}

/// Batching Manager for Transferring Operations.
pub struct TransferBatch {
    barrier_range_src: BTreeMap<ResourceKey<crate::Buffer>, Range<br::vk::VkDeviceSize>>,
    barrier_range_dst: BTreeMap<ResourceKey<crate::Buffer>, Range<br::vk::VkDeviceSize>>,
    org_layout_src: BTreeMap<ResourceKey<crate::Image>, br::ImageLayout>,
    org_layout_dst: BTreeMap<ResourceKey<crate::Image>, br::ImageLayout>,
    copy_buffers:
        HashMap<(ResourceKey<crate::Buffer>, ResourceKey<crate::Buffer>), Vec<VkBufferCopy>>,
    init_images: BTreeMap<ResourceKey<crate::Image>, (crate::Buffer, br::vk::VkDeviceSize)>,
    ready_barriers: BTreeMap<br::PipelineStageFlags, ReadyResourceBarriers>,
}
impl TransferBatch {
    pub fn new() -> Self {
        TransferBatch {
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
        src: crate::DeviceBufferView,
        dst: crate::DeviceBufferView,
        bytes: br::vk::VkDeviceSize,
    ) {
        trace!(
            "Registering COPYING-BUFFER: ({}, {}) -> {} bytes",
            src.offset,
            dst.offset,
            bytes
        );
        let (sk, dk) = (
            ResourceKey(src.buffer.clone()),
            ResourceKey(dst.buffer.clone()),
        );
        Self::update_barrier_range_for(&mut self.barrier_range_src, sk.clone(), src.range(bytes));
        Self::update_barrier_range_for(&mut self.barrier_range_dst, dk.clone(), dst.range(bytes));
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
    pub fn add_mirroring_buffer(
        &mut self,
        src: &crate::Buffer,
        dst: &crate::Buffer,
        offset: br::vk::VkDeviceSize,
        bytes: br::vk::VkDeviceSize,
    ) {
        self.add_copying_buffer(
            src.with_dev_offset(offset),
            dst.with_dev_offset(offset),
            bytes,
        );
    }
    /// Add image content initializing operation, from the buffer.
    pub fn init_image_from(&mut self, dest: &crate::Image, src: crate::DeviceBufferView) {
        self.init_images
            .insert(ResourceKey(dest.clone()), (src.buffer.clone(), src.offset));
        let size = (dest.size().0 * dest.size().1) as u64 * (dest.format().bpp() >> 3) as u64;
        Self::update_barrier_range_for(
            &mut self.barrier_range_src,
            ResourceKey(src.buffer.clone()),
            src.range(size),
        );
        self.org_layout_dst
            .insert(ResourceKey(dest.clone()), br::ImageLayout::Preinitialized);
    }

    /// Add ready barrier for buffers.
    pub fn add_buffer_graphics_ready(
        &mut self,
        dest_stage: br::PipelineStageFlags,
        res: &crate::Buffer,
        byterange: Range<br::vk::VkDeviceSize>,
        access_grants: br::vk::VkAccessFlags,
    ) {
        self.ready_barriers
            .entry(dest_stage)
            .or_insert_with(ReadyResourceBarriers::new)
            .buffer
            .push((res.clone(), byterange, access_grants));
    }
    /// Add ready barrier for images.
    pub fn add_image_graphics_ready(
        &mut self,
        dest_stage: br::PipelineStageFlags,
        res: &crate::Image,
        layout: br::ImageLayout,
    ) {
        self.ready_barriers
            .entry(dest_stage)
            .or_insert_with(ReadyResourceBarriers::new)
            .image
            .push((res.clone(), br::ImageSubresourceRange::color(0, 0), layout));
    }
    /// Have add_copying_buffer, add_mirroring_buffer or init_image_from been called?
    pub fn has_copy_ops(&self) -> bool {
        !self.copy_buffers.is_empty() || !self.init_images.is_empty()
    }
    /// Have add_buffer_graphics_ready or add_image_graphics_ready been called?
    pub fn has_ready_barrier_ops(&self) -> bool {
        !self.ready_barriers.is_empty()
    }

    fn update_barrier_range_for(
        map: &mut BTreeMap<ResourceKey<crate::Buffer>, Range<br::vk::VkDeviceSize>>,
        k: ResourceKey<crate::Buffer>,
        new_range: Range<br::vk::VkDeviceSize>,
    ) {
        let r = map.entry(k).or_insert_with(|| new_range.clone());
        r.start = r.start.min(new_range.start);
        r.end = r.end.max(new_range.end);
    }
}
/// Sinking Commands into CommandBuffers
impl TransferBatch {
    pub fn sink_transfer_commands(&self, r: &mut br::CmdRecord) {
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
                &br::ImageSubref::color(&b.0, 0, 0),
                l0,
                br::ImageLayout::TransferSrcOpt,
            )
        });
        let dst_barriers_i = self.org_layout_dst.iter().map(|(b, &l0)| {
            br::ImageMemoryBarrier::new(
                &br::ImageSubref::color(&b.0, 0, 0),
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
    pub fn sink_graphics_ready_commands(&self, r: &mut br::CmdRecord) {
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
                    br::ImageMemoryBarrier::new_raw(&r, range, br::ImageLayout::TransferDestOpt, *l)
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
        dest: br::vk::VkDescriptorSet,
        bound: u32,
        array: u32,
        info: br::DescriptorUpdateInfo,
    ) -> &mut Self {
        self.0
            .push(br::DescriptorSetWriteInfo(dest, bound, array, info));
        return self;
    }
    /// Write an information to bound index in destination.
    pub fn write(
        &mut self,
        dest: br::vk::VkDescriptorSet,
        bound: u32,
        info: br::DescriptorUpdateInfo,
    ) -> &mut Self {
        self.write_index(dest, bound, 0, info)
    }

    /// Submit entire batches
    pub fn submit(&self, d: &br::Device) {
        d.update_descriptor_sets(&self.0, &self.1);
    }
}
