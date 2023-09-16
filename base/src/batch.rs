//! Batched Operation Helpers

use crate::mthelper::{DynamicMut, DynamicMutabilityProvider, SharedRef};
use bedrock as br;
use br::vk::VkBufferCopy;
use br::{VkHandle, VulkanStructure};
use log::*;
use std::cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::ops::Range;

use crate::PixelFormat;

pub trait TransferrableBufferResource {
    fn grouping_key(&self) -> u64;
    fn raw_handle(&self) -> br::vk::VkBuffer;
}
impl<T> TransferrableBufferResource for SharedRef<T>
where
    T: TransferrableBufferResource,
{
    fn grouping_key(&self) -> u64 {
        T::grouping_key(&**self)
    }

    fn raw_handle(&self) -> br::vk::VkBuffer {
        T::raw_handle(&**self)
    }
}
impl<T> TransferrableBufferResource for DynamicMut<T>
where
    T: TransferrableBufferResource,
{
    fn grouping_key(&self) -> u64 {
        T::grouping_key(&self.borrow())
    }

    fn raw_handle(&self) -> br::vk::VkBuffer {
        T::raw_handle(&self.borrow())
    }
}

impl<Device: br::Device> TransferrableBufferResource for br::BufferObject<Device> {
    fn grouping_key(&self) -> u64 {
        unsafe { core::mem::transmute(self.native_ptr()) }
    }

    fn raw_handle(&self) -> br::vk::VkBuffer {
        self.native_ptr()
    }
}
impl<Backend: br::Buffer, Memory: br::DeviceMemory> TransferrableBufferResource
    for crate::Buffer<Backend, Memory>
{
    fn grouping_key(&self) -> u64 {
        unsafe { core::mem::transmute(self.native_ptr()) }
    }

    fn raw_handle(&self) -> br::vk::VkBuffer {
        self.native_ptr()
    }
}
impl<T> TransferrableBufferResource for Box<T>
where
    T: TransferrableBufferResource,
{
    fn grouping_key(&self) -> u64 {
        T::grouping_key(&**self)
    }

    fn raw_handle(&self) -> br::vk::VkBuffer {
        T::raw_handle(&**self)
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct ResourceKey<H>(SharedRef<dyn br::VkHandle<Handle = H>>);
impl<H: PartialEq> PartialEq for ResourceKey<H> {
    fn eq(&self, other: &Self) -> bool {
        self.0.native_ptr().eq(&other.0.native_ptr())
    }
}
impl<H: PartialOrd> PartialOrd for ResourceKey<H> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.native_ptr().partial_cmp(&other.0.native_ptr())
    }
}
impl<H: Eq> Eq for ResourceKey<H> {}
impl<H: Ord> Ord for ResourceKey<H> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.native_ptr().cmp(&other.0.native_ptr())
    }
}
impl<Hdl: Hash> Hash for ResourceKey<Hdl> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.native_ptr().hash(state)
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct ImageKey<Device: br::Device>(SharedRef<dyn br::Image<ConcreteDevice = Device>>);
impl<Device: br::Device> PartialEq for ImageKey<Device> {
    fn eq(&self, other: &Self) -> bool {
        self.0.native_ptr().eq(&other.0.native_ptr())
    }
}
impl<Device: br::Device> PartialOrd for ImageKey<Device> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.0.native_ptr().partial_cmp(&other.0.native_ptr())
    }
}
impl<Device: br::Device> Eq for ImageKey<Device> {}
impl<Device: br::Device> Ord for ImageKey<Device> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.native_ptr().cmp(&other.0.native_ptr())
    }
}
impl<Device: br::Device> Hash for ImageKey<Device> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.native_ptr().hash(state)
    }
}

pub struct ReadyResourceBarriers<Buffer, Image> {
    buffer: Vec<(Buffer, Range<u64>, br::vk::VkAccessFlags)>,
    image: Vec<(Image, br::vk::VkImageSubresourceRange, br::ImageLayout)>,
}
impl<Buffer, Image> ReadyResourceBarriers<Buffer, Image> {
    const fn new() -> Self {
        Self {
            buffer: Vec::new(),
            image: Vec::new(),
        }
    }
}

#[repr(transparent)]
pub struct TransferrableBufferResourceCompareCell(Box<dyn TransferrableBufferResource>);
impl PartialEq for TransferrableBufferResourceCompareCell {
    fn eq(&self, other: &Self) -> bool {
        self.0.grouping_key() == other.0.grouping_key()
    }
}
impl Eq for TransferrableBufferResourceCompareCell {}
impl Hash for TransferrableBufferResourceCompareCell {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.grouping_key().hash(state)
    }
}

pub struct CopyBufferPair(
    Box<dyn TransferrableBufferResource>,
    Box<dyn TransferrableBufferResource>,
);
impl Hash for CopyBufferPair {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0.grouping_key(), self.1.grouping_key()).hash(state)
    }
}
impl PartialEq for CopyBufferPair {
    fn eq(&self, other: &Self) -> bool {
        (self.0.grouping_key(), self.1.grouping_key())
            == (other.0.grouping_key(), other.1.grouping_key())
    }
}
impl Eq for CopyBufferPair {}

pub struct TransferBatch2 {
    copy_buffers: HashMap<CopyBufferPair, Vec<br::vk::VkBufferCopy>>,
    src_transition_sets: HashSet<(TransferrableBufferResourceCompareCell, Range<u64>)>,
    before_transitions: HashMap<
        br::vk::VkPipelineStageFlags,
        Vec<(
            Box<dyn TransferrableBufferResource>,
            Range<u64>,
            br::vk::VkAccessFlags,
        )>,
    >,
    after_transitions: HashMap<
        br::vk::VkPipelineStageFlags,
        Vec<(
            Box<dyn TransferrableBufferResource>,
            Range<u64>,
            br::vk::VkAccessFlags,
        )>,
    >,
}
impl TransferBatch2 {
    pub fn new() -> Self {
        Self {
            copy_buffers: HashMap::new(),
            src_transition_sets: HashSet::new(),
            before_transitions: HashMap::new(),
            after_transitions: HashMap::new(),
        }
    }

    pub fn has_ops(&self) -> bool {
        !self.copy_buffers.is_empty()
            || !self.before_transitions.is_empty()
            || !self.after_transitions.is_empty()
    }

    pub fn copy_buffer(
        &mut self,
        src: impl TransferrableBufferResource + Clone + 'static,
        src_offset: u64,
        dst: impl TransferrableBufferResource + 'static,
        dst_offset: u64,
        byte_length: u64,
    ) {
        self.copy_buffers
            .entry(CopyBufferPair(Box::new(src.clone()), Box::new(dst)))
            .or_insert_with(Vec::new)
            .push(br::vk::VkBufferCopy {
                srcOffset: src_offset,
                dstOffset: dst_offset,
                size: byte_length,
            });
        self.src_transition_sets.insert((
            TransferrableBufferResourceCompareCell(Box::new(src)),
            src_offset..src_offset + byte_length,
        ));
    }

    pub fn register_before_transition(
        &mut self,
        pipeline_stage: br::vk::VkPipelineStageFlags,
        res: impl TransferrableBufferResource + 'static,
        range: Range<u64>,
        access_mask: br::vk::VkAccessFlags,
    ) {
        self.before_transitions
            .entry(pipeline_stage)
            .or_insert_with(Vec::new)
            .push((Box::new(res), range, access_mask));
    }

    pub fn register_after_transition(
        &mut self,
        pipeline_stage: br::vk::VkPipelineStageFlags,
        res: impl TransferrableBufferResource + 'static,
        range: Range<u64>,
        access_mask: br::vk::VkAccessFlags,
    ) {
        self.after_transitions
            .entry(pipeline_stage)
            .or_insert_with(Vec::new)
            .push((Box::new(res), range, access_mask));
    }

    pub fn register_outer_usage(
        &mut self,
        pipeline_stage: br::vk::VkPipelineStageFlags,
        res: impl TransferrableBufferResource + Clone + 'static,
        range: Range<u64>,
        access_mask: br::vk::VkAccessFlags,
    ) {
        self.register_before_transition(pipeline_stage, res.clone(), range.clone(), access_mask);
        self.register_after_transition(pipeline_stage, res, range, access_mask);
    }

    pub fn generate_commands(
        &self,
        rec: &mut br::CmdRecord<impl br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized>,
    ) {
        let _ = rec.pipeline_barrier(
            br::PipelineStageFlags::HOST,
            br::PipelineStageFlags::TRANSFER,
            false,
            &[],
            &self
                .src_transition_sets
                .iter()
                .map(|(res, range)| {
                    br::BufferMemoryBarrier::from(br::vk::VkBufferMemoryBarrier {
                        sType: br::vk::VkBufferMemoryBarrier::TYPE,
                        pNext: core::ptr::null(),
                        srcAccessMask: br::AccessFlags::HOST.write,
                        dstAccessMask: br::AccessFlags::TRANSFER.read,
                        srcQueueFamilyIndex: br::vk::VK_QUEUE_FAMILY_IGNORED,
                        dstQueueFamilyIndex: br::vk::VK_QUEUE_FAMILY_IGNORED,
                        buffer: res.0.raw_handle(),
                        offset: range.start,
                        size: range.end - range.start,
                    })
                })
                .collect::<Vec<_>>(),
            &[],
        );
        for (&p, trans) in self.before_transitions.iter() {
            let _ = rec.pipeline_barrier(
                br::PipelineStageFlags(p),
                br::PipelineStageFlags::TRANSFER,
                false,
                &[],
                &trans
                    .iter()
                    .map(|(res, range, a)| {
                        br::BufferMemoryBarrier::from(br::vk::VkBufferMemoryBarrier {
                            sType: br::vk::VkBufferMemoryBarrier::TYPE,
                            pNext: core::ptr::null(),
                            srcAccessMask: *a,
                            dstAccessMask: br::AccessFlags::TRANSFER.write,
                            srcQueueFamilyIndex: br::vk::VK_QUEUE_FAMILY_IGNORED,
                            dstQueueFamilyIndex: br::vk::VK_QUEUE_FAMILY_IGNORED,
                            buffer: res.raw_handle(),
                            offset: range.start,
                            size: range.end - range.start,
                        })
                    })
                    .collect::<Vec<_>>(),
                &[],
            );
        }

        for (CopyBufferPair(src, dst), ranges) in self.copy_buffers.iter() {
            let _ = rec.copy_buffer(
                &unsafe { br::VkHandleRef::dangling(src.raw_handle()) },
                &unsafe { br::VkHandleRef::dangling(dst.raw_handle()) },
                &ranges,
            );
        }

        for (&p, ts) in self.after_transitions.iter() {
            let _ = rec.pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags(p),
                false,
                &[],
                &ts.iter()
                    .map(|(res, range, a)| {
                        br::BufferMemoryBarrier::from(br::vk::VkBufferMemoryBarrier {
                            sType: br::vk::VkBufferMemoryBarrier::TYPE,
                            pNext: core::ptr::null(),
                            srcAccessMask: br::AccessFlags::TRANSFER.write,
                            dstAccessMask: *a,
                            srcQueueFamilyIndex: br::vk::VK_QUEUE_FAMILY_IGNORED,
                            dstQueueFamilyIndex: br::vk::VK_QUEUE_FAMILY_IGNORED,
                            buffer: res.raw_handle(),
                            offset: range.start,
                            size: range.end - range.start,
                        })
                    })
                    .collect::<Vec<_>>(),
                &[],
            );
        }
        let _ = rec.pipeline_barrier(
            br::PipelineStageFlags::TRANSFER,
            br::PipelineStageFlags::HOST,
            false,
            &[],
            &self
                .src_transition_sets
                .iter()
                .map(|(res, range)| {
                    br::BufferMemoryBarrier::from(br::vk::VkBufferMemoryBarrier {
                        sType: br::vk::VkBufferMemoryBarrier::TYPE,
                        pNext: core::ptr::null(),
                        srcAccessMask: br::AccessFlags::TRANSFER.read,
                        dstAccessMask: br::AccessFlags::HOST.write,
                        srcQueueFamilyIndex: br::vk::VK_QUEUE_FAMILY_IGNORED,
                        dstQueueFamilyIndex: br::vk::VK_QUEUE_FAMILY_IGNORED,
                        buffer: res.0.raw_handle(),
                        offset: range.start,
                        size: range.end - range.start,
                    })
                })
                .collect::<Vec<_>>(),
            &[],
        );
    }
}

/// Batching Manager for Transferring Operations.
pub struct TransferBatch<Device: br::Device = super::DeviceObject> {
    barrier_range_src: BTreeMap<ResourceKey<br::vk::VkBuffer>, Range<br::vk::VkDeviceSize>>,
    barrier_range_dst: BTreeMap<ResourceKey<br::vk::VkBuffer>, Range<br::vk::VkDeviceSize>>,
    org_layout_src: BTreeMap<ImageKey<Device>, br::ImageLayout>,
    org_layout_dst: BTreeMap<ImageKey<Device>, br::ImageLayout>,
    copy_buffers:
        HashMap<(ResourceKey<br::vk::VkBuffer>, ResourceKey<br::vk::VkBuffer>), Vec<VkBufferCopy>>,
    init_images: BTreeMap<
        ImageKey<Device>,
        (
            br::vk::VkExtent3D,
            SharedRef<dyn br::VkHandle<Handle = br::vk::VkBuffer>>,
            br::vk::VkDeviceSize,
        ),
    >,
    ready_barriers: BTreeMap<
        br::PipelineStageFlags,
        ReadyResourceBarriers<
            SharedRef<dyn br::VkHandle<Handle = br::vk::VkBuffer>>,
            SharedRef<dyn br::VkHandle<Handle = br::vk::VkImage>>,
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
        src: crate::DeviceBufferView<SharedRef<dyn br::VkHandle<Handle = br::vk::VkBuffer>>>,
        dst: crate::DeviceBufferView<SharedRef<dyn br::VkHandle<Handle = br::vk::VkBuffer>>>,
        bytes: br::vk::VkDeviceSize,
    ) {
        trace!(
            "Registering COPYING-BUFFER: ({}, {}) -> {bytes} bytes",
            src.offset,
            dst.offset
        );

        Self::update_barrier_range_for(
            &mut self.barrier_range_src,
            ResourceKey(src.buffer.clone()),
            src.range(bytes),
        );
        Self::update_barrier_range_for(
            &mut self.barrier_range_dst,
            ResourceKey(dst.buffer.clone()),
            dst.range(bytes),
        );
        self.copy_buffers
            .entry((ResourceKey(src.buffer), ResourceKey(dst.buffer)))
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
        src: SharedRef<dyn br::VkHandle<Handle = br::vk::VkBuffer>>,
        dst: SharedRef<dyn br::VkHandle<Handle = br::vk::VkBuffer>>,
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
        dest: SharedRef<dyn br::Image<ConcreteDevice = Device>>,
        src: crate::DeviceBufferView<SharedRef<dyn br::VkHandle<Handle = br::vk::VkBuffer>>>,
    ) {
        let extent = dest.size();
        let byte_length = (extent.width * extent.height) as u64
            * (PixelFormat::from(dest.format()).bpp() >> 3) as u64;

        self.init_images.insert(
            ImageKey(dest.clone()),
            (extent.clone(), src.buffer.clone(), src.offset),
        );
        let sr = src.range(byte_length);
        Self::update_barrier_range_for(&mut self.barrier_range_src, ResourceKey(src.buffer), sr);
        self.org_layout_dst
            .insert(ImageKey(dest), br::ImageLayout::Preinitialized);
    }

    /// Add ready barrier for buffers.
    pub fn add_buffer_graphics_ready(
        &mut self,
        dest_stage: br::PipelineStageFlags,
        res: SharedRef<dyn br::VkHandle<Handle = br::vk::VkBuffer>>,
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
        res: SharedRef<dyn br::VkHandle<Handle = br::vk::VkImage>>,
        layout: br::ImageLayout,
    ) {
        self.ready_barriers
            .entry(dest_stage)
            .or_insert_with(ReadyResourceBarriers::new)
            .image
            .push((
                res,
                br::vk::VkImageSubresourceRange {
                    aspectMask: br::AspectMask::COLOR.0,
                    baseMipLevel: 0,
                    levelCount: 1,
                    baseArrayLayer: 0,
                    layerCount: 1,
                },
                layout,
            ));
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
        map: &mut BTreeMap<ResourceKey<br::vk::VkBuffer>, Range<br::vk::VkDeviceSize>>,
        k: ResourceKey<br::vk::VkBuffer>,
        new_range: Range<br::vk::VkDeviceSize>,
    ) {
        let r = map.entry(k).or_insert_with(|| new_range.clone());
        r.start = r.start.min(new_range.start);
        r.end = r.end.max(new_range.end);
    }
}
/// Sinking Commands into CommandBuffers
impl<Device: br::Device> TransferBatch<Device> {
    pub fn sink_transfer_commands(
        &self,
        r: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut>,
    ) {
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
                br::vk::VkImageSubresourceRange {
                    aspectMask: br::AspectMask::COLOR.0,
                    baseMipLevel: 0,
                    levelCount: 1,
                    baseArrayLayer: 0,
                    layerCount: 1,
                },
                l0,
                br::ImageLayout::TransferSrcOpt,
            )
        });
        let dst_barriers_i = self.org_layout_dst.iter().map(|(b, &l0)| {
            br::ImageMemoryBarrier::new(
                &b.0,
                br::vk::VkImageSubresourceRange {
                    aspectMask: br::AspectMask::COLOR.0,
                    baseMipLevel: 0,
                    levelCount: 1,
                    baseArrayLayer: 0,
                    layerCount: 1,
                },
                l0,
                br::ImageLayout::TransferDestOpt,
            )
        });
        let barriers_i: Vec<_> = src_barriers_i.chain(dst_barriers_i).collect();

        let _ = r.pipeline_barrier(
            br::PipelineStageFlags::HOST,
            br::PipelineStageFlags::TRANSFER,
            false,
            &[],
            &barriers,
            &barriers_i,
        );
        for (&(ref s, ref d), ref rs) in &self.copy_buffers {
            let _ = r.copy_buffer(&s.0, &d.0, &rs);
        }
        for (d, (dex, s, so)) in &self.init_images {
            trace!("Copying Image: extent={dex:?}");

            let _ = r.copy_buffer_to_image(
                &s,
                &d.0,
                br::ImageLayout::TransferDestOpt,
                &[br::vk::VkBufferImageCopy {
                    bufferOffset: *so,
                    bufferRowLength: 0,
                    bufferImageHeight: 0,
                    // TODO: これもいじれるようにしたほうがいいんだろうか......
                    imageSubresource: br::vk::VkImageSubresourceLayers {
                        aspectMask: br::vk::VK_IMAGE_ASPECT_COLOR_BIT,
                        mipLevel: 0,
                        baseArrayLayer: 0,
                        layerCount: 1,
                    },
                    imageOffset: br::vk::VkOffset3D { x: 0, y: 0, z: 0 },
                    imageExtent: dex.clone(),
                }],
            );
        }
    }

    pub fn sink_graphics_ready_commands(
        &self,
        r: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut>,
    ) {
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
                        &r,
                        range.clone(),
                        br::ImageLayout::TransferDestOpt,
                        *l,
                    )
                })
                .collect();
            let _ = r.pipeline_barrier(
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
pub struct DescriptorSetUpdateBatch<'r>(
    Vec<br::DescriptorSetWriteInfo<'r>>,
    Vec<br::DescriptorSetCopyInfo>,
);
impl<'r> DescriptorSetUpdateBatch<'r> {
    /// Create an Empty batch
    pub const fn new() -> Self {
        DescriptorSetUpdateBatch(Vec::new(), Vec::new())
    }

    /// Write an information to bound index and array index in destination.
    pub fn write_index(
        &mut self,
        dest: impl Into<br::vk::VkDescriptorSet>,
        bound: u32,
        array: u32,
        contents: br::DescriptorContents<'r>,
    ) -> &mut Self {
        self.0.push(
            br::DescriptorPointer::new(dest.into(), bound)
                .array_offset(array)
                .write(contents),
        );

        self
    }
    /// Write an information to bound index in destination.
    pub fn write(
        &mut self,
        dest: impl Into<br::vk::VkDescriptorSet>,
        bound: u32,
        contents: br::DescriptorContents<'r>,
    ) -> &mut Self {
        self.write_index(dest, bound, 0, contents)
    }

    /// Submit entire batches
    pub fn submit(&self, d: &impl br::Device) {
        d.update_descriptor_sets(&self.0, &self.1);
    }
}
