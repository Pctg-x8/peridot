//! CoreUtils: Operation Batches

use bedrock as br; use br::traits::*;
use br::vk::VkBufferCopy;
use std::cmp::{PartialEq, Eq, PartialOrd, Ord, Ordering};
use std::hash::{Hash, Hasher};
use std::collections::{HashMap, BTreeMap};
use std::ops::Range;
use crate::resource::{Buffer, Image};

#[derive(Clone)] pub struct ResourceKey<T: br::VkHandle>(T);
impl PartialEq for ResourceKey<Buffer>
{
    fn eq(&self, other: &Self) -> bool { (self.0.native_ptr() as u64).eq(&(other.0.native_ptr() as u64)) }
}
impl PartialEq for ResourceKey<Image>
{
    fn eq(&self, other: &Self) -> bool { (self.0.native_ptr() as u64).eq(&(other.0.native_ptr() as u64)) }
}
impl PartialOrd for ResourceKey<Buffer>
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>
    {
        (self.0.native_ptr() as u64).partial_cmp(&(other.0.native_ptr() as u64))
    }
}
impl PartialOrd for ResourceKey<Image>
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>
    {
        (self.0.native_ptr() as u64).partial_cmp(&(other.0.native_ptr() as u64))
    }
}
impl Eq for ResourceKey<Buffer> {}
impl Ord for ResourceKey<Buffer>
{
    fn cmp(&self, other: &Self) -> Ordering
    {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl Eq for ResourceKey<Image> {}
impl Ord for ResourceKey<Image>
{
    fn cmp(&self, other: &Self) -> Ordering
    {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl Hash for ResourceKey<Buffer> { fn hash<H: Hasher>(&self, hasher: &mut H) { self.0.native_ptr().hash(hasher) } }
impl Hash for ResourceKey<Image> { fn hash<H: Hasher>(&self, hasher: &mut H) { self.0.native_ptr().hash(hasher) } }
pub struct ReadyResourceBarriers
{
    buffer: Vec<(Buffer, Range<u64>, br::vk::VkAccessFlags)>,
    image: Vec<(Image, br::ImageSubresourceRange, br::ImageLayout)>
}
impl ReadyResourceBarriers
{
    fn new() -> Self { ReadyResourceBarriers { buffer: Vec::new(), image: Vec::new() } }
}
pub struct TransferBatch
{
    barrier_range_src: BTreeMap<ResourceKey<Buffer>, Range<u64>>,
    barrier_range_dst: BTreeMap<ResourceKey<Buffer>, Range<u64>>,
    org_layout_src: BTreeMap<ResourceKey<Image>, br::ImageLayout>,
    org_layout_dst: BTreeMap<ResourceKey<Image>, br::ImageLayout>,
    copy_buffers: HashMap<(ResourceKey<Buffer>, ResourceKey<Buffer>), Vec<VkBufferCopy>>,
    init_images: BTreeMap<ResourceKey<Image>, (Buffer, u64)>,
    ready_barriers: BTreeMap<br::PipelineStageFlags, ReadyResourceBarriers>
}
impl TransferBatch
{
    pub fn new() -> Self
    {
        TransferBatch
        {
            barrier_range_src: BTreeMap::new(), barrier_range_dst: BTreeMap::new(),
            org_layout_src: BTreeMap::new(), org_layout_dst: BTreeMap::new(),
            copy_buffers: HashMap::new(), init_images: BTreeMap::new(),
            ready_barriers: BTreeMap::new()
        }
    }
    pub fn add_copying_buffer(&mut self, src: (&Buffer, u64), dst: (&Buffer, u64), bytes: u64)
    {
        trace!("Registering COPYING-BUFFER: ({}, {}) -> {} bytes", src.1, dst.1, bytes);
        let (sk, dk) = (ResourceKey(src.0.clone()), ResourceKey(dst.0.clone()));
        Self::update_barrier_range_for(&mut self.barrier_range_src, sk.clone(), src.1 .. src.1 + bytes);
        Self::update_barrier_range_for(&mut self.barrier_range_dst, dk.clone(), dst.1 .. dst.1 + bytes);
        self.copy_buffers.entry((sk, dk)).or_insert_with(Vec::new)
            .push(VkBufferCopy { srcOffset: src.1 as _, dstOffset: dst.1 as _, size: bytes as _ })
    }
    pub fn add_mirroring_buffer(&mut self, src: &Buffer, dst: &Buffer, offset: u64, bytes: u64)
    {
        self.add_copying_buffer((src, offset), (dst, offset), bytes);
    }
    pub fn init_image_from(&mut self, dest: &Image, src: (&Buffer, u64))
    {
        self.init_images.insert(ResourceKey(dest.clone()), (src.0.clone(), src.1));
        let size = (dest.size().0 * dest.size().1) as u64 * (dest.format().bpp() >> 3) as u64;
        Self::update_barrier_range_for(&mut self.barrier_range_src, ResourceKey(src.0.clone()), src.1 .. src.1 + size);
        self.org_layout_dst.insert(ResourceKey(dest.clone()), br::ImageLayout::Preinitialized);
    }

    pub fn add_buffer_graphics_ready(&mut self, dest_stage: br::PipelineStageFlags,
        res: &Buffer, byterange: Range<u64>, access_grants: br::vk::VkAccessFlags)
    {
        self.ready_barriers.entry(dest_stage).or_insert_with(ReadyResourceBarriers::new)
            .buffer.push((res.clone(), byterange, access_grants));
    }
    pub fn add_image_graphics_ready(&mut self, dest_stage: br::PipelineStageFlags,
        res: &Image, layout: br::ImageLayout)
    {
        self.ready_barriers.entry(dest_stage).or_insert_with(ReadyResourceBarriers::new)
            .image.push((res.clone(), br::ImageSubresourceRange::color(0, 0), layout));
    }
    pub fn is_empty(&self) -> bool { self.copy_buffers.is_empty() }
    
    fn update_barrier_range_for(map: &mut BTreeMap<ResourceKey<Buffer>, Range<u64>>,
        k: ResourceKey<Buffer>, new_range: Range<u64>)
    {
        let r = map.entry(k).or_insert_with(|| new_range.clone());
        r.start = r.start.min(new_range.start);
        r.end = r.end.max(new_range.end);
    }
}
/// Sinking Commands into CommandBuffers
impl TransferBatch
{
    pub fn sink_transfer_commands(&self, r: &mut br::CmdRecord)
    {
        let src_barriers = self.barrier_range_src.iter()
            .map(|(b, r)| br::BufferMemoryBarrier::new(
                &b.0, r.start as _ .. r.end as _, br::AccessFlags::HOST.write, br::AccessFlags::TRANSFER.read));
        let dst_barriers = self.barrier_range_dst.iter()
            .map(|(b, r)| br::BufferMemoryBarrier::new(
                &b.0, r.start as _ .. r.end as _, 0, br::AccessFlags::TRANSFER.write));
        let barriers: Vec<_> = src_barriers.chain(dst_barriers).collect();
        let src_barriers_i = self.org_layout_src.iter().map(|(b, &l0)| br::ImageMemoryBarrier::new(
            &br::ImageSubref::color(&b.0, 0, 0), l0, br::ImageLayout::TransferSrcOpt
        ));
        let dst_barriers_i = self.org_layout_dst.iter().map(|(b, &l0)| br::ImageMemoryBarrier::new(
            &br::ImageSubref::color(&b.0, 0, 0), l0, br::ImageLayout::TransferDestOpt
        ));
        let barriers_i: Vec<_> = src_barriers_i.chain(dst_barriers_i).collect();
        
        r.pipeline_barrier(br::PipelineStageFlags::HOST, br::PipelineStageFlags::TRANSFER, false,
            &[], &barriers, &barriers_i);
        for (&(ref s, ref d), ref rs) in &self.copy_buffers { r.copy_buffer(&s.0, &d.0, &rs); }
        for (d, s) in &self.init_images
        {
            trace!("Copying Image: extent={:?}", br::vk::VkExtent3D::from(d.0.size().clone()));
            r.copy_buffer_to_image(&s.0, &d.0, br::ImageLayout::TransferDestOpt, &[br::vk::VkBufferImageCopy {
                bufferOffset: s.1, bufferRowLength: 0, bufferImageHeight: 0,
                imageSubresource: br::vk::VkImageSubresourceLayers::default(),
                imageOffset: br::vk::VkOffset3D { x: 0, y: 0, z: 0 },
                imageExtent: d.0.size().clone().into()
            }]);
        }
    }
    pub fn sink_graphics_ready_commands(&self, r: &mut br::CmdRecord)
    {
        for (&stg, &ReadyResourceBarriers { ref buffer, ref image, .. }) in &self.ready_barriers
        {
            let buf_barriers: Vec<_> = buffer.iter()
                .map(|&(ref r, ref br, a)| br::BufferMemoryBarrier::new(&r, br.start as _ .. br.end as _,
                    br::AccessFlags::TRANSFER.read, a)).collect();
            let img_barriers: Vec<_> = image.iter()
                .map(|(r, range, l)| br::ImageMemoryBarrier::new_raw(&r, range, br::ImageLayout::TransferDestOpt, *l))
                .collect();
            r.pipeline_barrier(br::PipelineStageFlags::TRANSFER, stg, false,
                &[], &buf_barriers, &img_barriers);
        }
    }
}

/// Batching mechanism for Updating Descriptor Sets
pub struct DescriptorSetUpdateBatch(Vec<br::DescriptorSetWriteInfo>, Vec<br::DescriptorSetCopyInfo>);
impl DescriptorSetUpdateBatch
{
    /// Create an Empty batch
    pub fn new() -> Self { DescriptorSetUpdateBatch(Vec::new(), Vec::new()) }
    /// Write an information to bound index and array index in destination.
    pub fn write_index(&mut self, dest: br::vk::VkDescriptorSet,
        bound: u32, array: u32, info: br::DescriptorUpdateInfo) -> &mut Self
    {
        self.0.push(br::DescriptorSetWriteInfo(dest, bound, array, info));
        return self;
    }
    /// Write an information to bound index in destination.
    pub fn write(&mut self, dest: br::vk::VkDescriptorSet, bound: u32, info: br::DescriptorUpdateInfo) -> &mut Self
    {
        self.write_index(dest, bound, 0, info)
    }
    /// Submit entire batches
    pub fn submit(&self, d: &br::Device) { d.update_descriptor_sets(&self.0, &self.1); }
}
