//! CoreUtils: Resource Allocations/Manipulations

use bedrock as br; use bedrock::traits::*;
use peridot_math::Vector2;
use num::Integer;
use std::mem::{size_of, align_of, ManuallyDrop, transmute};
use std::rc::Rc;
use std::ops::{Deref, Range};
use crate::{Texture2D, TransferBatch, TextureInitializationGroup};

fn common_alignment(flags: br::BufferUsage, mut align: u64, pd: &br::PhysicalDevice) -> u64
{
    if flags.is_uniform() { align = align.lcm(&pd.properties().limits.minUniformBufferOffsetAlignment); }
    if flags.is_storage() { align = align.lcm(&pd.properties().limits.minStorageBufferOffsetAlignment); }

    align
}

/// (size, align)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferContent
{
    Vertex(u64, u64), Index(u64, u64),
    Uniform(u64, u64), UniformTexel(u64, u64),
    Storage(u64, u64), StorageTexel(u64, u64),
    Raw(u64, u64)
}
impl BufferContent
{
    fn usage(self) -> br::BufferUsage
    {
        match self
        {
            Self::Vertex(_, _) => br::BufferUsage::VERTEX_BUFFER,
            Self::Index(_, _) => br::BufferUsage::INDEX_BUFFER,
            Self::Uniform(_, _) => br::BufferUsage::UNIFORM_BUFFER,
            Self::UniformTexel(_, _) => br::BufferUsage::UNIFORM_TEXEL_BUFFER,
            Self::Storage(_, _) => br::BufferUsage::STORAGE_BUFFER,
            Self::StorageTexel(_, _) => br::BufferUsage::STORAGE_TEXEL_BUFFER,
            Self::Raw(_, _) => br::BufferUsage(0)
        }
    }
    fn alignment(self, pd: &br::PhysicalDevice) -> u64
    {
        match self
        {
            Self::Vertex(_, a) | Self::Index(_, a) | Self::Raw(_, a) => a,
            Self::Uniform(_, a) | Self::UniformTexel(_, a) => u64::lcm(
                &a, &pd.properties().limits.minUniformBufferOffsetAlignment
            ),
            Self::Storage(_, a) | Self::StorageTexel(_, a) => u64::lcm(
                &a, &pd.properties().limits.minStorageBufferOffsetAlignment
            )
        }
    }
    fn size(self) -> u64
    {
        match self
        {
            Self::Vertex(s, _) | Self::Index(s, _) | Self::Raw(s, _) |
            Self::Uniform(s, _) | Self::UniformTexel(s, _) |
            Self::Storage(s, _) | Self::StorageTexel(s, _) => s
        }
    }
}
/// Generic Shorthands
impl BufferContent
{
    pub fn vertex<T>() -> Self { Self::Vertex(size_of::<T>() as _, align_of::<T>() as _) }
    pub fn vertices<T>(count: usize) -> Self { Self::Vertex((size_of::<T>() * count) as _, align_of::<T>() as _) }
    pub fn index<T>() -> Self { Self::Index(size_of::<T>() as _, align_of::<T>() as _) }
    pub fn indices<T>(count: usize) -> Self { Self::Index((size_of::<T>() * count) as _, align_of::<T>() as _) }
    pub fn uniform<T>() -> Self { Self::Uniform(size_of::<T>() as _, align_of::<T>() as _) }
    pub fn uniforms<T>(count: usize) -> Self { Self::Uniform((size_of::<T>() * count) as _, align_of::<T>() as _) }
    pub fn storage<T>() -> Self { Self::Storage(size_of::<T>() as _, align_of::<T>() as _) }
    pub fn storages<T>(count: usize) -> Self { Self::Storage((size_of::<T>() * count) as _, align_of::<T>() as _) }
    pub fn raw<T>() -> Self { Self::Raw(size_of::<T>() as _, align_of::<T>() as _) }
    pub fn raw_elements<T>(count: usize) -> Self { Self::Raw((size_of::<T>() * count) as _, align_of::<T>() as _) }
    pub fn uniform_texel<T>() -> Self { Self::UniformTexel(size_of::<T>() as _, align_of::<T>() as _) }
    pub fn uniform_texels<T>(count: usize) -> Self { Self::UniformTexel((size_of::<T>() * count) as _, align_of::<T>() as _) }
    pub fn storage_texel<T>() -> Self { Self::StorageTexel(size_of::<T>() as _, align_of::<T>() as _) }
    pub fn storage_texels<T>(count: usize) -> Self { Self::StorageTexel((size_of::<T>() * count) as _, align_of::<T>() as _) }
}
macro_rules! align2 { ($v: expr, $a: expr) => (($v + ($a - 1)) & !($a - 1)) }

#[derive(Clone)]
pub struct BufferPrealloc<'g>
{
    pd: &'g br::PhysicalDevice,
    usage: br::BufferUsage, offsets: Vec<u64>, total: u64, common_align: u64
}
impl<'g> BufferPrealloc<'g>
{
    pub fn new(pd: &'g br::PhysicalDevice) -> Self
    {
        BufferPrealloc
        {
            pd, usage: br::BufferUsage(0), offsets: Vec::new(), total: 0, common_align: 0
        }
    }
    pub fn build(&self, device: &br::Device) -> br::Result<br::Buffer>
    {
        br::BufferDesc::new(self.total as _, self.usage).create(device)
    }
    pub fn build_transferred(&self, device: &br::Device) -> br::Result<br::Buffer>
    {
        br::BufferDesc::new(self.total as _, self.usage.transfer_dest()).create(device)
    }
    pub fn build_upload(&self, device: &br::Device) -> br::Result<br::Buffer>
    {
        br::BufferDesc::new(self.total as _, self.usage.transfer_src()).create(device)
    }

    /// returns placement offset of the content
    pub fn add(&mut self, content: BufferContent) -> u64
    {
        self.usage |= content.usage();
        let content_align = content.alignment(self.pd);
        self.common_align = self.common_align.lcm(&content_align);
        let offs = align2!(self.total, content_align);
        self.total = offs + content.size() as u64;
        self.offsets.push(offs);
        return offs;
    }
    pub fn total_size(&self) -> u64 { self.total }

    /// Returns first offset of merged(other's) prealloc-ed block
    pub fn merge(&mut self, other: &Self) -> u64
    {
        self.common_align = self.common_align.lcm(&other.common_align);
        let offs = align2!(self.total, other.common_align);
        self.usage |= other.usage;
        self.total = offs + other.total;
        self.offsets.extend(other.offsets.iter().map(|&o| o + offs));
        return offs;
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ResourceTiling { Linear, NonLinear }
impl ResourceTiling
{
    fn is_additional_alignment_required(self, other: Self) -> bool { self != other }
}
pub enum MemoryBadgetEntry { Buffer(br::Buffer), Image(br::Image) }
pub enum MemoryBoundResource { Buffer(Buffer), Image(Image) }
impl From<br::Buffer> for MemoryBadgetEntry {
    fn from(v: br::Buffer) -> Self { MemoryBadgetEntry::Buffer(v) }
}
impl From<br::Image> for MemoryBadgetEntry {
    fn from(v: br::Image) -> Self { MemoryBadgetEntry::Image(v) }
}
impl MemoryBadgetEntry
{
    fn tiling(&self) -> ResourceTiling
    {
        match self
        {
            MemoryBadgetEntry::Buffer(_) => ResourceTiling::Linear,
            // Note: Peridotが扱うImageは全てNonLinearTiling
            MemoryBadgetEntry::Image(_) => ResourceTiling::NonLinear
        }
    }
}
impl MemoryBoundResource
{
    pub fn unwrap_buffer(self) -> Buffer
    {
        match self { MemoryBoundResource::Buffer(b) => b, _ => panic!("Not a buffer") }
    }
    pub fn unwrap_image(self) -> Image
    {
        match self { MemoryBoundResource::Image(b) => b, _ => panic!("Not an image") }
    }
}

pub struct MemoryBadget<'g>
{
    pd: &'g br::PhysicalDevice, entries: Vec<(MemoryBadgetEntry, u64)>, total_size: u64,
    memory_type_bitmask: u32, last_resource_tiling: Option<ResourceTiling>
}
impl<'g> MemoryBadget<'g>
{
    pub fn new(pd: &'g br::PhysicalDevice) -> Self
    {
        MemoryBadget
        {
            pd, entries: Vec::new(), total_size: 0, memory_type_bitmask: 0, last_resource_tiling: None
        }
    }
    pub fn add<V: Into<MemoryBadgetEntry> + br::MemoryBound>(&mut self, v: V) -> u64
    {
        let req = v.requirements();
        let new_offset = align2!(self.total_size, req.alignment);
        let entry = v.into();
        let require_additional_align =
            self.last_resource_tiling.map_or(false, |t| t.is_additional_alignment_required(entry.tiling()));
        let new_offset = if require_additional_align
        {
            align2!(new_offset, self.pd.properties().limits.bufferImageGranularity)
        }
        else { new_offset };
        self.last_resource_tiling = Some(entry.tiling());
        self.entries.push((entry, new_offset));
        self.total_size = new_offset + req.size;
        self.memory_type_bitmask |= req.memoryTypeBits;
        return new_offset;
    }
    pub fn memory_type_index_mask(&self) -> u32 { self.memory_type_bitmask }
    pub fn alloc(self, device: &br::Device, memory_type_index: u32) -> br::Result<Vec<MemoryBoundResource>>
    {
        info!(target: "peridot", "Allocating Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, memory_type_index, self.memory_type_bitmask);
        let mem: Rc<_> = br::DeviceMemory::allocate(device, self.total_size as _, memory_type_index)?.into();

        self.entries.into_iter().map(|(x, o)| match x
        {
            MemoryBadgetEntry::Buffer(b) => Buffer::bound(b, &mem, o as _).map(MemoryBoundResource::Buffer),
            MemoryBadgetEntry::Image(b) => Image::bound(b, &mem, o as _).map(MemoryBoundResource::Image)
        }).collect()
    }
}

/// Image Format
#[repr(i32)] #[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PixelFormat
{
    RGB24 = br::vk::VK_FORMAT_R8G8B8_UNORM,
    RGBA32 = br::vk::VK_FORMAT_R8G8B8A8_UNORM,
    BGR24 = br::vk::VK_FORMAT_B8G8R8_UNORM,
    BGRA32 = br::vk::VK_FORMAT_B8G8R8A8_UNORM
}
impl PixelFormat
{
    /// Bits per pixel for each format enums
    pub fn bpp(self) -> usize
    {
        match self
        {
            Self::RGBA32 | Self::BGRA32 => 32,
            Self::RGB24 | Self::BGR24 => 24
        }
    }
}
/// Describing Image Pixel Data
pub struct PixelData 
{
    pub pixels: Vec<u8>, pub size: Vector2<u32>,
    pub format: PixelFormat, pub stride: usize
}
impl PixelData
{
    pub fn u8_pixels(&self) -> &[u8] { &self.pixels }
}

/// A refcounted memory object.
/// Convertable from `br::DeviceMemory` via `Into::into`
pub type Memory = Rc<br::DeviceMemory>;
/// A refcounted buffer object bound with a memory object.
#[derive(Clone)]
pub struct Buffer(Rc<br::Buffer>, Memory, u64);
/// A refcounted image object bound with a memory object.
#[derive(Clone)]
pub struct Image(Rc<br::Image>, Memory, u64);
impl Buffer
{
    pub fn bound(b: br::Buffer, mem: &Memory, offset: u64) -> br::Result<Self>
    {
        b.bind(mem, offset as _).map(|_| Buffer(b.into(), mem.clone(), offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &Memory { &self.1 }

    pub fn map(&self, size: u64) -> br::Result<br::MappedMemoryRange>
    {
        self.1.map(self.2 as _ .. (self.2 + size) as _)
    }
    pub unsafe fn unmap(&self) { self.1.unmap() }
    pub fn guard_map<F: FnOnce(&br::MappedMemoryRange) -> R, R>(&self, size: u64, f: F) -> br::Result<R>
    {
        Ok(f(&AutocloseMappedMemoryRange(&self.1, ManuallyDrop::new(self.map(size)?))))
    }
}
impl Image
{
    pub fn bound(r: br::Image, mem: &Memory, offset: u64) -> br::Result<Self>
    {
        r.bind(mem, offset as _).map(|_| Image(r.into(), mem.clone(), offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &Memory { &self.1 }

    pub fn format(&self) -> PixelFormat { unsafe { transmute(self.0.format()) } }
}
impl Deref for Buffer
{
    type Target = br::Buffer; fn deref(&self) -> &br::Buffer { &self.0 }
}
impl Deref for Image
{
    type Target = br::Image; fn deref(&self) -> &br::Image { &self.0 }
}
impl br::VkHandle for Buffer
{
    type Handle = <br::Buffer as br::VkHandle>::Handle; fn native_ptr(&self) -> Self::Handle { self.0.native_ptr() }
}
impl br::VkHandle for Image
{
    type Handle = <br::Image as br::VkHandle>::Handle; fn native_ptr(&self) -> Self::Handle { self.0.native_ptr() }
}

pub struct AutocloseMappedMemoryRange<'m>(&'m br::DeviceMemory, ManuallyDrop<br::MappedMemoryRange<'m>>);
impl<'m> Deref for AutocloseMappedMemoryRange<'m>
{
    type Target = br::MappedMemoryRange<'m>; fn deref(&self) -> &Self::Target { &self.1 }
}
impl<'m> Drop for AutocloseMappedMemoryRange<'m>
{
    fn drop(&mut self)
    {
        unsafe
        {
            ManuallyDrop::drop(&mut self.1);
            self.0.unmap();
        }
    }
}

/// Describing the type that can be used as initializer of `FixedBuffer`s
pub trait FixedBufferInitializer
{
    /// Setup memory data in staging buffer
    fn stage_data(&mut self, m: &br::MappedMemoryRange);
    fn buffer_graphics_ready(&self, tfb: &mut TransferBatch, buf: &Buffer, range: Range<u64>);
}
/// The Fix-sized buffers and textures manager
pub struct FixedMemory
{
    /// Device accessible buffer object
    pub buffer: (Buffer, u64),
    /// Host buffer staging per-frame mutable data
    pub mut_buffer: (Buffer, u64),
    /// The placement offset of mut_buffer data in buffer
    pub mut_buffer_placement: u64,
    /// Textures
    pub textures: Vec<Texture2D>
}
impl FixedMemory
{
    /// Initialize a FixedMemory using preallocation structures
    pub fn new<'g, I: FixedBufferInitializer + ?Sized>(
        device: &'g br::Device, pd: &'g br::PhysicalDevice,
        mut prealloc: BufferPrealloc<'g>,
        prealloc_mut: BufferPrealloc<'g>,
        textures: TextureInitializationGroup<'g>,
        initializer: &mut I, tfb: &mut TransferBatch) -> br::Result<Self>
    {
        let mut_buffer = prealloc_mut.build_upload(device)?;
        let mut p_bufferdata_prealloc = prealloc.clone();
        let imm_buffer_size = p_bufferdata_prealloc.total_size();
        let mut_buffer_placement = p_bufferdata_prealloc.merge(&prealloc_mut);
        let buffer = p_bufferdata_prealloc.build_transferred(device)?;

        let tex_preallocs = textures.prealloc(&mut prealloc)?;
        let stg_buffer_fullsize = prealloc.total_size();
        let stg_buffer = prealloc.build_upload(device)?;

        let (mut mb, mut mb_mut) = (MemoryBadget::new(pd), MemoryBadget::new(pd));
        mb.add(buffer);
        mb_mut.add(mut_buffer);
        let (textures, mut bufs) = tex_preallocs.alloc_and_instantiate(mb, device, pd)?;
        let buffer = bufs.pop().expect("objectless").unwrap_buffer();
        let pd_memory_props = pd.memory_properties();
        let mb_mt_index = pd_memory_props.find_host_visible_index(mb_mut.memory_type_index_mask())
            .expect("no matching memory type index for mut buffer");
        let mut_buffer = mb_mut.alloc(device, mb_mt_index)?.pop().expect("objectless").unwrap_buffer();
        let mut mb_stg = MemoryBadget::new(pd);
        mb_stg.add(stg_buffer);
        let stg_mt_index = pd_memory_props.find_host_visible_index(mb_stg.memory_type_index_mask())
            .expect("no matching memory type index for stg buffer");
        let stg_buffer = mb_stg.alloc(device, stg_mt_index)?.pop().expect("objectless").unwrap_buffer();

        stg_buffer.guard_map(stg_buffer_fullsize, |m| { textures.stage_data(m); initializer.stage_data(m); })?;

        textures.copy_from_stage_batches(tfb, &stg_buffer);
        tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, imm_buffer_size);
        initializer.buffer_graphics_ready(tfb, &buffer, 0 .. imm_buffer_size);

        Ok(FixedMemory
        {
            buffer: (buffer, imm_buffer_size), mut_buffer: (mut_buffer, prealloc_mut.total_size()),
            mut_buffer_placement,
            textures: textures.into_textures()
        })
    }

    pub fn range_in_mut_buffer<T>(&self, r: Range<T>) -> Range<T> where
        T: std::ops::Add<Output = T> + std::convert::TryFrom<u64> + Copy
    {
        match T::try_from(self.mut_buffer_placement)
        {
            Ok(p) => r.start + p .. r.end + p,
            Err(_) => panic!("Overflowing Placement offset")
        }
    }
}
