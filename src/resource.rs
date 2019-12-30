use bedrock as br; use self::br::traits::*;
use super::*;
use std::ops::{Deref, Range};
use std::mem::{size_of, transmute, align_of};
use num::Integer;

/// (size, align)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferContent
{
    Vertex(u64, u64), Index(u64, u64), Uniform(u64, u64), Raw(u64, u64), UniformTexel(u64, u64)
}
impl BufferContent
{
    fn usage(&self, src: br::BufferUsage) -> br::BufferUsage
    {
        use self::BufferContent::*;

        match *self
        {
            Vertex(_, _) => src.vertex_buffer(),
            Index(_, _) => src.index_buffer(),
            Uniform(_, _) => src.uniform_buffer(),
            Raw(_, _) => src,
            UniformTexel(_, _) => src.uniform_texel_buffer()
        }
    }
    fn alignment(&self, pd: &br::PhysicalDevice) -> u64
    {
        use self::BufferContent::*;

        match *self
        {
            Vertex(_, a) | Index(_, a) | Raw(_, a) => a,
            Uniform(_, a) | UniformTexel(_, a) =>
                u64::lcm(&pd.properties().limits.minUniformBufferOffsetAlignment as _, &a),
        }
    }
    fn size(&self) -> u64
    {
        use self::BufferContent::*;

        match *self
        {
            Vertex(v, _) | Index(v, _) | Uniform(v, _) | Raw(v, _) | UniformTexel(v, _) => v
        }
    }

    /// Generic Shorthands
    pub fn vertex<T>() -> Self
    {
        BufferContent::Vertex(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn vertices<T>(count: usize) -> Self
    {
        BufferContent::Vertex(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }
    pub fn index<T>()  -> Self
    {
        BufferContent::Index(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn indices<T>(count: usize) -> Self
    {
        BufferContent::Index(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }
    pub fn uniform<T>() -> Self
    {
        BufferContent::Uniform(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn uniform_dynarray<T>(count: usize) -> Self
    {
        BufferContent::Uniform(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }
    pub fn uniform_texel<T>() -> Self
    {
        BufferContent::UniformTexel(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn uniform_texel_dynarray<T>(count: usize) -> Self
    {
        BufferContent::UniformTexel(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }
}
macro_rules! align2 {
    ($v: expr, $a: expr) => (($v + ($a - 1)) & !($a - 1))
}
#[derive(Clone)]
pub struct BufferPrealloc<'g>
{
    g: &'g Graphics, usage: br::BufferUsage, offsets: Vec<u64>, total: u64, common_align: u64
}
impl<'g> BufferPrealloc<'g>
{
    pub fn new(g: &'g Graphics) -> Self
    {
        BufferPrealloc { g, usage: br::BufferUsage(0), offsets: Vec::new(), total: 0, common_align: 1 }
    }
    pub fn build(&self) -> br::Result<br::Buffer>
    {
        br::BufferDesc::new(self.total as _, self.usage).create(&self.g.device)
    }
    pub fn build_transferred(&self) -> br::Result<br::Buffer>
    {
        br::BufferDesc::new(self.total as _, self.usage.transfer_dest()).create(&self.g.device)
    }
    pub fn build_upload(&self) -> br::Result<br::Buffer>
    {
        br::BufferDesc::new(self.total as _, self.usage.transfer_src()).create(&self.g.device)
    }

    pub fn add(&mut self, content: BufferContent) -> u64
    {
        self.usage = content.usage(self.usage);
        let content_align = content.alignment(&self.g.adapter);
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

pub struct MemoryBadget<'g>
{
    g: &'g Graphics, entries: Vec<(MemoryBadgetEntry, u64)>, total_size: u64,
    memory_type_bitmask: u32, last_resource_tiling: Option<ResourceTiling>
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
impl<'g> MemoryBadget<'g>
{
    pub fn new(g: &'g Graphics) -> Self
    {
        MemoryBadget
        {
            g, entries: Vec::new(), total_size: 0, memory_type_bitmask: 0, last_resource_tiling: None
        }
    }
    pub fn add<V: Into<MemoryBadgetEntry> + br::MemoryBound>(&mut self, v: V) -> u64
    {
        let req = v.requirements();
        let new_offset = align2!(self.total_size, req.alignment);
        let entry = v.into();
        let new_offset =
            if self.last_resource_tiling.map_or(false, |t| t.is_additional_alignment_required(entry.tiling()))
            {
                align2!(new_offset, self.g.adapter.properties().limits.bufferImageGranularity)
            }
            else { new_offset };
        self.last_resource_tiling = Some(entry.tiling());
        self.entries.push((entry, new_offset));
        self.total_size = new_offset + req.size;
        self.memory_type_bitmask |= req.memoryTypeBits;
        return new_offset;
    }
    pub fn alloc(self) -> br::Result<Vec<MemoryBoundResource>>
    {
        let mt = self.g.memory_type_index_for(br::MemoryPropertyFlags::DEVICE_LOCAL, self.memory_type_bitmask)
            .expect("No Device-Local Memory");
        info!(target: "peridot", "Allocating Device Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt, self.memory_type_bitmask);
        let mem: Rc<_> = br::DeviceMemory::allocate(&self.g.device, self.total_size as _, mt)?.into();

        self.entries.into_iter().map(|(x, o)| match x
        {
            MemoryBadgetEntry::Buffer(b) => Buffer::bound(b, &mem, o as _).map(MemoryBoundResource::Buffer),
            MemoryBadgetEntry::Image(b) => Image::bound(b, &mem, o as _).map(MemoryBoundResource::Image)
        }).collect()
    }
    pub fn alloc_upload(self) -> br::Result<Vec<MemoryBoundResource>>
    {
        let mt = self.g.memory_type_index_for(br::MemoryPropertyFlags::HOST_VISIBLE.host_coherent(),
            self.memory_type_bitmask).expect("No Host-Visible memory");
        info!(target: "peridot", "Allocating Uploading Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt, self.memory_type_bitmask);
        let mem: Rc<_> = br::DeviceMemory::allocate(&self.g.device, self.total_size as _, mt)?.into();
        
        self.entries.into_iter().map(|(x, o)| match x
        {
            MemoryBadgetEntry::Buffer(b) => Buffer::bound(b, &mem, o as _).map(MemoryBoundResource::Buffer),
            MemoryBadgetEntry::Image(b) => Image::bound(b, &mem, o as _).map(MemoryBoundResource::Image)
        }).collect()
    }
}

use std::mem::ManuallyDrop;
pub struct AutocloseMappedMemoryRange<'m>(&'m br::DeviceMemory, ManuallyDrop<br::MappedMemoryRange<'m>>);
impl<'m> Deref for AutocloseMappedMemoryRange<'m> {
    type Target = br::MappedMemoryRange<'m>; fn deref(&self) -> &Self::Target { &self.1 }
}
impl<'m> Drop for AutocloseMappedMemoryRange<'m> {
    fn drop(&mut self) {
        unsafe {
            ManuallyDrop::drop(&mut self.1);
            self.0.unmap();
        }
    }
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
impl Buffer {
    pub fn bound(b: br::Buffer, mem: &Memory, offset: u64) -> br::Result<Self> {
        b.bind(mem, offset as _).map(|_| Buffer(b.into(), mem.clone(), offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &Memory { &self.1 }

    pub fn map(&self, size: u64) -> br::Result<br::MappedMemoryRange> {
        self.1.map(self.2 as _ .. (self.2 + size) as _)
    }
    pub unsafe fn unmap(&self) { self.1.unmap() }
    pub fn guard_map<F: FnOnce(&br::MappedMemoryRange) -> R, R>(&self, size: u64, f: F) -> br::Result<R> {
        Ok(f(&AutocloseMappedMemoryRange(&self.1, ManuallyDrop::new(self.map(size)?))))
    }
}
impl Image {
    pub fn bound(r: br::Image, mem: &Memory, offset: u64) -> br::Result<Self> {
        r.bind(mem, offset as _).map(|_| Image(r.into(), mem.clone(), offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &Memory { &self.1 }

    pub fn format(&self) -> PixelFormat { unsafe { transmute(self.0.format()) } }
}
impl Deref for Buffer {
    type Target = br::Buffer; fn deref(&self) -> &br::Buffer { &self.0 }
}
impl Deref for Image {
    type Target = br::Image; fn deref(&self) -> &br::Image { &self.0 }
}
impl br::VkHandle for Buffer {
    type Handle = <br::Buffer as br::VkHandle>::Handle; fn native_ptr(&self) -> Self::Handle { self.0.native_ptr() }
}
impl br::VkHandle for Image {
    type Handle = <br::Image as br::VkHandle>::Handle; fn native_ptr(&self) -> Self::Handle { self.0.native_ptr() }
}

#[derive(Clone, Copy)] #[repr(i32)]
pub enum PixelFormat {
    RGBA32 = br::vk::VK_FORMAT_R8G8B8A8_UNORM,
    BGRA32 = br::vk::VK_FORMAT_B8G8R8A8_UNORM,
    RGB24 = br::vk::VK_FORMAT_R8G8B8_UNORM,
    BGR24 = br::vk::VK_FORMAT_B8G8R8_UNORM
}
impl PixelFormat {
    /// Bits per pixel for each format enums
    pub fn bpp(self) -> usize
    {
        match self
        {
            PixelFormat::RGBA32 | PixelFormat::BGRA32 => 32,
            PixelFormat::RGB24 | PixelFormat::BGR24 => 24
        }
    }
}

pub struct Texture2D(br::ImageView, Image);
impl Texture2D
{
    pub fn init(g: &br::Device, size: &math::Vector2<u32>, format: PixelFormat, prealloc: &mut BufferPrealloc)
        -> br::Result<(br::Image, u64)>
    {
        let idesc = br::ImageDesc::new(size, format as _, br::ImageUsage::SAMPLED.transfer_dest(),
            br::ImageLayout::Preinitialized);
        let bytes_per_pixel = (format.bpp() >> 3) as u64;
        let pixels_stg = prealloc.add(
            BufferContent::Raw((size.x() * size.y()) as u64 * bytes_per_pixel, bytes_per_pixel));
        return idesc.create(g).map(|o| (o, pixels_stg));
    }
    pub fn new(img: Image) -> br::Result<Self>
    {
        let (fmt, cmap) = match img.format()
        {
            PixelFormat::RGB24 =>
            (
                Some(PixelFormat::RGBA32 as _),
                br::ComponentMapping(br::ComponentSwizzle::Identity, br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity, br::ComponentSwizzle::One)
            ),
            PixelFormat::BGR24 =>
            (
                Some(PixelFormat::BGRA32 as _),
                br::ComponentMapping(br::ComponentSwizzle::Identity, br::ComponentSwizzle::Identity,
                    br::ComponentSwizzle::Identity, br::ComponentSwizzle::One)
            ),
            _ => (None, br::ComponentMapping::default())
        };

        img.create_view(fmt, None, &cmap, &br::ImageSubresourceRange::color(0, 0)).map(|v| Texture2D(v, img))
    }

    pub fn image(&self) -> &Image { &self.1 }
}
impl Deref for Texture2D {
    type Target = br::ImageView;
    fn deref(&self) -> &br::ImageView { &self.0 }
}

/// Low Dynamic Range(8bit colors) image asset
pub trait LDRImageAsset
{
    fn into_pixel_data_info(self) -> DecodedPixelData;
}
impl LDRImageAsset for BMP { fn into_pixel_data_info(self) -> DecodedPixelData { self.0 } }
impl LDRImageAsset for PNG { fn into_pixel_data_info(self) -> DecodedPixelData { self.0 } }
impl LDRImageAsset for TGA { fn into_pixel_data_info(self) -> DecodedPixelData { self.0 } }
impl LDRImageAsset for TIFF { fn into_pixel_data_info(self) -> DecodedPixelData { self.0 } }
impl LDRImageAsset for WebP { fn into_pixel_data_info(self) -> DecodedPixelData { self.0 } }

/// Stg1. Group what textures are being initialized
pub struct TextureInitializationGroup<'g>(&'g br::Device, Vec<DecodedPixelData>);
/// Stg2. Describes where textures are being staged
pub struct TexturePreallocatedGroup(Vec<(DecodedPixelData, u64)>, Vec<br::Image>);
/// Stg3. Describes where textures are being staged, allocated and bound their memory
pub struct TextureInstantiatedGroup(Vec<(DecodedPixelData, u64)>, Vec<Texture2D>);

impl<'g> TextureInitializationGroup<'g>
{
    pub fn new(device: &'g br::Device) -> Self { TextureInitializationGroup(device, Vec::new()) }
    pub fn add<A: LDRImageAsset>(&mut self, asset: A) -> usize
    {
        let index = self.1.len();
        self.1.push(asset.into_pixel_data_info());
        return index;
    }
    pub fn prealloc(self, prealloc: &mut BufferPrealloc) -> br::Result<TexturePreallocatedGroup>
    {
        let (mut images, mut stage_info) = (Vec::with_capacity(self.1.len()), Vec::with_capacity(self.1.len()));
        for pd in self.1 {
            let (o, offs) = Texture2D::init(self.0, &pd.size, pd.format(), prealloc)?;
            images.push(o); stage_info.push((pd, offs));
        }
        return Ok(TexturePreallocatedGroup(stage_info, images));
    }
}
impl TexturePreallocatedGroup
{
    pub fn alloc_and_instantiate(self, mut badget: MemoryBadget)
        -> br::Result<(TextureInstantiatedGroup, Vec<MemoryBoundResource>)>
    {
        let img_count = self.1.len();
        for isrc in self.1 { badget.add(isrc); }
        let mut resources = badget.alloc()?;
        let textures = resources.drain(resources.len() - img_count ..)
            .map(|r| Texture2D::new(r.unwrap_image())).collect::<Result<Vec<_>, _>>()?;

        return Ok((TextureInstantiatedGroup(self.0, textures), resources));
    }
}
impl TextureInstantiatedGroup
{
    /// Copy texture pixels into a staging buffer.
    pub fn stage_data(&self, mr: &br::MappedMemoryRange)
    {
        trace!("Staging Texture Data...");
        for &(ref pd, offs) in &self.0
        {
            let s = unsafe
            {
                mr.slice_mut(offs as _, (pd.size.x() * pd.size.y()) as usize * (pd.format().bpp() >> 3) as usize)
            };
            s.copy_from_slice(pd.u8_pixels());
        }
    }
    /// Push transferring operations into a batcher.
    pub fn copy_from_stage_batches(&self, tb: &mut TransferBatch, stgbuf: &Buffer)
    {
        for (t, &(_, offs)) in self.1.iter().zip(self.0.iter())
        {
            tb.init_image_from(t.image(), (stgbuf, offs));
            tb.add_image_graphics_ready(br::PipelineStageFlags::FRAGMENT_SHADER, t.image(),
                br::ImageLayout::ShaderReadOnlyOpt);
        }
    }

    /// Returns a list of Texture2D.
    pub fn into_textures(self) -> Vec<Texture2D> { return self.1; }
}
impl Deref for TextureInstantiatedGroup
{
    type Target = [Texture2D];
    fn deref(&self) -> &[Texture2D] { &self.1 }
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
        g: &'g Graphics,
        mut prealloc: BufferPrealloc<'g>,
        prealloc_mut: BufferPrealloc<'g>,
        textures: TextureInitializationGroup<'g>,
        initializer: &mut I, tfb: &mut TransferBatch) -> br::Result<Self>
    {
        let mut_buffer = prealloc_mut.build_upload()?;
        let mut p_bufferdata_prealloc = prealloc.clone();
        let imm_buffer_size = p_bufferdata_prealloc.total_size();
        let mut_buffer_placement = p_bufferdata_prealloc.merge(&prealloc_mut);
        let buffer = p_bufferdata_prealloc.build_transferred()?;

        let tex_preallocs = textures.prealloc(&mut prealloc)?;
        let stg_buffer_fullsize = prealloc.total_size();
        let stg_buffer = prealloc.build_upload()?;

        let (mut mb, mut mb_mut) = (MemoryBadget::new(g), MemoryBadget::new(g));
        mb.add(buffer);
        mb_mut.add(mut_buffer);
        let (textures, mut bufs) = tex_preallocs.alloc_and_instantiate(mb)?;
        let buffer = bufs.pop().expect("objectless").unwrap_buffer();
        let mut_buffer = mb_mut.alloc_upload()?.pop().expect("objectless").unwrap_buffer();
        let mut mb_stg = MemoryBadget::new(g);
        mb_stg.add(stg_buffer);
        let stg_buffer = mb_stg.alloc_upload()?.pop().expect("objectless").unwrap_buffer();

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
