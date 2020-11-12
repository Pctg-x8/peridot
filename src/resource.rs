use bedrock as br; use self::br::traits::*;
use super::*;
use std::ops::{Deref, Range};
use std::mem::{size_of, transmute, align_of};
use num::Integer;

/// (size, align)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferContent
{
    Vertex(u64, u64), Index(u64, u64), Uniform(u64, u64), Raw(u64, u64), UniformTexel(u64, u64),
    Storage(u64, u64), StorageTexel(u64, u64)
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
            UniformTexel(_, _) => src.uniform_texel_buffer(),
            Storage(_, _) => src.storage_buffer(),
            StorageTexel(_, _) => src.storage_texel_buffer()
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
            Storage(_, a) | StorageTexel(_, a) =>
                u64::lcm(&pd.properties().limits.minStorageBufferOffsetAlignment as _, &a)
        }
    }
    fn size(&self) -> u64
    {
        use self::BufferContent::*;

        match *self
        {
            Vertex(v, _) | Index(v, _) | Uniform(v, _) | Raw(v, _) | UniformTexel(v, _) |
            Storage(v, _) | StorageTexel(v, _) => v
        }
    }

    /// Generic Shorthands
    pub fn vertex<T>() -> Self {
        BufferContent::Vertex(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn vertices<T>(count: usize) -> Self {
        BufferContent::Vertex(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }

    pub fn index<T>()  -> Self {
        BufferContent::Index(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn indices<T>(count: usize) -> Self {
        BufferContent::Index(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }

    pub fn uniform<T>() -> Self {
        BufferContent::Uniform(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn uniform_dynarray<T>(count: usize) -> Self {
        BufferContent::Uniform(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }

    pub fn uniform_texel<T>() -> Self {
        BufferContent::UniformTexel(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn uniform_texel_dynarray<T>(count: usize) -> Self {
        BufferContent::UniformTexel(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }

    pub fn raw<T>() -> Self {
        BufferContent::Raw(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn raw_multiple<T>(count: usize) -> Self {
        BufferContent::Raw(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }

    pub fn storage<T>() -> Self {
        BufferContent::Storage(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn storage_dynarray<T>(count: usize) -> Self {
        BufferContent::Storage(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }

    pub fn storage_texel<T>() -> Self {
        BufferContent::StorageTexel(size_of::<T>() as _, align_of::<T>() as _)
    }
    pub fn storage_texel_dynarray<T>(count: usize) -> Self {
        BufferContent::StorageTexel(size_of::<T>() as u64 * count as u64, align_of::<T>() as _)
    }
}
macro_rules! align2 {
    ($v: expr, $a: expr) => (($v + ($a - 1)) & !($a - 1))
}
#[derive(Clone)]
pub struct BufferPrealloc<'g> {
    g: &'g Graphics, usage: br::BufferUsage, offsets: Vec<u64>, total: u64, common_align: u64
}
impl<'g> BufferPrealloc<'g> {
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
impl From<br::Buffer> for MemoryBadgetEntry
{
    fn from(v: br::Buffer) -> Self { MemoryBadgetEntry::Buffer(v) }
}
impl From<br::Image> for MemoryBadgetEntry
{
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
        let align_required = self.last_resource_tiling
            .map_or(false, |t| t.is_additional_alignment_required(entry.tiling()));
        let new_offset = if align_required
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
            // 1を確実に先に破棄したいのでManuallyDropで殺す
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
impl Buffer
{
    pub fn bound(b: br::Buffer, mem: &Memory, offset: u64) -> br::Result<Self>
    {
        b.bind(mem, offset as _).map(|_| Buffer(b.into(), mem.clone(), offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &Memory { &self.1 }

    pub fn map(&self, range: Range<u64>) -> br::Result<br::MappedMemoryRange>
    {
        self.1.map((self.2 + range.start) as _ .. (self.2 + range.end) as _)
    }
    pub unsafe fn unmap(&self) { self.1.unmap() }
    pub fn guard_map<F: FnOnce(&br::MappedMemoryRange) -> R, R>(&self, range: Range<u64>, f: F) -> br::Result<R>
    {
        Ok(f(&AutocloseMappedMemoryRange(&self.1, ManuallyDrop::new(self.map(range)?))))
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
    type Handle = <br::Buffer as br::VkHandle>::Handle;
    fn native_ptr(&self) -> Self::Handle { self.0.native_ptr() }
}
impl br::VkHandle for Image
{
    type Handle = <br::Image as br::VkHandle>::Handle;
    fn native_ptr(&self) -> Self::Handle { self.0.native_ptr() }
}

/// A view of the buffer.
#[derive(Clone, Copy)]
pub struct BufferView<'b> { pub buffer: &'b Buffer, pub offset: usize }
impl Buffer {
    pub fn with_offset(&self, offset: usize) -> BufferView {
        BufferView { buffer: self, offset }
    }
}
impl BufferView<'_> {
    pub fn with_offset(self, offset: usize) -> Self {
        BufferView { buffer: self.buffer, offset: self.offset + offset }
    }
    pub fn range(&self, bytes: usize) -> std::ops::Range<usize> {
        self.offset .. self.offset + bytes
    }
}
/// Conversion for Bedrock bind_vertex_buffers form
impl<'b> From<BufferView<'b>> for (&'b Buffer, usize) {
    fn from(v: BufferView<'b>) -> Self { (v.buffer, v.offset) }
}

/// a view of the buffer in GPU Address.
#[derive(Clone, Copy)]
pub struct DeviceBufferView<'b> { pub buffer: &'b Buffer, pub offset: br::vk::VkDeviceSize }
impl Buffer {
    pub fn with_dev_offset(&self, offset: br::vk::VkDeviceSize) -> DeviceBufferView {
        DeviceBufferView { buffer: self, offset }
    }
}
impl DeviceBufferView<'_> {
    pub fn with_offset(&self, offset: br::vk::VkDeviceSize) -> Self {
        DeviceBufferView { buffer: self.buffer, offset: self.offset + offset }
    }
    pub fn range(&self, bytes: br::vk::VkDeviceSize) -> std::ops::Range<br::vk::VkDeviceSize> {
        self.offset .. self.offset + bytes
    }
}

#[derive(Clone, Copy)] #[repr(i32)]
pub enum PixelFormat
{
    RGBA32 = br::vk::VK_FORMAT_R8G8B8A8_UNORM,
    BGRA32 = br::vk::VK_FORMAT_B8G8R8A8_UNORM,
    RGB24 = br::vk::VK_FORMAT_R8G8B8_UNORM,
    BGR24 = br::vk::VK_FORMAT_B8G8R8_UNORM,
    RGBA64F = br::vk::VK_FORMAT_R16G16B16A16_SFLOAT
}
impl PixelFormat
{
    /// Bits per pixel for each format enums
    pub fn bpp(self) -> usize
    {
        match self
        {
            PixelFormat::RGBA32 | PixelFormat::BGRA32 => 32,
            PixelFormat::RGB24 | PixelFormat::BGR24 => 24,
            PixelFormat::RGBA64F => 64
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
impl Deref for Texture2D
{
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
        for pd in self.1
        {
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
            tb.init_image_from(t.image(), stgbuf.with_dev_offset(offs));
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

/// RenderTexture2D without Readback to CPU
pub struct DeviceWorkingTexture2D
{
    size: math::Vector2<u32>,
    format: PixelFormat,
    res: Image,
    view: br::ImageView
}
impl DeviceWorkingTexture2D
{
    /// Size of this texture
    pub fn size(&self) -> &math::Vector2<u32> { &self.size }
    /// Width of this texture
    pub fn width(&self) -> u32 { self.size.0 }
    /// Height of this texture
    pub fn height(&self) -> u32 { self.size.1 }
    /// Format of this texture
    pub fn format(&self) -> PixelFormat { self.format }

    /// Gets underlying resource object
    pub fn underlying(&self) -> &Image { &self.res }
}
impl Deref for DeviceWorkingTexture2D
{
    type Target = br::ImageView;
    fn deref(&self) -> &br::ImageView { &self.view }
}
/// RenderTexture3D without Readback to CPU
pub struct DeviceWorkingTexture3D
{
    size: math::Vector3<u32>,
    format: PixelFormat,
    res: Image,
    view: br::ImageView
}
impl DeviceWorkingTexture3D
{
    /// Size of this texture
    pub fn size(&self) -> &math::Vector3<u32> { &self.size }
    /// Width of this texture
    pub fn width(&self) -> u32 { self.size.0 }
    /// Height of this texture
    pub fn height(&self) -> u32 { self.size.1 }
    /// Depth of this texture
    pub fn depth(&self) -> u32 { self.size.2 }
    /// Format of this texture
    pub fn format(&self) -> PixelFormat { self.format }

    /// Gets underlying resource object
    pub fn underlying(&self) -> &Image { &self.res }
}
impl Deref for DeviceWorkingTexture3D
{
    type Target = br::ImageView;
    fn deref(&self) -> &br::ImageView { &self.view }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct DeviceWorkingTexture2DRef(usize);
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct DeviceWorkingTexture3DRef(usize);
/// DeviceWorkingTexture Management Arena
pub struct DeviceWorkingTextureAllocator<'d>
{
    planes: Vec<br::ImageDesc<'d>>,
    volumes: Vec<br::ImageDesc<'d>>
}
impl DeviceWorkingTextureAllocator<'_>
{
    /// Initializes the allocator
    pub fn new() -> Self
    {
        DeviceWorkingTextureAllocator
        {
            planes: Vec::new(),
            volumes: Vec::new()
        }
    }

    /// Add new DeviceWorkingTexture2D allocation
    pub fn new2d(&mut self, size: math::Vector2<u32>, format: PixelFormat, usage: br::ImageUsage)
        -> DeviceWorkingTexture2DRef
    {
        self.planes.push(br::ImageDesc::new(&size, format as _, usage, br::ImageLayout::Preinitialized));
        DeviceWorkingTexture2DRef(self.planes.len() - 1)
    }

    /// Add new DeviceWorkingTexture3D allocation
    pub fn new3d(&mut self, size: math::Vector3<u32>, format: PixelFormat, usage: br::ImageUsage)
        -> DeviceWorkingTexture3DRef
    {
        self.volumes.push(br::ImageDesc::new(&size, format as _, usage, br::ImageLayout::Preinitialized));
        DeviceWorkingTexture3DRef(self.volumes.len() - 1)
    }

    /// Allocates all of added textures
    pub fn alloc(self, g: &Graphics) -> br::Result<DeviceWorkingTextureStore>
    {
        let images2 = self.planes.iter().map(|d| d.create(g));
        let images3 = self.volumes.iter().map(|d| d.create(g));
        let images: Vec<_> = images2.chain(images3).collect::<Result<_, _>>()?;
        let mut mb = MemoryBadget::new(g);
        for img in images { mb.add(img); }
        let mut bound_images = mb.alloc()?;
        
        let v3s = bound_images.split_off(bound_images.len() - self.volumes.len());
        Ok(DeviceWorkingTextureStore
        {
            planes: self.planes.into_iter().zip(bound_images.into_iter()).map(|(d, res)|
            {
                let res = res.unwrap_image();
                let view = res.create_view(
                    None, None, &br::ComponentMapping::default(),
                    &br::ImageSubresourceRange::color(0..1, 0..1)
                )?; 

                Ok(DeviceWorkingTexture2D
                {
                    size: math::Vector2(d.as_ref().extent.width, d.as_ref().extent.height),
                    format: unsafe { std::mem::transmute(d.as_ref().format) },
                    view,
                    res
                })
            }).collect::<Result<_, _>>()?,
            volumes: self.volumes.into_iter().zip(v3s.into_iter()).map(|(d, res)|
            {
                let res = res.unwrap_image();
                let view = res.create_view(
                    None, None, &br::ComponentMapping::default(),
                    &br::ImageSubresourceRange::color(0..1, 0..1)
                )?; 

                Ok(DeviceWorkingTexture3D
                {
                    size: math::Vector3(d.as_ref().extent.width, d.as_ref().extent.height, d.as_ref().extent.depth),
                    format: unsafe { std::mem::transmute(d.as_ref().format) },
                    view,
                    res
                })
            }).collect::<Result<_, _>>()?
        })
    }
}
/// Allocated DeviceWorkingTexture Arena
pub struct DeviceWorkingTextureStore
{
    planes: Vec<DeviceWorkingTexture2D>,
    volumes: Vec<DeviceWorkingTexture3D>
}
/// DeviceWorkingTexture Reference
pub trait DeviceWorkingTextureRef
{
    /// Type of the Texture that this reference referring to
    type TextureT;
    /// Gets texture object from the store
    fn get(self, store: &DeviceWorkingTextureStore) -> &Self::TextureT;
}
impl DeviceWorkingTextureStore
{
    /// Gets texture object by References
    pub fn get<R: DeviceWorkingTextureRef>(&self, r: R) -> &<R as DeviceWorkingTextureRef>::TextureT
    {
        r.get(self)
    }
}
impl DeviceWorkingTextureRef for DeviceWorkingTexture2DRef
{
    type TextureT = DeviceWorkingTexture2D;
    fn get(self, store: &DeviceWorkingTextureStore) -> &DeviceWorkingTexture2D { &store.planes[self.0] }
}
impl DeviceWorkingTextureRef for DeviceWorkingTexture3DRef
{
    type TextureT = DeviceWorkingTexture3D;
    fn get(self, store: &DeviceWorkingTextureStore) -> &DeviceWorkingTexture3D { &store.volumes[self.0] }
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

        stg_buffer.guard_map(0 .. stg_buffer_fullsize, |m| { textures.stage_data(m); initializer.stage_data(m); })?;

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
