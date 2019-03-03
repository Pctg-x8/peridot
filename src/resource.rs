use bedrock as br; use self::br::traits::*;
use super::*;
use std::ops::Deref;
use std::mem::{size_of, transmute};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferContent { Vertex(u64), Index(u64), Uniform(u64), Raw(u64), UniformTexel(u64) }
impl BufferContent {
    fn usage(&self, src: br::BufferUsage) -> br::BufferUsage {
        match *self {
            BufferContent::Vertex(_) => src.vertex_buffer(),
            BufferContent::Index(_) => src.index_buffer(),
            BufferContent::Uniform(_) => src.uniform_buffer(),
            BufferContent::Raw(_) => src,
            BufferContent::UniformTexel(_) => src.uniform_texel_buffer()
        }
    }
    fn alignment(&self, a: &br::PhysicalDevice) -> u64 {
        match *self {
            BufferContent::Uniform(_) | BufferContent::UniformTexel(_) =>
                a.properties().limits.minUniformBufferOffsetAlignment as _,
            _ => 1
        }
    }
    fn size(&self) -> u64 {
        match *self {
            BufferContent::Vertex(v) | BufferContent::Index(v) | BufferContent::Uniform(v) | BufferContent::Raw(v) |
            BufferContent::UniformTexel(v) => v
        }
    }

    /// Generic Shorthands
    pub fn vertex<T>() -> Self { BufferContent::Vertex(size_of::<T>() as _) }
    pub fn vertices<T>(count: usize) -> Self { BufferContent::Vertex(size_of::<T>() as u64 * count as u64) }
    pub fn index<T>()  -> Self { BufferContent::Index(size_of::<T>() as _) }
    pub fn indices<T>(count: usize) -> Self { BufferContent::Index(size_of::<T>() as u64 * count as u64) }
    pub fn uniform<T>() -> Self { BufferContent::Uniform(size_of::<T>() as _) }
    pub fn uniform_dynarray<T>(count: usize) -> Self { BufferContent::Uniform(size_of::<T>() as u64 * count as u64) }
    pub fn uniform_texel<T>() -> Self { BufferContent::UniformTexel(size_of::<T>() as _) }
    pub fn uniform_texel_dynarray<T>(count: usize) -> Self {
        BufferContent::UniformTexel(size_of::<T>() as u64 * count as u64)
    }
}
macro_rules! align2 {
    ($v: expr, $a: expr) => (($v + ($a - 1)) & !($a - 1))
}
pub struct BufferPrealloc<'g> { g: &'g Graphics, usage: br::BufferUsage, offsets: Vec<u64>, total: u64 }
impl<'g> BufferPrealloc<'g> {
    pub fn new(g: &'g Graphics) -> Self {
        BufferPrealloc { g, usage: br::BufferUsage(0), offsets: Vec::new(), total: 0 }
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

    pub fn add(&mut self, content: BufferContent) -> u64 {
        self.usage = content.usage(self.usage);
        let offs = align2!(self.total, content.alignment(&self.g.adapter));
        self.total = offs + content.size() as u64;
        self.offsets.push(offs);
        return offs;
    }
    pub fn total_size(&self) -> u64 { self.total }
}

pub struct MemoryBadget<'g> {
    g: &'g Graphics, entries: Vec<(MemoryBadgetEntry, u64)>, total_size: u64,
    memory_type_bitmask: u32
}
pub enum MemoryBadgetEntry { Buffer(br::Buffer), Image(br::Image) }
pub enum MemoryBoundResource { Buffer(Buffer), Image(Image) }
impl From<br::Buffer> for MemoryBadgetEntry {
    fn from(v: br::Buffer) -> Self { MemoryBadgetEntry::Buffer(v) }
}
impl From<br::Image> for MemoryBadgetEntry {
    fn from(v: br::Image) -> Self { MemoryBadgetEntry::Image(v) }
}
impl MemoryBoundResource {
    pub fn unwrap_buffer(self) -> Buffer {
        match self { MemoryBoundResource::Buffer(b) => b, _ => panic!("Not a buffer") }
    }
    pub fn unwrap_image(self) -> Image {
        match self { MemoryBoundResource::Image(b) => b, _ => panic!("Not an image") }
    }
}
impl<'g> MemoryBadget<'g> {
    pub fn new(g: &'g Graphics) -> Self {
        MemoryBadget { g, entries: Vec::new(), total_size: 0, memory_type_bitmask: 0 }
    }
    pub fn add<V: Into<MemoryBadgetEntry> + br::MemoryBound>(&mut self, v: V) -> u64 {
        let req = v.requirements();
        let new_offset = align2!(self.total_size, req.alignment);
        self.entries.push((v.into(), new_offset));
        self.total_size = new_offset + req.size;
        self.memory_type_bitmask |= req.memoryTypeBits;
        return new_offset;
    }
    pub fn alloc(self) -> br::Result<Vec<MemoryBoundResource>> {
        let mt = self.g.memory_type_index_for(br::MemoryPropertyFlags::DEVICE_LOCAL, self.memory_type_bitmask)
            .expect("No Device-Local Memory");
        info!(target: "peridot", "Allocating Device Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt, self.memory_type_bitmask);
        let mem: Rc<_> = br::DeviceMemory::allocate(&self.g.device, self.total_size as _, mt)?.into();

        self.entries.into_iter().map(|(x, o)| match x {
            MemoryBadgetEntry::Buffer(b) => Buffer::bound(b, &mem, o as _).map(MemoryBoundResource::Buffer),
            MemoryBadgetEntry::Image(b) => Image::bound(b, &mem, o as _).map(MemoryBoundResource::Image)
        }).collect()
    }
    pub fn alloc_upload(self) -> br::Result<Vec<MemoryBoundResource>> {
        let mt = self.g.memory_type_index_for(br::MemoryPropertyFlags::HOST_VISIBLE.host_coherent(),
            self.memory_type_bitmask).expect("No Host-Visible memory");
        info!(target: "peridot", "Allocating Uploading Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt, self.memory_type_bitmask);
        let mem: Rc<_> = br::DeviceMemory::allocate(&self.g.device, self.total_size as _, mt)?.into();
        
        self.entries.into_iter().map(|(x, o)| match x {
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
    BGRA32 = br::vk::VK_FORMAT_B8G8R8A8_UNORM
}
impl PixelFormat {
    /// Bits per pixel for each format enums
    pub fn bpp(&self) -> usize {
        match *self {
            PixelFormat::RGBA32 | PixelFormat::BGRA32 => 32
        }
    }
}

pub struct Texture2D(br::ImageView, Image);
impl Texture2D {
    pub fn init(g: &br::Device, size: &math::Vector2<u32>, format: PixelFormat, prealloc: &mut BufferPrealloc)
            -> br::Result<(br::Image, u64)> {
        let idesc = br::ImageDesc::new(size, format as _, br::ImageUsage::SAMPLED.transfer_dest(),
            br::ImageLayout::Preinitialized);
        let pixels_stg = prealloc.add(BufferContent::Raw((size.x() * size.y()) as u64 * (format.bpp() >> 3) as u64));
        return idesc.create(g).map(|o| (o, pixels_stg));
    }
    pub fn new(img: Image) -> br::Result<Self> {
        return img.create_view(None, None, &br::ComponentMapping::default(), &br::ImageSubresourceRange::color(0, 0))
            .map(|v| Texture2D(v, img))
    }

    pub fn image(&self) -> &Image { &self.1 }
}
impl Deref for Texture2D {
    type Target = br::ImageView;
    fn deref(&self) -> &br::ImageView { &self.0 }
}
