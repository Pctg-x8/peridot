use bedrock as br; use self::br::traits::*;
use super::*;
use std::ops::Deref;
use std::mem::size_of;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BufferContent { Vertex(usize), Index(usize), Uniform(usize) }
impl BufferContent {
    fn usage(&self, src: br::BufferUsage) -> br::BufferUsage {
        match *self {
            BufferContent::Vertex(_) => src.vertex_buffer(),
            BufferContent::Index(_) => src.index_buffer(),
            BufferContent::Uniform(_) => src.uniform_buffer()
        }
    }
    fn alignment(&self, a: &br::PhysicalDevice) -> usize {
        match *self {
            BufferContent::Uniform(_) => a.properties().limits.minUniformBufferOffsetAlignment as _,
            _ => 1
        }
    }
    fn size(&self) -> usize {
        match *self { BufferContent::Vertex(v) | BufferContent::Index(v) | BufferContent::Uniform(v) => v }
    }

    /// Generic Shorthands
    pub fn vertex<T>() -> Self { BufferContent::Vertex(size_of::<T>()) }
    pub fn index<T>()  -> Self { BufferContent::Index(size_of::<T>()) }
    pub fn uniform<T>() -> Self { BufferContent::Uniform(size_of::<T>()) }
}
macro_rules! align2 {
    ($v: expr, $a: expr) => (($v + ($a - 1)) & !($a - 1))
}
pub struct BufferPrealloc<'g> { g: &'g Graphics, usage: br::BufferUsage, offsets: Vec<usize>, total: usize }
impl<'g> BufferPrealloc<'g> {
    pub fn new(g: &'g Graphics) -> Self {
        BufferPrealloc { g, usage: br::BufferUsage(0), offsets: Vec::new(), total: 0 }
    }
    pub fn build(&self) -> br::Result<br::Buffer> {
        let obj = br::BufferDesc::new(self.total, self.usage).create(&self.g.device)?;
        return Ok(obj);
    }
    pub fn build_transferred(&self) -> br::Result<br::Buffer> {
        br::BufferDesc::new(self.total, self.usage.transfer_dest()).create(&self.g.device)
    }
    pub fn build_upload(&self) -> br::Result<br::Buffer> {
        br::BufferDesc::new(self.total, self.usage.transfer_src()).create(&self.g.device)
    }

    pub fn add(&mut self, content: BufferContent) -> usize {
        self.usage = content.usage(self.usage);
        let offs = align2!(self.total, content.alignment(&self.g.adapter));
        self.total = offs + content.size();
        self.offsets.push(offs);
        return offs;
    }
    pub fn total_size(&self) -> usize { self.total }
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
pub struct Buffer(Rc<br::Buffer>, Memory, usize);
/// A refcounted image object bound with a memory object.
#[derive(Clone)]
pub struct Image(Rc<br::Image>, Memory);
impl Buffer {
    pub fn bound(b: br::Buffer, mem: &Memory, offset: usize) -> br::Result<Self> {
        b.bind(mem, offset).map(|_| Buffer(b.into(), mem.clone(), offset))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &Memory { &self.1 }

    pub fn map(&self, size: usize) -> br::Result<br::MappedMemoryRange> {
        self.1.map(self.2 .. self.2 + size)
    }
    pub unsafe fn unmap(&self) { self.1.unmap() }
    pub fn guard_map<F: FnOnce(&br::MappedMemoryRange)>(&self, size: usize, f: F) -> br::Result<()> {
        f(&AutocloseMappedMemoryRange(&self.1, ManuallyDrop::new(self.map(size)?))); return Ok(());
    }
}
impl Image {
    pub fn bound(r: br::Image, mem: &Memory, offset: usize) -> br::Result<Self> {
        r.bind(mem, offset).map(|_| Image(r.into(), mem.clone()))
    }
    /// Reference to a memory object bound with this object.
    pub fn memory(&self) -> &Memory { &self.1 }
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
