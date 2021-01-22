//! DeviceMemory Helper

use bedrock as br;
use std::rc::Rc;

/// A refcounted memory object.
/// Convertable from `br::DeviceMemory` via `Into::into`
pub type Memory = Rc<br::DeviceMemory>;

pub struct MemoryBadget<'g> {
    g: &'g crate::Graphics,
    entries: Vec<(MemoryBadgetEntry, u64)>,
    total_size: u64,
    memory_type_bitmask: u32,
    last_resource_tiling: Option<ResourceTiling>
}
pub enum MemoryBadgetEntry { Buffer(br::Buffer), Image(br::Image) }
pub enum MemoryBoundResource { Buffer(super::Buffer), Image(super::Image) }
impl From<br::Buffer> for MemoryBadgetEntry {
    fn from(v: br::Buffer) -> Self { MemoryBadgetEntry::Buffer(v) }
}
impl From<br::Image> for MemoryBadgetEntry {
    fn from(v: br::Image) -> Self { MemoryBadgetEntry::Image(v) }
}
impl MemoryBadgetEntry {
    fn tiling(&self) -> ResourceTiling {
        match self {
            MemoryBadgetEntry::Buffer(_) => ResourceTiling::Linear,
            // Note: Peridotが扱うImageは全てNonLinearTiling
            MemoryBadgetEntry::Image(_) => ResourceTiling::NonLinear
        }
    }
}
impl MemoryBoundResource {
    pub fn unwrap_buffer(self) -> super::Buffer {
        match self { MemoryBoundResource::Buffer(b) => b, _ => panic!("Not a buffer") }
    }
    pub fn unwrap_image(self) -> super::Image {
        match self { MemoryBoundResource::Image(b) => b, _ => panic!("Not an image") }
    }
}
impl<'g> MemoryBadget<'g> {
    pub fn new(g: &'g crate::Graphics) -> Self {
        MemoryBadget {
            g, entries: Vec::new(), total_size: 0, memory_type_bitmask: 0, last_resource_tiling: None
        }
    }
    pub fn add<V: Into<MemoryBadgetEntry> + br::MemoryBound>(&mut self, v: V) -> u64 {
        let req = v.requirements();
        let new_offset = super::align2!(self.total_size, req.alignment);
        let entry = v.into();
        let align_required = self.last_resource_tiling
            .map_or(false, |t| t.is_additional_alignment_required(entry.tiling()));
        let new_offset = if align_required {
            super::align2!(new_offset, self.g.adapter.properties().limits.bufferImageGranularity)
        }
        else {
            new_offset
        };
        self.last_resource_tiling = Some(entry.tiling());
        self.entries.push((entry, new_offset));
        self.total_size = new_offset + req.size;
        self.memory_type_bitmask |= req.memoryTypeBits;
        return new_offset;
    }
    pub fn alloc(self) -> br::Result<Vec<MemoryBoundResource>> {
        let mt = self.g.memory_type_manager.device_local_index(self.memory_type_bitmask)
            .expect("No device-local memory")
            .index();
        log::info!(target: "peridot", "Allocating Device Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt, self.memory_type_bitmask);
        let mem: Rc<_> = br::DeviceMemory::allocate(&self.g.device, self.total_size as _, mt)?.into();

        self.entries.into_iter().map(|(x, o)| match x
        {
            MemoryBadgetEntry::Buffer(b) => super::Buffer::bound(b, &mem, o as _).map(MemoryBoundResource::Buffer),
            MemoryBadgetEntry::Image(b) => super::Image::bound(b, &mem, o as _).map(MemoryBoundResource::Image)
        }).collect()
    }
    pub fn alloc_upload(self) -> br::Result<Vec<MemoryBoundResource>> {
        let mt = self.g.memory_type_manager
            .host_visible_index(self.memory_type_bitmask, br::MemoryPropertyFlags::HOST_COHERENT)
            .expect("No host-visible memory");
        if !mt.is_host_coherent() {
            log::warn!("ENGINE TODO: non-coherent memory requires expicit flushing operations");
        }
        log::info!(target: "peridot", "Allocating Uploading Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt.index(), self.memory_type_bitmask);
        let mem: Rc<_> = br::DeviceMemory::allocate(&self.g.device, self.total_size as _, mt.index())?.into();
        
        self.entries.into_iter().map(|(x, o)| match x {
            MemoryBadgetEntry::Buffer(b) => super::Buffer::bound(b, &mem, o as _).map(MemoryBoundResource::Buffer),
            MemoryBadgetEntry::Image(b) => super::Image::bound(b, &mem, o as _).map(MemoryBoundResource::Image)
        }).collect()
    }
}

pub struct AutocloseMappedMemoryRange<'m>(pub(super) &'m br::DeviceMemory, pub(super) std::mem::ManuallyDrop<br::MappedMemoryRange<'m>>);
impl<'m> std::ops::Deref for AutocloseMappedMemoryRange<'m> {
    type Target = br::MappedMemoryRange<'m>; fn deref(&self) -> &Self::Target { &self.1 }
}
impl<'m> Drop for AutocloseMappedMemoryRange<'m> {
    fn drop(&mut self) {
        unsafe {
            // 1を確実に先に破棄したいのでManuallyDropで殺す
            std::mem::ManuallyDrop::drop(&mut self.1);
            self.0.unmap();
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ResourceTiling { Linear, NonLinear }
impl ResourceTiling {
    fn is_additional_alignment_required(self, other: Self) -> bool { self != other }
}
