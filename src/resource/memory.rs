//! DeviceMemory Helper
#![allow(deprecated)]

use bedrock as br;
use br::{Device, PhysicalDevice};

use crate::{
    mthelper::{DynamicMut, SharedRef},
    DeviceObject,
};

#[deprecated = "use BulkedResourceStorageAllocator and ResourceStorage for more efficient memory allocation"]
pub struct MemoryBadget<'g, Buffer: br::Buffer, Image: br::Image> {
    g: &'g crate::Graphics,
    entries: Vec<(MemoryBadgetEntry<Buffer, Image>, u64)>,
    total_size: u64,
    memory_type_bitmask: u32,
    last_resource_tiling: Option<ResourceTiling>,
}
pub enum MemoryBadgetEntry<Buffer: br::Buffer, Image: br::Image> {
    Buffer(Buffer),
    Image(Image),
}
pub enum MemoryBoundResource<Buffer: br::Buffer, Image: br::Image, DeviceMemory: br::DeviceMemory> {
    Buffer(super::Buffer<Buffer, DeviceMemory>),
    Image(super::Image<Image, DeviceMemory>),
}
impl<Buffer: br::Buffer, Image: br::Image> MemoryBadgetEntry<Buffer, Image> {
    #[inline]
    const fn tiling(&self) -> ResourceTiling {
        match self {
            Self::Buffer(_) => ResourceTiling::Linear,
            // Note: Peridotが扱うImageは全てNonLinearTiling
            Self::Image(_) => ResourceTiling::NonLinear,
        }
    }

    #[inline]
    fn requirements(&self) -> br::vk::VkMemoryRequirements
    where
        Buffer: br::MemoryBound,
        Image: br::MemoryBound,
    {
        match self {
            Self::Buffer(b) => b.requirements(),
            Self::Image(r) => r.requirements(),
        }
    }
}
impl<Buffer: br::Buffer, Image: br::Image, DeviceMemory: br::DeviceMemory>
    MemoryBoundResource<Buffer, Image, DeviceMemory>
{
    #[inline]
    pub fn unwrap_buffer(self) -> super::Buffer<Buffer, DeviceMemory> {
        match self {
            MemoryBoundResource::Buffer(b) => b,
            _ => panic!("Not a buffer"),
        }
    }

    #[inline]
    pub fn unwrap_image(self) -> super::Image<Image, DeviceMemory> {
        match self {
            MemoryBoundResource::Image(b) => b,
            _ => panic!("Not an image"),
        }
    }
}
impl<'g, Buffer, Image> MemoryBadget<'g, Buffer, Image>
where
    Buffer: br::Buffer<ConcreteDevice = DeviceObject> + br::MemoryBound,
    Image: br::Image<ConcreteDevice = DeviceObject> + br::MemoryBound,
{
    pub const fn new(g: &'g crate::Graphics) -> Self {
        Self {
            g,
            entries: Vec::new(),
            total_size: 0,
            memory_type_bitmask: 0,
            last_resource_tiling: None,
        }
    }

    pub fn add(&mut self, v: MemoryBadgetEntry<Buffer, Image>) -> u64
    where
        Buffer: br::MemoryBound,
        Image: br::MemoryBound,
    {
        let req = v.requirements();
        let new_offset = super::align2!(self.total_size, req.alignment);
        let align_required = self
            .last_resource_tiling
            .map_or(false, |t| t.is_additional_alignment_required(v.tiling()));
        let new_offset = if align_required {
            super::align2!(
                new_offset,
                self.g.adapter.properties().limits.bufferImageGranularity
            )
        } else {
            new_offset
        };
        self.last_resource_tiling = Some(v.tiling());
        self.entries.push((v, new_offset));
        self.total_size = new_offset + req.size;
        self.memory_type_bitmask |= req.memoryTypeBits;
        return new_offset;
    }

    pub fn alloc(
        self,
    ) -> br::Result<Vec<MemoryBoundResource<Buffer, Image, br::DeviceMemoryObject<DeviceObject>>>>
    {
        let mt = self
            .g
            .memory_type_manager
            .device_local_index(self.memory_type_bitmask)
            .expect("No device-local memory")
            .index();
        log::info!(target: "peridot", "Allocating Device Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt, self.memory_type_bitmask);
        let mem = SharedRef::new(DynamicMut::new(
            self.g
                .device
                .clone()
                .allocate_memory(self.total_size as _, mt)?,
        ));

        self.entries
            .into_iter()
            .map(|(x, o)| match x {
                MemoryBadgetEntry::Buffer(b) => {
                    super::Buffer::bound(b, &mem, o as _).map(MemoryBoundResource::Buffer)
                }
                MemoryBadgetEntry::Image(b) => {
                    super::Image::bound(b, &mem, o as _).map(MemoryBoundResource::Image)
                }
            })
            .collect()
    }

    pub fn alloc_upload(
        self,
    ) -> br::Result<Vec<MemoryBoundResource<Buffer, Image, br::DeviceMemoryObject<DeviceObject>>>>
    {
        let mt = self
            .g
            .memory_type_manager
            .host_visible_index(
                self.memory_type_bitmask,
                br::MemoryPropertyFlags::HOST_COHERENT,
            )
            .expect("No host-visible memory");
        if !mt.is_host_coherent() {
            log::warn!("ENGINE TODO: non-coherent memory requires expicit flushing operations");
        }
        log::info!(target: "peridot", "Allocating Uploading Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt.index(), self.memory_type_bitmask);
        let mem = SharedRef::new(DynamicMut::new(
            self.g
                .device
                .clone()
                .allocate_memory(self.total_size as _, mt.index())?,
        ));

        self.entries
            .into_iter()
            .map(|(x, o)| match x {
                MemoryBadgetEntry::Buffer(b) => {
                    super::Buffer::bound(b, &mem, o as _).map(MemoryBoundResource::Buffer)
                }
                MemoryBadgetEntry::Image(b) => {
                    super::Image::bound(b, &mem, o as _).map(MemoryBoundResource::Image)
                }
            })
            .collect()
    }
}

#[repr(transparent)]
pub struct AutocloseMappedMemoryRange<'m, DeviceMemory: br::DeviceMemory + ?Sized + 'm>(
    pub(super) Option<br::MappedMemoryRange<'m, DeviceMemory>>,
);
impl<'m, DeviceMemory: br::DeviceMemory + ?Sized + 'm> std::ops::Deref
    for AutocloseMappedMemoryRange<'m, DeviceMemory>
{
    type Target = br::MappedMemoryRange<'m, DeviceMemory>;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().expect("object has been dropped")
    }
}
impl<'m, DeviceMemory: br::DeviceMemory + ?Sized + 'm> Drop
    for AutocloseMappedMemoryRange<'m, DeviceMemory>
{
    fn drop(&mut self) {
        self.0.take().expect("object has been dropped").end();
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ResourceTiling {
    Linear,
    NonLinear,
}
impl ResourceTiling {
    fn is_additional_alignment_required(self, other: Self) -> bool {
        self != other
    }
}
