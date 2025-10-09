//! DeviceMemory Helper

use bedrock::{self as br, VkHandle};

use crate::{
    graphics::VulkanGfx,
    mthelper::{make_shared_mutable_ref, DynamicMut, DynamicMutabilityProvider, SharedMutableRef},
};

use super::{UnboundedStandaloneBuffer, UnboundedStandaloneImage};

pub(crate) struct SharedMemoryBlockInner {
    pub(crate) device: VulkanGfx,
    pub(crate) handle: br::vk::VkDeviceMemory,
}
impl Drop for SharedMemoryBlockInner {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::free_memory(self.device.0.device, self.handle, None);
        }
    }
}
#[derive(Clone)]
pub struct SharedMemoryBlock(SharedMutableRef<SharedMemoryBlockInner>);
impl SharedMemoryBlock {
    pub(crate) fn lock_shared<'a>(
        &'a self,
    ) -> <DynamicMut<SharedMemoryBlockInner> as DynamicMutabilityProvider<
        'a,
        SharedMemoryBlockInner,
    >>::BorrowType {
        self.0.borrow()
    }

    pub(crate) fn lock_exclusive<'a>(&'a self) -> ExclusiveLockedSharedMemoryBlock<'a> {
        ExclusiveLockedSharedMemoryBlock(self.0.borrow_mut())
    }
}

#[repr(transparent)]
pub struct ExclusiveLockedSharedMemoryBlock<'a>(
    <DynamicMut<SharedMemoryBlockInner> as DynamicMutabilityProvider<
        'a,
        SharedMemoryBlockInner,
    >>::BorrowMutType,
);
impl br::DeviceChildHandle for ExclusiveLockedSharedMemoryBlock<'_> {
    #[inline]
    fn device_handle(&self) -> bedrock::vk::VkDevice {
        self.0.device.0.device
    }
}
impl br::DeviceChild for ExclusiveLockedSharedMemoryBlock<'_> {
    type ConcreteDevice = VulkanGfx;

    #[inline]
    fn device(&self) -> &Self::ConcreteDevice {
        &self.0.device
    }
}
impl br::VkHandle for ExclusiveLockedSharedMemoryBlock<'_> {
    type Handle = br::vk::VkDeviceMemory;

    #[inline]
    fn native_ptr(&self) -> Self::Handle {
        self.0.handle
    }
}
impl br::VkHandleMut for ExclusiveLockedSharedMemoryBlock<'_> {
    #[inline]
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.0.handle
    }
}
impl br::DeviceMemory for ExclusiveLockedSharedMemoryBlock<'_> {}
impl br::DeviceMemoryMut for ExclusiveLockedSharedMemoryBlock<'_> {}

pub struct MemoryBadget<'g> {
    g: &'g crate::Graphics,
    entries: Vec<(MemoryBadgetEntry, u64)>,
    total_size: u64,
    memory_type_bitmask: u32,
    last_resource_tiling: Option<ResourceTiling>,
}
pub enum MemoryBadgetEntry {
    Buffer(UnboundedStandaloneBuffer),
    Image(UnboundedStandaloneImage),
}
pub enum MemoryBoundResource {
    Buffer(super::Buffer),
    Image(super::Image),
}
impl MemoryBadgetEntry {
    #[inline]
    const fn tiling(&self) -> ResourceTiling {
        match self {
            Self::Buffer(_) => ResourceTiling::Linear,
            // Note: Peridotが扱うImageは全てNonLinearTiling
            Self::Image(_) => ResourceTiling::NonLinear,
        }
    }

    #[inline]
    fn requirements(&self, gfx_device: &VulkanGfx) -> br::vk::VkMemoryRequirements {
        match self {
            Self::Buffer(b) => unsafe {
                br::vkfn_wrapper::get_buffer_memory_requirements(
                    gfx_device.0.device,
                    b.native_ptr(),
                )
            },
            Self::Image(r) => unsafe {
                br::vkfn_wrapper::get_image_memory_requirements(gfx_device.0.device, r.native_ptr())
            },
        }
    }
}
impl MemoryBoundResource {
    #[inline]
    pub fn unwrap_buffer(self) -> super::Buffer {
        match self {
            MemoryBoundResource::Buffer(b) => b,
            _ => panic!("Not a buffer"),
        }
    }

    #[inline]
    pub fn unwrap_image(self) -> super::Image {
        match self {
            MemoryBoundResource::Image(b) => b,
            _ => panic!("Not an image"),
        }
    }
}
impl<'g> MemoryBadget<'g> {
    pub const fn new(g: &'g crate::Graphics) -> Self {
        Self {
            g,
            entries: Vec::new(),
            total_size: 0,
            memory_type_bitmask: 0,
            last_resource_tiling: None,
        }
    }

    pub fn add(&mut self, v: MemoryBadgetEntry) -> u64 {
        let req = v.requirements(&self.g.gfx_device);
        let new_offset = super::align2!(self.total_size, req.alignment);
        let align_required = self
            .last_resource_tiling
            .is_some_and(|t| t.is_additional_alignment_required(v.tiling()));
        let new_offset = if align_required {
            super::align2!(
                new_offset,
                self.g.gfx_device.adapter_limits().bufferImageGranularity
            )
        } else {
            new_offset
        };
        self.last_resource_tiling = Some(v.tiling());
        self.entries.push((v, new_offset));
        self.total_size = new_offset + req.size;
        self.memory_type_bitmask |= req.memoryTypeBits;

        new_offset
    }

    pub fn alloc(self) -> br::Result<Vec<MemoryBoundResource>> {
        let mt = self
            .g
            .gfx_device
            .0
            .memory_type_manager
            .device_local_index(self.memory_type_bitmask)
            .expect("No device-local memory")
            .index();
        tracing::info!(target: "peridot", "Allocating Device Memory: {} bytes in 0x{mt:x}(?0x{:x})",
            self.total_size, self.memory_type_bitmask);
        let mem = SharedMemoryBlock(make_shared_mutable_ref(SharedMemoryBlockInner {
            handle: unsafe {
                br::vkfn_wrapper::allocate_memory(
                    self.g.gfx_device.0.device,
                    &br::MemoryAllocateInfo::new(self.total_size, mt),
                    None,
                )?
            },
            device: self.g.gfx_device.clone(),
        }));

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

    pub fn alloc_upload(self) -> br::Result<Vec<MemoryBoundResource>> {
        let mt = self
            .g
            .gfx_device
            .0
            .memory_type_manager
            .host_visible_index(
                self.memory_type_bitmask,
                br::MemoryPropertyFlags::HOST_COHERENT,
            )
            .expect("No host-visible memory");
        if !mt.is_host_coherent() {
            tracing::warn!(
                "ENGINE TODO: non-coherent memory requires explicit flushing operations"
            );
        }
        tracing::info!(target: "peridot", "Allocating Uploading Memory: {} bytes in 0x{:x}(?0x{:x})",
            self.total_size, mt.index(), self.memory_type_bitmask);
        let mem = SharedMemoryBlock(make_shared_mutable_ref(SharedMemoryBlockInner {
            handle: unsafe {
                br::vkfn_wrapper::allocate_memory(
                    self.g.gfx_device.0.device,
                    &br::MemoryAllocateInfo::new(self.total_size, mt.index()),
                    None,
                )?
            },
            device: self.g.gfx_device.clone(),
        }));

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
pub struct AutocloseMappedMemoryRange<'m, DeviceMemory: br::DeviceMemoryMut + ?Sized + 'm>(
    pub(super) Option<br::MappedMemory<'m, DeviceMemory>>,
);
impl<'m, DeviceMemory: br::DeviceMemoryMut + ?Sized + 'm> std::ops::Deref
    for AutocloseMappedMemoryRange<'m, DeviceMemory>
{
    type Target = br::MappedMemory<'m, DeviceMemory>;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref().expect("object has been dropped")
    }
}
impl<'m, DeviceMemory: br::DeviceMemoryMut + ?Sized + 'm> Drop
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
