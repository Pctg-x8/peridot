use std::collections::BTreeMap;

use bedrock as br;
use br::{Device, MemoryBound, StructureChainQuery, VkHandle, VulkanStructure};
use num_integer::Integer;
#[allow(unused_imports)]
use peridot::mthelper::DynamicMutabilityProvider;
use peridot::mthelper::{make_shared_mutable_ref, SharedMutableRef};
use resource_wrapper::BackingMemory;
use slab::{MemoryBlockSlabCache, SLAB_ALLOC_BASE_SIZE};
use sub_block::{MemoryBlockSlabCacheFreeAreaManager, SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE};
use utils::{align2, align2_u64, power_of_2_series_from, round_up_to_next_power_of_two};

use crate::sub_block::MemoryBlockSlabCacheFreeAreaAcquisitionFailure;

mod resource_wrapper;
mod slab;
mod sub_block;
mod utils;

pub use resource_wrapper::{AnyPointer, Buffer, BufferMapMode, Image, LinearImageBuffer};

#[allow(dead_code)]
struct MemoryType {
    pub index: u32,
    pub heap_index: u32,
    pub is_coherent: bool,
    pub is_cached: bool,
}
impl MemoryType {
    const fn index_mask(&self) -> u32 {
        1 << self.index
    }
}
#[allow(dead_code)]
struct HeapStats {
    pub info: br::vk::VkMemoryHeap,
    pub used_bytes: u64,
}

struct MemoryBlock<Device: br::Device> {
    object: br::DeviceMemoryObject<Device>,
    /// 64*2^n where n is index of the vec
    slab_cache_by_object_size: Vec<MemoryBlockSlabCache>,
    slab_cache_free_area_manager: MemoryBlockSlabCacheFreeAreaManager,
}
impl<Device: br::Device> MemoryBlock<Device> {
    fn new(object: br::DeviceMemoryObject<Device>, total_size: u64) -> Self {
        let total_block_count = total_size / SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64;

        Self {
            object,
            slab_cache_by_object_size: power_of_2_series_from(SLAB_ALLOC_BASE_SIZE)
                .take_while(|&x| x <= total_size as usize)
                .map(MemoryBlockSlabCache::new)
                .collect(),
            slab_cache_free_area_manager: MemoryBlockSlabCacheFreeAreaManager::new(
                total_block_count as _,
            ),
        }
    }

    fn align_object_size(size: usize) -> usize {
        round_up_to_next_power_of_two(size.max(SLAB_ALLOC_BASE_SIZE) as _) as _
    }

    /// slab_cache_by_object_sizeのキーを計算
    const fn slab_level(aligned_size: usize) -> usize {
        (aligned_size.trailing_zeros() - SLAB_ALLOC_BASE_SIZE.trailing_zeros()) as usize
    }

    const fn max_block_count_for_slab_level(slab_level: usize) -> u32 {
        1u32 << slab_level
    }

    #[tracing::instrument(skip(self), fields(aligned_size = Self::align_object_size(size)))]
    fn suballocate(&mut self, size: usize) -> Option<u64> {
        let slab_object_size = Self::align_object_size(size);
        let slab_level = Self::slab_level(slab_object_size);
        let allocator = &mut self.slab_cache_by_object_size[slab_level as usize];

        tracing::debug!("using level #{slab_level}");

        if let Some(o) = allocator.find_free_object_offset() {
            return Some(o);
        }

        // 既存のslab cache内ではみつからなかった 新しく登録して試す
        let (new_slab_cache_offset_block, max_objects, block_count) = match self
            .slab_cache_free_area_manager
            .acquire(Self::max_block_count_for_slab_level(slab_level))
        {
            Ok(b) => (b, 32, Self::max_block_count_for_slab_level(slab_level)),
            Err(MemoryBlockSlabCacheFreeAreaAcquisitionFailure::TooLarge) => {
                let (max_available_block_start, max_available_block_count) = self
                    .slab_cache_free_area_manager
                    .max_unallocated_memory_block_length();
                tracing::debug!(
                    { max_available_block_start, max_available_block_count },
                    "trying maximum allocation"
                );
                let max_available_block_size =
                    MemoryBlockSlabCacheFreeAreaManager::block_count_to_bytes(
                        max_available_block_count,
                    );
                if max_available_block_size < slab_object_size as u64 {
                    // no enough blocks left
                    return None;
                }

                // 全部使うともったいないので半分にする
                let new_slab_object_count =
                    ((max_available_block_size / slab_object_size as u64) >> 1).max(1);
                let new_slab_object_block_count =
                    MemoryBlockSlabCacheFreeAreaManager::byte_length_to_block_count(
                        new_slab_object_count * slab_object_size as u64,
                    );
                let new_block_start = self
                    .slab_cache_free_area_manager
                    .acquire(new_slab_object_block_count)
                    .ok()?;

                tracing::trace!("object too large! managing only {new_slab_object_count} objects");

                (
                    new_block_start,
                    new_slab_object_count,
                    new_slab_object_block_count,
                )
            }
            Err(_) => return None,
        };
        allocator.append_empty_slab(
            (new_slab_cache_offset_block, block_count),
            MemoryBlockSlabCacheFreeAreaManager::block_number_to_bytes(new_slab_cache_offset_block),
            max_objects as _,
        );

        allocator.find_free_object_offset()
    }

    #[tracing::instrument(skip(self), fields(aligned_size = Self::align_object_size(size)))]
    fn free(&mut self, size: usize, offset: u64) {
        let slab_object_size = Self::align_object_size(size);
        let slab_level = Self::slab_level(slab_object_size);

        tracing::debug!("using level #{slab_level}");

        self.slab_cache_by_object_size[slab_level as usize].free_object(offset, |empty| {
            self.slab_cache_free_area_manager
                .release_power_of_two_block(empty.block_info.0, empty.block_info.1);
        });
    }
}

struct OptimalBufferLinearImagePlacementInfo {
    pub alignment: u64,
    pub row_pitch_alignment: u64,
}

pub struct MemoryManager {
    /// Device Local only
    device_local_memory_types: Vec<MemoryType>,
    /// Host Visible only
    host_visible_memory_types: Vec<MemoryType>,
    #[allow(dead_code)]
    /// Both Device Local and Host Visible (this memory can be directly mapped)
    direct_memory_types: Vec<MemoryType>,
    #[allow(dead_code)]
    heap_stats: Vec<HeapStats>,
    optimal_buffer_linear_image_placement_info: OptimalBufferLinearImagePlacementInfo,
    managed_blocks_per_type:
        BTreeMap<u32, Vec<SharedMutableRef<MemoryBlock<peridot::DeviceObject>>>>,
}
impl MemoryManager {
    /// 1MB以下はSmall Allocationとして判定する
    const SMALL_ALLOCATION_THRESHOLD: u64 = 1024 * 1024;
    const NEW_MANAGED_ALLOCATION_SIZE: u64 = 4 * Self::SMALL_ALLOCATION_THRESHOLD;

    pub fn new(e: &peridot::Graphics) -> Self {
        let features = e.adapter_available_features();
        let limits = e.adapter_limits();
        let memory_properties = e.adapter_memory_properties();

        let heap_stats = memory_properties
            .heaps()
            .map(|h| HeapStats {
                info: h.clone(),
                used_bytes: 0,
            })
            .collect::<Vec<_>>();

        let (mut device_local_memory_types, mut host_visible_memory_types, mut direct_memory_types) =
            (Vec::new(), Vec::new(), Vec::new());
        for (n, t) in memory_properties.types().enumerate() {
            let mt = MemoryType {
                index: n as _,
                heap_index: t.heapIndex,
                is_coherent: (t.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) != 0,
                is_cached: (t.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_CACHED_BIT) != 0,
            };

            if (t.propertyFlags & br::vk::VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) != 0 {
                if (t.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) != 0 {
                    direct_memory_types.push(mt);
                } else {
                    device_local_memory_types.push(mt);
                }
            } else if (t.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) != 0 {
                host_visible_memory_types.push(mt);
            }
        }

        tracing::info!("Initializing MemoryManager");
        tracing::debug!("adapter features: {features:#?}");
        tracing::debug!("adapter limits: {limits:#?}");
        tracing::debug!("memory heaps");
        for (n, h) in memory_properties.heaps().enumerate() {
            let mut flags = Vec::new();
            if (h.flags & br::vk::VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) != 0 {
                flags.push("Device Local");
            }

            let (mut v, mut units) = (h.size as f64, "bytes");
            if v > 1000.0 {
                v = v / 1024.0;
                units = "KB";
            }
            if v > 1000.0 {
                v = v / 1024.0;
                units = "MB";
            }
            if v > 1000.0 {
                v = v / 1024.0;
                units = "GB";
            }
            if v > 1000.0 {
                v = v / 1024.0;
                units = "TB";
            }

            tracing::debug!(
                "* #{n}: size {}({v:.1} {units}) {}",
                h.size,
                flags.join("/")
            );

            for (n, t) in memory_properties
                .types()
                .enumerate()
                .filter(|(_, t)| t.heapIndex == n as _)
            {
                let flags = [
                    (br::vk::VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, "Device Local"),
                    (br::vk::VK_MEMORY_PROPERTY_HOST_CACHED_BIT, "Host Cached"),
                    (
                        br::vk::VK_MEMORY_PROPERTY_HOST_COHERENT_BIT,
                        "Host Coherent",
                    ),
                    (br::vk::VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT, "Host Visible"),
                    (
                        br::vk::VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT,
                        "Lazily Allocated",
                    ),
                    (br::vk::VK_MEMORY_PROPERTY_PROTECTED_BIT, "Protected"),
                ]
                .into_iter()
                .filter_map(|(f, d)| ((t.propertyFlags & f) != 0).then_some(d))
                .collect::<Vec<_>>();

                tracing::debug!("  * type #{n}: {}", flags.join("/"));
            }
        }

        Self {
            device_local_memory_types,
            host_visible_memory_types,
            direct_memory_types,
            heap_stats,
            optimal_buffer_linear_image_placement_info: OptimalBufferLinearImagePlacementInfo {
                alignment: limits.optimalBufferCopyOffsetAlignment,
                row_pitch_alignment: limits.optimalBufferCopyRowPitchAlignment,
            },
            managed_blocks_per_type: BTreeMap::new(),
        }
    }

    fn device_local_memory_type(&self, index_mask: u32) -> Option<&MemoryType> {
        self.device_local_memory_types
            .iter()
            .find(|t| (index_mask & t.index_mask()) != 0)
    }

    #[tracing::instrument(skip(self, device, dedicated_allocate_additional_ops))]
    fn allocate_internal(
        &mut self,
        device: &peridot::DeviceObject,
        memory_requirements: &br::vk::VkMemoryRequirements2KHR,
        memory_index: u32,
        dedicated_allocate_additional_ops: impl FnOnce(
            br::DeviceMemoryRequest,
        ) -> br::DeviceMemoryRequest,
    ) -> br::Result<(BackingMemory, u64)> {
        let (require_dedicated, prefer_dedicated) = memory_requirements
            .query_structure::<br::vk::VkMemoryDedicatedRequirementsKHR>()
            .map_or((false, true), |x| {
                (
                    x.requiresDedicatedAllocation != 0,
                    x.prefersDedicatedAllocation != 0,
                )
            });
        if require_dedicated
            || (prefer_dedicated
                && memory_requirements.memoryRequirements.size >= Self::SMALL_ALLOCATION_THRESHOLD)
        {
            tracing::trace!("requested resource is enough big for 1:1 allocation");

            let memory = dedicated_allocate_additional_ops(br::DeviceMemoryRequest::allocate(
                memory_requirements.memoryRequirements.size as _,
                memory_index,
            ))
            .execute(device.clone())?;
            return Ok((BackingMemory::Native(memory), 0));
        }

        let blocks = self
            .managed_blocks_per_type
            .entry(memory_index)
            .or_insert_with(Vec::new);

        if let Some((t, offset)) = blocks.iter().find_map(|t| {
            t.borrow_mut()
                .suballocate(memory_requirements.memoryRequirements.size as _)
                .map(|o| (t, o))
        }) {
            tracing::trace!("suballocation ok! resource will be placed at {offset}");

            return Ok((BackingMemory::Managed(t.clone()), offset));
        }

        // 既存のものにはもう入らないので新規で確保
        tracing::trace!(
            "allocating new device memory block: {} bytes",
            Self::NEW_MANAGED_ALLOCATION_SIZE
        );
        let mut new_block = MemoryBlock::new(
            br::DeviceMemoryRequest::allocate(Self::NEW_MANAGED_ALLOCATION_SIZE as _, memory_index)
                .execute(device.clone())?,
            Self::NEW_MANAGED_ALLOCATION_SIZE,
        );
        // 絶対確保できるはず
        let new_offset = unsafe {
            new_block
                .suballocate(memory_requirements.memoryRequirements.size as _)
                .unwrap_unchecked()
        };
        tracing::trace!("first buffer will be placed at {new_offset}");
        let new_block = make_shared_mutable_ref(new_block);
        blocks.push(new_block.clone());

        Ok((BackingMemory::Managed(new_block), new_offset))
    }

    pub fn allocate_device_local_buffer(
        &mut self,
        e: &peridot::Graphics,
        desc: br::BufferDesc,
    ) -> br::Result<Buffer> {
        let exact_size = desc.size();
        let mut o = desc.create(e.device().clone())?;

        let mut req = br::vk::VkMemoryRequirements2KHR::uninit_sink();
        let mut sink_dedicated_alloc = br::vk::VkMemoryDedicatedRequirementsKHR::uninit_sink();
        if e.can_request_extended_memory_requirements() {
            unsafe {
                (*req.as_mut_ptr()).pNext = sink_dedicated_alloc.as_mut_ptr() as _;
            }
            o.requirements2().query(&mut req);
        } else {
            unsafe {
                (*req.as_mut_ptr()).memoryRequirements = o.requirements();
            }
        }
        let req = unsafe { req.assume_init() };

        let memory_index = self
            .device_local_memory_type(req.memoryRequirements.memoryTypeBits)
            .expect("no memory type index")
            .index;

        let (memory, offset) = self.allocate_internal(e.device(), &req, memory_index, |r| {
            if e.dedicated_allocation_available() {
                tracing::info!("using dedicated allocation");

                unsafe { r.for_dedicated_buffer_allocation(&o) }
            } else {
                r
            }
        })?;
        // TODO: 強制アラインメント(VkMemoryRequirements::sizeがアラインメント調整用パディングを含んでいる前提
        // この前提が崩れることがあったら考え直す)
        let aligned_offset = align2_u64(offset, req.memoryRequirements.alignment);
        match memory {
            BackingMemory::Managed(ref m) => o.bind(&m.borrow().object, aligned_offset as _),
            BackingMemory::Native(ref m) => o.bind(m, aligned_offset as _),
            BackingMemory::NativeShared(ref m) => o.bind(&m.borrow(), aligned_offset as _),
        }?;

        Ok(Buffer {
            object: o,
            memory_block: memory,
            requires_flushing: false,
            offset: aligned_offset,
            size: exact_size as _,
            malloc: (req.memoryRequirements.size, offset),
        })
    }

    pub fn allocate_multiple_device_local_buffers<'r>(
        &mut self,
        e: &peridot::Graphics,
        descs: impl IntoIterator<Item = br::BufferDesc<'r>>,
    ) -> br::Result<Vec<Buffer>> {
        let descs = descs.into_iter();
        let (s, _) = descs.size_hint();
        let (mut exact_sizes, mut objects, mut requirements) = (
            Vec::with_capacity(s),
            Vec::with_capacity(s),
            Vec::with_capacity(s),
        );
        for d in descs {
            exact_sizes.push(d.size());
            let object = d.create(e.device().clone())?;

            let mut req = br::vk::VkMemoryRequirements2KHR::uninit_sink();
            let mut sink_dedicated_alloc = br::vk::VkMemoryDedicatedRequirementsKHR::uninit_sink();
            if e.can_request_extended_memory_requirements() {
                unsafe {
                    (*req.as_mut_ptr()).pNext = sink_dedicated_alloc.as_mut_ptr() as _;
                }
                object.requirements2().query(&mut req);
            } else {
                unsafe {
                    (*req.as_mut_ptr()).memoryRequirements = object.requirements();

                    // 情報取れないのでないものとして設定
                    (*sink_dedicated_alloc.as_mut_ptr()).prefersDedicatedAllocation = false as _;
                    (*sink_dedicated_alloc.as_mut_ptr()).requiresDedicatedAllocation = false as _;
                }
            }

            requirements.push(unsafe { (req.assume_init(), sink_dedicated_alloc.assume_init()) });
            objects.push(object);
        }

        let alloc_info = ObjectAllocationInfo::compute(&requirements);
        let memory_index = self
            .device_local_memory_type(alloc_info.combined_memory_index_mask)
            .expect("no memory type")
            .index;

        let combined_native_memory = if alloc_info.total_size >= Self::SMALL_ALLOCATION_THRESHOLD {
            // SMALL_ALLOCATION_THRESHOLD以上の場合はひとつのNativeを確保して配置する
            tracing::trace!(
                { total_size = alloc_info.total_size },
                "multiple request is enough big to sharing one native memory"
            );

            Some(make_shared_mutable_ref(
                br::DeviceMemoryRequest::allocate(alloc_info.total_size as _, memory_index)
                    .execute(e.device().clone())?,
            ))
        } else {
            None
        };

        let (mut bound_objects, mut binding_info) = (
            Vec::with_capacity(objects.len()),
            Vec::with_capacity(objects.len()),
        );
        for (((object, exact_size), mode), (req, _)) in objects
            .into_iter()
            .zip(exact_sizes.into_iter())
            .zip(alloc_info.allocation_modes.into_iter())
            .zip(requirements.into_iter())
        {
            match mode {
                ObjectAllocationMode::Dedicated => {
                    tracing::trace!("requested resource is enough big for 1:1 allocation");

                    let memory_index = self
                        .device_local_memory_type(req.memoryRequirements.memoryTypeBits)
                        .expect("no memory type")
                        .index;

                    let mut memory_req = br::DeviceMemoryRequest::allocate(
                        req.memoryRequirements.size as _,
                        memory_index,
                    );
                    if e.dedicated_allocation_available() {
                        memory_req = unsafe { memory_req.for_dedicated_buffer_allocation(&object) };
                    }
                    let memory = memory_req.execute(e.device().clone())?;

                    binding_info.push(br::vk::VkBindBufferMemoryInfoKHR {
                        sType: br::vk::VkBindBufferMemoryInfoKHR::TYPE,
                        pNext: core::ptr::null(),
                        buffer: object.native_ptr(),
                        memory: memory.native_ptr(),
                        memoryOffset: 0,
                    });
                    bound_objects.push(Buffer {
                        object,
                        memory_block: BackingMemory::Native(memory),
                        requires_flushing: false,
                        offset: 0,
                        size: exact_size as _,
                        malloc: (req.memoryRequirements.size, 0),
                    });
                }
                ObjectAllocationMode::Small(offset) => {
                    if let Some(ref combined) = combined_native_memory {
                        // placement into combined native memory

                        binding_info.push(br::vk::VkBindBufferMemoryInfoKHR {
                            sType: br::vk::VkBindBufferMemoryInfoKHR::TYPE,
                            pNext: core::ptr::null(),
                            buffer: object.native_ptr(),
                            memory: combined.borrow().native_ptr(),
                            memoryOffset: offset,
                        });
                        bound_objects.push(Buffer {
                            object,
                            memory_block: BackingMemory::NativeShared(combined.clone()),
                            requires_flushing: false,
                            offset,
                            size: exact_size as _,
                            malloc: (req.memoryRequirements.size, offset),
                        });
                    } else {
                        // normal small allocation
                        let (memory, offset) =
                            self.allocate_internal(e.device(), &req, memory_index, |_| {
                                unreachable!("no dedicated allocation must occurs!")
                            })?;
                        // TODO: 強制アラインメント(VkMemoryRequirements::sizeがアラインメント調整用パディングを含んでいる前提
                        // この前提が崩れることがあったら考え直す)
                        let aligned_offset = align2_u64(offset, req.memoryRequirements.alignment);

                        binding_info.push(br::vk::VkBindBufferMemoryInfoKHR {
                            sType: br::vk::VkBindBufferMemoryInfoKHR::TYPE,
                            pNext: core::ptr::null(),
                            buffer: object.native_ptr(),
                            memory: match memory {
                                BackingMemory::Managed(ref m) => m.borrow().object.native_ptr(),
                                BackingMemory::Native(ref m) => m.native_ptr(),
                                BackingMemory::NativeShared(ref m) => m.borrow().native_ptr(),
                            },
                            memoryOffset: aligned_offset,
                        });
                        bound_objects.push(Buffer {
                            object,
                            memory_block: memory,
                            requires_flushing: false,
                            offset: aligned_offset,
                            size: exact_size as _,
                            malloc: (req.memoryRequirements.size, offset),
                        });
                    }
                }
            }
        }

        bind_buffers(e, &binding_info)?;
        Ok(bound_objects)
    }

    pub fn allocate_device_local_buffer_array<const N: usize>(
        &mut self,
        e: &peridot::Graphics,
        descs: [br::BufferDesc; N],
    ) -> br::Result<[Buffer; N]> {
        self.allocate_multiple_device_local_buffers(e, descs)
            .map(|x| unsafe { x.try_into().unwrap_unchecked() })
    }

    pub fn allocate_device_local_buffer_with_contents(
        &mut self,
        e: &peridot::Graphics,
        contents: impl IntoIterator<Item = peridot::BufferContent>,
        add_usage: br::BufferUsage,
    ) -> br::Result<(Buffer, Vec<u64>)> {
        let mut bp = peridot::BufferPrealloc::new(e);
        let offsets = contents.into_iter().map(|c| bp.add(c)).collect::<Vec<_>>();
        let obj = self.allocate_device_local_buffer(e, bp.build_desc().and_usage(add_usage))?;

        Ok((obj, offsets))
    }

    pub fn allocate_upload_buffer(
        &mut self,
        e: &peridot::Graphics,
        desc: br::BufferDesc,
    ) -> br::Result<Buffer> {
        let exact_size = desc.size();
        let mut o = desc.create(e.device().clone())?;

        let mut req = br::vk::VkMemoryRequirements2KHR::uninit_sink();
        let mut sink_dedicated_alloc = br::vk::VkMemoryDedicatedRequirementsKHR::uninit_sink();
        if e.can_request_extended_memory_requirements() {
            unsafe {
                (*req.as_mut_ptr()).pNext = sink_dedicated_alloc.as_mut_ptr() as _;
            }
            o.requirements2().query(&mut req);
        } else {
            unsafe {
                (*req.as_mut_ptr()).memoryRequirements = o.requirements();
            }
        }
        let req = unsafe { req.assume_init() };

        // prefer host coherent(for less operations)
        let memory_type = self
            .host_visible_memory_types
            .iter()
            .find(|t| {
                (req.memoryRequirements.memoryTypeBits & t.index_mask()) != 0 && t.is_coherent
            })
            .or_else(|| {
                self.host_visible_memory_types
                    .iter()
                    .find(|t| (req.memoryRequirements.memoryTypeBits & t.index_mask()) != 0)
            })
            .expect("no memory type index");
        let requires_flushing = !memory_type.is_coherent;

        let (memory, offset) =
            self.allocate_internal(e.device(), &req, memory_type.index, |r| {
                if e.dedicated_allocation_available() {
                    tracing::info!("using dedicated allocation");

                    unsafe { r.for_dedicated_buffer_allocation(&o) }
                } else {
                    r
                }
            })?;
        // TODO: 強制アラインメント(VkMemoryRequirements::sizeがアラインメント調整用パディングを含んでいる前提
        // この前提が崩れることがあったら考え直す)
        let aligned_offset = align2_u64(offset, req.memoryRequirements.alignment);
        match memory {
            BackingMemory::Managed(ref m) => o.bind(&m.borrow().object, aligned_offset as _),
            BackingMemory::Native(ref m) => o.bind(m, aligned_offset as _),
            BackingMemory::NativeShared(ref m) => o.bind(&m.borrow(), aligned_offset as _),
        }?;

        Ok(Buffer {
            object: o,
            memory_block: memory,
            requires_flushing,
            offset: aligned_offset,
            size: exact_size as _,
            malloc: (req.memoryRequirements.size, offset),
        })
    }

    pub fn allocate_multiple_upload_buffers<'r>(
        &mut self,
        e: &peridot::Graphics,
        descs: impl IntoIterator<Item = br::BufferDesc<'r>>,
    ) -> br::Result<Vec<Buffer>> {
        let descs = descs.into_iter();
        let (s, _) = descs.size_hint();
        let (mut exact_sizes, mut objects, mut requirements) = (
            Vec::with_capacity(s),
            Vec::with_capacity(s),
            Vec::with_capacity(s),
        );
        for d in descs {
            exact_sizes.push(d.size());
            let object = d.create(e.device().clone())?;

            let mut req = br::vk::VkMemoryRequirements2KHR::uninit_sink();
            let mut sink_dedicated_alloc = br::vk::VkMemoryDedicatedRequirementsKHR::uninit_sink();
            if e.can_request_extended_memory_requirements() {
                unsafe {
                    (*req.as_mut_ptr()).pNext = sink_dedicated_alloc.as_mut_ptr() as _;
                }
                object.requirements2().query(&mut req);
            } else {
                unsafe {
                    (*req.as_mut_ptr()).memoryRequirements = object.requirements();

                    // 情報取れないのでないものとして設定
                    (*sink_dedicated_alloc.as_mut_ptr()).prefersDedicatedAllocation = false as _;
                    (*sink_dedicated_alloc.as_mut_ptr()).requiresDedicatedAllocation = false as _;
                }
            }

            requirements.push(unsafe { (req.assume_init(), sink_dedicated_alloc.assume_init()) });
            objects.push(object);
        }

        let alloc_info = ObjectAllocationInfo::compute(&requirements);

        // prefer host coherent(for less operations)
        let memory_type = self
            .host_visible_memory_types
            .iter()
            .find(|t| {
                (alloc_info.combined_memory_index_mask & t.index_mask()) != 0 && t.is_coherent
            })
            .or_else(|| {
                self.host_visible_memory_types
                    .iter()
                    .find(|t| (alloc_info.combined_memory_index_mask & t.index_mask()) != 0)
            })
            .expect("no memory type index");
        let requires_flushing = !memory_type.is_coherent;
        let memory_index = memory_type.index;

        let combined_native_memory = if alloc_info.total_size >= Self::SMALL_ALLOCATION_THRESHOLD {
            // SMALL_ALLOCATION_THRESHOLD以上の場合はひとつのNativeを確保して配置する
            tracing::trace!(
                { total_size = alloc_info.total_size },
                "multiple request is enough big to sharing one native memory"
            );

            Some(make_shared_mutable_ref(
                br::DeviceMemoryRequest::allocate(alloc_info.total_size as _, memory_index)
                    .execute(e.device().clone())?,
            ))
        } else {
            None
        };

        let (mut bound_objects, mut binding_info) = (
            Vec::with_capacity(objects.len()),
            Vec::with_capacity(objects.len()),
        );
        for (((object, exact_size), mode), (req, _)) in objects
            .into_iter()
            .zip(exact_sizes.into_iter())
            .zip(alloc_info.allocation_modes.into_iter())
            .zip(requirements.into_iter())
        {
            match mode {
                ObjectAllocationMode::Dedicated => {
                    tracing::trace!("requested resource is enough big for 1:1 allocation");

                    // prefer host coherent(for less operations)
                    let memory_type = self
                        .host_visible_memory_types
                        .iter()
                        .find(|t| {
                            (req.memoryRequirements.memoryTypeBits & t.index_mask()) != 0
                                && t.is_coherent
                        })
                        .or_else(|| {
                            self.host_visible_memory_types.iter().find(|t| {
                                (req.memoryRequirements.memoryTypeBits & t.index_mask()) != 0
                            })
                        })
                        .expect("no memory type index");
                    let requires_flushing = !memory_type.is_coherent;

                    let mut memory_req = br::DeviceMemoryRequest::allocate(
                        req.memoryRequirements.size as _,
                        memory_type.index,
                    );
                    if e.dedicated_allocation_available() {
                        memory_req = unsafe { memory_req.for_dedicated_buffer_allocation(&object) };
                    }
                    let memory = memory_req.execute(e.device().clone())?;

                    binding_info.push(br::vk::VkBindBufferMemoryInfoKHR {
                        sType: br::vk::VkBindBufferMemoryInfoKHR::TYPE,
                        pNext: core::ptr::null(),
                        buffer: object.native_ptr(),
                        memory: memory.native_ptr(),
                        memoryOffset: 0,
                    });
                    bound_objects.push(Buffer {
                        object,
                        memory_block: BackingMemory::Native(memory),
                        requires_flushing,
                        offset: 0,
                        size: exact_size as _,
                        malloc: (req.memoryRequirements.size, 0),
                    });
                }
                ObjectAllocationMode::Small(offset) => {
                    if let Some(ref combined) = combined_native_memory {
                        // placement into combined native memory

                        binding_info.push(br::vk::VkBindBufferMemoryInfoKHR {
                            sType: br::vk::VkBindBufferMemoryInfoKHR::TYPE,
                            pNext: core::ptr::null(),
                            buffer: object.native_ptr(),
                            memory: combined.borrow().native_ptr(),
                            memoryOffset: offset,
                        });
                        bound_objects.push(Buffer {
                            object,
                            memory_block: BackingMemory::NativeShared(combined.clone()),
                            requires_flushing: false,
                            offset,
                            size: exact_size as _,
                            malloc: (req.memoryRequirements.size, offset),
                        });
                    } else {
                        // normal small allocation
                        let (memory, offset) =
                            self.allocate_internal(e.device(), &req, memory_index, |_| {
                                unreachable!("no dedicated allocation must occurs!")
                            })?;
                        // TODO: 強制アラインメント(VkMemoryRequirements::sizeがアラインメント調整用パディングを含んでいる前提
                        // この前提が崩れることがあったら考え直す)
                        let aligned_offset = align2_u64(offset, req.memoryRequirements.alignment);

                        binding_info.push(br::vk::VkBindBufferMemoryInfoKHR {
                            sType: br::vk::VkBindBufferMemoryInfoKHR::TYPE,
                            pNext: core::ptr::null(),
                            buffer: object.native_ptr(),
                            memory: match memory {
                                BackingMemory::Managed(ref m) => m.borrow().object.native_ptr(),
                                BackingMemory::Native(ref m) => m.native_ptr(),
                                BackingMemory::NativeShared(ref m) => m.borrow().native_ptr(),
                            },
                            memoryOffset: aligned_offset,
                        });
                        bound_objects.push(Buffer {
                            object,
                            memory_block: memory,
                            requires_flushing,
                            offset: aligned_offset,
                            size: exact_size as _,
                            malloc: (req.memoryRequirements.size, offset),
                        });
                    }
                }
            }
        }

        bind_buffers(e, &binding_info)?;
        Ok(bound_objects)
    }

    pub fn allocate_upload_buffer_array<const N: usize>(
        &mut self,
        e: &peridot::Graphics,
        descs: [br::BufferDesc; N],
    ) -> br::Result<[Buffer; N]> {
        self.allocate_multiple_upload_buffers(e, descs)
            .map(|x| unsafe { x.try_into().unwrap_unchecked() })
    }

    pub fn allocate_upload_buffer_with_contents(
        &mut self,
        e: &peridot::Graphics,
        contents: impl IntoIterator<Item = peridot::BufferContent>,
        usage: br::BufferUsage,
    ) -> br::Result<(Buffer, Vec<u64>)> {
        let mut bp = peridot::BufferPrealloc::new(e);
        let offsets = contents.into_iter().map(|c| bp.add(c)).collect::<Vec<_>>();
        let obj = self.allocate_upload_buffer(e, bp.build_desc_custom_usage(usage))?;

        Ok((obj, offsets))
    }

    pub fn allocate_upload_linear_image_buffer(
        &mut self,
        e: &peridot::Graphics,
        width: u32,
        height: u32,
        format: peridot::PixelFormat,
        usage: br::BufferUsage,
    ) -> br::Result<LinearImageBuffer> {
        let (byte_length, _alignment, row_texels) =
            self.compute_optimal_linear_image_buffer_layout(width, height, format);
        let object =
            self.allocate_upload_buffer(e, br::BufferDesc::new(byte_length as _, usage))?;

        Ok(LinearImageBuffer {
            inner: object,
            row_texels,
            height,
        })
    }

    /// returns: (byte length, alignment, row texel count)
    pub fn compute_optimal_linear_image_buffer_layout(
        &self,
        width: u32,
        height: u32,
        format: peridot::PixelFormat,
    ) -> (u64, u64, u32) {
        let row_texels = align2(
            width as usize * (format.bpp() >> 3),
            self.optimal_buffer_linear_image_placement_info
                .row_pitch_alignment as usize,
        ) / (format.bpp() >> 3);

        (
            row_texels as u64 * height as u64 * (format.bpp() as u64 >> 3),
            self.optimal_buffer_linear_image_placement_info
                .alignment
                .lcm(&(format.alignment() as u64)),
            row_texels as u32,
        )
    }

    pub fn allocate_device_local_image(
        &mut self,
        e: &peridot::Graphics,
        desc: br::ImageDesc,
    ) -> br::Result<Image> {
        let mut o = desc.create(e.device().clone())?;

        let mut req = br::vk::VkMemoryRequirements2KHR::uninit_sink();
        let mut sink_dedicated_alloc = br::vk::VkMemoryDedicatedRequirementsKHR::uninit_sink();
        if e.can_request_extended_memory_requirements() {
            unsafe {
                (*req.as_mut_ptr()).pNext = sink_dedicated_alloc.as_mut_ptr() as _;
            }
            o.requirements2().query(&mut req);
        } else {
            unsafe {
                (*req.as_mut_ptr()).memoryRequirements = o.requirements();
            }
        }
        let req = unsafe { req.assume_init() };

        let memory_index = self
            .device_local_memory_types
            .iter()
            .find(|t| (req.memoryRequirements.memoryTypeBits & t.index_mask()) != 0)
            .expect("no memory type index")
            .index;

        let (memory, offset) = self.allocate_internal(e.device(), &req, memory_index, |r| {
            if e.dedicated_allocation_available() {
                tracing::info!("using dedicated allocation");

                unsafe { r.for_dedicated_image_allocation(&o) }
            } else {
                r
            }
        })?;
        // TODO: 強制アラインメント(VkMemoryRequirements::sizeがアラインメント調整用パディングを含んでいる前提
        // この前提が崩れることがあったら考え直す)
        let aligned_offset = align2_u64(offset, req.memoryRequirements.alignment);
        match memory {
            BackingMemory::Managed(ref m) => o.bind(&m.borrow().object, aligned_offset as _),
            BackingMemory::Native(ref m) => o.bind(m, aligned_offset as _),
            BackingMemory::NativeShared(ref m) => o.bind(&m.borrow(), aligned_offset as _),
        }?;

        Ok(Image {
            object: o,
            memory_block: memory,
            offset: aligned_offset,
            byte_length: req.memoryRequirements.size as _,
            malloc_offset: offset,
        })
    }

    pub fn allocate_multiple_device_local_images<'r>(
        &mut self,
        e: &peridot::Graphics,
        descriptions: impl IntoIterator<Item = br::ImageDesc<'r>>,
    ) -> br::Result<Vec<Image>> {
        let descs = descriptions.into_iter();
        let (s, _) = descs.size_hint();
        let (mut objects, mut requirements) = (Vec::with_capacity(s), Vec::with_capacity(s));
        for d in descs {
            let object = d.create(e.device().clone())?;

            let mut req = br::vk::VkMemoryRequirements2KHR::uninit_sink();
            let mut sink_dedicated_alloc = br::vk::VkMemoryDedicatedRequirementsKHR::uninit_sink();
            if e.can_request_extended_memory_requirements() {
                unsafe {
                    (*req.as_mut_ptr()).pNext = sink_dedicated_alloc.as_mut_ptr() as _;
                }
                object.requirements2().query(&mut req);
            } else {
                unsafe {
                    (*req.as_mut_ptr()).memoryRequirements = object.requirements();

                    // 情報取れないのでないものとして設定
                    (*sink_dedicated_alloc.as_mut_ptr()).prefersDedicatedAllocation = false as _;
                    (*sink_dedicated_alloc.as_mut_ptr()).requiresDedicatedAllocation = false as _;
                }
            }

            requirements.push(unsafe { (req.assume_init(), sink_dedicated_alloc.assume_init()) });
            objects.push(object);
        }

        let alloc_info = ObjectAllocationInfo::compute(&requirements);

        let memory_index = self
            .device_local_memory_types
            .iter()
            .find(|t| (alloc_info.combined_memory_index_mask & t.index_mask()) != 0)
            .expect("no memory type index")
            .index;

        let combined_native_memory = if alloc_info.total_size >= Self::SMALL_ALLOCATION_THRESHOLD {
            // SMALL_ALLOCATION_THRESHOLD以上の場合はひとつのNativeを確保して配置する
            tracing::trace!(
                { total_size = alloc_info.total_size },
                "multiple request is enough big to sharing one native memory"
            );

            Some(make_shared_mutable_ref(
                br::DeviceMemoryRequest::allocate(alloc_info.total_size as _, memory_index)
                    .execute(e.device().clone())?,
            ))
        } else {
            None
        };

        let (mut bound_objects, mut bind_infos) = (
            Vec::with_capacity(objects.len()),
            Vec::with_capacity(objects.len()),
        );
        for ((mut object, mode), (req, _)) in objects
            .into_iter()
            .zip(alloc_info.allocation_modes.into_iter())
            .zip(requirements.into_iter())
        {
            match mode {
                ObjectAllocationMode::Dedicated => {
                    tracing::trace!("requested resource is enough big for 1:1 allocation");

                    let memory_index = self
                        .device_local_memory_types
                        .iter()
                        .find(|t| (req.memoryRequirements.memoryTypeBits & t.index_mask()) != 0)
                        .expect("no memory type index")
                        .index;

                    let mut memory_req = br::DeviceMemoryRequest::allocate(
                        req.memoryRequirements.size as _,
                        memory_index,
                    );
                    if e.dedicated_allocation_available() {
                        memory_req = unsafe { memory_req.for_dedicated_image_allocation(&object) };
                    }
                    let memory = memory_req.execute(e.device().clone())?;
                    object.bind(&memory, 0)?;

                    bind_infos.push(br::vk::VkBindImageMemoryInfoKHR {
                        sType: br::vk::VkBindImageMemoryInfoKHR::TYPE,
                        pNext: core::ptr::null(),
                        image: object.native_ptr(),
                        memory: memory.native_ptr(),
                        memoryOffset: 0,
                    });
                    bound_objects.push(Image {
                        object,
                        memory_block: BackingMemory::Native(memory),
                        offset: 0,
                        byte_length: req.memoryRequirements.size as _,
                        malloc_offset: 0,
                    });
                }
                ObjectAllocationMode::Small(offset) => {
                    if let Some(ref combined) = combined_native_memory {
                        // placement into combined native memory
                        bind_infos.push(br::vk::VkBindImageMemoryInfoKHR {
                            sType: br::vk::VkBindImageMemoryInfoKHR::TYPE,
                            pNext: core::ptr::null(),
                            image: object.native_ptr(),
                            memory: combined.borrow().native_ptr(),
                            memoryOffset: offset as _,
                        });
                        bound_objects.push(Image {
                            object,
                            memory_block: BackingMemory::NativeShared(combined.clone()),
                            offset,
                            byte_length: req.memoryRequirements.size as _,
                            malloc_offset: offset,
                        });
                    } else {
                        // normal small allocation
                        let (memory, offset) =
                            self.allocate_internal(e.device(), &req, memory_index, |_| {
                                unreachable!("no dedicated allocation must occurs!")
                            })?;
                        // TODO: 強制アラインメント(VkMemoryRequirements::sizeがアラインメント調整用パディングを含んでいる前提
                        // この前提が崩れることがあったら考え直す)
                        let aligned_offset = align2_u64(offset, req.memoryRequirements.alignment);

                        bind_infos.push(br::vk::VkBindImageMemoryInfoKHR {
                            sType: br::vk::VkBindImageMemoryInfoKHR::TYPE,
                            pNext: core::ptr::null(),
                            image: object.native_ptr(),
                            memory: match memory {
                                BackingMemory::Managed(ref m) => m.borrow().object.native_ptr(),
                                BackingMemory::Native(ref m) => m.native_ptr(),
                                BackingMemory::NativeShared(ref m) => m.borrow().native_ptr(),
                            },
                            memoryOffset: aligned_offset as _,
                        });
                        bound_objects.push(Image {
                            object,
                            memory_block: memory,
                            offset: aligned_offset,
                            byte_length: req.memoryRequirements.size as _,
                            malloc_offset: offset,
                        });
                    }
                }
            }
        }

        bind_images(e, &bind_infos)?;
        Ok(bound_objects)
    }

    pub fn allocate_device_local_image_array<const N: usize>(
        &mut self,
        e: &peridot::Graphics,
        descs: [br::ImageDesc; N],
    ) -> br::Result<[Image; N]> {
        self.allocate_multiple_device_local_images(e, descs)
            .map(|x| unsafe { x.try_into().unwrap_unchecked() })
    }
}

enum ObjectAllocationMode {
    Small(u64),
    Dedicated,
}
struct ObjectAllocationInfo {
    total_size: u64,
    combined_memory_index_mask: u32,
    allocation_modes: Vec<ObjectAllocationMode>,
}
impl ObjectAllocationInfo {
    fn compute(
        requirements: &[(
            br::vk::VkMemoryRequirements2KHR,
            br::vk::VkMemoryDedicatedRequirementsKHR,
        )],
    ) -> Self {
        let mut collected = Self {
            total_size: 0,
            combined_memory_index_mask: u32::MAX,
            allocation_modes: Vec::with_capacity(requirements.len()),
        };

        for (r, dedicated) in requirements.iter() {
            if dedicated.requiresDedicatedAllocation != 0
                || dedicated.prefersDedicatedAllocation != 0
            {
                // use dedicated allocation for this resource
                collected
                    .allocation_modes
                    .push(ObjectAllocationMode::Dedicated);
            } else {
                let offset = align2_u64(collected.total_size, r.memoryRequirements.alignment);
                collected.total_size = offset + r.memoryRequirements.size;
                collected.combined_memory_index_mask &= r.memoryRequirements.memoryTypeBits;
                collected
                    .allocation_modes
                    .push(ObjectAllocationMode::Small(offset));
            }
        }

        collected
    }
}

fn bind_buffers(
    e: &peridot::Graphics,
    binds: &[br::vk::VkBindBufferMemoryInfoKHR],
) -> br::Result<()> {
    if e.extended_memory_binding_available() {
        // use batched binding

        e.device().bind_buffers(&binds)?;
    } else {
        // use old binding

        for b in binds.iter() {
            unsafe {
                br::vkresolve::bind_buffer_memory(
                    e.device().native_ptr(),
                    b.buffer,
                    b.memory,
                    b.memoryOffset,
                )
                .into_result()?;
            }
        }
    }

    Ok(())
}

fn bind_images(
    e: &peridot::Graphics,
    binds: &[br::vk::VkBindImageMemoryInfoKHR],
) -> br::Result<()> {
    if e.extended_memory_binding_available() {
        // use batched binding

        e.device().bind_images(&binds)?;
    } else {
        // use old binding

        for b in binds.iter() {
            unsafe {
                br::vkresolve::bind_image_memory(
                    e.device().native_ptr(),
                    b.image,
                    b.memory,
                    b.memoryOffset,
                )
                .into_result()?;
            }
        }
    }

    Ok(())
}
