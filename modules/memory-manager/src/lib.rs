use std::collections::BTreeMap;

use bedrock as br;
use br::{DeviceChild, DeviceMemory, MemoryBound, VkHandle};
use peridot::mthelper::{make_shared_mutable_ref, DynamicMutabilityProvider, SharedMutableRef};

pub struct MemoryType {
    pub index: u32,
    pub heap_index: u32,
    pub is_coherent: bool,
    pub is_cached: bool,
}
impl MemoryType {
    pub const fn index_mask(&self) -> u32 {
        1 << self.index
    }

    pub const fn host_property_flags(&self) -> br::vk::VkMemoryPropertyFlags {
        let coherent_bit = if self.is_coherent {
            br::vk::VK_MEMORY_PROPERTY_HOST_COHERENT_BIT
        } else {
            0
        };
        let cached_bit = if self.is_cached {
            br::vk::VK_MEMORY_PROPERTY_HOST_CACHED_BIT
        } else {
            0
        };

        coherent_bit | cached_bit
    }
}
pub struct HeapStats {
    pub info: br::vk::VkMemoryHeap,
    pub used_bytes: u64,
}

// 64 consists of sizeof(mat4) = 4 * 4 * sizeof(f32)
const SLAB_ALLOC_BASE_SIZE: usize = 64;

pub struct MemoryBlockSlab {
    offset: u64,
    free_bits: u32,
    next: Option<Box<MemoryBlockSlab>>,
}
impl MemoryBlockSlab {
    pub const fn new(offset: u64) -> Self {
        Self {
            offset,
            free_bits: 0,
            next: None,
        }
    }

    pub const fn is_filled(&self) -> bool {
        self.free_bits.count_zeros() == 0
    }

    pub const fn is_empty(&self) -> bool {
        self.free_bits.count_ones() == 0
    }

    pub const fn get_first_free(&self) -> Option<usize> {
        let x = self.free_bits.trailing_ones() as usize;

        if x < 32 {
            Some(x)
        } else {
            None
        }
    }

    pub fn contains_object(&self, object_size: usize, offset: u64) -> bool {
        (self.offset..self.offset + (object_size * 32) as u64).contains(&offset)
    }

    pub fn try_compute_object_index(&self, object_size: usize, offset: u64) -> Option<usize> {
        if !self.contains_object(object_size, offset) {
            return None;
        }

        Some((offset - self.offset) as usize / object_size)
    }

    pub fn mark_used(&mut self, index: usize) {
        self.free_bits |= 1 << index;
    }

    pub fn mark_unused(&mut self, index: usize) {
        self.free_bits &= !(1 << index);
    }
}

/// Slab Allocatorによる小サイズメモリ（64bytes～）の管理
pub struct MemoryBlockSlabCache {
    pub object_size: usize,
    pub empty_slabs_head: Option<Box<MemoryBlockSlab>>,
    pub partially_allocated_slabs_head: Option<Box<MemoryBlockSlab>>,
    pub filled_slabs_head: Option<Box<MemoryBlockSlab>>,
}
impl MemoryBlockSlabCache {
    pub fn new(object_size: usize) -> Self {
        Self {
            object_size,
            empty_slabs_head: None,
            partially_allocated_slabs_head: None,
            filled_slabs_head: None,
        }
    }

    pub const fn is_empty(&self) -> bool {
        self.partially_allocated_slabs_head.is_none() && self.filled_slabs_head.is_none()
    }

    pub fn clear_chains(&mut self) {
        self.empty_slabs_head = None;
        self.partially_allocated_slabs_head = None;
        self.filled_slabs_head = None;
    }

    pub fn append_empty_slab(&mut self, offset: u64) {
        Self::append_slab_chain(
            &mut self.empty_slabs_head,
            Box::new(MemoryBlockSlab::new(offset)),
        );
    }

    fn append_slab_chain(head: &mut Option<Box<MemoryBlockSlab>>, new_slab: Box<MemoryBlockSlab>) {
        match head.as_mut() {
            None => {
                *head = Some(new_slab);
            }
            Some(h) => {
                let mut term = h;
                loop {
                    match term.next {
                        Some(ref mut t) => {
                            term = t;
                        }
                        None => {
                            term.next = Some(new_slab);
                            break;
                        }
                    }
                }
            }
        }
    }

    /// returns global offset in container MemoryBlock
    pub fn find_free_object_offset(&mut self) -> Option<u64> {
        // finds from partially
        let mut prev = None::<*mut MemoryBlockSlab>;
        let mut p = self
            .partially_allocated_slabs_head
            .as_mut()
            .map(|x| x.as_mut() as *mut MemoryBlockSlab);
        while let Some(h) = p {
            let slab = unsafe { &mut *h };
            if let Some(x) = slab.get_first_free() {
                // found
                slab.mark_used(x);
                let ptr = slab.offset + (self.object_size * x) as u64;
                if slab.is_filled() {
                    // move to filled
                    let filled = match prev {
                        Some(p) => unsafe {
                            core::mem::replace(&mut (&mut *p).next, slab.next.take())
                        },
                        None => core::mem::replace(
                            &mut self.partially_allocated_slabs_head,
                            slab.next.take(),
                        ),
                    };
                    // whileにこれたならかならずあるはず
                    Self::append_slab_chain(&mut self.filled_slabs_head, unsafe {
                        filled.unwrap_unchecked()
                    });
                }

                return Some(ptr);
            }

            prev = Some(h);
            p = slab.next.as_mut().map(|x| x.as_mut() as *mut _);
        }

        // finds from empty
        let mut prev = None::<*mut MemoryBlockSlab>;
        let mut p = self
            .empty_slabs_head
            .as_mut()
            .map(|x| x.as_mut() as *mut MemoryBlockSlab);
        while let Some(h) = p {
            let slab = unsafe { &mut *h };
            if let Some(x) = slab.get_first_free() {
                // found
                slab.mark_used(x);
                let ptr = slab.offset + (self.object_size * x) as u64;

                // move to partial
                let filled = match prev {
                    Some(p) => unsafe { core::mem::replace(&mut (&mut *p).next, slab.next.take()) },
                    None => core::mem::replace(&mut self.empty_slabs_head, slab.next.take()),
                };
                // whileにこれたならかならずあるはず
                Self::append_slab_chain(&mut self.partially_allocated_slabs_head, unsafe {
                    filled.unwrap_unchecked()
                });

                return Some(ptr);
            }

            prev = Some(h);
            p = slab.next.as_mut().map(|x| x.as_mut() as *mut _);
        }

        None
    }

    pub fn free_object(&mut self, offset: u64) {
        // finds from filled
        let mut prev = None::<*mut MemoryBlockSlab>;
        let mut p = self
            .filled_slabs_head
            .as_mut()
            .map(|x| x.as_mut() as *mut MemoryBlockSlab);
        while let Some(h) = p {
            let slab = unsafe { &mut *h };

            if let Some(n) = slab.try_compute_object_index(self.object_size, offset) {
                // found
                slab.mark_unused(n);

                // move to partial
                let filled = match prev {
                    Some(p) => unsafe { core::mem::replace(&mut (&mut *p).next, slab.next.take()) },
                    None => core::mem::replace(&mut self.filled_slabs_head, slab.next.take()),
                };
                // whileにこれたならかならずあるはず
                Self::append_slab_chain(&mut self.partially_allocated_slabs_head, unsafe {
                    filled.unwrap_unchecked()
                });

                return;
            }

            prev = Some(h);
            p = slab.next.as_mut().map(|x| x.as_mut() as *mut _);
        }

        // finds from partial
        let mut prev = None::<*mut MemoryBlockSlab>;
        let mut p = self
            .partially_allocated_slabs_head
            .as_mut()
            .map(|x| x.as_mut() as *mut MemoryBlockSlab);
        while let Some(h) = p {
            let slab = unsafe { &mut *h };

            if let Some(n) = slab.try_compute_object_index(self.object_size, offset) {
                // found
                slab.mark_unused(n);

                if slab.is_empty() {
                    // move to empty
                    let filled = match prev {
                        Some(p) => unsafe {
                            core::mem::replace(&mut (&mut *p).next, slab.next.take())
                        },
                        None => core::mem::replace(
                            &mut self.partially_allocated_slabs_head,
                            slab.next.take(),
                        ),
                    };
                    // whileにこれたならかならずあるはず
                    Self::append_slab_chain(&mut self.empty_slabs_head, unsafe {
                        filled.unwrap_unchecked()
                    });
                }

                return;
            }

            prev = Some(h);
            p = slab.next.as_mut().map(|x| x.as_mut() as *mut _);
        }
    }
}

const SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE: usize = SLAB_ALLOC_BASE_SIZE * 32;

/// Slab Allocatorが管理する用の一定サイズのメモリブロックを切り出す（Buddy Systemの簡易実装）
pub struct MemoryBlockSlabCacheFreeAreaManager {
    free_block_index_by_chains: Vec<Vec<u64>>,
}
impl MemoryBlockSlabCacheFreeAreaManager {
    pub fn new(block_count: u32) -> Self {
        // power of 2に切り上げ
        let aligned_block_count = 1 << ((32 - block_count.leading_zeros()) - 1);
        let mut n = 1;

        Self {
            free_block_index_by_chains: core::iter::from_fn(|| {
                n <<= 1;
                Some(n >> 1)
            })
            .take_while(|&n| n < aligned_block_count)
            .map(|_| Vec::new())
            .chain(core::iter::once(vec![0]))
            .collect(),
        }
    }

    fn block_count_to_level(count: u32) -> usize {
        count.trailing_zeros() as _
    }

    /// returns block number(not global byte offset)
    pub fn acquire(&mut self, block_count: u32) -> Option<u64> {
        // power of 2に切り上げ
        let aligned_block_count = 1u32 << ((32 - block_count.leading_zeros()) - 1);
        let base_level = Self::block_count_to_level(aligned_block_count);

        let exact_match_block = self.free_block_index_by_chains[base_level as usize].pop();
        if let Some(b) = exact_match_block {
            return Some(b);
        }

        // divide large blocks
        let large_block = self
            .free_block_index_by_chains
            .iter_mut()
            .enumerate()
            .filter_map(|(n, x)| {
                if n > base_level as usize {
                    x.pop().map(move |f| (n, f))
                } else {
                    None
                }
            })
            .next();
        if let Some((found_level, b)) = large_block {
            let mut left_block_count = (1 << found_level) - block_count;
            for l in (1..=found_level - 1).rev() {
                self.free_block_index_by_chains[l].push(b + (1 << l) as u64);
                left_block_count -= 1 << l;
                if left_block_count == 0 {
                    break;
                }
            }

            return Some(b);
        }

        None
    }

    pub fn release_power_of_two_block(&mut self, block_number: u32, mut block_count: u32) {
        let base_level = Self::block_count_to_level(block_count);

        for l in base_level..self.free_block_index_by_chains.len() {
            match self.free_block_index_by_chains[l]
                .iter()
                .enumerate()
                .find(|&(_, &b)| (block_number + block_count) as u64 == b)
            {
                Some((n, _)) => {
                    // chain
                    block_count <<= 1;
                    self.free_block_index_by_chains[l].swap_remove(n);
                }
                None => {
                    // not chain
                    self.free_block_index_by_chains[l].push(block_number as _);
                    return;
                }
            }
        }

        unreachable!("chained to large block more than container??");
    }

    pub fn release(&mut self, mut block_number: u32, mut block_count: u32) {
        // power of 2に切り上げ(こいつが最大レベル)
        let aligned_block_count = 1u32 << ((32 - block_count.leading_zeros()) - 1);
        let max_level = Self::block_count_to_level(aligned_block_count);

        // 下からpower of 2に切り出しながら返却
        for l in (0..max_level).rev() {
            if block_count >= (1 << l) {
                // subdiv
                self.release_power_of_two_block(block_number, 1 << l);
                block_number += 1 << l;
                block_count -= 1 << l;
            }

            if block_count <= 0 {
                break;
            }
        }
    }
}

fn power_of_2_series_from_u64(mut base: u64) -> impl Iterator<Item = u64> {
    core::iter::from_fn(move || {
        base <<= 1;
        Some(base >> 1)
    })
}
fn power_of_2_series_from(mut base: usize) -> impl Iterator<Item = usize> {
    core::iter::from_fn(move || {
        base <<= 1;
        Some(base >> 1)
    })
}

pub struct MemoryBlock<Device: br::Device> {
    pub object: br::DeviceMemoryObject<Device>,
    /// 64*2^n where n is index of the vec
    pub slab_cache_by_object_size: Vec<MemoryBlockSlabCache>,
    pub slab_cache_free_area_manager: MemoryBlockSlabCacheFreeAreaManager,
}
impl<Device: br::Device> MemoryBlock<Device> {
    pub fn new(object: br::DeviceMemoryObject<Device>, total_size: u64) -> Self {
        let total_block_count = total_size / SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64;

        Self {
            object,
            slab_cache_by_object_size: power_of_2_series_from(SLAB_ALLOC_BASE_SIZE)
                .take_while(|&x| x <= MemoryManager::SMALL_ALLOCATION_THRESHOLD as usize)
                .map(MemoryBlockSlabCache::new)
                .collect(),
            slab_cache_free_area_manager: MemoryBlockSlabCacheFreeAreaManager::new(
                total_block_count as _,
            ),
        }
    }

    /// SLAB_ALLOC_BASE_SIZE以上 2の累乗数に切り上げ
    fn aligned_object_size(size: usize) -> usize {
        1usize << (64 - size.max(SLAB_ALLOC_BASE_SIZE).leading_zeros() - 1)
    }

    /// slab_cache_by_object_sizeのキーを計算
    const fn slab_allocator_key(aligned_size: usize) -> usize {
        (aligned_size.trailing_zeros() - SLAB_ALLOC_BASE_SIZE.trailing_zeros()) as usize
    }

    pub fn suballocate(&mut self, size: usize) -> Option<u64> {
        let slab_object_size = Self::aligned_object_size(size);
        let slab_allocator_key = Self::slab_allocator_key(slab_object_size);

        if let Some(o) =
            self.slab_cache_by_object_size[slab_allocator_key as usize].find_free_object_offset()
        {
            return Some(o);
        }

        // 既存のslab cache内ではみつからなかった 新しく登録して試す
        let new_slab_cache_offset_block = self
            .slab_cache_free_area_manager
            .acquire(1 << (slab_allocator_key + 1))?;
        self.slab_cache_by_object_size[slab_allocator_key as usize]
            .append_empty_slab(new_slab_cache_offset_block);
        self.slab_cache_by_object_size[slab_allocator_key as usize].find_free_object_offset()
    }

    pub fn free(&mut self, size: usize, offset: u64) {
        let slab_object_size = Self::aligned_object_size(size);
        let slab_allocator_key = Self::slab_allocator_key(slab_object_size);

        tracing::trace!(
            "releasing suballocated block size:{}(aligned:{}) from offset {}",
            size,
            slab_object_size,
            offset
        );

        self.slab_cache_by_object_size[slab_allocator_key as usize].free_object(offset);
        // TODO: 空になった場合にfree_area_managerにメモリブロックを返却する ただここで今すぐやらなくてもいいかも
    }
}

enum BackingMemory {
    Managed(SharedMutableRef<MemoryBlock<peridot::DeviceObject>>),
    Native(br::DeviceMemoryObject<peridot::DeviceObject>),
}

pub struct Buffer {
    object: br::BufferObject<peridot::DeviceObject>,
    memory_block: BackingMemory,
    offset: u64,
    size: usize,
}
impl Buffer {
    pub fn guard_map<R>(&mut self, op: impl FnOnce(*mut ()) -> R) -> br::Result<R> {
        match self.memory_block {
            BackingMemory::Managed(ref m) => {
                let mut locked = m.borrow_mut();
                let ptr = unsafe {
                    locked
                        .object
                        .map_raw(self.offset..self.offset + self.size as br::vk::VkDeviceSize)?
                };
                let r = op(ptr as _);
                unsafe {
                    locked.object.unmap();
                }

                Ok(r)
            }
            BackingMemory::Native(ref mut m) => {
                let ptr = unsafe {
                    m.map_raw(self.offset..self.offset + self.size as br::vk::VkDeviceSize)?
                };
                let r = op(ptr as _);
                unsafe {
                    m.unmap();
                }

                Ok(r)
            }
        }
    }
}
impl Drop for Buffer {
    fn drop(&mut self) {
        if let BackingMemory::Managed(ref b) = self.memory_block {
            b.borrow_mut().free(self.size, self.offset);
        }
    }
}
impl br::VkHandle for Buffer {
    type Handle = <br::BufferObject<peridot::DeviceObject> as br::VkHandle>::Handle;

    fn native_ptr(&self) -> Self::Handle {
        self.object.native_ptr()
    }
}
impl br::VkObject for Buffer {
    const TYPE: br::vk::VkObjectType =
        <br::BufferObject<peridot::DeviceObject> as br::VkObject>::TYPE;
}
impl br::VkHandleMut for Buffer {
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.object.native_ptr_mut()
    }
}
impl br::DeviceChild for Buffer {
    type ConcreteDevice =
        <br::BufferObject<peridot::DeviceObject> as br::DeviceChild>::ConcreteDevice;

    fn device(&self) -> &Self::ConcreteDevice {
        self.object.device()
    }
}
impl br::Buffer for Buffer {}

pub struct MemoryManager {
    /// Device Local only
    pub device_local_memory_types: Vec<MemoryType>,
    /// Host Visible only
    pub host_visible_memory_types: Vec<MemoryType>,
    /// Both Device Local and Host Visible (this memory can be directly mapped)
    pub direct_memory_types: Vec<MemoryType>,
    pub heap_stats: Vec<HeapStats>,
    pub managed_blocks_per_type:
        BTreeMap<u32, Vec<SharedMutableRef<MemoryBlock<peridot::DeviceObject>>>>,
}
impl MemoryManager {
    /// 1MB以下はSmall Allocationとして判定する
    const SMALL_ALLOCATION_THRESHOLD: u64 = 1024 * 1024;

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
            managed_blocks_per_type: BTreeMap::new(),
        }
    }

    fn allocate_buffer_internal(
        &mut self,
        mut object: br::BufferObject<peridot::DeviceObject>,
        memory_requirements: br::vk::VkMemoryRequirements,
        memory_index: u32,
    ) -> br::Result<Buffer> {
        tracing::debug!(
            "allocate {} bytes a:{} bytes type_mask:{:08x} in type #{memory_index}",
            memory_requirements.size,
            memory_requirements.alignment,
            memory_requirements.memoryTypeBits
        );

        if memory_requirements.size >= Self::SMALL_ALLOCATION_THRESHOLD {
            tracing::trace!(
                "requested resource(size:{}) is enough big for 1:1 allocation",
                memory_requirements.size
            );

            let memory =
                br::DeviceMemoryRequest::allocate(memory_requirements.size as _, memory_index)
                    .execute(object.device().clone())?;
            object.bind(&memory, 0)?;
            return Ok(Buffer {
                object,
                memory_block: BackingMemory::Native(memory),
                offset: 0,
                size: memory_requirements.size as _,
            });
        }

        let blocks = self
            .managed_blocks_per_type
            .entry(memory_index)
            .or_insert_with(Vec::new);

        if let Some((t, offset)) = blocks
            .iter()
            .filter_map(|t| {
                t.borrow_mut()
                    .suballocate(memory_requirements.size as _)
                    .map(|o| (t, o))
            })
            .next()
        {
            tracing::debug!(
                "suballocation ok! buffer (size {}) will be placed at {}",
                memory_requirements.size,
                offset
            );

            object.bind(&t.borrow().object, offset as _)?;
            return Ok(Buffer {
                object,
                memory_block: BackingMemory::Managed(t.clone()),
                offset: offset as _,
                size: memory_requirements.size as _,
            });
        }

        // 既存のものにはもう入らないので新規で確保
        tracing::debug!(
            "allocating new device memory block: {} bytes on type #{memory_index}",
            Self::SMALL_ALLOCATION_THRESHOLD
        );
        let mut new_block = MemoryBlock::new(
            br::DeviceMemoryRequest::allocate(Self::SMALL_ALLOCATION_THRESHOLD as _, memory_index)
                .execute(object.device().clone())?,
            Self::SMALL_ALLOCATION_THRESHOLD,
        );
        // 絶対確保できるはず
        let new_offset = unsafe {
            new_block
                .suballocate(memory_requirements.size as _)
                .unwrap_unchecked()
        };
        tracing::debug!(
            "first buffer (size {}) will be placed at {}",
            memory_requirements.size,
            new_offset
        );
        object.bind(&new_block.object, new_offset as _)?;
        let new_block = make_shared_mutable_ref(new_block);
        blocks.push(new_block.clone());
        Ok(Buffer {
            object,
            memory_block: BackingMemory::Managed(new_block),
            offset: new_offset as _,
            size: memory_requirements.size as _,
        })
    }

    pub fn allocate_device_local_buffer(
        &mut self,
        e: &peridot::Graphics,
        desc: br::BufferDesc,
    ) -> br::Result<Buffer> {
        let o = desc.create(e.device().clone())?;
        let req = o.requirements();
        let memory_index = self
            .device_local_memory_types
            .iter()
            .find(|t| (req.memoryTypeBits & t.index_mask()) != 0)
            .expect("no memory type index")
            .index;

        self.allocate_buffer_internal(o, req, memory_index)
    }

    pub fn allocate_upload_buffer(
        &mut self,
        e: &peridot::Graphics,
        desc: br::BufferDesc,
    ) -> br::Result<Buffer> {
        let o = desc.create(e.device().clone())?;
        let req = o.requirements();
        // prefer host coherent(for less operations)
        // TODO: coherentじゃないmemory typeが選択されたときどうしよう（Flush/Invalidateが必要なのでなんとか情報を利用側で取れる必要がある）
        let memory_type = self
            .host_visible_memory_types
            .iter()
            .find(|t| (req.memoryTypeBits & t.index_mask()) != 0 && t.is_coherent)
            .or_else(|| {
                self.host_visible_memory_types
                    .iter()
                    .find(|t| (req.memoryTypeBits & t.index_mask()) != 0)
            })
            .expect("no memory type index");
        if !memory_type.is_coherent {
            tracing::warn!(
                "TODO: selected memory type is not host-coherent, requires explicit flushing"
            );
        }

        self.allocate_buffer_internal(o, req, memory_type.index)
    }
}
