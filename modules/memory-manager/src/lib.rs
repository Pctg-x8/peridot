use std::collections::BTreeMap;

use bedrock as br;
use br::{DeviceMemory, MemoryBound, StructureChainQuery};
use num_integer::Integer;
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
    /// (number, count)
    block_info: (u64, u32),
    offset: u64,
    free_bits: u32,
    max: u32,
    next: Option<Box<MemoryBlockSlab>>,
}
impl MemoryBlockSlab {
    pub const fn new(block_info: (u64, u32), offset: u64, max: u32) -> Self {
        Self {
            block_info,
            offset,
            free_bits: 0,
            max,
            next: None,
        }
    }

    pub const fn is_filled(&self) -> bool {
        self.free_bits.count_ones() == self.max
    }

    pub const fn is_empty(&self) -> bool {
        self.free_bits.count_ones() == 0
    }

    pub const fn get_first_free(&self) -> Option<usize> {
        let x = self.free_bits.trailing_ones();

        if x < self.max {
            Some(x as _)
        } else {
            None
        }
    }

    pub fn contains_object(&self, object_size: usize, offset: u64) -> bool {
        (self.offset..self.offset + (object_size * self.max as usize) as u64).contains(&offset)
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

    pub fn append_empty_slab(&mut self, block_info: (u64, u32), offset: u64, max: u32) {
        Self::append_slab_chain(
            &mut self.empty_slabs_head,
            Box::new(MemoryBlockSlab::new(block_info, offset, max)),
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

    pub fn free_object(
        &mut self,
        offset: u64,
        block_release_fn: impl FnOnce(Box<MemoryBlockSlab>),
    ) {
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
                    // move to empty(or release)
                    let empty = match prev {
                        Some(p) => unsafe {
                            core::mem::replace(&mut (&mut *p).next, slab.next.take())
                        },
                        None => core::mem::replace(
                            &mut self.partially_allocated_slabs_head,
                            slab.next.take(),
                        ),
                    };

                    if let Some(e) = empty {
                        // releaseしちゃっていい
                        block_release_fn(e);
                    }
                }

                return;
            }

            prev = Some(h);
            p = slab.next.as_mut().map(|x| x.as_mut() as *mut _);
        }
    }
}

const SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE: usize = SLAB_ALLOC_BASE_SIZE * 32;

pub enum MemoryBlockSlabCacheFreeAreaAcquisitionFailure {
    TooLarge,
    NoBlocksLeft,
}

/// Slab Allocatorが管理する用の一定サイズのメモリブロックを切り出す（Buddy Systemの簡易実装）
pub struct MemoryBlockSlabCacheFreeAreaManager {
    free_block_index_by_chains: Vec<Vec<u64>>,
}
impl MemoryBlockSlabCacheFreeAreaManager {
    #[tracing::instrument]
    pub fn new(block_count: u32) -> Self {
        let aligned_block_count = round_up_to_next_power_of_two(block_count as _);
        tracing::info!({ aligned_block_count }, "Initializing FreeBlockManager");

        Self {
            free_block_index_by_chains: power_of_2_series_from_u64(1)
                .take_while(|&n| n < aligned_block_count)
                .map(|_| Vec::new())
                .chain(core::iter::once(vec![0]))
                .collect(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.free_block_index_by_chains
            .last()
            .is_some_and(|r| r.len() == 1 && r.first() == Some(&0))
    }

    pub fn try_fill(&mut self) -> Option<u32> {
        if self.is_empty() {
            // 最後の要素しかないのでそれをclearすれば全部使ってることになる
            unsafe {
                self.free_block_index_by_chains
                    .last_mut()
                    .unwrap_unchecked()
                    .clear()
            };

            Some(1 << (self.free_block_index_by_chains.len() - 1))
        } else {
            None
        }
    }

    /// returns: (start block number, block count)
    pub fn max_unallocated_memory_block_length(&self) -> (u64, u64) {
        let largest = self
            .free_block_index_by_chains
            .iter()
            .enumerate()
            .rev()
            .find(|(_, v)| !v.is_empty())
            .map(|(level, v)| {
                (
                    unsafe { v.first().copied().unwrap_unchecked() },
                    1 << level,
                    level,
                )
            });
        let Some((mut starting_number, mut block_count, largest_level)) = largest else {
            return (0, 0);
        };

        // find descendant levels for chainable block
        for (level, v) in self.free_block_index_by_chains[..largest_level]
            .iter()
            .enumerate()
            .rev()
        {
            let level_block_count = 1 << level;

            let chainable_before = v
                .iter()
                .find(|&&n| n + level_block_count == starting_number);
            if let Some(&chainable_start) = chainable_before {
                block_count += level_block_count;
                starting_number = chainable_start;
            }

            let chainable_after = v.iter().find(|&&n| starting_number + block_count == n);
            if chainable_after.is_some() {
                block_count += level_block_count;
            }
        }

        (starting_number, block_count)
    }

    fn block_count_to_level(count: u32) -> usize {
        count.trailing_zeros() as _
    }

    #[tracing::instrument(skip(self))]
    /// returns block number(not global byte offset)
    pub fn acquire(
        &mut self,
        block_count: u32,
    ) -> Result<u64, MemoryBlockSlabCacheFreeAreaAcquisitionFailure> {
        tracing::trace!("acquire");

        let aligned_block_count = round_up_to_next_power_of_two(block_count as _) as u32;
        let base_level = Self::block_count_to_level(aligned_block_count);

        if base_level >= self.free_block_index_by_chains.len() {
            // too large
            return Err(MemoryBlockSlabCacheFreeAreaAcquisitionFailure::TooLarge);
        }

        let exact_match_block = self.free_block_index_by_chains[base_level as usize].pop();
        if let Some(b) = exact_match_block {
            return Ok(b);
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

            return Ok(b);
        }

        Err(MemoryBlockSlabCacheFreeAreaAcquisitionFailure::NoBlocksLeft)
    }

    #[tracing::instrument(skip(self))]
    pub fn release_power_of_two_block(&mut self, block_number: u64, mut block_count: u32) {
        tracing::trace!("release");

        let base_level = Self::block_count_to_level(block_count);
        let mut level = self.free_block_index_by_chains.len() - 1;
        for l in base_level..self.free_block_index_by_chains.len() {
            match self.free_block_index_by_chains[l]
                .iter()
                .enumerate()
                .find(|&(_, &b)| block_number + block_count as u64 == b)
            {
                Some((n, _)) => {
                    // chain
                    block_count <<= 1;
                    self.free_block_index_by_chains[l].swap_remove(n);
                }
                None => {
                    // not chain
                    level = l;
                    break;
                }
            }
        }

        self.free_block_index_by_chains[level].push(block_number as _);
    }

    #[tracing::instrument(skip(self))]
    pub fn release(&mut self, mut block_number: u64, mut block_count: u32) {
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

/// 切り上げ a(アラインメント)は2の累乗数
const fn align2(x: usize, a: usize) -> usize {
    (x + (a - 1)) & !(a - 1)
}

/// 最上位ビットのみが立っている状態にする
/// # Example
/// ```
/// assert_eq!(only_top_bit(0b0100_0000), 0b0100_0000);
/// assert_eq!(only_top_bit(0b0110_1101), 0b0100_0000);
/// ```
const fn only_top_bit(x: u64) -> u64 {
    1u64 << (64 - x.leading_zeros() - 1)
}

const fn round_up_to_next_power_of_two(value: u64) -> u64 {
    // Note: 最上位ビット以下のみ立っている状態（= 最上位ビットのみ立っている状態 - 1）を足すと
    // 純粋な2の累乗数以外は繰り上がるので、その状態で再び最上位ビットのみの状態にすると切り上げになる
    only_top_bit(value + only_top_bit(value) - 1) as _
}

pub struct MemoryBlock<Device: br::Device> {
    pub object: br::DeviceMemoryObject<Device>,
    pub total_size: u64,
    /// 64*2^n where n is index of the vec
    pub slab_cache_by_object_size: Vec<MemoryBlockSlabCache>,
    pub slab_cache_free_area_manager: MemoryBlockSlabCacheFreeAreaManager,
}
impl<Device: br::Device> MemoryBlock<Device> {
    pub fn new(object: br::DeviceMemoryObject<Device>, total_size: u64) -> Self {
        let total_block_count = total_size / SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64;

        Self {
            object,
            total_size,
            slab_cache_by_object_size: power_of_2_series_from(SLAB_ALLOC_BASE_SIZE)
                .take_while(|&x| x <= total_size as usize)
                .map(MemoryBlockSlabCache::new)
                .collect(),
            slab_cache_free_area_manager: MemoryBlockSlabCacheFreeAreaManager::new(
                total_block_count as _,
            ),
        }
    }

    /// SLAB_ALLOC_BASE_SIZE以上 2の累乗数に切り上げ
    fn aligned_object_size(size: usize) -> usize {
        round_up_to_next_power_of_two(size.max(SLAB_ALLOC_BASE_SIZE) as _) as _
    }

    /// slab_cache_by_object_sizeのキーを計算
    const fn slab_level(aligned_size: usize) -> usize {
        (aligned_size.trailing_zeros() - SLAB_ALLOC_BASE_SIZE.trailing_zeros()) as usize
    }

    #[tracing::instrument(skip(self))]
    pub fn suballocate(&mut self, size: usize) -> Option<u64> {
        let slab_object_size = Self::aligned_object_size(size);
        let slab_level = Self::slab_level(slab_object_size);
        let allocator = &mut self.slab_cache_by_object_size[slab_level as usize];

        tracing::debug!("size aligned to {slab_object_size}, using level #{slab_level}");

        allocator.find_free_object_offset().or_else(|| {
            // 既存のslab cache内ではみつからなかった 新しく登録して試す
            let (new_slab_cache_offset_block, max_objects, block_count) =
                match self.slab_cache_free_area_manager.acquire(1 << slab_level) {
                    Ok(b) => (b, 32, 1 << slab_level),
                    Err(MemoryBlockSlabCacheFreeAreaAcquisitionFailure::TooLarge) => {
                        let (max_available_block_start, max_available_block_count) = self.slab_cache_free_area_manager.max_unallocated_memory_block_length();
                        tracing::debug!("unavailable blocks: {max_available_block_start} {max_available_block_count}");
                        let max_available_block_size = max_available_block_count * SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64;
                        if max_available_block_size < slab_object_size as u64 {
                            // no enough blocks left
                            return None;
                        }

                        let new_slab_object_count = ((max_available_block_size / slab_object_size as u64) >> 1).max(1);
                        let new_slab_object_block_count = ((new_slab_object_count * slab_object_size as u64) / SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64) as u32;
                        let new_block_start = self.slab_cache_free_area_manager.acquire(new_slab_object_block_count).ok()?;

                        tracing::trace!("object too large! managing only {new_slab_object_count} objects");
                        (new_block_start, new_slab_object_count, new_slab_object_block_count)
                    }
                    Err(_) => return None,
                };
            allocator.append_empty_slab(
                (new_slab_cache_offset_block, block_count),
                new_slab_cache_offset_block * SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64,
                max_objects as _,
            );

            allocator.find_free_object_offset()
        })
    }

    #[tracing::instrument(skip(self))]
    pub fn free(&mut self, size: usize, offset: u64) {
        let slab_object_size = Self::aligned_object_size(size);
        let slab_level = Self::slab_level(slab_object_size);

        tracing::debug!("size aligned:{slab_object_size}, using level #{slab_level}");

        self.slab_cache_by_object_size[slab_level as usize].free_object(offset, |empty| {
            self.slab_cache_free_area_manager
                .release_power_of_two_block(empty.block_info.0, empty.block_info.1);
        });
    }
}

enum BackingMemory {
    Managed(SharedMutableRef<MemoryBlock<peridot::DeviceObject>>),
    Native(br::DeviceMemoryObject<peridot::DeviceObject>),
}

pub struct Image {
    object: br::ImageObject<peridot::DeviceObject>,
    memory_block: BackingMemory,
    offset: u64,
    byte_length: usize,
}
impl Drop for Image {
    fn drop(&mut self) {
        if let BackingMemory::Managed(ref b) = self.memory_block {
            b.borrow_mut().free(self.byte_length, self.offset);
        }
    }
}
impl br::VkHandle for Image {
    type Handle = <br::ImageObject<peridot::DeviceObject> as br::VkHandle>::Handle;

    fn native_ptr(&self) -> Self::Handle {
        self.object.native_ptr()
    }
}
impl br::VkHandleMut for Image {
    fn native_ptr_mut(&mut self) -> Self::Handle {
        self.object.native_ptr_mut()
    }
}
impl br::VkObject for Image {
    const TYPE: br::vk::VkObjectType =
        <br::ImageObject<peridot::DeviceObject> as br::VkObject>::TYPE;
}
impl br::DeviceChild for Image {
    type ConcreteDevice =
        <br::ImageObject<peridot::DeviceObject> as br::DeviceChild>::ConcreteDevice;

    fn device(&self) -> &Self::ConcreteDevice {
        self.object.device()
    }
}
impl br::Image for Image {
    fn format(&self) -> br::vk::VkFormat {
        self.object.format()
    }

    fn size(&self) -> &br::vk::VkExtent3D {
        self.object.size()
    }

    fn dimension(&self) -> br::vk::VkImageViewType {
        self.object.dimension()
    }
}

pub struct Buffer {
    object: br::BufferObject<peridot::DeviceObject>,
    memory_block: BackingMemory,
    offset: u64,
    size: usize,
}
impl Buffer {
    pub const fn byte_length(&self) -> usize {
        self.size
    }

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

    /// Writes value as buffer content. checked whether value size and buffer size are equal.
    pub fn write_content<T>(&mut self, value: T) -> br::Result<()> {
        assert_eq!(self.size, core::mem::size_of::<T>());

        unsafe { self.write_content_unchecked(value) }
    }

    pub unsafe fn write_content_unchecked<T>(&mut self, value: T) -> br::Result<()> {
        self.guard_map(|ptr| {
            (*(ptr as *mut T)) = value;
        })
    }

    pub fn clone_content_from_slice<T: Clone>(&mut self, values: &[T]) -> br::Result<()> {
        assert_eq!(self.size, core::mem::size_of::<T>() * values.len());

        self.guard_map(|ptr| unsafe {
            core::slice::from_raw_parts_mut(ptr as *mut T, values.len()).clone_from_slice(values);
        })
    }

    pub fn copy_content_from_slice<T: Copy>(&mut self, values: &[T]) -> br::Result<()> {
        assert_eq!(self.size, core::mem::size_of::<T>() * values.len());

        self.guard_map(|ptr| unsafe {
            core::slice::from_raw_parts_mut(ptr as *mut T, values.len()).copy_from_slice(values);
        })
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

pub struct LinearImageBuffer {
    pub inner: Buffer,
    pub row_texels: u32,
    pub height: u32,
}
impl std::ops::Deref for LinearImageBuffer {
    type Target = Buffer;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
impl std::ops::DerefMut for LinearImageBuffer {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub struct OptimalBufferLinearImagePlacementInfo {
    pub alignment: u64,
    pub row_pitch_alignment: u64,
}

pub struct MemoryManager {
    /// Device Local only
    pub device_local_memory_types: Vec<MemoryType>,
    /// Host Visible only
    pub host_visible_memory_types: Vec<MemoryType>,
    /// Both Device Local and Host Visible (this memory can be directly mapped)
    pub direct_memory_types: Vec<MemoryType>,
    pub heap_stats: Vec<HeapStats>,
    pub optimal_buffer_linear_image_placement_info: OptimalBufferLinearImagePlacementInfo,
    pub managed_blocks_per_type:
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

                unsafe { r.for_dedicated_buffer_allocation(&o) }
            } else {
                r
            }
        })?;
        match memory {
            BackingMemory::Managed(ref m) => o.bind(&m.borrow().object, offset as _),
            BackingMemory::Native(ref m) => o.bind(m, offset as _),
        }?;

        Ok(Buffer {
            object: o,
            memory_block: memory,
            offset,
            size: req.memoryRequirements.size as _,
        })
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
        // TODO: coherentじゃないmemory typeが選択されたときどうしよう（Flush/Invalidateが必要なのでなんとか情報を利用側で取れる必要がある）
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
        if !memory_type.is_coherent {
            tracing::warn!(
                "TODO: selected memory type is not host-coherent, requires explicit flushing"
            );
        }

        let (memory, offset) =
            self.allocate_internal(e.device(), &req, memory_type.index, |r| {
                if e.dedicated_allocation_available() {
                    tracing::info!("using dedicated allocation");

                    unsafe { r.for_dedicated_buffer_allocation(&o) }
                } else {
                    r
                }
            })?;
        match memory {
            BackingMemory::Managed(ref m) => o.bind(&m.borrow().object, offset as _),
            BackingMemory::Native(ref m) => o.bind(m, offset as _),
        }?;

        Ok(Buffer {
            object: o,
            memory_block: memory,
            offset,
            size: req.memoryRequirements.size as _,
        })
    }

    pub fn allocate_upload_buffer_with_contents(
        &mut self,
        e: &peridot::Graphics,
        contents: impl IntoIterator<Item = peridot::BufferContent>,
        add_usage: br::BufferUsage,
    ) -> br::Result<(Buffer, Vec<u64>)> {
        let mut bp = peridot::BufferPrealloc::new(e);
        let offsets = contents.into_iter().map(|c| bp.add(c)).collect::<Vec<_>>();
        let obj = self.allocate_upload_buffer(e, bp.build_desc().and_usage(add_usage))?;

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
        match memory {
            BackingMemory::Managed(ref m) => o.bind(&m.borrow().object, offset as _),
            BackingMemory::Native(ref m) => o.bind(m, offset as _),
        }?;

        Ok(Image {
            object: o,
            memory_block: memory,
            offset,
            byte_length: req.memoryRequirements.size as _,
        })
    }
}
