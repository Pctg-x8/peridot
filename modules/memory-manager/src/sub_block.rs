use crate::{
    slab::SLAB_ALLOC_BASE_SIZE,
    utils::{power_of_2_series_from_u64, round_up_to_next_power_of_two},
};

pub const SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE: usize = SLAB_ALLOC_BASE_SIZE * 32;

pub enum MemoryBlockSlabCacheFreeAreaAcquisitionFailure {
    TooLarge,
    NoBlocksLeft,
}

/// Slab Allocatorが管理する用の一定サイズのメモリブロックを切り出す（Buddy Systemの簡易実装）
pub struct MemoryBlockSlabCacheFreeAreaManager {
    free_block_index_by_chains: Vec<Vec<u32>>,
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

    #[inline]
    const fn level_to_block_count(level: usize) -> u32 {
        1u32 << level
    }

    #[inline]
    const fn block_count_to_level(aligned_block_count: u32) -> usize {
        aligned_block_count.trailing_zeros() as _
    }

    #[inline]
    pub const fn block_number_to_bytes(block_number: u32) -> u64 {
        block_number as u64 * SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64
    }

    #[inline]
    pub const fn block_count_to_bytes(block_count: u32) -> u64 {
        block_count as u64 * SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64
    }

    #[inline]
    pub const fn byte_length_to_block_count(byte_length: u64) -> u32 {
        (byte_length / SLAB_CACHE_FREE_MANAGER_BLOCK_SIZE as u64) as _
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

            Some(Self::level_to_block_count(
                self.free_block_index_by_chains.len() - 1,
            ))
        } else {
            None
        }
    }

    /// returns: (start block number, block count)
    pub fn max_unallocated_memory_block_length(&self) -> (u32, u32) {
        let largest = self
            .free_block_index_by_chains
            .iter()
            .enumerate()
            .rev()
            .find(|(_, v)| !v.is_empty())
            .map(|(level, v)| {
                (
                    unsafe { v.first().copied().unwrap_unchecked() },
                    Self::level_to_block_count(level),
                    level,
                )
            });
        let Some((mut starting_number, mut block_count, largest_level)) = largest else {
            // 空きがない
            return (0, 0);
        };

        // find descendant levels for chainable block
        for (level, v) in self.free_block_index_by_chains[..largest_level]
            .iter()
            .enumerate()
            .rev()
        {
            let level_block_count = Self::level_to_block_count(level);

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

    #[tracing::instrument(skip(self))]
    /// returns block number(not global byte offset)
    pub fn acquire(
        &mut self,
        block_count: u32,
    ) -> Result<u32, MemoryBlockSlabCacheFreeAreaAcquisitionFailure> {
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
            .find_map(|(n, x)| {
                if n > base_level as usize {
                    x.pop().map(move |f| (n, f))
                } else {
                    None
                }
            });
        if let Some((found_level, b)) = large_block {
            let mut left_block_count = Self::level_to_block_count(found_level) - block_count;
            for l in (1..=found_level - 1).rev() {
                self.free_block_index_by_chains[l].push(b + Self::level_to_block_count(l));
                left_block_count -= Self::level_to_block_count(l);
                if left_block_count == 0 {
                    break;
                }
            }

            return Ok(b);
        }

        Err(MemoryBlockSlabCacheFreeAreaAcquisitionFailure::NoBlocksLeft)
    }

    #[tracing::instrument(skip(self))]
    pub fn release_power_of_two_block(&mut self, block_number: u32, mut block_count: u32) {
        tracing::trace!("release");

        let base_level = Self::block_count_to_level(block_count);
        let mut level = self.free_block_index_by_chains.len() - 1;
        for l in base_level..self.free_block_index_by_chains.len() {
            match self.free_block_index_by_chains[l]
                .iter()
                .enumerate()
                .find(|&(_, &b)| block_number + block_count == b)
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
    pub fn release(&mut self, mut block_number: u32, mut block_count: u32) {
        // power of 2に切り上げ(こいつが最大レベル)
        let aligned_block_count = 1u32 << ((32 - block_count.leading_zeros()) - 1);
        let max_level = Self::block_count_to_level(aligned_block_count);

        // 下からpower of 2に切り出しながら返却
        for l in (0..max_level).rev() {
            let bc = Self::level_to_block_count(l);

            if block_count >= bc {
                // subdivide
                self.release_power_of_two_block(block_number, bc);
                block_number += bc;
                block_count -= bc;
            }

            if block_count <= 0 {
                break;
            }
        }
    }
}
