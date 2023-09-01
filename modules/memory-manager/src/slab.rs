// 64 consists of sizeof(mat4) = 4 * 4 * sizeof(f32)
pub const SLAB_ALLOC_BASE_SIZE: usize = 64;

pub struct MemoryBlockSlab {
    /// (number, count)
    pub block_info: (u32, u32),
    offset: u64,
    free_bits: u32,
    max: u32,
    next: Option<Box<MemoryBlockSlab>>,
}
impl MemoryBlockSlab {
    const fn new(block_info: (u32, u32), offset: u64, max: u32) -> Self {
        Self {
            block_info,
            offset,
            free_bits: 0,
            max,
            next: None,
        }
    }

    const fn is_filled(&self) -> bool {
        self.free_bits.count_ones() == self.max
    }

    const fn is_empty(&self) -> bool {
        self.free_bits.count_ones() == 0
    }

    const fn get_first_free(&self) -> Option<usize> {
        let x = self.free_bits.trailing_ones();

        if x < self.max {
            Some(x as _)
        } else {
            None
        }
    }

    fn contains_object(&self, object_size: usize, offset: u64) -> bool {
        (self.offset..self.offset + (object_size * self.max as usize) as u64).contains(&offset)
    }

    fn try_compute_object_index(&self, object_size: usize, offset: u64) -> Option<usize> {
        if !self.contains_object(object_size, offset) {
            return None;
        }

        Some((offset - self.offset) as usize / object_size)
    }

    fn mark_used(&mut self, index: usize) {
        self.free_bits |= 1 << index;
    }

    fn mark_unused(&mut self, index: usize) {
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

    pub fn append_empty_slab(&mut self, block_info: (u32, u32), offset: u64, max: u32) {
        Self::append_slab_chain(
            &mut self.empty_slabs_head,
            Box::new(MemoryBlockSlab::new(block_info, offset, max)),
        );
    }

    fn append_slab_chain(c: &mut Option<Box<MemoryBlockSlab>>, new_slab: Box<MemoryBlockSlab>) {
        match c.as_mut() {
            None => {
                *c = Some(new_slab);
            }
            Some(h) => {
                Self::append_slab_chain(&mut h.next, new_slab);
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
                    let here = prev.map_or(&mut self.partially_allocated_slabs_head, |p| unsafe {
                        &mut (&mut *p).next
                    });
                    let filled = core::mem::replace(here, slab.next.take());
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
                let here = prev.map_or(&mut self.empty_slabs_head, |p| unsafe {
                    &mut (&mut *p).next
                });
                let partial_filled = core::mem::replace(here, slab.next.take());
                // whileにこれたならかならずあるはず
                Self::append_slab_chain(&mut self.partially_allocated_slabs_head, unsafe {
                    partial_filled.unwrap_unchecked()
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
                let here = prev.map_or(&mut self.filled_slabs_head, |p| unsafe {
                    &mut (&mut *p).next
                });
                let partial_filled = core::mem::replace(here, slab.next.take());
                // whileにこれたならかならずあるはず
                Self::append_slab_chain(&mut self.partially_allocated_slabs_head, unsafe {
                    partial_filled.unwrap_unchecked()
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
                    let here = prev.map_or(&mut self.partially_allocated_slabs_head, |p| unsafe {
                        &mut (&mut *p).next
                    });
                    let empty = core::mem::replace(here, slab.next.take());

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
