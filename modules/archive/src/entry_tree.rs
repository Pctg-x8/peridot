//! asset entry hash tree properties

use std::convert::TryFrom;

pub const TARGET_PAGE_BLOCK_SIZE: usize = 2048;
pub const EXACT_TREE_ENTRY_STRIDE: usize = 8 * 2;
pub const NORMAL_TREE_ENTRY_STRIDE: usize = 8 * 3;

pub const fn first_hash_tree_block_size(header_size: usize) -> usize {
    // header_size: header size of uncompressed region(= file header)
    // 1: content flags
    // 4: size of hash tree block
    // 8: size of exact name block
    TARGET_PAGE_BLOCK_SIZE - header_size - 1 - 4 - 8
}

pub const NON_ROOT_EXACT_TREE_MAX_ELEMENT_COUNT: usize =
    (TARGET_PAGE_BLOCK_SIZE - 2) / EXACT_TREE_ENTRY_STRIDE;

pub const fn normal_tree_entry_count(block_size: usize) -> usize {
    // - 8: larger tree pointer(written tail of the block)
    // 8 * 3: size of an entry(name_hash + exact_match_pointer + smaller_tree_pointer)
    (block_size - 8) / NORMAL_TREE_ENTRY_STRIDE
}
pub const fn normal_tree_block_size(element_count: usize) -> usize {
    element_count * NORMAL_TREE_ENTRY_STRIDE + 8
}
pub const fn trim_normal_tree_block_size(size: usize) -> usize {
    normal_tree_block_size(normal_tree_entry_count(size))
}
pub const MAX_ENTRY_COUNT: usize = normal_tree_entry_count(TARGET_PAGE_BLOCK_SIZE);

pub const fn exact_root_tree_entry_count(block_size: usize) -> usize {
    assert!(
        block_size & (EXACT_TREE_ENTRY_STRIDE - 1) == 0,
        "exact root hash tree has extra byte?"
    );

    block_size / EXACT_TREE_ENTRY_STRIDE
}
pub const fn exact_root_tree_block_size(entry_count: usize) -> usize {
    entry_count * EXACT_TREE_ENTRY_STRIDE
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EntryTreePointer(u64);
impl EntryTreePointer {
    const EXACT_TREE_BIT: u64 = 0x8000_0000_0000_0000;

    pub const fn from_u64(x: u64) -> Self {
        Self(x)
    }

    pub const fn pointer_value(self) -> u64 {
        self.0 & !Self::EXACT_TREE_BIT
    }

    pub const fn to_le_bytes(self) -> [u8; 8] {
        self.0.to_le_bytes()
    }

    pub const fn from_le_bytes(b: [u8; 8]) -> Self {
        Self(u64::from_le_bytes(b))
    }

    pub const fn exact_tree(self) -> Self {
        Self(self.0 | Self::EXACT_TREE_BIT)
    }

    pub const fn is_exact_tree(&self) -> bool {
        self.0 & Self::EXACT_TREE_BIT != 0
    }
}

#[repr(transparent)]
pub struct ExactEntryView<'b>(pub &'b [u8]);
impl<'b> ExactEntryView<'b> {
    #[inline(always)]
    pub fn from_offset_and_index(buf: &'b [u8], offset: usize, index: usize) -> Self {
        Self(&buf[offset..offset + index * EXACT_TREE_ENTRY_STRIDE])
    }

    #[inline(always)]
    pub fn name_hash(&self) -> u64 {
        u64::from_le_bytes(unsafe { TryFrom::try_from(&self.0[0..8]).unwrap_unchecked() })
    }

    #[inline(always)]
    pub fn exact_block_offset(&self) -> u64 {
        u64::from_le_bytes(unsafe { TryFrom::try_from(&self.0[8..16]).unwrap_unchecked() })
    }
}

#[repr(transparent)]
pub struct HashTreeEntryView<'b>(pub &'b [u8]);
impl HashTreeEntryView<'_> {
    #[inline(always)]
    pub fn name_hash(&self) -> u64 {
        u64::from_le_bytes(unsafe { TryFrom::try_from(&self.0[0..8]).unwrap_unchecked() })
    }

    #[inline(always)]
    pub fn exact_block_offset(&self) -> u64 {
        u64::from_le_bytes(unsafe { TryFrom::try_from(&self.0[8..16]).unwrap_unchecked() })
    }

    #[inline(always)]
    pub fn smaller_tree_pointer(&self) -> EntryTreePointer {
        EntryTreePointer::from_le_bytes(unsafe {
            TryFrom::try_from(&self.0[16..24]).unwrap_unchecked()
        })
    }
}

#[repr(transparent)]
pub struct EntryMutableView<'a>(pub &'a mut [u8]);
impl<'a> EntryMutableView<'a> {
    #[inline(always)]
    pub fn at(buffer: &'a mut [u8], base_pointer: usize, at: usize) -> Self {
        Self(&mut buffer[base_pointer + at * NORMAL_TREE_ENTRY_STRIDE..])
    }

    #[inline(always)]
    pub fn set_name_hash(&mut self, name_hash: u64) {
        self.0[0..8].copy_from_slice(&name_hash.to_le_bytes());
    }

    #[inline(always)]
    pub fn set_exact_block_offset(&mut self, offset: u64) {
        self.0[8..16].copy_from_slice(&offset.to_le_bytes());
    }

    #[inline(always)]
    pub fn set_smaller_tree_pointer(&mut self, ptr: EntryTreePointer) {
        self.0[16..24].copy_from_slice(&ptr.to_le_bytes());
    }
}

#[repr(transparent)]
pub struct BlockView<'b>(pub &'b [u8]);
impl<'b> BlockView<'b> {
    #[inline(always)]
    pub fn from_offset_and_element_count(
        buf: &'b [u8],
        offset: usize,
        element_count: usize,
    ) -> Self {
        Self(&buf[offset..offset + normal_tree_block_size(element_count)])
    }

    pub const fn entry_count(&self) -> usize {
        normal_tree_entry_count(self.0.len())
    }

    #[inline(always)]
    pub fn smallest_entry(&self) -> HashTreeEntryView<'b> {
        HashTreeEntryView(&self.0[0..])
    }

    #[inline(always)]
    pub fn entry(&self, at: usize) -> HashTreeEntryView<'b> {
        HashTreeEntryView(&self.0[at * NORMAL_TREE_ENTRY_STRIDE..])
    }

    #[inline(always)]
    pub fn largest_entry(&self) -> HashTreeEntryView<'b> {
        HashTreeEntryView(&self.0[self.0.len() - 32..])
    }

    #[inline(always)]
    pub fn larger_tree_pointer(&self) -> EntryTreePointer {
        EntryTreePointer::from_le_bytes(unsafe {
            TryFrom::try_from(&self.0[self.0.len() - 8..]).unwrap_unchecked()
        })
    }
}

#[repr(transparent)]
pub struct BlockMutableView<'a>(pub &'a mut [u8]);
impl<'a> BlockMutableView<'a> {
    #[inline(always)]
    pub fn from_offset_and_element_count(
        buf: &'a mut [u8],
        base_pointer: usize,
        count: usize,
    ) -> Self {
        Self(&mut buf[base_pointer..base_pointer + normal_tree_block_size(count)])
    }

    #[inline(always)]
    pub fn set_larger_tree_pointer(&mut self, ptr: EntryTreePointer) {
        let p = self.0.len() - 8;

        self.0[p..].copy_from_slice(&ptr.to_le_bytes());
    }
}

#[repr(transparent)]
pub struct ExactRootBlockView<'b>(pub &'b [u8]);
impl<'b> ExactBlockViewOps<'b> for ExactRootBlockView<'b> {
    #[inline(always)]
    fn entry_count(&self) -> usize {
        exact_root_tree_entry_count(self.0.len())
    }

    #[inline(always)]
    fn entry(&self, at: usize) -> ExactEntryView<'b> {
        ExactEntryView(
            &self.0[at * EXACT_TREE_ENTRY_STRIDE
                ..at * EXACT_TREE_ENTRY_STRIDE + EXACT_TREE_ENTRY_STRIDE],
        )
    }
}

#[repr(transparent)]
pub struct ExactBlockView<'b>(pub &'b [u8]);
impl<'b> ExactBlockViewOps<'b> for ExactBlockView<'b> {
    #[inline(always)]
    fn entry_count(&self) -> usize {
        u16::from_le_bytes(unsafe { TryFrom::try_from(&self.0[0..2]).unwrap_unchecked() }) as _
    }

    #[inline(always)]
    fn entry(&self, at: usize) -> ExactEntryView<'b> {
        ExactEntryView::from_offset_and_index(self.0, 2, at)
    }
}

#[repr(transparent)]
pub struct ExactBlockMutableView<'a>(pub &'a mut [u8]);
impl<'a> ExactBlockMutableView<'a> {
    #[inline(always)]
    pub fn at(buffer: &'a mut [u8], at: usize) -> Self {
        Self(&mut buffer[at * EXACT_TREE_ENTRY_STRIDE..])
    }

    #[inline(always)]
    pub fn set_name_hash(&mut self, name_hash: u64) {
        self.0[0..8].copy_from_slice(&name_hash.to_le_bytes());
    }

    #[inline(always)]
    pub fn set_exact_block_offset(&mut self, offset: u64) {
        self.0[8..16].copy_from_slice(&offset.to_le_bytes());
    }
}

pub trait ExactBlockViewOps<'b> {
    fn entry_count(&self) -> usize;
    fn entry(&self, at: usize) -> ExactEntryView<'b>;
}
