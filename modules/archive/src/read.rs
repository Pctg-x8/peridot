use std::{
    convert::TryFrom,
    io::{Error as IOError, IoSliceMut, Read, Result as IOResult},
    path::Path,
};

use crate::{
    AssetEntryHeadingPair, CompressionMethod, ContentFlags,
    entry::AssetNameRef,
    entry_tree::EntryTreePointer,
    native_io::{AsyncNativeFileReader, NativeFileMemoryMapProvider, NativeFileReader},
};
use crc::crc32;
use libflate::deflate as zlib;
use peridot_serialization_utils::{VariableUInt, VariableULong};

#[cfg(windows)]
type PlatformNativeFileReader = crate::native_io::windows::WindowsNativeFileReader;
#[cfg(windows)]
type PlatformNativeFileReaderAsync = crate::native_io::windows::WindowsAsyncNativeFileReader;
#[cfg(target_os = "linux")]
type PlatformNativeFileReader = crate::native_io::linux::LinuxNativeFileReader;
#[cfg(target_os = "linux")]
type PlatformNativeFileReaderAsync = crate::native_io::linux::LinuxAsyncNativeFileReader;

#[non_exhaustive]
#[derive(Debug)]
pub enum ArchiveReadError {
    IO(IOError),
    IntegrityCheckFailed,
    SignatureMismatch,
    Lz4DecompressError(lz4_compression::decompress::Error),
}
impl From<IOError> for ArchiveReadError {
    fn from(e: IOError) -> Self {
        Self::IO(e)
    }
}
impl From<lz4_compression::decompress::Error> for ArchiveReadError {
    fn from(e: lz4_compression::decompress::Error) -> Self {
        Self::Lz4DecompressError(e)
    }
}
pub type ArchiveReadResult<T> = Result<T, ArchiveReadError>;
impl From<ArchiveReadError> for IOError {
    fn from(e: ArchiveReadError) -> Self {
        match e {
            ArchiveReadError::IO(e) => e,
            ArchiveReadError::IntegrityCheckFailed => {
                IOError::other("Archive Integrity check failed")
            }
            ArchiveReadError::SignatureMismatch => IOError::other("Archive Signature Mismatch"),
            ArchiveReadError::Lz4DecompressError(e) => {
                IOError::other(format!("Lz4DecompressError: {e:?}"))
            }
        }
    }
}

fn list_entry(
    head_size: usize,
    hash_tree_root_exact: bool,
    hash_tree_block: &[u8],
    exact_match_block: &[u8],
    mut callback: impl FnMut(AssetNameRef),
) {
    fn enumerate_exact_block_content(
        block: &[u8],
        pointer: usize,
        callback: &mut impl FnMut(AssetNameRef),
    ) {
        let (VariableUInt(entry_count), entry_count_len) =
            VariableUInt::from_bytes_head(&block[pointer..]);
        let mut read_ptr = pointer + entry_count_len;
        for _ in 0..entry_count {
            let (VariableULong(n), name_bytes) = VariableULong::from_bytes_head(&block[read_ptr..]);
            read_ptr += name_bytes;
            let name_ext = &block[read_ptr..read_ptr + n as usize];
            read_ptr += n as usize;
            let (_, hlen) = AssetEntryHeadingPair::from_bytes_head(&block[read_ptr..]);
            read_ptr += hlen;

            let name_split = name_ext
                .iter()
                .position(|&x| x == 0)
                .unwrap_or(name_ext.len());
            let n = AssetNameRef {
                name: unsafe { core::str::from_utf8_unchecked(&name_ext[..name_split]) },
                ext: if name_split >= name_ext.len() - 1 {
                    // no ext
                    ""
                } else {
                    unsafe { core::str::from_utf8_unchecked(&name_ext[name_split + 1..]) }
                },
            };

            callback(n);
        }
    }

    fn enumerate_exact_tree<'b>(
        block_view: &(impl crate::entry_tree::ExactBlockViewOps<'b> + ?Sized),
        exact_match_block: &[u8],
        callback: &mut impl FnMut(AssetNameRef),
    ) {
        for ptr in 0..block_view.entry_count() {
            enumerate_exact_block_content(
                exact_match_block,
                block_view.entry(ptr).exact_block_offset() as _,
                callback,
            );
        }
    }

    fn enumerate_tree(
        block_view: &crate::entry_tree::BlockView,
        hash_tree_block: &[u8],
        exact_match_block: &[u8],
        callback: &mut impl FnMut(AssetNameRef),
    ) {
        for ptr in 0..block_view.entry_count() {
            let e = block_view.entry(ptr);
            enumerate_subtree(
                hash_tree_block,
                exact_match_block,
                callback,
                e.smaller_tree_pointer(),
            );
            enumerate_exact_block_content(exact_match_block, e.exact_block_offset() as _, callback);
        }

        enumerate_subtree(
            hash_tree_block,
            exact_match_block,
            callback,
            block_view.larger_tree_pointer(),
        );
    }

    fn enumerate_subtree(
        hash_tree_block: &[u8],
        exact_match_block: &[u8],
        callback: &mut impl FnMut(AssetNameRef),
        tree_pointer: EntryTreePointer,
    ) {
        if tree_pointer.is_exact_tree() {
            // Exact Tree
            enumerate_exact_tree(
                &crate::entry_tree::ExactBlockView(
                    &hash_tree_block[tree_pointer.pointer_value() as usize..],
                ),
                exact_match_block,
                callback,
            );

            return;
        }

        // normal tree
        enumerate_tree(
            &crate::entry_tree::BlockView::from_offset_and_element_count(
                hash_tree_block,
                tree_pointer.pointer_value() as _,
                crate::entry_tree::MAX_ENTRY_COUNT,
            ),
            hash_tree_block,
            exact_match_block,
            callback,
        );
    }

    if hash_tree_root_exact {
        // Exact tree only
        enumerate_exact_tree(
            &crate::entry_tree::ExactRootBlockView(hash_tree_block),
            exact_match_block,
            &mut callback,
        );

        return;
    }

    enumerate_tree(
        &crate::entry_tree::BlockView(
            &hash_tree_block[..crate::entry_tree::trim_normal_tree_block_size(
                crate::entry_tree::first_hash_tree_block_size(head_size),
            )],
        ),
        hash_tree_block,
        exact_match_block,
        &mut callback,
    );
}

fn find_entry(
    head_size: usize,
    name: &str,
    ext: &str,
    hash_tree_root_exact: bool,
    hash_tree_block: &[u8],
    exact_match_block: &[u8],
) -> Option<AssetEntryHeadingPair> {
    let name = AssetNameRef { name, ext };
    let name_hash = name.hash();

    fn find_exact<'b>(
        block_view: &(impl crate::entry_tree::ExactBlockViewOps<'b> + ?Sized),
        target: u64,
    ) -> Option<u64> {
        let (mut top, mut bottom) = (0, block_view.entry_count());
        loop {
            let ptr = (top + bottom) / 2;
            let e = block_view.entry(ptr);

            match target.cmp(&e.name_hash()) {
                // match
                core::cmp::Ordering::Equal => return Some(e.exact_block_offset()),
                core::cmp::Ordering::Less => {
                    // bottom: exclusive
                    bottom = ptr;
                }
                core::cmp::Ordering::Greater => {
                    top = ptr + 1;
                }
            }

            if bottom <= top {
                return None;
            }
        }
    }

    fn find(
        block_view: &crate::entry_tree::BlockView,
        target: u64,
    ) -> Result<u64, EntryTreePointer> {
        // edge check
        let e = block_view.largest_entry();
        match target.cmp(&e.name_hash()) {
            // exact largest
            core::cmp::Ordering::Equal => return Ok(e.exact_block_offset()),
            // more greater
            core::cmp::Ordering::Greater => return Err(block_view.larger_tree_pointer()),
            core::cmp::Ordering::Less => (/* nop */),
        }

        let e = block_view.smallest_entry();
        match target.cmp(&e.name_hash()) {
            // exact smallest
            core::cmp::Ordering::Equal => return Ok(e.exact_block_offset()),
            // more smaller
            core::cmp::Ordering::Less => return Err(e.smaller_tree_pointer()),
            core::cmp::Ordering::Greater => (/* nop */),
        }

        // binary search
        let (mut top, mut bottom) = (0, block_view.entry_count());
        loop {
            let ptr = (top + bottom) / 2;
            let e = block_view.entry(ptr);

            match target.cmp(&e.name_hash()) {
                // match
                core::cmp::Ordering::Equal => return Ok(e.exact_block_offset()),
                core::cmp::Ordering::Less => {
                    // bottom: exclusive
                    bottom = ptr;
                }
                core::cmp::Ordering::Greater => {
                    top = ptr + 1;
                }
            }

            if bottom <= top {
                // search under here
                return Err(e.smaller_tree_pointer());
            }
        }
    }

    let exact_block_offset = 'hash_tree_finder: {
        if hash_tree_root_exact {
            // Exact Root Tree
            break 'hash_tree_finder find_exact(
                &crate::entry_tree::ExactRootBlockView(hash_tree_block),
                name_hash,
            );
        }

        fn find_subtree(
            name_hash: u64,
            hash_tree_block: &[u8],
            tree_pointer: EntryTreePointer,
        ) -> Option<u64> {
            if tree_pointer.is_exact_tree() {
                // Exact Tree
                return find_exact(
                    &crate::entry_tree::ExactBlockView(
                        &hash_tree_block[tree_pointer.pointer_value() as usize..],
                    ),
                    name_hash,
                );
            }

            // normal tree
            let block_view = crate::entry_tree::BlockView::from_offset_and_element_count(
                hash_tree_block,
                tree_pointer.pointer_value() as _,
                crate::entry_tree::MAX_ENTRY_COUNT,
            );

            match find(&block_view, name_hash) {
                Ok(x) => Some(x),
                Err(p) => find_subtree(name_hash, hash_tree_block, p),
            }
        }

        let block_view = crate::entry_tree::BlockView(
            &hash_tree_block[..crate::entry_tree::trim_normal_tree_block_size(
                crate::entry_tree::first_hash_tree_block_size(head_size),
            )],
        );

        match find(&block_view, name_hash) {
            Ok(x) => Some(x),
            Err(p) => find_subtree(name_hash, hash_tree_block, p),
        }
    }?;

    // linear search conflicting hash bucket
    let (VariableUInt(exact_entry_count), exact_entry_offset) =
        VariableUInt::from_bytes_head(&exact_match_block[exact_block_offset as usize..]);
    let mut read_ptr = exact_block_offset as usize + exact_entry_offset;
    for _ in 0..exact_entry_count {
        let (VariableULong(n), name_bytes) =
            VariableULong::from_bytes_head(&exact_match_block[read_ptr..]);
        read_ptr += name_bytes;
        let name_ext = &exact_match_block[read_ptr..read_ptr + n as usize];
        read_ptr += n as usize;
        let (h, hlen) = AssetEntryHeadingPair::from_bytes_head(&exact_match_block[read_ptr..]);
        read_ptr += hlen;

        let name_split = name_ext
            .iter()
            .position(|&x| x == 0)
            .unwrap_or(name_ext.len());
        let n = AssetNameRef {
            name: unsafe { core::str::from_utf8_unchecked(&name_ext[..name_split]) },
            ext: if name_split >= name_ext.len() - 1 {
                // no ext
                ""
            } else {
                unsafe { core::str::from_utf8_unchecked(&name_ext[name_split + 1..]) }
            },
        };

        if n == name {
            // match!
            return Some(h);
        }
    }

    // no match in exact name list
    None
}

pub struct OnMemoryArchiveBinReader<'a> {
    pub archive: &'a OnMemoryArchive,
    pub pointer: u64,
    pub pointer_limit: u64,
}
impl std::io::Read for OnMemoryArchiveBinReader<'_> {
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        let read_len = buf.len().min((self.pointer_limit - self.pointer) as usize);
        buf[..read_len].copy_from_slice(
            &self.archive.block[self.pointer as usize..self.pointer as usize + read_len],
        );
        self.pointer += read_len as u64;

        Ok(read_len)
    }
}
#[cfg(feature = "async-rt-async-std")]
impl async_std::io::Read for OnMemoryArchiveBinReader<'_> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        _cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<IOResult<usize>> {
        let read_len = buf.len().min((self.pointer_limit - self.pointer) as usize);
        buf[..read_len].copy_from_slice(
            &self.archive.block[self.pointer as usize..self.pointer as usize + read_len],
        );
        self.get_mut().pointer += read_len as u64;

        std::task::Poll::Ready(Ok(read_len))
    }
}

pub struct OnMemoryArchive {
    pub head_size: usize,
    pub block: Vec<u8>,
    pub content_flags: ContentFlags,
    pub hash_tree_block_range: core::ops::Range<usize>,
    pub exact_match_block_range: core::ops::Range<usize>,
    pub content_baseptr: usize,
}
impl OnMemoryArchive {
    fn new(compression: CompressionMethod, body: Vec<u8>) -> ArchiveReadResult<Self> {
        let (body, head_size) = match compression {
            CompressionMethod::Lz4(_) => (lz4_compression::prelude::decompress(&body)?, 4 + 8 + 4),
            CompressionMethod::Zlib(ub) => {
                let mut sink = Vec::with_capacity(ub as _);
                zlib::Decoder::new(std::io::Cursor::new(body)).read_to_end(&mut sink)?;
                (sink, 4 + 8 + 4)
            }
            CompressionMethod::Zstd11(ub) => {
                let mut sink = Vec::with_capacity(ub as _);
                zstd::Decoder::new(std::io::Cursor::new(body))?.read_to_end(&mut sink)?;
                (sink, 4 + 8 + 4)
            }
            CompressionMethod::None => (body, 4 + 4),
        };

        let content_flags = ContentFlags::from_bits_retain(body[0]);
        let hash_tree_block_len =
            (u32::from_le_bytes(unsafe { TryFrom::try_from(&body[1..5]).unwrap_unchecked() })
                as u64)
                << 3;
        let exact_match_block_len =
            u64::from_le_bytes(unsafe { TryFrom::try_from(&body[5..13]).unwrap_unchecked() });

        Ok(Self {
            head_size,
            block: body,
            content_flags,
            hash_tree_block_range: 13..(13 + hash_tree_block_len) as usize,
            exact_match_block_range: (13 + hash_tree_block_len) as usize
                ..(13 + hash_tree_block_len + exact_match_block_len) as usize,
            content_baseptr: (13 + hash_tree_block_len + exact_match_block_len) as usize,
        })
    }

    fn list_entry(&self, callback: impl FnMut(AssetNameRef)) {
        list_entry(
            self.head_size,
            self.content_flags
                .contains(ContentFlags::ROOT_HASH_TREE_EXACT),
            &self.block[self.hash_tree_block_range.clone()],
            &self.block[self.exact_match_block_range.clone()],
            callback,
        )
    }

    fn find_entry(&self, name: &str, ext: &str) -> Option<AssetEntryHeadingPair> {
        find_entry(
            self.head_size,
            name,
            ext,
            self.content_flags
                .contains(ContentFlags::ROOT_HASH_TREE_EXACT),
            &self.block[self.hash_tree_block_range.clone()],
            &self.block[self.exact_match_block_range.clone()],
        )
    }

    fn read_bin<'a>(&'a self, heading: AssetEntryHeadingPair) -> OnMemoryArchiveBinReader<'a> {
        OnMemoryArchiveBinReader {
            archive: self,
            pointer: self.content_baseptr as u64 + heading.relative_offset,
            pointer_limit: self.content_baseptr as u64
                + heading.relative_offset
                + heading.byte_length,
        }
    }
}

pub struct FileStreamingArchiveBinReader<'a> {
    archive: &'a FileStreamingArchive,
    pointer: u64,
    pointer_limit: u64,
}
impl std::io::Read for FileStreamingArchiveBinReader<'_> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        let read_len = (buf.len() as u64).min(self.pointer_limit - self.pointer);
        let r = self
            .archive
            .handle
            .pread(&mut buf[..read_len as _], self.pointer)?;
        self.pointer += r as u64;

        Ok(r)
    }
}

pub struct FileStreamingArchiveBinReaderAsync<'a> {
    archive: &'a FileStreamingArchiveAsync,
    pointer: u64,
    pointer_limit: u64,
}
impl<'a> FileStreamingArchiveBinReaderAsync<'a> {
    pub async fn read<'b>(&mut self, buf: &'b mut [u8]) -> IOResult<usize> {
        let read_len = (buf.len() as u64).min(self.pointer_limit - self.pointer);
        let bytes = self
            .archive
            .handle
            .pread_async(&mut buf[..read_len as usize], self.pointer)
            .await?;
        self.pointer += bytes as u64;

        Ok(bytes)
    }

    pub async fn read_exact(&mut self, mut buf: &mut [u8]) -> IOResult<()> {
        while !buf.is_empty() {
            let r = self.read(buf).await?;
            buf = &mut buf[r..];
        }

        Ok(())
    }

    pub async fn read_all(&mut self) -> IOResult<Vec<u8>> {
        let mut b = vec![0u8; (self.pointer_limit - self.pointer) as usize];
        self.read_exact(&mut b).await?;

        Ok(b)
    }
}

pub struct FileStreamingArchiveAsync {
    pub handle: PlatformNativeFileReaderAsync,
    pub entry_mapped_head: core::sync::atomic::AtomicPtr<core::ffi::c_void>,
    pub entry_unmap_data:
        Option<<PlatformNativeFileReaderAsync as NativeFileMemoryMapProvider>::MemoryUnmapData>,
    pub content_flags: ContentFlags,
    pub hash_tree_block_range: core::ops::Range<usize>,
    pub exact_match_block_range: core::ops::Range<usize>,
    pub content_baseptr: u64,
}
impl FileStreamingArchiveAsync {
    async fn new(mut handle: PlatformNativeFileReaderAsync) -> IOResult<Self> {
        let mut content_flags_buf = [0u8];
        let mut hash_tree_block_len_buf = [0u8; 4];
        let mut exact_match_block_len_buf = [0u8; 8];

        let mut slices = &mut [
            IoSliceMut::new(&mut content_flags_buf),
            IoSliceMut::new(&mut hash_tree_block_len_buf),
            IoSliceMut::new(&mut exact_match_block_len_buf),
        ][..];
        while !slices.is_empty() {
            let r = handle.readv_async(slices).await?;
            IoSliceMut::advance_slices(&mut slices, r);
        }

        let content_flags = ContentFlags::from_bits_retain(content_flags_buf[0]);
        let hash_tree_block_len = (u32::from_le_bytes(hash_tree_block_len_buf) as u64) << 3;
        let exact_match_block_len = u64::from_le_bytes(exact_match_block_len_buf);

        let entry_block_start_pos = handle.current_pointer_pos()?;
        let (entry_mapped_head, entry_unmap_data) = handle.mmap(
            entry_block_start_pos,
            hash_tree_block_len + exact_match_block_len,
        )?;

        Ok(Self {
            handle,
            entry_mapped_head: core::sync::atomic::AtomicPtr::new(entry_mapped_head),
            entry_unmap_data: Some(entry_unmap_data),
            content_flags,
            hash_tree_block_range: 0..hash_tree_block_len as usize,
            exact_match_block_range: hash_tree_block_len as usize
                ..(hash_tree_block_len + exact_match_block_len) as usize,
            content_baseptr: entry_block_start_pos + hash_tree_block_len + exact_match_block_len,
        })
    }

    fn list_entry(&self, callback: impl FnMut(AssetNameRef)) {
        let entry_ptr = self
            .entry_mapped_head
            .load(core::sync::atomic::Ordering::Acquire);

        list_entry(
            // FileStreamingのときは4+4固定になる（非圧縮でしかこれにならないので）
            4 + 4,
            self.content_flags
                .contains(ContentFlags::ROOT_HASH_TREE_EXACT),
            unsafe {
                core::slice::from_raw_parts(
                    entry_ptr.byte_add(self.hash_tree_block_range.start) as *const u8,
                    self.hash_tree_block_range.len(),
                )
            },
            unsafe {
                core::slice::from_raw_parts(
                    entry_ptr.byte_add(self.exact_match_block_range.start) as *const u8,
                    self.exact_match_block_range.len(),
                )
            },
            callback,
        )
    }

    fn find_entry(&self, name: &str, ext: &str) -> Option<AssetEntryHeadingPair> {
        let entry_ptr = self
            .entry_mapped_head
            .load(core::sync::atomic::Ordering::Acquire);

        find_entry(
            // FileStreamingのときは4+4固定になる（非圧縮でしかこれにならないので）
            4 + 4,
            name,
            ext,
            self.content_flags
                .contains(ContentFlags::ROOT_HASH_TREE_EXACT),
            unsafe {
                core::slice::from_raw_parts(
                    entry_ptr.byte_add(self.hash_tree_block_range.start) as *const u8,
                    self.hash_tree_block_range.len(),
                )
            },
            unsafe {
                core::slice::from_raw_parts(
                    entry_ptr.byte_add(self.exact_match_block_range.start) as *const u8,
                    self.exact_match_block_range.len(),
                )
            },
        )
    }

    fn read_bin<'a>(
        &'a self,
        heading: AssetEntryHeadingPair,
    ) -> FileStreamingArchiveBinReaderAsync<'a> {
        FileStreamingArchiveBinReaderAsync {
            archive: self,
            pointer: self.content_baseptr + heading.relative_offset,
            pointer_limit: self.content_baseptr + heading.relative_offset + heading.byte_length,
        }
    }
}

pub struct FileStreamingArchive {
    pub handle: PlatformNativeFileReader,
    pub entry_mapped_head: core::sync::atomic::AtomicPtr<core::ffi::c_void>,
    pub entry_unmap_data:
        Option<<PlatformNativeFileReader as NativeFileMemoryMapProvider>::MemoryUnmapData>,
    pub content_flags: ContentFlags,
    pub hash_tree_block_range: core::ops::Range<usize>,
    pub exact_match_block_range: core::ops::Range<usize>,
    pub content_baseptr: u64,
}
impl Drop for FileStreamingArchive {
    fn drop(&mut self) {
        let _ = self
            .handle
            .munmap(self.entry_unmap_data.take().expect("drop twice!"));
    }
}
impl FileStreamingArchive {
    fn new(mut handle: PlatformNativeFileReader) -> IOResult<Self> {
        let mut content_flags_buf = [0u8];
        let mut hash_tree_block_len_buf = [0u8; 4];
        let mut exact_match_block_len_buf = [0u8; 8];

        let mut slices = &mut [
            IoSliceMut::new(&mut content_flags_buf),
            IoSliceMut::new(&mut hash_tree_block_len_buf),
            IoSliceMut::new(&mut exact_match_block_len_buf),
        ][..];
        while !slices.is_empty() {
            let r = handle.readv(slices)?;
            IoSliceMut::advance_slices(&mut slices, r);
        }

        let content_flags = ContentFlags::from_bits_retain(content_flags_buf[0]);
        let hash_tree_block_len = (u32::from_le_bytes(hash_tree_block_len_buf) as u64) << 3;
        let exact_match_block_len = u64::from_le_bytes(exact_match_block_len_buf);

        let entry_block_start_pos = handle.current_pointer_pos()?;
        let (entry_mapped_head, entry_unmap_data) = handle.mmap(
            entry_block_start_pos,
            hash_tree_block_len + exact_match_block_len,
        )?;

        Ok(Self {
            handle,
            entry_mapped_head: core::sync::atomic::AtomicPtr::new(entry_mapped_head),
            entry_unmap_data: Some(entry_unmap_data),
            content_flags,
            hash_tree_block_range: 0..hash_tree_block_len as usize,
            exact_match_block_range: hash_tree_block_len as usize
                ..(hash_tree_block_len + exact_match_block_len) as usize,
            content_baseptr: entry_block_start_pos + hash_tree_block_len + exact_match_block_len,
        })
    }

    fn list_entry(&self, callback: impl FnMut(AssetNameRef)) {
        let entry_ptr = self
            .entry_mapped_head
            .load(core::sync::atomic::Ordering::Acquire);

        list_entry(
            // FileStreamingのときは4+4固定になる（非圧縮でしかこれにならないので）
            4 + 4,
            self.content_flags
                .contains(ContentFlags::ROOT_HASH_TREE_EXACT),
            unsafe {
                core::slice::from_raw_parts(
                    entry_ptr.byte_add(self.hash_tree_block_range.start) as *const u8,
                    self.hash_tree_block_range.len(),
                )
            },
            unsafe {
                core::slice::from_raw_parts(
                    entry_ptr.byte_add(self.exact_match_block_range.start) as *const u8,
                    self.exact_match_block_range.len(),
                )
            },
            callback,
        )
    }

    fn find_entry(&self, name: &str, ext: &str) -> Option<AssetEntryHeadingPair> {
        let entry_ptr = self
            .entry_mapped_head
            .load(core::sync::atomic::Ordering::Acquire);

        find_entry(
            // FileStreamingのときは4+4固定になる（非圧縮でしかこれにならないので）
            4 + 4,
            name,
            ext,
            self.content_flags
                .contains(ContentFlags::ROOT_HASH_TREE_EXACT),
            unsafe {
                core::slice::from_raw_parts(
                    entry_ptr.byte_add(self.hash_tree_block_range.start) as *const u8,
                    self.hash_tree_block_range.len(),
                )
            },
            unsafe {
                core::slice::from_raw_parts(
                    entry_ptr.byte_add(self.exact_match_block_range.start) as *const u8,
                    self.exact_match_block_range.len(),
                )
            },
        )
    }

    fn read_bin<'a>(&'a self, heading: AssetEntryHeadingPair) -> FileStreamingArchiveBinReader<'a> {
        FileStreamingArchiveBinReader {
            archive: self,
            pointer: self.content_baseptr + heading.relative_offset,
            pointer_limit: self.content_baseptr + heading.relative_offset + heading.byte_length,
        }
    }
}

pub enum ArchiveBinReader<'a> {
    OnMemory(OnMemoryArchiveBinReader<'a>),
    FileStreaming(FileStreamingArchiveBinReader<'a>),
}
impl std::io::Read for ArchiveBinReader<'_> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        match *self {
            Self::OnMemory(ref mut x) => x.read(buf),
            Self::FileStreaming(ref mut x) => x.read(buf),
        }
    }
}

pub enum ArchiveBinReaderAsync<'a> {
    OnMemory(OnMemoryArchiveBinReader<'a>),
    FileStreaming(FileStreamingArchiveBinReaderAsync<'a>),
}
impl<'a> ArchiveBinReaderAsync<'a> {
    #[inline]
    pub async fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        match *self {
            Self::OnMemory(ref mut x) => x.read(buf),
            Self::FileStreaming(ref mut x) => x.read(buf).await,
        }
    }

    #[inline]
    pub async fn read_exact(&mut self, buf: &mut [u8]) -> IOResult<()> {
        match *self {
            Self::OnMemory(ref mut x) => x.read_exact(buf),
            Self::FileStreaming(ref mut x) => x.read_exact(buf).await,
        }
    }

    #[inline]
    pub async fn read_all(&mut self) -> IOResult<Vec<u8>> {
        match *self {
            Self::OnMemory(ref mut x) => {
                let mut sink = Vec::new();
                x.read_to_end(&mut sink)?;

                Ok(sink)
            }
            Self::FileStreaming(ref mut x) => x.read_all().await,
        }
    }
}

pub enum ArchiveAsync {
    OnMemory(OnMemoryArchive),
    FileStreaming(FileStreamingArchiveAsync),
}
impl ArchiveAsync {
    pub async fn open(
        path: &(impl AsRef<Path> + ?Sized),
        check_integrity: bool,
    ) -> ArchiveReadResult<Self> {
        let mut f = PlatformNativeFileReaderAsync::open(path)?;
        let (comp, crc) = Self::read_file_header(&mut f).await?;
        if check_integrity {
            // read entire file for compute crc32
            let body = f.read_to_end().await?;
            let input_crc = crc32::checksum_ieee(&body[..]);
            if input_crc != crc {
                return Err(ArchiveReadError::IntegrityCheckFailed);
            }

            return Ok(Self::OnMemory(OnMemoryArchive::new(comp, body)?));
        }

        match comp {
            CompressionMethod::None => Ok(Self::FileStreaming(
                FileStreamingArchiveAsync::new(f).await?,
            )),
            _ => {
                // read entire file for decompression
                let body = f.read_to_end().await?;
                Ok(Self::OnMemory(OnMemoryArchive::new(comp, body)?))
            }
        }
    }

    async fn read_file_header(
        reader: &mut (impl AsyncNativeFileReader + ?Sized),
    ) -> ArchiveReadResult<(CompressionMethod, u32)> {
        let mut signature = [0u8; 4];
        reader.read_exact(&mut signature[..]).await.map(drop)?;
        let mut sink_64_bits = [0u8; 8];
        let comp = match &signature {
            b"par " => CompressionMethod::None,
            b"pard" => reader
                .read_exact(&mut sink_64_bits)
                .await
                .map(|_| CompressionMethod::Zlib(u64::from_le_bytes(sink_64_bits)))?,
            b"parz" => reader
                .read_exact(&mut sink_64_bits)
                .await
                .map(|_| CompressionMethod::Lz4(u64::from_le_bytes(sink_64_bits)))?,
            b"par1" => reader
                .read_exact(&mut sink_64_bits)
                .await
                .map(|_| CompressionMethod::Zstd11(u64::from_le_bytes(sink_64_bits)))?,
            _ => return Err(ArchiveReadError::SignatureMismatch),
        };

        let mut crc32_bytes = [0u8; 4];
        reader.read_exact(&mut crc32_bytes).await?;

        Ok((comp, u32::from_le_bytes(crc32_bytes)))
    }

    #[inline]
    pub fn list_entry(&self, callback: impl FnMut(AssetNameRef)) {
        match *self {
            Self::OnMemory(ref x) => x.list_entry(callback),
            Self::FileStreaming(ref x) => x.list_entry(callback),
        }
    }

    #[inline]
    pub fn find_entry(&self, name: &str, ext: &str) -> Option<AssetEntryHeadingPair> {
        match *self {
            Self::OnMemory(ref x) => x.find_entry(name, ext),
            Self::FileStreaming(ref x) => x.find_entry(name, ext),
        }
    }

    #[inline]
    pub fn read_bin<'a>(&'a self, heading: AssetEntryHeadingPair) -> ArchiveBinReaderAsync<'a> {
        match *self {
            Self::OnMemory(ref x) => ArchiveBinReaderAsync::OnMemory(x.read_bin(heading)),
            Self::FileStreaming(ref x) => ArchiveBinReaderAsync::FileStreaming(x.read_bin(heading)),
        }
    }
}

pub enum Archive {
    OnMemory(OnMemoryArchive),
    FileStreaming(FileStreamingArchive),
}
impl Archive {
    pub fn open(
        path: &(impl AsRef<Path> + ?Sized),
        check_integrity: bool,
    ) -> ArchiveReadResult<Self> {
        let mut f = PlatformNativeFileReader::open(path)?;
        let (comp, crc) = Self::read_file_header(&mut f)?;
        if check_integrity {
            // read entire file for compute crc32
            let body = f.read_to_end()?;
            let input_crc = crc32::checksum_ieee(&body[..]);
            if input_crc != crc {
                return Err(ArchiveReadError::IntegrityCheckFailed);
            }

            return Ok(Self::OnMemory(OnMemoryArchive::new(comp, body)?));
        }

        match comp {
            CompressionMethod::None => Ok(Self::FileStreaming(FileStreamingArchive::new(f)?)),
            _ => {
                // read entire file for decompression
                let body = f.read_to_end()?;
                Ok(Self::OnMemory(OnMemoryArchive::new(comp, body)?))
            }
        }
    }

    fn read_file_header(
        reader: &mut (impl NativeFileReader + ?Sized),
    ) -> ArchiveReadResult<(CompressionMethod, u32)> {
        let mut signature = [0u8; 4];
        reader.read_exact(&mut signature[..]).map(drop)?;
        let mut sink_64_bits = [0u8; 8];
        let comp = match &signature {
            b"par " => CompressionMethod::None,
            b"pard" => reader
                .read_exact(&mut sink_64_bits)
                .map(|_| CompressionMethod::Zlib(u64::from_le_bytes(sink_64_bits)))?,
            b"parz" => reader
                .read_exact(&mut sink_64_bits)
                .map(|_| CompressionMethod::Lz4(u64::from_le_bytes(sink_64_bits)))?,
            b"par1" => reader
                .read_exact(&mut sink_64_bits)
                .map(|_| CompressionMethod::Zstd11(u64::from_le_bytes(sink_64_bits)))?,
            _ => return Err(ArchiveReadError::SignatureMismatch),
        };

        let mut crc32_bytes = [0u8; 4];
        reader.read_exact(&mut crc32_bytes)?;

        Ok((comp, u32::from_le_bytes(crc32_bytes)))
    }

    #[inline]
    pub fn list_entry(&self, callback: impl FnMut(AssetNameRef)) {
        match *self {
            Self::OnMemory(ref x) => x.list_entry(callback),
            Self::FileStreaming(ref x) => x.list_entry(callback),
        }
    }

    #[inline]
    pub fn find_entry(&self, name: &str, ext: &str) -> Option<AssetEntryHeadingPair> {
        match *self {
            Self::OnMemory(ref x) => x.find_entry(name, ext),
            Self::FileStreaming(ref x) => x.find_entry(name, ext),
        }
    }

    #[inline]
    pub fn read_bin<'a>(&'a self, heading: AssetEntryHeadingPair) -> ArchiveBinReader<'a> {
        match *self {
            Self::OnMemory(ref x) => ArchiveBinReader::OnMemory(x.read_bin(heading)),
            Self::FileStreaming(ref x) => ArchiveBinReader::FileStreaming(x.read_bin(heading)),
        }
    }
}
