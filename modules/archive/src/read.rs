use std::{
    collections::HashMap,
    convert::TryFrom,
    fs::File,
    io::{
        BufRead, BufReader, Cursor, Error as IOError, ErrorKind, IoSliceMut, Read,
        Result as IOResult, Seek, SeekFrom,
    },
    os::windows::ffi::OsStrExt,
    path::Path,
};

use crate::{
    AssetEntryHeadingPair, CompressionMethod, ContentFlags, EitherArchiveReaderAsync,
    EntryTreePointer, WhereArchiveAsync,
};
use crc::crc32;
use libflate::deflate as zlib;
use peridot_serialization_utils::{PascalString, VariableUInt, VariableULong};

#[repr(transparent)]
pub struct NativeFileReadWrapper<R: NativeFileReader>(pub R);
impl<R: NativeFileReader> NativeFileReadWrapper<R> {
    pub const fn from_mut_ref(r: &mut R) -> &mut Self {
        unsafe { core::mem::transmute(r) }
    }
}
impl<R: NativeFileReader> std::io::Read for NativeFileReadWrapper<R> {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        self.0.read(buf)
    }
}

pub trait NativeFileReader {
    type MemoryUnmapData;

    fn current_pointer_pos(&self) -> std::io::Result<u64>;
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>;
    fn readv(&mut self, buf: &mut [std::io::IoSliceMut]) -> std::io::Result<usize>;
    fn pread(&self, buf: &mut [u8], offs: u64) -> std::io::Result<usize>;
    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)>;
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()>;

    fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()> {
        let mut o = 0;
        while o < buf.len() {
            let r = self.read(&mut buf[o..])?;
            o += r;
        }

        Ok(())
    }

    fn read_to_end(&mut self) -> std::io::Result<Vec<u8>> {
        const GROW_SIZE: usize = 8192;

        let mut buf = Vec::with_capacity(GROW_SIZE);
        unsafe {
            buf.set_len(GROW_SIZE);
        }
        let mut o = 0;
        loop {
            match self.read(&mut buf[o..]) {
                Ok(0) => {
                    buf.truncate(o);
                    break;
                }
                Ok(r) => {
                    o += r;
                    if o >= buf.len() {
                        buf.reserve_exact(GROW_SIZE);
                        unsafe {
                            buf.set_len(buf.capacity());
                        }
                    }
                }
                Err(e) if e.kind() == std::io::ErrorKind::UnexpectedEof => {
                    buf.truncate(o);
                    break;
                }
                Err(e) => return Err(e),
            }
        }

        Ok(buf)
    }
}

#[cfg(windows)]
#[repr(transparent)]
pub struct WindowsNativeFileReader(windows::Win32::Foundation::HANDLE);
#[cfg(windows)]
impl Drop for WindowsNativeFileReader {
    fn drop(&mut self) {
        unsafe {
            let _ = windows::Win32::Foundation::CloseHandle(self.0);
        }
    }
}
#[cfg(windows)]
impl WindowsNativeFileReader {
    pub fn open(name: &(impl AsRef<std::path::Path> + ?Sized)) -> std::io::Result<Self> {
        let path_wstr = name
            .as_ref()
            .as_os_str()
            .encode_wide()
            .chain(core::iter::once(0))
            .collect::<Vec<_>>();

        let h = unsafe {
            windows::Win32::Storage::FileSystem::CreateFileW(
                windows::core::PCWSTR::from_raw(path_wstr.as_ptr()),
                windows::Win32::Foundation::GENERIC_READ.0,
                windows::Win32::Storage::FileSystem::FILE_SHARE_READ,
                None,
                windows::Win32::Storage::FileSystem::OPEN_EXISTING,
                windows::Win32::Storage::FileSystem::FILE_ATTRIBUTE_NORMAL
                    | windows::Win32::Storage::FileSystem::FILE_FLAG_RANDOM_ACCESS,
                None,
            )?
        };

        Ok(Self(h))
    }
}
#[cfg(windows)]
pub struct WindowsMemoryUnmapData {
    handle: windows::Win32::Foundation::HANDLE,
    base_addr: windows::Win32::System::Memory::MEMORY_MAPPED_VIEW_ADDRESS,
}
#[cfg(windows)]
impl NativeFileReader for WindowsNativeFileReader {
    type MemoryUnmapData = WindowsMemoryUnmapData;

    #[inline]
    fn current_pointer_pos(&self) -> std::io::Result<u64> {
        let mut pos = 0;
        unsafe {
            windows::Win32::Storage::FileSystem::SetFilePointerEx(
                self.0,
                0,
                Some(&mut pos),
                windows::Win32::Storage::FileSystem::FILE_CURRENT,
            )?;
        }

        Ok(pos as _)
    }

    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
        let mut read_bytes = 0;
        unsafe {
            windows::Win32::Storage::FileSystem::ReadFile(
                self.0,
                Some(buf),
                Some(&mut read_bytes),
                None,
            )?;
        }

        Ok(read_bytes as _)
    }

    #[inline]
    fn readv(&mut self, buf: &mut [std::io::IoSliceMut]) -> std::io::Result<usize> {
        // no support for windows
        self.read(&mut buf[0])
    }

    #[inline]
    fn pread(&self, buf: &mut [u8], offs: u64) -> std::io::Result<usize> {
        let mut read_bytes = 0;
        unsafe {
            windows::Win32::Storage::FileSystem::ReadFile(
                self.0,
                Some(buf),
                Some(&mut read_bytes),
                Some(&mut windows::Win32::System::IO::OVERLAPPED {
                    Internal: 0,
                    InternalHigh: 0,
                    Anonymous: windows::Win32::System::IO::OVERLAPPED_0 {
                        Anonymous: windows::Win32::System::IO::OVERLAPPED_0_0 {
                            Offset: (offs & 0xffff_ffff) as u32,
                            OffsetHigh: ((offs >> 32) & 0xffff_ffff) as u32,
                        },
                    },
                    hEvent: windows::Win32::Foundation::HANDLE(core::ptr::null_mut()),
                }),
            )?;
        }

        Ok(read_bytes as _)
    }

    fn mmap(
        &self,
        offs: u64,
        len: u64,
    ) -> std::io::Result<(*mut core::ffi::c_void, Self::MemoryUnmapData)> {
        // TODO: 必要そうならキャッシュする
        let mut sysinfo = core::mem::MaybeUninit::uninit();
        unsafe {
            windows::Win32::System::SystemInformation::GetSystemInfo(sysinfo.as_mut_ptr());
        }
        let page_size = unsafe { sysinfo.assume_init_ref().dwPageSize };

        // オフセットをページ境界にあわせる必要があるらしい
        let offset_aligned = (offs / page_size as u64) * page_size as u64;
        let offset_in_mapped_range = offs - offset_aligned;
        let len_extended = len + offset_in_mapped_range;

        let h = unsafe {
            windows::Win32::System::Memory::CreateFileMappingW(
                self.0,
                None,
                windows::Win32::System::Memory::PAGE_READONLY,
                ((len_extended >> 32) & 0xffff_ffff) as u32,
                (len_extended & 0xffff_ffff) as u32,
                None,
            )
            .expect("r")
        };
        let ptr = unsafe {
            windows::Win32::System::Memory::MapViewOfFile(
                h,
                windows::Win32::System::Memory::FILE_MAP_READ,
                ((offset_aligned >> 32) & 0xffff_ffff) as u32,
                (offset_aligned & 0xffff_ffff) as u32,
                0,
            )
        };

        Ok((
            unsafe { ptr.Value.byte_add(offset_in_mapped_range as _) },
            WindowsMemoryUnmapData {
                handle: h,
                base_addr: ptr,
            },
        ))
    }

    #[inline]
    fn munmap(&self, data: Self::MemoryUnmapData) -> std::io::Result<()> {
        unsafe {
            windows::Win32::System::Memory::UnmapViewOfFile(data.base_addr)?;
            windows::Win32::Foundation::CloseHandle(data.handle)?;
        }

        Ok(())
    }
}

#[cfg(windows)]
type PlatformNativeFileReader = WindowsNativeFileReader;

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
                IOError::new(ErrorKind::Other, "Archive Integrity check failed")
            }
            ArchiveReadError::SignatureMismatch => {
                IOError::new(ErrorKind::Other, "Archive Signature Mismatch")
            }
            ArchiveReadError::Lz4DecompressError(e) => {
                IOError::new(ErrorKind::Other, format!("Lz4DecompressError: {:?}", e))
            }
        }
    }
}

pub struct ArchiveReadAsync {
    entries: HashMap<String, AssetEntryHeadingPair>,
    content: EitherArchiveReaderAsync,
    content_baseptr: u64,
}
#[cfg(feature = "async-rt-async-std")]
impl ArchiveReadAsync {
    pub async fn from_file(
        path: impl AsRef<async_std::path::Path>,
        check_integrity: bool,
    ) -> ArchiveReadResult<Self> {
        let mut fi = async_std::fs::File::open(path)
            .await
            .map(async_std::io::BufReader::new)?;
        let (comp, crc) = Self::read_file_header(&mut fi).await?;
        let mut body = WhereArchiveAsync::FromIO(fi);
        if check_integrity {
            let input_crc = crc32::checksum_ieee(&body.on_memory().await?[..]);
            if input_crc != crc {
                // CRCミスマッチ
                return Err(ArchiveReadError::IntegrityCheckFailed);
            }
        }

        match comp {
            CompressionMethod::Lz4(_) => {
                body = WhereArchiveAsync::OnMemory(lz4_compression::prelude::decompress(
                    body.on_memory().await?,
                )?);
            }
            CompressionMethod::Zlib(ubl) => {
                // TODO: ライブラリが対応してないので、全部オンメモリに展開してからじゃないと処理できない
                let mut compressed = Vec::new();
                async_std::io::ReadExt::read_to_end(
                    &mut EitherArchiveReaderAsync::from(body),
                    &mut compressed,
                )
                .await?;
                let mut sink = Vec::with_capacity(ubl as _);
                zlib::Decoder::new(Cursor::new(compressed)).read_to_end(&mut sink)?;
                body = WhereArchiveAsync::OnMemory(sink);
            }
            CompressionMethod::Zstd11(ubl) => {
                // TODO: ライブラリが対応してないので、全部オンメモリに展開してからじゃないと処理できない
                let mut compressed = Vec::new();
                async_std::io::ReadExt::read_to_end(
                    &mut EitherArchiveReaderAsync::from(body),
                    &mut compressed,
                )
                .await?;
                let mut sink = Vec::with_capacity(ubl as _);
                zstd::Decoder::new(Cursor::new(compressed))?.read_to_end(&mut sink)?;
                body = WhereArchiveAsync::OnMemory(sink);
            }
            CompressionMethod::None => (/* Nothing to do */),
        }
        let mut areader = EitherArchiveReaderAsync::from(body);
        let entries = Self::read_asset_entries(&mut areader).await?;
        let content_baseptr =
            async_std::io::SeekExt::seek(&mut areader, SeekFrom::Current(0)).await?;

        Ok(Self {
            entries,
            content: areader,
            content_baseptr,
        })
    }

    async fn read_file_header(
        reader: &mut (impl async_std::io::Read + Unpin + ?Sized),
    ) -> ArchiveReadResult<(CompressionMethod, u32)> {
        let mut signature = [0u8; 4];
        async_std::io::ReadExt::read_exact(reader, &mut signature).await?;
        let mut sink_64_bits = [0u8; 8];
        let comp = match &signature {
            b"par " => CompressionMethod::None,
            b"pard" => async_std::io::ReadExt::read_exact(reader, &mut sink_64_bits)
                .await
                .map(|_| CompressionMethod::Zlib(u64::from_le_bytes(sink_64_bits)))?,
            b"parz" => async_std::io::ReadExt::read_exact(reader, &mut sink_64_bits)
                .await
                .map(|_| CompressionMethod::Lz4(u64::from_le_bytes(sink_64_bits)))?,
            b"par1" => async_std::io::ReadExt::read_exact(reader, &mut sink_64_bits)
                .await
                .map(|_| CompressionMethod::Zstd11(u64::from_le_bytes(sink_64_bits)))?,
            _ => return Err(ArchiveReadError::SignatureMismatch),
        };
        let mut crc32_bytes = [0u8; 4];
        async_std::io::ReadExt::read_exact(reader, &mut crc32_bytes).await?;

        Ok((comp, u32::from_le_bytes(crc32_bytes)))
    }

    async fn read_asset_entries(
        reader: &mut (impl async_std::io::BufRead + Unpin + ?Sized),
    ) -> IOResult<HashMap<String, AssetEntryHeadingPair>> {
        let VariableUInt(count) = VariableUInt::read_async(reader).await?;
        if count <= 0 {
            return Ok(HashMap::new());
        }

        let mut elements = HashMap::with_capacity(count as _);
        for _ in 0..count {
            let heading = AssetEntryHeadingPair::read_async(reader).await?;
            let PascalString(id_ref) = PascalString::read_async(reader).await?;
            elements.insert(id_ref, heading);
        }

        Ok(elements)
    }

    pub async fn read_bin(&mut self, path: &str) -> IOResult<Option<Vec<u8>>> {
        let Some(entry_pair) = self.find(path) else {
            // ない
            return Ok(None);
        };

        async_std::io::SeekExt::seek(&mut self.content, SeekFrom::Start(entry_pair.byte_offset))
            .await?;
        let mut sink = Vec::with_capacity(entry_pair.byte_length as _);
        unsafe {
            sink.set_len(entry_pair.byte_length as _);
        }

        async_std::io::ReadExt::read_exact(&mut self.content, &mut sink).await?;
        Ok(Some(sink))
    }
}
impl ArchiveReadAsync {
    pub fn entry_names(&self) -> impl Iterator<Item = &str> {
        self.entries.keys().map(|k| k.as_str())
    }

    pub fn find<'s>(&'s self, path: &str) -> Option<AssetEntryInfo> {
        self.entries.get(path).map(|x| AssetEntryInfo {
            byte_length: x.byte_length,
            byte_offset: self.content_baseptr + x.relative_offset,
        })
    }

    pub fn into_inner_reader(self) -> EitherArchiveReaderAsync {
        self.content
    }
}

pub enum WhereArchive {
    OnMemory(Vec<u8>),
    FromIO(PlatformNativeFileReader),
}
impl WhereArchive {
    pub fn on_memory(&mut self) -> IOResult<&[u8]> {
        let replace_buf = if let WhereArchive::FromIO(ref mut r) = self {
            Some(r.read_to_end()?)
        } else {
            None
        };

        if let Some(b) = replace_buf {
            *self = WhereArchive::OnMemory(b);
        }
        match self {
            WhereArchive::OnMemory(ref b) => Ok(b),
            _ => unreachable!(),
        }
    }
}

pub enum EitherArchiveReader {
    OnMemory(Cursor<Vec<u8>>),
    FromIO(PlatformNativeFileReader),
}
impl EitherArchiveReader {
    fn new(a: WhereArchive) -> Self {
        match a {
            WhereArchive::FromIO(r) => EitherArchiveReader::FromIO(r),
            WhereArchive::OnMemory(b) => EitherArchiveReader::OnMemory(Cursor::new(b)),
        }
    }
    pub fn unwrap(self) -> WhereArchive {
        match self {
            EitherArchiveReader::FromIO(r) => WhereArchive::FromIO(r),
            EitherArchiveReader::OnMemory(c) => WhereArchive::OnMemory(c.into_inner()),
        }
    }
}
impl Read for EitherArchiveReader {
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        match self {
            Self::FromIO(ref mut r) => r.read(buf),
            Self::OnMemory(ref mut c) => c.read(buf),
        }
    }
}
// impl BufRead for EitherArchiveReader {
//     #[inline]
//     fn fill_buf(&mut self) -> IOResult<&[u8]> {
//         match self {
//             Self::FromIO(ref mut r) => r.fill_buf(),
//             Self::OnMemory(ref mut c) => c.fill_buf(),
//         }
//     }

//     #[inline]
//     fn consume(&mut self, amt: usize) {
//         match self {
//             Self::FromIO(ref mut r) => r.consume(amt),
//             Self::OnMemory(ref mut c) => c.consume(amt),
//         }
//     }
// }
// impl Seek for EitherArchiveReader {
//     #[inline]
//     fn seek(&mut self, pos: SeekFrom) -> IOResult<u64> {
//         match self {
//             Self::FromIO(ref mut r) => r.seek(pos),
//             Self::OnMemory(ref mut c) => c.seek(pos),
//         }
//     }
// }

#[repr(transparent)]
struct ExactHashTreeEntryView<'b>(pub &'b [u8]);
impl ExactHashTreeEntryView<'_> {
    #[inline(always)]
    fn name_hash(&self) -> u64 {
        u64::from_le_bytes(unsafe { TryFrom::try_from(&self.0[0..8]).unwrap_unchecked() })
    }

    #[inline(always)]
    fn exact_block_offset(&self) -> u64 {
        u64::from_le_bytes(unsafe { TryFrom::try_from(&self.0[8..16]).unwrap_unchecked() })
    }
}

#[repr(transparent)]
struct HashTreeEntryView<'b>(pub &'b [u8]);
impl HashTreeEntryView<'_> {
    #[inline(always)]
    fn name_hash(&self) -> u64 {
        u64::from_le_bytes(unsafe { TryFrom::try_from(&self.0[0..8]).unwrap_unchecked() })
    }

    #[inline(always)]
    fn exact_block_offset(&self) -> u64 {
        u64::from_le_bytes(unsafe { TryFrom::try_from(&self.0[8..16]).unwrap_unchecked() })
    }

    #[inline(always)]
    fn smaller_tree_pointer(&self) -> EntryTreePointer {
        EntryTreePointer::from_le_bytes(unsafe {
            TryFrom::try_from(&self.0[16..24]).unwrap_unchecked()
        })
    }
}

#[repr(transparent)]
struct HashTreeBlockView<'b>(pub &'b [u8]);
impl<'b> HashTreeBlockView<'b> {
    #[inline(always)]
    fn smallest_entry(&self) -> HashTreeEntryView<'b> {
        HashTreeEntryView(&self.0[0..])
    }

    #[inline(always)]
    fn entry(&self, at: usize) -> HashTreeEntryView<'b> {
        HashTreeEntryView(&self.0[at * (8 * 3)..])
    }

    #[inline(always)]
    fn largest_entry(&self) -> HashTreeEntryView<'b> {
        HashTreeEntryView(&self.0[self.0.len() - 32..])
    }

    #[inline(always)]
    fn larger_tree_pointer(&self) -> EntryTreePointer {
        EntryTreePointer::from_le_bytes(unsafe {
            TryFrom::try_from(&self.0[self.0.len() - 8..]).unwrap_unchecked()
        })
    }
}

#[repr(transparent)]
struct ExactHashTreeBlockView<'b>(pub &'b [u8]);
impl<'b> ExactHashTreeBlockView<'b> {
    #[inline(always)]
    fn entry_count(&self) -> u16 {
        u16::from_le_bytes(unsafe { TryFrom::try_from(&self.0[0..2]).unwrap_unchecked() })
    }

    #[inline(always)]
    fn entry(&self, at: usize) -> ExactHashTreeEntryView<'b> {
        ExactHashTreeEntryView(&self.0[2 + at * 8 * 2..])
    }
}

fn list_entry(
    head_size: usize,
    hash_tree_root_exact: bool,
    hash_tree_block: &[u8],
    exact_match_block: &[u8],
    mut callback: impl FnMut(&str),
) {
    fn enumerate_exact_block_content(
        block: &[u8],
        pointer: usize,
        callback: &mut impl FnMut(&str),
    ) {
        let (VariableUInt(entry_count), entry_count_len) =
            VariableUInt::from_bytes_head(&block[pointer..]);
        let mut read_ptr = pointer + entry_count_len;
        for _ in 0..entry_count {
            let (VariableUInt(name_len), name_len_len) =
                VariableUInt::from_bytes_head(&block[read_ptr..]);
            let name_str = unsafe {
                core::str::from_utf8_unchecked(
                    &block[read_ptr + name_len_len..read_ptr + name_len_len + name_len as usize],
                )
            };
            let (_, hlen) = AssetEntryHeadingPair::from_bytes_head(
                &block[read_ptr + name_len_len + name_len as usize..],
            );

            callback(name_str);
            read_ptr += name_len_len + name_len as usize + hlen;
        }
    }

    if hash_tree_root_exact {
        // Exact tree only
        assert!(
            hash_tree_block.len() & (16 - 1) == 0,
            "exact root hash tree has extra byte?"
        );
        let entry_count = hash_tree_block.len() / (8 * 2);

        for ptr in 0..entry_count {
            enumerate_exact_block_content(
                exact_match_block,
                ExactHashTreeEntryView(&hash_tree_block[ptr * 8 * 2..]).exact_block_offset() as _,
                &mut callback,
            );
        }

        return;
    }

    fn enumerate_subtree(
        hash_tree_block: &[u8],
        exact_match_block: &[u8],
        callback: &mut impl FnMut(&str),
        tree_pointer: EntryTreePointer,
    ) {
        if tree_pointer.is_exact_tree() {
            // Exact Tree
            let block_view =
                ExactHashTreeBlockView(&hash_tree_block[tree_pointer.pointer_value() as usize..]);
            let entry_count = block_view.entry_count() as usize;

            for ptr in 0..entry_count {
                enumerate_exact_block_content(
                    exact_match_block,
                    block_view.entry(ptr).exact_block_offset() as _,
                    callback,
                );
            }

            return;
        }
        // normal tree

        // TODO: ここのマジックナンバーは後々共通のところにおきたい
        const TARGET_PAGE_BLOCK_SIZE: usize = 8192;
        let entry_count = (TARGET_PAGE_BLOCK_SIZE - 8) / (8 * 3);

        let block_view = HashTreeBlockView(
            &hash_tree_block[tree_pointer.pointer_value() as usize
                ..(tree_pointer.pointer_value() as usize + entry_count * (8 * 3) + 8)],
        );

        for ptr in 0..entry_count {
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

    // TODO: ここのマジックナンバーは後々共通のところにおきたい
    const TARGET_PAGE_BLOCK_SIZE: usize = 8192;
    let entry_count = (TARGET_PAGE_BLOCK_SIZE - head_size - 1 - 4 - 8 - 8) / (8 * 3);

    let block_view = HashTreeBlockView(&hash_tree_block[0..entry_count * (8 / 3) + 8]);

    for ptr in 0..entry_count {
        let e = block_view.entry(ptr);
        enumerate_subtree(
            hash_tree_block,
            exact_match_block,
            &mut callback,
            e.smaller_tree_pointer(),
        );
        enumerate_exact_block_content(
            exact_match_block,
            e.exact_block_offset() as _,
            &mut callback,
        );
    }

    enumerate_subtree(
        hash_tree_block,
        exact_match_block,
        &mut callback,
        block_view.larger_tree_pointer(),
    );
}

fn find_entry(
    head_size: usize,
    name: &str,
    hash_tree_root_exact: bool,
    hash_tree_block: &[u8],
    exact_match_block: &[u8],
) -> Option<AssetEntryHeadingPair> {
    let name_hash = xxhash_rust::xxh3::xxh3_64(name.as_bytes());

    let exact_block_offset = 'hash_tree_finder: {
        if hash_tree_root_exact {
            // Exact tree

            assert!(
                hash_tree_block.len() & (16 - 1) == 0,
                "exact root hash tree has extra byte?"
            );
            let entry_count = hash_tree_block.len() / (8 * 2);
            let (mut top, mut bottom) = (0, entry_count);
            loop {
                let ptr = (top + bottom) / 2;
                let e = ExactHashTreeEntryView(&hash_tree_block[ptr * (8 * 2)..]);

                match name_hash.cmp(&e.name_hash()) {
                    // match
                    core::cmp::Ordering::Equal => break Some(e.exact_block_offset()),
                    core::cmp::Ordering::Less => {
                        // bottom: exclusive
                        bottom = ptr;
                    }
                    core::cmp::Ordering::Greater => {
                        top = ptr + 1;
                    }
                }

                if bottom <= top {
                    break None;
                }
            }
        } else {
            fn find_subtree(
                name_hash: u64,
                hash_tree_block: &[u8],
                tree_pointer: EntryTreePointer,
            ) -> Option<u64> {
                if tree_pointer.is_exact_tree() {
                    // Exact Tree
                    let block_view = ExactHashTreeBlockView(
                        &hash_tree_block[tree_pointer.pointer_value() as usize..],
                    );
                    let entry_count = block_view.entry_count();
                    let (mut top, mut bottom) = (0, entry_count as usize);
                    loop {
                        let ptr = (top + bottom) / 2;
                        let e = block_view.entry(ptr);

                        match name_hash.cmp(&e.name_hash()) {
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
                // normal tree

                // TODO: ここのマジックナンバーは後々共通のところにおきたい
                const TARGET_PAGE_BLOCK_SIZE: usize = 8192;
                let entry_count = (TARGET_PAGE_BLOCK_SIZE - 8) / (8 * 3);

                let block_view = HashTreeBlockView(
                    &hash_tree_block[tree_pointer.pointer_value() as usize
                        ..(tree_pointer.pointer_value() as usize + entry_count * (8 * 3) + 8)],
                );

                // edge check
                let e = block_view.largest_entry();
                match name_hash.cmp(&e.name_hash()) {
                    // exact largest
                    core::cmp::Ordering::Equal => return Some(e.exact_block_offset()),
                    // more greater
                    core::cmp::Ordering::Greater => {
                        return find_subtree(
                            name_hash,
                            hash_tree_block,
                            block_view.larger_tree_pointer(),
                        )
                    }
                    core::cmp::Ordering::Less => (/* nop */),
                }

                let e = block_view.smallest_entry();
                match name_hash.cmp(&e.name_hash()) {
                    // exact smallest
                    core::cmp::Ordering::Equal => return Some(e.exact_block_offset()),
                    // more smaller
                    core::cmp::Ordering::Less => {
                        return find_subtree(name_hash, hash_tree_block, e.smaller_tree_pointer())
                    }
                    core::cmp::Ordering::Greater => (/* nop */),
                }

                // binary search
                let (mut top, mut bottom) = (0, entry_count);
                loop {
                    let ptr = (top + bottom) / 2;
                    let e = block_view.entry(ptr);

                    match name_hash.cmp(&e.name_hash()) {
                        // match
                        core::cmp::Ordering::Equal => break Some(e.exact_block_offset()),
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
                        return find_subtree(name_hash, hash_tree_block, e.smaller_tree_pointer());
                    }
                }
            }

            // TODO: ここのマジックナンバーは後々共通のところにおきたい
            const TARGET_PAGE_BLOCK_SIZE: usize = 8192;
            let entry_count = (TARGET_PAGE_BLOCK_SIZE - head_size - 1 - 4 - 8 - 8) / (8 * 3);

            let block_view = HashTreeBlockView(&hash_tree_block[0..entry_count * (8 / 3) + 8]);

            // edge check
            let e = block_view.largest_entry();
            match name_hash.cmp(&e.name_hash()) {
                // exact largest
                core::cmp::Ordering::Equal => break 'hash_tree_finder Some(e.exact_block_offset()),
                // more greater
                core::cmp::Ordering::Greater => {
                    break 'hash_tree_finder find_subtree(
                        name_hash,
                        hash_tree_block,
                        block_view.larger_tree_pointer(),
                    )
                }
                core::cmp::Ordering::Less => (/* nop */),
            }

            let e = block_view.smallest_entry();
            match name_hash.cmp(&e.name_hash()) {
                // exact smallest
                core::cmp::Ordering::Equal => break 'hash_tree_finder Some(e.exact_block_offset()),
                // more smaller
                core::cmp::Ordering::Less => {
                    break 'hash_tree_finder find_subtree(
                        name_hash,
                        hash_tree_block,
                        e.smaller_tree_pointer(),
                    );
                }
                core::cmp::Ordering::Greater => (/* nop */),
            }

            // binary search
            let (mut top, mut bottom) = (0, entry_count);
            loop {
                let ptr = (top + bottom) / 2;
                let e = block_view.entry(ptr);

                match name_hash.cmp(&e.name_hash()) {
                    // match
                    core::cmp::Ordering::Equal => break Some(e.exact_block_offset()),
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
                    break find_subtree(name_hash, hash_tree_block, e.smaller_tree_pointer());
                }
            }
        }
    }?;

    // linear search conflicting hash bucket
    let (VariableUInt(exact_entry_count), exact_entry_offset) =
        VariableUInt::from_bytes_head(&exact_match_block[exact_block_offset as usize..]);
    let mut read_ptr = exact_block_offset as usize + exact_entry_offset;
    for _ in 0..exact_entry_count {
        let (VariableUInt(name_len), name_len_bytes) =
            VariableUInt::from_bytes_head(&exact_match_block[read_ptr..]);
        let name_bytes = unsafe {
            core::str::from_utf8_unchecked(
                &exact_match_block
                    [read_ptr + name_len_bytes..read_ptr + name_len_bytes + name_len as usize],
            )
        };
        let (h, hlen) = AssetEntryHeadingPair::from_bytes_head(
            &exact_match_block[read_ptr + name_len_bytes + name_len as usize..],
        );

        if name_bytes == name {
            // match!
            return Some(h);
        }

        read_ptr += name_len_bytes + name_len as usize + hlen;
    }

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

    fn list_entry(&self, callback: impl FnMut(&str)) {
        list_entry(
            self.head_size,
            self.content_flags
                .contains(ContentFlags::ROOT_HASH_TREE_EXACT),
            &self.block[self.hash_tree_block_range.clone()],
            &self.block[self.exact_match_block_range.clone()],
            callback,
        )
    }

    fn find_entry(&self, name: &str) -> Option<AssetEntryHeadingPair> {
        find_entry(
            self.head_size,
            name,
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

pub struct FileStreamingArchive {
    pub handle: PlatformNativeFileReader,
    pub entry_mapped_head: core::sync::atomic::AtomicPtr<core::ffi::c_void>,
    pub entry_unmap_data: Option<<PlatformNativeFileReader as NativeFileReader>::MemoryUnmapData>,
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

    fn list_entry(&self, callback: impl FnMut(&str)) {
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

    fn find_entry(&self, name: &str) -> Option<AssetEntryHeadingPair> {
        let entry_ptr = self
            .entry_mapped_head
            .load(core::sync::atomic::Ordering::Acquire);

        find_entry(
            // FileStreamingのときは4+4固定になる（非圧縮でしかこれにならないので）
            4 + 4,
            name,
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
        match self {
            Self::OnMemory(ref mut x) => x.read(buf),
            Self::FileStreaming(ref mut x) => x.read(buf),
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
        let (comp, crc) = ArchiveRead::read_file_header(&mut f)?;
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

    #[inline]
    pub fn list_entry(&self, callback: impl FnMut(&str)) {
        match self {
            Self::OnMemory(ref x) => x.list_entry(callback),
            Self::FileStreaming(ref x) => x.list_entry(callback),
        }
    }

    #[inline]
    pub fn find_entry(&self, name: &str) -> Option<AssetEntryHeadingPair> {
        match self {
            Self::OnMemory(ref x) => x.find_entry(name),
            Self::FileStreaming(ref x) => x.find_entry(name),
        }
    }

    #[inline]
    pub fn read_bin<'a>(&'a self, heading: AssetEntryHeadingPair) -> ArchiveBinReader<'a> {
        match self {
            Self::OnMemory(ref x) => ArchiveBinReader::OnMemory(x.read_bin(heading)),
            Self::FileStreaming(ref x) => ArchiveBinReader::FileStreaming(x.read_bin(heading)),
        }
    }
}

pub struct ArchiveRead {
    entries: HashMap<String, AssetEntryHeadingPair>,
    content: EitherArchiveReader,
    content_baseptr: u64,
}
impl ArchiveRead {
    pub fn from_file<P: AsRef<Path>>(path: P, check_integrity: bool) -> ArchiveReadResult<Self> {
        let mut fi = PlatformNativeFileReader::open(&path)?;
        let (comp, crc) = Self::read_file_header(&mut fi)?;
        let mut body = WhereArchive::FromIO(fi);
        if check_integrity {
            let input_crc = crc32::checksum_ieee(&body.on_memory()?[..]);
            if input_crc != crc {
                return Err(ArchiveReadError::IntegrityCheckFailed);
            }
        }

        match comp {
            CompressionMethod::Lz4(_) => {
                let mut compressed = Vec::new();
                EitherArchiveReader::new(body).read_to_end(&mut compressed)?;
                body = lz4_compression::prelude::decompress(&compressed)
                    .map(WhereArchive::OnMemory)?;
            }
            CompressionMethod::Zlib(ub) => {
                let mut sink = Vec::with_capacity(ub as _);
                let reader = EitherArchiveReader::new(body);
                let mut decoder = zlib::Decoder::new(reader);
                decoder.read_to_end(&mut sink)?;
                body = WhereArchive::OnMemory(sink);
            }
            CompressionMethod::Zstd11(ub) => {
                let mut sink = Vec::with_capacity(ub as _);
                let mut decoder = zstd::Decoder::new(EitherArchiveReader::new(body))?;
                decoder.read_to_end(&mut sink)?;
                body = WhereArchive::OnMemory(sink);
            }
            CompressionMethod::None => (/* Nothing to do */),
        }

        unimplemented!("needs fix");

        // let mut areader = EitherArchiveReader::new(body);
        // let entries = Self::read_asset_entries(&mut areader)?;
        // let content_baseptr = areader.seek(SeekFrom::Current(0))?;

        // Ok(ArchiveRead {
        //     entries,
        //     content: areader,
        //     content_baseptr,
        // })
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

    fn read_asset_entries(
        reader: &mut (impl BufRead + ?Sized),
    ) -> IOResult<HashMap<String, AssetEntryHeadingPair>> {
        let VariableUInt(count) = VariableUInt::read(reader)?;
        if count <= 0 {
            return Ok(HashMap::new());
        }
        let mut elements = HashMap::with_capacity(count as _);
        for _ in 0..count {
            let heading = AssetEntryHeadingPair::read(reader)?;
            let PascalString(id_ref) = PascalString::read(reader)?;
            elements.insert(id_ref, heading);
        }
        return Ok(elements);
    }

    pub fn read_bin(&mut self, path: &str) -> IOResult<Option<Vec<u8>>> {
        if let Some(entry_pair) = self.find(path) {
            unimplemented!("needs fix");
            // self.content.seek(SeekFrom::Start(entry_pair.byte_offset))?;
            let mut sink = Vec::with_capacity(entry_pair.byte_length as _);
            unsafe {
                sink.set_len(entry_pair.byte_length as _);
            }

            self.content.read_exact(&mut sink).map(move |_| Some(sink))
        } else {
            Ok(None)
        }
    }
    pub fn entry_names(&self) -> impl Iterator<Item = &str> {
        self.entries.keys().map(|k| k.as_str())
    }
    pub fn find<'s>(&'s self, path: &str) -> Option<AssetEntryInfo> {
        self.entries.get(path).map(|x| AssetEntryInfo {
            byte_length: x.byte_length,
            byte_offset: self.content_baseptr + x.relative_offset,
        })
    }

    pub fn into_inner_reader(self) -> EitherArchiveReader {
        self.content
    }
}

#[derive(Debug)]
pub struct AssetEntryInfo {
    pub byte_length: u64,
    pub byte_offset: u64,
}
