use std::{
    collections::HashMap,
    fs::File,
    io::{BufRead, BufReader, Cursor, Error as IOError, Read, Result as IOResult, Seek, SeekFrom},
    path::Path,
};

use crate::{
    AssetEntryHeadingPair, CompressionMethod, EitherArchiveReader, EitherArchiveReaderAsync,
    WhereArchive, WhereArchiveAsync,
};
use crc::crc32;
use libflate::deflate as zlib;
use peridot_serialization_utils::{PascalString, VariableUInt};

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
                IOError::other(format!("Lz4DecompressError: {:?}", e))
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
            let input_crc = crc32::checksum_ieee(&body.on_memory().await?);
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
        if count == 0 {
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
        async_std::io::ReadExt::read_exact(&mut self.content, unsafe {
            core::mem::transmute(sink.spare_capacity_mut())
        })
        .await?;
        unsafe {
            sink.set_len(sink.capacity());
        }

        Ok(Some(sink))
    }
}
impl ArchiveReadAsync {
    pub fn entry_names(&self) -> impl Iterator<Item = &str> {
        self.entries.keys().map(|k| k.as_str())
    }

    pub fn find(&self, path: &str) -> Option<AssetEntryInfo> {
        self.entries.get(path).map(|x| AssetEntryInfo {
            byte_length: x.byte_length,
            byte_offset: self.content_baseptr + x.relative_offset,
        })
    }

    pub fn into_inner_reader(self) -> EitherArchiveReaderAsync {
        self.content
    }
}

pub struct ArchiveRead {
    entries: HashMap<String, AssetEntryHeadingPair>,
    content: EitherArchiveReader,
    content_baseptr: u64,
}
impl ArchiveRead {
    pub fn from_file<P: AsRef<Path>>(path: P, check_integrity: bool) -> ArchiveReadResult<Self> {
        let mut fi = File::open(path).map(BufReader::new)?;
        let (comp, crc) = Self::read_file_header(&mut fi)?;
        let mut body = WhereArchive::FromIO(fi);
        if check_integrity {
            let input_crc = crc32::checksum_ieee(&body.on_memory()?);
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
        let mut areader = EitherArchiveReader::new(body);
        let entries = Self::read_asset_entries(&mut areader)?;
        let content_baseptr = areader.stream_position()?;

        Ok(ArchiveRead {
            entries,
            content: areader,
            content_baseptr,
        })
    }

    fn read_file_header(
        reader: &mut (impl Read + ?Sized),
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
        reader
            .read_exact(&mut crc32_bytes)
            .map(move |_| (comp, u32::from_le_bytes(crc32_bytes)))
            .map_err(From::from)
    }

    fn read_asset_entries(
        reader: &mut (impl BufRead + ?Sized),
    ) -> IOResult<HashMap<String, AssetEntryHeadingPair>> {
        let VariableUInt(count) = VariableUInt::read(reader)?;
        if count == 0 {
            return Ok(HashMap::new());
        }
        let mut elements = HashMap::with_capacity(count as _);
        for _ in 0..count {
            let heading = AssetEntryHeadingPair::read(reader)?;
            let PascalString(id_ref) = PascalString::read(reader)?;
            elements.insert(id_ref, heading);
        }

        Ok(elements)
    }

    pub fn read_bin(&mut self, path: &str) -> IOResult<Option<Vec<u8>>> {
        if let Some(entry_pair) = self.find(path) {
            self.content.seek(SeekFrom::Start(entry_pair.byte_offset))?;
            let mut sink = Vec::with_capacity(entry_pair.byte_length as _);
            self.content
                .read_exact(unsafe { core::mem::transmute(sink.spare_capacity_mut()) })?;
            unsafe {
                sink.set_len(sink.capacity());
            }

            Ok(Some(sink))
        } else {
            Ok(None)
        }
    }
    pub fn entry_names(&self) -> impl Iterator<Item = &str> {
        self.entries.keys().map(|k| k.as_str())
    }
    pub fn find(&self, path: &str) -> Option<AssetEntryInfo> {
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
