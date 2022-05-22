//! Peridot Archive

use async_std::fs::File;
use async_std::io::prelude::{BufRead, Read, ReadExt, SeekExt, Write, WriteExt};
use async_std::io::{BufReader, Cursor, Seek, SeekFrom};
use async_std::path::Path;
use async_std::task::{Context, Poll};
use crc::crc32;
use libflate::deflate as zlib;
use lz4_compression;
use peridot_serialization_utils::*;
use std::collections::HashMap;
use std::io::{Error as IOError, ErrorKind, Result as IOResult};
use std::mem::{replace, transmute};
use std::pin::Pin;
use zstd;

#[repr(C)]
pub struct LinearPaired2u64(u64, u64);
#[derive(Debug)]
#[repr(C)]
pub struct AssetEntryHeadingPair {
    pub byte_length: u64,
    pub relative_offset: u64,
}
impl AssetEntryHeadingPair {
    async fn write<W: Write + Unpin>(&self, writer: &mut W) -> IOResult<usize> {
        writer
            .write(unsafe { &transmute::<_, &[u8; 8 * 2]>(self)[..] })
            .await
            .map(|_| 16)
    }
    async fn read<R: Read + Unpin>(reader: &mut R) -> IOResult<Self> {
        let mut sink = AssetEntryHeadingPair {
            byte_length: 0,
            relative_offset: 0,
        };
        reader
            .read_exact(unsafe { &mut transmute::<_, &mut [u8; 8 * 2]>(&mut sink)[..] })
            .await
            .map(|_| sink)
    }
}

/// 展開後のサイズが値として入る。圧縮指定時には無視されるので適当な値を指定する
#[derive(Debug)]
pub enum CompressionMethod {
    None,
    Zlib(u64),
    Lz4(u64),
    Zstd11(u64),
}

pub struct ArchiveWrite(
    CompressionMethod,
    HashMap<String, AssetEntryHeadingPair>,
    Vec<u8>,
);
impl ArchiveWrite {
    pub fn new(comp: CompressionMethod) -> Self {
        ArchiveWrite(comp, HashMap::new(), Vec::new())
    }
    pub fn add(&mut self, name: String, content: Vec<u8>) -> bool {
        if self.1.contains_key(&name) {
            return false;
        }
        let relative_offset = self.2.len() as u64;
        self.2.extend(content);
        self.1.insert(
            name,
            AssetEntryHeadingPair {
                relative_offset,
                byte_length: self.2.len() as u64 - relative_offset,
            },
        );

        true
    }
    /// return -> written bytes(raw)
    async fn write_asset_entries<W: Write + Unpin>(&self, writer: &mut W) -> IOResult<usize> {
        let mut written_bytes = VariableUInt(self.1.len() as _).write(writer).await?;
        for (n, h) in &self.1 {
            let wlen = h.write(writer).await?;
            let swlen = PascalStr(n).write(writer).await?;
            written_bytes += wlen + swlen;
        }

        Ok(written_bytes)
    }
    pub async fn write<W: Write + Unpin>(&self, writer: &mut W) -> IOResult<()> {
        let mut body = Cursor::new(Vec::new());
        let entry_bytes = self.write_asset_entries(&mut body).await?;
        body.write_all(&self.2[..]).await?;
        let uncompressed_bytes = entry_bytes + self.2.len();

        match self.0 {
            CompressionMethod::None => {
                Self::write_common(writer, b"par ", None, &body.into_inner()[..]).await
            }
            CompressionMethod::Zlib(_) => {
                async {
                    let encoded = async_std::task::spawn_blocking(|| {
                        let mut encoder = zlib::Encoder::new(std::io::Cursor::new(Vec::new()));
                        std::io::Write::write_all(&mut encoder, &body.into_inner())?;
                        encoder.finish().into_result().map(|r| r.into_inner())
                    })
                    .await?;

                    Self::write_common(
                        writer,
                        b"pard",
                        Some(uncompressed_bytes as u64),
                        &encoded[..],
                    )
                    .await
                }
                .await
            }
            CompressionMethod::Lz4(_) => {
                async {
                    let encoded = lz4_compression::prelude::compress(&body.into_inner());

                    Self::write_common(
                        writer,
                        b"parz",
                        Some(uncompressed_bytes as u64),
                        &encoded[..],
                    )
                    .await
                }
                .await
            }
            CompressionMethod::Zstd11(_) => {
                async {
                    let encoded = async_std::task::spawn_blocking(|| {
                        zstd::stream::encode_all(std::io::Cursor::new(body.into_inner()), 11)
                    })
                    .await?;

                    Self::write_common(
                        writer,
                        b"par1",
                        Some(uncompressed_bytes as u64),
                        &encoded[..],
                    )
                    .await
                }
                .await
            }
        }
    }
    async fn write_common<W: Write + Unpin>(
        writer: &mut W,
        signature: &[u8],
        uncompressed_bytes: Option<u64>,
        body: &[u8],
    ) -> IOResult<()> {
        let checksum = crc32::checksum_ieee(body);
        writer.write_all(signature).await?;
        if let Some(ub) = uncompressed_bytes {
            writer.write_all(&ub.to_le_bytes()).await?;
        }
        writer.write_all(&checksum.to_le_bytes()).await?;
        writer.write_all(body).await
    }
}

enum WhereArchive {
    OnMemory(Vec<u8>),
    FromIO(BufReader<File>),
}
impl WhereArchive {
    async fn on_memory(&mut self) -> IOResult<&[u8]> {
        let replace_buf = if let WhereArchive::FromIO(ref mut r) = self {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf).await?;
            Some(buf)
        } else {
            None
        };
        if let Some(b) = replace_buf {
            replace(self, WhereArchive::OnMemory(b));
        }

        Ok(self.ref_onmemory_buffer())
    }
    fn ref_onmemory_buffer(&self) -> &[u8] {
        if let WhereArchive::OnMemory(ref b) = self {
            b
        } else {
            unreachable!();
        }
    }
}
enum EitherArchiveReader {
    OnMemory(Cursor<Vec<u8>>),
    FromIO(BufReader<File>),
}
impl<'r> From<WhereArchive> for EitherArchiveReader {
    fn from(a: WhereArchive) -> Self {
        match a {
            WhereArchive::FromIO(r) => Self::FromIO(r),
            WhereArchive::OnMemory(b) => Self::OnMemory(Cursor::new(b)),
        }
    }
}
impl Read for EitherArchiveReader {
    fn poll_read(self: Pin<&mut Self>, ctx: &mut Context, buf: &mut [u8]) -> Poll<IOResult<usize>> {
        match self.get_mut() {
            Self::FromIO(ref mut r) => Pin::new(r).poll_read(ctx, buf),
            Self::OnMemory(ref mut c) => Pin::new(c).poll_read(ctx, buf),
        }
    }
}
impl BufRead for EitherArchiveReader {
    fn poll_fill_buf(self: Pin<&mut Self>, ctx: &mut Context) -> Poll<IOResult<&[u8]>> {
        match self.get_mut() {
            Self::FromIO(ref mut r) => Pin::new(r).poll_fill_buf(ctx),
            Self::OnMemory(ref mut c) => Pin::new(c).poll_fill_buf(ctx),
        }
    }
    fn consume(self: Pin<&mut Self>, amt: usize) {
        match self.get_mut() {
            Self::FromIO(ref mut r) => Pin::new(r).consume(amt),
            Self::OnMemory(ref mut c) => Pin::new(c).consume(amt),
        }
    }
}
impl Seek for EitherArchiveReader {
    fn poll_seek(self: Pin<&mut Self>, ctx: &mut Context, pos: SeekFrom) -> Poll<IOResult<u64>> {
        match self.get_mut() {
            Self::FromIO(ref mut r) => Pin::new(r).poll_seek(ctx, pos),
            Self::OnMemory(ref mut c) => Pin::new(c).poll_seek(ctx, pos),
        }
    }
}

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

pub struct ArchiveRead {
    entries: HashMap<String, AssetEntryHeadingPair>,
    content: EitherArchiveReader,
    content_baseptr: u64,
}
impl ArchiveRead {
    pub async fn from_file<P: AsRef<Path>>(
        path: P,
        check_integrity: bool,
    ) -> ArchiveReadResult<Self> {
        let mut fi = File::open(path).await.map(BufReader::new)?;
        let (comp, crc) = Self::read_file_header(&mut fi).await?;
        let mut body = WhereArchive::FromIO(fi);
        if check_integrity {
            let input_crc = crc32::checksum_ieee(&body.on_memory().await?[..]);
            if input_crc != crc {
                return Err(ArchiveReadError::IntegrityCheckFailed);
            }
        }

        let body = match comp {
            CompressionMethod::Lz4(_) => {
                body.on_memory().await?;
                let decompressed = async_std::task::spawn_blocking(move || {
                    let compressed = body.ref_onmemory_buffer();
                    lz4_compression::prelude::decompress(&compressed)
                })
                .await?;

                WhereArchive::OnMemory(decompressed)
            }
            CompressionMethod::Zlib(ub) => {
                body.on_memory().await?;

                async_std::task::spawn_blocking(move || {
                    let compressed = body.ref_onmemory_buffer();
                    let mut decoder = zlib::Decoder::new(&compressed[..]);
                    let mut sink = Vec::with_capacity(ub as _);
                    std::io::prelude::Read::read_to_end(&mut decoder, &mut sink)?;

                    IOResult::Ok(WhereArchive::OnMemory(sink))
                })
                .await?
            }
            CompressionMethod::Zstd11(ub) => {
                body.on_memory().await?;

                async_std::task::spawn_blocking(move || {
                    let compressed = body.ref_onmemory_buffer();
                    let mut decoder = zstd::Decoder::new(&compressed[..])?;
                    let mut sink = Vec::with_capacity(ub as _);
                    std::io::prelude::Read::read_to_end(&mut decoder, &mut sink)?;

                    IOResult::Ok(WhereArchive::OnMemory(sink))
                })
                .await?
            }
            CompressionMethod::None => body,
        };
        let mut areader = EitherArchiveReader::from(body);
        let entries = Self::read_asset_entries(&mut areader).await?;
        let content_baseptr = areader.seek(SeekFrom::Current(0)).await?;

        Ok(ArchiveRead {
            entries,
            content: areader,
            content_baseptr,
        })
    }
    async fn read_file_header<R: BufRead + Unpin>(
        reader: &mut R,
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
        reader
            .read_exact(&mut crc32_bytes)
            .await
            .map(move |_| (comp, u32::from_le_bytes(crc32_bytes)))
            .map_err(From::from)
    }
    async fn read_asset_entries<R: BufRead + Unpin>(
        reader: &mut R,
    ) -> IOResult<HashMap<String, AssetEntryHeadingPair>> {
        let VariableUInt(count) = VariableUInt::read(reader).await?;
        if count <= 0 {
            return Ok(HashMap::new());
        }
        let mut elements = HashMap::with_capacity(count as _);
        for _ in 0..count {
            let heading = AssetEntryHeadingPair::read(reader).await?;
            let PascalString(id_ref) = PascalString::read(reader).await?;
            elements.insert(id_ref, heading);
        }
        return Ok(elements);
    }

    pub async fn read_bin(&mut self, path: &str) -> IOResult<Option<Vec<u8>>> {
        if let Some(entry_pair) = self.find(path) {
            self.content
                .seek(SeekFrom::Start(entry_pair.byte_offset))
                .await?;
            let mut sink = Vec::with_capacity(entry_pair.byte_length as _);
            unsafe {
                sink.set_len(entry_pair.byte_length as _);
            }

            self.content
                .read_exact(&mut sink)
                .await
                .map(move |_| Some(sink))
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
}
#[derive(Debug)]
pub struct AssetEntryInfo {
    pub byte_length: u64,
    pub byte_offset: u64,
}
