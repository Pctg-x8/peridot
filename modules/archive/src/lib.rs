//! Peridot Archive

use async_std::io::Cursor;
use crc::crc32;
use libflate::deflate as zlib;
use lz4_compression;
use peridot_serialization_utils::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::{BufRead, Read, Write};
use std::io::{BufReader, IoSliceMut, Seek, SeekFrom};
use std::io::{Error as IOError, ErrorKind, IoSlice, Result as IOResult};
use std::mem::transmute;
use std::path::Path;
use zstd;

#[cfg(feature = "async-rt-async-std")]
async fn write_all_vectored_async(
    w: &mut (impl async_std::io::Write + ?Sized),
    mut buffers: &mut [IoSlice],
) -> IOResult<()> {
    // reduce empty ioslices
    IoSlice::advance_slices(&mut buffers, 0);

    while !buffers.is_empty() {
        match async_std::io::WriteExt::write_vectored(writer, buffers).await {
            Ok(0) => {
                return Err(IOError::new(
                    ErrorKind::WriteZero,
                    "Failed to write whole buffer",
                ))
            }
            Ok(n) => IoSlice::advance_slices(&mut buffers, n),
            Err(e) if e.kind() == ErrorKind::Interrupted => (),
            Err(e) => return Err(e),
        }
    }

    Ok(())
}

#[repr(C)]
pub struct LinearPaired2u64(u64, u64);
#[derive(Debug)]
pub struct AssetEntryHeadingPair {
    pub byte_length: u64,
    pub relative_offset: u64,
}
impl AssetEntryHeadingPair {
    fn write(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        writer
            .write_all_vectored(&mut [
                IoSlice::new(&self.byte_length.to_le_bytes()),
                IoSlice::new(&self.relative_offset.to_le_bytes()),
            ])
            .map(|_| 16)
    }

    #[cfg(feature = "async-rt-async-std")]
    async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + ?Sized),
    ) -> IOResult<usize> {
        write_all_vectored_async(
            writer,
            &mut [
                IoSlice::new(&self.byte_length.to_le_bytes()),
                IoSlice::new(&self.relative_offset.to_le_bytes()),
            ],
        )
        .await
        .map(|_| 16)
    }

    fn read(reader: &mut (impl Read + ?Sized)) -> IOResult<Self> {
        let (mut byte_length_sink, mut relative_offset_sink) = ([0u8; 8], [0u8; 8]);

        // read_all_vectored
        let mut buffers = &mut [
            IoSliceMut::new(&mut byte_length_sink),
            IoSliceMut::new(&mut relative_offset_sink),
        ];
        while !buffers.is_empty() {
            match reader.read_vectored(buffers) {
                Ok(0) => {
                    return Err(IOError::new(
                        ErrorKind::UnexpectedEof,
                        "Failed to fill whole buffer",
                    ))
                }
                Ok(n) => IoSliceMut::advance_slices(&mut buffers, n),
                Err(e) if e.kind() == ErrorKind::Interrupted => (),
                Err(e) => return Err(e),
            }
        }

        Ok(Self {
            byte_length: u64::from_le_bytes(byte_length_sink),
            relative_offset: u64::from_le_bytes(relative_offset_sink),
        })
    }

    #[cfg(feature = "async-rt-async-std")]
    async fn read_async(reader: &mut (impl async_std::io::Read + ?Sized)) -> IOResult<Self> {
        let (mut byte_length_sink, mut relative_offset_sink) = ([0u8; 8], [0u8; 8]);

        // read_all_vectored
        let mut buffers = &mut [
            IoSliceMut::new(&mut byte_length_sink),
            IoSliceMut::new(&mut relative_offset_sink),
        ];
        while !buffers.is_empty() {
            match async_std::io::ReadExt::read_vectored(reader, buffers).await {
                Ok(0) => {
                    return Err(IOError::new(
                        ErrorKind::UnexpectedEof,
                        "Failed to fill whole buffer",
                    ))
                }
                Ok(n) => IoSliceMut::advance_slices(&mut buffers, n),
                Err(e) if e.kind() == ErrorKind::Interrupted => (),
                Err(e) => return Err(e),
            }
        }

        Ok(Self {
            byte_length: u64::from_le_bytes(byte_length_sink),
            relative_offset: u64::from_le_bytes(relative_offset_sink),
        })
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

pub struct ArchiveWrite {
    compression_method: CompressionMethod,
    entries: HashMap<String, AssetEntryHeadingPair>,
    data_bytes: Vec<u8>,
}
impl ArchiveWrite {
    pub fn new(comp: CompressionMethod) -> Self {
        Self {
            compression_method: comp,
            entries: HashMap::new(),
            data_bytes: Vec::new(),
        }
    }

    /// エントリを追加する 成功したらtrue
    pub fn add(&mut self, name: String, content: Vec<u8>) -> bool {
        if self.entries.contains_key(&name) {
            // すでにある
            return false;
        }

        let relative_offset = self.data_bytes.len() as u64;
        self.data_bytes.extend(content);
        self.entries.insert(
            name,
            AssetEntryHeadingPair {
                relative_offset,
                byte_length: self.data_bytes.len() as u64 - relative_offset,
            },
        );

        true
    }

    /// return -> written bytes(raw)
    fn write_asset_entries(&self, writer: &mut (impl Write + ?Sized)) -> IOResult<usize> {
        let mut written_bytes = VariableUInt(self.entries.len() as _).write(writer)?;
        for (n, h) in &self.entries {
            written_bytes += h
                .write(writer)
                .and_then(|w1| PascalStr(n).write(writer).map(move |w2| w1 + w2))?;
        }

        Ok(written_bytes)
    }

    pub fn write<W: Write>(&self, writer: &mut W) -> IOResult<()> {
        match self.0 {
            CompressionMethod::None => {
                let mut body = Vec::new();
                self.write_asset_entries(&mut body)?;
                body.write_all(&self.2[..])?;

                Self::write_common(writer, b"par ", None, &body.into_inner()[..])
            }
            CompressionMethod::Zlib(_) => {
                let mut body = zlib::Encoder::new(Vec::new());
                let uncompressed_bytes = self
                    .write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))?
                    as u64;

                Self::write_common(
                    writer,
                    b"pard",
                    Some(uncompressed_bytes),
                    &body.finish().into_result()?.into_inner()[..],
                )
            }
            CompressionMethod::Lz4(_) => {
                let mut body = Vec::new();
                let uncompressed_bytes = self
                    .write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))?
                    as u64;
                let body = lz4_compression::prelude::compress(&body.into_inner());

                Self::write_common(writer, b"parz", Some(uncompressed_bytes), &body[..])
            }
            CompressionMethod::Zstd11(_) => {
                let mut body = zstd::Encoder::new(Vec::new(), 11)?;
                let uncompressed_bytes = self
                    .write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))?
                    as u64;

                Self::write_common(
                    writer,
                    b"par1",
                    Some(uncompressed_bytes),
                    &body.finish()?.into_inner()[..],
                )
            }
        }
    }

    #[cfg(feature = "async-rt-async-std")]
    pub async fn write_async(
        &self,
        writer: &mut (impl async_std::io::Write + ?Sized),
    ) -> IOResult<()> {
        match self.compression_method {
            CompressionMethod::None => {
                let mut body = Vec::new();
                self.write_asset_entries(&mut body)?;
                body.write_all(&self.data_bytes[..])?;

                Self::write_common_async(writer, b"par ", None, &body.into_inner()[..]).await
            }
            CompressionMethod::Zlib(_) => {
                let mut body = zlib::Encoder::new(Vec::new());
                let uncompressed_bytes = self
                    .write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))?
                    as u64;

                Self::write_common_async(
                    writer,
                    b"pard",
                    Some(uncompressed_bytes),
                    &body.finish().into_result()?.into_inner()[..],
                )
                .await
            }
            CompressionMethod::Lz4(_) => {
                let mut body = Vec::new();
                let uncompressed_bytes = self
                    .write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))?
                    as u64;
                let body = lz4_compression::prelude::compress(&body.into_inner());

                Self::write_common_async(writer, b"parz", Some(uncompressed_bytes), &body[..]).await
            }
            CompressionMethod::Zstd11(_) => {
                let mut body = zstd::Encoder::new(Vec::new(), 11)?;
                let uncompressed_bytes = self
                    .write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))?
                    as u64;

                Self::write_common_async(
                    writer,
                    b"par1",
                    Some(uncompressed_bytes),
                    &body.finish()?.into_inner()[..],
                )
                .await
            }
        }
    }

    fn write_common(
        writer: &mut (impl Write + ?Sized),
        signature: &[u8],
        uncompressed_bytes: Option<u64>,
        body: &[u8],
    ) -> IOResult<()> {
        let checksum = crc32::checksum_ieee(body);
        writer.write_all(signature)?;
        if let Some(ub) = uncompressed_bytes {
            writer.write_all(&ub.to_le_bytes())?;
        }
        writer.write_all(&checksum.to_le_bytes())?;
        writer.write_all(body)
    }

    #[cfg(feature = "async-rt-async-std")]
    async fn write_common_async(
        writer: &mut (impl async_std::io::Write + ?Sized),
        signature: &[u8],
        uncompressed_byte_length: Option<u64>,
        body: &[u8],
    ) -> IOResult<()> {
        let checksum = crc32::checksum_ieee(body);

        let mut write_buffers = Vec::with_capacity(4);
        write_buffers.push(IoSlice::new(signature));
        if let Some(x) = uncompressed_byte_length {
            write_buffers.push(IoSlice::new(&x.to_le_bytes()));
        }
        write_buffers.extend([IoSlice::new(&checksum.to_le_bytes()), IoSlice::new(body)]);

        write_all_vectored_async(writer, &mut write_buffers).await?;
        Ok(())
    }
}

pub enum WhereArchive {
    OnMemory(Vec<u8>),
    FromIO(BufReader<File>),
}
impl WhereArchive {
    pub fn on_memory(&mut self) -> IOResult<&[u8]> {
        let replace_buf = if let WhereArchive::FromIO(ref mut r) = self {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf)?;
            Some(buf)
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

pub enum WhereArchiveAsync {
    OnMemory(Vec<u8>),
    #[cfg(feature = "async-rt-async-std")]
    FromIO(async_std::io::BufReader<async_std::fs::File>),
}
impl WhereArchiveAsync {
    pub async fn on_memory(&mut self) -> IOResult<&[u8]> {
        let replace_buf = match self {
            #[cfg(feature = "async-rt-async-std")]
            Self::FromIO(ref mut r) => {
                let mut buf = Vec::new();
                async_std::io::ReadExt::read_to_end(r, &mut buf).await?;
                Some(buf)
            }
            Self::OnMemory(_) => None,
        };

        if let Some(b) = replace_buf {
            *self = Self::OnMemory(b);
        }

        match self {
            Self::OnMemory(ref b) => Ok(b),
            _ => unreachable!(),
        }
    }
}

pub enum EitherArchiveReader {
    OnMemory(Cursor<Vec<u8>>),
    FromIO(BufReader<File>),
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
impl BufRead for EitherArchiveReader {
    #[inline]
    fn fill_buf(&mut self) -> IOResult<&[u8]> {
        match self {
            Self::FromIO(ref mut r) => r.fill_buf(),
            Self::OnMemory(ref mut c) => c.fill_buf(),
        }
    }

    #[inline]
    fn consume(&mut self, amt: usize) {
        match self {
            Self::FromIO(ref mut r) => r.consume(amt),
            Self::OnMemory(ref mut c) => c.consume(amt),
        }
    }
}
impl Seek for EitherArchiveReader {
    #[inline]
    fn seek(&mut self, pos: SeekFrom) -> IOResult<u64> {
        match self {
            Self::FromIO(ref mut r) => r.seek(pos),
            Self::OnMemory(ref mut c) => c.seek(pos),
        }
    }
}

pub enum EitherArchiveReaderAsync {
    OnMemory(Cursor<Vec<u8>>),
    #[cfg(feature = "async-rt-async-std")]
    IO(async_std::io::BufReader<async_std::fs::File>),
}
impl From<WhereArchiveAsync> for EitherArchiveReaderAsync {
    #[inline]
    fn from(value: WhereArchiveAsync) -> Self {
        match value {
            WhereArchiveAsync::OnMemory(v) => Self::OnMemory(Cursor::new(v)),
            #[cfg(feature = "async-rt-async-std")]
            WhereArchiveAsync::FromIO(v) => Self::IO(v),
        }
    }
}
impl From<EitherArchiveReaderAsync> for WhereArchiveAsync {
    #[inline]
    fn from(value: EitherArchiveReaderAsync) -> Self {
        match value {
            EitherArchiveReaderAsync::OnMemory(v) => Self::OnMemory(v.into_inner()),
            #[cfg(feature = "async-rt-async-std")]
            EitherArchiveReaderAsync::IO(v) => Self::FromIO(v),
        }
    }
}
#[cfg(feature = "async-rt-async-std")]
impl async_std::io::Read for EitherArchiveReaderAsync {
    #[inline]
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut [u8],
    ) -> std::task::Poll<IOResult<usize>> {
        match self.get_mut() {
            Self::IO(ref mut r) => async_std::io::Read::poll_read(std::pin::Pin::new(r), cx, buf),
            Self::OnMemory(ref mut c) => std::task::Poll::Ready(c.read(buf)),
        }
    }

    #[inline]
    fn poll_read_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &mut [IoSliceMut<'_>],
    ) -> std::task::Poll<IOResult<usize>> {
        match self.get_mut() {
            Self::IO(ref mut r) => {
                async_std::io::Read::poll_read_vectored(std::pin::Pin::new(r), cx, bufs)
            }
            Self::OnMemory(ref mut c) => std::task::Poll::Ready(c.read_vectored(bufs)),
        }
    }
}
#[cfg(feature = "async-rt-async-std")]
impl async_std::io::BufRead for EitherArchiveReaderAsync {
    #[inline]
    fn poll_fill_buf(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<IOResult<&[u8]>> {
        match self.get_mut() {
            Self::IO(ref mut r) => async_std::io::BufRead::poll_fill_buf(std::pin::Pin::new(r), cx),
            Self::OnMemory(ref mut c) => std::task::Poll::Ready(c.fill_buf()),
        }
    }

    #[inline]
    fn consume(self: std::pin::Pin<&mut Self>, amt: usize) {
        match self.get_mut() {
            Self::IO(ref mut r) => async_std::io::BufRead::consume(std::pin::Pin::new(r), amt),
            Self::OnMemory(ref mut c) => c.consume(amt),
        }
    }
}
#[cfg(feature = "async-rt-async-std")]
impl async_std::io::Seek for EitherArchiveReaderAsync {
    #[inline]
    fn poll_seek(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        pos: SeekFrom,
    ) -> std::task::Poll<IOResult<u64>> {
        match self.get_mut() {
            Self::IO(ref mut r) => async_std::io::Seek::poll_seek(std::pin::Pin::new(r), cx, pos),
            Self::OnMemory(ref mut c) => std::task::Poll::Ready(c.seek(pos)),
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

/// archive reader 非同期版
pub struct ArchiveReadAsync {
    entries: HashMap<String, AssetEntryHeadingPair>,
    content: EitherArchiveReaderAsync,
    content_baseptr: u64,
}
#[cfg(feature = "async-rt-async-std")]
impl ArchiveReadAsync {
    pub async fn from_file(
        path: impl AsRef<Path>,
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
                    &body.on_memory().await?,
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
                zlib::Decoder::new(compressed).read_to_end(&mut sink)?;
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
                zstd::Decoder::new(compressed).read_to_end(&mut sink)?;
                body = WhereArchiveAsync::OnMemory(sink);
            }
            CompressionMethod::None => (/* Nothing to do */),
        }
        let mut areader = EitherArchiveReaderAsync::from(body);
        let entries = Self::read_asset_entries(&mut areader).await?;
        let content_basepr =
            async_std::io::SeekExt::seek(&mut areader, SeekFrom::Current(0)).await?;

        Ok(Self {
            entries,
            content: areader,
            content_baseptr,
        })
    }

    async fn read_file_header(
        reader: &mut (impl async_std::io::Read + ?Sized),
    ) -> ArchiveReadResult<(CompressionMethod, u32)> {
        let mut signature = [0u8; 4];
        async_std::io::ReadExt::read_exact(reader, &mut signature).await?;
        let mut sink_64_bits = [0u8; 8];
        let comp = match signature {
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
        reader: &mut (impl async_std::io::BufRead + ?Sized),
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
        let mut areader = EitherArchiveReader::new(body);
        let entries = Self::read_asset_entries(&mut areader)?;
        let content_baseptr = areader.seek(SeekFrom::Current(0))?;

        Ok(ArchiveRead {
            entries,
            content: areader,
            content_baseptr,
        })
    }

    fn read_file_header<R: BufRead>(reader: &mut R) -> ArchiveReadResult<(CompressionMethod, u32)> {
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

    fn read_asset_entries<R: BufRead>(
        reader: &mut R,
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
            self.content.seek(SeekFrom::Start(entry_pair.byte_offset))?;
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
