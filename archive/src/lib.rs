//! Peridot Archive

use peridot_serialization_utils::*;
use std::io::prelude::{Write, Read, BufRead};
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::io::{SeekFrom, Seek, BufReader};
use std::fs::File;
use std::mem::transmute;
use std::collections::HashMap;
use libflate::deflate as zlib; use lz4_compression; use zstd;
use crc::crc32;
use std::path::Path;

#[repr(C)] pub struct LinearPaired2u64(u64, u64);
#[derive(Debug)]
#[repr(C)] pub struct AssetEntryHeadingPair { pub byte_length: u64, pub relative_offset: u64 }
impl AssetEntryHeadingPair
{
    fn write<W: Write>(&self, writer: &mut W) -> IOResult<usize>
    {
        writer.write(unsafe { &transmute::<_, &[u8; 8 * 2]>(self)[..] }).map(|_| 16)
    }
    fn read<R: Read>(reader: &mut R) -> IOResult<Self>
    {
        let mut sink = AssetEntryHeadingPair { byte_length: 0, relative_offset: 0 };
        reader.read_exact(unsafe { &mut transmute::<_, &mut [u8; 8 * 2]>(&mut sink)[..] }).map(|_| sink)
    }
}

/// 展開後のサイズが値として入る。圧縮指定時には無視されるので適当な値を指定する
#[derive(Debug)]
pub enum CompressionMethod
{
    None, Zlib(u64), Lz4(u64), Zstd11(u64)
}

use std::io::Cursor;
pub struct ArchiveWrite(CompressionMethod, HashMap<String, AssetEntryHeadingPair>, Vec<u8>);
impl ArchiveWrite
{
    pub fn new(comp: CompressionMethod) -> Self
    {
        ArchiveWrite(comp, HashMap::new(), Vec::new())
    }
    pub fn add(&mut self, name: String, content: Vec<u8>) -> bool
    {
        if self.1.contains_key(&name) { return false; }
        let relative_offset = self.2.len() as u64;
        self.2.extend(content);
        self.1.insert(name, AssetEntryHeadingPair
        {
            relative_offset, byte_length: self.2.len() as u64 - relative_offset
        });
        
        true
    }
    /// return -> written bytes(raw)
    fn write_asset_entries<W: Write>(&self, writer: &mut W) -> IOResult<usize>
    {
        let mut written_bytes = VariableUInt(self.1.len() as _).write(writer)?;
        for (n, h) in &self.1
        {
            written_bytes += h.write(writer).and_then(|w1| PascalStr(n).write(writer).map(move |w2| w1 + w2))?;
        }
        return Ok(written_bytes);
    }
    pub fn write<W: Write>(&self, writer: &mut W) -> IOResult<()>
    {
        match self.0
        {
            CompressionMethod::None =>
            {
                let mut body = Cursor::new(Vec::new());
                self.write_asset_entries(&mut body)?; body.write_all(&self.2[..])?;

                Self::write_common(writer, b"par ", None, &body.into_inner()[..])
            },
            CompressionMethod::Zlib(_) =>
            {
                let mut body = zlib::Encoder::new(Cursor::new(Vec::new()));
                let uncompressed_bytes = self.write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))? as u64;

                Self::write_common(writer, b"pard", Some(uncompressed_bytes),
                    &body.finish().into_result()?.into_inner()[..])
            }
            CompressionMethod::Lz4(_) =>
            {
                let mut body = Cursor::new(Vec::new());
                let uncompressed_bytes = self.write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))? as u64;
                let body = lz4_compression::prelude::compress(&body.into_inner());

                Self::write_common(writer, b"parz", Some(uncompressed_bytes), &body[..])
            },
            CompressionMethod::Zstd11(_) =>
            {
                let mut body = zstd::Encoder::new(Cursor::new(Vec::new()), 11)?;
                let uncompressed_bytes = self.write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))? as u64;
                
                Self::write_common(writer, b"par1", Some(uncompressed_bytes), &body.finish()?.into_inner()[..])
            }
        }
    }
    fn write_common<W: Write>(writer: &mut W, signature: &[u8], uncompressed_bytes: Option<u64>, body: &[u8])
        -> IOResult<()>
    {
        let checksum = crc32::checksum_ieee(body);
        writer.write_all(signature)?;
        if let Some(ub) = uncompressed_bytes { writer.write_all(&ub.to_le_bytes())?; }
        writer.write_all(&checksum.to_le_bytes())?;
        writer.write_all(body)
    }
}

pub enum WhereArchive { OnMemory(Vec<u8>), FromIO(BufReader<File>) }
impl WhereArchive
{
    pub fn on_memory(&mut self) -> IOResult<&[u8]>
    {
        let replace_buf = if let WhereArchive::FromIO(ref mut r) = self
        {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf)?; Some(buf)
        }
        else { None };
        if let Some(b) = replace_buf { *self = WhereArchive::OnMemory(b); }
        match self
        {
            WhereArchive::OnMemory(ref b) => Ok(b),
            _ => unreachable!()
        }
    }
}
pub enum EitherArchiveReader { OnMemory(Cursor<Vec<u8>>), FromIO(BufReader<File>) }
impl EitherArchiveReader
{
    fn new(a: WhereArchive) -> Self
    {
        match a
        {
            WhereArchive::FromIO(r) => EitherArchiveReader::FromIO(r),
            WhereArchive::OnMemory(b) => EitherArchiveReader::OnMemory(Cursor::new(b))
        }
    }
    pub fn unwrap(self) -> WhereArchive
    {
        match self
        {
            EitherArchiveReader::FromIO(r) => WhereArchive::FromIO(r),
            EitherArchiveReader::OnMemory(c) => WhereArchive::OnMemory(c.into_inner())
        }
    }
}
impl Read for EitherArchiveReader
{
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize>
    {
        match self
        {
            EitherArchiveReader::FromIO(ref mut r) => r.read(buf),
            EitherArchiveReader::OnMemory(ref mut c) => c.read(buf)
        }
    }
}
impl BufRead for EitherArchiveReader
{
    fn fill_buf(&mut self) -> IOResult<&[u8]>
    {
        match self
        {
            EitherArchiveReader::FromIO(ref mut r) => r.fill_buf(),
            EitherArchiveReader::OnMemory(ref mut c) => c.fill_buf()
        }
    }
    fn consume(&mut self, amt: usize)
    {
        match self
        {
            EitherArchiveReader::FromIO(ref mut r) => r.consume(amt),
            EitherArchiveReader::OnMemory(ref mut c) => c.consume(amt)
        }
    }
}
impl Seek for EitherArchiveReader
{
    fn seek(&mut self, pos: SeekFrom) -> IOResult<u64>
    {
        match self
        {
            EitherArchiveReader::FromIO(ref mut r) => r.seek(pos),
            EitherArchiveReader::OnMemory(ref mut c) => c.seek(pos)
        }
    }
}

#[non_exhaustive]
#[derive(Debug)]
pub enum ArchiveReadError
{
    IO(IOError),
    IntegrityCheckFailed,
    SignatureMismatch,
    Lz4DecompressError(lz4_compression::decompress::Error)
}
impl From<IOError> for ArchiveReadError { fn from(e: IOError) -> Self { Self::IO(e) } }
impl From<lz4_compression::decompress::Error> for ArchiveReadError
{
    fn from(e: lz4_compression::decompress::Error) -> Self { Self::Lz4DecompressError(e) }
}
pub type ArchiveReadResult<T> = Result<T, ArchiveReadError>;
impl From<ArchiveReadError> for IOError {
    fn from(e: ArchiveReadError) -> Self {
        match e {
            ArchiveReadError::IO(e) => e,
            ArchiveReadError::IntegrityCheckFailed => IOError::new(ErrorKind::Other, "Archive Integrity check failed"),
            ArchiveReadError::SignatureMismatch => IOError::new(ErrorKind::Other, "Archive Signature Mismatch"),
            ArchiveReadError::Lz4DecompressError(e) => IOError::new(
                ErrorKind::Other, format!("Lz4DecompressError: {:?}", e)
            )
        }
    }
}

pub struct ArchiveRead
{
    entries: HashMap<String, AssetEntryHeadingPair>,
    content: EitherArchiveReader,
    content_baseptr: u64
}
impl ArchiveRead
{
    pub fn from_file<P: AsRef<Path>>(path: P, check_integrity: bool) -> ArchiveReadResult<Self>
    {
        let mut fi = File::open(path).map(BufReader::new)?;
        let (comp, crc) = Self::read_file_header(&mut fi)?;
        let mut body = WhereArchive::FromIO(fi);
        if check_integrity
        {
            let input_crc = crc32::checksum_ieee(&body.on_memory()?[..]);
            if input_crc != crc { return Err(ArchiveReadError::IntegrityCheckFailed); }
        }

        match comp
        {
            CompressionMethod::Lz4(_) =>
            {
                let mut compressed = Vec::new();
                EitherArchiveReader::new(body).read_to_end(&mut compressed)?;
                body = lz4_compression::prelude::decompress(&compressed).map(WhereArchive::OnMemory)?;
            },
            CompressionMethod::Zlib(ub) =>
            {
                let mut sink = Vec::with_capacity(ub as _);
                let reader = EitherArchiveReader::new(body);
                let mut decoder = zlib::Decoder::new(reader);
                decoder.read_to_end(&mut sink)?;
                body = WhereArchive::OnMemory(sink);
            },
            CompressionMethod::Zstd11(ub) =>
            {
                let mut sink = Vec::with_capacity(ub as _);
                let mut decoder = zstd::Decoder::new(EitherArchiveReader::new(body))?;
                decoder.read_to_end(&mut sink)?;
                body = WhereArchive::OnMemory(sink);
            },
            CompressionMethod::None => (/* Nothing to do */)
        }
        let mut areader = EitherArchiveReader::new(body);
        let entries = Self::read_asset_entries(&mut areader)?;
        let content_baseptr = areader.seek(SeekFrom::Current(0))?;

        Ok(ArchiveRead { entries, content: areader, content_baseptr })
    }
    fn read_file_header<R: BufRead>(reader: &mut R) -> ArchiveReadResult<(CompressionMethod, u32)>
    {
        let mut signature = [0u8; 4];
        reader.read_exact(&mut signature[..]).map(drop)?;
        let mut sink_64_bits = [0u8; 8];
        let comp = match &signature
        {
            b"par " => CompressionMethod::None,
            b"pard" => reader.read_exact(&mut sink_64_bits)
                .map(|_| CompressionMethod::Zlib(u64::from_le_bytes(sink_64_bits)))?,
            b"parz" => reader.read_exact(&mut sink_64_bits)
                .map(|_| CompressionMethod::Lz4(u64::from_le_bytes(sink_64_bits)))?,
            b"par1" => reader.read_exact(&mut sink_64_bits)
                .map(|_| CompressionMethod::Zstd11(u64::from_le_bytes(sink_64_bits)))?,
            _ => return Err(ArchiveReadError::SignatureMismatch)
        };
        let mut crc32_bytes = [0u8; 4];
        reader.read_exact(&mut crc32_bytes).map(move |_| (comp, u32::from_le_bytes(crc32_bytes))).map_err(From::from)
    }
    fn read_asset_entries<R: BufRead>(reader: &mut R) -> IOResult<HashMap<String, AssetEntryHeadingPair>>
    {
        let VariableUInt(count) = VariableUInt::read(reader)?;
        if count <= 0 { return Ok(HashMap::new()); }
        let mut elements = HashMap::with_capacity(count as _);
        for _ in 0 .. count
        {
            let heading = AssetEntryHeadingPair::read(reader)?;
            let PascalString(id_ref) = PascalString::read(reader)?;
            elements.insert(id_ref, heading);
        }
        return Ok(elements);
    }

    pub fn read_bin(&mut self, path: &str) -> IOResult<Option<Vec<u8>>>
    {
        if let Some(entry_pair) = self.find(path)
        {
            self.content.seek(SeekFrom::Start(entry_pair.byte_offset))?;
            let mut sink = Vec::with_capacity(entry_pair.byte_length as _);
            unsafe { sink.set_len(entry_pair.byte_length as _); }

            self.content.read_exact(&mut sink).map(move |_| Some(sink))
        }
        else { Ok(None) }
    }
    pub fn entry_names(&self) -> impl Iterator<Item = &str>
    {
        self.entries.keys().map(|k| k.as_str())
    }
    pub fn find<'s>(&'s self, path: &str) -> Option<AssetEntryInfo>
    {
        self.entries.get(path).map(|x| AssetEntryInfo
        {
            byte_length: x.byte_length, byte_offset: self.content_baseptr + x.relative_offset
        })
    }

    pub fn into_inner_reader(self) -> EitherArchiveReader { self.content }
}
#[derive(Debug)]
pub struct AssetEntryInfo { pub byte_length: u64, pub byte_offset: u64 }
