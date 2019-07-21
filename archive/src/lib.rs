//! Peridot Archive

use peridot_serialization_utils::*;
use std::io::prelude::{Write, Read, BufRead};
use std::io::{Result as IOResult, Error as IOError, ErrorKind};
use std::io::{SeekFrom, Seek, BufReader};
use std::fs::File;
use std::mem::{transmute, replace};
use std::collections::HashMap;
use libflate::deflate as zlib; use lz4; use zstd;
use crc::crc32;
use std::path::Path;

#[repr(C)] pub struct LinearPaired2u64(u64, u64);
#[derive(Debug)]
#[repr(C)] pub struct AssetEntryHeadingPair { pub byte_length: u64, pub relative_offset: u64 }
impl AssetEntryHeadingPair {
    fn write<W: Write>(&self, writer: &mut W) -> IOResult<usize> {
        writer.write(unsafe { &(*(self as *const Self as *const [u8; 8 * 2]))[..] }).map(|_| 16)
    }
    fn read<R: BufRead>(reader: &mut R) -> IOResult<Self> {
        let mut sink = AssetEntryHeadingPair { byte_length: 0, relative_offset: 0 };
        reader.read_exact(unsafe { &mut (*(&mut sink as *mut Self as *mut [u8; 8 * 2]))[..] }).map(|_| sink)
    }
}
fn read_asset_entries<R: BufRead>(reader: &mut R) -> IOResult<HashMap<String, AssetEntryHeadingPair>> {
    let VariableUInt(count) = VariableUInt::read(reader)?;
    if count == 0 { return Ok(HashMap::new()); }
    let mut elements = HashMap::with_capacity(count as _);
    for _ in 0 .. count {
        let heading = AssetEntryHeadingPair::read(reader)?;
        let PascalString(id_ref) = PascalString::read(reader)?;
        elements.insert(id_ref, heading);
    }
    return Ok(elements);
}

/// 展開後のサイズが値として入る。圧縮指定時には無視されるので適当な値を指定する
#[derive(Debug)]
pub enum CompressionMethod {
    None, Zlib(u64), Lz4(u64), Zstd11(u64)
}
fn read_file_header<R: BufRead>(reader: &mut R) -> IOResult<(CompressionMethod, u32)> {
    let mut signature = [0u8; 4];
    reader.read_exact(&mut signature[..]).map(drop)?;
    let mut sink_64 = 0u64;
    let comp = match &signature {
        b"par " => CompressionMethod::None,
        b"pard" => reader.read_exact(unsafe { &mut transmute::<_, &mut [u8; 8]>(&mut sink_64)[..] })
            .map(|_| CompressionMethod::Zlib(sink_64))?,
        b"parz" => reader.read_exact(unsafe { &mut transmute::<_, &mut [u8; 8]>(&mut sink_64)[..] })
            .map(|_| CompressionMethod::Lz4(sink_64))?,
        b"par1" => reader.read_exact(unsafe { &mut transmute::<_, &mut [u8; 8]>(&mut sink_64)[..] })
            .map(|_| CompressionMethod::Zstd11(sink_64))?,
        _ => return Err(IOError::new(ErrorKind::Other, "Signature Mismatch or Unsupported Compression method"))
    };
    let mut crc32 = 0u32;
    reader.read_exact(unsafe { &mut transmute::<_, &mut [u8; 4]>(&mut crc32)[..] }).map(drop)?;
    return Ok((comp, crc32));
}

use std::io::Cursor;
pub struct ArchiveWrite(CompressionMethod, HashMap<String, AssetEntryHeadingPair>, Vec<u8>);
impl ArchiveWrite {
    pub fn new(comp: CompressionMethod) -> Self {
        ArchiveWrite(comp, HashMap::new(), Vec::new())
    }
    pub fn add(&mut self, name: String, content: Vec<u8>) -> bool {
        if self.1.contains_key(&name) { return false; }
        let relative_offset = self.2.len() as u64;
        self.2.extend(content);
        self.1.insert(name, AssetEntryHeadingPair {
            relative_offset, byte_length: self.2.len() as u64 - relative_offset
        });
        return true;
    }
    /// return -> written bytes(raw)
    fn write_asset_entries<W: Write>(&self, writer: &mut W) -> IOResult<usize> {
        let mut written_bytes = VariableUInt(self.1.len() as _).write(writer)?;
        for (n, h) in &self.1 {
            written_bytes += h.write(writer).and_then(|w1| PascalStr(n).write(writer).map(move |w2| w1 + w2))?;
        }
        return Ok(written_bytes);
    }
    pub fn write<W: Write>(&self, writer: &mut W) -> IOResult<()> {
        match self.0 {
            CompressionMethod::None => {
                let mut body = Cursor::new(Vec::new());
                self.write_asset_entries(&mut body)?; body.write_all(&self.2[..])?;

                Self::write_common(writer, b"par ", None, &body.into_inner()[..])
            },
            CompressionMethod::Zlib(_) => {
                let mut body = zlib::Encoder::new(Cursor::new(Vec::new()));
                let uncompressed_bytes = self.write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))? as u64;

                Self::write_common(writer, b"pard", Some(uncompressed_bytes),
                    &body.finish().into_result()?.into_inner()[..])
            }
            CompressionMethod::Lz4(_) => {
                let mut body = lz4::EncoderBuilder::new().build(Cursor::new(Vec::new()))?;
                let uncompressed_bytes = self.write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))? as u64;
                let (body, r) = body.finish(); r?;

                Self::write_common(writer, b"parz", Some(uncompressed_bytes), &body.into_inner()[..])
            },
            CompressionMethod::Zstd11(_) => {
                let mut body = zstd::Encoder::new(Cursor::new(Vec::new()), 11)?;
                let uncompressed_bytes = self.write_asset_entries(&mut body)
                    .and_then(|wa| body.write_all(&self.2[..]).map(move |_| wa + self.2.len()))? as u64;
                
                Self::write_common(writer, b"par1", Some(uncompressed_bytes), &body.finish()?.into_inner()[..])
            }
        }
    }
    fn write_common<W: Write>(writer: &mut W, signature: &[u8], uncompressed_bytes: Option<u64>, body: &[u8])
            -> IOResult<()> {
        let checksum = crc32::checksum_ieee(body);
        writer.write_all(signature)?;
        if let Some(ub) = uncompressed_bytes { writer.write_all(unsafe { &transmute::<_, &[u8; 8]>(&ub)[..] })?; }
        writer.write_all(unsafe { &transmute::<_, &[u8; 4]>(&checksum)[..] })?;
        writer.write_all(body)
    }
}

pub enum WhereArchive { OnMemory(Vec<u8>), FromIO(BufReader<File>) }
impl WhereArchive {
    pub fn on_memory(&mut self) -> IOResult<&[u8]> {
        let replace_buf = if let WhereArchive::FromIO(ref mut r) = self {
            let mut buf = Vec::new();
            r.read_to_end(&mut buf)?; Some(buf)
        }
        else { None };
        if let Some(b) = replace_buf { replace(self, WhereArchive::OnMemory(b)); }
        match self {
            WhereArchive::OnMemory(ref b) => Ok(b), _ => unreachable!()
        }
    }
}
pub enum EitherArchiveReader { OnMemory(Cursor<Vec<u8>>), FromIO(BufReader<File>) }
impl EitherArchiveReader {
    fn new(a: WhereArchive) -> Self {
        match a {
            WhereArchive::FromIO(r) => EitherArchiveReader::FromIO(r),
            WhereArchive::OnMemory(b) => EitherArchiveReader::OnMemory(Cursor::new(b))
        }
    }
    pub fn unwrap(self) -> WhereArchive {
        match self {
            EitherArchiveReader::FromIO(r) => WhereArchive::FromIO(r),
            EitherArchiveReader::OnMemory(c) => WhereArchive::OnMemory(c.into_inner())
        }
    }
}
impl Read for EitherArchiveReader {
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        match self {
            EitherArchiveReader::FromIO(ref mut r) => r.read(buf),
            EitherArchiveReader::OnMemory(ref mut c) => c.read(buf)
        }
    }
}
impl BufRead for EitherArchiveReader {
    fn fill_buf(&mut self) -> IOResult<&[u8]> {
        match self {
            EitherArchiveReader::FromIO(ref mut r) => r.fill_buf(),
            EitherArchiveReader::OnMemory(ref mut c) => c.fill_buf()
        }
    }
    fn consume(&mut self, amt: usize) {
        match self {
            EitherArchiveReader::FromIO(ref mut r) => r.consume(amt),
            EitherArchiveReader::OnMemory(ref mut c) => c.consume(amt)
        }
    }
}
impl Seek for EitherArchiveReader {
    fn seek(&mut self, pos: SeekFrom) -> IOResult<u64> {
        match self {
            EitherArchiveReader::FromIO(ref mut r) => r.seek(pos),
            EitherArchiveReader::OnMemory(ref mut c) => c.seek(pos)
        }
    }
}

pub struct ArchiveRead {
    entries: HashMap<String, AssetEntryHeadingPair>, content: EitherArchiveReader,
    content_baseptr: u64
}
impl ArchiveRead {
    pub fn from_file<P: AsRef<Path>>(path: P, check_integrity: bool) -> IOResult<Self> {
        let mut fi = File::open(path).map(BufReader::new).expect("file opening error");
        let (comp, crc) = read_file_header(&mut fi).expect("header reading error");
        // println!("Compression Method: {:?}", comp);
        // println!("Checksum: 0x{:08x}", crc);
        let mut body = WhereArchive::FromIO(fi);
        if check_integrity {
            // std::io::stdout().write_all(b"Checking archive integrity...").unwrap();
            // std::io::stdout().flush().unwrap();
            let input_crc = crc32::checksum_ieee(&body.on_memory().expect("reading error")[..]);
            if input_crc != crc {
                panic!("Checking Integrity Failed: Mismatching CRC-32: input=0x{:08x}", input_crc);
            }
            // println!(" ok");
        }
        match comp {
            CompressionMethod::Lz4(ub) => {
                let mut sink = Vec::with_capacity(ub as _);
                let mut decoder = lz4::Decoder::new(EitherArchiveReader::new(body)).expect("initializing lz4 decoder");
                decoder.read_to_end(&mut sink).expect("decoding error");
                body = WhereArchive::OnMemory(sink);
            },
            CompressionMethod::Zlib(ub) => {
                let mut sink = Vec::with_capacity(ub as _);
                let reader = EitherArchiveReader::new(body);
                let mut decoder = zlib::Decoder::new(reader);
                decoder.read_to_end(&mut sink).expect("decoding error");
                body = WhereArchive::OnMemory(sink);
            },
            CompressionMethod::Zstd11(ub) => {
                let mut sink = Vec::with_capacity(ub as _);
                let mut decoder = zstd::Decoder::new(EitherArchiveReader::new(body))
                    .expect("initializing zstd decoder");
                decoder.read_to_end(&mut sink).expect("decoding error");
                body = WhereArchive::OnMemory(sink);
            },
            _ => ()
        }
        let mut areader = EitherArchiveReader::new(body);
        let entries = read_asset_entries(&mut areader).expect("reading error");
        /*for (n, d) in &entries {
            println!("- {}: {} {}", n, d.relative_offset, d.byte_length);
        }*/
        let content_baseptr = areader.seek(SeekFrom::Current(0)).expect("telling ptr");

        return Ok(ArchiveRead {
            entries, content: areader, content_baseptr
        });
    }

    pub fn read_bin(&mut self, path: &str) -> IOResult<Option<Vec<u8>>> {
        if let Some(entry_pair) = self.find(path) {
            self.content.seek(SeekFrom::Start(entry_pair.byte_offset))?;
            let mut sink = Vec::with_capacity(entry_pair.byte_length as _);
            unsafe { sink.set_len(entry_pair.byte_length as _); }
            self.content.read_exact(&mut sink)?;
            return Ok(Some(sink));
        }
        else { return Ok(None); }
    }
    pub fn entry_names(&self) -> ArchiveEntryIterator {
        ArchiveEntryIterator(self.entries.keys())
    }
    pub fn find<'s>(&'s self, path: &str) -> Option<AssetEntryInfo> {
        self.entries.get(path).map(|x| AssetEntryInfo {
            byte_length: x.byte_length, byte_offset: self.content_baseptr + x.relative_offset
        })
    }

    pub fn into_inner_reader(self) -> EitherArchiveReader { self.content }
}
use std::collections::hash_map::Keys;
pub struct ArchiveEntryIterator<'a>(Keys<'a, String, AssetEntryHeadingPair>);
impl<'a> Iterator for ArchiveEntryIterator<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<&'a str> { self.0.next().map(|a| a.as_str()) }
}
#[derive(Debug)]
pub struct AssetEntryInfo { pub byte_length: u64, pub byte_offset: u64 }
