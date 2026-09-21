//! glTF binary format

use std::io::Read;

pub const MAGIC: u32 = 0x46546c67;
pub fn try_verify_magic(r: &mut (impl Read + ?Sized)) -> bool {
    read_u32(r).is_ok_and(|x| x == MAGIC)
}

#[derive(Debug)]
pub struct Header {
    pub version: u32,
    pub length: u32,
}
impl Header {
    pub fn read(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let [version, length] = readva_u32(r)?;

        Ok(Self { version, length })
    }
}

#[derive(Debug)]
pub struct ChunkHeader {
    pub length: u32,
    pub r#type: ChunkType,
}
impl ChunkHeader {
    pub fn read(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let [length, type_v] = readva_u32(r)?;

        Ok(Self {
            length,
            r#type: ChunkType::from_binary(type_v).expect("invalid chunk type"),
        })
    }

    pub const fn padding_tail_length(&self) -> u32 {
        (4 - (self.length & 3)) & 3
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChunkType {
    Json,
    Bin,
}
impl ChunkType {
    const fn from_binary(v: u32) -> Result<Self, u32> {
        match v {
            0x4e4f534a => Ok(Self::Json),
            0x004e4942 => Ok(Self::Bin),
            _ => Err(v),
        }
    }
}

#[inline(always)]
pub fn read_u32(r: &mut (impl Read + ?Sized)) -> std::io::Result<u32> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}

#[inline(always)]
fn readva_u32<const N: usize>(r: &mut (impl Read + ?Sized)) -> std::io::Result<[u32; N]> {
    let mut bufs = [[0u8; 4]; N];
    let mut iovs0 = std::array::from_fn::<_, N, _>(|i| {
        std::io::IoSliceMut::new(unsafe { &mut *bufs.as_mut_ptr().add(i) })
    });
    let mut iovs = &mut iovs0[..];

    std::io::IoSliceMut::advance_slices(&mut iovs, 0);
    while !iovs.is_empty() {
        let b = r.read_vectored(iovs)?;
        std::io::IoSliceMut::advance_slices(&mut iovs, b);
    }

    Ok(std::array::from_fn::<_, N, _>(|b| {
        u32::from_le_bytes(bufs[b])
    }))
}
