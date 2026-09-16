use std::{fs::File, io::Read, path::PathBuf};

use clap::Parser;

pub mod gltf;

#[derive(Parser)]
struct App {
    input: PathBuf,
}

fn main() {
    let args = App::parse();

    let mut reader = File::open(&args.input).expect("failed to open input file");
    let magic = read_u32(&mut reader).expect("failed to read magic");
    assert_eq!(magic, 0x46546c67, "magic mismatch");
    let version = read_u32(&mut reader).expect("failed to read version");
    let length = read_u32(&mut reader).expect("failed to read length");
    println!("glb detected: version={version} length={length}");

    let chunk0_length = read_u32(&mut reader).expect("failed to read chunk length");
    let chunk0_type =
        ChunkType::from_binary(read_u32(&mut reader).expect("failed to read chunk type"))
            .expect("invalid chunk type");
    assert_eq!(chunk0_type, ChunkType::Json, "chunk 0 must be json");
    println!("chunk 0: length={chunk0_length} type={chunk0_type:?}");
    let mut content = Vec::<u8>::with_capacity(chunk0_length as usize);
    reader
        .read_exact(unsafe {
            core::mem::transmute(&mut content.spare_capacity_mut()[..chunk0_length as usize])
        })
        .expect("failed to read chunk content");
    unsafe {
        content.set_len(chunk0_length as usize);
    }
    let content = unsafe { str::from_utf8_unchecked(&content) };
    let parsed = serde_json::from_str::<gltf::GLTF>(content).expect("invalid gltf json");
    println!("{parsed:#?}");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChunkType {
    Json,
    Bin,
}
impl ChunkType {
    pub const fn from_binary(v: u32) -> Result<Self, u32> {
        match v {
            0x4e4f534a => Ok(Self::Json),
            0x004e4942 => Ok(Self::Bin),
            _ => Err(v),
        }
    }
}

#[inline(always)]
fn read_u32(r: &mut (impl Read + ?Sized)) -> std::io::Result<u32> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}
