//! Peridot Archive

mod entry;
mod entry_tree;
pub mod native_io;
mod utils;

use bitflags::bitflags;

pub use self::entry::{AssetEntryHeadingPair, AssetNameRef};
mod write;
pub use self::write::ArchiveWrite;
mod read;
pub use self::read::{Archive, ArchiveAsync, ArchiveReadError};

#[repr(C)]
pub struct LinearPaired2u64(u64, u64);

bitflags! {
    pub struct ContentFlags : u8 {
        const EMPTY = 0;
        const ROOT_HASH_TREE_EXACT = 0x01;
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
