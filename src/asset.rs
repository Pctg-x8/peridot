
use std::io::{Result as IOResult, BufReader, Error as IOError, ErrorKind, Cursor, SeekFrom};
use std::io::prelude::{Read, Seek};

pub trait InputStream: Read {
    fn skip(&mut self, amount: u64) -> IOResult<u64>;
}
impl<T> InputStream for T where T: Seek + Read {
    fn skip(&mut self, amount: u64) -> IOResult<u64> { self.seek(SeekFrom::Current(amount as _)) }
}

pub trait PlatformAssetLoader {
    type Asset: Read + Seek;
    type StreamingAsset: InputStream + 'static;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>;
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset>;
}
pub trait LogicalAssetData: Sized {
    const EXT: &'static str;
}
pub trait FromAsset: LogicalAssetData {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> IOResult<Self>;
    
    fn from_archive(reader: &mut archive::ArchiveRead, path: &str) -> IOResult<Self> {
        let bin = reader.read_bin(path)?;
        match bin {
            None => Err(IOError::new(ErrorKind::NotFound, "No Entry in primary asset package")),
            Some(b) => Self::from_asset(Cursor::new(b))
        }
    }
}
pub trait FromStreamingAsset: LogicalAssetData {
    fn from_asset<Asset: InputStream + 'static>(asset: Asset) -> IOResult<Self>;
}
use vertex_processing_pack::*;
impl LogicalAssetData for PvpContainer { const EXT: &'static str = "pvp"; }
impl FromAsset for PvpContainer {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> IOResult<Self> {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container)
    }
}
