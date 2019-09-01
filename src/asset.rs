
use std::io::{Result as IOResult, BufReader, Error as IOError, ErrorKind, Cursor};
use std::io::prelude::{Read, Seek};
use super::PixelFormat;

pub trait PlatformAssetLoader {
    type Asset: Read + Seek + 'static;
    type StreamingAsset: Read + 'static;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>;
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset>;
}
pub trait LogicalAssetData: Sized {
    const EXT: &'static str;
}
pub trait FromAsset: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, Self::Error>;
    
    fn from_archive(reader: &mut archive::ArchiveRead, path: &str) -> Result<Self, Self::Error> {
        let bin = reader.read_bin(path)?;
        match bin {
            None => Err(IOError::new(ErrorKind::NotFound, "No Entry in primary asset package").into()),
            Some(b) => Self::from_asset(Cursor::new(b))
        }
    }
}
pub trait FromStreamingAsset: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset<Asset: Read + 'static>(asset: Asset) -> Result<Self, Self::Error>;
}
use vertex_processing_pack::*;
impl LogicalAssetData for PvpContainer { const EXT: &'static str = "pvp"; }
impl FromAsset for PvpContainer {
    type Error = IOError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> IOResult<Self> {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container)
    }
}
