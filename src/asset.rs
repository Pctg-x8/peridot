
use std::io::{Result as IOResult, BufReader};
use std::io::prelude::{Read, Seek};

pub trait PlatformAssetLoader {
    type Asset: Read + Seek;
    type StreamingAsset: Read;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>;
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset>;
}
pub trait LogicalAssetData: Sized {
    const EXT: &'static str;
}
pub trait FromAsset: LogicalAssetData {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> IOResult<Self>;
    
    fn from_archive(reader: archive::ArchiveRead, path: &str) -> IOResult<Self> {
        reader.read_bin(path).and_then(Self::from_asset)
    }
}
pub trait FromStreamingAsset: LogicalAssetData {
    fn from_asset<Asset: Read>(asset: Asset) -> IOResult<Self>;
}
use vertex_processing_pack::*;
impl LogicalAssetData for PvpContainer { const EXT: &'static str = "pvp"; }
impl FromAsset for PvpContainer {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> IOResult<Self> {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container)
    }
}
