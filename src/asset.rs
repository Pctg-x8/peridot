
use std::io::{Result as IOResult, BufReader, Error as IOError, ErrorKind, Cursor, SeekFrom};
use std::io::prelude::{Read, Seek};

pub trait InputStream: Read
{
    fn skip(&mut self, amount: u64) -> IOResult<u64>;
}
impl<T> InputStream for T where T: Seek + Read
{
    fn skip(&mut self, amount: u64) -> IOResult<u64> { self.seek(SeekFrom::Current(amount as _)) }
}

pub trait PlatformAssetLoader
{
    type Asset: Read + Seek + 'static;
    type StreamingAsset: InputStream + 'static;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>;
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset>;
}
pub trait LogicalAssetData: Sized
{
    const EXT: &'static str;
}
pub trait FromAsset: LogicalAssetData
{
    type Error: From<IOError>;
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, Self::Error>;
    
    fn from_archive(reader: &mut peridot_archive::ArchiveRead, path: &str) -> Result<Self, Self::Error>
    {
        match reader.read_bin(path)?
        {
            None => Err(IOError::new(ErrorKind::NotFound, "No Entry in primary asset package").into()),
            Some(b) => Self::from_asset(Cursor::new(b))
        }
    }
}
pub trait FromStreamingAsset: LogicalAssetData
{
    type Error: From<IOError>;
    fn from_asset<Asset: InputStream + 'static>(asset: Asset) -> Result<Self, Self::Error>;
}

// Shader Blob //
use bedrock as br;

/// An shader blob representation as Asset
pub struct SpirvShaderBlob(Vec<u8>);
impl SpirvShaderBlob
{
    /// Instantiates the Shader Binary as a VkShaderModule
    pub fn instantiate(&self, dev: &br::Device) -> br::Result<br::ShaderModule>
    {
        br::ShaderModule::from_memory(dev, &self.0)
    }
}
impl LogicalAssetData for SpirvShaderBlob
{
    const EXT: &'static str = "spv";
}
impl FromAsset for SpirvShaderBlob
{
    type Error = IOError;
    fn from_asset<Asset: Read + Seek + 'static>(mut asset: Asset) -> Result<Self, IOError>
    {
        let mut buf = Vec::new();
        asset.read_to_end(&mut buf).map(move |_| SpirvShaderBlob(buf))
    }
}
