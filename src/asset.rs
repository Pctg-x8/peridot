use super::PixelFormat;
use std::io::prelude::{Read, Seek};
use std::io::{Cursor, Error as IOError, ErrorKind, Result as IOResult, SeekFrom};
use std::pin::Pin;
use std::task::{Context, Poll};

pub trait InputStream: Read {
    fn skip(&mut self, amount: u64) -> IOResult<u64>;
}
impl<T> InputStream for T
where
    T: Seek + Read,
{
    fn skip(&mut self, amount: u64) -> IOResult<u64> {
        self.seek(SeekFrom::Current(amount as _))
    }
}

pub trait PlatformAssetLoader {
    type Asset: Read + Seek + 'static;
    type StreamingAsset: InputStream + 'static;

    fn poll_get_binary(
        self: Pin<&mut Self>,
        cx: &mut Context,
        path: &str,
        ext: &str,
    ) -> Poll<IOResult<Vec<u8>>>;
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset>;
}
pub trait LogicalAssetData: Sized {
    const EXT: &'static str;
}
pub trait Asset: LogicalAssetData {
    type Error: From<IOError>;
    fn from_binary(content: Vec<u8>) -> Result<Self, Self::Error>;
}
/// TODO: make Streaming Decoding/Reading interface
pub trait StreamingAsset: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset<Asset: InputStream + 'static>(asset: Asset) -> Result<Self, Self::Error>;
}

// Shader Blob //
use bedrock as br;

/// An shader blob representation as Asset
pub struct SpirvShaderBlob(Vec<u8>);
impl SpirvShaderBlob {
    /// Instantiates the Shader Binary as a VkShaderModule
    pub fn instantiate(&self, dev: &br::Device) -> br::Result<br::ShaderModule> {
        br::ShaderModule::from_memory(dev, &self.0)
    }
}
impl LogicalAssetData for SpirvShaderBlob {
    const EXT: &'static str = "spv";
}
impl FromAsset for SpirvShaderBlob {
    type Error = IOError;
    fn from_binary(content: Vec<u8>) -> Result<Self, Self::Error> {
        Ok(SpirvShaderBlob(content))
    }
}
