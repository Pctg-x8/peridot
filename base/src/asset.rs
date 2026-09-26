use std::io::prelude::{Read, Seek};
use std::io::{Error as IOError, Result as IOResult, SeekFrom};

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
    type AssetBlob<'a>: AssetBlob + 'a
    where
        Self: 'a;
    type AssetBlobAsync<'a>: AssetBlobAsync + 'a
    where
        Self: 'a;
    type StreamingAsset<'a>: InputStream + Sync + Send + 'a
    where
        Self: 'a;

    fn get<'a>(&'a self, path: &str, ext: &str) -> IOResult<Self::AssetBlob<'a>>;
    fn get_async<'a>(
        &'a self,
        path: &str,
        ext: &str,
    ) -> impl core::future::Future<Output = IOResult<Self::AssetBlobAsync<'a>>>;
    fn get_streaming<'a>(&'a self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset<'a>>;
}
pub trait LogicalAssetData: Sized {
    const EXT: &'static str;
}

pub trait AssetBlob: peridot_native_io::RandomReadBlob + peridot_native_io::BlobMetadata {}
impl AssetBlob for peridot_native_io::PlatformNativeFileReader {}
impl<'a, T: peridot_native_io::RandomReadBlob + peridot_native_io::MemoryMapBlob + 'a> AssetBlob
    for peridot_archive::ArchiveBinReader<'a, T>
{
}

pub trait FromAssetBlob: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error>;
}

pub trait FromStreamingAsset<'a>: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset<Asset: InputStream + Sync + Send + 'a>(asset: Asset)
        -> Result<Self, Self::Error>;
}

pub trait AssetBlobAsync:
    peridot_native_io::RandomReadBlobAsync + peridot_native_io::BlobMetadataAsync
{
}
impl AssetBlobAsync for peridot_native_io::PlatformNativeFileReaderAsync {}
impl<'a, T: peridot_native_io::RandomReadBlobAsync + peridot_native_io::MemoryMapBlob + 'a>
    AssetBlobAsync for peridot_archive::ArchiveBinReaderAsync<'a, T>
{
}

pub trait FromAssetBlobAsync: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset_blob_async<'a, Blob: AssetBlobAsync + 'a>(
        blob: Blob,
    ) -> impl core::future::Future<Output = Result<Self, Self::Error>>;
}

// Shader Blob //
use bedrock as br;

/// An shader blob representation as Asset
pub struct SpirvShaderBlob(Vec<u32>);
impl SpirvShaderBlob {
    /// Instantiates the Shader Binary as a VkShaderModule
    #[inline]
    pub fn instantiate<Device: br::Device>(
        &self,
        dev: Device,
    ) -> br::Result<br::ShaderModuleObject<Device>> {
        br::ShaderModuleObject::new(dev, &br::ShaderModuleCreateInfo::new(&self.0))
    }
}
impl LogicalAssetData for SpirvShaderBlob {
    const EXT: &'static str = "spv";
}
impl FromAssetBlob for SpirvShaderBlob {
    type Error = IOError;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, IOError> {
        let len = blob.byte_length()?;
        let mut buf = Vec::with_capacity((len as usize + 3) >> 2);
        blob.read_exact(0, unsafe {
            core::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut _, buf.capacity() << 2)
        })?;
        unsafe {
            buf.set_len(buf.capacity());
        }

        Ok(SpirvShaderBlob(buf))
    }
}
impl FromAssetBlobAsync for SpirvShaderBlob {
    type Error = IOError;

    fn from_asset_blob_async<'a, Blob: AssetBlobAsync + 'a>(
        blob: Blob,
    ) -> impl core::future::Future<Output = Result<Self, Self::Error>> {
        async move {
            let len = blob.byte_length_async().await?;
            let mut buf = Vec::with_capacity((len as usize + 3) >> 2);
            blob.read_exact_async(0, unsafe {
                core::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut _, buf.capacity() << 2)
            })
            .await?;
            unsafe {
                buf.set_len(buf.capacity());
            }

            Ok(Self(buf))
        }
    }
}
