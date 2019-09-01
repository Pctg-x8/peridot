//! Asset Interface

use std::io::Error as IOError;
use std::io::prelude::{Read, Seek};
use peridot_math as math;
use bedrock as br;

/// Asset Data Description in Filesystem.
pub trait LogicalAssetData: Sized
{
    /// Extension
    const EXT: &'static str;
}

/// Construct Asset Object from Readers.
pub trait FromAsset: LogicalAssetData
{
    /// Reading Error
    type Error: From<IOError>;
    /// Construct Asset from Reader
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, Self::Error>;
}
/// Construct Streaming Asset Object with Readers.
pub trait FromStreamingAsset: LogicalAssetData
{
    /// Reading Error
    type Error: From<IOError>;
    /// Construct Streaming Asset with Reader.
    fn from_asset<Asset: Read + 'static>(asset: Asset) -> Result<Self, Self::Error>;
}

/// Image Format
#[repr(i32)] #[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PixelFormat
{
    RGB24 = br::vk::VK_FORMAT_R8G8B8_UNORM,
    RGBA32 = br::vk::VK_FORMAT_R8G8B8A8_UNORM,
    BGR24 = br::vk::VK_FORMAT_B8G8R8_UNORM,
    BGRA32 = br::vk::VK_FORMAT_B8G8R8A8_UNORM
}
impl PixelFormat
{
    /// Bits per pixel for each format enums
    pub fn bpp(self) -> usize
    {
        match self
        {
            Self::RGBA32 | Self::BGRA32 => 32,
            Self::RGB24 | Self::BGR24 => 24
        }
    }
}
/// Describing Image Pixel Data
pub struct PixelData 
{
    pub pixels: Vec<u8>, pub size: math::Vector2<u32>,
    pub format: PixelFormat, pub stride: usize
}
impl PixelData
{
    pub fn u8_pixels(&self) -> &[u8] { &self.pixels }
}

/// Low Dynamic Range(8bit colors) image asset
pub trait LDRImageAsset
{
    fn into_pixel_data_info(self) -> PixelData;
}
