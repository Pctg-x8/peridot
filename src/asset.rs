
use std::io::{Result as IOResult, Error as IOError};
use std::io::prelude::{Read, Seek};
use super::PixelFormat;
use std::task::{Poll, Context};
use std::pin::Pin;

pub trait PlatformAssetLoader
{
    type Asset: Read + Seek + 'static;
    type StreamingAsset: Read + 'static;

    fn poll_get_binary(self: Pin<&mut Self>, cx: &mut Context, path: &str, ext: &str) -> Poll<IOResult<Vec<u8>>>;
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset>;
}
pub trait LogicalAssetData: Sized
{
    const EXT: &'static str;
}
pub trait Asset: LogicalAssetData
{
    type Error: From<IOError>;
    fn from_binary(content: Vec<u8>) -> Result<Self, Self::Error>;
}
/// TODO: make Streaming Decoding/Reading interface
pub trait StreamingAsset: LogicalAssetData
{
    type Error: From<IOError>;
    fn from_asset<Asset: Read + 'static>(asset: Asset) -> Result<Self, Self::Error>;
}

use image::{ImageDecoder, ImageResult, ImageError};
use image::hdr::{HdrDecoder, HDRMetadata, RGBE8Pixel};
pub struct DecodedPixelData
{
    pub pixels: Vec<u8>, pub size: math::Vector2<u32>,
    pub color: image::ColorType, pub stride: usize
}
impl DecodedPixelData
{
    pub fn new<'d, D>(decoder: D) -> ImageResult<Self> where D: ImageDecoder<'d>
    {
        let color = decoder.color_type();
        let (w, h) = decoder.dimensions();
        let stride = decoder.scanline_bytes();
        let mut pixels = Vec::with_capacity(decoder.total_bytes() as _);
        unsafe { pixels.set_len(decoder.total_bytes() as _); }
        decoder.read_image(&mut pixels)?;
        
        Ok(DecodedPixelData { pixels, size: math::Vector2(w as _, h as _), color, stride: stride as _ })
    }
    pub fn format(&self) -> PixelFormat
    {
        match self.color
        {
            image::ColorType::Rgb8 => PixelFormat::RGB24,
            image::ColorType::Rgba8 => PixelFormat::RGBA32,
            image::ColorType::Bgr8 => PixelFormat::BGR24,
            image::ColorType::Bgra8 => PixelFormat::BGRA32,
            _ => panic!("unsupported color type: {:?}", self.color)
        }
    }

    pub fn u8_pixels(&self) -> &[u8] { &self.pixels }
}
pub struct PNG(pub DecodedPixelData);
pub struct TGA(pub DecodedPixelData);
pub struct TIFF(pub DecodedPixelData);
pub struct WebP(pub DecodedPixelData);
pub struct BMP(pub DecodedPixelData);
pub struct HDR { pub info: HDRMetadata, pub pixels: Vec<RGBE8Pixel> }
impl LogicalAssetData for PNG { const EXT: &'static str = "png"; }
impl LogicalAssetData for TGA { const EXT: &'static str = "tga"; }
impl LogicalAssetData for TIFF { const EXT: &'static str = "tiff"; }
impl LogicalAssetData for WebP { const EXT: &'static str = "webp"; }
impl LogicalAssetData for BMP { const EXT: &'static str = "bmp"; }
impl LogicalAssetData for HDR { const EXT: &'static str = "hdr"; }
impl Asset for PNG
{
    type Error = ImageError;
    fn from_binary(content: Vec<u8>) -> Result<Self, ImageError>
    {
        image::png::PngDecoder::new(std::io::Cursor::new(content)).and_then(DecodedPixelData::new).map(PNG)
    }
}
impl Asset for TGA
{
    type Error = ImageError;
    fn from_binary(content: Vec<u8>) -> Result<Self, ImageError>
    {
        image::tga::TgaDecoder::new(std::io::Cursor::new(content)).and_then(DecodedPixelData::new).map(TGA)
    }
}
impl Asset for TIFF
{
    type Error = ImageError;
    fn from_binary(content: Vec<u8>) -> Result<Self, ImageError>
    {
        image::tiff::TiffDecoder::new(std::io::Cursor::new(content)).and_then(DecodedPixelData::new).map(TIFF)
    }
}
impl Asset for WebP
{
    type Error = ImageError;
    fn from_binary(content: Vec<u8>) -> Result<Self, ImageError>
    {
        image::webp::WebPDecoder::new(std::io::Cursor::new(content)).and_then(DecodedPixelData::new).map(WebP)
    }
}
impl Asset for BMP
{
    type Error = ImageError;
    fn from_binary(content: Vec<u8>) -> Result<Self, ImageError>
    {
        image::bmp::BmpDecoder::new(std::io::Cursor::new(content)).and_then(DecodedPixelData::new).map(BMP)
    }
}
impl Asset for HDR
{
    type Error = ImageError;
    fn from_binary(content: Vec<u8>) -> Result<Self, ImageError>
    {
        let ireader = HdrDecoder::new(std::io::Cursor::new(content))?;
        let meta = ireader.metadata();
        let pixels = ireader.read_image_native()?;

        Ok(HDR { info: meta, pixels })
    }
}
