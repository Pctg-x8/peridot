
use std::io::{Result as IOResult, BufReader, Error as IOError, ErrorKind, Cursor};
use std::io::prelude::{Read, Seek};
use rayon::prelude::*;
use super::PixelFormat;

pub trait PlatformAssetLoader
{
    type Asset: Read + Seek + 'static;
    type StreamingAsset: Read + 'static;

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
    fn from_asset<Asset: Read + Seek + 'static>(path: &str, asset: Asset) -> Result<Self, Self::Error>;
    
    fn from_archive(reader: &mut archive::ArchiveRead, path: &str) -> Result<Self, Self::Error>
    {
        match reader.read_bin(path)?
        {
            None => Err(IOError::new(ErrorKind::NotFound, "No Entry in primary asset package").into()),
            Some(b) => Self::from_asset(path, Cursor::new(b))
        }
    }
}
pub trait FromStreamingAsset: LogicalAssetData
{
    type Error: From<IOError>;
    fn from_asset<Asset: Read + 'static>(path: &str, asset: Asset) -> Result<Self, Self::Error>;
}

pub enum PixelFormatAlphaed<'d, T: 'd + IndexedParallelIterator<Item = [u8; 4]>> { Raw(&'d [u8]), Converted(T) }

use image::{ImageDecoder, ImageResult, ImageError};
use image::hdr::{HDRDecoder, HDRMetadata, RGBE8Pixel};
pub struct DecodedPixelData
{
    pub pixels: Vec<u8>, pub size: math::Vector2<u32>,
    pub color: image::ColorType, pub stride: usize
}
impl DecodedPixelData
{
    pub fn new<'d, D>(decoder: D) -> ImageResult<Self> where D: ImageDecoder<'d>
    {
        let color = decoder.colortype();
        let (w, h) = decoder.dimensions();
        let stride = decoder.row_bytes();
        let pixels = decoder.read_image()?;
        
        Ok(DecodedPixelData { pixels, size: math::Vector2(w as _, h as _), color, stride: stride as _ })
    }
    pub fn format(&self) -> PixelFormat
    {
        match self.color
        {
            image::ColorType::RGB(8) => PixelFormat::RGB24,
            image::ColorType::RGBA(8) => PixelFormat::RGBA32,
            image::ColorType::BGR(8) => PixelFormat::BGR24,
            image::ColorType::BGRA(8) => PixelFormat::BGRA32,
            _ => panic!("unsupported color type: {:?}", self.color)
        }
    }

    pub fn u8_pixels(&self) -> &[u8] { &self.pixels }
    pub fn u8_pixels_alphaed<'d>(&'d self)
        -> PixelFormatAlphaed<'d, rayon::iter::Map<rayon::slice::Chunks<'d, u8>, impl Fn(&'d [u8]) -> [u8; 4]>>
    {
        match self.color
        {
            image::ColorType::RGBA(8) | image::ColorType::BGRA(8) =>
                PixelFormatAlphaed::Raw(&self.pixels),
            image::ColorType::RGB(8) | image::ColorType::BGR(8) =>
                PixelFormatAlphaed::Converted(self.pixels.par_chunks(3).map(|rgb| [rgb[0], rgb[1], rgb[2], 255])),
            c => panic!("conversion method not found for format {:?}", c)
        }
    }
    pub fn is_lacking_alpha_format(&self) -> bool { !self.has_alpha() }
    pub fn has_alpha(&self) -> bool
    {
        match self.color
        {
            image::ColorType::RGBA(8) | image::ColorType::BGRA(8) => true,
            _ => false
        }
    }
    pub fn format_alpha(&self) -> super::PixelFormat
    {
        match self.color
        {
            image::ColorType::RGBA(8) | image::ColorType::RGB(8) => super::PixelFormat::RGBA32,
            image::ColorType::BGRA(8) | image::ColorType::BGR(8) => super::PixelFormat::BGRA32,
            c => panic!("unsupported format: {:?}", c)
        }
    }
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
impl FromAsset for PNG {
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> Result<Self, ImageError> {
        image::png::PNGDecoder::new(asset).and_then(DecodedPixelData::new).map(PNG)
    }
}
impl FromAsset for TGA {
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> Result<Self, ImageError> {
        image::tga::TGADecoder::new(asset).and_then(DecodedPixelData::new).map(TGA)
    }
}
impl FromAsset for TIFF {
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> Result<Self, ImageError> {
        image::tiff::TIFFDecoder::new(asset).and_then(DecodedPixelData::new).map(TIFF)
    }
}
impl FromAsset for WebP {
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> Result<Self, ImageError> {
        image::webp::WebpDecoder::new(asset).and_then(DecodedPixelData::new).map(WebP)
    }
}
impl FromAsset for BMP {
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> Result<Self, ImageError> {
        image::bmp::BMPDecoder::new(asset).and_then(DecodedPixelData::new).map(BMP)
    }
}
impl FromAsset for HDR {
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> Result<Self, ImageError> {
        let ireader = HDRDecoder::new(BufReader::new(asset))?;
        let meta = ireader.metadata();
        let pixels = ireader.read_image_native()?;

        Ok(HDR { info: meta, pixels })
    }
}
