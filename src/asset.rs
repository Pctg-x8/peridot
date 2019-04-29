
use std::io::{Result as IOResult, BufReader, Error as IOError, ErrorKind, Cursor};
use std::io::prelude::{Read, Seek, BufRead};
use std::borrow::Cow;
use rayon::prelude::*;

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
    fn from_asset<Asset: Read + Seek + 'static>(path: &str, asset: Asset) -> Result<Self, Self::Error>;
    
    fn from_archive(reader: &mut archive::ArchiveRead, path: &str) -> Result<Self, Self::Error> {
        let bin = reader.read_bin(path)?;
        match bin {
            None => Err(IOError::new(ErrorKind::NotFound, "No Entry in primary asset package").into()),
            Some(b) => Self::from_asset(path, Cursor::new(b))
        }
    }
}
pub trait FromStreamingAsset: LogicalAssetData {
    type Error: From<IOError>;

    fn from_asset<Asset: Read + 'static>(path: &str, asset: Asset) -> Result<Self, Self::Error>;
}
use vertex_processing_pack::*;
impl LogicalAssetData for PvpContainer { const EXT: &'static str = "pvp"; }
impl FromAsset for PvpContainer {
    type Error = IOError;

    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> IOResult<Self> {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container)
    }
}

impl LogicalAssetData for super::PolygonModelExtended { const EXT: &'static str = "pmx"; }
impl FromAsset for super::PolygonModelExtended {
    type Error = super::mmdloader::pmx::LoadingError;

    fn from_asset<Asset: Read + Seek + 'static>(path: &str, asset: Asset) -> Result<Self, Self::Error> {
        let mut base = path.split(".").map(|c| c.to_owned()).collect::<Vec<_>>(); base.pop();
        super::PolygonModelExtended::load(base, BufReader::new(asset))
    }
}
impl LogicalAssetData for super::mmdloader::vmd::MotionData { const EXT: &'static str = "vmd"; }
impl FromAsset for super::mmdloader::vmd::MotionData
{
    type Error = super::mmdloader::vmd::LoadingError;

    fn from_asset<Asset: Read + Seek + 'static>(_path: &str, asset: Asset) -> Result<Self, Self::Error>
    {
        super::mmdloader::vmd::MotionData::read(&mut BufReader::new(asset))
    }
}

pub enum PixelFormatAlphaed<'d, T: 'd + IndexedParallelIterator<Item = [u8; 4]>> { Raw(&'d [u8]), Converted(T) }

use image::{ImageDecoder, ImageResult, ImageError};
use image::hdr::HDRDecoder;
pub struct DecodedPixelData {
    pub pixels: Vec<u8>, pub size: math::Vector2<u32>,
    pub color: image::ColorType, pub stride: usize
}
impl DecodedPixelData {
    pub fn new<D: ImageDecoder>(decoder: D) -> ImageResult<Self> {
        let color = decoder.colortype();
        let (w, h) = decoder.dimensions();
        let stride = decoder.row_bytes();
        let pixels = decoder.read_image()?;
        
        Ok(DecodedPixelData { pixels, size: math::Vector2(w as _, h as _), color, stride: stride as _ })
    }

    pub fn u8_pixels(&self) -> &[u8] { &self.pixels }
    pub fn u8_pixels_alphaed<'d>(&'d self) -> PixelFormatAlphaed<'d, impl 'd + IndexedParallelIterator<Item = [u8; 4]>>
    {
        match self.color
        {
            image::ColorType::RGBA(8) | image::ColorType::BGRA(8) => PixelFormatAlphaed::Raw(&self.pixels),
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
    pub fn format(&self) -> super::PixelFormat {
        match self.color {
            image::ColorType::RGBA(8) => super::PixelFormat::RGBA32,
            image::ColorType::BGRA(8) => super::PixelFormat::BGRA32,
            image::ColorType::RGB(8) => super::PixelFormat::RGB24,
            image::ColorType::BGR(8) => super::PixelFormat::BGR24,
            c => panic!("unsupported format: {:?}", c)
        }
    }
    pub fn format_alpha(&self) -> super::PixelFormat {
        match self.color {
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
pub struct HDR(pub HDRDecoder<Box<BufRead + 'static>>);
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
        image::hdr::HDRDecoder::new(Box::new(BufReader::new(asset)) as _).map(HDR)
    }
}
