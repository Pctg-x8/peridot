
use std::io::{Result as IOResult, BufReader, Error as IOError, ErrorKind, Cursor};
use std::io::prelude::{Read, Seek};
use super::GenericResult;

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
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> GenericResult<Self>;
    
    fn from_archive(reader: &mut archive::ArchiveRead, path: &str) -> GenericResult<Self> {
        let bin = reader.read_bin(path)?;
        match bin {
            None => Err(IOError::new(ErrorKind::NotFound, "No Entry in primary asset package").into()),
            Some(b) => Self::from_asset(Cursor::new(b))
        }
    }
}
pub trait FromStreamingAsset: LogicalAssetData {
    fn from_asset<Asset: Read>(asset: Asset) -> GenericResult<Self>;
}
use vertex_processing_pack::*;
impl LogicalAssetData for PvpContainer { const EXT: &'static str = "pvp"; }
impl FromAsset for PvpContainer {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> GenericResult<Self> {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container).map_err(From::from)
    }
}

use image::{ImageDecoder, ImageResult};
pub struct DecodedPixelData {
    pub pixels: image::DecodingResult, pub size: math::Vector2<u32>,
    pub color: image::ColorType, pub stride: usize
}
impl DecodedPixelData {
    pub fn new<D: ImageDecoder>(mut decoder: D) -> ImageResult<Self> {
        let color = decoder.colortype()?;
        let (w, h) = decoder.dimension()?;
        let pixels = decoder.read_image()?;
        let stride = decoder.row_len()?;
        
        Ok(DecodedPixelData { pixels, size: math::Vector2(w, h), color, stride })
    }
}
pub struct PNG(DecodedPixelData);
pub struct TGA(DecodedPixelData);
pub struct TIFF(DecodedPixelData);
pub struct WebP(DecodedPixelData);
pub struct BMP(DecodedPixelData);
impl LogicalAssetData for PNG { const EXT: &'static str = "png"; }
impl LogicalAssetData for TGA { const EXT: &'static str = "tga"; }
impl LogicalAssetData for TIFF { const EXT: &'static str = "tiff"; }
impl LogicalAssetData for WebP { const EXT: &'static str = "webp"; }
impl LogicalAssetData for BMP { const EXT: &'static str = "bmp"; }
impl FromAsset for PNG {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> GenericResult<Self> {
        DecodedPixelData::new(image::png::PNGDecoder::new(asset)).map(PNG).map_err(From::from)
    }
}
impl FromAsset for TGA {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> GenericResult<Self> {
        DecodedPixelData::new(image::tga::TGADecoder::new(asset)).map(PNG).map_err(From::from)
    }
}
impl FromAsset for TIFF {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> GenericResult<Self> {
        DecodedPixelData::new(image::tiff::TIFFDecoder::new(asset)?).and_then(TIFF).map_err(From::from)
    }
}
impl FromAsset for WebP {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> GenericResult<Self> {
        DecodedPixelData::new(image::webp::WebpDecoder::new(asset)).map(WebP).map_err(From::from)
    }
}
impl FromAsset for BMP {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> GenericResult<Self> {
        DecodedPixelData::new(image::bmp::BMPDecoder::new(asset)).map(BMP).map_err(From::from)
    }
}
