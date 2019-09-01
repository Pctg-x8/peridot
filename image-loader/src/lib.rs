use peridot_module_interface::{LogicalAssetData, FromAsset, PixelData, PixelFormat, LDRImageAsset};
use image::hdr::{HDRMetadata, RGBE8Pixel};
use image::{ImageError, ImageResult, ImageDecoder};
use std::io::prelude::{Read, Seek};
use std::io::BufReader;
use peridot_math as math;

pub struct PNG(pub PixelData);
pub struct TGA(pub PixelData);
pub struct TIFF(pub PixelData);
pub struct WebP(pub PixelData);
pub struct BMP(pub PixelData);
pub struct HDR { pub info: HDRMetadata, pub pixels: Vec<RGBE8Pixel> }

impl LogicalAssetData for PNG { const EXT: &'static str = "png"; }
impl LogicalAssetData for TGA { const EXT: &'static str = "tga"; }
impl LogicalAssetData for TIFF { const EXT: &'static str = "tiff"; }
impl LogicalAssetData for WebP { const EXT: &'static str = "webp"; }
impl LogicalAssetData for BMP { const EXT: &'static str = "bmp"; }
impl LogicalAssetData for HDR { const EXT: &'static str = "hdr"; }

impl FromAsset for PNG
{
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError>
    {
        image::png::PNGDecoder::new(asset).and_then(pixeldata_from_decoder).map(PNG)
    }
}
impl FromAsset for TGA
{
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError>
    {
        image::tga::TGADecoder::new(asset).and_then(pixeldata_from_decoder).map(TGA)
    }
}
impl FromAsset for TIFF
{
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError>
    {
        image::tiff::TIFFDecoder::new(asset).and_then(pixeldata_from_decoder).map(TIFF)
    }
}
impl FromAsset for WebP
{
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError>
    {
        image::webp::WebpDecoder::new(asset).and_then(pixeldata_from_decoder).map(WebP)
    }
}
impl FromAsset for BMP
{
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError>
    {
        image::bmp::BMPDecoder::new(asset).and_then(pixeldata_from_decoder).map(BMP)
    }
}
impl FromAsset for HDR
{
    type Error = ImageError;
    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError>
    {
        let ireader = image::hdr::HDRDecoder::new(BufReader::new(asset))?;
        let meta = ireader.metadata();
        let pixels = ireader.read_image_native()?;

        Ok(HDR { info: meta, pixels })
    }
}

impl LDRImageAsset for BMP { fn into_pixel_data_info(self) -> PixelData { self.0 } }
impl LDRImageAsset for PNG { fn into_pixel_data_info(self) -> PixelData { self.0 } }
impl LDRImageAsset for TGA { fn into_pixel_data_info(self) -> PixelData { self.0 } }
impl LDRImageAsset for TIFF { fn into_pixel_data_info(self) -> PixelData { self.0 } }
impl LDRImageAsset for WebP { fn into_pixel_data_info(self) -> PixelData { self.0 } }

pub fn pixeldata_from_decoder<'d, D>(decoder: D) -> ImageResult<PixelData> where D: ImageDecoder<'d>
{
    let format = match decoder.colortype()
    {
        image::ColorType::RGB(8) => PixelFormat::RGB24,
        image::ColorType::RGBA(8) => PixelFormat::RGBA32,
        image::ColorType::BGR(8) => PixelFormat::BGR24,
        image::ColorType::BGRA(8) => PixelFormat::BGRA32,
        c => panic!("unsupported color type: {:?}", c)
    };
    let (w, h) = decoder.dimensions();
    let stride = decoder.row_bytes();
    let pixels = decoder.read_image()?;
    
    Ok(PixelData { pixels, size: math::Vector2(w as _, h as _), format, stride: stride as _ })
}
