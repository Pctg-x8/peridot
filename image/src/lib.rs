use image::codecs::hdr::{HdrDecoder, HdrMetadata, Rgbe8Pixel};
use image::{ImageDecoder, ImageError, ImageResult};
use peridot::{DecodedPixelData, FromAsset, LDRImageAsset, LogicalAssetData, PixelFormat};
use std::io::{BufReader, Read, Seek};

fn load_image<'d, D>(decoder: D) -> ImageResult<DecodedPixelData>
where
    D: ImageDecoder<'d>,
{
    let color = decoder.color_type();
    let (w, h) = decoder.dimensions();
    let stride = decoder.scanline_bytes();
    let mut pixels = Vec::with_capacity(decoder.total_bytes() as _);
    unsafe {
        pixels.set_len(decoder.total_bytes() as _);
    }
    decoder.read_image(&mut pixels)?;

    Ok(DecodedPixelData {
        pixels,
        size: peridot::math::Vector2(w as _, h as _),
        format: format_map(color),
        stride: stride as _,
    })
}
fn format_map(fmt: image::ColorType) -> PixelFormat {
    match fmt {
        image::ColorType::Rgb8 => PixelFormat::RGB24,
        image::ColorType::Rgba8 => PixelFormat::RGBA32,
        _ => unimplemented!("unsupported color type: {:?}", fmt),
    }
}

#[repr(transparent)]
pub struct PNG(pub DecodedPixelData);
#[repr(transparent)]
pub struct TGA(pub DecodedPixelData);
#[repr(transparent)]
pub struct TIFF(pub DecodedPixelData);
#[repr(transparent)]
pub struct WebP(pub DecodedPixelData);
#[repr(transparent)]
pub struct BMP(pub DecodedPixelData);
pub struct HDR {
    pub info: HdrMetadata,
    pub pixels: Vec<Rgbe8Pixel>,
}

impl LogicalAssetData for PNG {
    const EXT: &'static str = "png";
}
impl LogicalAssetData for TGA {
    const EXT: &'static str = "tga";
}
impl LogicalAssetData for TIFF {
    const EXT: &'static str = "tiff";
}
impl LogicalAssetData for WebP {
    const EXT: &'static str = "webp";
}
impl LogicalAssetData for BMP {
    const EXT: &'static str = "bmp";
}
impl LogicalAssetData for HDR {
    const EXT: &'static str = "hdr";
}
impl FromAsset for PNG {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::png::PngDecoder::new(asset)
            .and_then(load_image)
            .map(PNG)
    }
}
impl FromAsset for TGA {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::tga::TgaDecoder::new(asset)
            .and_then(load_image)
            .map(TGA)
    }
}
impl FromAsset for TIFF {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::tiff::TiffDecoder::new(asset)
            .and_then(load_image)
            .map(TIFF)
    }
}
impl FromAsset for WebP {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::webp::WebPDecoder::new(asset)
            .and_then(load_image)
            .map(WebP)
    }
}
impl FromAsset for BMP {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::bmp::BmpDecoder::new(asset)
            .and_then(load_image)
            .map(BMP)
    }
}
impl FromAsset for HDR {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        let ireader = HdrDecoder::new(BufReader::new(asset))?;
        let meta = ireader.metadata();
        let pixels = ireader.read_image_native()?;

        Ok(HDR { info: meta, pixels })
    }
}

impl LDRImageAsset for BMP {
    fn into_pixel_data_info(self) -> DecodedPixelData {
        self.0
    }
}
impl LDRImageAsset for PNG {
    fn into_pixel_data_info(self) -> DecodedPixelData {
        self.0
    }
}
impl LDRImageAsset for TGA {
    fn into_pixel_data_info(self) -> DecodedPixelData {
        self.0
    }
}
impl LDRImageAsset for TIFF {
    fn into_pixel_data_info(self) -> DecodedPixelData {
        self.0
    }
}
impl LDRImageAsset for WebP {
    fn into_pixel_data_info(self) -> DecodedPixelData {
        self.0
    }
}
