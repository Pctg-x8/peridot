use image::codecs::hdr::{HdrDecoder, HdrMetadata};
use image::{ImageDecoder, ImageError, ImageResult};
use peridot::{DecodedPixelData, FromAsset, LDRImageAsset, LogicalAssetData, PixelFormat};
use std::io::{BufReader, Read, Seek};

fn load_image<D>(decoder: D) -> ImageResult<DecodedPixelData>
where
    D: ImageDecoder,
{
    let color = decoder.color_type();
    let (w, h) = decoder.dimensions();
    let mut pixels = Vec::with_capacity(decoder.total_bytes() as _);
    decoder.read_image(unsafe {
        core::mem::transmute::<&mut [core::mem::MaybeUninit<_>], &mut [_]>(
            pixels.spare_capacity_mut(),
        )
    })?;
    unsafe {
        pixels.set_len(pixels.capacity());
    }

    Ok(DecodedPixelData {
        pixels,
        size: peridot::math::Vector2(w as _, h as _),
        format: format_map(color),
        stride: w as _,
    })
}
fn format_map(fmt: image::ColorType) -> PixelFormat {
    match fmt {
        image::ColorType::Rgb8 => PixelFormat::RGB24,
        image::ColorType::Rgba8 => PixelFormat::RGBA32,
        image::ColorType::Rgb32F => PixelFormat::RGB96F,
        _ => unimplemented!("unsupported color type: {fmt:?}"),
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
    pub pixel_data: DecodedPixelData,
    pub hdr_metadata: HdrMetadata,
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
        image::codecs::png::PngDecoder::new(std::io::BufReader::new(asset))
            .and_then(load_image)
            .map(PNG)
    }
}
impl FromAsset for TGA {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::tga::TgaDecoder::new(std::io::BufReader::new(asset))
            .and_then(load_image)
            .map(TGA)
    }
}
impl FromAsset for TIFF {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::tiff::TiffDecoder::new(std::io::BufReader::new(asset))
            .and_then(load_image)
            .map(TIFF)
    }
}
impl FromAsset for WebP {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::webp::WebPDecoder::new(std::io::BufReader::new(asset))
            .and_then(load_image)
            .map(WebP)
    }
}
impl FromAsset for BMP {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        image::codecs::bmp::BmpDecoder::new(std::io::BufReader::new(asset))
            .and_then(load_image)
            .map(BMP)
    }
}
impl FromAsset for HDR {
    type Error = ImageError;

    fn from_asset<Asset: Read + Seek + 'static>(asset: Asset) -> Result<Self, ImageError> {
        let reader = HdrDecoder::new(BufReader::new(asset))?;

        Ok(Self {
            hdr_metadata: reader.metadata(),
            pixel_data: load_image(reader)?,
        })
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

#[repr(transparent)]
pub struct StdTexture2DAsset(pub ktx::Owned<ktx::Texture2>);
impl LogicalAssetData for StdTexture2DAsset {
    const EXT: &'static str = "pa1-texture2d";
}
impl FromAsset for StdTexture2DAsset {
    type Error = std::io::Error;

    fn from_asset<Asset: Read + Seek + 'static>(mut asset: Asset) -> Result<Self, Self::Error> {
        let mut buf = Vec::new();
        asset.read_to_end(&mut buf).expect("Failed to read");
        let container = ktx::Texture2::from_memory(&buf, ktx::TextureCreateFlags::empty())
            .expect("Failed to load ktx2");

        Ok(Self(container))
    }
}
