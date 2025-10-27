use core::pin::Pin;
use image::codecs::hdr::{HdrDecoder, HdrMetadata};
use image::{ImageDecoder, ImageError, ImageResult};
use peridot::{
    AssetBlob, DecodedPixelData, FromAssetBlob, LDRImageAsset, LogicalAssetData, PixelFormat,
};
use std::io::BufReader;

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
impl FromAssetBlob for PNG {
    type Error = ImageError;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, ImageError> {
        image::codecs::png::PngDecoder::new(std::io::BufReader::new(
            peridot::native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
        .and_then(load_image)
        .map(PNG)
    }
}
impl FromAssetBlob for TGA {
    type Error = ImageError;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, ImageError> {
        image::codecs::tga::TgaDecoder::new(std::io::BufReader::new(
            peridot::native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
        .and_then(load_image)
        .map(TGA)
    }
}
impl FromAssetBlob for TIFF {
    type Error = ImageError;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, ImageError> {
        image::codecs::tiff::TiffDecoder::new(std::io::BufReader::new(
            peridot::native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
        .and_then(load_image)
        .map(TIFF)
    }
}
impl FromAssetBlob for WebP {
    type Error = ImageError;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, ImageError> {
        image::codecs::webp::WebPDecoder::new(std::io::BufReader::new(
            peridot::native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
        .and_then(load_image)
        .map(WebP)
    }
}
impl FromAssetBlob for BMP {
    type Error = ImageError;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, ImageError> {
        image::codecs::bmp::BmpDecoder::new(std::io::BufReader::new(
            peridot::native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
        .and_then(load_image)
        .map(BMP)
    }
}
impl FromAssetBlob for HDR {
    type Error = ImageError;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, ImageError> {
        let reader = HdrDecoder::new(BufReader::new(
            peridot::native_io::RandomBlobReadSeekAdapter::new(blob),
        ))?;

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

pub struct StdTexture2DAsset(
    pub ktx::Owned<ktx::Texture2>,
    #[allow(dead_code)] Pin<Box<[u8]>>,
);
impl LogicalAssetData for StdTexture2DAsset {
    const EXT: &'static str = "pa1-texture2d";
}
impl FromAssetBlob for StdTexture2DAsset {
    type Error = std::io::Error;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        let buf = blob.read_to_end(0)?;
        let buf = Pin::new(buf.into_boxed_slice());
        let container = ktx::Texture2::from_memory(&buf, ktx::TextureCreateFlags::empty())
            .expect("Failed to load ktx2");

        Ok(Self(container, buf))
    }
}
