use core::pin::Pin;
use image::codecs::hdr::{HdrDecoder, HdrMetadata};
use image::{ImageDecoder, ImageError, ImageResult};
use ktx::Texture;
use peridot::{
    AssetBlob, DecodedPixelData, FromAssetBlob, FromAssetBlobAsync, LDRImageAsset,
    LogicalAssetData, PixelFormat,
};
use std::convert::TryInto;
use std::io::{BufReader, Seek, Write};

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
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_IMAGE2D;
}
impl LogicalAssetData for TGA {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_IMAGE2D;
}
impl LogicalAssetData for TIFF {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_IMAGE2D;
}
impl LogicalAssetData for WebP {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_IMAGE2D;
}
impl LogicalAssetData for BMP {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_IMAGE2D;
}
impl LogicalAssetData for HDR {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_IMAGE2D;
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

pub struct StdTexture2DAssetGenerated<'a>(pub ktx::Owned<ktx::Texture2>, pub &'a [u8]);
impl<'a> StdTexture2DAssetGenerated<'a> {
    pub fn write<W: Write + Seek>(&self, w: &mut W) -> std::io::Result<()> {
        struct WriteStreamData<'a, W> {
            ptr: &'a mut W,
            pos_base: u64,
        }
        extern "C" fn stream_read(
            _: *mut ktx::ffi::ktxStream,
            _: *mut core::ffi::c_void,
            _: usize,
        ) -> ktx::ffi::ktx_error_code_e {
            unimplemented!();
        }
        extern "C" fn stream_skip(
            _: *mut ktx::ffi::ktxStream,
            _: usize,
        ) -> ktx::ffi::ktx_error_code_e {
            unimplemented!();
        }
        extern "C" fn stream_write<W: Write>(
            this: *mut ktx::ffi::ktxStream,
            src: *const core::ffi::c_void,
            size: usize,
            count: usize,
        ) -> ktx::ffi::ktx_error_code_e {
            let writer =
                unsafe { &mut *((*this).data.custom_ptr.address as *mut WriteStreamData<W>) };
            writer
                .ptr
                .write_all(unsafe { core::slice::from_raw_parts(src.cast::<u8>(), size * count) })
                .expect("stream_write");
            0
        }
        extern "C" fn stream_getpos<W: Seek>(
            this: *mut ktx::ffi::ktxStream,
            pos: *mut i64,
        ) -> ktx::ffi::ktx_error_code_e {
            let writer =
                unsafe { &mut *((*this).data.custom_ptr.address as *mut WriteStreamData<W>) };
            unsafe {
                pos.write(
                    (writer
                        .ptr
                        .stream_position()
                        .expect("writer.stream_position")
                        - writer.pos_base)
                        .try_into()
                        .expect("invalid pos"),
                );
            }
            0
        }
        extern "C" fn stream_setpos(
            _: *mut ktx::ffi::ktxStream,
            _: i64,
        ) -> ktx::ffi::ktx_error_code_e {
            unimplemented!();
        }
        extern "C" fn stream_getsize(
            _: *mut ktx::ffi::ktxStream,
            _: *mut usize,
        ) -> ktx::ffi::ktx_error_code_e {
            unimplemented!();
        }
        extern "C" fn stream_destruct(_: *mut ktx::ffi::ktxStream) {
            println!("destruct");
        }

        let mut writer_stream_data = WriteStreamData {
            pos_base: w.stream_position().expect("stream_position"),
            ptr: w,
        };
        let mut stream = ktx::ffi::ktxStream {
            read: stream_read,
            skip: stream_skip,
            write: stream_write::<W>,
            getpos: stream_getpos::<W>,
            setpos: stream_setpos,
            getsize: stream_getsize,
            destruct: stream_destruct,
            r#type: ktx::ffi::eStreamTypeCustom,
            data: ktx::ffi::ktxStreamData {
                custom_ptr: ktx::ffi::ktxStreamDataCustomPtr {
                    address: core::ptr::from_mut(&mut writer_stream_data).cast(),
                    allocatorAddress: core::ptr::null_mut(),
                    size: 0,
                },
            },
            readpos: 0,
            closeOnDestruct: false,
        };
        unsafe {
            self.0
                .write_to_stream_raw(&mut stream)
                .expect("content.write_to_stream_raw");
        }

        Ok(())
    }
}

pub struct StdTexture2DAsset(
    pub ktx::Owned<ktx::Texture2>,
    #[allow(dead_code)] Pin<Box<[u8]>>,
);
impl LogicalAssetData for StdTexture2DAsset {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_IMAGE2D;
}
impl FromAssetBlob for StdTexture2DAsset {
    type Error = std::io::Error;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        let buf = blob.read_to_end(0)?;
        let buf = Box::into_pin(buf.into_boxed_slice());
        let container = ktx::Texture2::from_memory(&buf, ktx::TextureCreateFlags::empty())
            .expect("Failed to load ktx2");

        Ok(Self(container, buf))
    }
}
impl FromAssetBlobAsync for StdTexture2DAsset {
    type Error = std::io::Error;

    fn from_asset_blob_async<'a, Blob: peridot::AssetBlobAsync + 'a>(
        blob: Blob,
    ) -> impl core::future::Future<Output = Result<Self, Self::Error>> {
        async move {
            let buf = blob.read_to_end_async(0).await?;
            let buf = Box::into_pin(buf.into_boxed_slice());
            let container = ktx::Texture2::from_memory(&buf, ktx::TextureCreateFlags::empty())
                .expect("Failed to load ktx2");

            Ok(Self(container, buf))
        }
    }
}
