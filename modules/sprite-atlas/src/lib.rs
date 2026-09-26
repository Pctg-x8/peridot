use bedrock as br;
use ktx::Texture;
use peridot_serialization_utils::{PascalStr, PascalString};
use std::{
    collections::HashMap,
    io::{BufRead, Seek, Write},
};

pub mod source;

pub struct Sprite {
    pub left: u32,
    pub top: u32,
    pub width: u32,
    pub height: u32,
    pub uvst: peridot_math::Vector4F32,
    pub border_left: u32,
    pub border_top: u32,
    pub border_right: u32,
    pub border_bottom: u32,
}

pub struct SpriteAtlasAsset {
    pub width: u32,
    pub height: u32,
    pub sprites: HashMap<String, Sprite>,
    pub content: ktx::Owned<ktx::Texture2>,
}

impl SpriteAtlasAsset {
    pub fn write<W: Write>(&self, sink: &mut W) -> std::io::Result<()> {
        sink.write_all(&0x1234u16.to_ne_bytes())?; // endian marker
        sink.write_all(&self.width.to_ne_bytes())?;
        sink.write_all(&self.height.to_ne_bytes())?;
        sink.write_all(&(self.sprites.len() as u64).to_ne_bytes())?;
        for (id, v) in self.sprites.iter() {
            PascalStr(&id).write(sink)?;
            sink.write_all(&v.left.to_ne_bytes())?;
            sink.write_all(&v.top.to_ne_bytes())?;
            sink.write_all(&v.width.to_ne_bytes())?;
            sink.write_all(&v.height.to_ne_bytes())?;
            sink.write_all(&v.border_left.to_ne_bytes())?;
            sink.write_all(&v.border_top.to_ne_bytes())?;
            sink.write_all(&v.border_right.to_ne_bytes())?;
            sink.write_all(&v.border_bottom.to_ne_bytes())?;
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
            let writer = unsafe { &mut *((*this).data.custom_ptr.address as *mut W) };
            writer
                .write_all(unsafe { core::slice::from_raw_parts(src.cast::<u8>(), size * count) })
                .expect("stream_write");
            0
        }
        extern "C" fn stream_getpos(
            _: *mut ktx::ffi::ktxStream,
            _: *mut i64,
        ) -> ktx::ffi::ktx_error_code_e {
            unimplemented!();
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
        let mut stream = ktx::ffi::ktxStream {
            read: stream_read,
            skip: stream_skip,
            write: stream_write::<W>,
            getpos: stream_getpos,
            setpos: stream_setpos,
            getsize: stream_getsize,
            destruct: stream_destruct,
            r#type: ktx::ffi::eStreamTypeCustom,
            data: ktx::ffi::ktxStreamData {
                custom_ptr: ktx::ffi::ktxStreamDataCustomPtr {
                    address: sink as *mut _ as _,
                    allocatorAddress: core::ptr::null_mut(),
                    size: 0,
                },
            },
            readpos: 0,
            closeOnDestruct: false,
        };
        unsafe {
            self.content
                .write_to_stream_raw(&mut stream)
                .expect("content.write_to_stream_raw");
        }

        Ok(())
    }

    pub fn read<R: BufRead + Seek>(source: &mut R) -> std::io::Result<Self> {
        let mut endian_marker = 0u16;
        source
            .read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 2]>(&mut endian_marker) })?;
        let mut width_native = 0u32;
        source.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 4]>(&mut width_native) })?;
        let mut height_native = 0u32;
        source
            .read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 4]>(&mut height_native) })?;
        let mut sprites_native = 0u64;
        source
            .read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 8]>(&mut sprites_native) })?;

        let (width, height, sprites_count);
        if endian_marker == 0x1234 {
            // generated on same endian machine
            width = width_native;
            height = height_native;
            sprites_count = sprites_native;
        } else if endian_marker.swap_bytes() == 0x1234 {
            // generated on different endian machine
            width = width_native.swap_bytes();
            height = height_native.swap_bytes();
            sprites_count = sprites_native.swap_bytes();
        } else {
            panic!("endian_marker mismatch: 0x{endian_marker:04x}")
        }

        let atlas_width = width;
        let atlas_height = height;
        let mut sprites = HashMap::with_capacity(sprites_count as _);
        for _ in 0..sprites_count {
            let id = PascalString::read(source)?;
            let mut left = 0u32;
            source.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 4]>(&mut left) })?;
            let mut top = 0u32;
            source.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 4]>(&mut top) })?;
            let mut width = 0u32;
            source.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 4]>(&mut width) })?;
            let mut height = 0u32;
            source.read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 4]>(&mut height) })?;
            let mut border_left = 0u32;
            source
                .read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 4]>(&mut border_left) })?;
            let mut border_top = 0u32;
            source
                .read_exact(unsafe { core::mem::transmute::<_, &mut [u8; 4]>(&mut border_top) })?;
            let mut border_right = 0u32;
            source.read_exact(unsafe {
                core::mem::transmute::<_, &mut [u8; 4]>(&mut border_right)
            })?;
            let mut border_bottom = 0u32;
            source.read_exact(unsafe {
                core::mem::transmute::<_, &mut [u8; 4]>(&mut border_bottom)
            })?;

            sprites.insert(
                id.0,
                Sprite {
                    left,
                    top,
                    width,
                    height,
                    uvst: peridot_math::Vector4(
                        width as f32 / atlas_width as f32,
                        height as f32 / atlas_height as f32,
                        left as f32 / atlas_width as f32,
                        top as f32 / atlas_height as f32,
                    ),
                    border_left,
                    border_top,
                    border_right,
                    border_bottom,
                },
            );
        }

        let mut content_bytes = Vec::new();
        source.read_to_end(&mut content_bytes)?;
        let content =
            ktx::Texture2::from_memory(&content_bytes, ktx::TextureCreateFlags::LOAD_IMAGE_DATA)
                .expect("content.from_stream_raw");

        Ok(Self {
            width,
            height,
            sprites,
            content,
        })
    }
}

#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for SpriteAtlasAsset {
    const EXT: &'static str = "pa1-sprite-atlas";
}
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAssetBlob for SpriteAtlasAsset {
    type Error = std::io::Error;

    fn from_asset_blob<'a, Blob: peridot::AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        Self::read(&mut std::io::BufReader::new(
            peridot::native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
    }
}

#[cfg(feature = "with-asset-processing")]
#[derive(Debug, thiserror::Error)]
pub enum ProcessError {
    #[error("invalid content in source asset")]
    InvalidContent,
}

#[cfg(feature = "with-asset-processing")]
pub struct AssetProcessor;
#[cfg(feature = "with-asset-processing")]
impl peridot_asset_processing::AssetProcessor for AssetProcessor {
    fn can_process(&self, source_path: &std::path::Path) -> bool {
        source_path.extension().is_some_and(|x| x == "spratlas")
    }

    fn dest_path(
        &self,
        source_file_name: &std::ffi::OsStr,
        out_dir_path: &std::path::Path,
    ) -> std::path::PathBuf {
        out_dir_path
            .join(source_file_name)
            .with_extension("pa1-sprite-atlas")
    }

    #[tracing::instrument(
        name = "AssetProcessor::process",
        skip(self, _metadata),
        fields(atlas_width, atlas_height)
    )]
    fn process(
        &self,
        source_path: &std::path::Path,
        _metadata: &HashMap<peridot_asset_processing::metadata::Key, String>,
        out_path: &std::path::Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use ktx::Texture;

        let source = std::fs::read_to_string(source_path)?;
        let mut config = None;
        let mut sprites = HashMap::new();
        let mut has_content_error = false;
        for x in source::Parser::new(&source) {
            match x {
                Err(e) => {
                    tracing::error!(reason = ?e, "error in parsing content");
                    has_content_error = true;
                }
                Ok(source::Record::Configuration(cfg)) => match config {
                    Some(_) => {
                        tracing::error!("already configured");
                        has_content_error = true;
                    }
                    None => {
                        config = Some(cfg);
                    }
                },
                Ok(source::Record::Sprite(mut s)) => {
                    match sprites.entry(core::mem::replace(&mut s.id, String::new())) {
                        std::collections::hash_map::Entry::Vacant(x) => {
                            x.insert(s);
                        }
                        std::collections::hash_map::Entry::Occupied(x) => {
                            tracing::error!(id = x.key(), "conflicting sprite atlas id");
                            has_content_error = true;
                        }
                    }
                }
            }
        }

        if has_content_error {
            return Err(ProcessError::InvalidContent.into());
        }

        let config = config.unwrap_or_else(|| {
            tracing::warn!("no configuration specified, using default value");

            source::ConfigurationRecord {
                width: 512,
                height: 512,
            }
        });
        tracing::Span::current().record("atlas_width", config.width);
        tracing::Span::current().record("atlas_height", config.height);

        let mut pixel_format = None;
        let mut pixels = Vec::new();
        let mut out_sprites = HashMap::with_capacity(sprites.len());
        for (id, s) in sprites {
            let path = match source_path.parent() {
                None => s.source_file_path,
                Some(p) => p.join(s.source_file_path),
            };
            let img = match image::open(&path) {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(reason = ?e, ?path, "Failed to open source image");
                    continue;
                }
            };
            let width = img.width();
            let height = img.height();

            let (color_bytes, bpp);
            match pixel_format {
                None => {
                    let c = img.color();
                    tracing::debug!(format = ?c, "inferred pixel format");
                    pixel_format = Some(c);
                    pixels = vec![
                        0u8;
                        (config.width * config.height) as usize
                            * (c.bits_per_pixel() as usize >> 3)
                    ];

                    color_bytes = img.into_bytes();
                    bpp = c.bits_per_pixel();
                }
                Some(p) => {
                    if p == img.color() {
                        color_bytes = img.into_bytes();
                        bpp = p.bits_per_pixel();
                    } else {
                        todo!("pixel format conversion");
                    }
                }
            }

            let actual_width = width.min(config.width - s.left);
            let actual_height = height.min(config.height - s.top);
            if width != actual_width || height != actual_height {
                tracing::warn!(
                    left = s.left,
                    top = s.top,
                    width,
                    height,
                    "the sprite rect cannot be fitted into the atlas, some visual may be cropped"
                );
            }

            for y in 0..actual_height {
                unsafe {
                    core::ptr::copy_nonoverlapping(
                        color_bytes
                            .as_ptr()
                            .add((y * width) as usize * (bpp as usize >> 3)),
                        pixels.as_mut_ptr().add(
                            ((y + s.top) * config.width + s.left) as usize * (bpp as usize >> 3),
                        ),
                        actual_width as usize * (bpp as usize >> 3),
                    );
                }
            }

            out_sprites.insert(
                id,
                Sprite {
                    left: s.left,
                    top: s.top,
                    width: actual_width,
                    height: actual_height,
                    uvst: peridot_math::Vector4(
                        actual_width as f32 / config.width as f32,
                        actual_height as f32 / config.height as f32,
                        s.left as f32 / config.width as f32,
                        s.top as f32 / config.height as f32,
                    ),
                    border_left: s.border_left,
                    border_top: s.border_top,
                    border_right: s.border_right,
                    border_bottom: s.border_bottom,
                },
            );
        }

        let mut ktx = ktx::Texture2::new(
            &ktx::ffi::ktxTextureCreateInfo {
                glInternalformat: 0,
                // TODO: いったん空出力の場合はRgba8を想定する そのうちカラーフォーマットは指定できるようにしたいかも
                vkFormat: match pixel_format.unwrap_or(image::ColorType::Rgba8) {
                    image::ColorType::Rgba8 => br::vk::VK_FORMAT_R8G8B8A8_UNORM as _,
                    _ => todo!("vkFormat"),
                },
                pDfd: core::ptr::null_mut(),
                baseWidth: config.width,
                baseHeight: config.height,
                baseDepth: 1,
                numDimensions: 2,
                numLevels: 1,
                numLayers: 1,
                numFaces: 1,
                isArray: false,
                generateMipmaps: false,
            },
            true,
        )
        .inspect_err(|e| tracing::error!(reason = ?e, "Failed to create ktx2 texture"))?;
        ktx.set_image_from_memory(0, 0, 0, &pixels).inspect_err(
            |e| tracing::error!(reason = ?e, "Failed to set image data into ktx2 texture"),
        )?;
        ktx.compress_basis_ex(&mut ktx::BasisParams::new().uastc().uastc_rdo())
            .inspect_err(|e| tracing::error!(reason = ?e, "Failed to compress ktx2 texture"))?;
        ktx.deflate_zstd(11)
            .inspect_err(|e| tracing::error!(reason = ?e, "Failed to deflate ktx2 texture"))?;

        SpriteAtlasAsset {
            width: config.width,
            height: config.height,
            sprites: out_sprites,
            content: ktx,
        }
        .write(
            &mut std::fs::File::options()
                .write(true)
                .truncate(true)
                .create(true)
                .open(out_path)
                .inspect_err(|e| tracing::error!(reason = ?e, "Failed to open output"))?,
        )
        .inspect_err(|e| tracing::error!(reason = ?e, "Failed to write asset data"))?;

        Ok(())
    }
}

#[cfg(feature = "with-asset-processing")]
#[derive(Debug, thiserror::Error)]
pub enum AssetProcessError {
    #[error(transparent)]
    KtxOperationFailure(#[from] ktx::Error),
    #[error(transparent)]
    IO(#[from] std::io::Error),
}
