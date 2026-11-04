use bedrock as br;
use peridot_serialization_utils::{PascalStr, PascalString};
use peridot_types::AssetID;
use std::{
    collections::HashMap,
    io::{BufRead, IoSlice, IoSliceMut, Read, Seek, Write},
};
use vectored_io_helper::{read_vectored_all, write_vectored_all};

pub mod source;

pub struct SpriteAtlasAsset {
    pub texture_asset_id: AssetID,
    pub sprite_ids: HashMap<String, AssetID>,
}

impl SpriteAtlasAsset {
    pub fn write(&self, sink: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        sink.write_all(&0x1234u16.to_ne_bytes())?; // endian marker
        sink.write_all(self.texture_asset_id.as_bytes())?;
        sink.write_all(&(self.sprite_ids.len() as u64).to_ne_bytes())?;
        for (name, id) in self.sprite_ids.iter() {
            PascalStr(name).write(sink)?;
            sink.write_all(id.as_bytes())?;
        }

        Ok(())
    }

    pub fn read<R: BufRead + Seek>(source: &mut R) -> std::io::Result<Self> {
        let mut endian_marker = 0u16;
        source.read_exact(unsafe {
            core::mem::transmute::<&mut _, &mut [u8; 2]>(&mut endian_marker)
        })?;
        let is_native_endian = if endian_marker == 0x1234 {
            true
        } else if endian_marker.swap_bytes() == 0x1234 {
            false
        } else {
            panic!("endian_marker mismatch: 0x{endian_marker:04x}")
        };

        let mut texture_asset_id = core::mem::MaybeUninit::<AssetID>::uninit();
        let mut sprites_native = 0u64;
        read_vectored_all(
            source,
            &mut [
                IoSliceMut::new(unsafe { &mut *texture_asset_id.as_mut_ptr() }.as_bytes_mut()),
                IoSliceMut::new(unsafe {
                    core::mem::transmute::<&mut _, &mut [_; 8]>(&mut sprites_native)
                }),
            ],
        )?;
        let texture_asset_id = unsafe { texture_asset_id.assume_init() };

        let sprites_count = if is_native_endian {
            sprites_native
        } else {
            sprites_native.swap_bytes()
        };

        let mut sprite_ids = HashMap::with_capacity(sprites_count as _);
        for _ in 0..sprites_count {
            let name = PascalString::read(source)?;
            let mut id = core::mem::MaybeUninit::<AssetID>::uninit();
            source.read_exact(unsafe { &mut *id.as_mut_ptr() }.as_bytes_mut())?;

            sprite_ids.insert(name.0, unsafe { id.assume_init() });
        }

        Ok(Self {
            texture_asset_id,
            sprite_ids,
        })
    }
}

#[derive(Debug)]
pub struct SpriteAsset {
    pub atlas_texture_id: AssetID,
    pub left: u32,
    pub top: u32,
    pub width: u32,
    pub height: u32,
    pub border_left: u32,
    pub border_top: u32,
    pub border_right: u32,
    pub border_bottom: u32,
}
impl SpriteAsset {
    pub fn write(&self, sink: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        #[inline(always)]
        fn ios<'a, T>(d: &'a T) -> IoSlice<'a> {
            IoSlice::new(unsafe {
                core::slice::from_raw_parts(core::ptr::from_ref(d).cast::<u8>(), size_of_val(d))
            })
        }

        write_vectored_all(
            sink,
            &mut [
                ios(&0x1234u16),
                IoSlice::new(self.atlas_texture_id.as_bytes()),
                ios(&self.left),
                ios(&self.top),
                ios(&self.width),
                ios(&self.height),
                ios(&self.border_left),
                ios(&self.border_top),
                ios(&self.border_right),
                ios(&self.border_bottom),
            ],
        )
    }

    pub fn read(source: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        #[inline(always)]
        fn ioslice<'a, T>(this: *mut SpriteAsset, offs: usize) -> IoSliceMut<'a> {
            IoSliceMut::new(unsafe {
                core::slice::from_raw_parts_mut(this.cast::<u8>().byte_add(offs), size_of::<T>())
            })
        }

        let mut endian_marker = 0u16;
        source.read_exact(unsafe {
            core::mem::transmute::<&mut _, &mut [u8; 2]>(&mut endian_marker)
        })?;
        let is_native_endian = if endian_marker == 0x1234 {
            true
        } else if endian_marker.swap_bytes() == 0x1234 {
            false
        } else {
            panic!("invalid endian marker");
        };

        let mut this = core::mem::MaybeUninit::<Self>::uninit();
        read_vectored_all(
            source,
            &mut [
                IoSliceMut::new(
                    unsafe {
                        &mut *this
                            .as_mut_ptr()
                            .byte_add(core::mem::offset_of!(Self, atlas_texture_id))
                            .cast::<AssetID>()
                    }
                    .as_bytes_mut(),
                ),
                ioslice::<u32>(this.as_mut_ptr(), core::mem::offset_of!(Self, left)),
                ioslice::<u32>(this.as_mut_ptr(), core::mem::offset_of!(Self, top)),
                ioslice::<u32>(this.as_mut_ptr(), core::mem::offset_of!(Self, width)),
                ioslice::<u32>(this.as_mut_ptr(), core::mem::offset_of!(Self, height)),
                ioslice::<u32>(this.as_mut_ptr(), core::mem::offset_of!(Self, border_left)),
                ioslice::<u32>(this.as_mut_ptr(), core::mem::offset_of!(Self, border_top)),
                ioslice::<u32>(this.as_mut_ptr(), core::mem::offset_of!(Self, border_right)),
                ioslice::<u32>(
                    this.as_mut_ptr(),
                    core::mem::offset_of!(Self, border_bottom),
                ),
            ],
        )?;
        let this = unsafe { this.assume_init() };
        if is_native_endian {
            Ok(this)
        } else {
            Ok(Self {
                atlas_texture_id: this.atlas_texture_id,
                left: this.left.swap_bytes(),
                top: this.top.swap_bytes(),
                width: this.width.swap_bytes(),
                height: this.height.swap_bytes(),
                border_left: this.border_left.swap_bytes(),
                border_top: this.border_top.swap_bytes(),
                border_right: this.border_right.swap_bytes(),
                border_bottom: this.border_bottom.swap_bytes(),
            })
        }
    }

    #[inline]
    pub fn uvst(&self, atlas_width: u32, atlas_height: u32) -> peridot_math::Vector4F32 {
        peridot_math::Vector4(
            self.width as f32 / atlas_width as f32,
            self.height as f32 / atlas_height as f32,
            self.left as f32 / atlas_width as f32,
            self.top as f32 / atlas_height as f32,
        )
    }
}

#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for SpriteAsset {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_SPRITE;
}
#[cfg(feature = "with-loader-impl")]
impl peridot::FromAssetBlob for SpriteAsset {
    type Error = std::io::Error;

    #[inline(always)]
    fn from_asset_blob<'a, Blob: peridot::AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        Self::read(&mut peridot::native_io::RandomBlobReadSeekAdapter::new(
            blob,
        ))
    }
}

#[cfg(feature = "with-loader-impl")]
impl peridot::LogicalAssetData for SpriteAtlasAsset {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_SPRITE_ATLAS;
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

    #[tracing::instrument(
        name = "AssetProcessor::process",
        skip(self, _metadata, ctx),
        fields(atlas_width, atlas_height)
    )]
    fn process(
        &self,
        source_path: &std::path::Path,
        asset_group_id: peridot::AssetID,
        _metadata: &HashMap<peridot_asset_processing::metadata::Key, String>,
        ctx: &mut peridot_asset_processing::AssetProcessContext,
    ) -> Result<(), Box<dyn std::error::Error>> {
        use ktx::Texture;

        let asset_id =
            ctx.register_child_asset(&asset_group_id, peridot::ASSET_TYPE_SPRITE_ATLAS, 0);
        // not bound with any asset group(source file)
        let texture_asset_id = ctx.register_child_asset(
            &AssetID::GLOBAL_ASSET_GROUP_ID,
            peridot::ASSET_TYPE_IMAGE2D,
            0,
        );

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

            let sprite_metadata =
                peridot_asset_processing::load_metadata(path.with_extension("p-meta"));
            let sprite_assigned_group_id =
                peridot::AssetID::deserialize_chars(sprite_metadata["id"].chars())
                    .expect("invalid sprite id");
            // TODO: local_idどうするか(そもそもいる？)
            let sprite_asset_id =
                ctx.register_child_asset(&sprite_assigned_group_id, peridot::ASSET_TYPE_SPRITE, 0);
            SpriteAsset {
                atlas_texture_id: texture_asset_id.clone(),
                left: s.left,
                top: s.top,
                width: actual_width,
                height: actual_height,
                border_left: s.border_left,
                border_top: s.border_top,
                border_right: s.border_right,
                border_bottom: s.border_bottom,
            }
            .write(
                &mut std::fs::File::options()
                    .create(true)
                    .truncate(true)
                    .write(true)
                    .open(ctx.prepare_runtime_asset_output(&sprite_asset_id))
                    .expect("Failed to open output"),
            )
            .expect("Failed to write sprite asset data");

            out_sprites.insert(id, sprite_asset_id);
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
        peridot_image::StdTexture2DAssetGenerated(ktx, &pixels)
            .write(
                &mut std::fs::File::options()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(ctx.prepare_runtime_asset_output(&texture_asset_id))
                    .inspect_err(
                        |e| tracing::error!(reason = %e, "Failed to open texture output"),
                    )?,
            )
            .inspect_err(|e| tracing::error!(reason = ?e, "Failed to write texture asset data"))?;

        SpriteAtlasAsset {
            texture_asset_id,
            sprite_ids: out_sprites,
        }
        .write(
            &mut std::fs::File::options()
                .write(true)
                .truncate(true)
                .create(true)
                .open(ctx.prepare_runtime_asset_output(&asset_id))
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
