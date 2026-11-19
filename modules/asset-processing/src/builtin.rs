use std::path::Path;

use ktx::Texture;

use crate::{AssetProcessContext, AssetProcessResult, MetadataAccess, ProcessTargetFileData};

#[derive(thiserror::Error, Debug)]
pub enum ImageAssetProcessError {
    #[error("Failed to open asset: {0}")]
    OpenFailed(image::ImageError),
    #[error("Failed to create ktx2 texture: {0:?}")]
    CreateKtx2Failed(ktx::Error),
    #[error("Ktx2 Operation Failure({0}): {1:?}")]
    Ktx2OperationFailure(&'static str, ktx::Error),
    #[error("invalid utf-8 sequence in out path")]
    InvalidOutPath,
    #[error("Nul byte in out path")]
    NulByteInOutPath(std::ffi::NulError),
}

#[derive(Default, PartialEq, Eq)]
enum AssetType {
    #[default]
    Texture,
    Sprite,
}
impl AssetType {
    pub fn from_metadata_value(value: &str) -> Option<Self> {
        if value.eq_ignore_ascii_case("texture") {
            return Some(Self::Texture);
        }
        if value.eq_ignore_ascii_case("sprite") {
            return Some(Self::Sprite);
        }

        None
    }
}

macro_rules! concat_2str_const {
    ($a: expr, $b: expr) => {{
        const LEN: usize = $a.len() + $b.len();
        const fn combine(a: &'static str, b: &'static str) -> [u8; LEN] {
            let mut sink = [0u8; LEN];
            unsafe {
                core::ptr::copy_nonoverlapping(a.as_ptr(), sink.as_mut_ptr(), a.len());
                core::ptr::copy_nonoverlapping(b.as_ptr(), sink.as_mut_ptr().add(a.len()), b.len());
            }

            sink
        }

        const COMBINED: [u8; LEN] = const { combine($a, $b) };
        const { unsafe { core::str::from_utf8_unchecked(&COMBINED) } }
    }};
}

pub struct ImageAssetProcessor;
impl ImageAssetProcessor {
    const META_KEY_PREFIX: &str = "io.ct2.peridot.asset-processor.image";

    const META_OPT_TYPE_KEY: &str = "peridot.type";
    const META_OPT_UASTC_LEVEL_KEY: &str = "peridot.image.uastc-level";
    const META_OPT_ZSTD_LEVEL_KEY: &str = "peridot.image.zstd-level";
    const META_OPT_GENERATE_MIPMAPS_KEY: &str = "peridot.image.generate-mipmaps";
    const META_GENERATED_ASSET_ID_KEY: &str =
        concat_2str_const!(ImageAssetProcessor::META_KEY_PREFIX, ".generated-asset-id");
}
impl crate::AssetProcessor for ImageAssetProcessor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path
            .extension()
            .is_some_and(|x| x == "png" || x == "jpg" || x == "tiff")
    }

    #[tracing::instrument(skip(self, ctx, metadata))]
    fn process(
        &self,
        source_path: &Path,
        data: &ProcessTargetFileData,
        asset_group_id: peridot::AssetID,
        metadata: &mut MetadataAccess,
        ctx: &mut AssetProcessContext,
    ) -> Result<AssetProcessResult, Box<dyn std::error::Error>> {
        if !data.is_dirty {
            return Ok(AssetProcessResult::Skipped);
        }

        let r#type = metadata
            .get(Self::META_OPT_TYPE_KEY)
            .and_then(AssetType::from_metadata_value)
            .unwrap_or_default();
        if r#type == AssetType::Sprite {
            tracing::trace!("skipping `sprite` asset (packed by sprite atlas processor)");
            return Ok(AssetProcessResult::Skipped);
        }

        let asset_id = ctx.acquire_asset_id(
            metadata,
            Self::META_GENERATED_ASSET_ID_KEY.into(),
            &asset_group_id,
            peridot::ASSET_TYPE_IMAGE2D,
            0,
        );
        let img = image::open(source_path).map_err(ImageAssetProcessError::OpenFailed)?;

        let uastc_level_flag = metadata
            .get(Self::META_OPT_UASTC_LEVEL_KEY)
            .and_then(|x| match x {
                x if x.eq_ignore_ascii_case("slower") => {
                    Some(ktx::ffi::KTX_PACK_UASTC_LEVEL_VERYSLOW)
                }
                x if x.eq_ignore_ascii_case("slow") => Some(ktx::ffi::KTX_PACK_UASTC_LEVEL_SLOWER),
                x if x.eq_ignore_ascii_case("default") => {
                    Some(ktx::ffi::KTX_PACK_UASTC_LEVEL_DEFAULT)
                }
                x if x.eq_ignore_ascii_case("fast") => Some(ktx::ffi::KTX_PACK_UASTC_LEVEL_FASTER),
                x if x.eq_ignore_ascii_case("fastest") => {
                    Some(ktx::ffi::KTX_PACK_UASTC_LEVEL_FASTEST)
                }
                _ => None,
            })
            .unwrap_or(ktx::ffi::KTX_PACK_UASTC_LEVEL_DEFAULT);
        let zstd_level = metadata
            .get(Self::META_OPT_ZSTD_LEVEL_KEY)
            .and_then(|x| x.parse().ok())
            .unwrap_or(11);
        let generate_mipmaps = metadata
            .get(Self::META_OPT_GENERATE_MIPMAPS_KEY)
            .and_then(|x| match x {
                x if x.eq_ignore_ascii_case("true") => Some(true),
                x if x.eq_ignore_ascii_case("false") => Some(false),
                _ => None,
            })
            .unwrap_or(false);

        let mut ktx = ktx::Texture2::new(
            &ktx::ffi::ktxTextureCreateInfo {
                glInternalformat: 0,
                vkFormat: bedrock::vk::VK_FORMAT_R8G8B8A8_UNORM as _,
                pDfd: core::ptr::null_mut(),
                baseWidth: img.width(),
                baseHeight: img.height(),
                baseDepth: 1,
                numDimensions: 2,
                numLevels: 1,
                numLayers: 1,
                numFaces: 1,
                isArray: false,
                generateMipmaps: generate_mipmaps,
            },
            true,
        )
        .map_err(ImageAssetProcessError::CreateKtx2Failed)?;
        ktx.set_image_from_memory(0, 0, 0, img.to_rgba8().as_raw())
            .map_err(|e| {
                ImageAssetProcessError::Ktx2OperationFailure("set_image_from_memory", e)
            })?;
        ktx.compress_basis_ex(
            &mut ktx::BasisParams::new()
                .uastc()
                .uastc_flags(uastc_level_flag)
                .uastc_rdo(),
        )
        .map_err(|e| ImageAssetProcessError::Ktx2OperationFailure("compress_basis_ex", e))?;
        ktx.deflate_zstd(zstd_level)
            .map_err(|e| ImageAssetProcessError::Ktx2OperationFailure("deflate_zstd", e))?;
        ktx.write_to_named_file(
            &std::ffi::CString::new(
                ctx.prepare_runtime_asset_output(&asset_id)
                    .to_str()
                    .ok_or(ImageAssetProcessError::InvalidOutPath)?,
            )
            .map_err(ImageAssetProcessError::NulByteInOutPath)?,
        )
        .map_err(|e| ImageAssetProcessError::Ktx2OperationFailure("write_to_named_file", e))?;

        Ok(AssetProcessResult::Ok)
    }
}

#[derive(thiserror::Error, Debug)]
pub enum SoundAssetProcessError {
    #[error("Failed to copy asset file: {0}")]
    CopyFailed(std::io::Error),
}

pub struct SoundAssetProcessor;
impl SoundAssetProcessor {
    const META_KEY_PREFIX: &str = "io.ct2.peridot.asset-processor.sound";

    const META_GENERATED_ASSET_ID_KEY: &str =
        concat_2str_const!(SoundAssetProcessor::META_KEY_PREFIX, ".generated-asset-id");
}
impl crate::AssetProcessor for SoundAssetProcessor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path
            .extension()
            .is_some_and(|x| x == "wav" || x == "mp3" || x == "ogg" || x == "flac")
    }

    fn process(
        &self,
        source_path: &Path,
        data: &ProcessTargetFileData,
        asset_group_id: peridot::AssetID,
        metadata: &mut MetadataAccess,
        ctx: &mut AssetProcessContext,
    ) -> Result<AssetProcessResult, Box<dyn std::error::Error>> {
        if !data.is_dirty {
            return Ok(AssetProcessResult::Skipped);
        }

        // TODO: convert to what?
        let asset_id = ctx.acquire_asset_id(
            metadata,
            Self::META_GENERATED_ASSET_ID_KEY.into(),
            &asset_group_id,
            peridot::ASSET_TYPE_SOUND,
            0,
        );
        std::fs::copy(source_path, ctx.prepare_runtime_asset_output(&asset_id))
            .map_err(SoundAssetProcessError::CopyFailed)?;

        Ok(AssetProcessResult::Ok)
    }
}
