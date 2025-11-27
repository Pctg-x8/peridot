use std::{
    collections::HashMap,
    ffi::OsStr,
    path::{Path, PathBuf},
};

use ktx::Texture;

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

pub struct ImageAssetProcessor;
impl crate::AssetProcessor for ImageAssetProcessor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path
            .extension()
            .is_some_and(|x| x == "png" || x == "jpg" || x == "tiff")
    }

    fn dest_path(&self, source_file_name: &OsStr, out_dir_path: &Path) -> PathBuf {
        out_dir_path
            .join(source_file_name)
            .with_extension("pa1-texture2d")
    }

    fn process(
        &self,
        source_path: &Path,
        _metadata: &HashMap<crate::metadata::Key, String>,
        out_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let img = image::open(source_path).map_err(ImageAssetProcessError::OpenFailed)?;

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
                generateMipmaps: false,
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
                .uastc_flags(ktx::ffi::KTX_PACK_UASTC_LEVEL_DEFAULT)
                .uastc_rdo(),
        )
        .map_err(|e| ImageAssetProcessError::Ktx2OperationFailure("compress_basis_ex", e))?;
        ktx.deflate_zstd(11)
            .map_err(|e| ImageAssetProcessError::Ktx2OperationFailure("deflate_zstd", e))?;
        ktx.write_to_named_file(
            &std::ffi::CString::new(
                out_path
                    .to_str()
                    .ok_or(ImageAssetProcessError::InvalidOutPath)?,
            )
            .map_err(ImageAssetProcessError::NulByteInOutPath)?,
        )
        .map_err(|e| ImageAssetProcessError::Ktx2OperationFailure("write_to_named_file", e))?;

        Ok(())
    }
}

#[derive(thiserror::Error, Debug)]
pub enum SoundAssetProcessError {
    #[error("Failed to copy asset file: {0}")]
    CopyFailed(std::io::Error),
}

pub struct SoundAssetProcessor;
impl crate::AssetProcessor for SoundAssetProcessor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path
            .extension()
            .is_some_and(|x| x == "wav" || x == "mp3" || x == "ogg" || x == "flac")
    }

    fn dest_path(&self, source_file_name: &OsStr, out_dir_path: &Path) -> PathBuf {
        out_dir_path
            .join(source_file_name)
            .with_extension("pa1-audio")
    }

    fn process(
        &self,
        source_path: &Path,
        _metadata: &HashMap<crate::metadata::Key, String>,
        out_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // TODO: convert to what?
        std::fs::copy(source_path, out_path).map_err(SoundAssetProcessError::CopyFailed)?;

        Ok(())
    }
}
