use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use clap::Parser;
use ktx::Texture;

#[derive(Parser)]
pub struct Args {
    source_path: PathBuf,
    #[arg(long, short = 'f')]
    force_rebuild: bool,
    #[arg(long, short = 'o')]
    out_dir: Option<PathBuf>,
}

pub trait AssetProcessor {
    fn can_process(&self, source_path: &Path) -> bool;
    fn dest_path(&self, source_file_name: &OsStr, out_dir_path: &Path) -> PathBuf;
    fn process(
        &self,
        source_path: &Path,
        out_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>>;
}

#[derive(thiserror::Error, Debug)]
pub enum RenderingConfigurationAssetProcessError {
    #[error("Failed to read source file: {0}")]
    ReadingFailed(std::io::Error),
    #[error("Error generating asset")]
    GeneratingAssetFailure,
    #[error("Failed to open destination file for writing: {0}")]
    DestWriteOpenFailed(std::io::Error),
    #[error("Error writing asset: {0}")]
    WritingAssetFailure(std::io::Error),
}

pub struct RenderingConfigurationAssetProcessor;
impl AssetProcessor for RenderingConfigurationAssetProcessor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path.extension().is_some_and(|x| x == "prc")
    }

    fn dest_path(&self, source_file_name: &OsStr, out_dir_path: &Path) -> PathBuf {
        out_dir_path
            .join(source_file_name)
            .with_extension("pa1-rendering-configuration")
    }

    fn process(
        &self,
        source_path: &Path,
        out_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let content = std::fs::read_to_string(source_path)
            .map_err(RenderingConfigurationAssetProcessError::ReadingFailed)?;
        let asset = peridot_rendering_configuration::compilation::compile(&content)
            .ok_or(RenderingConfigurationAssetProcessError::GeneratingAssetFailure)?;
        peridot_rendering_configuration::write(
            &mut std::fs::File::options()
                .write(true)
                .truncate(true)
                .create(true)
                .open(out_path)
                .map_err(RenderingConfigurationAssetProcessError::DestWriteOpenFailed)?,
            asset,
        )
        .map_err(RenderingConfigurationAssetProcessError::WritingAssetFailure)?;

        Ok(())
    }
}

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
impl AssetProcessor for ImageAssetProcessor {
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
impl AssetProcessor for SoundAssetProcessor {
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
        out_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // TODO: convert to what?
        std::fs::copy(source_path, out_path).map_err(SoundAssetProcessError::CopyFailed)?;

        Ok(())
    }
}

fn main() {
    tracing_subscriber::fmt()
        .pretty()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let args = Args::parse();
    let (Some(source_dir), Some(source_file_name)) =
        (args.source_path.parent(), args.source_path.file_name())
    else {
        tracing::error!(
            path = %args.source_path.display(),
            "invalid source file path provided"
        );
        std::process::exit(1);
    };
    let dest_dir = args.out_dir.as_deref().unwrap_or(source_dir);

    let span = tracing::span!(
        tracing::Level::INFO,
        "main",
        source_path = %args.source_path.display(),
    );
    let _span_enter = span.enter();

    let processors: [Box<dyn AssetProcessor>; _] = [
        Box::new(RenderingConfigurationAssetProcessor),
        Box::new(ImageAssetProcessor),
        Box::new(SoundAssetProcessor),
    ];
    let mut matching_processors_iter = processors
        .iter()
        .filter(|x| x.can_process(&args.source_path));
    let Some(processor) = matching_processors_iter.next() else {
        // unknown assets
        tracing::warn!("found unknown assets(not processed)");
        let dest_path = dest_dir.join(source_file_name);

        if let Err(e) = std::fs::copy(&args.source_path, &dest_path) {
            tracing::error!(reason = ?e, "Failed to copy asset file");
            std::process::exit(1);
        }

        return;
    };
    if matching_processors_iter.next().is_some() {
        tracing::error!("Cannot determine an asset processor");
        std::process::exit(1);
    }

    let dest_path = processor.dest_path(source_file_name, dest_dir);
    if !args.force_rebuild
        && let (Ok(x), Ok(y)) = (args.source_path.metadata(), dest_path.metadata())
        && x.modified().unwrap() <= y.modified().unwrap()
    {
        tracing::info!(reason = "modified time", "skip asset");
        return;
    }

    if let Err(e) = processor.process(&args.source_path, &dest_path) {
        tracing::error!(reason = ?e, "Failed to process asset");
        std::process::exit(1);
    }
}
