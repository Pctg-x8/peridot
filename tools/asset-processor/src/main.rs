use std::path::PathBuf;

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

fn main() {
    tracing_subscriber::fmt()
        .pretty()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let args = Args::parse();
    let span = tracing::span!(
        tracing::Level::INFO,
        "main",
        source_path = %args.source_path.display(),
    );
    let _span_enter = span.enter();

    let ext = args.source_path.extension();
    if ext.is_some_and(|x| x == "prc") {
        // rendering configuration: just compile
        let dest_path = args
            .out_dir
            .as_deref()
            .unwrap_or_else(|| args.source_path.parent().expect("no parent?"))
            .join(
                args.source_path
                    .file_name()
                    .expect("no file name in source path"),
            )
            .with_extension("pa1-rendering-configuration");
        if !args.force_rebuild
            && let (Ok(x), Ok(y)) = (args.source_path.metadata(), dest_path.metadata())
            && x.modified().unwrap() <= y.modified().unwrap()
        {
            tracing::info!(reason = "modified time", "skip asset");
            return;
        }

        let mut process = match std::process::Command::new(
            std::env::current_exe()
                .expect("current_exe")
                .parent()
                .expect("dirname")
                .join("peridot-rendering-configuration-compiler"),
        )
        .arg(&args.source_path)
        .arg("--output")
        .arg(&dest_path)
        .spawn()
        {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to spawn peridot-rendering-configuration-compiler");
                std::process::exit(1);
            }
        };
        if let Err(e) = process.wait() {
            tracing::error!(reason = ?e, "process.wait failed");
            std::process::exit(1);
        }
    } else if ext.is_some_and(|x| x == "png" || x == "jpg" || x == "tiff") {
        // image asset: decompress to rgba and recompress(TODO: if needed, specified by metadata file)
        let dest_path = args
            .out_dir
            .as_deref()
            .unwrap_or_else(|| args.source_path.parent().expect("no parent?"))
            .join(
                args.source_path
                    .file_name()
                    .expect("no file name in source path"),
            )
            .with_extension("pa1-texture2d");
        if !args.force_rebuild
            && let (Ok(x), Ok(y)) = (args.source_path.metadata(), dest_path.metadata())
            && x.modified().unwrap() <= y.modified().unwrap()
        {
            tracing::info!(reason = "modified time", "skip asset");
            return;
        }

        let img = match image::open(&args.source_path) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to open asset");
                std::process::exit(1);
            }
        };

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
        .expect("failed to initialize ktxTexture2");
        ktx.set_image_from_memory(0, 0, 0, img.to_rgba8().as_raw())
            .expect("ktx.set_image_from_memory failed");
        ktx.compress_basis_ex(
            &mut ktx::BasisParams::new()
                .uastc()
                .uastc_flags(ktx::ffi::KTX_PACK_UASTC_LEVEL_DEFAULT)
                .uastc_rdo(),
        )
        .expect("ktx.compress_basis_ex failed");
        ktx.deflate_zstd(11).expect("ktx.deflate_zstd failed");
        ktx.write_to_named_file(
            &std::ffi::CString::new(dest_path.to_str().expect("invalid utf-8 seq"))
                .expect("invalid cstr seq"),
        )
        .expect("ktx.write_to_named_file failed");
    } else if ext.is_some_and(|x| x == "wav" || x == "mp3" || x == "ogg" || x == "flac") {
        // sound asset(TODO: convert to what?)
        let dest_path = args
            .out_dir
            .as_deref()
            .unwrap_or_else(|| args.source_path.parent().expect("no parent?"))
            .join(
                args.source_path
                    .file_name()
                    .expect("no file name in source path"),
            )
            .with_extension("pa1-audio");
        if !args.force_rebuild
            && let (Ok(x), Ok(y)) = (args.source_path.metadata(), dest_path.metadata())
            && x.modified().unwrap() <= y.modified().unwrap()
        {
            tracing::info!(reason = "modified time", "skip asset");
            return;
        }

        if let Err(e) = std::fs::copy(&args.source_path, &dest_path) {
            tracing::error!(reason = ?e, "Failed to copy asset file");
            std::process::exit(1);
        }
    } else {
        // unknown assets
        tracing::warn!("found unknown assets(not processed)");
        let dest_path = args
            .out_dir
            .as_deref()
            .unwrap_or_else(|| args.source_path.parent().expect("no parent?"))
            .join(
                args.source_path
                    .file_name()
                    .expect("no file name in source path"),
            );

        if let Err(e) = std::fs::copy(&args.source_path, &dest_path) {
            tracing::error!(reason = ?e, "Failed to copy asset file");
            std::process::exit(1);
        }
    }
}
