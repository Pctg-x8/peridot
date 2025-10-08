use std::path::PathBuf;

use clap::Parser;

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
    let res = peridot_asset_processing::process(
        &[
            Box::new(peridot_rendering_configuration::AssetProcessor),
            Box::new(peridot_asset_processing::builtin::ImageAssetProcessor),
            Box::new(peridot_asset_processing::builtin::SoundAssetProcessor),
        ],
        &args.source_path,
        peridot_asset_processing::ProcessOptions {
            out_dir: args.out_dir.as_deref(),
            force_rebuild: args.force_rebuild,
        },
    );
    if res.is_none() {
        tracing::error!("Error in processing asset");
        std::process::exit(1);
    }
}
