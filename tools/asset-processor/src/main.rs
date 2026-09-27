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
    peridot_asset_processing::process(
        &[
            Box::new(peridot_rendering_configuration::AssetProcessor),
            Box::new(peridot_asset_processing::builtin::ImageAssetProcessor),
            Box::new(peridot_asset_processing::builtin::SoundAssetProcessor),
            Box::new(peridot_asset_processor_model::Processor),
        ],
        &mut peridot_asset_processing::AssetProcessContext {
            assetdb: peridot::AssetDatabase::open("test.adb").expect("assetdb.open"),
            asset_id_generator: peridot::AssetIDGenerator::new(),
        },
        args.source_path.parent().expect("not a regular input"),
        &args.source_path,
        args.out_dir
            .unwrap_or_else(|| std::env::current_dir().expect("current_dir")),
        peridot_asset_processing::ProcessOptions {
            force_rebuild: args.force_rebuild,
        },
    )
    .expect("Error in processing asset");
}
