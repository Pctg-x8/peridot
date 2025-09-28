use crate::platform::Platform;
use std::path::PathBuf;

/// Builds an game
#[derive(clap::Parser, Debug)]
pub struct Args {
    /// Path to userlib crate
    userlib_path: PathBuf,
    /// Target Platform
    #[arg(long, short = 'p', value_enum, required = true)]
    platform: Vec<Platform>,
    /// Userlib features
    #[arg(long, short = 'f')]
    feature: Vec<String>,
}

pub fn run(args: Args) {
    for p in args.platform {
        p.gen_manifest(
            &args.userlib_path,
            args.feature.iter().map(|s| s as &str).collect(),
        );
    }
}
