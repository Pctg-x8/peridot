
use std::path::PathBuf;
use structopt::StructOpt;
use crate::platform::Platform;

/// Builds an game
#[derive(StructOpt, Debug)]
pub struct Args {
    /// Path to userlib crate
    userlib_path: PathBuf,
    /// Target Platform
    #[structopt(long, short = "p", possible_values = &Platform::variants(), case_insensitive = true, required = true)]
    platform: Vec<Platform>,
    /// Userlib features
    #[structopt(long, short = "f")]
    feature: Vec<String>,
}

pub fn run(args: Args) {
    for p in args.platform {
        p.gen_manifest(&args.userlib_path, args.feature.iter().map(|s| s as &str).collect());
    }
}
