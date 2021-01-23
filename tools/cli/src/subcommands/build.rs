
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
    /// Entry TypeName
    #[structopt(long, short = "e", default_value = "Game")]
    entry_ty_name: String,
    /// Run after build
    #[structopt(long, short = "r")]
    run: bool,
    /// Asset Directory
    #[structopt(long, short = "a")]
    asset_directory: Option<PathBuf>,
    /// Rust features
    #[structopt(long, short = "f")]
    feature: Vec<String>,
    /// Update dependencies(cargo update) before build
    #[structopt(long, short = "u")]
    update_deps: bool
}

pub fn run(args: Args) {
    for p in args.platform {
        p.build(&args.userlib_path, &args.feature, args.update_deps, args.run, args.asset_directory.as_deref());
    }
}
