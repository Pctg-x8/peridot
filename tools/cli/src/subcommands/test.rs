
use std::path::PathBuf;
use structopt::StructOpt;
use crate::platform::Platform;

/// Test game code
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
    /// Userlib features
    #[structopt(long, short = "f")]
    feature: Vec<String>,
    /// Update dependencies(cargo update) before build
    #[structopt(long, short = "u")]
    update_deps: bool,
    /// Application Identifier
    #[structopt(long, default_value = "jp.ct2.peridot")]
    app_package_id: String
}

pub fn run(args: Args) {
    let options = crate::platform::BuildOptions {
        userlib: &args.userlib_path,
        features: args.feature.iter().map(|s| s as &str).collect(),
        update_deps: args.update_deps,
        ext_asset_path: args.asset_directory.as_deref(),
        entry_ty_name: &args.entry_ty_name,
        appid: &args.app_package_id
    };

    for p in args.platform {
        p.build(&options, "test");
    }
}
