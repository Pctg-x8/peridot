
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
    /// Asset Directory
    #[structopt(long, short = "a")]
    asset_directory: Option<PathBuf>,
    /// Userlib features
    #[structopt(long, short = "f")]
    feature: Vec<String>,
    /// Engine features
    #[structopt(long, short = "F")]
    engine_feature: Vec<String>,
    /// Update dependencies(cargo update) before build
    #[structopt(long, short = "u")]
    update_deps: bool,
    /// Application Identifier
    #[structopt(long = "appid", default_value = "jp.ct2.peridot")]
    app_package_id: String
}
impl Args {
    pub fn to_build_options(&self) -> crate::platform::BuildOptions {
        crate::platform::BuildOptions {
            userlib: &self.userlib_path,
            features: self.feature.iter().map(|s| s as &str).collect(),
            engine_features: self.engine_feature.iter().map(|s| s as &str).collect(),
            update_deps: self.update_deps,
            ext_asset_path: self.asset_directory.as_deref(),
            entry_ty_name: &self.entry_ty_name,
            appid: &self.app_package_id
        }
    }
}

#[derive(StructOpt, Debug)]
pub struct BuildArgs {
    #[structopt(flatten)]
    base: Args,
    /// Run after build
    #[structopt(long, short = "r")]
    run: bool
}

pub fn run(args: BuildArgs) {
    let options = args.base.to_build_options();

    for p in &args.base.platform {
        p.build(&options, if args.run { "run" } else { "build" });
    }
}

pub fn run_check(args: Args) {
    let options = args.to_build_options();

    for p in &args.platform {
        p.build(&options, "check");
    }
}

pub fn run_test(args: Args) {
    let options = args.to_build_options();

    for p in &args.platform {
        p.build(&options, "test");
    }
}
