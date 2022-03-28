use crate::{
    platform::Platform,
    project::{PlatformConfiguration, Project},
};
use std::path::PathBuf;
use structopt::StructOpt;

/// Builds an game
#[derive(StructOpt, Debug)]
pub struct Args {
    /// Path to userlib crate
    userlib_path: PathBuf,
    /// Target Platform
    #[structopt(long, short = "p", possible_values = &Platform::variants(), case_insensitive = true, required = true)]
    platform: Vec<Platform>,
    /// Entry TypeName(default: Game)
    #[structopt(long, short = "e")]
    entry_ty_name: Option<String>,
    /// Asset Directory
    #[structopt(long, short = "a")]
    asset_directory: Option<PathBuf>,
    /// Userlib features
    #[structopt(long, short = "f")]
    feature: Option<Vec<String>>,
    /// Engine features
    #[structopt(long, short = "F")]
    engine_feature: Option<Vec<String>>,
    /// Update dependencies(cargo update) before build
    #[structopt(long, short = "u")]
    update_deps: bool,
    /// Application Identifier
    #[structopt(long = "appid")]
    app_package_id: Option<String>,
    /// Use fast build(no copy of builtin-assets)
    #[structopt(long)]
    fast_build: bool,
    /// Release build
    #[structopt(long)]
    release: bool,
}
impl Args {
    pub fn to_build_options<'s>(
        &'s self,
        project_config: PlatformConfiguration<'s>,
    ) -> crate::platform::BuildOptions<'s> {
        let mut engine_features: Vec<_> = self
            .engine_feature
            .as_deref()
            .unwrap_or(project_config.engine_features)
            .iter()
            .map(|s| s as &str)
            .collect();
        if !self.release {
            engine_features.push("debug");
        }

        crate::platform::BuildOptions {
            userlib: &self.userlib_path,
            features: self
                .feature
                .as_deref()
                .unwrap_or(project_config.features)
                .iter()
                .map(|s| s as &str)
                .collect(),
            engine_features,
            update_deps: self.update_deps,
            ext_asset_path: self
                .asset_directory
                .as_deref()
                .map(std::borrow::Cow::Borrowed)
                .or(project_config
                    .asset_dir
                    .map(|p| std::borrow::Cow::Owned(self.userlib_path.join(p)))),
            entry_ty_name: self
                .entry_ty_name
                .as_deref()
                .or(project_config.entry_type_name)
                .unwrap_or("Game"),
            appid: self
                .app_package_id
                .as_deref()
                .unwrap_or(project_config.app_package_id),
            fast_build: self.fast_build,
            release: self.release,
        }
    }

    pub fn project_config_path(&self) -> PathBuf {
        self.userlib_path.join("peridot.toml")
    }
}

#[derive(StructOpt, Debug)]
pub struct BuildArgs {
    #[structopt(flatten)]
    base: Args,
    /// Run after build
    #[structopt(long, short = "r")]
    run: bool,
}

pub fn run(args: BuildArgs) {
    let project: Project = toml::from_str(
        &std::fs::read_to_string(args.base.project_config_path())
            .expect("Failed to load project configuration"),
    )
    .expect("Invalid project configuration");

    for p in &args.base.platform {
        let options = args
            .base
            .to_build_options(project.resolve_config(p.identifier()));
        p.build(&options, if args.run { "run" } else { "build" });
    }
}

pub fn run_check(args: Args) {
    let project: Project = toml::from_str(
        &std::fs::read_to_string(args.project_config_path())
            .expect("Failed to load project configuration"),
    )
    .expect("Invalid project configuration");

    for p in &args.platform {
        let options = args.to_build_options(project.resolve_config(p.identifier()));
        p.build(&options, "check");
    }
}

pub fn run_test(args: Args) {
    let project: Project = toml::from_str(
        &std::fs::read_to_string(args.project_config_path())
            .expect("Failed to load project configuration"),
    )
    .expect("Invalid project configuration");

    for p in &args.platform {
        let options = args.to_build_options(project.resolve_config(p.identifier()));
        p.build(&options, "test");
    }
}
