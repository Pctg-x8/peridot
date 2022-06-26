//! Build Steps

use std::collections::HashMap;
use std::io::Write;
use std::path::{Path, PathBuf};

use crate::project::WindowExtents;

pub struct BuildContext<'s> {
    pub cradle_directory: PathBuf,
    cradle_name: &'s str,
}
impl<'s> BuildContext<'s> {
    pub fn new(cradle_name: &'s str) -> Self {
        Self {
            cradle_name,
            cradle_directory: crate::platform::cradle_directory().join(cradle_name),
        }
    }

    pub fn print_step(&self, text: &str) {
        println!(
            " {} [{}] {}",
            console::style("*").fg(console::Color::Green).bold(),
            console::style(self.cradle_name)
                .fg(console::Color::White)
                .bright(),
            text
        );
    }
    pub fn cwd_cradle_dir(&self) {
        std::env::set_current_dir(&self.cradle_directory)
            .expect("Failed to change current directory");
    }
    pub fn within_cradle_dir<F: FnOnce()>(&self, exec: F) {
        let org_directory =
            std::env::current_dir().expect("Failed to get current working directory");
        self.cwd_cradle_dir();

        exec();

        std::env::set_current_dir(org_directory).expect("Failed to rollback org directory");
    }
}

pub fn gen_manifest(
    ctx: &BuildContext,
    userlib_path: &Path,
    userlib_name: &str,
    features: Vec<&str>,
) {
    ctx.print_step("Generating manifest...");

    crate::manifest::gen_manifest(
        &ctx.cradle_directory.join("Cargo.toml"),
        &ctx.cradle_directory.join("Cargo.template.toml"),
        &std::fs::canonicalize(userlib_path).expect("Failed to canonicalize userlib path"),
        userlib_name,
        features,
    );
}
pub fn gen_userlib_import_code(
    ctx: &BuildContext,
    userlib_name: &str,
    userlib_title: &str,
    userlib_version: &semver::Version,
    entry_ty_name: &str,
    default_extents: &WindowExtents,
) {
    ctx.print_step("Generating Userlib Entry code...");

    let mut w = std::fs::OpenOptions::new()
        .create(true)
        .truncate(true)
        .write(true)
        .open(ctx.cradle_directory.join("src/userlib.rs"))
        .expect("Failed to open userlib.rs");
    writeln!(
        w,
        "//! Auto Generated by Build script

pub use {}::{entry_ty_name} as Game;
pub const APP_IDENTIFIER: &'static str = {userlib_name:?};
pub const APP_TITLE: &'static str = {userlib_title:?};
pub const APP_VERSION: (u32, u32, u32) = ({}, {}, {});
pub const APP_DEFAULT_EXTENTS: peridot::WindowExtents = {}",
        userlib_name.replace('-', "_"),
        userlib_version.major,
        userlib_version.minor,
        userlib_version.patch,
        default_extents.map_peridot_code()
    )
    .expect("Failed to write userlib.rs");
}
pub fn update_deps(ctx: &BuildContext) {
    ctx.print_step("Updating dependencies...");

    let e = std::process::Command::new("cargo")
        .args(&["update"])
        .spawn()
        .expect("Failed to spawn `cargo update`")
        .wait()
        .expect("Failed to wait `cargo update`");
    crate::shellutil::handle_process_result("`cargo update`", e);
}

pub struct Cargo<'x> {
    ctx: &'x BuildContext<'x>,
    ext_features: Vec<&'x str>,
    env: HashMap<&'x str, &'x str>,
    target_spec: Option<&'x str>,
    release: bool,
}
impl<'x> Cargo<'x> {
    pub fn with_ext_features(mut self, ext_features: Vec<&'x str>) -> Self {
        self.ext_features = ext_features;
        self
    }

    pub fn with_env(mut self, env: HashMap<&'x str, &'x str>) -> Self {
        self.env = env;
        self
    }

    pub fn with_target_spec(mut self, spec: &'x str) -> Self {
        self.target_spec = Some(spec);
        self
    }

    pub fn enable_release_build(mut self) -> Self {
        self.release = true;
        self
    }

    fn run_raw_subcommand(self, subcommand: &str) {
        self.ctx.print_step("Compiling code...");

        let mut cmd = std::process::Command::new("cargo");
        cmd.arg(subcommand).envs(self.env);
        if let Some(t) = self.target_spec {
            cmd.args(&["--target", t]);
        }
        if !self.ext_features.is_empty() {
            let ext_features = self.ext_features.join(",");
            cmd.args(vec![String::from("--features"), ext_features]);
        }
        if self.release {
            cmd.args(&["--release"]);
        }

        let e = cmd
            .spawn()
            .expect("Failed to spawn cargo command")
            .wait()
            .expect("Failed to wait cargo command");
        crate::shellutil::handle_process_result("cargo command", e);
    }

    pub fn build(self) {
        self.run_raw_subcommand("build");
    }

    pub fn run(self) {
        self.run_raw_subcommand("run");
    }

    pub fn test(self) {
        self.run_raw_subcommand("test");
    }

    pub fn check(self) {
        self.run_raw_subcommand("check");
    }
}

pub fn cargo<'x>(ctx: &'x BuildContext) -> Cargo<'x> {
    Cargo {
        ctx,
        ext_features: Vec::new(),
        env: HashMap::new(),
        target_spec: None,
        release: false,
    }
}

pub fn package_assets(ctx: &BuildContext, asset_path: Option<&Path>, output_path: &Path) {
    // Prerequired build step
    let stg_path = std::env::temp_dir().join(".peridot/build/assets");
    merge_assets(ctx, &stg_path, asset_path);

    ctx.print_step("Packaging assets...");

    let mut basedir_str = String::from(stg_path.to_str().expect("invalid sequence in asset path"));
    if !basedir_str.ends_with("/") {
        basedir_str.push('/');
    }
    let e = std::process::Command::new(crate::path::archiver_path())
        .args(&[
            "new",
            stg_path.to_str().expect("invalid sequence in asset path"),
            "-o",
            output_path
                .to_str()
                .expect("invalid sequence in output path"),
            "-b",
            &basedir_str,
        ])
        .spawn()
        .expect("Failed to spawn peridot-archive")
        .wait()
        .expect("Failed to wait peridot-archive");
    crate::shellutil::handle_process_result("peridot-archive", e);
}
pub fn merge_assets(ctx: &BuildContext, stg_directory_path: &Path, user_assets: Option<&Path>) {
    ctx.print_step("Merging assets...");

    if !stg_directory_path.exists() {
        std::fs::create_dir_all(stg_directory_path).expect("Failed to create asset stg directory");
    }
    if let Some(p) = user_assets {
        crate::shellutil::handle_process_result(
            "asset sync command",
            crate::shellutil::sh_mirror(p, stg_directory_path, &[])
                .expect("Failed to run mirror command"),
        );
    }
    crate::shellutil::handle_process_result(
        "builtin asset sync command",
        crate::shellutil::sh_mirror(
            &crate::path::builtin_assets_path(),
            &stg_directory_path.join("builtin"),
            &["Makefile"],
        )
        .expect("Failed to run mirror command"),
    );
}
