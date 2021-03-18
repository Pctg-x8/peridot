
mod linux;
mod windows;
mod android;
mod mac;

use std::path::{Path, PathBuf};
use structopt::clap::arg_enum;

arg_enum! {
    #[derive(Debug, Clone, Copy)]
    pub enum Platform {
        Windows,
        Mac,
        Linux,
        Android
    }
}

pub struct BuildOptions<'s> {
    pub userlib: &'s Path,
    pub features: Vec<&'s str>,
    pub engine_features: Vec<&'s str>,
    pub update_deps: bool,
    pub ext_asset_path: Option<&'s Path>,
    pub entry_ty_name: &'s str,
    pub appid: &'s str,
    pub fast_build: bool,
    pub release: bool
}
impl<'s> Default for BuildOptions<'s> {
    fn default() -> Self {
        BuildOptions {
            userlib: Path::new(""),
            features: Vec::new(),
            engine_features: Vec::new(),
            update_deps: false,
            ext_asset_path: None,
            entry_ty_name: "",
            appid: "",
            fast_build: false,
            release: false
        }
    }
}

impl Platform {
    pub const fn cradle_name(self) -> &'static str {
        match self {
            Self::Windows => "windows",
            Self::Mac => "mac",
            Self::Linux => "linux",
            Self::Android => "android"
        }
    }

    pub fn build(self, options: &BuildOptions, final_cargo_cmd: &str) {
        match self {
            Self::Windows => self::windows::build(options, final_cargo_cmd),
            Self::Mac => self::mac::build(options, final_cargo_cmd),
            Self::Linux => self::linux::build(options, final_cargo_cmd),
            Self::Android => self::android::build(options, final_cargo_cmd)
        }
    }

    pub fn gen_manifest(self, userlib: &Path, features: Vec<&str>) {
        let user_manifest_loaded = std::fs::read_to_string(userlib.join("Cargo.toml"))
            .expect("Failed to load Userlib Cargo.toml");
        let user_manifest: crate::manifest::CargoManifest = toml::from_str(&user_manifest_loaded)
            .expect("Failed to parse Userlib Cargo.toml");
        let project_name = user_manifest.package.as_ref().and_then(|p| p.name.as_deref())
            .unwrap_or("?Unnamed Project?");

        crate::steps::gen_manifest(
            &crate::steps::BuildContext::new(self.cradle_name()),
            userlib,
            project_name,
            features
        );
    }
}

/// Find cradles base directory
pub fn cradle_directory() -> PathBuf {
    if let Ok(b) = std::env::var("PERIDOT_CLI_CRADLE_BASE") {
        PathBuf::from(b)
    } else {
        // Note: dev-packageのフォルダ構造に依存しているので、そっちを変えたらこっちも変える
        std::env::current_exe().expect("Failed to query exe path").parent().expect("no parent?").join("../cradle")
    }
}

pub fn print_start_build(platform_name: &str, project_name: &str) {
    println!(
        "🛠  Building Project {} for {} Deployment...",
        console::style(project_name).bold().fg(console::Color::Cyan),
        console::style(platform_name).fg(console::Color::Yellow).bright()
    );
}
