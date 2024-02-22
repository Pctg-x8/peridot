mod android;
mod linux;
mod mac;
mod windows;

use std::path::{Path, PathBuf};
use structopt::clap::arg_enum;

use crate::{project::PlatformConfiguration, subcommands::build::BuildMode};

arg_enum! {
    #[derive(Debug, Clone, Copy)]
    pub enum Platform {
        Windows,
        Mac,
        Linux,
        Android
    }
}
impl Platform {
    pub const fn runtime() -> Option<Self> {
        if cfg!(windows) {
            Some(Self::Windows)
        } else if cfg!(target_os = "macos") {
            Some(Self::Mac)
        } else if cfg!(target_os = "linux") {
            Some(Self::Linux)
        } else if cfg!(target_os = "android") {
            Some(Self::Android)
        } else {
            None
        }
    }
}

pub struct BuildOptions<'s> {
    pub userlib: &'s Path,
    pub features: Vec<&'s str>,
    pub engine_features: Vec<&'s str>,
    pub update_deps: bool,
    pub ext_asset_path: Option<std::borrow::Cow<'s, Path>>,
    pub entry_ty_name: &'s str,
    pub appid: &'s str,
    pub fast_build: bool,
    pub release: bool,
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
            release: false,
        }
    }
}

impl Platform {
    pub const fn identifier(self) -> &'static str {
        match self {
            Self::Windows => "windows",
            Self::Mac => "mac",
            Self::Linux => "linux",
            Self::Android => "android",
        }
    }

    pub const fn cradle_name(self) -> &'static str {
        match self {
            Self::Windows => "windows",
            Self::Mac => "mac",
            Self::Linux => "linux",
            Self::Android => "android",
        }
    }

    pub fn build(
        self,
        options: &BuildOptions,
        project_config: &PlatformConfiguration,
        build_mode: BuildMode,
    ) {
        match self {
            Self::Windows => self::windows::build(options, project_config, build_mode),
            Self::Mac => self::mac::build(options, project_config, build_mode),
            Self::Linux => self::linux::build(options, project_config, build_mode),
            Self::Android => self::android::build(options, build_mode),
        }
    }

    pub fn gen_manifest(self, userlib: &Path, features: Vec<&str>) {
        let user_manifest_loaded = std::fs::read_to_string(userlib.join("Cargo.toml"))
            .expect("Failed to load Userlib Cargo.toml");
        let user_manifest: crate::manifest::CargoManifest =
            toml::from_str(&user_manifest_loaded).expect("Failed to parse Userlib Cargo.toml");
        let project_name = user_manifest
            .package
            .as_ref()
            .and_then(|p| p.name.as_deref())
            .unwrap_or("?Unnamed Project?");

        crate::steps::gen_manifest(
            &crate::steps::BuildContext::new(self.cradle_name()),
            userlib,
            project_name,
            features,
        );
    }
}

/// Find cradles base directory
pub fn cradle_directory() -> PathBuf {
    if let Ok(b) = std::env::var("PERIDOT_CLI_CRADLE_BASE") {
        PathBuf::from(b)
    } else {
        // Note: dev-packageのフォルダ構造に依存しているので、そっちを変えたらこっちも変える
        std::env::current_exe()
            .expect("Failed to query exe path")
            .parent()
            .expect("no parent?")
            .join("../cradle")
    }
}

pub fn print_start_build(platform_name: &str, project_name: &str) {
    println!(
        "🛠  Building Project {} for {} Deployment...",
        console::style(project_name).bold().fg(console::Color::Cyan),
        console::style(platform_name)
            .fg(console::Color::Yellow)
            .bright()
    );
}
