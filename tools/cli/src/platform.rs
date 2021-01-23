
mod linux;
mod windows;

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

impl Platform {
    pub const fn cradle_subdir_path(self) -> &'static str {
        match self {
            Self::Windows => "windows",
            Self::Mac => "mac",
            Self::Linux => "linux",
            Self::Android => "android"
        }
    }

    pub fn build(
        self,
        userlib: &Path, features: &[String], update_deps: bool, after_run: bool,
        ext_asset_path: Option<&Path>, entry_ty_name: &str, appid: &str
    ) {
        match self {
            Self::Windows => self::windows::build(
                userlib,
                features,
                update_deps,
                if after_run { "run" } else { "build" },
                ext_asset_path,
                entry_ty_name,
                appid
            ),
            Self::Mac => todo!("Build Process for Mac"),
            Self::Linux => self::linux::build(userlib, features, update_deps, after_run, ext_asset_path),
            Self::Android => todo!("Build Process for Android")
        }
    }

    pub fn gen_manifest(self, userlib: &Path, features: Vec<&str>) {
        let user_manifest_loaded = std::fs::read_to_string(userlib.join("Cargo.toml"))
            .expect("Failed to load Userlib Cargo.toml");
        let user_manifest: crate::manifest::CargoManifest = toml::from_str(&user_manifest_loaded)
            .expect("Failed to parse Userlib Cargo.toml");
        let project_name = user_manifest.package.as_ref().and_then(|p| p.name.as_deref())
            .unwrap_or("?Unnamed Project?");
        
        crate::steps::run_steps(self.cradle_subdir_path(), std::iter::once(crate::steps::BuildStep::GenManifest {
            userlib_path: userlib,
            userlib_name: project_name,
            features
        }));
    }
}

/// Find cradles base directory
pub fn cradle_directory() -> PathBuf {
    if let Ok(b) = std::env::var("PERIDOT_CLI_CRADLE_BASE") {
        PathBuf::from(b)
    } else {
        // Note: dev-packageのフォルダ構造に依存しているので、そっちを変えたらこっちも変える
        std::env::current_exe().expect("Failed to query exe path").join("../cradle")
    }
}
