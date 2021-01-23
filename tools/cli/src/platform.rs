
mod linux;

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
    pub fn build(
        self,
        userlib: &Path, features: &[String], update_deps: bool, after_run: bool,
        ext_asset_path: Option<&Path>
    ) {
        match self {
            Self::Windows => todo!("Build Process for Windows"),
            Self::Mac => todo!("Build Process for Mac"),
            Self::Linux => self::linux::build(userlib, features, update_deps, after_run, ext_asset_path),
            Self::Android => todo!("Build Process for Android")
        }
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
