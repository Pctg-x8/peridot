//! Peridot Tools Path

use std::path::PathBuf;

/// absolute path to peridot-archiver
pub fn archiver_path() -> PathBuf {
    if let Ok(p) = std::env::var("PERIDOT_CLI_ARCHIVER_PATH") {
        PathBuf::from(p)
    } else {
        // Note: dev-packageのフォルダ構造に依存しているので、そっちを変えたらこっちも変える
        std::env::current_exe().expect("Failed to get exe path").with_file_name("peridot-archiver")
    }
}

/// absolute path to builtin assets
pub fn builtin_assets_path() -> PathBuf {
    if let Ok(p) = std::env::var("PERIDOT_CLI_BUILTIN_ASSETS_PATH") {
        PathBuf::from(p)
    } else {
        // Note: dev-packageのフォルダ構成に依存しているので、そっちを変えたらこっちも変える
        std::env::current_exe().expect("Failed to get exe path").parent().expect("no parent?").join("../builtin-assets")
    }
}
