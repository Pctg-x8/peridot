//! Peridot Tools Path

use std::path::PathBuf;

/// absolute path to peridot-archiver
pub fn archiver_path() -> PathBuf {
    if let Some(p) = std::env::var_os("PERIDOT_CLI_ARCHIVER_PATH") {
        PathBuf::from(p)
    } else {
        // Note: dev-packageのフォルダ構造に依存しているので、そっちを変えたらこっちも変える
        current_exe().with_file_name("peridot-archiver")
    }
}

/// absolute path to builtin assets
pub fn builtin_assets_path() -> PathBuf {
    if let Some(p) = std::env::var_os("PERIDOT_CLI_BUILTIN_ASSETS_PATH") {
        PathBuf::from(p)
    } else {
        // Note: dev-packageのフォルダ構成に依存しているので、そっちを変えたらこっちも変える
        current_exe()
            .parent()
            .expect("no parent?")
            .join("../builtin-assets")
    }
}

#[inline(always)]
fn current_exe() -> PathBuf {
    std::env::current_exe().expect("Failed to get exe path")
}
