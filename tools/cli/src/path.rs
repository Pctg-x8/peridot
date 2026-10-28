//! Peridot Tools Path

use std::path::{Path, PathBuf};

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
pub fn vulkan_sdk_path() -> PathBuf {
    PathBuf::from(std::env::var_os("VULKAN_SDK").expect("VULKAN_SDK not set"))
}

#[inline(always)]
fn current_exe() -> PathBuf {
    std::env::current_exe().expect("Failed to get exe path")
}

/// ビルド中に生成されるRuntime Assetの格納パス
///
/// 基本的にはプロジェクトごとに生成される（UnityのLibraryとかと同じ扱い）
pub fn generated_runtime_assets_dir(userlib_path: impl AsRef<Path>) -> PathBuf {
    userlib_path.as_ref().join(".runtime-assets")
}
