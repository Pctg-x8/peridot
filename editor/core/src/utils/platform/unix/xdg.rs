//! Cross Desktop Group related definitions

use std::path::PathBuf;

/// `$XDG_STATE_HOME` or default
pub fn state_home() -> PathBuf {
    std::env::var_os("XDG_STATE_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|| super::home_dir().join(".local/state"))
}

/// `$XDG_CACHE_HOME` or default
pub fn cache_home() -> PathBuf {
    std::env::var_os("XDG_CACHE_HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|| super::home_dir().join(".cache"))
}
