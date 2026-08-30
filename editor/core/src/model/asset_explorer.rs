use std::path::PathBuf;

use crate::model::ApplicationAccess;

pub(super) struct State {
    current_dir: PathBuf,
}
impl State {
    pub(super) fn new() -> Self {
        Self {
            current_dir: std::env::current_dir().expect("current_dir"),
        }
    }
}

pub struct FileEntry {
    pub name: String,
}

pub fn current_dir_entries(
    state: &(impl ApplicationAccess + ?Sized),
) -> impl Iterator<Item = FileEntry> {
    std::fs::read_dir(&state.application().asset_explorer.current_dir)
        .expect("read_dir")
        .map(|e| {
            let e = e.expect("read_dir.iter");

            FileEntry {
                name: e.file_name().into_string().expect("invalid file name"),
            }
        })
}
