use std::path::PathBuf;

use crate::model::{ApplicationAccess, ApplicationMutableAccess, ViewFeedback};

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

pub enum FileEntryType {
    File,
    Directory(PathBuf),
}

pub struct FileEntry {
    pub name: String,
    pub r#type: FileEntryType,
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
                r#type: if e.metadata().is_ok_and(|e| e.is_dir()) {
                    FileEntryType::Directory(e.path())
                } else {
                    FileEntryType::File
                },
            }
        })
}

pub fn interact(state: &mut (impl ApplicationMutableAccess + ?Sized), etype: &FileEntryType) {
    match etype {
        FileEntryType::File => {}
        FileEntryType::Directory(path) => {
            state.application_mut().asset_explorer.current_dir = path.clone();
            state.dispatch_view_feedback(ViewFeedback::asset_explorer_current_directory_changed());
        }
    }
}

pub struct ViewFeedbackCurrentDirectoryChanged;
