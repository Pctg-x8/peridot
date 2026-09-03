use std::path::{Path, PathBuf};

use crate::model::{ApplicationAccess, ApplicationMutableAccess};

pub(super) struct State {
    breadcumbs: Vec<String>,
}
impl State {
    pub(super) fn new() -> Self {
        Self {
            breadcumbs: vec!["Project Root".into()],
        }
    }
}

pub fn breadcumb_elements(state: &(impl ApplicationAccess + ?Sized)) -> &[String] {
    &state.application().asset_explorer.breadcumbs
}

pub fn move_dir_by_breadcumb_index(
    state: &mut (impl ApplicationMutableAccess + ?Sized),
    index: usize,
) {
    state
        .application_mut()
        .asset_explorer
        .breadcumbs
        .truncate(index + 1);
    state.dispatch_view_feedback(ViewFeedbackCurrentDirectoryChanged);
}

pub fn breadcumb_next_directory_list(
    state: &(impl ApplicationAccess + ?Sized),
    index: usize,
) -> impl Iterator<Item = String> {
    let app = state.application();
    let base_dir = app
        .asset_explorer
        .breadcumbs
        .iter()
        .skip(1)
        .take(index)
        .fold(app.project.root_dir.clone(), |a, b| a.join(b));

    std::fs::read_dir(base_dir)
        .expect("read_dir")
        .filter_map(|e| {
            let e = e.expect("read_dir.element");

            if e.metadata().expect("read_dir.element.metadata").is_dir() {
                Some(e.file_name().into_string().expect("invalid str"))
            } else {
                None
            }
        })
}

pub fn current_path(state: &(impl ApplicationAccess + ?Sized)) -> PathBuf {
    let app = state.application();

    app.asset_explorer
        .breadcumbs
        .iter()
        .skip(1)
        .fold(app.project.root_dir.clone(), |a, b| a.join(b))
}

pub enum FileEntryType {
    File,
    Directory,
}

pub struct FileEntry {
    pub name: String,
    pub r#type: FileEntryType,
}

pub fn current_dir_entries(
    state: &(impl ApplicationAccess + ?Sized),
) -> impl Iterator<Item = FileEntry> {
    std::fs::read_dir(current_path(state))
        .expect("read_dir")
        .map(|e| {
            let e = e.expect("read_dir.iter");

            FileEntry {
                name: e.file_name().into_string().expect("invalid file name"),
                r#type: if e.metadata().is_ok_and(|e| e.is_dir()) {
                    FileEntryType::Directory
                } else {
                    FileEntryType::File
                },
            }
        })
}

pub fn interact(state: &mut (impl ApplicationMutableAccess + ?Sized), e: &FileEntry) {
    match e.r#type {
        FileEntryType::File => {}
        FileEntryType::Directory => {
            state
                .application_mut()
                .asset_explorer
                .breadcumbs
                .push(e.name.clone());
            state.dispatch_view_feedback(ViewFeedbackCurrentDirectoryChanged);
        }
    }
}

pub struct ViewFeedbackCurrentDirectoryChanged;
