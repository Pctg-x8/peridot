//! Opening Project Data

use std::path::PathBuf;

use crate::model::ApplicationAccess;

pub(super) struct State {
    pub(super) name: String,
    pub(super) root_dir: PathBuf,
}
impl State {
    pub(super) fn new() -> Self {
        // TODO: このあたり外部から与えて直接初期化できるようにしたいかも
        Self {
            name: "New Project".into(),
            root_dir: std::env::current_dir().expect("current_dir"),
        }
    }
}

pub fn name(state: &(impl ApplicationAccess + ?Sized)) -> &str {
    &state.application().project.name
}
