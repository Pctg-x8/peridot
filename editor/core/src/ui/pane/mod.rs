#[macro_export]
macro_rules! internal_pane_identifier {
    ($name: literal) => {
        concat!("io.ct2.peridot.editor.internal.pane.", $name)
    };
}

pub mod inspector;
pub mod object_tree;
