use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

pub mod builtin;

/// An asset processor interface.
pub trait AssetProcessor {
    /// Determines if this processor can handle the given source file.
    fn can_process(&self, source_path: &Path) -> bool;

    /// Constructs the destination path for the processed asset based on the source file name and output directory.
    fn dest_path(&self, source_file_name: &OsStr, out_dir_path: &Path) -> PathBuf;

    /// Processes the asset from the source path.
    ///
    /// Expected some files to be created and placed in `out_path`.
    fn process(
        &self,
        source_path: &Path,
        out_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>>;
}

pub struct ProcessOptions<'p> {
    pub out_dir: Option<&'p Path>,
    pub force_rebuild: bool,
}
impl<'p> Default for ProcessOptions<'p> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            out_dir: None,
            force_rebuild: false,
        }
    }
}

#[tracing::instrument(skip(processors, options), fields(source_path = %source_path.as_ref().display()))]
pub fn process(
    processors: &[Box<dyn AssetProcessor>],
    source_path: impl AsRef<Path>,
    options: ProcessOptions,
) -> Option<PathBuf> {
    let (Some(source_dir), Some(source_file_name)) = (
        source_path.as_ref().parent(),
        source_path.as_ref().file_name(),
    ) else {
        tracing::error!("invalid source file path provided");
        return None;
    };
    let dest_dir = options.out_dir.unwrap_or(source_dir);

    let mut matching_processors_iter = processors
        .iter()
        .filter(|x| x.can_process(source_path.as_ref()));
    let Some(processor) = matching_processors_iter.next() else {
        // unknown assets
        tracing::warn!("found unknown assets(not processed)");
        let dest_path = dest_dir.join(source_file_name);

        if let Err(e) = std::fs::copy(source_path, &dest_path) {
            tracing::error!(reason = ?e, "Failed to copy asset file");
            return None;
        }

        return Some(dest_path);
    };
    if matching_processors_iter.next().is_some() {
        tracing::error!("Cannot determine an asset processor");
        return None;
    }

    let dest_path = processor.dest_path(source_file_name, dest_dir);
    if !options.force_rebuild
        && let (Ok(x), Ok(y)) = (
            source_path.as_ref().metadata().inspect_err(
                |e| tracing::warn!(reason = ?e, path = ?source_path.as_ref(), "retrieving metadata failed"),
            ),
            dest_path.metadata().inspect_err(
                |e| tracing::warn!(reason = ?e, path = ?dest_path, "retrieving metadata failed"),
            ),
        )
        && let (Ok(x), Ok(y)) = (
            x.modified().inspect_err(
                |e| tracing::warn!(reason = ?e, path = ?source_path.as_ref(), "retrieving modified date failed"),
            ),
            y.modified().inspect_err(
                |e| tracing::warn!(reason = ?e, path = ?dest_path, "retrieving modified date failed"),
            )
        )
        && x <= y
    {
        tracing::info!(reason = "modified time", "skip asset");
        return Some(dest_path);
    }

    if let Err(e) = processor.process(source_path.as_ref(), &dest_path) {
        tracing::error!(reason = ?e, "Failed to process asset");
        return None;
    }

    Some(dest_path)
}
