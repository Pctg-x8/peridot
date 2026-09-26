//! Processing user assets into runtime assets.

use std::{
    collections::HashMap,
    ffi::OsStr,
    path::{Path, PathBuf},
};

pub mod builtin;
pub mod metadata;

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
        metadata: &HashMap<metadata::Key, String>,
        dest_dir: &Path,
        ctx: &mut AssetProcessContext,
    ) -> Result<(), Box<dyn std::error::Error>>;
}

pub fn build_runtime_asset_path(dest_dir: &Path, asset_id: &peridot::AssetID) -> PathBuf {
    let mut file_path = String::with_capacity(16 * 2);
    for byte in asset_id.as_bytes() {
        file_path.push_str(&format!("{:02x}", byte));
    }

    dest_dir.join(file_path)
}

pub struct AssetProcessContext {
    pub assetdb: peridot::AssetDatabase,
    pub asset_id_generator: peridot::AssetIDGenerator,
}
impl AssetProcessContext {
    pub fn register_or_update_asset_group(&mut self, source_path: &Path) -> peridot::AssetID {
        let new_group_id = self.asset_id_generator.generate();
        let source_last_modified = source_path
            .metadata()
            .inspect_err(|e| tracing::error!(reason = %e, "Failed to query source asset metadata"))
            .ok()
            .and_then(
                |x| x
                    .modified()
                    .inspect_err(|e| tracing::error!(reason = %e, "Failed to query source asset last modified time"))
                    .ok()
            )
            .unwrap_or_else(std::time::SystemTime::now)
            .duration_since(std::time::SystemTime::UNIX_EPOCH)
            .inspect_err(|e| tracing::error!(reason = %e, "Invalid source asset last modified date"))
            .map_or(0, |x| x.as_millis()) as i64;

        let mut stmt = self.assetdb.0.prepare("Insert into user_asset (source_path, group_id, last_processed) values (?, ?, ?) on conflict do update set last_processed = excluded.last_processed where excluded.last_processed > last_processed returning group_id, last_processed").expect("assetdb op prepare");
        stmt.bind_text(1, source_path.to_str().expect("invalid str"))
            .expect("stmt.bind_text");
        stmt.bind_blob(2, new_group_id.as_bytes())
            .expect("stmt.bind_blob");
        stmt.bind_i64(3, source_last_modified)
            .expect("stmt.bind_int");
        let has_next = stmt.step().expect("stmt.step");
        if !has_next {
            // no insertion occurred(file is too old)
            let mut stmt = self
                .assetdb
                .0
                .prepare("Select group_id from user_asset where source_path = ?")
                .expect("assetdb op prepare");
            stmt.bind_text(1, source_path.to_str().expect("invalid str"))
                .expect("stmt.bind_text");
            let has_next = stmt.step().expect("stmt.step");
            assert!(has_next);
            let group_id_len = stmt.column_bytes(0);
            assert_eq!(group_id_len, 16);
            let group_id = stmt.column_blob(0);

            return unsafe { peridot::AssetID::from_bytes(*group_id.cast()) };
        }

        let group_id_len = stmt.column_bytes(0);
        assert_eq!(group_id_len, 16);
        let group_id = stmt.column_blob(0);
        let last_processed = stmt.column_i64(1);

        eprintln!(
            "source_path: {source_path:?}, group_id: {:?}(new {new_group_id:?}), last_processed: {last_processed}",
            unsafe { peridot::AssetID::from_bytes(*group_id.cast()) }
        );
        unsafe { peridot::AssetID::from_bytes(*group_id.cast()) }
    }

    pub fn register_or_update_child_asset(
        &mut self,
        group_id: &peridot::AssetID,
        asset_type: AssetType,
        local_id: i32,
    ) -> peridot::AssetID {
        let new_asset_id = self.asset_id_generator.generate();
        let mut stmt = self.assetdb
            .0
            .prepare(
                "Insert into asset_group (id, asset_type, local_id, runtime_asset_id) values (?, ?, ?, ?) on conflict do nothing returning runtime_asset_id",
            )
            .expect("prepare_cached failed");
        stmt.bind_blob(1, group_id.as_bytes())
            .expect("stmt.bind_blob");
        stmt.bind_int(2, asset_type as _).expect("stmt.bind_int");
        stmt.bind_int(3, local_id as _).expect("stmt.bind_int");
        stmt.bind_blob(4, new_asset_id.as_bytes())
            .expect("stmt.bind_blob");
        let r = stmt.step().expect("stmt.step");
        if !r {
            // reuse existing entry
            let mut stmt = self
                .assetdb
                .0
                .prepare("Select runtime_asset_id from asset_group where id = ? and asset_type = ? and local_id = ?")
                .expect("assetdb op prepare");
            stmt.bind_blob(1, group_id.as_bytes())
                .expect("stmt.bind_blob");
            stmt.bind_int(2, asset_type as _).expect("stmt.bind_int");
            stmt.bind_int(3, local_id as _).expect("stmt.bind_int");
            let has_next = stmt.step().expect("stmt.step");
            assert!(has_next);

            let runtime_asset_id_len = stmt.column_bytes(0);
            assert_eq!(runtime_asset_id_len, 16);
            return unsafe { peridot::AssetID::from_bytes(*stmt.column_blob(0).cast()) };
        }

        let runtime_asset_id_len = stmt.column_bytes(0);
        assert_eq!(runtime_asset_id_len, 16);
        unsafe { peridot::AssetID::from_bytes(*stmt.column_blob(0).cast()) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum AssetType {
    Mesh = 1,
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

#[tracing::instrument(skip(processors, ctx, options), fields(source_path = %source_path.as_ref().display()))]
pub fn process(
    processors: &[Box<dyn AssetProcessor>],
    ctx: &mut AssetProcessContext,
    source_path: impl AsRef<Path>,
    options: ProcessOptions,
) -> Option<PathBuf> {
    tracing::info!("Processing...");

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

    let metadata_path = source_path.as_ref().with_extension("p-meta");
    let dest_path = processor.dest_path(source_file_name, dest_dir);

    'determine_rebuild: {
        if options.force_rebuild {
            // forced
            break 'determine_rebuild;
        }

        let source_meta = match source_path.as_ref().metadata() {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, path = ?source_path.as_ref(), "retrieving file metadata failed");
                // cannot determine(force rebuild)
                break 'determine_rebuild;
            }
        };
        let meta_meta = match metadata_path.metadata() {
            Ok(x) => Some(x),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
            Err(e) => {
                tracing::warn!(reason = ?e, path = ?metadata_path, "retrieving file metadata failed");
                // cannot determine(force rebuild)
                break 'determine_rebuild;
            }
        };
        let dest_meta = match dest_path.metadata() {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, path = ?dest_path, "retrieving file metadata failed");
                // cannot determine(force rebuild)
                break 'determine_rebuild;
            }
        };

        let source_modtime = match source_meta.modified() {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, path = ?source_path.as_ref(), "retrieving modified time failed");
                // cannot determine(force rebuild)
                break 'determine_rebuild;
            }
        };
        let meta_modtime = match meta_meta.map(|x| x.modified()).transpose() {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, path = ?metadata_path, "retrieving modified time failed");
                // cannot determine(force rebuild)
                break 'determine_rebuild;
            }
        };
        let dest_modtime = match dest_meta.modified() {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(reason = ?e, path = ?dest_path, "retrieving modified time failed");
                // cannot determine(force rebuild)
                break 'determine_rebuild;
            }
        };

        if source_modtime <= dest_modtime && meta_modtime.is_some_and(|x| x <= dest_modtime) {
            tracing::info!(reason = "modified time", "skip asset");
            return Some(dest_path);
        }
    }

    let metadata = 'load_metadata: {
        match metadata_path.try_exists() {
            Ok(true) => (),
            Ok(false) => {
                tracing::trace!(source_path = ?source_path.as_ref(), metadata_path = ?metadata_path, "no metadata exists for this asset");
                break 'load_metadata None;
            }
            Err(e) => {
                tracing::warn!(reason = ?e, path = ?metadata_path, "querying metadata existential failed");
                break 'load_metadata None;
            }
        }

        let content = match std::fs::read_to_string(&metadata_path) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, path = ?metadata_path, "reading metadata content failed");
                break 'load_metadata None;
            }
        };

        Some(
            metadata::Parser::new(&content)
                .filter_map(
                    |x| x
                        .inspect_err(|e| tracing::error!(reason = ?e, path = ?metadata_path, "parsing metadata failed"))
                        .ok()
                )
                .map(|(k, v)| (k, v.to_owned()))
                .collect::<HashMap<_, _>>()
        )
    };
    let metadata = metadata.unwrap_or_else(HashMap::new);

    if let Err(e) = processor.process(source_path.as_ref(), &metadata, dest_dir, ctx) {
        tracing::error!(reason = ?e, "Failed to process asset");
        return None;
    }

    Some(dest_path)
}
