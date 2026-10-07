//! Processing user assets into runtime assets.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

pub mod builtin;
pub mod metadata;

/// An asset processor interface.
pub trait AssetProcessor {
    /// Determines if this processor can handle the given source file.
    fn can_process(&self, source_path: &Path) -> bool;

    /// Processes the asset from the source path.
    ///
    /// Expected some files to be created and placed in `out_path`.
    fn process(
        &self,
        source_path: &Path,
        asset_group_id: peridot::AssetID,
        metadata: &HashMap<metadata::Key, String>,
        ctx: &mut AssetProcessContext,
    ) -> Result<(), Box<dyn std::error::Error>>;
}

pub struct AssetProcessContext<'a> {
    pub dest_dir: &'a Path,
    pub assetdb: peridot::AssetDatabase,
    pub asset_id_generator: peridot::AssetIDGenerator,
}
impl<'a> AssetProcessContext<'a> {
    pub fn prepare_runtime_asset_output(&self, id: &peridot::AssetID) -> PathBuf {
        let path = self.dest_dir.join(id.build_runtime_asset_path_relative());
        if let Some(p) = path.parent()
            && let Err(e) = std::fs::create_dir_all(p)
        {
            tracing::error!(reason = %e, path = ?p, "Failed to prepare output directory");
        }

        path
    }

    fn try_process_new_asset_group(
        &mut self,
        source_path: &Path,
        to_be_processed: i64,
    ) -> (peridot::AssetID, i64, bool) {
        let new_group_id = self.asset_id_generator.generate();
        self.assetdb
            .try_process_new_asset_group(source_path, &new_group_id, to_be_processed)
    }

    fn try_process_new_asset_group_with_existing_id(
        &mut self,
        source_path: &Path,
        group_id: &peridot::AssetID,
        to_be_processed: i64,
    ) -> (i64, bool) {
        self.assetdb.try_process_new_asset_group_with_existing_id(
            source_path,
            group_id,
            to_be_processed,
        )
    }

    fn register_loadable_asset(&mut self, load_identifier: &str, group_id: &peridot::AssetID) {
        self.assetdb
            .register_loadable_asset(load_identifier, group_id)
    }

    pub fn register_child_asset(
        &mut self,
        group_id: &peridot::AssetID,
        asset_type: peridot::AssetType,
        local_id: i32,
    ) -> peridot::AssetID {
        let new_asset_id = self.asset_id_generator.generate();
        self.assetdb
            .register_child_asset(group_id, &new_asset_id, asset_type, local_id)
    }
}

pub struct ProcessOptions<'a> {
    pub force_rebuild: bool,
    pub loadable_prefix: Option<&'a str>,
}
impl Default for ProcessOptions<'_> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            force_rebuild: false,
            loadable_prefix: None,
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ProcessError {
    #[error("Failed to copy asset file: {0:?}")]
    RawAssetCopyFailed(std::io::Error),
    #[error("Cannot determine the asset processor")]
    MultipleAssetProcessor,
    #[error(transparent)]
    ProcessorFailure(Box<dyn std::error::Error>),
}

pub fn loadable_asset_identifier(
    source_path: impl AsRef<Path>,
    base_path: impl AsRef<Path>,
) -> Option<String> {
    let source_path = source_path.as_ref();
    let base_path = base_path.as_ref();

    let Ok(relative_path) = source_path.strip_prefix(base_path) else {
        // not prefixed
        return None;
    };
    let mut id_components = Vec::<&str>::new();
    for c in relative_path.components() {
        match c {
            std::path::Component::RootDir => unreachable!("root dir in relative path"),
            std::path::Component::CurDir => { /* nothing to do */ }
            std::path::Component::ParentDir => {
                id_components.pop().expect("cannot pop parent dir");
            }
            std::path::Component::Normal(c) => {
                let c = c.to_str().expect("invalid path component");
                let (pre_dot_part, _) = c.split_once('.').unwrap_or((c, ""));
                if pre_dot_part.is_empty() {
                    // started by dot part found(hidden file or under a hidden dir)
                    return None;
                }

                id_components.push(pre_dot_part);
            }
            std::path::Component::Prefix(_) => unreachable!("prefix in relative path"),
        }
    }

    Some(id_components.join("."))
}

pub fn is_metadata_file(path: impl AsRef<Path>) -> bool {
    path.as_ref().extension().is_some_and(|e| e == "p-meta")
}

#[tracing::instrument(skip(processors, ctx, base_path, options), fields(source_path = ?source_path.as_ref(), dest_dir = ?ctx.dest_dir), err)]
pub fn process(
    processors: &[Box<dyn AssetProcessor>],
    ctx: &mut AssetProcessContext,
    base_path: impl AsRef<Path>,
    source_path: impl AsRef<Path>,
    options: &ProcessOptions,
) -> Result<(), ProcessError> {
    let source_path = source_path.as_ref();
    let metadata_path = source_path.with_extension("p-meta");

    let mut matching_processors_iter = processors.iter().filter(|x| x.can_process(source_path));
    let processor = matching_processors_iter.next();
    if matching_processors_iter.next().is_some() {
        return Err(ProcessError::MultipleAssetProcessor);
    }

    let source_meta = source_path
        .metadata()
        .inspect_err(
            |e| tracing::warn!(reason = %e, path = ?source_path, "retrieving file metadata failed"),
        )
        .ok();
    let meta_meta = metadata_path
        .metadata()
        .inspect_err(|e| {
            if e.kind() == std::io::ErrorKind::NotFound {
                // これはなくてもいいファイルなのでなかったらスルー
                return;
            }

            tracing::warn!(reason = %e, path = ?metadata_path, "retrieving file metadata failed");
        })
        .ok();
    let source_modtime = source_meta
        .and_then(|m| {
            m.modified()
                .inspect_err(|e| tracing::warn!(reason = %e, path = ?source_path, "retrieving modified time failed"))
                .ok()
        });
    let meta_modtime = meta_meta
        .and_then(|m| {
            m.modified()
                .inspect_err(|e| tracing::warn!(reason = %e, path = ?metadata_path, "retrieving modified time failed"))
                .ok()
        });
    let to_be_processed = match (source_modtime, meta_modtime) {
        (Some(s), Some(m)) => s.max(m),
        (Some(a), None) | (None, Some(a)) => a,
        (None, None) => std::time::SystemTime::now(),
    };
    let to_be_processed = match to_be_processed.duration_since(std::time::SystemTime::UNIX_EPOCH) {
        Ok(d) => d.as_millis(),
        Err(_) => {
            tracing::warn!(target = ?to_be_processed, "retrieving duration since unix epoch failed");
            0
        }
    } as i64;

    let metadata = 'load_metadata: {
        match metadata_path.try_exists() {
            Ok(true) => (),
            Ok(false) => {
                tracing::trace!(source_path = ?source_path, metadata_path = ?metadata_path, "no metadata exists for this asset");
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
    let mut metadata = metadata.unwrap_or_else(HashMap::new);

    let assigned_asset_id =
        metadata
            .get("id")
            .and_then(|x| match peridot::AssetID::deserialize_chars(x.chars()) {
                Some(x) => Some(x),
                None => {
                    tracing::warn!("serialized asset id is invalid");
                    None
                }
            });
    let (asset_group_id, needs_build) = match assigned_asset_id {
        Some(existing) => {
            // use asset group id from metadata
            let (db_last_processed, new_inserted) = ctx
                .try_process_new_asset_group_with_existing_id(
                    source_path.as_ref(),
                    &existing,
                    to_be_processed,
                );
            let needs_build = new_inserted || db_last_processed < to_be_processed;
            (existing, needs_build)
        }
        None => {
            // issue new group and write into metadata
            let (asset_group_id, db_last_processed, new_inserted) =
                ctx.try_process_new_asset_group(source_path.as_ref(), to_be_processed);

            let asset_str = asset_group_id.serialize_chars().collect::<String>();
            metadata.insert("id".into(), asset_str);
            'try_writeback_meta: {
                let mut fp = std::io::BufWriter::new(match std::fs::File::create(&metadata_path) {
                    Ok(x) => x,
                    Err(e) => {
                        tracing::error!(reason = %e, "Failed to open metadata file for writing");
                        break 'try_writeback_meta;
                    }
                });
                if let Err(e) = metadata::serialize_metadata(
                    &mut fp,
                    metadata.iter().map(|(k, v)| (k, v.as_str())),
                ) {
                    tracing::error!(reason = %e, "Failed to write metadata");
                    break 'try_writeback_meta;
                }
            }

            let needs_build = new_inserted || db_last_processed < to_be_processed;
            (asset_group_id, needs_build)
        }
    };
    if !options.force_rebuild && !needs_build {
        tracing::info!("skip asset");
        return Ok(());
    }

    tracing::info!("Processing...");
    if let Some(load_identifier) = loadable_asset_identifier(source_path, base_path) {
        ctx.register_loadable_asset(
            &format!("{}{load_identifier}", options.loadable_prefix.unwrap_or("")),
            &asset_group_id,
        );
    }
    match processor {
        None => {
            // Raw Asset Processing
            tracing::warn!("unknown assets(processed as raw)");
            let asset_id = ctx.register_child_asset(&asset_group_id, peridot::ASSET_TYPE_RAW, 0);

            let dest_path = ctx.prepare_runtime_asset_output(&asset_id);
            std::fs::copy(source_path, dest_path)
                .map_err(ProcessError::RawAssetCopyFailed)
                .map(drop)
        }
        Some(p) => p
            .process(source_path.as_ref(), asset_group_id, &metadata, ctx)
            .map_err(ProcessError::ProcessorFailure),
    }
}
