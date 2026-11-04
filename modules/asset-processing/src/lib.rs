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
        data: &ProcessTargetFileData,
        asset_group_id: peridot::AssetID,
        metadata: &HashMap<metadata::Key, String>,
        ctx: &mut AssetProcessContext,
    ) -> Result<AssetProcessResult, Box<dyn std::error::Error>>;
}

pub enum AssetProcessResult {
    Ok,
    Skipped,
}

pub struct AssetProcessContext<'a> {
    pub dest_dir: &'a Path,
    pub assetdb: peridot::AssetDatabase,
    pub asset_id_generator: peridot::AssetIDGenerator,
    pub dirty_file_set: ProcessedFileSet<'a>,
}

pub enum ProcessedFileSet<'a> {
    All,
    Selected(&'a HashMap<PathBuf, ProcessTargetFileData>),
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

    pub fn file_is_dirty(&self, path: impl AsRef<Path>) -> bool {
        match self.dirty_file_set {
            ProcessedFileSet::All => true,
            ProcessedFileSet::Selected(xs) => match path.as_ref().canonicalize() {
                Ok(x) => xs.get(&x).is_some_and(|x| x.is_dirty),
                Err(e) => {
                    tracing::error!(reason = %e, "path canonicalize failed");
                    false
                }
            },
        }
    }

    pub fn register_child_asset(
        &mut self,
        group_id: &peridot::AssetID,
        asset_type: peridot::AssetType,
        local_id: i32,
    ) -> peridot::AssetID {
        let new_asset_id = self.asset_id_generator.generate();
        self.assetdb
            .register_child_asset(group_id, &new_asset_id, asset_type, local_id);
        new_asset_id
    }

    #[inline(always)]
    pub fn register_asset_dependency(
        &mut self,
        asset_id: &peridot::AssetID,
        required_asset_id: &peridot::AssetID,
    ) {
        self.assetdb
            .register_asset_dependency(asset_id, required_asset_id);
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
    #[error(transparent)]
    IO(#[from] std::io::Error),
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

struct RecursiveFiles {
    stack: Vec<std::fs::ReadDir>,
}
impl RecursiveFiles {
    #[inline(always)]
    pub fn new(root_path: impl AsRef<Path>) -> std::io::Result<Self> {
        Ok(Self {
            stack: vec![std::fs::read_dir(root_path)?],
        })
    }
}
impl Iterator for RecursiveFiles {
    type Item = std::io::Result<std::fs::DirEntry>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(mut active) = self.stack.pop() {
            let Some(e) = active.next() else {
                continue;
            };

            self.stack.push(active);
            match e {
                Ok(e) => {
                    let md = match e.metadata() {
                        Ok(x) => x,
                        Err(e) => return Some(Err(e)),
                    };
                    if md.is_dir() {
                        let child_rd = match std::fs::read_dir(e.path()) {
                            Ok(x) => x,
                            Err(e) => return Some(Err(e)),
                        };

                        self.stack.push(child_rd);
                        continue;
                    }

                    return Some(Ok(e));
                }
                Err(e) => return Some(Err(e)),
            }
        }

        None
    }
}

#[derive(Debug)]
pub struct ProcessTargetFileData {
    pub modtime: i64,
    pub is_dirty: bool,
    pub asset_group_id: peridot::AssetID,
}

#[tracing::instrument(skip(assetdb, group_id_generator))]
pub fn collect_target_files(
    builtin_assets_path: &Path,
    assets_path: Option<&Path>,
    assetdb: &mut peridot::AssetDatabase,
    group_id_generator: &mut peridot::AssetIDGenerator,
) -> HashMap<PathBuf, ProcessTargetFileData> {
    let ts = std::time::Instant::now();
    let modtime_fallback = std::time::SystemTime::now()
        .duration_since(std::time::SystemTime::UNIX_EPOCH)
        .expect("invalid nowtime")
        .as_millis();
    let mut batch_process = peridot::AssetDatabaseUserAssetUpdateProcess::begin(assetdb);
    let mut target_files = HashMap::new();
    let mut scanned_files = 0usize;
    for e in RecursiveFiles::new(builtin_assets_path).expect("readdir builtin root") {
        scanned_files += 1;

        let e = e.expect("readdir entry");
        let source_path = e.path().canonicalize().expect("path canonicalize");
        if source_path
            .file_name()
            .is_some_and(|x| x.to_str().is_some_and(|x| x.starts_with('.')))
        {
            // hidden files
            continue;
        }
        if is_metadata_file(&source_path) {
            // meta files
            continue;
        }

        let metadata_path = source_path.with_extension("p-meta");

        let modtime = source_path
            .metadata()
            .inspect_err(|e| tracing::error!(reason = %e, "metadata"))
            .ok()
            .and_then(|m| {
                m.modified()
                    .inspect_err(|e| tracing::error!(reason = %e, "modtime"))
                    .ok()
            })
            .and_then(|e| {
                e.duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .inspect_err(|e| tracing::error!(reason = %e, "modtime to unix timestamp"))
                    .ok()
            })
            .map(|x| x.as_millis());
        let meta_modtime = metadata_path
            .metadata()
            .inspect_err(|e| tracing::error!(reason = %e, "metadata"))
            .ok()
            .and_then(|m| {
                m.modified()
                    .inspect_err(|e| tracing::error!(reason = %e, "modtime"))
                    .ok()
            })
            .and_then(|e| {
                e.duration_since(std::time::SystemTime::UNIX_EPOCH)
                    .inspect_err(|e| tracing::error!(reason = %e, "modtime to unix timestamp"))
                    .ok()
            })
            .map(|e| e.as_millis());
        let modtime = match (modtime, meta_modtime) {
            (None, None) => None,
            (Some(a), None) | (None, Some(a)) => Some(a),
            (Some(a), Some(b)) => Some(a.max(b)),
        };
        let modtime: i64 = modtime
            .unwrap_or(modtime_fallback)
            .try_into()
            .inspect_err(|e| tracing::warn!(reason = %e, "modtime too large(truncated)"))
            .unwrap_or(i64::MAX);

        let mut metadata = load_metadata(&metadata_path);
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
                let (db_last_processed, new_inserted) = batch_process
                    .try_process_new_asset_group_with_existing_id(
                        source_path.as_ref(),
                        &existing,
                        modtime,
                    );
                let needs_build = new_inserted || db_last_processed < modtime;
                (existing, needs_build)
            }
            None => {
                // issue new group and write into metadata
                let new_group_id = group_id_generator.generate();
                let (asset_group_id, db_last_processed, new_inserted) = batch_process
                    .try_process_new_asset_group(source_path.as_ref(), &new_group_id, modtime);

                let asset_str = asset_group_id.serialize_chars().collect::<String>();
                metadata.insert("id".into(), asset_str);
                writeback_metadata(&metadata_path, &metadata);

                (asset_group_id, true)
            }
        };

        if needs_build {
            let loadable_asset_identifier =
                loadable_asset_identifier(e.path(), builtin_assets_path)
                    .map(|x| format!("builtin.{x}"));
            if let Some(loadable_asset_identifier) = loadable_asset_identifier {
                batch_process.register_loadable_asset(&loadable_asset_identifier, &asset_group_id);
            }
        }
        target_files.insert(
            source_path,
            ProcessTargetFileData {
                modtime,
                is_dirty: needs_build,
                asset_group_id,
            },
        );
    }
    if let Some(assets_path) = assets_path {
        for e in RecursiveFiles::new(assets_path).expect("readdir assets root") {
            scanned_files += 1;

            let e = e.expect("readdir entry");
            let source_path = e.path().canonicalize().expect("path canonicalize");
            if source_path
                .file_name()
                .is_some_and(|x| x.to_str().is_some_and(|x| x.starts_with('.')))
            {
                // hidden files
                continue;
            }
            if is_metadata_file(&source_path) {
                // meta files
                continue;
            }

            let metadata_path = source_path.with_extension("p-meta");

            let modtime = source_path
                .metadata()
                .inspect_err(|e| tracing::error!(reason = %e, "metadata"))
                .ok()
                .and_then(|m| {
                    m.modified()
                        .inspect_err(|e| tracing::error!(reason = %e, "modtime"))
                        .ok()
                })
                .and_then(|e| {
                    e.duration_since(std::time::SystemTime::UNIX_EPOCH)
                        .inspect_err(|e| tracing::error!(reason = %e, "modtime to unix timestamp"))
                        .ok()
                })
                .map(|x| x.as_millis());
            let meta_modtime = metadata_path
                .metadata()
                .inspect_err(|e| tracing::error!(reason = %e, "metadata"))
                .ok()
                .and_then(|m| {
                    m.modified()
                        .inspect_err(|e| tracing::error!(reason = %e, "modtime"))
                        .ok()
                })
                .and_then(|e| {
                    e.duration_since(std::time::SystemTime::UNIX_EPOCH)
                        .inspect_err(|e| tracing::error!(reason = %e, "modtime to unix timestamp"))
                        .ok()
                })
                .map(|e| e.as_millis());
            let modtime = match (modtime, meta_modtime) {
                (None, None) => None,
                (Some(a), None) | (None, Some(a)) => Some(a),
                (Some(a), Some(b)) => Some(a.max(b)),
            };
            let modtime: i64 = modtime
                .unwrap_or(modtime_fallback)
                .try_into()
                .inspect_err(|e| tracing::warn!(reason = %e, "modtime too large(truncated)"))
                .unwrap_or(i64::MAX);

            let mut metadata = load_metadata(&metadata_path);
            let assigned_asset_id = metadata.get("id").and_then(|x| {
                match peridot::AssetID::deserialize_chars(x.chars()) {
                    Some(x) => Some(x),
                    None => {
                        tracing::warn!("serialized asset id is invalid");
                        None
                    }
                }
            });
            let (asset_group_id, needs_build) = match assigned_asset_id {
                Some(existing) => {
                    // use asset group id from metadata
                    let (db_last_processed, new_inserted) = batch_process
                        .try_process_new_asset_group_with_existing_id(
                            source_path.as_ref(),
                            &existing,
                            modtime,
                        );
                    let needs_build = new_inserted || db_last_processed < modtime;
                    (existing, needs_build)
                }
                None => {
                    // issue new group and write into metadata
                    let new_group_id = group_id_generator.generate();
                    let (asset_group_id, db_last_processed, new_inserted) = batch_process
                        .try_process_new_asset_group(source_path.as_ref(), &new_group_id, modtime);

                    let asset_str = asset_group_id.serialize_chars().collect::<String>();
                    metadata.insert("id".into(), asset_str);
                    writeback_metadata(&metadata_path, &metadata);

                    (asset_group_id, true)
                }
            };

            if needs_build {
                let loadable_asset_identifier = loadable_asset_identifier(e.path(), assets_path);
                if let Some(loadable_asset_identifier) = loadable_asset_identifier {
                    batch_process
                        .register_loadable_asset(&loadable_asset_identifier, &asset_group_id);
                }
            }
            target_files.insert(
                source_path,
                ProcessTargetFileData {
                    modtime,
                    is_dirty: needs_build,
                    asset_group_id,
                },
            );
        }
    }
    batch_process.commit();

    let elapsed = ts.elapsed();
    tracing::info!(scanned_files, ?elapsed, "asset files scanned");
    target_files
}

#[tracing::instrument(fields(path = ?path.as_ref()))]
pub fn load_metadata(path: impl AsRef<Path>) -> HashMap<metadata::Key, String> {
    match path.as_ref().try_exists() {
        Ok(true) => (),
        Ok(false) => {
            tracing::trace!("no metadata exists for this asset");
            return HashMap::new();
        }
        Err(e) => {
            tracing::warn!(reason = ?e,  "querying metadata existential failed");
            return HashMap::new();
        }
    }

    let content = match std::fs::read_to_string(&path) {
        Ok(x) => x,
        Err(e) => {
            tracing::error!(reason = ?e,  "reading metadata content failed");
            return HashMap::new();
        }
    };

    metadata::Parser::new(&content)
        .filter_map(|x| {
            x.inspect_err(|e| tracing::error!(reason = ?e, "parsing metadata failed"))
                .ok()
        })
        .map(|(k, v)| (k, v.to_owned()))
        .collect()
}

#[tracing::instrument(skip(metadata), fields(path = ?path.as_ref()))]
pub fn writeback_metadata(path: impl AsRef<Path>, metadata: &HashMap<metadata::Key, String>) {
    let mut fp = std::io::BufWriter::new(match std::fs::File::create(path) {
        Ok(x) => x,
        Err(e) => {
            tracing::error!(reason = %e, "Failed to open metadata file for writing");
            return;
        }
    });
    if let Err(e) =
        metadata::serialize_metadata(&mut fp, metadata.iter().map(|(k, v)| (k, v.as_str())))
    {
        tracing::error!(reason = %e, "Failed to write metadata");
    }
}

#[tracing::instrument(skip(processors, ctx ), fields(source_path = ?source_path.as_ref()))]
pub fn process(
    source_path: impl AsRef<Path>,
    data: &ProcessTargetFileData,
    processors: &[Box<dyn AssetProcessor>],
    ctx: &mut AssetProcessContext,
) -> Result<(), ProcessError> {
    let source_path = source_path.as_ref();
    let metadata_path = source_path.with_extension("p-meta");

    let mut matching_processors_iter = processors.iter().filter(|x| x.can_process(source_path));
    let processor = matching_processors_iter.next();
    if matching_processors_iter.next().is_some() {
        return Err(ProcessError::MultipleAssetProcessor);
    }

    tracing::info!("Processing...");
    let result = match processor {
        None => {
            // Raw Asset Processing
            tracing::warn!("unknown assets(processed as raw)");
            if data.is_dirty {
                let asset_id =
                    ctx.register_child_asset(&data.asset_group_id, peridot::ASSET_TYPE_RAW, 0);

                let dest_path = ctx.prepare_runtime_asset_output(&asset_id);
                match std::fs::copy(source_path, dest_path) {
                    Ok(_) => AssetProcessResult::Ok,
                    Err(e) => return Err(ProcessError::RawAssetCopyFailed(e)),
                }
            } else {
                AssetProcessResult::Skipped
            }
        }
        Some(p) => p
            .process(
                source_path.as_ref(),
                data,
                data.asset_group_id.clone(),
                &load_metadata(&metadata_path),
                ctx,
            )
            .map_err(ProcessError::ProcessorFailure)?,
    };

    match result {
        AssetProcessResult::Skipped => {
            tracing::info!("Process skippped");
            return Ok(());
        }
        AssetProcessResult::Ok => (),
    }

    Ok(())
}

/*
#[tracing::instrument(skip(processors, external_ctx, base_path, options), fields(source_path = ?source_path.as_ref(), dest_dir = ?ctx.dest_dir), err)]
pub fn process_old(
    processors: &[Box<dyn AssetProcessor>],
    external_ctx: &mut AssetProcessExternalContext,
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
            let (db_last_processed, new_inserted) = external_ctx
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
                external_ctx.try_process_new_asset_group(source_path.as_ref(), to_be_processed);

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
        external_ctx.register_loadable_asset(
            &format!("{}{load_identifier}", options.loadable_prefix.unwrap_or("")),
            &asset_group_id,
        );
    }
    match processor {
        None => {
            // Raw Asset Processing
            tracing::warn!("unknown assets(processed as raw)");
            let asset_id =
                external_ctx.register_child_asset(&asset_group_id, peridot::ASSET_TYPE_RAW, 0);

            let dest_path = external_ctx.prepare_runtime_asset_output(&asset_id);
            std::fs::copy(source_path, dest_path)
                .map_err(ProcessError::RawAssetCopyFailed)
                .map(drop)
        }
        Some(p) => p
            .process(source_path.as_ref(), asset_group_id, &metadata, ctx)
            .map_err(ProcessError::ProcessorFailure),
    }
}*/
