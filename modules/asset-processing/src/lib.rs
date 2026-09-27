//! Processing user assets into runtime assets.

use std::{
    collections::HashMap,
    path::{Path, PathBuf},
};

use peridot_tp_sqlite3::PrepareFlags;

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
        dest_dir: &Path,
        ctx: &mut AssetProcessContext,
    ) -> Result<(), Box<dyn std::error::Error>>;
}

pub fn prepare_runtime_asset_output(dest_dir: &Path, asset_id: &peridot::AssetID) -> PathBuf {
    let path = asset_id.build_runtime_asset_path(dest_dir);
    if let Some(p) = path.parent()
        && let Err(e) = std::fs::create_dir_all(p)
    {
        tracing::error!(reason = %e, path = ?p, "Failed to prepare output directory");
    }

    path
}

pub struct AssetProcessContext {
    pub assetdb: peridot::AssetDatabase,
    pub asset_id_generator: peridot::AssetIDGenerator,
}
impl AssetProcessContext {
    fn register_or_update_asset_group(
        &mut self,
        source_path: &Path,
        to_be_processed: i64,
    ) -> (peridot::AssetID, i64, bool) {
        let new_group_id = self.asset_id_generator.generate();

        let mut stmt = self.assetdb.db.prepare(
            "Insert into dev_user_asset (source_path, group_id, last_processed) values (?, ?, ?) on conflict do update set last_processed = max(last_processed, excluded.last_processed) returning group_id, last_processed",
            PrepareFlags::empty(),
        ).expect("assetdb op prepare");
        stmt.bind_text(1, source_path.to_str().expect("invalid str"))
            .expect("stmt.bind_text");
        stmt.bind_blob(2, new_group_id.as_bytes())
            .expect("stmt.bind_blob");
        stmt.bind_i64(3, to_be_processed).expect("stmt.bind_int");
        let has_insertion = stmt.step().expect("stmt.step");
        assert!(has_insertion);

        assert_eq!(stmt.column_bytes(0), 16);
        let inserted_group_id =
            unsafe { peridot::AssetID::from_bytes(*stmt.column_blob(0).cast()) };
        let inserted_last_processed = stmt.column_i64(1);
        let new_inserted = inserted_group_id == new_group_id;

        // update asset group info
        let mut stmt = self
            .assetdb
            .db
            .prepare(
                "Replace into asset_group_info (id, name) values (?, ?)",
                PrepareFlags::empty(),
            )
            .expect("assetdb op prepare");
        stmt.bind_blob(1, new_group_id.as_bytes())
            .expect("stmt.bind_blob");
        stmt.bind_text(
            2,
            source_path
                .file_stem()
                .map_or("", |x| x.to_str().expect("invalid str")),
        )
        .expect("stmt.bind_text");
        stmt.step().expect("stmt.step");

        (inserted_group_id, inserted_last_processed, new_inserted)
    }

    fn register_existing_asset_group(
        &mut self,
        source_path: &Path,
        group_id: &peridot::AssetID,
        to_be_processed: i64,
    ) -> i64 {
        let mut stmt = self.assetdb.db.prepare(
            "Insert into dev_user_asset (source_path, group_id, last_processed) values (?, ?, ?) on conflict do update set group_id = excluded.group_id, last_processed = max(last_processed, excluded.last_processed) returning last_processed",
            PrepareFlags::empty(),
        ).expect("assetdb op prepare");
        stmt.bind_text(1, source_path.to_str().expect("invalid str"))
            .expect("stmt.bind_text");
        stmt.bind_blob(2, group_id.as_bytes())
            .expect("stmt.bind_blob");
        stmt.bind_i64(3, to_be_processed).expect("stmt.bind_int");
        let has_insertion = stmt.step().expect("stmt.step");
        assert!(has_insertion);

        let inserted_last_processed = stmt.column_i64(0);

        // update asset group info
        let mut stmt = self
            .assetdb
            .db
            .prepare(
                "Replace into asset_group_info (id, name) values (?, ?)",
                PrepareFlags::empty(),
            )
            .expect("assetdb op prepare");
        stmt.bind_blob(1, group_id.as_bytes())
            .expect("stmt.bind_blob");
        stmt.bind_text(
            2,
            source_path
                .file_stem()
                .map_or("", |x| x.to_str().expect("invalid str")),
        )
        .expect("stmt.bind_text");
        stmt.step().expect("stmt.step");

        inserted_last_processed
    }

    fn register_loadable_asset(&mut self, load_identifier: &str, group_id: &peridot::AssetID) {
        let mut stmt = self
            .assetdb
            .db
            .prepare(
                "Replace into loadable_asset (identifier, group_id) values (?, ?)",
                PrepareFlags::empty(),
            )
            .expect("assetdb op prepare");
        stmt.bind_text(1, load_identifier).expect("stmt.bind_text");
        stmt.bind_blob(2, group_id.as_bytes())
            .expect("stmt.bind_blob");
        stmt.step().expect("stmt.step");
    }

    pub fn register_or_update_child_asset(
        &mut self,
        group_id: &peridot::AssetID,
        asset_type: peridot::AssetType,
        local_id: i32,
    ) -> peridot::AssetID {
        let new_asset_id = self.asset_id_generator.generate();
        let mut stmt = self.assetdb
            .db
            .prepare(
                "Insert into asset_group (id, asset_type, local_id, runtime_asset_id) values (?, ?, ?, ?) on conflict do nothing returning runtime_asset_id",
                PrepareFlags::empty()
            )
            .expect("prepare_cached failed");
        stmt.bind_blob(1, group_id.as_bytes())
            .expect("stmt.bind_blob");
        stmt.bind_int(2, asset_type as _).expect("stmt.bind_int");
        stmt.bind_int(3, local_id as _).expect("stmt.bind_int");
        stmt.bind_blob(4, new_asset_id.as_bytes())
            .expect("stmt.bind_blob");
        let has_insertion = stmt.step().expect("stmt.step");
        if !has_insertion {
            // reuse existing entry
            let mut stmt = self
                .assetdb
                .db
                .prepare("Select runtime_asset_id from asset_group where id = ? and asset_type = ? and local_id = ?", PrepareFlags::empty())
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

#[tracing::instrument(skip(processors, ctx, base_path, options), fields(source_path = ?source_path.as_ref(), dest_dir = ?dest_dir.as_ref()), err)]
pub fn process(
    processors: &[Box<dyn AssetProcessor>],
    ctx: &mut AssetProcessContext,
    base_path: impl AsRef<Path>,
    source_path: impl AsRef<Path>,
    dest_dir: impl AsRef<Path>,
    options: &ProcessOptions,
) -> Result<(), ProcessError> {
    tracing::info!("Processing...");
    let source_path = source_path.as_ref();
    let dest_dir = dest_dir.as_ref();
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
            .and_then(|x| match peridot::AssetID::deserialize_text(x) {
                Some(x) => Some(x),
                None => {
                    tracing::warn!("serialized asset id is invalid");
                    None
                }
            });
    let (asset_group_id, needs_build) = match assigned_asset_id {
        Some(existing) => {
            // use asset group id from metadata
            let db_last_processed =
                ctx.register_existing_asset_group(source_path.as_ref(), &existing, to_be_processed);
            let needs_build = db_last_processed < to_be_processed;
            (existing, needs_build)
        }
        None => {
            // issue new group and write into metadata
            let (asset_group_id, db_last_processed, new_inserted) =
                ctx.register_or_update_asset_group(source_path.as_ref(), to_be_processed);

            let mut asset_str = String::with_capacity(peridot::AssetID::SERIALIZE_TEXT_LEN);
            asset_group_id
                .serialize_text(&mut asset_str)
                .expect("serialize_text failed");
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
            let asset_id =
                ctx.register_or_update_child_asset(&asset_group_id, peridot::ASSET_TYPE_RAW, 0);

            std::fs::copy(
                source_path,
                prepare_runtime_asset_output(dest_dir, &asset_id),
            )
            .map_err(ProcessError::RawAssetCopyFailed)
            .map(drop)
        }
        Some(p) => p
            .process(
                source_path.as_ref(),
                asset_group_id,
                &metadata,
                dest_dir,
                ctx,
            )
            .map_err(ProcessError::ProcessorFailure),
    }
}
