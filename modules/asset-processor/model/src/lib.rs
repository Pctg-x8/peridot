use std::{collections::HashMap, fs::File, io::BufReader, path::Path};

use peridot_asset_processing::{AssetProcessResult, AssetProcessor, ProcessTargetFileData};

mod glb;

pub struct Processor;
impl Processor {
    pub(self) fn meta_generated_mesh_asset_id_key(
        index: usize,
    ) -> peridot_asset_processing::metadata::Key {
        format!("io.ct2.peridot.asset-processor.model.generated-asset-id.mesh[{index}]").into()
    }
}
impl AssetProcessor for Processor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path.extension().is_some_and(|x| x == "glb")
    }

    fn process(
        &self,
        source_path: &Path,
        data: &ProcessTargetFileData,
        asset_group_id: peridot::AssetID,
        metadata: &mut peridot_asset_processing::MetadataAccess,
        ctx: &mut peridot_asset_processing::AssetProcessContext,
    ) -> Result<AssetProcessResult, Box<dyn std::error::Error>> {
        // TODO: モデルアセットによっては外部ファイル依存があるかもしれない(glb形式じゃないgltfとか)
        if !data.is_dirty {
            return Ok(AssetProcessResult::Skipped);
        }

        let mut r = BufReader::new(File::open(source_path)?);
        if peridot_tp_gltf::binary::try_verify_magic(&mut r) {
            glb::process(r, asset_group_id, metadata, ctx)?;
            return Ok(AssetProcessResult::Ok);
        }

        Err("unknown file format".into())
    }
}
