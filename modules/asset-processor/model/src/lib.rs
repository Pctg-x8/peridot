use std::{fs::File, io::BufReader, path::Path};

use peridot_asset_processing::{AssetProcessResult, AssetProcessor, ProcessTargetFileData};

mod glb;

pub(self) struct ProcessOptions {
    extract_material: bool,
}
impl Default for ProcessOptions {
    #[inline(always)]
    fn default() -> Self {
        Self {
            extract_material: true,
        }
    }
}

// 一旦固定値を埋め込み
// TODO: ファイルから取得した方がいいか......？
pub(self) const DEFAULT_3D_PBR_SHADER_ASSET_ID: peridot::AssetID = peridot::AssetID::from_bytes([
    0xfb, 0x23, 0x22, 0x97, 0x6d, 0xcc, 0xaf, 0x68, 0x7b, 0x3e, 0x27, 0x08, 0x0b, 0xc5, 0x6f, 0x03,
]);

pub struct Processor;
impl Processor {
    const META_OPT_EXTRACT_MATERIAL_KEY: &str =
        "io.ct2.peridot.asset-processor.model.extract-material";

    pub(self) fn meta_generated_mesh_asset_id_key(
        index: usize,
    ) -> peridot_asset_processing::metadata::Key {
        format!("io.ct2.peridot.asset-processor.model.generated-asset-id.mesh[{index}]").into()
    }

    pub(self) fn meta_generated_material_asset_id_key(
        index: usize,
    ) -> peridot_asset_processing::metadata::Key {
        format!("io.ct2.peridot.asset-processor.model.generated-asset-id.material[{index}]").into()
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

        let mut opts = ProcessOptions::default();
        if let Some(x) = metadata.get(Self::META_OPT_EXTRACT_MATERIAL_KEY) {
            opts.extract_material = x.eq_ignore_ascii_case("true");
        }

        let mut r = BufReader::new(File::open(source_path)?);
        if peridot_tp_gltf::binary::try_verify_magic(&mut r) {
            glb::process(r, asset_group_id, opts, metadata, ctx)?;
            return Ok(AssetProcessResult::Ok);
        }

        Err("unknown file format".into())
    }
}
