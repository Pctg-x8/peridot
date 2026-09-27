use std::{collections::HashMap, fs::File, io::BufReader, path::Path};

use peridot_asset_processing::AssetProcessor;

mod glb;

pub struct Processor;
impl AssetProcessor for Processor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path.extension().is_some_and(|x| x == "glb")
    }

    fn process(
        &self,
        source_path: &Path,
        asset_group_id: peridot::AssetID,
        _metadata: &HashMap<peridot_asset_processing::metadata::Key, String>,
        dest_dir: &Path,
        ctx: &mut peridot_asset_processing::AssetProcessContext,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut r = BufReader::new(File::open(source_path)?);
        if peridot_tp_gltf::binary::try_verify_magic(&mut r) {
            return glb::process(r, dest_dir, asset_group_id, ctx).map_err(From::from);
        }

        Err("unknown file format".into())
    }
}
