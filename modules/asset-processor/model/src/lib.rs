use std::{
    collections::HashMap,
    ffi::OsStr,
    fs::File,
    io::BufReader,
    path::{Path, PathBuf},
};

use peridot_asset_processing::AssetProcessor;

mod glb;

pub struct Processor;
impl AssetProcessor for Processor {
    fn can_process(&self, source_path: &Path) -> bool {
        source_path.extension().is_some_and(|x| x == "glb")
    }

    fn dest_path(&self, source_file_name: &OsStr, out_dir_path: &Path) -> PathBuf {
        // 0番目のメッシュはだいたいほぼ生成されるのでそれで更新判断する（複数アセットが出来上がるかどうかはsource_file開いてみないとわからないので一旦これで）
        let mut path = out_dir_path.join(source_file_name);
        path.set_file_name(format!(
            "{}-mesh0-0.pa1-mesh",
            path.file_stem().unwrap_or_default().display()
        ));
        path
    }

    fn process(
        &self,
        source_path: &Path,
        _metadata: &HashMap<peridot_asset_processing::metadata::Key, String>,
        out_path: &Path,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let mut r = BufReader::new(File::open(source_path)?);
        if peridot_tp_gltf::binary::try_verify_magic(&mut r) {
            return glb::process(r, out_path).map_err(From::from);
        }

        Err("unknown file format".into())
    }
}
