use crate::{Environment, build};

pub struct AssetProcessor;
impl AssetProcessor {
    const META_GENERATED_ASSET_ID_KEY: &str = "io.ct2.peridot.shader.generated-asset-id";
}
impl peridot_asset_processing::AssetProcessor for AssetProcessor {
    fn can_process(&self, source_path: &std::path::Path) -> bool {
        source_path.extension().is_some_and(|x| x == "slang")
    }

    fn process(
        &self,
        source_path: &std::path::Path,
        data: &peridot_asset_processing::ProcessTargetFileData,
        asset_group_id: peridot::AssetID,
        metadata: &mut peridot_asset_processing::MetadataAccess,
        ctx: &mut peridot_asset_processing::AssetProcessContext,
    ) -> Result<peridot_asset_processing::AssetProcessResult, Box<dyn std::error::Error>> {
        if !data.is_dirty {
            return Ok(peridot_asset_processing::AssetProcessResult::Skipped);
        }

        let id = ctx.acquire_asset_id(
            metadata,
            Self::META_GENERATED_ASSET_ID_KEY.into(),
            &asset_group_id,
            peridot::ASSET_TYPE_SHADER,
            0,
        );

        build(
            source_path,
            &Environment {
                peridot_stdlib_search_path: std::ffi::CString::new(
                    std::env::var("PERIDOT_SHADER_STDLIB_PATH").expect("invalid stdlib path"),
                )
                .expect("nul in path"),
            },
        )?
        .write(
            &mut std::fs::File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(ctx.prepare_runtime_asset_output(&id))?,
        )?;
        Ok(peridot_asset_processing::AssetProcessResult::Ok)
    }
}
