
use crate::manifest::*;
use crate::steps;

pub fn build(options: &super::BuildOptions, cargo_cmd: &str) {
    let user_manifest_loaded = std::fs::read_to_string(options.userlib.join("Cargo.toml"))
        .expect("Failed to load Userlib Cargo.toml");
    let user_manifest: CargoManifest = toml::from_str(&user_manifest_loaded)
        .expect("Failed to parse Userlib Cargo.toml");
    let project_name = user_manifest.package.as_ref().and_then(|p| p.name.as_deref())
        .unwrap_or("?Unnamed Project?");
    super::print_start_build("Linux", project_name);

    let ctx = steps::BuildContext::new("linux");
    steps::gen_manifest(&ctx, options.userlib, project_name, options.features.clone());
    steps::gen_userlib_import_code(&ctx, project_name, options.entry_ty_name);
    ctx.cwd_cradle_dir();
    if options.update_deps {
        steps::update_deps(&ctx);
    }

    let mut env = std::collections::HashMap::new();
    let mut ext_features = options.engine_features.clone();
    if let Some(p) = options.ext_asset_path {
        env.insert("PERIDOT_EXTERNAL_ASSET_PATH", p.to_str().expect("invalid sequence in asset path"));
        ext_features.push("UseExternalAssetPath");
    }
    steps::cargo(&ctx, cargo_cmd, ext_features, env, Some("x86_64-unknown-linux-gnu"));
}
