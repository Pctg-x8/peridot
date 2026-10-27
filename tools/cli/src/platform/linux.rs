use crate::manifest::*;
use crate::path::generated_runtime_assets_dir;
use crate::project::PlatformConfiguration;
use crate::steps;
use crate::subcommands::build::BuildMode;

pub fn build(
    options: &super::BuildOptions,
    project_config: &PlatformConfiguration,
    build_mode: BuildMode,
) {
    let user_manifest_loaded = std::fs::read_to_string(options.userlib.join("Cargo.toml"))
        .expect("Failed to load Userlib Cargo.toml");
    let user_manifest: CargoManifest =
        toml::from_str(&user_manifest_loaded).expect("Failed to parse Userlib Cargo.toml");
    let project_name = user_manifest
        .package
        .as_ref()
        .and_then(|p| p.name.as_deref())
        .unwrap_or("<Unnamed Peridot Project>");
    let project_version = semver::Version::parse(
        user_manifest
            .package
            .as_ref()
            .and_then(|p| p.version.as_deref())
            .unwrap_or("0.0.0"),
    )
    .expect("illformed project version");

    super::print_start_build("Linux", project_name);

    let ctx = steps::BuildContext::new("linux");
    steps::gen_manifest(
        &ctx,
        options.userlib,
        project_name,
        options.features.clone(),
    );
    steps::gen_userlib_import_code(
        &ctx,
        project_name,
        project_config.title.unwrap_or(project_name),
        &project_version,
        options.entry_fn_name,
    );
    steps::process_assets(&ctx, options.userlib, options.ext_asset_path.as_deref());
    let runtime_asset_path =
        generated_runtime_assets_dir(options.userlib.canonicalize().expect("canonicalize"));

    // starting build...
    ctx.cwd_cradle_dir();
    if options.update_deps {
        steps::update_deps(&ctx);
    }

    let mut env = std::collections::HashMap::new();
    let mut ext_features = options.engine_features.clone();
    env.insert(
        "PERIDOT_EXTERNAL_ASSET_PATH",
        runtime_asset_path
            .to_str()
            .expect("invalid sequence in asset path"),
    );
    ext_features.push("UseExternalAssetPath");
    let mut cargo = steps::cargo()
        .with_ext_features(ext_features)
        .with_env(env)
        .with_target_spec("x86_64-unknown-linux-gnu");
    if options.release {
        cargo = cargo.enable_release_build();
    }
    ctx.print_step("Compiling code...");
    match build_mode {
        BuildMode::Normal => cargo.build(),
        BuildMode::Run => cargo.run(),
        BuildMode::Test => cargo.test(),
        BuildMode::Check => cargo.check(),
    }
}
