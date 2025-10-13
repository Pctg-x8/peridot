use crate::manifest::*;
use crate::project::PlatformConfiguration;
use crate::steps;
use crate::subcommands::build::BuildMode;

pub fn build(
    options: &super::BuildOptions,
    project_config: &PlatformConfiguration,
    build_mode: BuildMode,
) {
    let asset_path_abs = options
        .ext_asset_path
        .as_ref()
        .map(|x| x.canonicalize().expect("Failed to resolve ext asset path"));
    #[cfg(windows)]
    let asset_path_abs = 'try_remove_verbatim_disk: {
        // Windowsの場合はネットワークを表すパスが頭についちゃうらしい
        let Some(ref p) = asset_path_abs else {
            break 'try_remove_verbatim_disk asset_path_abs;
        };
        let mut components = p.components();
        let Some(std::path::Component::Prefix(prefix)) = components.next() else {
            break 'try_remove_verbatim_disk asset_path_abs;
        };

        match prefix.kind() {
            std::path::Prefix::VerbatimDisk(disk_letter) => {
                Some(std::path::Path::new(&format!("{}:", disk_letter as char)).join(components))
            }
            _ => asset_path_abs,
        }
    };

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

    super::print_start_build("Win32", project_name);

    let ctx = steps::BuildContext::new("windows");
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
    ctx.cwd_cradle_dir();
    if options.update_deps {
        steps::update_deps(&ctx);
    }

    let mut env = std::collections::HashMap::new();
    let mut ext_features = options.engine_features.clone();
    env.insert("PERIDOT_WINDOWS_APPID", options.appid);

    // prepare assets
    let runtime_asset_path = ctx.cradle_directory.join(".runtime-assets");
    std::fs::create_dir_all(&runtime_asset_path)
        .expect("std::fs::create_dir_all runtime-asset-path failed");
    steps::process_assets(&ctx, asset_path_abs.as_deref(), &runtime_asset_path);

    env.insert(
        "PERIDOT_EXTERNAL_ASSET_PATH",
        runtime_asset_path
            .to_str()
            .expect("invalid sequence in asset path"),
    );
    ext_features.push("UseExternalAssetPath");

    let mut cargo = steps::cargo(&ctx)
        .with_env(env)
        .with_ext_features(ext_features)
        .with_target_spec("x86_64-pc-windows-msvc");
    if options.release {
        cargo = cargo.enable_release_build();
    }
    match build_mode {
        BuildMode::Normal => cargo.build(),
        BuildMode::Run => cargo.run(),
        BuildMode::Test => cargo.test(),
        BuildMode::Check => cargo.check(),
    }
}
