
use crate::manifest::*;
use crate::steps::{BuildStep, run_steps};

pub fn build(options: &super::BuildOptions, cargo_cmd: &str) {
    let user_manifest_loaded = std::fs::read_to_string(options.userlib.join("Cargo.toml"))
        .expect("Failed to load Userlib Cargo.toml");
    let user_manifest: CargoManifest = toml::from_str(&user_manifest_loaded)
        .expect("Failed to parse Userlib Cargo.toml");
    let project_name = user_manifest.package.as_ref().and_then(|p| p.name.as_deref())
        .unwrap_or("?Unnamed Project?");
    println!(
        "🛠  Building Project {} for {} Deployment...",
        console::style(project_name).bold().fg(console::Color::Cyan),
        console::style("Linux").fg(console::Color::Yellow).bright()
    );

    let mut steps = vec![
        BuildStep::GenManifest {
            userlib_path: options.userlib,
            userlib_name: project_name,
            features: options.features.clone()
        }
    ];
    if options.update_deps {
        steps.push(BuildStep::UpdateDeps);
    }

    let mut env = std::collections::HashMap::new();
    let mut ext_features = options.engine_features.clone();
    if let Some(p) = options.ext_asset_path {
        env.insert("PERIDOT_EXTERNAL_ASSET_PATH", p.to_str().expect("invalid sequence in asset path"));
        ext_features.push("UseExternalAssetPath");
    }
    steps.push(BuildStep::BuildWithCargo {
        subcmd: cargo_cmd,
        ext_features,
        env,
        target_spec: Some("x86_64-unknown-linux-gnu")
    });

    run_steps("linux", steps.into_iter());
}
