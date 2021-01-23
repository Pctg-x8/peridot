
use std::path::Path;
use crate::manifest::*;
use crate::steps::{BuildStep, run_steps};

pub fn build(
    userlib: &Path, features: &[String],
    update_deps: bool, after_run: bool,
    ext_asset_path: Option<&Path>
) {
    let user_manifest_loaded = std::fs::read_to_string(userlib.join("Cargo.toml"))
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
            userlib_path: userlib,
            userlib_name: project_name,
            features: features.iter().map(|s| s as &str).collect()
        }
    ];
    if update_deps {
        steps.push(BuildStep::UpdateDeps);
    }

    let mut env = std::collections::HashMap::new();
    let mut ext_features = Vec::new();
    if let Some(p) = ext_asset_path {
        env.insert("PERIDOT_EXTERNAL_ASSET_PATH", p.to_str().expect("invalid sequence in asset path"));
        ext_features.push("UseExternalAssetPath");
    }
    steps.push(BuildStep::BuildWithCargo {
        subcmd: if after_run { "run" } else { "build" },
        ext_features,
        env,
        target_spec: Some("x86_64-unknown-linux-gnu")
    });

    run_steps("linux", steps.into_iter());
}
