
use std::path::Path;
use crate::manifest::*;
use crate::steps::{BuildStep, run_steps};

pub fn build(
    userlib: &Path, features: &[String],
    update_deps: bool, cargo_cmd: &str,
    ext_asset_path: Option<&Path>,
    entry_ty_name: &str,
    appid: &str
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
        console::style("Win32").fg(console::Color::Yellow).bright()
    );

    let mut steps = vec![
        BuildStep::GenManifest {
            userlib_path: userlib,
            userlib_name: project_name,
            features: features.iter().map(|s| s as &str).collect()
        },
        BuildStep::GenUserlibImportCode {
            userlib_name: project_name,
            entry_ty_name
        }
    ];
    if update_deps {
        steps.push(BuildStep::UpdateDeps);
    }

    let mut env = std::collections::HashMap::new();
    let mut ext_features = Vec::new();
    env.insert("PERIDOT_WINDOWS_APPID", appid);
    if let Some(p) = ext_asset_path {
        env.insert("PERIDOT_EXTERNAL_ASSET_PATH", p.to_str().expect("invalid sequence in asset path"));
        ext_features.push("UseExternalAssetPath");
    }
    steps.push(BuildStep::BuildWithCargo {
        subcmd: cargo_cmd,
        ext_features,
        env,
        target_spec: Some("x86_64-pc-windows-msvc")
    });

    run_steps("windows", steps.into_iter());
}
