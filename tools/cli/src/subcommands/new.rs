use std::{collections::HashMap, path::PathBuf};

use clap::Parser;

use crate::shellutil::handle_process_result;

/// Generates a new project directory
#[derive(Parser)]
pub struct Args {
    /// Path to directory of the project
    path: PathBuf,
    /// Name(crate name) of the project
    #[clap(long)]
    name: Option<String>,
    /// Application Identifier(default: the last part of the path)
    #[clap(long)]
    app_id: Option<String>,
}

pub fn run(args: Args) {
    let mut cmd = std::process::Command::new("cargo");
    cmd.arg("new").arg(&args.path).arg("--lib");
    if let Some(ref name) = args.name {
        cmd.args(["--name", name]);
    }

    handle_process_result(
        "cargo new",
        cmd.spawn()
            .expect("cargo spawn failed")
            .wait()
            .expect("cargo process wait failed"),
    );

    let peridot_project = crate::project::Project {
        app_package_id: args.app_id.unwrap_or_else(|| {
            args.path
                .components()
                .last()
                .expect("empty path?")
                .as_os_str()
                .to_string_lossy()
                .into_owned()
        }),
        title: None,
        entry_fn_name: None,
        asset_dir: None,
        features: Vec::new(),
        engine_features: Vec::new(),
        platform: HashMap::new(),
    };
    std::fs::write(
        args.path.join("peridot.toml"),
        peridot_project
            .serialize()
            .expect("peridot project serialization failed"),
    )
    .expect("peridot project write failed");
}
