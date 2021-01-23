
use std::{path::Path, process::ExitStatus};
use crate::manifest::*;

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

    print_step(format_args!("Generating manifest..."));
    let cradle_directory = super::cradle_directory();
    gen_manifest(
        &cradle_directory.join("Cargo.toml"),
        &cradle_directory.join("Cargo.template.toml"),
        &std::fs::canonicalize(userlib).expect("Failed to canonicalize userlib path"),
        project_name,
        features.iter().map(|s| s as &str).collect()
    );

    std::env::set_current_dir(cradle_directory).expect("Failed to set working directory");
    if update_deps {
        print_step(format_args!("Updating dependencies..."));
        let e = std::process::Command::new("cargo")
            .args(&["update"])
            .spawn()
            .expect("Failed to spawn `cargo update`")
            .wait()
            .expect("Failed to wait `cargo update`");
        handle_process_result(e);
    }

    print_step(format_args!("Compiling code..."));
    let mut cmd = std::process::Command::new("cargo");
    cmd.args(&[if after_run { "run" } else { "build" }, "--target", "x86_64-unknown-linux-gnu"]);
    if let Some(p) = ext_asset_path {
        cmd
            .args(&["--features", "UseExternalAssetPath"])
            .env("PERIDOT_EXTERNAL_ASSET_PATH", p);
    }
    let e = cmd
        .spawn()
        .expect("Failed to spawn cargo build command")
        .wait()
        .expect("Failed to wait cargo build command");
    handle_process_result(e);
}

fn print_step(args: std::fmt::Arguments) {
    println!(" {} {}", console::style("*").fg(console::Color::Green).bold(), args);
}

fn handle_process_result(e: ExitStatus) {
    if e.success() { return; }
    
    if let Some(c) = e.code() {
        eprintln!(
            "{}: cargo build command failed with code {:?}",
            console::style("ERROR").bold().fg(console::Color::Red),
            c
        );
        std::process::exit(c);
    } else {
        panic!("Child process killed by signal");
    }
}
