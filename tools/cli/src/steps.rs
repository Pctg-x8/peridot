//! Build Steps

use std::collections::HashMap;
use std::path::Path;
use std::process::ExitStatus;

pub enum BuildStep<'s> {
    GenManifest {
        userlib_path: &'s Path,
        userlib_name: &'s str,
        features: Vec<&'s str>
    },
    UpdateDeps,
    BuildWithCargo {
        subcmd: &'s str,
        ext_features: Vec<&'s str>,
        env: HashMap<&'s str, &'s str>,
        target_spec: Option<&'s str>
    }
}
pub fn run_steps<'s>(cradle_subdirectory_path: &str, steps: impl Iterator<Item = BuildStep<'s>>) {
    let cradle_directory = crate::platform::cradle_directory().join(cradle_subdirectory_path);

    for s in steps {
        match s {
            BuildStep::GenManifest { userlib_path, userlib_name, features } => {
                print_step(cradle_subdirectory_path, format_args!("Generating manifest..."));

                crate::manifest::gen_manifest(
                    &cradle_directory.join("Cargo.toml"),
                    &cradle_directory.join("Cargo.template.toml"),
                    &std::fs::canonicalize(userlib_path).expect("Failed to canonicalize userlib path"),
                    userlib_name,
                    features
                );
            },
            BuildStep::UpdateDeps => {
                print_step(cradle_subdirectory_path, format_args!("Updating dependencies..."));

                std::env::set_current_dir(&cradle_directory).expect("Failed to enter cradle directory");
                let e = std::process::Command::new("cargo")
                    .args(&["update"])
                    .spawn()
                    .expect("Failed to spawn `cargo update`")
                    .wait()
                    .expect("Failed to wait `cargo update`");
                handle_process_result(e);
            },
            BuildStep::BuildWithCargo { subcmd, ext_features, env, target_spec } => {
                print_step(cradle_subdirectory_path, format_args!("Compiling code..."));

                std::env::set_current_dir(&cradle_directory).expect("Failed to enter cradle directory");
                let ext_features = ext_features.join(",");
                let mut cmd = std::process::Command::new("cargo");
                cmd.arg(subcmd).envs(env);
                if let Some(t) = target_spec {
                    cmd.args(&["--target", t]);
                }
                if !ext_features.is_empty() {
                    cmd.args(&["--features", &ext_features]);
                }
                let e = cmd
                    .spawn()
                    .expect("Failed to spawn cargo build command")
                    .wait()
                    .expect("Failed to wait cargo build command");
                handle_process_result(e);
            }
        }
    }
}

pub fn print_step(cradle_subdir_path: &str, args: std::fmt::Arguments) {
    println!(
        " {} [{}] {}",
        console::style("*").fg(console::Color::Green).bold(),
        console::style(cradle_subdir_path).fg(console::Color::White).bright(),
        args
    );
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
