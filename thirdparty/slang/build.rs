use std::path::PathBuf;

use build_script_helper::{peridot_build_skip_cdeps, peridot_build_switch_enable};

fn main() {
    build_cdeps();
}

fn build_cdeps() {
    if peridot_build_skip_cdeps() {
        return;
    }

    let source_repo_path = std::env::current_dir()
        .expect("Failed to get current dir")
        .join("source-repo");

    if !peridot_build_switch_enable!("TP_SLANG_SKIP_CMAKE") {
        let configure_preset = std::env::var("PERIDOT_BUILD_TP_SLANG_CONFIGURE_PRESET");
        let r = std::process::Command::new("cmake")
            .args(&[
                "--preset",
                configure_preset.as_deref().unwrap_or("default"),
                "-DSLANG_ENABLE_SLANG_RHI=FALSE",
                "-DSLANG_ENABLE_GFX=FALSE",
                "-DSLANG_ENABLE_SLANGD=FALSE",
                "-DSLANG_ENABLE_SLANGC=FALSE",
                "-DSLANG_ENABLE_SLANGI=FALSE",
                "-DSLANG_ENABLE_SLANGRT=FALSE",
                "-DSLANG_ENABLE_TESTS=FALSE",
                "-DSLANG_ENABLE_EXAMPLES=FALSE",
            ])
            .current_dir(&source_repo_path)
            .spawn()
            .expect("Failed to spwan cmake")
            .wait()
            .expect("executing cmake prepare");
        if !r.success() {
            panic!("cmake prepare failed with exit code {r:?}");
        }

        let r = std::process::Command::new("cmake")
            .args(&["--build", "--preset", "releaseWithDebugInfo"])
            .current_dir(&source_repo_path)
            .spawn()
            .expect("Failed to spawn cmake")
            .wait()
            .expect("executing cmake build");
        if !r.success() {
            panic!("cmake build failed with exit code {r:?}");
        }
    }

    println!("cargo::rustc-link-lib=slang");
    println!(
        "cargo::rustc-link-search={}",
        std::env::var_os("PERIDOT_BUILD_TP_SLANG_LIB_PATH")
            .map_or_else(
                || source_repo_path.join("build/RelWithDebInfo/lib"),
                PathBuf::from,
            )
            .display()
    );
}
