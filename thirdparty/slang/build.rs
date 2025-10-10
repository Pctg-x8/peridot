fn main() {
    let source_repo_path = std::env::current_dir()
        .expect("Failed to get current dir")
        .join("source-repo");

    let configure_preset = std::env::var("PERIDOT_BUILD_TP_SLANG_CONFIGURE_PRESET");
    let r = std::process::Command::new("cmake")
        .args(&[
            "--preset",
            configure_preset.as_deref().unwrap_or("default"),
            "-DSLANG_ENABLE_SLANG_RHI=FALSE",
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
        .args(&[
            "--build",
            "--preset",
            "releaseWithDebugInfo",
            "-DSLANG_ENABLE_SLANG_RHI=FALSE",
            "--target",
            "slang",
            "slang-glslang",
            "slang-glsl-module",
        ])
        .current_dir(&source_repo_path)
        .spawn()
        .expect("Failed to spawn cmake")
        .wait()
        .expect("executing cmake build");
    if !r.success() {
        panic!("cmake build failed with exit code {r:?}");
    }

    println!("cargo::rustc-link-lib=slang");
    println!(
        "cargo::rustc-link-search={}",
        source_repo_path.join("build/RelWithDebInfo/lib").display()
    );
}
