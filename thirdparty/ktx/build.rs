fn main() {
    // source-repo: https://github.com/KhronosGroup/KTX-Software
    let source_repo_path = std::env::current_dir()
        .expect("Failed to get current dir")
        .join("source-repo");

    let r = std::process::Command::new("cmake")
        .args(&[
            ".",
            "-B",
            "build",
            "-DCMAKE_CXX_COMPILER_LAUNCHER=ccache",
            "-DCMAKE_C_COMPILER_LAUNCHER=ccache",
            "-DKTX_FEATURE_TESTS=OFF",
            "-DKTX_FEATURE_VK_UPLOAD=OFF",
            "-DKTX_FEATURE_GL_UPLOAD=OFF",
            "-DKTX_FEATURE_TOOLS=OFF",
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
        .args(&["--build", "build"])
        .current_dir(&source_repo_path)
        .spawn()
        .expect("Failed to spawn cmake")
        .wait()
        .expect("executing cmake build");
    if !r.success() {
        panic!("cmake build failed with exit code {r:?}");
    }

    // どうやらWindows(厳密にはおそらくmsvc)だと出力先が微妙に違うらしい
    #[cfg(not(windows))]
    println!(
        "cargo::rustc-link-search={}",
        source_repo_path.join("build").display()
    );
    #[cfg(windows)]
    println!(
        "cargo::rustc-link-search={}",
        source_repo_path.join("build\\Debug").display()
    );
}
