use build_script_helper::{
    peridot_build_skip_cdeps, peridot_build_switch_enable, peridot_build_watch_skip_cdeps,
};

fn main() {
    build_cdeps();
}

fn build_cdeps() {
    peridot_build_watch_skip_cdeps();
    if peridot_build_skip_cdeps() {
        return;
    }

    let source_repo_path = std::env::current_dir()
        .expect("current_dir")
        .join("source-repo");

    if !peridot_build_switch_enable!("TP_SPIRV_TOOLS_SKIP_CMAKE") {
        let build_dir_path = source_repo_path.join("build");
        if !build_dir_path.exists() {
            std::fs::create_dir(&build_dir_path).expect("create_dir for build");
        }

        let mut cmd = std::process::Command::new("cmake");
        cmd.current_dir(&build_dir_path).args(&[
            "-G",
            "Ninja",
            "-DSPIRV_SKIP_TESTS=ON",
            "-DSPIRV_SKIP_EXECUTABLES=ON",
            "..",
        ]);
        let r = cmd.spawn().expect("cmake").wait().expect("wait: cmake");
        if !r.success() {
            panic!("cmake prepare failed with exit code {r:?}");
        }

        let mut cmd = std::process::Command::new("cmake");
        cmd.current_dir(&build_dir_path).args(&["--build", "."]);
        let r = cmd
            .spawn()
            .expect("cmake build")
            .wait()
            .expect("wait: cmake build");
        if !r.success() {
            panic!("cmake build failed with exit code {r:?}");
        }
    }

    println!(
        "cargo::rustc-link-search={}",
        source_repo_path.join("build/source").display()
    );
    println!("cargo::rustc-link-lib=SPIRV-Tools-shared");
}
