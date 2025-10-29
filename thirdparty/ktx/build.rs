use std::path::PathBuf;

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

    // source-repo: https://github.com/KhronosGroup/KTX-Software
    let source_repo_path = std::env::current_dir()
        .expect("Failed to get current dir")
        .join("source-repo");

    if !peridot_build_switch_enable!("TP_KTX_SKIP_CMAKE") {
        let mut cmd = std::process::Command::new("cmake");
        cmd.args(&[
            ".",
            "-B",
            "build",
            "-DKTX_FEATURE_TESTS=OFF",
            "-DKTX_FEATURE_VK_UPLOAD=OFF",
            "-DKTX_FEATURE_GL_UPLOAD=OFF",
            "-DKTX_FEATURE_TOOLS=OFF",
            "--fresh",
        ]);

        if std::env::var_os("TARGET").is_some_and(|e| e == "aarch64-linux-android") {
            // android on arm64-v8a specific args
            cmd.arg(format!(
                "-DANDROID_PLATFORM=android-{}",
                std::env::var("NDK_PLATFORM_TARGET").expect("no NDK_PLATFORM_TARGET")
            ))
            .arg("-DANDROID_ABI=arm64-v8a")
            .arg(format!(
                "-DCMAKE_TOOLCHAIN_FILE={}",
                PathBuf::from(std::env::var_os("ANDROID_NDK").expect("no ANDROID_NDK"))
                    .join("build/cmake/android.toolchain.cmake")
                    .display()
            ))
            // aarch64ではSSEつかえないので切る
            .arg("-DBASISU_SUPPORT_SSE=OFF")
            .arg("-GNinja")
            // shared libだとsoをapkに封入する必要があって面倒なのでstatic libにする
            .arg("-DBUILD_SHARED_LIBS=OFF");
        }

        let r = cmd
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
    }

    if cfg!(windows) && std::env::var_os("CARGO_CFG_WINDOWS").is_some() {
        // どうやらWindows(厳密にはおそらくmsvc)だと出力先が微妙に違うらしい
        println!(
            "cargo::rustc-link-search={}",
            source_repo_path.join("build\\Debug").display()
        );
    } else {
        println!(
            "cargo::rustc-link-search={}",
            source_repo_path.join("build").display()
        );
    }
}
