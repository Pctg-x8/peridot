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
        #[cfg(windows)]
        let extra_envs = {
            // setup msvc devenv
            let devenv = MSVCDevelopmentEnv::locate_latest();

            vec![
                (
                    "PATH",
                    format!(
                        "{};{}",
                        std::env::var("PATH").unwrap_or_default(),
                        devenv.additional_paths
                    ),
                ),
                ("INCLUDE", devenv.include),
                ("LIB", devenv.lib),
            ]
        };
        #[cfg(not(windows))]
        let extra_envs = Vec::new();

        let configure_preset = std::env::var("PERIDOT_BUILD_TP_SLANG_CONFIGURE_PRESET");
        let r = std::process::Command::new("cmake")
            .args(&[
                "--preset",
                configure_preset.as_deref().unwrap_or("default"),
                "-DSLANG_ENABLE_DXIL=FALSE",
                "-DSLANG_ENABLE_GFX=FALSE",
                "-DSLANG_ENABLE_SLANGD=FALSE",
                "-DSLANG_ENABLE_SLANGC=FALSE",
                "-DSLANG_ENABLE_SLANGI=FALSE",
                "-DSLANG_ENABLE_SLANGRT=FALSE",
                "-DSLANG_ENABLE_SLANG_GLSLANG=FALSE",
                "-DSLANG_ENABLE_SLANG_PROXY=FALSE",
                "-DSLANG_ENABLE_TESTS=FALSE",
                "-DSLANG_ENABLE_EXAMPLES=FALSE",
                "-DSLANG_ENABLE_REPLAYER=FALSE",
                "-DSLANG_ENABLE_RECORD_REPLAY=FALSE",
                "-DSLANG_ENABLE_SLANG_RHI=FALSE",
                "-DSLANG_LIB_TYPE=STATIC",
            ])
            .current_dir(&source_repo_path)
            .envs(extra_envs.iter().map(|&(a, ref b)| (a, b)))
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
            .envs(extra_envs.iter().map(|&(a, ref b)| (a, b)))
            .spawn()
            .expect("Failed to spawn cmake")
            .wait()
            .expect("executing cmake build");
        if !r.success() {
            panic!("cmake build failed with exit code {r:?}");
        }
    }

    println!("cargo::rustc-link-lib=static=slang-compiler");
    println!("cargo::rustc-link-lib=static=core");
    println!("cargo::rustc-link-lib=static=compiler-core");
    if !cfg!(windows) {
        // Windowsの場合はなしでいけるっぽい
        println!("cargo::rustc-link-lib=stdc++");
    }
    println!(
        "cargo::rustc-link-search={}",
        std::env::var_os("PERIDOT_BUILD_TP_SLANG_LIB_PATH")
            .map_or_else(
                || source_repo_path.join("build/RelWithDebInfo/lib"),
                PathBuf::from,
            )
            .display()
    );

    // slang external deps
    println!("cargo::rustc-link-lib=static=miniz");
    println!(
        "cargo::rustc-link-search={}",
        std::env::var_os("PERIDOT_BUILD_TP_SLANG_LIB_PATH")
            .map_or_else(
                || source_repo_path.join("build/external/miniz/RelWithDebInfo"),
                PathBuf::from,
            )
            .display()
    );
    println!("cargo::rustc-link-lib=static=lz4");
    println!(
        "cargo::rustc-link-search={}",
        std::env::var_os("PERIDOT_BUILD_TP_SLANG_LIB_PATH")
            .map_or_else(
                || source_repo_path.join("build/external/lz4/build/cmake/RelWithDebInfo"),
                PathBuf::from,
            )
            .display()
    );
    println!("cargo::rustc-link-lib=static=cmark-gfm");
    println!(
        "cargo::rustc-link-search={}",
        std::env::var_os("PERIDOT_BUILD_TP_SLANG_LIB_PATH")
            .map_or_else(
                || source_repo_path.join("build/external/cmark/src/RelWithDebInfo"),
                PathBuf::from,
            )
            .display()
    );
}

#[cfg(windows)]
struct MSVCDevelopmentEnv {
    pub additional_paths: String,
    pub include: String,
    pub lib: String,
}
#[cfg(windows)]
impl MSVCDevelopmentEnv {
    pub fn locate_latest() -> Self {
        // locate visual studio
        let vs_installation = vswhere::VSWhere::default()
            .latest()
            .property("resolvedInstallationPath")
            .get_output()
            .expect("vswhere");
        let vs_installation = vs_installation
            .extract_single_path()
            .expect("invalid output from vswher");

        // locate win10 sdk
        let sdk_registry = win10_sdk_locator::Windows10SdkInstallationRegistry::open()
            .expect("failed to open registry key");
        let installation_folder = sdk_registry
            .installation_folder()
            .expect("failed to read installation folder");
        let product_version = sdk_registry
            .product_version()
            .expect("failed to read product version")
            .into_string()
            .expect("invalid product version string");

        let target = std::env::var("TARGET").expect("target unspecified?");
        let build_tools_base_dir = vs_installation.join(r#"VC\Tools\MSVC"#);
        // Note: とりあえず特定ディレクトリ直下のフォルダ名を辞書順で並べて最大のものを最新のBuildToolsとする この出し方が正しいのかは不明
        let latest_installed_build_tool_path = build_tools_base_dir.join(
            std::fs::read_dir(&build_tools_base_dir)
                .expect("buildtool installation enumeration failed")
                .filter_map(|x| {
                    let x = match x {
                        Ok(x) => x,
                        Err(e) => {
                            eprintln!("erroneous entry: {e:?}");
                            return None;
                        }
                    };

                    if !x.metadata().is_ok_and(|x| x.is_dir()) {
                        return None;
                    }

                    Some(x.path())
                })
                .fold(None, |a, b| match a {
                    None => Some(b),
                    Some(a) => Some(a.max(b)),
                })
                .expect("no build tool installation found"),
        );
        let host_dir_name = if cfg!(target_arch = "x86") {
            "Hostx86"
        } else if cfg!(target_arch = "x86_64") {
            "Hostx64"
        } else if cfg!(target_arch = "aarch64") {
            "HostArm64"
        } else {
            unreachable!("unsupported host arch");
        };
        let target_dir_name = if target.starts_with("x86-") {
            "x86"
        } else if target.starts_with("x86_64-") {
            "x64"
        } else if target.starts_with("aarch64-") {
            "arm64"
        } else {
            unreachable!("unsupported target arch");
        };

        let vs_buildtool_path = latest_installed_build_tool_path
            .join(format!(r#"bin\{host_dir_name}\{target_dir_name}"#));
        let vs_include_path = latest_installed_build_tool_path.join(format!(r#"include"#));
        let vs_lib_path =
            latest_installed_build_tool_path.join(format!(r#"lib\{target_dir_name}"#));
        let ucrt_include_path =
            installation_folder.join(format!("Include\\{product_version}.0\\ucrt"));
        let um_include_path = installation_folder.join(format!("Include\\{product_version}.0\\um"));
        let shared_include_path =
            installation_folder.join(format!("Include\\{product_version}.0\\shared"));
        let um_lib_path = installation_folder.join(format!("Lib\\{product_version}.0\\um\\x64"));
        let ucrt_lib_path =
            installation_folder.join(format!("Lib\\{product_version}.0\\ucrt\\x64"));
        let win10sdk_bin_path = installation_folder.join(format!("bin\\{product_version}.0\\x64"));

        Self {
            additional_paths: format!(
                "{};{}",
                vs_buildtool_path.display(),
                win10sdk_bin_path.display()
            ),
            include: format!(
                "{};{};{};{}",
                vs_include_path.display(),
                ucrt_include_path.display(),
                um_include_path.display(),
                shared_include_path.display()
            ),
            lib: format!(
                "{};{};{}",
                ucrt_lib_path.display(),
                um_lib_path.display(),
                vs_lib_path.display()
            ),
        }
    }
}
