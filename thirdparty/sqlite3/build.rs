#[cfg(windows)]
fn main() {
    use vswhere::VSWhere;
    use win10_sdk_locator::Windows10SdkInstallationRegistry;

    let source_repo_path = std::env::current_dir()
        .expect("current_dir")
        .join("source-repo");

    // locate visual studio
    let vs_installation = VSWhere::default()
        .latest()
        .property("resolvedInstallationPath")
        .get_output()
        .expect("vswhere");
    let vs_installation = vs_installation
        .extract_single_path()
        .expect("invalid output from vswher");

    // locate win10 sdk
    let sdk_registry =
        Windows10SdkInstallationRegistry::open().expect("failed to open registry key");
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

    let vs_buildtool_path =
        latest_installed_build_tool_path.join(format!(r#"bin\{host_dir_name}\{target_dir_name}"#));
    let vs_include_path = latest_installed_build_tool_path.join(format!(r#"include"#));
    let vs_lib_path = latest_installed_build_tool_path.join(format!(r#"lib\{target_dir_name}"#));
    let ucrt_include_path = installation_folder.join(format!("Include\\{product_version}.0\\ucrt"));
    let um_include_path = installation_folder.join(format!("Include\\{product_version}.0\\um"));
    let shared_include_path =
        installation_folder.join(format!("Include\\{product_version}.0\\shared"));
    let um_lib_path = installation_folder.join(format!("Lib\\{product_version}.0\\um\\x64"));
    let ucrt_lib_path = installation_folder.join(format!("Lib\\{product_version}.0\\ucrt\\x64"));
    let win10sdk_bin_path = installation_folder.join(format!("bin\\{product_version}.0\\x64"));

    let newenv_path = std::env::var("PATH").unwrap_or_default()
        + &format!(
            ";{};{}",
            vs_buildtool_path.display(),
            win10sdk_bin_path.display()
        );
    let res = std::process::Command::new("nmake")
        .args(["/f", "Makefile.msc", "libsqlite3.lib"])
        .current_dir(&source_repo_path)
        // Note: cargoかなんかがこれを設定していてMakefile.msc内の条件式がエラーになるので上書きする
        .env(
            "DEBUG",
            std::env::var("DEBUG").map_or("0", |x| {
                if x.eq_ignore_ascii_case("true") {
                    "1"
                } else {
                    "0"
                }
            }),
        )
        .env("PATH", newenv_path)
        .env(
            "INCLUDE",
            format!(
                "{};{};{};{}",
                vs_include_path.display(),
                ucrt_include_path.display(),
                um_include_path.display(),
                shared_include_path.display()
            ),
        )
        .env(
            "LIB",
            format!(
                "{};{};{}",
                ucrt_lib_path.display(),
                um_lib_path.display(),
                vs_lib_path.display()
            ),
        )
        .status()
        .expect("failed");
    if !res.success() {
        panic!("nmake exited with status: {}", res);
    }

    println!("cargo::rustc-link-search={}", source_repo_path.display());
    println!("cargo::rustc-link-lib=libsqlite3");
}

#[cfg(unix)]
fn main() {
    let target = std::env::var("TARGET").expect("no target?");
    let clib_build_path = std::env::current_dir()
        .expect("current_dir")
        .join(format!("clib-build/{target}"));
    let cc = std::env::var_os(format!("CC_{target}")).or_else(|| std::env::var_os("CC"));
    let cflags = std::env::var_os(format!("CFLAGS_{target}")).or_else(|| std::env::var_os("CFLGS"));
    println!("build dir: {}", clib_build_path.display());

    let source_repo_path = std::env::current_dir()
        .expect("current_dir")
        .join("source-repo");
    if !source_repo_path.join("Makefile").exists() {
        let r = std::process::Command::new("/bin/sh")
            .args(["./configure"])
            .current_dir(&source_repo_path)
            .status()
            .expect("configure");
        if !r.success() {
            panic!("configure exited with code {r:?}");
        }
    }

    let r = std::process::Command::new("make")
        .args(["sqlite3.c"])
        .current_dir(&source_repo_path)
        .status()
        .expect("make");
    if !r.success() {
        panic!("make exited with code {r:?}");
    }

    let r = std::process::Command::new("make")
        .current_dir(&clib_build_path)
        .envs(
            [cc.map(|x| ("CC", x)), cflags.map(|x| ("CFLAGS", x))]
                .into_iter()
                .flatten(),
        )
        .status()
        .expect("platform make");
    if !r.success() {
        panic!("make exited with code {r:?}");
    }

    println!("cargo::rustc-link-search={}", clib_build_path.display());
    println!("cargo::rustc-link-lib=sqlite3");
}
