use crate::manifest::*;
use crate::steps;
use std::path::Path;

pub fn build(options: &super::BuildOptions, cargo_cmd: &str) {
    let user_manifest_loaded = std::fs::read_to_string(options.userlib.join("Cargo.toml"))
        .expect("Failed to load Userlib Cargo.toml");
    let user_manifest: CargoManifest =
        toml::from_str(&user_manifest_loaded).expect("Failed to parse Userlib Cargo.toml");
    let project_name = user_manifest
        .package
        .as_ref()
        .and_then(|p| p.name.as_deref())
        .unwrap_or("<Unnamed Peridot Project>");
    let project_version = semver::Version::parse(
        user_manifest
            .package
            .as_ref()
            .and_then(|p| p.version.as_deref())
            .unwrap_or("0.0.0"),
    )
    .expect("illformed project version");

    super::print_start_build("Android", project_name);

    let ctx = steps::BuildContext::new("android");
    steps::gen_manifest(
        &ctx,
        options.userlib,
        project_name,
        options.features.clone(),
    );
    gen_build_files(&ctx, options.appid);
    // Note: Androidはタイトルを別ファイル（string.xml）で指定するので、ここで渡した値は使われない
    steps::gen_userlib_import_code(
        &ctx,
        project_name,
        project_name,
        &project_version,
        options.entry_ty_name,
    );
    merge_resource_directory(&ctx, options.userlib);
    mirror_ext_libraries(&ctx, options.userlib);
    steps::merge_assets(
        &ctx,
        &ctx.cradle_directory.join("assets"),
        options.ext_asset_path.as_deref(),
    );

    ctx.within_cradle_dir(|| {
        let gradle_build_target_path = ctx.cradle_directory.join("target/arm64-v8a-linux-android");
        let cargo_build_target_path = ctx.cradle_directory.join("target/aarch64-linux-android");
        let jnilibs_path = ctx
            .cradle_directory
            .join("apkbuild/app/src/main/jniLibs/arm64-v8a");
        let result_file_path = cargo_build_target_path.join("debug/libpegamelib.so");

        if gradle_build_target_path.exists() {
            // これいるんだろうか
            std::fs::rename(&gradle_build_target_path, &cargo_build_target_path)
                .expect("Failed to rename build target directory");
        }
        if options.update_deps {
            steps::update_deps(&ctx);
        }

        let compile_version = std::env::var("NDK_PLATFORM_TARGET")
            .expect("no NDK_PLATFORM_TARGET set?")
            .parse()
            .expect("invalid number in NDK_PLATFORM_TARGET");
        let mut env = std::collections::HashMap::new();
        let ext_features = options.engine_features.clone();
        env.insert("PACKAGE_ID", options.appid);
        cargo_ndk(
            &ctx,
            if cargo_cmd == "run" {
                "build"
            } else {
                cargo_cmd
            },
            ext_features,
            env,
            "arm64-v8a",
            compile_version,
        );

        if !jnilibs_path.exists() {
            std::fs::create_dir_all(&jnilibs_path)
                .expect("Failed to create jniLibs directory for arm64-v8a");
        }
        std::fs::rename(&result_file_path, jnilibs_path.join("libpegamelib.so"))
            .expect("Failed to rename lib");
    });

    std::env::set_current_dir(ctx.cradle_directory.join("apkbuild"))
        .expect("Failed to change working directory for apkbuild");
    build_apk(&ctx);
    if cargo_cmd == "run" {
        run_apk(&ctx, options.appid);
    }
}

fn gen_build_files(ctx: &steps::BuildContext, appid: &str) {
    ctx.print_step("Generating build files...");

    let android_app_base = ctx.cradle_directory.join("apkbuild/app");
    let c = std::fs::read_to_string(android_app_base.join("build-template.gradle"))
        .expect("Failed to read template gradle script");
    let c = c.replace("**APKAPPID**", &format!("'{}'", appid));
    std::fs::write(android_app_base.join("build.gradle"), c)
        .expect("Failed to write gradle script");

    let c = std::fs::read_to_string(android_app_base.join("src/main/AndroidManifest-template.xml"))
        .expect("Failed to read template android manifest");
    let c = c.replace("**APKAPPID**", appid);
    std::fs::write(android_app_base.join("src/main/AndroidManifest.xml"), c)
        .expect("Failed to write android manifest");
}
fn merge_resource_directory(ctx: &steps::BuildContext, userlib: &Path) {
    ctx.print_step("Merging Customized resource directories...");

    let resource_path = userlib.join("android-res");
    let target_path = ctx.cradle_directory.join("apkbuild/app/src/main/res");
    let default_res_path = ctx
        .cradle_directory
        .join("apkbuild/app/src/main/res-default");
    // Make default structure then mirrors the user-defined structure,
    // results an user-customized resource structure
    crate::shellutil::handle_process_result(
        "default sync command",
        crate::shellutil::sh_mirror(&default_res_path, &target_path, &[])
            .expect("Failed to run default mirror command"),
    );
    if resource_path.exists() {
        crate::shellutil::handle_process_result(
            "appending copy command",
            crate::shellutil::sh_append_copy(&resource_path, &target_path)
                .expect("Failed to run appending copy command"),
        );
    }
}
fn mirror_ext_libraries(ctx: &steps::BuildContext, userlib: &Path) {
    ctx.print_step("Mirroring External libraries...");

    let extlib_path = userlib.join("extlib/android");
    if extlib_path.exists() {
        let target_path = ctx.cradle_directory.join("apkbuild/app/src/main/jniLibs");
        crate::shellutil::handle_process_result(
            "sync command",
            crate::shellutil::sh_mirror(&extlib_path, &target_path, &[".*"])
                .expect("Failed to run mirror command"),
        );
    }
}
fn cargo_ndk(
    ctx: &steps::BuildContext,
    subcmd: &str,
    ext_features: Vec<&str>,
    env: std::collections::HashMap<&str, &str>,
    target_spec: &str,
    ndk_platform_target: u32,
) {
    ctx.print_step("Compiling code with ndk...");

    let mut cmd = std::process::Command::new("cargo");
    let ndk_platform_target_str = ndk_platform_target.to_string();
    cmd.envs(env).args(&[
        "ndk",
        "--target",
        target_spec,
        "--platform",
        &ndk_platform_target_str,
        "--",
        subcmd,
    ]);
    let joined_ext_features = ext_features.join(",");
    if !joined_ext_features.is_empty() {
        cmd.args(&["--features", &joined_ext_features]);
    }

    let e = cmd
        .spawn()
        .expect("Failed to spawn cargo ndk")
        .wait()
        .expect("Failed to wait cargo ndk");
    crate::shellutil::handle_process_result("`cargo ndk`", e);
}
#[cfg(windows)]
fn build_apk(ctx: &steps::BuildContext) {
    ctx.print_step("Building apk...");

    let e = std::process::Command::new("cmd")
        .args(&["/c", "gradlew.bat", "assembleDebug"])
        .spawn()
        .expect("Failed to spawn gradle")
        .wait()
        .expect("Failed to wait gradle");
    crate::shellutil::handle_process_result("gradle", e);
}
#[cfg(not(windows))]
fn build_apk(ctx: &steps::BuildContext) {
    ctx.print_step("Building apk...");

    let e = std::process::Command::new("./gradlew")
        .args(&["assembleDebug"])
        .spawn()
        .expect("Failed to spawn gradle")
        .wait()
        .expect("Failed to wait gradle");
    crate::shellutil::handle_process_result("gradle", e);
}
fn run_apk(ctx: &steps::BuildContext, package_id: &str) {
    ctx.print_step("Installing apk...");

    let adb = std::path::PathBuf::from(std::env::var("ANDROID_HOME").expect("no ANDROID_HOME set"))
        .join("platform-tools/adb");
    let apk_path = ctx
        .cradle_directory
        .join("apkbuild/app/build/outputs/apk/debug/app-debug.apk");
    std::process::Command::new(&adb)
        .args(&["uninstall", package_id])
        .spawn()
        .expect("Failed to spawn uninstall command")
        .wait()
        .expect("Failed to wait uninstall completion");
    let e = std::process::Command::new(&adb)
        .args(&[
            "install",
            apk_path.to_str().expect("invalid sequence in apk path"),
        ])
        .spawn()
        .expect("Failed to spawn install command")
        .wait()
        .expect("Failed to wait install completion");
    crate::shellutil::handle_process_result("adb install command", e);

    let native_activity_path = format!("{}/jp.ct2.peridot.NativeActivity", package_id);
    let e = std::process::Command::new(&adb)
        .args(&["shell", "am", "start", "-n", &native_activity_path])
        .spawn()
        .expect("Failed to spawn start command")
        .wait()
        .expect("Failed to wait start command");
    crate::shellutil::handle_process_result("`adb shell am start`", e);
}
