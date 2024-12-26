use crate::manifest::*;
use crate::steps;
use crate::subcommands::build::BuildMode;
use std::path::Path;

pub fn build(options: &super::BuildOptions, build_mode: BuildMode) {
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
        options.entry_fn_name,
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
        env.insert("PACKAGE_ID", options.appid);
        let cargo_step = CargoNdk::new(&ctx, compile_version)
            .with_env(env)
            .with_ext_features(options.engine_features.clone());
        match build_mode {
            BuildMode::Normal | BuildMode::Run => cargo_step.build(),
            BuildMode::Test => cargo_step.test(),
            BuildMode::Check => cargo_step.check(),
        }

        if build_mode == BuildMode::Normal || build_mode == BuildMode::Run {
            if !jnilibs_path.exists() {
                std::fs::create_dir_all(&jnilibs_path)
                    .expect("Failed to create jniLibs directory for arm64-v8a");
            }
            std::fs::rename(&result_file_path, jnilibs_path.join("libpegamelib.so"))
                .expect("Failed to rename lib");
        }
    });

    if build_mode == BuildMode::Normal || build_mode == BuildMode::Run {
        std::env::set_current_dir(ctx.cradle_directory.join("apkbuild"))
            .expect("Failed to change working directory for apkbuild");
        build_apk(&ctx);
        if build_mode == BuildMode::Run {
            run_apk(&ctx, options.appid);
        }
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

struct CargoNdk<'s> {
    ctx: &'s steps::BuildContext<'s>,
    ext_features: Vec<&'s str>,
    env: std::collections::HashMap<&'s str, &'s str>,
    target_spec: &'s str,
    platform_target: u32,
}
impl<'s> CargoNdk<'s> {
    pub fn new(ctx: &'s steps::BuildContext<'s>, platform_target: u32) -> Self {
        Self {
            ctx,
            platform_target,
            ext_features: Vec::new(),
            env: std::collections::HashMap::new(),
            target_spec: "arm64-v8a",
        }
    }

    pub fn with_ext_features(mut self, ext_features: Vec<&'s str>) -> Self {
        self.ext_features = ext_features;
        self
    }

    pub fn with_env(mut self, env: std::collections::HashMap<&'s str, &'s str>) -> Self {
        self.env = env;
        self
    }

    fn run_raw_subcommand(self, subcmd: &str) {
        self.ctx.print_step("Compiling code with ndk...");

        let mut cmd = std::process::Command::new("cargo");
        cmd.envs(self.env).args(&[
            "ndk",
            "--target",
            self.target_spec,
            "--platform",
            &self.platform_target.to_string(),
            subcmd,
        ]);
        if !self.ext_features.is_empty() {
            cmd.args(vec![
                String::from("--features"),
                self.ext_features.join(","),
            ]);
        }

        let e = cmd
            .spawn()
            .expect("Failed to spawn cargo ndk")
            .wait()
            .expect("Failed to wait cargo ndk");
        crate::shellutil::handle_process_result("`cargo ndk`", e);
    }

    pub fn build(self) {
        self.run_raw_subcommand("build")
    }

    pub fn test(self) {
        self.run_raw_subcommand("test")
    }

    pub fn check(self) {
        self.run_raw_subcommand("check")
    }
}

pub struct GradleWrapper {}
impl GradleWrapper {
    pub fn new() -> Self {
        Self {}
    }

    pub fn assemble_debug(self) {
        #[cfg(windows)]
        let mut cmd = {
            let mut cmd = std::process::Command::new("cmd");
            cmd.args(&["/c", "gradlew.bat"]);
            cmd
        };
        #[cfg(not(windows))]
        let mut cmd = std::process::Command::new("./gradlew");

        let e = cmd
            .arg("assembleDebug")
            .spawn()
            .expect("Failed to spawn gradle")
            .wait()
            .expect("Failed to wait gradle");
        crate::shellutil::handle_process_result("gradle", e);
    }
}

fn build_apk(ctx: &steps::BuildContext) {
    ctx.print_step("Building apk...");

    GradleWrapper::new().assemble_debug();
}

pub struct AndroidDeviceBridge {
    exec_path: std::path::PathBuf,
}
impl AndroidDeviceBridge {
    pub fn new() -> Self {
        Self {
            exec_path: std::path::PathBuf::from(
                std::env::var("ANDROID_HOME").expect("no ANDROID_HOME set"),
            )
            .join("platform-tools/adb"),
        }
    }

    #[inline]
    pub fn uninstall(&self, package_id: &str) -> std::io::Result<()> {
        std::process::Command::new(&self.exec_path)
            .args(["uninstall", package_id])
            .spawn()?
            .wait()?;

        Ok(())
    }

    #[inline]
    pub fn install(&self, apk_path: &std::path::Path) -> std::io::Result<std::process::ExitStatus> {
        std::process::Command::new(&self.exec_path)
            .arg("install")
            .arg(apk_path)
            .spawn()?
            .wait()
    }

    #[inline]
    pub fn start_activity(
        &self,
        package_id: &str,
        activity_class: &str,
    ) -> std::io::Result<std::process::ExitStatus> {
        let launch_str = format!("{package_id}/{activity_class}");
        std::process::Command::new(&self.exec_path)
            .args(["shell", "am", "start", "-n", &launch_str])
            .spawn()?
            .wait()
    }
}

fn run_apk(ctx: &steps::BuildContext, package_id: &str) {
    ctx.print_step("Installing/Launching apk...");

    let adb = AndroidDeviceBridge::new();
    let apk_path = ctx
        .cradle_directory
        .join("apkbuild/app/build/outputs/apk/debug/app-debug.apk");
    adb.uninstall(package_id)
        .expect("Failed to exec uninstall command");
    crate::shellutil::handle_process_result(
        "adb install command",
        adb.install(&apk_path)
            .expect("Failed to exec install command"),
    );

    crate::shellutil::handle_process_result(
        "`adb shell am start`",
        adb.start_activity(package_id, "io.ct2.peridot.PeridotActivity")
            .expect("Failed to exec start-activity command"),
    );
}
