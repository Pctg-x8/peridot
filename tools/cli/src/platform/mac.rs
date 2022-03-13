
use crate::manifest::*;
use crate::steps;

pub fn build(options: &super::BuildOptions, cargo_cmd: &str) {
    let postlink = cargo_cmd == "build" || cargo_cmd == "run";
    let after_run = cargo_cmd == "run";

    let user_manifest_loaded = std::fs::read_to_string(options.userlib.join("Cargo.toml"))
        .expect("Failed to load Userlib Cargo.toml");
    let user_manifest: CargoManifest = toml::from_str(&user_manifest_loaded)
        .expect("Failed to parse Userlib Cargo.toml");
    let project_name = user_manifest.package.as_ref().and_then(|p| p.name.as_deref())
        .unwrap_or("?Unnamed Project?");
    super::print_start_build("macOS", project_name);

    let ctx = steps::BuildContext::new("mac");
    steps::gen_manifest(&ctx, options.userlib, project_name, options.features.clone());
    steps::gen_userlib_import_code(&ctx, project_name, options.entry_ty_name);
    steps::package_assets(&ctx, options.ext_asset_path, &ctx.cradle_directory.join("peridot-cradle/assets.par"));

    ctx.within_cradle_dir(|| {
        if options.update_deps {
            steps::update_deps(&ctx);
        }
        
        let env = std::collections::HashMap::new();
        let ext_features = options.engine_features.clone();
        steps::cargo(
            &ctx,
            if cargo_cmd == "run" { "build" } else { cargo_cmd },
            ext_features,
            env,
            None,
            options.release
        );
    });

    if postlink {
        build_app_bundle(&ctx);
    }
    if after_run {
        let executable_path = ctx.cradle_directory.join("peridot-cradle/build/Debug/peridot-cradle.app");
        let e = std::process::Command::new("lldb")
            .args(&[
                "-o",
                "run",
                executable_path.to_str().expect("invalid sequence in executable path")
            ])
            .spawn()
            .expect("Failed to spawn lldb session")
            .wait()
            .expect("Failed to wait lldb section");
        crate::shellutil::handle_process_result("`lldb`", e);
    }
}

fn build_app_bundle(ctx: &steps::BuildContext) {
    ctx.print_step("Building app bundle...");

    let rust_library_path = ctx.cradle_directory.join("peridot-cradle/rlibs");
    if !rust_library_path.exists() {
        std::fs::create_dir_all(&rust_library_path)
            .expect("Failed to create rust library path");
    }
    std::fs::copy(
        ctx.cradle_directory.join("target/debug/libpegamelib.a"),
        rust_library_path.join("libpegamelib.a")
    ).expect("Failed to copy built library");

    let xcode_project_path = ctx.cradle_directory.join("peridot-cradle/peridot-cradle.xcodeproj");
    let e = std::process::Command::new("xcodebuild")
        .args(&[
            "-project",
            xcode_project_path.to_str().expect("invalid sequence in xcode project path"),
            "-configuration",
            "Debug",
            "build"
        ])
        .spawn()
        .expect("Failed to spawn xcodebuild")
        .wait()
        .expect("Failed to wait xcodebuild");
    crate::shellutil::handle_process_result("`xcodebuild`", e);
}
