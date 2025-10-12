use std::borrow::Cow;

use pbxproj::{Decodable, ElementWrite};

use crate::manifest::*;
use crate::project::PlatformConfiguration;
use crate::steps;
use crate::subcommands::build::BuildMode;

pub fn build(
    options: &super::BuildOptions,
    project_config: &PlatformConfiguration,
    build_mode: BuildMode,
) {
    let postlink = build_mode == BuildMode::Normal || build_mode == BuildMode::Run;
    let after_run = build_mode == BuildMode::Run;

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

    super::print_start_build("macOS", project_name);

    let ctx = steps::BuildContext::new("mac");
    steps::gen_manifest(
        &ctx,
        options.userlib,
        project_name,
        options.features.clone(),
    );
    steps::gen_userlib_import_code(
        &ctx,
        project_name,
        project_config.title.unwrap_or(project_name),
        &project_version,
        options.entry_fn_name,
    );
    steps::package_assets(
        &ctx,
        options.ext_asset_path.as_deref(),
        &ctx.cradle_directory.join("assets.par"),
    );

    ctx.within_cradle_dir(|| {
        if options.update_deps {
            steps::update_deps(&ctx);
        }

        let mut cargo = steps::cargo(&ctx).with_ext_features(options.engine_features.clone());
        if options.release {
            cargo = cargo.enable_release_build();
        }
        match build_mode {
            BuildMode::Normal | BuildMode::Run => cargo.build(),
            BuildMode::Test => cargo.test(),
            BuildMode::Check => cargo.check(),
        }
    });

    if postlink {
        build_app_bundle(&ctx);
    }
    if after_run {
        let executable_path = ctx
            .cradle_directory
            .join("peridot-cradle/build/Debug/peridot-cradle.app");

        LLDB::new(&executable_path).with_init_command("run").run();
    }
}

fn build_app_bundle(ctx: &steps::BuildContext) {
    ctx.print_step("Building app bundle...");

    let xcode_project_dir_path = ctx.cradle_directory.join("peridot-cradle");
    let xcode_project_template_dir_path = ctx.cradle_directory.join("peridot-cradle.template");

    // restore xcode project from template
    if xcode_project_dir_path.exists() {
        std::fs::remove_dir_all(&xcode_project_dir_path)
            .expect("Failed to remove old xcode project");
    }
    crate::shellutil::handle_process_result(
        "restore xcodeproj",
        crate::shellutil::sh_mirror(
            &xcode_project_template_dir_path,
            &xcode_project_dir_path,
            &[],
        )
        .expect("Failed to spawn sh_mirror"),
    );

    // copy assets/binaries
    std::fs::copy(
        ctx.cradle_directory.join("assets.par"),
        xcode_project_dir_path.join("assets.par"),
    )
    .expect("Failed to copy assets archive");
    let rust_library_path = xcode_project_dir_path.join("rlibs");
    if !rust_library_path.exists() {
        std::fs::create_dir_all(&rust_library_path).expect("Failed to create rust library path");
    }
    std::fs::copy(
        ctx.cradle_directory.join("target/debug/libpegamelib.a"),
        rust_library_path.join("libpegamelib.a"),
    )
    .expect("Failed to copy built library");

    // tweak pbxproj
    let pbxproj_path = xcode_project_dir_path.join("peridot-cradle.xcodeproj/project.pbxproj");
    let pbxproj_content = std::fs::read_to_string(&pbxproj_path).expect("Failed to read pbxproj");
    let mut ps = pbxproj::ParserState::new(&pbxproj_content);
    ps.skip_spaces();
    let mut pbxproj = pbxproj::PBXProjectFile::decode(
        pbxproj::parse_value(&mut ps).expect("Failed to parse root object"),
    )
    .expect("Failed to decode to PBXProjectFile");

    let system_vk_sdk_path = std::env::var("VULKAN_SDK").expect("VULKAN_SDK not set");
    let mut build_configuration_ids = Vec::new();
    for t in pbxproj.root_project().targets.iter() {
        match t.entity(&pbxproj).expect("no target entity found") {
            pbxproj::PBXObject::NativeTarget(t) => {
                for bc in t
                    .build_configuration_list
                    .entity(&pbxproj)
                    .expect("no buildConfigurationList entity")
                    .build_configurations
                    .iter()
                {
                    build_configuration_ids.push(bc.clone());
                }
            }
            t => eprintln!("unknown target type: {t:?}"),
        }
    }
    for bc in build_configuration_ids {
        bc.entity_mut(&mut pbxproj)
            .expect("no buildConfiguration entity")
            .build_settings
            .insert(
                "VULKAN_SDK",
                pbxproj::Value::Single(Cow::Borrowed(&system_vk_sdk_path)),
            );
    }

    pbxproj
        .write(&mut pbxproj::Writer::new(
            std::fs::File::options()
                .create(true)
                .truncate(true)
                .write(true)
                .open(&pbxproj_path)
                .expect("Failed to open project file"),
        ))
        .expect("Failed to write pbxproj");

    XcodeBuild::new(&xcode_project_dir_path.join("peridot-cradle.xcodeproj"))
        .with_configuration("Debug")
        .build();
}

pub struct LLDB<'s> {
    init_command: Option<&'s str>,
    executable_path: &'s std::path::Path,
}
impl<'s> LLDB<'s> {
    pub fn new(executable_path: &'s std::path::Path) -> Self {
        Self {
            executable_path,
            init_command: None,
        }
    }

    pub fn with_init_command(mut self, command: &'s str) -> Self {
        self.init_command = Some(command);
        self
    }

    pub fn run(self) {
        let mut cmd = std::process::Command::new("lldb");
        if let Some(c) = self.init_command {
            cmd.args(&["-o", c]);
        }

        let e = cmd
            .arg(self.executable_path)
            .spawn()
            .expect("Failed to spawn lldb session")
            .wait()
            .expect("Failed to wait lldb section");
        crate::shellutil::handle_process_result("`lldb`", e);
    }
}

pub struct XcodeBuild<'s> {
    project_path: &'s std::path::Path,
    configuration: Option<&'s str>,
}
impl<'s> XcodeBuild<'s> {
    pub fn new(project_path: &'s std::path::Path) -> Self {
        Self {
            project_path,
            configuration: None,
        }
    }

    pub fn with_configuration(mut self, configuration: &'s str) -> Self {
        self.configuration = Some(configuration);
        self
    }

    pub fn build(self) {
        let mut cmd = std::process::Command::new("xcodebuild");
        cmd.arg("-project").arg(self.project_path);
        if let Some(cfg) = self.configuration {
            cmd.args(&["-configuration", cfg]);
        }

        let e = cmd
            .arg("build")
            .spawn()
            .expect("Failed to spawn xcodebuild")
            .wait()
            .expect("Failed to wait xcodebuild");
        crate::shellutil::handle_process_result("`xcodebuild`", e);
    }
}
