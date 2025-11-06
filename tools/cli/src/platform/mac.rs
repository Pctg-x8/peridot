use std::borrow::Cow;
use std::collections::HashMap;

use pbxproj::{Decodable, ElementWrite};

use crate::manifest::*;
use crate::project::PlatformConfiguration;
use crate::steps;
use crate::subcommands::build::BuildMode;
use crate::util_traits::DirectoryPathExt;

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
        build_app_bundle(&ctx, options, options.appid);
    }
    if after_run {
        let executable_path = ctx
            .cradle_directory
            .join("peridot-cradle/build/Debug/peridot-cradle.app");

        LLDB::new(&executable_path).with_init_command("run").run();
    }
}

fn build_app_bundle(ctx: &steps::BuildContext, options: &super::BuildOptions, identifier: &str) {
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
    std::fs::rename(
        ctx.cradle_directory.join("assets.par"),
        xcode_project_dir_path.join("assets.par"),
    )
    .expect("Failed to move assets archive");
    let rust_library_path = xcode_project_dir_path.join("rlibs");
    rust_library_path
        .ensure_directory()
        .expect("check or create `rlibs` directory");
    std::fs::rename(
        ctx.cradle_directory.join("target/debug/libpegamelib.a"),
        rust_library_path.join("libpegamelib.a"),
    )
    .expect("Failed to move built library");

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
        let bc = bc
            .entity_mut(&mut pbxproj)
            .expect("no buildConfiguration entity");

        bc.build_settings.insert(
            "VULKAN_SDK",
            pbxproj::Value::Single(Cow::Borrowed(&system_vk_sdk_path)),
        );
        bc.build_settings.insert(
            "PRODUCT_BUNDLE_IDENTIFIER",
            pbxproj::Value::Single(Cow::Borrowed(identifier)),
        );
    }

    // search extra-libs and import to link
    let extra_libs_dir = options.userlib.join("extra-libs");
    if extra_libs_dir.exists() {
        let target_paths = extra_libs_dir
            .read_dir_recursive()
            .expect("initiate read_dir")
            .filter(|x| {
                x.as_ref().is_ok_and(|x| {
                    x.file_name()
                        .to_str()
                        .is_some_and(|x| x.starts_with("lib") && x.ends_with(".dylib"))
                })
            })
            .map(|x| x.map(|x| x.path()))
            .collect::<Result<Vec<_>, _>>()
            .expect("enumerating external libs");
        let mut additional_build_files = Vec::with_capacity(target_paths.len());
        for (n, p) in target_paths.iter().enumerate() {
            let fileref_id = format!("peridot_tweak_extlib_fileref_{n}");
            let buildfile_id = format!("peridot_tweak_extlib_buildfile_{n}");

            pbxproj.objects.insert(
                Cow::Owned(fileref_id.clone()),
                pbxproj::PBXObject::FileReference(pbxproj::PBXFileReference {
                    name: Some(
                        p.file_name()
                            .expect("no file name")
                            .to_str()
                            .expect("invalid charcode")
                            .to_owned()
                            .into(),
                    ),
                    path: p.to_str().expect("invalid charcode").to_owned().into(),
                    source_tree: "<absolute>".into(),
                    last_known_file_type: Some("compiled.mach-o.dylib".into()),
                    extras: HashMap::new(),
                }),
            );
            pbxproj.objects.insert(
                Cow::Owned(buildfile_id.clone()),
                pbxproj::PBXObject::BuildFile(pbxproj::PBXBuildFile {
                    file_ref: pbxproj::PBXObjectIDRef::from(fileref_id),
                    settings: None,
                    extras: HashMap::new(),
                }),
            );

            additional_build_files.push(buildfile_id);
        }
        let link_phase_ids = pbxproj
            .root_project()
            .targets
            .iter()
            .flat_map(
                |t| match t.entity(&pbxproj).expect("no target entity found") {
                    pbxproj::PBXObject::NativeTarget(t) => t
                        .build_phases
                        .iter()
                        .filter_map(|p| {
                            match p.entity(&pbxproj).expect("no build phase entity found") {
                                pbxproj::PBXObject::FrameworksBuildPhase(_) => Some(p),
                                _ => None,
                            }
                        })
                        .collect::<Vec<_>>(),
                    t => {
                        eprintln!("unknown target type: {t:?}");
                        Vec::new()
                    }
                },
            )
            .cloned()
            .collect::<Vec<_>>();
        for t in link_phase_ids {
            match t.entity_mut(&mut pbxproj).expect("no target entity found") {
                pbxproj::PBXObject::FrameworksBuildPhase(t) => {
                    t.files
                        .extend(additional_build_files.iter().map(|x| x.to_owned().into()));
                }
                _ => unreachable!(),
            }
        }
    }

    // writeback final pbxproj
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
