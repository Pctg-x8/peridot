fn main() {
    println!("cargo:rustc-link-search=static={}/Lib", env!("VK_SDK_PATH"));

    if cfg!(feature = "IterationBuild") {
        println!("cargo:rerun-if-env-changed=PERIDOT_BUILTIN_ASSET_PATH");
    }

    winres::WindowsResource::new()
        .add_toolkit_include(true)
        .set_manifest(include_str!("manifest.xml"))
        .append_rc_content(include_str!("app.rc"))
        .compile()
        .expect("Failed to compile resource file");
}
