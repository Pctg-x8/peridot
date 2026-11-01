fn main() {
    println!(
        "cargo::rustc-link-search={}",
        std::path::PathBuf::from(std::env::var_os("VULKAN_SDK").expect("no VULKAN_SDK"))
            .join("Lib")
            .display()
    );

    // inject manifest
    let mut r = winres::WindowsResource::new();
    r.set_manifest_file("./peridot-marble-editor.exe.manifest");
    r.compile().expect("Failed to compile resource");
}
