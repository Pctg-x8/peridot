fn main() {
    #[cfg(windows)]
    println!(
        "cargo::rustc-link-search={}",
        std::path::PathBuf::from(std::env::var_os("VK_SDK_PATH").expect("no VK_SDK_PATH"))
            .join("Lib")
            .display()
    );

    #[cfg(windows)]
    inject_windows_manifest();
}

#[cfg(windows)]
fn inject_windows_manifest() {
    let mut r = winres::WindowsResource::new();
    r.set_manifest_file("./peridot-marble-editor.exe.manifest");
    r.compile().expect("Failed to compile resource");
}
