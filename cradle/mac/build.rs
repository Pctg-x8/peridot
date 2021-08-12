fn main() {
    let vk_sdk_base = std::path::PathBuf::from(env!("VULKAN_SDK"))
        .parent()
        .expect("Failed to calc parent for VULKAN_SDK")
        .to_path_buf();

    println!(
        "cargo:rustc-link-search=framework={}",
        vk_sdk_base.join("MoltenVK/macOS").display()
    );
    println!("cargo:rustc-link-lib=c++");
    println!("cargo:rustc-link-lib=framework=IOSurface");
    println!("cargo:rustc-link-lib=framework=IOKit");
}
