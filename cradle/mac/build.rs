
fn main() {
    let mvk_sdk_path = std::path::PathBuf::from(env!("VULKAN_SDK")).parent().expect("parent detection failed").to_path_buf();

    println!("cargo:rustc-link-search=framework={}", mvk_sdk_path.join("MoltenVK").display());
    println!("cargo:rustc-link-search=framework={}", mvk_sdk_path.join("macOS/Frameworks").display());
    println!("cargo:rustc-link-lib=c++");
    println!("cargo:rustc-link-lib=framework=IOSurface");
    println!("cargo:rustc-link-lib=framework=IOKit");
    println!("cargo:rustc-link-lib=framework=vulkan");
}
