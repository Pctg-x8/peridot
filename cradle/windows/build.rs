fn main() {
    #[cfg(not(feature = "bedrock/DynamicLoaded"))]
    println!("cargo:rustc-link-search=static={}/Lib", env!("VK_SDK_PATH"));
}
