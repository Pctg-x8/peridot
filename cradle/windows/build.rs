fn main() {
    println!("cargo::rustc-link-search=static={}/Lib", env!("VULKAN_SDK"));

    if cfg!(feature = "IterationBuild") {
        println!("cargo::rerun-if-env-changed=PERIDOT_BUILTIN_ASSET_PATH");
    }
}
