fn main() {
    println!("cargo:rustc-link-search=static={}/Lib", env!("VK_SDK_PATH"));
    
    if cfg!(feature = "IterationBuild") {
        println!("cargo:rerun-if-env-changed=PERIDOT_BUILTIN_ASSET_PATH");
    }
}
