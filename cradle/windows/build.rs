fn main() {
    println!("cargo:rustc-link-search=static={}/Lib", env!("VK_SDK_PATH"));
}