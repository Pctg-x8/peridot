fn main() {
    if cfg!(feature = "use-freetype") {
        println!("cargo:rustc-link-search={}", std::env::current_dir().expect("Failed to get current directory").join("extlib/android/arm64-v8a").display());
    }
}
