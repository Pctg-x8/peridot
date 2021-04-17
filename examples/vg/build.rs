fn main() {
    if cfg!(feature = "use-freetype") {
        let extlib_path = std::env::current_dir().expect("Failed to get current directory")
            .join("extlib/android/arm64-v8a");
        println!("cargo:rustc-link-search={}", extlib_path.display());
    }
}
