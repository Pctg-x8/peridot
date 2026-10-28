fn main() {
    if cfg!(target_os = "macos")
        && std::env::var_os("PERIDOT_BUILD_CLI_SKIP_DEBUG_RPATH").is_none_or(|x| x != "1")
    {
        // TODO: デバッグ用 正式にrpathどう設定するか......
        println!(
            "cargo::rustc-link-arg-bins=-Wl,-rpath,{}",
            std::env::current_dir()
                .expect("Failed to query current dir")
                .join("../../thirdparty/slang/source-repo/build/RelWithDebInfo/lib")
                .display()
        );
        println!(
            "cargo::rustc-link-arg-bins=-Wl,-rpath,{}",
            std::env::current_dir()
                .expect("Failed to query current dir")
                .join("../../thirdparty/ktx/cdeps-build")
                .join(std::env::var_os("TARGET").expect("no TARGET"))
                .display()
        );
        println!(
            "cargo::rustc-link-arg-bins=-Wl,-rpath,{}",
            std::path::PathBuf::from(std::env::var_os("VULKAN_SDK").expect("no VULKAN_SDK"))
                .join("lib")
                .display()
        );
    }
}
