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
                .join("../../thirdparty/ktx/source-repo/build")
                .display()
        );
    }
}
