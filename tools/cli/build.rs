fn main() {
    if cfg!(target_os = "macos") {
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
