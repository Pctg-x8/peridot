fn main() {
    // TODO: ここはそのうちなんとかしたい
    #[cfg(target_os = "macos")]
    println!(
        "cargo::rustc-link-arg=-Wl,-rpath,@executable_path/../../../thirdparty/ktx/source-repo/build"
    );
    #[cfg(target_os = "macos")]
    println!(
        "cargo::rustc-link-arg=-Wl,-rpath,@executable_path/../../../thirdparty/slang/source-repo/build/RelWithDebInfo/lib"
    );
}
