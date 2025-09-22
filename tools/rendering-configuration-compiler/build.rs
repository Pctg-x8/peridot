fn main() {
    if cfg!(unix) {
        // add local lib path for lld
        println!("cargo::rustc-link-search=/usr/local/lib");
    }
}
