fn main() {
    let mut res = winres::WindowsResource::new();
    res.set_manifest_file("./marble-editor-win.exe.manifest");
    res.compile().expect("Failed to compile exe resource");
}
