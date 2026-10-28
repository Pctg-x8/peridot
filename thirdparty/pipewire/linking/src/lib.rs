pub fn emit_link_args() {
    let ld_args = String::from_utf8(
        std::process::Command::new("pkg-config")
            .args(["--libs", "libpipewire-0.3"])
            .stdout(std::process::Stdio::piped())
            .spawn()
            .unwrap()
            .wait_with_output()
            .unwrap()
            .stdout,
    )
    .unwrap();

    for a in ld_args.split(' ') {
        println!("cargo::rustc-link-arg={a}");
    }
}
