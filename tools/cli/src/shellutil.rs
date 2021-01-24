//! Shell Command Helper

use std::process::ExitStatus;
use std::path::Path;

pub fn handle_process_result(msg_prefix: &str, e: ExitStatus) {
    if e.success() { return; }

    if let Some(c) = e.code() {
        eprintln!(
            "{}: {} failed with code {:?}",
            console::style("ERROR").bold().fg(console::Color::Red),
            msg_prefix,
            c
        );
        std::process::exit(c);
    } else {
        panic!("Child process killed by signal");
    }
}

#[cfg(windows)]
pub fn sh_mirror(source: &Path, target: &Path, excludes: &[&str]) -> Result<ExitStatus, std::io::Error> {
    let mut args = vec![
        source.to_str().expect("invalid sequence in source path"),
        target.to_str().expect("invalid sequence in target path"),
        "/MIR"
    ];
    args.extend(excludes.iter().flat_map(|x| vec!["/XF", x]));

    let e = std::process::Command::new("robocopy")
        .args(&args)
        .spawn()?
        .wait()?;
    Ok(if e.code().map_or(false, |c| c < 8) {
        // 8未満は成功ステータスなので、適当なコマンドを発行して潰す
        std::process::Command::new("cmd").args(&["/c", "exit", "/b", "0"])
            .spawn()?
            .wait()?
    } else { e })
}
#[cfg(not(windows))]
pub fn sh_mirror(source: &Path, target: &Path, excludes: &[&str]) -> Result<ExitStatus, std::io::Error> {
    let source = source.join("*");
    let mut args = vec![
        "-auz",
        source.to_str().expect("invalid sequence in source path"),
        target.to_str().expect("invalid sequence in target path"),
    ];
    args.extend(excludes.iter().flat_map(|x| vec!["--exclude", x]));

    std::process::Command::new("rsync")
        .args(&args)
        .spawn()?
        .wait()
}

#[cfg(windows)]
pub fn sh_append_copy(source: &Path, target: &Path) -> Result<ExitStatus, std::io::Error> {
    let e = std::process::Command::new("robocopy")
        .args(&[
            source.to_str().expect("invalid sequence in source path"),
            target.to_str().expect("invalid sequence in target path"),
            "/IS", "/E"
        ])
        .spawn()?
        .wait()?;
    
    Ok(if e.code().map_or(false, |c| c < 8) {
        // 8未満は成功ステータスなので、適当なコマンドを発行して潰す
        std::process::Command::new("cmd").args(&["/c", "exit", "/b", "0"])
            .spawn()?
            .wait()?
    } else { e })
}
#[cfg(not(windows))]
pub fn sh_append_copy(source: &Path, target: &Path) -> Result<ExitStatus, std::io::Error> {
    std::process::Command::new("rsync")
        .args(&[
            "-a",
            source.join("*").to_str().expect("invalid sequence in source path"),
            target.to_str().expect("invalid sequence in target path")
        ])
        .spawn()?
        .wait()
}
