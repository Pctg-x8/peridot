use std::{io::Write, path::PathBuf};

use clap::Parser;

#[derive(Parser)]
pub struct Args {
    source_path: PathBuf,
    #[arg(long, short = 'f')]
    force_rebuild: bool,
    #[arg(long, short = 'o')]
    out_dir: Option<PathBuf>,
}

fn main() {
    let args = Args::parse();
    println!("process: {:?}", args.source_path);

    let ext = args.source_path.extension();
    if ext.is_some_and(|x| x == "prc") {
        // rendering configuration: just compile
        let dest_path = args
            .out_dir
            .as_deref()
            .unwrap_or_else(|| args.source_path.parent().expect("no parent?"))
            .join(
                args.source_path
                    .file_name()
                    .expect("no file name in source path"),
            )
            .with_extension("pa1-rendering-configuration");
        if !args.force_rebuild
            && let (Ok(x), Ok(y)) = (args.source_path.metadata(), dest_path.metadata())
            && x.modified().unwrap() <= y.modified().unwrap()
        {
            println!("skip asset: {:?} (modified time)", args.source_path);
            return;
        }

        std::process::Command::new(
            std::env::current_exe()
                .expect("current_exe")
                .parent()
                .expect("dirname")
                .join("peridot-rendering-configuration-compiler"),
        )
        .arg(&args.source_path)
        .arg("--output")
        .arg(&dest_path)
        .spawn()
        .expect("Failed to spawn rendering-configuration-compiler")
        .wait()
        .expect("rendering-configuration-compiler");
    } else if ext.is_some_and(|x| x == "png") {
        // image asset: decompress to rgba and recompress(if needed, specified by metadata file)
        let dest_path = args
            .out_dir
            .as_deref()
            .unwrap_or_else(|| args.source_path.parent().expect("no parent?"))
            .join(
                args.source_path
                    .file_name()
                    .expect("no file name in source path"),
            )
            .with_extension("pa1-texture2d");
        if !args.force_rebuild
            && let (Ok(x), Ok(y)) = (args.source_path.metadata(), dest_path.metadata())
            && x.modified().unwrap() <= y.modified().unwrap()
        {
            println!("skip asset: {:?} (modified time)", args.source_path);
            return;
        }

        let img = image::open(&args.source_path)
            .expect("Failed to open asset")
            .to_rgba8();
        let w = img.width();
        let h = img.height();

        let mut f = std::fs::File::options()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&dest_path)
            .expect("Failed to open dest");
        f.write_all(&0x12345678u32.to_ne_bytes())
            .expect("Failed to write bom");
        f.write_all(&w.to_ne_bytes())
            .expect("Failed to write width");
        f.write_all(&h.to_ne_bytes())
            .expect("Failed to write height");
        f.write_all(img.as_raw()).expect("Failed to write content");
    } else {
        panic!("unknown asset: {ext:?}");
    }
}
