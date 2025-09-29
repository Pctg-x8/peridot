use std::path::PathBuf;

use clap::Parser;

#[derive(Parser)]
pub struct Args {
    source_path: PathBuf,
    #[arg(long, short = 'f')]
    force_rebuild: bool,
}

fn main() {
    let args = Args::parse();
    println!("process: {:?}", args.source_path);

    let ext = args.source_path.extension();
    if ext.is_some_and(|x| x == "prc") {
        // rendering configuration: just compile
        let dest_path = args.source_path.with_extension("prcc");
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
        .arg("-o")
        .arg(&dest_path)
        .spawn()
        .expect("Failed to spawn rendering-configuration-compiler")
        .wait()
        .expect("rendering-configuration-compiler");
    } else {
        panic!("unknown asset: {ext:?}");
    }
}
