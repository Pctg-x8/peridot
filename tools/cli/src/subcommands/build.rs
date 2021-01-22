
use std::path::PathBuf;
use structopt::StructOpt;

/// Builds an game
#[derive(StructOpt)]
pub struct Args {
    /// Path to userlib crate
    userlib_path: PathBuf
}

pub fn run(args: Args) {
    println!("[Building Game in {}]", args.userlib_path.display());
}
