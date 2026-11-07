use std::path::PathBuf;

use clap::Parser;
use peridot_rendering_configuration as prc;

#[derive(Parser)]
pub struct App {
    input: PathBuf,
    #[arg(long, short = 'o')]
    output: Option<PathBuf>,
}

fn main() {
    tracing_subscriber::fmt()
        .pretty()
        .with_env_filter(tracing_subscriber::filter::EnvFilter::from_default_env())
        .init();
    let args = App::parse();

    let content = std::fs::read_to_string(&args.input).expect("failed to read input");
    let asset = prc::compilation::compile(&content).expect("Error in generating asset");

    let opath = args
        .output
        .unwrap_or_else(|| args.input.with_extension("prcc"));
    let mut o = std::fs::File::options()
        .write(true)
        .truncate(true)
        .create(true)
        .open(opath)
        .expect("Failed to open write file");
    let writes = prc::write(&mut o, asset).expect("failed to write asset");
    tracing::info!(bytes = writes, "asset write");
}
