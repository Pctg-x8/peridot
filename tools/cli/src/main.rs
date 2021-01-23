
mod manifest;
mod platform;
mod subcommands;

use structopt::StructOpt;

/// Peridot Engine BuildProcess CLI
#[derive(StructOpt)]
#[structopt(name = "peridot")]
pub enum SubCommands {
    Build(subcommands::build::Args)
}

fn main() {
    match SubCommands::from_args() {
        SubCommands::Build(b) => subcommands::build::run(b)
    }
}
