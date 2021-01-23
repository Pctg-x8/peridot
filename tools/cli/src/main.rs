
mod manifest;
mod steps;
mod platform;
mod subcommands;

use structopt::StructOpt;

/// Peridot Engine BuildProcess CLI
#[derive(StructOpt)]
#[structopt(name = "peridot")]
pub enum SubCommands {
    Build(subcommands::build::Args),
    Check(subcommands::check::Args),
    Test(subcommands::test::Args),
    GenManifest(subcommands::gen_manifest::Args)
}

fn main() {
    match SubCommands::from_args() {
        SubCommands::Build(b) => subcommands::build::run(b),
        SubCommands::Check(b) => subcommands::check::run(b),
        SubCommands::Test(b) => subcommands::test::run(b),
        SubCommands::GenManifest(b) => subcommands::gen_manifest::run(b)
    }
}
