
mod manifest;
mod steps;
mod platform;
mod subcommands;

use structopt::StructOpt;

/// Peridot Engine BuildProcess CLI
#[derive(StructOpt)]
#[structopt(name = "peridot")]
pub enum SubCommands {
    Build(subcommands::build::BuildArgs),
    /// Check game code
    Check(subcommands::build::Args),
    /// Test game code
    Test(subcommands::build::Args),
    GenManifest(subcommands::gen_manifest::Args)
}

fn main() {
    match SubCommands::from_args() {
        SubCommands::Build(b) => subcommands::build::run(b),
        SubCommands::Check(b) => subcommands::build::run_check(b),
        SubCommands::Test(b) => subcommands::build::run_test(b),
        SubCommands::GenManifest(b) => subcommands::gen_manifest::run(b)
    }
}
