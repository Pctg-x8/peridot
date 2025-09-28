mod manifest;
mod path;
mod platform;
mod project;
mod shellutil;
mod steps;
mod subcommands;

use clap::Parser;

/// Peridot Engine BuildProcess CLI
#[derive(Parser)]
#[clap(name = "peridot")]
pub enum SubCommands {
    Build(subcommands::build::BuildArgs),
    /// Check game code
    Check(subcommands::build::Args),
    /// Test game code
    Test(subcommands::build::Args),
    GenManifest(subcommands::gen_manifest::Args),
}

fn main() {
    match SubCommands::parse() {
        SubCommands::Build(b) => subcommands::build::run(b),
        SubCommands::Check(b) => subcommands::build::run_check(b),
        SubCommands::Test(b) => subcommands::build::run_test(b),
        SubCommands::GenManifest(b) => subcommands::gen_manifest::run(b),
    }
}
