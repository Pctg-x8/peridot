use std::path::PathBuf;

use clap::Parser;
use peridot_rendering_configuration as prc;

#[derive(Parser)]
pub enum App {
    ListPass(AppListPass),
    ExtractSpv(AppExtractSpv),
}

#[derive(Parser)]
pub struct AppListPass {
    input: PathBuf,
}

#[derive(Parser)]
pub struct AppExtractSpv {
    input: PathBuf,
    pass_name: String,
    output: PathBuf,
}

fn main() {
    match App::parse() {
        App::ListPass(p) => list_pass(p),
        App::ExtractSpv(p) => extract_spv(p),
    }
}

fn list_pass(args: AppListPass) {
    let prc = prc::read(&mut std::io::BufReader::new(
        std::fs::File::open(&args.input).expect("failed to open input"),
    ))
    .expect("failed to read asset");

    for x in prc.passes.keys() {
        println!("{x}");
    }
}

fn extract_spv(args: AppExtractSpv) {
    let prc = prc::read(&mut std::io::BufReader::new(
        std::fs::File::open(&args.input).expect("failed to open input"),
    ))
    .expect("failed to read asset");

    let Some(p) = prc.passes.get(&args.pass_name) else {
        panic!("could not find pass");
    };

    match p {
        prc::ShadingPassVk::SimpleDeriveBuiltinPass { name } => {
            panic!("cannot extract spv from SimpleDeriveBuiltinPass: {name}");
        }
        prc::ShadingPassVk::Custom { code, .. } => {
            std::fs::write(&args.output, unsafe {
                core::slice::from_raw_parts(code.as_ptr() as *const u8, code.len() << 2)
            })
            .expect("failed to write file");
        }
    }
}
