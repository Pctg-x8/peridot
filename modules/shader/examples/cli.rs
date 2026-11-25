use std::{ffi::CString, path::PathBuf};

use peridot_shader::{Environment, build};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};

fn main() {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().pretty())
        .with(tracing_subscriber::filter::EnvFilter::from_default_env())
        .init();

    let asset = build(
        PathBuf::from(std::env::args_os().nth(1).expect("no input files")),
        &Environment {
            peridot_stdlib_search_path: CString::new(
                std::env::var("PERIDOT_SHADER_STDLIB_PATH")
                    .expect("invalid PERIDOT_SHADER_STDLIB_PATH"),
            )
            .expect("nul in path"),
        },
    )
    .expect("build failed");

    println!("{:?}", asset.vertex_inputs);
    println!("{:?}", asset.material_properties);
    println!("{:?}", asset.input_variable_layout);

    asset
        .write(
            &mut std::fs::File::options()
                .create(true)
                .truncate(true)
                .write(true)
                .open("generated")
                .expect("file.open"),
        )
        .expect("file.write");
}
