use std::{ffi::CString, path::PathBuf};

use peridot_shader::{Environment, build};

fn main() {
    build(
        PathBuf::from(std::env::args_os().nth(1).expect("no input files")),
        &Environment {
            peridot_stdlib_search_path: CString::new(
                PathBuf::from(
                    std::env::var_os("PERIDOT_CLI_BUILTIN_ASSETS_PATH")
                        .expect("no builtin assets path set"),
                )
                .join("slangModules")
                .to_str()
                .expect("invalid path generated"),
            )
            .expect("nul in path"),
        },
    );
}
