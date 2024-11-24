mod decombiner;
use clap::Parser;
use decombiner::*;
use peridot_vertex_processing_pack::*;
use std::borrow::Cow;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

/// Combined Shader Script to Combined SPIR-V Builder for Peridot Engine
#[derive(Parser)]
struct Args {
    /// Input File(s)
    #[arg(
        required = true,
        long_help = "Input File(s). format: `ifile=ofile`, but ofile can be omitted(used file stem of ifile for output)"
    )]
    pub input_file: Vec<String>,
}

fn main() {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().pretty())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let args = Args::parse();
    for fp in args.input_file.into_iter() {
        // ifile=ofileのペアで渡ってくるはず
        let mut fp_pair = fp.split("=");
        let Some(ifile) = fp_pair.next() else {
            tracing::error!("No input file");
            std::process::exit(1);
        };
        // ofileの指定がなければ拡張子を変更して使う
        let ofile = match fp_pair.next() {
            Some(x) => Cow::Borrowed(Path::new(x)),
            None => Cow::Owned(PathBuf::from(ifile).with_extension("pvp")),
        };

        process(ifile, &ofile);
    }
}

fn process<I: AsRef<Path>, O: AsRef<Path>>(infile_path: I, outfile_path: O) {
    println!(
        "Loading/Decomposing \"{}\"...",
        infile_path.as_ref().display()
    );

    let content = std::fs::File::open(infile_path)
        .and_then(|mut fp| {
            let mut s = String::new();
            fp.read_to_string(&mut s).map(|_| s)
        })
        .expect("reading source");
    let mut tok = Tokenizer::new(&content);
    let comsh = CombinedShader::from_parsed_blocks(tok.toplevel_blocks());
    let compile_vs = run_compiler_process("vertex", &comsh.emit_vertex_shader())
        .expect("Failed to spawn compiler process");
    let mut err = false;
    let fragment_shader = if comsh.is_provided_fsh() {
        let compile_fs = run_compiler_process("fragment", &comsh.emit_fragment_shader())
            .expect("Failed to spawn compiler process");
        let cfs_out = compile_fs
            .wait_with_output()
            .expect("Failed to waiting compiler");
        if !cfs_out.status.success() {
            eprintln!("There are some errors while compiling fragment shader");
            err = true;
            None
        } else {
            let cout = std::str::from_utf8(&cfs_out.stdout).expect("in shaderc[f] output");
            tracing::trace!("Fragment shader output:\n{cout}");
            Some(parse_num_output(cout))
        }
    } else {
        None
    };
    let cvs_out = compile_vs
        .wait_with_output()
        .expect("Failed to waiting compiler");
    if !cvs_out.status.success() {
        eprintln!("There are some errors while compiling vertex shader.");
        err = true;
    }
    if err {
        return;
    }
    let cout = std::str::from_utf8(&cvs_out.stdout).expect("in shaderc[v] output");
    tracing::trace!("Vertex shader output:\n{cout}");
    // let vsh_str = String::from_utf8(cvs_out.stdout).unwrap();
    // println!("cvs output: {:?}", vsh_str);
    let vertex_shader = parse_num_output(cout);
    // println!("vsh size: {}", vertex_shader.len());

    println!(
        "Packaging compiled vertex processing stages to \"{}\"...",
        outfile_path.as_ref().display()
    );
    let container = PvpContainer {
        vertex_bindings: comsh.emit_vertex_bindings(),
        vertex_attributes: comsh.emit_vertex_attributes(),
        vertex_shader,
        fragment_shader,
    };
    // println!("!Container: {:?}", container);
    let mut fp_out = std::fs::File::create(outfile_path).expect("Failed to create output file");
    container
        .write(&mut fp_out)
        .expect("Failed to write Peridot Vertex Processing file");
}
fn run_compiler_process(
    shader_stage: &str,
    stdin_bytes: &str,
) -> std::io::Result<std::process::Child> {
    tracing::trace!(
        "Compiling {}: Generated Code: \n{}",
        shader_stage,
        stdin_bytes
    );

    let mut compiler = Command::new("glslc")
        .arg(&format!("-fshader-stage={}", shader_stage))
        .args(&["-o", "-", "-mfmt=num", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()?;
    compiler
        .stdin
        .as_mut()
        .expect("Failed to open stdin of compiler process")
        .write_all(stdin_bytes.as_bytes())
        .map(move |_| compiler)
}
fn parse_num_output(cout: &str) -> Vec<u8> {
    let mut bytes = Vec::new();
    let elements = cout.split("\r\n").flat_map(|line| line.split(","));
    for nums in elements
        .filter(|s| !s.is_empty())
        .map(|s| s.trim_matches(&['\n', '\r', ' ', '\t'][..]))
    {
        let n = u32::from_str_radix(&nums[2..], 16)
            .expect(&format!("invalid hexstr output: {:?}", &nums));
        bytes.extend_from_slice(&n.to_le_bytes());
    }
    return bytes;
}
