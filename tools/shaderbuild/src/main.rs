mod decombiner;
use clap::Parser;
use decombiner::*;
use peridot_vertex_processing_pack::*;
use std::borrow::Cow;
use std::fmt::Debug;
use std::io::{Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Parser)]
struct Args {
    /// Input File(s)
    #[arg(
        required = true,
        long_help = "Input File(s). format: `ifile=ofile`, but ofile can be omitted(used file stem of ifile for output)"
    )]
    pub input_file: Vec<String>,
}

#[derive(Debug, thiserror::Error)]
enum AppError {
    #[error("No input file")]
    NoInputFile,
    #[error(transparent)]
    ProcessError(#[from] ProcessError),
}

fn main() -> Result<(), AppError> {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer().pretty())
        .with(tracing_subscriber::EnvFilter::from_default_env())
        .init();

    let args = Args::parse();
    for fp in args.input_file.into_iter() {
        // ifile=ofileのペアで渡ってくるはず
        let mut fp_pair = fp.split("=");
        let ifile = fp_pair.next().ok_or(AppError::NoInputFile)?;
        // ofileの指定がなければ拡張子を変更して使う
        let ofile = match fp_pair.next() {
            Some(x) => Cow::Borrowed(Path::new(x)),
            None => Cow::Owned(PathBuf::from(ifile).with_extension("pvp")),
        };

        process(ifile, &ofile)?;
    }

    Ok(())
}

#[derive(Debug, thiserror::Error)]
enum ProcessError {
    #[error("reading source failed: {0}")]
    ReadingSourceFailed(#[source] std::io::Error),
    #[error("file output error: {0}")]
    FileOutputError(#[source] std::io::Error),
    #[error("glsl compilation failed")]
    GLSLCompilationFailed,
}

#[tracing::instrument]
fn process(
    infile_path: &(impl AsRef<Path> + Debug + ?Sized),
    outfile_path: &(impl AsRef<Path> + Debug + ?Sized),
) -> Result<(), ProcessError> {
    println!(
        "Loading/Decomposing \"{}\"...",
        infile_path.as_ref().display()
    );

    let content = std::fs::File::open(infile_path)
        .and_then(|mut fp| {
            let mut s = String::new();
            fp.read_to_string(&mut s).map(move |_| s)
        })
        .map_err(ProcessError::ReadingSourceFailed)?;
    let mut tok = Tokenizer::new(&content);
    let comsh = CombinedShader::from_parsed_blocks(tok.toplevel_blocks());

    let mut err = false;
    let compile_vs = run_compiler_process("vertex", &comsh.emit_vertex_shader())
        .expect("Failed to spawn compiler process");
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
        return Err(ProcessError::GLSLCompilationFailed);
    }

    let cout = std::str::from_utf8(&cvs_out.stdout).expect("in shaderc[v] output");
    tracing::trace!("Vertex shader output:\n{cout}");
    let vertex_shader = parse_num_output(cout);

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
    container
        .write(&mut std::fs::File::create(outfile_path).map_err(ProcessError::FileOutputError)?)
        .map_err(ProcessError::FileOutputError)?;

    Ok(())
}

#[tracing::instrument]
fn run_compiler_process(
    shader_stage: &str,
    stdin_bytes: &str,
) -> std::io::Result<std::process::Child> {
    let mut compiler = Command::new("glslc")
        .arg(&format!("-fshader-stage={shader_stage}"))
        .args(&["-o", "-", "-mfmt=num", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()?;
    compiler
        .stdin
        .as_mut()
        .expect("Failed to open stdin of compiler process")
        .write_all(stdin_bytes.as_bytes())?;

    Ok(compiler)
}

fn parse_num_output(cout: &str) -> Vec<u8> {
    let mut bytes = Vec::new();
    let elements = cout.split("\r\n").flat_map(|line| line.split(","));
    for nums in elements
        .filter(|s| !s.is_empty())
        .map(|s| s.trim_matches(['\n', '\r', ' ', '\t']))
    {
        // assumes that nums is 0x-prefixed 32bit hexstring
        assert_eq!(&nums[..2], "0x");

        let Ok(n) = u32::from_str_radix(&nums[2..], 16) else {
            panic!("invalid hexstr output: {nums:?}");
        };

        bytes.extend_from_slice(&n.to_le_bytes());
    }
    return bytes;
}
