mod decombiner;
use clap::Parser;
use decombiner::*;
use peridot_vertex_processing_pack::*;
use smol::io::AsyncWriteExt;
use std::borrow::Cow;
use std::fmt::Debug;
use std::path::{Path, PathBuf};
use std::process::Stdio;
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
    smol::block_on(async move {
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

            process(ifile, &ofile).await?;
        }

        Ok(())
    })
}

#[derive(Debug, thiserror::Error)]
enum ProcessError {
    #[error("reading source failed: {0}")]
    ReadingSourceFailed(#[source] std::io::Error),
    #[error("file output error: {0}")]
    FileOutputError(#[source] std::io::Error),
    #[error("glsl compilation failed")]
    GLSLCompilationFailed,
    #[error("compiler process i/o error: {0}")]
    CompilerProcessIOError(#[source] std::io::Error),
}

#[tracing::instrument]
async fn process(
    infile_path: &(impl AsRef<Path> + Debug + ?Sized),
    outfile_path: &(impl AsRef<Path> + Debug + ?Sized),
) -> Result<(), ProcessError> {
    println!(
        "Loading/Decomposing \"{}\"...",
        infile_path.as_ref().display()
    );

    let content =
        std::fs::read_to_string(infile_path).map_err(ProcessError::ReadingSourceFailed)?;
    let mut tok = Tokenizer::new(&content);
    let comsh = CombinedShader::from_parsed_blocks(tok.toplevel_blocks());

    let c1 = compile_glsl("vertex", comsh.emit_vertex_shader()).await.map_err(ProcessError::CompilerProcessIOError)?;
    let c2 = if comsh.is_provided_fsh() {
        compile_glsl("fragment", comsh.emit_fragment_shader()).await.map_err(ProcessError::CompilerProcessIOError)?
    } else {
        CompilationResult::Successful(Vec::new())
    };

    // let compilation_results =
    //     futures_util::try_join!(compile_glsl("vertex", comsh.emit_vertex_shader()), async {
    //         if comsh.is_provided_fsh() {
    //             compile_glsl("fragment", comsh.emit_fragment_shader()).await
    //         } else {
    //             Ok(CompilationResult::Successful(Vec::new()))
    //         }
    //     })
    //     .map_err(ProcessError::CompilerProcessIOError)?;
    let compilation_results = (c1, c2);
    let (vertex_shader, fragment_shader) = match compilation_results {
        (CompilationResult::Successful(vs), CompilationResult::Successful(fs)) => (vs, fs),
        (CompilationResult::Failed, CompilationResult::Successful(_)) => {
            eprintln!("There are some errors while compiling vertex shader.");

            return Err(ProcessError::GLSLCompilationFailed);
        }
        (CompilationResult::Successful(_), CompilationResult::Failed) => {
            eprintln!("There are some errors while compiling fragment shader.");

            return Err(ProcessError::GLSLCompilationFailed);
        }
        (CompilationResult::Failed, CompilationResult::Failed) => {
            eprintln!("There are some errors while compiling both vertex and fragment shader.");

            return Err(ProcessError::GLSLCompilationFailed);
        }
    };

    println!(
        "Packaging compiled vertex processing stages to \"{}\"...",
        outfile_path.as_ref().display()
    );
    let container = PvpContainer {
        vertex_bindings: comsh.emit_vertex_bindings(),
        vertex_attributes: comsh.emit_vertex_attributes(),
        vertex_shader,
        fragment_shader: comsh.is_provided_fsh().then_some(fragment_shader),
    };
    container
        .write(&mut std::fs::File::create(outfile_path).map_err(ProcessError::FileOutputError)?)
        .map_err(ProcessError::FileOutputError)?;

    Ok(())
}

enum CompilationResult {
    Successful(Vec<u32>),
    Failed,
}

#[tracing::instrument]
async fn compile_glsl(
    shader_stage: &str,
    stdin_bytes: String,
) -> std::io::Result<CompilationResult> {
    let shader_stage_option = format!("-fshader-stage={shader_stage}");
    let mut p = async_process::Command::new("glslc")
        .args([&shader_stage_option, "-o", "-", "-mfmt=num", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()?;
    let stdin = p.stdin.as_mut().expect("No stdin for compiler process?");
    stdin.write_all(stdin_bytes.as_bytes()).await?;
    stdin.flush().await?;
    let o = p.output().await?;

    if !o.status.success() {
        // このタイミングでログ出す（と食わせたglslがstdin_bytesとしてtracingで出せる）
        tracing::error!("glsl compilation failed");
        return Ok(CompilationResult::Failed);
    }

    let output_str =
        std::str::from_utf8(&o.stdout).expect("invalid utf-8 sequence in glslc output");
    tracing::trace!("glslc[{shader_stage}] output:\n{output_str}");
    Ok(CompilationResult::Successful(parse_num_output(output_str)))
}

fn parse_num_output(cout: &str) -> Vec<u32> {
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

        bytes.push(n);
    }

    bytes
}
