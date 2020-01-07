extern crate clap;
extern crate bedrock;
extern crate regex;
extern crate peridot_vertex_processing_pack;
extern crate env_logger;
#[macro_use] extern crate log;

mod decombiner; use decombiner::*;
use peridot_vertex_processing_pack::*;
use std::io::{Read, Write};
use std::process::{Stdio, Command};
use std::path::{Path, PathBuf};
use std::borrow::Cow;

fn main()
{
    env_logger::init();

    let app = clap::App::new("peridot-shaderbuild")
        .version("0.1.0").author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .about("Combined Shader to Combined SPIR-V Builder for Peridot Engine via Google's shaderc")
        .arg(clap::Arg::with_name("input-file").help("Input File(s)").required(true).multiple(true));
    let matches = app.get_matches();
    for fp in matches.values_of("input-file").expect("no input file")
    {
        // ifile=ofileのペアで渡ってくるはず
        let mut fp_pair = fp.split("=");
        let ifile = fp_pair.next().expect("No input file");
        // ofileの指定がなければ拡張子を変更して使う
        let ofile = fp_pair.next().map_or_else(|| {
            let mut pb = PathBuf::from(ifile); pb.set_extension("pvp"); return Cow::Owned(pb);
        }, |s| Cow::Borrowed(Path::new(s)));
        process(ifile, &ofile);
    }
}

fn process<I: AsRef<Path>, O: AsRef<Path>>(infile_path: I, outfile_path: O)
{
    println!("Loading/Decomposing \"{}\"...", infile_path.as_ref().display());

    let content = std::fs::File::open(infile_path)
        .and_then(|mut fp| { let mut s = String::new(); fp.read_to_string(&mut s).map(|_| s) })
        .expect("reading source");
    let mut tok = Tokenizer::new(&content);
    let comsh = CombinedShader::from_parsed_blocks(tok.toplevel_blocks());
    let compile_vs = run_compiler_process("vertex", &comsh.emit_vertex_shader())
        .expect("Failed to spawn compiler process");
    let mut err = false;
    let fragment_shader = if comsh.is_provided_fsh()
    {
        let compile_fs = run_compiler_process("fragment", &comsh.emit_fragment_shader())
            .expect("Failed to spawn compiler process");
        let cfs_out = compile_fs.wait_with_output().expect("Failed to waiting compiler");
        if !cfs_out.status.success()
        {
            eprintln!("There are some errors while compiling fragment shader");
            err = true;
            None
        }
        else
        {
            let cout = std::str::from_utf8(&cfs_out.stdout).expect("in shaderc[f] output");
            trace!("Fragment shader output:\n{}", cout);
            // println!("cfs output: {:?}", cfs_out.stdout);
            Some(parse_num_output(cout))
        }
    }
    else { None };
    let cvs_out = compile_vs.wait_with_output().expect("Failed to waiting compiler");
    if !cvs_out.status.success()
    {
        eprintln!("There are some errors while compiling vertex shader.");
        err = true;
    }
    if err { return; }
    let cout = std::str::from_utf8(&cvs_out.stdout).expect("in shaderc[v] output");
    trace!("Vertex shader output:\n{}", cout);
    // let vsh_str = String::from_utf8(cvs_out.stdout).unwrap();
    // println!("cvs output: {:?}", vsh_str);
    let vertex_shader = parse_num_output(cout);
    // println!("vsh size: {}", vertex_shader.len());

    println!("Packaging compiled vertex processing stages to \"{}\"...", outfile_path.as_ref().display());
    let container = PvpContainer
    {
        vertex_bindings: comsh.emit_vertex_bindings(), vertex_attributes: comsh.emit_vertex_attributes(),
        vertex_shader, fragment_shader
    };
    // println!("!Container: {:?}", container);
    let mut fp_out = std::fs::File::create(outfile_path).expect("Failed to create output file");
    container.write(&mut fp_out).expect("Failed to write Peridot Vertex Processing file");
}
fn run_compiler_process(shader_stage: &str, stdin_bytes: &str) -> std::io::Result<std::process::Child>
{
    trace!("Compiling {}: Generated Code: \n{}", shader_stage, stdin_bytes);

    let mut compiler = Command::new("glslc").arg(&format!("-fshader-stage={}", shader_stage))
        .args(&["-o", "-", "-mfmt=num", "-"]).stdin(Stdio::piped())
        .stdout(Stdio::piped()).stderr(Stdio::inherit()).spawn()?;
    compiler.stdin.as_mut().expect("Failed to open stdin of compiler process")
        .write_all(stdin_bytes.as_bytes()).map(move |_| compiler)
}
fn parse_num_output(cout: &str) -> Vec<u8>
{
    let mut bytes = Vec::new();
    let elements = cout.split("\r\n").flat_map(|line| line.split(","));
    for nums in elements.filter(|s| !s.is_empty()).map(|s| s.trim_matches(&['\n', '\r', ' ', '\t'][..]))
    {
        let n = u32::from_str_radix(&nums[2..], 16).expect(&format!("invalid hexstr output: {:?}", &nums));
        bytes.extend_from_slice(&n.to_le_bytes());
    }
    return bytes;
}
