
extern crate peridot_serialization_utils;
extern crate clap; extern crate glob; mod par; extern crate libc;
extern crate crc; extern crate lz4; extern crate libflate; extern crate zstd;
use clap::{App, Arg, ArgMatches};
use std::fs::{metadata, read_dir, read, File};
use std::io::prelude::Write;
use std::io::Result as IOResult;

fn extract(args: &ArgMatches) {
    let mut archive = par::ArchiveRead::from_file(args.value_of("arc").expect("arc not found"),
        args.is_present("check")).expect("reading archive error");

    if let Some(apath) = args.value_of("apath") {
        if let Some(b) = archive.read_bin(apath).expect("readbin") {
            let foptr = unsafe { libc::fdopen(libc::dup(1), "wb\x00".as_ptr() as *const _) };
            NativeOfstream::from_stream_ptr(foptr).expect("open stream").write_all(&b[..]).expect("writing");
        }
        else {
            panic!("Entry not found in archive: {}", apath);
        }
    }
}
fn list(args: &ArgMatches) {
    let archive = par::ArchiveRead::from_file(args.value_of("arc").expect("arc not found"),
        args.is_present("check")).expect("check not found");

    for n in archive.entry_names() {
        println!("{}", n);
    }
}
fn main() {
    let extract_matcher = App::new("extract").version("0.1").author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .arg(Arg::with_name("arc").value_name("FILE").required(true).help("Archive file"))
        .arg(Arg::with_name("apath").value_name("ASSET_PATH").help("An Asset Path (Optional)"))
        .arg(Arg::with_name("check").long("check-integrity").help("Checks an archive integrity by checksum"));
    let ls_matcher = App::new("list").version("0.1").author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .arg(Arg::with_name("arc").value_name("FILE").required(true).help("Archive file"))
        .arg(Arg::with_name("check").long("check-integrity").help("Checks an archive integrity by checksum"));
    let create_matcher = App::new("new").version("0.1").author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .arg(Arg::with_name("ofile").short("o").long("output").value_name("FILE")
            .help("Describes where archive file will be written"))
        .arg(Arg::with_name("ifiled").help("Input File/Directory").required(true).multiple(true))
        .arg(Arg::with_name("cmethod").short("c").long("compress").value_name("METHOD")
            .possible_values(&["lz4", "zlib", "zstd11"]).takes_value(true).help("Describes the compression method"));
    let matcher = App::new("peridot-archive").version("0.1").author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .subcommands(vec![extract_matcher, create_matcher, ls_matcher]);
    let matches = matcher.get_matches();

    if let Some(matches) = matches.subcommand_matches("new") { new(matches); }
    if let Some(matches) = matches.subcommand_matches("list") { list(matches); }
    if let Some(matches) = matches.subcommand_matches("extract") { extract(matches); }
}

fn new(args: &ArgMatches) {
    let directory_walker = args.values_of("ifiled").expect("noargs: ifiled")
        .flat_map(|f| if cfg!(windows) && f.contains('*') {
            Box::new(glob::glob(f).expect("glob match")
                .flat_map(|f| extract_directory(&f.expect("filename decode err"))))
        }
        else { extract_directory(Path::new(f)) });
    let compression_method = args.value_of("cmethod").map(|s| match s {
        "lz4" => par::CompressionMethod::Lz4(0),
        "zlib" => par::CompressionMethod::Zlib(0),
        "zstd11" => par::CompressionMethod::Zstd11(0),
        _ => unreachable!()
    }).unwrap_or(par::CompressionMethod::None);
    let mut archive = par::ArchiveWrite::new(compression_method);
    for f in directory_walker {
        // println!("input <<= {}", f.display());
        let fstr = f.to_str().expect("nullchar");
        if !archive.add(fstr.to_owned(), read(&f).expect("file io error")) {
            eprintln!("Warn: {:?} has already been added", fstr);
        }
    }
    if let Some(ofpath) = args.value_of("ofile") {
        archive.write(&mut File::create(ofpath).expect("file open error"))
    }
    else {
        let foptr = unsafe { libc::fdopen(libc::dup(1), "wb\x00".as_ptr() as *const _) };
        archive.write(&mut NativeOfstream::from_stream_ptr(foptr).expect("fstream open error"))
    }.expect("fileio write error")
}

use std::ptr::NonNull;
struct NativeOfstream(NonNull<libc::FILE>);
impl NativeOfstream {
    pub fn from_stream_ptr(p: *mut libc::FILE) -> Option<Self> {
        NonNull::new(p).map(NativeOfstream)
    }
}
impl Drop for NativeOfstream {
    fn drop(&mut self) {
        unsafe { libc::fclose(self.0.as_ptr()); }
    }
}
impl Write for NativeOfstream {
    fn write(&mut self, buf: &[u8]) -> IOResult<usize> {
        let written = unsafe { libc::fwrite(buf.as_ptr() as *const _, 1, buf.len() as _, self.0.as_ptr()) };
        return Ok(written);
    }
    fn flush(&mut self) -> IOResult<()> {
        let code = unsafe { libc::fflush(self.0.as_ptr()) };
        if code == 0 { Ok(()) } else { Err(std::io::Error::last_os_error()) }
    }
}

use std::path::{Path, PathBuf}; use std::borrow::ToOwned;
fn extract_directory(p: &Path) -> Box<Iterator<Item = PathBuf>> {
    if metadata(p).expect("metadata fetch failed").is_dir() {
        Box::new(read_dir(p).expect("reading dir error")
            .flat_map(|f| extract_directory(f.expect("nopath").path().as_path())))
    }
    else {
        Box::new(Some(p.to_owned()).into_iter())
    }
}
