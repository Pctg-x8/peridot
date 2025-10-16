use async_std::fs::File;
use clap::{App, Arg, ArgMatches};
use peridot_archive as par;
use std::fs::{metadata, read, read_dir};
use std::io::prelude::Write;
use std::io::{Read, Result as IOResult};

#[async_std::main]
async fn main() {
    let extract_matcher = App::new("extract")
        .version("0.1")
        .author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .arg(
            Arg::with_name("arc")
                .value_name("FILE")
                .required(true)
                .help("Archive file"),
        )
        .arg(
            Arg::with_name("apath")
                .value_name("ASSET_PATH")
                .help("An Asset Path (Optional)"),
        )
        .arg(
            Arg::with_name("check")
                .long("check-integrity")
                .help("Checks an archive integrity by checksum"),
        );
    let ls_matcher = App::new("list")
        .version("0.1")
        .author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .arg(
            Arg::with_name("arc")
                .value_name("FILE")
                .required(true)
                .help("Archive file"),
        )
        .arg(
            Arg::with_name("check")
                .long("check-integrity")
                .help("Checks an archive integrity by checksum"),
        );
    let create_matcher = App::new("new")
        .version("0.1")
        .author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .arg(
            Arg::with_name("ofile")
                .short("o")
                .long("output")
                .value_name("FILE")
                .help("Describes where archive file will be written"),
        )
        .arg(
            Arg::with_name("basedir")
                .short("b")
                .long("basedir")
                .value_name("DIR")
                .help("Base Directory(Common Prefix) for Name of each entries"),
        )
        .arg(
            Arg::with_name("ifiled")
                .help("Input File/Directory")
                .required(true)
                .multiple(true),
        )
        .arg(
            Arg::with_name("cmethod")
                .short("c")
                .long("compress")
                .value_name("METHOD")
                .possible_values(&["lz4", "zlib", "zstd11"])
                .takes_value(true)
                .help("Describes the compression method"),
        );
    let matcher = App::new("peridot-archive")
        .version("0.1")
        .author("S.Percentage <Syn.Tri.Naga@gmail.com>")
        .subcommands(vec![extract_matcher, create_matcher, ls_matcher]);
    let matches = matcher.get_matches();

    if let Some(matches) = matches.subcommand_matches("new") {
        new(matches).await;
    }
    if let Some(matches) = matches.subcommand_matches("list") {
        list(matches);
    }
    if let Some(matches) = matches.subcommand_matches("extract") {
        extract(matches);
    }
}

// TODO: アセット名生成部分が正しくないのであとで書き直す
async fn new(args: &ArgMatches<'_>) {
    let ifiled = args.values_of("ifiled").expect("noargs: ifiled");
    #[cfg(windows)]
    let directory_walker = ifiled.flat_map(|f| {
        if f.contains('*') {
            let glb = glob::glob(f).expect("glob match");
            Box::new(glb.flat_map(|f| extract_directory(&f.expect("filename decode err"))))
        } else {
            extract_directory(Path::new(f))
        }
    });
    #[cfg(not(windows))]
    let directory_walker = ifiled.flat_map(|f| extract_directory(Path::new(f)));

    let compression_method = args
        .value_of("cmethod")
        .map(|s| match s {
            "lz4" => par::CompressionMethod::Lz4(0),
            "zlib" => par::CompressionMethod::Zlib(0),
            "zstd11" => par::CompressionMethod::Zstd11(0),
            _ => unreachable!(),
        })
        .unwrap_or(par::CompressionMethod::None);
    let basedir = args.value_of("basedir").unwrap_or_default();
    println!("EntryName CommonPrefix={basedir}");
    let mut archive = par::ArchiveWrite::new(compression_method);
    for f in directory_walker {
        println!("Archiver input <<= {}", f.display());
        let fstr = f.to_str().expect("nullchar");
        let ename = fstr.strip_prefix(&basedir).unwrap_or(&fstr);
        if ename.is_empty() {
            eprintln!("Warn: empty entry name. wont be written");
        }
        let path = std::path::PathBuf::from(ename);
        let Some(stem) = path.file_stem() else {
            eprintln!("Warn: empty file stem, ignroing");
            continue;
        };
        let dir = path
            .parent()
            .map_or("", |x| x.to_str().expect("invalid utf-8 sequence"));
        let name = if dir.is_empty() {
            stem.to_str().expect("invalid utf-8 sequence").to_owned()
        } else {
            let mut x = String::with_capacity(dir.len() + 1 + stem.len());
            x.extend(dir.chars().map(|x| {
                if x == std::path::MAIN_SEPARATOR {
                    '.'
                } else {
                    x
                }
            }));
            x.push('.');
            x.push_str(stem.to_str().expect("invalid utf-8 sequence"));

            x
        };
        let ext = path
            .extension()
            .map_or("", |x| x.to_str().expect("invalid utf-8 sequence"));
        if !archive.add(
            name.to_owned(),
            ext.to_owned(),
            read(&f).expect("file io error"),
        ) {
            eprintln!("Warn: {:?}({name}.{ext}) has already been added", fstr);
        }
    }

    if let Some(ofpath) = args.value_of("ofile") {
        archive
            .write_async(&mut File::create(ofpath).await.expect("file open error"))
            .await
    } else {
        let foptr = unsafe { libc::fdopen(libc::dup(1), "wb\x00".as_ptr() as *const _) };
        archive.write(&mut NativeOfstream::from_stream_ptr(foptr).expect("fstream open error"))
    }
    .expect("fileio write error")
}

fn extract(args: &ArgMatches) {
    let archive = par::Archive::new(
        par::native_io::PlatformNativeFileReader::open(
            args.value_of("arc").expect("arc not found"),
        )
        .expect("Failed to open archive"),
        args.is_present("check"),
    )
    .expect("Failed to read archive");

    if let Some(apath) = args.value_of("apath") {
        let (name, ext) = match &apath.rsplitn(2, '.').collect::<Vec<_>>()[..] {
            &[name] => (name, ""),
            &[ext, name] => (name, ext),
            _ => unreachable!(),
        };
        let Some(h) = archive.find_entry(name, ext) else {
            panic!("Entry not found in archive: {apath}");
        };
        let mut content = Vec::new();
        archive
            .read_bin(h)
            .read_to_end(&mut content)
            .expect("Failed to read content");

        let foptr = unsafe { libc::fdopen(libc::dup(1), "wb\x00".as_ptr() as *const _) };
        NativeOfstream::from_stream_ptr(foptr)
            .expect("open stream")
            .write_all(&content[..])
            .expect("writing");
    }
}
fn list(args: &ArgMatches) {
    let archive = par::Archive::new(
        par::native_io::PlatformNativeFileReader::open(
            args.value_of("arc").expect("arc not found"),
        )
        .expect("Failed to open archive"),
        args.is_present("check"),
    )
    .expect("Failed to open archive");

    archive.list_entry(|n| println!("{} ({})", n.name, n.ext));
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
        unsafe {
            libc::fclose(self.0.as_ptr());
        }
    }
}
impl Write for NativeOfstream {
    fn write(&mut self, buf: &[u8]) -> IOResult<usize> {
        let written =
            unsafe { libc::fwrite(buf.as_ptr() as *const _, 1, buf.len() as _, self.0.as_ptr()) };
        return Ok(written);
    }
    fn flush(&mut self) -> IOResult<()> {
        let code = unsafe { libc::fflush(self.0.as_ptr()) };
        if code == 0 {
            Ok(())
        } else {
            Err(std::io::Error::last_os_error())
        }
    }
}

use std::borrow::ToOwned;
use std::path::{Path, PathBuf};
fn extract_directory(p: &Path) -> Box<dyn Iterator<Item = PathBuf>> {
    if metadata(p).expect("metadata fetch failed").is_dir() {
        let walk = read_dir(p)
            .expect("reading dir error")
            .flat_map(|f| extract_directory(f.expect("nopath").path().as_path()));

        Box::new(walk)
    } else {
        Box::new(Some(p.to_owned()).into_iter())
    }
}
