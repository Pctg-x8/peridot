use async_std::fs::File;
use clap::{Parser, ValueEnum};
use peridot_archive as par;
use std::fs::{metadata, read, read_dir};
use std::io::prelude::Write;
use std::io::{Read, Result as IOResult};

#[derive(Parser)]
#[command(name = "extract")]
pub struct CmdExtract {
    /// Archive file
    #[arg(value_name("FILE"))]
    pub arc: PathBuf,
    /// An Asset Path (Optional)
    #[arg(value_name("ASSET_PATH"))]
    pub apath: String,
    /// Checks an archive integrity by checksum
    #[arg(long = "check-integrity", action = clap::ArgAction::SetTrue, default_value = "false")]
    pub check: bool,
}

#[derive(Parser)]
#[command(name = "list")]
pub struct CmdList {
    /// Archive file
    #[arg(value_name("FILE"))]
    pub arc: PathBuf,
    /// Checks an archive integrity by checksum
    #[arg(long = "check-integrity", action = clap::ArgAction::SetTrue, default_value = "false")]
    pub check: bool,
}

#[derive(Parser)]
#[command(name = "new")]
pub struct CmdNew {
    /// Describes where archive file will be written
    #[arg(short = 'o', long = "output", value_name("FILE"))]
    pub ofile: Option<PathBuf>,
    /// Base Directory(Common Prefix) for Name of each entries
    #[arg(short = 'b', long = "basedir", value_name("DIR"))]
    pub basedir: Option<PathBuf>,
    /// Input File/Directory
    #[arg(required = true)]
    pub ifiled: Vec<PathBuf>,
    /// Describes the compression method
    #[arg(short = 'c', long = "compress", value_name("METHOD"), value_enum)]
    pub cmethod: Option<CompressionMethod>,
}

#[derive(ValueEnum, Clone, Copy)]
pub enum CompressionMethod {
    #[value(name = "lz4")]
    LZ4,
    #[value(name = "zlib")]
    Zlib,
    #[value(name = "zstd11")]
    Zstd11,
}

#[derive(Parser)]
pub enum Args {
    Extract(CmdExtract),
    List(CmdList),
    New(CmdNew),
}

#[async_std::main]
async fn main() {
    let args = Args::parse();

    match args {
        Args::Extract(x) => extract(x),
        Args::List(x) => list(x),
        Args::New(x) => new(x).await,
    }
}

// TODO: アセット名生成部分が正しくないのであとで書き直す
async fn new(args: CmdNew) {
    #[cfg(windows)]
    let directory_walker = args.ifiled.iter().flat_map(|f| {
        if f.contains('*') {
            let glb = glob::glob(f).expect("glob match");
            Box::new(glb.flat_map(|f| extract_directory(&f.expect("filename decode err"))))
        } else {
            extract_directory(f)
        }
    });
    #[cfg(not(windows))]
    let directory_walker = args.ifiled.iter().flat_map(|f| extract_directory(f));

    let compression_method = args
        .cmethod
        .map_or(par::CompressionMethod::None, |s| match s {
            CompressionMethod::LZ4 => par::CompressionMethod::Lz4(0),
            CompressionMethod::Zlib => par::CompressionMethod::Zlib(0),
            CompressionMethod::Zstd11 => par::CompressionMethod::Zstd11(0),
        });
    let basedir = args.basedir.unwrap_or_default();
    println!("EntryName CommonPrefix={}", basedir.display());
    let mut archive = par::ArchiveWrite::new(compression_method);
    for f in directory_walker {
        println!("Archiver input <<= {}", f.display());
        let ename = f.strip_prefix(&basedir).unwrap_or(&f);
        if ename.as_os_str().is_empty() {
            eprintln!("Warn: empty entry name. wont be written");
        }
        let Some(stem) = ename.file_stem() else {
            eprintln!("Warn: empty file stem, ignroing");
            continue;
        };
        let dir = ename
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
        let ext = ename
            .extension()
            .map_or("", |x| x.to_str().expect("invalid utf-8 sequence"));
        if !archive.add(
            name.to_owned(),
            ext.to_owned(),
            read(&f).expect("file io error"),
        ) {
            eprintln!("Warn: {f:?}({name}.{ext}) has already been added");
        }
    }

    if let Some(ofpath) = args.ofile {
        archive
            .write_async(&mut File::create(ofpath).await.expect("file open error"))
            .await
    } else {
        let foptr = unsafe { libc::fdopen(libc::dup(1), "wb\x00".as_ptr() as *const _) };
        archive.write(&mut NativeOfstream::from_stream_ptr(foptr).expect("fstream open error"))
    }
    .expect("fileio write error")
}

fn extract(args: CmdExtract) {
    let archive = par::Archive::new(
        par::native_io::PlatformNativeFileReader::open(&args.arc).expect("Failed to open archive"),
        args.check,
    )
    .expect("Failed to read archive");

    let (name, ext) = match &args.apath.rsplitn(2, '.').collect::<Vec<_>>()[..] {
        &[name] => (name, ""),
        &[ext, name] => (name, ext),
        _ => unreachable!(),
    };
    let Some(h) = archive.find_entry(name, ext) else {
        panic!("Entry not found in archive: {}", args.apath);
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
fn list(args: CmdList) {
    let archive = par::Archive::new(
        par::native_io::PlatformNativeFileReader::open(&args.arc).expect("Failed to open archive"),
        args.check,
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
