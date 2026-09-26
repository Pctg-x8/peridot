use crate::native_interface::nsbundle_path_for_resource;

pub struct PlatformAssetLoader {
    par_path: String,
    par: peridot_archive::Archive,
    par_async: Option<peridot_archive::ArchiveAsync>,
}
impl PlatformAssetLoader {
    pub fn new() -> Self {
        const PAR_PATH: &str = "assets";
        const PAR_EXT: &str = "par";

        let mut par_path_short = [0u8; 256];
        let mut par_path_len = par_path_short.len();
        let par_path = if unsafe {
            nsbundle_path_for_resource(
                PAR_PATH.as_ptr(),
                PAR_PATH.len(),
                PAR_EXT.as_ptr(),
                PAR_EXT.len(),
                par_path_short.as_mut_ptr(),
                &mut par_path_len,
            )
        } {
            unsafe { String::from_utf8_unchecked(par_path_short[..par_path_len].into()) }
        } else {
            let mut buf = Vec::with_capacity(par_path_len);
            unsafe {
                nsbundle_path_for_resource(
                    PAR_PATH.as_ptr(),
                    PAR_PATH.len(),
                    PAR_EXT.as_ptr(),
                    PAR_EXT.len(),
                    buf.spare_capacity_mut().as_mut_ptr().cast(),
                    &mut par_path_len,
                );
            }
            unsafe { String::from_utf8_unchecked(buf) }
        };
        println!("par_path: {par_path}");

        PlatformAssetLoader {
            par: peridot_archive::Archive::new(
                peridot::native_io::PlatformNativeFileReader::open(&par_path)
                    .expect("Failed to open primary asset"),
                false,
            )
            .map_err(|e| match e {
                peridot::archive::ArchiveReadError::IO(e) => e,
                peridot::archive::ArchiveReadError::IntegrityCheckFailed => {
                    tracing::error!("PrimaryArchive integrity check failed!");
                    std::io::Error::other("PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::SignatureMismatch => {
                    tracing::error!("PrimaryArchive signature mismatch!");
                    std::io::Error::other("PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::Lz4DecompressError(e) => {
                    tracing::error!(reason = ?e, "lz4 decompress error");
                    std::io::Error::other("PrimaryArchive read error")
                }
                _ => std::io::Error::other("PrimaryArchive read error"),
            })
            .expect("Failed to intiialize primary asset reader"),
            par_path,
            par_async: None,
        }
    }

    pub async fn post_init(&mut self) {
        self.par_async = Some(
            peridot_archive::ArchiveAsync::new(
                peridot::native_io::PlatformNativeFileReaderAsync::open(&self.par_path)
                    .expect("Failed to open primary asset"),
                false,
            )
            .await
            .map_err(|e| match e {
                peridot::archive::ArchiveReadError::IO(e) => e,
                peridot::archive::ArchiveReadError::IntegrityCheckFailed => {
                    tracing::error!("PrimaryArchive integrity check failed!");
                    std::io::Error::other("PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::SignatureMismatch => {
                    tracing::error!("PrimaryArchive signature mismatch!");
                    std::io::Error::other("PrimaryArchive read error")
                }
                peridot::archive::ArchiveReadError::Lz4DecompressError(e) => {
                    tracing::error!(reason = ?e, "lz4 decompress error");
                    std::io::Error::other("PrimaryArchive read error")
                }
                _ => std::io::Error::other("PrimaryArchive read error"),
            })
            .expect("Failed to intiialize primary asset reader"),
        );
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type AssetBlob<'a> =
        peridot_archive::ArchiveBinReader<'a, peridot::native_io::PlatformNativeFileReader>;
    type AssetBlobAsync<'a> = peridot_archive::ArchiveBinReaderAsync<
        'a,
        peridot::native_io::PlatformNativeFileReaderAsync,
    >;
    type StreamingAsset<'a> =
        peridot_archive::ArchiveBinReader<'a, peridot::native_io::PlatformNativeFileReader>;

    fn get<'a>(&'a self, path: &str, ext: &str) -> std::io::Result<Self::AssetBlob<'a>> {
        let entry = self.par.find_entry(path, ext).ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::NotFound, "not in primary package")
        })?;

        Ok(self.par.read_bin(entry))
    }

    fn get_async<'a>(
        &'a self,
        path: &str,
        ext: &str,
    ) -> impl core::future::Future<Output = std::io::Result<Self::AssetBlobAsync<'a>>> {
        async move {
            let par_async = unsafe { self.par_async.as_ref().unwrap_unchecked() };

            let entry = par_async.find_entry(path, ext).ok_or_else(|| {
                std::io::Error::new(std::io::ErrorKind::NotFound, "not in primary asset package")
            })?;

            Ok(par_async.read_bin(entry))
        }
    }

    fn get_streaming<'a>(
        &'a self,
        path: &str,
        ext: &str,
    ) -> std::io::Result<Self::StreamingAsset<'a>> {
        let entry = self.par.find_entry(path, ext).ok_or_else(|| {
            std::io::Error::new(std::io::ErrorKind::NotFound, "not in primary asset package")
        })?;

        Ok(self.par.read_bin(entry))
    }
}
