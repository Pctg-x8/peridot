pub struct PlatformAssetLoader {
    assetdb: peridot::AssetDatabase,
}
impl PlatformAssetLoader {
    pub fn new() -> Self {
        let assetdb_path = nsbundle_path_for_resource(".runtime-assets", "");
        eprintln!("path resolved: {assetdb_path:?}");
        let assetdb = match peridot::AssetDatabase::open(assetdb_path) {
            Ok(db) => db,
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to open asset database");
                std::process::abort();
            }
        };

        PlatformAssetLoader { assetdb }
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader {
    type AssetBlob<'a> = peridot::native_io::PlatformNativeFileReader;
    type AssetBlobAsync<'a> = peridot::native_io::PlatformNativeFileReaderAsync;
    type StreamingAsset<'a> =
        peridot::native_io::RandomBlobReadSeekAdapter<peridot::native_io::PlatformNativeFileReader>;

    #[inline(always)]
    fn asset_db<'a>(&'a self) -> &'a peridot::AssetDatabase {
        &self.assetdb
    }

    fn get<'a>(&'a self, id: peridot::AssetID) -> std::io::Result<Self::AssetBlob<'a>> {
        peridot::native_io::PlatformNativeFileReader::open(resolve_nsbundle_path(
            std::path::Path::new(".runtime-assets").join(id.build_runtime_asset_path_relative()),
        ))
    }

    fn get_async<'a>(
        &'a self,
        id: peridot::AssetID,
    ) -> impl core::future::Future<Output = std::io::Result<Self::AssetBlobAsync<'a>>> {
        async move {
            peridot::native_io::PlatformNativeFileReaderAsync::open(resolve_nsbundle_path(
                std::path::Path::new(".runtime-assets")
                    .join(id.build_runtime_asset_path_relative()),
            ))
        }
    }

    fn get_streaming<'a>(
        &'a self,
        id: peridot::AssetID,
    ) -> std::io::Result<Self::StreamingAsset<'a>> {
        peridot::native_io::PlatformNativeFileReader::open(resolve_nsbundle_path(
            std::path::Path::new(".runtime-assets").join(id.build_runtime_asset_path_relative()),
        ))
        .map(peridot::native_io::RandomBlobReadSeekAdapter::new)
    }
}

fn resolve_nsbundle_path(path: impl AsRef<std::path::Path>) -> String {
    let path = path.as_ref();
    match path.parent() {
        Some(p) => nsbundle_path_for_resource_in_subdirectory(
            path.file_name()
                .expect("no filename")
                .to_str()
                .expect("invalid str"),
            path.extension()
                .map_or("", |x| x.to_str().expect("invalid str")),
            p.to_str().expect("invalid str"),
        ),
        None => nsbundle_path_for_resource(
            path.file_name()
                .expect("no filename")
                .to_str()
                .expect("invalid str"),
            path.extension()
                .map_or("", |x| x.to_str().expect("invalid str")),
        ),
    }
}

fn nsbundle_path_for_resource(path: &str, ext: &str) -> String {
    let mut buf = [core::mem::MaybeUninit::<u8>::uninit(); 256];
    let mut len = 256;
    if unsafe {
        crate::native_interface::nsbundle_path_for_resource(
            path.as_ptr(),
            path.len(),
            ext.as_ptr(),
            ext.len(),
            buf.as_mut_ptr().cast(),
            &mut len,
        )
    } {
        unsafe {
            str::from_utf8_unchecked(core::mem::transmute::<&[core::mem::MaybeUninit<_>], &[_]>(
                &buf[..len],
            ))
            .to_owned()
        }
    } else {
        let mut buf = Vec::with_capacity(len);
        unsafe {
            crate::native_interface::nsbundle_path_for_resource(
                path.as_ptr(),
                path.len(),
                ext.as_ptr(),
                ext.len(),
                buf.spare_capacity_mut().as_mut_ptr().cast(),
                &mut len,
            );
        }
        unsafe { String::from_utf8_unchecked(buf) }
    }
}

fn nsbundle_path_for_resource_in_subdirectory(path: &str, ext: &str, subdir: &str) -> String {
    let mut buf = [core::mem::MaybeUninit::<u8>::uninit(); 256];
    let mut len = 256;
    if unsafe {
        crate::native_interface::nsbundle_path_for_resource_in_subdirectory(
            path.as_ptr(),
            path.len(),
            ext.as_ptr(),
            ext.len(),
            subdir.as_ptr(),
            subdir.len(),
            buf.as_mut_ptr().cast(),
            &mut len,
        )
    } {
        unsafe {
            str::from_utf8_unchecked(core::mem::transmute::<&[core::mem::MaybeUninit<_>], &[_]>(
                &buf[..len],
            ))
            .to_owned()
        }
    } else {
        let mut buf = Vec::with_capacity(len);
        unsafe {
            crate::native_interface::nsbundle_path_for_resource_in_subdirectory(
                path.as_ptr(),
                path.len(),
                ext.as_ptr(),
                ext.len(),
                subdir.as_ptr(),
                subdir.len(),
                buf.spare_capacity_mut().as_mut_ptr().cast(),
                &mut len,
            );
        }
        unsafe { String::from_utf8_unchecked(buf) }
    }
}
