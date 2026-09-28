use std::collections::HashMap;
use std::fmt::Write;
use std::io::prelude::{Read, Seek};
use std::io::{Error as IOError, Result as IOResult, SeekFrom};
use std::path::{Path, PathBuf};

#[repr(transparent)]
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct AssetID([u8; 16]);
impl core::fmt::Debug for AssetID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for &b in &self.0[0..4] {
            hex2(f, b)?;
        }
        f.write_char('-')?;
        for &b in &self.0[4..8] {
            hex2(f, b)?;
        }
        f.write_char('-')?;
        for &b in &self.0[8..12] {
            hex2(f, b)?;
        }
        f.write_char('-')?;
        for &b in &self.0[12..16] {
            hex2(f, b)?;
        }

        Ok(())
    }
}
impl AssetID {
    pub const fn from_bytes(b: [u8; 16]) -> Self {
        Self(b)
    }

    pub const fn as_bytes(&self) -> &[u8; 16] {
        &self.0
    }

    pub fn build_runtime_asset_path_part(&self) -> String {
        let mut x = String::with_capacity(16 * 2 + 1);
        for &b in &self.0[..2] {
            let _ = hex2(&mut x, b);
        }
        x.push('/');
        for &b in &self.0[2..] {
            let _ = hex2(&mut x, b);
        }

        x
    }

    pub fn build_runtime_asset_path(&self, runtime_asset_dir: impl AsRef<Path>) -> PathBuf {
        runtime_asset_dir
            .as_ref()
            .join(self.build_runtime_asset_path_part())
    }

    pub const SERIALIZE_TEXT_LEN: usize = 32;

    pub fn serialize_text(&self, sink: &mut (impl core::fmt::Write + ?Sized)) -> core::fmt::Result {
        for b in self.0 {
            #[inline(always)]
            fn h(v: u8) -> u8 {
                match v {
                    0..=9 => v + b'0',
                    10..=15 => v - 10 + b'a',
                    _ => unreachable!(),
                }
            }

            sink.write_char(char::from(h(b >> 4)))?;
            sink.write_char(char::from(h(b & 0x0f)))?;
        }

        Ok(())
    }

    pub fn deserialize_text(text: &str) -> Option<Self> {
        if text.len() != 32 {
            return None;
        }
        let mut bytes = [0u8; 16];
        let mut wptr = 0;
        let mut ub = None;
        for c in text.chars() {
            let v = match c {
                '0'..='9' => c as u8 - b'0',
                'a'..='f' => c as u8 - b'a' + 10,
                _ => return None,
            };

            match ub {
                None => {
                    ub = Some(v);
                }
                Some(ub1) => {
                    bytes[wptr] = (ub1 << 4) | v;

                    ub = None;
                    wptr += 1;
                }
            }
        }

        Some(Self(bytes))
    }
}

#[inline(always)]
fn hex2(f: &mut (impl core::fmt::Write + ?Sized), v: u8) -> core::fmt::Result {
    #[inline(always)]
    fn h(v: u8) -> char {
        match v {
            0..=9 => (v + b'0') as char,
            10..=15 => (v - 10 + b'a') as char,
            _ => unreachable!(),
        }
    }

    f.write_char(h(v >> 4))?;
    f.write_char(h(v & 0x0f))?;

    Ok(())
}

#[repr(transparent)]
pub struct AssetIDGenerator(rand_pcg::Pcg64Mcg);
impl AssetIDGenerator {
    pub fn new() -> Self {
        match rand_pcg::Pcg64Mcg::try_from_rng(&mut rand::rngs::SysRng) {
            Ok(r) => Self(r),
            Err(e) => {
                tracing::error!(reason = %e, "SysRng failed, falling back to thread rng as seed");
                Self(rand_pcg::Pcg64Mcg::from_rng(
                    &mut rand::rngs::ThreadRng::default(),
                ))
            }
        }
    }

    pub fn generate(&mut self) -> AssetID {
        let mut buf = [0u8; 16];
        self.0.fill_bytes(&mut buf);
        AssetID(buf)
    }
}

/// アセット種別を表す数値
pub type AssetType = u16;

// エンジン側で定義済みのアセット種別レジストリ
/// 生アセット（Asset Processorで処理されていないもの）
pub const ASSET_TYPE_RAW: AssetType = 0;
/// メッシュ
pub const ASSET_TYPE_MESH: AssetType = 1;
/// 2D画像
pub const ASSET_TYPE_IMAGE2D: AssetType = 10;
/// Sprite Atlas
pub const ASSET_TYPE_SPRITE_ATLAS: AssetType = 19;
/// サウンド
pub const ASSET_TYPE_SOUND: AssetType = 20;
/// Rendering Configuration (Vulkan向け ここの区別は将来的になくしたい)
pub const ASSET_TYPE_COMPILED_RENDERING_CONFIGURATION_VK: AssetType = 100;

pub struct AssetDatabase {
    pub db: peridot_tp_sqlite3::Owned<peridot_tp_sqlite3::DB>,
}
impl AssetDatabase {
    pub fn open(runtime_asset_dir: impl AsRef<Path>) -> Result<Self, peridot_tp_sqlite3::Error> {
        let path = runtime_asset_dir.as_ref().join("db");
        let needs_initialization = !path.exists();
        let con = peridot_tp_sqlite3::DB::open(
            path,
            peridot_tp_sqlite3::OpenFlags::READWRITE | peridot_tp_sqlite3::OpenFlags::CREATE,
        )?;

        Ok(Self::from_raw_connection(con, needs_initialization))
    }

    pub fn from_raw_connection(
        mut con: peridot_tp_sqlite3::Owned<peridot_tp_sqlite3::DB>,
        needs_initialization: bool,
    ) -> Self {
        if needs_initialization {
            if let Err(e) = con.exec(include_str!("../assetdb.sql")) {
                tracing::error!(reason = ?e, msg = ?con.errmsg(), "assetdb initialization failed");
            }
        }

        Self { db: con }
    }

    #[tracing::instrument(skip(self))]
    pub fn query_first_loadable_asset_id_of_type(
        &self,
        identifier: &str,
        ty: AssetType,
    ) -> Option<AssetID> {
        const Q: &str = "Select runtime_asset_id from asset_group inner join loadable_asset on group_id = asset_group.id where loadable_asset.identifier = ? and asset_group.asset_type = ? limit 1";

        // TODO: 本当は使いまわしたい（毎回コンパイルしたくない）がいまいちうまい方法が思いつかない（特にasyncで多重化してるのでスレッドローカルじゃなくて同一スレッドでの多重処理を考慮する必要がある）
        let mut stmt = self.db.prepare(Q, PrepareFlags::empty()).expect("prepare");
        stmt.bind_text(1, identifier).expect("bind");
        stmt.bind_int(2, ty as _).expect("bind");
        let has_next = stmt.step().expect("step");
        if !has_next {
            // no asset found
            tracing::warn!("asset not found in the db");
            return None;
        }

        assert_eq!(stmt.column_bytes(0), 16);
        Some(AssetID::from_bytes(unsafe { *stmt.column_blob(0).cast() }))
    }

    pub fn query_loadable_asset_ids_of_type(
        &self,
        identifier: &str,
        ty: AssetType,
    ) -> Vec<AssetID> {
        const Q: &str = "Select runtime_asset_id from asset_group inner join loadable_asset on group_id = asset_group.id where loadable_asset.identifier = ? and asset_group.asset_type = ?";

        // TODO: 本当は使いまわしたい（毎回コンパイルしたくない）がいまいちうまい方法が思いつかない（特にasyncで多重化してるのでスレッドローカルじゃなくて同一スレッドでの多重処理を考慮する必要がある）
        let mut stmt = self.db.prepare(Q, PrepareFlags::empty()).expect("prepare");
        stmt.bind_text(1, identifier).expect("bind");
        stmt.bind_int(2, ty as _).expect("bind");

        let mut results = Vec::new();
        while stmt.step().expect("step") {
            assert_eq!(stmt.column_bytes(0), 16);
            results.push(AssetID::from_bytes(unsafe { *stmt.column_blob(0).cast() }));
        }

        results
    }
}

pub trait InputStream: Read {
    fn skip(&mut self, amount: u64) -> IOResult<u64>;
}
impl<T> InputStream for T
where
    T: Seek + Read,
{
    fn skip(&mut self, amount: u64) -> IOResult<u64> {
        self.seek(SeekFrom::Current(amount as _))
    }
}

pub trait PlatformAssetLoader {
    type AssetBlob<'a>: AssetBlob + 'a
    where
        Self: 'a;
    type AssetBlobAsync<'a>: AssetBlobAsync + 'a
    where
        Self: 'a;
    type StreamingAsset<'a>: InputStream + Sync + Send + 'a
    where
        Self: 'a;

    fn asset_db<'a>(&'a self) -> &'a AssetDatabase;

    fn get<'a>(&'a self, id: AssetID) -> IOResult<Self::AssetBlob<'a>>;
    fn get_async<'a>(
        &'a self,
        id: AssetID,
    ) -> impl core::future::Future<Output = IOResult<Self::AssetBlobAsync<'a>>>;
    fn get_streaming<'a>(&'a self, id: AssetID) -> IOResult<Self::StreamingAsset<'a>>;
}
pub trait LogicalAssetData: Sized {
    const EXT: &'static str;
    const ASSET_TYPE: AssetType;
}

pub trait AssetBlob: peridot_native_io::RandomReadBlob + peridot_native_io::BlobMetadata {}
impl AssetBlob for peridot_native_io::PlatformNativeFileReader {}
impl<'a, T: peridot_native_io::RandomReadBlob + peridot_native_io::MemoryMapBlob + 'a> AssetBlob
    for peridot_archive::ArchiveBinReader<'a, T>
{
}

pub trait FromAssetBlob: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error>;
}

pub trait FromStreamingAsset<'a>: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset<Asset: InputStream + Sync + Send + 'a>(asset: Asset)
        -> Result<Self, Self::Error>;
}

pub trait AssetBlobAsync:
    peridot_native_io::RandomReadBlobAsync + peridot_native_io::BlobMetadataAsync
{
}
impl AssetBlobAsync for peridot_native_io::PlatformNativeFileReaderAsync {}
impl<'a, T: peridot_native_io::RandomReadBlobAsync + peridot_native_io::MemoryMapBlob + 'a>
    AssetBlobAsync for peridot_archive::ArchiveBinReaderAsync<'a, T>
{
}

pub trait FromAssetBlobAsync: LogicalAssetData {
    type Error: From<IOError>;
    fn from_asset_blob_async<'a, Blob: AssetBlobAsync + 'a>(
        blob: Blob,
    ) -> impl core::future::Future<Output = Result<Self, Self::Error>>;
}

// Shader Blob //
use bedrock as br;
use peridot_tp_sqlite3::PrepareFlags;
use rand::{Rng, SeedableRng};

/// An shader blob representation as Asset
pub struct SpirvShaderBlob(Vec<u32>);
impl SpirvShaderBlob {
    /// Instantiates the Shader Binary as a VkShaderModule
    #[inline]
    pub fn instantiate<Device: br::Device>(
        &self,
        dev: Device,
    ) -> br::Result<br::ShaderModuleObject<Device>> {
        br::ShaderModuleObject::new(dev, &br::ShaderModuleCreateInfo::new(&self.0))
    }
}
impl LogicalAssetData for SpirvShaderBlob {
    const EXT: &'static str = "spv";
    const ASSET_TYPE: u16 = 0; // raw asset
}
impl FromAssetBlob for SpirvShaderBlob {
    type Error = IOError;

    fn from_asset_blob<'a, Blob: AssetBlob + 'a>(blob: Blob) -> Result<Self, IOError> {
        let len = blob.byte_length()?;
        let mut buf = Vec::with_capacity((len as usize + 3) >> 2);
        blob.read_exact(0, unsafe {
            core::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut _, buf.capacity() << 2)
        })?;
        unsafe {
            buf.set_len(buf.capacity());
        }

        Ok(SpirvShaderBlob(buf))
    }
}
impl FromAssetBlobAsync for SpirvShaderBlob {
    type Error = IOError;

    fn from_asset_blob_async<'a, Blob: AssetBlobAsync + 'a>(
        blob: Blob,
    ) -> impl core::future::Future<Output = Result<Self, Self::Error>> {
        async move {
            let len = blob.byte_length_async().await?;
            let mut buf = Vec::with_capacity((len as usize + 3) >> 2);
            blob.read_exact_async(0, unsafe {
                core::slice::from_raw_parts_mut(buf.as_mut_ptr() as *mut _, buf.capacity() << 2)
            })
            .await?;
            unsafe {
                buf.set_len(buf.capacity());
            }

            Ok(Self(buf))
        }
    }
}
