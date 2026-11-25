use std::{
    collections::HashMap,
    io::{BufRead, IoSlice, Read, Write},
};

use bedrock as br;
use peridot::{FromAssetBlob, LogicalAssetData};
use peridot_math::{Vector4, Vector4F32};
use peridot_serialization_utils::{PascalStr, PascalString, VariableULong};
use peridot_types::AssetID;
use vectored_io_helper::{
    mkioslice, mkioslicem, mkioslicem_field, read_vectored_all, write_vectored_all,
};

pub enum MaterialPropertySlot<ExternalAssetRef> {
    Float4(Vector4F32),
    Texture2D(ExternalAssetRef),
}
impl MaterialPropertySlot<AssetID> {
    fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::Float4(v) => write_vectored_all(
                w,
                &mut [
                    IoSlice::new(&[0x00]),
                    mkioslice(&v.0),
                    mkioslice(&v.1),
                    mkioslice(&v.2),
                    mkioslice(&v.3),
                ],
            ),
            Self::Texture2D(r) => {
                write_vectored_all(w, &mut [IoSlice::new(&[0x80]), IoSlice::new(r.as_bytes())])
            }
        }
    }

    fn read(r: &mut (impl Read + ?Sized), swap_bytes: bool) -> std::io::Result<Self> {
        let mut first_byte = [0u8];
        r.read_exact(&mut first_byte)?;

        match first_byte[0] {
            0x00 => {
                let mut f4 = core::mem::MaybeUninit::<Vector4F32>::uninit();
                read_vectored_all(
                    r,
                    &mut [
                        mkioslicem_field!(f4, Vector4F32, 0),
                        mkioslicem_field!(f4, Vector4F32, 1),
                        mkioslicem_field!(f4, Vector4F32, 2),
                        mkioslicem_field!(f4, Vector4F32, 3),
                    ],
                )?;

                if swap_bytes {
                    let f4 = unsafe { f4.assume_init_mut() };
                    f4.0 = f32::from_bits(f4.0.to_bits().swap_bytes());
                    f4.1 = f32::from_bits(f4.1.to_bits().swap_bytes());
                    f4.2 = f32::from_bits(f4.2.to_bits().swap_bytes());
                    f4.3 = f32::from_bits(f4.3.to_bits().swap_bytes());
                }

                Ok(Self::Float4(unsafe { f4.assume_init() }))
            }
            0x80 => {
                let mut asset_id = core::mem::MaybeUninit::<AssetID>::uninit();
                read_vectored_all(r, &mut [mkioslicem(&mut asset_id)])?;

                // AssetID is byte-order unaware

                Ok(Self::Texture2D(unsafe { asset_id.assume_init() }))
            }
            v => panic!("unknown type byte: 0x{v:02x}"),
        }
    }
}

pub struct MaterialPropertyBlock<ExternalAssetRef> {
    pub map: HashMap<String, MaterialPropertySlot<ExternalAssetRef>>,
}
impl<ExternalAssetRef> Default for MaterialPropertyBlock<ExternalAssetRef> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            map: HashMap::new(),
        }
    }
}
impl<ExternalAssetRef> MaterialPropertyBlock<ExternalAssetRef> {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn set_float4(&mut self, name: String, value: Vector4F32) {
        self.map.insert(name, MaterialPropertySlot::Float4(value));
    }

    pub fn set_texture2d(&mut self, name: String, value: ExternalAssetRef) {
        self.map
            .insert(name, MaterialPropertySlot::Texture2D(value));
    }
}
impl MaterialPropertyBlock<AssetID> {
    fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        VariableULong(self.map.len() as _).write(w)?;
        for (k, v) in self.map.iter() {
            PascalStr(k.as_str()).write(w)?;
            v.write(w)?;
        }

        Ok(())
    }

    fn read(r: &mut (impl BufRead + ?Sized), swap_bytes: bool) -> std::io::Result<Self> {
        let len = VariableULong::read(r)?;
        let mut map = HashMap::with_capacity(len.0 as _);
        for _ in 0..len.0 {
            let k = PascalString::read(r)?;
            let v = MaterialPropertySlot::read(r, swap_bytes)?;

            map.insert(k.0, v);
        }

        Ok(Self { map })
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Default)]
pub enum CullingMode {
    None = 0,
    Front = 1,
    #[default]
    Back = 2,
    Both = 3,
}
impl CullingMode {
    #[inline(always)]
    pub const fn into_vk(self) -> br::CullModeFlags {
        match self {
            Self::None => br::CullModeFlags::NONE,
            Self::Front => br::CullModeFlags::FRONT,
            Self::Back => br::CullModeFlags::BACK,
            Self::Both => br::CullModeFlags::FRONT_AND_BACK,
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Default)]
pub enum PrimitiveOrder {
    Clockwise = 0,
    #[default]
    CounterClockwise = 1,
}
impl PrimitiveOrder {
    #[inline(always)]
    pub const fn into_vk(self) -> br::FrontFace {
        match self {
            Self::Clockwise => br::FrontFace::Clockwise,
            Self::CounterClockwise => br::FrontFace::CounterClockwise,
        }
    }
}

pub struct MaterialAsset {
    pub shader: AssetID,
    pub properties: MaterialPropertyBlock<AssetID>,
    pub wireframed: bool,
    pub culling: CullingMode,
    pub prim_order: PrimitiveOrder,
}
impl MaterialAsset {
    pub fn write(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        // bom
        w.write_all(&0x1234u16.to_ne_bytes())?;
        // shader
        w.write_all(self.shader.as_bytes())?;
        // wpXXXXcc
        w.write_all(&[if self.wireframed { 0x80 } else { 0 }
            | ((self.prim_order as u8) << 6)
            | (self.culling as u8 & 0x03)])?;
        // props
        self.properties.write(w)?;

        Ok(())
    }

    pub fn read(r: &mut (impl BufRead + ?Sized)) -> std::io::Result<Self> {
        let mut bom = 0u16;
        r.read_exact(unsafe {
            core::slice::from_raw_parts_mut(core::ptr::from_mut(&mut bom).cast(), 2)
        })?;
        let swap_bytes = if bom == 0x1234 {
            false
        } else if bom.swap_bytes() == 0x1234 {
            true
        } else {
            panic!("invalid bom")
        };

        let mut shader = core::mem::MaybeUninit::<AssetID>::uninit();
        r.read_exact(unsafe {
            core::slice::from_raw_parts_mut(shader.as_mut_ptr().cast(), size_of::<AssetID>())
        })?;
        let shader = unsafe { shader.assume_init() };

        let mut render_option_bits = [0u8];
        r.read_exact(&mut render_option_bits)?;
        let wireframed = render_option_bits[0] & 0x80 != 0;
        let prim_order = unsafe {
            core::mem::transmute::<u8, PrimitiveOrder>((render_option_bits[0] >> 6) & 0x01)
        };
        let culling =
            unsafe { core::mem::transmute::<u8, CullingMode>(render_option_bits[0] & 0x03) };

        let properties = MaterialPropertyBlock::read(r, swap_bytes)?;

        Ok(Self {
            shader,
            properties,
            wireframed,
            culling,
            prim_order,
        })
    }
}
impl LogicalAssetData for MaterialAsset {
    const ASSET_TYPE: peridot::AssetType = peridot::ASSET_TYPE_MATERIAL;
}
impl FromAssetBlob for MaterialAsset {
    type Error = std::io::Error;

    #[inline(always)]
    fn from_asset_blob<'a, Blob: peridot::AssetBlob + 'a>(blob: Blob) -> Result<Self, Self::Error> {
        Self::read(&mut std::io::BufReader::new(
            peridot_native_io::RandomBlobReadSeekAdapter::new(blob),
        ))
    }
}

#[cfg(feature = "user-asset")]
#[derive(Default)]
pub struct MaterialUserAsset {
    pub shader: Option<AssetID>,
    pub properties: MaterialPropertyBlock<AssetID>,
    pub wireframed: bool,
    pub culling: CullingMode,
    pub prim_order: PrimitiveOrder,
}
#[cfg(feature = "user-asset")]
impl MaterialUserAsset {
    pub fn read(r: &mut (impl BufRead + ?Sized)) -> Result<Self, UserAssetReadError> {
        let mut sink = Self::default();

        let mut has_illformed = false;
        for line in r.lines() {
            let line = line?;
            let line = line.trim_start();

            if line.is_empty() {
                continue;
            }

            let Some((key, rest)) = line.split_once('=') else {
                eprintln!("invalid kvp form");
                continue;
            };

            match key.trim().to_lowercase().as_str() {
                "shader" => {
                    let Some(asset_id) = AssetID::deserialize_chars(rest.trim().chars()) else {
                        eprintln!("invalid asset id");
                        has_illformed = true;
                        continue;
                    };

                    sink.shader = Some(asset_id);
                }
                "render-options.wireframed" => {
                    sink.wireframed = rest.trim().eq_ignore_ascii_case("true");
                }
                "render-options.culling" => {
                    sink.culling = match rest.trim().to_lowercase().as_str() {
                        "none" => CullingMode::None,
                        "front" => CullingMode::Front,
                        "back" => CullingMode::Back,
                        "both" => CullingMode::Both,
                        x => {
                            eprintln!("invalid culling mode: {x}");
                            has_illformed = true;
                            continue;
                        }
                    };
                }
                "render-options.prim_order" => {
                    sink.prim_order = match rest.trim().to_lowercase().as_str() {
                        "clockwise" => PrimitiveOrder::Clockwise,
                        "counter-clockwise" => PrimitiveOrder::CounterClockwise,
                        x => {
                            eprintln!("invalid primitive order: {x}");
                            has_illformed = true;
                            continue;
                        }
                    }
                }
                x if x.starts_with("property.") => {
                    let property_key = x.strip_prefix("property.").expect("not prefixed?");
                    let Some((r#type, value)) = rest.split_once('=') else {
                        eprintln!("invalid property value");
                        has_illformed = true;
                        continue;
                    };

                    sink.properties.map.insert(
                        property_key.into(),
                        match r#type.trim().to_lowercase().as_str() {
                            "float4" => {
                                let mut values = value.splitn(4, ',');
                                let x = values.next().unwrap_or("0");
                                let y = values.next().unwrap_or("0");
                                let z = values.next().unwrap_or("0");
                                let w = values.next().unwrap_or("0");
                                let Ok(x) = x.trim().parse::<f32>() else {
                                    eprintln!("invalid float4.x value");
                                    has_illformed = true;
                                    continue;
                                };
                                let Ok(y) = y.trim().parse::<f32>() else {
                                    eprintln!("invalid float4.y value");
                                    has_illformed = true;
                                    continue;
                                };
                                let Ok(z) = z.trim().parse::<f32>() else {
                                    eprintln!("invalid float4.z value");
                                    has_illformed = true;
                                    continue;
                                };
                                let Ok(w) = w.trim().parse::<f32>() else {
                                    eprintln!("invalid float4.w value");
                                    has_illformed = true;
                                    continue;
                                };

                                MaterialPropertySlot::Float4(Vector4(x, y, z, w))
                            }
                            "texture2d" => {
                                let Some(asset_id) =
                                    AssetID::deserialize_chars(value.trim().chars())
                                else {
                                    eprintln!("invalid asset id");
                                    has_illformed = true;
                                    continue;
                                };

                                MaterialPropertySlot::Texture2D(asset_id)
                            }
                            x => {
                                eprintln!("unsupported property type: {x}");
                                has_illformed = true;
                                continue;
                            }
                        },
                    );
                }
                x => {
                    eprintln!("unknown key: {x}");
                    has_illformed = true;
                    continue;
                }
            }
        }

        if has_illformed {
            return Err(UserAssetReadError::Illformed);
        }

        Ok(sink)
    }
}

#[cfg(feature = "user-asset")]
#[derive(Debug, thiserror::Error)]
pub enum UserAssetReadError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error("illformed asset content")]
    Illformed,
}

#[cfg(feature = "asset-processing")]
pub struct AssetProcessor;
#[cfg(feature = "asset-processing")]
impl AssetProcessor {
    const META_GENERATED_ASSET_KEY: &str =
        "io.ct2.peridot.asset-processor.material.generated-asset-id";
}
#[cfg(feature = "asset-processing")]
impl peridot_asset_processing::AssetProcessor for AssetProcessor {
    fn can_process(&self, source_path: &std::path::Path) -> bool {
        source_path.extension().is_some_and(|x| x == "p-mat")
    }

    fn process(
        &self,
        source_path: &std::path::Path,
        data: &peridot_asset_processing::ProcessTargetFileData,
        asset_group_id: peridot::AssetID,
        metadata: &mut peridot_asset_processing::MetadataAccess,
        ctx: &mut peridot_asset_processing::AssetProcessContext,
    ) -> Result<peridot_asset_processing::AssetProcessResult, Box<dyn std::error::Error>> {
        if !data.is_dirty {
            return Ok(peridot_asset_processing::AssetProcessResult::Skipped);
        }

        let user_asset_data = MaterialUserAsset::read(&mut std::io::BufReader::new(
            std::fs::File::open(source_path)?,
        ))?;
        let asset_data = MaterialAsset {
            shader: user_asset_data
                .shader
                .ok_or(AssetProcessingError::NoShaderSpecified)?,
            properties: user_asset_data.properties,
            wireframed: user_asset_data.wireframed,
            culling: user_asset_data.culling,
            prim_order: user_asset_data.prim_order,
        };

        let asset_id = ctx.acquire_asset_id(
            metadata,
            Self::META_GENERATED_ASSET_KEY.into(),
            &asset_group_id,
            peridot::ASSET_TYPE_MATERIAL,
            0,
        );
        asset_data.write(
            &mut std::fs::File::options()
                .write(true)
                .create(true)
                .truncate(true)
                .open(ctx.prepare_runtime_asset_output(&asset_id))?,
        )?;

        Ok(peridot_asset_processing::AssetProcessResult::Ok)
    }
}

#[cfg(feature = "asset-processing")]
#[derive(Debug, thiserror::Error)]
pub enum AssetProcessingError {
    #[error("no shader specified")]
    NoShaderSpecified,
}
