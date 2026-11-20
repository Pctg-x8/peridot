use std::{
    collections::HashMap,
    io::{BufRead, IoSlice, Read, Write},
};

use peridot::{FromAssetBlob, LogicalAssetData};
use peridot_math::Vector4F32;
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
#[derive(Clone, Copy, Debug)]
pub enum CullingMode {
    None = 0,
    Front = 1,
    Back = 2,
    Both = 3,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug)]
pub enum PrimitiveOrder {
    Clcowise = 0,
    CounterClockwise = 1,
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
