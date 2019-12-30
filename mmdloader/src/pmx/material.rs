
use super::{IndexValue, GlobalizedStrings, LoadingError, Header, Vec3, Vec4, TypedReader};
use std::io::Read;
use std::mem::transmute;

#[derive(Debug)]
pub enum EnvBlendMode { Disabled, Multiply, Additive, AdditionalVec4 }
pub enum ToonReference { Texture(Option<IndexValue>), Internal(i8) }
pub struct Material {
    pub name: GlobalizedStrings, pub diffuse_color: Vec4, pub specular_color: Vec3, pub specular_strength: f32,
    pub ambient_color: Vec3, pub drawing_flags: u8, pub edge_color: Vec4, pub edge_scale: f32,
    pub texture_index: Option<IndexValue>, pub envmap_texture_index: Option<IndexValue>,
    pub envmap_blend_mode: EnvBlendMode, pub toon_reference: ToonReference, pub meta: String, pub surface_affects: i32
}
impl Material {
    pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
        let name = header.string_reader.read_globalized(reader)?;
        let mut fixed_bytes = vec![0u8; 4 * (4 + 3 + 1 + 3 + 4 + 1) + 3 + header.index_sizes.texture.bytesize() * 2];
        reader.read_exact(&mut fixed_bytes)?;
        let envmap_blend_mode = match fixed_bytes[4*(4+3+1+3+4+1)+1+header.index_sizes.texture.bytesize()*2] {
            0 => EnvBlendMode::Disabled, 1 => EnvBlendMode::Multiply, 2 => EnvBlendMode::Additive,
            3 => EnvBlendMode::AdditionalVec4, v => return Err(LoadingError::UnknownEnvBlendMode(v))
        };
        let toon_reference = match fixed_bytes[fixed_bytes.len() - 1] {
            0 => {
                let mut buf = vec![0u8; header.index_sizes.texture.bytesize()];
                reader.read_exact(&mut buf)?;
                ToonReference::Texture(IndexValue::from_bytes(&buf, header.index_sizes.texture))
            },
            1 => {
                let mut buf = [0u8; 1];
                reader.read_exact(&mut buf)?;
                ToonReference::Internal(unsafe { transmute(buf[0]) })
            },
            v => return Err(LoadingError::UnknownToonReference(v))
        };
        let meta = header.string_reader.read_string(reader)?;
        let surface_affects = i32::read_value1(reader)?;

        Ok(Material
        {
            name, diffuse_color: Vec4::from_bytes(&fixed_bytes[..]),
            specular_color: Vec3::from_bytes(&fixed_bytes[4*4..]),
            specular_strength: f32::from_bytes(&fixed_bytes[4*(4+3)..]),
            ambient_color: Vec3::from_bytes(&fixed_bytes[4*(4+3+1)..]),
            drawing_flags: fixed_bytes[4*(4+3+1+3)],
            edge_color: Vec4::from_bytes(&fixed_bytes[4*(4+3+1+3)+1..]),
            edge_scale: f32::from_bytes(&fixed_bytes[4*(4+3+1+3+4)+1..]),
            texture_index: IndexValue::from_bytes(&fixed_bytes[4*(4+3+1+3+4+1)+1..], header.index_sizes.texture),
            envmap_texture_index: IndexValue::from_bytes(
                &fixed_bytes[4*(4+3+1+3+4+1)+1+header.index_sizes.texture.bytesize()..],
                header.index_sizes.texture),
            envmap_blend_mode, toon_reference, meta, surface_affects
        })
    }
}
