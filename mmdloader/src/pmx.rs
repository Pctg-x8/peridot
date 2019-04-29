
mod mesh; pub use self::mesh::*;
mod material; pub use self::material::*;
mod anim; pub use self::anim::*;
mod physics; pub use self::physics::*;

mod types; pub use self::types::*;
mod reader; pub use self::reader::*;

use std::io::Read;

pub struct Header
{
    pub version: f32, pub string_reader: Box<StringReader>, pub additional_vec4_count: u8,
    pub index_sizes: IndexSizes, pub model_name: GlobalizedStrings, pub comment: GlobalizedStrings
}
#[repr(C)] struct Globals
{
    string_encoding_type: u8, additional_vec4_count: u8,
    vertex_index_size: u8, texture_index_size: u8, material_index_size: u8,
    bone_index_size: u8, morph_index_size: u8, rigid_body_index_size: u8
}
pub struct IndexSizes
{
    pub vertex: IndexSize, pub texture: IndexSize, pub material: IndexSize, pub bone: IndexSize,
    pub morph: IndexSize, pub rigid_body: IndexSize
}
impl Header
{
    pub fn load<R: Read>(reader: &mut R) -> Result<Self, LoadingError>
    {
        #[repr(C)] struct FixedHeader { signature: [u8; 4], version: f32, globals_count: u8 }
        let mut fixed_headers: FixedHeader = unsafe { std::mem::uninitialized() };
        reader.read_exact(unsafe { &mut *(&mut fixed_headers as *mut _ as *mut [u8; 9]) })?;
        if fixed_headers.signature != [0x50, 0x4d, 0x58, 0x20] { return Err(LoadingError::SignatureMismatch); }
        if fixed_headers.globals_count != 8 { return Err(LoadingError::MissingGlobals(fixed_headers.globals_count)); }

        // Globals //
        let mut globals_b = vec![0u8; fixed_headers.globals_count as _];
        reader.read_exact(&mut globals_b)?;
        let globals = unsafe { &*(globals_b.as_ptr() as *const Globals) };
        let string_reader: Box<StringReader> = match globals.string_encoding_type
        {
            0 => Box::new(Utf16LEStringReader) as _, 1 => Box::new(Utf8StringReader) as _,
            v => return Err(LoadingError::UnknownStringEncoding(v))
        };
        let index_sizes = IndexSizes
        {
            vertex: IndexSize::from(globals.vertex_index_size),
            texture: IndexSize::from(globals.texture_index_size),
            material: IndexSize::from(globals.material_index_size),
            bone: IndexSize::from(globals.bone_index_size),
            morph: IndexSize::from(globals.morph_index_size),
            rigid_body: IndexSize::from(globals.rigid_body_index_size)
        };

        let model_name = string_reader.read_globalized(reader)?;
        let comment = string_reader.read_globalized(reader)?;

        Ok(Header
        {
            version: fixed_headers.version, string_reader, index_sizes, model_name, comment,
            additional_vec4_count: globals.additional_vec4_count
        })
    }
}
