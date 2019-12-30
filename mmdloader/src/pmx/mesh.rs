
use std::io::{Read, Result as IOResult};
use super::{Vec2, Vec3, Vec4, LoadingError, IndexSize, TypedReader};

#[derive(Debug)]
pub struct Vertex {
    pub position: Vec3, pub normal: Vec3, pub uv: Vec2, pub additional_vec4s: Vec<Vec4>,
    pub deform: WeightDeform, pub edge_scale: f32
}
#[derive(Debug)]
pub enum WeightDeform {
    AffectSingleBone(i32),
    AffectDoubleBones(i32, i32, f32),
    AffectFourBones([i32; 4], [f32; 4]),
    Spherical { b1: i32, b2: i32, weight1: f32, c: Vec3, r0: Vec3, r1: Vec3 },
    DualQuaternion([i32; 4], [f32; 4])
}
impl Vertex
{
    pub fn read<R: Read + ?Sized>(reader: &mut R, additional_vec4_count: usize, bone_index_size: IndexSize)
        -> Result<Self, LoadingError>
    {
        let mut head_bytes = vec![0u8; 4 * (3 + 3 + 2 + 4 * additional_vec4_count) + 1];
        reader.read_exact(&mut head_bytes)?;
        let position = Vec3::from_bytes(&head_bytes[..]);
        let normal = Vec3::from_bytes(&head_bytes[4*3..]);
        let uv = Vec2::from_bytes(&head_bytes[4 * (3 + 3)..]);
        let additional_vec4s = (0 .. additional_vec4_count)
            .map(|x| Vec4::from_bytes(&head_bytes[4 * (3 + 3 + 2 + 4 * x)..]))
            .collect();
        let deform_type = head_bytes[head_bytes.len() - 1];
        let deform = match deform_type {
            0 => WeightDeform::AffectSingleBone(bone_index_size.read_bytes_signed(reader)?),
            1 => {
                let mut bytes = vec![0u8; bone_index_size.bytesize() * 2 + 4];
                reader.read_exact(&mut bytes)?;
                let bones = bone_index_size.decode_bytes_signed2(&bytes);
                WeightDeform::AffectDoubleBones(
                    bones[0], bones[1],
                    f32::from_bytes(&bytes[bone_index_size.bytesize() * 2..])
                )
            },
            2 => {
                let mut bytes = vec![0u8; bone_index_size.bytesize() * 4 + 4 * 4];
                reader.read_exact(&mut bytes)?;
                WeightDeform::AffectFourBones(
                    bone_index_size.decode_bytes_signed4(&bytes), [
                        f32::from_bytes(&bytes[bone_index_size.bytesize()*4..]),
                        f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4..]),
                        f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4*2..]),
                        f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4*3..])
                    ]
                )
            },
            3 => {
                let mut bytes = vec![0u8; bone_index_size.bytesize() * 2 + 4 + 4 * 3 * 3];
                reader.read_exact(&mut bytes)?;
                let bones = bone_index_size.decode_bytes_signed2(&bytes);
                WeightDeform::Spherical {
                    b1: bones[0], b2: bones[1],
                    weight1: f32::from_bytes(&bytes[bone_index_size.bytesize()*2..]),
                    c: Vec3::from_bytes(&bytes[bone_index_size.bytesize()*2+4..]),
                    r0: Vec3::from_bytes(&bytes[bone_index_size.bytesize()*2+4+4*3..]),
                    r1: Vec3::from_bytes(&bytes[bone_index_size.bytesize()*2+4+4*3*2..])
                }
            },
            4 => {
                let mut bytes = vec![0u8; bone_index_size.bytesize() * 4 + 4 * 4];
                reader.read_exact(&mut bytes)?;
                WeightDeform::DualQuaternion(
                    bone_index_size.decode_bytes_signed4(&bytes),
                    [
                        f32::from_bytes(&bytes[bone_index_size.bytesize()*4..]),
                        f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4..]),
                        f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4*2..]),
                        f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4*3..])
                    ]
                )
            },
            v => return Err(LoadingError::UnknownDeformType(v))
        };
        let edge_scale = f32::read_value1(reader)?;

        Ok(Vertex { position, normal, uv, additional_vec4s, deform, edge_scale })
    }
}

#[derive(Debug)]
pub enum SurfaceSection { Byte(Vec<[u8; 3]>), Short(Vec<[u16; 3]>), Long(Vec<[u32; 3]>) }
impl SurfaceSection {
    pub fn read<R: Read + ?Sized>(reader: &mut R, vertex_index_size: IndexSize) -> IOResult<Self> {
        let count = i32::read_value1(reader)?;
        match vertex_index_size
        {
            IndexSize::Byte =>
            {
                let mut bytes = Vec::<[u8; 3]>::with_capacity((count / 3) as _);
                unsafe { bytes.set_len((count / 3) as _); }
                let sink = unsafe { std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as _, count as _) };

                reader.read_exact(sink).map(move |_| SurfaceSection::Byte(bytes))
            },
            IndexSize::Short =>
            {
                let mut bytes = Vec::<[u16; 3]>::with_capacity((count / 3) as _);
                unsafe { bytes.set_len((count / 3) as _); }
                let sink = unsafe { std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as _, (count * 2) as _) };

                reader.read_exact(sink).map(move |_| SurfaceSection::Short(bytes))
            },
            IndexSize::Long =>
            {
                let mut bytes = Vec::<[u32; 3]>::with_capacity((count / 3) as _);
                unsafe { bytes.set_len((count / 3) as _); }
                let sink = unsafe { std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as _, (count * 4) as _) };

                reader.read_exact(sink).map(move |_| SurfaceSection::Long(bytes))
            }
        }
    }
    pub fn len(&self) -> usize {
        match self {
            SurfaceSection::Byte(v) => v.len(),
            SurfaceSection::Short(v) => v.len(),
            SurfaceSection::Long(v) => v.len()
        }
    }
    pub fn is_empty(&self) -> bool { self.len() == 0 }
}
