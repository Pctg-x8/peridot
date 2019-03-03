// Peridot MMD(Polygon Model eXtended/Vocaloid(!?) Motion Data) Loader

extern crate encoding;

use std::fs::File;
use std::env::args;
use std::path::{Path, PathBuf};

mod vmd;

pub struct PolygonModelExtended {
    header: pmx::Header, vertices: Vec<pmx::Vertex>, surfaces: pmx::SurfaceSection,
    textures: Vec<PathBuf>, materials: Vec<pmx::Material>, bones: Vec<pmx::Bone>,
    morphs: Vec<pmx::Morph>, display_frames: Vec<pmx::DisplayFrame>, rigid_bodies: Vec<pmx::RigidBody>,
    joints: Vec<pmx::Joint>, softbodies: Vec<pmx::Softbody>
}
impl PolygonModelExtended {
    pub fn load<P: AsRef<Path> + ?Sized>(filepath: &P) -> Result<Self, pmx::LoadingError> {
        let mut bin = File::open(filepath)?;
        let header = pmx::Header::load(&mut bin)?;
        let vertices = pmx::read_array(&mut bin,
            |r| pmx::Vertex::read(r, header.additional_vec4_count as _, header.index_sizes.bone))?;
        let surfaces = pmx::SurfaceSection::read(&mut bin, header.index_sizes.vertex)?;
        let textures = pmx::read_array(&mut bin,
            |r| header.string_reader.read_string(r).map(PathBuf::from).map_err(From::from))?;
        let materials = pmx::read_array(&mut bin, |r| pmx::Material::read(r, &header))?;
        let bones = pmx::read_array(&mut bin, |r| pmx::Bone::read(r, &header))?;
        let morphs = pmx::read_array(&mut bin, |r| pmx::Morph::read(r, &header))?;
        let display_frames = pmx::read_array(&mut bin, |r| pmx::DisplayFrame::read(r, &header))?;
        let rigid_bodies = pmx::read_array(&mut bin, |r| pmx::RigidBody::read(r, &header))?;
        let joints = pmx::read_array(&mut bin, |r| pmx::Joint::read(r, &header))?;
        let softbodies = if header.version > 2.0 {
            pmx::read_array(&mut bin, |r| pmx::Softbody::read(r, &header))?
        }
        else { Vec::new() };
        return Ok(PolygonModelExtended {
            header, vertices, surfaces, textures, materials, bones, morphs, display_frames, rigid_bodies,
            joints, softbodies
        });
    }
}

pub mod pmx {
    use std::io::prelude::*;
    use std::io::{Result as IOResult, Error as IOError};
    use std::mem::transmute;
    use std::slice::{from_raw_parts_mut, from_raw_parts};

    macro_rules! ReadExactBytes {
        ($reader: expr, $n: expr) => {{
            let mut bytes = [0u8; $n];
            $reader.read_exact(&mut bytes[..]).map(|_| bytes)
        }};
        (var $reader:expr, $n: expr) => {{
            let mut bytes = Vec::with_capacity($n); unsafe { bytes.set_len($n); }
            $reader.read_exact(&mut bytes).map(|_| bytes)
        }}
    }

    pub fn read_array<R: Read + ?Sized, Fr: Fn(&mut R) -> Result<T, LoadingError>, T>(reader: &mut R, value_reader: Fr)
            -> Result<Vec<T>, LoadingError> {
        let count = i32::read_value1(reader)?;
        let mut values = Vec::with_capacity(count as _);
        for _ in 0 .. count { values.push(value_reader(reader)?); }
        return Ok(values);
    }

    pub trait TypedReader: Sized {
        fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>;
        fn from_bytes(bytes: &[u8]) -> Self;
    }
    impl TypedReader for i8 {
        fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute(bytes[0]) } }
        fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
            ReadExactBytes!(reader, 1).map(|v| unsafe { transmute(v[0]) })
        }
    }
    impl TypedReader for i16 {
        fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute::<_, &[Self]>(bytes)[0] } }
        fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
            ReadExactBytes!(reader, 2).map(|v| <Self as TypedReader>::from_bytes(&v))
        }
    }
    impl TypedReader for i32 {
        fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute::<_, &[Self]>(bytes)[0] } }
        fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
            ReadExactBytes!(reader, 4).map(|v| <Self as TypedReader>::from_bytes(&v))
        }
    }
    impl TypedReader for f32 {
        fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute::<_, &[Self]>(bytes)[0] } }
        fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
            ReadExactBytes!(reader, 4).map(|v| Self::from_bytes(&v[..]))
        }
    }
    #[repr(C)] #[derive(Clone, Debug)]
    pub struct Vec2(f32, f32);
    #[repr(C)] #[derive(Clone, Debug)]
    pub struct Vec3(f32, f32, f32);
    #[repr(C)] #[derive(Clone, Debug)]
    pub struct Vec4(f32, f32, f32, f32);
    impl TypedReader for Vec2 {
        fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute::<_, &[Self]>(bytes)[0].clone() } }
        fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
            ReadExactBytes!(reader, 4 * 2).map(|v| Self::from_bytes(&v[..]))
        }
    }
    impl TypedReader for Vec3 {
        fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute::<_, &[Self]>(bytes)[0].clone() } }
        fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
            ReadExactBytes!(reader, 4 * 3).map(|v| Self::from_bytes(&v[..]))
        }
    }
    impl TypedReader for Vec4 {
        fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute::<_, &[Self]>(bytes)[0].clone() } }
        fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
            ReadExactBytes!(reader, 4 * 4).map(|v| Self::from_bytes(&v[..]))
        }
    }

    pub trait StringReader {
        fn read_string(&self, reader: &mut Read) -> IOResult<String>;
        fn read_globalized(&self, reader: &mut Read) -> IOResult<GlobalizedStrings> {
            Ok(GlobalizedStrings { jp: self.read_string(reader)?, univ: self.read_string(reader)? })
        }
    }
    pub struct Utf8StringReader;
    pub struct Utf16LEStringReader;
    impl StringReader for Utf8StringReader {
        fn read_string(&self, reader: &mut Read) -> IOResult<String> {
            let byte_length = i32::read_value1(reader)?;
            let mut bytes = vec![0u8; byte_length as _];
            reader.read_exact(&mut bytes)?;
            return Ok(String::from_utf8(bytes).unwrap());
        }
    }
    impl StringReader for Utf16LEStringReader {
        fn read_string(&self, reader: &mut Read) -> IOResult<String> {
            let byte_length = i32::read_value1(reader)?;
            let mut bytes = vec![0u8; byte_length as _];
            reader.read_exact(unsafe { transmute(&mut bytes[..]) })?;
            let bytes_16 = unsafe { from_raw_parts(bytes.as_ptr() as _, byte_length as usize / 2) };
            return Ok(String::from_utf16_lossy(bytes_16));
        }
    }

    pub struct Header {
        pub version: f32, pub string_reader: Box<StringReader>, pub additional_vec4_count: u8,
        pub index_sizes: IndexSizes, pub model_name: GlobalizedStrings, pub comment: GlobalizedStrings
    }
    #[derive(Clone, Copy)]
    pub enum IndexSize { Byte, Short, Long }
    pub struct IndexSizes {
        pub vertex: IndexSize, pub texture: IndexSize, pub material: IndexSize, pub bone: IndexSize,
        pub morph: IndexSize, pub rigid_body: IndexSize
    }
    pub struct GlobalizedStrings { pub jp: String, pub univ: String }
    pub enum IndexValue { Byte(i8), Short(i16), Long(i32) }
    impl IndexValue {
        fn from_bytes(bytes: &[u8], size: IndexSize) -> Option<Self> {
            match size {
                IndexSize::Byte => unsafe {
                    let v = transmute(bytes[0]);
                    if v < 0 { None } else { IndexValue::Byte(v).into() }
                },
                IndexSize::Short => unsafe {
                    let v = transmute::<_, &[i16]>(bytes)[0];
                    if v < 0 { None } else { IndexValue::Short(v).into() }
                },
                IndexSize::Long => unsafe {
                    let v = transmute::<_, &[i32]>(bytes)[0];
                    if v < 0 { None } else { IndexValue::Long(v).into() }
                }
            }
        }
        pub fn as_index(&self) -> isize {
            match *self {
                IndexValue::Byte(v) => v as _,
                IndexValue::Short(v) => v as _,
                IndexValue::Long(v) => v as _
            }
        }
    }

    impl IndexSize {
        fn from_bytes_unsigned(&self, bytes: &[u8]) -> u32 {
            match self {
                &IndexSize::Byte => bytes[0] as _,
                &IndexSize::Short => unsafe { transmute::<_, &[u16]>(bytes)[0] as _ },
                &IndexSize::Long => unsafe { transmute::<_, &[u32]>(bytes)[0] }
            }
        }
        fn from_bytes_signed(&self, bytes: &[u8]) -> i32 {
            match self {
                &IndexSize::Byte => <i8 as TypedReader>::from_bytes(bytes) as i32,
                &IndexSize::Short => <i16 as TypedReader>::from_bytes(bytes) as i32,
                &IndexSize::Long => <i32 as TypedReader>::from_bytes(bytes)
            }
        }
        fn from_bytes_signed2(&self, bytes: &[u8]) -> [i32; 2] {
            [self.from_bytes_signed(bytes), self.from_bytes_signed(&bytes[self.bytesize()..])]
        }
        fn from_bytes_signed4(&self, bytes: &[u8]) -> [i32; 4] {
            [
                self.from_bytes_signed(bytes),
                self.from_bytes_signed(&bytes[self.bytesize()..]),
                self.from_bytes_signed(&bytes[self.bytesize()*2..]),
                self.from_bytes_signed(&bytes[self.bytesize()*3..])
            ]
        }
        fn bytesize(&self) -> usize {
            match self {
                &IndexSize::Byte => 1, &IndexSize::Short => 2, &IndexSize::Long => 4
            }
        }
    }

    #[derive(Debug)]
    pub struct Vertex {
        position: Vec3, normal: Vec3, uv: Vec2, additional_vec4s: Vec<Vec4>, deform: WeightDeform, edge_scale: f32
    }
    #[derive(Debug)]
    pub enum WeightDeform {
        AffectSingleBone(i32), AffectDoubleBones(i32, i32, f32), AffectFourBones([i32; 4], [f32; 4]),
        Spherical { b1: i32, b2: i32, weight1: f32, c: Vec3, r0: Vec3, r1: Vec3 },
        DualQuaternion([i32; 4], [f32; 4])
    }
    impl Vertex {
        pub fn read<R: Read + ?Sized>(reader: &mut R, additional_vec4_count: usize, bone_index_size: IndexSize)
                -> Result<Self, LoadingError> {
            let head_bytes = ReadExactBytes!(var reader, 4 * (3 + 3 + 2 + 4 * additional_vec4_count) + 1)?;
            let position = Vec3::from_bytes(&head_bytes[..]);
            let normal = Vec3::from_bytes(&head_bytes[4*3..]);
            let uv = Vec2::from_bytes(&head_bytes[4 * (3 + 3)..]);
            let additional_vec4s = (0 .. additional_vec4_count)
                .map(|x| Vec4::from_bytes(&head_bytes[4 * (3 + 3 + 2 + 4 * x)..]))
                .collect();
            let deform_type = head_bytes[head_bytes.len() - 1];
            let deform = match deform_type {
                0 => {
                    let bytes = ReadExactBytes!(var reader, bone_index_size.bytesize())?;
                    WeightDeform::AffectSingleBone(bone_index_size.from_bytes_signed(&bytes))
                },
                1 => {
                    let bytes = ReadExactBytes!(var reader, bone_index_size.bytesize() * 2 + 4)?;
                    let bones = bone_index_size.from_bytes_signed2(&bytes);
                    WeightDeform::AffectDoubleBones(
                        bones[0], bones[1],
                        f32::from_bytes(&bytes[bone_index_size.bytesize() * 2..])
                    )
                },
                2 => {
                    let bytes = ReadExactBytes!(var reader, bone_index_size.bytesize() * 4 + 4 * 4)?;
                    WeightDeform::AffectFourBones(
                        bone_index_size.from_bytes_signed4(&bytes), [
                            f32::from_bytes(&bytes[bone_index_size.bytesize()*4..]),
                            f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4..]),
                            f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4*2..]),
                            f32::from_bytes(&bytes[bone_index_size.bytesize()*4+4*3..])
                        ]
                    )
                },
                3 => {
                    let bytes = ReadExactBytes!(var reader, bone_index_size.bytesize() * 2 + 4 + 4 * 3 * 3)?;
                    let bones = bone_index_size.from_bytes_signed2(&bytes);
                    WeightDeform::Spherical {
                        b1: bones[0], b2: bones[1],
                        weight1: f32::from_bytes(&bytes[bone_index_size.bytesize()*2..]),
                        c: Vec3::from_bytes(&bytes[bone_index_size.bytesize()*2+4..]),
                        r0: Vec3::from_bytes(&bytes[bone_index_size.bytesize()*2+4+4*3..]),
                        r1: Vec3::from_bytes(&bytes[bone_index_size.bytesize()*2+4+4*3*2..])
                    }
                },
                4 => {
                    let bytes = ReadExactBytes!(var reader, bone_index_size.bytesize() * 4 + 4 * 4)?;
                    WeightDeform::DualQuaternion(
                        bone_index_size.from_bytes_signed4(&bytes),
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
            return Ok(Vertex { position, normal, uv, additional_vec4s, deform, edge_scale });
        }
    }

    #[derive(Debug)]
    pub enum SurfaceSection { Byte(Vec<[u8; 3]>), Short(Vec<[u16; 3]>), Long(Vec<[u32; 3]>) }
    impl SurfaceSection {
        pub fn read<R: Read + ?Sized>(reader: &mut R, vertex_index_size: IndexSize) -> IOResult<Self> {
            let count = i32::read_value1(reader)?;
            match vertex_index_size {
                IndexSize::Byte => {
                    let mut bytes = vec![[0u8; 3]; (count / 3) as _];
                    let mut sink = unsafe { from_raw_parts_mut(bytes.as_ptr() as _, count as _) };
                    reader.read_exact(sink)?;
                    return Ok(SurfaceSection::Byte(bytes));
                },
                IndexSize::Short => {
                    let mut bytes = vec![[0u16; 3]; (count / 3) as _];
                    let mut sink = unsafe { from_raw_parts_mut(bytes.as_ptr() as _, (count * 2) as _) };
                    reader.read_exact(sink)?;
                    return Ok(SurfaceSection::Short(bytes));
                },
                IndexSize::Long => {
                    let mut bytes = vec![[0u32; 3]; (count / 3) as _];
                    let mut sink = unsafe { from_raw_parts_mut(bytes.as_ptr() as _, (count * 4) as _) };
                    reader.read_exact(sink)?;
                    return Ok(SurfaceSection::Long(bytes));
                }
            }
        }
    }

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
            let fixed_bytes = ReadExactBytes!(var reader,
                4 * (4 + 3 + 1 + 3 + 4 + 1) + 3 + header.index_sizes.texture.bytesize() * 2)?;
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
            return Ok(Material {
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
            });
        }
    }

    pub struct BoneFlags(u16);
    impl BoneFlags {
        pub fn indexed_tail_position(&self) -> bool { (self.0 & 0x0001) != 0 }
        pub fn can_rotate(&self) -> bool { (self.0 & 0x0002) != 0 }
        pub fn can_translate(&self) -> bool { (self.0 & 0x0004) != 0 }
        pub fn is_visible(&self) -> bool { (self.0 & 0x0008) != 0 }
        pub fn enabled(&self) -> bool { (self.0 & 0x0010) != 0 }
        pub fn use_ik(&self) -> bool { (self.0 & 0x0020) != 0 }
        pub fn inherit_rotation(&self) -> bool { (self.0 & 0x0100) != 0 }
        pub fn inherit_translation(&self) -> bool { (self.0 & 0x0200) != 0 }
        pub fn fixed_axis(&self) -> bool { (self.0 & 0x0400) != 0 }
        pub fn local_coordinate(&self) -> bool { (self.0 & 0x0800) != 0 }
        pub fn physics_after_deform(&self) -> bool { (self.0 & 0x1000) != 0 }
        pub fn external_parent_deform(&self) -> bool { (self.0 & 0x2000) != 0 }
    }
    pub enum TailPosition { Indexed(Option<IndexValue>), Raw(Vec3) }
    pub struct InheritBone { pub parent: Option<IndexValue>, pub influence: f32 }
    pub struct BoneFixedAxis { pub direction: Vec3 }
    pub struct BoneLocalCoordinate { pub x: Vec3, pub z: Vec3 }
    pub struct BoneExternalParent { pub parent: Option<IndexValue> }
    pub struct IKAngleLimit { pub min: Vec3, pub max: Vec3 }
    pub struct IKLinks { pub bone: Option<IndexValue>, pub limits: Option<IKAngleLimit> }
    pub struct BoneIK {
        pub target: Option<IndexValue>, pub loopcount: i32, pub limit_radian: f32,
        pub links: Vec<IKLinks>
    }
    pub struct Bone {
        pub name: GlobalizedStrings, pub position: Vec3, pub parent_index: Option<IndexValue>,
        pub layer: i32, pub bone_flags: BoneFlags, pub tail_position: TailPosition,
        pub inherit_rotation: Option<InheritBone>, pub inherit_translation: Option<InheritBone>,
        pub fixed_axis: Option<BoneFixedAxis>,
        pub local_coordinate: Option<BoneLocalCoordinate>, pub external_parent: Option<BoneExternalParent>,
        pub ik: Option<BoneIK>
    }
    impl Bone {
        pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            let name = header.string_reader.read_globalized(reader)?;
            let fixed_buf = ReadExactBytes!(var reader, 4 * 3 + header.index_sizes.bone.bytesize() + 4 + 2)?;
            let bone_flags = BoneFlags(unsafe {
                *transmute::<_, *const _>(fixed_buf.as_ptr().offset((fixed_buf.len() - 2) as _))
            });
            
            let tail_position = if bone_flags.indexed_tail_position() {
                let buf = ReadExactBytes!(var reader, header.index_sizes.bone.bytesize())?;
                TailPosition::Indexed(IndexValue::from_bytes(&buf, header.index_sizes.bone))
            }
            else { TailPosition::Raw(Vec3::read_value1(reader)?) };

            let inherit_rotation = if bone_flags.inherit_rotation() {
                let buf = ReadExactBytes!(var reader, header.index_sizes.bone.bytesize() + 4)?;
                InheritBone {
                    parent: IndexValue::from_bytes(&buf, header.index_sizes.bone),
                    influence: f32::from_bytes(&buf[header.index_sizes.bone.bytesize()..])
                }.into()
            }
            else { None };
            let inherit_translation = if bone_flags.inherit_translation() {
                let buf = ReadExactBytes!(var reader, header.index_sizes.bone.bytesize() + 4)?;
                InheritBone {
                    parent: IndexValue::from_bytes(&buf, header.index_sizes.bone),
                    influence: f32::from_bytes(&buf[header.index_sizes.bone.bytesize()..])
                }.into()
            }
            else { None };
            let fixed_axis = if bone_flags.fixed_axis() {
                BoneFixedAxis { direction: Vec3::read_value1(reader)? }.into()
            }
            else { None };
            let local_coordinate = if bone_flags.local_coordinate() {
                let buf = ReadExactBytes!(reader, 4 * 3 * 2)?;
                BoneLocalCoordinate {
                    x: Vec3::from_bytes(&buf), z: Vec3::from_bytes(&buf[4 * 3..])
                }.into()
            }
            else { None };
            let external_parent = if bone_flags.external_parent_deform() {
                let buf = ReadExactBytes!(var reader, header.index_sizes.bone.bytesize())?;
                BoneExternalParent { parent: IndexValue::from_bytes(&buf, header.index_sizes.bone) }.into()
            }
            else { None };
            let ik = if bone_flags.use_ik() {
                let buf = ReadExactBytes!(var reader, header.index_sizes.bone.bytesize() + 4 * 3)?;
                let link_count = <i32 as TypedReader>::from_bytes(&buf[buf.len() - 4..]);
                let mut links = Vec::with_capacity(link_count as _);
                for _ in 0 .. link_count {
                    let buf = ReadExactBytes!(var reader, header.index_sizes.bone.bytesize() + 1)?;
                    let limits = if buf[buf.len() - 1] == 1 {
                        // ik angle limit
                        let buf = ReadExactBytes!(reader, 4 * 3 * 2)?;
                        IKAngleLimit { min: Vec3::from_bytes(&buf), max: Vec3::from_bytes(&buf[4 * 3..]) }.into()
                    }
                    else { None };
                    links.push(IKLinks { bone: IndexValue::from_bytes(&buf, header.index_sizes.bone), limits });
                }
                BoneIK {
                    target: IndexValue::from_bytes(&buf, header.index_sizes.bone),
                    loopcount: <i32 as TypedReader>::from_bytes(&buf[header.index_sizes.bone.bytesize()..]),
                    limit_radian: <f32 as TypedReader>::from_bytes(&buf[header.index_sizes.bone.bytesize() + 4..]),
                    links
                }.into()
            }
            else { None };

            return Ok(Bone {
                name, position: Vec3::from_bytes(&fixed_buf),
                parent_index: IndexValue::from_bytes(&fixed_buf[4*3..], header.index_sizes.bone),
                layer: <i32 as TypedReader>::from_bytes(&fixed_buf[4*3+header.index_sizes.bone.bytesize()..]),
                bone_flags, tail_position, inherit_rotation, inherit_translation, fixed_axis, local_coordinate,
                external_parent, ik
            });
        }
    }

    pub enum MorphOffset {
        Group(Option<IndexValue>, f32), Vertex(u32, Vec3), Bone(Option<IndexValue>, Vec3, Vec4),
        UV(u32, Vec4),
        Material {
            index: Option<IndexValue>, diffuse: Vec4, specular: Vec3, specular_str: f32,
            ambient: Vec3, edge_color: Vec4, edge_size: f32, texture_tint: Vec4, envmap_tint: Vec4, toon_tint: Vec4
        },
        Flip(Option<IndexValue>, f32), Impulse(Option<IndexValue>, bool, Vec3, Vec3)
    }
    impl MorphOffset {
        fn read_as_group<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            ReadExactBytes!(var reader, header.index_sizes.morph.bytesize() + 4).map(|m| MorphOffset::Group(
                IndexValue::from_bytes(&m, header.index_sizes.morph),
                f32::from_bytes(&m[header.index_sizes.morph.bytesize()..])
            )).map_err(From::from)
        }
        fn read_as_vertex<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            ReadExactBytes!(var reader, header.index_sizes.vertex.bytesize() + 4 * 3).map(|m| MorphOffset::Vertex(
                header.index_sizes.vertex.from_bytes_unsigned(&m),
                Vec3::from_bytes(&m[header.index_sizes.vertex.bytesize()..])
            )).map_err(From::from)
        }
        fn read_as_bone<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            ReadExactBytes!(var reader, header.index_sizes.bone.bytesize() + 4 * (3 + 4)).map(|m| MorphOffset::Bone(
                IndexValue::from_bytes(&m, header.index_sizes.bone),
                Vec3::from_bytes(&m[header.index_sizes.bone.bytesize()..]),
                Vec4::from_bytes(&m[header.index_sizes.bone.bytesize() + 4 * 3..]),
            )).map_err(From::from)
        }
        fn read_as_uv<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            ReadExactBytes!(var reader, header.index_sizes.vertex.bytesize() + 4 * 4).map(|m| MorphOffset::UV(
                header.index_sizes.vertex.from_bytes_unsigned(&m),
                Vec4::from_bytes(&m[header.index_sizes.vertex.bytesize()..])
            )).map_err(From::from)
        }
        fn read_as_material<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            let buf = ReadExactBytes!(var reader,
                header.index_sizes.material.bytesize() + 1 + 4 * (4 + 3 + 1 + 3 + 4 + 1 + 4 * 3))?;
            Ok(MorphOffset::Material {
                index: IndexValue::from_bytes(&buf, header.index_sizes.material),
                diffuse: Vec4::from_bytes(&buf[header.index_sizes.material.bytesize()+1..]),
                specular: Vec3::from_bytes(&buf[header.index_sizes.material.bytesize()+1+4*4..]),
                specular_str: f32::from_bytes(&buf[header.index_sizes.material.bytesize()+1+4*(4+3)..]),
                ambient: Vec3::from_bytes(&buf[header.index_sizes.material.bytesize()+1+4*(4+3+1)..]),
                edge_color: Vec4::from_bytes(&buf[header.index_sizes.material.bytesize()+1+4*(4+3+1+3)..]),
                edge_size: f32::from_bytes(&buf[header.index_sizes.material.bytesize()+1+4*(4+3+1+3+4)..]),
                texture_tint: Vec4::from_bytes(&buf[header.index_sizes.material.bytesize()+1+4*(4+3+1+3+4+1)..]),
                envmap_tint: Vec4::from_bytes(&buf[header.index_sizes.material.bytesize()+1+4*(4+3+1+3+4+1+4)..]),
                toon_tint: Vec4::from_bytes(&buf[header.index_sizes.material.bytesize()+1+4*(4+3+1+3+4+1+4+4)..])
            })
        }
        fn read_as_flip<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            ReadExactBytes!(var reader, header.index_sizes.morph.bytesize() + 4).map(|m| MorphOffset::Flip(
                IndexValue::from_bytes(&m, header.index_sizes.morph),
                f32::from_bytes(&m[header.index_sizes.morph.bytesize()..])
            )).map_err(From::from)
        }
        fn read_as_impulse<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            let buf = ReadExactBytes!(var reader, header.index_sizes.rigid_body.bytesize() + 1 + 4 * 3 * 2)?;
            return Ok(MorphOffset::Impulse(
                IndexValue::from_bytes(&buf, header.index_sizes.rigid_body),
                buf[header.index_sizes.rigid_body.bytesize()] == 1,
                Vec3::from_bytes(&buf[header.index_sizes.rigid_body.bytesize() + 1..]),
                Vec3::from_bytes(&buf[header.index_sizes.rigid_body.bytesize() + 1 + 4 * 3..])
            ));
        }
    }
    pub struct Morph {
        pub name: GlobalizedStrings, pub panel_type: i8, pub morph_type: i8, pub offsets: Vec<MorphOffset>
    }
    impl Morph {
        pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            let name = header.string_reader.read_globalized(reader)?;
            let fixed_bytes = ReadExactBytes!(reader, 2 + 4)?;
            let morph_type = fixed_bytes[1] as _;
            let offset_count = <i32 as TypedReader>::from_bytes(&fixed_bytes[2..]);
            let mut offsets = Vec::with_capacity(offset_count as _);
            for _ in 0 .. offset_count {
                offsets.push(match morph_type {
                    0 => MorphOffset::read_as_group(reader, header)?,
                    1 => MorphOffset::read_as_vertex(reader, header)?,
                    2 => MorphOffset::read_as_bone(reader, header)?,
                    3 ... 7 => MorphOffset::read_as_uv(reader, header)?,
                    8 => MorphOffset::read_as_material(reader, header)?,
                    9 => MorphOffset::read_as_flip(reader, header)?,
                    10 => MorphOffset::read_as_impulse(reader, header)?,
                    v => return Err(LoadingError::UnknownMorphType(v))
                });
            }

            return Ok(Morph { name, panel_type: unsafe { transmute(fixed_bytes[0]) }, morph_type, offsets });
        }
    }

    use std::fmt::{Debug, Formatter, Result as FmtResult};
    pub enum Frame { Bone(Option<IndexValue>), Morph(Option<IndexValue>) }
    impl Debug for Frame {
        fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
            match *self {
                Frame::Bone(Some(ref v)) => write!(fmt, "Bone[{}]", v.as_index()),
                Frame::Morph(Some(ref v)) => write!(fmt, "Morph[{}]", v.as_index()),
                Frame::Bone(None) => write!(fmt, "Bone[]"),
                Frame::Morph(None) => write!(fmt, "Morph[]")
            }
        }
    }
    pub struct DisplayFrame {
        pub name: GlobalizedStrings, pub is_special: bool, pub frames: Vec<Frame>
    }
    impl DisplayFrame {
        pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            let name = header.string_reader.read_globalized(reader)?;
            let bytes = ReadExactBytes!(reader, 1 + 4)?;
            let frame_count = <i32 as TypedReader>::from_bytes(&bytes[1..]);
            let mut frames = Vec::with_capacity(frame_count as _);
            for _ in 0 .. frame_count {
                let variant = ReadExactBytes!(reader, 1)?[0];
                frames.push(match variant {
                    0 => ReadExactBytes!(var reader, header.index_sizes.bone.bytesize())
                        .map(|b| Frame::Bone(IndexValue::from_bytes(&b, header.index_sizes.bone)))?,
                    1 => ReadExactBytes!(var reader, header.index_sizes.morph.bytesize())
                        .map(|b| Frame::Morph(IndexValue::from_bytes(&b, header.index_sizes.morph)))?,
                    v => return Err(LoadingError::InvalidFrameData(v))
                });
            }

            return Ok(DisplayFrame { name, is_special: bytes[0] == 1, frames });
        }
    }
    
    pub enum ShapeType { Sphere, Box, Capsule }
    pub enum PhysicsMode { FollowBone, Physics, PhysicsWithBone }
    pub struct RigidBody {
        pub name: GlobalizedStrings, pub related_bone_index: Option<IndexValue>, pub group_id: i8,
        pub non_collision_mask: i16, pub shape: ShapeType, pub shape_size: Vec3, pub shape_position: Vec3,
        pub shape_rotation: Vec3, pub mass: f32, pub move_atten: f32, pub rotation_damp: f32, pub repulsion: f32,
        pub friction_force: f32, pub mode: PhysicsMode
    }
    impl RigidBody {
        pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            let name = header.string_reader.read_globalized(reader)?;
            let bytes = ReadExactBytes!(var reader,
                header.index_sizes.bone.bytesize() + 1 + 2 + 1 + 4 * (3 * 3 + 1 * 5) + 1)?;
            let shape = match bytes[header.index_sizes.bone.bytesize() + 1 + 2] {
                0 => ShapeType::Sphere, 1 => ShapeType::Box, 2 => ShapeType::Capsule,
                v => return Err(LoadingError::UnknownShapeType(v))
            };
            let mode = match bytes[bytes.len() - 1] {
                0 => PhysicsMode::FollowBone, 1 => PhysicsMode::Physics, 2 => PhysicsMode::PhysicsWithBone,
                v => return Err(LoadingError::UnknownPhysicsMode(v))
            };

            return Ok(RigidBody {
                name, related_bone_index: IndexValue::from_bytes(&bytes, header.index_sizes.bone),
                group_id: <i8 as TypedReader>::from_bytes(&bytes[header.index_sizes.bone.bytesize()..]),
                non_collision_mask: <i16 as TypedReader>::from_bytes(&bytes[header.index_sizes.bone.bytesize() + 1..]),
                shape,
                shape_size: Vec3::from_bytes(&bytes[header.index_sizes.bone.bytesize() + 1 + 2 + 1..]),
                shape_position: Vec3::from_bytes(&bytes[header.index_sizes.bone.bytesize() + 1 + 2 + 1 + 4 * 3..]),
                shape_rotation: Vec3::from_bytes(&bytes[header.index_sizes.bone.bytesize() + 1 + 2 + 1 + 4 * 3 * 2..]),
                mass: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3..]),
                move_atten: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3+4*1..]),
                rotation_damp: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3+4*2..]),
                repulsion: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3+4*3..]),
                friction_force: f32::from_bytes(&bytes[header.index_sizes.bone.bytesize()+1+2+1+4*3*3+4*4..]), mode
            });
        }
    }

    pub enum JointType { Spring6, SixDof, P2P, ConeTwist, Slider, Hinge }
    pub struct Joint {
        pub name: GlobalizedStrings, pub _type: JointType,
        pub rigid_body_indices: (Option<IndexValue>, Option<IndexValue>),
        pub position: Vec3, pub rotation: Vec3, pub position_min: Vec3, pub position_max: Vec3,
        pub rotation_min: Vec3, pub rotation_max: Vec3, pub position_spring: Vec3,
        pub rotation_spring: Vec3
    }
    impl Joint {
        pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            let name = header.string_reader.read_globalized(reader)?;
            let bytes = ReadExactBytes!(var reader, header.index_sizes.rigid_body.bytesize() * 2 + 1 + 4 * 3 * 8)?;
            let _type = match bytes[0] {
                0 => JointType::Spring6, 1 => JointType::SixDof, 2 => JointType::P2P, 3 => JointType::ConeTwist,
                4 => JointType::Slider, 5 => JointType::Hinge, v => return Err(LoadingError::UnknonwnJointType(v))
            };
            let rigid_body_indices = (IndexValue::from_bytes(&bytes[1..], header.index_sizes.rigid_body),
                IndexValue::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize()..],
                    header.index_sizes.rigid_body));
            return Ok(Joint {
                name, _type, rigid_body_indices,
                position: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 ..]),
                rotation: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 ..]),
                position_min: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 2 ..]),
                position_max: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 3 ..]),
                rotation_min: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 4 ..]),
                rotation_max: Vec3::from_bytes(&bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 5 ..]),
                position_spring: Vec3::from_bytes(
                    &bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 6 ..]),
                rotation_spring: Vec3::from_bytes(
                    &bytes[1 + header.index_sizes.rigid_body.bytesize() * 2 + 4 * 3 * 7 ..])
            });
        }
    }

    pub enum SoftbodyShapeType { TriMesh, Rope }
    pub struct SoftbodyFlags(u8);
    impl SoftbodyFlags {
        pub fn b_link(&self) -> bool { (self.0 & 0x01) != 0 }
        pub fn cluster_creation(&self) -> bool { (self.0 & 0x02) != 0 }
        pub fn link_crossing(&self) -> bool { (self.0 & 0x04) != 0 }
    }
    pub enum AeroDynamicsModel { Point, TwoSidedV, OneSidedV, TwoSidedF, OneSidedF }
    pub struct AnchorRigidBody { pub index: Option<IndexValue>, pub vindex: u32, pub near_mode: i8 }
    pub struct VertexPin(u32);
    pub struct Softbody {
        pub name: GlobalizedStrings, pub shape: SoftbodyShapeType, pub mindex: Option<IndexValue>,
        pub group: i8, pub non_collision_mask: i16, pub flags: SoftbodyFlags, pub b_link_create_distance: i32,
        pub cluster_count: i32, pub total_mass: f32, pub collision_margin: f32, pub aerodynamics_model: AeroDynamicsModel,
        pub velocities_correction_factor: f32, pub dampling_coefficient: f32, pub drag_coefficient: f32,
        pub lift_coefficient: f32, pub pressure_coefficient: f32, pub volume_conversation_coefficient: f32,
        pub dynamic_friction_coeffecient: f32, pub pose_matching_coefficient: f32, pub rigid_contracts_hardness: f32,
        pub kinetic_contracts_hardness: f32, pub soft_contracts_hardness: f32, pub anchors_hardness: f32,
        pub soft_vs_rigid_hardness: f32, pub soft_vs_kinetic_hardness: f32, pub soft_vs_soft_hardness: f32,
        pub soft_vs_soft_impulse_split: f32, pub soft_vs_rigid_impulse_split: f32, pub soft_vs_kinetic_impulse_split: f32,
        pub velocities_solver_iterations: i32, pub positions_solver_iterations: i32, pub drift_solver_iterations: i32,
        pub cluster_solver_iteratons: i32, pub linear_stiffness_coefficient: i32,
        pub area_angular_stiffness_coefficient: i32, pub volume_stiffness_coefficient: i32,
        pub anchor_rigid_bodies: Vec<AnchorRigidBody>, pub vertex_pins: Vec<VertexPin>
    }
    impl Softbody {
        pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
            let name = header.string_reader.read_globalized(reader)?;
            let bytes = ReadExactBytes!(var reader, header.index_sizes.material.bytesize() + 1 * 3 + 2 + 4 * 29)?;
            let shape = match bytes[0] {
                0 => SoftbodyShapeType::TriMesh, 1 => SoftbodyShapeType::Rope,
                v => return Err(LoadingError::UnknownShapeType(v))
            };
            let aerodynamics_model = match bytes[5 + 4 * 4 + header.index_sizes.material.bytesize()] {
                0 => AeroDynamicsModel::Point, 1 => AeroDynamicsModel::TwoSidedV, 2 => AeroDynamicsModel::OneSidedV,
                3 => AeroDynamicsModel::TwoSidedF, 4 => AeroDynamicsModel::OneSidedF,
                v => return Err(LoadingError::UnknownAerodynamicsMode(v))
            };
            let mindex = IndexValue::from_bytes(&bytes[1..], header.index_sizes.material);
            let group = <i8 as TypedReader>::from_bytes(&bytes[1 + header.index_sizes.material.bytesize()..]);
            let non_collision_mask = i16::from_bytes(&bytes[1 + header.index_sizes.material.bytesize() + 1..]);
            let flags = SoftbodyFlags(bytes[1 + header.index_sizes.material.bytesize() + 1 + 2]);
            let anchor_rigid_body_count = <i32 as TypedReader>::from_bytes(&bytes[bytes.len() - 4..]);
            let mut anchor_rigid_bodies = Vec::with_capacity(anchor_rigid_body_count as _);
            println!("rigid body count: {}", anchor_rigid_body_count);
            for _ in 0 .. anchor_rigid_body_count {
                let buf = ReadExactBytes!(var reader,
                    (header.index_sizes.rigid_body.bytesize() + header.index_sizes.vertex.bytesize() + 1) as _)?;
                anchor_rigid_bodies.push(AnchorRigidBody {
                    index: IndexValue::from_bytes(&buf, header.index_sizes.rigid_body),
                    vindex: header.index_sizes.rigid_body.from_bytes_unsigned(
                        &buf[header.index_sizes.rigid_body.bytesize()..]),
                    near_mode: unsafe { transmute(buf[buf.len() - 1]) }
                });
            }
            let vertex_pin_count = i32::read_value1(reader)?;
            let vertex_pins = match header.index_sizes.vertex {
                IndexSize::Byte => ReadExactBytes!(var reader, vertex_pin_count as _)?
                    .into_iter().map(|v| VertexPin(v as _)).collect(),
                IndexSize::Short => {
                    let mut elements = vec![0u16; vertex_pin_count as _];
                    let sink = unsafe { from_raw_parts_mut(elements.as_mut_ptr() as _, (vertex_pin_count * 2) as _) };
                    reader.read_exact(sink)?;
                    elements.into_iter().map(|v| VertexPin(v as _)).collect()
                },
                IndexSize::Long => {
                    let mut elements = vec![0u32; vertex_pin_count as _];
                    let sink = unsafe { from_raw_parts_mut(elements.as_mut_ptr() as _, (vertex_pin_count * 4) as _) };
                    reader.read_exact(sink)?;
                    elements.into_iter().map(VertexPin).collect()
                }
            };

            return Ok(Softbody {
                name, shape, mindex, group, non_collision_mask, flags,
                b_link_create_distance: <i32 as TypedReader>::from_bytes(&bytes),
                cluster_count: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4..]),
                total_mass: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*2..]),
                collision_margin: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*2..]),
                aerodynamics_model,
                velocities_correction_factor: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*4..]),
                dampling_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*5..]),
                drag_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*6..]),
                lift_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*7..]),
                pressure_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*8..]),
                volume_conversation_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*9..]),
                dynamic_friction_coeffecient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*10..]),
                pose_matching_coefficient: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*11..]),
                rigid_contracts_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*12..]),
                kinetic_contracts_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*13..]),
                soft_contracts_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*14..]),
                anchors_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*15..]),
                soft_vs_rigid_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*16..]),
                soft_vs_kinetic_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*17..]),
                soft_vs_soft_hardness: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*18..]),
                soft_vs_rigid_impulse_split: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*19..]),
                soft_vs_kinetic_impulse_split: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*20..]),
                soft_vs_soft_impulse_split: f32::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*21..]),
                velocities_solver_iterations: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*22..]),
                positions_solver_iterations: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*23..]),
                drift_solver_iterations: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*24..]),
                cluster_solver_iteratons: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*25..]),
                linear_stiffness_coefficient: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*26..]),
                area_angular_stiffness_coefficient: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*27..]),
                volume_stiffness_coefficient: <i32 as TypedReader>::from_bytes(&bytes[header.index_sizes.material.bytesize() + 5 + 4*28..]),
                anchor_rigid_bodies, vertex_pins
            });
        }
    }

    #[derive(Debug)]
    pub enum LoadingError {
        IO(IOError), SignatureMismatch, MissingGlobals(u8), UnknownStringEncoding(u8), UnknownDeformType(u8),
        UnknownToonReference(u8), UnknownEnvBlendMode(u8), UnknownMorphType(i8), InvalidFrameData(u8),
        UnknownShapeType(u8), UnknownPhysicsMode(u8), UnknonwnJointType(u8), UnknownAerodynamicsMode(u8)
    }
    impl From<IOError> for LoadingError { fn from(v: IOError) -> Self { LoadingError::IO(v) } }

    #[repr(C)] struct Globals {
        string_encoding_type: u8, additional_vec4_count: u8,
        vertex_index_size: u8, texture_index_size: u8, material_index_size: u8,
        bone_index_size: u8, morph_index_size: u8, rigid_body_index_size: u8
    }
    impl Header {
        pub fn load<R: Read>(reader: &mut R) -> Result<Self, LoadingError> {
            let fixed_headers = ReadExactBytes!(reader, 9)?;
            if &fixed_headers[..4] != &[0x50, 0x4d, 0x58, 0x20] {
                return Err(LoadingError::SignatureMismatch);
            }
            let version = unsafe { *transmute::<_, *const f32>(fixed_headers.as_ptr()).offset(1) };
            let globals_count = fixed_headers[8];
            if globals_count < 8 { return Err(LoadingError::MissingGlobals(globals_count)); }

            // Globals //
            let globals_b = ReadExactBytes!(var reader, globals_count as _)?;
            let globals = unsafe { &*transmute::<_, *const Globals>(globals_b.as_ptr()) };
            let string_reader: Box<StringReader> = match globals.string_encoding_type {
                0 => Box::new(Utf16LEStringReader) as _, 1 => Box::new(Utf8StringReader) as _,
                v => return Err(LoadingError::UnknownStringEncoding(v))
            };
            let index_sizes = IndexSizes {
                vertex: IndexSize::from(globals.vertex_index_size),
                texture: IndexSize::from(globals.texture_index_size),
                material: IndexSize::from(globals.material_index_size),
                bone: IndexSize::from(globals.bone_index_size),
                morph: IndexSize::from(globals.morph_index_size),
                rigid_body: IndexSize::from(globals.rigid_body_index_size)
            };

            let model_name = string_reader.read_globalized(reader)?;
            let comment = string_reader.read_globalized(reader)?;
            return Ok(Header {
                version, string_reader, index_sizes, model_name, comment,
                additional_vec4_count: globals.additional_vec4_count
            });
        }
    }
    impl From<u8> for IndexSize {
        fn from(v: u8) -> Self {
            match v {
                1 => IndexSize::Byte, 2 => IndexSize::Short, 4 => IndexSize::Long,
                v => panic!("Unable to convert into IndexSize: {}", v)
            }
        }
    }
}

fn main() {
    let filename = args().skip(1).next().map(PathBuf::from).expect("Filename");
    println!("Loading {}...", filename.display());
    // let mut vmdfile = File::open(filename).unwrap();
    // let vmd = vmd::MotionData::read(&mut vmdfile).unwrap();
    // println!("VMD Name: {:?}", vmd.name);
    let model = PolygonModelExtended::load(&filename).unwrap();
    println!("<<Polygon Model Extended v{}>>", model.header.version);
    println!("{}/{}", model.header.model_name.jp, model.header.model_name.univ);
    println!("*** Comments ***");
    println!("{}", model.header.comment.jp);
    println!("Vertices:");
    println!("{:?}", model.vertices);
    println!("SurfaceSection:");
    println!("{:?}", model.surfaces);
    println!("Textures:");
    for t in &model.textures { println!("* {}", t.display()); }
    println!("Materials:");
    for m in &model.materials {
        let mut flags_displayed = Vec::new();
        if (m.drawing_flags & 0x01) != 0 { flags_displayed.push("No-cull"); }
        if (m.drawing_flags & 0x02) != 0 { flags_displayed.push("GroundShadow"); }
        if (m.drawing_flags & 0x04) != 0 { flags_displayed.push("DrawShadow"); }
        if (m.drawing_flags & 0x08) != 0 { flags_displayed.push("ReceiveShadow"); }
        if (m.drawing_flags & 0x10) != 0 { flags_displayed.push("HasEdge"); }
        if (m.drawing_flags & 0x20) != 0 { flags_displayed.push("VertexColor"); }
        if (m.drawing_flags & 0x40) != 0 { flags_displayed.push("PointDrawing"); }
        if (m.drawing_flags & 0x80) != 0 { flags_displayed.push("LineDrawing"); }

        println!("* {}/{} [{}]", m.name.jp, m.name.univ, flags_displayed.join("/"));
        println!("  diffuse={:?} specular={:?}(str={})", m.diffuse_color, m.specular_color, m.specular_strength);
        println!("  ambient={:?}", m.ambient_color);
        if let Some(ref t) = m.texture_index {
            println!("  texture={}({})", t.as_index(), model.textures[t.as_index() as usize].display());
        }
        if let Some(ref t) = m.envmap_texture_index {
            println!("  envmap={}({}) blendmode={:?}", t.as_index(), model.textures[t.as_index() as usize].display(), m.envmap_blend_mode);
        }
        println!("  EdgeRendering: color={:?} scale={}", m.edge_color, m.edge_scale);
        match m.toon_reference {
            pmx::ToonReference::Internal(n) => println!("  ToonRef: Internal index={}", n),
            pmx::ToonReference::Texture(Some(ref t)) =>
                println!("  ToonRef: Texture #{}({})", t.as_index(), model.textures[t.as_index() as usize].display()),
            pmx::ToonReference::Texture(None) => println!("  ToonRef: None")
        }
        println!("  surface affection count={}", m.surface_affects);
        println!("  metadata: {}", m.meta);
    }
    println!("Bones: ");
    for (n, b) in model.bones.iter().enumerate() {
        println!("* {}: {}/{}", n, b.name.jp, b.name.univ);
        if let Some(ref ik) = b.ik {
            if let Some(ref v) = ik.target {
                println!("  BoneIK: Target={} loopcount={} limit_radian={}", v.as_index(), ik.loopcount, ik.limit_radian);
            }
            else {
                println!("  BoneIK: [NoTarget] loopcount={} limit_radian={}", ik.loopcount, ik.limit_radian);
            }
        }
    }
    println!("Morphs: ");
    for (n, m) in model.morphs.iter().enumerate() {
        println!("* {}: {}/{}", n, m.name.jp, m.name.univ);
    }
    println!("Display-Frames: ");
    for df in &model.display_frames {
        println!("* {}/{}", df.name.jp, df.name.univ);
        println!("  Frames: {}", df.frames.iter().map(|x| format!("{:?}", x)).collect::<Vec<_>>().join(" "));
    }
    /*println!("Rigid Bodies: ");
    for rb in &model.rigid_bodies {
        println!("* {}/{}", rb.name.jp, rb.name.univ);
    }
    println!("Joints: ");
    for j in &model.joints {
        println!("* {}/{}", j.name.jp, j.name.univ);
    }
    println!("SoftBodies: ");
    for sb in &model.softbodies {
        println!("* {}/{}", sb.name.jp, sb.name.univ);
    }*/
    /*println!("Loaded Model: {} (PMX Version {})", model.header.model_name.jp, model.header.version);
    println!("---Comments---");
    println!("{}", model.header.comment.jp);*/
}
