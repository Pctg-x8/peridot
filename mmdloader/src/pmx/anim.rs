
use std::fmt::{Debug, Formatter, Result as FmtResult};
use std::io::Read;
use super::{IndexValue, Vec3, Vec4, GlobalizedStrings, Header, LoadingError, TypedReader};

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
impl std::fmt::Debug for BoneFlags
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(fmt, "BoneFlags(0x{:04x})", self.0)
    }
}
#[derive(Debug)]
pub enum TailPosition { Indexed(Option<IndexValue>), Raw(Vec3) }
#[derive(Debug)]
pub struct InheritBone { pub parent: Option<IndexValue>, pub influence: f32 }
#[derive(Debug)]
pub struct BoneFixedAxis { pub direction: Vec3 }
#[derive(Debug)]
pub struct BoneLocalCoordinate { pub x: Vec3, pub z: Vec3 }
#[derive(Debug)]
pub struct BoneExternalParent { pub parent: Option<IndexValue> }
#[derive(Debug)]
pub struct IKAngleLimit { pub min: Vec3, pub max: Vec3 }
#[derive(Debug)]
pub struct IKLinks { pub bone: Option<IndexValue>, pub limits: Option<IKAngleLimit> }
#[derive(Debug)]
pub struct BoneIK
{
    pub target: Option<IndexValue>, pub loopcount: i32, pub limit_radian: f32,
    pub links: Vec<IKLinks>
}
#[derive(Debug)]
pub struct Bone
{
    pub name: GlobalizedStrings, pub position: Vec3, pub parent_index: Option<IndexValue>,
    pub layer: i32, pub bone_flags: BoneFlags, pub tail_position: TailPosition,
    pub inherit_rotation: Option<InheritBone>, pub inherit_translation: Option<InheritBone>,
    pub fixed_axis: Option<BoneFixedAxis>,
    pub local_coordinate: Option<BoneLocalCoordinate>, pub external_parent: Option<BoneExternalParent>,
    pub ik: Option<BoneIK>
}
impl Bone
{
    pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError>
    {
        let name = header.string_reader.read_globalized(reader)?;
        let mut fixed_buf = vec![0u8; 4 * 3 + header.index_sizes.bone.bytesize() + 4 + 2];
        reader.read_exact(&mut fixed_buf)?;
        #[allow(clippy::cast_ptr_alignment)]
        let bone_flags = unsafe { BoneFlags(*(fixed_buf.as_ptr().add(fixed_buf.len() - 2) as *const _)) };
        
        let tail_position = if bone_flags.indexed_tail_position()
        {
            let mut buf = vec![0u8; header.index_sizes.bone.bytesize()];
            reader.read_exact(&mut buf)?;
            TailPosition::Indexed(IndexValue::from_bytes(&buf, header.index_sizes.bone))
        }
        else { TailPosition::Raw(Vec3::read_value1(reader)?) };

        let inherit_rotation = if bone_flags.inherit_rotation()
        {
            let mut buf = vec![0u8; header.index_sizes.bone.bytesize() + 4];
            reader.read_exact(&mut buf)?;
            InheritBone {
                parent: IndexValue::from_bytes(&buf, header.index_sizes.bone),
                influence: f32::from_bytes(&buf[header.index_sizes.bone.bytesize()..])
            }.into()
        }
        else { None };
        let inherit_translation = if bone_flags.inherit_translation() {
            let mut buf = vec![0u8; header.index_sizes.bone.bytesize() + 4];
            reader.read_exact(&mut buf)?;
            InheritBone {
                parent: IndexValue::from_bytes(&buf, header.index_sizes.bone),
                influence: f32::from_bytes(&buf[header.index_sizes.bone.bytesize()..])
            }.into()
        }
        else { None };

        let fixed_axis = if bone_flags.fixed_axis() {
            Some(BoneFixedAxis { direction: Vec3::read_value1(reader)? })
        }
        else { None };
        let local_coordinate = if bone_flags.local_coordinate() {
            let mut buf = [0u8; 4 * 3 * 2];
            reader.read_exact(&mut buf)?;
            BoneLocalCoordinate {
                x: Vec3::from_bytes(&buf), z: Vec3::from_bytes(&buf[4 * 3..])
            }.into()
        }
        else { None };
        let external_parent = if bone_flags.external_parent_deform() {
            Some(BoneExternalParent { parent: IndexValue::read1(reader, header.index_sizes.bone)? })
        }
        else { None };

        let ik = if bone_flags.use_ik()
        {
            let mut buf = vec![0u8; header.index_sizes.bone.bytesize() + 4 * 3];
            reader.read_exact(&mut buf)?;
            #[allow(clippy::cast_ptr_alignment)]
            let link_count = unsafe { *(buf.as_ptr().add(buf.len() - 4) as *const i32) };
            let mut links = Vec::with_capacity(link_count as _);
            for _ in 0 .. link_count {
                let mut buf = vec![0u8; header.index_sizes.bone.bytesize() + 1];
                reader.read_exact(&mut buf)?;
                let limits = if buf[buf.len() - 1] == 1 {
                    // ik angle limit
                    let mut buf = vec![0u8; 4 * 3 * 2];
                    reader.read_exact(&mut buf)?;
                    IKAngleLimit { min: Vec3::from_bytes(&buf), max: Vec3::from_bytes(&buf[4 * 3..]) }.into()
                }
                else { None };
                links.push(IKLinks { bone: IndexValue::from_bytes(&buf, header.index_sizes.bone), limits });
            }
            #[allow(clippy::cast_ptr_alignment)]
            BoneIK {
                target: IndexValue::from_bytes(&buf, header.index_sizes.bone),
                loopcount: unsafe { *(buf.as_ptr().add(header.index_sizes.bone.bytesize()) as *const _) },
                limit_radian: unsafe { *(buf.as_ptr().add(header.index_sizes.bone.bytesize() + 4) as *const _) },
                links
            }.into()
        }
        else { None };

        #[allow(clippy::cast_ptr_alignment)]
        Ok(Bone
        {
            name, position: Vec3::from_bytes(&fixed_buf),
            parent_index: IndexValue::from_bytes(&fixed_buf[4*3..], header.index_sizes.bone),
            layer: unsafe { *(fixed_buf.as_ptr().add(4*3 + header.index_sizes.bone.bytesize()) as *const _) },
            bone_flags, tail_position, inherit_rotation, inherit_translation, fixed_axis, local_coordinate,
            external_parent, ik
        })
    }
}

#[derive(Debug)]
pub enum MorphOffset
{
    Group(Option<IndexValue>, f32), Vertex(u32, Vec3), Bone(Option<IndexValue>, Vec3, Vec4),
    UV(u32, Vec4),
    Material
    {
        index: Option<IndexValue>, diffuse: Vec4, specular: Vec3, specular_str: f32,
        ambient: Vec3, edge_color: Vec4, edge_size: f32, texture_tint: Vec4, envmap_tint: Vec4, toon_tint: Vec4
    },
    Flip(Option<IndexValue>, f32), Impulse(Option<IndexValue>, bool, Vec3, Vec3)
}
impl MorphOffset
{
    fn read_as_group<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError>
    {
        let mut buf = vec![0u8; header.index_sizes.morph.bytesize() + 4];
        #[allow(clippy::cast_ptr_alignment)]
        reader.read_exact(&mut buf).map(move |_| MorphOffset::Group(
            IndexValue::from_bytes(&buf, header.index_sizes.morph),
            unsafe { *(buf.as_ptr().add(header.index_sizes.morph.bytesize()) as *const _) }
        )).map_err(From::from)
    }
    fn read_as_vertex<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError>
    {
        let mut buf = vec![0u8; header.index_sizes.vertex.bytesize() + 4 * 3];
        reader.read_exact(&mut buf).map(move |_| MorphOffset::Vertex(
            header.index_sizes.vertex.decode_bytes_unsigned(&buf),
            Vec3::from_bytes(&buf[header.index_sizes.vertex.bytesize()..])
        )).map_err(From::from)
    }
    fn read_as_bone<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError>
    {
        let mut buf = vec![0u8; header.index_sizes.bone.bytesize() + 4 * (3 + 4)];
        reader.read_exact(&mut buf).map(move |_| MorphOffset::Bone(
            IndexValue::from_bytes(&buf, header.index_sizes.bone),
            Vec3::from_bytes(&buf[header.index_sizes.bone.bytesize()..]),
            Vec4::from_bytes(&buf[header.index_sizes.bone.bytesize() + 4 * 3..])
        )).map_err(From::from)
    }
    fn read_as_uv<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError>
    {
        let mut buf = vec![0u8; header.index_sizes.vertex.bytesize() + 4 * 3];
        reader.read_exact(&mut buf).map(move |_| MorphOffset::UV(
            header.index_sizes.vertex.decode_bytes_unsigned(&buf),
            Vec4::from_bytes(&buf[header.index_sizes.vertex.bytesize()..])
        )).map_err(From::from)
    }
    fn read_as_material<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError>
    {
        let mut buf = vec![0u8; header.index_sizes.material.bytesize() + 1 + 4 * (4 + 3 + 1 + 3 + 4 + 1 + 4 * 3)];
        reader.read_exact(&mut buf).map(move |_| MorphOffset::Material
        {
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
        }).map_err(From::from)
    }
    fn read_as_flip<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
        let mut buf = vec![0u8; header.index_sizes.morph.bytesize() + 4];
        reader.read_exact(&mut buf).map(move |_| MorphOffset::Flip(
            IndexValue::from_bytes(&buf, header.index_sizes.morph),
            f32::from_bytes(&buf[header.index_sizes.morph.bytesize()..])
        )).map_err(From::from)
    }
    fn read_as_impulse<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
        let mut buf = vec![0u8; header.index_sizes.rigid_body.bytesize() + 1 + 4 * 3 * 2];
        reader.read_exact(&mut buf).map(move |_| MorphOffset::Impulse(
            IndexValue::from_bytes(&buf, header.index_sizes.rigid_body),
            buf[header.index_sizes.rigid_body.bytesize()] == 1,
            Vec3::from_bytes(&buf[header.index_sizes.rigid_body.bytesize() + 1..]),
            Vec3::from_bytes(&buf[header.index_sizes.rigid_body.bytesize() + 1 + 4 * 3..])
        )).map_err(From::from)
    }
}
#[derive(Debug)]
pub struct Morph {
    pub name: GlobalizedStrings, pub panel_type: i8, pub morph_type: i8, pub offsets: Vec<MorphOffset>
}
impl Morph {
    pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError> {
        let name = header.string_reader.read_globalized(reader)?;
        let mut buffer = [0i32; 2];
        reader.read_exact(unsafe
        {
            std::slice::from_raw_parts_mut((buffer.as_mut_ptr() as *mut u8).offset(2), 4 + 2)
        })?;
        let panel_type = unsafe { *(buffer.as_ptr() as *const i8).offset(2) };
        let morph_type = unsafe { *(buffer.as_ptr() as *const i8).offset(3) };
        let offset_count = buffer[1];
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

        Ok(Morph { name, panel_type, morph_type, offsets })
    }
}

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
#[derive(Debug)]
pub struct DisplayFrame
{
    pub name: GlobalizedStrings, pub is_special: bool, pub frames: Vec<Frame>
}
impl DisplayFrame
{
    pub fn read<R: Read>(reader: &mut R, header: &Header) -> Result<Self, LoadingError>
    {
        let name = header.string_reader.read_globalized(reader)?;
        let mut bytes = [0u8; 1 + 4];
        reader.read_exact(&mut bytes)?;
        let frame_count = unsafe { *(bytes.as_ptr().offset(1) as *const i32) };
        let mut frames = Vec::with_capacity(frame_count as _);
        for _ in 0 .. frame_count
        {
            let mut variant = [0u8];
            reader.read_exact(&mut variant)?;
            frames.push(match variant[0]
            {
                0 => IndexValue::read1(reader, header.index_sizes.bone).map(Frame::Bone)?,
                1 => IndexValue::read1(reader, header.index_sizes.morph).map(Frame::Morph)?,
                v => return Err(LoadingError::InvalidFrameData(v))
            });
        }

        Ok(DisplayFrame { name, is_special: bytes[0] == 1, frames })
    }
}
