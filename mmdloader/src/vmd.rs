//! Vocaloid Motion Data

use std::io::prelude::*;
use std::io::Error as IOError;
use encoding::codec::japanese::Windows31JEncoding;
use encoding::{Encoding, DecoderTrap};
use std::borrow::Cow;
use std::mem::size_of;
use super::pmx::TypedReader;
use std::collections::{HashMap, BinaryHeap};
use std::cmp::{PartialOrd, Ord, Ordering};
use peridot_math::{Vector3, Quaternion};

pub struct BoneKeyframeData
{
    pub seconds: f32,
    pub position: Option<Vector3<f32>>, pub rot_quaternion: Option<Quaternion<f32>>,
    pub interpolation_bytes: InterpolationData
}
impl PartialEq for BoneKeyframeData
{
    fn eq(&self, other: &Self) -> bool { self.seconds.eq(&other.seconds) }
}
impl PartialOrd for BoneKeyframeData
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> { self.seconds.partial_cmp(&other.seconds) }
}
impl Eq for BoneKeyframeData {}
impl Ord for BoneKeyframeData
{
    fn cmp(&self, other: &Self) -> Ordering
    {
        self.seconds.partial_cmp(&other.seconds).expect("NaN is invalid for timecode")
    }
}
pub type SortedVec<T> = Vec<T>;
#[derive(Debug)]
pub struct BonePosture
{
    /// Position of bone
    pub pos: Option<Vector3<f32>>,
    /// Rotation of bone in quaternion
    pub qrot: Option<Quaternion<f32>>
}
pub struct BoneMotionController
{
    keyframes: HashMap<String, SortedVec<BoneKeyframeData>>
}
impl BoneMotionController
{
    pub fn new(raw_data: &[BoneMotionData], framerate: f32) -> Self
    {
        let mut keyframes = HashMap::new();
        for rd in raw_data
        {
            let position = if !rd.has_position_data() { None } else {
                Vector3(rd.x, rd.y, rd.z).into()
            };
            let rotation = if !rd.has_rotation_data() { None } else {
                Quaternion(rd.quaternion_x, rd.quaternion_y, rd.quaternion_z, rd.quaternion_w).into()
            };

            keyframes.entry(&rd.name_bytes).or_insert_with(BinaryHeap::new)
                .push(BoneKeyframeData
                {
                    seconds: rd.frame_number as f32 / framerate, position, rot_quaternion: rotation,
                    interpolation_bytes: rd.interpolation_bytes.clone()
                });
        }
        
        BoneMotionController
        {
            keyframes: keyframes.into_iter().map(|(k, v)|
            {
                #[allow(clippy::or_fun_call)]
                let key_length = k.iter().position(|&b| b == 0).unwrap_or(k.len());
                (Windows31JEncoding.decode(&k[..key_length], DecoderTrap::Replace).expect("Invalid BoneName"),
                    v.into_sorted_vec())
            }).collect()
        }
    }

    pub fn get_bone_postures(&self, for_second: f32) -> HashMap<&str, BonePosture>
    {
        self.keyframes.iter()
            .filter_map(|(k, v)| Self::interpolate_keyframes(v, for_second).map(move |p| (k as &str, p)))
            .collect()
    }
    /// Returns `None` if the bone is not in timeline
    pub fn get_posture_for_bone(&self, bone_name: &str, for_second: f32) -> Option<BonePosture>
    {
        self.keyframes.get(bone_name).and_then(|v| Self::interpolate_keyframes(v, for_second))
    }

    fn interpolate_keyframes(keyframes: &[BoneKeyframeData], for_second: f32) -> Option<BonePosture>
    {
        match keyframes.binary_search_by(|p| p.seconds.partial_cmp(&for_second).expect("NaN is invalid"))
        {
            // Found on exact keyframe
            Ok(kfx) => Some(BonePosture
            {
                pos: keyframes[kfx].position.clone(), qrot: keyframes[kfx].rot_quaternion.clone()
            }),
            // Not Found and the keyframe has no previous state, which means there is no morphs.
            Err(0) => None,
            // Not Found but kfxplus points next element from current second
            Err(kfxplus) => if kfxplus >= keyframes.len() { None } else {
                let begin_kf = &keyframes[kfxplus - 1];
                // let end_kf = &keyframes[kfxplus];
                
                // TODO: interpolate values here
                Some(BonePosture { pos: begin_kf.position.clone(), qrot: begin_kf.rot_quaternion.clone() })
            }
        }
    }
}

pub struct MotionData
{
    name_bytes: Vec<u8>,
    bone_motions: Vec<BoneMotionData>, face_motions: Vec<FaceMotionData>, camera_motions: Vec<CameraMotionData>
}
#[repr(C)] #[derive(Clone)] pub struct InterpolationData([i8; 64]);
#[repr(C)] #[derive(Debug)]
pub struct BoneMotionData
{
    name_bytes: [u8; 15], pub frame_number: u32,
    pub x: f32, pub y: f32, pub z: f32,
    pub quaternion_x: f32, pub quaternion_y: f32, pub quaternion_z: f32, pub quaternion_w: f32,
    interpolation_bytes: InterpolationData
}
impl BoneMotionData
{
    pub fn has_position_data(&self) -> bool { self.x + self.y + self.z != 0.0 }
    pub fn has_rotation_data(&self) -> bool
    {
        self.quaternion_x + self.quaternion_y + self.quaternion_z != 0.0 &&
            (1.0 - self.quaternion_w).abs() > std::f32::EPSILON
    }
}
impl std::fmt::Debug for InterpolationData
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(fmt, "{:?}", self.0.iter().map(|&x| f32::from(x) / 127.0).collect::<Vec<_>>())
    }
}
#[repr(C)]
pub struct FaceMotionData
{
    name_bytes: [u8; 15], pub frame_number: u32, pub weight: f32
}
#[repr(C)]
pub struct CameraMotionData
{
    pub frame_number: u32, pub length: u32,
    pub x: f32, pub y: f32, pub z: f32,
    pub rotation_x: f32, pub rotation_y: f32, pub rotation_z: f32,
    interpolation_bytes: [u8; 24], pub fov_angle: u32, pub perspective: u8
}
impl MotionData
{
    pub fn read<R: Read + Seek>(reader: &mut R) -> Result<Self, LoadingError>
    {
        let mut header_bytes = [0u8; 50];
        reader.read_exact(&mut header_bytes)?;
        if &header_bytes[..30] != b"Vocaloid Motion Data 0002\0\0\0\0\0"
        {
            return Err(LoadingError::SignatureMismatch);
        }
        let name_bytecount = header_bytes[30..50].iter().position(|&b| b == 0x00).unwrap_or(20);

        let bone_keyframe_count = u32::from_le(u32::read_value1(reader)?);
        let mut bone_keyframes = Vec::with_capacity(bone_keyframe_count as _);
        for _ in 0 .. bone_keyframe_count
        {
            let mut md: BoneMotionData = unsafe { std::mem::zeroed() };
            reader.read_exact(&mut md.name_bytes)?;
            reader.read_exact(unsafe { &mut *((&mut md as *mut _ as *mut u8).offset(16) as *mut [u8; 96]) })?;
            bone_keyframes.push(md);
        }

        let face_keyframe_count = u32::from_le(u32::read_value1(reader)?);
        let mut face_keyframes = Vec::with_capacity(face_keyframe_count as _);
        for _ in 0 .. face_keyframe_count
        {
            let mut md: FaceMotionData = unsafe { std::mem::zeroed() };
            reader.read_exact(&mut md.name_bytes)?;
            reader.read_exact(unsafe { &mut *((&mut md as *mut _ as *mut u8).offset(16) as *mut [u8; 4 * 2]) })?;
            face_keyframes.push(md);
        }

        let camera_keyframe_count = u32::from_le(u32::read_value1(reader)?);
        let mut camera_keyframes = Vec::with_capacity(camera_keyframe_count as _);
        unsafe { camera_keyframes.set_len(camera_keyframe_count as _) };
        reader.read_exact(unsafe
        {
            std::slice::from_raw_parts_mut(camera_keyframes.as_mut_ptr() as *mut _,
                size_of::<CameraMotionData>() * camera_keyframe_count as usize)
        })?;

        Ok(MotionData
        {
            name_bytes: header_bytes[30..30+name_bytecount].to_owned(),
            bone_motions: bone_keyframes, face_motions: face_keyframes, camera_motions: camera_keyframes
        })
    }

    /// Decodes the vmd name.
    /// Returns an error text if name contains invalid Shift-JIS sequence.
    pub fn decode_name(&self) -> Result<String, Cow<str>>
    {
        Windows31JEncoding.decode(&self.name_bytes, DecoderTrap::Strict)
    }

    pub fn bone_keyframes(&self) -> &[BoneMotionData] { &self.bone_motions }
    pub fn face_keyframes(&self) -> &[FaceMotionData] { &self.face_motions }
    pub fn camera_keyframes(&self) -> &[CameraMotionData] { &self.camera_motions }
}
impl BoneMotionData
{
    pub fn decode_name(&self) -> Result<String, Cow<str>>
    {
        Windows31JEncoding.decode(&self.name_bytes, DecoderTrap::Replace)
    }
}
impl FaceMotionData
{
    pub fn decode_name(&self) -> Result<String, Cow<str>>
    {
        Windows31JEncoding.decode(&self.name_bytes, DecoderTrap::Strict)
    }
}

#[derive(Debug)]
pub enum LoadingError
{
    IO(IOError), SignatureMismatch
}
impl From<IOError> for LoadingError { fn from(v: IOError) -> Self { LoadingError::IO(v) } }
