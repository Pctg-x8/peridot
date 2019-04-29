//! Vocaloid Motion Data

use std::io::prelude::*;
use std::io::{Error as IOError, SeekFrom};
use encoding::codec::japanese::Windows31JEncoding;
use encoding::{Encoding, DecoderTrap};
use std::borrow::Cow;
use std::slice::from_raw_parts;
use std::mem::size_of;
use super::pmx::TypedReader;

pub struct MotionData
{
    name_bytes: Vec<u8>,
    bone_motions: Vec<BoneMotionData>, face_motions: Vec<FaceMotionData>, camera_motions: Vec<CameraMotionData>
}
#[repr(C)]
pub struct BoneMotionData
{
    name_bytes: [u8; 15], pub frame_number: u32,
    pub x: f32, pub y: f32, pub z: f32,
    pub quaternion_x: f32, pub quaterion_y: f32, pub quaternion_z: f32, pub quaterion_w: f32,
    interpolation_bytes: [u8; 64]
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
pub enum LoadingError {
    IO(IOError), SignatureMismatch
}
impl From<IOError> for LoadingError { fn from(v: IOError) -> Self { LoadingError::IO(v) } }
