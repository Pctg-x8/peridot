//! Vocaloid Motion Data

use std::io::prelude::*;
use std::io::Error as IOError;
use encoding::codec::japanese::Windows31JEncoding;
use encoding::{Encoding, DecoderTrap};
use std::borrow::Cow;
use std::slice::from_raw_parts;
use std::mem::size_of;

pub struct MotionData
{
    name_bytes: Vec<u8>, file_bytes: Vec<u8>,
    pub bone_keyframe_count: u32, pub face_keyframe_count: u32, pub camera_keyframe_count: u32
}
#[repr(C, packed)]
pub struct BoneMotionData
{
    name_bytes: [u8; 15], pub frame_number: u32,
    pub x: f32, pub y: f32, pub z: f32,
    pub quaternion_x: f32, pub quaterion_y: f32, pub quaternion_z: f32, pub quaterion_w: f32,
    interpolation_bytes: [u8; 64]
}
#[repr(C, packed)]
pub struct FaceMotionData
{
    name_bytes: [u8; 15], pub frame_number: u32, pub weight: f32
}
#[repr(C, packed)]
pub struct CameraMotionData
{
    pub frame_number: u32, pub length: u32,
    pub x: f32, pub y: f32, pub z: f32,
    pub rotation_x: f32, pub rotation_y: f32, pub rotation_z: f32,
    interpolation_bytes: [u8; 24], pub fov_angle: u32, pub perspective: u8
}
impl MotionData {
    pub fn read<R: Read>(reader: &mut R) -> Result<Self, LoadingError> {
        let mut header_bytes = [0u8; 50];
        reader.read_exact(&mut header_bytes)?;
        if &header_bytes[..30] != b"Vocaloid Motion Data 0002\0\0\0\0\0" {
            return Err(LoadingError::SignatureMismatch);
        }
        let name_bytecount = header_bytes[30..50].iter().position(|&b| b == 0x00).unwrap_or(20);
        let mut file_bytes = Vec::new();
        reader.read_to_end(&mut file_bytes)?;
        let bone_keyframe_count = unsafe { *(file_bytes.as_ptr() as *const u32) };
        let face_keyframe_count = unsafe
        {
            *(file_bytes.as_ptr().add(4 + size_of::<BoneMotionData>() * bone_keyframe_count as usize) as *const u32)
        };
        let camera_keyframe_count = unsafe
        {
            let ptr_face = file_bytes.as_ptr().add(8 + size_of::<BoneMotionData>() * bone_keyframe_count as usize);
            *(ptr_face.add(size_of::<FaceMotionData>() * face_keyframe_count as usize) as *const u32)
        };

        return Ok(MotionData
        {
            file_bytes, name_bytes: header_bytes[30..30+name_bytecount].to_owned(),
            bone_keyframe_count, face_keyframe_count, camera_keyframe_count
        });
    }

    /// Decodes the vmd name.
    /// Returns an error text if name contains invalid Shift-JIS sequence.
    pub fn decode_name(&self) -> Result<String, Cow<str>>
    {
        Windows31JEncoding.decode(&self.name_bytes, DecoderTrap::Strict)
    }

    pub fn bone_keyframes(&self) -> &[BoneMotionData]
    {
        unsafe
        {
            let md_start_ptr = self.file_bytes.as_ptr().offset(4);
            from_raw_parts(md_start_ptr as *const _, self.bone_keyframe_count as _)
        }
    }
    pub fn face_keyframes(&self) -> &[FaceMotionData]
    {
        unsafe
        {
            let f_start_ptr =
                self.file_bytes.as_ptr().add(4 + size_of::<BoneMotionData>() * self.bone_keyframe_count as usize);
            from_raw_parts(f_start_ptr as *const _, self.face_keyframe_count as _)
        }
    }
    pub fn camera_keyframes(&self) -> &[CameraMotionData]
    {
        unsafe {
            let cm_start_ptr = self.file_bytes.as_ptr()
                .add(4 + size_of::<BoneMotionData>() * self.bone_keyframe_count as usize)
                .add(4 + size_of::<FaceMotionData>() * self.face_keyframe_count as usize);
            from_raw_parts(cm_start_ptr as *const _, self.camera_keyframe_count as _)
        }
    }
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
