//! Vocaloid Motion Data

use std::io::prelude::*;
use std::io::Error as IOError;
use std::mem::transmute;
use encoding::codec::japanese::Windows31JEncoding;
use encoding::{Encoding, DecoderTrap};

pub struct MotionData {
    pub name_cell: String
}
impl MotionData {
    pub fn read<R: Read>(reader: &mut R) -> Result<Self, LoadingError> {
        let mut header_bytes = [0u8; 54];
        reader.read_exact(&mut header_bytes)?;
        if &header_bytes[..30] != b"Vocaloid Motion Data 0002\0\0\0\0\0" {
            return Err(LoadingError::SignatureMismatch);
        }
        let fc = unsafe { transmute::<_, &[u32]>(&header_bytes[50..])[0] };
        println!("FrameCount: {}", fc);
        let name_bytecount = header_bytes[30..50].iter().position(|&b| b == 0x00).unwrap_or(20);
        let name = Windows31JEncoding.decode(&header_bytes[30..30 + name_bytecount], DecoderTrap::Strict)
            .expect("Invalid Shift-JIS Sequence in name");
        return Ok(MotionData { name });
    }
}

#[derive(Debug)]
pub enum LoadingError {
    IO(IOError), SignatureMismatch
}
impl From<IOError> for LoadingError { fn from(v: IOError) -> Self { LoadingError::IO(v) } }