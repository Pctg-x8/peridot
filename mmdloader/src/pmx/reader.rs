
use std::io::{Read, Result as IOResult};
use super::{GlobalizedStrings, LoadingError};
use std::mem::{transmute, transmute_copy};

#[macro_export]
macro_rules! ReadExactBytes
{
    ($reader: expr, $n: expr) =>
    {{
        let mut bytes = [0u8; $n];
        $reader.read_exact(&mut bytes[..]).map(|_| bytes)
    }};
    (var $reader:expr, $n: expr) =>
    {{
        let mut bytes = Vec::with_capacity($n); unsafe { bytes.set_len($n); }
        $reader.read_exact(&mut bytes).map(|_| bytes)
    }}
}

pub trait StringReader
{
    fn read_string(&self, reader: &mut Read) -> IOResult<String>;
    fn read_globalized(&self, reader: &mut Read) -> IOResult<GlobalizedStrings>
    {
        Ok(GlobalizedStrings { jp: self.read_string(reader)?, univ: self.read_string(reader)? })
    }
}
pub struct Utf8StringReader;
pub struct Utf16LEStringReader;
impl StringReader for Utf8StringReader
{
    fn read_string(&self, reader: &mut Read) -> IOResult<String>
    {
        let byte_length = i32::read_value1(reader)?;
        let mut bytes = vec![0u8; byte_length as _];
        
        reader.read_exact(&mut bytes).map(move |_| String::from_utf8(bytes).expect("Decoding UTF-8"))
    }
}
impl StringReader for Utf16LEStringReader
{
    fn read_string(&self, reader: &mut Read) -> IOResult<String>
    {
        let byte_length = i32::read_value1(reader)?;
        let mut bytes = vec![0u16; (byte_length >> 1) as _];

        reader.read_exact(unsafe { std::slice::from_raw_parts_mut(bytes.as_mut_ptr() as *mut u8, byte_length as _) })
            .map(move |_| String::from_utf16_lossy(&bytes))
    }
}

pub trait TypedReader: Sized
{
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>;
    fn from_bytes(bytes: &[u8]) -> Self;
}
impl TypedReader for i8
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute(bytes[0]) } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>
    {
        let mut bytes = [0i8; 1];
        reader.read_exact(unsafe { &mut *(&mut bytes as *mut _ as *mut [u8; 1]) }).map(move |_| bytes[0])
    }
}
impl TypedReader for u8
{
    fn from_bytes(bytes: &[u8]) -> Self { bytes[0] }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>
    {
        let mut bytes = [0u8; 1];
        reader.read_exact(&mut bytes).map(move |_| bytes[0])
    }
}
impl TypedReader for i16
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute_copy::<_, &[Self]>(&bytes)[0] } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>
    {
        let mut bytes = [0i16; 1];
        reader.read_exact(unsafe { &mut *(&mut bytes as *mut _ as *mut [u8; 2]) }).map(move |_| bytes[0])
    }
}
impl TypedReader for u16
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute_copy::<_, &[Self]>(&bytes)[0] } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>
    {
        let mut bytes = [0u16; 1];
        reader.read_exact(unsafe { &mut *(&mut bytes as *mut _ as *mut [u8; 2]) }).map(move |_| bytes[0])
    }
}
impl TypedReader for i32
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute_copy::<_, &[Self]>(&bytes)[0] } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>
    {
        let mut bytes = [0i32; 1];
        reader.read_exact(unsafe { &mut *(&mut bytes as *mut _ as *mut [u8; 4]) }).map(move |_| bytes[0])
    }
}
impl TypedReader for u32
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute_copy::<_, &[Self]>(&bytes)[0] } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>
    {
        let mut bytes = [0u32; 1];
        reader.read_exact(unsafe { &mut *(&mut bytes as *mut _ as *mut [u8; 4]) }).map(move |_| bytes[0])
    }
}
impl TypedReader for f32
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { transmute_copy::<_, &[Self]>(&bytes)[0] } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self>
    {
        ReadExactBytes!(reader, 4).map(|v| Self::from_bytes(&v[..]))
    }
}

pub fn read_array<R: ?Sized, Fr, T>(reader: &mut R, value_reader: Fr) -> Result<Vec<T>, LoadingError> where
    R: Read, Fr: Fn(&mut R) -> Result<T, LoadingError>
{
    let count = i32::read_value1(reader)?;
    (0..count).map(move |_| value_reader(reader)).collect()
}
