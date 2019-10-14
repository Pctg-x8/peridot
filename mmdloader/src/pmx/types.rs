
use std::io::{Read, Error as IOError, Result as IOResult};
use super::TypedReader;
use std::mem::{transmute, transmute_copy};

#[derive(Debug)]
pub enum LoadingError
{
    IO(IOError), SignatureMismatch, MissingGlobals(u8), UnknownStringEncoding(u8), UnknownDeformType(u8),
    UnknownToonReference(u8), UnknownEnvBlendMode(u8), UnknownMorphType(i8), InvalidFrameData(u8),
    UnknownShapeType(u8), UnknownPhysicsMode(u8), UnknonwnJointType(u8), UnknownAerodynamicsMode(u8)
}
impl From<IOError> for LoadingError { fn from(v: IOError) -> Self { LoadingError::IO(v) } }

#[derive(Debug)]
pub struct GlobalizedStrings { pub jp: String, pub univ: String }
pub enum IndexValue { Byte(i8), Short(i16), Long(i32) }
impl IndexValue
{
    pub(crate) fn from_bytes(bytes: &[u8], size: IndexSize) -> Option<Self>
    {
        #[allow(clippy::cast_ptr_alignment)]
        match size
        {
            IndexSize::Byte => unsafe
            {
                let v = transmute(bytes[0]);
                if v < 0 { None } else { IndexValue::Byte(v).into() }
            },
            IndexSize::Short => unsafe
            {
                let v = std::ptr::read_unaligned(bytes.as_ptr() as *const i16);
                if v < 0 { None } else { IndexValue::Short(v).into() }
            },
            IndexSize::Long => unsafe
            {
                let v = std::ptr::read_unaligned(bytes.as_ptr() as *const i32);
                if v < 0 { None } else { IndexValue::Long(v).into() }
            }
        }
    }
    pub fn as_index(&self) -> isize
    {
        match *self
        {
            IndexValue::Byte(v) => v as _,
            IndexValue::Short(v) => v as _,
            IndexValue::Long(v) => v as _
        }
    }

    /// Returns `None` if value less than 0
    pub fn read1<R: Read>(reader: &mut R, size: IndexSize) -> IOResult<Option<Self>>
    {
        match size
        {
            IndexSize::Byte => i8::read_value1(reader).map(|v| if v < 0 { None } else { Some(IndexValue::Byte(v)) }),
            IndexSize::Short => i16::read_value1(reader).map(|v| if v < 0 { None } else { Some(IndexValue::Short(v)) }),
            IndexSize::Long => i32::read_value1(reader).map(|v| if v < 0 { None } else { Some(IndexValue::Long(v)) })
        }
    }
}
impl std::fmt::Debug for IndexValue
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        match self
        {
            IndexValue::Byte(v) => write!(fmt, "{}i8", v),
            IndexValue::Short(v) => write!(fmt, "{}i16", v),
            IndexValue::Long(v) => write!(fmt, "{}i32", v)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum IndexSize { Byte, Short, Long }
impl IndexSize
{
    pub fn decode_bytes_unsigned(self, bytes: &[u8]) -> u32
    {
        match self
        {
            IndexSize::Byte => u32::from(bytes[0]),
            IndexSize::Short => unsafe { u32::from(transmute_copy::<_, &[u16]>(&bytes)[0]) },
            IndexSize::Long => unsafe { transmute_copy::<_, &[u32]>(&bytes)[0] }
        }
    }
    pub fn decode_bytes_signed(self, bytes: &[u8]) -> i32
    {
        match self
        {
            IndexSize::Byte => i32::from(<i8 as TypedReader>::from_bytes(bytes)),
            IndexSize::Short => i32::from(<i16 as TypedReader>::from_bytes(bytes)),
            IndexSize::Long => <i32 as TypedReader>::from_bytes(bytes)
        }
    }
    pub fn decode_bytes_signed2(self, bytes: &[u8]) -> [i32; 2]
    {
        [self.decode_bytes_signed(bytes), self.decode_bytes_signed(&bytes[self.bytesize()..])]
    }
    pub fn decode_bytes_signed4(self, bytes: &[u8]) -> [i32; 4]
    {
        [
            self.decode_bytes_signed(bytes),
            self.decode_bytes_signed(&bytes[self.bytesize()..]),
            self.decode_bytes_signed(&bytes[self.bytesize()*2..]),
            self.decode_bytes_signed(&bytes[self.bytesize()*3..])
        ]
    }
    pub fn bytesize(self) -> usize
    {
        match self
        {
            IndexSize::Byte => 1, IndexSize::Short => 2, IndexSize::Long => 4
        }
    }

    pub fn read_bytes_signed<R: Read + ?Sized>(self, reader: &mut R) -> IOResult<i32>
    {
        match self
        {
            IndexSize::Byte => i8::read_value1(reader).map(i32::from),
            IndexSize::Short => i16::read_value1(reader).map(i32::from),
            IndexSize::Long => i32::read_value1(reader)
        }
    }
}
impl From<u8> for IndexSize
{
    fn from(v: u8) -> Self
    {
        match v {
            1 => IndexSize::Byte, 2 => IndexSize::Short, 4 => IndexSize::Long,
            v => panic!("Unable to convert into IndexSize: {}", v)
        }
    }
}

#[repr(C)] #[derive(Clone, Debug)]
pub struct Vec2(pub f32, pub f32);
#[repr(C)] #[derive(Clone, Debug)]
pub struct Vec3(pub f32, pub f32, pub f32);
#[repr(C)] #[derive(Clone, Debug)]
pub struct Vec4(pub f32, pub f32, pub f32, pub f32);
#[allow(clippy::cast_ptr_alignment)]
impl TypedReader for Vec2
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { Clone::clone(&*(bytes.as_ptr() as *const Self)) } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
        let mut bytes = [0u8; 4 * 2];
        reader.read_exact(&mut bytes).map(move |_| unsafe { transmute(bytes) })
    }
}
#[allow(clippy::cast_ptr_alignment)]
impl TypedReader for Vec3
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { Clone::clone(&*(bytes.as_ptr() as *const Self)) } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
        let mut bytes = [0u8; 4 * 3];
        reader.read_exact(&mut bytes).map(move |_| unsafe { transmute(bytes) })
    }
}
#[allow(clippy::cast_ptr_alignment)]
impl TypedReader for Vec4
{
    fn from_bytes(bytes: &[u8]) -> Self { unsafe { Clone::clone(&*(bytes.as_ptr() as *const Self)) } }
    fn read_value1<R: Read + ?Sized>(reader: &mut R) -> IOResult<Self> {
        let mut bytes = [0u8; 4 * 4];
        reader.read_exact(&mut bytes).map(move |_| unsafe { transmute(bytes) })
    }
}
