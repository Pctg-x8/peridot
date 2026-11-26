// Base fixed point types
pub type Fixed = i32;
pub type Fract = i32;
pub type UnsignedFixed = u32;
pub type ShortFixed = core::ffi::c_short;

#[repr(C)]
pub struct Float80 {
    pub exp: i16,
    pub man: [u16; 4],
}

#[repr(C)]
pub struct Float96 {
    pub exp: [i16; 2],
    pub man: [u16; 4],
}

#[repr(C)]
pub struct Float32Point {
    pub x: f32,
    pub y: f32,
}

pub type Ptr = *mut core::ffi::c_char;
pub type Handle = *mut Ptr;
pub type Size = core::ffi::c_long;

// Higher level basic types
pub type OSErr = i16;
pub type OSStatus = i32;
pub type LogicalAddress = *mut core::ffi::c_void;
pub type ConstLogicalAddress = *const core::ffi::c_void;
pub type PhysicalAddress = *mut core::ffi::c_void;
pub type BytePtr = *mut u8;
pub type ByteCount = core::ffi::c_ulong;
pub type ByteOfset = core::ffi::c_ulong;
pub type Duration = i32;
pub type AbsoluteTime = u64;
pub type OptionBits = u32;
pub type ItemCount = core::ffi::c_ulong;
pub type PBVersion = u32;
pub type ScriptCode = i16;
pub type LangCode = i16;
pub type RegionCode = i16;
pub type FourCharCode = u32;
pub type OSType = FourCharCode;
pub type ResType = FourCharCode;

pub type Boolean = core::ffi::c_uchar;
