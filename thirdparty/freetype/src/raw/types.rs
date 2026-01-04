#![allow(non_camel_case_types)]
//! Basic Data Types.
//! Note: FT_IntXX and FT_UIntXX types are directly mapped to iXX and uXX.

pub type FT_Bool = core::ffi::c_uchar;
pub type FT_FWord = i16;
pub type FT_UFWord = u16;
pub type FT_Char = core::ffi::c_char;
pub type FT_Byte = core::ffi::c_uchar;
pub type FT_Bytes = *const FT_Byte;
pub type FT_Tag = u32;
pub type FT_String = core::ffi::c_char;
pub type FT_Short = core::ffi::c_short;
pub type FT_UShort = core::ffi::c_ushort;
pub type FT_Int = core::ffi::c_int;
pub type FT_UInt = core::ffi::c_uint;
pub type FT_Long = core::ffi::c_long;
pub type FT_ULong = core::ffi::c_ulong;
pub type FT_F2Dot14 = i16;
pub type FT_F26Dot6 = i32;
pub type FT_Fixed = core::ffi::c_long;
pub type FT_Error = core::ffi::c_int;
pub type FT_Pointer = *mut core::ffi::c_void;
pub type FT_Offset = usize;
// pub type FT_PtrDist = core::ffi::c_ptrdiff_t; (Unstable)
pub type FT_PtrDist = isize;
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_UnitVector {
    pub x: FT_F2Dot14,
    pub y: FT_F2Dot14,
}
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_Matrix {
    pub xx: FT_Fixed,
    pub xy: FT_Fixed,
    pub yx: FT_Fixed,
    pub yy: FT_Fixed,
}

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_Data {
    pub pointer: *const FT_Byte,
    pub length: FT_Int,
}
impl FT_Data {
    pub unsafe fn as_slice<'a>(&self) -> &'a [u8] {
        unsafe { std::slice::from_raw_parts(self.pointer, self.length as _) }
    }
}

pub type FT_Generic_Finalizer = extern "C" fn(object: *mut core::ffi::c_void);
#[repr(C)]
#[derive(Clone)]
pub struct FT_Generic {
    pub data: *mut core::ffi::c_void,
    pub finalizer: FT_Generic_Finalizer,
}

#[inline(always)]
pub const fn FT_MAKE_TAG(x1: FT_ULong, x2: FT_ULong, x3: FT_ULong, x4: FT_ULong) -> FT_Tag {
    ((x1 << 24) | (x2 << 16) | (x3 << 8) | x4) as FT_Tag
}

pub type FT_ListNode = *mut FT_ListNodeRec;
pub type FT_List = *mut FT_ListRec;
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_ListNodeRec {
    pub prev: FT_ListNode,
    pub next: FT_ListNode,
    pub data: *mut core::ffi::c_void,
}
#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FT_ListRec {
    pub head: FT_ListNode,
    pub tail: FT_ListNode,
}
impl FT_ListRec {
    /// FT_IS_EMPTY
    pub fn is_empty(&self) -> bool {
        self.head.is_null()
    }
}
