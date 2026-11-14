#![allow(non_camel_case_types)]

pub type FT_Memory = *mut FT_MemoryRec_;

pub type FT_Alloc_Func =
    extern "system" fn(memory: FT_Memory, size: core::ffi::c_long) -> *mut core::ffi::c_void;
pub type FT_Free_Func = extern "system" fn(memory: FT_Memory, block: *mut core::ffi::c_void);
pub type FT_Realloc_Func = extern "system" fn(
    memory: FT_Memory,
    cur_size: core::ffi::c_long,
    new_size: core::ffi::c_long,
    block: *mut core::ffi::c_void,
) -> *mut core::ffi::c_void;

#[repr(C)]
pub struct FT_MemoryRec_ {
    pub user: *mut core::ffi::c_void,
    pub alloc: FT_Alloc_Func,
    pub free: FT_Free_Func,
    pub realloc: FT_Realloc_Func,
}

pub type FT_Stream = *mut FT_StreamRec;
#[repr(C)]
pub union FT_StreamDesc {
    pub value: core::ffi::c_long,
    pub pointer: *mut core::ffi::c_void,
}

pub type FT_Stream_IoFunc = extern "system" fn(
    stream: FT_Stream,
    offset: core::ffi::c_ulong,
    buffer: *mut core::ffi::c_uchar,
    count: core::ffi::c_ulong,
) -> core::ffi::c_ulong;
pub type FT_Stream_CloseFunc = extern "system" fn(stream: FT_Stream);
#[repr(C)]
pub struct FT_StreamRec {
    pub base: *mut core::ffi::c_uchar,
    pub size: core::ffi::c_ulong,
    pub pos: core::ffi::c_ulong,

    pub descriptor: FT_StreamDesc,
    pub pathname: FT_StreamDesc,
    pub read: FT_Stream_IoFunc,
    pub close: FT_Stream_CloseFunc,

    pub memory: FT_Memory,
    pub cursor: *mut core::ffi::c_uchar,
    pub limit: *mut core::ffi::c_uchar,
}
