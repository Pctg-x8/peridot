use bitflags::bitflags;

use crate::raw;

pub mod pod;

#[repr(transparent)]
pub struct Data(raw::spa_data);
impl Data {
    /// `Ok(None)` for `SPA_ID_INVALID`
    #[inline(always)]
    pub fn r#type(&self) -> Result<Option<DataType>, u32> {
        if self.0.r#type == raw::SPA_ID_INVALID {
            Ok(None)
        } else {
            self.0.r#type.try_into().map(Some)
        }
    }

    #[inline(always)]
    pub fn flags(&self) -> &DataFlags {
        unsafe { core::mem::transmute(&self.0.flags) }
    }

    #[inline(always)]
    pub fn max_size(&self) -> u32 {
        self.0.maxsize
    }

    #[inline(always)]
    pub fn data_ptr(&self) -> *mut core::ffi::c_void {
        self.0.data
    }

    #[inline(always)]
    pub fn chunk_offset_mut(&mut self) -> &mut u32 {
        unsafe { &mut (*self.0.chunk).offset }
    }

    #[inline(always)]
    pub fn chunk_size_mut(&mut self) -> &mut u32 {
        unsafe { &mut (*self.0.chunk).size }
    }

    #[inline(always)]
    pub fn chunk_stride_mut(&mut self) -> &mut i32 {
        unsafe { &mut (*self.0.chunk).stride }
    }

    #[inline(always)]
    pub fn chunk_flags_mut(&mut self) -> &mut ChunkFlags {
        unsafe { core::mem::transmute(&mut (*self.0.chunk).flags) }
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct DataFlags : u32 {
        const NONE = 0;
        const READABLE = 1 << 0;
        const WRITEABLE = 1 << 1;
        const DYNAMIC = 1 << 2;
        const MAPPABLE = 1 << 3;
    }
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct ChunkFlags : i32 {
        const NONE = 0;
        const CORRUPTED = 1 << 0;
        const EMPTY = 1 << 1;
    }
}

#[repr(u32)]
pub enum DataType {
    Invalid = raw::SPA_DATA_Invalid,
    MemPtr = raw::SPA_DATA_MemPtr,
    MemFd = raw::SPA_DATA_MemFd,
    DmfBuf = raw::SPA_DATA_DmaBuf,
    MemId = raw::SPA_DATA_MemId,
    SyncObj = raw::SPA_DATA_SyncObj,
}
impl TryFrom<u32> for DataType {
    type Error = u32;

    #[inline(always)]
    fn try_from(value: u32) -> Result<Self, Self::Error> {
        if Self::Invalid as u32 <= value && value <= Self::SyncObj as u32 {
            Ok(unsafe { core::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}
