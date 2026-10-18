#![allow(non_upper_case_globals)]

use core::ffi::*;

use crate::raw::{spa_pod_sequence, spa_point, spa_rectangle, spa_region};

pub type spa_meta_type = u32;
pub const SPA_META_Invalid: spa_meta_type = 0;
pub const SPA_META_Header: spa_meta_type = 1;
pub const SPA_META_VideoCrop: spa_meta_type = 2;
pub const SPA_META_VideoDamage: spa_meta_type = 3;
pub const SPA_META_Bitmap: spa_meta_type = 4;
pub const SPA_META_Cursor: spa_meta_type = 5;
pub const SPA_META_Control: spa_meta_type = 6;
pub const SPA_META_Busy: spa_meta_type = 7;
pub const SPA_META_VideoTransform: spa_meta_type = 8;
pub const SPA_META_SyncTimeline: spa_meta_type = 9;

#[repr(C)]
pub struct spa_meta {
    pub r#type: u32,
    pub size: u32,
    pub data: *mut c_void,
}
impl spa_meta {
    #[inline(always)]
    pub const fn first(&self) -> *mut c_void {
        self.data
    }

    #[inline(always)]
    pub const fn end(&self) -> *mut c_void {
        unsafe { self.data.byte_add(self.size as _) }
    }

    #[inline(always)]
    pub fn check<T>(&self, p: *mut T) -> bool {
        unsafe { p.add(1) <= self.end().cast::<T>() }
    }

    #[inline(always)]
    pub const fn iter<'a, T>(&'a self) -> spa_meta_for_each_iter<'a, T> {
        spa_meta_for_each_iter::new(self)
    }
}

#[repr(C)]
pub struct spa_meta_header {
    pub flags: u32,
    pub offset: u32,
    pub pts: i64,
    pub dts_offset: i64,
    pub seq: u64,
}

pub const SPA_META_HEADER_FLAG_DISCONT: u32 = 1 << 0;
pub const SPA_META_HEADER_FLAG_CORRUPTED: u32 = 1 << 1;
pub const SPA_META_HEADER_FLAG_MARKER: u32 = 1 << 2;
pub const SPA_META_HEADER_FLAG_HEADER: u32 = 1 << 3;
pub const SPA_META_HEADER_FLAG_GAP: u32 = 1 << 4;
pub const SPA_META_HEADER_FLAG_DELTA_UNIT: u32 = 1 << 5;

#[repr(C)]
pub struct spa_meta_region {
    pub region: spa_region,
}
impl spa_meta_region {
    #[inline(always)]
    pub const fn is_valid(&self) -> bool {
        self.region.size.width != 0 && self.region.size.height != 0
    }
}

pub struct spa_meta_for_each_iter<'a, T> {
    pub meta: &'a spa_meta,
    pub pos: *mut T,
}
impl<'a, T> spa_meta_for_each_iter<'a, T> {
    pub const fn new(meta: &'a spa_meta) -> Self {
        Self {
            meta,
            pos: meta.first().cast::<T>(),
        }
    }
}
impl<'a, T> Iterator for spa_meta_for_each_iter<'a, T> {
    type Item = *mut T;

    fn next(&mut self) -> Option<Self::Item> {
        if !self.meta.check(self.pos) {
            return None;
        }

        let v = self.pos;
        self.pos = unsafe { self.pos.add(1) };
        Some(v)
    }
}

#[repr(C)]
pub struct spa_meta_bitmap {
    pub format: u32,
    pub size: spa_rectangle,
    pub stride: i32,
    pub offset: u32,
}
impl spa_meta_bitmap {
    #[inline(always)]
    pub const fn is_valid(&self) -> bool {
        self.format != 0
    }
}

#[repr(C)]
pub struct spa_meta_cursor {
    pub id: u32,
    pub flags: u32,
    pub position: spa_point,
    pub hotspot: spa_point,
    pub bitmap_offset: u32,
}
impl spa_meta_cursor {
    #[inline(always)]
    pub const fn is_valid(&self) -> bool {
        self.id != 0
    }
}

#[repr(C)]
pub struct spa_meta_control {
    pub sequence: spa_pod_sequence,
}

#[repr(C)]
pub struct spa_meta_busy {
    pub flags: u32,
    pub count: u32,
}

pub type spa_meta_videotransform_value = u32;
pub const SPA_META_TRANSFORMATION_None: spa_meta_videotransform_value = 0;
pub const SPA_META_TRANSFORMATION_90: spa_meta_videotransform_value = 1;
pub const SPA_META_TRANSFORMATION_180: spa_meta_videotransform_value = 2;
pub const SPA_META_TRANSFORMATION_270: spa_meta_videotransform_value = 3;
pub const SPA_META_TRANSFORMATION_Flipped: spa_meta_videotransform_value = 4;
pub const SPA_META_TRANSFORMATION_Flipped90: spa_meta_videotransform_value = 5;
pub const SPA_META_TRANSFORMATION_Flipped180: spa_meta_videotransform_value = 6;
pub const SPA_META_TRANSFORMATION_Flipped270: spa_meta_videotransform_value = 7;

#[repr(C)]
pub struct spa_meta_videotransform {
    pub transform: u32,
}

#[repr(C)]
pub struct spa_meta_sync_timeline {
    pub flags: u32,
    pub padding: u32,
    pub acquire_point: u64,
    pub release_point: u64,
}

pub type spa_data_type = u32;
pub const SPA_DATA_Invalid: spa_data_type = 0;
pub const SPA_DATA_MemPtr: spa_data_type = 1;
pub const SPA_DATA_MemFd: spa_data_type = 2;
pub const SPA_DATA_DmaBuf: spa_data_type = 3;
pub const SPA_DATA_MemId: spa_data_type = 4;
pub const SPA_DATA_SyncObj: spa_data_type = 5;

#[repr(C)]
pub struct spa_chunk {
    pub offset: u32,
    pub size: u32,
    pub stride: i32,
    pub flags: i32,
}

pub const SPA_CHUNK_FLAG_NONE: i32 = 0;
pub const SPA_CHUNK_FLAG_CORRUPTED: i32 = 1 << 0;
pub const SPA_CHUNK_FLAG_EMPTY: i32 = 1 << 1;

#[repr(C)]
pub struct spa_data {
    pub r#type: u32,
    pub flags: u32,
    pub fd: i64,
    pub mapoffset: u32,
    pub maxsize: u32,
    pub data: *mut c_void,
    pub chunk: *mut spa_chunk,
}

pub const SPA_DATA_FLAG_NONE: u32 = 0;
pub const SPA_DATA_FLAG_READABLE: u32 = 1 << 0;
pub const SPA_DATA_FLAG_WRITABLE: u32 = 1 << 1;
pub const SPA_DATA_FLAG_DYNAMIC: u32 = 1 << 2;
pub const SPA_DATA_FLAG_READWRITE: u32 = SPA_DATA_FLAG_READABLE | SPA_DATA_FLAG_WRITABLE;
pub const SPA_DATA_FLAG_MAPPABLE: u32 = 1 << 3;

#[repr(C)]
pub struct spa_buffer {
    pub n_metas: u32,
    pub n_datas: u32,
    pub metas: *mut spa_meta,
    pub datas: *mut spa_data,
}
impl spa_buffer {
    #[inline(always)]
    pub const fn metas(&self) -> &[spa_meta] {
        unsafe { core::slice::from_raw_parts(self.metas, self.n_metas as _) }
    }

    #[inline(always)]
    pub const fn datas(&self) -> &[spa_data] {
        unsafe { core::slice::from_raw_parts(self.datas, self.n_datas as _) }
    }
}
