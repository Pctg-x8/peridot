use crate::raw::*;

pub type FTC_FaceID = FT_Pointer;
pub type FTC_Face_Requester = extern "system" fn(
    face_id: FTC_FaceID,
    library: FT_Library,
    req_data: FT_Pointer,
    aface: *mut FT_Face,
) -> FT_Error;

ExternOpaqueStruct!(pub struct FTC_ManagerRec);
pub type FTC_Manager = *mut FTC_ManagerRec;
ExternOpaqueStruct!(pub struct FTC_NodeRec);
pub type FTC_Node = *mut FTC_NodeRec;

unsafe extern "system" {
    pub fn FTC_Manager_New(
        library: FT_Library,
        max_faces: FT_UInt,
        max_sizes: FT_UInt,
        max_bytes: FT_UInt,
        requester: FTC_Face_Requester,
        req_data: FT_Pointer,
        amanager: *mut FTC_Manager,
    ) -> FT_Error;
    pub fn FTC_Manager_Reset(manager: FTC_Manager);
    pub fn FTC_Manager_Done(manager: FTC_Manager);
    pub fn FTC_Manager_LookupFace(
        manager: FTC_Manager,
        face_id: FTC_FaceID,
        aface: *mut FT_Face,
    ) -> FT_Error;
}

#[repr(C)]
pub struct FTC_ScalerRec {
    pub face_id: FTC_FaceID,
    pub width: FT_UInt,
    pub height: FT_UInt,
    pub pixel: FT_Int,
    pub x_res: FT_UInt,
    pub y_res: FT_UInt,
}
pub type FTC_Scaler = *mut FTC_ScalerRec;

unsafe extern "system" {
    pub fn FTC_Manager_LookupSize(
        manager: FTC_Manager,
        scaler: FTC_Scaler,
        asize: *mut FT_Size,
    ) -> FT_Error;
    pub fn FTC_Node_Unref(node: FTC_Node, manager: FTC_Manager);
    pub fn FTC_Manager_RemoveFaceID(manager: FTC_Manager, face_id: FTC_FaceID);
}

ExternOpaqueStruct!(pub struct FTC_CMapCacheRec);
pub type FTC_CMapCache = *mut FTC_CMapCacheRec;
unsafe extern "system" {
    pub fn FTC_CMapCache_New(manager: FTC_Manager, acache: *mut FTC_CMapCache) -> FT_Error;
    pub fn FTC_CMapCache_Lookup(
        cache: FTC_CMapCache,
        face_id: FTC_FaceID,
        cmap_index: FT_Int,
        char_code: u32,
    ) -> FT_UInt;
}

#[repr(C)]
pub struct FTC_ImageTypeRec {
    pub face_id: FTC_FaceID,
    pub width: FT_UInt,
    pub height: FT_UInt,
    pub flags: i32,
}
pub type FTC_ImageType = *mut FTC_ImageTypeRec;
#[macro_export]
macro_rules! FTC_IMAGE_TYPE_COMPARE {
    ($d1: expr, $d2: expr) => {
        ((*$d1).face_id = (*$d2).face_id)
            && ((*$d1).width = (*$d2).width)
            && ((*$d1).flags = (*$d2).flags)
    };
}

ExternOpaqueStruct!(pub struct FTC_ImageCacheRec);
pub type FTC_ImageCache = *mut FTC_ImageCacheRec;
unsafe extern "system" {
    pub fn FTC_ImageCache_New(manager: FTC_Manager, acache: *mut FTC_ImageCache) -> FT_Error;
    pub fn FTC_ImageCache_Lookup(
        cache: FTC_ImageCache,
        type_: FTC_ImageType,
        gindex: FT_UInt,
        aglyph: *mut FT_Glyph,
        anode: *mut FTC_Node,
    ) -> FT_Error;
    pub fn FTC_ImageCache_LookupScaler(
        cache: FTC_ImageCache,
        scaler: FTC_Scaler,
        load_flags: FT_ULong,
        gindex: FT_UInt,
        aglyph: *mut FT_Glyph,
        anode: *mut FTC_Node,
    ) -> FT_Error;
}

pub type FTC_SBit = *mut FTC_SBitRec;
#[repr(C)]
pub struct FTC_SBitRec {
    pub width: FT_Byte,
    pub height: FT_Byte,
    pub left: FT_Char,
    pub top: FT_Char,

    pub format: FT_Byte,
    pub max_grays: FT_Byte,
    pub pitch: FT_Short,
    pub xadvance: FT_Char,
    pub yadvance: FT_Char,

    pub buffer: *mut FT_Byte,
}

ExternOpaqueStruct!(pub struct FTC_SBitCacheRec);
pub type FTC_SBitCache = *mut FTC_SBitCacheRec;
unsafe extern "system" {
    pub fn FTC_SBitCache_New(manager: FTC_Manager, acache: *mut FTC_SBitCache) -> FT_Error;
    pub fn FTC_SBitCache_Lookup(
        cache: FTC_SBitCache,
        type_: FTC_ImageType,
        gindex: FT_UInt,
        sbit: *mut FTC_SBit,
        anode: *mut FTC_Node,
    ) -> FT_Error;
    pub fn FTC_SBitCache_LookupScaler(
        cache: FTC_SBitCache,
        scaler: FTC_Scaler,
        load_flags: FT_ULong,
        gindex: FT_UInt,
        sbit: *mut FTC_SBit,
        anode: *mut FTC_Node,
    ) -> FT_Error;
}
