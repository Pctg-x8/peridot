use super::*;
use peridot_tp_freetype::raw::*;

unsafe extern "C" {
    pub unsafe fn FcFreeTypeCharIndex(face: FT_Face, ucs4: FcChar32) -> FT_UInt;
    pub unsafe fn FcFreeTypeCharSetAndSpacing(
        face: FT_Face,
        blanks: *mut FcBlanks,
        spacing: *mut core::ffi::c_int,
    ) -> *mut FcCharSet;
    pub unsafe fn FcFreeTypeCharSet(face: FT_Face, blanks: *mut FcBlanks) -> *mut FcCharSet;
    pub unsafe fn FcPatternGetFTFace(
        p: *const FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        f: *mut FT_Face,
    ) -> FcResult;
    pub unsafe fn FcPatternAddFTFace(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        f: FT_Face,
    ) -> FcBool;
    pub unsafe fn FcFreeTypeQueryface(
        face: FT_Face,
        file: *const FcChar8,
        id: core::ffi::c_uint,
        blanks: *mut FcBlanks,
    ) -> *mut FcPattern;
}
