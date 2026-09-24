use crate::ffi::hb_font_t;

unsafe extern "C" {
    pub fn hb_font_reference(font: *mut hb_font_t) -> *mut hb_font_t;
    pub fn hb_font_destroy(font: *mut hb_font_t);
}
