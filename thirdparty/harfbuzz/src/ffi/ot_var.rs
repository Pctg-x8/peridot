use super::*;

pub const HB_OT_TAG_VAR_AXIS_ITALIC: hb_tag_t = HB_TAG(b'i', b't', b'a', b'l');
pub const HB_OT_TAG_VAR_AXIS_OPTICAL_SIZE: hb_tag_t = HB_TAG(b'o', b'p', b's', b'z');
pub const HB_OT_TAG_VAR_AXIS_SLANT: hb_tag_t = HB_TAG(b's', b'l', b'n', b't');
pub const HB_OT_TAG_VAR_AXIS_WIDTH: hb_tag_t = HB_TAG(b'w', b'd', b't', b'h');
pub const HB_OT_TAG_VAR_AXIS_WEIGHT: hb_tag_t = HB_TAG(b'w', b'g', b'h', b't');

pub type hb_ot_var_axis_flags_t = u32;
pub const HB_OT_VAR_AXIS_FLAG_HIDDEN: hb_ot_var_axis_flags_t = 0x01;

#[derive(Debug)]
#[repr(C)]
pub struct hb_ot_var_axis_info_t {
    pub axis_index: c_uint,
    pub tag: hb_tag_t,
    pub name_id: hb_ot_name_id_t,
    pub flags: hb_ot_var_axis_flags_t,
    pub min_value: c_float,
    pub default_value: c_float,
    pub max_value: c_float,
    _reserved: c_uint,
}

unsafe extern "C" {
    pub fn hb_ot_var_has_data(face: *mut hb_face_t) -> hb_bool_t;
    pub fn hb_ot_var_get_axis_count(face: *mut hb_face_t) -> c_uint;
    pub fn hb_ot_var_get_axis_infos(
        face: *mut hb_face_t,
        start_offset: c_uint,
        axes_count: *mut c_uint,
        axes_array: *mut hb_ot_var_axis_info_t,
    ) -> c_uint;
    pub fn hb_ot_var_find_axis_info(
        face: *mut hb_face_t,
        axis_tag: hb_tag_t,
        axis_info: *mut hb_ot_var_axis_info_t,
    ) -> hb_bool_t;
}
