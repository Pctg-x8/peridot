//! harfbuzz script list (part)

use crate::ffi::{HB_TAG_BSTR, HB_TAG_NONE, hb_tag_t};

pub type hb_script_t = hb_tag_t;
pub const HB_SCRIPT_INVALID: hb_script_t = HB_TAG_NONE;
pub const HB_SCRIPT_COMMON: hb_script_t = HB_TAG_BSTR(*b"Zyyy");
pub const HB_SCRIPT_INHERITED: hb_script_t = HB_TAG_BSTR(*b"Zinh");
pub const HB_SCRIPT_UNKNOWN: hb_script_t = HB_TAG_BSTR(*b"Zzzz");
