//! Common Switches

#[inline(always)]
pub fn peridot_build_skip_cdeps() -> bool {
    crate::peridot_build_switch_enable!("SKIP_CDEPS")
}
