//! Common Switches

#[inline(always)]
pub fn peridot_build_watch_skip_cdeps() {
    crate::peridot_build_watch!("SKIP_CDEPS");
}

#[inline(always)]
pub fn peridot_build_skip_cdeps() -> bool {
    crate::peridot_build_switch_enable!("SKIP_CDEPS")
}
