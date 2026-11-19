use super::*;

pub type FT_LcdFilter = FT_UInt;
pub const FT_LCD_FILTER_NONE: FT_LcdFilter = 0;
pub const FT_LCD_FILTER_DEFAULT: FT_LcdFilter = 1;
pub const FT_LCD_FILTER_LIGHT: FT_LcdFilter = 2;
pub const FT_LCD_FILTER_LEGACY1: FT_LcdFilter = 3;
pub const FT_LCD_FILTER_LEGACY: FT_LcdFilter = 16;

unsafe extern "system" {
    pub fn FT_Library_SetLcdFilter(library: FT_Library, filter: FT_LcdFilter) -> FT_Error;
    pub fn FT_Library_SetLcdFilterWeights(
        library: FT_Library,
        weights: *mut core::ffi::c_uchar,
    ) -> FT_Error;
}

pub type FT_LcdFiveTapFilter = [FT_Byte; 5];

unsafe extern "system" {
    pub fn FT_Library_SelectLcdGeometry(library: FT_Library, sub: *mut FT_Vector) -> FT_Error;
}
