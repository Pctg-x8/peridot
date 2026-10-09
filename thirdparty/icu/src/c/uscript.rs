use super::*;
use core::ffi::*;

pub type UScriptCode = c_int;
pub const USCRIPT_HAN: UScriptCode = 17;
pub const USCRIPT_HANGUL: UScriptCode = 18;
pub const USCRIPT_HIRAGANA: UScriptCode = 20;
pub const USCRIPT_KATAKANA: UScriptCode = 22;
pub const USCRIPT_THAI: UScriptCode = 38;

#[link(name = "icuuc", kind = "dylib")]
unsafe extern "C" {
    #[link_name = symbol_rename!(uscript_getScript)]
    pub fn uscript_getScript(codepoint: UChar32, err: *mut UErrorCode) -> UScriptCode;
}
