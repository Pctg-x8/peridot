use super::*;

pub const U_PARSE_CONTEXT_LEN: usize = 16;

#[repr(C)]
pub struct UParseError {
    pub line: i32,
    pub offset: i32,
    pub preContext: [UChar; U_PARSE_CONTEXT_LEN],
    pub postContext: [UChar; U_PARSE_CONTEXT_LEN],
}
