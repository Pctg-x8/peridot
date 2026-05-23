use super::*;
use core::ffi::*;

#[repr(C)]
pub struct UBreakIterator(super::OpaqueStruct);

pub type UBreakIteratorType = c_int;
pub const UBRK_CHARACTER: UBreakIteratorType = 0;
pub const UBRK_WORD: UBreakIteratorType = 1;
pub const UBRK_LINE: UBreakIteratorType = 2;
pub const UBRK_SENTENCE: UBreakIteratorType = 3;

pub const UBRK_DONE: i32 = -1;

pub type UWordBreak = c_int;
pub const UBRK_WORD_NONE: UWordBreak = 0;
pub const UBRK_WORD_NONE_LIMIT: UWordBreak = 100;
pub const UBRK_WORD_NUMBER: UWordBreak = 100;
pub const UBRK_WORD_NUMBER_LIMIT: UWordBreak = 200;
pub const UBRK_WORD_LETTER: UWordBreak = 200;
pub const UBRK_WORD_LETTER_LIMIT: UWordBreak = 300;
pub const UBRK_WORD_KANA: UWordBreak = 300;
pub const UBRK_WORD_KANA_LIMIT: UWordBreak = 400;
pub const UBRK_WORD_IDEO: UWordBreak = 400;
pub const UBRK_WORD_IDEO_LIMIT: UWordBreak = 500;

pub type ULineBreakTag = c_int;
pub const UBRK_LINE_SOFT: ULineBreakTag = 0;
pub const UBRK_LINE_SOFT_LIMIT: ULineBreakTag = 100;
pub const UBRK_LINE_HARD: ULineBreakTag = 100;
pub const UBRK_LINE_HARD_LIMIT: ULineBreakTag = 200;

pub type USentenceBreakTag = c_int;
pub const UBRK_SENTENCE_TERM: USentenceBreakTag = 0;
pub const UBRK_SENTENCE_TERM_LIMIT: USentenceBreakTag = 100;
pub const UBRK_SENTENCE_SEP: USentenceBreakTag = 100;
pub const UBRK_SENTENCE_SEP_LIMIT: USentenceBreakTag = 200;

macro_rules! symbol_rename {
    ($name: ident) => {
        concat!(stringify!($name), "_76")
    };
}

#[link(name = "icuuc", kind = "dylib")]
unsafe extern "C" {
    #[link_name = symbol_rename!(ubrk_open)]
    pub fn ubrk_open(
        r#type: UBreakIteratorType,
        locale: *const c_char,
        text: *const UChar,
        textLength: i32,
        status: *mut UErrorCode,
    ) -> *mut UBreakIterator;
    #[link_name = symbol_rename!(ubrk_openRules)]
    pub fn ubrk_openRules(
        rules: *const UChar,
        rulesLength: i32,
        text: *const UChar,
        textLength: i32,
        parseErr: *mut UParseError,
        status: *mut UErrorCode,
    ) -> *mut UBreakIterator;
    #[link_name = symbol_rename!(ubrk_openBinaryRules)]
    pub fn ubrk_openBinaryRules(
        binaryRules: *const u8,
        rulesLength: i32,
        text: *const UChar,
        textLength: i32,
        status: *mut UErrorCode,
    ) -> *mut UBreakIterator;

    #[link_name = symbol_rename!(ubrk_clone)]
    pub fn ubrk_clone(bi: *const UBreakIterator, status: *mut UErrorCode) -> *mut UBreakIterator;

    #[link_name = symbol_rename!(ubrk_close)]
    pub fn ubrk_close(bi: *mut UBreakIterator);

    #[link_name = symbol_rename!(ubrk_setText)]
    pub fn ubrk_setText(
        bi: *mut UBreakIterator,
        text: *const UChar,
        textLength: i32,
        status: *mut UErrorCode,
    );
    #[link_name = symbol_rename!(ubrk_setUText)]
    pub fn ubrk_setUText(bi: *mut UBreakIterator, text: *mut UText, status: *mut UErrorCode);

    #[link_name = symbol_rename!(ubrk_current)]
    pub fn ubrk_current(bi: *const UBreakIterator) -> i32;
    #[link_name = symbol_rename!(ubrk_next)]
    pub fn ubrk_next(bi: *mut UBreakIterator) -> i32;
    #[link_name = symbol_rename!(ubrk_previous)]
    pub fn ubrk_previous(bi: *mut UBreakIterator) -> i32;
    #[link_name = symbol_rename!(ubrk_first)]
    pub fn ubrk_first(bi: *mut UBreakIterator) -> i32;
    #[link_name = symbol_rename!(ubrk_last)]
    pub fn ubrk_last(bi: *mut UBreakIterator) -> i32;
    #[link_name = symbol_rename!(ubrk_preceding)]
    pub fn ubrk_preceding(bi: *mut UBreakIterator, offset: i32) -> i32;
    #[link_name = symbol_rename!(ubrk_following)]
    pub fn ubrk_following(bi: *mut UBreakIterator, offset: i32) -> i32;
    #[link_name = symbol_rename!(ubrk_getAvailable)]
    pub fn ubrk_getAvailable(index: i32) -> *const c_char;
    #[link_name = symbol_rename!(ubrk_countAvailable)]
    pub fn ubrk_countAvailable() -> i32;
    #[link_name = symbol_rename!(ubrk_isBoundary)]
    pub fn ubrk_isBounadry(bi: *mut UBreakIterator, offset: i32) -> UBool;
    #[link_name = symbol_rename!(ubrk_getRuleStatus)]
    pub fn ubrk_getRuleStatus(bi: *mut UBreakIterator) -> i32;
    #[link_name = symbol_rename!(ubrk_RuleStatusVec)]
    pub fn ubrk_getRuleStatusVec(
        bi: *mut UBreakIterator,
        fillInVec: *mut i32,
        capacity: i32,
        status: *mut UErrorCode,
    ) -> i32;
    #[link_name = symbol_rename!(ubrk_LocaleByType)]
    pub fn ubrk_getLocaleByType(
        bi: *const UBreakIterator,
        r#type: ULocDataLocaleType,
        status: *mut UErrorCode,
    ) -> *const c_char;
    #[link_name = symbol_rename!(ubrk_refreshUText)]
    pub fn ubrk_refreshUText(bi: *mut UBreakIterator, text: *mut UText, status: *mut UErrorCode);
    #[link_name = symbol_rename!(ubrk_getBinaryRules)]
    pub fn ubrk_getBinaryRules(
        bi: *mut UBreakIterator,
        binaryRules: *mut u8,
        rulesCapacity: i32,
        status: *mut UErrorCode,
    ) -> i32;
}
