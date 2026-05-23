use super::*;
use core::ffi::*;

#[repr(C)]
pub struct UText {
    pub magic: u32,
    pub flags: i32,
    pub providerProperties: i32,
    pub sizeOfStruct: i32,
    pub chunkNativeLimit: i64,
    pub extraSize: i32,
    pub nativeIndexingLimit: i32,
    pub chunkNativeStart: i64,
    pub chunkOffset: i32,
    pub chunkLength: i32,
    pub chunkContents: *const UChar,
    pub pFuncs: *const UTextFuncs,
    pub pExtra: *mut c_void,
    pub context: *const c_void,
    pub p: *const c_void,
    pub q: *const c_void,
    pub r: *const c_void,
    pub privP: *mut c_void,
    pub a: i64,
    pub b: i32,
    pub c: i32,
    pub privA: i64,
    pub privB: i32,
    pub privC: i32,
}

pub const UTEXT_PROVIDER_LENGTH_IS_EXPENSIVE: c_int = 1;
pub const UTEXT_PROVIDER_STABLE_CHUNKS: c_int = 2;
pub const UTEXT_PROVIDER_WRITABLE: c_int = 3;
pub const UTEXT_PROVIDER_HAS_META_DATA: c_int = 4;
pub const UTEXT_PROVIDER_OWNS_TEXT: c_int = 5;

pub type UTextClone = extern "C" fn(
    dest: *mut UText,
    src: *const UText,
    deep: UBool,
    status: *mut UErrorCode,
) -> *mut UText;
pub type UTextNativeLength = extern "C" fn(ut: *mut UText) -> i64;
pub type UTextAccess = extern "C" fn(ut: *mut UText, nativeIndex: i64, forward: UBool) -> UBool;
pub type UTextExtract = extern "C" fn(
    ut: *mut UText,
    nativeStart: i64,
    nativeLimit: i64,
    dest: *mut UChar,
    destCapacity: i32,
    status: *mut UErrorCode,
) -> i32;
pub type UTextReplace = extern "C" fn(
    ut: *mut UText,
    nativeStart: i64,
    nativeLimit: i64,
    replacementText: *const UChar,
    replacementLength: i32,
    status: *mut UErrorCode,
) -> i32;
pub type UTextCopy = extern "C" fn(
    ut: *mut UText,
    nativeStart: i64,
    nativeLimit: i64,
    nativeDest: i64,
    r#move: UBool,
    status: *mut UErrorCode,
);
pub type UTextMapOffsetToNative = extern "C" fn(ut: *const UText) -> i64;
pub type UTextMapNativeIndexToUTF16 = extern "C" fn(ut: *const UText, nativeIndex: i64) -> i32;
pub type UTextClose = extern "C" fn(ut: *mut UText);

#[repr(C)]
pub struct UTextFuncs {
    pub tableSize: i32,
    pub reserved1: i32,
    pub reserved2: i32,
    pub reserved3: i32,
    pub clone: UTextClone,
    pub nativeLength: UTextNativeLength,
    pub access: UTextAccess,
    pub extract: UTextExtract,
    pub replace: UTextReplace,
    pub copy: UTextCopy,
    pub mapOffsetToNative: UTextMapOffsetToNative,
    pub mapNativeIndexToUTF16: UTextMapNativeIndexToUTF16,
    pub close: UTextClose,
    pub spare1: UTextClose,
    pub spare2: UTextClose,
    pub spare3: UTextClose,
}

pub const UTEXT_MAGIC: u32 = 0x345ad82c;

pub const UTEXT_INITIALIZER: UText = UText {
    magic: UTEXT_MAGIC,
    flags: 0,
    providerProperties: 0,
    sizeOfStruct: core::mem::size_of::<UText>() as _,
    chunkNativeLimit: 0,
    extraSize: 0,
    nativeIndexingLimit: 0,
    chunkNativeStart: 0,
    chunkOffset: 0,
    chunkLength: 0,
    chunkContents: core::ptr::null(),
    pFuncs: core::ptr::null_mut(),
    pExtra: core::ptr::null_mut(),
    context: core::ptr::null(),
    p: core::ptr::null(),
    q: core::ptr::null(),
    r: core::ptr::null(),
    privP: core::ptr::null_mut(),
    a: 0,
    b: 0,
    c: 0,
    privA: 0,
    privB: 0,
    privC: 0,
};

unsafe extern "C" {
    pub fn utext_close(ut: *mut UText) -> *mut UText;

    pub fn utext_openUTF8(
        ut: *mut UText,
        s: *const c_char,
        length: i64,
        status: *mut UErrorCode,
    ) -> *mut UText;
    pub fn utext_openUChars(
        ut: *mut UText,
        s: *const UChar,
        length: i64,
        status: *mut UErrorCode,
    ) -> *mut UText;

    pub fn utext_clone(
        dest: *mut UText,
        src: *const UText,
        deep: UBool,
        readOnly: UBool,
        status: *mut UErrorCode,
    ) -> *mut UText;
    pub fn utext_equals(a: *const UText, b: *const UText) -> UBool;
    pub fn utext_nativeLength(ut: *mut UText) -> i64;
    pub fn utext_isLengthExpensive(ut: *const UText) -> UBool;

    pub fn utext_char32At(ut: *mut UText, nativeIndex: i64) -> UChar32;
    pub fn utext_current32(ut: *mut UText) -> UChar32;
    pub fn utext_next32(ut: *mut UText) -> UChar32;
    pub fn unext_previous32(ut: *mut UText) -> UChar32;
    pub fn utext_next32From(ut: *mut UText, nativeIndex: i64) -> UChar32;
    pub fn utext_previous32From(ut: *mut UText, nativeIndex: i64) -> UChar32;
    pub fn utext_getNativeIndex(ut: *const UText) -> i64;
    pub fn utext_setNativeIndex(ut: *mut UText, nativeIndex: i64);
    pub fn utext_moveIndex32(ut: *mut UText, delta: i32) -> UBool;
    pub fn utext_getPreviousNativeIndex(ut: *mut UText) -> i64;
    pub fn utext_extract(
        ut: *mut UText,
        nativeStart: i64,
        nativeLimit: i64,
        dest: *mut UChar,
        destCapacity: i32,
        status: *mut UErrorCode,
    ) -> i32;

    pub fn utext_isWritable(ut: *const UText) -> UBool;
    pub fn utext_hasMetaData(ut: *const UText) -> UBool;
    pub fn utext_replace(
        ut: *mut UText,
        nativeStart: i64,
        nativeLimit: i64,
        replacementText: *const UChar,
        replacementLength: i32,
        status: *mut UErrorCode,
    ) -> i32;
    pub fn utext_copy(
        ut: *mut UText,
        nativeStart: i64,
        nativeLimit: i64,
        destIndex: i64,
        r#move: UBool,
        status: *mut UErrorCode,
    );
    pub fn utext_freeze(ut: *mut UText);

    pub fn utext_setup(ut: *mut UText, extraSpace: i32, status: *mut UErrorCode) -> *mut UText;
}
