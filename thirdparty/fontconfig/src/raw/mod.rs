#![allow(non_upper_case_globals)]

/// https://doc.rust-lang.org/nomicon/ffi.html#representing-opaque-structs
macro_rules! ExternOpaqueStruct {
    ($v: vis struct $t: ident) => {
        #[repr(C)]
        $v struct $t {
            _data: [u8; 0],
            _marker: core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
        }
    };
}

#[cfg(feature = "with-freetype")]
mod freetype;
#[cfg(feature = "with-freetype")]
pub use self::freetype::*;

pub type FcChar8 = core::ffi::c_uchar;
pub type FcChar16 = core::ffi::c_ushort;
pub type FcChar32 = core::ffi::c_uint;
pub type FcBool = core::ffi::c_int;

pub const FC_MAJOR: core::ffi::c_int = 2;
pub const FC_MINOR: core::ffi::c_int = 13;
pub const FC_REVISION: core::ffi::c_int = 1;
pub const FC_VERSION: core::ffi::c_int = FC_MAJOR * 10000 + FC_MINOR * 100 + FC_REVISION;

pub const FC_CACHE_VERSION_NUMBER: core::ffi::c_int = 7;
macro_rules! FC_CACHE_VERSION {
    () => {
        7
    };
}

pub const FcFalse: FcBool = 0;
pub const FcTrue: FcBool = 1;
pub const FcDontCare: FcBool = 2;

pub const FC_FAMILY: &'static core::ffi::CStr = c"family";
pub const FC_STYLE: &'static core::ffi::CStr = c"style";
pub const FC_SLANT: &'static core::ffi::CStr = c"slant";
pub const FC_WEIGHT: &'static core::ffi::CStr = c"weight";
pub const FC_SIZE: &'static core::ffi::CStr = c"size";
pub const FC_ASPECT: &'static core::ffi::CStr = c"aspect";
pub const FC_PIXEL_SIZE: &'static core::ffi::CStr = c"pixelsize";
pub const FC_SPACING: &'static core::ffi::CStr = c"spacing";
pub const FC_FOUNDRY: &'static core::ffi::CStr = c"foundry";
pub const FC_ANTIALIAS: &'static core::ffi::CStr = c"antialias";
pub const FC_HINTING: &'static core::ffi::CStr = c"hinting";
pub const FC_HINT_STYLE: &'static core::ffi::CStr = c"hintstyle";
pub const FC_VERTICAL_LAYOUT: &'static core::ffi::CStr = c"verticallayout";
pub const FC_AUTOHINT: &'static core::ffi::CStr = c"autohint";
pub const FC_GLOBAL_ADVANCE: &'static core::ffi::CStr = c"globaladvance";
pub const FC_WIDTH: &'static core::ffi::CStr = c"width";
pub const FC_FILE: &'static core::ffi::CStr = c"file";
pub const FC_INDEX: &'static core::ffi::CStr = c"index";
pub const FC_FT_FACE: &'static core::ffi::CStr = c"ftface";
pub const FC_RASTERIZER: &'static core::ffi::CStr = c"rasterizer";
pub const FC_OUTLINE: &'static core::ffi::CStr = c"outline";
pub const FC_SCALABLE: &'static core::ffi::CStr = c"scalable";
pub const FC_COLOR: &'static core::ffi::CStr = c"color";
pub const FC_VARIABLE: &'static core::ffi::CStr = c"variable";
pub const FC_SCALE: &'static core::ffi::CStr = c"scale";
pub const FC_SYMBOL: &'static core::ffi::CStr = c"symbol";
pub const FC_DPI: &'static core::ffi::CStr = c"dpi";
pub const FC_RGBA: &'static core::ffi::CStr = c"rgba";
pub const FC_MINSPACE: &'static core::ffi::CStr = c"minspace";
pub const FC_SOURCE: &'static core::ffi::CStr = c"source";
pub const FC_CHARSET: &'static core::ffi::CStr = c"charset";
pub const FC_LANG: &'static core::ffi::CStr = c"lang";
pub const FC_FONTVERSION: &'static core::ffi::CStr = c"fontversion";
pub const FC_FULLNAME: &'static core::ffi::CStr = c"fullname";
pub const FC_FAMILYLANG: &'static core::ffi::CStr = c"familylang";
pub const FC_STYLELANG: &'static core::ffi::CStr = c"stylelang";
pub const FC_FULLNAMELANG: &'static core::ffi::CStr = c"fullnamelang";
pub const FC_CAPABILITY: &'static core::ffi::CStr = c"capability";
pub const FC_FONTFORMAT: &'static core::ffi::CStr = c"fontformat";
pub const FC_EMBOLDEN: &'static core::ffi::CStr = c"embolden";
pub const FC_EMBEDDED_BITMAP: &'static core::ffi::CStr = c"embeddedbitmap";
pub const FC_DECORATIVE: &'static core::ffi::CStr = c"decorative";
pub const FC_LCD_FILTER: &'static core::ffi::CStr = c"lcdfilter";
pub const FC_FONT_FEATURES: &'static core::ffi::CStr = c"fontfeatures";
pub const FC_FONT_VARIATIONS: &'static core::ffi::CStr = c"fontvariations";
pub const FC_NAMELANG: &'static core::ffi::CStr = c"namelang";
pub const FC_PRGNAME: &'static core::ffi::CStr = c"prgname";
pub const FC_HASH: &'static core::ffi::CStr = c"hash";
pub const FC_POSTSCRIPT_NAME: &'static core::ffi::CStr = c"postscriptname";

pub const FC_CACHE_SUFFIX: &str = concat!(".cache-", FC_CACHE_VERSION!(), "\x00");
pub const FC_DIR_CACHE_FILE: &str = concat!("fonts.cache-", FC_CACHE_VERSION!(), "\x00");
pub const FC_USER_CACHE_FILE: &str = concat!(".fonts.cache-", FC_CACHE_VERSION!(), "\x00");

pub const FC_CHARWIDTH: &'static core::ffi::CStr = c"charwidth";
pub const FC_CHAR_WIDTH: &'static core::ffi::CStr = FC_CHARWIDTH;
pub const FC_CHAR_HEIGHT: &'static core::ffi::CStr = c"charheight";
pub const FC_MATRIX: &'static core::ffi::CStr = c"matrix";

pub const FC_WEIGHT_THIN: core::ffi::c_int = 0;
pub const FC_WEIGHT_EXTRALIGHT: core::ffi::c_int = 40;
pub const FC_WEIGHT_ULTRALIGHT: core::ffi::c_int = FC_WEIGHT_EXTRALIGHT;
pub const FC_WEIGHT_LIGHT: core::ffi::c_int = 50;
pub const FC_WEIGHT_DEMILIGHT: core::ffi::c_int = 55;
pub const FC_WEIGHT_SEMILIGHT: core::ffi::c_int = FC_WEIGHT_DEMILIGHT;
pub const FC_WEIGHT_BOOK: core::ffi::c_int = 75;
pub const FC_WEIGHT_REGULAR: core::ffi::c_int = 80;
pub const FC_WEIGHT_NORMAL: core::ffi::c_int = FC_WEIGHT_REGULAR;
pub const FC_WEIGHT_MEDIUM: core::ffi::c_int = 100;
pub const FC_WEIGHT_DEMIBOLD: core::ffi::c_int = 180;
pub const FC_WEIGHT_SEMIBOLD: core::ffi::c_int = FC_WEIGHT_DEMIBOLD;
pub const FC_WEIGHT_BOLD: core::ffi::c_int = 200;
pub const FC_WEIGHT_EXTRABOLD: core::ffi::c_int = 205;
pub const FC_WEIGHT_ULTRABOLD: core::ffi::c_int = FC_WEIGHT_EXTRABOLD;
pub const FC_WEIGHT_BLACK: core::ffi::c_int = 210;
pub const FC_WEIGHT_HEAVY: core::ffi::c_int = FC_WEIGHT_BLACK;
pub const FC_WEIGHT_EXTRABLACK: core::ffi::c_int = 215;
pub const FC_WEIGHT_ULTRABLACK: core::ffi::c_int = FC_WEIGHT_EXTRABLACK;

pub const FC_SLANT_ROMAN: core::ffi::c_int = 0;
pub const FC_SLANT_ITALIC: core::ffi::c_int = 100;
pub const FC_SLANT_OBLIQUE: core::ffi::c_int = 110;

pub const FC_WIDTH_ULTRACONDENSED: core::ffi::c_int = 50;
pub const FC_WIDTH_EXTRACONDENSED: core::ffi::c_int = 63;
pub const FC_WIDTH_CONDENSED: core::ffi::c_int = 75;
pub const FC_WIDTH_SEMICONDENSED: core::ffi::c_int = 87;
pub const FC_WIDTH_NORMAL: core::ffi::c_int = 100;
pub const FC_WIDTH_SEMIEXPANDED: core::ffi::c_int = 113;
pub const FC_WIDTH_EXPANDED: core::ffi::c_int = 125;
pub const FC_WIDTH_EXTRAEXPANDED: core::ffi::c_int = 150;
pub const FC_WIDTH_ULTRAEXPANDED: core::ffi::c_int = 200;

pub const FC_PROPORTIONAL: core::ffi::c_int = 0;
pub const FC_DUAL: core::ffi::c_int = 90;
pub const FC_MONO: core::ffi::c_int = 100;
pub const FC_CHARCELL: core::ffi::c_int = 110;

pub const FC_RGBA_UNKNOWN: core::ffi::c_int = 0;
pub const FC_RGBA_RGB: core::ffi::c_int = 1;
pub const FC_RGBA_BGR: core::ffi::c_int = 2;
pub const FC_RGBA_VRGB: core::ffi::c_int = 3;
pub const FC_RGBA_VBGR: core::ffi::c_int = 4;
pub const FC_RGBA_NONE: core::ffi::c_int = 5;

pub const FC_HINT_NONE: core::ffi::c_int = 0;
pub const FC_HINT_SLIGHT: core::ffi::c_int = 1;
pub const FC_HINT_MEDIUM: core::ffi::c_int = 2;
pub const FC_HINT_FULL: core::ffi::c_int = 3;

pub const FC_LCD_NONE: core::ffi::c_int = 0;
pub const FC_LCD_DEFAULT: core::ffi::c_int = 1;
pub const FC_LCD_LIGHT: core::ffi::c_int = 2;
pub const FC_LCD_LEGACY: core::ffi::c_int = 3;

pub type FcType = core::ffi::c_int;
pub const FcTypeUnknown: FcType = -1;
pub const FcTypeVoid: FcType = 0;
pub const FcTypeInteger: FcType = 1;
pub const FcTypeDouble: FcType = 2;
pub const FcTypeString: FcType = 3;
pub const FcTypeBool: FcType = 4;
pub const FcTypeMatrix: FcType = 5;
pub const FcTypeCharSet: FcType = 6;
pub const FcTypeFTFace: FcType = 7;
pub const FcTypeLangSet: FcType = 8;
pub const FcTypeRange: FcType = 9;

#[repr(C)]
#[derive(Debug, Clone, PartialEq)]
pub struct FcMatrix {
    pub xx: core::ffi::c_double,
    pub xy: core::ffi::c_double,
    pub yx: core::ffi::c_double,
    pub yy: core::ffi::c_double,
}
impl Default for FcMatrix {
    /// FcMatrixInit
    fn default() -> Self {
        FcMatrix {
            xx: 1.0,
            yy: 1.0,
            xy: 0.0,
            yx: 0.0,
        }
    }
}

ExternOpaqueStruct!(pub struct FcCharSet);

#[repr(C)]
pub struct FcObjectType {
    pub object: *mut core::ffi::c_char,
    pub type_: FcType,
}
#[repr(C)]
pub struct FcConstant {
    pub name: *const FcChar8,
    pub object: *const core::ffi::c_char,
    pub value: core::ffi::c_int,
}

pub type FcResult = core::ffi::c_int;
pub const FcResultMatch: FcResult = 0;
pub const FcResultNoMatch: FcResult = 1;
pub const FcResultTypeMismatch: FcResult = 2;
pub const FcResultNoId: FcResult = 3;
pub const FcResultOutOfMemory: FcResult = 4;

pub type FcValueBinding = core::ffi::c_int;
pub const FcValueBindingWeak: FcValueBinding = 0;
pub const FcValueBindingStrong: FcValueBinding = 1;
pub const FcValueBindingSame: FcValueBinding = 2;

ExternOpaqueStruct!(pub struct FcPattern);
#[repr(C)]
pub struct FcPatternIter {
    pub dummy1: *mut core::ffi::c_void,
    pub dummy2: *mut core::ffi::c_void,
}
ExternOpaqueStruct!(pub struct FcLangSet);
ExternOpaqueStruct!(pub struct FcRange);

#[repr(C)]
pub union FcValueUnion {
    pub s: *const FcChar8,
    pub i: core::ffi::c_int,
    pub b: FcBool,
    pub d: core::ffi::c_double,
    pub m: *const FcMatrix,
    pub c: *const FcCharSet,
    pub f: *mut core::ffi::c_void,
    pub l: *const FcLangSet,
    pub r: *const FcRange,
}
#[repr(C)]
pub struct FcValue {
    pub type_: FcType,
    pub u: FcValueUnion,
}

#[repr(C)]
pub struct FcFontSet {
    pub nfont: core::ffi::c_int,
    pub sfont: core::ffi::c_int,
    pub fonts: *mut *mut FcPattern,
}
#[repr(C)]
pub struct FcObjectSet {
    pub nobject: core::ffi::c_int,
    pub sobject: core::ffi::c_int,
    pub objects: *mut *const core::ffi::c_char,
}

pub type FcMatchKind = core::ffi::c_int;
pub const FcMatchPattern: FcMatchKind = 0;
pub const FcMatchFont: FcMatchKind = 1;
pub const FcMatchScan: FcMatchKind = 2;

pub type FcLangResult = core::ffi::c_int;
pub const FcLangEqual: FcLangResult = 0;
pub const FcLangDifferentCountry: FcLangResult = 1;
pub const FcLangDifferentTerritory: FcLangResult = 1;
pub const FcLangDifferentLang: FcLangResult = 2;

pub type FcSetName = core::ffi::c_int;
pub const FcSetSystem: FcSetName = 0;
pub const FcSetApplication: FcSetName = 1;

#[repr(C)]
pub struct FcConfigFileInfoIter {
    pub dummy1: *mut core::ffi::c_void,
    pub dummy2: *mut core::ffi::c_void,
    pub dummy3: *mut core::ffi::c_void,
}

ExternOpaqueStruct!(pub struct FcAtomic);

pub type FcEndian = core::ffi::c_int;
pub const FcEndianBig: FcEndian = 0;
pub const FcEndianLittle: FcEndian = 1;

ExternOpaqueStruct!(pub struct FcConfig);
ExternOpaqueStruct!(pub struct FcFileCache);
ExternOpaqueStruct!(pub struct FcBlanks);
ExternOpaqueStruct!(pub struct FcStrList);
ExternOpaqueStruct!(pub struct FcStrSet);
ExternOpaqueStruct!(pub struct FcCache);

#[link(name = "fontconfig")]
unsafe extern "C" {
    pub unsafe fn FcBlanksCreate() -> *mut FcBlanks;
    pub unsafe fn FcBlanksDestroy(b: *mut FcBlanks);
    pub unsafe fn FcBlanksAdd(b: *mut FcBlanks, ucs4: FcChar32) -> FcBool;
    pub unsafe fn FcBlanksIsMember(b: *mut FcBlanks, ucs4: FcChar32) -> FcBool;

    pub unsafe fn FcCacheDir(c: *const FcCache) -> *const FcChar8;
    pub unsafe fn FcCacheCopySet(c: *const FcCache) -> *mut FcFontSet;
    pub unsafe fn FcCacheSubdir(c: *const FcCache, i: core::ffi::c_int) -> *const FcChar8;
    pub unsafe fn FcCacheNumSubdir(c: *const FcCache) -> core::ffi::c_int;
    pub unsafe fn FcCacheNumFont(c: *const FcCache) -> core::ffi::c_int;
    pub unsafe fn FcDirCacheUnlink(dir: *const FcChar8, config: *mut FcConfig) -> FcBool;
    pub unsafe fn FcDirCacheValid(cache_file: *const FcChar8) -> FcBool;
    pub unsafe fn FcDirCAcheClean(cache_dir: *const FcChar8, verbose: FcBool) -> FcBool;
    pub unsafe fn FcCacheCreateTagFile(config: *const FcConfig);
    pub unsafe fn FcDirCacheCreateUUID(
        dir: *mut FcChar8,
        force: FcBool,
        config: *mut FcConfig,
    ) -> FcBool;
    pub unsafe fn FcDirCacheDeleteUUID(dir: *const FcChar8, config: *mut FcConfig) -> FcBool;

    pub unsafe fn FcConfigHome() -> *mut FcChar8;
    pub unsafe fn FcConfigEnableHome(enable: FcBool) -> FcBool;
    pub unsafe fn FcConfigFilename(url: *const FcChar8) -> *mut FcChar8;
    pub unsafe fn FcConfigCreate() -> *mut FcConfig;
    pub unsafe fn FcConfigReference(config: *mut FcConfig) -> *mut FcConfig;
    pub unsafe fn FcConfigDestroy(config: *mut FcConfig);
    pub unsafe fn FcConfigSetCurrent(config: *mut FcConfig);
    pub unsafe fn FcConfigGetCurrent() -> *mut FcConfig;
    pub unsafe fn FcConfigUptoDate(config: *mut FcConfig) -> FcBool;
    pub unsafe fn FcConfigBuildFonts(config: *mut FcConfig) -> FcBool;
    pub unsafe fn FcConfigGetFontDirs(config: *mut FcConfig) -> *mut FcStrList;
    pub unsafe fn FcConfigGetConfigDirs(config: *mut FcConfig) -> *mut FcStrList;
    pub unsafe fn FcConfigGetConfigFiles(config: *mut FcConfig) -> *mut FcStrList;
    pub unsafe fn FcConfigGetCache(config: *mut FcConfig) -> *mut FcChar8;
    pub unsafe fn FcConfigGetBlanks(config: *mut FcConfig) -> *mut FcBlanks;
    pub unsafe fn FcConfigGetCacheDirs(config: *const FcConfig) -> *mut FcStrList;
    pub unsafe fn FcConfigGetRescanInterval(config: *mut FcConfig) -> core::ffi::c_int;
    pub unsafe fn FcConfigSetRescanInterval(
        config: *mut FcConfig,
        rescan_interval: core::ffi::c_int,
    ) -> FcBool;
    pub unsafe fn FcConfigGetFonts(config: *mut FcConfig, set: FcSetName) -> *mut FcFontSet;
    pub unsafe fn FcConfigAppFontAddFile(config: *mut FcConfig, file: *const FcChar8) -> FcBool;
    pub unsafe fn FcConfigAppFontAddDir(config: *mut FcConfig, dir: *const FcChar8) -> FcBool;
    pub unsafe fn FcConfigAppFontClear(config: *mut FcConfig);
    pub unsafe fn FcConfigSubstituteWithPat(
        config: *mut FcConfig,
        p: *mut FcPattern,
        p_pat: *mut FcPattern,
        kind: FcMatchKind,
    ) -> FcBool;
    pub unsafe fn FcConfigSubstitute(
        config: *mut FcConfig,
        p: *mut FcPattern,
        kind: FcMatchKind,
    ) -> FcBool;
    pub unsafe fn FcConfigGetSysRoot(config: *const FcConfig) -> *const FcChar8;
    pub unsafe fn FcConfigSetSysRoot(config: *mut FcConfig, sysroot: *const FcChar8);
    pub unsafe fn FcConfigFileInfoIterInit(config: *mut FcConfig, iter: *mut FcConfigFileInfoIter);
    pub unsafe fn FcConfigFileInfoIterNext(
        config: *mut FcConfig,
        iter: *mut FcConfigFileInfoIter,
    ) -> FcBool;
    pub unsafe fn FcConfigFileInfoIterGet(
        config: *mut FcConfig,
        iter: *mut FcConfigFileInfoIter,
        name: *mut *mut FcChar8,
        description: *mut *mut FcChar8,
        enabled: *mut FcBool,
    ) -> FcBool;

    pub unsafe fn FcCharSetCreate() -> *mut FcCharSet;
    pub unsafe fn FcCharSetDestroy(fcs: *mut FcCharSet);
    pub unsafe fn FcCharSetAddChar(fcs: *mut FcCharSet, ucs4: FcChar32) -> FcBool;
    pub unsafe fn FcCharSetDelChar(fcs: *mut FcCharSet, ucs4: FcChar32) -> FcBool;
    pub unsafe fn FcCharSetCopy(src: *mut FcCharSet) -> *mut FcCharSet;
    pub unsafe fn FcCharSetEqual(a: *const FcCharSet, b: *const FcCharSet) -> FcBool;
    pub unsafe fn FcCharSetItersect(a: *const FcCharSet, b: *const FcCharSet) -> *mut FcCharSet;
    pub unsafe fn FcCharSetUnion(a: *const FcCharSet, b: *const FcCharSet) -> *mut FcCharSet;
    pub unsafe fn FcCharSetSubtract(a: *const FcCharSet, b: *const FcCharSet) -> *mut FcCharSet;
    pub unsafe fn FcCharSetMerge(
        a: *mut FcCharSet,
        b: *const FcCharSet,
        changed: *mut FcBool,
    ) -> FcBool;
    pub unsafe fn FcCharSetHasChar(fcs: *const FcCharSet, ucs4: FcChar32) -> FcBool;
    pub unsafe fn FcCharSetCount(a: *const FcCharSet) -> FcChar32;
    pub unsafe fn FcCharSetIntersectCount(a: *const FcCharSet, b: *const FcCharSet) -> FcChar32;
    pub unsafe fn FcCharSetSubtractCount(a: *const FcCharSet, b: *const FcCharSet) -> FcChar32;
    pub unsafe fn FcCharSetIsSubset(a: *const FcCharSet, b: *const FcCharSet) -> FcBool;
    pub unsafe fn FcCharSetFirstPage(
        a: *const FcCharSet,
        map: *mut FcChar32,
        next: *mut FcChar32,
    ) -> FcChar32;
    pub unsafe fn FcCharSetNextPage(
        a: *const FcCharSet,
        map: *mut FcChar32,
        next: *mut FcChar32,
    ) -> FcChar32;
    pub unsafe fn FcCharSetCoverage(
        a: *const FcCharSet,
        page: FcChar32,
        result: *mut FcChar32,
    ) -> FcChar32;

    pub unsafe fn FcValuePrint(v: FcValue);
    pub unsafe fn FcPatternPrint(p: *const FcPattern);
    pub unsafe fn FcFontSetPrint(s: *const FcFontSet);

    pub unsafe fn FcGetDefaultLangs() -> *mut FcStrSet;
    pub unsafe fn FcDefaultSubstitute(pattern: *mut FcPattern);

    pub unsafe fn FcFileIsDir(file: *const FcChar8) -> FcBool;
    pub unsafe fn FcFileScan(
        set: *mut FcFontSet,
        dirs: *mut FcStrSet,
        cache: *mut FcFileCache,
        blanks: *mut FcBlanks,
        file: *const FcChar8,
        force: FcBool,
    ) -> FcBool;
    pub unsafe fn FcDirScan(
        set: *mut FcFontSet,
        dirs: *mut FcStrSet,
        cache: *mut FcFileCache,
        blanks: *mut FcBlanks,
        dir: *const FcChar8,
        force: FcBool,
    ) -> FcBool;
    pub unsafe fn FcDirSave(
        set: *mut FcFontSet,
        dirs: *mut FcStrSet,
        dir: *const FcChar8,
    ) -> FcBool;
    pub unsafe fn FcDirCacheLoad(
        dir: *const FcChar8,
        config: *mut FcConfig,
        cache_file: *mut *mut FcChar8,
    ) -> *mut FcCache;
    pub unsafe fn FcDirCacheRescan(dir: *const FcChar8, config: *mut FcConfig) -> *mut FcCache;
    pub unsafe fn FcDirCacheRead(
        dir: *const FcChar8,
        force: FcBool,
        config: *mut FcConfig,
    ) -> *mut FcCache;
    pub unsafe fn FcDirCacheLoadFile(
        cache_file: *const FcChar8,
        file_stat: *mut libc::stat,
    ) -> *mut FcCache;
    pub unsafe fn FcDirCacheUnload(cache: *mut FcCache);

    pub unsafe fn FcFreeTypeQuery(
        file: *const FcChar8,
        id: core::ffi::c_uint,
        blanks: *mut FcBlanks,
        count: *mut core::ffi::c_int,
    ) -> *mut FcPattern;
    pub unsafe fn FcFreeTypeQueryAll(
        file: *const FcChar8,
        id: core::ffi::c_uint,
        blanks: *mut FcBlanks,
        count: *mut core::ffi::c_int,
        set: *mut FcFontSet,
    ) -> core::ffi::c_uint;

    pub unsafe fn FcFontSetCreate() -> *mut FcFontSet;
    pub unsafe fn FcFontSetDestroy(s: *mut FcFontSet);
    pub unsafe fn FcFontSetAdd(s: *mut FcFontSet, font: *mut FcPattern) -> FcBool;

    pub unsafe fn FcInitLoadConfig() -> *mut FcConfig;
    pub unsafe fn FcInitLoadConfigAndFonts() -> *mut FcConfig;
    pub unsafe fn FcInit() -> FcBool;
    pub unsafe fn FcFini();
    pub unsafe fn FcGetVersion() -> core::ffi::c_int;
    pub unsafe fn FcInitReinitialize() -> FcBool;
    pub unsafe fn FcInitBringUptoDate() -> FcBool;
    pub unsafe fn FcGetLangs() -> *mut FcStrSet;
    pub unsafe fn FcLangNormalize(lang: *const FcChar8) -> *mut FcChar8;
    pub unsafe fn FcLangGetCharSet(lang: *const FcChar8) -> *const FcCharSet;
    pub unsafe fn FcLangSetCreate(lang: *const FcChar8) -> *const FcCharSet;
    pub unsafe fn FcLangSetDestroy(ls: *mut FcLangSet);
    pub unsafe fn FcLangSetCopy(ls: *const FcLangSet) -> *mut FcLangSet;
    pub unsafe fn FcLangSetAdd(ls: *mut FcLangSet, lang: *const FcChar8) -> FcBool;
    pub unsafe fn FcLangSetDel(ls: *mut FcLangSet, lang: *const FcChar8) -> FcBool;
    pub unsafe fn FcLangSetHasLang(ls: *const FcLangSet, lang: *const FcChar8) -> FcLangResult;
    pub unsafe fn FcLangSetCompare(lsa: *const FcLangSet, lsb: *const FcLangSet) -> FcLangResult;
    pub unsafe fn FcLangSetContains(lsa: *const FcLangSet, lsb: *const FcLangSet) -> FcBool;
    pub unsafe fn FcLangSetEqual(lsa: *const FcLangSet, lsb: *const FcLangSet) -> FcBool;
    pub unsafe fn FcLangSetHash(ls: *const FcLangSet) -> FcChar32;
    pub unsafe fn FcLangGetLangs(ls: *const FcLangSet) -> *mut FcStrSet;
    pub unsafe fn FcLangSetUnion(a: *const FcLangSet, b: *const FcLangSet) -> *mut FcLangSet;
    pub unsafe fn FcLangSetSubtract(a: *const FcLangSet, b: *const FcLangSet) -> *mut FcLangSet;

    pub unsafe fn FcObjectSetCreate() -> *mut FcObjectSet;
    pub unsafe fn FcObjectSetAdd(os: *mut FcObjectSet, object: *const core::ffi::c_char) -> FcBool;
    pub unsafe fn FcObjectSetDestroy(os: *mut FcObjectSet);
    //FcObjectSetVaBuild
    pub unsafe fn FcObjectSetBuild(first: *const core::ffi::c_char, ...) -> *mut FcObjectSet;
    pub unsafe fn FcFontSetList(
        config: *mut FcConfig,
        sets: *mut *mut FcFontSet,
        nsets: core::ffi::c_int,
        p: *mut FcPattern,
        os: *mut FcObjectSet,
    ) -> *mut FcFontSet;
    pub unsafe fn FcFontList(
        config: *mut FcConfig,
        p: *mut FcPattern,
        os: *mut FcObjectSet,
    ) -> *mut FcFontSet;

    pub unsafe fn FcAtomicCreate(file: *const FcChar8) -> *mut FcAtomic;
    pub unsafe fn FcAtomicLock(atomic: *mut FcAtomic) -> FcBool;
    pub unsafe fn FcAtomicNewFile(atomic: *mut FcAtomic) -> *mut FcChar8;
    pub unsafe fn FcAtomicOrigFile(atomic: *mut FcAtomic) -> *mut FcChar8;
    pub unsafe fn FcAtomicReplaceOrig(atomic: *mut FcAtomic) -> FcBool;
    pub unsafe fn FcAtomicDeleteNew(atomic: *mut FcAtomic);
    pub unsafe fn FcAtomicUnlock(atomic: *mut FcAtomic);
    pub unsafe fn FcAtomicDestroy(atomic: *mut FcAtomic);

    pub unsafe fn FcFontSetMatch(
        config: *mut FcConfig,
        sets: *mut *mut FcFontSet,
        nsets: core::ffi::c_int,
        p: *mut FcPattern,
        result: *mut FcResult,
    ) -> *mut FcPattern;
    pub unsafe fn FcFontMatch(
        config: *mut FcConfig,
        p: *mut FcPattern,
        result: *mut FcResult,
    ) -> *mut FcPattern;
    pub unsafe fn FcFontRenderPrepare(
        config: *mut FcConfig,
        pat: *mut FcPattern,
        font: *mut FcPattern,
    ) -> *mut FcPattern;
    pub unsafe fn FcFontSetSort(
        config: *mut FcConfig,
        sets: *mut *mut FcFontSet,
        nsets: core::ffi::c_int,
        p: *mut FcPattern,
        trim: FcBool,
        csp: *mut *mut FcCharSet,
        result: *mut FcResult,
    ) -> *mut FcFontSet;
    pub unsafe fn FcFontSort(
        config: *mut FcConfig,
        p: *mut FcPattern,
        trim: FcBool,
        csp: *mut *mut FcCharSet,
        result: *mut FcResult,
    ) -> *mut FcFontSet;
    pub unsafe fn FcFontSetSortDestroy(fs: *mut FcFontSet);

    pub unsafe fn FcMatrixCopy(mat: *const FcMatrix) -> *mut FcMatrix;
    pub unsafe fn FcMatrixEqual(mat1: *const FcMatrix, mat2: *const FcMatrix) -> FcBool;
    pub unsafe fn FcMatrixMultiply(reuslt: *mut FcMatrix, a: *const FcMatrix, b: *const FcMatrix);
    pub unsafe fn FcMatrixRotate(m: *mut FcMatrix, c: core::ffi::c_double, s: core::ffi::c_double);
    pub unsafe fn FcMatrixScale(m: *mut FcMatrix, sx: core::ffi::c_double, sy: core::ffi::c_double);
    pub unsafe fn FcMatrixShear(m: *mut FcMatrix, sh: core::ffi::c_double, sv: core::ffi::c_double);

    pub unsafe fn FcNameGetObjectType(object: *const core::ffi::c_char) -> *const FcObjectType;
    pub unsafe fn FcNameGetConstant(string: *const FcChar8) -> *const FcConstant;
    pub unsafe fn FcNameConstant(string: *const FcChar8, result: *mut core::ffi::c_int) -> FcBool;
    pub unsafe fn FcNameParse(name: *const FcChar8) -> *mut FcPattern;
    pub unsafe fn FcNameUnparse(pat: *mut FcPattern) -> *mut FcChar8;

    pub unsafe fn FcPatternCreate() -> *mut FcPattern;
    pub unsafe fn FcPatternDuplicate(p: *const FcPattern) -> *mut FcPattern;
    pub unsafe fn FcPatternReference(p: *mut FcPattern);
    pub unsafe fn FcPatternFilter(p: *mut FcPattern, os: *const FcObjectSet) -> *mut FcPattern;
    pub unsafe fn FcValueDestroy(v: FcValue);
    pub unsafe fn FcValueEqual(va: FcValue, vb: FcValue) -> FcBool;
    pub unsafe fn FcValueSave(v: FcValue) -> FcValue;
    pub unsafe fn FcPatternDestroy(p: *mut FcPattern);
    pub unsafe fn FcPatternObjectCount(pat: *const FcPattern) -> core::ffi::c_int;
    pub unsafe fn FcPatternEqual(pa: *const FcPattern, pb: *const FcPattern) -> FcBool;
    pub unsafe fn FcPatternEqualSubset(
        pa: *const FcPattern,
        pb: *const FcPattern,
        os: *const FcObjectSet,
    ) -> FcBool;
    pub unsafe fn FcPatternHash(p: *const FcPattern) -> FcChar32;
    pub unsafe fn FcPatternAdd(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        value: FcValue,
        append: FcBool,
    ) -> FcBool;
    pub unsafe fn FcPatternAddWeak(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        value: FcValue,
        append: FcBool,
    ) -> FcBool;
    pub unsafe fn FcPatternGet(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        id: core::ffi::c_int,
        v: *mut FcValue,
    ) -> FcResult;
    pub unsafe fn FcPatternGetWithBinding(
        p: *const FcPattern,
        object: *const core::ffi::c_char,
        id: core::ffi::c_int,
        v: *mut FcValue,
        b: *mut FcValueBinding,
    ) -> FcResult;
    pub unsafe fn FcPatternDel(p: *mut FcPattern, object: *const core::ffi::c_char) -> FcBool;
    pub unsafe fn FcPatternRemove(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        id: core::ffi::c_int,
    ) -> FcBool;
    pub unsafe fn FcPatternAddInteger(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        i: core::ffi::c_int,
    ) -> FcBool;
    pub unsafe fn FcPatternAddDouble(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        d: core::ffi::c_double,
    ) -> FcBool;
    pub unsafe fn FcPatternAddString(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        s: *const FcChar8,
    ) -> FcBool;
    pub unsafe fn FcPatternAddMatrix(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        s: *const FcMatrix,
    ) -> FcBool;
    pub unsafe fn FcPatternAddCharSet(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        c: *const FcCharSet,
    ) -> FcBool;
    pub unsafe fn FcPatternAddBool(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        b: FcBool,
    ) -> FcBool;
    pub unsafe fn FcPatternAddLangSet(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        ls: *const FcLangSet,
    ) -> FcBool;
    pub unsafe fn FcPatternAddRange(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        r: *const FcRange,
    ) -> FcBool;
    pub unsafe fn FcPatternGetInteger(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        i: *mut core::ffi::c_int,
    ) -> FcBool;
    pub unsafe fn FcPatternGetDouble(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        d: *mut core::ffi::c_double,
    ) -> FcBool;
    pub unsafe fn FcPatternGetString(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        s: *mut *mut FcChar8,
    ) -> FcBool;
    pub unsafe fn FcPatternGetMatrix(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        s: *mut *mut FcMatrix,
    ) -> FcBool;
    pub unsafe fn FcPatternGetCharSet(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        c: *mut *mut FcCharSet,
    ) -> FcBool;
    pub unsafe fn FcPatternGetBool(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        b: *mut FcBool,
    ) -> FcBool;
    pub unsafe fn FcPatternGetLangSet(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        ls: *mut *mut FcLangSet,
    ) -> FcBool;
    pub unsafe fn FcPatternGetRange(
        p: *mut FcPattern,
        object: *const core::ffi::c_char,
        n: core::ffi::c_int,
        r: *mut *mut FcRange,
    ) -> FcBool;
    //FcPatternVaBuild
    pub unsafe fn FcPatternBuild(p: *mut FcPattern, ...) -> *mut FcPattern;
    pub unsafe fn FcPatternFormat(pat: *mut FcPattern, format: *const FcChar8) -> *mut FcChar8;

    pub unsafe fn FcRangeCreateDouble(
        begin: core::ffi::c_double,
        end: core::ffi::c_double,
    ) -> *mut FcRange;
    pub unsafe fn FcRangeCreateInteger(begin: FcChar32, end: FcChar32) -> *mut FcRange;
    pub unsafe fn FcRangeDestroy(range: *mut FcRange);
    pub unsafe fn FcRangeCopy(r: *const FcRange) -> *mut FcRange;
    pub unsafe fn FcRangeGetDouble(
        range: *const FcRange,
        begin: *mut core::ffi::c_double,
        end: *mut core::ffi::c_double,
    ) -> FcBool;
    pub unsafe fn FcPatternIterStart(pat: *const FcPattern, iter: *mut FcPatternIter);
    pub unsafe fn FcPatternIterNext(pat: *const FcPattern, iter: *mut FcPatternIter) -> FcBool;
    pub unsafe fn FcPatternIterEqual(
        o1: *const FcPattern,
        i1: *mut FcPatternIter,
        p2: *const FcPattern,
        i2: *mut FcPatternIter,
    ) -> FcBool;
    pub unsafe fn FcPatternFindIter(
        pat: *const FcPattern,
        iter: *mut FcPatternIter,
        object: *const core::ffi::c_char,
    ) -> FcBool;
    pub unsafe fn FcPatternIterIsValid(pat: *const FcPattern, iter: *mut FcPatternIter) -> FcBool;
    pub unsafe fn FcPatternIterGetObject(
        pat: *const FcPattern,
        iter: *mut FcPatternIter,
    ) -> *const core::ffi::c_char;
    pub unsafe fn FcPatternIterValudCount(
        pat: *const FcPattern,
        iter: *mut FcPatternIter,
    ) -> core::ffi::c_int;
    pub unsafe fn FcPatternIterGetValue(
        pat: *const FcPattern,
        iter: *mut FcPatternIter,
        id: core::ffi::c_int,
        v: *mut FcValue,
        b: *mut FcValueBinding,
    ) -> FcResult;

    pub unsafe fn FcWeightFromOpenType(ot_weight: core::ffi::c_int) -> core::ffi::c_int;
    pub unsafe fn FcWeightFromOpenTypeDouble(ot_weight: core::ffi::c_double)
    -> core::ffi::c_double;
    pub unsafe fn FcWeightToOpenType(fc_weight: core::ffi::c_int) -> core::ffi::c_int;
    pub unsafe fn FcWeightToOpenTypoeDouble(fc_weight: core::ffi::c_double) -> core::ffi::c_double;

    pub unsafe fn FcStrCopy(s: *const FcChar8) -> *mut FcChar8;
    pub unsafe fn FcStrCopyFilename(s: *const FcChar8) -> *mut FcChar8;
    pub unsafe fn FcStrPlus(s1: *const FcChar8, s2: *const FcChar8) -> *mut FcChar8;
    pub unsafe fn FcStrFree(s: *mut FcChar8);
    pub unsafe fn FcStrDowncase(s: *const FcChar8) -> *mut FcChar8;
    pub unsafe fn FcStrCmpIgnoreCase(s1: *const FcChar8, s2: *const FcChar8) -> core::ffi::c_int;
    pub unsafe fn FcStrCmp(s1: *const FcChar8, s2: *const FcChar8) -> core::ffi::c_int;
    pub unsafe fn FcStrStrIgnoreCase(s1: *const FcChar8, s2: *const FcChar8) -> *const FcChar8;
    pub unsafe fn FcStrStr(s1: *const FcChar8, s2: *const FcChar8) -> *const FcChar8;
    pub unsafe fn FcUtf8ToUcs4(
        src_orig: *const FcChar8,
        dst: *mut FcChar32,
        len: core::ffi::c_int,
    ) -> core::ffi::c_int;
    pub unsafe fn FcUtf8Len(
        string: *const FcChar8,
        len: core::ffi::c_int,
        n_char: *mut core::ffi::c_int,
        wchar: *mut core::ffi::c_int,
    ) -> FcBool;
    pub unsafe fn FcUcs4ToUtf8(ucs4: FcChar32, dest: *mut FcChar8) -> core::ffi::c_int;
    pub unsafe fn FcUtf16ToUcs4(
        src_orig: *const FcChar8,
        endian: FcEndian,
        dst: *mut FcChar32,
        len: core::ffi::c_int,
    ) -> core::ffi::c_int;
    pub unsafe fn FcUtf16Len(
        string: *const FcChar8,
        endian: FcEndian,
        len: core::ffi::c_int,
        nchar: *mut core::ffi::c_int,
        wchar: *mut core::ffi::c_int,
    ) -> FcBool;
    pub unsafe fn FcStrBuildFilename(path: *const FcChar8, ...) -> *mut FcChar8;
    pub unsafe fn FcStrDirname(file: *const FcChar8) -> *mut FcChar8;
    pub unsafe fn FcStrBasename(file: *const FcChar8) -> *mut FcChar8;
    pub unsafe fn FcStrSetCreate() -> *mut FcStrSet;
    pub unsafe fn FcStrSetMember(set: *mut FcStrSet, s: *const FcChar8) -> FcBool;
    pub unsafe fn FcStrSetEqual(sa: *mut FcStrSet, sb: *mut FcStrSet) -> FcBool;
    pub unsafe fn FcStrSetAdd(set: *mut FcStrSet, s: *const FcChar8) -> FcBool;
    pub unsafe fn FcStrSetAddFilename(set: *mut FcStrSet, s: *const FcChar8) -> FcBool;
    pub unsafe fn FcStrSetDel(set: *mut FcStrSet, s: *const FcChar8) -> FcBool;
    pub unsafe fn FcStrSetDestroy(set: *mut FcStrSet);
    pub unsafe fn FcStrListCreate(set: *mut FcStrSet) -> *mut FcStrList;
    pub unsafe fn FcStrListFirst(list: *mut FcStrList);
    pub unsafe fn FcStrListNext(list: *mut FcStrList) -> *mut FcChar8;
    pub unsafe fn FcStrListDone(list: *mut FcStrList);

    pub unsafe fn FcConfigParseAndLoad(
        config: *mut FcConfig,
        file: *const FcChar8,
        complain: FcBool,
    ) -> FcBool;
    pub unsafe fn FcCofnigParseAndLoadFromMemory(
        config: *mut FcConfig,
        buffer: *const FcChar8,
        complain: FcBool,
    ) -> FcBool;
}

pub const FC_CHARSET_MAP_SIZE: usize = 256 / 32;
pub const FC_CHARSET_DONE: FcChar32 = 0xffff_ffff; // -1
pub const FC_UTF8_MAX_LEN: usize = 6;
