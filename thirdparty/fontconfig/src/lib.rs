pub mod raw;

#[inline(always)]
pub unsafe fn init() -> Result<(), ()> {
    if unsafe { raw::FcInit() } == raw::FcTrue {
        Ok(())
    } else {
        Err(())
    }
}

#[inline(always)]
pub unsafe fn fini() {
    unsafe { raw::FcFini() }
}

pub type Config = raw::FcConfig;
impl Config {
    #[inline(always)]
    pub unsafe fn current() -> Option<core::ptr::NonNull<Self>> {
        unsafe { core::ptr::NonNull::new(raw::FcConfigGetCurrent().cast()) }
    }

    #[inline(always)]
    pub unsafe fn substitute(&mut self, pat: &mut Pattern, kind: MatchKind) -> Result<(), ()> {
        match unsafe { raw::FcConfigSubstitute(self, pat, kind as _) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}

#[repr(i32)]
#[derive(Clone, Copy)]
pub enum MatchKind {
    Pattern = raw::FcMatchPattern,
    Font = raw::FcMatchFont,
    Scan = raw::FcMatchScan,
}

pub type Pattern = raw::FcPattern;
impl Pattern {
    #[inline(always)]
    pub fn new() -> Option<core::ptr::NonNull<Self>> {
        unsafe { core::ptr::NonNull::new(raw::FcPatternCreate().cast()) }
    }

    #[inline(always)]
    pub unsafe fn destroy(&mut self) {
        unsafe { raw::FcPatternDestroy(self) }
    }

    #[inline(always)]
    pub unsafe fn reference(&mut self) {
        unsafe { raw::FcPatternReference(self) }
    }

    #[inline(always)]
    pub fn add<T: PatternValueAdd + ?Sized>(
        &mut self,
        key: &core::ffi::CStr,
        value: &T,
    ) -> Result<(), ()> {
        value.add(self, key)
    }

    #[inline(always)]
    pub fn get<'a, T: PatternValueGet<'a>>(
        &'a mut self,
        key: &core::ffi::CStr,
    ) -> Result<Option<T>, raw::FcResult> {
        T::get(self, key)
    }

    #[inline(always)]
    pub fn default_substitute(&mut self) {
        unsafe { raw::FcDefaultSubstitute(self) }
    }

    #[inline(always)]
    pub fn print(&self) {
        unsafe { raw::FcPatternPrint(self) }
    }

    pub const KEY_FAMILY: &core::ffi::CStr = raw::FC_FAMILY;
    pub const KEY_STYLE: &core::ffi::CStr = raw::FC_STYLE;
    pub const KEY_SLANT: &core::ffi::CStr = raw::FC_SLANT;
    pub const KEY_WEIGHT: &core::ffi::CStr = raw::FC_WEIGHT;
    pub const KEY_SIZE: &core::ffi::CStr = raw::FC_SIZE;
    pub const KEY_FILE: &core::ffi::CStr = raw::FC_FILE;
    pub const KEY_INDEX: &core::ffi::CStr = raw::FC_INDEX;
}

pub trait PatternValueAdd {
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()>;
}
pub trait PatternValueGet<'a>
where
    Self: Sized + 'a,
{
    fn get(pattern: &'a mut Pattern, key: &core::ffi::CStr) -> Result<Option<Self>, raw::FcResult>;
}
impl PatternValueAdd for bool {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        match unsafe {
            raw::FcPatternAddBool(
                pattern,
                key.as_ptr(),
                if *self { raw::FcTrue } else { raw::FcFalse },
            )
        } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
impl PatternValueGet<'_> for bool {
    #[inline(always)]
    fn get(pattern: &'_ mut Pattern, key: &core::ffi::CStr) -> Result<Option<Self>, raw::FcResult> {
        let mut value = core::mem::MaybeUninit::uninit();
        match unsafe { raw::FcPatternGetBool(pattern, key.as_ptr(), 0, value.as_mut_ptr()) } {
            raw::FcResultMatch => Ok(Some(unsafe { value.assume_init() } == raw::FcTrue)),
            raw::FcResultNoMatch => Ok(None),
            e => Err(e),
        }
    }
}
impl PatternValueAdd for core::ffi::c_short {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        // widening
        match unsafe { raw::FcPatternAddInteger(pattern, key.as_ptr(), *self as _) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
impl PatternValueAdd for core::ffi::c_ushort {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        // widening
        match unsafe { raw::FcPatternAddInteger(pattern, key.as_ptr(), *self as _) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
impl PatternValueAdd for core::ffi::c_int {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        match unsafe { raw::FcPatternAddInteger(pattern, key.as_ptr(), *self) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
impl PatternValueGet<'_> for core::ffi::c_int {
    #[inline(always)]
    fn get(pattern: &'_ mut Pattern, key: &core::ffi::CStr) -> Result<Option<Self>, raw::FcResult> {
        let mut value = core::mem::MaybeUninit::uninit();
        match unsafe { raw::FcPatternGetInteger(pattern, key.as_ptr(), 0, value.as_mut_ptr()) } {
            raw::FcResultMatch => Ok(Some(unsafe { value.assume_init() })),
            raw::FcResultNoMatch => Ok(None),
            e => Err(e),
        }
    }
}
impl PatternValueAdd for core::ffi::c_double {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        match unsafe { raw::FcPatternAddDouble(pattern, key.as_ptr(), *self) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
impl PatternValueGet<'_> for core::ffi::c_double {
    #[inline(always)]
    fn get(pattern: &'_ mut Pattern, key: &core::ffi::CStr) -> Result<Option<Self>, raw::FcResult> {
        let mut value = core::mem::MaybeUninit::uninit();
        match unsafe { raw::FcPatternGetDouble(pattern, key.as_ptr(), 0, value.as_mut_ptr()) } {
            raw::FcResultMatch => Ok(Some(unsafe { value.assume_init() })),
            raw::FcResultNoMatch => Ok(None),
            e => Err(e),
        }
    }
}
impl PatternValueAdd for core::ffi::CStr {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        match unsafe { raw::FcPatternAddString(pattern, key.as_ptr(), self.as_ptr().cast()) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
impl<'a> PatternValueGet<'a> for &'a core::ffi::CStr {
    #[inline(always)]
    fn get(pattern: &'a mut Pattern, key: &core::ffi::CStr) -> Result<Option<Self>, raw::FcResult> {
        let mut value = core::mem::MaybeUninit::uninit();
        match unsafe { raw::FcPatternGetString(pattern, key.as_ptr(), 0, value.as_mut_ptr()) } {
            raw::FcResultMatch => Ok(Some(unsafe {
                core::ffi::CStr::from_ptr(value.assume_init().cast())
            })),
            raw::FcResultNoMatch => Ok(None),
            e => Err(e),
        }
    }
}
impl PatternValueAdd for std::ffi::CString {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        // deref
        self.as_c_str().add(pattern, key)
    }
}

#[inline(always)]
pub unsafe fn sort(
    config: &mut Config,
    pat: &mut Pattern,
    trim: bool,
    charset: Option<&mut core::mem::MaybeUninit<*mut raw::FcCharSet>>,
) -> Result<core::ptr::NonNull<FontSet>, raw::FcResult> {
    let mut result = core::mem::MaybeUninit::uninit();
    let fontset = unsafe {
        raw::FcFontSort(
            config,
            pat,
            if trim { raw::FcTrue } else { raw::FcFalse },
            charset.map_or(core::ptr::null_mut(), |x| x.as_mut_ptr()),
            result.as_mut_ptr(),
        )
    };
    let result = unsafe { result.assume_init() };
    if result == raw::FcResultMatch {
        Ok(unsafe { core::ptr::NonNull::new_unchecked(fontset) })
    } else {
        Err(result)
    }
}

pub type FontSet = raw::FcFontSet;
impl FontSet {
    #[inline(always)]
    pub unsafe fn destroy(&mut self) {
        unsafe { raw::FcFontSetDestroy(self) }
    }

    #[inline(always)]
    pub fn print(&self) {
        unsafe { raw::FcFontSetPrint(self) }
    }

    #[inline(always)]
    pub const fn fonts_slice(&self) -> &[core::ptr::NonNull<Pattern>] {
        unsafe { core::slice::from_raw_parts(self.fonts.cast(), self.nfont as _) }
    }
}
