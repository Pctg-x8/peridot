pub mod raw;

pub trait Destruct {
    unsafe fn destruct(&mut self);
}

pub trait RefCounted: Destruct {
    fn reference(&mut self);
}

#[repr(transparent)]
pub struct Owned<T: Destruct>(core::ptr::NonNull<T>);
impl<T: Destruct> Drop for Owned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { T::destruct(self.0.as_mut()) }
    }
}
impl<T: RefCounted> Clone for Owned<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        T::reference(unsafe { &mut *self.0.as_ptr() });
        Owned(self.0)
    }
}
impl<T: Destruct> core::ops::Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}
impl<T: Destruct> core::ops::DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}
impl<T: Destruct> Owned<T> {
    #[inline(always)]
    pub const fn into_raw(self) -> *mut T {
        let ptr = self.0.as_ptr();
        core::mem::forget(self);
        ptr
    }
}

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

pub type Pattern = raw::FcPattern;
impl Destruct for Pattern {
    #[inline(always)]
    unsafe fn destruct(&mut self) {
        unsafe {
            raw::FcPatternDestroy(self);
        }
    }
}
impl RefCounted for Pattern {
    #[inline(always)]
    fn reference(&mut self) {
        unsafe {
            raw::FcPatternReference(self);
        }
    }
}
impl Pattern {
    #[inline(always)]
    pub fn new() -> Option<Owned<Self>> {
        unsafe { core::ptr::NonNull::new(raw::FcPatternCreate().cast()).map(Owned) }
    }

    #[inline(always)]
    pub fn add<T: PatternValue>(&mut self, key: &core::ffi::CStr, value: &T) -> Result<(), ()> {
        value.add(self, key)
    }

    #[inline(always)]
    pub fn print(&self) {
        unsafe { raw::FcPatternPrint(self) }
    }
}

pub trait PatternValue {
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()>;
}
impl PatternValue for core::ffi::c_int {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        match unsafe { raw::FcPatternAddInteger(pattern, key.as_ptr(), *self) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
impl PatternValue for bool {
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
impl PatternValue for core::ffi::c_double {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        match unsafe { raw::FcPatternAddDouble(pattern, key.as_ptr(), *self) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
impl PatternValue for core::ffi::CStr {
    #[inline(always)]
    fn add(&self, pattern: &mut Pattern, key: &core::ffi::CStr) -> Result<(), ()> {
        match unsafe { raw::FcPatternAddString(pattern, key.as_ptr(), self.as_ptr().cast()) } {
            raw::FcTrue => Ok(()),
            _ => Err(()),
        }
    }
}
