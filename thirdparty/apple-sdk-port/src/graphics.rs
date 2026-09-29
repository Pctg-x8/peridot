use crate::{Object, Owned, raw::*};

#[repr(transparent)]
pub struct Path(CGPath);
impl Object for Path {
    #[inline(always)]
    fn as_typeref(&self) -> CFTypeRef {
        &self.0 as *const _ as _
    }
}
impl Path {
    #[inline(always)]
    pub fn new_rect(rect: CGRect, transform: Option<*const CGAffineTransform>) -> Owned<Self> {
        unsafe {
            Owned::from_ptr_unchecked(CGPathCreateWithRect(
                rect,
                transform.unwrap_or(core::ptr::null()),
            ) as *mut Self)
        }
    }

    pub fn apply<F: FnMut(&CGPathElement)>(&self, mut func: F) {
        extern "C" fn wrapper<F: FnMut(&CGPathElement)>(
            info: *mut core::ffi::c_void,
            element: *const CGPathElement,
        ) {
            unsafe {
                (*info.cast::<F>())(&*element);
            }
        }

        self.apply_raw(&mut func as *mut _ as _, wrapper::<F>);
    }

    #[inline(always)]
    pub fn apply_raw(&self, info: *mut core::ffi::c_void, function: CGPathApplierFunction) {
        unsafe { CGPathApply(&self.0, info, function) }
    }
}
