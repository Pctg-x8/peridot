use core::{ffi::CStr, mem::MaybeUninit, ptr::NonNull};

pub mod c;

#[repr(transparent)]
pub struct Error(c::UErrorCode);
impl core::fmt::Debug for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let name = unsafe { CStr::from_ptr(c::u_errorName(self.0)) };
        write!(f, "{name:?}({})", self.0)
    }
}

#[repr(i32)]
pub enum BreakIteratorType {
    Character = c::UBRK_CHARACTER,
    Word = c::UBRK_WORD,
    Line = c::UBRK_LINE,
    Sentence = c::UBRK_SENTENCE,
}

#[repr(transparent)]
pub struct BreakIterator<'a>(
    NonNull<c::UBreakIterator>,
    core::marker::PhantomData<&'a [u16]>,
);
impl Drop for BreakIterator<'_> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            c::ubrk_close(self.0.as_ptr());
        }
    }
}
impl<'a> BreakIterator<'a> {
    #[inline(always)]
    pub fn new(
        r#type: BreakIteratorType,
        locale: Option<&CStr>,
        text: &'a [c::UChar],
    ) -> Result<Self, Error> {
        let mut status = 0;
        let p = unsafe {
            c::ubrk_open(
                r#type as _,
                locale.map_or(core::ptr::null(), CStr::as_ptr),
                text.as_ptr(),
                text.len().try_into().expect("too long text"),
                &mut status,
            )
        };

        match NonNull::new(p) {
            Some(x) => Ok(Self(x, core::marker::PhantomData)),
            None => Err(Error(status)),
        }
    }

    #[inline(always)]
    pub fn first(&mut self) -> i32 {
        unsafe { c::ubrk_first(self.0.as_ptr()) }
    }

    #[inline(always)]
    pub fn next(&mut self) -> Option<i32> {
        match unsafe { c::ubrk_next(self.0.as_ptr()) } {
            r if r == c::UBRK_DONE => None,
            r => Some(r),
        }
    }
}
impl<'a> IntoIterator for BreakIterator<'a> {
    type IntoIter = BreakIter<'a>;
    type Item = i32;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        BreakIter {
            iterator: self,
            first_call: false,
        }
    }
}

pub struct BreakIter<'a> {
    iterator: BreakIterator<'a>,
    first_call: bool,
}
impl Iterator for BreakIter<'_> {
    type Item = i32;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        if !core::mem::replace(&mut self.first_call, true) {
            // first call
            return Some(self.iterator.first());
        }

        return self.iterator.next();
    }
}

#[inline(always)]
pub fn get_script(codepoint: c::UChar32) -> Result<c::UScriptCode, Error> {
    let mut e = 0;
    let r = unsafe { c::uscript_getScript(codepoint, &mut e) };
    if c::U_FAILURE(e) {
        Err(Error(e))
    } else {
        Ok(r)
    }
}
