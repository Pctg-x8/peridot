use core::{cell::UnsafeCell, ptr::NonNull};
use std::path::Path;

pub mod raw;

#[repr(transparent)]
pub struct Error(pub core::ffi::c_int);
impl core::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe { core::ffi::CStr::from_ptr(raw::sqlite3_errstr(self.0)) }.fmt(f)
    }
}

pub trait Resource {
    fn drop(&mut self);
}

pub struct Owned<T: Resource>(pub NonNull<T>);
impl<T: Resource> Drop for Owned<T> {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe { (*self.0.as_ptr()).drop() };
    }
}
impl<T: Resource> core::ops::Deref for Owned<T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0.as_ptr() }
    }
}
impl<T: Resource> core::ops::DerefMut for Owned<T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0.as_ptr() }
    }
}
impl<T: Resource> Owned<T> {
    pub unsafe fn from_ptr(raw: *mut T) -> Option<Self> {
        NonNull::new(raw).map(Self)
    }

    pub fn into_raw(self) -> *mut T {
        self.0.as_ptr()
    }
}

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct OpenFlags : core::ffi::c_int {
        const READONLY = raw::SQLITE_OPEN_READONLY;
        const READWRITE = raw::SQLITE_OPEN_READWRITE;
        const CREATE = raw::SQLITE_OPEN_CREATE;
    }
}

#[repr(transparent)]
pub struct DB(UnsafeCell<raw::sqlite3>);
impl Resource for DB {
    #[inline(always)]
    fn drop(&mut self) {
        let r = unsafe { raw::sqlite3_close(self.0.get_mut()) };
        if r != raw::SQLITE_OK {
            eprintln!("sqlite3_close error: {r}")
        }
    }
}
impl DB {
    pub fn open(path: impl AsRef<Path>, flags: OpenFlags) -> Result<Owned<Self>, Error> {
        let mut db = core::mem::MaybeUninit::uninit();
        let r = unsafe {
            raw::sqlite3_open_v2(
                std::ffi::CString::new(path.as_ref().to_str().expect("invalid path"))
                    .expect("cstringify path")
                    .as_ptr(),
                db.as_mut_ptr(),
                flags.bits(),
                core::ptr::null(),
            )
        };

        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(unsafe { Owned::from_ptr(db.assume_init().cast()).expect("nul db returned") })
        }
    }

    pub fn exec<F>(&mut self, sql: &str, mut callback: F) -> Result<(), Error>
    where
        F: FnMut(
            core::ffi::c_int,
            *mut *mut core::ffi::c_char,
            *mut *mut core::ffi::c_char,
        ) -> core::ffi::c_int,
    {
        extern "C" fn wrapper<
            F: FnMut(
                core::ffi::c_int,
                *mut *mut core::ffi::c_char,
                *mut *mut core::ffi::c_char,
            ) -> core::ffi::c_int,
        >(
            f: *mut core::ffi::c_void,
            a: core::ffi::c_int,
            b: *mut *mut core::ffi::c_char,
            c: *mut *mut core::ffi::c_char,
        ) -> core::ffi::c_int {
            let f = unsafe { &mut *(f as *mut F) };
            f(a, b, c)
        }

        let r = unsafe {
            raw::sqlite3_exec(
                self.0.get_mut(),
                sql.as_ptr().cast(),
                wrapper::<F>,
                core::ptr::from_mut(&mut callback).cast(),
                core::ptr::null_mut(),
            )
        };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    pub fn prepare(&mut self, sql: &str) -> Result<Owned<Statement>, Error> {
        let mut stmt = core::mem::MaybeUninit::uninit();
        let r = unsafe {
            raw::sqlite3_prepare(
                self.0.get_mut(),
                sql.as_ptr().cast(),
                sql.len() as _,
                stmt.as_mut_ptr(),
                core::ptr::null_mut(),
            )
        };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(unsafe { Owned::from_ptr(stmt.assume_init().cast()).expect("nul stmt returned") })
        }
    }
}

#[repr(transparent)]
pub struct Statement(UnsafeCell<raw::sqlite3_stmt>);
impl Resource for Statement {
    #[inline(always)]
    fn drop(&mut self) {
        let r = unsafe { raw::sqlite3_finalize(self.0.get_mut()) };
        if r != raw::SQLITE_OK {
            eprintln!("sqlite3_finalize error: {r}")
        }
    }
}
impl Statement {
    pub fn bind_int(&mut self, index: i32, value: core::ffi::c_int) -> Result<(), Error> {
        let r = unsafe { raw::sqlite3_bind_int(self.0.get_mut(), index, value) };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    pub fn bind_i64(&mut self, index: i32, value: i64) -> Result<(), Error> {
        let r = unsafe { raw::sqlite3_bind_int64(self.0.get_mut(), index, value) };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    pub fn bind_double(&mut self, index: i32, value: f64) -> Result<(), Error> {
        let r = unsafe { raw::sqlite3_bind_double(self.0.get_mut(), index, value) };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    pub fn bind_text(&mut self, index: i32, value: &str) -> Result<(), Error> {
        let r = unsafe {
            raw::sqlite3_bind_text(
                self.0.get_mut(),
                index,
                value.as_ptr().cast(),
                value.len() as _,
                raw::SQLITE_STATIC,
            )
        };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    pub fn bind_blob(&mut self, index: i32, value: &[u8]) -> Result<(), Error> {
        let r = unsafe {
            raw::sqlite3_bind_blob(
                self.0.get_mut(),
                index,
                value.as_ptr().cast(),
                value.len() as _,
                raw::SQLITE_STATIC,
            )
        };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    pub fn bind_null(&mut self, index: i32) -> Result<(), Error> {
        let r = unsafe { raw::sqlite3_bind_null(self.0.get_mut(), index) };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    /// returns whether have a new row
    pub fn step(&mut self) -> Result<bool, Error> {
        let r = unsafe { raw::sqlite3_step(self.0.get_mut()) };
        if r == raw::SQLITE_OK || r == raw::SQLITE_ROW || r == raw::SQLITE_DONE {
            Ok(r == raw::SQLITE_ROW)
        } else {
            Err(Error(r))
        }
    }

    pub fn column_type(&self, index: i32) -> i32 {
        unsafe { raw::sqlite3_column_type(self.0.get(), index) }
    }

    pub fn column_int(&self, index: i32) -> i32 {
        unsafe { raw::sqlite3_column_int(self.0.get(), index) }
    }

    pub fn column_i64(&self, index: i32) -> i64 {
        unsafe { raw::sqlite3_column_int64(self.0.get(), index) }
    }

    pub fn column_double(&self, index: i32) -> f64 {
        unsafe { raw::sqlite3_column_double(self.0.get(), index) }
    }

    pub fn column_bytes(&self, index: i32) -> usize {
        unsafe { raw::sqlite3_column_bytes(self.0.get(), index) as _ }
    }

    pub fn column_text(&self, index: i32) -> *const core::ffi::c_char {
        unsafe { raw::sqlite3_column_text(self.0.get(), index) }.cast()
    }

    pub fn column_blob(&self, index: i32) -> *const core::ffi::c_void {
        unsafe { raw::sqlite3_column_blob(self.0.get(), index) }
    }
}
