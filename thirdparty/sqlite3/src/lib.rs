use core::{cell::UnsafeCell, ptr::NonNull};
use std::path::Path;

pub mod raw;

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Error(pub core::ffi::c_int);
impl core::fmt::Debug for Error {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        errstr(self.0).fmt(f)
    }
}
impl core::fmt::Display for Error {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Debug::fmt(self, f)
    }
}
impl core::error::Error for Error {}

#[repr(transparent)]
pub struct OpenError(*mut raw::sqlite3);
impl Drop for OpenError {
    fn drop(&mut self) {
        let r = unsafe { raw::sqlite3_close(self.0) };
        if r != raw::SQLITE_OK {
            tracing::warn!(reason = ?errstr(r), "sqlite3_close failed");
        }
    }
}
impl core::fmt::Debug for OpenError {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let msg_ptr = if self.0.is_null() {
            // sqlite3_open_v2 has failed to allocate memory
            unsafe { raw::sqlite3_errstr(raw::SQLITE_NOMEM) }
        } else {
            unsafe { raw::sqlite3_errmsg(self.0) }
        };

        unsafe { core::ffi::CStr::from_ptr(msg_ptr) }.fmt(f)
    }
}
impl core::fmt::Display for OpenError {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        core::fmt::Debug::fmt(self, f)
    }
}
impl core::error::Error for OpenError {}

#[derive(Clone, Copy, Debug)]
pub enum ExecResult {
    Ok,
    Aborted,
    Err(Error),
}
impl ExecResult {
    #[inline(always)]
    pub const fn into_result(self) -> Result<(), Error> {
        match self {
            Self::Ok => Ok(()),
            Self::Aborted => Err(Error(raw::SQLITE_ABORT)),
            Self::Err(e) => Err(e),
        }
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
    /// # Safety
    ///
    /// `raw` must be a valid pointer of the value `T`.
    pub unsafe fn from_ptr(raw: *mut T) -> Option<Self> {
        NonNull::new(raw).map(Self)
    }

    /// # Safety
    ///
    /// `raw` must be a valid non-null pointer of the value `T`.
    pub const unsafe fn from_ptr_unchecked(ptr: *mut T) -> Self {
        Self(unsafe { NonNull::new_unchecked(ptr) })
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

bitflags::bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub struct PrepareFlags : core::ffi::c_uint {
        const PERSISTENT = raw::SQLITE_PREPARE_PERSISTENT.cast_unsigned();
        const NORMALIZE = raw::SQLITE_PREPARE_NORMALIZE.cast_unsigned();
        const NO_VTAB = raw::SQLITE_PREPARE_NO_VTAB.cast_unsigned();
        const DONT_LOG = raw::SQLITE_PREPARE_DONT_LOG.cast_unsigned();
        const FROM_DDL = raw::SQLITE_PREPARE_FROM_DDL.cast_unsigned();
    }
}

#[repr(transparent)]
pub struct DB(UnsafeCell<raw::sqlite3>);
impl Resource for DB {
    #[inline(always)]
    fn drop(&mut self) {
        let r = unsafe { raw::sqlite3_close(self.0.get_mut()) };
        if r != raw::SQLITE_OK {
            tracing::warn!(reason = ?errstr(r), "sqlite3_close failed");
        }
    }
}
impl DB {
    pub fn open_v2(
        path: &core::ffi::CStr,
        flags: OpenFlags,
        vfs_name: Option<&core::ffi::CStr>,
    ) -> Result<Owned<Self>, OpenError> {
        let mut db = core::mem::MaybeUninit::uninit();
        let r = unsafe {
            raw::sqlite3_open_v2(
                path.as_ptr(),
                db.as_mut_ptr(),
                flags.bits(),
                vfs_name.map_or(core::ptr::null(), core::ffi::CStr::as_ptr),
            )
        };
        let db = unsafe { db.assume_init() };

        if r != raw::SQLITE_OK {
            Err(OpenError(db))
        } else {
            Ok(unsafe { Owned::from_ptr_unchecked(db.cast()) })
        }
    }

    #[inline(always)]
    pub fn open(path: impl AsRef<Path>, flags: OpenFlags) -> Result<Owned<Self>, OpenError> {
        Self::open_v2(
            &std::ffi::CString::new(path.as_ref().to_str().expect("invalid path"))
                .expect("cstringify path"),
            flags,
            None,
        )
    }

    #[inline(always)]
    pub fn open_vfs(
        path: &core::ffi::CStr,
        flags: OpenFlags,
        vfs_name: &core::ffi::CStr,
    ) -> Result<Owned<Self>, OpenError> {
        Self::open_v2(path, flags, Some(vfs_name))
    }

    #[inline(always)]
    pub fn errmsg(&self) -> Option<&core::ffi::CStr> {
        let p = unsafe { raw::sqlite3_errmsg(self.0.get()) };
        if p.is_null() {
            None
        } else {
            Some(unsafe { core::ffi::CStr::from_ptr(p) })
        }
    }

    #[inline]
    pub fn exec(&mut self, sql: &core::ffi::CStr) -> ExecResult {
        let r = unsafe {
            raw::sqlite3_exec(
                self.0.get_mut(),
                sql.as_ptr(),
                None,
                core::ptr::null_mut(),
                core::ptr::null_mut(),
            )
        };

        if r == raw::SQLITE_OK {
            ExecResult::Ok
        } else if r == raw::SQLITE_ABORT {
            ExecResult::Aborted
        } else {
            ExecResult::Err(Error(r))
        }
    }

    #[inline]
    pub fn prepare(&self, sql: &str, flags: PrepareFlags) -> Result<Owned<Statement>, Error> {
        assert!(!sql.is_empty());

        let mut stmt = core::mem::MaybeUninit::uninit();
        let r = unsafe {
            raw::sqlite3_prepare_v3(
                self.0.get(),
                sql.as_ptr().cast(),
                sql.len() as _,
                flags.bits(),
                stmt.as_mut_ptr(),
                core::ptr::null_mut(),
            )
        };

        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(unsafe { Owned::from_ptr_unchecked(stmt.assume_init().cast()) })
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
            tracing::warn!(reason = ?errstr(r), "sqlite3_finalize failed");
        }
    }
}
impl Statement {
    #[inline(always)]
    pub fn bind_int(&mut self, index: i32, value: core::ffi::c_int) -> Result<(), Error> {
        let r = unsafe { raw::sqlite3_bind_int(self.0.get_mut(), index, value) };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    pub fn bind_i64(&mut self, index: i32, value: i64) -> Result<(), Error> {
        let r = unsafe { raw::sqlite3_bind_int64(self.0.get_mut(), index, value) };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    #[inline(always)]
    pub fn bind_double(&mut self, index: i32, value: f64) -> Result<(), Error> {
        let r = unsafe { raw::sqlite3_bind_double(self.0.get_mut(), index, value) };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    /// # Safety
    ///
    /// `value` must be valid until the query is executed.
    #[inline(always)]
    pub unsafe fn bind_text(&mut self, index: i32, value: &str) -> Result<(), Error> {
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

    #[inline(always)]
    pub fn bind_text_copied(&mut self, index: i32, value: &str) -> Result<(), Error> {
        let r = unsafe {
            raw::sqlite3_bind_text(
                self.0.get_mut(),
                index,
                value.as_ptr().cast(),
                value.len() as _,
                raw::SQLITE_TRANSIENT,
            )
        };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    /// # Safety
    ///
    /// `value` must be avlid until the query is executed.
    #[inline(always)]
    pub unsafe fn bind_blob(&mut self, index: i32, value: &[u8]) -> Result<(), Error> {
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

    #[inline(always)]
    pub fn bind_blob_copied(&mut self, index: i32, value: &[u8]) -> Result<(), Error> {
        let r = unsafe {
            raw::sqlite3_bind_blob(
                self.0.get_mut(),
                index,
                value.as_ptr().cast(),
                value.len() as _,
                raw::SQLITE_TRANSIENT,
            )
        };
        if r != raw::SQLITE_OK {
            Err(Error(r))
        } else {
            Ok(())
        }
    }

    #[inline(always)]
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

    /// mutability: this method may attempt to convert the datatype of the result.
    ///
    /// # Safety
    ///
    /// * If the SQL statement does not currently point to a valid row, or if the column index is out of range, the result is undefined.
    /// * These routines may only be called when the most recent call to [`sqlite3_step`] has returned [`SQLITE_ROW`] and neither [`sqlite3_reset`] nor [`sqlite3_finalize`] have been called subsequently.
    ///   * If any of these routines are called after [`sqlite3_reset`] or [`sqlite3_finalize`] or after [`sqlite3_step`] has returned something other than [`SQLITE_ROW`], the results are undefined.
    /// * If [`sqlite3_step`] or [`sqlite3_reset`] or [`sqlite3_finalize`] are called from a different thread while any of these routines are pending, then the results are undefined.
    #[inline(always)]
    pub unsafe fn column_int(&mut self, index: i32) -> i32 {
        unsafe { raw::sqlite3_column_int(self.0.get_mut(), index) }
    }

    /// mutability: this method may attempt to convert the datatype of the result.
    ///
    /// # Safety
    ///
    /// * If the SQL statement does not currently point to a valid row, or if the column index is out of range, the result is undefined.
    /// * These routines may only be called when the most recent call to [`sqlite3_step`] has returned [`SQLITE_ROW`] and neither [`sqlite3_reset`] nor [`sqlite3_finalize`] have been called subsequently.
    ///   * If any of these routines are called after [`sqlite3_reset`] or [`sqlite3_finalize`] or after [`sqlite3_step`] has returned something other than [`SQLITE_ROW`], the results are undefined.
    /// * If [`sqlite3_step`] or [`sqlite3_reset`] or [`sqlite3_finalize`] are called from a different thread while any of these routines are pending, then the results are undefined.
    #[inline(always)]
    pub unsafe fn column_i64(&mut self, index: i32) -> i64 {
        unsafe { raw::sqlite3_column_int64(self.0.get_mut(), index) }
    }

    /// mutability: this method may attempt to convert the datatype of the result.
    ///
    /// # Safety
    ///
    /// * If the SQL statement does not currently point to a valid row, or if the column index is out of range, the result is undefined.
    /// * These routines may only be called when the most recent call to [`sqlite3_step`] has returned [`SQLITE_ROW`] and neither [`sqlite3_reset`] nor [`sqlite3_finalize`] have been called subsequently.
    ///   * If any of these routines are called after [`sqlite3_reset`] or [`sqlite3_finalize`] or after [`sqlite3_step`] has returned something other than [`SQLITE_ROW`], the results are undefined.
    /// * If [`sqlite3_step`] or [`sqlite3_reset`] or [`sqlite3_finalize`] are called from a different thread while any of these routines are pending, then the results are undefined.
    #[inline(always)]
    pub unsafe fn column_double(&mut self, index: i32) -> f64 {
        unsafe { raw::sqlite3_column_double(self.0.get_mut(), index) }
    }

    /// mutability: this method may attempt to convert the datatype of the result.
    ///
    /// # Safety
    ///
    /// * If the SQL statement does not currently point to a valid row, or if the column index is out of range, the result is undefined.
    /// * These routines may only be called when the most recent call to [`sqlite3_step`] has returned [`SQLITE_ROW`] and neither [`sqlite3_reset`] nor [`sqlite3_finalize`] have been called subsequently.
    ///   * If any of these routines are called after [`sqlite3_reset`] or [`sqlite3_finalize`] or after [`sqlite3_step`] has returned something other than [`SQLITE_ROW`], the results are undefined.
    /// * If [`sqlite3_step`] or [`sqlite3_reset`] or [`sqlite3_finalize`] are called from a different thread while any of these routines are pending, then the results are undefined.
    #[inline(always)]
    pub unsafe fn column_bytes(&mut self, index: i32) -> usize {
        unsafe { raw::sqlite3_column_bytes(self.0.get_mut(), index) as _ }
    }

    /// mutability: this method may attempt to convert the datatype of the result.
    ///
    /// # Safety
    ///
    /// * If the SQL statement does not currently point to a valid row, or if the column index is out of range, the result is undefined.
    /// * These routines may only be called when the most recent call to [`sqlite3_step`] has returned [`SQLITE_ROW`] and neither [`sqlite3_reset`] nor [`sqlite3_finalize`] have been called subsequently.
    ///   * If any of these routines are called after [`sqlite3_reset`] or [`sqlite3_finalize`] or after [`sqlite3_step`] has returned something other than [`SQLITE_ROW`], the results are undefined.
    /// * If [`sqlite3_step`] or [`sqlite3_reset`] or [`sqlite3_finalize`] are called from a different thread while any of these routines are pending, then the results are undefined.
    #[inline(always)]
    pub unsafe fn column_text(&mut self, index: i32) -> *const core::ffi::c_char {
        unsafe { raw::sqlite3_column_text(self.0.get_mut(), index) }.cast()
    }

    /// mutability: this method may attempt to convert the datatype of the result.
    ///
    /// # Safety
    ///
    /// * If the SQL statement does not currently point to a valid row, or if the column index is out of range, the result is undefined.
    /// * These routines may only be called when the most recent call to [`sqlite3_step`] has returned [`SQLITE_ROW`] and neither [`sqlite3_reset`] nor [`sqlite3_finalize`] have been called subsequently.
    ///   * If any of these routines are called after [`sqlite3_reset`] or [`sqlite3_finalize`] or after [`sqlite3_step`] has returned something other than [`SQLITE_ROW`], the results are undefined.
    /// * If [`sqlite3_step`] or [`sqlite3_reset`] or [`sqlite3_finalize`] are called from a different thread while any of these routines are pending, then the results are undefined.
    #[inline(always)]
    pub unsafe fn column_blob(&mut self, index: i32) -> *const core::ffi::c_void {
        unsafe { raw::sqlite3_column_blob(self.0.get_mut(), index) }
    }
}

#[inline(always)]
fn errstr(e: core::ffi::c_int) -> Option<&'static core::ffi::CStr> {
    let p = unsafe { raw::sqlite3_errstr(e) };
    if p.is_null() {
        None
    } else {
        Some(unsafe { core::ffi::CStr::from_ptr(p) })
    }
}
