#![allow(non_camel_case_types)]

use core::ffi::*;

#[repr(C)]
struct FFIOpaqueStruct(
    [u8; 0],
    core::marker::PhantomData<(*mut u8, core::marker::PhantomPinned)>,
);

#[repr(C)]
pub struct sqlite3(FFIOpaqueStruct);
#[repr(C)]
pub struct sqlite3_stmt(FFIOpaqueStruct);

pub const SQLITE_OK: c_int = 0;
pub const SQLITE_ROW: c_int = 100;
pub const SQLITE_DONE: c_int = 101;

pub const SQLITE_INTEGER: c_int = 1;
pub const SQLITE_FLOAT: c_int = 2;
pub const SQLITE_BLOB: c_int = 4;
pub const SQLITE_NULL: c_int = 5;
pub const SQLITE_TEXT: c_int = 3;

pub const SQLITE_UTF8: c_int = 1;

/// extern "C" fn(*mut c_void); or special value
pub type sqlite3_destructor_type = *const c_void;
pub const SQLITE_STATIC: sqlite3_destructor_type = core::ptr::null();
pub const SQLITE_TRANSIENT: sqlite3_destructor_type =
    core::ptr::without_provenance((-1isize).cast_unsigned());

pub const SQLITE_OPEN_READONLY: c_int = 0x00000001;
pub const SQLITE_OPEN_READWRITE: c_int = 0x00000002;
pub const SQLITE_OPEN_CREATE: c_int = 0x00000004;

unsafe extern "C" {
    pub fn sqlite3_open_v2(
        filename: *const c_char,
        ppdb: *mut *mut sqlite3,
        flags: c_int,
        vfs: *const c_char,
    ) -> c_int;

    pub fn sqlite3_close(sqlite: *mut sqlite3) -> c_int;
    pub fn sqlite3_close_v2(sqlite: *mut sqlite3) -> c_int;

    pub fn sqlite3_exec(
        sqlite: *mut sqlite3,
        sql: *const c_char,
        callback: extern "C" fn(*mut c_void, c_int, *mut *mut c_char, *mut *mut c_char) -> c_int,
        ctx: *mut c_void,
        errmsg: *mut *mut c_char,
    ) -> c_int;

    pub fn sqlite3_errmsg(db: *mut sqlite3) -> *const c_char;
    pub fn sqlite3_errstr(e: c_int) -> *const c_char;

    pub fn sqlite3_prepare(
        db: *mut sqlite3,
        sql: *const c_char,
        bytes: c_int,
        ppstmt: *mut *mut sqlite3_stmt,
        ptail: *mut *const c_char,
    ) -> c_int;

    pub fn sqlite3_bind_blob(
        stmt: *mut sqlite3_stmt,
        index: c_int,
        data: *const c_void,
        bytes: c_int,
        lifetime: sqlite3_destructor_type,
    ) -> c_int;
    pub fn sqlite3_bind_blob64(
        stmt: *mut sqlite3_stmt,
        index: c_int,
        data: *const c_void,
        bytes: u64,
        lifetime: sqlite3_destructor_type,
    ) -> c_int;
    pub fn sqlite3_bind_double(stmt: *mut sqlite3_stmt, index: c_int, value: c_double) -> c_int;
    pub fn sqlite3_bind_int(stmt: *mut sqlite3_stmt, index: c_int, value: c_int) -> c_int;
    pub fn sqlite3_bind_int64(stmt: *mut sqlite3_stmt, index: c_int, value: i64) -> c_int;
    pub fn sqlite3_bind_null(stmt: *mut sqlite3_stmt, index: c_int) -> c_int;
    pub fn sqlite3_bind_text(
        stmt: *mut sqlite3_stmt,
        index: c_int,
        value: *const c_char,
        bytes: c_int,
        lifetime: sqlite3_destructor_type,
    ) -> c_int;
    pub fn sqlite3_bind_text64(
        stmt: *mut sqlite3_stmt,
        index: c_int,
        value: *const c_char,
        bytes: u64,
        lifetime: sqlite3_destructor_type,
        encoding: c_uchar,
    ) -> c_int;

    pub fn sqlite3_column_name(stmt: *mut sqlite3_stmt, n: c_int) -> *const c_char;
    pub fn sqlite3_column_decltype(stmt: *mut sqlite3_stmt, n: c_int) -> *const c_char;

    pub fn sqlite3_step(stmt: *mut sqlite3_stmt) -> c_int;
    pub fn sqlite3_data_count(stmt: *mut sqlite3_stmt) -> c_int;

    pub fn sqlite3_column_blob(stmt: *mut sqlite3_stmt, icol: c_int) -> *const c_void;
    pub fn sqlite3_column_double(stmt: *mut sqlite3_stmt, icol: c_int) -> c_double;
    pub fn sqlite3_column_int(stmt: *mut sqlite3_stmt, icol: c_int) -> c_int;
    pub fn sqlite3_column_int64(stmt: *mut sqlite3_stmt, icol: c_int) -> i64;
    pub fn sqlite3_column_text(stmt: *mut sqlite3_stmt, icol: c_int) -> *const c_uchar;
    pub fn sqlite3_column_bytes(stmt: *mut sqlite3_stmt, icol: c_int) -> c_int;
    pub fn sqlite3_column_type(stmt: *mut sqlite3_stmt, icol: c_int) -> c_int;

    pub fn sqlite3_finalize(stmt: *mut sqlite3_stmt) -> c_int;
    pub fn sqlite3_reset(stmt: *mut sqlite3_stmt) -> c_int;
}
