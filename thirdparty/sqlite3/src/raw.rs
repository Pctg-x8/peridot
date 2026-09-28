#![allow(non_camel_case_types, non_snake_case)]

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
pub const SQLITE_ERROR: c_int = 1;
pub const SQLITE_ROW: c_int = 100;
pub const SQLITE_DONE: c_int = 101;
pub const SQLITE_IOERR: c_int = 10;
pub const SQLITE_NOTFOUND: c_int = 12;

pub const SQLITE_IOERR_READ: c_int = SQLITE_IOERR | (1 << 8);
pub const SQLITE_IOERR_SHORT_READ: c_int = SQLITE_IOERR | (2 << 8);

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

pub const SQLITE_PREPARE_PERSISTENT: c_int = 0x01;
pub const SQLITE_PREPARE_NORMALIZE: c_int = 0x02;
pub const SQLITE_PREPARE_NO_VTAB: c_int = 0x04;
pub const SQLITE_PREPARE_DONT_LOG: c_int = 0x10;
pub const SQLITE_PREPARE_FROM_DDL: c_int = 0x20;

pub const SQLITE_IOCAP_ATOMIC: c_int = 0x00000001;
pub const SQLITE_IOCAP_ATOMIC512: c_int = 0x00000002;
pub const SQLITE_IOCAP_ATOMIC1K: c_int = 0x00000004;
pub const SQLITE_IOCAP_ATOMIC2K: c_int = 0x00000008;
pub const SQLITE_IOCAP_ATOMIC4K: c_int = 0x00000010;
pub const SQLITE_IOCAP_ATOMIC8K: c_int = 0x00000020;
pub const SQLITE_IOCAP_ATOMIC16K: c_int = 0x00000040;
pub const SQLITE_IOCAP_ATOMIC32K: c_int = 0x00000080;
pub const SQLITE_IOCAP_ATOMIC64K: c_int = 0x00000100;
pub const SQLITE_IOCAP_SAFE_APPEND: c_int = 0x00000200;
pub const SQLITE_IOCAP_SEQUENTIAL: c_int = 0x00000400;
pub const SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN: c_int = 0x00000800;
pub const SQLITE_IOCAP_POWERSAFE_OVERWRITE: c_int = 0x00001000;
pub const SQLITE_IOCAP_IMMUTABLE: c_int = 0x00002000;
pub const SQLITE_IOCAP_BATCH_ATOMIC: c_int = 0x00004000;
pub const SQLITE_IOCAP_SUBPAGE_READ: c_int = 0x00008000;

pub const SQLITE_LOCK_NONE: c_int = 0;
pub const SQLITE_LOCK_SHARED: c_int = 1;
pub const SQLITE_LOCK_RESERVED: c_int = 2;
pub const SQLITE_LOCK_PENDING: c_int = 3;
pub const SQLITE_LOCK_EXCLUSIVE: c_int = 4;

pub const SQLITE_SYNC_NORMAL: c_int = 0x0002;
pub const SQLITE_SYNC_FULL: c_int = 0x0003;
pub const SQLITE_SYNC_DATAONLY: c_int = 0x0010;

pub const SQLITE_FCNTL_BUSYHANDLER: c_int = 15;
pub const SQLITE_FCNTL_MMAP_SIZE: c_int = 18;

#[repr(C)]
pub struct sqlite3_file {
    pub pMethods: *const sqlite3_io_methods,
}

#[repr(C)]
pub struct sqlite3_io_methods {
    pub iVersion: c_int,
    pub xClose: extern "C" fn(this: *mut sqlite3_file) -> c_int,
    pub xRead: extern "C" fn(
        this: *mut sqlite3_file,
        buf: *mut c_void,
        amount: c_int,
        offset: i64,
    ) -> c_int,
    pub xWrite: extern "C" fn(
        this: *mut sqlite3_file,
        buf: *const c_void,
        amount: c_int,
        offset: i64,
    ) -> c_int,
    pub xTruncate: extern "C" fn(this: *mut sqlite3_file, size: i64) -> c_int,
    pub xSync: extern "C" fn(this: *mut sqlite3_file, flags: c_int) -> c_int,
    pub xFileSize: extern "C" fn(this: *mut sqlite3_file, size: *mut i64) -> c_int,
    pub xLock: extern "C" fn(this: *mut sqlite3_file, flags: c_int) -> c_int,
    pub xUnlock: extern "C" fn(this: *mut sqlite3_file, flags: c_int) -> c_int,
    pub xCheckReservedLock: extern "C" fn(this: *mut sqlite3_file, res_out: *mut c_int) -> c_int,
    pub xFileControl: extern "C" fn(this: *mut sqlite3_file, op: c_int, arg: *mut c_void) -> c_int,
    pub xSectorSize: extern "C" fn(this: *mut sqlite3_file) -> c_int,
    pub xDeviceCharacteristics: extern "C" fn(this: *mut sqlite3_file) -> c_int,
    pub xShmMap: extern "C" fn(
        this: *mut sqlite3_file,
        pg: c_int,
        pgsz: c_int,
        arg1: c_int,
        ptr: *mut *mut c_void,
    ) -> c_int,
    pub xShmLock:
        extern "C" fn(this: *mut sqlite3_file, offset: c_int, n: c_int, flags: c_int) -> c_int,
    pub xShmBarrier: extern "C" fn(this: *mut sqlite3_file),
    pub xShmUnmap: extern "C" fn(this: *mut sqlite3_file, delete_flag: c_int) -> c_int,
    pub xFetch: extern "C" fn(
        this: *mut sqlite3_file,
        offset: i64,
        amount: c_int,
        pp: *mut *mut c_void,
    ) -> c_int,
    pub xUnfetch: extern "C" fn(this: *mut sqlite3_file, offset: i64, p: *mut c_void) -> c_int,
}

#[repr(C)]
pub struct sqlite3_vfs {
    pub iVersion: c_int,
    pub szOsFile: c_int,
    pub mxPathname: c_int,
    pub pNext: *mut sqlite3_vfs,
    pub zName: *const c_char,
    pub pAppData: *mut c_void,
    pub xOpen: extern "C" fn(
        this: *mut sqlite3_vfs,
        name: *const c_char,
        ofile: *mut sqlite3_file,
        flags: c_int,
        out_flags: *mut c_int,
    ) -> c_int,
    pub xDelete:
        extern "C" fn(this: *mut sqlite3_vfs, name: *const c_char, sync_dir: c_int) -> c_int,
    pub xAccess: extern "C" fn(
        this: *mut sqlite3_vfs,
        name: *const c_char,
        flags: c_int,
        res_out: *mut c_int,
    ) -> c_int,
    pub xFullPathname: extern "C" fn(
        this: *mut sqlite3_vfs,
        name: *const c_char,
        out: c_int,
        out_str: *mut c_char,
    ) -> c_int,
    pub xDlOpen: extern "C" fn(this: *mut sqlite3_vfs, filename: *const c_char) -> *mut c_void,
    pub xDlError: extern "C" fn(this: *mut sqlite3_vfs, byte: c_int, errmsg: *mut c_char),
    pub xDlSym: extern "C" fn(
        this: *mut sqlite3_vfs,
        handle: *mut c_void,
        symbol: *const c_char,
    ) -> Option<extern "C" fn()>,
    pub xDlClose: extern "C" fn(this: *mut sqlite3_vfs, handle: *mut c_void),
    pub xRandomness: extern "C" fn(this: *mut sqlite3_vfs, byte: c_int, out: *mut c_char) -> c_int,
    pub xSleep: extern "C" fn(this: *mut sqlite3_vfs, microseconds: c_int) -> c_int,
    pub xCurrentTime: extern "C" fn(this: *mut sqlite3_vfs, out: *mut c_double) -> c_int,
    pub xGetLastError:
        extern "C" fn(this: *mut sqlite3_vfs, value: c_int, msg: *mut c_char) -> c_int,
    pub xCurrentTimeInt64: extern "C" fn(this: *mut sqlite3_vfs, out: *mut i64) -> c_int,
    pub xSetSystemCall: extern "C" fn(
        this: *mut sqlite3_vfs,
        name: *const c_char,
        ptr: Option<extern "C" fn()>,
    ) -> c_int,
    pub xGetSystemCall:
        extern "C" fn(this: *mut sqlite3_vfs, name: *const c_char) -> Option<extern "C" fn()>,
    pub xNextSystemCall:
        extern "C" fn(this: *mut sqlite3_vfs, name: *const c_char) -> *const c_char,
}

pub const SQLITE_ACCESS_EXISTS: c_int = 0;
pub const SQLITE_ACCESS_READWRITE: c_int = 1;
pub const SQLITE_ACCESS_READ: c_int = 2;

pub const SQLITE_SHM_UNLOCK: c_int = 1;
pub const SQLITE_SHM_LOCK: c_int = 2;
pub const SQLITE_SHM_SHARED: c_int = 4;
pub const SQLITE_SHM_EXCLUSIVE: c_int = 8;
pub const SQLITE_SHM_NLOCK: c_int = 8;

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
        callback: Option<
            extern "C" fn(*mut c_void, c_int, *mut *mut c_char, *mut *mut c_char) -> c_int,
        >,
        ctx: *mut c_void,
        errmsg: *mut *mut c_char,
    ) -> c_int;

    pub fn sqlite3_errmsg(db: *mut sqlite3) -> *const c_char;
    pub fn sqlite3_errstr(e: c_int) -> *const c_char;

    pub fn sqlite3_prepare_v2(
        db: *mut sqlite3,
        sql: *const c_char,
        bytes: c_int,
        ppstmt: *mut *mut sqlite3_stmt,
        ptail: *mut *const c_char,
    ) -> c_int;
    pub fn sqlite3_prepare_v3(
        db: *mut sqlite3,
        sql: *const c_char,
        bytes: c_int,
        prep_flags: c_uint,
        stmt: *mut *mut sqlite3_stmt,
        tail: *mut *const c_char,
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

    pub fn sqlite3_vfs_register(vfs: *mut sqlite3_vfs, make_default: c_int) -> c_int;
    pub fn sqlite3_vfs_unregister(vfs: *mut sqlite3_vfs) -> c_int;
}
