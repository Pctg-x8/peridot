use core::ffi::*;

use android::{
    AAsset, AAssetManager, AAssetManager_open, AAsset_close, AAsset_getLength64, AAsset_read,
    AAsset_seek, AAsset_seek64, AASSET_MODE_RANDOM,
};
use libc::SEEK_SET;
use peridot_tp_sqlite3::raw::{
    sqlite3_file, sqlite3_io_methods, sqlite3_vfs, sqlite3_vfs_register, SQLITE_FCNTL_MMAP_SIZE,
    SQLITE_IOCAP_IMMUTABLE, SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN, SQLITE_IOERR_READ,
    SQLITE_IOERR_SHORT_READ, SQLITE_NOTFOUND, SQLITE_OK, SQLITE_OPEN_CREATE, SQLITE_OPEN_READWRITE,
};

#[inline(always)]
pub fn register(amgr: &crate::native_wrapper::AssetManager) {
    unsafe {
        VFS.pAppData = amgr.as_ptr().cast();
        sqlite3_vfs_register(&raw mut VFS, 0);
    }
}

pub const VFS_NAME: &core::ffi::CStr = c"peridot-asset-sqlite3-vfs";

static mut VFS: sqlite3_vfs = sqlite3_vfs {
    iVersion: 3,
    szOsFile: size_of::<File>() as _,
    mxPathname: 128,
    pNext: core::ptr::null_mut(), // filled by sqlite3
    zName: VFS_NAME.as_ptr(),
    pAppData: core::ptr::null_mut(),
    xOpen: vfs_open,
    xDelete: vfs_delete,
    xAccess: vfs_access,
    xFullPathname: vfs_full_pathname,
    xDlOpen: vfs_dlopen,
    xDlError: vfs_dlerror,
    xDlSym: vfs_dlsym,
    xDlClose: vfs_dlclose,
    xRandomness: vfs_randomness,
    xSleep: vfs_sleep,
    xCurrentTime: vfs_current_time,
    xGetLastError: vfs_get_last_error,
    xCurrentTimeInt64: vfs_current_time_int64,
    xSetSystemCall: vfs_set_system_call,
    xGetSystemCall: vfs_get_system_call,
    xNextSystemCall: vfs_next_system_call,
};

static IO_METHODS: sqlite3_io_methods = sqlite3_io_methods {
    iVersion: 3,
    xClose: File::close,
    xRead: File::read,
    xWrite: File::write,
    xTruncate: File::truncate,
    xSync: File::sync,
    xFileSize: File::file_size,
    xLock: File::lock,
    xUnlock: File::unlock,
    xCheckReservedLock: File::check_reserved_lock,
    xFileControl: File::file_control,
    xSectorSize: File::sector_size,
    xDeviceCharacteristics: File::device_characteristics,
    xShmMap: File::shm_map,
    xShmLock: File::shm_lock,
    xShmBarrier: File::shm_barrier,
    xShmUnmap: File::shm_unmap,
    xFetch: File::fetch,
    xUnfetch: File::unfetch,
};

#[repr(C)]
pub struct File {
    base: sqlite3_file,
    asset: core::ptr::NonNull<AAsset>,
}
impl File {
    extern "C" fn close(this: *mut sqlite3_file) -> c_int {
        let this = unsafe { &mut *this.cast::<Self>() };
        unsafe {
            AAsset_close(this.asset.as_ptr());
        }

        SQLITE_OK
    }

    extern "C" fn read(
        this: *mut sqlite3_file,
        buf: *mut c_void,
        amount: c_int,
        offset: i64,
    ) -> c_int {
        let this = unsafe { &mut *this.cast::<Self>() };
        unsafe { AAsset_seek64(this.asset.as_ptr(), offset, SEEK_SET) };
        let r = unsafe { AAsset_read(this.asset.as_ptr(), buf, amount as _) };
        if r < 0 {
            tracing::error!("read err");
            return SQLITE_IOERR_READ;
        }
        if r == amount {
            return SQLITE_OK;
        }

        // fill non-read portion of the buffer
        if r < amount {
            unsafe {
                core::ptr::write_bytes(buf.byte_add(r as _).cast::<u8>(), 0, (amount - r) as _);
            }
            return SQLITE_IOERR_SHORT_READ;
        }

        return SQLITE_IOERR_READ;
    }

    extern "C" fn write(
        _this: *mut sqlite3_file,
        _buf: *const c_void,
        _amount: c_int,
        _offset: i64,
    ) -> c_int {
        unreachable!("writing to this vfs is not supported")
    }

    extern "C" fn truncate(_this: *mut sqlite3_file, _size: i64) -> c_int {
        unreachable!("writing to this vfs is not supported")
    }

    extern "C" fn sync(_this: *mut sqlite3_file, flags: c_int) -> c_int {
        tracing::trace!(flags, "file::sync");
        SQLITE_OK
    }

    extern "C" fn file_size(this: *mut sqlite3_file, size: *mut i64) -> c_int {
        let this = unsafe { &mut *this.cast::<Self>() };
        unsafe {
            size.write(AAsset_getLength64(this.asset.as_ptr()));
        }

        SQLITE_OK
    }

    extern "C" fn lock(this: *mut sqlite3_file, flags: c_int) -> c_int {
        unimplemented!("file::lock {flags:x}")
    }

    extern "C" fn unlock(this: *mut sqlite3_file, flags: c_int) -> c_int {
        unimplemented!("file::unlock {flags:x}")
    }

    extern "C" fn check_reserved_lock(this: *mut sqlite3_file, res_out: *mut c_int) -> c_int {
        unimplemented!("file::check_reserved_lock")
    }

    extern "C" fn file_control(this: *mut sqlite3_file, op: c_int, arg: *mut c_void) -> c_int {
        if op == SQLITE_FCNTL_MMAP_SIZE {
            if unsafe { arg.cast::<i64>().read() } < 0 {
                tracing::warn!("TODO: mmap_size(set)");
                return SQLITE_NOTFOUND;
            } else {
                tracing::debug!(size = unsafe { arg.cast::<i64>().read() }, "mmap size set");
                return SQLITE_OK;
            }
        }

        tracing::warn!(op, "TODO: unhandled file::file_control");
        SQLITE_NOTFOUND
    }

    extern "C" fn sector_size(this: *mut sqlite3_file) -> c_int {
        unimplemented!("file::sector_size")
    }

    extern "C" fn device_characteristics(_this: *mut sqlite3_file) -> c_int {
        SQLITE_IOCAP_UNDELETABLE_WHEN_OPEN | SQLITE_IOCAP_IMMUTABLE
    }

    extern "C" fn shm_map(
        this: *mut sqlite3_file,
        pg: c_int,
        pg_size: c_int,
        offs: c_int,
        ptr: *mut *mut c_void,
    ) -> c_int {
        unimplemented!("file::shm_map {pg} {pg_size} {offs}")
    }

    extern "C" fn shm_lock(
        this: *mut sqlite3_file,
        offset: c_int,
        n: c_int,
        flags: c_int,
    ) -> c_int {
        unimplemented!("file::shm_lock {offset} {n} {flags}")
    }

    extern "C" fn shm_barrier(this: *mut sqlite3_file) {
        unimplemented!("file::shm_barrier")
    }

    extern "C" fn shm_unmap(this: *mut sqlite3_file, delete_flag: c_int) -> c_int {
        unimplemented!("file::shm_unmap {delete_flag:x}")
    }

    extern "C" fn fetch(
        this: *mut sqlite3_file,
        offset: i64,
        amount: c_int,
        pp: *mut *mut c_void,
    ) -> c_int {
        unimplemented!("file::fetch {offset} {amount}")
    }

    extern "C" fn unfetch(this: *mut sqlite3_file, offset: i64, p: *mut c_void) -> c_int {
        unimplemented!("file::unfetch {offset}")
    }
}

extern "C" fn vfs_open(
    vfs: *mut sqlite3_vfs,
    name: *const c_char,
    ofile: *mut sqlite3_file,
    flags: c_int,
    oflags: *mut c_int,
) -> c_int {
    if flags & (SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE) != 0 {
        tracing::error!("vfs cannot handle writable file operation");
        return 1;
    }

    let amgr = unsafe { (*vfs).pAppData.cast::<AAssetManager>() };
    let Some(asset) =
        core::ptr::NonNull::new(unsafe { AAssetManager_open(amgr, name, AASSET_MODE_RANDOM) })
    else {
        tracing::error!(name = ?unsafe { CStr::from_ptr(name) }, "db not found in asset");
        return 1;
    };

    unsafe {
        ofile.cast::<File>().write(File {
            base: sqlite3_file {
                pMethods: &IO_METHODS,
            },
            asset,
        });
        oflags.write(flags);
    }
    SQLITE_OK
}

extern "C" fn vfs_delete(_vfs: *mut sqlite3_vfs, _name: *const c_char, _sync_dir: c_int) -> c_int {
    unreachable!("writing is not supported on this vfs")
}

extern "C" fn vfs_access(
    vfs: *mut sqlite3_vfs,
    name: *const c_char,
    flags: c_int,
    res_out: *mut c_int,
) -> c_int {
    unimplemented!("vfs_access {:?} {flags}", unsafe { CStr::from_ptr(name) })
}

extern "C" fn vfs_full_pathname(
    _vfs: *mut sqlite3_vfs,
    name: *const c_char,
    out: c_int,
    out_str: *mut c_char,
) -> c_int {
    let name = unsafe { CStr::from_ptr(name) };
    if name.count_bytes() + 1 >= out as usize {
        tracing::error!("too short buffer");
        return 1;
    }

    unsafe {
        core::ptr::copy_nonoverlapping(name.as_ptr(), out_str, name.count_bytes() + 1);
    }
    SQLITE_OK
}

extern "C" fn vfs_dlopen(vfs: *mut sqlite3_vfs, name: *const c_char) -> *mut c_void {
    unimplemented!("vfs_dlopen {:?}", unsafe { CStr::from_ptr(name) })
}

extern "C" fn vfs_dlerror(vfs: *mut sqlite3_vfs, byte: c_int, errmsg: *mut c_char) {
    unimplemented!("vfs_dlerror {byte}")
}

extern "C" fn vfs_dlsym(
    vfs: *mut sqlite3_vfs,
    handle: *mut c_void,
    sym: *const c_char,
) -> Option<extern "C" fn()> {
    unimplemented!("vfs_dlsym {handle:p} {:?}", unsafe { CStr::from_ptr(sym) })
}

extern "C" fn vfs_dlclose(vfs: *mut sqlite3_vfs, handle: *mut c_void) {
    unimplemented!("vfs_dlclose {handle:p}")
}

extern "C" fn vfs_randomness(vfs: *mut sqlite3_vfs, byte: c_int, out: *mut c_char) -> c_int {
    unimplemented!("vfs_randomness {byte}")
}

extern "C" fn vfs_sleep(vfs: *mut sqlite3_vfs, microseconds: c_int) -> c_int {
    unimplemented!("vfs_sleep {microseconds}")
}

extern "C" fn vfs_current_time(vfs: *mut sqlite3_vfs, out: *mut c_double) -> c_int {
    unimplemented!("vfs_current_time")
}

extern "C" fn vfs_get_last_error(vfs: *mut sqlite3_vfs, a: c_int, b: *mut c_char) -> c_int {
    unimplemented!("vfs_get_last_error {a}")
}

extern "C" fn vfs_current_time_int64(vfs: *mut sqlite3_vfs, out: *mut i64) -> c_int {
    unimplemented!("vfs_current_time_int64")
}

extern "C" fn vfs_set_system_call(
    vfs: *mut sqlite3_vfs,
    name: *const c_char,
    ptr: Option<extern "C" fn()>,
) -> c_int {
    unimplemented!("vfs_set_system_call {:?}", unsafe { CStr::from_ptr(name) })
}

extern "C" fn vfs_get_system_call(
    vfs: *mut sqlite3_vfs,
    name: *const c_char,
) -> Option<extern "C" fn()> {
    unimplemented!("vfs_get_system_call {:?}", unsafe { CStr::from_ptr(name) })
}

extern "C" fn vfs_next_system_call(vfs: *mut sqlite3_vfs, name: *const c_char) -> *const c_char {
    unimplemented!("vfs_next_system_call {:?}", unsafe { CStr::from_ptr(name) })
}
