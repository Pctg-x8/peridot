use android::{
    AAsset, AAssetManager, AAssetManager_fromJava, AAssetManager_open, AAsset_close, AAsset_read,
    AAsset_seek64, ANativeWindow, ANativeWindow_acquire, ANativeWindow_fromSurface,
    ANativeWindow_getHeight, ANativeWindow_getWidth, ANativeWindow_release,
};
use jni::{
    objects::{GlobalRef, JObject},
    JNIEnv,
};
use std::io::Result as IOResult;

#[repr(transparent)]
pub struct Window(core::ptr::NonNull<ANativeWindow>);
unsafe impl Sync for Window {}
unsafe impl Send for Window {}
impl Window {
    #[inline]
    pub fn from_surface(env: &JNIEnv, surface_ref: &JObject) -> Option<Self> {
        let ptr = core::ptr::NonNull::new(unsafe {
            ANativeWindow_fromSurface(env.get_raw(), surface_ref.as_raw())
        })?;

        Some(Self(ptr))
    }

    pub const fn as_ptr(&self) -> *mut ANativeWindow {
        self.0.as_ptr()
    }

    #[inline(always)]
    pub fn width(&self) -> i32 {
        unsafe { ANativeWindow_getWidth(self.0.as_ptr()) }
    }

    #[inline(always)]
    pub fn height(&self) -> i32 {
        unsafe { ANativeWindow_getHeight(self.0.as_ptr()) }
    }
}
impl Clone for Window {
    #[inline(always)]
    fn clone(&self) -> Self {
        unsafe {
            ANativeWindow_acquire(self.0.as_ptr());
        }

        Self(self.0)
    }
}
impl Drop for Window {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            ANativeWindow_release(self.0.as_ptr());
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum AssetManagerCreateError {
    #[error(transparent)]
    JNI(#[from] jni::errors::Error),
    #[error("No corresponding AssetManager object associated to the JObject ref")]
    NoCorrespondingObject,
}

pub struct AssetManager(
    core::ptr::NonNull<AAssetManager>,
    #[allow(dead_code)] GlobalRef,
);
unsafe impl Sync for AssetManager {}
unsafe impl Send for AssetManager {}
impl AssetManager {
    pub fn from_java(env: &JNIEnv, obj_ref: &JObject) -> Result<Self, AssetManagerCreateError> {
        let gref = env.new_global_ref(obj_ref)?;
        let ptr = core::ptr::NonNull::new(unsafe {
            AAssetManager_fromJava(env.get_raw(), gref.as_raw())
        })
        .ok_or(AssetManagerCreateError::NoCorrespondingObject)?;

        Ok(Self(ptr, gref))
    }

    #[inline]
    pub fn open(&mut self, filename: &core::ffi::CStr, mode: core::ffi::c_int) -> Option<Asset> {
        let ptr = core::ptr::NonNull::new(unsafe {
            AAssetManager_open(self.0.as_ptr(), filename.as_ptr(), mode)
        })?;

        Some(Asset(ptr))
    }
}

#[repr(transparent)]
pub struct Asset(core::ptr::NonNull<AAsset>);
unsafe impl Sync for Asset {}
unsafe impl Send for Asset {}
impl Drop for Asset {
    #[inline(always)]
    fn drop(&mut self) {
        unsafe {
            AAsset_close(self.0.as_ptr());
        }
    }
}
impl std::io::Read for Asset {
    fn read(&mut self, buf: &mut [u8]) -> IOResult<usize> {
        let read_len = unsafe { AAsset_read(self.0.as_ptr(), buf.as_mut_ptr() as _, buf.len()) };

        if read_len < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(read_len as _)
        }
    }
}
impl std::io::Seek for Asset {
    fn seek(&mut self, pos: std::io::SeekFrom) -> IOResult<u64> {
        let new_pos = match pos {
            std::io::SeekFrom::Current(o) => unsafe {
                AAsset_seek64(self.0.as_ptr(), o, libc::SEEK_CUR)
            },
            std::io::SeekFrom::Start(o) => unsafe {
                AAsset_seek64(
                    self.0.as_ptr(),
                    o.try_into().map_err(|_| {
                        std::io::Error::new(
                            std::io::ErrorKind::Other,
                            "too large offset for seeking",
                        )
                    })?,
                    libc::SEEK_SET,
                )
            },
            std::io::SeekFrom::End(o) => unsafe {
                AAsset_seek64(self.0.as_ptr(), o, libc::SEEK_END)
            },
        };

        if new_pos < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(new_pos as _)
        }
    }
}
