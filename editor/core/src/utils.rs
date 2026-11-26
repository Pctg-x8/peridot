pub mod platform;
pub mod text;

use shared::{PixelsUnit, Size};

#[repr(transparent)]
pub struct UnsafeMainThreadOnlyOnceCell<T>(pub core::cell::OnceCell<T>);
unsafe impl<T> Sync for UnsafeMainThreadOnlyOnceCell<T> {}
unsafe impl<T> Send for UnsafeMainThreadOnlyOnceCell<T> {}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct DummyDebug<T>(pub T);
impl<T> core::fmt::Debug for DummyDebug<T> {
    #[inline(always)]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct(&format!("Hidden<{}>", std::any::type_name::<T>()))
            .finish_non_exhaustive()
    }
}
impl<T> From<T> for DummyDebug<T> {
    #[inline(always)]
    fn from(value: T) -> Self {
        Self(value)
    }
}

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NonCloneable<T>(pub T);
impl<T> Clone for NonCloneable<T> {
    fn clone(&self) -> Self {
        panic!("cannot clone this type: {}", std::any::type_name::<T>())
    }
}
impl<T> From<T> for NonCloneable<T> {
    #[inline(always)]
    fn from(value: T) -> Self {
        Self(value)
    }
}

impl<T> From<T> for NonCloneable<DummyDebug<T>> {
    #[inline(always)]
    fn from(value: T) -> Self {
        NonCloneable(DummyDebug(value))
    }
}

#[repr(transparent)]
pub struct ByteLengthFormatter(pub usize);
impl core::fmt::Display for ByteLengthFormatter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (mut v, mut u) = (self.0 as f64, "bytes");
        if v >= 1000.0 {
            v /= 1024.0;
            u = "KB";
        }
        if v >= 1000.0 {
            v /= 1024.0;
            u = "MB";
        }
        if v >= 1000.0 {
            v /= 1024.0;
            u = "GB";
        }

        if u == "bytes" {
            write!(f, "{} bytes", self.0)
        } else {
            write!(f, "{v:.3} {u}")
        }
    }
}

#[cfg(all(unix, not(target_os = "macos")))]
#[inline(always)]
pub fn is_budou_cluster_char(c: char) -> bool {
    // 一部Commonにあるらしいので特別対応
    c as u32 == 0x30fc
        || c as u32 == 0xff70
        || peridot_tp_icu::get_script(c as _).is_ok_and(|s| {
            s == peridot_tp_icu::c::USCRIPT_HIRAGANA
                || s == peridot_tp_icu::c::USCRIPT_KATAKANA
                || s == peridot_tp_icu::c::USCRIPT_HAN
                || s == peridot_tp_icu::c::USCRIPT_THAI
        })
}

#[inline(always)]
pub const fn size_to_vk(size: &Size<PixelsUnit>) -> bedrock::Extent2D {
    bedrock::Extent2D {
        width: size.width,
        height: size.height,
    }
}
