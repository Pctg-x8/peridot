use std::{ffi::OsString, os::windows::ffi::OsStringExt};

use windows::{
    Win32::System::Registry::{
        HKEY, HKEY_LOCAL_MACHINE, REG_SAM_FLAGS, RRF_RT_REG_SZ, RegCloseKey, RegGetValueW,
        RegOpenKeyExW,
    },
    core::PCWSTR,
};

pub use windows::Win32::System::Registry::KEY_READ;

#[repr(transparent)]
pub struct RegistryKey(HKEY);
impl Drop for RegistryKey {
    #[inline(always)]
    fn drop(&mut self) {
        if let Err(e) = unsafe { RegCloseKey(self.0).ok() } {
            tracing::error!(reason = %e, "RegCloseKey failed");
        }
    }
}
impl RegistryKey {
    pub const LOCAL_MACHINE: &'static Self =
        unsafe { core::mem::transmute::<&HKEY, &Self>(&HKEY_LOCAL_MACHINE) };

    pub fn open(
        &self,
        subkey: PCWSTR,
        options: Option<u32>,
        sam_desired: REG_SAM_FLAGS,
    ) -> windows::core::Result<Self> {
        let mut k = core::mem::MaybeUninit::uninit();
        unsafe {
            RegOpenKeyExW(self.0, subkey, options, sam_desired, k.as_mut_ptr()).ok()?;
        }

        Ok(Self(unsafe { k.assume_init() }))
    }

    pub fn value_osstr<const FAST_PASS_CHAR_COUNT: usize>(
        &self,
        name: PCWSTR,
    ) -> windows::core::Result<OsString> {
        let mut buf = [core::mem::MaybeUninit::<u16>::uninit(); FAST_PASS_CHAR_COUNT];
        let mut len = size_of_val(&buf) as u32;
        let r = unsafe {
            RegGetValueW(
                self.0,
                None,
                name,
                RRF_RT_REG_SZ,
                None,
                Some(buf.as_mut_ptr().cast()),
                Some(&mut len),
            )
            .ok()
        };
        match r {
            Ok(_) => {
                return Ok(OsString::from_wide(unsafe {
                    core::mem::transmute::<&[core::mem::MaybeUninit<_>], &[_]>(
                        &buf[..(len as usize / 2) - 1],
                    )
                }));
            }
            Err(e) if e != windows::Win32::Foundation::ERROR_MORE_DATA.into() => {
                return Err(e);
            }
            _ => (),
        }

        let mut buf = Vec::<u16>::with_capacity(len as _);
        unsafe {
            RegGetValueW(
                self.0,
                None,
                name,
                RRF_RT_REG_SZ,
                None,
                Some(buf.spare_capacity_mut().as_mut_ptr().cast()),
                Some(&mut len),
            )
            .ok()?;
        }
        unsafe { buf.set_len((len as usize / 2) - 1) };
        Ok(OsString::from_wide(&buf))
    }
}
