use std::{ffi::OsString, os::windows::ffi::OsStringExt, path::PathBuf};

use windows::{
    Win32::System::Registry::{
        HKEY, HKEY_LOCAL_MACHINE, KEY_READ, REG_SAM_FLAGS, RRF_RT_REG_SZ, RegCloseKey,
        RegGetValueW, RegOpenKeyExW,
    },
    core::{PCWSTR, w},
};

#[cfg(target_pointer_width = "64")]
const KITS_ROOT_KEY: PCWSTR = w!(r#"SOFTWARE\WOW6432Node\Microsoft\Microsoft SDKs\Windows\v10.0"#);
#[cfg(target_pointer_width = "32")]
const KITS_ROOT_KEY: PCWSTR = w!(r#"SOFTWARE\Microsoft\Microsoft SDKs\Windows\v10.0"#);

const INSTALLATION_FOLDER_VALUE_NAME: PCWSTR = w!("InstallationFolder");
const PRODUCT_VERSION_VALUE_NAME: PCWSTR = w!("ProductVersion");

#[repr(transparent)]
pub struct Windows10SdkInstallationRegistry(RegistryKey);
impl Windows10SdkInstallationRegistry {
    pub fn open() -> windows::core::Result<Self> {
        Ok(Self(RegistryKey::LOCAL_MACHINE.open(
            KITS_ROOT_KEY,
            None,
            KEY_READ,
        )?))
    }

    #[inline(always)]
    pub fn installation_folder(&self) -> windows::core::Result<PathBuf> {
        Ok(PathBuf::from(
            self.0.value_osstr::<256>(INSTALLATION_FOLDER_VALUE_NAME)?,
        ))
    }

    #[inline(always)]
    pub fn product_version(&self) -> windows::core::Result<OsString> {
        self.0.value_osstr::<16>(PRODUCT_VERSION_VALUE_NAME)
    }
}

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
