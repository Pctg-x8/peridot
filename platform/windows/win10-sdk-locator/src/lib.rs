use std::{ffi::OsString, path::PathBuf};

use windows::core::{PCWSTR, w};
use windows_registry::{KEY_READ, RegistryKey};

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
