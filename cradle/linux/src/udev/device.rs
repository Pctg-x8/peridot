#![allow(dead_code)]

use super::ffi::*;
use std::ptr::NonNull;

#[repr(transparent)]
pub struct Device(NonNull<udev_device>);
impl Drop for Device {
    fn drop(&mut self) {
        unsafe {
            udev_device_unref(self.0.as_ptr());
        }
    }
}
impl Clone for Device {
    fn clone(&self) -> Self {
        unsafe { Self(NonNull::new_unchecked(udev_device_ref(self.0.as_ptr()))) }
    }
}
impl Device {
    pub const PROPERTY_KEY_ID_INPUT_MOUSE: &'static std::ffi::CStr =
        unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"ID_INPUT_MOUSE\0") };
    pub const PROPERTY_KEY_NAME: &'static std::ffi::CStr =
        unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(b"NAME\0") };

    pub unsafe fn from_ptr(p: *mut udev_device) -> Option<Self> {
        NonNull::new(p).map(Self)
    }
    pub fn as_ptr(&self) -> *mut udev_device {
        self.0.as_ptr()
    }

    pub fn from_syspath(udev: &super::Context, syspath: &std::ffi::CStr) -> Option<Self> {
        unsafe {
            NonNull::new(udev_device_new_from_syspath(
                udev.as_ptr(),
                syspath.as_ptr(),
            ))
            .map(Self)
        }
    }
    pub fn from_devnum(udev: &super::Context, ty: libc::c_char, num: dev_t) -> Option<Self> {
        unsafe { NonNull::new(udev_device_new_from_devnum(udev.as_ptr(), ty, num)).map(Self) }
    }
    pub fn from_subsystem_sysname(
        udev: &super::Context,
        subsystem: &std::ffi::CStr,
        sysname: &std::ffi::CStr,
    ) -> Option<Self> {
        unsafe {
            NonNull::new(udev_device_new_from_subsystem_sysname(
                udev.as_ptr(),
                subsystem.as_ptr(),
                sysname.as_ptr(),
            ))
            .map(Self)
        }
    }
    pub fn from_device_id(udev: &super::Context, id: &std::ffi::CStr) -> Option<Self> {
        unsafe {
            NonNull::new(udev_device_new_from_device_id(udev.as_ptr(), id.as_ptr())).map(Self)
        }
    }
    pub fn from_environment(udev: &super::Context) -> Option<Self> {
        unsafe { NonNull::new(udev_device_new_from_environment(udev.as_ptr())).map(Self) }
    }

    pub fn udev(&self) -> super::Context {
        unsafe {
            super::Context::from_ptr(udev_device_get_udev(self.as_ptr())).expect("no context?")
        }
    }
    pub fn parent(&self) -> Option<Self> {
        unsafe { Self::from_ptr(udev_device_ref(udev_device_get_parent(self.as_ptr()))) }
    }
    pub fn parent_with_subsystem_devtype(
        &self,
        subsystem: &std::ffi::CStr,
        devtype: &std::ffi::CStr,
    ) -> Option<Self> {
        unsafe {
            Self::from_ptr(udev_device_get_parent_with_subsystem_devtype(
                self.as_ptr(),
                subsystem.as_ptr(),
                devtype.as_ptr(),
            ))
        }
    }

    pub fn devpath(&self) -> Option<&std::ffi::CStr> {
        unsafe {
            let p = udev_device_get_devpath(self.as_ptr());
            if p.is_null() {
                None
            } else {
                Some(std::ffi::CStr::from_ptr(p))
            }
        }
    }
    pub fn subsystem(&self) -> Option<&std::ffi::CStr> {
        unsafe {
            let p = udev_device_get_subsystem(self.as_ptr());
            if p.is_null() {
                None
            } else {
                Some(std::ffi::CStr::from_ptr(p))
            }
        }
    }
    pub fn devtype(&self) -> Option<&std::ffi::CStr> {
        unsafe {
            let p = udev_device_get_devtype(self.as_ptr());
            if p.is_null() {
                None
            } else {
                Some(std::ffi::CStr::from_ptr(p))
            }
        }
    }
    pub fn syspath(&self) -> Option<&std::ffi::CStr> {
        unsafe {
            let p = udev_device_get_syspath(self.as_ptr());
            if p.is_null() {
                None
            } else {
                Some(std::ffi::CStr::from_ptr(p))
            }
        }
    }
    pub fn sysname(&self) -> Option<&std::ffi::CStr> {
        unsafe {
            let p = udev_device_get_sysname(self.as_ptr());
            if p.is_null() {
                None
            } else {
                Some(std::ffi::CStr::from_ptr(p))
            }
        }
    }
    pub fn sysnum(&self) -> Option<&std::ffi::CStr> {
        unsafe {
            let p = udev_device_get_sysnum(self.as_ptr());
            if p.is_null() {
                None
            } else {
                Some(std::ffi::CStr::from_ptr(p))
            }
        }
    }
    pub fn devnode(&self) -> Option<&std::ffi::CStr> {
        unsafe {
            let p = udev_device_get_devnode(self.as_ptr());
            if p.is_null() {
                None
            } else {
                Some(std::ffi::CStr::from_ptr(p))
            }
        }
    }
    pub fn is_initialized(&self) -> bool {
        unsafe { udev_device_get_is_initialized(self.as_ptr()) == 1 }
    }
    pub fn property_value(&self, key: &std::ffi::CStr) -> Option<&std::ffi::CStr> {
        unsafe {
            let p = udev_device_get_property_value(self.as_ptr(), key.as_ptr());
            if p.is_null() {
                None
            } else {
                Some(std::ffi::CStr::from_ptr(p))
            }
        }
    }
    pub fn driver(&self) -> &std::ffi::CStr {
        unsafe { std::ffi::CStr::from_ptr(udev_device_get_driver(self.as_ptr())) }
    }
    pub fn devnum(&self) -> dev_t {
        unsafe { udev_device_get_devnum(self.as_ptr()) }
    }
    pub fn action(&self) -> &std::ffi::CStr {
        unsafe { std::ffi::CStr::from_ptr(udev_device_get_action(self.as_ptr())) }
    }
    pub fn seqnum(&self) -> libc::c_ulonglong {
        unsafe { udev_device_get_seqnum(self.as_ptr()) }
    }
    pub fn usec_since_initialized(&self) -> libc::c_ulonglong {
        unsafe { udev_device_get_usec_since_initialized(self.as_ptr()) }
    }
    pub fn sysattr_value(&self, sysattr: &std::ffi::CStr) -> &std::ffi::CStr {
        unsafe {
            std::ffi::CStr::from_ptr(udev_device_get_sysattr_value(
                self.as_ptr(),
                sysattr.as_ptr(),
            ))
        }
    }
    pub fn set_sysattr_value(
        &mut self,
        sysattr: &std::ffi::CStr,
        value: &std::ffi::CStr,
    ) -> Result<(), libc::c_int> {
        let r = unsafe {
            udev_device_set_sysattr_value(self.as_ptr(), sysattr.as_ptr(), value.as_ptr())
        };
        if r >= 0 {
            Ok(())
        } else {
            Err(r)
        }
    }
    pub fn has_tag(&self, tag: &std::ffi::CStr) -> bool {
        unsafe { udev_device_has_tag(self.as_ptr(), tag.as_ptr()) == 1 }
    }

    pub fn iter_devlinks(&self) -> super::ListIterator {
        unsafe {
            super::ListIterator::from(super::ListEntry::from_ptr(
                udev_device_get_devlinks_list_entry(self.as_ptr()),
            ))
        }
    }
    pub fn iter_properties(&self) -> super::ListIterator {
        unsafe {
            super::ListIterator::from(super::ListEntry::from_ptr(
                udev_device_get_properties_list_entry(self.as_ptr()),
            ))
        }
    }
    pub fn iter_tags(&self) -> super::ListIterator {
        unsafe {
            super::ListIterator::from(super::ListEntry::from_ptr(udev_device_get_tags_list_entry(
                self.as_ptr(),
            )))
        }
    }
    pub fn iter_sysattr(&self) -> super::ListIterator {
        unsafe {
            super::ListIterator::from(super::ListEntry::from_ptr(
                udev_device_get_sysattr_list_entry(self.as_ptr()),
            ))
        }
    }

    pub fn is_mouse(&self) -> bool {
        self.property_value(Self::PROPERTY_KEY_ID_INPUT_MOUSE)
            .is_some_and(|x| x.to_str() == Ok("1"))
    }

    pub fn name(&self) -> Option<&std::ffi::CStr> {
        self.property_value(Self::PROPERTY_KEY_NAME)
    }
}
