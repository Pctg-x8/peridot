use super::ffi::*;
use core::{cell::UnsafeCell, ffi::*};

#[repr(transparent)]
pub struct Device(UnsafeCell<udev_device>);
unsafe impl Sync for Device {}
unsafe impl Send for Device {}
impl crate::RefCounted for Device {
    #[inline(always)]
    fn r#ref(&mut self) {
        unsafe {
            udev_device_ref(self.0.get_mut());
        }
    }

    #[inline(always)]
    fn unref(&mut self) {
        unsafe {
            udev_device_unref(self.0.get_mut());
        }
    }
}
impl Device {
    pub const PROPERTY_KEY_ID_INPUT_MOUSE: &CStr = c"ID_INPUT_MOUSE";
    pub const PROPERTY_KEY_NAME: &CStr = c"NAME";

    #[inline(always)]
    pub const fn as_raw_ptr(&self) -> *mut crate::ffi::udev_device {
        self.0.get()
    }

    #[inline(always)]
    pub fn from_syspath(udev: &crate::Context, syspath: &CStr) -> Option<crate::Owned<Self>> {
        unsafe {
            crate::Owned::unretain_from_ptr(
                udev_device_new_from_syspath(udev.as_ptr(), syspath.as_ptr()).cast(),
            )
        }
    }

    #[inline(always)]
    pub fn from_devnum(
        udev: &crate::Context,
        ty: c_char,
        num: dev_t,
    ) -> Option<crate::Owned<Self>> {
        unsafe {
            crate::Owned::unretain_from_ptr(
                udev_device_new_from_devnum(udev.as_ptr(), ty, num).cast(),
            )
        }
    }

    #[inline(always)]
    pub fn from_subsystem_sysname(
        udev: &crate::Context,
        subsystem: &CStr,
        sysname: &CStr,
    ) -> Option<crate::Owned<Self>> {
        unsafe {
            crate::Owned::unretain_from_ptr(
                udev_device_new_from_subsystem_sysname(
                    udev.as_ptr(),
                    subsystem.as_ptr(),
                    sysname.as_ptr(),
                )
                .cast(),
            )
        }
    }

    #[inline(always)]
    pub fn from_device_id(udev: &crate::Context, id: &CStr) -> Option<crate::Owned<Self>> {
        unsafe {
            crate::Owned::unretain_from_ptr(
                udev_device_new_from_device_id(udev.as_ptr(), id.as_ptr()).cast(),
            )
        }
    }

    #[inline(always)]
    pub fn from_environment(udev: &crate::Context) -> Option<crate::Owned<Self>> {
        unsafe {
            crate::Owned::unretain_from_ptr(udev_device_new_from_environment(udev.as_ptr()).cast())
        }
    }

    #[inline(always)]
    pub fn udev(&self) -> crate::Context {
        unsafe {
            crate::Context::from_ptr(udev_device_get_udev(self.0.get())).expect("no context?")
        }
    }

    #[inline(always)]
    pub fn parent(&self) -> Option<&Self> {
        unsafe { udev_device_get_parent(self.0.get()).cast::<Self>().as_ref() }
    }

    #[inline(always)]
    pub fn parent_with_subsystem_devtype<'a>(
        &'a self,
        subsystem: &CStr,
        devtype: &CStr,
    ) -> Option<&'a Self> {
        unsafe {
            udev_device_get_parent_with_subsystem_devtype(
                self.0.get(),
                subsystem.as_ptr(),
                devtype.as_ptr(),
            )
            .cast::<Self>()
            .as_ref()
        }
    }

    #[inline]
    pub fn devpath(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_devpath(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline]
    pub fn subsystem(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_subsystem(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline]
    pub fn devtype(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_devtype(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline]
    pub fn syspath(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_syspath(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline]
    pub fn sysname(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_sysname(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline]
    pub fn sysnum(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_sysnum(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline]
    pub fn devnode(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_devnode(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline(always)]
    pub fn is_initialized(&self) -> bool {
        unsafe { udev_device_get_is_initialized(self.0.get()) == 1 }
    }

    #[inline]
    pub fn property_value(&self, key: &CStr) -> Option<&CStr> {
        match unsafe { udev_device_get_property_value(self.0.get(), key.as_ptr()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline]
    pub fn driver(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_driver(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline(always)]
    pub fn devnum(&self) -> dev_t {
        unsafe { udev_device_get_devnum(self.0.get()) }
    }

    #[inline]
    pub fn action(&self) -> Option<&CStr> {
        match unsafe { udev_device_get_action(self.0.get()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline(always)]
    pub fn seqnum(&self) -> c_ulonglong {
        unsafe { udev_device_get_seqnum(self.0.get()) }
    }

    #[inline(always)]
    pub fn usec_since_initialized(&self) -> c_ulonglong {
        unsafe { udev_device_get_usec_since_initialized(self.0.get()) }
    }

    #[inline]
    pub fn sysattr_value(&self, sysattr: &CStr) -> Option<&CStr> {
        match unsafe { udev_device_get_sysattr_value(self.0.get(), sysattr.as_ptr()) } {
            p if p.is_null() => None,
            p => Some(unsafe { CStr::from_ptr(p) }),
        }
    }

    #[inline]
    pub fn set_sysattr_value(&mut self, sysattr: &CStr, value: &CStr) -> std::io::Result<()> {
        match unsafe {
            udev_device_set_sysattr_value(self.0.get(), sysattr.as_ptr(), value.as_ptr())
        } {
            r if r < 0 => Err(std::io::Error::from_raw_os_error(-r)),
            _ => Ok(()),
        }
    }

    #[inline(always)]
    pub fn has_tag(&self, tag: &std::ffi::CStr) -> bool {
        unsafe { udev_device_has_tag(self.0.get(), tag.as_ptr()) == 1 }
    }

    #[inline(always)]
    pub fn iter_devlinks(&self) -> crate::ListIterator {
        unsafe {
            crate::ListIterator::from(crate::ListEntry::from_ptr(
                udev_device_get_devlinks_list_entry(self.0.get()),
            ))
        }
    }

    #[inline(always)]
    pub fn iter_properties(&self) -> crate::ListIterator {
        unsafe {
            crate::ListIterator::from(crate::ListEntry::from_ptr(
                udev_device_get_properties_list_entry(self.0.get()),
            ))
        }
    }

    #[inline(always)]
    pub fn iter_tags(&self) -> crate::ListIterator {
        unsafe {
            crate::ListIterator::from(crate::ListEntry::from_ptr(udev_device_get_tags_list_entry(
                self.0.get(),
            )))
        }
    }

    #[inline(always)]
    pub fn iter_sysattr(&self) -> crate::ListIterator {
        unsafe {
            crate::ListIterator::from(crate::ListEntry::from_ptr(
                udev_device_get_sysattr_list_entry(self.0.get()),
            ))
        }
    }

    #[inline(always)]
    pub fn is_mouse(&self) -> bool {
        self.property_value(Self::PROPERTY_KEY_ID_INPUT_MOUSE) == Some(c"1")
    }

    #[inline(always)]
    pub fn name(&self) -> Option<&CStr> {
        self.property_value(Self::PROPERTY_KEY_NAME)
    }
}
