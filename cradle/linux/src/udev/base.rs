
use libc::*;
use super::ffi::*;
use std::ptr::NonNull;

#[repr(transparent)]
pub struct Context(NonNull<udev>);
impl Context {
	pub fn new() -> Option<Self> {
		unsafe { NonNull::new(udev_new()).map(Self) }
	}
	pub fn as_ptr(&self) -> *mut udev { self.0.as_ptr() }
	pub unsafe fn from_ptr(p: *mut udev) -> Option<Self> {
		NonNull::new(p).map(Self)
	}

	pub fn get_userdata(&self) -> *mut libc::c_void {
		unsafe { udev_get_userdata(self.0.as_ptr()) }
	}
	pub fn set_userdata(&mut self, d: *mut libc::c_void) {
		unsafe { udev_set_userdata(self.0.as_ptr(), d); }
	}
}
impl Drop for Context {
	fn drop(&mut self) {
		unsafe { udev_unref(self.0.as_ptr()); }
	}
}
impl Clone for Context {
	fn clone(&self) -> Self {
		unsafe { Self(NonNull::new_unchecked(udev_ref(self.0.as_ptr()))) }
	}
}

#[repr(transparent)]
pub struct ListEntry(NonNull<udev_list_entry>);
impl ListEntry {
	pub unsafe fn from_ptr(p: *mut udev_list_entry) -> Option<Self> {
		NonNull::new(p).map(Self)
	}

	pub fn next(&self) -> Option<Self> {
		unsafe { Self::from_ptr(udev_list_entry_get_next(self.0.as_ptr())) }
	}
	pub fn get_by_name(&self, name: &std::ffi::CStr) -> Option<Self> {
		unsafe {
			NonNull::new(udev_list_entry_get_by_name(self.0.as_ptr(), name.as_ptr())).map(Self)
		}
	}

	pub fn name(&self) -> Option<&std::ffi::CStr> {
		unsafe {
			let p = udev_list_entry_get_name(self.0.as_ptr());
			if p.is_null() { None } else { Some(std::ffi::CStr::from_ptr(p)) }
		}
	}
	pub fn value(&self) -> Option<&std::ffi::CStr> {
		unsafe {
			let p = udev_list_entry_get_value(self.0.as_ptr());
			if p.is_null() { None } else { Some(std::ffi::CStr::from_ptr(p)) }
		}
	}
}
#[repr(transparent)]
pub struct ListIterator(Option<ListEntry>);
impl From<ListEntry> for ListIterator { fn from(v: ListEntry) -> Self { Self(Some(v)) } }
impl From<Option<ListEntry>> for ListIterator { fn from(v: Option<ListEntry>) -> Self { Self(v) } }
impl Iterator for ListIterator {
	type Item = ListEntry;
	fn next(&mut self) -> Option<ListEntry> {
		let c = self.0.take();
		self.0 = c.as_ref().and_then(|c| c.next());
		c
	}
}

#[repr(transparent)]
pub struct Monitor(NonNull<udev_monitor>);
impl Monitor {
	pub unsafe fn from_ptr(p: *mut udev_monitor) -> Option<Self> { NonNull::new(p).map(Self) }
	pub fn as_ptr(&self) -> *mut udev_monitor { self.0.as_ptr() }

	pub fn from_netlink(context: &Context, name: &std::ffi::CStr) -> Option<Self> {
		unsafe { Self::from_ptr(udev_monitor_new_from_netlink(context.as_ptr(), name.as_ptr())) }
	}

	pub fn udev(&self) -> Context {
		unsafe { Context::from_ptr(udev_monitor_get_udev(self.as_ptr())).expect("no context?") }
	}

	pub fn enable_receiving(&self) -> Result<(), c_int> {
		let r = unsafe { udev_monitor_enable_receiving(self.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}

	pub fn set_receive_buffer_size(&self, size: c_int) -> Result<(), c_int> {
		let r = unsafe { udev_monitor_set_receive_buffer_size(self.as_ptr(), size) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn fd(&self) -> Result<c_int, c_int> {
		let r = unsafe { udev_monitor_get_fd(self.as_ptr()) };
		if r >= 0 { Ok(r) } else { Err(r) }
	}
	pub fn receive_device(&self) -> Option<super::Device> {
		unsafe { super::Device::from_ptr(udev_monitor_receive_device(self.as_ptr())) }
	}
	
	pub fn filter_add_match_subsystem_devtype(
		&self, subsystem: &std::ffi::CStr, devtype: Option<&std::ffi::CStr>
	) -> Result<(), c_int> {
		let r = unsafe {
			udev_monitor_filter_add_match_subsystem_devtype(
				self.as_ptr(), subsystem.as_ptr(), devtype.map_or_else(std::ptr::null, |s| s.as_ptr())
			)
		};
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn filter_add_match_tag(&self, tag: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_monitor_filter_add_match_tag(self.as_ptr(), tag.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn filter_update(&self) -> Result<(), c_int> {
		let r = unsafe { udev_monitor_filter_update(self.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn filter_remove(&self) -> Result<(), c_int> {
		let r = unsafe { udev_monitor_filter_remove(self.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
}
impl Drop for Monitor {
	fn drop(&mut self) {
		unsafe { udev_monitor_unref(self.as_ptr()); }
	}
}
impl Clone for Monitor {
	fn clone(&self) -> Self {
		unsafe { Self::from_ptr(udev_monitor_ref(self.as_ptr())).expect("clone failed") }
	}
}

#[repr(transparent)]
pub struct Enumerate(NonNull<udev_enumerate>);
impl Enumerate {
	pub unsafe fn from_ptr(p: *mut udev_enumerate) -> Option<Self> { NonNull::new(p).map(Self) }
	pub fn as_ptr(&self) -> *mut udev_enumerate { self.0.as_ptr() }

	pub fn new(context: &Context) -> Option<Self> {
		unsafe { Self::from_ptr(udev_enumerate_new(context.as_ptr())) }
	}

	pub fn udev(&self) -> Context {
		unsafe { Context::from_ptr(udev_enumerate_get_udev(self.as_ptr())).expect("no context?") }
	}

	pub fn add_match_subsystem(&self, subsystem: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_match_subsystem(self.as_ptr(), subsystem.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_nomatch_subsystem(&self, subsystem: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_nomatch_subsystem(self.as_ptr(), subsystem.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_match_sysattr(&self, sysattr: &std::ffi::CStr, value: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_match_sysattr(self.as_ptr(), sysattr.as_ptr(), value.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_nomatch_sysattr(&self, sysattr: &std::ffi::CStr, value: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_nomatch_sysattr(self.as_ptr(), sysattr.as_ptr(), value.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_match_property(&self, property: &std::ffi::CStr, value: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_match_property(self.as_ptr(), property.as_ptr(), value.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_match_sysname(&self, sysname: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_match_sysname(self.as_ptr(), sysname.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_match_tag(&self, tag: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_match_tag(self.as_ptr(), tag.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_match_parent(&self, parent: &super::Device) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_match_parent(self.as_ptr(), parent.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_match_is_initialized(&self) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_match_is_initialized(self.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn add_syspath(&self, syspath: &std::ffi::CStr) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_add_syspath(self.as_ptr(), syspath.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn scan_devices(&self) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_scan_devices(self.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	pub fn scan_subsystems(&self) -> Result<(), c_int> {
		let r = unsafe { udev_enumerate_scan_subsystems(self.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
	
	pub fn iter(&self) -> ListIterator {
		unsafe { ListIterator::from(ListEntry::from_ptr(udev_enumerate_get_list_entry(self.as_ptr()))) }
	}
}
impl Drop for Enumerate {
	fn drop(&mut self) {
		unsafe { udev_enumerate_unref(self.as_ptr()); }
	}
}
impl Clone for Enumerate {
	fn clone(&self) -> Self {
		unsafe { Self::from_ptr(udev_enumerate_ref(self.as_ptr())).expect("clone failed") }
	}
}

#[repr(transparent)]
pub struct Queue(NonNull<udev_queue>);
impl Queue {
	pub unsafe fn from_ptr(p: *mut udev_queue) -> Option<Self> { NonNull::new(p).map(Self) }
	pub fn as_ptr(&self) -> *mut udev_queue { self.0.as_ptr() }

	pub fn new(context: &Context) -> Option<Self> {
		unsafe { Self::from_ptr(udev_queue_new(context.as_ptr())) }
	}
	
	pub fn udev(&self) -> Context {
		unsafe { Context::from_ptr(udev_queue_get_udev(self.as_ptr())).expect("no context?") }
	}
	pub fn udev_is_active(&self) -> bool {
		unsafe { udev_queue_get_udev_is_active(self.as_ptr()) == 1 }
	}
	pub fn is_empty(&self) -> bool {
		unsafe { udev_queue_get_queue_is_empty(self.as_ptr()) == 1 }
	}
	pub fn fd(&self) -> Result<c_int, c_int> {
		let r = unsafe { udev_queue_get_fd(self.as_ptr()) };
		if r >= 0 { Ok(r) } else { Err(r) }
	}
	pub fn flush(&self) -> Result<(), c_int> {
		let r = unsafe { udev_queue_flush(self.as_ptr()) };
		if r >= 0 { Ok(()) } else { Err(r) }
	}
}
impl Drop for Queue {
	fn drop(&mut self) {
		unsafe { udev_queue_unref(self.as_ptr()); }
	}
}
impl Clone for Queue {
	fn clone(&self) -> Self {
		unsafe { Self::from_ptr(udev_queue_ref(self.as_ptr())).expect("clone failed") }
	}
}

#[repr(transparent)]
pub struct HardwareDatabase(NonNull<udev_hwdb>);
impl HardwareDatabase {
	pub unsafe fn from_ptr(p: *mut udev_hwdb) -> Option<Self> { NonNull::new(p).map(Self) }
	pub fn as_ptr(&self) -> *mut udev_hwdb { self.0.as_ptr() }

	pub fn new(context: &Context) -> Option<Self> {
		unsafe { Self::from_ptr(udev_hwdb_new(context.as_ptr())) }
	}
	
	pub fn iter_properties(&self, modalias: &std::ffi::CStr, flags: c_uint) -> ListIterator {
		unsafe {
			ListIterator::from(
				ListEntry::from_ptr(udev_hwdb_get_properties_list_entry(self.as_ptr(), modalias.as_ptr(), flags))
			)
		}
	}
}
impl Drop for HardwareDatabase {
	fn drop(&mut self) {
		unsafe { udev_hwdb_unref(self.as_ptr()); }
	}
}
impl Clone for HardwareDatabase {
	fn clone(&self) -> Self {
		unsafe { Self::from_ptr(udev_hwdb_ref(self.as_ptr())).expect("clone failed") }
	}
}
