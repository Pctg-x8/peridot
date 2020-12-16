//! Generic Buffer Manager port

use std::ptr::NonNull;

pub type BufferObjectFlags = u32;
pub const BO_USE_SCANOUT: BufferObjectFlags = 1 << 0;
pub const BO_USE_RENDERING: BufferObjectFlags = 1 << 2;

pub type BufferObjectFormat = u32;
pub const BO_FORMAT_XRGB8888: BufferObjectFormat = 0;
pub const BO_FORMAT_ARGB8888: BufferObjectFormat = 1;

pub type BufferObjectTransferFlags = u32;
pub const BO_TRANSFER_READ: BufferObjectTransferFlags = 1 << 0;
pub const BO_TRANSFER_WRITE: BufferObjectTransferFlags = 1 << 1;

pub const MAX_PLANES: usize = 4;

#[repr(transparent)]
pub struct Device(NonNull<self::raw::gbm_device>);
impl Device {
	pub unsafe fn from_ptr(p: *mut self::raw::gbm_device) -> Option<Self> {
		NonNull::new(p).map(Self)
	}
	pub fn as_ptr(&self) -> *mut self::raw::gbm_device { self.0.as_ptr() }

	pub fn new(fd: libc::c_int) -> Option<Self> {
		unsafe { Self::from_ptr(self::raw::gbm_create_device(fd)) }
	}
	pub fn backend_name(&self) -> &std::ffi::CStr {
		unsafe { std::ffi::CStr::from_ptr(self::raw::gbm_device_get_backend_name(self.0.as_ptr())) }
	}

	pub fn new_bo(&self, width: u32, height: u32, format: u32, flags: u32) -> Option<BufferObject> {
		unsafe { BufferObject::from_ptr(self::raw::gbm_bo_create(self.0.as_ptr(), width, height, format, flags)) }
	}
	pub fn new_bo_with_modifiers(&self, width: u32, height: u32, format: u32, modifiers: &[u64]) -> Option<BufferObject> {
		unsafe {
			BufferObject::from_ptr(
				self::raw::gbm_bo_create_with_modifiers(self.0.as_ptr(), width, height, format, modifiers.as_ptr(), modifiers.len() as _)
			)
		}
	}

	pub fn new_surface(&self, width: u32, height: u32, format: u32, flags: u32) -> Option<Surface> {
		unsafe { Surface::from_ptr(self::raw::gbm_surface_create(self.0.as_ptr(), width, height, format, flags)) }
	}
	pub fn new_surface_with_modifiers(&self, width: u32, height: u32, format: u32, modifiers: &[u64]) -> Option<Surface> {
		unsafe {
			Surface::from_ptr(
				self::raw::gbm_surface_create_with_modifiers(self.0.as_ptr(), width, height, format, modifiers.as_ptr(), modifiers.len() as _)
			)
		}
	}

	pub fn is_format_supported(&self, fmt: u32, usage: u32) -> bool {
		unsafe { self::raw::gbm_device_is_format_supported(self.0.as_ptr(), fmt, usage) != 0 }
	}
}
impl Drop for Device {
	fn drop(&mut self) {
		unsafe { self::raw::gbm_device_destroy(self.0.as_ptr()); }
	}
}

#[repr(transparent)]
pub struct BufferObject(NonNull<self::raw::gbm_bo>);
impl BufferObject {
	pub unsafe fn from_ptr(p: *mut self::raw::gbm_bo) -> Option<Self> {
		NonNull::new(p).map(Self)
	}

	pub fn fd(&self) -> libc::c_int { unsafe { self::raw::gbm_bo_get_fd(self.0.as_ptr()) } }
	pub fn format(&self) -> u32 { unsafe { self::raw::gbm_bo_get_format(self.0.as_ptr()) } }
	pub fn modifier(&self) -> u64 {
		unsafe { self::raw::gbm_bo_get_modifier(self.0.as_ptr()) }
	}
	pub fn plane_count(&self) -> usize {
		unsafe { self::raw::gbm_bo_get_plane_count(self.0.as_ptr()) as _ }
	}
	pub fn handle(&self, plane: Option<usize>) -> u64 {
		match plane {
			Some(p) => unsafe { self::raw::gbm_bo_get_handle_for_plane(self.0.as_ptr(), p as _) },
			None => unsafe { self::raw::gbm_bo_get_handle(self.0.as_ptr()) }
		}
	}
	pub fn stride(&self, plane: Option<usize>) -> u32 {
		match plane {
			Some(p) => unsafe { self::raw::gbm_bo_get_stride_for_plane(self.0.as_ptr(), p as _) },
			None => unsafe { self::raw::gbm_bo_get_stride(self.0.as_ptr()) }
		}
	}
	pub fn offset(&self, plane: usize) -> u32 {
		unsafe { self::raw::gbm_bo_get_offset(self.0.as_ptr(), plane as _) }
	}

	pub fn map(&mut self, x: u32, y: u32, width: u32, height: u32, flags: u32) -> Option<MappedBuffer> {
		let mut p = std::ptr::null_mut();
		let mut stride = 0;
		let rp = unsafe { self::raw::gbm_bo_map(self.0.as_ptr(), x, y, width, height, flags, &mut stride, &mut p) };
        // dbg!((rp, p, stride));
		if rp.is_null() {
            None
        } else {
            Some(MappedBuffer { map_data: p, mapped_address: rp, stride, obj: self })
        }
	}
	pub unsafe fn unmap(&self, p: *mut libc::c_void) {
		self::raw::gbm_bo_unmap(self.0.as_ptr(), p);
	}
}
impl Drop for BufferObject {
	fn drop(&mut self) {
		unsafe { self::raw::gbm_bo_destroy(self.0.as_ptr()); }
	}
}

#[repr(transparent)]
pub struct Surface(NonNull<self::raw::gbm_surface>);
impl Surface {
	pub unsafe fn from_ptr(p: *mut self::raw::gbm_surface) -> Option<Self> {
		NonNull::new(p).map(Self)
	}

	pub fn lock_front_buffer(&mut self) -> Option<LockedBuffer> {
		let bo = unsafe { self::raw::gbm_surface_lock_front_buffer(self.0.as_ptr()) };
		NonNull::new(bo).map(move |bo| LockedBuffer(bo, self))
	}
}
impl Drop for Surface {
	fn drop(&mut self) {
		unsafe { self::raw::gbm_surface_destroy(self.0.as_ptr()); }
	}
}

#[repr(C)]
pub struct LockedBuffer<'s>(NonNull<self::raw::gbm_bo>, &'s mut Surface);
impl Drop for LockedBuffer<'_> {
	fn drop(&mut self) {
		unsafe { self::raw::gbm_surface_release_buffer(self.1 .0.as_ptr(), self.0.as_ptr()); }
	}
}
impl std::ops::Deref for LockedBuffer<'_> {
	type Target = BufferObject;
	fn deref(&self) -> &BufferObject { unsafe { std::mem::transmute(self) } }
}

pub struct MappedBuffer<'s> {
    map_data: *mut libc::c_void,
    mapped_address: *mut libc::c_void,
    stride: u32,
    obj: &'s mut BufferObject
}
impl Drop for MappedBuffer<'_> {
	fn drop(&mut self) {
		unsafe { self::raw::gbm_bo_unmap(self.obj.0.as_ptr(), self.map_data); }
	}
}
impl MappedBuffer<'_> {
	pub fn pointer(&self) -> *mut libc::c_void { self.mapped_address }
	pub fn stride(&self) -> u32 { self.stride }
    pub fn map_data(&self) -> *mut libc::c_void { self.map_data }
}

#[allow(non_camel_case_types)]
pub mod raw {
	pub enum gbm_device {}
	pub enum gbm_bo {}
	pub enum gbm_surface {}

	#[link(name = "gbm")]
	extern "C" {
		pub fn gbm_create_device(fd: libc::c_int) -> *mut gbm_device;
		pub fn gbm_device_destroy(gbm: *mut gbm_device);
		pub fn gbm_device_is_format_supported(gbm: *mut gbm_device, format: u32, usage: u32) -> libc::c_int;
		pub fn gbm_device_get_backend_name(gbm: *mut gbm_device) -> *const libc::c_char;

		pub fn gbm_bo_create(gbm: *mut gbm_device, width: u32, height: u32, format: u32, flags: u32) -> *mut gbm_bo;
		pub fn gbm_bo_create_with_modifiers(gbm: *mut gbm_device, width: u32, height: u32, format: u32, modifiers: *const u64, count: libc::c_uint) -> *mut gbm_bo;
		pub fn gbm_bo_destroy(bo: *mut gbm_bo);
		pub fn gbm_bo_get_fd(bo: *mut gbm_bo) -> libc::c_int;
		pub fn gbm_bo_get_format(bo: *mut gbm_bo) -> u32;
		pub fn gbm_bo_get_stride(bo: *mut gbm_bo) -> u32;
		pub fn gbm_bo_get_stride_for_plane(bo: *mut gbm_bo, plane: libc::c_int) -> u32;
		pub fn gbm_bo_get_offset(bo: *mut gbm_bo, plane: libc::c_int) -> u32;
		// u64 is the largest size of the handle
		pub fn gbm_bo_get_handle(bo: *mut gbm_bo) -> u64;
		pub fn gbm_bo_get_handle_for_plane(bo: *mut gbm_bo, plane: libc::c_int) -> u64;
		pub fn gbm_bo_get_modifier(bo: *mut gbm_bo) -> u64;
		pub fn gbm_bo_get_plane_count(bo: *mut gbm_bo) -> libc::c_int;
		pub fn gbm_bo_map(bo: *mut gbm_bo, x: u32, y: u32, width: u32, height: u32, flags: u32, stride: *mut u32, map_data: *mut *mut libc::c_void) -> *mut libc::c_void;
		pub fn gbm_bo_unmap(bo: *mut gbm_bo, map_data: *mut libc::c_void);

		pub fn gbm_surface_create(gbm: *mut gbm_device, width: u32, height: u32, format: u32, flags: u32) -> *mut gbm_surface;
		pub fn gbm_surface_create_with_modifiers(gbm: *mut gbm_device, width: u32, height: u32, format: u32, modifiers: *const u64, count: libc::c_uint) -> *mut gbm_surface;
		pub fn gbm_surface_destroy(surface: *mut gbm_surface);
		pub fn gbm_surface_lock_front_buffer(surface: *mut gbm_surface) -> *mut gbm_bo;
		pub fn gbm_surface_release_buffer(surface: *mut gbm_surface, bo: *mut gbm_bo);
	}
}
