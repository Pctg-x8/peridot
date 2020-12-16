//! EGL Light Binding

use std::ffi::CStr;
use once_cell::unsync::Lazy;

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct DisplayRef(EGLDisplay);
impl DisplayRef {
	pub fn from_gbm_device(device: &crate::gbm::Device) -> Option<Self> {
		let fp = unsafe { get_fnptr::<PFNEGLGETPLATFORMDISPLAYPROC>(b"eglGetPlatformDisplayEXT\0")? };
		let p = fp(EGL_PLATFORM_GBM_KHR, device.as_ptr() as *mut _, std::ptr::null());
		if p.is_null() { None } else { Some(Self(p)) }
	}

	pub fn vendor_name<'a>(self) -> Option<&'a CStr> {
		let p = unsafe { eglQueryString(self.0, EGL_VENDOR) };
		if p.is_null() { None } else { unsafe { Some(CStr::from_ptr(p)) } }
	}
}

pub fn initialize(display: DisplayRef) -> Option<(i32, i32)> {
	let (mut major, mut minor) = (0, 0);
	let res = unsafe { eglInitialize(display.0, &mut major, &mut minor) };
	if res == 0 { None } else { Some((major, minor)) }
}

pub struct Context(EGLContext, DisplayRef);
impl Context {
	pub fn new(
		display: DisplayRef, config: Option<EGLConfig>, share_context: Option<&Context>, attrib: &[EGLint]
	) -> Option<Self> {
		let cfg = config.unwrap_or_else(std::ptr::null_mut);
		let share_ctx = share_context.map_or_else(std::ptr::null_mut, |p| p.0);
		let p = unsafe { eglCreateContext(display.0, cfg, share_ctx, attrib.as_ptr()) };
		if p.is_null() { None } else { Some(Self(p, display)) }
	}

	pub fn make_current(&self, draw: Option<EGLSurface>, read: Option<EGLSurface>) -> bool {
		let draw = draw.unwrap_or_else(std::ptr::null_mut);
		let read = read.unwrap_or_else(std::ptr::null_mut);

		unsafe { eglMakeCurrent(self.1 .0, draw, read, self.0) != 0 }
	}
}
impl Drop for Context {
	fn drop(&mut self) {
		unsafe { eglDestroyContext(self.1 .0, self.0); }
	}
}

#[derive(Clone, Copy)]
#[repr(C)]
pub enum API {
	OpenGL = EGL_OPENGL_API as _
}
pub fn bind_api(api: API) -> bool { unsafe { eglBindAPI(api as _) != 0 } }

#[derive(Clone, Copy)]
pub enum ImageTarget {
	DMABufExt = EGL_LINUX_DMA_BUF_EXT as _
}
pub struct Image(EGLImageKHR, DisplayRef);
impl Image {
	pub fn new(
		display: DisplayRef, context: Option<EGLContext>,
		target: ImageTarget, client_buffer: Option<EGLClientBuffer>, attribs: &[EGLint]
	) -> Option<Self> {
		let ctx = context.unwrap_or(EGL_NO_CONTEXT);
		let cbuf = client_buffer.unwrap_or_else(std::ptr::null_mut);
		let fp = unsafe { get_fnptr::<PFNEGLCREATEIMAGEKHRPROC>(b"eglCreateImageKHR\0")? };
		let p = fp(display.0, ctx, target as _, cbuf, attribs.as_ptr());
		if p.is_null() { None } else { Some(Self(p, display)) }
	}
	pub fn as_ptr(&self) -> EGLImageKHR { self.0 }
}
impl Drop for Image {
	fn drop(&mut self) {
		let fp = unsafe {
			get_fnptr::<PFNEGLDESTROYIMAGEKHRPROC>(b"eglDestroyImageKHR\0").expect("no destroy fnptr found")
		};
		fp(self.1 .0, self.0);
	}
}

const EGL_QUERY_DEVICES_EXT: Lazy<PFNEGLQUERYDEVICESEXTPROC> = Lazy::new(|| unsafe {
	get_fnptr(b"eglQueryDevicesEXT\0").expect("no eglQueryDeviceEXT found")
});
const EGL_QUERY_DEVICE_STRING_EXT: Lazy<PFNEGLQUERYDEVICESTRINGEXTPROC> = Lazy::new(|| unsafe {
	get_fnptr(b"eglQueryDeviceStringEXT\0").expect("no eglQueryDeviceStringEXT found")
});

#[derive(Clone, Copy)]
pub struct Device(EGLDeviceEXT);
impl Device {
	pub fn query() -> Vec<Self> {
		let mut count = 0;
		(*EGL_QUERY_DEVICES_EXT)(0, std::ptr::null_mut(), &mut count);
		let mut ptrs = vec![std::ptr::null_mut(); count as _];
		(*EGL_QUERY_DEVICES_EXT)(count, ptrs.as_mut_ptr(), &mut count);
		ptrs.into_iter().map(Self).collect()
	}

	pub fn get_string<'a>(self, name: EGLint) -> Option<&'a CStr> {
		let p = (*EGL_QUERY_DEVICE_STRING_EXT)(self.0, name);
		if p.is_null() { None } else { Some(unsafe { CStr::from_ptr(p) }) }
	}
}

pub fn get_error() -> EGLint { unsafe { eglGetError() } }

pub unsafe fn get_fnptr<T: FunctionPtr>(name_bytes_with_nul: &[u8]) -> Option<T> {
	let p = eglGetProcAddress(CStr::from_bytes_with_nul_unchecked(name_bytes_with_nul).as_ptr());
	if (p as *const ()).is_null() { None } else { Some(T::from_raw(p)) }
}
pub unsafe trait FunctionPtr {
	unsafe fn from_raw(raw: extern fn()) -> Self;
}
unsafe impl FunctionPtr for PFNEGLGETPLATFORMDISPLAYPROC {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}
unsafe impl FunctionPtr for PFNEGLCREATEIMAGEKHRPROC {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}
unsafe impl FunctionPtr for PFNEGLDESTROYIMAGEKHRPROC {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}
unsafe impl FunctionPtr for PFNEGLQUERYDEVICESEXTPROC {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}
unsafe impl FunctionPtr for PFNEGLQUERYDEVICESTRINGEXTPROC {
	unsafe fn from_raw(raw: extern fn()) -> Self { std::mem::transmute(raw) }
}

pub mod raw {
	#![allow(non_snake_case)]

	pub type EGLDisplay = *mut libc::c_void;
	pub type EGLContext = *mut libc::c_void;
	pub type EGLSurface = *mut libc::c_void;
	pub type EGLConfig = *mut libc::c_void;
	pub type EGLClientBuffer = *mut libc::c_void;
	pub type EGLenum = libc::c_uint;
	pub type EGLAttrib = isize;
	pub type EGLint = i32;
	pub type EGLBoolean = libc::c_uint;

	pub const EGL_NO_CONTEXT: EGLContext = std::ptr::null_mut();
	pub const EGL_VENDOR: EGLint = 0x3053;
	pub const EGL_OPENGL_API: EGLenum = 0x30a2;
	pub const EGL_CONTEXT_CLIENT_VERSION: EGLAttrib = 0x3098;
	pub const EGL_CONTEXT_OPENGL_DEBUG: EGLAttrib = 0x31b0;

	pub const EGL_NONE: EGLAttrib = 0x3038;
	pub const EGL_WIDTH: EGLAttrib = 0x3057;
	pub const EGL_HEIGHT: EGLAttrib = 0x3056;

	pub type EGLDeviceEXT = *mut libc::c_void;

	#[link(name = "EGL")]
	extern "C" {
		pub fn eglGetProcAddress(procname: *const libc::c_char) -> extern fn();
		pub fn eglInitialize(dpy: EGLDisplay, major: *mut EGLint, minor: *mut EGLint) -> EGLBoolean;
		pub fn eglQueryString(dpy: EGLDisplay, name: EGLint) -> *const libc::c_char;
		pub fn eglBindAPI(api: EGLenum) -> EGLBoolean;
		pub fn eglMakeCurrent(dpy: EGLDisplay, draw: EGLSurface, read: EGLSurface, ctx: EGLContext) -> EGLBoolean;
		pub fn eglGetError() -> EGLint;
		pub fn eglCreateContext(dpy: EGLDisplay, config: EGLConfig, share_context: EGLContext, attrib_list: *const EGLint) -> EGLContext;
		pub fn eglDestroyContext(dpy: EGLDisplay, ctx: EGLContext) -> EGLBoolean;
	}

	pub const EGL_PLATFORM_GBM_KHR: EGLenum = 0x31d7;

	pub type PFNEGLGETPLATFORMDISPLAYPROC = extern "C" fn(
		platform: EGLenum, native_display: *mut libc::c_void, attrib_list: *const EGLAttrib
	) -> EGLDisplay;

	pub type EGLImageKHR = *mut libc::c_void;
	pub type PFNEGLCREATEIMAGEKHRPROC = extern "C" fn(
		dpy: EGLDisplay, ctx: EGLContext, target: EGLenum, buffer: EGLClientBuffer, attrib_list: *const EGLint
	) -> EGLImageKHR;
	pub type PFNEGLDESTROYIMAGEKHRPROC = extern "C" fn(dpy: EGLDisplay, image: EGLImageKHR) -> EGLBoolean;

	pub const EGL_LINUX_DMA_BUF_EXT: EGLenum = 0x3270;
	pub const EGL_LINUX_DRM_FOURCC_EXT: EGLAttrib = 0x3271;
	pub const EGL_DMA_BUF_PLANE0_FD_EXT: EGLAttrib = 0x3272;
	pub const EGL_DMA_BUF_PLANE0_OFFSET_EXT: EGLAttrib = 0x3273;
	pub const EGL_DMA_BUF_PLANE0_PITCH_EXT: EGLAttrib = 0x3274;
	pub const EGL_DMA_BUF_PLANE0_MODIFIER_LO_EXT: EGLAttrib = 0x3443;
	pub const EGL_DMA_BUF_PLANE0_MODIFIER_HI_EXT: EGLAttrib = 0x3444;

	pub type PFNEGLQUERYDEVICESEXTPROC = extern "C" fn(max_devices: EGLint, devices: *mut EGLDeviceEXT, num_devices: *mut EGLint) -> EGLBoolean;
	pub type PFNEGLQUERYDEVICESTRINGEXTPROC = extern "C" fn(device: EGLDeviceEXT, name: EGLint) -> *const libc::c_char;
}
use self::raw::*;
