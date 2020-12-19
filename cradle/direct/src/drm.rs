//! Direct Rendering Manager APIs

pub mod mode {
	use std::ptr::NonNull;
	use super::raw::*;

	pub use super::raw::drmModeConnection as Connection;
	pub use super::raw::drmModeSubPixel as SubPixel;

	#[repr(C)]
	#[derive(Clone, Copy)]
	pub enum ObjectType {
		CRTC = DRM_MODE_OBJECT_CRTC as _,
		Connector = DRM_MODE_OBJECT_CONNECTOR as _,
		Plane = DRM_MODE_OBJECT_PLANE as _
	}

	pub struct ConnectorPtr(NonNull<drmModeConnector>);
	impl ConnectorPtr {
		pub unsafe fn from_ptr(p: *mut drmModeConnector) -> Option<Self> { NonNull::new(p).map(Self) }

		pub fn get(fd: libc::c_int, id: u32) -> Option<Self> {
			unsafe { Self::from_ptr(drmModeGetConnector(fd, id)) }
		}
	}
	impl std::ops::Deref for ConnectorPtr {
		type Target = drmModeConnector;
		fn deref(&self) -> &Self::Target { unsafe { self.0.as_ref() } }
	}
	impl Drop for ConnectorPtr {
		fn drop(&mut self) {
			unsafe { drmModeFreeConnector(self.0.as_ptr()); }
		}
	}

	pub struct EncoderPtr(NonNull<drmModeEncoder>);
	impl EncoderPtr {
		pub unsafe fn from_ptr(p: *mut drmModeEncoder) -> Option<Self> { NonNull::new(p).map(Self) }

		pub fn get(fd: libc::c_int, id: u32) -> Option<Self> {
			unsafe { Self::from_ptr(drmModeGetEncoder(fd, id)) }
		}
	}
	impl std::ops::Deref for EncoderPtr {
		type Target = drmModeEncoder;
		fn deref(&self) -> &Self::Target { unsafe { self.0.as_ref() } }
	}
	impl Drop for EncoderPtr {
		fn drop(&mut self) { unsafe { drmModeFreeEncoder(self.0.as_ptr()); } }
	}

	pub struct PlaneResourcesPtr(NonNull<drmModePlaneRes>);
	impl PlaneResourcesPtr {
		pub unsafe fn from_ptr(p: *mut drmModePlaneRes) -> Option<Self> { NonNull::new(p).map(Self) }

		pub fn get(fd: libc::c_int) -> Option<Self> {
			unsafe { Self::from_ptr(drmModeGetPlaneResources(fd)) }
		}
	}
	impl std::ops::Deref for PlaneResourcesPtr {
		type Target = drmModePlaneRes;
		fn deref(&self) -> &Self::Target { unsafe { self.0.as_ref() } }
	}
	impl Drop for PlaneResourcesPtr {
		fn drop(&mut self) { unsafe { drmModeFreePlaneResources(self.0.as_ptr()); } }
	}

	pub struct PlanePtr(NonNull<drmModePlane>);
	impl PlanePtr {
		pub unsafe fn from_ptr(p: *mut drmModePlane) -> Option<Self> { NonNull::new(p).map(Self) }

		pub fn get(fd: libc::c_int, id: u32) -> Option<Self> {
			unsafe { Self::from_ptr(drmModeGetPlane(fd, id)) }
		}
	}
	impl std::ops::Deref for PlanePtr {
		type Target = drmModePlane;
		fn deref(&self) -> &Self::Target { unsafe { self.0.as_ref() } }
	}
	impl Drop for PlanePtr {
		fn drop(&mut self) { unsafe { drmModeFreePlane(self.0.as_ptr()); } }
	}

	pub struct ObjectProperties(NonNull<drmModeObjectProperties>);
	impl ObjectProperties {
		pub unsafe fn from_ptr(p: *mut drmModeObjectProperties) -> Option<Self> { NonNull::new(p).map(Self) }

		pub fn get(fd: libc::c_int, id: u32, ty: ObjectType) -> Option<Self> {
			unsafe { Self::from_ptr(drmModeObjectGetProperties(fd, id, ty as _)) }
		}
	}
	impl std::ops::Deref for ObjectProperties {
		type Target = drmModeObjectProperties;
		fn deref(&self) -> &Self::Target { unsafe { self.0.as_ref() } }
	}
	impl Drop for ObjectProperties {
		fn drop(&mut self) { unsafe { drmModeFreeObjectProperties(self.0.as_ptr()); } }
	}

	pub struct Property(NonNull<drmModeProperty>);
	impl Property {
		pub unsafe fn from_ptr(p: *mut drmModeProperty) -> Option<Self> { NonNull::new(p).map(Self) }

		pub fn get(fd: libc::c_int, id: u32) -> Option<Self> {
			unsafe { Self::from_ptr(drmModeGetProperty(fd, id)) }
		}
	}
	impl std::ops::Deref for Property {
		type Target = drmModeProperty;
		fn deref(&self) -> &Self::Target { unsafe { self.0.as_ref() } }
	}
	impl Drop for Property {
		fn drop(&mut self) { unsafe { drmModeFreeProperty(self.0.as_ptr()); } }
	}

	pub struct AtomicRequest(NonNull<drmModeAtomicReq>);
	impl AtomicRequest {
		pub fn new() -> Option<Self> {
			unsafe { NonNull::new(drmModeAtomicAlloc()).map(Self) }
		}

		pub fn add_property(&mut self, object_id: u32, property_id: u32, value: u64) -> Result<(), libc::c_int> {
			let r = unsafe { drmModeAtomicAddProperty(self.0.as_ptr(), object_id, property_id, value) };
			if r < 0 { Err(r) } else { Ok(()) }
		}
		pub fn commit(self, fd: libc::c_int, flags: u32, user_data: *mut libc::c_void) -> Result<(), libc::c_int> {
			let r = unsafe { drmModeAtomicCommit(fd, self.0.as_ptr(), flags, user_data) };
			if r != 0 { Err(r) } else { Ok(()) }
		}
	}
	impl Drop for AtomicRequest {
		fn drop(&mut self) { unsafe { drmModeAtomicFree(self.0.as_ptr()); } }
	}
}

pub mod raw {
	#[repr(C)]
	pub struct drmPciBusInfo {
		pub domain: u16,
		pub bus: u8,
		pub dev: u8,
		pub func: u8
	}
	#[repr(C)]
	pub struct drmPciDeviceInfo {
		pub vendor_id: u16,
		pub device_id: u16,
		pub subvendor_id: u16,
		pub subdevice_id: u16,
		pub revision_id: u8
	}
	#[repr(C)]
	pub struct drmUsbBusInfo {
		pub bus: u8,
		pub dev: u8
	}
	#[repr(C)]
	pub struct drmUsbDeviceInfo {
		pub vendor: u16,
		pub product: u16
	}
	const DRM_PLATFORM_DEVICE_NAME_LEN: usize = 512;
	#[repr(C)]
	pub struct drmPlatformBusInfo {
		pub fullname: [libc::c_char; DRM_PLATFORM_DEVICE_NAME_LEN]
	}
	#[repr(C)]
	pub struct drmPlatformDeviceInfo {
		pub compatible: *mut *mut libc::c_char
	}
	const DRM_HOST1X_DEVICE_NAME_LEN: usize = 512;
	#[repr(C)]
	pub struct drmHost1xBusInfo {
		pub fullname: [libc::c_char; DRM_HOST1X_DEVICE_NAME_LEN]
	}
	#[repr(C)]
	pub struct drmHost1xDeviceInfo {
		pub compatible: *mut *mut libc::c_char
	}
	#[repr(C)]
	pub union drmDeviceBusInfo {
		pub pci: *mut drmPciBusInfo,
		pub usb: *mut drmUsbBusInfo,
		pub platform: *mut drmPlatformBusInfo,
		pub host1x: *mut drmHost1xBusInfo
	}
	#[repr(C)]
	pub union drmDeviceInfo {
		pub pci: *mut drmPciDeviceInfo,
		pub usb: *mut drmUsbDeviceInfo,
		pub platform: *mut drmPlatformDeviceInfo,
		pub host1x: *mut drmHost1xDeviceInfo
	}
	#[repr(C)]
	pub struct drmDevice {
		pub nodes: *mut *mut libc::c_char,
		pub available_nodes: libc::c_int,
		pub bustype: libc::c_int,
		pub businfo: drmDeviceBusInfo,
		pub deviceinfo: drmDeviceInfo
	}
	
	#[repr(C)]
	pub struct drmModeRes {
		pub count_fbs: libc::c_int,
		pub fbs: *mut u32,
		pub count_crtcs: libc::c_int,
		pub crtcs: *mut u32,
		pub count_connectors: libc::c_int,
		pub connectors: *mut u32,
		pub count_encoders: libc::c_int,
		pub encoders: *mut u32,
		pub min_width: u32,
		pub max_width: u32,
		pub min_height: u32,
		pub max_height: u32
	}
	impl drmModeRes {
		pub fn connectors(&self) -> &[u32] {
			unsafe { std::slice::from_raw_parts(self.connectors, self.count_connectors as _) }
		}
		pub fn encoders(&self) -> &[u32] {
			unsafe { std::slice::from_raw_parts(self.encoders, self.count_encoders as _) }
		}
		pub fn crtcs(&self) -> &[u32] {
			unsafe { std::slice::from_raw_parts(self.crtcs, self.count_crtcs as _) }
		}
	}
	
	const DRM_DISPLAY_MODE_LEN: usize = 32;
	const DRM_MODE_TYPE_PREFERRED: u32 = 1 << 3;
	#[repr(C)]
	pub struct drmModeModeInfo {
		pub clock: u32,
		pub hdisplay: u16,
		pub hsync_start: u16,
		pub hsync_end: u16,
		pub htotal: u16,
		pub hskew: u16,
		pub vdisplay: u16,
		pub vsync_start: u16,
		pub vsync_end: u16,
		pub vtotal: u16,
		pub vscan: u16,
		pub vrefresh: u32,
		pub flags: u32,
		pub r#type: u32,
		pub name: [libc::c_char; DRM_DISPLAY_MODE_LEN]
	}
	impl drmModeModeInfo {
		pub fn is_preferred(&self) -> bool {
			(self.r#type & DRM_MODE_TYPE_PREFERRED) != 0
		}
	}
	
	#[repr(C)]
	#[derive(PartialEq, Eq)]
	pub enum drmModeConnection {
		Connected = 1,
		Disconnected = 2,
		UnknownConnection = 3
	}
	#[repr(C)]
	#[derive(PartialEq, Eq)]
	pub enum drmModeSubPixel {
		Unknown = 1,
		HorizontalRGB = 2,
		HorizontalBGR = 3,
		VerticalRGB = 4,
		VerticalBGR = 5,
		None = 6
	}
	#[repr(C)]
	pub struct drmModeConnector {
		pub connector_id: u32,
		pub encoder_id: u32,
		pub connector_type: u32,
		pub connector_type_id: u32,
		pub connection: drmModeConnection,
		pub mm_width: u32,
		pub mm_height: u32,
		pub subpixel: drmModeSubPixel,
		pub count_modes: libc::c_int,
		pub modes: *mut drmModeModeInfo,
		pub count_props: libc::c_int,
		pub props: *mut u32,
		pub prop_values: *mut u64,
		pub count_encoders: libc::c_int,
		pub encoders: *mut u32
	}
	impl drmModeConnector {
		pub fn modes(&self) -> &[drmModeModeInfo] {
			unsafe { std::slice::from_raw_parts(self.modes, self.count_modes as _) }
		}
	}
	
	#[repr(C)]
	pub struct drmModeEncoder {
		pub encoder_id: u32,
		pub encoder_type: u32,
		pub crtc_id: u32,
		pub possible_crtcs: u32,
		pub possible_clones: u32
	}
	impl drmModeEncoder {
		pub fn has_possible_crtc_index_bit(&self, index: usize) -> bool {
			// possible_crtcsはビットマスクらしい
			// https://gitlab.freedesktop.org/mesa/kmscube/-/blob/master/drm-common.c#L132
			(self.possible_crtcs & (1 << index)) != 0
		}
	}

	#[repr(C)]
	pub struct drmModePlaneRes {
		pub count_planes: u32,
		pub planes: *mut u32
	}
	impl drmModePlaneRes {
		pub fn planes(&self) -> &[u32] {
			unsafe { std::slice::from_raw_parts(self.planes, self.count_planes as _) }
		}
	}

	#[repr(C)]
	pub struct drmModePlane {
		pub count_format: u32,
		pub formats: *mut u32,
		pub plane_id: u32,
		pub crtc_id: u32,
		pub fb_id: u32,
		pub crtc_x: u32,
		pub crtc_y: u32,
		pub x: u32,
		pub y: u32,
		pub possible_crtcs: u32,
		pub gamma_size: u32
	}

	pub const DRM_MODE_OBJECT_CRTC: u32 = 0xcccccccc;
	pub const DRM_MODE_OBJECT_CONNECTOR: u32 = 0xc0c0c0c0;
	pub const DRM_MODE_OBJECT_PLANE: u32 = 0xeeeeeeee;

	#[repr(C)]
	pub struct drmModeObjectProperties {
		pub count_props: u32,
		pub props: *mut u32,
		pub prop_values: *mut u64
	}
	impl drmModeObjectProperties {
		pub fn props(&self) -> &[u32] {
			unsafe { std::slice::from_raw_parts(self.props, self.count_props as _) }
		}
		pub fn prop_values(&self) -> &[u64] {
			unsafe { std::slice::from_raw_parts(self.prop_values, self.count_props as _) }
		}
		pub fn prop_pairs<'s>(&'s self) -> impl Iterator<Item = (u32, u64)> + 's {
			self.props().iter().copied().zip(self.prop_values().iter().copied())
		}
	}

	pub const DRM_PROP_NAME_LEN: usize = 32;
	#[repr(C)]
	pub struct drm_mode_property_enum {
		pub value: u64,
		pub name: [libc::c_char; DRM_PROP_NAME_LEN]
	}
	#[repr(C)]
	pub struct drmModeProperty {
		pub prop_id: u32,
		pub flags: u32,
		pub name: [libc::c_char; DRM_PROP_NAME_LEN],
		pub count_values: libc::c_int,
		pub values: *mut u64,
		pub count_enums: libc::c_int,
		pub enums: *mut drm_mode_property_enum,
		pub count_blobs: libc::c_int,
		pub blob_ids: *mut u32
	}
	impl drmModeProperty {
		pub fn name(&self) -> &std::ffi::CStr {
			unsafe { std::ffi::CStr::from_ptr(self.name.as_ptr()) }
		}
	}

	pub const DRM_PLANE_TYPE_OVERLAY: u64 = 0;
	pub const DRM_PLANE_TYPE_PRIMARY: u64 = 1;
	pub const DRM_PLANE_TYPE_CURSOR: u64 = 2;

	#[repr(C)]
	pub struct drmEventContext {
		pub version: libc::c_int,
		pub vblank_handler: Option<extern "C" fn(
			fd: libc::c_int, sequence: libc::c_uint, tv_sec: libc::c_uint, tv_usec: libc::c_uint,
			user_data: *mut libc::c_void
		)>,
		pub page_flip_handler: Option<extern "C" fn(
			fd: libc::c_int, sequence: libc::c_uint, tv_sec: libc::c_uint, tv_usec: libc::c_uint,
			user_data: *mut libc::c_void)>,
		pub page_flip_handler2: Option<extern "C" fn(
			fd: libc::c_int, sequence: libc::c_uint, tv_sec: libc::c_uint, tv_usec: libc::c_uint,
			crtc_id: libc::c_uint, user_data: *mut libc::c_void
		)>,
		pub sequence_handler: Option<extern "C" fn(
			fd: libc::c_int, sequence: libc::c_uint, ns: libc::c_uint, user_data: *mut libc::c_void
		)>
	}

	#[repr(C)]
	pub struct drm_mode_create_dumb {
		pub height: u32,
		pub width: u32,
		pub bpp: u32,
		pub flags: u32,
		pub handle: u32,
		pub pitch: u32,
		pub size: u32
	}
	impl drm_mode_create_dumb {
		pub fn new_request(width: u32, height: u32, bpp: u32, flags: u32) -> Self {
			drm_mode_create_dumb {
				height, width, bpp, flags,
				handle: 0, pitch: 0, size: 0
			}
		}
	}

	#[repr(C)]
	pub struct drm_mode_map_dumb {
		pub handle: u32,
		pub _pad: u32,
		pub offset: u64
	}

	pub enum drmModeAtomicReq {}
	pub const DRM_MODE_ATOMIC_ALLOW_MODESET: u32 = 0x0400;

	pub const DRM_CLIENT_CAP_UNIVERSAL_PLANES: u64 = 2;
	pub const DRM_CLIENT_CAP_ATOMIC: u64 = 3;

	const _IOC_NRSHIFT: libc::c_ulong = 0;
	const _IOC_NRBITS: libc::c_ulong = 8;
	const _IOC_TYPESHIFT: libc::c_ulong = _IOC_NRSHIFT + _IOC_NRBITS;
	const _IOC_TYPEBITS: libc::c_ulong = 8;
	const _IOC_SIZESHIFT: libc::c_ulong = _IOC_TYPESHIFT + _IOC_TYPEBITS;
	const _IOC_SIZEBITS: libc::c_ulong = 14;
	const _IOC_DIRSHIFT: libc::c_ulong = _IOC_SIZESHIFT + _IOC_SIZEBITS;
	const _IOC_DIRBITS: libc::c_ulong = 2;
	const _IOC_WRITE: u8 = 1;
	const _IOC_READ: u8 = 2;
	const fn _IOC(dir: u8, ty: u8, nr: u8, size: u16) -> libc::c_ulong {
		((dir as libc::c_ulong) << _IOC_DIRSHIFT) |
		((ty as libc::c_ulong) << _IOC_TYPESHIFT) |
		((nr as libc::c_ulong) << _IOC_NRSHIFT) |
		((size as libc::c_ulong) << _IOC_SIZESHIFT)
	}
	const fn _IOWR<T>(ty: u8, nr: u8) -> libc::c_ulong {
		_IOC(_IOC_READ | _IOC_WRITE, ty, nr, std::mem::size_of::<T>() as _)
	}

	pub const DRM_IOCTL_BASE: u8 = b'd';
	pub const fn DRM_IOWR<T>(nr: u8) -> libc::c_ulong { _IOWR::<T>(DRM_IOCTL_BASE, nr) }
	pub const DRM_IOCTL_MODE_CREATE_DUMB: libc::c_ulong = DRM_IOWR::<drm_mode_create_dumb>(0xb2);
	pub const DRM_IOCTL_MODE_MAP_DUMB: libc::c_ulong = DRM_IOWR::<drm_mode_map_dumb>(0xb3);
	
	#[link(name = "drm")]
	extern "C" {
		pub fn drmGetDevices2(flags: u32, devices: *mut *mut drmDevice, max_devices: libc::c_int) -> libc::c_int;
		pub fn drmGetMagic(fd: libc::c_int, magic: *mut libc::c_uint) -> libc::c_int;
		pub fn drmAuthMagic(fd: libc::c_int, magic: libc::c_uint) -> libc::c_int;
		pub fn drmModeGetResources(fd: libc::c_int) -> *mut drmModeRes;
		pub fn drmModeGetConnector(fd: libc::c_int, connector_id: u32) -> *mut drmModeConnector;
		pub fn drmModeFreeConnector(ptr: *mut drmModeConnector);
		pub fn drmModeGetEncoder(fd: libc::c_int, encoder_id: u32) -> *mut drmModeEncoder;
		pub fn drmModeFreeEncoder(ptr: *mut drmModeEncoder);
		pub fn drmModeAddFB(
			fd: libc::c_int, width: u32, height: u32, depth: u8, bpp: u8, pitch: u32, bo_handle: u32, buf_id: *mut u32
		) -> libc::c_int;
		pub fn drmModeAddFB2(
			fd: libc::c_int, width: u32, height: u32, pixel_format: u32,
			bo_handles: *const u32, pitches: *const u32, offsets: *const u32, buf_id: *mut u32, flags: u32
		) -> libc::c_int;
		pub fn drmModeAddFB2WithModifiers(
			fd: libc::c_int, width: u32, height: u32, pixel_format: u32,
			bo_handles: *const u32, pitches: *const u32, offsets: *const u32, modifiers: *const u64,
			buf_id: *mut u32, flags: u32
		) -> libc::c_int;
		pub fn drmModeSetCrtc(
			fd: libc::c_int, crtc_id: u32, buffer_id: u32, x: u32, y: u32, connectors: *mut u32, count: libc::c_int,
			mode: *mut drmModeModeInfo
		) -> libc::c_int;
		pub fn drmModePageFlip(
			fd: libc::c_int, crtc_id: u32, fb_id: u32, flags: u32, user_data: *mut libc::c_void
		) -> libc::c_int;
		pub fn drmHandleEvent(fd: libc::c_int, evctx: *mut drmEventContext) -> libc::c_int;
		pub fn drmModeGetPlaneResources(fd: libc::c_int) -> *mut drmModePlaneRes;
		pub fn drmModeFreePlaneResources(ptr: *mut drmModePlaneRes);
		pub fn drmModeGetPlane(fd: libc::c_int, plane_id: u32) -> *mut drmModePlane;
		pub fn drmModeFreePlane(ptr: *mut drmModePlane);
		pub fn drmModeObjectGetProperties(fd: libc::c_int, object_id: u32, object_type: u32) -> *mut drmModeObjectProperties;
		pub fn drmModeFreeObjectProperties(ptr: *mut drmModeObjectProperties);
		pub fn drmModeGetProperty(fd: libc::c_int, property_id: u32) -> *mut drmModeProperty;
		pub fn drmModeFreeProperty(ptr: *mut drmModeProperty);

		pub fn drmSetClientCap(fd: libc::c_int, capability: u64, value: u64) -> libc::c_int;
		pub fn drmIoctl(fd: libc::c_int, request: libc::c_ulong, arg: *mut libc::c_void) -> libc::c_int;

		pub fn drmModeAtomicAlloc() -> *mut drmModeAtomicReq;
		pub fn drmModeAtomicFree(req: *mut drmModeAtomicReq);
		pub fn drmModeAtomicAddProperty(req: *mut drmModeAtomicReq, object_id: u32, property_id: u32, value: u64) -> libc::c_int;
		pub fn drmModeAtomicCommit(fd: libc::c_int, req: *mut drmModeAtomicReq, flags: u32, user_data: *mut libc::c_void) -> libc::c_int;
		pub fn drmModeCreatePropertyBlob(fd: libc::c_int, data: *const libc::c_void, size: usize, id: *mut u32) -> libc::c_int;
		pub fn drmModeDestroyPropertyBlob(fd: libc::c_int, id: u32) -> libc::c_int;
	}
	
	pub const fn fourcc_code(a: u8, b: u8, c: u8, d: u8) -> u32 {
		u32::from_le_bytes([a, b, c, d])
	}
	pub const DRM_FORMAT_ARGB8888: u32 = fourcc_code(b'A', b'R', b'2', b'4');
	pub const DRM_FORMAT_ABGR8888: u32 = fourcc_code(b'A', b'B', b'2', b'4');
	pub const DRM_FORMAT_RGBA8888: u32 = fourcc_code(b'R', b'A', b'2', b'4');
	pub const DRM_FORMAT_BGRA8888: u32 = fourcc_code(b'B', b'A', b'2', b'4');
	
	pub const DRM_MODE_PAGE_FLIP_EVENT: u32 = 0x01;	
}
use self::raw::*;
