//! Direct Rendering Manager APIs

pub mod mode {
	use std::ptr::NonNull;
	use super::raw::*;

	pub use super::raw::drmModeConnection as Connection;
	pub use super::raw::drmModeSubPixel as SubPixel;

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
