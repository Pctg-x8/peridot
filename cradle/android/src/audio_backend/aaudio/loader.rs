//! libaaudio.so dynamic loader

use super::native;
use libloading::Library;

unsafe trait FunPtr
{
	unsafe fn from_void_ptr(p: *mut libc::c_void) -> Self;
}
unsafe impl<A, R> FunPtr for extern "C" fn(A) -> R
{
	unsafe fn from_void_ptr(p: *mut libc::c_void) -> Self { std::mem::transmute(p) }
}
unsafe impl<A, B, R> FunPtr for extern "C" fn(A, B) -> R
{
	unsafe fn from_void_ptr(p: *mut libc::c_void) -> Self { std::mem::transmute(p) }
}
unsafe impl<A, B, C, R> FunPtr for extern "C" fn(A, B, C) -> R
{
	unsafe fn from_void_ptr(p: *mut libc::c_void) -> Self { std::mem::transmute(p) }
}
unsafe impl<A, B, C, D, R> FunPtr for extern "C" fn(A, B, C, D) -> R
{
	unsafe fn from_void_ptr(p: *mut libc::c_void) -> Self { std::mem::transmute(p) }
}

pub struct LibAAudio
{
	libhandle: Library,
	pub convert_result_to_text: native::PFN_AAudio_convertResultToText,
	pub convert_stream_state_to_text: native::PFN_AAudio_convertStreamStateToText,
	pub create_stream_builder: native::PFN_AAudio_createStreamBuilder,
	pub sb_set_sample_rate: native::PFN_AAudioStreamBuilder_setSampleRate,
	pub sb_set_channel_count: native::PFN_AAudioStreamBuilder_setChannelCount,
	pub sb_set_format: native::PFN_AAudioStreamBuilder_setFormat,
	pub sb_set_sharing_mode: native::PFN_AAudioStreamBuilder_setSharingMode,
	pub sb_set_direction: native::PFN_AAudioStreamBuilder_setDirection,
	pub sb_set_performance_mode: native::PFN_AAudioStreamBuilder_setPerformanceMode,
	pub sb_set_data_callback: native::PFN_AAudioStreamBuilder_setDataCallback,
	pub sb_open_stream: native::PFN_AAudioStreamBuilder_openStream,
	pub sb_delete: native::PFN_AAudioStreamBuilder_delete,
	pub stream_close: native::PFN_AAudioStream_close,
	pub stream_request_start: native::PFN_AAudioStream_requestStart,
	pub stream_request_pause: native::PFN_AAudioStream_requestPause,
	pub stream_request_flush: native::PFN_AAudioStream_requestFlush,
	pub stream_request_stop: native::PFN_AAudioStream_requestStop,
	pub stream_get_state: native::PFN_AAudioStream_getState,
	pub stream_wait_for_state_change: native::PFN_AAudioStream_waitForStateChange
}
impl LibAAudio
{
	pub fn load() -> libloading::Result<Self>
	{
		let libhandle = Library::new("libaaudio.so")?;
		unsafe fn force_load_as<Fp: FunPtr>(lib: &Library, name: &str) -> Fp
		{
			Fp::from_void_ptr(lib.get::<Fp>(name.as_bytes()).expect(&format!("{} loading failed!", name)).into_raw().into_raw())
		}

		Ok(LibAAudio
		{
			convert_result_to_text: unsafe { force_load_as(&libhandle, "AAudio_convertResultToText") },
			convert_stream_state_to_text: unsafe { force_load_as(&libhandle, "AAudio_convertStreamStateToText") },
			create_stream_builder: unsafe { force_load_as(&libhandle, "AAudio_createStreamBuilder") },
			sb_set_sample_rate: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_setSampleRate") },
			sb_set_channel_count: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_setChannelCount") },
			sb_set_format: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_setFormat") },
			sb_set_sharing_mode: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_setSharingMode") },
			sb_set_direction: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_setDirection") },
			sb_set_performance_mode: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_setPerformanceMode") },
			sb_set_data_callback: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_setDataCallback") },
			sb_open_stream: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_openStream") },
			sb_delete: unsafe { force_load_as(&libhandle, "AAudioStreamBuilder_delete") },
			stream_close: unsafe { force_load_as(&libhandle, "AAudioStream_close") },
			stream_request_start: unsafe { force_load_as(&libhandle, "AAudioStream_requestStart") },
			stream_request_pause: unsafe { force_load_as(&libhandle, "AAudioStream_requestPause") },
			stream_request_flush: unsafe { force_load_as(&libhandle, "AAudioStream_requestFlush") },
			stream_request_stop: unsafe { force_load_as(&libhandle, "AAudioStream_requestStop") },
			stream_get_state: unsafe { force_load_as(&libhandle, "AAudioStream_getState") },
			stream_wait_for_state_change: unsafe { force_load_as(&libhandle, "AAudioStream_waitForStateChange") },
			libhandle
		})
	}
}
