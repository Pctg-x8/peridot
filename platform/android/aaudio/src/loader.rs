//! libaaudio.so dynamic loader

use crate::dl::{Library, RTLD_LOCAL, maybe_error_msg};

use super::native;

/// # Safety
///
/// Implemented type must be a c-linkage function pointer
unsafe trait FunPtr {
    unsafe fn from_void_ptr(p: *mut core::ffi::c_void) -> Self;
}
unsafe impl<A, R> FunPtr for extern "C" fn(A) -> R {
    #[inline(always)]
    unsafe fn from_void_ptr(p: *mut core::ffi::c_void) -> Self {
        unsafe { std::mem::transmute(p) }
    }
}
unsafe impl<A, B, R> FunPtr for extern "C" fn(A, B) -> R {
    #[inline(always)]
    unsafe fn from_void_ptr(p: *mut core::ffi::c_void) -> Self {
        unsafe { std::mem::transmute(p) }
    }
}
unsafe impl<A, B, C, R> FunPtr for extern "C" fn(A, B, C) -> R {
    #[inline(always)]
    unsafe fn from_void_ptr(p: *mut core::ffi::c_void) -> Self {
        unsafe { std::mem::transmute(p) }
    }
}
unsafe impl<A, B, C, D, R> FunPtr for extern "C" fn(A, B, C, D) -> R {
    #[inline(always)]
    unsafe fn from_void_ptr(p: *mut core::ffi::c_void) -> Self {
        unsafe { std::mem::transmute(p) }
    }
}

pub struct LibAAudio {
    #[allow(dead_code)]
    libhandle: Library,
    convert_result_to_text: native::PFN_AAudio_convertResultToText,
    convert_stream_state_to_text: native::PFN_AAudio_convertStreamStateToText,
    create_stream_builder: native::PFN_AAudio_createStreamBuilder,
    sb_set_sample_rate: native::PFN_AAudioStreamBuilder_setSampleRate,
    sb_set_channel_count: native::PFN_AAudioStreamBuilder_setChannelCount,
    sb_set_format: native::PFN_AAudioStreamBuilder_setFormat,
    sb_set_sharing_mode: native::PFN_AAudioStreamBuilder_setSharingMode,
    sb_set_direction: native::PFN_AAudioStreamBuilder_setDirection,
    sb_set_performance_mode: native::PFN_AAudioStreamBuilder_setPerformanceMode,
    sb_set_data_callback: native::PFN_AAudioStreamBuilder_setDataCallback,
    sb_open_stream: native::PFN_AAudioStreamBuilder_openStream,
    sb_delete: native::PFN_AAudioStreamBuilder_delete,
    stream_close: native::PFN_AAudioStream_close,
    stream_request_start: native::PFN_AAudioStream_requestStart,
    stream_request_pause: native::PFN_AAudioStream_requestPause,
    stream_request_flush: native::PFN_AAudioStream_requestFlush,
    stream_request_stop: native::PFN_AAudioStream_requestStop,
    stream_get_state: native::PFN_AAudioStream_getState,
    stream_wait_for_state_change: native::PFN_AAudioStream_waitForStateChange,
}
impl LibAAudio {
    pub fn load() -> Option<Self> {
        let dl = Library::open(c"libaaudio.so", RTLD_LOCAL)?;
        #[inline(always)]
        unsafe fn force_load_as<Fp: FunPtr>(lib: &Library, name: &core::ffi::CStr) -> Option<Fp> {
            let ptr = unsafe { lib.sym_raw(name) };
            if ptr.is_null() {
                let err = maybe_error_msg().unwrap_or(c"(Symbol located at null)");
                #[cfg(feature = "tracing")]
                tracing::error!(reason = ?err, ?name, "dlsym failed");
                #[cfg(not(feature = "tracing"))]
                eprintln!("dlsym({name:?}) failed: {err:?}");

                return None;
            }

            Some(unsafe { Fp::from_void_ptr(ptr) })
        }

        Some(LibAAudio {
            convert_result_to_text: unsafe { force_load_as(&dl, c"AAudio_convertResultToText")? },
            convert_stream_state_to_text: unsafe {
                force_load_as(&dl, c"AAudio_convertStreamStateToText")?
            },
            create_stream_builder: unsafe { force_load_as(&dl, c"AAudio_createStreamBuilder")? },
            sb_set_sample_rate: unsafe {
                force_load_as(&dl, c"AAudioStreamBuilder_setSampleRate")?
            },
            sb_set_channel_count: unsafe {
                force_load_as(&dl, c"AAudioStreamBuilder_setChannelCount")?
            },
            sb_set_format: unsafe { force_load_as(&dl, c"AAudioStreamBuilder_setFormat")? },
            sb_set_sharing_mode: unsafe {
                force_load_as(&dl, c"AAudioStreamBuilder_setSharingMode")?
            },
            sb_set_direction: unsafe { force_load_as(&dl, c"AAudioStreamBuilder_setDirection")? },
            sb_set_performance_mode: unsafe {
                force_load_as(&dl, c"AAudioStreamBuilder_setPerformanceMode")?
            },
            sb_set_data_callback: unsafe {
                force_load_as(&dl, c"AAudioStreamBuilder_setDataCallback")?
            },
            sb_open_stream: unsafe { force_load_as(&dl, c"AAudioStreamBuilder_openStream")? },
            sb_delete: unsafe { force_load_as(&dl, c"AAudioStreamBuilder_delete")? },
            stream_close: unsafe { force_load_as(&dl, c"AAudioStream_close")? },
            stream_request_start: unsafe { force_load_as(&dl, c"AAudioStream_requestStart")? },
            stream_request_pause: unsafe { force_load_as(&dl, c"AAudioStream_requestPause")? },
            stream_request_flush: unsafe { force_load_as(&dl, c"AAudioStream_requestFlush")? },
            stream_request_stop: unsafe { force_load_as(&dl, c"AAudioStream_requestStop")? },
            stream_get_state: unsafe { force_load_as(&dl, c"AAudioStream_getState")? },
            stream_wait_for_state_change: unsafe {
                force_load_as(&dl, c"AAudioStream_waitForStateChange")?
            },
            libhandle: dl,
        })
    }

    #[inline(always)]
    pub fn convert_result_to_text(&self, code: native::aaudio_result_t) -> &core::ffi::CStr {
        unsafe { core::ffi::CStr::from_ptr((self.convert_result_to_text)(code)) }
    }

    #[inline(always)]
    pub fn convert_stream_state_to_text(
        &self,
        state: native::aaudio_stream_state_t,
    ) -> &core::ffi::CStr {
        unsafe { core::ffi::CStr::from_ptr((self.convert_stream_state_to_text)(state)) }
    }

    #[inline(always)]
    pub fn create_stream_builder(
        &self,
        builder: *mut *mut native::AAudioStreamBuilder,
    ) -> crate::ResultCode {
        crate::ResultCode((self.create_stream_builder)(builder))
    }

    #[inline(always)]
    pub fn sb_set_sample_rate(&self, builder: &mut native::AAudioStreamBuilder, sample_rate: i32) {
        (self.sb_set_sample_rate)(builder, sample_rate)
    }

    #[inline(always)]
    pub fn sb_set_channel_count(
        &self,
        builder: &mut native::AAudioStreamBuilder,
        channel_count: i32,
    ) {
        (self.sb_set_channel_count)(builder, channel_count)
    }

    #[inline(always)]
    pub fn sb_set_format(
        &self,
        builder: &mut native::AAudioStreamBuilder,
        format: native::aaudio_format_t,
    ) {
        (self.sb_set_format)(builder, format)
    }

    #[inline(always)]
    pub fn sb_set_sharing_mode(
        &self,
        builder: &mut native::AAudioStreamBuilder,
        sharing_mode: native::aaudio_sharing_mode_t,
    ) {
        (self.sb_set_sharing_mode)(builder, sharing_mode)
    }

    #[inline(always)]
    pub fn sb_set_direction(
        &self,
        builder: &mut native::AAudioStreamBuilder,
        direction: native::aaudio_direction_t,
    ) {
        (self.sb_set_direction)(builder, direction)
    }

    #[inline(always)]
    pub fn sb_set_performance_mode(
        &self,
        builder: &mut native::AAudioStreamBuilder,
        mode: native::aaudio_performance_mode_t,
    ) {
        (self.sb_set_performance_mode)(builder, mode)
    }

    #[inline(always)]
    pub fn sb_set_data_callback(
        &self,
        builder: &mut native::AAudioStreamBuilder,
        callback: native::AAudioStream_dataCallback,
        user_data: *mut core::ffi::c_void,
    ) {
        (self.sb_set_data_callback)(builder, callback, user_data)
    }

    #[inline(always)]
    pub fn sb_open_stream(
        &self,
        builder: &mut native::AAudioStreamBuilder,
        stream: *mut *mut native::AAudioStream,
    ) -> crate::ResultCode {
        crate::ResultCode((self.sb_open_stream)(builder, stream))
    }

    #[inline(always)]
    pub fn sb_delete(&self, builder: &mut native::AAudioStreamBuilder) -> crate::ResultCode {
        crate::ResultCode((self.sb_delete)(builder))
    }

    #[inline(always)]
    pub fn stream_close(&self, stream: &mut native::AAudioStream) -> crate::ResultCode {
        crate::ResultCode((self.stream_close)(stream))
    }

    #[inline(always)]
    pub fn stream_request_start(&self, stream: &mut native::AAudioStream) -> crate::ResultCode {
        crate::ResultCode((self.stream_request_start)(stream))
    }

    #[inline(always)]
    pub fn stream_request_pause(&self, stream: &mut native::AAudioStream) -> crate::ResultCode {
        crate::ResultCode((self.stream_request_pause)(stream))
    }

    #[inline(always)]
    pub fn stream_request_flush(&self, stream: &mut native::AAudioStream) -> crate::ResultCode {
        crate::ResultCode((self.stream_request_flush)(stream))
    }

    #[inline(always)]
    pub fn stream_request_stop(&self, stream: &mut native::AAudioStream) -> crate::ResultCode {
        crate::ResultCode((self.stream_request_stop)(stream))
    }

    #[inline(always)]
    pub fn stream_get_state(&self, stream: &mut native::AAudioStream) -> crate::StreamState {
        crate::StreamState((self.stream_get_state)(stream))
    }

    #[inline(always)]
    pub fn stream_wait_for_state_change(
        &self,
        stream: &mut native::AAudioStream,
        input_state: crate::StreamState,
        next_state: *mut crate::StreamState,
        timeout_ns: i64,
    ) -> crate::ResultCode {
        crate::ResultCode((self.stream_wait_for_state_change)(
            stream,
            input_state.0,
            next_state.cast(),
            timeout_ns,
        ))
    }
}
