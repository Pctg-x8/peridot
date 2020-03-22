//! libaaudio bindings(def only)

#![allow(non_camel_case_types)]

use libc::{c_char, c_void};

pub type aaudio_direction_t = i32;
pub const AAUDIO_DIRECTION_OUTPUT: aaudio_direction_t = 0;
pub const AAUDIO_DIRECTION_INPUT: aaudio_direction_t = 1;

pub type aaudio_format_t = i32;
pub const AAUDIO_FORMAT_INVALID: aaudio_format_t = -1;
pub const AAUDIO_FORMAT_UNSPECIFIED: aaudio_format_t = 0;
pub const AAUDIO_FORMAT_PCM_I16: aaudio_format_t = 1;
pub const AAUDIO_FORMAT_PCM_FLOAT: aaudio_format_t = 2;

pub type aaudio_result_t = i32;
pub const AAUDIO_OK: aaudio_result_t = 0;
pub const AAUDIO_ERROR_BASE: aaudio_result_t = -900;
pub const AAUDIO_ERROR_DISCONNECTED: aaudio_result_t = AAUDIO_ERROR_BASE + 1;
pub const AAUDIO_ERROR_ILLEGAL_ARGUMENT: aaudio_result_t = AAUDIO_ERROR_BASE + 2;
pub const AAUDIO_ERROR_INTERNAL: aaudio_result_t = AAUDIO_ERROR_ILLEGAL_ARGUMENT + 2;
pub const AAUDIO_ERROR_INVALID_STATE: aaudio_result_t = AAUDIO_ERROR_INTERNAL + 1;
pub const AAUDIO_ERROR_INVALID_HANDLE: aaudio_result_t = AAUDIO_ERROR_INVALID_STATE + 3;
pub const AAUDIO_ERROR_UNIMPLEMENTED: aaudio_result_t = AAUDIO_ERROR_INVALID_HANDLE + 2;
pub const AAUDIO_ERROR_UNAVAILABLE: aaudio_result_t = AAUDIO_ERROR_UNIMPLEMENTED + 1;
pub const AAUDIO_ERROR_NO_FREE_HANDLES: aaudio_result_t = AAUDIO_ERROR_UNAVAILABLE + 1;
pub const AAUDIO_ERROR_NO_MEMORY: aaudio_result_t = AAUDIO_ERROR_NO_FREE_HANDLES + 1;
pub const AAUDIO_ERROR_NULL: aaudio_result_t = AAUDIO_ERROR_NO_MEMORY + 1;
pub const AAUDIO_ERROR_TIMEOUT: aaudio_result_t = AAUDIO_ERROR_NULL + 1;
pub const AAUDIO_ERROR_WOULD_BLOCK: aaudio_result_t = AAUDIO_ERROR_TIMEOUT + 1;
pub const AAUDIO_ERROR_INVALID_FORMAT: aaudio_result_t = AAUDIO_ERROR_WOULD_BLOCK + 1;
pub const AAUDIO_ERROR_OUT_OF_RANGE: aaudio_result_t = AAUDIO_ERROR_INVALID_FORMAT + 1;
pub const AAUDIO_ERROR_NO_SERVICE: aaudio_result_t = AAUDIO_ERROR_OUT_OF_RANGE + 1;
pub const AAUDIO_ERROR_INVALID_RATE: aaudio_result_t = AAUDIO_ERROR_NO_SERVICE + 1;

pub type aaudio_stream_state_t = i32;
pub const AAUDIO_STREAM_STATE_UNINITIALIZED: aaudio_stream_state_t = 0;
pub const AAUDIO_STREAM_STATE_UNKNOWN: aaudio_stream_state_t = 1;
pub const AAUDIO_STREAM_STATE_OPEN: aaudio_stream_state_t = 2;
pub const AAUDIO_STREAM_STATE_STARTING: aaudio_stream_state_t = 3;
pub const AAUDIO_STREAM_STATE_STARTED: aaudio_stream_state_t = 4;
pub const AAUDIO_STREAM_STATE_PAUSING: aaudio_stream_state_t = 5;
pub const AAUDIO_STREAM_STATE_PAUSED: aaudio_stream_state_t = 6;
pub const AAUDIO_STREAM_STATE_FLUSHING: aaudio_stream_state_t = 7;
pub const AAUDIO_STREAM_STATE_FLUSHED: aaudio_stream_state_t = 8;
pub const AAUDIO_STREAM_STATE_STOPPING: aaudio_stream_state_t = 9;
pub const AAUDIO_STREAM_STATE_STOPPED: aaudio_stream_state_t = 10;
pub const AAUDIO_STREAM_STATE_CLOSING: aaudio_stream_state_t = 11;
pub const AAUDIO_STREAM_STATE_CLOSED: aaudio_stream_state_t = 12;
pub const AAUDIO_STREAM_STATE_DISCONNECTED: aaudio_stream_state_t = 13;

pub type aaudio_sharing_mode_t = i32;
pub const AAUDIO_SHARING_MODE_EXCLUSIVE: aaudio_sharing_mode_t = 0;
pub const AAUDIO_SHARING_MODE_SHARED: aaudio_sharing_mode_t = 1;

pub type aaudio_performance_mode_t = i32;
pub const AAUDIO_PERFORMANCE_MODE_NONE: aaudio_performance_mode_t = 10;
pub const AAUDIO_PERFORMANCE_MODE_POWER_SAVING: aaudio_performance_mode_t = 11;
pub const AAUDIO_PERFORMANCE_MODE_LOW_LATENCY: aaudio_performance_mode_t = 12;

pub type aaudio_data_callback_result_t = i32;
pub const AAUDIO_CALLBACK_RESULT_CONTINUE: aaudio_data_callback_result_t = 0;
pub const AAUDIO_CALLBACK_RESULT_STOP: aaudio_data_callback_result_t = 1;

pub enum AAudioStream {}
pub enum AAudioStreamBuilder {}

pub type AAudioStream_dataCallback = extern "C" fn(stream: *mut AAudioStream,
    userData: *mut c_void, audioData: *mut c_void, numFrames: i32) -> aaudio_data_callback_result_t;

pub type PFN_AAudio_convertResultToText = extern "C" fn(code: aaudio_result_t) -> *const c_char;
pub type PFN_AAudio_convertStreamStateToText = extern "C" fn(state: aaudio_stream_state_t) -> *const c_char;

pub type PFN_AAudio_createStreamBuilder = extern "C" fn(builder: *mut *mut AAudioStreamBuilder) -> aaudio_result_t;
pub type PFN_AAudioStreamBuilder_setSampleRate = extern "C" fn(builder: *mut AAudioStreamBuilder, sampleRate: i32);
pub type PFN_AAudioStreamBuilder_setChannelCount = extern "C" fn(builder: *mut AAudioStreamBuilder, channelCount: i32);
pub type PFN_AAudioStreamBuilder_setFormat = extern "C" fn(builder: *mut AAudioStreamBuilder, format: aaudio_format_t);
pub type PFN_AAudioStreamBuilder_setSharingMode =
    extern "C" fn(builder: *mut AAudioStreamBuilder, sharingMode: aaudio_sharing_mode_t);
pub type PFN_AAudioStreamBuilder_setDirection =
    extern "C" fn(builder: *mut AAudioStreamBuilder, direction: aaudio_direction_t);
pub type PFN_AAudioStreamBuilder_setPerformanceMode =
    extern "C" fn(builder: *mut AAudioStreamBuilder, mode: aaudio_performance_mode_t);
pub type PFN_AAudioStreamBuilder_setDataCallback =
    extern "C" fn(builder: *mut AAudioStreamBuilder, callback: AAudioStream_dataCallback, userData: *mut c_void);
pub type PFN_AAudioStreamBuilder_openStream =
    extern "C" fn(builder: *mut AAudioStreamBuilder, stream: *mut *mut AAudioStream) -> aaudio_result_t;
pub type PFN_AAudioStreamBuilder_delete = extern "C" fn(builder: *mut AAudioStreamBuilder) -> aaudio_result_t;

pub type PFN_AAudioStream_close = extern "C" fn(stream: *mut AAudioStream) -> aaudio_result_t;
pub type PFN_AAudioStream_requestStart = extern "C" fn(stream: *mut AAudioStream) -> aaudio_result_t;
pub type PFN_AAudioStream_requestPause = extern "C" fn(stream: *mut AAudioStream) -> aaudio_result_t;
pub type PFN_AAudioStream_requestFlush = extern "C" fn(stream: *mut AAudioStream) -> aaudio_result_t;
pub type PFN_AAudioStream_requestStop = extern "C" fn(stream: *mut AAudioStream) -> aaudio_result_t;
pub type PFN_AAudioStream_getState = extern "C" fn(stream: *mut AAudioStream) -> aaudio_stream_state_t;
pub type PFN_AAudioStream_waitForStateChange = extern "C" fn(stream: *mut AAudioStream,
    inputState: aaudio_stream_state_t, nextState: *mut aaudio_stream_state_t, timeoutNanoseconds: i64)
    -> aaudio_result_t;
