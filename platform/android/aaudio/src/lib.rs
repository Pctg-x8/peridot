mod dl;
pub mod loader;
pub mod native;

pub use loader::LibAAudio;
pub use native::*;

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ResultCode(pub native::aaudio_result_t);
impl From<native::aaudio_result_t> for ResultCode {
    #[inline(always)]
    fn from(value: native::aaudio_result_t) -> Self {
        Self(value)
    }
}
impl ResultCode {
    #[inline(always)]
    pub const fn is_ok(&self) -> bool {
        self.0 == native::AAUDIO_OK
    }

    #[inline(always)]
    pub fn display<'lib>(&self, lib: &'lib LibAAudio) -> &'lib core::ffi::CStr {
        lib.convert_result_to_text(self.0)
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct StreamState(pub native::aaudio_stream_state_t);
impl From<native::aaudio_stream_state_t> for StreamState {
    #[inline(always)]
    fn from(value: native::aaudio_stream_state_t) -> Self {
        Self(value)
    }
}
impl StreamState {
    pub const PAUSED: Self = Self(native::AAUDIO_STREAM_STATE_PAUSED);

    #[inline(always)]
    pub fn display<'lib>(&self, lib: &'lib LibAAudio) -> &'lib core::ffi::CStr {
        lib.convert_stream_state_to_text(self.0)
    }
}
