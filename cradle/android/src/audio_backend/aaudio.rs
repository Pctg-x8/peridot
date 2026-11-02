use aaudio::LibAAudio;
use libc::c_void;
use std::mem::MaybeUninit;
use std::pin::Pin;
use std::ptr::NonNull;
use std::rc::Rc;

pub struct DisplayableResultCode(aaudio::ResultCode, Api);
impl std::fmt::Debug for DisplayableResultCode {
    #[inline(always)]
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.0.display(&self.1 .0).fmt(fmt)
    }
}
pub type Result<T> = std::result::Result<T, DisplayableResultCode>;

#[derive(Clone)]
pub struct Api(Rc<LibAAudio>);
impl Api {
    #[inline(always)]
    pub fn load() -> Option<Self> {
        LibAAudio::load().map(|a| Self(Rc::new(a)))
    }

    #[inline(always)]
    fn make_result(&self, code: aaudio::ResultCode) -> Result<()> {
        if code.is_ok() {
            Ok(())
        } else {
            Err(DisplayableResultCode(code, self.clone()))
        }
    }

    #[inline(always)]
    fn combine_result(&self, result: aaudio::ResultCode) -> DisplayableResultCode {
        DisplayableResultCode(result, self.clone())
    }

    #[inline(always)]
    pub fn display_result_code(&self, result: aaudio::ResultCode) -> &str {
        result
            .display(&self.0)
            .to_str()
            .expect("invalid result text")
    }

    #[inline(always)]
    pub fn display_stream_state(&self, state: aaudio::StreamState) -> &str {
        state
            .display(&self.0)
            .to_str()
            .expect("invalid stream state text")
    }

    #[inline(always)]
    pub fn new_stream_builder(&self) -> Result<StreamBuilder> {
        let mut builder = MaybeUninit::uninit();
        self.make_result(self.0.create_stream_builder(builder.as_mut_ptr()))
            .map(move |_| StreamBuilder {
                api: self.clone(),
                ptr: unsafe { NonNull::new_unchecked(builder.assume_init()) },
            })
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallbackResult {
    Continue = aaudio::AAUDIO_CALLBACK_RESULT_CONTINUE as _,
    Stop = aaudio::AAUDIO_CALLBACK_RESULT_STOP as _,
}

pub trait DataCallback {
    fn callback(
        &mut self,
        stream_ptr: *mut aaudio::AAudioStream,
        buf: *mut c_void,
        frames: usize,
    ) -> CallbackResult;
}

pub struct StreamBuilder {
    api: Api,
    ptr: NonNull<aaudio::AAudioStreamBuilder>,
}
impl StreamBuilder {
    #[inline(always)]
    pub fn set_sample_rate(&mut self, sample_rate: i32) -> &mut Self {
        self.api
            .0
            .sb_set_sample_rate(unsafe { &mut *self.ptr.as_ptr() }, sample_rate);
        self
    }

    #[inline(always)]
    pub fn set_channel_count(&mut self, channel_count: i32) -> &mut Self {
        self.api
            .0
            .sb_set_channel_count(unsafe { &mut *self.ptr.as_ptr() }, channel_count);
        self
    }

    #[inline(always)]
    pub fn use_int16_format(&mut self) -> &mut Self {
        self.api.0.sb_set_format(
            unsafe { &mut *self.ptr.as_ptr() },
            aaudio::AAUDIO_FORMAT_PCM_I16,
        );
        self
    }

    #[inline(always)]
    pub fn use_float_format(&mut self) -> &mut Self {
        self.api.0.sb_set_format(
            unsafe { &mut *self.ptr.as_ptr() },
            aaudio::AAUDIO_FORMAT_PCM_FLOAT,
        );
        self
    }

    #[inline(always)]
    pub fn use_exclusive(&mut self) -> &mut Self {
        self.api.0.sb_set_sharing_mode(
            unsafe { &mut *self.ptr.as_ptr() },
            aaudio::AAUDIO_SHARING_MODE_EXCLUSIVE,
        );
        self
    }

    #[inline(always)]
    pub fn use_shared(&mut self) -> &mut Self {
        self.api.0.sb_set_sharing_mode(
            unsafe { &mut *self.ptr.as_ptr() },
            aaudio::AAUDIO_SHARING_MODE_SHARED,
        );
        self
    }

    #[inline(always)]
    pub fn as_output(&mut self) -> &mut Self {
        self.api.0.sb_set_direction(
            unsafe { &mut *self.ptr.as_ptr() },
            aaudio::AAUDIO_DIRECTION_OUTPUT,
        );
        self
    }

    #[inline(always)]
    pub fn as_input(&mut self) -> &mut Self {
        self.api.0.sb_set_direction(
            unsafe { &mut *self.ptr.as_ptr() },
            aaudio::AAUDIO_DIRECTION_INPUT,
        );
        self
    }

    #[inline(always)]
    pub fn set_low_latency_mode(&mut self) -> &mut Self {
        self.api.0.sb_set_performance_mode(
            unsafe { &mut *self.ptr.as_ptr() },
            aaudio::AAUDIO_PERFORMANCE_MODE_LOW_LATENCY,
        );
        self
    }

    #[inline(always)]
    pub fn set_data_callback<C: DataCallback + Unpin>(&mut self, cb: Pin<&mut C>) -> &mut Self {
        extern "C" fn cb_wrapper<C: DataCallback>(
            stream: *mut aaudio::AAudioStream,
            ctx: *mut c_void,
            data: *mut c_void,
            num_frames: i32,
        ) -> aaudio::aaudio_data_callback_result_t {
            let cb = unsafe { (ctx as *mut C).as_mut().expect("null?") };
            cb.callback(stream, data, num_frames as _) as _
        }
        self.api.0.sb_set_data_callback(
            unsafe { &mut *self.ptr.as_ptr() },
            cb_wrapper::<C>,
            cb.get_mut() as *mut C as _,
        );
        self
    }

    #[inline(always)]
    pub fn open_stream(&mut self) -> Result<Stream> {
        let mut sp = MaybeUninit::uninit();
        let r = self
            .api
            .0
            .sb_open_stream(unsafe { &mut *self.ptr.as_ptr() }, sp.as_mut_ptr());
        self.api.make_result(r).map(move |_| Stream {
            api: self.api.clone(),
            ptr: unsafe { NonNull::new_unchecked(sp.assume_init()) },
            borrowed: false,
        })
    }
}
impl Drop for StreamBuilder {
    #[inline(always)]
    fn drop(&mut self) {
        self.api.0.sb_delete(unsafe { &mut *self.ptr.as_ptr() });
    }
}

pub struct Stream {
    api: Api,
    ptr: NonNull<aaudio::AAudioStream>,
    borrowed: bool,
}
impl Drop for Stream {
    fn drop(&mut self) {
        if !self.borrowed {
            self.api.0.stream_close(unsafe { &mut *self.ptr.as_ptr() });
        }
    }
}
impl Stream {
    #[inline(always)]
    pub unsafe fn from_unonwned_ptr(p: *mut aaudio::AAudioStream, api: &Api) -> Self {
        Stream {
            api: api.clone(),
            ptr: NonNull::new_unchecked(p),
            borrowed: true,
        }
    }

    #[inline(always)]
    pub fn request_start(&mut self) -> Result<()> {
        self.api.make_result(
            self.api
                .0
                .stream_request_start(unsafe { &mut *self.ptr.as_ptr() }),
        )
    }

    #[inline(always)]
    pub fn request_flush(&mut self) -> Result<()> {
        self.api.make_result(
            self.api
                .0
                .stream_request_flush(unsafe { &mut *self.ptr.as_ptr() }),
        )
    }

    #[inline(always)]
    pub fn request_pause(&mut self) -> Result<()> {
        self.api.make_result(
            self.api
                .0
                .stream_request_pause(unsafe { &mut *self.ptr.as_ptr() }),
        )
    }

    #[inline(always)]
    pub fn request_stop(&mut self) -> Result<()> {
        self.api.make_result(
            self.api
                .0
                .stream_request_stop(unsafe { &mut *self.ptr.as_ptr() }),
        )
    }

    #[inline(always)]
    pub fn state(&self) -> aaudio::StreamState {
        self.api
            .0
            .stream_get_state(unsafe { &mut *self.ptr.as_ptr() })
    }

    #[inline(always)]
    pub fn wait_for_state_change(
        &mut self,
        input_state: aaudio::StreamState,
        next_state: *mut aaudio::StreamState,
        timeout_ns: Option<i64>,
    ) -> Result<()> {
        self.api
            .make_result(self.api.0.stream_wait_for_state_change(
                unsafe { &mut *self.ptr.as_ptr() },
                input_state,
                next_state,
                timeout_ns.unwrap_or(i64::MAX),
            ))
    }
}
