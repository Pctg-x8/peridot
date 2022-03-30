
pub mod native;
mod loader;
use self::loader::LibAAudio;
use std::rc::Rc;
use std::ffi::CStr;
use std::ptr::NonNull;
use std::mem::MaybeUninit;
use std::pin::Pin;
use libc::c_void;

pub type ResultCode = native::aaudio_result_t;
pub type StreamState = native::aaudio_stream_state_t;
pub struct DisplayableResultCode(ResultCode, Api);
impl std::fmt::Debug for DisplayableResultCode
{
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(fmt, "{}", self.1.display_result_code(self.0))
    }
}
pub type Result<T> = std::result::Result<T, DisplayableResultCode>;

#[derive(Clone)]
pub struct Api(Rc<LibAAudio>);
impl Api
{
    pub fn load() -> Option<Self>
    {
        LibAAudio::load().ok().map(|a| Self(Rc::new(a)))
    }
    fn make_result(&self, code: ResultCode) -> Result<()>
    {
        if code != native::AAUDIO_OK { Err(self.combine_result(code)) } else { Ok(()) }
    }
    fn combine_result(&self, result: ResultCode) -> DisplayableResultCode
    {
        DisplayableResultCode(result, self.clone())
    }
    pub fn display_result_code(&self, result: ResultCode) -> &str
    {
        unsafe { CStr::from_ptr((self.0.convert_result_to_text)(result)).to_str().expect("invalid result text") }
    }
    pub fn display_stream_state(&self, state: StreamState) -> &str
    {
        unsafe
        {
            CStr::from_ptr((self.0.convert_stream_state_to_text)(state)).to_str().expect("invalid stream state text")
        }
    }

    pub fn new_stream_builder(&self) -> Result<StreamBuilder>
    {
        let mut builder = MaybeUninit::uninit();
        self.make_result((self.0.create_stream_builder)(builder.as_mut_ptr()))
            .map(move |_| StreamBuilder
            {
                api: self.clone(),
                ptr: unsafe { NonNull::new_unchecked(builder.assume_init()) }
            })
    }
}

#[repr(C)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CallbackResult
{
    Continue = native::AAUDIO_CALLBACK_RESULT_CONTINUE as _,
    Stop = native::AAUDIO_CALLBACK_RESULT_STOP as _
}

pub trait DataCallback
{
    fn callback(&mut self, stream_ptr: *mut self::native::AAudioStream, buf: *mut c_void, frames: usize)
        -> CallbackResult;
}

pub struct StreamBuilder
{
    api: Api,
    ptr: NonNull<native::AAudioStreamBuilder>
}
impl StreamBuilder
{
    pub fn set_sample_rate(&mut self, sample_rate: i32) -> &mut Self
    {
        (self.api.0.sb_set_sample_rate)(self.ptr.as_ptr(), sample_rate);
        self
    }
    pub fn set_channel_count(&mut self, channel_count: i32) -> &mut Self
    {
        (self.api.0.sb_set_channel_count)(self.ptr.as_ptr(), channel_count);
        self
    }
    pub fn use_int16_format(&mut self) -> &mut Self
    {
        (self.api.0.sb_set_format)(self.ptr.as_ptr(), native::AAUDIO_FORMAT_PCM_I16);
        self
    }
    pub fn use_float_format(&mut self) -> &mut Self
    {
        (self.api.0.sb_set_format)(self.ptr.as_ptr(), native::AAUDIO_FORMAT_PCM_FLOAT);
        self
    }
    pub fn use_exclusive(&mut self) -> &mut Self
    {
        (self.api.0.sb_set_sharing_mode)(self.ptr.as_ptr(), native::AAUDIO_SHARING_MODE_EXCLUSIVE);
        self
    }
    pub fn use_shared(&mut self) -> &mut Self
    {
        (self.api.0.sb_set_sharing_mode)(self.ptr.as_ptr(), native::AAUDIO_SHARING_MODE_SHARED);
        self
    }
    pub fn as_output(&mut self) -> &mut Self
    {
        (self.api.0.sb_set_direction)(self.ptr.as_ptr(), native::AAUDIO_DIRECTION_OUTPUT);
        self
    }
    pub fn as_input(&mut self) -> &mut Self
    {
        (self.api.0.sb_set_direction)(self.ptr.as_ptr(), native::AAUDIO_DIRECTION_INPUT);
        self
    }
    pub fn set_low_latency_mode(&mut self) -> &mut Self
    {
        (self.api.0.sb_set_performance_mode)(self.ptr.as_ptr(), native::AAUDIO_PERFORMANCE_MODE_LOW_LATENCY);
        self
    }
    pub fn set_data_callback<C: DataCallback + Unpin>(&mut self, cb: Pin<&mut C>) -> &mut Self
    {
        extern "C" fn cb_wrapper<C: DataCallback>(
            stream: *mut native::AAudioStream,
            ctx: *mut c_void, data: *mut c_void, num_frames: i32)
            -> native::aaudio_data_callback_result_t
        {
            let cb = unsafe { (ctx as *mut C).as_mut().expect("null?") };
            cb.callback(stream, data, num_frames as _) as _
        }
        (self.api.0.sb_set_data_callback)(self.ptr.as_ptr(), cb_wrapper::<C>, cb.get_mut() as *mut C as _);
        self
    }

    pub fn open_stream(&mut self) -> Result<Stream>
    {
        let mut sp = MaybeUninit::uninit();
        let r = (self.api.0.sb_open_stream)(self.ptr.as_ptr(), sp.as_mut_ptr());
        self.api.make_result(r).map(move |_| Stream
        {
            api: self.api.clone(),
            ptr: unsafe { NonNull::new_unchecked(sp.assume_init()) },
            borrowed: false
        })
    }
}
impl Drop for StreamBuilder
{
    fn drop(&mut self)
    {
        (self.api.0.sb_delete)(self.ptr.as_ptr());
    }
}

pub struct Stream
{
    api: Api,
    ptr: NonNull<native::AAudioStream>,
    borrowed: bool
}
impl Drop for Stream
{
    fn drop(&mut self)
    {
        if !self.borrowed
        {
            (self.api.0.stream_close)(self.ptr.as_ptr());
        }
    }
}
impl Stream
{
    pub unsafe fn from_unonwned_ptr(p: *mut native::AAudioStream, api: &Api) -> Self
    {
        Stream
        {
            api: api.clone(),
            ptr: NonNull::new_unchecked(p),
            borrowed: true
        }
    }

    pub fn request_start(&mut self) -> Result<()>
    {
        self.api.make_result((self.api.0.stream_request_start)(self.ptr.as_ptr()))
    }
    pub fn request_flush(&mut self) -> Result<()>
    {
        self.api.make_result((self.api.0.stream_request_flush)(self.ptr.as_ptr()))
    }
    pub fn request_pause(&mut self) -> Result<()>
    {
        self.api.make_result((self.api.0.stream_request_pause)(self.ptr.as_ptr()))
    }
    pub fn request_stop(&mut self) -> Result<()>
    {
        self.api.make_result((self.api.0.stream_request_stop)(self.ptr.as_ptr()))
    }
    pub fn state(&self) -> StreamState
    {
        (self.api.0.stream_get_state)(self.ptr.as_ptr())
    }
    pub fn wait_for_state_change(&mut self,
        input_state: StreamState, next_state: &mut StreamState, timeout_ns: Option<i64>)
        -> Result<()>
    {
        let r = (self.api.0.stream_wait_for_state_change)(self.ptr.as_ptr(),
            input_state, next_state, timeout_ns.unwrap_or(std::i64::MAX));
        self.api.make_result(r)
    }
}
