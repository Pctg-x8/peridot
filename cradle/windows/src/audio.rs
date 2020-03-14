#![allow(non_upper_case_globals)]

use winapi::Interface;
use winapi::um::audioclient::*;
use winapi::um::audiosessiontypes::AUDCLNT_SHAREMODE_SHARED;
use winapi::um::combaseapi::{CoCreateInstance, CLSCTX_ALL, PropVariantClear, CoTaskMemFree};
use winapi::um::mmdeviceapi::*;
use winapi::um::propidl::*;
use winapi::um::propsys::IPropertyStore;
use winapi::um::coml2api::STGM_READ;
use winapi::um::strmif::REFERENCE_TIME;
use winapi::shared::mmreg::*;
use winapi::shared::ksmedia::KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;
use winapi::shared::guiddef::IID;
use winapi::shared::wtypes::PROPERTYKEY;
use std::io::Result as IOResult;
use std::os::windows::ffi::OsStringExt;
use std::ffi::OsString;
use std::ptr::NonNull;
use crate::hr_into_result;

pub struct MMDeviceEnumerator(*mut IMMDeviceEnumerator);
impl MMDeviceEnumerator
{
    fn new() -> IOResult<Self>
    {
        let mut enumerator: *mut IMMDeviceEnumerator = std::ptr::null_mut();
        let hr = unsafe
        {
            CoCreateInstance(&CLSID_MMDeviceEnumerator, std::ptr::null_mut(),
                CLSCTX_ALL, &IMMDeviceEnumerator::uuidof(), std::mem::transmute(&mut enumerator))
        };

        hr_into_result(hr).map(|_| MMDeviceEnumerator(enumerator))
    }
}
impl Drop for MMDeviceEnumerator { fn drop(&mut self) { unsafe { (*self.0).Release(); } } }
struct MMDevice(*mut IMMDevice);
impl MMDeviceEnumerator
{
    fn default_audio_endpoint(&self, dataflow: EDataFlow, role: ERole) -> IOResult<MMDevice>
    {
        let mut md = std::ptr::null_mut();
        let hr = unsafe { (*self.0).GetDefaultAudioEndpoint(dataflow, role, &mut md) };

        hr_into_result(hr).map(|_| MMDevice(md))
    }
}
impl Drop for MMDevice { fn drop(&mut self) { unsafe { (*self.0).Release(); } } }

pub struct MMDeviceProperties(*mut IPropertyStore);
impl MMDevice
{
    pub fn properties(&self) -> IOResult<MMDeviceProperties>
    {
        let mut p = std::ptr::null_mut();
        let hr = unsafe { (*self.0).OpenPropertyStore(STGM_READ, &mut p) };

        hr_into_result(hr).map(|_| MMDeviceProperties(p))
    }
}
impl Drop for MMDeviceProperties { fn drop(&mut self) { unsafe { (*self.0).Release(); } } }

pub struct CoBox<T>(NonNull<T>);
impl<T> CoBox<T> { pub fn new(ptr: *mut T) -> Option<Self> { NonNull::new(ptr).map(CoBox) } }
impl<T> std::ops::Deref for CoBox<T> { type Target = T; fn deref(&self) -> &T { unsafe { self.0.as_ref() } } }
impl<T> std::ops::DerefMut for CoBox<T> { fn deref_mut(&mut self) -> &mut T { unsafe { self.0.as_mut() } } }
impl<T> Drop for CoBox<T> { fn drop(&mut self) { unsafe { CoTaskMemFree(self.0.as_ptr() as *mut _); } } }

pub struct AudioClient(*mut IAudioClient);
impl Drop for AudioClient { fn drop(&mut self) { unsafe { (*self.0).Release(); } } }
impl MMDevice
{
    pub fn activate(&self) -> IOResult<AudioClient>
    {
        let mut p = std::ptr::null_mut();
        let hr = unsafe
        {
            (*self.0).Activate(&IAudioClient::uuidof(), CLSCTX_ALL,
                std::ptr::null_mut(), &mut p)
        };

        hr_into_result(hr).map(|_| AudioClient(p as *mut _))
    }
}
impl AudioClient
{
    pub fn initialize_shared(&self, buffer_duration: REFERENCE_TIME, wfx: &WAVEFORMATEX) -> IOResult<()>
    {
        let hr = unsafe
        {
            (*self.0).Initialize(AUDCLNT_SHAREMODE_SHARED, 0,
                buffer_duration, 0, wfx as *const _, std::ptr::null_mut())
        };

        hr_into_result(hr)
    }
    pub fn service<IF: InterfaceWrapper>(&self) -> IOResult<IF>
    {
        let mut p = std::ptr::null_mut();
        let hr = unsafe { (*self.0).GetService(&IF::uuidof(), &mut p) };

        hr_into_result(hr).map(|_| IF::wrap(p as *mut _))
    }
    pub fn start(&self) -> IOResult<()> { hr_into_result(unsafe { (*self.0).Start() }) }

    pub fn mix_format(&self) -> IOResult<CoBox<WAVEFORMATEX>>
    {
        let mut p = std::ptr::null_mut();
        hr_into_result(unsafe { (*self.0).GetMixFormat(&mut p as *mut _) })?;
        return Ok(CoBox::new(p).expect("NullPointer Returned"));
    }
    pub fn buffer_size(&self) -> IOResult<usize>
    {
        let mut us = 0;
        let hr = unsafe { (*self.0).GetBufferSize(&mut us) };

        hr_into_result(hr).map(|_| us as usize)
    }
    pub fn current_padding(&self) -> IOResult<u32>
    {
        let mut v = 0;
        hr_into_result(unsafe { (*self.0).GetCurrentPadding(&mut v) }).map(|_| v)
    }
}
pub struct AudioRenderClient(NonNull<IAudioRenderClient>);
pub trait InterfaceWrapper
{
    type Interface: Interface;
    fn uuidof() -> IID { Self::Interface::uuidof() }
    fn wrap(p: *mut Self::Interface) -> Self;
}
impl InterfaceWrapper for AudioRenderClient
{
    type Interface = IAudioRenderClient;
    fn wrap(p: *mut IAudioRenderClient) -> Self { AudioRenderClient(NonNull::new(p).expect("null")) }
}
impl Drop for AudioRenderClient { fn drop(&mut self) { unsafe { self.0.as_ref().Release(); } } }
impl AudioRenderClient
{
    pub fn get_buffer_ptr(&self, req_frames: u32) -> IOResult<*mut u8>
    {
        let mut pp = std::ptr::null_mut();
        let hr = unsafe { self.0.as_ref().GetBuffer(req_frames, &mut pp) };

        hr_into_result(hr).map(|_| pp)
    }
    fn release_buffer(&self, written_frames: u32, silence: bool) -> IOResult<()>
    {
        hr_into_result(unsafe
        {
            self.0.as_ref().ReleaseBuffer(written_frames, if silence { AUDCLNT_BUFFERFLAGS_SILENT } else { 0 })
        })
    }
}

pub struct PropVariant(PROPVARIANT);
impl PropVariant
{
    pub fn init() -> Self { unsafe { PropVariant(std::mem::zeroed()) } }
    pub unsafe fn as_osstr_unchecked(&self) -> OsString
    {
        let ptr = *self.0.data.pwszVal();
        let len = (0..).find(|&x| *ptr.offset(x) == 0).expect("Infinite Length");
        let slice = std::slice::from_raw_parts(ptr, len as _);
        
        OsString::from_wide(slice)
    }
}
impl Drop for PropVariant { fn drop(&mut self) { unsafe { PropVariantClear(&mut self.0); } } }
winapi::DEFINE_PROPERTYKEY!(PKEY_DeviceInterface_FriendlyName,
    0x026e516e, 0xb814, 0x414b, 0x83, 0xcd, 0x85, 0x6d, 0x6f, 0xef, 0x48, 0x22, 2);
winapi::DEFINE_PROPERTYKEY!(PKEY_Device_DeviceDesc,
    0xa45c254e, 0xdf1c, 0x4efd, 0x80, 0x20, 0x67, 0xd1, 0x46, 0xa8, 0x50, 0xe0, 2);
winapi::DEFINE_PROPERTYKEY!(PKEY_Device_FriendlyName,
    0xa45c254e, 0xdf1c, 0x4efd, 0x80, 0x20, 0x67, 0xd1, 0x46, 0xa8, 0x50, 0xe0, 14);
impl MMDeviceProperties
{
    pub fn interface_friendly_name(&self) -> IOResult<String>
    {
        let mut pv = PropVariant::init();
        let hr = unsafe { (*self.0).GetValue(&PKEY_DeviceInterface_FriendlyName, &mut pv.0) };
        
        hr_into_result(hr).map(|_| unsafe
        {
            pv.as_osstr_unchecked().into_string().expect("Decoding FriendlyName")
        })
    }
    pub fn desc(&self) -> IOResult<String>
    {
        let mut pv = PropVariant::init();
        let hr = unsafe { (*self.0).GetValue(&PKEY_Device_DeviceDesc, &mut pv.0) };

        hr_into_result(hr).map(|_| unsafe
        {
            pv.as_osstr_unchecked().into_string().expect("Decoding DeviceDesc")
        })
    }
    pub fn friendly_name(&self) -> IOResult<String>
    {
        let mut pv = PropVariant::init();
        let hr = unsafe { (*self.0).GetValue(&PKEY_Device_FriendlyName, &mut pv.0) };

        hr_into_result(hr).map(|_| unsafe
        {
            pv.as_osstr_unchecked().into_string().expect("Decoding DeviceDesc")
        })
    }
}

use std::f32::consts::PI as PI_F32;
pub struct PSGSine { waverate: f32, current_hz: f32, sample_rate: f32, amp: f32, current_frame: u64 }
impl PSGSine
{
    pub fn new() -> Self
    {
        PSGSine { waverate: 44100.0 / 441.0, current_hz: 441.0, sample_rate: 44100.0, amp: 1.0, current_frame: 0 }
    }
    pub fn set_osc_hz(&mut self, hz: f32)
    {
        self.current_hz = hz;
        self.waverate = self.sample_rate / self.current_hz;
    }
    pub fn set_amp(&mut self, amp: f32) { self.amp = amp; }
}
impl peridot::audio::Processor for PSGSine
{
    fn set_sample_rate(&mut self, samples_per_sec: f32)
    {
        self.sample_rate = samples_per_sec;
        self.waverate = self.sample_rate / self.current_hz;
    }
    fn process(&mut self, flattened_buffer: &mut [f32])
    {
        for (n, b) in flattened_buffer.chunks_mut(2).enumerate()
        {
            let v = (2.0 * PI_F32 * (self.current_frame + n as u64) as f32 / self.waverate).sin() * self.amp;
            b[0] += v;
            b[1] += v;
        }
        self.current_frame += (flattened_buffer.len() >> 1) as u64;
    }
}

use std::thread::{JoinHandle, Builder as ThreadBuilder, sleep};
use std::time::Duration;
use std::sync::{Arc, atomic::AtomicBool, atomic::Ordering, RwLock};
pub struct NativeAudioEngine
{
    process_thread: Option<JoinHandle<()>>, exit_state: Arc<AtomicBool>
}
impl NativeAudioEngine
{
    pub fn new(mixer: Arc<RwLock<peridot::audio::Mixer>>) -> IOResult<Self>
    {
        let exit_state = Arc::new(AtomicBool::new(false));
        let exit_state_th = exit_state.clone();

        let process_thread = ThreadBuilder::new().name("Audio Process".to_owned()).spawn(move ||
        {
            let enumerator = MMDeviceEnumerator::new().expect("Enumerating MMDevices");
            let dev0 = enumerator.default_audio_endpoint(eRender, eConsole).expect("Getting Default AudioEP");
            
            info!("Audio Output Device: {}",
                dev0.properties().expect("Getting DeviceProps")
                    .friendly_name().expect("Getting FriendlyName of the Device"));

            let aclient = dev0.activate().expect("activate");
            let bits_per_sample = 32;
            let mf = aclient.mix_format().expect("Getting DefaultMixFormat");
            let samples_per_sec = mf.nSamplesPerSec;
            /*println!("device mixformat: bits={}", mf.wBitsPerSample);
            println!("device mixformat: samples/s={}", mf.nSamplesPerSec);
            println!("device mixformat: channels={}", mf.nChannels);
            println!("device mixformat: tag={}", mf.wFormatTag);*/
            let wfx = WAVEFORMATEXTENSIBLE
            {
                Format: WAVEFORMATEX
                {
                    wFormatTag: WAVE_FORMAT_EXTENSIBLE, nChannels: 2,
                    nSamplesPerSec: samples_per_sec, wBitsPerSample: bits_per_sample,
                    nBlockAlign: (bits_per_sample >> 3) * 2,
                    nAvgBytesPerSec: samples_per_sec * 2 * (bits_per_sample >> 3) as u32,
                    cbSize: std::mem::size_of::<WAVEFORMATEXTENSIBLE>() as _
                },
                Samples: bits_per_sample,
                dwChannelMask: SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT,
                SubFormat: KSDATAFORMAT_SUBTYPE_IEEE_FLOAT
            };
            aclient.initialize_shared(10 * 1000 * 10, unsafe { std::mem::transmute(&wfx) }).expect("initialize");

            let process_frames = aclient.buffer_size().expect("Getting BufferSize") as u32;
            info!("Processing Buffer Size: {}", process_frames);
            let sleep_duration = Duration::from_micros(
                (500_000.0 * process_frames as f64 / wfx.Format.nSamplesPerSec as f64) as _);
            let srv: AudioRenderClient = aclient.service().expect("No Render Service");
            mixer.write().expect("Setting SampleRate").set_sample_rate(samples_per_sec as _);

            info!("Starting AudioRender...");
            aclient.start().expect("Starting AudioRender");
            while !exit_state_th.load(Ordering::Acquire) {
                let pad = aclient.current_padding().expect("Current Padding");
                let available_frames = process_frames - pad;
                let bufp = srv.get_buffer_ptr(available_frames).expect("Get Buffer");
                let buf = unsafe { std::slice::from_raw_parts_mut(bufp as *mut f32, (available_frames << 1) as _) };
                for b in buf.iter_mut() { *b = 0.0; }
                
                let silence = mixer.write().expect("Processing WriteLock").process(buf);

                srv.release_buffer(available_frames, silence).expect("Release Buffer");
                sleep(sleep_duration);
            }
        })?.into();

        return Ok(NativeAudioEngine { process_thread, exit_state });
    }
}
impl Drop for NativeAudioEngine
{
    fn drop(&mut self)
    {
        self.exit_state.store(true, Ordering::Relaxed);
        self.process_thread.take().expect("already joined").join().expect("Joining AudioProcess");
    }
}
