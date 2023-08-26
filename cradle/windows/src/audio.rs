#![allow(non_upper_case_globals)]

use std::ffi::OsString;
use std::io::Result as IOResult;
use std::os::windows::ffi::OsStringExt;
use std::ptr::NonNull;
use windows::core::PWSTR;
use windows::Win32::Devices::FunctionDiscovery::{
    PKEY_DeviceInterface_FriendlyName, PKEY_Device_DeviceDesc, PKEY_Device_FriendlyName,
};
use windows::Win32::Media::Audio::{
    eConsole, eRender, EDataFlow, ERole, IAudioClient, IAudioRenderClient, IMMDevice,
    IMMDeviceEnumerator, AUDCLNT_BUFFERFLAGS_SILENT, AUDCLNT_SHAREMODE_SHARED, WAVEFORMATEX,
    WAVEFORMATEXTENSIBLE, WAVEFORMATEXTENSIBLE_0,
};
use windows::Win32::Media::KernelStreaming::{
    SPEAKER_FRONT_LEFT, SPEAKER_FRONT_RIGHT, WAVE_FORMAT_EXTENSIBLE,
};
use windows::Win32::Media::Multimedia::KSDATAFORMAT_SUBTYPE_IEEE_FLOAT;
use windows::Win32::System::Com::{CoCreateInstance, CoTaskMemFree, CLSCTX_ALL, STGM_READ};
use windows::Win32::UI::Shell::PropertiesSystem::IPropertyStore;

#[repr(transparent)]
pub struct MMDeviceEnumerator(IMMDeviceEnumerator);
impl MMDeviceEnumerator {
    #[inline]
    fn new() -> windows::core::Result<Self> {
        unsafe {
            CoCreateInstance(
                &windows::Win32::Media::Audio::MMDeviceEnumerator,
                None,
                CLSCTX_ALL,
            )
            .map(Self)
        }
    }

    #[inline]
    fn default_audio_endpoint(
        &self,
        dataflow: EDataFlow,
        role: ERole,
    ) -> windows::core::Result<MMDevice> {
        unsafe { self.0.GetDefaultAudioEndpoint(dataflow, role).map(MMDevice) }
    }
}

#[repr(transparent)]
struct MMDevice(IMMDevice);
impl MMDevice {
    #[inline]
    pub fn read_properties(&self) -> windows::core::Result<MMDeviceProperties> {
        unsafe { self.0.OpenPropertyStore(STGM_READ).map(MMDeviceProperties) }
    }

    #[inline]
    pub fn activate(&self) -> windows::core::Result<AudioClient> {
        unsafe { self.0.Activate(CLSCTX_ALL, None).map(AudioClient) }
    }
}

#[repr(transparent)]
pub struct MMDeviceProperties(IPropertyStore);
#[allow(dead_code)]
impl MMDeviceProperties {
    #[inline]
    pub fn interface_friendly_name(&self) -> windows::core::Result<PWSTR> {
        unsafe {
            self.0
                .GetValue(&PKEY_DeviceInterface_FriendlyName)
                .map(|p| p.Anonymous.Anonymous.Anonymous.pwszVal)
        }
    }

    #[inline]
    pub fn desc(&self) -> windows::core::Result<PWSTR> {
        unsafe {
            self.0
                .GetValue(&PKEY_Device_DeviceDesc)
                .map(|p| p.Anonymous.Anonymous.Anonymous.pwszVal)
        }
    }

    #[inline]
    pub fn friendly_name(&self) -> windows::core::Result<PWSTR> {
        unsafe {
            self.0
                .GetValue(&PKEY_Device_FriendlyName)
                .map(|p| p.Anonymous.Anonymous.Anonymous.pwszVal)
        }
    }
}

pub struct CoBox<T>(NonNull<T>);
impl<T> CoBox<T> {
    pub fn new(ptr: *mut T) -> Option<Self> {
        NonNull::new(ptr).map(CoBox)
    }
}
impl<T> std::ops::Deref for CoBox<T> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe { self.0.as_ref() }
    }
}
impl<T> std::ops::DerefMut for CoBox<T> {
    fn deref_mut(&mut self) -> &mut T {
        unsafe { self.0.as_mut() }
    }
}
impl<T> Drop for CoBox<T> {
    fn drop(&mut self) {
        unsafe {
            CoTaskMemFree(Some(self.0.as_ptr() as *const _));
        }
    }
}

pub struct AudioClient(IAudioClient);
impl AudioClient {
    #[inline]
    pub fn initialize_shared(
        &self,
        buffer_duration: i64,
        wfx: &WAVEFORMATEX,
    ) -> windows::core::Result<()> {
        unsafe {
            self.0.Initialize(
                AUDCLNT_SHAREMODE_SHARED,
                0,
                buffer_duration,
                0,
                wfx as *const _,
                None,
            )
        }
    }

    #[inline]
    pub fn service<IF: windows::core::Interface>(&self) -> windows::core::Result<IF> {
        unsafe { self.0.GetService() }
    }

    #[inline]
    pub fn start(&self) -> windows::core::Result<()> {
        unsafe { self.0.Start() }
    }

    #[inline]
    pub fn mix_format(&self) -> windows::core::Result<CoBox<WAVEFORMATEX>> {
        unsafe {
            self.0
                .GetMixFormat()
                .map(|p| CoBox::new(p).expect("null pointer returned"))
        }
    }

    #[inline]
    pub fn buffer_size(&self) -> windows::core::Result<usize> {
        unsafe { self.0.GetBufferSize().map(|x| x as _) }
    }

    #[inline]
    pub fn current_padding(&self) -> windows::core::Result<u32> {
        unsafe { self.0.GetCurrentPadding() }
    }
}

/* Example PSGSine unit implementation
use std::f32::consts::PI as PI_F32;
pub struct PSGSine { waverate: f32, current_hz: f32, sample_rate: f32, amp: f32, current_frame: u64 }
impl PSGSine {
    pub fn new() -> Self {
        PSGSine { waverate: 44100.0 / 441.0, current_hz: 441.0, sample_rate: 44100.0, amp: 1.0, current_frame: 0 }
    }
    pub fn set_osc_hz(&mut self, hz: f32) {
        self.current_hz = hz;
        self.waverate = self.sample_rate / self.current_hz;
    }
    pub fn set_amp(&mut self, amp: f32) { self.amp = amp; }
}
impl peridot::audio::Processor for PSGSine {
    fn set_sample_rate(&mut self, samples_per_sec: f32) {
        self.sample_rate = samples_per_sec;
        self.waverate = self.sample_rate / self.current_hz;
    }
    fn process(&mut self, flattened_buffer: &mut [f32]) {
        for (n, b) in flattened_buffer.chunks_mut(2).enumerate()
        {
            let v = (2.0 * PI_F32 * (self.current_frame + n as u64) as f32 / self.waverate).sin() * self.amp;
            b[0] += v;
            b[1] += v;
        }
        self.current_frame += (flattened_buffer.len() >> 1) as u64;
    }
}
*/

use parking_lot::RwLock;
use std::sync::{atomic::AtomicBool, atomic::Ordering, Arc};
use std::thread::{sleep, Builder as ThreadBuilder, JoinHandle};
use std::time::Duration;

pub struct NativeAudioEngine {
    process_thread: Option<JoinHandle<()>>,
    exit_state: Arc<AtomicBool>,
}
impl NativeAudioEngine {
    pub fn new(mixer: Arc<RwLock<peridot::audio::Mixer>>) -> IOResult<Self> {
        let exit_state = Arc::new(AtomicBool::new(false));
        let exit_state_th = exit_state.clone();

        let process_thread = ThreadBuilder::new()
            .name("Audio Process".to_owned())
            .spawn(move || {
                let enumerator = MMDeviceEnumerator::new().expect("Enumerating MMDevices");
                let dev0 = enumerator
                    .default_audio_endpoint(eRender, eConsole)
                    .expect("Getting Default AudioEP");

                log::info!("Audio Output Device: {}", unsafe {
                    dev0.read_properties()
                        .expect("Getting DeviceProps")
                        .friendly_name()
                        .expect("Getting FriendlyName of the Device")
                        .display()
                });

                let aclient = dev0.activate().expect("activate");
                let bits_per_sample = 32;
                let mf = aclient.mix_format().expect("Getting DefaultMixFormat");
                let samples_per_sec = mf.nSamplesPerSec;
                /*println!("device mixformat: bits={}", mf.wBitsPerSample);
                println!("device mixformat: samples/s={}", mf.nSamplesPerSec);
                println!("device mixformat: channels={}", mf.nChannels);
                println!("device mixformat: tag={}", mf.wFormatTag);*/
                let wfx = WAVEFORMATEXTENSIBLE {
                    Format: WAVEFORMATEX {
                        wFormatTag: WAVE_FORMAT_EXTENSIBLE as _,
                        nChannels: 2,
                        nSamplesPerSec: samples_per_sec,
                        wBitsPerSample: bits_per_sample,
                        nBlockAlign: (bits_per_sample >> 3) * 2,
                        nAvgBytesPerSec: samples_per_sec * 2 * (bits_per_sample >> 3) as u32,
                        cbSize: (std::mem::size_of::<WAVEFORMATEXTENSIBLE>()
                            - std::mem::size_of::<WAVEFORMATEX>())
                            as _,
                    },
                    Samples: WAVEFORMATEXTENSIBLE_0 {
                        wValidBitsPerSample: bits_per_sample as _,
                    },
                    dwChannelMask: SPEAKER_FRONT_LEFT | SPEAKER_FRONT_RIGHT,
                    SubFormat: KSDATAFORMAT_SUBTYPE_IEEE_FLOAT,
                };
                aclient
                    .initialize_shared(10 * 1000 * 10, unsafe { std::mem::transmute(&wfx) })
                    .expect("initialize");

                let process_frames = aclient.buffer_size().expect("Getting BufferSize") as u32;
                log::info!("Processing Buffer Size: {process_frames}");
                let sleep_duration = Duration::from_micros(
                    (500_000.0 * process_frames as f64 / wfx.Format.nSamplesPerSec as f64) as _,
                );
                let srv: IAudioRenderClient = aclient.service().expect("No Render Service");
                mixer.write().set_sample_rate(samples_per_sec as _);

                log::info!("Starting AudioRender...");
                aclient.start().expect("Starting AudioRender");
                mixer.write().start();
                while !exit_state_th.load(Ordering::Acquire) {
                    let pad = aclient.current_padding().expect("Current Padding");
                    let available_frames = process_frames - pad;
                    let bufp = unsafe { srv.GetBuffer(available_frames).expect("Get Buffer") };
                    let buf = unsafe {
                        std::slice::from_raw_parts_mut(
                            bufp as *mut f32,
                            (available_frames << 1) as _,
                        )
                    };
                    for b in buf.iter_mut() {
                        *b = 0.0;
                    }

                    let silence = mixer.write().process(buf);

                    unsafe {
                        srv.ReleaseBuffer(
                            available_frames,
                            if silence {
                                AUDCLNT_BUFFERFLAGS_SILENT.0 as _
                            } else {
                                0
                            },
                        )
                        .expect("Release Buffer");
                    }
                    sleep(sleep_duration);
                }
            })?
            .into();

        Ok(Self {
            process_thread,
            exit_state,
        })
    }
}
impl Drop for NativeAudioEngine {
    fn drop(&mut self) {
        self.exit_state.store(true, Ordering::Relaxed);
        self.process_thread
            .take()
            .expect("already joined")
            .join()
            .expect("Joining AudioProcess");
    }
}
