//! PulseAudio Sound Backend

use pulseaudio_rs as pa;
use std::cell::RefCell;
use std::pin::Pin;
use std::{
    mem::ManuallyDrop,
    sync::{Arc, RwLock},
};

use crate::sound_backend::{Float32Converter, SignedInt24LEConverter, SignedInt32LEConverter};

use super::{AudioBitstreamConverter, SoundBackend};

struct AudioDataWriter {
    mixer: Arc<RwLock<peridot::audio::Mixer>>,
    conv: RefCell<Box<dyn AudioBitstreamConverter>>,
}
impl pa::stream::WriteRequestHandler for AudioDataWriter {
    fn callback(&self, stream: &mut pa::StreamRef, nbytes: usize) {
        let mut rem = nbytes;
        while rem > 0 {
            let (bufptr, len) = stream.begin_write(rem).expect("BeginWrite failed!");
            let aslice = unsafe { std::slice::from_raw_parts_mut(bufptr as *mut u8, len) };
            let mut buf = vec![0.0f32; self.conv.borrow().sample_count(len)];
            self.mixer
                .write()
                .expect("Mixer Write Failed!")
                .process(&mut buf);
            self.conv.borrow().convert(&buf, aslice);
            stream
                .write_slice(&aslice, None)
                .expect("Writing Slice failed!");
            rem -= len;
        }
    }
}

pub struct NativeAudioEngine {
    mlp: Pin<Box<pa::mainloop::Threaded>>,
    context: ManuallyDrop<pa::Context>,
    stream: pa::Stream,
    _wh: Pin<Box<AudioDataWriter>>,
}
impl NativeAudioEngine {
    extern "C" fn state_callback(_: *mut pa::ffi::pa_context, user: *mut libc::c_void) {
        let p = unsafe { &mut *(user as *mut pa::mainloop::Threaded) };
        p.signal(false);
    }
    extern "C" fn stream_state_callback(_: *mut pa::ffi::pa_stream, user: *mut libc::c_void) {
        let p = unsafe { &mut *(user as *mut pa::mainloop::Threaded) };
        p.signal(false);
    }

    pub fn new(mixer: &Arc<RwLock<peridot::audio::Mixer>>) -> Self {
        tracing::info!("Starting AudioEngine via PulseAudio......");

        let mlp = Box::pin(
            pa::mainloop::Threaded::new().expect("Failed to initialize PulseAudio Mainloop"),
        );
        let (mut context, mut stream, writer);
        {
            let mut l = pa::mainloop::LockedLoop::new(Pin::into_inner(mlp.as_ref()));
            context = pa::Context::new(&l, "Peridot::NativeAudioEngine")
                .expect("Failed to initialize PulseAudio context");
            context.set_state_callback(
                Some(Self::state_callback),
                Pin::into_inner(mlp.as_ref()) as *const _ as _,
            );
            context
                .connect(None, pa::context::NOFLAGS, None)
                .expect("Failed to connect PulseAudio server");
            l.start().expect("Failed to start PulseAudio Mainloop");
            let mut current_state = context.state();
            while current_state != pa::context::State::Ready {
                l.wait();
                current_state = context.state();
            }

            let spec = pa::SampleSpec {
                format: pa::SampleFormat::FLOAT32LE as _,
                rate: 44100,
                channels: 2,
            };
            stream = context
                .new_stream("Peridot::NativeAudioEngine::Output", &spec, None)
                .expect("Failed to initialize playback stream");
            writer = Box::pin(AudioDataWriter {
                mixer: mixer.clone(),
                conv: RefCell::new(Box::new(Float32Converter)),
            });
            stream.set_write_request_callback(writer.as_ref());
            stream.set_state_callback(
                Some(Self::stream_state_callback),
                Pin::into_inner(mlp.as_ref()) as *const _ as _,
            );
            let flags = pa::stream::AUTO_TIMING_UPDATE
                | pa::stream::FIX_FORMAT
                | pa::stream::FIX_RATE
                | pa::stream::FIX_CHANNELS;
            stream
                .connect_playback(None, None, flags, None, None)
                .expect("Failed to connect playback stream");
            let mut current_state = stream.state();
            while current_state != pa::stream::State::Ready {
                l.wait();
                current_state = stream.state();
            }
            tracing::info!("PulseAudio Sink Device = {}", stream.device_name());
            let ss = stream.sample_spec();
            tracing::debug!("SampleSpec: {} {} {}", ss.format, ss.rate, ss.channels);
            *writer.conv.borrow_mut() = if ss.format == pa::SampleFormat::S24LE as _ {
                Box::new(SignedInt24LEConverter)
            } else if ss.format == pa::SampleFormat::S32LE as _ {
                Box::new(SignedInt32LEConverter)
            } else {
                panic!("pa: Unsupported sample format: {}", ss.format)
            };
        }

        tracing::trace!("Done!");
        NativeAudioEngine {
            mlp,
            context: ManuallyDrop::new(context),
            stream,
            _wh: writer,
        }
    }
}
impl Drop for NativeAudioEngine {
    fn drop(&mut self) {
        {
            let _ = pa::mainloop::LockedLoop::new(Pin::into_inner(self.mlp.as_ref()));
            self.stream.disconnect();
        }
        unsafe {
            ManuallyDrop::drop(&mut self.context);
        }
        Pin::into_inner(self.mlp.as_mut()).stop();
    }
}
impl SoundBackend for NativeAudioEngine {}
