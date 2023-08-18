//! PipeWire Sound Backend

use libspa_sys::spa_format_audio_raw_parse;
use parking_lot::RwLock;
use pw::spa::pod::deserialize::PodDeserializer;
use std::sync::Arc;
use std::sync::RwLock as StdRwLock;

use libspa_sys::{
    spa_audio_info_raw, spa_callbacks, spa_format_audio_raw_build, spa_pod_builder,
    spa_pod_builder_state, SPA_PARAM_EnumFormat, SPA_PARAM_Format, SPA_AUDIO_FORMAT_F32,
    SPA_AUDIO_FORMAT_F32_LE,
};
use pipewire as pw;

use super::AudioBitstreamConverter;
use super::Float32Converter;
use super::SoundBackend;

struct AudioWriter {
    mixer: Arc<StdRwLock<peridot::audio::Mixer>>,
    converter: Box<dyn AudioBitstreamConverter + Sync + Send>,
}
impl AudioWriter {
    fn new(mixer: Arc<StdRwLock<peridot::audio::Mixer>>) -> Self {
        Self {
            mixer,
            converter: Box::new(Float32Converter),
        }
    }

    fn generate(&self, stream: &pw::stream::StreamRef) {
        let mut buf = stream.dequeue_buffer().expect("Failed to dequeue buffer");
        let sample_count = self
            .converter
            .sample_count(buf.datas_mut()[0].data().expect("null buffer?").len());

        let mut generated = vec![0f32; sample_count];
        self.mixer
            .write()
            .expect("AudioMixer lock failed")
            .process(&mut generated);
        self.converter
            .convert(&generated, buf.datas_mut()[0].data().expect("null buffer?"));
    }
}
impl Default for AudioWriter {
    fn default() -> Self {
        unimplemented!("needs default?");
    }
}

pub struct NativeAudioEngine {
    th: Option<std::thread::JoinHandle<()>>,
    pw_sender: pw::channel::Sender<()>,
}
impl NativeAudioEngine {
    pub fn is_available() -> bool {
        let Ok(mainloop) = pw::MainLoop::new() else {
            return false;
        };
        let Ok(ctx) = pw::Context::new(&mainloop) else {
            return false;
        };

        ctx.connect(None).is_ok()
    }

    pub fn new(mixer: &Arc<StdRwLock<peridot::audio::Mixer>>) -> Self {
        info!("Starting AudioEngine via PipeWire......");

        pw::init();

        let (pw_sender, pw_receiver) = pw::channel::channel();
        let writer = AudioWriter::new(mixer.clone());
        let th = std::thread::Builder::new()
            .name(String::from("Peridot-PipeWire Processing Thread"))
            .spawn(move || Self::process_thread(pw_receiver, writer))
            .expect("Failed to spawn communication thread");

        Self {
            th: Some(th),
            pw_sender,
        }
    }

    fn process_thread(exit_signal_receiver: pw::channel::Receiver<()>, writer: AudioWriter) {
        let mainloop = pw::MainLoop::new().expect("Failed to create mainloop");
        let attach = exit_signal_receiver.attach(&mainloop, {
            let mlp = mainloop.clone();
            move |_| mlp.quit()
        });

        let ctx = pw::Context::new(&mainloop).expect("Failed to create pw context");
        let core = ctx.connect(None).expect("Failed to connect to pw");
        let stream = Arc::new(RwLock::new(
            pw::stream::Stream::new(
                &core,
                crate::userlib::APP_TITLE,
                pw::properties! {
                    *pw::keys::MEDIA_TYPE => "Audio",
                    *pw::keys::MEDIA_CATEGORY => "Playback"
                },
            )
            .expect("Failed to create stream"),
        ));
        let stream_wref = Arc::downgrade(&stream);
        let stream_listener = stream
            .write()
            .add_local_listener_with_user_data(writer)
            .state_changed(|old_state, new_state| {
                trace!("State Changed: {old_state:?} -> {new_state:?}");
            })
            .param_changed(move |stream, id, userdata, params| {
                trace!("Param Changed: id={id}");
                if let Some(p) = params {
                    if let Ok((_rest, param_value)) =
                        PodDeserializer::deserialize_any_from(p.as_bytes())
                    {
                        trace!("  Param: {param_value:?}");
                    }
                }

                if id == SPA_PARAM_Format {
                    // configure format
                    let params = params.expect("no params passed?");

                    let mut fmt = core::mem::MaybeUninit::uninit();
                    unsafe { spa_format_audio_raw_parse(params.as_raw_ptr(), fmt.as_mut_ptr()) };
                    let fmt = unsafe { fmt.assume_init() };
                    trace!("Configured Format: {fmt:?}");
                    stream
                        .update_params(&mut [params.as_raw_ptr()])
                        .expect("Failed to negotiate format");

                    if fmt.format == SPA_AUDIO_FORMAT_F32_LE {
                        userdata.converter = Box::new(Float32Converter);
                    } else {
                        unimplemented!("Format conversion not implemented: {}", fmt.format);
                    }
                }
            })
            .process(|stream, state| state.generate(stream))
            .register()
            .expect("Failed to register stream listener");

        let mut pod_buffer = [0u8; 1024];
        let mut pod_builder = spa_pod_builder {
            data: &mut pod_buffer as *mut u8 as *mut _,
            size: 1024,
            _padding: 0,
            state: spa_pod_builder_state {
                offset: 0,
                flags: 0,
                frame: std::ptr::null_mut(),
            },
            callbacks: spa_callbacks {
                funcs: std::ptr::null(),
                data: std::ptr::null_mut(),
            },
        };
        let mut format = spa_audio_info_raw {
            format: SPA_AUDIO_FORMAT_F32,
            channels: 2,
            rate: 44100,
            flags: 0,
            position: [0; 64],
        };
        let mut connect_params = [unsafe {
            pw::spa::pod::Pod::from_raw(spa_format_audio_raw_build(
                &mut pod_builder,
                SPA_PARAM_EnumFormat,
                &mut format,
            ))
        }];

        stream
            .read()
            .connect(
                pw::spa::Direction::Output,
                None,
                pw::stream::StreamFlags::MAP_BUFFERS
                    | pw::stream::StreamFlags::RT_PROCESS
                    | pw::stream::StreamFlags::AUTOCONNECT,
                &mut connect_params,
            )
            .expect("Failed to connect stream");

        mainloop.run();

        let _ = attach.deattach();
        stream_listener.unregister();
        stream
            .read()
            .disconnect()
            .expect("Failed to disconnect stream");
    }
}
impl Drop for NativeAudioEngine {
    fn drop(&mut self) {
        self.pw_sender
            .send(())
            .expect("Failed to send terminate signal");
        self.th
            .take()
            .expect("dropped twice?")
            .join()
            .expect("Communication Thread errored");

        unsafe {
            pw::deinit();
        }
    }
}
impl SoundBackend for NativeAudioEngine {}
