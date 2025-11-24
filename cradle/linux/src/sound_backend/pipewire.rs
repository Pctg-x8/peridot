//! PipeWire Sound Backend

use parking_lot::RwLock;
use pipewire as pw;
use std::sync::Arc;

use super::AudioBitstreamConverter;
use super::Float32Converter;
use super::SoundBackend;

struct AudioWriter {
    mixer: Arc<RwLock<peridot::audio::Mixer>>,
    converter: Box<dyn AudioBitstreamConverter + Sync + Send>,
}
impl AudioWriter {
    fn new(mixer: Arc<RwLock<peridot::audio::Mixer>>) -> Self {
        Self {
            mixer,
            converter: Box::new(Float32Converter),
        }
    }

    fn generate(&self, stream: &mut pw::Stream) {
        let Some(mut buf) = stream.rent_buffer() else {
            tracing::warn!("out of buffer");
            return;
        };

        let sample_count = self
            .converter
            .sample_count(buf.datas_mut()[0].max_size() as _);

        let mut generated = vec![0f32; sample_count];
        self.mixer.write().process(&mut generated);
        self.converter.convert(&generated, unsafe {
            core::slice::from_raw_parts_mut(
                buf.datas_mut()[0].data_ptr().cast(),
                core::mem::size_of::<f32>() * sample_count as usize,
            )
        });
        buf.datas_mut()[0].update_chunk_info(
            0,
            core::mem::size_of::<f32>() as _,
            (core::mem::size_of::<f32>() * sample_count) as _,
            pw::spa::ChunkFlags::NONE,
        );
    }
}
impl Default for AudioWriter {
    fn default() -> Self {
        unimplemented!("needs default?");
    }
}

pub struct LoopEngine {
    stream_ptr: *mut pw::Stream,
    writer: AudioWriter,
}
impl pw::StreamEventListener for LoopEngine {
    #[tracing::instrument(
        name = "<LoopEngine as pw::StreamEventListener>::state_changed",
        skip(self)
    )]
    fn state_changed(
        &mut self,
        old: Result<pipewire::StreamState, std::ffi::c_int>,
        state: Result<pipewire::StreamState, std::ffi::c_int>,
        error: Option<&std::ffi::CStr>,
    ) {
        tracing::trace!("State Changed");
    }

    #[tracing::instrument(
        name = "<LoopEngine as pw::StreamEventListener>::param_changed",
        skip(self)
    )]
    fn param_changed(&mut self, id: u32, param: *const pipewire::raw::spa_pod) {
        if id == pw::raw::SPA_PARAM_Format as _ {
            // configure format
            let Some(param) = (unsafe { param.as_ref() }) else {
                tracing::warn!("no params passed?");
                return;
            };
            let param_parser = pw::spa::pod::Parser::new(param)
                .try_as_object()
                .expect("not a object");
            assert_eq!(param_parser.object_type(), pw::raw::SPA_TYPE_OBJECT_Format);

            for p in param_parser.iter_props() {
                if p.key() == pw::raw::spa_format::AUDIO_format as _ {
                    if let Some(v) = p.value().try_as_id() {
                        tracing::trace!(format = v.value(), "audio format changed");

                        if v.value() == pw::raw::SPA_AUDIO_FORMAT_F32_LE as _ {
                            self.writer.converter = Box::new(Float32Converter);
                        } else {
                            tracing::warn!(format = v.value(), "Format conversion not implemented");
                        }
                    } else if let Some(v) = p.value().try_as_choice() {
                        if let Some(v) = v.try_as_none() {
                            match v.child_type() {
                                Ok(pw::spa::pod::Type::Id) => {
                                    let current_value = unsafe { *v.current_unchecked::<u32>() };
                                    tracing::trace!(format = current_value, "audio format changed");

                                    if current_value == pw::raw::SPA_AUDIO_FORMAT_F32_LE as _ {
                                        self.writer.converter = Box::new(Float32Converter);
                                    } else {
                                        tracing::warn!(
                                            format = current_value,
                                            "Format conversion not implemented"
                                        );
                                    }
                                }
                                t => {
                                    tracing::warn!(child_type = ?t, "unexpected spa_format::AUDIO_format value")
                                }
                            }
                        } else {
                            tracing::warn!(r#type = ?v.choice_type(), "unimplemented: format choice");
                        }
                    } else {
                        tracing::warn!(r#type = ?p.value().r#type(), "unexpected spa_format::AUDIO_format value");
                        continue;
                    }
                }
            }
        } else {
            if let Some(p) = unsafe { param.as_ref().map(pw::spa::pod::Parser::new) } {
                tracing::trace!(param_type = ?p.r#type(), "Unknown Param Changed");
            } else {
                tracing::trace!("Unknown Param Changed without value");
            }
        }
    }

    #[tracing::instrument(name = "<LoopEngine as pw::StreamEventListener>::process", skip(self))]
    fn process(&mut self) {
        self.writer.generate(unsafe { &mut *self.stream_ptr });
    }
}

pub struct NativeAudioEngineInit {
    // Note: should be terminated in this order...
    core: pw::Owned<pw::Core>,
    #[allow(dead_code)]
    ctx: pw::Owned<pw::Context>,
    mainloop: pw::Owned<pw::MainLoop>,
}

pub struct NativeAudioEngine {
    th: Option<std::thread::JoinHandle<()>>,
    mainloop_ptr: *mut pw::MainLoop,
}
impl NativeAudioEngine {
    #[tracing::instrument]
    pub fn try_init() -> Option<NativeAudioEngineInit> {
        pw::init();

        let mainloop = pw::MainLoop::new(None)
            .inspect_err(|e| tracing::warn!(reason = ?e, "MainLoop::new failed"))
            .ok()?;
        let mut ctx = pw::Context::new(&mainloop, None, 0)
            .inspect_err(|e| tracing::warn!(reason = ?e, "Context::new failed"))
            .ok()?;
        let core = ctx
            .connect(None, 0)
            .inspect_err(|e| tracing::warn!(reason = ?e, "context.connect failed"))
            .ok()?;

        Some(NativeAudioEngineInit {
            mainloop,
            ctx,
            core,
        })
    }

    pub fn new(init: NativeAudioEngineInit, mixer: &Arc<RwLock<peridot::audio::Mixer>>) -> Self {
        tracing::info!("Starting AudioEngine via PipeWire......");

        let mainloop_ptr = init.mainloop.as_ptr();
        let writer = AudioWriter::new(mixer.clone());
        let th = std::thread::Builder::new()
            .name(String::from("Peridot-PipeWire Processing Thread"))
            .spawn(move || Self::process_thread(init, writer))
            .expect("Failed to spawn communication thread");

        Self {
            th: Some(th),
            mainloop_ptr,
        }
    }

    fn process_thread(init: NativeAudioEngineInit, writer: AudioWriter) {
        let mut stream = pw::Stream::new(
            &init.core,
            &unsafe {
                std::ffi::CString::from_vec_unchecked(crate::userlib::APP_TITLE.as_bytes().to_vec())
            },
            Some(
                pw::Properties::new(&[
                    pw::spa::DictItem::new(c"media.type", c"Audio"),
                    pw::spa::DictItem::new(c"media.category", c"Playback"),
                    pw::spa::DictItem::new(c"media.role", c"Game"),
                ])
                .expect("Properties::new failed"),
            ),
        )
        .expect("Failed to create stream");

        let mut loop_engine = LoopEngine {
            stream_ptr: stream.as_ptr(),
            writer,
        };
        let mut stream_event_listener_hook = core::pin::pin!(pw::spa::Hook::new());
        stream.add_listener(stream_event_listener_hook.as_mut(), &mut loop_engine);

        let mut pod_builder = pw::spa::pod::Builder::with_capacity(1024);
        pod_builder
            .begin_object(
                pw::raw::SPA_TYPE_OBJECT_Format,
                pw::raw::SPA_PARAM_EnumFormat as _,
            )
            .prop_heading(pw::raw::spa_format::mediaType as _, 0)
            .id(pw::raw::spa_media_type::audio as _)
            .prop_heading(pw::raw::spa_format::mediaSubtype as _, 0)
            .id(pw::raw::spa_media_subtype::raw as _)
            .prop_heading(pw::raw::spa_format::AUDIO_format as _, 0)
            .id(pw::raw::SPA_AUDIO_FORMAT_F32_LE as _)
            .prop_heading(pw::raw::spa_format::AUDIO_rate as _, 0)
            .int(44100)
            .prop_heading(pw::raw::spa_format::AUDIO_channels as _, 0)
            .int(2)
            .end_object();
        let format_pod = pod_builder.into_bytes();

        stream
            .connect(
                pw::Direction::Output,
                pw::StreamFlags::MAP_BUFFERS
                    | pw::StreamFlags::RT_PROCESS
                    | pw::StreamFlags::AUTOCONNECT,
                &mut [format_pod.as_ptr().cast::<pw::raw::spa_pod>()],
            )
            .expect("Failed to connect stream");

        init.mainloop.run().expect("mainloop.run failed");
    }
}
impl Drop for NativeAudioEngine {
    fn drop(&mut self) {
        unsafe { &mut *self.mainloop_ptr }
            .quit()
            .expect("mainloop.quit failed");
        self.th
            .take()
            .expect("dropped twice?")
            .join()
            .expect("Communication Thread errored");

        pw::deinit();
    }
}
impl SoundBackend for NativeAudioEngine {}
