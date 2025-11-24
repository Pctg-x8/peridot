//! PipeWire Sound Backend

use parking_lot::RwLock;
use peridot_tp_pipewire as pw;
use std::sync::Arc;

use super::AudioBitstreamConverter;
use super::Float32Converter;
use super::SoundBackend;

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
        let mixer = mixer.clone();
        let th = std::thread::Builder::new()
            .name(String::from("Peridot-PipeWire Processing Thread"))
            .spawn(move || Self::process_thread(init, mixer))
            .expect("Failed to spawn communication thread");

        Self {
            th: Some(th),
            mainloop_ptr,
        }
    }

    fn process_thread(init: NativeAudioEngineInit, mixer: Arc<RwLock<peridot::audio::Mixer>>) {
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

        let mut loop_driver = LoopDriver {
            stream_ptr: stream.as_ptr(),
            mixer,
            converter: Box::new(Float32Converter),
        };
        let mut stream_event_listener_hook = core::pin::pin!(pw::spa::Hook::new());
        stream.add_listener(stream_event_listener_hook.as_mut(), &mut loop_driver);

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
    #[tracing::instrument(name = "NativeAudioEngine::drop", skip(self))]
    fn drop(&mut self) {
        let Some(thread) = self.th.take() else {
            tracing::warn!("NativeAudioEngine dropped twiece");
            return;
        };

        if let Err(e) = unsafe { &mut *self.mainloop_ptr }.quit() {
            tracing::warn!(reason = ?e, "mainloop.quit failed");
        }
        if let Err(e) = thread.join() {
            tracing::warn!(reason = ?e, "error thrown in communication thread");
        }
        pw::deinit();
    }
}
impl SoundBackend for NativeAudioEngine {}

struct LoopDriver {
    stream_ptr: *mut pw::Stream,
    mixer: Arc<RwLock<peridot::audio::Mixer>>,
    converter: Box<dyn AudioBitstreamConverter + Sync + Send>,
}
impl pw::StreamEventListener for LoopDriver {
    #[tracing::instrument(
        name = "<LoopDriver as pw::StreamEventListener>::state_changed",
        skip(self)
    )]
    fn state_changed(
        &mut self,
        old: Result<pw::StreamState, core::ffi::c_int>,
        state: Result<pw::StreamState, core::ffi::c_int>,
        error: Option<&core::ffi::CStr>,
    ) {
        tracing::trace!("State Changed");
    }

    #[tracing::instrument(
        name = "<LoopDriver as pw::StreamEventListener>::param_changed",
        skip(self, param), fields(id = ?pw::spa::ParamTypeStr(id), with_param = param.is_some())
    )]
    fn param_changed(&mut self, id: pw::raw::spa_param_type, param: Option<&pw::raw::spa_pod>) {
        if id == pw::raw::SPA_PARAM_Format {
            // configure format
            let Some(param) = param else {
                tracing::warn!("no params passed?");
                return;
            };
            let param_parser = pw::spa::pod::Parser::new(param)
                .try_as_object()
                .expect("not a object");
            assert_eq!(param_parser.object_type(), pw::raw::SPA_TYPE_OBJECT_Format);

            let mut format_changes = None;
            for p in param_parser.iter_props() {
                if p.key() == pw::raw::spa_format::AUDIO_format as _ {
                    if let Some(v) = p.value().try_as_id() {
                        format_changes = Some(v.value());
                        continue;
                    }
                    if let Some(v) = p.value().try_as_choice() {
                        if let Some(v) = v.try_as_none() {
                            match v.child_type() {
                                Ok(pw::spa::pod::Type::Id) => {
                                    format_changes = Some(unsafe { *v.current_unchecked::<u32>() });
                                }
                                t => {
                                    tracing::warn!(choice.none.child_type = ?t, "unexpected spa_format::AUDIO_format value");
                                }
                            }
                            continue;
                        }

                        tracing::warn!(r#type = ?v.choice_type(), "unimplemented: format choice");
                        continue;
                    }

                    tracing::warn!(r#type = ?p.value().r#type(), "unexpected spa_format::AUDIO_format value");
                }
            }

            if let Some(f) = format_changes {
                tracing::trace!(format = f, "audio format changed");

                if f == pw::raw::SPA_AUDIO_FORMAT_F32_LE as _ {
                    self.converter = Box::new(Float32Converter);
                } else {
                    tracing::warn!(format = f, "Format conversion not implemented");
                }
            }

            return;
        }

        // logging unknown
        if let Some(p) = param.map(pw::spa::pod::Parser::new) {
            tracing::debug!(param_type = ?p.r#type(), "Unknown Param Changed");
        } else {
            tracing::debug!("Unknown Param Changed without value");
        }
    }

    #[tracing::instrument(name = "<LoopDriver as pw::StreamEventListener>::process", skip(self))]
    fn process(&mut self) {
        let Some(mut buf) = unsafe { &mut *self.stream_ptr }.rent_buffer() else {
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
