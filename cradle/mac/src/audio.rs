use std::sync::Arc;

use parking_lot::RwLock;

#[derive(Debug, thiserror::Error)]
pub enum OutputAUInitializationError {
    #[error("No output audio component found")]
    NoComponentFound,
    #[error("AudioComponentInstanceNew failed with code {0}")]
    AudioComponentInstanceNewFailed(appkit::OSStatus),
    #[error("AudioUnitInitialize failed with code {0}")]
    AudioUnitInitializeFailed(appkit::OSStatus),
}

#[repr(transparent)]
pub struct OutputAU(appkit::AudioUnit);
impl OutputAU {
    fn new() -> Result<Self, OutputAUInitializationError> {
        let d = appkit::AudioComponentDescription {
            component_type: appkit::kAudioUnitType_Output,
            component_subtype: appkit::kAudioUnitSubType_DefaultOutput,
            component_manufacturer: appkit::kAudioUnitManufacturer_Apple,
            component_flags: 0,
            component_flags_mask: 0,
        };

        let c = unsafe { appkit::AudioComponentFindNext(core::ptr::null_mut(), &d) };
        if c.is_null() {
            return Err(OutputAUInitializationError::NoComponentFound);
        }

        let mut au = core::mem::MaybeUninit::uninit();
        let r = unsafe { appkit::AudioComponentInstanceNew(c, au.as_mut_ptr()) };
        if r != 0 {
            return Err(OutputAUInitializationError::AudioComponentInstanceNewFailed(r));
        }

        let au = unsafe { au.assume_init() };
        let r = unsafe { appkit::AudioUnitInitialize(au) };
        if r != 0 {
            return Err(OutputAUInitializationError::AudioUnitInitializeFailed(r));
        }

        Ok(Self(au))
    }

    #[inline(always)]
    fn set_stream_format(&self, format: &appkit::AudioStreamBasicDescription) -> appkit::OSStatus {
        unsafe {
            appkit::AudioUnitSetProperty(
                self.0,
                appkit::kAudioUnitProperty_StreamFormat,
                appkit::kAudioUnitScope_Input,
                0,
                format as *const _ as *const _,
                core::mem::size_of::<appkit::AudioStreamBasicDescription>() as _,
            )
        }
    }

    fn set_render_callback(
        &self,
        callback: appkit::AURenderCallback,
        context: *mut core::ffi::c_void,
    ) -> appkit::OSStatus {
        let cb = appkit::AURenderCallbackStruct {
            input_proc: callback,
            input_proc_ref_con: context,
        };

        unsafe {
            appkit::AudioUnitSetProperty(
                self.0,
                appkit::kAudioUnitProperty_SetRenderCallback,
                appkit::kAudioUnitScope_Input,
                0,
                &cb as *const _ as *const _,
                core::mem::size_of::<appkit::AURenderCallbackStruct>() as _,
            )
        }
    }

    fn start(&self) -> appkit::OSStatus {
        unsafe { appkit::AudioOutputUnitStart(self.0) }
    }
}
impl Drop for OutputAU {
    fn drop(&mut self) {
        unsafe {
            appkit::AudioComponentInstanceDispose(self.0);
        }
    }
}

pub struct NativeAudioEngine {
    output: Option<OutputAU>,
    amixer: Option<Box<Arc<RwLock<peridot::audio::Mixer>>>>,
}
impl NativeAudioEngine {
    pub fn init() -> Self {
        let output = match OutputAU::new() {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(cause = %e, "Failed to create output audio unit");

                return Self {
                    output: None,
                    amixer: None,
                };
            }
        };

        let af = appkit::AudioStreamBasicDescription {
            sample_rate: 44100.0,
            format_id: appkit::kAudioFormatLinearPCM,
            format_flags: appkit::kAudioFormatFlagIsFloat,
            bits_per_channel: 32,
            channels_per_frame: 2,
            bytes_per_frame: 2 * 4,
            frames_per_packet: 1,
            bytes_per_packet: 2 * 4 * 1,
            _reserved: 0,
        };
        let r = output.set_stream_format(&af);
        if r != 0 {
            panic!("Setting StreamFormat Failed: {r}");
        }

        Self {
            output: Some(output),
            amixer: None,
        }
    }

    pub fn start(&mut self, mixer: Arc<RwLock<peridot::audio::Mixer>>) {
        if let Some(ref o) = self.output {
            let mut mixer = Box::new(mixer);
            o.set_render_callback(Self::render as _, mixer.as_mut() as *mut _ as _);
            o.start();
            mixer.write().start();
            self.amixer = Some(mixer);
        }
    }

    extern "C" fn render(
        in_ref_con: *mut core::ffi::c_void,
        _io_action_flags: *mut appkit::AudioUnitRenderActionFlags,
        _in_time_stamp: *const appkit::AudioTimeStamp,
        _in_bus_number: u32,
        in_number_frames: u32,
        io_data: *mut appkit::AudioBufferList,
    ) -> appkit::OSStatus {
        let ctx = unsafe { &mut *(in_ref_con as *mut Arc<RwLock<peridot::audio::Mixer>>) };
        let bufptr = unsafe {
            std::slice::from_raw_parts_mut(
                (*io_data).buffers[0].data as *mut f32,
                (*io_data).buffers[0].number_channels as usize * in_number_frames as usize,
            )
        };
        for v in bufptr.iter_mut() {
            *v = 0.0;
        }
        ctx.write().process(bufptr);
        // trace!("render callback! {:?} {}", unsafe { &(*io_data).buffers[0] }, in_number_frames);
        0
    }
}
