use std::sync::Arc;

use parking_lot::RwLock;

pub struct AudioEngineContext {
    mixer: Arc<RwLock<peridot::audio::Mixer>>,
}
impl AudioEngineContext {
    pub fn new(mixer: Arc<RwLock<peridot::audio::Mixer>>) -> Self {
        Self { mixer }
    }

    pub fn connect(self: core::pin::Pin<&mut Self>, swift_context: *mut core::ffi::c_void) {
        let this = self.get_mut();

        extern "C" fn format_callback(
            _ctx: *mut core::ffi::c_void,
            channel_count: u32,
            sample_rate: core::ffi::c_double,
        ) {
            tracing::debug!("format callback: {channel_count} {sample_rate}");
        }
        extern "C" fn render_callback(
            ctx: *mut core::ffi::c_void,
            frame_count: u32,
            ab: *mut core::ffi::c_void,
        ) -> u8 {
            let ctx = unsafe { &mut *ctx.cast::<AudioEngineContext>() };
            let num_buffers = unsafe { *ab.byte_add(0).cast::<u32>() };
            assert_eq!(num_buffers, 1);
            let buf = unsafe {
                core::slice::from_raw_parts_mut(
                    *ab.byte_add(8 + 8).cast::<*mut f32>(),
                    (frame_count * 2) as _,
                )
            };
            buf.fill(0.0);
            let silence = ctx.mixer.write().process(buf);

            if silence {
                1
            } else {
                0
            }
        }
        unsafe {
            crate::native_interface::launch_audio(
                swift_context,
                this as *mut _ as _,
                format_callback,
                render_callback,
            );
        }
        this.mixer.write().start();
    }
}
