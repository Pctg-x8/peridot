use bedrock::{self as br, CommandBufferMut, CommandPoolMut, SubmissionBatch};

pub struct Game {
    render_cb: Vec<br::CommandBufferObject>,
}
impl peridot::FeatureRequests for Game {}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game {
    fn init(e: &mut peridot::Engine<NL>) -> Self {
        let mut render_cp = br::CommandPoolBuilder::new(e.graphics_queue_family_index())
            .create(e.graphics().device().clone())
            .expect("Failed to create render command pool");
        let mut render_cb = render_cp
            .alloc(e.back_buffer_count() as _, true)
            .expect("Failed to allocate render command buffers");
        for cb in render_cb.iter_mut() {
            unsafe {
                cb.begin(e.graphics().device())
                    .expect("Failed to begin render command recording")
            }
            .end()
            .expect("Failed to finish render command recording");
        }

        Self { render_cb }
    }

    fn update(
        &mut self,
        e: &mut peridot::Engine<NL>,
        on_back_buffer_of: u32,
        _delta_time: std::time::Duration,
    ) {
        e.do_render(
            on_back_buffer_of,
            None,
            br::EmptySubmissionBatch.with_command_buffers(
                &self.render_cb[on_back_buffer_of as usize..=on_back_buffer_of as usize],
            ),
        )
        .expect("Failed to render");
    }
}
