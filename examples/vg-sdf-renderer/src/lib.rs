
use bedrock as br;
use peridot::{NativeLinker, EngineEvents, Engine, FeatureRequests};
use peridot_vg::FlatPathBuilder;
use peridot_vertex_processing_pack::PvpShaderModules;
use std::rc::Rc;

pub struct Game<NL> {
    buffer: peridot::Buffer,
    stencil_buffer: peridot::Image,
    stencil_buffer_view: br::ImageView,
    rp: br::RenderPass,
    fb: Vec<br::Framebuffer>,
    triangle_fans_stencil_pipeline: peridot::LayoutedPipeline,
    curve_triangles_stencil_pipeline: peridot::LayoutedPipeline,
    cmd: peridot::CommandBundle,
    ph: std::marker::PhantomData<*const NL>
}
impl<NL> Game<NL> {
    pub const NAME: &'static str = "peridot-vg SDF Renderer";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<NL> FeatureRequests for Game<NL> {}
impl<NL: NativeLinker> EngineEvents<NL> for Game<NL> {
    fn init(e: &mut Engine<NL>) -> Self {
        let backbuffer_size = AsRef::<br::Extent2D>::as_ref(e.backbuffer(0).expect("no backbuffer?").size()).clone();

        let font = peridot_vg::FontProvider::new().expect("Failed to create font provider")
            .best_match("sans-serif", &peridot_vg::FontProperties::default(), 240.0)
            .expect("no suitable font");
        let gid = font.glyph_id('あ').expect("no glyph contained");
        let mut gen = peridot_vg::SDFGenerator::new(1.0, 8.0);
        font.outline(gid, &mut gen).expect("Failed to render glyph outline");
        let figure_vertices = gen.build();
        let (figure_triangle_fans_count, figure_curve_triangles_count) = figure_vertices.iter()
            .fold((0, 0), |(t, t2), f| (t + f.triangle_fans.len(), t2 + f.curve_triangles.len()));

        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let figures_triangle_fan_offset = bp.add(
            peridot::BufferContent::vertices::<peridot::math::Vector2<f32>>(figure_triangle_fans_count)
        );
        let figure_curve_triangles_offset = bp.add(
            peridot::BufferContent::vertices::<peridot::VertexUV2D>(figure_curve_triangles_count)
        );
        let buffer = bp.build_transferred().expect("Failed to allocate buffer");
        let buffer_init = bp.build_upload().expect("Failed to allocate init buffer");
        let stencil_buffer = br::ImageDesc::new(
            &backbuffer_size,
            br::vk::VK_FORMAT_S8_UINT,
            br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
            br::ImageLayout::Undefined
        ).create(e.graphics()).expect("Failed to create stencil buffer");

        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(buffer);
        mb.add(stencil_buffer);
        let mut objects = mb.alloc().expect("Failed to allocate memory");
        let stencil_buffer = objects.pop().expect("no objects?").unwrap_image();
        let buffer = objects.pop().expect("no objects?").unwrap_buffer();
        let mut mb_upload = peridot::MemoryBadget::new(e.graphics());
        mb_upload.add(buffer_init);
        let buffer_init = mb_upload.alloc_upload().expect("Failed to allocate init buffer memory")
            .pop().expect("no objects?").unwrap_buffer();
        
        let stencil_buffer_view = stencil_buffer.create_view(
            None, None, &Default::default(), &br::ImageSubresourceRange::stencil(0 .. 1, 0 .. 1)
        ).expect("Failed to create Stencil Buffer View");
        
        buffer_init.guard_map(0 .. bp.total_size(), |m| unsafe {
            let s = m.slice_mut(figures_triangle_fan_offset as _, figure_triangle_fans_count);
            let c = m.slice_mut(figure_curve_triangles_offset as _, figure_curve_triangles_count);
            let (mut s_offset, mut c_offset) = (0, 0);
            for f in figure_vertices.iter() {
                s[s_offset..s_offset + f.triangle_fans.len()].clone_from_slice(&f.triangle_fans);
                c[c_offset..c_offset + f.curve_triangles.len()].clone_from_slice(&f.curve_triangles);
                s_offset += f.triangle_fans.len();
                c_offset += f.curve_triangles.len();
            }
        }).expect("Failed to set init data");
        let mut tfb = peridot::TransferBatch::new();
        tfb.add_mirroring_buffer(&buffer_init, &buffer, 0, bp.total_size());
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT, &buffer, 0 .. bp.total_size(), br::AccessFlags::VERTEX_ATTRIBUTE_READ
        );

        e.submit_commands(|r| {
            tfb.sink_transfer_commands(r); tfb.sink_graphics_ready_commands(r);
            r.pipeline_barrier(
                br::PipelineStageFlags::BOTTOM_OF_PIPE, br::PipelineStageFlags::LATE_FRAGMENT_TESTS, true,
                &[], &[], &[
                    br::ImageMemoryBarrier::new(
                        &br::ImageSubref::stencil(&stencil_buffer, 0 .. 1, 0 .. 1),
                        br::ImageLayout::Undefined, br::ImageLayout::DepthStencilReadOnlyOpt
                    )
                ]
            );
        }).expect("Failed to initialize resources");
        
        let ad_main = br::AttachmentDescription::new(e.backbuffer_format(), e.requesting_backbuffer_layout().0, e.requesting_backbuffer_layout().0)
            .load_op(br::LoadOp::Clear).store_op(br::StoreOp::Store);
        let ad_stencil = br::AttachmentDescription::new(br::vk::VK_FORMAT_S8_UINT, br::ImageLayout::DepthStencilReadOnlyOpt, br::ImageLayout::DepthStencilReadOnlyOpt)
            .stencil_load_op(br::LoadOp::Clear).stencil_store_op(br::StoreOp::DontCare);
        let sp_main = br::SubpassDescription::new()
            .add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None)
            .depth_stencil(1, br::ImageLayout::DepthStencilAttachmentOpt);
        let spdep_color = br::vk::VkSubpassDependency {
            srcSubpass: br::vk::VK_SUBPASS_EXTERNAL,
            dstSubpass: 0,
            srcStageMask: (e.requesting_backbuffer_layout().1).0,
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.late_fragment_tests().0,
            srcAccessMask: 0,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write | br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
            dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT
        };
        let rp = br::RenderPassBuilder::new()
            .add_attachments(vec![ad_main, ad_stencil])
            .add_subpass(sp_main)
            .add_dependency(spdep_color)
            .create(e.graphics())
            .expect("Failed to create RenderPass");
        let fb = e.iter_backbuffers().map(|bb| br::Framebuffer::new(
            &rp, &[&bb, &stencil_buffer_view], &backbuffer_size, 1
        )).collect::<Result<Vec<_>, _>>().expect("Failed to create Framebuffers");

        let scissors = [
            br::vk::VkRect2D {
                offset: br::vk::VkOffset2D { x: 0, y: 0 },
                extent: backbuffer_size.into()
            }
        ];
        let viewports = [
            br::Viewport::from_rect_with_depth_range(&scissors[0], 0.0 .. 1.0).into()
        ];
        let triangle_fans_shader = PvpShaderModules::new(
            e.graphics(), e.load("shaders.triangle_fans").expect("Failed to load triangle_fans shader asset")
        ).expect("Failed to create triangle_fans shader modules");
        let curve_triangles_shader = PvpShaderModules::new(
            e.graphics(), e.load("shaders.curve_triangles").expect("Failed to load curve_triangles shader asset")
        ).expect("Failed to create curve_triangles_shader_modules");
        let empty_pl = Rc::new(br::PipelineLayout::new(e.graphics(), &[], &[]).expect("Failed to create empty pipeline layout"));
        let mut pipebuild = br::GraphicsPipelineBuilder::new(
            &empty_pl, (&rp, 0), triangle_fans_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN)
        );
        let stencil_ops = br::vk::VkStencilOpState {
            failOp: br::vk::VK_STENCIL_OP_INVERT,
            depthFailOp: br::vk::VK_STENCIL_OP_INVERT,
            passOp: br::vk::VK_STENCIL_OP_INVERT,
            compareOp: br::vk::VK_COMPARE_OP_ALWAYS,
            compareMask: 0,
            writeMask: 0x01,
            reference: 0
        };
        pipebuild
            .viewport_scissors(br::DynamicArrayState::Static(&viewports), br::DynamicArrayState::Static(&scissors))
            .multisample_state(Some(br::MultisampleState::new()))
            .stencil_control_front(stencil_ops.clone())
            .stencil_control_back(stencil_ops)
            .stencil_test_enable(true)
            .add_attachment_blend(br::AttachmentColorBlendState::noblend());
        let triangle_fans_stencil_pipeline = pipebuild.create(e.graphics(), None)
            .expect("Failed to create Triangle Fans Stencil Pipeline");
        pipebuild.vertex_processing(curve_triangles_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST));
        let curve_triangles_stencil_pipeline = pipebuild.create(e.graphics(), None)
            .expect("Failed to create Curve Triangles Stencil Pipeline");
        
        let cmd = peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Graphics, e.backbuffer_count()).expect("Failed to create CommandBundle");
        for (cx, (bb, fb)) in e.iter_backbuffers().zip(fb.iter()).enumerate() {
            let clear_values = &[br::ClearValue::Color([0.0; 4]), br::ClearValue::DepthStencil(0.0, 0)];

            let mut rec = cmd[cx].begin().expect("Failed to begin recording commands");
            rec
                .begin_render_pass(&rp, fb, scissors[0].clone(), clear_values, true)
                .bind_graphics_pipeline_pair(&triangle_fans_stencil_pipeline, &empty_pl)
                .bind_vertex_buffers(0, &[(&buffer, figures_triangle_fan_offset as _)]);
            let mut vo = 0;
            for f in figure_vertices.iter() {
                rec.draw(f.triangle_fans.len() as _, 1, vo, 0);
                vo += f.triangle_fans.len() as u32;
            }
            rec
                .bind_graphics_pipeline(&curve_triangles_stencil_pipeline)
                .bind_vertex_buffers(0, &[(&buffer, figure_curve_triangles_offset as _)]);
            let mut co = 0;
            for f in figure_vertices.iter() {
                rec.draw(f.curve_triangles.len() as _, 1, co, 0);
                co += f.curve_triangles.len() as u32;
            }
            rec.end_render_pass();
        }

        Game {
            buffer,
            stencil_buffer,
            stencil_buffer_view,
            rp,
            fb,
            triangle_fans_stencil_pipeline: peridot::LayoutedPipeline::combine(
                triangle_fans_stencil_pipeline, &empty_pl
            ),
            curve_triangles_stencil_pipeline: peridot::LayoutedPipeline::combine(
                curve_triangles_stencil_pipeline, &empty_pl
            ),
            cmd,
            ph: std::marker::PhantomData
        }
    }

    fn update(
        &mut self, _e: &Engine<NL>, on_backbuffer_of: u32, _delta_time: std::time::Duration
    ) -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        (None, br::SubmissionBatch {
            command_buffers: std::borrow::Cow::Borrowed(
                &self.cmd[on_backbuffer_of as usize .. on_backbuffer_of as usize + 1]
            ),
            .. Default::default()
        })
    }
}
