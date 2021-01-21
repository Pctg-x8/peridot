
use bedrock as br;
use peridot::{NativeLinker, EngineEvents, Engine, FeatureRequests};
use peridot_vg::FlatPathBuilder;
use peridot_vertex_processing_pack::PvpShaderModules;
use std::rc::Rc;
use peridot::SpecConstantStorage;

#[derive(peridot_derive::SpecConstantStorage)]
#[repr(C)]
pub struct FillFragmentShaderParameters {
    enable_color_output: bool
}

pub struct Game<NL> {
    buffer: peridot::Buffer,
    stencil_buffer: peridot::Image,
    stencil_buffer_view: br::ImageView,
    rp: br::RenderPass,
    fb: Vec<br::Framebuffer>,
    triangle_fans_stencil_pipeline: peridot::LayoutedPipeline,
    curve_triangles_stencil_pipeline: peridot::LayoutedPipeline,
    invert_pipeline: peridot::LayoutedPipeline,
    outline_distance_pipeline: peridot::LayoutedPipeline,
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
            .best_match("sans-serif", &peridot_vg::FontProperties::default(), 120.0)
            .expect("no suitable font");
        let gid = font.glyph_id('A').expect("no glyph contained");
        const SDF_SIZE: f32 = 32.0;
        let mut gen = peridot_vg::SDFGenerator::new(1.0, SDF_SIZE);
        gen.set_transform(peridot_vg::sdf_generator::Transform2D::create_translation(0.0, -font.ascent() - SDF_SIZE));
        font.outline(gid, &mut gen).expect("Failed to render glyph outline");
        let figure_vertices = gen.build();
        let (figure_triangle_fans_count, figure_curve_triangles_count, outline_rects_count) = figure_vertices.iter()
            .fold((0, 0, 0), |(t, t2, t3), f| (t + f.triangle_fans.len(), t2 + f.curve_triangles.len(), t3 + f.parabola_rects.len()));

        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let flip_fill_rect = bp.add(
            peridot::BufferContent::vertex::<[peridot::math::Vector2<f32>; 4]>()
        );
        let figures_triangle_fan_offset = bp.add(
            peridot::BufferContent::vertices::<peridot::math::Vector2<f32>>(figure_triangle_fans_count)
        );
        let figure_curve_triangles_offset = bp.add(
            peridot::BufferContent::vertices::<peridot::VertexUV2D>(figure_curve_triangles_count)
        );
        let outline_rects_offset = bp.add(
            peridot::BufferContent::vertices::<peridot_vg::sdf_generator::ParabolaRectVertex>(outline_rects_count * 6)
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
            m.slice_mut(flip_fill_rect as _, 4).clone_from_slice(&[
                peridot::math::Vector2(-640.0f32, -480.0),
                peridot::math::Vector2( 640.0, -480.0),
                peridot::math::Vector2(-640.0,  480.0),
                peridot::math::Vector2( 640.0,  480.0)
            ]);

            let s = m.slice_mut(figures_triangle_fan_offset as _, figure_triangle_fans_count);
            let c = m.slice_mut(figure_curve_triangles_offset as _, figure_curve_triangles_count);
            let o = m.slice_mut(outline_rects_offset as _, outline_rects_count * 6);
            let (mut s_offset, mut c_offset, mut o_offset) = (0, 0, 0);
            for f in figure_vertices.iter() {
                s[s_offset..s_offset + f.triangle_fans.len()].clone_from_slice(&f.triangle_fans);
                c[c_offset..c_offset + f.curve_triangles.len()].clone_from_slice(&f.curve_triangles);
                for pr in f.parabola_rects.iter() {
                    o[o_offset..o_offset + 6].clone_from_slice(&pr.make_vertices());
                    o_offset += 6;
                }
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
        let sp_stencil = br::SubpassDescription::new()
            .depth_stencil(1, br::ImageLayout::DepthStencilAttachmentOpt);
        let sp_main = br::SubpassDescription::new()
            .add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None)
            .depth_stencil(1, br::ImageLayout::DepthStencilReadOnlyOpt);
        let spdep_color = br::vk::VkSubpassDependency {
            srcSubpass: br::vk::VK_SUBPASS_EXTERNAL,
            dstSubpass: 0,
            srcStageMask: (e.requesting_backbuffer_layout().1).0,
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.late_fragment_tests().0,
            srcAccessMask: 0,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write | br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
            dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT
        };
        let spdep_stencil = br::vk::VkSubpassDependency {
            srcSubpass: 0,
            dstSubpass: 1,
            srcStageMask: br::PipelineStageFlags::LATE_FRAGMENT_TESTS.0,
            dstStageMask: br::PipelineStageFlags::EARLY_FRAGMENT_TESTS.0,
            srcAccessMask: br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
            dstAccessMask: br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.read,
            dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT
        };
        let rp = br::RenderPassBuilder::new()
            .add_attachments(vec![ad_main, ad_stencil])
            .add_subpasses(vec![sp_stencil, sp_main])
            .add_dependencies(vec![spdep_color, spdep_stencil])
            .create(e.graphics())
            .expect("Failed to create RenderPass");
        let fb = e.iter_backbuffers().map(|bb| br::Framebuffer::new(
            &rp, &[&bb, &stencil_buffer_view], &backbuffer_size, 1
        )).collect::<Result<Vec<_>, _>>().expect("Failed to create Framebuffers");

        let vertex_shader_parameter_values = [backbuffer_size.0 as f32, backbuffer_size.1 as _, SDF_SIZE];
        let vertex_shader_common_parameter_placements = [
            br::vk::VkSpecializationMapEntry {
                constantID: 0, offset: 0, size: std::mem::size_of::<f32>() as _
            },
            br::vk::VkSpecializationMapEntry {
                constantID: 1, offset: std::mem::size_of::<f32>() as _, size: std::mem::size_of::<f32>() as _
            }
        ];
        let outline_vsh_parameter_placements = [
            vertex_shader_common_parameter_placements[0].clone(),
            vertex_shader_common_parameter_placements[1].clone(),
            br::vk::VkSpecializationMapEntry {
                constantID: 2, offset: (std::mem::size_of::<f32>() * 2) as _, size: std::mem::size_of::<f32>() as _
            }
        ];
        let fill_fsh_color_output = FillFragmentShaderParameters {
            enable_color_output: true
        };

        let scissors = [
            br::vk::VkRect2D {
                offset: br::vk::VkOffset2D { x: 0, y: 0 },
                extent: backbuffer_size.into()
            }
        ];
        let viewports = [
            br::Viewport::from_rect_with_depth_range(&scissors[0], 0.0 .. 1.0).into()
        ];
        let fill_shader = PvpShaderModules::new(
            e.graphics(), e.load("shaders.triangle_fans").expect("Failed to load triangle_fans shader asset")
        ).expect("Failed to create triangle_fans shader modules");
        let curve_triangles_shader = PvpShaderModules::new(
            e.graphics(), e.load("shaders.curve_triangles").expect("Failed to load curve_triangles shader asset")
        ).expect("Failed to create curve_triangles shader modules");
        let outline_distance_shader = PvpShaderModules::new(
            e.graphics(), e.load("shaders.outline_distance").expect("Failed to load outline_distance shader asset")
        ).expect("Failed to create outline_disdtance shader modules");
        let empty_pl = Rc::new(br::PipelineLayout::new(e.graphics(), &[], &[]).expect("Failed to create empty pipeline layout"));

        let mut stencil_triangle_shader = fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_FAN);
        stencil_triangle_shader.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&vertex_shader_common_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values)
        ));
        let mut pipebuild = br::GraphicsPipelineBuilder::new(&empty_pl, (&rp, 0), stencil_triangle_shader);
        let stencil_ops = br::vk::VkStencilOpState {
            failOp: br::vk::VK_STENCIL_OP_INVERT,
            depthFailOp: br::vk::VK_STENCIL_OP_INVERT,
            passOp: br::vk::VK_STENCIL_OP_INVERT,
            compareOp: br::vk::VK_COMPARE_OP_ALWAYS,
            compareMask: 0,
            writeMask: 0x01,
            reference: 0
        };
        let stencil_noop_match = br::vk::VkStencilOpState {
            failOp: br::vk::VK_STENCIL_OP_KEEP,
            depthFailOp: br::vk::VK_STENCIL_OP_KEEP,
            passOp: br::vk::VK_STENCIL_OP_ZERO,
            compareOp: br::vk::VK_COMPARE_OP_EQUAL,
            compareMask: 0x01,
            writeMask: 0x01,
            reference: 0x01
        };
        let stencil_noop = br::vk::VkStencilOpState {
            failOp: br::vk::VK_STENCIL_OP_KEEP,
            depthFailOp: br::vk::VK_STENCIL_OP_KEEP,
            passOp: br::vk::VK_STENCIL_OP_KEEP,
            compareOp: br::vk::VK_COMPARE_OP_ALWAYS,
            compareMask: 0,
            writeMask: 0,
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
        let mut stencil_curve_shader = curve_triangles_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_curve_shader.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&vertex_shader_common_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values)
        ));
        pipebuild.vertex_processing(stencil_curve_shader);
        let curve_triangles_stencil_pipeline = pipebuild.create(e.graphics(), None)
            .expect("Failed to create Curve Triangles Stencil Pipeline");
        let mut invert_fill_shader = fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        invert_fill_shader.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&vertex_shader_common_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values)
        ));
        invert_fill_shader.fragment_shader_mut().expect("no fragment shader?").specinfo = Some(
            fill_fsh_color_output.as_pair()
        );
        pipebuild
            .render_pass(&rp, 1)
            .vertex_processing(invert_fill_shader)
            .stencil_control_front(stencil_noop_match.clone())
            .stencil_control_back(stencil_noop_match)
            .set_attachment_blends(vec![
                br::vk::VkPipelineColorBlendAttachmentState {
                    blendEnable: true as _,
                    srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR,
                    dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_ZERO,
                    colorBlendOp: br::vk::VK_BLEND_OP_ADD,
                    srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
                    dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ZERO,
                    alphaBlendOp: br::vk::VK_BLEND_OP_ADD,
                    .. Default::default()
                }
            ]);
        let invert_pipeline = pipebuild.create(e.graphics(), None).expect("Failed to create Invert Pipeline");
        let mut outline_render_vps = outline_distance_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        outline_render_vps.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&outline_vsh_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values)
        ));
        pipebuild
            .render_pass(&rp, 1)
            .vertex_processing(outline_render_vps)
            .stencil_control_front(stencil_noop.clone())
            .stencil_control_back(stencil_noop)
            .stencil_test_enable(false)
            .set_attachment_blends(vec![
                br::vk::VkPipelineColorBlendAttachmentState {
                    blendEnable: true as _,
                    srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_COLOR,
                    dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_DST_COLOR,
                    colorBlendOp: br::vk::VK_BLEND_OP_MAX,
                    srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
                    dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_DST_ALPHA,
                    alphaBlendOp: br::vk::VK_BLEND_OP_MAX,
                    .. Default::default()
                }
            ]);
        let outline_distance_pipeline = pipebuild.create(e.graphics(), None)
            .expect("Failed to create Outline Distance Pipeline");
        
        let cmd = peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Graphics, e.backbuffer_count()).expect("Failed to create CommandBundle");
        for (cx, (bb, fb)) in e.iter_backbuffers().zip(fb.iter()).enumerate() {
            let clear_values = &[br::ClearValue::Color([0.0; 4]), br::ClearValue::DepthStencil(0.0, 0)];

            let mut rec = cmd[cx].begin().expect("Failed to begin recording commands");
            // Stencil Pass
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
            rec.next_subpass(true);
            // Outline Distance and Invert Pass
            rec
                .bind_graphics_pipeline(&outline_distance_pipeline)
                .bind_vertex_buffers(0, &[(&buffer, outline_rects_offset as _)])
                .draw(outline_rects_count as u32 * 6, 1, 0, 0)
                .bind_graphics_pipeline(&invert_pipeline)
                .bind_vertex_buffers(0, &[(&buffer, flip_fill_rect as _)])
                .draw(4, 1, 0, 0);
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
            invert_pipeline: peridot::LayoutedPipeline::combine(invert_pipeline, &empty_pl),
            outline_distance_pipeline: peridot::LayoutedPipeline::combine(
                outline_distance_pipeline, &empty_pl
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
