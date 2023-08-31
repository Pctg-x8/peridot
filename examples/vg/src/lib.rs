use bedrock as br;
use br::{
    Buffer, CommandBuffer, DescriptorPool, Device, Image, ImageChild, ImageSubresourceSlice,
    SubmissionBatch,
};
use log::*;
use peridot::math::Vector2;
use peridot::mthelper::SharedRef;
use peridot::{
    BufferPrealloc, CBSubmissionType, CommandBundle, LayoutedPipeline, ModelData,
    SpecConstantStorage,
};
use peridot_command_object::{
    BeginRenderPass, BufferUsage, ColorAttachmentBlending, EndRenderPass, GraphicsCommand,
    GraphicsCommandCombiner, GraphicsCommandSubmission, PipelineBarrier, RangedBuffer, RangedImage,
};
use peridot_memory_manager::MemoryManager;
use peridot_vertex_processing_pack::PvpShaderModules;
use peridot_vg as pvg;
use peridot_vg::{FlatPathBuilder, PathBuilder};
use pvg::{FontProvider, FontProviderConstruct, RenderVG};
use std::borrow::Cow;
use std::marker::PhantomData;

#[derive(SpecConstantStorage)]
#[repr(C)]
pub struct VgRendererFragmentFixedColor {
    r: f32,
    g: f32,
    b: f32,
    a: f32,
}

pub struct Game<PL: peridot::NativeLinker> {
    memory_manager: MemoryManager,
    render_pass: br::RenderPassObject<peridot::DeviceObject>,
    framebuffers: Vec<
        br::FramebufferObject<
            peridot::DeviceObject,
            SharedRef<dyn br::ImageView<ConcreteDevice = peridot::DeviceObject>>,
        >,
    >,
    render_cb: CommandBundle<peridot::DeviceObject>,
    _bufview: br::BufferViewObject<SharedRef<peridot_memory_manager::Buffer>>,
    _bufview2: br::BufferViewObject<SharedRef<peridot_memory_manager::Buffer>>,
    _descriptors: (
        br::DescriptorSetLayoutObject<peridot::DeviceObject>,
        br::DescriptorPoolObject<peridot::DeviceObject>,
        Vec<br::DescriptorSet>,
    ),
    render_vgs:
        [pvg::RenderVG<peridot::DeviceObject, SharedRef<peridot_memory_manager::Buffer>>; 2],
    target_size: peridot::math::Vector2F32,
    ph: PhantomData<*const PL>,
}
impl<PL: peridot::NativeLinker> peridot::FeatureRequests for Game<PL> {}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &mut peridot::Engine<PL>) -> Self {
        let font_provider =
            pvg::DefaultFontProvider::new().expect("FontProvider initialization error");
        let font = font_provider
            .best_match("sans-serif", &pvg::FontProperties::default(), 18.0)
            .expect("No Fonts");
        let mut ctx = pvg::Context::new(1.0);
        ctx.text(&font, "Hello, World!|Opaque")
            .expect("Text Rendering failed");
        {
            let mut f0 = ctx.begin_figure(pvg::FillRule::Winding);
            f0.move_to(Vector2(10.0, -10.0).into());
            f0.quadratic_bezier_to(Vector2(100.0, -35.0).into(), Vector2(100.0, -100.0).into());
            f0.end();
        }
        /*{
            let mut f = ctx.begin_figure(pvg::FillRule::Winding);
            f.move_to(Vector2(200.0, -200.0 - 10.0).into());
            f.line_to(Vector2(200.0, -200.0 - 90.0).into());
            f.quadratic_bezier_to(Vector2(200.0, -300.0).into(), Vector2(210.0, -300.0).into());
            f.line_to(Vector2(340.0, -300.0).into());
            f.quadratic_bezier_to(Vector2(350.0, -300.0).into(), Vector2(350.0, -290.0).into());
            f.line_to(Vector2(350.0, -210.0).into());
            f.quadratic_bezier_to(Vector2(350.0, -200.0).into(), Vector2(340.0, -200.0).into());
            f.line_to(Vector2(210.0, -200.0).into());
            f.quadratic_bezier_to(Vector2(200.0, -200.0).into(), Vector2(200.0, -210.0).into());
            f.close(); f.end();
        }*/
        let mut ctx2 = pvg::Context::new(1.0);
        /*{
            let mut f0 = ctx2.begin_figure(pvg::FillRule::Winding);
            f0.move_to(Vector2(10.0, -10.0).into());
            /*f0.cubic_bezier_to(Vector2(100.0, -35.0).into(), Vector2(35.0, -80.0).into(),
                Vector2(100.0, -100.0).into());*/
            f0.quadratic_bezier_to(Vector2(100.0, -30.0).into(), Vector2(30.0, -100.0).into());
            // f0.quadratic_bezier_to(Vector2(200.0, -100.0).into(), Vector2(80.0, -60.0).into());
            // f0.stroke_outline(20.0);
            // f0.close();
            f0.end();
        }*/
        /*{
            let mut sp = pvg::StrokePathBuilder::new(1.0);
            sp.move_to(Vector2(200.0, -200.0 - 10.0).into());
            sp.line_to(Vector2(200.0, -200.0 - 90.0).into());
            sp.quadratic_bezier_to(Vector2(200.0, -300.0).into(), Vector2(210.0, -300.0).into());
            sp.line_to(Vector2(340.0, -300.0).into());
            sp.quadratic_bezier_to(Vector2(350.0, -300.0).into(), Vector2(350.0, -290.0).into());
            sp.line_to(Vector2(350.0, -210.0).into());
            sp.quadratic_bezier_to(Vector2(350.0, -200.0).into(), Vector2(340.0, -200.0).into());
            sp.line_to(Vector2(210.0, -200.0).into());
            sp.quadratic_bezier_to(Vector2(200.0, -200.0).into(), Vector2(200.0, -210.0).into());
            sp.close();
            let mut f = ctx2.begin_figure(vg::FillRule::EvenOdd);
            sp.sink_widened(&mut f);
            f.end();
        }*/
        {
            let mut f = ctx2.begin_figure(pvg::FillRule::Winding);
            f.move_to(Vector2(200.0, -200.0 - 10.0).into());
            f.line_to(Vector2(200.0, -200.0 - 90.0).into());
            f.quadratic_bezier_to(Vector2(200.0, -300.0).into(), Vector2(210.0, -300.0).into());
            f.line_to(Vector2(340.0, -300.0).into());
            f.quadratic_bezier_to(Vector2(350.0, -300.0).into(), Vector2(350.0, -290.0).into());
            f.line_to(Vector2(350.0, -210.0).into());
            f.quadratic_bezier_to(Vector2(350.0, -200.0).into(), Vector2(340.0, -200.0).into());
            f.line_to(Vector2(210.0, -200.0).into());
            f.quadratic_bezier_to(Vector2(200.0, -200.0).into(), Vector2(200.0, -210.0).into());
            f.close();
            f.end();
        }

        let mut bp = BufferPrealloc::new(&e.graphics());
        let vg_offs = ctx.prealloc(&mut bp);
        let vg_offs2 = ctx.prealloc(&mut bp);

        let mut memory_manager = MemoryManager::new(e.graphics());
        let buffer = memory_manager
            .allocate_device_local_buffer(
                e.graphics(),
                bp.build_desc().and_usage(br::BufferUsage::TRANSFER_DEST),
            )
            .expect("Buffer Allocation");
        let buf_length = buffer.byte_length();
        let buffer = RangedBuffer::from_offset_length(SharedRef::new(buffer), 0, buf_length);
        let mut stg_buffer: RangedBuffer<_> = memory_manager
            .allocate_upload_buffer(
                e.graphics(),
                bp.build_desc_custom_usage(br::BufferUsage::TRANSFER_SRC),
            )
            .expect("StgBuffer Allocation")
            .into();

        let rt_size = e
            .back_buffer(0)
            .expect("no back-buffers?")
            .image()
            .size()
            .wh();
        let msaa_count = br::vk::VK_SAMPLE_COUNT_4_BIT;
        let msaa_texture = memory_manager
            .allocate_device_local_image(
                e.graphics(),
                br::ImageDesc::new(
                    rt_size.clone(),
                    e.back_buffer_format(),
                    br::ImageUsage::COLOR_ATTACHMENT.transient_attachment(),
                    br::ImageLayout::Undefined,
                )
                .sample_counts(msaa_count),
            )
            .expect("Failed to create msaa render target");
        let msaa_texture = SharedRef::new(
            msaa_texture
                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create msaa render target view"),
        );

        let (vg_renderer_params, vg_renderer_params2) = stg_buffer
            .0
            .guard_map(|m| unsafe {
                let p0 = ctx.write_data_into(m.ptr(), vg_offs);
                let p1 = ctx2.write_data_into(m.ptr(), vg_offs2);
                return (p0, p1);
            })
            .expect("StgMem Initialization");

        let bufview = buffer
            .0
            .clone()
            .create_view(
                br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                vg_renderer_params.transforms_byterange(),
            )
            .expect("Creating Transform BufferView");
        let bufview2 = buffer
            .0
            .clone()
            .create_view(
                br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                vg_renderer_params2.transforms_byterange(),
            )
            .expect("Creating Transform BufferView 2");

        {
            let copy = buffer
                .subslice_ref(0..bp.total_size())
                .mirror_from(stg_buffer.subslice_ref(0..bp.total_size()));

            let [all_buffer_in_barrier, all_buffer_out_barrier] = buffer.make_ref().usage_barrier3(
                BufferUsage::UNUSED,
                BufferUsage::TRANSFER_DST,
                BufferUsage::VERTEX_BUFFER
                    | BufferUsage::INDEX_BUFFER
                    | BufferUsage::VERTEX_STORAGE_RO,
            );
            let in_barrier = PipelineBarrier::new()
                .with_barrier(
                    stg_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                )
                .with_barrier(all_buffer_in_barrier);
            let out_barrier = PipelineBarrier::new()
                .with_barrier(all_buffer_out_barrier)
                .with_barrier(
                    RangedImage::single_color_plane(msaa_texture.image()).barrier(
                        br::ImageLayout::Undefined,
                        br::ImageLayout::ColorAttachmentOpt,
                    ),
                );

            copy.between(in_barrier, out_barrier)
                .submit(e)
                .expect("ImmResource Initialization");
        }

        let attachments = [
            e.back_buffer_attachment_desc()
                .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store),
            br::AttachmentDescription::new(
                e.back_buffer_format(),
                br::ImageLayout::ColorAttachmentOpt,
                br::ImageLayout::ColorAttachmentOpt,
            )
            .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
            .samples(msaa_count),
        ];
        let color_subpass = br::SubpassDescription::new().add_color_output(
            1,
            br::ImageLayout::ColorAttachmentOpt,
            Some((0, br::ImageLayout::ColorAttachmentOpt)),
        );
        let color_subpass_enter_dep = br::vk::VkSubpassDependency {
            srcSubpass: br::vk::VK_SUBPASS_EXTERNAL,
            dstSubpass: 0,
            srcStageMask: br::PipelineStageFlags::BOTTOM_OF_PIPE.0,
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
            srcAccessMask: br::AccessFlags::MEMORY.read,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
            dependencyFlags: 0,
        };
        let color_subpass_leave_dep = br::vk::VkSubpassDependency {
            srcSubpass: 0,
            dstSubpass: br::vk::VK_SUBPASS_EXTERNAL,
            srcStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
            dstStageMask: br::PipelineStageFlags::TOP_OF_PIPE.0,
            srcAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
            dstAccessMask: br::AccessFlags::MEMORY.read,
            dependencyFlags: 0,
        };
        let render_pass = br::RenderPassBuilder::new()
            .add_attachments(attachments)
            .add_subpass(color_subpass)
            .add_dependencies(vec![color_subpass_enter_dep, color_subpass_leave_dep])
            .create(e.graphics_device().clone())
            .expect("Failed to create render pass");

        let screen_size = e.back_buffer(0).expect("no backbuffer").image().size().wh();
        let framebuffers = e
            .iter_back_buffers()
            .map(|bb| {
                e.graphics().device().clone().new_framebuffer(
                    &render_pass,
                    vec![
                        bb.clone()
                            as SharedRef<dyn br::ImageView<ConcreteDevice = peridot::DeviceObject>>,
                        msaa_texture.clone(),
                    ],
                    screen_size.as_ref(),
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Framebuffer Creation");

        let dsl = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::UniformTexelBuffer
                .make_binding(1)
                .only_for_vertex(),
        ])
        .create(e.graphics().device().clone())
        .expect("DescriptorSetLayout Creation");
        let mut dp = br::DescriptorPoolBuilder::new(2)
            .with_reservations(vec![br::DescriptorType::UniformTexelBuffer.with_count(2)])
            .create(e.graphics().device().clone())
            .expect("DescriptorPool Creation");
        let descs = dp.alloc(&[&dsl, &dsl]).expect("DescriptorSet Allocation");

        e.graphics().device().update_descriptor_sets(
            &[
                br::DescriptorPointer::new(descs[0].into(), 0).write(
                    br::DescriptorContents::UniformTexelBuffer(vec![br::VkHandleRef::new(
                        &bufview,
                    )]),
                ),
                br::DescriptorPointer::new(descs[1].into(), 0).write(
                    br::DescriptorContents::UniformTexelBuffer(vec![br::VkHandleRef::new(
                        &bufview2,
                    )]),
                ),
            ],
            &[],
        );

        let shader = PvpShaderModules::new(
            e.graphics().device(),
            e.load("shaders.interiorColorFixed")
                .expect("Loading PvpContainer"),
        )
        .expect("Creating Shader");
        let curve_shader = PvpShaderModules::new(
            e.graphics().device(),
            e.load("shaders.curveColorFixed")
                .expect("Loading CurveShader"),
        )
        .expect("Creating CurveShader");
        debug!("ScreenSize: {screen_size:?}");
        let sc = [screen_size.clone().into_rect(br::vk::VkOffset2D::ZERO)];
        let vp = [sc[0].make_viewport(0.0..1.0)];
        let pl = SharedRef::new(
            br::PipelineLayoutBuilder::new(vec![&dsl], vec![(br::ShaderStage::VERTEX, 0..4 * 4)])
                .create(e.graphics().device().clone())
                .expect("Create PipelineLayout"),
        );
        let spc_map = &[
            br::vk::VkSpecializationMapEntry {
                constantID: 0,
                offset: 0,
                size: 4,
            },
            br::vk::VkSpecializationMapEntry {
                constantID: 1,
                offset: 4,
                size: 4,
            },
        ];
        let mut interior_vertex_processing =
            shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        let mut curve_vertex_processing =
            curve_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        interior_vertex_processing.vertex_shader_mut().specinfo = Some((
            Cow::Borrowed(spc_map),
            br::DynamicDataCell::from_slice(&pvg::renderer_pivot::LEFT_TOP),
        ));
        curve_vertex_processing.vertex_shader_mut().specinfo = Some((
            Cow::Borrowed(spc_map),
            br::DynamicDataCell::from_slice(&pvg::renderer_pivot::LEFT_TOP),
        ));

        interior_vertex_processing
            .fragment_shader_mut()
            .expect("fragment shader not exist?")
            .specinfo = Some(
            VgRendererFragmentFixedColor {
                r: 1.0,
                g: 0.5,
                b: 0.0,
                a: 1.0,
            }
            .as_pair(),
        );
        curve_vertex_processing
            .fragment_shader_mut()
            .expect("fragment shader not exist?")
            .specinfo = Some(
            VgRendererFragmentFixedColor {
                r: 1.0,
                g: 0.5,
                b: 0.0,
                a: 1.0,
            }
            .as_pair(),
        );
        let mut gpb = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(&pl, (&render_pass, 0), interior_vertex_processing);
        gpb.multisample_state(Some({
            let mut state = br::MultisampleState::new();
            state.rasterization_samples(msaa_count as _);

            state
        }))
        .viewport_scissors(
            br::DynamicArrayState::Static(&vp),
            br::DynamicArrayState::Static(&sc),
        )
        .set_attachment_blends(vec![ColorAttachmentBlending::PREMULTIPLIED_ALPHA.into_vk()]);
        let gp = LayoutedPipeline::combine(
            gpb.create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Create GraphicsPipeline"),
            pl.clone(),
        );
        gpb.vertex_processing_mut()
            .fragment_shader_mut()
            .expect("Fragment shader not exist?")
            .specinfo = Some(
            VgRendererFragmentFixedColor {
                r: 0.0,
                g: 0.5,
                b: 1.0,
                a: 1.0,
            }
            .as_pair(),
        );
        let gp2 = LayoutedPipeline::combine(
            gpb.create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Creating GraphicsPipeline2"),
            pl.clone(),
        );
        gpb.vertex_processing(curve_vertex_processing);
        let gp_curve = LayoutedPipeline::combine(
            gpb.create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Create GraphicsPipeline of CurveRender"),
            pl.clone(),
        );
        gpb.vertex_processing_mut()
            .fragment_shader_mut()
            .expect("fragment shader not exist?")
            .specinfo = Some(
            VgRendererFragmentFixedColor {
                r: 0.0,
                g: 0.5,
                b: 1.0,
                a: 1.0,
            }
            .as_pair(),
        );
        let gp2_curve = LayoutedPipeline::combine(
            gpb.create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Creating GraphicsPipeline2 for CurveRender"),
            pl.clone(),
        );

        let render_vg = RenderVG {
            params: vg_renderer_params,
            buffer: buffer.0.clone(),
            interior_pipeline: gp,
            curve_pipeline: gp_curve,
            transform_buffer_descriptor_set: descs[0],
            target_pixels: Vector2(screen_size.width as _, screen_size.height as _),
            rendering_precision: e.rendering_precision(),
        };
        let render_vg2 = RenderVG {
            params: vg_renderer_params2,
            buffer: buffer.0,
            interior_pipeline: gp2,
            curve_pipeline: gp2_curve,
            transform_buffer_descriptor_set: descs[1],
            target_pixels: Vector2(screen_size.width as _, screen_size.height as _),

            rendering_precision: e.rendering_precision(),
        };
        let color_renders = [render_vg2, render_vg];

        let mut render_cb = CommandBundle::new(
            &e.graphics(),
            CBSubmissionType::Graphics,
            framebuffers.len(),
        )
        .expect("Creating RenderCB");
        for (r, f) in render_cb.iter_mut().zip(&framebuffers) {
            let rp =
                BeginRenderPass::for_entire_framebuffer(&render_pass, f).with_clear_values(vec![
                    br::ClearValue::color([1.0; 4]),
                    br::ClearValue::color([1.0; 4]),
                ]);

            (&color_renders)
                .between(rp, EndRenderPass)
                .execute_and_finish(unsafe {
                    r.begin()
                        .expect("Failed to begin render command recording")
                        .as_dyn_ref()
                })
                .expect("Failed to finish render commands");
        }

        Self {
            ph: PhantomData,
            memory_manager,
            render_pass,
            framebuffers,
            _bufview: bufview,
            _bufview2: bufview2,
            _descriptors: (dsl, dp, descs),
            render_cb,
            render_vgs: color_renders,
            target_size: peridot::math::Vector2(screen_size.width as _, screen_size.height as _),
        }
    }

    fn update(
        &mut self,
        e: &mut peridot::Engine<PL>,
        on_backbuffer_of: u32,
        _dt: std::time::Duration,
    ) {
        e.do_render(
            on_backbuffer_of,
            None::<br::EmptySubmissionBatch>,
            br::EmptySubmissionBatch.with_command_buffers(
                &self.render_cb[on_backbuffer_of as usize..=on_backbuffer_of as usize],
            ),
        )
        .expect("Failed to present");
    }

    fn discard_back_buffer_resources(&mut self) {
        self.render_cb.reset().expect("Resetting RenderCB");
        self.framebuffers.clear();
    }
    fn on_resize(&mut self, e: &mut peridot::Engine<PL>, _new_size: Vector2<usize>) {
        let rt_size = e
            .back_buffer(0)
            .expect("no back-buffers?")
            .image()
            .size()
            .wh();
        let msaa_count = br::vk::VK_SAMPLE_COUNT_4_BIT;
        let msaa_texture = self
            .memory_manager
            .allocate_device_local_image(
                e.graphics(),
                br::ImageDesc::new(
                    rt_size.clone(),
                    e.back_buffer_format(),
                    br::ImageUsage::COLOR_ATTACHMENT.transient_attachment(),
                    br::ImageLayout::Undefined,
                )
                .sample_counts(msaa_count),
            )
            .expect("Failed to create msaa render target");
        let msaa_texture = SharedRef::new(
            msaa_texture
                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create msaa render target view"),
        );

        PipelineBarrier::from(
            RangedImage::single_color_plane(msaa_texture.image()).barrier(
                br::ImageLayout::Undefined,
                br::ImageLayout::ColorAttachmentOpt,
            ),
        )
        .submit(e)
        .expect("Failed to initialize msaa rt");

        self.framebuffers = e
            .iter_back_buffers()
            .map(|bb| {
                e.graphics().device().clone().new_framebuffer(
                    &self.render_pass,
                    vec![
                        bb.clone()
                            as SharedRef<dyn br::ImageView<ConcreteDevice = peridot::DeviceObject>>,
                        msaa_texture.clone(),
                    ],
                    bb.image().size().as_ref(),
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Bind Framebuffer");

        for r in self.render_vgs.iter_mut() {
            r.set_target_pixels(self.target_size.clone());
        }

        for (r, f) in self.render_cb.iter_mut().zip(&self.framebuffers) {
            let rp = BeginRenderPass::for_entire_framebuffer(&self.render_pass, f)
                .with_clear_values(vec![
                    br::ClearValue::color([1.0; 4]),
                    br::ClearValue::color([1.0; 4]),
                ]);

            (&self.render_vgs)
                .between(rp, EndRenderPass)
                .execute_and_finish(unsafe { r.begin().expect("Start Recording CB").as_dyn_ref() })
                .expect("Failed to finish render commands");
        }
    }
}
