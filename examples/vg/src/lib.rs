use bedrock as br;
use br::{
    Buffer, CommandBuffer, DescriptorPool, Device, Image, ImageChild, ImageSubresourceSlice,
    SubmissionBatch,
};
use log::*;
use peridot::math::Vector2;
use peridot::mthelper::SharedRef;
use peridot::{
    BufferPrealloc, CBSubmissionType, CommandBundle, LayoutedPipeline, MemoryBadget, ModelData,
    SpecConstantStorage,
};
use peridot_command_object::{
    BeginRenderPass, BufferUsage, ColorAttachmentBlending, CopyBuffer, EndRenderPass,
    GraphicsCommand, GraphicsCommandCombiner, GraphicsCommandSubmission, PipelineBarrier,
    RangedBuffer, RangedImage,
};
use peridot_vertex_processing_pack::PvpShaderModules;
use peridot_vg as pvg;
use peridot_vg::{FlatPathBuilder, PathBuilder};
use pvg::{FontProvider, FontProviderConstruct, RenderVG};
use std::borrow::Cow;
use std::convert::TryInto;
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
    render_pass: br::RenderPassObject<peridot::DeviceObject>,
    framebuffers: Vec<br::FramebufferObject<peridot::DeviceObject>>,
    render_cb: CommandBundle<peridot::DeviceObject>,
    _bufview: br::BufferViewObject<
        SharedRef<
            peridot::Buffer<
                br::BufferObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
    >,
    _bufview2: br::BufferViewObject<
        SharedRef<
            peridot::Buffer<
                br::BufferObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
    >,
    descriptors: (
        br::DescriptorSetLayoutObject<peridot::DeviceObject>,
        br::DescriptorPoolObject<peridot::DeviceObject>,
        Vec<br::DescriptorSet>,
    ),
    render_vgs: [pvg::RenderVG<
        peridot::DeviceObject,
        SharedRef<
            peridot::Buffer<
                br::BufferObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
    >; 2],
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

        let buffer = bp.build_transferred().expect("Buffer Allocation");
        let stg_buffer = bp.build_upload().expect("StgBuffer Allocation");

        let mut mb = MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        let mut mb_stg =
            MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(buffer));
        let Ok::<[_; 1], _>([peridot::MemoryBoundResource::Buffer(buffer)]) =
            mb.alloc().expect("Mem Allocation").try_into() else {
                unreachable!("unexpected return combination");
            };
        let buffer = SharedRef::new(buffer);
        mb_stg.add(peridot::MemoryBadgetEntry::Buffer(stg_buffer));
        let Ok::<[_; 1], _>([peridot::MemoryBoundResource::Buffer(mut stg_buffer)]) =
            mb_stg.alloc_upload().expect("StgMem Allocation").try_into() else {
                unreachable!("unexpected return combination");
            };

        let rt_size = e
            .back_buffer(0)
            .expect("no back-buffers?")
            .image()
            .size()
            .wh();
        let msaa_count = br::vk::VK_SAMPLE_COUNT_4_BIT;
        let msaa_texture = br::ImageDesc::new(
            rt_size.clone(),
            e.back_buffer_format(),
            br::ImageUsage::COLOR_ATTACHMENT.transient_attachment(),
            br::ImageLayout::Undefined,
        )
        .sample_counts(msaa_count)
        .create(e.graphics_device().clone())
        .expect("Failed to create msaa render target");
        let mut image_mb =
            MemoryBadget::<br::BufferObject<peridot::DeviceObject>, _>::new(e.graphics());
        image_mb.add(peridot::MemoryBadgetEntry::Image(msaa_texture));
        let Ok::<[_; 1], _>([peridot::MemoryBoundResource::Image(msaa_texture)]) =
            image_mb.alloc().expect("Failed to allocate msaa texture memory").try_into() else {
                unreachable!("unexpected return set");
            };
        let msaa_texture = SharedRef::new(
            msaa_texture
                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create msaa render target view"),
        );

        let (vg_renderer_params, vg_renderer_params2) = stg_buffer
            .guard_map(0..bp.total_size(), |m| {
                let p0 = ctx.stage_data_into(m, vg_offs);
                let p1 = ctx2.stage_data_into(m, vg_offs2);
                return (p0, p1);
            })
            .expect("StgMem Initialization");

        let bufview = buffer
            .clone()
            .create_view(
                br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                vg_renderer_params.transforms_byterange(),
            )
            .expect("Creating Transform BufferView");
        let bufview2 = buffer
            .clone()
            .create_view(
                br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                vg_renderer_params2.transforms_byterange(),
            )
            .expect("Creating Transform BufferView 2");

        let transfer_total_size = bp.total_size();
        {
            let all_stg_buffer =
                RangedBuffer::from_offset_length(&stg_buffer, 0, transfer_total_size as _);
            let all_buffer =
                RangedBuffer::from_offset_length(&*buffer, 0, transfer_total_size as _);
            let msaa_texture = RangedImage::single_color_plane(msaa_texture.image());

            let copy =
                CopyBuffer::new(&stg_buffer, &*buffer).with_mirroring(0, transfer_total_size as _);

            let [all_buffer_in_barrier, all_buffer_out_barrier] = all_buffer.usage_barrier3(
                BufferUsage::UNUSED,
                BufferUsage::TRANSFER_DST,
                BufferUsage::VERTEX_BUFFER
                    | BufferUsage::INDEX_BUFFER
                    | BufferUsage::VERTEX_STORAGE_RO,
            );
            let in_barrier = [
                all_stg_buffer.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                all_buffer_in_barrier,
            ];
            let out_barrier = PipelineBarrier::new()
                .with_barrier(all_buffer_out_barrier)
                .with_barrier(msaa_texture.barrier(
                    br::ImageLayout::Undefined,
                    br::ImageLayout::ColorAttachmentOpt,
                ));

            copy.between(in_barrier, out_barrier)
                .submit(e)
                .expect("ImmResource Initialization");
        }

        let (rt_ext_layout, _) = e.requesting_back_buffer_layout();
        let rt_attachment =
            br::AttachmentDescription::new(e.back_buffer_format(), rt_ext_layout, rt_ext_layout)
                .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store);
        let msaa_rt_attachment = br::AttachmentDescription::new(
            e.back_buffer_format(),
            br::ImageLayout::ColorAttachmentOpt,
            br::ImageLayout::ColorAttachmentOpt,
        )
        .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
        .samples(msaa_count);
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
            .add_attachments(vec![rt_attachment, msaa_rt_attachment])
            .add_subpass(color_subpass)
            .add_dependencies(vec![color_subpass_enter_dep, color_subpass_leave_dep])
            .create(e.graphics_device().clone())
            .expect("Failed to create render pass");

        let screen_size = e.back_buffer(0).expect("no backbuffer").image().size().wh();
        // let render_pass = RenderPassTemplates::single_render(
        //     e.back_buffer_format(),
        //     e.requesting_back_buffer_layout().0,
        // )
        // .create(e.graphics().device().clone())
        // .expect("RenderPass Creation");
        let framebuffers = e
            .iter_back_buffers()
            .map(|bb| {
                br::FramebufferBuilder::new(&render_pass)
                    .with_attachment(bb.clone())
                    .with_attachment(msaa_texture.clone())
                    .create()
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
            buffer: buffer.clone(),
            interior_pipeline: gp,
            curve_pipeline: gp_curve,
            transform_buffer_descriptor_set: descs[0],
            target_pixels: Vector2(screen_size.width as _, screen_size.height as _),
            rendering_precision: e.rendering_precision(),
        };
        let render_vg2 = RenderVG {
            params: vg_renderer_params2,
            buffer,
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
            render_pass,
            framebuffers,
            _bufview: bufview,
            _bufview2: bufview2,
            descriptors: (dsl, dp, descs),
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
        let msaa_texture = br::ImageDesc::new(
            rt_size.clone(),
            e.back_buffer_format(),
            br::ImageUsage::COLOR_ATTACHMENT.transient_attachment(),
            br::ImageLayout::Undefined,
        )
        .sample_counts(msaa_count)
        .create(e.graphics_device().clone())
        .expect("Failed to create msaa render target");
        let mut image_mb =
            MemoryBadget::<br::BufferObject<peridot::DeviceObject>, _>::new(e.graphics());
        image_mb.add(peridot::MemoryBadgetEntry::Image(msaa_texture));
        let Ok::<[_; 1], _>([peridot::MemoryBoundResource::Image(msaa_texture)]) =
            image_mb.alloc().expect("Failed to allocate msaa texture memory").try_into() else {
                unreachable!("unexpected return set");
            };
        let msaa_texture = SharedRef::new(
            msaa_texture
                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create msaa render target view"),
        );

        e.submit_commands(|mut rec| {
            let _ = rec.pipeline_barrier(
                br::PipelineStageFlags::BOTTOM_OF_PIPE,
                br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                true,
                &[],
                &[],
                &[msaa_texture
                    .image()
                    .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                    .memory_barrier(
                        br::ImageLayout::Undefined,
                        br::ImageLayout::ColorAttachmentOpt,
                    )],
            );

            rec
        })
        .expect("Failed to initialize msaa rt");

        self.framebuffers = e
            .iter_back_buffers()
            .map(|bb| {
                br::FramebufferBuilder::new(&self.render_pass)
                    .with_attachment(bb.clone())
                    .with_attachment(msaa_texture.clone())
                    .create()
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
