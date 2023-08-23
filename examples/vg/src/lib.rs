use log::*;
use peridot_command_object::{
    BeginRenderPass, BufferUsage, CopyBuffer, DescriptorPointer, EndRenderPass, GraphicsCommand,
    RangedBuffer, RenderBaseModel,
};
use std::convert::TryInto;
use std::marker::PhantomData;
extern crate bedrock as br;
use br::{
    traits::*, Buffer, CommandBuffer, DescriptorPool, Device, Image, ImageChild, SubmissionBatch,
};
use peridot::math::Vector2;
use peridot::mthelper::SharedRef;
use peridot::{
    BufferPrealloc, CBSubmissionType, CommandBundle, DescriptorSetUpdateBatch, LayoutedPipeline,
    MemoryBadget, ModelData, RenderPassTemplates, SpecConstantStorage,
};
use peridot_vertex_processing_pack::PvpShaderModules;
use peridot_vg as pvg;
use peridot_vg::{FlatPathBuilder, PathBuilder};
use std::borrow::Cow;

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
    framebuffers: Vec<
        br::FramebufferObject<
            peridot::DeviceObject,
            SharedRef<<PL::Presenter as peridot::PlatformPresenter>::BackBuffer>,
        >,
    >,
    render_cb: CommandBundle<peridot::DeviceObject>,
    buffer: SharedRef<
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
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
    vg_renderer_params: pvg::RendererParams,
    vg_renderer_params2: pvg::RendererParams,
    gp1: LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        SharedRef<br::PipelineLayoutObject<peridot::DeviceObject>>,
    >,
    gp2: LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        SharedRef<br::PipelineLayoutObject<peridot::DeviceObject>>,
    >,
    gp1_curve: LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        SharedRef<br::PipelineLayoutObject<peridot::DeviceObject>>,
    >,
    gp2_curve: LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        SharedRef<br::PipelineLayoutObject<peridot::DeviceObject>>,
    >,
    target_size: peridot::math::Vector2F32,
    ph: PhantomData<*const PL>,
}
impl<PL: peridot::NativeLinker> peridot::FeatureRequests for Game<PL> {}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &mut peridot::Engine<PL>) -> Self {
        let font_provider = pvg::FontProvider::new().expect("FontProvider initialization error");
        let font = font_provider
            .best_match("sans-serif", &pvg::FontProperties::default(), 12.0)
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
            let out_barrier = [all_buffer_out_barrier];

            copy.between(in_barrier, out_barrier)
                .submit(e)
                .expect("ImmResource Initialization");
        }

        let screen_size = e.back_buffer(0).expect("no backbuffer").image().size().wh();
        let render_pass = RenderPassTemplates::single_render(
            e.back_buffer_format(),
            e.requesting_back_buffer_layout().0,
        )
        .create(e.graphics().device().clone())
        .expect("RenderPass Creation");
        let framebuffers = e
            .iter_back_buffers()
            .map(|bb| {
                e.graphics().device().clone().new_framebuffer(
                    &render_pass,
                    vec![bb.clone()],
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
        gpb.multisample_state(Some(br::MultisampleState::new()));
        gpb.viewport_scissors(
            br::DynamicArrayState::Static(&vp),
            br::DynamicArrayState::Static(&sc),
        )
        .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
        .multisample_state(Some(br::MultisampleState::new()));
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

        let mut render_cb = CommandBundle::new(
            &e.graphics(),
            CBSubmissionType::Graphics,
            framebuffers.len(),
        )
        .expect("Creating RenderCB");
        for (r, f) in render_cb.iter_mut().zip(&framebuffers) {
            let vg_renderer_exinst = pvg::RendererExternalInstances {
                interior_pipeline: &gp,
                curve_pipeline: &gp_curve,
                transform_buffer_descriptor_set: descs[0],
                target_pixels: Vector2(screen_size.width as _, screen_size.height as _),
            };
            let vg_renderer_exinst2 = pvg::RendererExternalInstances {
                interior_pipeline: &gp2,
                curve_pipeline: &gp2_curve,
                transform_buffer_descriptor_set: descs[1],
                target_pixels: Vector2(screen_size.width as _, screen_size.height as _),
            };

            let rp = BeginRenderPass::for_entire_framebuffer(&renderpass, f)
                .with_clear_values(vec![br::ClearValue::color([1.0; 4])]);
            let render_vg = RenderBaseModel {
                provider: &vg_renderer_params,
                engine: e,
                buffer: &buffer,
                extras: vg_renderer_exinst,
            };
            let render_vg2 = RenderBaseModel {
                provider: &vg_renderer_params2,
                engine: e,
                buffer: &buffer,
                extras: vg_renderer_exinst2,
            };

            (render_vg2, render_vg)
                .between(rp, EndRenderPass)
                .execute_and_finish(unsafe {
                    r.begin().expect("Failed to begin render command recording")
                })
                .expect("Failed to finish render commands");
        }

        Game {
            ph: PhantomData,
            buffer,
            render_pass,
            framebuffers,
            _bufview: bufview,
            _bufview2: bufview2,
            descriptors: (dsl, dp, descs),
            render_cb,
            vg_renderer_params,
            vg_renderer_params2,
            gp1: gp,
            gp2,
            gp1_curve: gp_curve,
            gp2_curve,
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
        self.framebuffers = e
            .iter_back_buffers()
            .map(|bb| {
                e.graphics().device().clone().new_framebuffer(
                    &self.render_pass,
                    vec![bb.clone()],
                    bb.image().size().as_ref(),
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Bind Framebuffer");
        for (r, f) in self.render_cb.iter_mut().zip(&self.framebuffers) {
            let vg_renderer_exinst = pvg::RendererExternalInstances {
                interior_pipeline: &self.gp1,
                curve_pipeline: &self.gp1_curve,
                transform_buffer_descriptor_set: self.descriptors.2[0],
                target_pixels: self.target_size.clone(),
            };
            let vg_renderer_exinst2 = pvg::RendererExternalInstances {
                interior_pipeline: &self.gp2,
                curve_pipeline: &self.gp2_curve,
                transform_buffer_descriptor_set: self.descriptors.2[1],
                target_pixels: self.target_size.clone(),
            };

            let rp = BeginRenderPass::for_entire_framebuffer(&self.renderpass, f)
                .with_clear_values(vec![br::ClearValue::color([1.0; 4])]);
            let render_vg = RenderBaseModel {
                provider: &self.vg_renderer_params,
                engine: e,
                buffer: &self.buffer,
                extras: vg_renderer_exinst,
            };
            let render_vg2 = RenderBaseModel {
                provider: &self.vg_renderer_params2,
                engine: e,
                buffer: &self.buffer,
                extras: vg_renderer_exinst2,
            };

            (render_vg2, render_vg)
                .between(rp, EndRenderPass)
                .execute_and_finish(unsafe { r.begin().expect("Start Recording CB") })
                .expect("Failed to finish render commands");
        }
    }
}
