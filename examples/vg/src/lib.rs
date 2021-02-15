
use std::marker::PhantomData;
use log::*;
use peridot_derive::SpecConstantStorage;
extern crate bedrock as br; use br::traits::*;
use peridot::{CommandBundle, LayoutedPipeline, Buffer, BufferPrealloc, MemoryBadget, ModelData,
    TransferBatch, DescriptorSetUpdateBatch, CBSubmissionType, RenderPassTemplates, DefaultRenderCommands,
    SpecConstantStorage};
use peridot_vertex_processing_pack::PvpShaderModules;
use peridot::math::Vector2;
use std::rc::Rc;
use std::borrow::Cow;
use peridot_vg::{PathBuilder, FlatPathBuilder};
use peridot_vg as pvg;

#[derive(SpecConstantStorage)] #[repr(C)]
pub struct VgRendererFragmentFixedColor
{
    r: f32, g: f32, b: f32, a: f32
}

pub struct Game<PL: peridot::NativeLinker>
{
    renderpass: br::RenderPass, framebuffers: Vec<br::Framebuffer>, render_cb: CommandBundle, buffer: Buffer,
    _bufview: br::BufferView, _bufview2: br::BufferView,
    _descriptors: (br::DescriptorSetLayout, br::DescriptorPool, Vec<br::vk::VkDescriptorSet>),
    vg_renderer_params: pvg::RendererParams,
    vg_renderer_params2: pvg::RendererParams,
    vg_renderer_exinst: pvg::RendererExternalInstances,
    vg_renderer_exinst2: pvg::RendererExternalInstances,
    ph: PhantomData<*const PL>
}
impl<PL: peridot::NativeLinker> Game<PL>
{
    pub const NAME: &'static str = "Peridot Examples - VectorGraphics";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::NativeLinker> peridot::FeatureRequests for Game<PL> {}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL>
{
    fn init(e: &mut peridot::Engine<PL>) -> Self
    {
        let font_provider = pvg::FontProvider::new().expect("FontProvider initialization error");
        let font = font_provider.best_match("sans-serif", &pvg::FontProperties::default(), 12.0)
            .expect("No Fonts");
        let mut ctx = pvg::Context::new(2.0);
        ctx.text(&font, "Hello, World!|Opaque").expect("Text Rendering failed");
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
            f.close(); f.end();
        }

        let mut bp = BufferPrealloc::new(&e.graphics());
        let vg_offs = ctx.prealloc(&mut bp);
        let vg_offs2 = ctx2.prealloc(&mut bp);

        let buffer = bp.build_transferred().expect("Buffer Allocation");
        let stg_buffer = bp.build_upload().expect("StgBuffer Allocation");
        
        let mut mb = MemoryBadget::new(e.graphics());
        let mut mb_stg = MemoryBadget::new(e.graphics());
        mb.add(buffer);
        mb_stg.add(stg_buffer);
        let buffer = mb.alloc().expect("Mem Allocation").pop().expect("no objects?").unwrap_buffer();
        let stg_buffer = mb_stg.alloc_upload().expect("StgMem Allocation").pop().expect("no objects?").unwrap_buffer();
        
        let (vg_renderer_params, vg_renderer_params2) = stg_buffer.guard_map(0 .. bp.total_size(), |m|
        {
            let p0 = ctx.stage_data_into(m, vg_offs);
            let p1 = ctx2.stage_data_into(m, vg_offs2);
            return (p0, p1);
        }).expect("StgMem Initialization");

        let bufview = buffer.create_view(br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            vg_renderer_params.transforms_byterange()).expect("Creating Transform BufferView");
        let bufview2 = buffer.create_view(br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            vg_renderer_params2.transforms_byterange()).expect("Creating Transform BufferView 2");

        e.submit_commands(|rec|
        {
            let mut tfb = TransferBatch::new();
            tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, bp.total_size() as _);
            tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_SHADER.vertex_input(), &buffer,
                0 .. bp.total_size() as _,
                br::AccessFlags::SHADER.read | br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::INDEX_READ);
            tfb.sink_transfer_commands(rec);
            tfb.sink_graphics_ready_commands(rec);
        }).expect("ImmResource Initialization");

        let screen_size = e.backbuffer(0).expect("no backbuffer").size().clone();
        let renderpass = RenderPassTemplates::single_render(e.backbuffer_format(), e.requesting_backbuffer_layout().0)
            .create(&e.graphics()).expect("RenderPass Creation");
        let framebuffers = (0..e.backbuffer_count())
            .map(|bb_index| {
                let bb = e.backbuffer(bb_index).expect("no backbuffer");
                br::Framebuffer::new(&renderpass, &[&bb], &screen_size, 1)
            })
            .collect::<Result<Vec<_>, _>>().expect("Framebuffer Creation");
        
        let dsl = br::DescriptorSetLayout::new(&e.graphics(), &[
            br::DescriptorSetLayoutBinding::UniformTexelBuffer(1, br::ShaderStage::VERTEX)
        ]).expect("DescriptorSetLayout Creation");
        let dp = br::DescriptorPool::new(&e.graphics(), 2,
            &[br::DescriptorPoolSize(br::DescriptorType::UniformTexelBuffer, 2)],
            false
        ).expect("DescriptorPool Creation");
        let descs = dp.alloc(&[&dsl, &dsl]).expect("DescriptorSet Allocation");

        let mut dub = DescriptorSetUpdateBatch::new();
        dub.write(descs[0], 0, br::DescriptorUpdateInfo::UniformTexelBuffer(vec![bufview.native_ptr()]));
        dub.write(descs[1], 0, br::DescriptorUpdateInfo::UniformTexelBuffer(vec![bufview2.native_ptr()]));
        dub.submit(&e.graphics());

        let shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.interiorColorFixed")
            .expect("Loading PvpContainer")).expect("Creating Shader");
        let curve_shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.curveColorFixed")
            .expect("Loading CurveShader")).expect("Creating CurveShader");
        let vp = [br::vk::VkViewport
        {
            width: screen_size.0 as _, height: screen_size.1 as _, x: 0.0, y: 0.0,
            minDepth: 0.0, maxDepth: 1.0
        }];
        let sc = [br::vk::VkRect2D
        {
            offset: br::vk::VkOffset2D::default(),
            extent: br::vk::VkExtent2D { width: screen_size.0, height: screen_size.1 }
        }];
        debug!("ScreenSize: {:?}", screen_size);
        let pl: Rc<_> = br::PipelineLayout::new(&e.graphics(), &[&dsl], &[(br::ShaderStage::VERTEX, 0 .. 4 * 4)])
            .expect("Create PipelineLayout").into();
        let spc_map = &[
            br::vk::VkSpecializationMapEntry { constantID: 0, offset: 0, size: 4 },
            br::vk::VkSpecializationMapEntry { constantID: 1, offset: 4, size: 4 }
        ];
        let mut interior_vertex_processing = shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        let mut curve_vertex_processing = curve_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        interior_vertex_processing.vertex_shader_mut().specinfo =
            Some((Cow::Borrowed(spc_map), br::DynamicDataCell::from_slice(&pvg::renderer_pivot::LEFT_TOP)));
        curve_vertex_processing.vertex_shader_mut().specinfo =
            Some((Cow::Borrowed(spc_map), br::DynamicDataCell::from_slice(&pvg::renderer_pivot::LEFT_TOP)));
        
        interior_vertex_processing.fragment_shader_mut().expect("fragment shader not exist?").specinfo =
            Some(VgRendererFragmentFixedColor { r: 1.0, g: 0.5, b: 0.0, a: 1.0 }.as_pair());
        curve_vertex_processing.fragment_shader_mut().expect("fragment shader not exist?").specinfo =
            Some(VgRendererFragmentFixedColor { r: 1.0, g: 0.5, b: 0.0, a: 1.0 }.as_pair());
        let mut gpb = br::GraphicsPipelineBuilder::new(&pl, (&renderpass, 0), interior_vertex_processing);
        gpb.multisample_state(Some(br::MultisampleState::new()));
        gpb.viewport_scissors(br::DynamicArrayState::Static(&vp), br::DynamicArrayState::Static(&sc))
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .multisample_state(Some(br::MultisampleState::new()));
        let gp = LayoutedPipeline::combine(gpb.create(&e.graphics(), None).expect("Create GraphicsPipeline"), &pl);
        gpb.vertex_processing_mut().fragment_shader_mut().expect("Fragment shader not exist?").specinfo =
            Some(VgRendererFragmentFixedColor { r: 0.0, g: 0.5, b: 1.0, a: 1.0 }.as_pair());
        let gp2 = LayoutedPipeline::combine(gpb.create(&e.graphics(), None).expect("Creating GraphicsPipeline2"), &pl);
        gpb.vertex_processing(curve_vertex_processing);
        let gp_curve = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline of CurveRender"), &pl);
        gpb.vertex_processing_mut().fragment_shader_mut().expect("fragment shader not exist?").specinfo =
            Some(VgRendererFragmentFixedColor { r: 0.0, g: 0.5, b: 1.0, a: 1.0 }.as_pair());
        let gp2_curve = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Creating GraphicsPipeline2 for CurveRender"), &pl);
        let vg_renderer_exinst = pvg::RendererExternalInstances
        {
            interior_pipeline: gp, curve_pipeline: gp_curve, transform_buffer_descriptor_set: descs[0],
            target_pixels: Vector2(screen_size.0 as _, screen_size.1 as _)
        };
        let vg_renderer_exinst2 = pvg::RendererExternalInstances
        {
            interior_pipeline: gp2, curve_pipeline: gp2_curve, transform_buffer_descriptor_set: descs[1],
            target_pixels: Vector2(screen_size.0 as _, screen_size.1 as _)
        };

        let render_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, framebuffers.len())
            .expect("Creating RenderCB");
        for (r, f) in render_cb.iter().zip(&framebuffers)
        {
            let mut cbr = r.begin().expect("Start Recoding CB");
            cbr.begin_render_pass(&renderpass, f, f.size().clone().into(), &[br::ClearValue::Color([1.0; 4])], true);
            vg_renderer_params2.default_render_commands(e, &mut cbr, &buffer, &vg_renderer_exinst2);
            vg_renderer_params.default_render_commands(e, &mut cbr, &buffer, &vg_renderer_exinst);
            cbr.end_render_pass();
        }

        Game
        {
            ph: PhantomData, buffer, renderpass, framebuffers, _bufview: bufview, _bufview2: bufview2,
            _descriptors: (dsl, dp, descs), render_cb, vg_renderer_params, vg_renderer_exinst,
            vg_renderer_params2, vg_renderer_exinst2
        }
    }

    fn update(&mut self, _e: &peridot::Engine<PL>, on_backbuffer_of: u32, _dt: std::time::Duration)
        -> (Option<br::SubmissionBatch>, br::SubmissionBatch)
    {
        (None, br::SubmissionBatch
        {
            command_buffers: Cow::Borrowed(&self.render_cb[on_backbuffer_of as usize..on_backbuffer_of as usize + 1]),
            .. Default::default()
        })
    }

    fn discard_backbuffer_resources(&mut self)
    {
        self.render_cb.reset().expect("Resetting RenderCB");
        self.framebuffers.clear();
    }
    fn on_resize(&mut self, e: &peridot::Engine<PL>, _new_size: Vector2<usize>)
    {
        self.framebuffers = (0..e.backbuffer_count())
            .map(|bb_index| {
                let bb = e.backbuffer(bb_index).expect("no backbuffer");
                br::Framebuffer::new(&self.renderpass, &[&bb], bb.size(), 1)
            })
            .collect::<Result<Vec<_>, _>>().expect("Bind Framebuffer");
        for (r, f) in self.render_cb.iter().zip(&self.framebuffers)
        {
            let mut cbr = r.begin().expect("Start Recording CB");
            self.render_commands(e, &mut cbr, f);
        }
    }
}

impl<PL: peridot::NativeLinker> Game<PL>
{
    fn render_commands(&self, e: &peridot::Engine<PL>, cmd: &mut br::CmdRecord, fb: &br::Framebuffer)
    {
        cmd.begin_render_pass(&self.renderpass, fb, fb.size().clone().into(), &[br::ClearValue::Color([1.0; 4])], true);
        self.vg_renderer_params2.default_render_commands(e, cmd, &self.buffer, &self.vg_renderer_exinst2);
        self.vg_renderer_params.default_render_commands(e, cmd, &self.buffer, &self.vg_renderer_exinst);
        cmd.end_render_pass();
    }
}
