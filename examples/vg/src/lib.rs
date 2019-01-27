
use std::marker::PhantomData;
extern crate bedrock as br; use br::traits::*;
use peridot::{CommandBundle, LayoutedPipeline, Buffer, BufferPrealloc, MemoryBadget, ModelData,
    TransferBatch, DescriptorSetUpdateBatch, CBSubmissionType, RenderPassTemplates, DefaultRenderCommands,
    PvpShaderModules};
use peridot::math::Vector2;
use std::rc::Rc;
use std::borrow::Cow;

pub struct Game<PL: peridot::NativeLinker> {
    renderpass: br::RenderPass, framebuffers: Vec<br::Framebuffer>, render_cb: CommandBundle, buffer: Buffer,
    _bufview: br::BufferView, _descriptors: (br::DescriptorSetLayout, br::DescriptorPool, Vec<br::vk::VkDescriptorSet>),
    vg_renderer_params: peridot::VgRendererParams,
    vg_renderer_exinst: peridot::VgRendererExternalInstances,
    ph: PhantomData<*const PL>
}
impl<PL: peridot::NativeLinker> Game<PL> {
    pub const NAME: &'static str = "Peridot Examples - VectorGraphics";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &peridot::Engine<Self, PL>) -> Self {
        let font = peridot::vg::Font::best_match(&[peridot::vg::FamilyName::SansSerif],
            &peridot::vg::FontProperties::new(), 12.0).expect("No Fonts");
        let mut ctx = peridot::vg::Context::new();
        ctx.text(&font, "Hello, World!");

        let mut bp = BufferPrealloc::new(&e.graphics());
        let vg_offs = ctx.prealloc(&mut bp);

        let buffer = bp.build_transferred().expect("Buffer Allocation");
        let stg_buffer = bp.build_upload().expect("StgBuffer Allocation");
        
        let buffer = MemoryBadget::new(&e.graphics()).alloc_with_buffer(buffer).expect("Mem Allocation");
        let stg_buffer = MemoryBadget::new(&e.graphics()).alloc_with_buffer_host_visible(stg_buffer)
            .expect("StgMem Allocation");
        
        let vg_renderer_params = stg_buffer.guard_map(bp.total_size(), |m| ctx.stage_data_into(m, vg_offs))
            .expect("StgMem Initialization");

        let bufview = buffer.create_view(br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            vg_renderer_params.transforms_byterange()).expect("Creating Transform BufferView");

        e.submit_commands(|rec| {
            let mut tfb = TransferBatch::new();
            tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, bp.total_size() as _);
            tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_SHADER.vertex_input(), &buffer,
                0 .. bp.total_size() as _,
                br::AccessFlags::SHADER.read | br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::INDEX_READ);
            tfb.sink_transfer_commands(rec);
            tfb.sink_graphics_ready_commands(rec);
        }).expect("ImmResource Initialization");

        let screen_size = e.backbuffers()[0].size().clone();
        let renderpass = RenderPassTemplates::single_render(e.backbuffer_format())
            .create(&e.graphics()).expect("RenderPass Creation");
        let framebuffers = e.backbuffers().iter().map(|v| br::Framebuffer::new(&renderpass, &[v], &screen_size, 1))
            .collect::<Result<Vec<_>, _>>().expect("Framebuffer Creation");
        
        let dsl = br::DescriptorSetLayout::new(&e.graphics(), &br::DSLBindings {
            uniform_texel_buffer: Some((0, 1, br::ShaderStage::VERTEX)),
            .. br::DSLBindings::empty()
        }).expect("DescriptorSetLayout Creation");
        let dp = br::DescriptorPool::new(&e.graphics(), 1,
            &[br::DescriptorPoolSize(br::DescriptorType::UniformTexelBuffer, 1)],
            false
        ).expect("DescriptorPool Creation");
        let descs = dp.alloc(&[&dsl]).expect("DescriptorSet Allocation");

        let mut dub = DescriptorSetUpdateBatch::new();
        dub.write(descs[0], 0, br::DescriptorUpdateInfo::UniformTexelBuffer(vec![bufview.native_ptr()]));
        dub.submit(&e.graphics());

        let shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.interior").expect("Loading PvpContainer"))
            .expect("Creating Shader");
        let curve_shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.curve").expect("Loading CurveShader"))
            .expect("Creating CurveShader");
        let vp = [br::vk::VkViewport {
            width: screen_size.0 as _, height: screen_size.1 as _, x: 0.0, y: 0.0,
            minDepth: 0.0, maxDepth: 1.0
        }];
        let sc = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D::default(),
            extent: br::vk::VkExtent2D { width: screen_size.0, height: screen_size.1 }
        }];
        debug!("ScreenSize: {:?}", screen_size);
        let pl: Rc<_> = br::PipelineLayout::new(&e.graphics(), &[&dsl], &[(br::ShaderStage::VERTEX, 0 .. 4 * 3)])
            .expect("Create PipelineLayout").into();
        let gp = br::GraphicsPipelineBuilder::new(&pl, (&renderpass, 0))
            .vertex_processing(shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST))
            .fixed_viewport_scissors(br::DynamicArrayState::Static(&vp), br::DynamicArrayState::Static(&sc))
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .create(&e.graphics(), None).expect("Create GraphicsPipeline");
        let gp = LayoutedPipeline::combine(gp, &pl);
        let gp_curve = br::GraphicsPipelineBuilder::new(&pl, (&renderpass, 0))
            .vertex_processing(curve_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST))
            .fixed_viewport_scissors(br::DynamicArrayState::Static(&vp), br::DynamicArrayState::Static(&sc))
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .create(&e.graphics(), None).expect("Create GraphicsPipeline of CurveRender");
        let gp_curve = LayoutedPipeline::combine(gp_curve, &pl);
        let vg_renderer_exinst = peridot::VgRendererExternalInstances {
            interior_pipeline: gp, curve_pipeline: gp_curve, transform_buffer_descriptor_set: descs[0],
            target_pixels: Vector2(screen_size.0, screen_size.1)
        };

        let render_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, framebuffers.len())
            .expect("Creating RenderCB");
        for (r, f) in render_cb.iter().zip(&framebuffers) {
            let mut cbr = r.begin().expect("Start Recoding CB");
            cbr.begin_render_pass(&renderpass, f, f.size().clone().into(), &[br::ClearValue::Color([0.0; 4])], true);
            vg_renderer_params.default_render_commands(&mut cbr, &buffer, &vg_renderer_exinst);
            cbr.end_render_pass();
        }

        Game {
            ph: PhantomData, buffer, renderpass, framebuffers, _bufview: bufview,
            _descriptors: (dsl, dp, descs), render_cb, vg_renderer_params, vg_renderer_exinst
        }
    }

    fn update(&mut self, e: &peridot::Engine<Self, PL>, on_backbuffer_of: u32)
            -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        (None, br::SubmissionBatch {
            command_buffers: Cow::Borrowed(&self.render_cb[on_backbuffer_of as usize..on_backbuffer_of as usize + 1]),
            .. Default::default()
        })
    }

    fn discard_backbuffer_resources(&mut self) {
        self.render_cb.reset().expect("Resetting RenderCB");
        self.framebuffers.clear();
    }
    fn on_resize(&mut self, e: &peridot::Engine<Self, PL>, new_size: Vector2<usize>) {
        self.framebuffers = e.backbuffers().iter().map(|v| br::Framebuffer::new(&self.renderpass, &[v], v.size(), 1))
            .collect::<Result<Vec<_>, _>>().expect("Bind Framebuffer");
        for (r, f) in self.render_cb.iter().zip(&self.framebuffers) {
            let mut cbr = r.begin().expect("Start Recording CB");
            self.render_commands(&mut cbr, f);
        }
    }
}

impl<PL: peridot::NativeLinker> Game<PL> {
    fn render_commands(&self, cmd: &mut br::CmdRecord, fb: &br::Framebuffer) {
        cmd.begin_render_pass(&self.renderpass, fb, fb.size().clone().into(), &[br::ClearValue::Color([0.0; 4])], true);
        self.vg_renderer_params.default_render_commands(cmd, &self.buffer, &self.vg_renderer_exinst);
        cmd.end_render_pass();
    }
}
