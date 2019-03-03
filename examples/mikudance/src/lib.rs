
use std::marker::PhantomData;
extern crate bedrock as br; use br::traits::*;
use peridot::{CommandBundle, LayoutedPipeline, Buffer, BufferPrealloc, MemoryBadget, ModelData,
    TransferBatch, DescriptorSetUpdateBatch, CBSubmissionType, RenderPassTemplates, DefaultRenderCommands,
    PvpShaderModules, vg, SpecConstantStorage, PolygonModelExtended, BufferContent};
use peridot::math::{Vector2, Vector3, Matrix4, Vector4, Camera, ProjectionMethod, Quaternion, One};
use std::rc::Rc;
use std::borrow::Cow;
use peridot::vg::{PathBuilder, FlatPathBuilder};

#[derive(SpecConstantStorage)] #[repr(C)]
pub struct VgRendererFragmentFixedColor {
    r: f32, g: f32, b: f32, a: f32
}

#[repr(C)] pub struct WorldSettings {
    vp: Matrix4<f32>, light_dir: Vector4<f32>
}
#[repr(C)] pub struct ObjectSettings {
    tf: Matrix4<f32>
}

pub struct Game<PL: peridot::NativeLinker> {
    renderpass: br::RenderPass, framebuffers: Vec<br::Framebuffer>, render_cb: CommandBundle, buffer: Buffer,
    _bufview: br::BufferView,
    _descriptors: (br::DescriptorSetLayout, br::DescriptorSetLayout, br::DescriptorPool, Vec<br::vk::VkDescriptorSet>),
    vg_renderer_params: peridot::VgRendererParams,
    vg_renderer_exinst: peridot::VgRendererExternalInstances,
    gp_model: LayoutedPipeline, model_render_params: peridot::PMXRenderingParams,
    ph: PhantomData<*const PL>
}
impl<PL: peridot::NativeLinker> Game<PL> {
    pub const NAME: &'static str = "Peridot Examples - VectorGraphics";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &peridot::Engine<Self, PL>) -> Self {
        let model: PolygonModelExtended =
            e.load("models.SiroDanceWinterCostume_white_v1_1.siro_dance_wintercostume_white_v1_1")
            .expect("Loading Model");
        
        let cam = Camera {
            projection: ProjectionMethod::Perspective { fov: 75.0f32.to_radians() },
            position: Vector3(0.0, 0.0, -15.0), rotation: Quaternion::ONE,
            depth_range: 1.0 .. 100.0
        };

        let font =
            vg::Font::best_match(&[vg::FamilyName::Title("Yu Gothic UI".to_owned())], &vg::FontProperties::new(), 12.0)
            .expect("No Fonts");
        let mut ctx = vg::Context::new();
        ctx.text(&font, &format!("Model: {} / {}", model.name_jp(), model.name()));
        /*{
            let mut f0 = ctx.begin_figure(vg::FillRule::Winding);
            f0.move_to(Vector2(10.0, -10.0).into());
            f0.quadratic_bezier_to(Vector2(100.0, -35.0).into(), Vector2(100.0, -100.0).into());
            f0.end();
        }*/

        let mut bp = BufferPrealloc::new(&e.graphics());
        let world_settings_offs = bp.add(BufferContent::uniform::<WorldSettings>());
        let object_settings_offs = bp.add(BufferContent::uniform::<ObjectSettings>());
        let model_offs = model.prealloc(&mut bp);
        let vg_offs = ctx.prealloc(&mut bp);

        let buffer = bp.build_transferred().expect("Buffer Allocation");
        let stg_buffer = bp.build_upload().expect("StgBuffer Allocation");
        
        let buffer = MemoryBadget::new(&e.graphics()).alloc_with_buffer(buffer).expect("Mem Allocation");
        let stg_buffer = MemoryBadget::new(&e.graphics()).alloc_with_buffer_host_visible(stg_buffer)
            .expect("StgMem Allocation");
        
        let screen_size: br::Extent3D = e.backbuffers()[0].size().clone().into();
        let (vg_renderer_params, model_render_params) = stg_buffer.guard_map(bp.total_size(), |m| {
            let (v, p) = cam.matrixes();
            let aspect = Matrix4::scale(Vector4(screen_size.1 as f32 / screen_size.0 as f32, 1.0, 1.0, 1.0));
            let vp = aspect * p * v;
            unsafe {
                *m.get_mut(world_settings_offs) = WorldSettings { vp, light_dir: Vector4(-0.7, -0.5, 0.3, 0.0) };
                *m.get_mut(object_settings_offs) = ObjectSettings { tf: Matrix4::ONE };
            }

            let model_render_params = model.stage_data_into(m, model_offs);
            let render_params = ctx.stage_data_into(m, vg_offs);
            return (render_params, model_render_params);
        }).expect("StgMem Initialization");

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
        let dsl_model = br::DescriptorSetLayout::new(&e.graphics(), &br::DSLBindings {
            uniform_buffer: Some((0, 1, br::ShaderStage::VERTEX)), .. br::DSLBindings::empty()
        }).expect("DescriptorSetLayout for ModelRendering creation");
        let dp = br::DescriptorPool::new(&e.graphics(), 3, &[
            br::DescriptorPoolSize(br::DescriptorType::UniformTexelBuffer, 1),
            br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 2)
        ], false).expect("DescriptorPool Creation");
        let descs = dp.alloc(&[&dsl, &dsl_model, &dsl_model]).expect("DescriptorSet Allocation");

        let mut dub = DescriptorSetUpdateBatch::new();
        dub.write(descs[0], 0, br::DescriptorUpdateInfo::UniformTexelBuffer(vec![bufview.native_ptr()]));
        dub.write(descs[1], 0, br::DescriptorUpdateInfo::UniformBuffer(vec![
            (buffer.native_ptr(), world_settings_offs .. world_settings_offs + std::mem::size_of::<WorldSettings>())
        ]));
        dub.write(descs[2], 0, br::DescriptorUpdateInfo::UniformBuffer(vec![
            (buffer.native_ptr(), object_settings_offs .. object_settings_offs + std::mem::size_of::<ObjectSettings>())
        ]));
        dub.submit(&e.graphics());

        let shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.interiorColorFixed")
            .expect("Loading PvpContainer")).expect("Creating Shader");
        let curve_shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.curveColorFixed")
            .expect("Loading CurveShader")).expect("Creating CurveShader");
        let model_shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.defaultMMDShader")
            .expect("Loading defaultMMDShader")).expect("Creating defaultMMDShader");
        let vp = [br::vk::VkViewport {
            width: screen_size.0 as _, height: screen_size.1 as _, x: 0.0, y: 0.0,
            minDepth: 0.0, maxDepth: 1.0
        }];
        let sc = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D::default(),
            extent: br::vk::VkExtent2D { width: screen_size.0, height: screen_size.1 }
        }];
        let pl: Rc<_> = br::PipelineLayout::new(&e.graphics(), &[&dsl], &[(br::ShaderStage::VERTEX, 0 .. 4 * 4)])
            .expect("Create PipelineLayout").into();
        let pl_model: Rc<_> = br::PipelineLayout::new(&e.graphics(), &[&dsl_model, &dsl_model], &[])
            .expect("Create PipelineLayout for ModelRendering").into();
        let spc_map = vec![
            br::vk::VkSpecializationMapEntry { constantID: 0, offset: 0, size: 4 },
            br::vk::VkSpecializationMapEntry { constantID: 1, offset: 4, size: 4 }
        ];
        let mut interior_vertex_processing = shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        let mut curve_vertex_processing = curve_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        interior_vertex_processing.mod_vertex_shader().specinfo =
            Some((spc_map.clone(), br::DynamicDataCell::from_slice(&peridot::vg::renderer_pivot::LEFT_TOP)));
        curve_vertex_processing.mod_vertex_shader().specinfo =
            Some((spc_map.clone(), br::DynamicDataCell::from_slice(&peridot::vg::renderer_pivot::LEFT_TOP)));
        
        interior_vertex_processing.mod_fragment_shader().expect("fragment shader not exist?").specinfo =
            Some(VgRendererFragmentFixedColor { r: 1.0, g: 1.0, b: 1.0, a: 1.0 }.as_pair());
        curve_vertex_processing.mod_fragment_shader().expect("fragment shader not exist?").specinfo =
            Some(VgRendererFragmentFixedColor { r: 1.0, g: 1.0, b: 1.0, a: 1.0 }.as_pair());
        let mut gpb = br::GraphicsPipelineBuilder::new(&pl, (&renderpass, 0));
        gpb.vertex_processing(interior_vertex_processing)
            .fixed_viewport_scissors(br::DynamicArrayState::Static(&vp), br::DynamicArrayState::Static(&sc))
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied());
        let gp = LayoutedPipeline::combine(gpb.create(&e.graphics(), None).expect("Create GraphicsPipeline"), &pl);
        gpb.vertex_processing(curve_vertex_processing);
        let gp_curve = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline of CurveRender"), &pl);
        let vg_renderer_exinst = peridot::VgRendererExternalInstances {
            interior_pipeline: gp, curve_pipeline: gp_curve, transform_buffer_descriptor_set: descs[0],
            target_pixels: Vector2(screen_size.0 as _, screen_size.1 as _)
        };

        gpb.layout(&pl_model)
            .vertex_processing(model_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST));
        let gp_model = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline for ModelRendering"), &pl_model);

        let render_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, framebuffers.len())
            .expect("Creating RenderCB");
        for (r, f) in render_cb.iter().zip(&framebuffers) {
            let mut cbr = r.begin().expect("Start Recoding CB");
            cbr.begin_render_pass(&renderpass, f, f.size().clone().into(), &[br::ClearValue::Color([0.0; 4])], true);

            gp_model.bind(&mut cbr);
            cbr.bind_graphics_descriptor_sets(0, &descs[1..3], &[]);
            model_render_params.default_render_commands(e, &mut cbr, &buffer, &());

            vg_renderer_params.default_render_commands(e, &mut cbr, &buffer, &vg_renderer_exinst);
            cbr.end_render_pass();
        }

        Game {
            ph: PhantomData, buffer, renderpass, framebuffers, _bufview: bufview,
            _descriptors: (dsl, dsl_model, dp, descs), render_cb, vg_renderer_params, vg_renderer_exinst,
            gp_model, model_render_params
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
            self.render_commands(e, &mut cbr, f);
        }
    }
}

impl<PL: peridot::NativeLinker> Game<PL> {
    fn render_commands(&self, e: &peridot::Engine<Self, PL>, cmd: &mut br::CmdRecord, fb: &br::Framebuffer) {
        cmd.begin_render_pass(&self.renderpass, fb, fb.size().clone().into(), &[br::ClearValue::Color([0.0; 4])], true);

        self.gp_model.bind(cmd);
        cmd.bind_graphics_descriptor_sets(0, &self._descriptors.3[1..3], &[]);
        self.model_render_params.default_render_commands(e, cmd, &self.buffer, &());

        self.vg_renderer_params.default_render_commands(e, cmd, &self.buffer, &self.vg_renderer_exinst);
        cmd.end_render_pass();
    }
}