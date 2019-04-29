
use std::marker::PhantomData;
extern crate bedrock as br; use br::traits::*;
use peridot::{CommandBundle, LayoutedPipeline, Buffer, BufferPrealloc, MemoryBadget, ModelData,
    TransferBatch, DescriptorSetUpdateBatch, CBSubmissionType, RenderPassTemplates, DefaultRenderCommands,
    PvpShaderModules, vg, SpecConstantStorage, PolygonModelExtended, BufferContent,
    DepthStencilTexture2D, TextureInitializationGroup, AssetLoaderService, Texture2D,
    TextureInstantiatedGroup};
use peridot::math::{Vector2, Vector3, Matrix4, Vector4, Camera, ProjectionMethod, Quaternion, One};
use std::rc::Rc;
use std::borrow::Cow;
use peridot::vg::{PathBuilder, FlatPathBuilder};
use peridot::Discardable;

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

pub struct FixedMemoryBlock {
    textures: Vec<Texture2D>, buffer: Buffer
}
impl FixedMemoryBlock {
    pub fn init(g: &peridot::Graphics, mut prealloc: BufferPrealloc, textures: TextureInitializationGroup)
            -> br::Result<(Buffer, TextureInstantiatedGroup, Buffer, u64)> {
        let texture_prealloc = textures.prealloc(&mut prealloc)?;
        let buffer = prealloc.build_transferred()?;
        let stg_buffer = prealloc.build_upload()?;

        let (mut mb, mut mb_stg) = (MemoryBadget::new(g), MemoryBadget::new(g));
        mb.add(buffer);
        mb_stg.add(stg_buffer);
        let (textures, mut res) = texture_prealloc.alloc_and_instantiate(mb)?;
        let buffer = res.pop().expect("No objects?").unwrap_buffer();
        let stg_buffer = mb_stg.alloc_upload()?.pop().expect("No objects?").unwrap_buffer();

        return Ok((buffer, textures, stg_buffer, prealloc.total_size()));
    }
    pub fn new(buffer: Buffer, textures: TextureInstantiatedGroup) -> Self
    {
        FixedMemoryBlock { textures: textures.into_textures_reversed(), buffer }
    }

    pub fn iter_textures(&self) -> impl Iterator<Item = &Texture2D> { self.textures.iter().rev() }
}

pub struct Game<PL: peridot::NativeLinker> {
    model: PolygonModelExtended,
    renderpass: br::RenderPass, framebuffers: Vec<br::Framebuffer>, render_cb: CommandBundle,
    memblock: FixedMemoryBlock,
    depth_buffer: Discardable<DepthStencilTexture2D>,
    _bufview: br::BufferView,
    _descriptors: (br::DescriptorSetLayout, br::DescriptorSetLayout, br::DescriptorPool, Vec<br::vk::VkDescriptorSet>),
    dsl_tex: br::DescriptorSetLayout,
    texture_descs: Vec<br::vk::VkDescriptorSet>,
    vg_renderer_params: peridot::VgRendererParams,
    vg_renderer_exinst: peridot::VgRendererExternalInstances,
    gp_model: LayoutedPipeline, gp_model_tex: LayoutedPipeline, model_render_params: peridot::PMXRenderingParams,
    ph: PhantomData<*const PL>
}
impl<PL: peridot::NativeLinker> Game<PL> {
    pub const NAME: &'static str = "Peridot Examples - MMD/VRM Loader";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &peridot::Engine<Self, PL>) -> Self {
        let model: PolygonModelExtended =
            e.load("models.SiroDanceWinterCostume_white_v1_1.siro_dance_wintercostume_white_v1_1")
            // e.load("models.サーバル.サーバル")
            .expect("Loading Model");
        for (n, tp) in model.textures.iter().enumerate() {
            println!("ModelTexture #{}: {}", n, tp.display());
        }
        for (n, b) in model.bones.iter().enumerate()
        {
            println!("Bone #{}: {}", n, b.name.jp);
        }

        let motion: peridot::mmdloader::vmd::MotionData =
            e.load("models.極楽浄土.極楽上半身2ボーンが長い用")
            .expect("Loading Motion");
        println!("MotionData:");
        println!("-- Name: {}", motion.decode_name().expect("Invalid Shift-JIS Sequence"));
        println!("-- Bones: ");
        for b in motion.bone_keyframes()
        {
            if b.frame_number != 0 { continue; }
            println!("---- {}: Frame #{}", b.decode_name().expect("Invalid Shift-JIS Sequence"), b.frame_number);
        }
        println!("-- FaceFrameCount: {}", motion.face_keyframe_count);
        println!("-- CameraFrameCount: {}", motion.camera_keyframe_count);
        
        let cam = Camera {
            projection: ProjectionMethod::Perspective { fov: 75.0f32.to_radians() },
            position: Vector3(0.0, 10.0, -20.0), rotation: Quaternion::ONE,
            depth_range: 1.0 .. 100.0
        };

        let font =
            vg::Font::best_match(&[vg::FamilyName::Title("Yu Gothic UI".to_owned())], &vg::FontProperties::new(), 12.0)
            .expect("No Fonts");
        let mut ctx = vg::Context::new();
        let title_width = ctx.text(&font, &format!("Model: {} / {}", model.name_jp(), model.name()))
            .expect("Rendering Text");
        {
            let mut f0 = ctx.begin_figure(vg::FillRule::Winding);
            f0.move_to(Vector2(0.0, -15.0).into()); f0.line_to(Vector2(title_width * 1.1, -15.0).into());
            f0.line_to(Vector2(title_width * 1.1, -16.0).into()); f0.line_to(Vector2(0.0, -16.0).into());
            f0.close(); f0.end();
        }

        let mut tinit = TextureInitializationGroup::new(&e.graphics());
        let mut bp = BufferPrealloc::new(&e.graphics());
        let world_settings_offs = bp.add(BufferContent::uniform::<WorldSettings>());
        let object_settings_offs = bp.add(BufferContent::uniform::<ObjectSettings>());
        let model_offs = model.prealloc(e, &mut bp, &mut tinit);
        let vg_offs = ctx.prealloc(e, &mut bp, &mut tinit);

        let screen_size: br::Extent3D = e.backbuffers()[0].size().clone().into();
        let depth_buffer = DepthStencilTexture2D::init(&e.graphics(), &Vector2(screen_size.0, screen_size.1),
            peridot::PixelFormat::D24S8)
            .expect("Init DepthStencilTexture2D");
        
        let (buffer, textures, stg_buffer, stg_size) = FixedMemoryBlock::init(&e.graphics(), bp, tinit)
            .expect("Alloc MemoryBlocks");

        let mut mb_scrbuf = MemoryBadget::new(&e.graphics());
        mb_scrbuf.add(depth_buffer);
        let depth_buffer = DepthStencilTexture2D::new(
            mb_scrbuf.alloc().expect("ScreenBuffer Memory Allocation").pop().expect("No objects?").unwrap_image()
        ).expect("Creating DepthStencilTexture2D");
        
        let (vg_renderer_params, model_render_params) = stg_buffer.guard_map(stg_size, |m| {
            let (v, p) = cam.matrixes();
            let aspect = Matrix4::scale(Vector4(screen_size.1 as f32 / screen_size.0 as f32, 1.0, 1.0, 1.0));
            let vp = aspect * p * v;
            unsafe {
                *m.get_mut(world_settings_offs as _) = WorldSettings { vp, light_dir: Vector4(-0.7, -0.5, 0.3, 0.0) };
                *m.get_mut(object_settings_offs as _) = ObjectSettings { tf: Matrix4::ONE };
            }

            let model_render_params = model.stage_data_into(m, model_offs);
            let render_params = ctx.stage_data_into(m, vg_offs);
            textures.stage_data(m);
            return (render_params, model_render_params);
        }).expect("StgMem Initialization");

        let bufview = buffer.create_view(br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            vg_renderer_params.transforms_byterange()).expect("Creating Transform BufferView");

        e.submit_commands(|rec| {
            let mut tfb = TransferBatch::new();
            tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, stg_size as _);
            tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_SHADER.vertex_input(),
                &buffer, 0 .. stg_size as _,
                br::AccessFlags::SHADER.read | br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::INDEX_READ);
            textures.copy_from_stage_batches(&mut tfb, &stg_buffer);
            tfb.sink_transfer_commands(rec);
            tfb.sink_graphics_ready_commands(rec);
            rec.pipeline_barrier(br::PipelineStageFlags::TOP_OF_PIPE, br::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
                false, &[], &[], &[
                    br::ImageMemoryBarrier::new_raw(&depth_buffer, &br::ImageSubresourceRange::depth_stencil(0, 0),
                        br::ImageLayout::Undefined, br::ImageLayout::DepthStencilAttachmentOpt)
                ]);
        }).expect("ImmResource Initialization");
        let memblock = FixedMemoryBlock::new(buffer, textures);

        let renderpass = RenderPassTemplates::single_render_with_depth(e.backbuffer_format(),
            br::vk::VK_FORMAT_D24_UNORM_S8_UINT)
            .create(&e.graphics()).expect("RenderPass Creation");
        let framebuffers = e.backbuffers().iter()
            .map(|v| br::Framebuffer::new(&renderpass, &[v, &depth_buffer], &screen_size, 1))
            .collect::<Result<Vec<_>, _>>().expect("Framebuffer Creation");
        
        let i_smp = br::SamplerBuilder::default().create(&e.graphics()).expect("Creating Sampler");
        let dsl = br::DescriptorSetLayout::new(&e.graphics(), &br::DSLBindings {
            uniform_texel_buffer: Some((0, 1, br::ShaderStage::VERTEX)),
            .. br::DSLBindings::empty()
        }).expect("DescriptorSetLayout Creation");
        let dsl_model = br::DescriptorSetLayout::new(&e.graphics(), &br::DSLBindings {
            uniform_buffer: Some((0, 1, br::ShaderStage::VERTEX)), .. br::DSLBindings::empty()
        }).expect("DescriptorSetLayout for ModelRendering creation");
        let dsl_tex = br::DescriptorSetLayout::new(&e.graphics(), &br::DSLBindings {
            combined_image_sampler: Some((0, 1, br::ShaderStage::FRAGMENT, vec![i_smp.native_ptr()])),
            .. br::DSLBindings::empty()
        }).expect("DescriptorSetLayout for Textured Rendering creation");
        let dp = br::DescriptorPool::new(&e.graphics(), (3 + memblock.textures.len()) as _, &[
            br::DescriptorPoolSize(br::DescriptorType::UniformTexelBuffer, 1),
            br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 2),
            br::DescriptorPoolSize(br::DescriptorType::CombinedImageSampler, memblock.textures.len() as _)
        ], false).expect("DescriptorPool Creation");
        let descs = dp.alloc(&[&dsl, &dsl_model, &dsl_model]).expect("DescriptorSet Allocation");
        let texture_descs = dp.alloc(&vec![&dsl_tex; memblock.textures.len()][..])
            .expect("DescriptorSet Allocation for Textures");

        let mut dub = DescriptorSetUpdateBatch::new();
        dub.write(descs[0], 0, br::DescriptorUpdateInfo::UniformTexelBuffer(vec![bufview.native_ptr()]));
        dub.write(descs[1], 0, br::DescriptorUpdateInfo::UniformBuffer(vec![
            (memblock.buffer.native_ptr(),
                world_settings_offs as usize .. world_settings_offs as usize + std::mem::size_of::<WorldSettings>())
        ]));
        dub.write(descs[2], 0, br::DescriptorUpdateInfo::UniformBuffer(vec![
            (memblock.buffer.native_ptr(),
                object_settings_offs as usize .. object_settings_offs as usize + std::mem::size_of::<ObjectSettings>())
        ]));
        for (&td, o) in texture_descs.iter().zip(memblock.iter_textures()) {
            dub.write(td, 0, br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                (None, o.view().native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
            ]));
        }
        dub.submit(&e.graphics());

        let shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.interiorColorFixed")
            .expect("Loading PvpContainer")).expect("Creating Shader");
        let curve_shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.curveColorFixed")
            .expect("Loading CurveShader")).expect("Creating CurveShader");
        let model_shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.defaultMMDShader")
            .expect("Loading defaultMMDShader")).expect("Creating defaultMMDShader");
        let model_shader_tex = PvpShaderModules::new(&e.graphics(), e.load("shaders.defaultMMDShader_tex")
            .expect("Loading defaultMMDShader with Texture")).expect("Creating defaultMMDShader_tex");
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
        let pl_model: Rc<_> = br::PipelineLayout::new(&e.graphics(),
            &[&dsl_model, &dsl_model], &[(br::ShaderStage::FRAGMENT, 0 .. 4 * 4)])
            .expect("Create PipelineLayout for ModelRendering").into();
        let pl_model_tex: Rc<_> = br::PipelineLayout::new(&e.graphics(),
            &[&dsl_model, &dsl_model, &dsl_tex], &[(br::ShaderStage::FRAGMENT, 0 .. 4 * 4)])
            .expect("Create PipelineLayout for ModelRendering with Texture").into();
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
        let text_color = VgRendererFragmentFixedColor { r: 1.0, g: 1.0, b: 1.0, a: 1.0 };
        interior_vertex_processing.mod_fragment_shader().expect("fragment shader not exist?").specinfo =
            Some(text_color.as_pair());
        curve_vertex_processing.mod_fragment_shader().expect("fragment shader not exist?").specinfo =
            Some(text_color.as_pair());
        let mut gpb = br::GraphicsPipelineBuilder::new(&pl, (&renderpass, 0));
        gpb.vertex_processing(interior_vertex_processing)
            .fixed_viewport_scissors(br::DynamicArrayState::Static(&vp), br::DynamicArrayState::Static(&sc))
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .depth_test_settings(None, false);
        let gp = LayoutedPipeline::combine(gpb.create(&e.graphics(), None).expect("Create GraphicsPipeline"), &pl);
        gpb.vertex_processing(curve_vertex_processing);
        let gp_curve = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline of CurveRender"), &pl);
        let vg_renderer_exinst = peridot::VgRendererExternalInstances {
            interior_pipeline: gp, curve_pipeline: gp_curve, transform_buffer_descriptor_set: descs[0],
            target_pixels: Vector2(screen_size.0 as _, screen_size.1 as _)
        };

        gpb.layout(&pl_model)
            .cull_mode(br::vk::VK_CULL_MODE_FRONT_BIT)
            .depth_test_settings(Some(br::CompareOp::Less), true)
            .vertex_processing(model_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST));
        let gp_model = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline for ModelRendering"), &pl_model);
        gpb.layout(&pl_model_tex)
            .cull_mode(br::vk::VK_CULL_MODE_FRONT_BIT)
            .depth_test_settings(Some(br::CompareOp::Less), true)
            .vertex_processing(model_shader_tex.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST));
        let gp_model_tex = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline for ModelRendering"), &pl_model_tex);

        let render_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, framebuffers.len())
            .expect("Creating RenderCB");
        for (r, f) in render_cb.iter().zip(&framebuffers) {
            let mut cbr = r.begin().expect("Start Recoding CB");
            cbr.begin_render_pass(&renderpass, f, f.size().clone().into(), &[
                br::ClearValue::Color([0.0, 0.1, 0.2, 1.0]), br::ClearValue::DepthStencil(1.0, 0)
            ], true);

            gp_model.bind(&mut cbr);
            model_render_params.set_vertex_buffer(&mut cbr, &memblock.buffer);
            cbr.bind_graphics_descriptor_sets(0, &descs[1..3], &[]);
            model_render_params.untextured_render(&mut cbr, &memblock.buffer, &model);
            gp_model_tex.bind(&mut cbr);
            cbr.bind_graphics_descriptor_sets(0, &descs[1..3], &[]);
            model_render_params.textured_render(&mut cbr, &memblock.buffer, &model, &texture_descs);

            vg_renderer_params.default_render_commands(e, &mut cbr, &memblock.buffer, &vg_renderer_exinst);
            cbr.end_render_pass();
        }

        Game {
            ph: PhantomData, renderpass, framebuffers, _bufview: bufview, memblock,
            _descriptors: (dsl, dsl_model, dp, descs), render_cb, vg_renderer_params, vg_renderer_exinst,
            gp_model, model_render_params, depth_buffer: Discardable::from(depth_buffer),
            model, gp_model_tex, dsl_tex, texture_descs
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
        self.depth_buffer.discard_lw();
    }
    fn on_resize(&mut self, e: &peridot::Engine<Self, PL>, new_size: Vector2<usize>) {
        let &br::Extent3D(w, h, _) = e.backbuffers()[0].size();
        
        let depth_buffer = DepthStencilTexture2D::init(&e.graphics(), &Vector2(w, h),
            peridot::PixelFormat::D24S8)
            .expect("Init DepthStencilTexture2D");
        let mut mb_scrbuf = MemoryBadget::new(&e.graphics());
        mb_scrbuf.add(depth_buffer);
        self.depth_buffer.set_lw(DepthStencilTexture2D::new(
            mb_scrbuf.alloc().expect("ScreenBuffer Memory Allocation").pop().expect("No objects?").unwrap_image()
        ).expect("Creating DepthStencilTexture2D"));
        e.submit_commands(|rec| {
            rec.pipeline_barrier(br::PipelineStageFlags::TOP_OF_PIPE, br::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
                false, &[], &[], &[
                    br::ImageMemoryBarrier::new_raw(&self.depth_buffer.get(),
                        &br::ImageSubresourceRange::depth_stencil(0, 0),
                        br::ImageLayout::Undefined, br::ImageLayout::DepthStencilAttachmentOpt)
                ]);
        }).expect("ImmResource Initialization");

        self.framebuffers = e.backbuffers().iter()
            .map(|v| br::Framebuffer::new(&self.renderpass, &[v, &self.depth_buffer.get()], v.size(), 1))
            .collect::<Result<Vec<_>, _>>().expect("Bind Framebuffer");
        for (r, f) in self.render_cb.iter().zip(&self.framebuffers) {
            let mut cbr = r.begin().expect("Start Recording CB");
            self.render_commands(e, &mut cbr, f);
        }
    }
}

impl<PL: peridot::NativeLinker> Game<PL> {
    fn render_commands(&self, e: &peridot::Engine<Self, PL>, cmd: &mut br::CmdRecord, fb: &br::Framebuffer) {
        cmd.begin_render_pass(&self.renderpass, fb, fb.size().clone().into(), &[
            br::ClearValue::Color([0.0, 0.1, 0.2, 1.0]), br::ClearValue::DepthStencil(1.0, 0)
        ], true);

        self.gp_model.bind(cmd);
        self.model_render_params.set_vertex_buffer(cmd, &self.memblock.buffer);
        cmd.bind_graphics_descriptor_sets(0, &self._descriptors.3[1..3], &[]);
        self.model_render_params.untextured_render(cmd, &self.memblock.buffer, &self.model);
        self.gp_model_tex.bind(cmd);
        cmd.bind_graphics_descriptor_sets(0, &self._descriptors.3[1..3], &[]);
        self.model_render_params.textured_render(cmd, &self.memblock.buffer, &self.model, &self.texture_descs);

        self.vg_renderer_params.default_render_commands(e, cmd, &self.memblock.buffer, &self.vg_renderer_exinst);
        cmd.end_render_pass();
    }
}
