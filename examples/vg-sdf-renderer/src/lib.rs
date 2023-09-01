use bedrock as br;
use br::{CommandBuffer, Device, Image, ImageChild, ImageSubresourceSlice, SubmissionBatch};
use peridot::mthelper::SharedRef;
use peridot::SpecConstantStorage;
use peridot::{Engine, EngineEvents, FeatureRequests};
use peridot_command_object::{
    BeginRenderPass, Blending, BufferUsage, ColorAttachmentBlending, EndRenderPass,
    GraphicsCommand, GraphicsCommandCombiner, GraphicsCommandSubmission, NextSubpass,
    PipelineBarrier, RangedBuffer, RangedImage, SimpleDrawIndexed, StandardIndexedMesh,
    StandardMesh,
};
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_vertex_processing_pack::PvpShaderModules;
use peridot_vg::{FlatPathBuilder, Font, FontProvider, FontProviderConstruct};

#[derive(SpecConstantStorage)]
#[repr(C)]
pub struct FillFragmentShaderParameters {
    enable_color_output: br::vk::VkBool32,
}

pub struct StencilOpGroup {
    pub fail: br::vk::VkCompareOp,
    pub depth_fail: br::vk::VkCompareOp,
    pub pass: br::vk::VkCompareOp,
}
pub trait StencilOp {
    fn construct(self) -> StencilOpGroup;
}
impl StencilOp for br::StencilOp {
    fn construct(self) -> StencilOpGroup {
        StencilOpGroup {
            fail: self as _,
            depth_fail: self as _,
            pass: self as _,
        }
    }
}
/// (fail, pass) pair
impl StencilOp for (br::StencilOp, br::StencilOp) {
    fn construct(self) -> StencilOpGroup {
        StencilOpGroup {
            fail: self.0 as _,
            depth_fail: self.0 as _,
            pass: self.1 as _,
        }
    }
}

pub struct StencilCompare {
    pub op: br::CompareOp,
    pub reference: u32,
    pub mask: u32,
}
impl StencilCompare {
    pub const fn new(op: br::CompareOp, reference: u32) -> Self {
        Self {
            op,
            reference,
            mask: 0xffff_ffff,
        }
    }

    pub const fn with_mask(self, mask: u32) -> Self {
        Self { mask, ..self }
    }
}

pub struct StencilState {
    pub ops: StencilOpGroup,
    pub compare: StencilCompare,
    pub write_mask: u32,
}
impl StencilState {
    pub fn new(ops: impl StencilOp) -> Self {
        Self {
            ops: ops.construct(),
            compare: StencilCompare::new(br::CompareOp::Always, 0),
            write_mask: 0xffff_ffff,
        }
    }

    pub const fn with_compare(self, compare: StencilCompare) -> Self {
        Self { compare, ..self }
    }

    pub const fn with_write_mask(self, write_mask: u32) -> Self {
        Self { write_mask, ..self }
    }

    pub const fn into_vk(self) -> br::vk::VkStencilOpState {
        br::vk::VkStencilOpState {
            failOp: self.ops.fail,
            passOp: self.ops.pass,
            depthFailOp: self.ops.depth_fail,
            compareOp: self.compare.op as _,
            compareMask: self.compare.mask,
            writeMask: self.write_mask,
            reference: self.compare.reference,
        }
    }
}
impl From<StencilState> for br::vk::VkStencilOpState {
    fn from(value: StencilState) -> Self {
        value.into_vk()
    }
}

#[repr(C)]
#[derive(SpecConstantStorage)]
struct StencilTriangleVertexShaderParameters {
    pub target_width: f32,
    pub target_height: f32,
}

#[repr(C)]
#[derive(SpecConstantStorage)]
struct OutlineVertexShaderParameters {
    pub target_width: f32,
    pub target_height: f32,
    pub sdf_max_distance: f32,
}

pub struct TwoPassStencilSDFRenderer {
    render_pass: br::RenderPassObject<peridot::DeviceObject>,
    target_size: peridot::math::Vector2<u32>,
    fill_shader: PvpShaderModules<'static, peridot::DeviceObject>,
    curve_fill_shader: PvpShaderModules<'static, peridot::DeviceObject>,
    outline_shader: PvpShaderModules<'static, peridot::DeviceObject>,
    triangle_fans_stencil_pipeline: peridot::LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        SharedRef<br::PipelineLayoutObject<peridot::DeviceObject>>,
    >,
    curve_triangles_stencil_pipeline: peridot::LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        SharedRef<br::PipelineLayoutObject<peridot::DeviceObject>>,
    >,
    outline_distance_pipeline: peridot::LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        SharedRef<br::PipelineLayoutObject<peridot::DeviceObject>>,
    >,
    invert_pipeline: peridot::LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        SharedRef<br::PipelineLayoutObject<peridot::DeviceObject>>,
    >,
}
impl TwoPassStencilSDFRenderer {
    fn stencil_invert() -> br::vk::VkStencilOpState {
        StencilState::new(br::StencilOp::Invert)
            .with_write_mask(0x01)
            .into_vk()
    }
    fn stencil_match() -> br::vk::VkStencilOpState {
        StencilState::new(br::StencilOp::Keep)
            .with_compare(StencilCompare::new(br::CompareOp::Equal, 0x01).with_mask(0x01))
            .into_vk()
    }
    fn stencil_noop() -> br::vk::VkStencilOpState {
        StencilState::new(br::StencilOp::Keep).into_vk()
    }

    pub fn new(
        e: &peridot::Engine<impl peridot::NativeLinker>,
        color_format: br::vk::VkFormat,
        target_final_layout: br::ImageLayout,
        target_layout_transition_stage: br::PipelineStageFlags,
        init_target_size: peridot::math::Vector2<u32>,
        sdf_max_distance: f32,
    ) -> Self {
        let attachments = [
            br::AttachmentDescription::new(color_format, target_final_layout, target_final_layout)
                .color_memory_op(br::LoadOp::Load, br::StoreOp::Store),
            br::AttachmentDescription::new(
                br::vk::VK_FORMAT_S8_UINT,
                br::ImageLayout::DepthStencilReadOnlyOpt,
                br::ImageLayout::DepthStencilReadOnlyOpt,
            )
            .stencil_load_op(br::LoadOp::Clear),
        ];
        let subpasses = [
            br::SubpassDescription::new()
                .depth_stencil(1, br::ImageLayout::DepthStencilAttachmentOpt),
            br::SubpassDescription::new()
                .add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None)
                .depth_stencil(1, br::ImageLayout::DepthStencilReadOnlyOpt),
        ];
        let spdep_color = br::vk::VkSubpassDependency {
            srcSubpass: br::vk::VK_SUBPASS_EXTERNAL,
            dstSubpass: 0,
            srcStageMask: target_layout_transition_stage.0,
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT
                // Note: LoadOpがClearだとLoad時にWriteが走るらしいのでearlyステージで遷移できてないといけない
                .early_fragment_tests()
                .0,
            srcAccessMask: 0,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write
                | br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
            dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT,
        };
        let spdep_stencil = br::vk::VkSubpassDependency {
            srcSubpass: 0,
            dstSubpass: 1,
            srcStageMask: br::PipelineStageFlags::LATE_FRAGMENT_TESTS.0,
            dstStageMask: br::PipelineStageFlags::EARLY_FRAGMENT_TESTS.0,
            srcAccessMask: br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
            dstAccessMask: br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.read,
            dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT,
        };
        let render_pass = br::RenderPassBuilder::new()
            .add_attachments(attachments)
            .add_subpasses(subpasses)
            .add_dependencies([spdep_color, spdep_stencil])
            .create(e.graphics().device().clone())
            .expect("Failed to create RenderPass");

        let stencil_triangle_vsh_parameters = StencilTriangleVertexShaderParameters {
            target_width: init_target_size.0 as _,
            target_height: init_target_size.1 as _,
        };
        let outline_vsh_parameters = OutlineVertexShaderParameters {
            target_width: init_target_size.0 as _,
            target_height: init_target_size.1 as _,
            sdf_max_distance,
        };
        let fill_fsh_color_output = FillFragmentShaderParameters {
            enable_color_output: true as _,
        };

        let scissors =
            [br::vk::VkExtent2D::from(init_target_size).into_rect(br::vk::VkOffset2D::ZERO)];
        let viewports = [scissors[0].make_viewport(0.0..1.0)];
        let fill_shader = PvpShaderModules::new(
            e.graphics().device(),
            e.load("builtin.vg.sdf.shaders.triangle_fans")
                .expect("Failed to load triangle_fans shader asset"),
        )
        .expect("Failed to create triangle_fans shader modules");
        let curve_fill_shader = PvpShaderModules::new(
            e.graphics().device(),
            e.load("builtin.vg.sdf.shaders.curve_triangles")
                .expect("Failed to load curve_triangles shader asset"),
        )
        .expect("Failed to create curve_triangles shader modules");
        let outline_shader = PvpShaderModules::new(
            e.graphics().device(),
            e.load("builtin.vg.sdf.shaders.outline_distance")
                .expect("Failed to load outline_distance shader asset"),
        )
        .expect("Failed to create outline_disdtance shader modules");
        let empty_pl = SharedRef::new(
            br::PipelineLayoutBuilder::new(vec![], vec![])
                .create(e.graphics().device().clone())
                .expect("Failed to create empty pipeline layout"),
        );

        let mut stencil_triangle_shader =
            fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_triangle_shader.vertex_shader_mut().specinfo =
            Some(stencil_triangle_vsh_parameters.as_pair());
        let mut pipebuild = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(&empty_pl, (&render_pass, 0), stencil_triangle_shader);
        pipebuild
            .viewport_scissors(
                br::DynamicArrayState::Static(&viewports),
                br::DynamicArrayState::Static(&scissors),
            )
            .multisample_state(Some(br::MultisampleState::new()))
            .stencil_control(Self::stencil_invert())
            .stencil_test_enable(true)
            .set_attachment_blends(vec![ColorAttachmentBlending::Disabled.into_vk()]);
        let triangle_fans_stencil_pipeline = pipebuild
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Triangle Fans Stencil Pipeline");
        let mut stencil_curve_shader =
            curve_fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_curve_shader.vertex_shader_mut().specinfo =
            Some(stencil_triangle_vsh_parameters.as_pair());
        pipebuild.vertex_processing(stencil_curve_shader);
        let curve_triangles_stencil_pipeline = pipebuild
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Curve Triangles Stencil Pipeline");
        let mut invert_fill_shader =
            fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        invert_fill_shader
            .fragment_shader_mut()
            .expect("no fragment shader?")
            .specinfo = Some(fill_fsh_color_output.as_pair());
        pipebuild
            .render_pass(&render_pass, 1)
            .vertex_processing(invert_fill_shader)
            .stencil_control(Self::stencil_match())
            .set_attachment_blends(vec![ColorAttachmentBlending::new(
                Blending::source_only(br::BlendFactor::OneMinusDestColor),
                Blending::source_only(br::BlendFactor::OneMinusDestAlpha),
            )
            .into_vk()]);
        let invert_pipeline = pipebuild
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Invert Pipeline");
        let mut outline_render_vps =
            outline_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        outline_render_vps.vertex_shader_mut().specinfo = Some(outline_vsh_parameters.as_pair());
        pipebuild
            .vertex_processing(outline_render_vps)
            .stencil_control(Self::stencil_noop())
            .stencil_test_enable(false)
            .set_attachment_blends(vec![ColorAttachmentBlending::MAX.into_vk()]);
        let outline_distance_pipeline = pipebuild
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Outline Distance Pipeline");

        Self {
            render_pass,
            target_size: init_target_size,
            fill_shader,
            curve_fill_shader,
            outline_shader,
            triangle_fans_stencil_pipeline: peridot::LayoutedPipeline::combine(
                triangle_fans_stencil_pipeline,
                empty_pl.clone(),
            ),
            curve_triangles_stencil_pipeline: peridot::LayoutedPipeline::combine(
                curve_triangles_stencil_pipeline,
                empty_pl.clone(),
            ),
            invert_pipeline: peridot::LayoutedPipeline::combine(invert_pipeline, empty_pl.clone()),
            outline_distance_pipeline: peridot::LayoutedPipeline::combine(
                outline_distance_pipeline,
                empty_pl.clone(),
            ),
        }
    }

    pub fn resize(
        &mut self,
        g: &peridot::Graphics,
        new_size: peridot::math::Vector2<u32>,
        sdf_max_distance: f32,
    ) {
        let stencil_vsh_parameters = StencilTriangleVertexShaderParameters {
            target_width: new_size.0 as _,
            target_height: new_size.1 as _,
        };
        let outline_vsh_parameters = OutlineVertexShaderParameters {
            target_width: new_size.0 as _,
            target_height: new_size.1 as _,
            sdf_max_distance,
        };
        let fill_fsh_color_output = FillFragmentShaderParameters {
            enable_color_output: true as _,
        };

        let scissors = [br::vk::VkExtent2D::from(new_size).into_rect(br::vk::VkOffset2D::ZERO)];
        let viewports = [scissors[0].make_viewport(0.0..1.0)];

        let mut stencil_triangle_shader = self
            .fill_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_triangle_shader.vertex_shader_mut().specinfo =
            Some(stencil_vsh_parameters.as_pair());
        let mut pipebuild = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(
            self.triangle_fans_stencil_pipeline.layout(),
            (&self.render_pass, 0),
            stencil_triangle_shader,
        );
        pipebuild
            .viewport_scissors(
                br::DynamicArrayState::Static(&viewports),
                br::DynamicArrayState::Static(&scissors),
            )
            .multisample_state(Some(br::MultisampleState::new()))
            .stencil_control(Self::stencil_invert())
            .stencil_test_enable(true)
            .set_attachment_blends(vec![ColorAttachmentBlending::Disabled.into_vk()]);
        let triangle_fans_stencil_pipeline = pipebuild
            .create(
                g.device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to recreate Triangle Fans Stencil Pipeline");
        let mut stencil_curve_shader = self
            .curve_fill_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_curve_shader.vertex_shader_mut().specinfo = Some(stencil_vsh_parameters.as_pair());
        pipebuild.vertex_processing(stencil_curve_shader);
        let curve_triangles_stencil_pipeline = pipebuild
            .create(
                g.device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Curve Triangles Stencil Pipeline");
        let mut invert_fill_shader = self
            .fill_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        invert_fill_shader
            .fragment_shader_mut()
            .expect("no fragment shader?")
            .specinfo = Some(fill_fsh_color_output.as_pair());
        pipebuild
            .render_pass(&self.render_pass, 1)
            .vertex_processing(invert_fill_shader)
            .stencil_control(Self::stencil_match())
            .set_attachment_blends(vec![ColorAttachmentBlending::new(
                Blending::source_only(br::BlendFactor::OneMinusDestColor),
                Blending::source_only(br::BlendFactor::OneMinusDestAlpha),
            )
            .into_vk()]);
        let invert_pipeline = pipebuild
            .create(
                g.device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Invert Pipeline");
        let mut outline_render_vps = self
            .outline_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        outline_render_vps.vertex_shader_mut().specinfo = Some(outline_vsh_parameters.as_pair());
        pipebuild
            .vertex_processing(outline_render_vps)
            .stencil_control(Self::stencil_noop())
            .stencil_test_enable(false)
            .set_attachment_blends(vec![ColorAttachmentBlending::MAX.into_vk()]);
        let outline_distance_pipeline = pipebuild
            .create(
                g.device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Outline Distance Pipeline");

        self.target_size = new_size;
        self.triangle_fans_stencil_pipeline = peridot::LayoutedPipeline::combine(
            triangle_fans_stencil_pipeline,
            self.triangle_fans_stencil_pipeline.layout().clone(),
        );
        self.curve_triangles_stencil_pipeline = peridot::LayoutedPipeline::combine(
            curve_triangles_stencil_pipeline,
            self.curve_triangles_stencil_pipeline.layout().clone(),
        );
        self.invert_pipeline = peridot::LayoutedPipeline::combine(
            invert_pipeline,
            self.invert_pipeline.layout().clone(),
        );
        self.outline_distance_pipeline = peridot::LayoutedPipeline::combine(
            outline_distance_pipeline,
            self.outline_distance_pipeline.layout().clone(),
        );
    }

    pub const fn render_area(&self) -> br::vk::VkRect2D {
        br::vk::VkExtent2D {
            width: self.target_size.0,
            height: self.target_size.1,
        }
        .into_rect(br::vk::VkOffset2D::ZERO)
    }

    pub const CLEAR_VALUES: &'static [br::ClearValue] = &[
        br::ClearValue::color_f32([0.0; 4]), // ignored
        br::ClearValue::depth_stencil(0.0, 0),
    ];
}
pub struct TwoPassStencilSDFRendererBuffers {
    fill_triangle_mesh: StandardIndexedMesh<
        SharedRef<peridot_memory_manager::Buffer>,
        SharedRef<peridot_memory_manager::Buffer>,
    >,
    fill_triangle_groups: Vec<(u32, u32)>,
    curve_triangles_mesh: StandardMesh<SharedRef<peridot_memory_manager::Buffer>>,
    outline_rects_mesh: StandardMesh<SharedRef<peridot_memory_manager::Buffer>>,
    invert_fill_rect_mesh: StandardMesh<SharedRef<peridot_memory_manager::Buffer>>,
}
impl TwoPassStencilSDFRenderer {
    pub fn commands<'s>(
        &'s self,
        framebuffer: &'s impl br::Framebuffer,
        buffers: &'s TwoPassStencilSDFRendererBuffers,
    ) -> impl GraphicsCommand + 's {
        let rp = BeginRenderPass::new(&self.render_pass, framebuffer, self.render_area())
            .with_clear_values(Self::CLEAR_VALUES.into());

        let stencil_fill_triangles_render = buffers
            .fill_triangle_groups
            .iter()
            .fold((vec![], 0), |(mut commands, vo), &(vertices, indices)| {
                commands.push(SimpleDrawIndexed::new(indices, 1).with_vertex_offset(vo as _));
                (commands, vo + vertices)
            })
            .0;
        let stencil_pass = (
            stencil_fill_triangles_render
                .after_of(buffers.fill_triangle_mesh.ref_pre_configure_for_draw())
                .after_of(&self.triangle_fans_stencil_pipeline),
            buffers
                .curve_triangles_mesh
                .ref_draw(1)
                .after_of(&self.curve_triangles_stencil_pipeline),
        );
        let outline_distance_pass = (
            buffers
                .outline_rects_mesh
                .ref_draw(1)
                .after_of(&self.outline_distance_pipeline),
            buffers
                .invert_fill_rect_mesh
                .ref_draw(1)
                .after_of(&self.invert_pipeline),
        );

        (
            stencil_pass,
            NextSubpass::WITH_INLINE_COMMANDS,
            outline_distance_pass,
        )
            .between(rp, EndRenderPass)
    }
}

pub struct Game<NL: peridot::NativeLinker> {
    memory_manager: MemoryManager,
    buffers: TwoPassStencilSDFRendererBuffers,
    stencil_buffer_view: SharedRef<br::ImageViewObject<peridot_memory_manager::Image>>,
    fb: Vec<
        br::FramebufferObject<
            peridot::DeviceObject,
            SharedRef<dyn br::ImageView<ConcreteDevice = peridot::DeviceObject>>,
        >,
    >,
    sdf_renderer: TwoPassStencilSDFRenderer,
    cmd: peridot::CommandBundle<peridot::DeviceObject>,
    ph: std::marker::PhantomData<*const NL>,
}
impl<NL: peridot::NativeLinker> Game<NL> {
    const SDF_SIZE: f32 = 32.0;
}
impl<NL: peridot::NativeLinker> FeatureRequests for Game<NL> {}
impl<NL: peridot::NativeLinker> EngineEvents<NL> for Game<NL> {
    fn init(e: &mut Engine<NL>) -> Self {
        let back_buffer_size = e
            .back_buffer(0)
            .expect("no back-buffer?")
            .image()
            .size()
            .wh();

        let font = peridot_vg::DefaultFontProvider::new()
            .expect("Failed to create font provider")
            .best_match("sans-serif", &peridot_vg::FontProperties::default(), 120.0)
            .expect("no suitable font");
        let gid = font.glyph_id('A').expect("no glyph contained");
        let mut gen = peridot_vg::SDFGenerator::new(1.0, Self::SDF_SIZE);
        let glyph_metrics = font.bounds(&gid).expect("Failed to get glyph bounds");
        font.outline(
            &gid,
            &peridot_vg::sdf_generator::Transform2D::create_translation(
                -glyph_metrics.origin.x + Self::SDF_SIZE,
                -glyph_metrics.origin.y - Self::SDF_SIZE,
            ),
            &mut gen,
        )
        .expect("Failed to render glyph outline");
        let figure_vertices = gen.build();
        let (
            figure_fill_triangle_points_count,
            figure_fill_triangle_indices_count,
            figure_curve_triangles_count,
            outline_rects_count,
        ) = figure_vertices
            .iter()
            .fold((0, 0, 0, 0), |(t, t2, t3, t4), f| {
                (
                    t + f.fill_triangle_points.len(),
                    t2 + f.fill_triangle_indices.len(),
                    t3 + f.curve_triangles.len(),
                    t4 + f.parabola_rects.len(),
                )
            });

        let mut memory_manager = MemoryManager::new(e.graphics());

        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let flip_fill_rect = bp.add(peridot::BufferContent::vertex::<
            [peridot::math::Vector2<f32>; 4],
        >());
        let figures_fill_triangle_points_offset =
            bp.add(peridot::BufferContent::vertices::<
                peridot::math::Vector2<f32>,
            >(figure_fill_triangle_points_count));
        let figures_fill_triangle_indices_offset = bp.add(peridot::BufferContent::indices::<u16>(
            figure_fill_triangle_indices_count,
        ));
        let figure_curve_triangles_offset = bp.add(peridot::BufferContent::vertices::<
            peridot::VertexUV2D,
        >(figure_curve_triangles_count));
        let outline_rects_offset = bp.add(peridot::BufferContent::vertices::<
            peridot_vg::sdf_generator::ParabolaRectVertex,
        >(outline_rects_count * 6));

        let buffer = SharedRef::new(
            memory_manager
                .allocate_device_local_buffer(
                    e.graphics(),
                    bp.build_desc().and_usage(br::BufferUsage::TRANSFER_DEST),
                )
                .expect("Failed to allocate buffer"),
        );
        let mut buffer_init: RangedBuffer<_> = memory_manager
            .allocate_upload_buffer(
                e.graphics(),
                bp.build_desc_custom_usage(br::BufferUsage::TRANSFER_SRC),
            )
            .expect("Failed to allocate init buffer")
            .into();
        let stencil_buffer = memory_manager
            .allocate_device_local_image(
                e.graphics(),
                br::ImageDesc::new(
                    back_buffer_size.clone(),
                    br::vk::VK_FORMAT_S8_UINT,
                    br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
                    br::ImageLayout::Undefined,
                ),
            )
            .expect("Failed to allocate stencil buffer");
        let stencil_buffer_view = SharedRef::new(
            stencil_buffer
                .subresource_range(br::AspectMask::STENCIL, 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create Stencil Buffer View"),
        );

        buffer_init
            .0
            .guard_map(BufferMapMode::Write, |m| unsafe {
                m.clone_slice_to(
                    flip_fill_rect as _,
                    &[
                        peridot::math::Vector2(0.0f32, 0.0),
                        peridot::math::Vector2(1.0, 0.0),
                        peridot::math::Vector2(0.0, -1.0),
                        peridot::math::Vector2(1.0, -1.0),
                    ],
                );

                let s = m.slice_mut(
                    figures_fill_triangle_points_offset as _,
                    figure_fill_triangle_points_count,
                );
                let si = m.slice_mut(
                    figures_fill_triangle_indices_offset as _,
                    figure_fill_triangle_indices_count,
                );
                let c = m.slice_mut(
                    figure_curve_triangles_offset as _,
                    figure_curve_triangles_count,
                );
                let o = m.slice_mut(outline_rects_offset as _, outline_rects_count * 6);
                let (mut s_offset, mut si_offset, mut c_offset, mut o_offset) = (0, 0, 0, 0);
                for f in figure_vertices.iter() {
                    s[s_offset..s_offset + f.fill_triangle_points.len()]
                        .clone_from_slice(&f.fill_triangle_points);
                    si[si_offset..si_offset + f.fill_triangle_indices.len()]
                        .copy_from_slice(&f.fill_triangle_indices);
                    c[c_offset..c_offset + f.curve_triangles.len()]
                        .clone_from_slice(&f.curve_triangles);
                    for pr in f.parabola_rects.iter() {
                        o[o_offset..o_offset + 6].clone_from_slice(&pr.make_vertices());
                        o_offset += 6;
                    }
                    s_offset += f.fill_triangle_points.len();
                    si_offset += f.fill_triangle_indices.len();
                    c_offset += f.curve_triangles.len();
                }
            })
            .expect("Failed to set init data");

        {
            let all_buffer = RangedBuffer::from(&*buffer);
            let stencil_buffer = RangedImage::single_stencil_plane(stencil_buffer_view.image());

            let copy = all_buffer.byref_mirror_from(&buffer_init);

            let [all_buffer_in_barrier, all_buffer_out_barrier] =
                all_buffer.clone().usage_barrier3(
                    BufferUsage::UNUSED,
                    BufferUsage::TRANSFER_DST,
                    BufferUsage::VERTEX_BUFFER | BufferUsage::INDEX_BUFFER,
                );
            let in_barriers = [
                buffer_init
                    .make_ref()
                    .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                all_buffer_in_barrier,
            ];
            let out_barriers = PipelineBarrier::new()
                .with_barrier(all_buffer_out_barrier)
                .with_barrier(stencil_buffer.barrier(
                    br::ImageLayout::Undefined,
                    br::ImageLayout::DepthStencilReadOnlyOpt,
                ))
                .by_region();

            copy.between(in_barriers, out_barriers)
                .submit(e)
                .expect("Failed to initialize resources");
        }

        let figures_fill_triangle_points_buffer = RangedBuffer::from_offset_length(
            buffer.clone(),
            figures_fill_triangle_points_offset,
            core::mem::size_of::<peridot::math::Vector2<f32>>() * figure_fill_triangle_points_count,
        );
        let figures_fill_triangle_indices_buffer = RangedBuffer::from_offset_length(
            buffer.clone(),
            figures_fill_triangle_indices_offset,
            core::mem::size_of::<u16>() * figure_fill_triangle_indices_count,
        );
        let figures_curve_triangles_buffer = RangedBuffer::from_offset_length(
            buffer.clone(),
            figure_curve_triangles_offset,
            core::mem::size_of::<peridot::VertexUV2D>() * figure_curve_triangles_count,
        );
        let outline_rects_buffer = RangedBuffer::from_offset_length(
            buffer.clone(),
            outline_rects_offset,
            core::mem::size_of::<peridot_vg::sdf_generator::ParabolaRectVertex>()
                * outline_rects_count,
        );
        let flip_fill_rect_buffer =
            RangedBuffer::for_type::<[peridot::math::Vector2<f32>; 4]>(buffer, flip_fill_rect as _);

        let sdf_renderer = TwoPassStencilSDFRenderer::new(
            e,
            e.back_buffer_format(),
            e.requesting_back_buffer_layout().0,
            e.requesting_back_buffer_layout().1,
            back_buffer_size.clone().into(),
            Self::SDF_SIZE,
        );

        let fb = e
            .iter_back_buffers()
            .map(|bb| {
                e.graphics().device().clone().new_framebuffer(
                    &sdf_renderer.render_pass,
                    vec![
                        bb.clone()
                            as SharedRef<dyn br::ImageView<ConcreteDevice = peridot::DeviceObject>>,
                        stencil_buffer_view.clone(),
                    ],
                    &back_buffer_size,
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Failed to create Framebuffers");

        let fill_triangle_groups: Vec<_> = figure_vertices
            .iter()
            .map(|f| {
                (
                    f.fill_triangle_points.len() as u32,
                    f.fill_triangle_indices.len() as u32,
                )
            })
            .collect();
        let buffers = TwoPassStencilSDFRendererBuffers {
            fill_triangle_mesh: StandardIndexedMesh {
                vertex_buffers: vec![figures_fill_triangle_points_buffer],
                index_buffer: figures_fill_triangle_indices_buffer,
                index_type: br::IndexType::U16,
                vertex_count: 0, // ignored value
            },
            fill_triangle_groups,
            curve_triangles_mesh: StandardMesh {
                vertex_buffers: vec![figures_curve_triangles_buffer],
                vertex_count: figure_curve_triangles_count as _,
            },
            outline_rects_mesh: StandardMesh {
                vertex_buffers: vec![outline_rects_buffer],
                vertex_count: (outline_rects_count * 6) as _,
            },
            invert_fill_rect_mesh: StandardMesh {
                vertex_buffers: vec![flip_fill_rect_buffer],
                vertex_count: 4,
            },
        };
        let mut cmd = peridot::CommandBundle::new(
            e.graphics(),
            peridot::CBSubmissionType::Graphics,
            e.back_buffer_count(),
        )
        .expect("Failed to create CommandBundle");
        for (cx, fb) in fb.iter().enumerate() {
            sdf_renderer
                .commands(fb, &buffers)
                .execute_and_finish(unsafe {
                    cmd[cx]
                        .begin()
                        .expect("Failed to begin recording commands")
                        .as_dyn_ref()
                })
                .expect("Failed to record commands");
        }

        Self {
            memory_manager,
            buffers,
            stencil_buffer_view,
            sdf_renderer,
            fb,
            cmd,
            ph: std::marker::PhantomData,
        }
    }

    fn update(
        &mut self,
        e: &mut Engine<NL>,
        on_back_buffer_of: u32,
        _delta_time: std::time::Duration,
    ) {
        e.do_render(
            on_back_buffer_of,
            None::<br::EmptySubmissionBatch>,
            br::EmptySubmissionBatch.with_command_buffers(
                &self.cmd[on_back_buffer_of as usize..=on_back_buffer_of as usize],
            ),
        )
        .expect("Failed to present");
    }

    fn discard_back_buffer_resources(&mut self) {
        self.fb.clear();
    }
    fn on_resize(&mut self, e: &mut peridot::Engine<NL>, new_size: peridot::math::Vector2<usize>) {
        // rebuild font meshes
        let font = peridot_vg::DefaultFontProvider::new()
            .expect("Failed to create font provider")
            .best_match(
                "MS UI Gothic",
                &peridot_vg::FontProperties::default(),
                120.0,
            )
            .expect("no suitable font");
        let gid = font.glyph_id('A').expect("no glyph contained");
        let mut gen = peridot_vg::SDFGenerator::new(1.0, Self::SDF_SIZE);
        let glyph_metrics = font.bounds(&gid).expect("Failed to get glyph bounds");
        font.outline(
            &gid,
            &peridot_vg::sdf_generator::Transform2D::create_translation(
                -glyph_metrics.origin.x + Self::SDF_SIZE,
                -glyph_metrics.origin.y - Self::SDF_SIZE,
            ),
            &mut gen,
        )
        .expect("Failed to render glyph outline");
        let figure_vertices = gen.build();
        let (
            figure_fill_triangle_points_count,
            figure_fill_triangle_indices_count,
            figure_curve_triangles_count,
            outline_rects_count,
        ) = figure_vertices
            .iter()
            .fold((0, 0, 0, 0), |(t, t2, t3, t4), f| {
                (
                    t + f.fill_triangle_points.len(),
                    t2 + f.fill_triangle_indices.len(),
                    t3 + f.curve_triangles.len(),
                    t4 + f.parabola_rects.len(),
                )
            });

        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let flip_fill_rect = bp.add(peridot::BufferContent::vertex::<
            [peridot::math::Vector2<f32>; 4],
        >());
        let figures_fill_triangle_points_offset =
            bp.add(peridot::BufferContent::vertices::<
                peridot::math::Vector2<f32>,
            >(figure_fill_triangle_points_count));
        let figures_fill_triangle_indices_offset = bp.add(peridot::BufferContent::indices::<u16>(
            figure_fill_triangle_indices_count,
        ));
        let figure_curve_triangles_offset = bp.add(peridot::BufferContent::vertices::<
            peridot::VertexUV2D,
        >(figure_curve_triangles_count));
        let outline_rects_offset = bp.add(peridot::BufferContent::vertices::<
            peridot_vg::sdf_generator::ParabolaRectVertex,
        >(outline_rects_count * 6));

        let buffer = SharedRef::new(
            self.memory_manager
                .allocate_device_local_buffer(
                    e.graphics(),
                    bp.build_desc().and_usage(br::BufferUsage::TRANSFER_DEST),
                )
                .expect("Failed to allocate buffer"),
        );
        let mut buffer_init: RangedBuffer<_> = self
            .memory_manager
            .allocate_upload_buffer(
                e.graphics(),
                bp.build_desc_custom_usage(br::BufferUsage::TRANSFER_SRC),
            )
            .expect("Failed to allocate init buffer")
            .into();
        let stencil_buffer = self
            .memory_manager
            .allocate_device_local_image(
                e.graphics(),
                br::ImageDesc::new(
                    peridot::math::Vector2(new_size.0 as u32, new_size.1 as u32),
                    br::vk::VK_FORMAT_S8_UINT,
                    br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
                    br::ImageLayout::Undefined,
                ),
            )
            .expect("Failed to allocate stencil buffer");
        self.stencil_buffer_view = SharedRef::new(
            stencil_buffer
                .subresource_range(br::AspectMask::STENCIL, 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create Stencil Buffer View"),
        );

        buffer_init
            .0
            .guard_map(BufferMapMode::Write, |m| unsafe {
                m.clone_slice_to(
                    flip_fill_rect as _,
                    &[
                        peridot::math::Vector2(0.0f32, 0.0),
                        peridot::math::Vector2(1.0, 0.0),
                        peridot::math::Vector2(0.0, -1.0),
                        peridot::math::Vector2(1.0, -1.0),
                    ],
                );

                let s = m.slice_mut(
                    figures_fill_triangle_points_offset as _,
                    figure_fill_triangle_points_count,
                );
                let si = m.slice_mut(
                    figures_fill_triangle_indices_offset as _,
                    figure_fill_triangle_indices_count,
                );
                let c = m.slice_mut(
                    figure_curve_triangles_offset as _,
                    figure_curve_triangles_count,
                );
                let o = m.slice_mut(outline_rects_offset as _, outline_rects_count * 6);
                let (mut s_offset, mut si_offset, mut c_offset, mut o_offset) = (0, 0, 0, 0);
                for f in figure_vertices.iter() {
                    s[s_offset..s_offset + f.fill_triangle_points.len()]
                        .clone_from_slice(&f.fill_triangle_points);
                    si[si_offset..si_offset + f.fill_triangle_indices.len()]
                        .copy_from_slice(&f.fill_triangle_indices);
                    c[c_offset..c_offset + f.curve_triangles.len()]
                        .clone_from_slice(&f.curve_triangles);
                    for pr in f.parabola_rects.iter() {
                        o[o_offset..o_offset + 6].clone_from_slice(&pr.make_vertices());
                        o_offset += 6;
                    }
                    s_offset += f.fill_triangle_points.len();
                    si_offset += f.fill_triangle_indices.len();
                    c_offset += f.curve_triangles.len();
                }
            })
            .expect("Failed to set init data");

        self.fb = e
            .iter_back_buffers()
            .map(|bb| {
                e.graphics().device().clone().new_framebuffer(
                    &self.sdf_renderer.render_pass,
                    vec![
                        bb.clone()
                            as SharedRef<dyn br::ImageView<ConcreteDevice = peridot::DeviceObject>>,
                        self.stencil_buffer_view.clone(),
                    ],
                    bb.image().size().as_ref(),
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Failed to create Framebuffers");

        {
            let stg_copied_buffer = buffer_init.subslice_ref(0..bp.total_size() as _);
            let all_buffer = RangedBuffer::from_offset_length(&*buffer, 0, bp.total_size() as _);
            let stencil_buffer =
                RangedImage::single_stencil_plane(self.stencil_buffer_view.image());

            let copy = all_buffer.byref_mirror_from(&stg_copied_buffer);

            let [all_buffer_in_barrier, all_buffer_out_barrier] =
                all_buffer.make_ref().usage_barrier3(
                    BufferUsage::UNUSED,
                    BufferUsage::TRANSFER_DST,
                    BufferUsage::VERTEX_BUFFER | BufferUsage::INDEX_BUFFER,
                );
            let in_barriers = [
                stg_copied_buffer
                    .make_ref()
                    .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                all_buffer_in_barrier,
            ];
            let out_barriers = PipelineBarrier::new()
                .with_barrier(all_buffer_out_barrier)
                .with_barrier(stencil_buffer.barrier(
                    br::ImageLayout::Undefined,
                    br::ImageLayout::DepthStencilReadOnlyOpt,
                ))
                .by_region();

            copy.between(in_barriers, out_barriers)
                .submit(e)
                .expect("Failed to initialize resources");
        }

        let figures_fill_triangle_points_buffer = RangedBuffer::from_offset_length(
            buffer.clone(),
            figures_fill_triangle_points_offset,
            core::mem::size_of::<peridot::math::Vector2<f32>>() * figure_fill_triangle_points_count,
        );
        let figures_fill_triangle_indices_buffer = RangedBuffer::from_offset_length(
            buffer.clone(),
            figures_fill_triangle_indices_offset,
            core::mem::size_of::<u16>() * figure_fill_triangle_indices_count,
        );
        let figures_curve_triangles_buffer = RangedBuffer::from_offset_length(
            buffer.clone(),
            figure_curve_triangles_offset,
            core::mem::size_of::<peridot::VertexUV2D>() * figure_curve_triangles_count,
        );
        let outline_rects_buffer = RangedBuffer::from_offset_length(
            buffer.clone(),
            outline_rects_offset,
            core::mem::size_of::<peridot_vg::sdf_generator::ParabolaRectVertex>()
                * outline_rects_count,
        );
        let flip_fill_rect_buffer =
            RangedBuffer::for_type::<[peridot::math::Vector2<f32>; 4]>(buffer, flip_fill_rect as _);

        self.sdf_renderer.resize(
            e.graphics(),
            peridot::math::Vector2(new_size.0 as _, new_size.1 as _),
            Self::SDF_SIZE,
        );

        let fill_triangle_groups: Vec<_> = figure_vertices
            .iter()
            .map(|f| {
                (
                    f.fill_triangle_points.len() as u32,
                    f.fill_triangle_indices.len() as u32,
                )
            })
            .collect();
        self.buffers = TwoPassStencilSDFRendererBuffers {
            fill_triangle_mesh: StandardIndexedMesh {
                vertex_buffers: vec![figures_fill_triangle_points_buffer],
                index_buffer: figures_fill_triangle_indices_buffer,
                index_type: br::IndexType::U16,
                vertex_count: 0, // ignored value
            },
            fill_triangle_groups,
            curve_triangles_mesh: StandardMesh {
                vertex_buffers: vec![figures_curve_triangles_buffer],
                vertex_count: figure_curve_triangles_count as _,
            },
            outline_rects_mesh: StandardMesh {
                vertex_buffers: vec![outline_rects_buffer],
                vertex_count: (outline_rects_count * 6) as _,
            },
            invert_fill_rect_mesh: StandardMesh {
                vertex_buffers: vec![flip_fill_rect_buffer],
                vertex_count: 4,
            },
        };
        self.cmd = peridot::CommandBundle::new(
            e.graphics(),
            peridot::CBSubmissionType::Graphics,
            e.back_buffer_count(),
        )
        .expect("Failed to create CommandBundle");
        for (cx, fb) in self.fb.iter().enumerate() {
            self.sdf_renderer
                .commands(fb, &self.buffers)
                .execute_and_finish(unsafe {
                    self.cmd[cx]
                        .begin()
                        .expect("Failed to begin recording commands")
                        .as_dyn_ref()
                })
                .expect("Failed to record commands");
        }
    }
}
