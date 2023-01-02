use bedrock as br;
use br::{CommandBuffer, Device, Image, ImageChild, SubmissionBatch};
use peridot::mthelper::SharedRef;
use peridot::SpecConstantStorage;
use peridot::{Engine, EngineEvents, FeatureRequests};
use peridot_vertex_processing_pack::PvpShaderModules;
use peridot_vg::FlatPathBuilder;

#[derive(peridot_derive::SpecConstantStorage)]
#[repr(C)]
pub struct FillFragmentShaderParameters {
    enable_color_output: bool,
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
    const STENCIL_INVERT: br::vk::VkStencilOpState = br::vk::VkStencilOpState {
        failOp: br::vk::VK_STENCIL_OP_INVERT,
        depthFailOp: br::vk::VK_STENCIL_OP_INVERT,
        passOp: br::vk::VK_STENCIL_OP_INVERT,
        compareOp: br::vk::VK_COMPARE_OP_ALWAYS,
        compareMask: 0,
        writeMask: 0x01,
        reference: 0,
    };
    const STENCIL_MATCH: br::vk::VkStencilOpState = br::vk::VkStencilOpState {
        failOp: br::vk::VK_STENCIL_OP_KEEP,
        depthFailOp: br::vk::VK_STENCIL_OP_KEEP,
        passOp: br::vk::VK_STENCIL_OP_ZERO,
        compareOp: br::vk::VK_COMPARE_OP_EQUAL,
        compareMask: 0x01,
        writeMask: 0x01,
        reference: 0x01,
    };
    const STENCIL_NOOP: br::vk::VkStencilOpState = br::vk::VkStencilOpState {
        failOp: br::vk::VK_STENCIL_OP_KEEP,
        depthFailOp: br::vk::VK_STENCIL_OP_KEEP,
        passOp: br::vk::VK_STENCIL_OP_KEEP,
        compareOp: br::vk::VK_COMPARE_OP_ALWAYS,
        compareMask: 0,
        writeMask: 0,
        reference: 0,
    };

    pub fn new<NL: peridot::NativeLinker>(
        e: &peridot::Engine<NL>,
        color_format: br::vk::VkFormat,
        target_final_layout: br::ImageLayout,
        target_layout_transition_stage: br::PipelineStageFlags,
        init_target_size: peridot::math::Vector2<u32>,
        sdf_max_distance: f32,
    ) -> Self {
        let ad_main =
            br::AttachmentDescription::new(color_format, target_final_layout, target_final_layout)
                .load_op(br::LoadOp::Load)
                .store_op(br::StoreOp::Store);
        let ad_stencil = br::AttachmentDescription::new(
            br::vk::VK_FORMAT_S8_UINT,
            br::ImageLayout::DepthStencilReadOnlyOpt,
            br::ImageLayout::DepthStencilReadOnlyOpt,
        )
        .stencil_load_op(br::LoadOp::Clear)
        .stencil_store_op(br::StoreOp::DontCare);
        let sp_stencil = br::SubpassDescription::new()
            .depth_stencil(1, br::ImageLayout::DepthStencilAttachmentOpt);
        let sp_main = br::SubpassDescription::new()
            .add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None)
            .depth_stencil(1, br::ImageLayout::DepthStencilReadOnlyOpt);
        let spdep_color = br::vk::VkSubpassDependency {
            srcSubpass: br::vk::VK_SUBPASS_EXTERNAL,
            dstSubpass: 0,
            srcStageMask: target_layout_transition_stage.0,
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT
                .late_fragment_tests()
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
            .add_attachments(vec![ad_main, ad_stencil])
            .add_subpasses(vec![sp_stencil, sp_main])
            .add_dependencies(vec![spdep_color, spdep_stencil])
            .create(e.graphics().device().clone())
            .expect("Failed to create RenderPass");

        let vertex_shader_parameter_values = [
            init_target_size.0 as f32,
            init_target_size.1 as _,
            sdf_max_distance,
        ];
        let vertex_shader_common_parameter_placements = [
            br::vk::VkSpecializationMapEntry {
                constantID: 0,
                offset: 0,
                size: std::mem::size_of::<f32>() as _,
            },
            br::vk::VkSpecializationMapEntry {
                constantID: 1,
                offset: std::mem::size_of::<f32>() as _,
                size: std::mem::size_of::<f32>() as _,
            },
        ];
        let outline_vsh_parameter_placements = [
            vertex_shader_common_parameter_placements[0].clone(),
            vertex_shader_common_parameter_placements[1].clone(),
            br::vk::VkSpecializationMapEntry {
                constantID: 2,
                offset: (std::mem::size_of::<f32>() * 2) as _,
                size: std::mem::size_of::<f32>() as _,
            },
        ];
        let fill_fsh_color_output = FillFragmentShaderParameters {
            enable_color_output: true,
        };

        let scissors =
            [br::vk::VkExtent2D::from(init_target_size)
                .into_rect(br::vk::VkOffset2D { x: 0, y: 0 })];
        let viewports = [br::vk::VkViewport::from_rect_with_depth_range(
            &scissors[0],
            0.0..1.0,
        )];
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
            e.graphics()
                .device()
                .clone()
                .new_pipeline_layout(
                    &[] as &[br::DescriptorSetLayoutObject<peridot::DeviceObject>],
                    &[],
                )
                .expect("Failed to create empty pipeline layout"),
        );

        let mut stencil_triangle_shader =
            fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_triangle_shader.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&vertex_shader_common_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values),
        ));
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
            .stencil_control_front(Self::STENCIL_INVERT)
            .stencil_control_back(Self::STENCIL_INVERT)
            .stencil_test_enable(true)
            .add_attachment_blend(br::AttachmentColorBlendState::noblend());
        let triangle_fans_stencil_pipeline = pipebuild
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Triangle Fans Stencil Pipeline");
        let mut stencil_curve_shader =
            curve_fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_curve_shader.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&vertex_shader_common_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values),
        ));
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
            .stencil_control_front(Self::STENCIL_MATCH)
            .stencil_control_back(Self::STENCIL_MATCH)
            .set_attachment_blends(vec![br::vk::VkPipelineColorBlendAttachmentState {
                blendEnable: true as _,
                srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR,
                dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_ZERO,
                colorBlendOp: br::vk::VK_BLEND_OP_ADD,
                srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
                dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ZERO,
                alphaBlendOp: br::vk::VK_BLEND_OP_ADD,
                colorWriteMask: br::vk::VK_COLOR_COMPONENT_R_BIT
                    | br::vk::VK_COLOR_COMPONENT_G_BIT
                    | br::vk::VK_COLOR_COMPONENT_B_BIT
                    | br::vk::VK_COLOR_COMPONENT_A_BIT,
            }]);
        let invert_pipeline = pipebuild
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Invert Pipeline");
        let mut outline_render_vps =
            outline_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        outline_render_vps.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&outline_vsh_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values),
        ));
        pipebuild
            .vertex_processing(outline_render_vps)
            .stencil_control_front(Self::STENCIL_NOOP)
            .stencil_control_back(Self::STENCIL_NOOP)
            .stencil_test_enable(false)
            .set_attachment_blends(vec![br::vk::VkPipelineColorBlendAttachmentState {
                blendEnable: true as _,
                srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_COLOR,
                dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_DST_COLOR,
                colorBlendOp: br::vk::VK_BLEND_OP_MAX,
                srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
                dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_DST_ALPHA,
                alphaBlendOp: br::vk::VK_BLEND_OP_MAX,
                colorWriteMask: br::vk::VK_COLOR_COMPONENT_R_BIT
                    | br::vk::VK_COLOR_COMPONENT_G_BIT
                    | br::vk::VK_COLOR_COMPONENT_B_BIT
                    | br::vk::VK_COLOR_COMPONENT_A_BIT,
            }]);
        let outline_distance_pipeline = pipebuild
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Outline Distance Pipeline");

        TwoPassStencilSDFRenderer {
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
        let vertex_shader_parameter_values = [new_size.0 as f32, new_size.1 as _, sdf_max_distance];
        let vertex_shader_common_parameter_placements = [
            br::vk::VkSpecializationMapEntry {
                constantID: 0,
                offset: 0,
                size: std::mem::size_of::<f32>() as _,
            },
            br::vk::VkSpecializationMapEntry {
                constantID: 1,
                offset: std::mem::size_of::<f32>() as _,
                size: std::mem::size_of::<f32>() as _,
            },
        ];
        let outline_vsh_parameter_placements = [
            vertex_shader_common_parameter_placements[0].clone(),
            vertex_shader_common_parameter_placements[1].clone(),
            br::vk::VkSpecializationMapEntry {
                constantID: 2,
                offset: (std::mem::size_of::<f32>() * 2) as _,
                size: std::mem::size_of::<f32>() as _,
            },
        ];
        let fill_fsh_color_output = FillFragmentShaderParameters {
            enable_color_output: true,
        };

        let scissors = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D { x: 0, y: 0 },
            extent: br::vk::VkExtent2D {
                width: new_size.0,
                height: new_size.1,
            },
        }];
        let viewports = [br::vk::VkViewport::from_rect_with_depth_range(
            &scissors[0],
            0.0..1.0,
        )];

        let mut stencil_triangle_shader = self
            .fill_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_triangle_shader.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&vertex_shader_common_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values),
        ));
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
            .stencil_control_front(Self::STENCIL_INVERT)
            .stencil_control_back(Self::STENCIL_INVERT)
            .stencil_test_enable(true)
            .add_attachment_blend(br::AttachmentColorBlendState::noblend());
        let triangle_fans_stencil_pipeline = pipebuild
            .create(
                g.device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to recreate Triangle Fans Stencil Pipeline");
        let mut stencil_curve_shader = self
            .curve_fill_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        stencil_curve_shader.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&vertex_shader_common_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values),
        ));
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
            .stencil_control_front(Self::STENCIL_MATCH)
            .stencil_control_back(Self::STENCIL_MATCH)
            .set_attachment_blends(vec![br::vk::VkPipelineColorBlendAttachmentState {
                blendEnable: true as _,
                srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR,
                dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_ZERO,
                colorBlendOp: br::vk::VK_BLEND_OP_ADD,
                srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA,
                dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ZERO,
                alphaBlendOp: br::vk::VK_BLEND_OP_ADD,
                colorWriteMask: br::vk::VK_COLOR_COMPONENT_R_BIT
                    | br::vk::VK_COLOR_COMPONENT_G_BIT
                    | br::vk::VK_COLOR_COMPONENT_B_BIT
                    | br::vk::VK_COLOR_COMPONENT_A_BIT,
            }]);
        let invert_pipeline = pipebuild
            .create(
                g.device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create Invert Pipeline");
        let mut outline_render_vps = self
            .outline_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        outline_render_vps.vertex_shader_mut().specinfo = Some((
            std::borrow::Cow::Borrowed(&outline_vsh_parameter_placements),
            br::DynamicDataCell::from_slice(&vertex_shader_parameter_values),
        ));
        pipebuild
            .vertex_processing(outline_render_vps)
            .stencil_control_front(Self::STENCIL_NOOP)
            .stencil_control_back(Self::STENCIL_NOOP)
            .stencil_test_enable(false)
            .set_attachment_blends(vec![br::vk::VkPipelineColorBlendAttachmentState {
                blendEnable: true as _,
                srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_COLOR,
                dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_DST_COLOR,
                colorBlendOp: br::vk::VK_BLEND_OP_MAX,
                srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
                dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_DST_ALPHA,
                alphaBlendOp: br::vk::VK_BLEND_OP_MAX,
                colorWriteMask: br::vk::VK_COLOR_COMPONENT_R_BIT
                    | br::vk::VK_COLOR_COMPONENT_G_BIT
                    | br::vk::VK_COLOR_COMPONENT_B_BIT
                    | br::vk::VK_COLOR_COMPONENT_A_BIT,
            }]);
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
        br::vk::VkRect2D {
            offset: br::vk::VkOffset2D { x: 0, y: 0 },
            extent: br::vk::VkExtent2D {
                width: self.target_size.0,
                height: self.target_size.1,
            },
        }
    }
    pub const CLEAR_VALUES: &'static [br::ClearValue] = &[
        br::ClearValue::color_f32([0.0; 4]), // ignored
        br::ClearValue::depth_stencil(0.0, 0),
    ];
}
pub struct TwoPassStencilSDFRendererBuffers<'b> {
    buffer: &'b br::BufferObject<peridot::DeviceObject>,
    fill_triangle_points_offset: u64,
    fill_triangle_indices_offset: u64,
    fill_triangle_groups: &'b [(u32, u32)],
    curve_triangles_offset: u64,
    curve_triangles_count: u32,
    outline_rects_offset: u64,
    outline_rects_count: u32,
    invert_fill_rect_offset: u64,
}
impl TwoPassStencilSDFRenderer {
    pub fn populate_commands(
        &self,
        rec: &mut br::CmdRecord<impl br::CommandBuffer + ?Sized>,
        framebuffer: &impl br::Framebuffer,
        buffers: &TwoPassStencilSDFRendererBuffers,
    ) {
        // Stencil Pass
        rec.begin_render_pass(
            &self.render_pass,
            framebuffer,
            self.render_area(),
            Self::CLEAR_VALUES,
            true,
        );
        self.triangle_fans_stencil_pipeline.bind(rec);
        rec.bind_vertex_buffers(
            0,
            &[(&buffers.buffer, buffers.fill_triangle_points_offset as _)],
        )
        .bind_index_buffer(
            buffers.buffer,
            buffers.fill_triangle_indices_offset as _,
            br::IndexType::U16,
        );
        let mut vo = 0;
        for &(vertices, indices) in buffers.fill_triangle_groups.iter() {
            rec.draw_indexed(indices, 1, 0, vo as i32, 0);
            vo += vertices;
        }
        rec.bind_graphics_pipeline(self.curve_triangles_stencil_pipeline.pipeline())
            .bind_vertex_buffers(0, &[(&buffers.buffer, buffers.curve_triangles_offset as _)])
            .draw(buffers.curve_triangles_count, 1, 0, 0);

        // Outline Distance and Invert Pass
        rec.next_subpass(true)
            .bind_graphics_pipeline(self.outline_distance_pipeline.pipeline())
            .bind_vertex_buffers(0, &[(&buffers.buffer, buffers.outline_rects_offset as _)])
            .draw(buffers.outline_rects_count * 6, 1, 0, 0)
            .bind_graphics_pipeline(self.invert_pipeline.pipeline())
            .bind_vertex_buffers(
                0,
                &[(&buffers.buffer, buffers.invert_fill_rect_offset as _)],
            )
            .draw(4, 1, 0, 0);
    }
}

pub struct Game<NL: peridot::NativeLinker> {
    buffer: SharedRef<
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    stencil_buffer_view: SharedRef<
        br::ImageViewObject<
            peridot::Image<
                br::ImageObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
    >,
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
        let backbuffer_size = AsRef::<br::vk::VkExtent2D>::as_ref(
            e.backbuffer(0).expect("no backbuffer?").image().size(),
        )
        .clone();

        let font = peridot_vg::FontProvider::new()
            .expect("Failed to create font provider")
            .best_match("sans-serif", &peridot_vg::FontProperties::default(), 120.0)
            .expect("no suitable font");
        let gid = font.glyph_id('A').expect("no glyph contained");
        let mut gen = peridot_vg::SDFGenerator::new(1.0, Self::SDF_SIZE);
        let glyph_metrics = font.bounds(gid).expect("Failed to get glyph bounds");
        gen.set_transform(peridot_vg::sdf_generator::Transform2D::create_translation(
            -glyph_metrics.origin.x + Self::SDF_SIZE,
            -glyph_metrics.origin.y - Self::SDF_SIZE,
        ));
        font.outline(gid, &mut gen)
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
        let buffer = bp.build_transferred().expect("Failed to allocate buffer");
        let buffer_init = bp.build_upload().expect("Failed to allocate init buffer");
        let stencil_buffer = br::ImageDesc::new(
            &backbuffer_size,
            br::vk::VK_FORMAT_S8_UINT,
            br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
            br::ImageLayout::Undefined,
        )
        .create(e.graphics().device().clone())
        .expect("Failed to create stencil buffer");

        let mut mb =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(buffer));
        let buffer = mb
            .alloc()
            .expect("Failed to allocate memory")
            .pop()
            .expect("no objects?")
            .unwrap_buffer();
        let buffer = SharedRef::new(buffer);
        let mut mb_upload =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb_upload.add(peridot::MemoryBadgetEntry::Buffer(buffer_init));
        let mut buffer_init = mb_upload
            .alloc_upload()
            .expect("Failed to allocate init buffer memory")
            .pop()
            .expect("no objects?")
            .unwrap_buffer();
        let mut mb_tex =
            peridot::MemoryBadget::<br::BufferObject<peridot::DeviceObject>, _>::new(e.graphics());
        mb_tex.add(peridot::MemoryBadgetEntry::Image(stencil_buffer));
        let stencil_buffer = mb_tex
            .alloc()
            .expect("Failed to allocate Texture Memory")
            .pop()
            .expect("no objects?")
            .unwrap_image();

        let stencil_buffer_view = SharedRef::new(
            stencil_buffer
                .create_view(
                    None,
                    None,
                    &Default::default(),
                    &br::ImageSubresourceRange::stencil(0..1, 0..1),
                )
                .expect("Failed to create Stencil Buffer View"),
        );

        buffer_init
            .guard_map(0..bp.total_size(), |m| unsafe {
                m.slice_mut(flip_fill_rect as _, 4).clone_from_slice(&[
                    peridot::math::Vector2(0.0f32, 0.0),
                    peridot::math::Vector2(1.0, 0.0),
                    peridot::math::Vector2(0.0, -1.0),
                    peridot::math::Vector2(1.0, -1.0),
                ]);

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
        let mut tfb = peridot::TransferBatch::new();
        tfb.add_mirroring_buffer(
            SharedRef::new(buffer_init),
            buffer.clone(),
            0,
            bp.total_size(),
        );
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT,
            buffer.clone(),
            0..bp.total_size(),
            br::AccessFlags::VERTEX_ATTRIBUTE_READ,
        );

        e.submit_commands(|r| {
            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
            r.pipeline_barrier(
                br::PipelineStageFlags::BOTTOM_OF_PIPE,
                br::PipelineStageFlags::LATE_FRAGMENT_TESTS,
                true,
                &[],
                &[],
                &[br::ImageMemoryBarrier::new(
                    stencil_buffer_view.image(),
                    br::ImageSubresourceRange::stencil(0, 0),
                    br::ImageLayout::Undefined,
                    br::ImageLayout::DepthStencilReadOnlyOpt,
                )],
            );
        })
        .expect("Failed to initialize resources");

        let sdf_renderer = TwoPassStencilSDFRenderer::new(
            e,
            e.backbuffer_format(),
            e.requesting_backbuffer_layout().0,
            e.requesting_backbuffer_layout().1,
            peridot::math::Vector2(backbuffer_size.width, backbuffer_size.height),
            Self::SDF_SIZE,
        );

        let fb = e
            .iter_backbuffers()
            .map(|bb| {
                e.graphics().device().clone().new_framebuffer(
                    &sdf_renderer.render_pass,
                    vec![
                        bb.clone()
                            as SharedRef<dyn br::ImageView<ConcreteDevice = peridot::DeviceObject>>,
                        stencil_buffer_view.clone(),
                    ],
                    &backbuffer_size,
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
            buffer: &buffer,
            fill_triangle_points_offset: figures_fill_triangle_points_offset,
            fill_triangle_indices_offset: figures_fill_triangle_indices_offset,
            fill_triangle_groups: &fill_triangle_groups,
            curve_triangles_offset: figure_curve_triangles_offset,
            curve_triangles_count: figure_curve_triangles_count as _,
            outline_rects_offset,
            outline_rects_count: outline_rects_count as _,
            invert_fill_rect_offset: flip_fill_rect,
        };
        let mut cmd = peridot::CommandBundle::new(
            e.graphics(),
            peridot::CBSubmissionType::Graphics,
            e.backbuffer_count(),
        )
        .expect("Failed to create CommandBundle");
        for (cx, fb) in fb.iter().enumerate() {
            let mut rec = unsafe { cmd[cx].begin().expect("Failed to begin recording commands") };
            sdf_renderer.populate_commands(&mut rec, fb, &buffers);
            rec.end_render_pass();
        }

        Game {
            buffer,
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
        on_backbuffer_of: u32,
        _delta_time: std::time::Duration,
    ) {
        e.do_render(
            on_backbuffer_of,
            None::<br::EmptySubmissionBatch>,
            br::EmptySubmissionBatch.with_command_buffers(
                &self.cmd[on_backbuffer_of as usize..=on_backbuffer_of as usize],
            ),
        )
        .expect("Failed to present");
    }

    fn discard_backbuffer_resources(&mut self) {
        self.fb.clear();
    }
    fn on_resize(&mut self, e: &mut peridot::Engine<NL>, new_size: peridot::math::Vector2<usize>) {
        // rebuild font meshes
        let font = peridot_vg::FontProvider::new()
            .expect("Failed to create font provider")
            .best_match(
                "MS UI Gothic",
                &peridot_vg::FontProperties::default(),
                120.0,
            )
            .expect("no suitable font");
        let gid = font.glyph_id('A').expect("no glyph contained");
        let mut gen = peridot_vg::SDFGenerator::new(1.0, Self::SDF_SIZE);
        let glyph_metrics = font.bounds(gid).expect("Failed to get glyph bounds");
        gen.set_transform(peridot_vg::sdf_generator::Transform2D::create_translation(
            -glyph_metrics.origin.x + Self::SDF_SIZE,
            -glyph_metrics.origin.y - Self::SDF_SIZE,
        ));
        font.outline(gid, &mut gen)
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
        let buffer = bp.build_transferred().expect("Failed to allocate buffer");
        let buffer_init = bp.build_upload().expect("Failed to allocate init buffer");
        let mut mb =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(buffer));
        self.buffer = SharedRef::new(
            mb.alloc()
                .expect("Failed to allocate memory")
                .pop()
                .expect("no objects?")
                .unwrap_buffer(),
        );
        let mut mb_upload =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb_upload.add(peridot::MemoryBadgetEntry::Buffer(buffer_init));
        let mut buffer_init = mb_upload
            .alloc_upload()
            .expect("Failed to allocate init buffer memory")
            .pop()
            .expect("no objects?")
            .unwrap_buffer();

        let stencil_buffer = br::ImageDesc::new(
            &br::vk::VkExtent2D {
                width: new_size.0 as _,
                height: new_size.1 as _,
            },
            br::vk::VK_FORMAT_S8_UINT,
            br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
            br::ImageLayout::Undefined,
        )
        .create(e.graphics().device().clone())
        .expect("Failed to create stencil buffer");
        let mut mb_tex =
            peridot::MemoryBadget::<br::BufferObject<peridot::DeviceObject>, _>::new(e.graphics());
        mb_tex.add(peridot::MemoryBadgetEntry::Image(stencil_buffer));
        let stencil_buffer = mb_tex
            .alloc()
            .expect("Failed to allocate Texture Memory")
            .pop()
            .expect("no objects?")
            .unwrap_image();
        self.stencil_buffer_view = SharedRef::new(
            stencil_buffer
                .create_view(
                    None,
                    None,
                    &Default::default(),
                    &br::ImageSubresourceRange::stencil(0..1, 0..1),
                )
                .expect("Failed to create Stencil Buffer View"),
        );
        self.fb = e
            .iter_backbuffers()
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

        buffer_init
            .guard_map(0..bp.total_size(), |m| unsafe {
                m.slice_mut(flip_fill_rect as _, 4).clone_from_slice(&[
                    peridot::math::Vector2(0.0f32, 0.0),
                    peridot::math::Vector2(1.0, 0.0),
                    peridot::math::Vector2(0.0, -1.0),
                    peridot::math::Vector2(1.0, -1.0),
                ]);

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
        let mut tfb = peridot::TransferBatch::new();
        tfb.add_mirroring_buffer(
            SharedRef::new(buffer_init),
            self.buffer.clone(),
            0,
            bp.total_size(),
        );
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT,
            self.buffer.clone(),
            0..bp.total_size(),
            br::AccessFlags::VERTEX_ATTRIBUTE_READ,
        );

        e.submit_commands(|r| {
            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
            r.pipeline_barrier(
                br::PipelineStageFlags::BOTTOM_OF_PIPE,
                br::PipelineStageFlags::LATE_FRAGMENT_TESTS,
                true,
                &[],
                &[],
                &[br::ImageMemoryBarrier::new(
                    self.stencil_buffer_view.image(),
                    br::ImageSubresourceRange::stencil(0, 0),
                    br::ImageLayout::Undefined,
                    br::ImageLayout::DepthStencilReadOnlyOpt,
                )],
            );
        })
        .expect("Failed to initialize resources");

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
        let buffers = TwoPassStencilSDFRendererBuffers {
            buffer: &self.buffer,
            fill_triangle_points_offset: figures_fill_triangle_points_offset,
            fill_triangle_indices_offset: figures_fill_triangle_indices_offset,
            fill_triangle_groups: &fill_triangle_groups,
            curve_triangles_offset: figure_curve_triangles_offset,
            curve_triangles_count: figure_curve_triangles_count as _,
            outline_rects_offset,
            outline_rects_count: outline_rects_count as _,
            invert_fill_rect_offset: flip_fill_rect,
        };
        let mut cmd = peridot::CommandBundle::new(
            e.graphics(),
            peridot::CBSubmissionType::Graphics,
            e.backbuffer_count(),
        )
        .expect("Failed to create CommandBundle");
        for (cx, fb) in self.fb.iter().enumerate() {
            let mut rec = unsafe { cmd[cx].begin().expect("Failed to begin recording commands") };
            self.sdf_renderer.populate_commands(&mut rec, fb, &buffers);
            rec.end_render_pass();
        }
    }
}
