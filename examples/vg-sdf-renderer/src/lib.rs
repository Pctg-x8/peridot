use bedrock::{self as br, AnyRenderPassCreateInfo, Device, VkHandle};
use peridot::mthelper::SharedRef;
use peridot_command_object::{
    BeginRenderPass, Blending, BufferUsage, ColorAttachmentBlending, EndRenderPass,
    GraphicsCommand, GraphicsCommandCombiner, GraphicsCommandSubmission, NextSubpass,
    PipelineBarrier, RangedBuffer, RangedImage, SimpleDrawIndexed, StandardIndexedMesh,
    StandardMesh,
};
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_vertex_processing_pack::{PvpContainer, PvpShaderModules};
use peridot_vg::{FlatPathBuilder, Font, FontProvider, FontProviderConstruct};

#[derive(br::SpecializationConstants)]
#[repr(C)]
pub struct FillFragmentShaderParameters {
    #[constant_id = 0]
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
#[derive(br::SpecializationConstants)]
struct StencilTriangleVertexShaderParameters {
    #[constant_id = 0]
    pub target_width: f32,
    #[constant_id = 1]
    pub target_height: f32,
}

#[repr(C)]
#[derive(br::SpecializationConstants)]
struct OutlineVertexShaderParameters {
    #[constant_id = 0]
    pub target_width: f32,
    #[constant_id = 1]
    pub target_height: f32,
    #[constant_id = 2]
    pub sdf_max_distance: f32,
}

pub struct TwoPassStencilSDFRenderer {
    gfx_device: peridot::VulkanGfx,
    render_pass: br::vk::VkRenderPass,
    target_size: peridot::math::Vector2<u32>,
    fill_shader: PvpContainer,
    fill_shader_modules: PvpShaderModules<peridot::VulkanGfx>,
    curve_fill_shader: PvpContainer,
    curve_fill_shader_modules: PvpShaderModules<peridot::VulkanGfx>,
    outline_shader: PvpContainer,
    outline_shader_modules: PvpShaderModules<peridot::VulkanGfx>,
    pipeline_layout: br::vk::VkPipelineLayout,
    triangle_fans_stencil_pipeline: br::vk::VkPipeline,
    curve_triangles_stencil_pipeline: br::vk::VkPipeline,
    outline_distance_pipeline: br::vk::VkPipeline,
    invert_pipeline: br::vk::VkPipeline,
}
impl Drop for TwoPassStencilSDFRenderer {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                self.invert_pipeline,
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                self.outline_distance_pipeline,
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                self.curve_triangles_stencil_pipeline,
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                self.triangle_fans_stencil_pipeline,
                None,
            );
            br::vkfn_wrapper::destroy_pipeline_layout(
                self.gfx_device.native_ptr(),
                self.pipeline_layout,
                None,
            );
            br::vkfn_wrapper::destroy_render_pass(
                self.gfx_device.native_ptr(),
                self.render_pass,
                None,
            );
        }
    }
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
            br::vk::VkAttachmentDescription::new(
                color_format,
                target_final_layout,
                target_final_layout,
            )
            .color_memory_op(br::LoadOp::Load, br::StoreOp::Store),
            br::vk::VkAttachmentDescription::new(
                br::vk::VK_FORMAT_S8_UINT,
                br::ImageLayout::DepthStencilReadOnlyOpt,
                br::ImageLayout::DepthStencilReadOnlyOpt,
            )
            .stencil_load_op(br::LoadOp::Clear),
        ];
        let depth_stencil_attachment_ref =
            br::vk::VkAttachmentReference::new(1, br::ImageLayout::DepthStencilAttachmentOpt);
        let color_attachments = [br::vk::VkAttachmentReference::new(
            0,
            br::ImageLayout::ColorAttachmentOpt,
        )];
        let subpasses = [
            br::SubpassDescription::new().depth_stencil_attachment(&depth_stencil_attachment_ref),
            br::SubpassDescription::new()
                .color_attachments(&color_attachments, &[])
                .depth_stencil_attachment(&depth_stencil_attachment_ref),
        ];
        let spdep_color = br::vk::VkSubpassDependency {
            srcSubpass: br::vk::VK_SUBPASS_EXTERNAL,
            dstSubpass: 0,
            srcStageMask: target_layout_transition_stage.0,
            dstStageMask: br::vk::VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT
                // Note: LoadOpがClearだとLoad時にWriteが走るらしいのでearlyステージで遷移できてないといけない
                | br::vk::VK_PIPELINE_STAGE_EARLY_FRAGMENT_TESTS_BIT,
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
        let render_pass =
            br::RenderPassCreateInfo::new(&attachments, &subpasses, &[spdep_color, spdep_stencil])
                .execute(e.graphics().device(), None)
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
        let viewport_state = br::PipelineViewportStateCreateInfo::new(&viewports, &scissors);

        let fill_shader: PvpContainer = e
            .load("builtin.vg.sdf.shaders.triangle_fans")
            .expect("Failed to load triangle_fans shader asset");
        let fill_shader_modules = PvpShaderModules::new(e.graphics().device(), &fill_shader)
            .expect("Failed to create triangle_fans shader modules");
        let fill_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &fill_shader.vertex_bindings,
            &fill_shader.vertex_attributes,
        );

        let curve_fill_shader: PvpContainer = e
            .load("builtin.vg.sdf.shaders.curve_triangles")
            .expect("Failed to load curve_triangles shader asset");
        let curve_fill_shader_modules =
            PvpShaderModules::new(e.graphics().device(), &curve_fill_shader)
                .expect("Failed to create curve_triangles shader modules");
        let curve_fill_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &curve_fill_shader.vertex_bindings,
            &curve_fill_shader.vertex_attributes,
        );

        let outline_shader: PvpContainer = e
            .load("builtin.vg.sdf.shaders.outline_distance")
            .expect("Failed to load outline_distance shader asset");
        let outline_shader_modules = PvpShaderModules::new(e.graphics().device(), &outline_shader)
            .expect("Failed to create outline_disdtance shader modules");
        let outline_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &outline_shader.vertex_bindings,
            &outline_shader.vertex_attributes,
        );

        let pipeline_layout = unsafe {
            br::vkfn_wrapper::create_pipeline_layout(
                e.graphics().device().native_ptr(),
                &br::PipelineLayoutCreateInfo::new(&[], &[]),
                None,
            )
            .expect("Failed to create pipeline layout")
        };

        let stencil_triangle_vsh_parameters =
            br::SpecializationInfo::new(&stencil_triangle_vsh_parameters);
        let fill_color_fsh_parameters = &br::SpecializationInfo::new(&fill_fsh_color_output);
        let outline_vsh_parameters = br::SpecializationInfo::new(&outline_vsh_parameters);

        let color_blends = [ColorAttachmentBlending::Disabled.into_vk()];
        let color_blend_state = br::PipelineColorBlendStateCreateInfo::new(&color_blends);

        let rasterization_state = br::PipelineRasterizationStateCreateInfo::new(
            br::PolygonMode::Fill,
            br::CullModeFlags::NONE,
            br::FrontFace::CounterClockwise,
        );
        let multisample_state = br::PipelineMultisampleStateCreateInfo::new();

        let [triangle_fans_stencil_pipeline, curve_triangles_stencil_pipeline, invert_pipeline, outline_distance_pipeline] = unsafe {
            br::vkfn_wrapper::create_graphics_pipeline_array(
                e.graphics().device().native_ptr(),
                None,
                &[
                    br::GraphicsPipelineCreateInfo::new(
                        &br::VkHandleRef::dangling(pipeline_layout),
                        br::SubpassRef(&br::VkHandleRef::dangling(render_pass), 0),
                        &[
                            fill_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&stencil_triangle_vsh_parameters),
                            fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &viewport_state,
                        &rasterization_state,
                        &color_blend_state,
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::stencil_invert())
                            .stencil_state_back(Self::stencil_invert())
                            .stencil_test(true),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &br::VkHandleRef::dangling(pipeline_layout),
                        br::SubpassRef(&br::VkHandleRef::dangling(render_pass), 0),
                        &[
                            curve_fill_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&stencil_triangle_vsh_parameters),
                            curve_fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &curve_fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &viewport_state,
                        &rasterization_state,
                        &color_blend_state,
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::stencil_invert())
                            .stencil_state_back(Self::stencil_invert())
                            .stencil_test(true),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &br::VkHandleRef::dangling(pipeline_layout),
                        br::SubpassRef(&br::VkHandleRef::dangling(render_pass), 1),
                        &[
                            fill_shader_modules.pipeline_vertex_shader_stage(),
                            fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?")
                                .with_specialization_info(&fill_color_fsh_parameters),
                        ],
                        &fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleStrip,
                        ),
                        &viewport_state,
                        &rasterization_state,
                        &br::PipelineColorBlendStateCreateInfo::new(&[
                            ColorAttachmentBlending::new(
                                Blending::source_only(br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR),
                                Blending::source_only(br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA),
                            )
                            .into_vk(),
                        ]),
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::stencil_match())
                            .stencil_state_back(Self::stencil_match())
                            .stencil_test(true),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &br::VkHandleRef::dangling(pipeline_layout),
                        br::SubpassRef(&br::VkHandleRef::dangling(render_pass), 1),
                        &[
                            outline_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&outline_vsh_parameters),
                            outline_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &outline_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &viewport_state,
                        &rasterization_state,
                        &br::PipelineColorBlendStateCreateInfo::new(&[
                            ColorAttachmentBlending::MAX.into_vk(),
                        ]),
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::stencil_noop())
                            .stencil_state_back(Self::stencil_noop())
                            .stencil_test(false),
                    ),
                ],
                None,
            )
            .expect("Failed to create graphics pipelines")
        };

        Self {
            gfx_device: e.graphics().device().clone(),
            render_pass,
            target_size: init_target_size,
            fill_shader,
            fill_shader_modules,
            curve_fill_shader,
            curve_fill_shader_modules,
            outline_shader,
            outline_shader_modules,
            pipeline_layout,
            triangle_fans_stencil_pipeline,
            curve_triangles_stencil_pipeline,
            invert_pipeline,
            outline_distance_pipeline,
        }
    }

    pub const fn render_pass<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkRenderPass> {
        unsafe { br::VkHandleRef::dangling(self.render_pass) }
    }

    pub const fn pipeline_layout<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkPipelineLayout> {
        unsafe { br::VkHandleRef::dangling(self.pipeline_layout) }
    }

    pub const fn triangle_fans_stencil_pipeline<'s>(
        &'s self,
    ) -> br::VkHandleRef<'s, br::vk::VkPipeline> {
        unsafe { br::VkHandleRef::dangling(self.triangle_fans_stencil_pipeline) }
    }

    pub const fn curve_triangles_stencil_pipeline<'s>(
        &'s self,
    ) -> br::VkHandleRef<'s, br::vk::VkPipeline> {
        unsafe { br::VkHandleRef::dangling(self.curve_triangles_stencil_pipeline) }
    }

    pub const fn invert_pipeline<'s>(&'s self) -> br::VkHandleRef<'s, br::vk::VkPipeline> {
        unsafe { br::VkHandleRef::dangling(self.invert_pipeline) }
    }

    pub const fn outline_distance_pipeline<'s>(
        &'s self,
    ) -> br::VkHandleRef<'s, br::vk::VkPipeline> {
        unsafe { br::VkHandleRef::dangling(self.outline_distance_pipeline) }
    }

    pub fn resize(&mut self, new_size: peridot::math::Vector2<u32>, sdf_max_distance: f32) {
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
        let viewport_state = br::PipelineViewportStateCreateInfo::new(&viewports, &scissors);

        let fill_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &self.fill_shader.vertex_bindings,
            &self.fill_shader.vertex_attributes,
        );
        let curve_fill_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &self.curve_fill_shader.vertex_bindings,
            &self.curve_fill_shader.vertex_attributes,
        );
        let outline_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &self.outline_shader.vertex_bindings,
            &self.outline_shader.vertex_attributes,
        );

        let vsh_parameters = br::SpecializationInfo::new(&stencil_vsh_parameters);
        let fill_color_fsh_parameters = &br::SpecializationInfo::new(&fill_fsh_color_output);
        let outline_vsh_parameters = br::SpecializationInfo::new(&outline_vsh_parameters);

        let color_blends = [ColorAttachmentBlending::Disabled.into_vk()];
        let color_blend_state = br::PipelineColorBlendStateCreateInfo::new(&color_blends);

        let rasterization_state = br::PipelineRasterizationStateCreateInfo::new(
            br::PolygonMode::Fill,
            br::CullModeFlags::NONE,
            br::FrontFace::CounterClockwise,
        );
        let multisample_state = br::PipelineMultisampleStateCreateInfo::new();

        let [triangles_stencil_pipeline, curve_triangles_stencil_pipeline, invert_pipeline, outline_distance_pipeline] = unsafe {
            br::vkfn_wrapper::create_graphics_pipeline_array(
                self.gfx_device.native_ptr(),
                None,
                &[
                    br::GraphicsPipelineCreateInfo::new(
                        &self.pipeline_layout(),
                        br::SubpassRef(&self.render_pass(), 0),
                        &[
                            self.fill_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&vsh_parameters),
                            self.fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &viewport_state,
                        &rasterization_state,
                        &color_blend_state,
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::stencil_invert())
                            .stencil_state_back(Self::stencil_invert())
                            .stencil_test(true),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &self.pipeline_layout(),
                        br::SubpassRef(&self.render_pass(), 0),
                        &[
                            self.curve_fill_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&vsh_parameters),
                            self.curve_fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &curve_fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &viewport_state,
                        &rasterization_state,
                        &color_blend_state,
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::stencil_invert())
                            .stencil_state_back(Self::stencil_invert())
                            .stencil_test(true),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &self.pipeline_layout(),
                        br::SubpassRef(&self.render_pass(), 1),
                        &[
                            self.fill_shader_modules.pipeline_vertex_shader_stage(),
                            self.fill_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?")
                                .with_specialization_info(&fill_color_fsh_parameters),
                        ],
                        &fill_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleStrip,
                        ),
                        &viewport_state,
                        &rasterization_state,
                        &br::PipelineColorBlendStateCreateInfo::new(&[
                            ColorAttachmentBlending::new(
                                Blending::source_only(br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_COLOR),
                                Blending::source_only(br::vk::VK_BLEND_FACTOR_ONE_MINUS_DST_ALPHA),
                            )
                            .into_vk(),
                        ]),
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::stencil_match())
                            .stencil_state_back(Self::stencil_match())
                            .stencil_test(true),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        &self.pipeline_layout(),
                        br::SubpassRef(&self.render_pass(), 1),
                        &[
                            self.outline_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&outline_vsh_parameters),
                            self.outline_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?"),
                        ],
                        &outline_vertex_input_state,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleList,
                        ),
                        &viewport_state,
                        &rasterization_state,
                        &br::PipelineColorBlendStateCreateInfo::new(&[
                            ColorAttachmentBlending::MAX.into_vk(),
                        ]),
                    )
                    .set_multisample_state(&multisample_state)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .stencil_state_front(Self::stencil_noop())
                            .stencil_state_back(Self::stencil_noop())
                            .stencil_test(false),
                    ),
                ],
                None,
            )
            .expect("Failed to create graphics pipelines")
        };

        self.target_size = new_size;
        unsafe {
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                core::mem::replace(
                    &mut self.triangle_fans_stencil_pipeline,
                    triangles_stencil_pipeline,
                ),
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                core::mem::replace(
                    &mut self.curve_triangles_stencil_pipeline,
                    curve_triangles_stencil_pipeline,
                ),
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                core::mem::replace(&mut self.invert_pipeline, invert_pipeline),
                None,
            );
            br::vkfn_wrapper::destroy_pipeline(
                self.gfx_device.native_ptr(),
                core::mem::replace(
                    &mut self.outline_distance_pipeline,
                    outline_distance_pipeline,
                ),
                None,
            );
        }
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
pub struct TwoPassStencilSDFRenderTarget {
    gfx_device: peridot::VulkanGfx,
    framebuffer: br::vk::VkFramebuffer,
}
impl Drop for TwoPassStencilSDFRenderTarget {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_framebuffer(
                self.gfx_device.native_ptr(),
                self.framebuffer,
                None,
            );
        }
    }
}
impl br::VkHandle for TwoPassStencilSDFRenderTarget {
    type Handle = br::vk::VkFramebuffer;

    fn native_ptr(&self) -> Self::Handle {
        self.framebuffer
    }
}
impl TwoPassStencilSDFRenderTarget {
    pub fn new(
        g: &peridot::VulkanGfx,
        renderer: &TwoPassStencilSDFRenderer,
        color_buffer_view: br::VkHandleRef<br::vk::VkImageView>,
        stencil_buffer_view: br::VkHandleRef<br::vk::VkImageView>,
        size: peridot::math::Vector2<u32>,
    ) -> Self {
        let framebuffer = unsafe {
            br::vkfn_wrapper::create_framebuffer(
                g.native_ptr(),
                &br::FramebufferCreateInfo::new(
                    &renderer.render_pass(),
                    &[color_buffer_view, stencil_buffer_view],
                    size.0,
                    size.1,
                ),
                None,
            )
            .expect("Failed to create framebuffer")
        };

        Self {
            gfx_device: g.clone(),
            framebuffer,
        }
    }
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
        framebuffer: &'s (impl br::VkHandle<Handle = br::vk::VkFramebuffer> + ?Sized),
        buffers: &'s TwoPassStencilSDFRendererBuffers,
    ) -> impl GraphicsCommand + 's {
        let rp = BeginRenderPass::new(
            self.render_pass(),
            framebuffer,
            self.render_area(),
            br::SubpassContents::Inline,
        )
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
                .after_of(peridot_command_object::BindGraphicsPipeline(
                    self.triangle_fans_stencil_pipeline(),
                )),
            buffers.curve_triangles_mesh.ref_draw(1).after_of(
                peridot_command_object::BindGraphicsPipeline(
                    self.curve_triangles_stencil_pipeline(),
                ),
            ),
        );
        let outline_distance_pass = (
            buffers.outline_rects_mesh.ref_draw(1).after_of(
                peridot_command_object::BindGraphicsPipeline(self.outline_distance_pipeline()),
            ),
            buffers.invert_fill_rect_mesh.ref_draw(1).after_of(
                peridot_command_object::BindGraphicsPipeline(self.invert_pipeline()),
            ),
        );

        (
            stencil_pass,
            NextSubpass::WITH_INLINE_COMMANDS,
            outline_distance_pass,
        )
            .between(rp, EndRenderPass)
    }
}

pub struct StandaloneImageView {
    gfx_device: peridot::VulkanGfx,
    handle: br::vk::VkImageView,
}
impl Drop for StandaloneImageView {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(self.gfx_device.native_ptr(), self.handle, None);
        }
    }
}
impl br::VkHandle for StandaloneImageView {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
impl StandaloneImageView {
    pub fn new(g: &peridot::VulkanGfx, create_info: &br::ImageViewCreateInfo) -> br::Result<Self> {
        let handle =
            unsafe { br::vkfn_wrapper::create_image_view(g.native_ptr(), create_info, None)? };

        Ok(Self {
            gfx_device: g.clone(),
            handle,
        })
    }
}

pub struct StencilBuffer {
    gfx_device: peridot::VulkanGfx,
    image: peridot_memory_manager::Image,
    image_view: br::vk::VkImageView,
}
impl Drop for StencilBuffer {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(
                self.gfx_device.native_ptr(),
                self.image_view,
                None,
            );
        }
    }
}
impl br::VkHandle for StencilBuffer {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.image_view
    }
}
impl StencilBuffer {
    pub fn new(
        g: &peridot::Graphics,
        memory_manager: &mut MemoryManager,
        size: peridot::math::Vector2<u32>,
        format: br::Format,
    ) -> Self {
        let image = memory_manager
            .allocate_device_local_image(
                g,
                br::ImageCreateInfo::new(size, format)
                    .with_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT),
            )
            .expect("Failed to create stencil buffer image");
        let image_view = unsafe {
            br::vkfn_wrapper::create_image_view(
                g.device().native_ptr(),
                &br::ImageViewCreateInfo::new(
                    &image,
                    br::vk::VkImageSubresourceRange {
                        aspectMask: br::AspectMask::STENCIL.bits(),
                        baseMipLevel: 0,
                        levelCount: 1,
                        baseArrayLayer: 0,
                        layerCount: 1,
                    },
                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                    format,
                ),
                None,
            )
            .expect("Failed to create stencil buffer view")
        };

        Self {
            gfx_device: g.device().clone(),
            image,
            image_view,
        }
    }

    pub fn resize(
        &mut self,
        g: &peridot::Graphics,
        memory_manager: &mut MemoryManager,
        size: peridot::math::Vector2<u32>,
        format: br::Format,
    ) {
        self.image = memory_manager
            .allocate_device_local_image(
                g,
                br::ImageCreateInfo::new(size, format)
                    .with_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT),
            )
            .expect("Failed to allocate stencil buffer");
        let view = unsafe {
            br::vkfn_wrapper::create_image_view(
                self.gfx_device.native_ptr(),
                &br::ImageViewCreateInfo::new(
                    &self.image,
                    br::vk::VkImageSubresourceRange {
                        aspectMask: br::AspectMask::STENCIL.bits(),
                        baseMipLevel: 0,
                        levelCount: 1,
                        baseArrayLayer: 0,
                        layerCount: 1,
                    },
                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                    format,
                ),
                None,
            )
            .expect("Failed to create stencil buffer view")
        };
        unsafe {
            br::vkfn_wrapper::destroy_image_view(
                self.gfx_device.native_ptr(),
                core::mem::replace(&mut self.image_view, view),
                None,
            );
        }
    }
}

const SDF_SIZE: f32 = 32.0;

pub async fn game_main<'q>(e: &mut peridot::Engine<'q, impl peridot::NativeLinker>) {
    let back_buffer_size = e.back_buffer_size();

    let font = peridot_vg::DefaultFontProvider::new()
        .expect("Failed to create font provider")
        .best_match("sans-serif", &peridot_vg::FontProperties::default(), 120.0)
        .expect("no suitable font");
    let gid = font.glyph_id('A').expect("no glyph contained");
    let mut gen = peridot_vg::SDFGenerator::new(1.0, SDF_SIZE);
    let glyph_metrics = font.bounds(&gid).expect("Failed to get glyph bounds");
    font.outline(
        &gid,
        &peridot_vg::sdf_generator::Transform2D::create_translation(
            -glyph_metrics.origin.x + SDF_SIZE,
            -glyph_metrics.origin.y - SDF_SIZE,
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
    let figures_fill_triangle_points_offset = bp.add(peridot::BufferContent::vertices::<
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
                bp.build_desc().with_usage(br::BufferUsage::TRANSFER_DEST),
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
    let mut stencil_buffer = StencilBuffer::new(
        e.graphics(),
        &mut memory_manager,
        back_buffer_size,
        br::vk::VK_FORMAT_S8_UINT,
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
        let stencil_buffer = RangedImage::single_stencil_plane(&stencil_buffer.image);

        let copy = all_buffer.byref_mirror_from(&buffer_init);

        let [all_buffer_in_barrier, all_buffer_out_barrier] = all_buffer.clone().usage_barrier3(
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
            .with_barrier(
                stencil_buffer.barrier(br::ImageLayout::DepthStencilReadOnlyOpt.from_undefined()),
            )
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
        core::mem::size_of::<peridot_vg::sdf_generator::ParabolaRectVertex>() * outline_rects_count,
    );
    let flip_fill_rect_buffer =
        RangedBuffer::for_type::<[peridot::math::Vector2<f32>; 4]>(buffer, flip_fill_rect as _);

    let mut sdf_renderer = TwoPassStencilSDFRenderer::new(
        e,
        e.back_buffer_format(),
        e.requesting_back_buffer_layout().0,
        e.requesting_back_buffer_layout().1,
        back_buffer_size.clone().into(),
        SDF_SIZE,
    );

    let mut backbuffer_resources = e
        .iter_back_buffers()
        .map(|b| {
            StandaloneImageView::new(
                e.graphics().device(),
                &br::ImageViewCreateInfo::new(
                    &b,
                    br::vk::VkImageSubresourceRange {
                        aspectMask: br::AspectMask::COLOR.bits(),
                        baseMipLevel: 0,
                        levelCount: 1,
                        baseArrayLayer: 0,
                        layerCount: 1,
                    },
                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                    e.back_buffer_format(),
                ),
            )
            .expect("Failed to create back buffer view")
        })
        .collect::<Vec<_>>();
    let mut fb = backbuffer_resources
        .iter()
        .map(|bb| {
            TwoPassStencilSDFRenderTarget::new(
                e.graphics().device(),
                &sdf_renderer,
                bb.as_transparent_ref(),
                stencil_buffer.as_transparent_ref(),
                back_buffer_size,
            )
        })
        .collect::<Vec<_>>();

    let fill_triangle_groups: Vec<_> = figure_vertices
        .iter()
        .map(|f| {
            (
                f.fill_triangle_points.len() as u32,
                f.fill_triangle_indices.len() as u32,
            )
        })
        .collect();
    let mut buffers = TwoPassStencilSDFRendererBuffers {
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
            .execute_and_finish(
                cmd.synchronized_nth(cx)
                    .begin(&br::CommandBufferBeginInfo::new())
                    .expect("Failed to begin recording commands"),
            )
            .expect("Failed to record commands");
    }

    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let fd = e.prepare_frame().expect("Failed to prepare frame");

                let mut render_batch = peridot::SubmissionBatchBuilder::new();
                let render_cb = cmd.nth_ref(fd.backbuffer_index as _);
                render_batch.add_command_buffers([render_cb.as_transparent_ref()]);
                e.do_render(fd.backbuffer_index, None, render_batch)
                    .expect("Failed to present");
            }
            peridot::Event::Resize(new_size) => {
                e.wait_for_last_rendering_completion()
                    .expect("Failed to wait last command completion");

                drop(fb);
                drop(backbuffer_resources);

                e.resize_presenter_backbuffers(new_size);

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
                let mut gen = peridot_vg::SDFGenerator::new(1.0, SDF_SIZE);
                let glyph_metrics = font.bounds(&gid).expect("Failed to get glyph bounds");
                font.outline(
                    &gid,
                    &peridot_vg::sdf_generator::Transform2D::create_translation(
                        -glyph_metrics.origin.x + SDF_SIZE,
                        -glyph_metrics.origin.y - SDF_SIZE,
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

                sdf_renderer.resize(
                    peridot::math::Vector2(new_size.0 as _, new_size.1 as _),
                    SDF_SIZE,
                );

                let mut bp = peridot::BufferPrealloc::new(e.graphics());
                let flip_fill_rect = bp.add(peridot::BufferContent::vertex::<
                    [peridot::math::Vector2<f32>; 4],
                >());
                let figures_fill_triangle_points_offset =
                    bp.add(peridot::BufferContent::vertices::<
                        peridot::math::Vector2<f32>,
                    >(figure_fill_triangle_points_count));
                let figures_fill_triangle_indices_offset = bp.add(
                    peridot::BufferContent::indices::<u16>(figure_fill_triangle_indices_count),
                );
                let figure_curve_triangles_offset =
                    bp.add(peridot::BufferContent::vertices::<peridot::VertexUV2D>(
                        figure_curve_triangles_count,
                    ));
                let outline_rects_offset = bp.add(peridot::BufferContent::vertices::<
                    peridot_vg::sdf_generator::ParabolaRectVertex,
                >(outline_rects_count * 6));

                let buffer = SharedRef::new(
                    memory_manager
                        .allocate_device_local_buffer(
                            e.graphics(),
                            bp.build_desc().with_usage(br::BufferUsage::TRANSFER_DEST),
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
                stencil_buffer.resize(
                    e.graphics(),
                    &mut memory_manager,
                    new_size,
                    br::vk::VK_FORMAT_S8_UINT,
                );
                backbuffer_resources = e
                    .iter_back_buffers()
                    .map(|b| {
                        StandaloneImageView::new(
                            e.graphics().device(),
                            &br::ImageViewCreateInfo::new(
                                &b,
                                br::vk::VkImageSubresourceRange {
                                    aspectMask: br::AspectMask::COLOR.bits(),
                                    baseMipLevel: 0,
                                    levelCount: 1,
                                    baseArrayLayer: 0,
                                    layerCount: 1,
                                },
                                br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                e.back_buffer_format(),
                            ),
                        )
                        .expect("Failed to create back buffer view")
                    })
                    .collect::<Vec<_>>();
                fb = backbuffer_resources
                    .iter()
                    .map(|bb| {
                        TwoPassStencilSDFRenderTarget::new(
                            e.graphics().device(),
                            &sdf_renderer,
                            bb.as_transparent_ref(),
                            stencil_buffer.as_transparent_ref(),
                            new_size,
                        )
                    })
                    .collect::<Vec<_>>();

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
                        let (mut s_offset, mut si_offset, mut c_offset, mut o_offset) =
                            (0, 0, 0, 0);
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
                    let stg_copied_buffer = buffer_init.subslice_ref(0..bp.total_size() as _);
                    let all_buffer =
                        RangedBuffer::from_offset_length(&*buffer, 0, bp.total_size() as _);
                    let stencil_buffer = RangedImage::single_stencil_plane(&stencil_buffer.image);

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
                        .with_barrier(
                            stencil_buffer
                                .barrier(br::ImageLayout::DepthStencilReadOnlyOpt.from_undefined()),
                        )
                        .by_region();

                    copy.between(in_barriers, out_barriers)
                        .submit(e)
                        .expect("Failed to initialize resources");
                }

                let figures_fill_triangle_points_buffer = RangedBuffer::from_offset_length(
                    buffer.clone(),
                    figures_fill_triangle_points_offset,
                    core::mem::size_of::<peridot::math::Vector2<f32>>()
                        * figure_fill_triangle_points_count,
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
                let flip_fill_rect_buffer = RangedBuffer::for_type::<
                    [peridot::math::Vector2<f32>; 4],
                >(buffer, flip_fill_rect as _);

                let fill_triangle_groups: Vec<_> = figure_vertices
                    .iter()
                    .map(|f| {
                        (
                            f.fill_triangle_points.len() as u32,
                            f.fill_triangle_indices.len() as u32,
                        )
                    })
                    .collect();
                buffers = TwoPassStencilSDFRendererBuffers {
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
                cmd = peridot::CommandBundle::new(
                    e.graphics(),
                    peridot::CBSubmissionType::Graphics,
                    e.back_buffer_count(),
                )
                .expect("Failed to create CommandBundle");
                for (cx, fb) in fb.iter().enumerate() {
                    sdf_renderer
                        .commands(fb, &buffers)
                        .execute_and_finish(
                            cmd.synchronized_nth(cx)
                                .begin(&br::CommandBufferBeginInfo::new())
                                .expect("Failed to begin recording commands"),
                        )
                        .expect("Failed to record commands");
                }
            }
        }
    }

    unsafe {
        e.graphics().device().wait().expect("Failed to wait works");
    }
}
