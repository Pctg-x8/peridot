use bedrock::{self as br, CommandBufferMut, DescriptorPoolMut, RenderPass, VkHandle};
use br::{Device, ImageChild};
use log::*;
use peridot::math::Vector2;
use peridot::mthelper::SharedRef;
use peridot::{BufferPrealloc, CBSubmissionType, CommandBundle, LayoutedPipeline, ModelData};
use peridot_command_object::{
    BeginRenderPass, BufferUsage, ColorAttachmentBlending, EndRenderPass, GraphicsCommand,
    GraphicsCommandCombiner, GraphicsCommandSubmission, PipelineBarrier, RangedBuffer, RangedImage,
};
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_vertex_processing_pack::{PvpContainer, PvpShaderModules};
use peridot_vg as pvg;
use peridot_vg::{FlatPathBuilder, PathBuilder};
use pvg::{FontProvider, FontProviderConstruct, RenderVG};

#[derive(br::SpecializationConstants)]
#[repr(C)]
pub struct VgRendererFragmentFixedColor {
    #[constant_id = 0]
    r: f32,
    #[constant_id = 1]
    g: f32,
    #[constant_id = 2]
    b: f32,
    #[constant_id = 3]
    a: f32,
}

const unsafe fn as_u8_slice<T>(slice: &[T]) -> &[u8] {
    core::slice::from_raw_parts(
        slice.as_ptr() as *const u8,
        slice.len() * core::mem::size_of::<T>(),
    )
}

pub struct StandaloneImageView {
    gfx_device: peridot::VulkanGfx,
    handle: br::vk::VkImageView,
}
impl Drop for StandaloneImageView {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(
                self.gfx_device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.handle),
                None,
            );
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
    pub fn new(
        device: &peridot::VulkanGfx,
        create_info: &br::ImageViewCreateInfo,
    ) -> br::Result<Self> {
        Ok(Self {
            handle: unsafe {
                br::vkfn_wrapper::create_image_view(device.as_transparent_ref(), create_info, None)?
            },
            gfx_device: device.clone(),
        })
    }
}

pub async fn game_main<'q>(e: &mut peridot::Engine<'q, impl peridot::NativeLinker>) {
    let mut font_provider =
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
            bp.build_desc().with_usage(br::BufferUsage::TRANSFER_DEST),
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

    let rt_size = e.back_buffer_size();
    let msaa_count = br::vk::VK_SAMPLE_COUNT_4_BIT;
    let msaa_texture = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(rt_size.clone(), e.back_buffer_format())
                .with_usage(
                    br::ImageUsageFlags::COLOR_ATTACHMENT
                        | br::ImageUsageFlags::TRANSIENT_ATTACHMENT,
                )
                .sample_counts(msaa_count),
        )
        .expect("Failed to create msaa render target");
    let mut msaa_texture = SharedRef::new(
        br::ImageViewBuilder::new(
            msaa_texture,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("Failed to create msaa render target view"),
    );

    let (vg_renderer_params, vg_renderer_params2) = stg_buffer
        .0
        .guard_map(BufferMapMode::Write, |m| unsafe {
            let p0 = ctx.write_data_into(m.ptr().as_ptr(), vg_offs);
            let p1 = ctx2.write_data_into(m.ptr().as_ptr(), vg_offs2);
            return (p0, p1);
        })
        .expect("StgMem Initialization");

    let bufview = br::BufferViewObject::new(
        buffer.0.clone(),
        &br::BufferViewCreateInfo::new(
            &buffer.0,
            br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            vg_renderer_params.transforms_byterange(),
        ),
    )
    .expect("Creating Transform BufferView");
    let bufview2 = br::BufferViewObject::new(
        buffer.0.clone(),
        &br::BufferViewCreateInfo::new(
            &buffer.0,
            br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            vg_renderer_params2.transforms_byterange(),
        ),
    )
    .expect("Creating Transform BufferView 2");

    {
        let copy = buffer.byref_mirror_from(&stg_buffer);

        let [all_buffer_in_barrier, all_buffer_out_barrier] = buffer.make_ref().usage_barrier3(
            BufferUsage::UNUSED,
            BufferUsage::TRANSFER_DST,
            BufferUsage::VERTEX_BUFFER | BufferUsage::INDEX_BUFFER | BufferUsage::VERTEX_STORAGE_RO,
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
                RangedImage::single_color_plane(msaa_texture.image())
                    .barrier(br::ImageLayout::ColorAttachmentOpt.from_undefined()),
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
    let color_outputs = [br::AttachmentReference::new(
        1,
        br::ImageLayout::ColorAttachmentOpt,
    )];
    let color_resolves = [br::AttachmentReference::new(
        0,
        br::ImageLayout::ColorAttachmentOpt,
    )];
    let color_subpass =
        br::SubpassDescription::new().color_attachments(&color_outputs, &color_resolves);
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
    let render_pass = br::RenderPassObject::new(
        e.graphics().device().clone(),
        &br::RenderPassCreateInfo::new(
            &attachments,
            &[color_subpass],
            &[color_subpass_enter_dep, color_subpass_leave_dep],
        ),
    )
    .expect("Failed to create render pass");

    let screen_size = e.back_buffer_size();
    let mut backbuffer_resources = e
        .iter_back_buffers()
        .map(|x| {
            StandaloneImageView::new(
                e.graphics().device(),
                &br::ImageViewCreateInfo::new(
                    &x,
                    br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                    e.back_buffer_format(),
                ),
            )
            .expect("Failed to create backbuffer view")
        })
        .collect::<Vec<_>>();
    let mut framebuffers = backbuffer_resources
        .iter()
        .map(|bb| {
            br::FramebufferObject::new(
                e.graphics_device().clone(),
                &br::FramebufferCreateInfo::new(
                    &render_pass,
                    &[bb.as_transparent_ref(), msaa_texture.as_transparent_ref()],
                    screen_size.0,
                    screen_size.1,
                ),
            )
        })
        .collect::<Result<Vec<_>, _>>()
        .expect("Framebuffer Creation");

    let dsl = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::UniformTexelBuffer
            .make_binding(0, 1)
            .only_for_vertex()]),
    )
    .expect("DescriptorSetLayout Creation");
    let mut dp = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            2,
            &[br::DescriptorType::UniformTexelBuffer.make_size(2)],
        ),
    )
    .expect("DescriptorPool Creation");
    let [desc_interior, desc_curve] = dp
        .alloc_array(&[dsl.as_transparent_ref(), dsl.as_transparent_ref()])
        .expect("DescriptorSet Allocation");

    e.graphics().device().update_descriptor_sets(
        &[
            desc_interior
                .binding_at(0)
                .write(br::DescriptorContents::UniformTexelBuffer(vec![
                    br::VkHandleRef::new(&bufview),
                ])),
            desc_curve
                .binding_at(0)
                .write(br::DescriptorContents::UniformTexelBuffer(vec![
                    br::VkHandleRef::new(&bufview2),
                ])),
        ],
        &[],
    );

    let shader: PvpContainer = e
        .load("shaders.interiorColorFixed")
        .expect("Loading PvpContainer");
    let shader_modules =
        PvpShaderModules::new(e.graphics().device(), &shader).expect("Creating Shader");
    let curve_shader: PvpContainer = e
        .load("shaders.curveColorFixed")
        .expect("Loading CurveShader");
    let curve_shader_modules =
        PvpShaderModules::new(e.graphics().device(), &curve_shader).expect("Creating CurveShader");
    debug!("ScreenSize: {screen_size:?}");
    let pl = SharedRef::new(
        br::PipelineLayoutObject::new(
            e.graphics().device().clone(),
            &br::PipelineLayoutCreateInfo::new(
                &[dsl.as_transparent_ref()],
                &[br::PushConstantRange::new(
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT,
                    0..4 * 4,
                )],
            ),
        )
        .expect("Create PipelineLayout"),
    );

    let [gp, gp_curve, gp2, gp2_curve] = {
        let sc = [br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO)];
        let vp = [sc[0].make_viewport(0.0..1.0)];
        let viewport_state = br::PipelineViewportStateCreateInfo::new_array(&vp, &sc);

        let spc_map = &[
            br::SpecializationMapEntry::for_type::<f32>(0, 0),
            br::SpecializationMapEntry::for_type::<f32>(1, 4),
        ];
        let vsh_parameters = unsafe {
            br::SpecializationInfo::from_binary(
                spc_map,
                as_u8_slice(&pvg::renderer_pivot::LEFT_TOP[..]),
            )
        };

        let gp1_fsh_parameters = br::SpecializationInfo::new(&VgRendererFragmentFixedColor {
            r: 1.0,
            g: 0.5,
            b: 0.0,
            a: 1.0,
        });
        let gp2_fsh_parameters = br::SpecializationInfo::new(&VgRendererFragmentFixedColor {
            r: 0.0,
            g: 0.5,
            b: 1.0,
            a: 1.0,
        });

        let vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &shader.vertex_bindings,
            &shader.vertex_attributes,
        );
        let curve_vertex_input_state = br::PipelineVertexInputStateCreateInfo::new(
            &curve_shader.vertex_bindings,
            &curve_shader.vertex_attributes,
        );
        let input_assembly_state =
            br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleList);
        let ms =
            br::PipelineMultisampleStateCreateInfo::new().rasterization_samples(msaa_count as _);
        let rs = br::PipelineRasterizationStateCreateInfo::new(
            br::PolygonMode::Fill,
            br::CullModeFlags::NONE,
            br::FrontFace::CounterClockwise,
        );
        let color_blends = [ColorAttachmentBlending::PREMULTIPLIED_ALPHA.into_vk()];

        e.graphics()
            .device()
            .new_graphics_pipeline_array(
                &[
                    br::GraphicsPipelineCreateInfo::new(
                        &pl,
                        render_pass.subpass(0),
                        &[
                            shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&vsh_parameters),
                            shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?")
                                .with_specialization_info(&gp1_fsh_parameters),
                        ],
                        &vertex_input_state,
                        &input_assembly_state,
                        &viewport_state,
                        &rs,
                        &br::PipelineColorBlendStateCreateInfo::new(&color_blends),
                    )
                    .set_multisample_state(&ms),
                    br::GraphicsPipelineCreateInfo::new(
                        &pl,
                        render_pass.subpass(0),
                        &[
                            curve_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&vsh_parameters),
                            curve_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?")
                                .with_specialization_info(&gp1_fsh_parameters),
                        ],
                        &curve_vertex_input_state,
                        &input_assembly_state,
                        &viewport_state,
                        &rs,
                        &br::PipelineColorBlendStateCreateInfo::new(&color_blends),
                    )
                    .set_multisample_state(&ms),
                    br::GraphicsPipelineCreateInfo::new(
                        &pl,
                        render_pass.subpass(0),
                        &[
                            shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&vsh_parameters),
                            shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?")
                                .with_specialization_info(&gp2_fsh_parameters),
                        ],
                        &vertex_input_state,
                        &input_assembly_state,
                        &viewport_state,
                        &rs,
                        &br::PipelineColorBlendStateCreateInfo::new(&color_blends),
                    )
                    .set_multisample_state(&ms),
                    br::GraphicsPipelineCreateInfo::new(
                        &pl,
                        render_pass.subpass(0),
                        &[
                            curve_shader_modules
                                .pipeline_vertex_shader_stage()
                                .with_specialization_info(&vsh_parameters),
                            curve_shader_modules
                                .pipeline_fragment_shader_stage()
                                .expect("no fsh?")
                                .with_specialization_info(&gp2_fsh_parameters),
                        ],
                        &curve_vertex_input_state,
                        &input_assembly_state,
                        &viewport_state,
                        &rs,
                        &br::PipelineColorBlendStateCreateInfo::new(&color_blends),
                    )
                    .set_multisample_state(&ms),
                ],
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create graphics pipelines")
    };
    let gp = LayoutedPipeline::combine(gp.clone_parent(), pl.clone());
    let gp_curve = LayoutedPipeline::combine(gp_curve.clone_parent(), pl.clone());
    let gp2 = LayoutedPipeline::combine(gp2.clone_parent(), pl.clone());
    let gp2_curve = LayoutedPipeline::combine(gp2_curve.clone_parent(), pl.clone());

    let render_vg = RenderVG {
        params: vg_renderer_params,
        buffer: buffer.0.clone(),
        interior_pipeline: gp,
        curve_pipeline: gp_curve,
        transform_buffer_descriptor_set: desc_interior,
        target_pixels: Vector2(screen_size.0 as _, screen_size.1 as _),
        rendering_precision: e.rendering_precision(),
    };
    let render_vg2 = RenderVG {
        params: vg_renderer_params2,
        buffer: buffer.0,
        interior_pipeline: gp2,
        curve_pipeline: gp2_curve,
        transform_buffer_descriptor_set: desc_curve,
        target_pixels: Vector2(screen_size.0 as _, screen_size.1 as _),
        rendering_precision: e.rendering_precision(),
    };
    let mut color_renders = [render_vg2, render_vg];

    let mut render_cb = CommandBundle::new(
        &e.graphics(),
        CBSubmissionType::Graphics,
        framebuffers.len(),
    )
    .expect("Creating RenderCB");
    for (mut r, f) in render_cb.iter_mut().zip(&framebuffers) {
        let rp = BeginRenderPass::new(
            &render_pass,
            f,
            br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO),
            br::SubpassContents::Inline,
        )
        .with_clear_values(vec![
            br::ClearValue::color([1.0; 4]),
            br::ClearValue::color([1.0; 4]),
        ]);

        (&color_renders[..])
            .between(rp, EndRenderPass)
            .execute_and_finish(unsafe {
                r.begin(&br::CommandBufferBeginInfo::new())
                    .expect("Failed to begin render command recording")
            })
            .expect("Failed to finish render commands");
    }

    let target_size = peridot::math::Vector2(screen_size.0 as _, screen_size.1 as _);

    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let fd = e.prepare_frame().expect("Failed to prepare frame");

                let mut render_batch = peridot::SubmissionBatchBuilder::new();
                let render_cb = render_cb.nth_ref(fd.backbuffer_index as _);
                render_batch.add_command_buffers([render_cb.as_transparent_ref()]);
                e.do_render(fd.backbuffer_index, None, render_batch)
                    .expect("Failed to present");
            }
            peridot::Event::Resize(new_size) => {
                e.wait_for_last_rendering_completion()
                    .expect("Failed to wait last rendering completion");

                unsafe { render_cb.reset().expect("Resetting RenderCB") };
                drop(framebuffers);
                drop(backbuffer_resources);

                e.resize_presenter_backbuffers(new_size);

                let rt_size = br::vk::VkExtent2D {
                    width: new_size.0 as _,
                    height: new_size.1 as _,
                };

                let msaa_count = br::vk::VK_SAMPLE_COUNT_4_BIT;
                let msaa_texture_res = memory_manager
                    .allocate_device_local_image(
                        e.graphics(),
                        br::ImageCreateInfo::new(rt_size.clone(), e.back_buffer_format())
                            .with_usage(
                                br::ImageUsageFlags::COLOR_ATTACHMENT
                                    | br::ImageUsageFlags::TRANSIENT_ATTACHMENT,
                            )
                            .sample_counts(msaa_count),
                    )
                    .expect("Failed to create msaa render target");
                msaa_texture = SharedRef::new(
                    br::ImageViewBuilder::new(
                        msaa_texture_res,
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    )
                    .create()
                    .expect("Failed to create msaa render target view"),
                );

                PipelineBarrier::from(
                    RangedImage::single_color_plane(msaa_texture.image()).barrier(
                        br::ImageLayout::Undefined.to(br::ImageLayout::ColorAttachmentOpt),
                    ),
                )
                .submit(e)
                .expect("Failed to initialize msaa rt");

                backbuffer_resources = e
                    .iter_back_buffers()
                    .map(|x| {
                        StandaloneImageView::new(
                            e.graphics().device(),
                            &br::ImageViewCreateInfo::new(
                                &x,
                                br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                                br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                e.back_buffer_format(),
                            ),
                        )
                        .expect("Failed to create backbuffer view")
                    })
                    .collect();
                framebuffers = backbuffer_resources
                    .iter()
                    .map(|bb| {
                        br::FramebufferObject::new(
                            e.graphics_device().clone(),
                            &br::FramebufferCreateInfo::new(
                                &render_pass,
                                &[bb.as_transparent_ref(), msaa_texture.as_transparent_ref()],
                                new_size.0,
                                new_size.1,
                            ),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .expect("Bind Framebuffer");

                for r in color_renders.iter_mut() {
                    r.set_target_pixels(target_size.clone());
                }

                for (mut r, f) in render_cb.iter_mut().zip(&framebuffers) {
                    let rp = BeginRenderPass::new(
                        &render_pass,
                        f,
                        br::vk::VkExtent2D::from(new_size).into_rect(br::vk::VkOffset2D::ZERO),
                        br::SubpassContents::Inline,
                    )
                    .with_clear_values(vec![
                        br::ClearValue::color([1.0; 4]),
                        br::ClearValue::color([1.0; 4]),
                    ]);

                    (&color_renders[..])
                        .between(rp, EndRenderPass)
                        .execute_and_finish(unsafe {
                            r.begin(&br::CommandBufferBeginInfo::new())
                                .expect("Start Recording CB")
                        })
                        .expect("Failed to finish render commands");
                }
            }
        }
    }

    unsafe {
        e.graphics().device().wait().expect("Failed to wait works");
    }
}
