use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Device, GraphicsPipelineBuilder, Image,
    ImageChild, RenderPass, SubmissionBatch, VkHandle, VkRawHandle, VulkanStructure,
};
use peridot_vertex_processing_pack::PvpShaderModules;

#[repr(C)]
pub struct Vertex {
    pub pos: peridot::math::Vector2<f32>,
}
#[repr(C)]
pub struct BoxInstance {
    pub pos_st: peridot::math::Vector4<f32>,
    pub col: peridot::math::Vector4<f32>,
}

pub async fn game_main(e: &mut peridot::Engine<impl peridot::NativeLinker>) {
    let screen_size = e
        .back_buffer(0)
        .expect("no backbuffers?")
        .image()
        .size()
        .as_2d_ref()
        .clone();
    let scissor_rect = screen_size.into_rect(br::vk::VkOffset2D::ZERO);
    let viewport = scissor_rect.make_viewport(0.0..1.0);

    let main_renderpass = br::RenderPassBuilder::new(
        &[e.back_buffer_attachment_desc()
            .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)],
        &[br::SubpassDescription::new().color_attachments(
            &[br::AttachmentReference::new(
                0,
                br::ImageLayout::ColorAttachmentOpt,
            )],
            &[],
        )],
        &[peridot::SubpassDependencyTemplates::to_color_attachment_in(
            None, 0, true,
        )],
    )
    .create(e.graphics().device().clone())
    .expect("Failed to create main renderpass");
    let main_framebuffers = e
        .iter_back_buffers()
        .map(|bb| {
            br::FramebufferBuilder::new_with_attachment(&main_renderpass, bb)
                .create()
                .expect("Failed to create main framebuffer")
        })
        .collect::<Vec<_>>();

    let unlit_fill_shader = PvpShaderModules::new(
        e.graphics().device(),
        e.load("shaders.unlit_fill")
            .expect("Failed to load unlit_fill shader"),
    )
    .expect("Failed to create unlit_fill shader modules");
    let unlit_fill_pipeline_layout = br::PipelineLayoutBuilder::new(
        &[],
        &[
            br::PushConstantRange::for_type::<peridot::math::Vector2<f32>>(
                br::ShaderStage::VERTEX,
                0,
            ),
        ],
    )
    .create(e.graphics().device().clone())
    .expect("Failed to create pipeline layout");
    let unlit_fill_pipeline = {
        let mut builder = br::NonDerivedGraphicsPipelineBuilder::new(
            &unlit_fill_pipeline_layout,
            main_renderpass.subpass(0),
            unlit_fill_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP),
        );
        builder
            .viewport_scissors(
                br::DynamicArrayState::Static(&[viewport]),
                br::DynamicArrayState::Static(&[scissor_rect]),
            )
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .multisample_state(Some(br::MultisampleState::new()));

        builder
            .create(
                e.graphics().device().clone(),
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create unlit_fill pipeline")
    };

    let mut pmm = peridot_memory_manager::MemoryManager::new(e.graphics());
    let [vertex_buffer, instance_buffer] = pmm
        .allocate_device_local_buffer_array(
            e.graphics(),
            [
                br::BufferDesc::new_for_type::<[Vertex; 4]>(
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                br::BufferDesc::new_for_type::<[BoxInstance; 2]>(
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
            ],
        )
        .expect("Failed to create device local buffers");
    #[repr(C)]
    struct BufferInitContent {
        vertex: [Vertex; 4],
        instance: [BoxInstance; 2],
    }
    let mut init_buffer = pmm
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferDesc::new_for_type::<BufferInitContent>(br::BufferUsage::TRANSFER_SRC),
        )
        .expect("Failed to create init buffer");
    init_buffer
        .write_content(BufferInitContent {
            vertex: [
                Vertex {
                    pos: peridot::math::Vector2(0.0, 0.0),
                },
                Vertex {
                    pos: peridot::math::Vector2(1.0, 0.0),
                },
                Vertex {
                    pos: peridot::math::Vector2(0.0, 1.0),
                },
                Vertex {
                    pos: peridot::math::Vector2(1.0, 1.0),
                },
            ],
            instance: [
                BoxInstance {
                    pos_st: peridot::math::Vector4(100.0, 100.0, 8.0, 8.0),
                    col: peridot::math::Vector4(1.0, 1.0, 1.0, 1.0),
                },
                BoxInstance {
                    pos_st: peridot::math::Vector4(160.0, 100.0, 8.0, 116.0),
                    col: peridot::math::Vector4(1.0, 1.0, 0.0, 1.0),
                },
            ],
        })
        .expect("Failed to write init buffer content");
    let content_init = e
        .submit_commands_async(|r| {
            r.copy_buffer(
                &init_buffer,
                &vertex_buffer,
                &[br::BufferCopy::copy_data::<[Vertex; 4]>(
                    core::mem::offset_of!(BufferInitContent, vertex) as _,
                    0,
                )],
            )
            .copy_buffer(
                &init_buffer,
                &instance_buffer,
                &[br::BufferCopy::copy_data::<[BoxInstance; 2]>(
                    core::mem::offset_of!(BufferInitContent, instance) as _,
                    0,
                )],
            )
            .pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::VERTEX_INPUT,
                false,
                &[br::vk::VkMemoryBarrier {
                    sType: br::vk::VkMemoryBarrier::TYPE,
                    pNext: core::ptr::null(),
                    srcAccessMask: br::AccessFlags::TRANSFER.write,
                    dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ,
                }],
                &[],
                &[],
            )
        })
        .expect("Failed to send init commands");

    let mut ui_render_cp = br::CommandPoolBuilder::new(e.graphics_queue_family_index())
        .create(e.graphics().device().clone())
        .expect("Failed to create ui render command pool");
    let [mut ui_render_cb] = ui_render_cp
        .alloc_array::<1>(false)
        .expect("Failed to allocate ui render command buffer");
    unsafe {
        let inherit_info = br::vk::VkCommandBufferInheritanceInfo {
            sType: br::vk::VkCommandBufferInheritanceInfo::TYPE,
            pNext: core::ptr::null(),
            renderPass: main_renderpass.native_ptr(),
            subpass: 0,
            framebuffer: br::vk::VkFramebuffer::NULL,
            occlusionQueryEnable: false as _,
            queryFlags: 0,
            pipelineStatistics: 0,
        };
        let begin_info = br::vk::VkCommandBufferBeginInfo {
            sType: br::vk::VkCommandBufferBeginInfo::TYPE,
            pNext: core::ptr::null(),
            flags: br::vk::VK_COMMAND_BUFFER_USAGE_RENDER_PASS_CONTINUE_BIT
                | br::vk::VK_COMMAND_BUFFER_USAGE_SIMULTANEOUS_USE_BIT,
            pInheritanceInfo: &inherit_info,
        };

        ui_render_cb
            .begin_raw(&begin_info, e.graphics().device())
            .expect("Failed to begin ui render command recording")
    }
    .bind_graphics_pipeline(&unlit_fill_pipeline)
    .push_constant(
        &unlit_fill_pipeline_layout,
        br::ShaderStage::VERTEX,
        0,
        &peridot::math::Vector2(640.0f32, 480.0),
    )
    .bind_vertex_buffers(
        0,
        &[
            br::BufferObjectRef::new(&vertex_buffer),
            br::BufferObjectRef::new(&instance_buffer),
        ],
        &[0, 0],
    )
    .draw(4, 2, 0, 0)
    .end()
    .expect("Failed to finish ui render command recording");

    let mut render_cp = br::CommandPoolBuilder::new(e.graphics_queue_family_index())
        .create(e.graphics().device().clone())
        .expect("Failed to create render command pool");
    let mut render_cb = render_cp
        .alloc(e.back_buffer_count() as _, true)
        .expect("Failed to allocate render command buffers");
    for (cb, fb) in render_cb.iter_mut().zip(main_framebuffers.iter()) {
        unsafe {
            cb.begin(e.graphics().device())
                .expect("Failed to begin render command recording")
                .begin_render_pass(
                    &main_renderpass,
                    fb,
                    scissor_rect,
                    &[br::ClearValue::color_f32([0.1, 0.2, 0.3, 0.0])],
                    false,
                )
                .execute_commands(&[ui_render_cb.native_ptr()])
                .end_render_pass()
                .end()
                .expect("Failed to finish render command recording");
        }
    }

    content_init.await.expect("Failed to initialize content");

    while let Some(ev) = e.event_receivers().wait_for_event().await {
        match ev {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let fd = e.prepare_frame().expect("Failed to prepare frame");

                e.do_render(
                    fd.backbuffer_index,
                    None::<br::EmptySubmissionBatch>,
                    br::EmptySubmissionBatch.with_command_buffers(
                        &render_cb[fd.backbuffer_index as usize..=fd.backbuffer_index as usize],
                    ),
                )
                .expect("Failed to render");
            }
            peridot::Event::Resize(ns) => {
                println!("not implemented: Resize: {ns:?}");
            }
        }
    }

    unsafe {
        e.graphics().device().wait().expect("Failed to wait works");
    }
}
