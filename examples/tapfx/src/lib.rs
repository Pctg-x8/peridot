use bedrock::{self as br, CommandBufferMut, DescriptorPoolMut, RenderPass, VkHandle};
use br::{Device, Image, ImageChild, ImageSubresourceSlice, SubmissionBatch};
use peridot::mthelper::SharedRef;
use peridot_command_object::{
    BeginRenderPass, BindGraphicsPipeline, BufferImageDataDesc, BufferUsage,
    ColorAttachmentBlending, CopyBuffer, CopyBufferToImage, DescriptorSets, EndRenderPass,
    GraphicsCommand, GraphicsCommandCombiner, GraphicsCommandSubmission, ImageResourceRange,
    PipelineBarrier, RangedBuffer, RangedImage, StandardMesh,
};
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_semantic_shader::{ShaderPackAsset, VertexInputSemantic};

#[repr(C)]
#[derive(Clone)]
pub struct UniformValues {
    pub mat: peridot::math::Matrix4F32,
    pub time: f32,
    pub _resv: f32,
    pub offset: peridot::math::Vector2F32,
}

pub const INPUT_PLANE_DOWN: u16 = 0;
pub const INPUT_PLANE_LEFT: u8 = 0;
pub const INPUT_PLANE_TOP: u8 = 1;
fn init_controls(e: &mut peridot::Engine<impl peridot::NativeLinker>) {
    e.input_mut()
        .map(peridot::NativeButtonInput::Mouse(0), INPUT_PLANE_DOWN);
    e.input_mut()
        .map(peridot::NativeButtonInput::Touch(0), INPUT_PLANE_DOWN);
    e.input_mut()
        .map(peridot::NativeAnalogInput::MouseX, INPUT_PLANE_LEFT);
    e.input_mut()
        .map(peridot::NativeAnalogInput::TouchMoveX(0), INPUT_PLANE_LEFT);
    e.input_mut()
        .map(peridot::NativeAnalogInput::MouseY, INPUT_PLANE_TOP);
    e.input_mut()
        .map(peridot::NativeAnalogInput::TouchMoveY(0), INPUT_PLANE_TOP);
}

pub async fn game_main<'q>(e: &mut peridot::Engine<'q, impl peridot::NativeLinker>) {
    init_controls(e);

    let bb_size = e.back_buffer_size();

    let renderpass = br::RenderPassObject::new(
        e.graphics().device().clone(),
        &br::RenderPassCreateInfo::new(
            &[e.back_buffer_attachment_desc()
                .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)],
            &[br::SubpassDescription::new().color_attachments(
                &[br::vk::VkAttachmentReference::new(
                    0,
                    br::ImageLayout::ColorAttachmentOpt,
                )],
                &[],
            )],
            &[peridot::SubpassDependencyTemplates::to_color_attachment_in(
                None, 0, true,
            )],
        ),
    )
    .expect("Failed to create RenderPass");
    let backbuffer_resources = e.iter_back_buffers().map(|x| unsafe {}).collect::<Vec<_>>();
    let framebuffers: Vec<_> = backbuffer_resources
        .iter()
        .map(|b| {
            br::FramebufferObject::new(
                e.graphics_device().clone(),
                &br::FramebufferCreateInfo::new(
                    &renderpass,
                    &[b.as_transparent_ref()],
                    bb_size.width,
                    bb_size.height,
                ),
            )
        })
        .collect::<Result<_, _>>()
        .expect("Failed to create Framebuffer");

    let smp = br::SamplerObject::new(e.graphics().device().clone(), &br::SamplerCreateInfo::new())
        .expect("Failed to create sampler");
    let dsl = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::UniformBuffer
            .make_binding(0, 1)
            .only_for_vertex()]),
    )
    .expect("Failed to create DescriptorSetLayout");
    let dsl2 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::CombinedImageSampler
            .make_binding(0, 1)
            .only_for_fragment()
            .with_immutable_samplers(&[smp.as_transparent_ref()])]),
    )
    .expect("Failed to create DescriptorSetLayout for FragmentShader");
    let mut dp = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            2,
            &[
                br::DescriptorType::UniformBuffer.make_size(1),
                br::DescriptorType::CombinedImageSampler.make_size(1),
            ],
        ),
    )
    .expect("Failed to create DescriptorPool");
    let [descriptor_obj, descriptor_tex] = dp
        .alloc_array(&[dsl.as_transparent_ref(), dsl2.as_transparent_ref()])
        .expect("Failed to alloc Required Descriptors");

    let shaders = e
        .load::<ShaderPackAsset>("shaders.blit")
        .expect("Failed to load blit shader asset")
        .instantiate(e.graphics().device().clone())
        .expect("Failed to instantiate blit shader");
    let pl = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &[dsl.as_transparent_ref(), dsl2.as_transparent_ref()],
            &[],
        ),
    )
    .expect("Failed to create PipelineLayout");

    let scissors = [bb_size.clone().into_rect(br::vk::VkOffset2D::ZERO)];
    let viewports = [scissors[0].make_viewport(0.0..1.0)];
    let [pipeline] = e
        .graphics()
        .device()
        .new_graphics_pipeline_array(
            &[br::GraphicsPipelineCreateInfo::new(
                &pl,
                renderpass.subpass(0),
                &[
                    shaders.pipeline_vertex_shader(),
                    shaders.pipeline_fragment_shader().expect("no fsh?"),
                ],
                &br::PipelineVertexInputStateCreateInfo::new(
                    &[br::vk::VkVertexInputBindingDescription::per_vertex_typed::<
                        peridot::VertexUV2D,
                    >(0)],
                    &[
                        br::vk::VkVertexInputAttributeDescription {
                            binding: 0,
                            location: shaders
                                .resolve_input_semantic_location(VertexInputSemantic::Position(0))
                                .expect("no position input?"),
                            format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                            offset: core::mem::offset_of!(peridot::VertexUV2D, pos) as _,
                        },
                        br::vk::VkVertexInputAttributeDescription {
                            binding: 0,
                            location: shaders
                                .resolve_input_semantic_location(VertexInputSemantic::Texcoord(0))
                                .expect("no texcoord input?"),
                            format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                            offset: core::mem::offset_of!(peridot::VertexUV2D, uv) as _,
                        },
                    ],
                ),
                &br::PipelineInputAssemblyStateCreateInfo::new(
                    br::PrimitiveTopology::TriangleStrip,
                ),
                &br::PipelineViewportStateCreateInfo::new_array(&viewports, &scissors),
                &br::PipelineRasterizationStateCreateInfo::new(
                    br::PolygonMode::Fill,
                    br::CullModeFlags::NONE,
                    br::FrontFace::CounterClockwise,
                ),
                &br::PipelineColorBlendStateCreateInfo::new(&[
                    ColorAttachmentBlending::PREMULTIPLIED_ALPHA.into_vk(),
                ]),
            )
            .multisample_state(&br::PipelineMultisampleStateCreateInfo::new())],
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Failed to create GraphicsPipeline");
    let pipeline = pipeline.clone_parent();

    let main_image_data: peridot_image::PNG = e
        .load("images.peridot_default_tapfx_circle")
        .expect("Failed to load main_image_data");
    let sprite_plane = peridot::Primitive::uv_plane_centric(32.0);

    let mut memory_manager = MemoryManager::new(e.graphics());

    let (buffer, offsets) = memory_manager
        .allocate_device_local_buffer_with_contents(
            e.graphics(),
            [
                peridot::BufferContent::vertices_for(&sprite_plane.vertices),
                peridot::BufferContent::uniform::<UniformValues>(),
            ],
            br::BufferUsage::TRANSFER_DEST,
        )
        .expect("Failed to create device buffer");
    let buffer = SharedRef::new(buffer);
    let vertex_buffer =
        RangedBuffer::from_offset_length(buffer.clone(), offsets[0], sprite_plane.byte_length());
    let uniform_buffer = RangedBuffer::for_type::<UniformValues>(buffer.clone(), offsets[1]);
    let main_image = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(main_image_data.0.size, main_image_data.0.format as _)
                .sampled()
                .transfer_dest()
                .init_layout(br::ImageLayout::Preinitialized),
        )
        .expect("Failed to allocate main image");

    let (main_image_byte_length, main_image_alignment, main_image_row_texels) = memory_manager
        .compute_optimal_linear_image_buffer_layout(
            *main_image_data.0.size.x(),
            *main_image_data.0.size.y(),
            main_image_data.0.format,
        );
    let mut uniform_mut_buffer: RangedBuffer<_> = memory_manager
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                core::mem::size_of::<UniformValues>(),
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("Failed to allocate mutable buffer")
        .into();
    let (mut staging_buffer, staging_offsets) = memory_manager
        .allocate_upload_buffer_with_contents(
            e.graphics(),
            [
                peridot::BufferContent::raw_for_slice(&sprite_plane.vertices),
                peridot::BufferContent::Raw(main_image_byte_length, main_image_alignment),
            ],
            br::BufferUsage::TRANSFER_SRC,
        )
        .expect("Failed to create staging buffer");
    staging_buffer
        .guard_map(BufferMapMode::Write, |ptr| unsafe {
            ptr.clone_slice_to(staging_offsets[0] as _, &sprite_plane.vertices);
            ptr.copy_slice_to(staging_offsets[1] as _, main_image_data.0.u8_pixels());
        })
        .expect("Failed to stage initial vertex buffer memory");

    {
        let all_stg_buffer = RangedBuffer::from(&staging_buffer);
        let image = RangedImage::single_color_plane(&main_image);

        let init_vertices = CopyBuffer::new(&staging_buffer, &vertex_buffer.0).with_range(
            staging_offsets[0],
            vertex_buffer.offset(),
            sprite_plane.byte_length(),
        );
        let init_main_image = CopyBufferToImage::new(&staging_buffer, &main_image).with_range(
            BufferImageDataDesc::new(staging_offsets[1], main_image_row_texels),
            ImageResourceRange::for_single_color_from_rect2d(
                main_image.size().wh().into_rect(br::vk::VkOffset2D::ZERO),
            ),
        );
        let init = (init_vertices, init_main_image);

        let [image_in_barrier, image_out_barrier] = image.barrier3(
            br::ImageLayout::Preinitialized,
            br::ImageLayout::TransferDestOpt,
            br::ImageLayout::ShaderReadOnlyOpt,
        );
        let in_barriers = PipelineBarrier::new()
            .by_region()
            .with_barrier(image_in_barrier)
            .with_barrier(
                vertex_buffer
                    .make_ref()
                    .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
            )
            .with_barrier(
                all_stg_buffer.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
            );
        let out_barriers = PipelineBarrier::new()
            .by_region()
            .with_barrier(image_out_barrier)
            .with_barriers([
                vertex_buffer
                    .make_ref()
                    .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_BUFFER),
                uniform_buffer
                    .make_ref()
                    .usage_barrier(BufferUsage::UNUSED, BufferUsage::VERTEX_UNIFORM),
            ]);

        init.between(in_barriers, out_barriers)
            .submit(e)
            .expect("Failed to execute init command");
    }

    let main_image_view = main_image
        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
        .view_builder()
        .create()
        .expect("Failed to create main image view");

    e.graphics().device().update_descriptor_sets(
        &[
            br::DescriptorPointer::new(descriptor_obj.into(), 0).write(
                br::DescriptorContents::UniformBuffer(vec![
                    uniform_buffer.make_descriptor_buffer_ref()
                ]),
            ),
            br::DescriptorPointer::new(descriptor_tex.into(), 0).write(
                br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageInfo::new(
                    &main_image_view,
                    br::ImageLayout::ShaderReadOnlyOpt,
                )]),
            ),
        ],
        &[],
    );

    let mut update_data = UniformValues {
        mat: peridot::math::Camera {
            projection: Some(peridot::math::ProjectionMethod::UI {
                design_width: bb_size.width as _,
                design_height: bb_size.height as _,
            }),
            ..Default::default()
        }
        .projection_matrix(1.0),
        time: 1.0,
        offset: peridot::math::Vector2(0.0, 0.0),
        _resv: 0.0,
    };
    let mut update_commands =
        peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Transfer, 1)
            .expect("Failed to allocate update commands");
    {
        let copy = CopyBuffer::new(&uniform_mut_buffer.0, &uniform_buffer.0)
            .with_range_for_type::<UniformValues>(0, uniform_buffer.offset());

        let [uniform_in_barrier, uniform_out_barrier] = uniform_buffer
            .make_ref()
            .usage_barrier3_switching(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST);
        let [dynamic_in_barrier, dynamic_out_barrier] = uniform_mut_buffer
            .make_ref()
            .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);
        let in_barriers = PipelineBarrier::new()
            .with_barrier(uniform_in_barrier)
            .with_barrier(dynamic_in_barrier);
        let out_barriers = PipelineBarrier::new()
            .with_barrier(uniform_out_barrier)
            .with_barrier(dynamic_out_barrier);

        copy.between(in_barriers, out_barriers)
            .execute_and_finish(unsafe {
                update_commands[0]
                    .begin(&br::CommandBufferBeginInfo::new(), e.graphics_device())
                    .expect("Failed to begin recording update commands")
            })
            .expect("Failed to record commands");
    }

    let descriptor_sets = DescriptorSets(vec![descriptor_obj, descriptor_tex]);
    let mesh = StandardMesh {
        vertex_buffers: vec![vertex_buffer],
        vertex_count: 4,
    };
    let setup = (
        BindGraphicsPipeline(pipeline),
        descriptor_sets.into_bind_graphics(pl),
    );
    let color_renders = mesh.draw(1).after_of(setup);

    let mut main_commands = peridot::CommandBundle::new(
        e.graphics(),
        peridot::CBSubmissionType::Graphics,
        e.back_buffer_count(),
    )
    .expect("Failed to allocate render commands");
    for (b, fb) in main_commands.iter_mut().zip(&framebuffers) {
        let rp = BeginRenderPass::new(
            &renderpass,
            fb,
            scissors[0].clone(),
            br::SubpassContents::Inline,
        )
        .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

        (&color_renders)
            .between(rp, EndRenderPass)
            .execute_and_finish(unsafe {
                b.begin(&br::CommandBufferBeginInfo::new(), e.graphics_device())
                    .expect("Failed to begin recording main commands")
            })
            .expect("Failed to record commands");
    }

    let mut last_mouse_input = false;
    loop {
        match e.next_event().await {
            peridot::Event::NextFrame => {
                let fd = e.prepare_frame().expect("Failed to prepare frame");

                update_data.time += fd.delta_time.as_secs_f32();

                let current_mouse_input = e.input().button_pressing_time(INPUT_PLANE_DOWN)
                    > std::time::Duration::default();
                if !last_mouse_input && current_mouse_input {
                    update_data.time = 0.0;
                    let (ox, oy) = e.input().get_plane_position(0).unwrap_or((0.0, 0.0));
                    update_data.offset = peridot::math::Vector2(ox, oy);
                }
                last_mouse_input = current_mouse_input;

                uniform_mut_buffer
                    .0
                    .write_content(update_data.clone())
                    .expect("Failed to map dynamic buffer");

                e.do_render(
                    fd.backbuffer_index,
                    Some(br::EmptySubmissionBatch.with_command_buffers(&update_commands)),
                    br::EmptySubmissionBatch.with_command_buffers(
                        &main_commands[fd.backbuffer_index as usize..=fd.backbuffer_index as usize],
                    ),
                )
                .expect("Failed to present");
            }
            peridot::Event::Shutdown => break,
            peridot::Event::Resize(ns) => {
                println!("not implemented: Resize: {ns:?}");
            }
        }
    }

    unsafe {
        e.graphics()
            .device()
            .wait()
            .expect("Failed to waiting shutdown");
    }
}
