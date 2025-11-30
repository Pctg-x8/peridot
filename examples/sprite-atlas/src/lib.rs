use bedrock::{
    self as br, CommandBufferMut, DescriptorPoolMut, Device, RenderPass, ShaderModule,
    TypedVulkanStructure, VkHandle,
};
use peridot::math::One;

pub async fn game_main(e: &mut peridot::Engine<'_, impl peridot::NativeLinker>) {
    let mut camera = peridot::math::Camera {
        projection: Some(peridot::math::ProjectionMethod::Perspective {
            fov: 60.0f32.to_radians(),
        }),
        position: peridot::math::Vector3(0.0, 0.0, -5.0),
        rotation: peridot::math::Quaternion::ONE,
        depth_range: 0.1..100.0,
    };
    camera.look_at(peridot::math::Vector3(0.0, 0.0, 0.0));

    let mesh = peridot::Primitive::uv_plane_centric_xy(1.0, 0.0);

    let shader = e
        .load::<peridot_rendering_configuration::CompiledRenderingConfigurationVk>(
            "shaders.unlit_image_atlas",
        )
        .expect("no shader");

    let mut memory_manager = peridot_memory_manager::MemoryManager::new(e.graphics());

    let screen_size = e.back_buffer_size();
    let screen_aspect = screen_size.0 as f32 / screen_size.1 as f32;

    let render_pass = br::RenderPassObject::new(
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
    .expect("render_pass new");

    let dsl_ub1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[
            br::DescriptorType::UniformBuffer.make_binding(0, 1)
        ]),
    )
    .expect("dsl_ub1 new");
    let dsl_sb1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[
            br::DescriptorType::StorageBuffer.make_binding(0, 1)
        ]),
    )
    .expect("dsl_sb1 new");
    let dsl_mat = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(
            &shader
                .descriptor_set_bindings
                .iter()
                .enumerate()
                .map(|(n, t)| match t {
                    peridot_rendering_configuration::DescriptorTypeVk::CombinedImageSampler => {
                        br::DescriptorType::CombinedImageSampler.make_binding(n as _, 1)
                    }
                    peridot_rendering_configuration::DescriptorTypeVk::UniformBuffer { .. } => {
                        br::DescriptorType::UniformBuffer.make_binding(n as _, 1)
                    }
                    peridot_rendering_configuration::DescriptorTypeVk::StorageBuffer { .. } => {
                        br::DescriptorType::StorageBuffer.make_binding(n as _, 1)
                    }
                })
                .collect::<Vec<_>>(),
        ),
    )
    .expect("dsl_mat new");

    let (pipeline_layout, mut pipeline);
    match shader.passes["Unlit"] {
        peridot_rendering_configuration::ShadingPassVk::SimpleDeriveBuiltinPass { .. } => {
            unimplemented!("SimpleDeriveBuiltinPass")
        }
        peridot_rendering_configuration::ShadingPassVk::Custom {
            ref option_overrides,
            ref variants,
        } => {
            let peridot_rendering_configuration::Code {
                ref vertex_semantic_to_location,
                ref vertex_entry_point_name,
                ref fragment_entry_point_name,
                ref words,
            } = variants[&peridot_rendering_configuration::VariantKey { instancing: true }];

            let scissor_rects = [br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO)];
            let viewports = [scissor_rects[0].make_viewport(0.0..1.0)];

            let shader_module = br::ShaderModuleObject::new(
                e.graphics().device().clone(),
                &br::ShaderModuleCreateInfo::new(words),
            )
            .expect("material shader module new");
            let mut shader_stage_with_entry_names = Vec::with_capacity(2);
            shader_stage_with_entry_names.extend(vertex_entry_point_name.as_deref().map(|e| {
                (
                    br::ShaderStage::Vertex,
                    std::ffi::CString::new(e).expect("invalid entry point name"),
                )
            }));
            shader_stage_with_entry_names.extend(fragment_entry_point_name.as_deref().map(|e| {
                (
                    br::ShaderStage::Fragment,
                    std::ffi::CString::new(e).expect("invalid entry point name"),
                )
            }));

            pipeline_layout = br::PipelineLayoutObject::new(
                e.graphics().device().clone(),
                &br::PipelineLayoutCreateInfo::new(
                    &[
                        dsl_ub1.as_transparent_ref(),
                        dsl_sb1.as_transparent_ref(),
                        dsl_mat.as_transparent_ref(),
                    ],
                    &if shader.push_constant_buffer_size_bytes > 0 {
                        vec![br::PushConstantRange::new(
                            br::vk::VK_SHADER_STAGE_ALL,
                            0..shader.push_constant_buffer_size_bytes as _,
                        )]
                    } else {
                        vec![]
                    },
                ),
            )
            .expect("pipeline_layout new");
            let [objects] = e.graphics().device().new_graphics_pipeline_array(&[
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    render_pass.subpass(0),
                    &shader_stage_with_entry_names.iter().map(|&(s, ref e)| shader_module.on_stage(s, e)).collect::<Vec<_>>(),
                    &br::PipelineVertexInputStateCreateInfo::new(
                        &[br::vk::VkVertexInputBindingDescription::per_vertex_typed::<peridot::VertexUV>(0)],
                        &[
                            br::VertexInputAttributeDescription {
                                binding: 0,
                                location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Position(0)],
                                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                offset: core::mem::offset_of!(peridot::VertexUV, pos) as _
                            },
                            br::VertexInputAttributeDescription {
                                binding: 0,
                                location: vertex_semantic_to_location[&peridot_rendering_configuration::VertexInputSemantic::Texcoord(0)],
                                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                offset: core::mem::offset_of!(peridot::VertexUV, uv) as _
                            }
                        ]
                    ),
                    &br::PipelineInputAssemblyStateCreateInfo::new(br::PrimitiveTopology::TriangleStrip),
                    &br::PipelineViewportStateCreateInfo::new(&viewports, &scissor_rects),
                    &br::PipelineRasterizationStateCreateInfo::new(
                            match option_overrides.mode.unwrap_or_default() {
                                peridot_rendering_configuration::PolygonRasterizationMode::Point => br::PolygonMode::Point,
                                peridot_rendering_configuration::PolygonRasterizationMode::Line => br::PolygonMode::Line,
                                peridot_rendering_configuration::PolygonRasterizationMode::Fill => br::PolygonMode::Fill,
                            },
                            match option_overrides.culling.unwrap_or_default() {
                                peridot_rendering_configuration::FaceCulling::None => br::CullModeFlags::NONE,
                                peridot_rendering_configuration::FaceCulling::Front => br::CullModeFlags::FRONT,
                                peridot_rendering_configuration::FaceCulling::Back => br::CullModeFlags::BACK,
                                peridot_rendering_configuration::FaceCulling::Both => br::CullModeFlags::FRONT_AND_BACK,
                            },
                            match option_overrides.front_face.unwrap_or_default() {
                                peridot_rendering_configuration::FrontFace::CounterClockwise => br::FrontFace::CounterClockwise,
                                peridot_rendering_configuration::FrontFace::Clockwise => br::FrontFace::Clockwise,
                            },
                    ),
                    &br::PipelineColorBlendStateCreateInfo::new(&[
                        br::vk::VkPipelineColorBlendAttachmentState::NOBLEND
                    ])
                ).set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
            ],
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>).expect("pipeline new");
            pipeline = objects.clone_parent();
        }
    }

    let (
        fixed_device_buffer,
        [
            mesh_vertex_offset,
            camera_parameters_offset,
            object_parameters_offset,
        ],
    ) = memory_manager
        .allocate_device_local_buffer_with_content_array(
            e.graphics(),
            &[
                peridot::BufferContent::vertices_for(&mesh.vertices),
                peridot::BufferContent::uniform::<
                    peridot_rendering_configuration::UniformCameraParameters,
                >(),
                peridot::BufferContent::uniform::<
                    peridot_rendering_configuration::UniformObjectParameters,
                >(),
            ],
            br::BufferUsage::TRANSFER_DEST,
        )
        .expect("alloc device buffer(fixed)");
    let instance_buffer = memory_manager
        .allocate_device_local_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                core::mem::size_of::<peridot::math::Vector4F32>() * 1024,
                br::BufferUsage::VERTEX_BUFFER | br::BufferUsage::TRANSFER_DEST,
            ),
        )
        .expect("alloc device buffer(dynamic instances)");

    pub struct BufferInitContent {
        camera_parameters: peridot_rendering_configuration::UniformCameraParameters,
        object_parameters: peridot_rendering_configuration::UniformObjectParameters,
    }
    let mesh_vertices_size = core::mem::size_of_val(&mesh.vertices[..]);
    let mut upload_buffer = memory_manager
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                mesh_vertices_size + core::mem::size_of::<BufferInitContent>(),
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("alloc upload buffer");
    upload_buffer
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            p.clone_slice_to(0, &mesh.vertices);
            *p.get_mut_at(mesh_vertices_size) = BufferInitContent {
                camera_parameters: peridot_rendering_configuration::UniformCameraParameters {
                    view_projection_matrix: camera.view_projection_matrix(screen_aspect),
                },
                object_parameters: peridot_rendering_configuration::UniformObjectParameters {
                    transform_matrix: peridot::math::Matrix4::ONE,
                },
            };
        })
        .expect("write upload content");
    let mut instance_staging_buffer = memory_manager
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                core::mem::size_of::<peridot::math::Vector4F32>() * 1024,
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("alloc instance staging buffer");
    instance_staging_buffer
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            *p.get_mut_at(0) = peridot::math::Vector4(1.0f32, 1.0, 0.0, 0.0);
        })
        .expect("write instance staging data");

    e.submit_commands(|rec| {
        rec.pipeline_barrier(br::PipelineStageFlags::BOTTOM_OF_PIPE, br::PipelineStageFlags::TRANSFER, 0, &[br::vk::VkMemoryBarrier {
            sType: br::vk::VkMemoryBarrier::TYPE,
            pNext: core::ptr::null(),
            srcAccessMask: 0,
            dstAccessMask: br::AccessFlags::TRANSFER.write
        }], &[], &[])
        .copy_buffer(&upload_buffer, &fixed_device_buffer, &[
            br::BufferCopy {
                srcOffset: 0,
                dstOffset: mesh_vertex_offset,
                size: mesh_vertices_size as _
            },
            br::BufferCopy::copy_data::<peridot_rendering_configuration::UniformCameraParameters>(
                (mesh_vertices_size + core::mem::offset_of!(BufferInitContent, camera_parameters)) as _,
                camera_parameters_offset
            ),
            br::BufferCopy::copy_data::<peridot_rendering_configuration::UniformObjectParameters>(
                (mesh_vertices_size + core::mem::offset_of!(BufferInitContent, object_parameters)) as _,
                object_parameters_offset
            )
        ])
        .copy_buffer(&instance_staging_buffer, &instance_buffer, &[
            br::BufferCopy::mirror(0, (core::mem::size_of::<peridot::math::Vector4F32>() * 1024) as _)
        ]).pipeline_barrier(
            br::PipelineStageFlags::TRANSFER,
            br::PipelineStageFlags::VERTEX_INPUT | br::PipelineStageFlags::VERTEX_SHADER,
            0,
            &[
                br::vk::VkMemoryBarrier {
                    sType: br::vk::VkMemoryBarrier::TYPE,
                    pNext: core::ptr::null(),
                    srcAccessMask: br::AccessFlags::TRANSFER.write,
                    dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::UNIFORM_READ | br::AccessFlags::SHADER.read
                }
            ], &[], &[]
        )
    }).expect("setup command error");

    let mut back_buffer_views = e
        .iter_back_buffers()
        .map(|x| LocalImageView {
            handle: unsafe {
                br::vkfn_wrapper::create_image_view(
                    e.graphics().device().native_ptr(),
                    &br::ImageViewCreateInfo::new(
                        &x,
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        e.back_buffer_format(),
                    ),
                    None,
                )
                .expect("back buffer image view create")
            },
            device: e.graphics().device().clone(),
        })
        .collect::<Vec<_>>();
    let mut framebuffers = back_buffer_views
        .iter()
        .map(|b| {
            br::FramebufferObject::new(
                e.graphics().device().clone(),
                &br::FramebufferCreateInfo::new(
                    &render_pass,
                    &[b.as_transparent_ref()],
                    screen_size.0,
                    screen_size.1,
                ),
            )
            .expect("framebuffer create")
        })
        .collect::<Vec<_>>();

    let mut dp = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            3,
            &shader
                .descriptor_set_bindings
                .iter()
                .enumerate()
                .map(|(n, t)| match t {
                    peridot_rendering_configuration::DescriptorTypeVk::CombinedImageSampler => {
                        br::DescriptorType::CombinedImageSampler.make_size(1)
                    }
                    peridot_rendering_configuration::DescriptorTypeVk::UniformBuffer { .. } => {
                        br::DescriptorType::UniformBuffer.make_size(1)
                    }
                    peridot_rendering_configuration::DescriptorTypeVk::StorageBuffer { .. } => {
                        br::DescriptorType::StorageBuffer.make_size(1)
                    }
                })
                .chain([
                    br::DescriptorType::UniformBuffer.make_size(1),
                    br::DescriptorType::StorageBuffer.make_size(1),
                ])
                .collect::<Vec<_>>(),
        ),
    )
    .expect("descriptor pool create");
    let [
        descriptor_camera_parameters,
        descriptor_object_parameters,
        descriptor_mat,
    ] = dp
        .alloc_array(&[
            dsl_ub1.as_transparent_ref(),
            dsl_sb1.as_transparent_ref(),
            dsl_mat.as_transparent_ref(),
        ])
        .expect("descriptor alloc");
    e.graphics().device().update_descriptor_sets(
        &[
            descriptor_camera_parameters.binding_at(0).write(
                br::DescriptorContents::uniform_buffer(
                    &fixed_device_buffer,
                    camera_parameters_offset
                        ..camera_parameters_offset
                            + core::mem::size_of::<
                                peridot_rendering_configuration::UniformCameraParameters,
                            >() as u64,
                ),
            ),
            descriptor_object_parameters.binding_at(0).write(
                br::DescriptorContents::storage_buffer(
                    &fixed_device_buffer,
                    object_parameters_offset
                        ..object_parameters_offset
                            + core::mem::size_of::<
                                peridot_rendering_configuration::UniformObjectParameters,
                            >() as u64,
                ),
            ),
            descriptor_mat
                .binding_at(1)
                .write(br::DescriptorContents::storage_buffer(
                    &instance_buffer,
                    0..(core::mem::size_of::<peridot::math::Vector4F32>() * 1024) as u64,
                )),
        ],
        &[],
    );

    let mut render_cb = peridot::CommandBundle::new(
        e.graphics(),
        peridot::CBSubmissionType::Graphics,
        e.back_buffer_count(),
    )
    .expect("command bundle new");
    for (n, (mut cb, fb)) in render_cb.iter_mut().zip(framebuffers.iter()).enumerate() {
        unsafe {
            cb.begin(&br::CommandBufferBeginInfo::new())
                .expect("cb begin")
        }
        .begin_render_pass(
            &br::RenderPassBeginInfo::new(
                &render_pass,
                fb,
                br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO),
                &[br::ClearValue::color_f32([0.0; 4])],
            ),
            br::SubpassContents::Inline,
        )
        .bind_pipeline(br::PipelineBindPoint::Graphics, &pipeline)
        .bind_descriptor_sets(
            br::PipelineBindPoint::Graphics,
            &pipeline_layout,
            0,
            &[
                descriptor_camera_parameters,
                descriptor_object_parameters,
                descriptor_mat,
            ],
            &[],
        )
        .bind_vertex_buffer_array(
            0,
            &[fixed_device_buffer.as_transparent_ref()],
            &[mesh_vertex_offset],
        )
        .draw(4, 1, 0, 0)
        .end_render_pass()
        .end()
        .expect("cb end");
    }

    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let fd = match e.prepare_frame() {
                    Ok(x) => x,
                    Err(peridot::PrepareFrameError::FramebufferOutOfDate) => {
                        todo!("framebuffer out of date");
                    }
                };

                let render_cb = render_cb.nth_ref(fd.backbuffer_index as _);
                let mut render_batch = peridot::SubmissionBatchBuilder::new();
                render_batch.add_command_buffers([render_cb.as_transparent_ref()]);

                e.do_render(fd.backbuffer_index, None, render_batch)
                    .expect("do_render");
            }
            peridot::Event::Resize(new_size) => {
                e.wait_for_last_rendering_completion()
                    .expect("wait_for_last_rendering_completion");
                e.resize_presenter_backbuffers(new_size);
            }
        }
    }
}

struct LocalImageView {
    handle: br::vk::VkImageView,
    device: peridot::VulkanGfx,
}
impl Drop for LocalImageView {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(self.device.native_ptr(), self.handle, None);
        }
    }
}
impl br::VkHandle for LocalImageView {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}
