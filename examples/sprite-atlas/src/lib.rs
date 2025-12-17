use bedrock::{
    self as br, CommandBufferMut, DescriptorPoolMut, Device, ImageChild, RenderPass, ShaderModule,
    TypedVulkanStructure, VkHandle,
};
use ktx::Texture;
use peridot::math::One;
use rand::Rng;

pub async fn game_main(e: &mut peridot::Engine<'_, impl peridot::NativeLinker>) {
    let mut camera = peridot::math::Camera {
        projection: Some(peridot::math::ProjectionMethod::Perspective {
            fov: 60.0f32.to_radians(),
        }),
        position: peridot::math::Vector3(0.0, 0.0, -20.0),
        rotation: peridot::math::Quaternion::ONE,
        depth_range: 0.1..100.0,
    };
    camera.look_at(peridot::math::Vector3(0.0, 0.0, 0.0));

    let mut sprite_atlas = e
        .load::<peridot_sprite_atlas::SpriteAtlasAsset>("images.testatlas")
        .expect("no sprite atlas");
    assert!(sprite_atlas.content.needs_transcoding());
    sprite_atlas
        .content
        .transcode_basis(ktx::ffi::KTX_TTF_BC7_RGBA, ktx::TranscodeFlags::empty())
        .expect("transcode_basis");
    let atlas_width = sprite_atlas.width;
    let atlas_height = sprite_atlas.height;
    let offs = sprite_atlas
        .content
        .image_offset(0, 0, 0)
        .expect("image_offset");

    let mesh = peridot::Primitive::uv_plane_centric_xy(1.0, 0.0);

    let shader = e
        .load::<peridot_rendering_configuration::CompiledRenderingConfigurationVk>(
            "shaders.unlit_image_atlas",
        )
        .expect("no shader");

    let mut memory_manager = peridot_memory_manager::MemoryManager::new(e.graphics());

    let screen_size = e.back_buffer_size();
    let screen_aspect = screen_size.0 as f32 / screen_size.1 as f32;

    const DEPTH_BUFFER_FORMAT: br::Format = br::vk::VK_FORMAT_D32_SFLOAT;

    let render_pass = br::RenderPassObject::new(
        e.graphics().device().clone(),
        &br::RenderPassCreateInfo::new(
            &[
                e.back_buffer_attachment_desc()
                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store),
                br::vk::VkAttachmentDescription::new(
                    DEPTH_BUFFER_FORMAT,
                    br::ImageLayout::Undefined,
                    br::ImageLayout::DepthStencilAttachmentOpt,
                )
                .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare),
            ],
            &[br::SubpassDescription::new()
                .color_attachments(
                    &[br::vk::VkAttachmentReference::new(
                        0,
                        br::ImageLayout::ColorAttachmentOpt,
                    )],
                    &[],
                )
                .depth_stencil_attachment(&br::vk::VkAttachmentReference::new(
                    1,
                    br::ImageLayout::DepthStencilAttachmentOpt,
                ))],
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

    let (dsl_mat, descriptor_sizes, pipeline_layout, mut pipeline);
    match shader.passes["Unlit"] {
        peridot_rendering_configuration::ShadingPassVk::SimpleDeriveBuiltinPass { .. } => {
            unimplemented!("SimpleDeriveBuiltinPass")
        }
        peridot_rendering_configuration::ShadingPassVk::Custom {
            ref option_overrides,
            ref variants,
        } => {
            let peridot_rendering_configuration::Code {
                push_constant_buffer_size_bytes,
                ref descriptor_set_bindings,
                ref vertex_semantic_to_location,
                ref vertex_entry_point_name,
                ref fragment_entry_point_name,
                ref words,
            } = variants[&peridot_rendering_configuration::VariantKey { instancing: true }];

            dsl_mat = br::DescriptorSetLayoutObject::new(
                e.graphics().device().clone(),
                &br::DescriptorSetLayoutCreateInfo::new(
                    &descriptor_set_bindings
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
            descriptor_sizes = descriptor_set_bindings
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
                .collect::<Vec<_>>();

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
                    &if push_constant_buffer_size_bytes > 0 {
                        vec![br::PushConstantRange::new(
                            br::vk::VK_SHADER_STAGE_ALL,
                            0..push_constant_buffer_size_bytes as _,
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
                        br::vk::VkPipelineColorBlendAttachmentState::PREMULTIPLIED
                    ])
                ).set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new().enable_alpha_to_coverage())
                .set_depth_stencil_state(&br::PipelineDepthStencilStateCreateInfo::new().config_depth(Some(br::CompareOp::Less), true))
            ],
                None::<&br::PipelineCacheObject<peridot::DeviceObject>>).expect("pipeline new");
            pipeline = objects.clone_parent();
        }
    }

    let (fixed_device_buffer, [mesh_vertex_offset, camera_parameters_offset]) = memory_manager
        .allocate_device_local_buffer_with_content_array(
            e.graphics(),
            &[
                peridot::BufferContent::vertices_for(&mesh.vertices),
                peridot::BufferContent::uniform::<
                    peridot_rendering_configuration::UniformCameraParameters,
                >(),
            ],
            br::BufferUsage::TRANSFER_DEST,
        )
        .expect("alloc device buffer(fixed)");
    let (
        instance_buffer,
        [
            instance_offset_object_parameter,
            instance_offset_material_props,
        ],
    ) = memory_manager
        .allocate_device_local_buffer_with_content_array(
            e.graphics(),
            &[
                peridot::BufferContent::storage_dynarray::<
                    peridot_rendering_configuration::UniformObjectParameters,
                >(1024),
                peridot::BufferContent::storage_dynarray::<peridot::math::Vector4F32>(1024),
            ],
            br::BufferUsage::TRANSFER_DEST,
        )
        .expect("alloc device buffer(dynamic instances)");
    let sprite_atlas_image = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(
                br::Extent2D {
                    width: atlas_width,
                    height: atlas_height,
                },
                br::vk::VK_FORMAT_BC7_UNORM_BLOCK,
            )
            .set_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::TRANSFER_DEST),
        )
        .expect("alloc sprite atlas image");
    let sprite_atlas_image_view = br::ImageViewBuilder::new(
        sprite_atlas_image,
        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
    )
    .create()
    .expect("sprite atlas image view create");

    pub struct BufferInitContent {
        camera_parameters: peridot_rendering_configuration::UniformCameraParameters,
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
            };
        })
        .expect("write upload content");
    let mut image_upload_buffer = memory_manager
        .allocate_upload_linear_image_buffer(
            e.graphics(),
            atlas_width,
            atlas_height,
            peridot::PixelFormat::BC7,
            br::BufferUsage::TRANSFER_SRC,
        )
        .expect("image upload buffer alloc");
    image_upload_buffer
        .copy_content_from_slice(unsafe {
            core::slice::from_raw_parts(
                sprite_atlas.content.data().add(offs),
                sprite_atlas.content.data_size(),
            )
        })
        .expect("write image upload buffer");
    let (
        mut instance_staging_buffer,
        [
            instance_staging_offset_object_parameter,
            instance_staging_offset_material_props,
        ],
    ) = memory_manager
        .allocate_upload_buffer_with_content_array(
            e.graphics(),
            &[
                peridot::BufferContent::raw_dynarray::<
                    peridot_rendering_configuration::UniformObjectParameters,
                >(1024),
                peridot::BufferContent::raw_dynarray::<peridot::math::Vector4F32>(1024),
            ],
            br::BufferUsage::TRANSFER_SRC,
        )
        .expect("alloc instance staging buffer");
    instance_staging_buffer
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            *p.get_mut_at(instance_staging_offset_object_parameter as _) =
                peridot_rendering_configuration::UniformObjectParameters {
                    transform_matrix: peridot::math::Matrix4::ONE,
                };
            *p.get_mut_at(instance_staging_offset_material_props as _) =
                peridot::math::Vector4(1.0f32, 1.0, 0.0, 0.0);
        })
        .expect("write instance staging data");

    e.submit_commands(|rec| {
        rec.pipeline_barrier(br::PipelineStageFlags::BOTTOM_OF_PIPE, br::PipelineStageFlags::TRANSFER, 0, &[br::vk::VkMemoryBarrier {
            sType: br::vk::VkMemoryBarrier::TYPE,
            pNext: core::ptr::null(),
            srcAccessMask: 0,
            dstAccessMask: br::AccessFlags::TRANSFER.write
        }], &[], &[
            br::ImageMemoryBarrier::new(sprite_atlas_image_view.image(), br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1), br::ImageLayout::TransferDestOpt.from_undefined())
        ])
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
        ])
        .copy_buffer_to_image(&image_upload_buffer.inner, sprite_atlas_image_view.image(), br::ImageLayout::TransferDestOpt, &[
            br::vk::VkBufferImageCopy {
                bufferOffset: 0,
                bufferRowLength: image_upload_buffer.row_texels,
                bufferImageHeight: atlas_height,
                imageOffset: br::Offset3D { x: 0, y: 0, z: 0 },
                imageExtent: br::Extent3D { width: atlas_width, height: atlas_height, depth: 1 },
                imageSubresource: br::ImageSubresourceLayers::new(br::AspectMask::COLOR, 0, 0..1)
            }
        ])
        .copy_buffer(&instance_staging_buffer, &instance_buffer, &[
            br::BufferCopy {
                srcOffset: instance_staging_offset_object_parameter,
                dstOffset: instance_offset_object_parameter,
                size: (core::mem::size_of::<peridot_rendering_configuration::UniformObjectParameters>() * 1024) as _
            },
            br::BufferCopy {
                srcOffset: instance_staging_offset_material_props,
                dstOffset: instance_offset_material_props,
                size: (core::mem::size_of::<peridot::math::Vector4F32>() * 1024) as _
            }
        ]).pipeline_barrier(
            br::PipelineStageFlags::TRANSFER,
            br::PipelineStageFlags::VERTEX_INPUT | br::PipelineStageFlags::VERTEX_SHADER | br::PipelineStageFlags::FRAGMENT_SHADER,
            0,
            &[
                br::vk::VkMemoryBarrier {
                    sType: br::vk::VkMemoryBarrier::TYPE,
                    pNext: core::ptr::null(),
                    srcAccessMask: br::AccessFlags::TRANSFER.write,
                    dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::UNIFORM_READ | br::AccessFlags::SHADER.read
                }
            ], &[], &[
                br::ImageMemoryBarrier::new(sprite_atlas_image_view.image(), br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1), br::ImageLayout::TransferDestOpt.to(br::ImageLayout::ShaderReadOnlyOpt))]
        )
    }).expect("setup command error");

    let mut depth_buffer = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(screen_size, DEPTH_BUFFER_FORMAT).set_usage(
                br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT
                    | br::ImageUsageFlags::TRANSIENT_ATTACHMENT,
            ),
        )
        .expect("alloc depth buffer");
    let mut depth_buffer_view = br::ImageViewBuilder::new(
        depth_buffer,
        br::ImageSubresourceRange::new(br::AspectMask::DEPTH, 0..1, 0..1),
    )
    .create()
    .expect("depth buffer view create");
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
                    &[
                        b.as_transparent_ref(),
                        depth_buffer_view.as_transparent_ref(),
                    ],
                    screen_size.0,
                    screen_size.1,
                ),
            )
            .expect("framebuffer create")
        })
        .collect::<Vec<_>>();

    let smp = br::SamplerObject::new(e.graphics().device().clone(), &br::SamplerCreateInfo::new())
        .expect("sampler create");
    let mut dp = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            3,
            &descriptor_sizes,
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
                    &instance_buffer,
                    instance_offset_object_parameter
                        ..instance_offset_object_parameter
                            + (core::mem::size_of::<
                                peridot_rendering_configuration::UniformObjectParameters,
                            >() * 1024) as u64,
                ),
            ),
            descriptor_mat
                .binding_at(0)
                .write(br::DescriptorContents::CombinedImageSampler(vec![
                    br::DescriptorImageInfo::new(
                        &sprite_atlas_image_view,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )
                    .with_sampler(&smp),
                ])),
            descriptor_mat
                .binding_at(1)
                .write(br::DescriptorContents::storage_buffer(
                    &instance_buffer,
                    instance_offset_material_props
                        ..instance_offset_material_props
                            + (core::mem::size_of::<peridot::math::Vector4F32>() * 1024) as u64,
                )),
        ],
        &[],
    );

    const OBJECTS: usize = 100;
    let sprite_atlas_ids = sprite_atlas.sprites.keys().collect::<Vec<_>>();

    let mut update_cb =
        peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Graphics, 1)
            .expect("update command bundle new");
    update_cb
        .synchronized_nth(0)
        .begin(&br::CommandBufferBeginInfo::new())
        .expect("update command buffer begin")
        .pipeline_barrier(
            br::PipelineStageFlags::VERTEX_SHADER,
            br::PipelineStageFlags::TRANSFER,
            0,
            &[br::vk::VkMemoryBarrier {
                sType: br::vk::VkMemoryBarrier::TYPE,
                pNext: core::ptr::null(),
                srcAccessMask: br::AccessFlags::SHADER.read,
                dstAccessMask: br::AccessFlags::TRANSFER.write,
            }],
            &[],
            &[],
        )
        .copy_buffer(
            &instance_staging_buffer,
            &instance_buffer,
            &[
                br::BufferCopy {
                    srcOffset: instance_staging_offset_object_parameter,
                    dstOffset: instance_offset_object_parameter,
                    size: (core::mem::size_of::<
                        peridot_rendering_configuration::UniformObjectParameters,
                    >() * 1024) as _,
                },
                br::BufferCopy {
                    srcOffset: instance_staging_offset_material_props,
                    dstOffset: instance_offset_material_props,
                    size: (core::mem::size_of::<peridot::math::Vector4F32>() * 1024) as _,
                },
            ],
        )
        .pipeline_barrier(
            br::PipelineStageFlags::TRANSFER,
            br::PipelineStageFlags::VERTEX_SHADER,
            0,
            &[br::vk::VkMemoryBarrier {
                sType: br::vk::VkMemoryBarrier::TYPE,
                pNext: core::ptr::null(),
                srcAccessMask: br::AccessFlags::TRANSFER.write,
                dstAccessMask: br::AccessFlags::SHADER.read,
            }],
            &[],
            &[],
        )
        .end()
        .expect("update command buffer end");
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
                &[
                    br::ClearValue::color_f32([0.0; 4]),
                    br::ClearValue::depth_stencil(1.0, 0),
                ],
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
        .draw(4, OBJECTS as _, 0, 0)
        .end_render_pass()
        .end()
        .expect("cb end");
    }

    struct PlaneState {
        r: f32,
        p: peridot::math::Vector3F32,
        sprite_atlas_id: String,
    }
    let mut plane_states = [const {
        PlaneState {
            r: 0.0,
            p: peridot::math::Vector3(0.0, 5.0, 0.0),
            sprite_atlas_id: String::new(),
        }
    }; OBJECTS];
    let mut rng = rand::rng();
    for p in plane_states.iter_mut() {
        p.r = rng.random_range(0.0..360.0);
        p.p = peridot::math::Vector3(
            rng.random_range(-10.0..=10.0),
            rng.random_range(-10.0..=10.0),
            rng.random_range(-10.0..=10.0),
        );
        p.sprite_atlas_id = sprite_atlas_ids[rng.random_range(0..sprite_atlas_ids.len())].clone();
    }
    instance_staging_buffer
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            let material_props = p.slice_mut::<peridot::math::Vector4F32>(
                instance_staging_offset_material_props as _,
                1024,
            );

            for (n, p) in plane_states.iter_mut().enumerate() {
                material_props[n] = sprite_atlas.sprites[&p.sprite_atlas_id].uvst.clone();
            }
        })
        .expect("update instance staging buffer");

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

                instance_staging_buffer
                    .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
                        let object_parameters = p.slice_mut::<peridot_rendering_configuration::UniformObjectParameters>(instance_staging_offset_object_parameter as _, 1024);
                        let material_props = p.slice_mut::<peridot::math::Vector4F32>(instance_staging_offset_material_props as _, 1024);

                        for (n, p) in plane_states.iter_mut().enumerate() {
                            p.r += 60.0 * fd.delta_time.as_secs_f32();
                            p.p.1 -= 1.0 * fd.delta_time.as_secs_f32();
                            if p.p.1 < -10.0 {
                                p.p = peridot::math::Vector3(rng.random_range(-10.0..=10.0), p.p.1 + 20.0, rng.random_range(-10.0..=10.0));
                                p.sprite_atlas_id = sprite_atlas_ids[rng.random_range(0..sprite_atlas_ids.len())].clone();
                                material_props[n] = sprite_atlas.sprites[&p.sprite_atlas_id].uvst.clone();
                            }

                            object_parameters[n].transform_matrix = peridot::math::Matrix4::trs(p.p.clone(), peridot::math::Quaternion::new(p.r.to_radians(), peridot::math::Vector3::up()), peridot::math::Vector3::ONE);
                        }
                    })
                    .expect("update instance staging buffer");

                let render_cb = render_cb.nth_ref(fd.backbuffer_index as _);
                let mut render_batch = peridot::SubmissionBatchBuilder::new();
                render_batch.add_command_buffers([render_cb.as_transparent_ref()]);
                let update_cb = update_cb.nth_ref(0);
                let mut update_batch = peridot::SubmissionBatchBuilder::new();
                update_batch.add_command_buffers([update_cb.as_transparent_ref()]);

                e.do_render(fd.backbuffer_index, Some(update_batch), render_batch)
                    .expect("do_render");
            }
            peridot::Event::Resize(_new_size) => {}
        }
    }

    e.graphics_mut()
        .wait_operations()
        .expect("wait gfx operations");
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
