use bedrock::{
    self as br, CommandBufferMut, DescriptorPoolMut, Device, RenderPass, ShaderModule,
    TypedVulkanStructure, VkHandle,
};
use futures_util::StreamExt;
use peridot::math::One;

pub async fn game_main<'e, NL: peridot::NativeLinker>(e: &mut peridot::Engine<'e, NL>) {
    let mut backbuffer_size = e.back_buffer_size();

    let shader = e
        .load_async::<peridot_rendering_configuration::CompiledRenderingConfigurationVk>(
            "shaders.test",
        )
        .await
        .expect("shader load failed");
    let peridot_rendering_configuration::ShadingPassVk::Custom { ref variants, .. } =
        shader.passes["Unlit"]
    else {
        unreachable!();
    };
    let shading_variant =
        &variants[&peridot_rendering_configuration::VariantKey { instancing: false }];
    let shader_stage_with_entry_names = shading_variant
        .vertex_entry_point_name
        .as_deref()
        .map(|e| {
            (
                br::ShaderStage::Vertex,
                std::ffi::CString::new(e).expect("invalid entry point name"),
            )
        })
        .into_iter()
        .chain(
            shading_variant
                .fragment_entry_point_name
                .as_deref()
                .map(|e| {
                    (
                        br::ShaderStage::Fragment,
                        std::ffi::CString::new(e).expect("invlaid entry point name"),
                    )
                })
                .into_iter(),
        )
        .collect::<Vec<_>>();
    let shader_module = br::ShaderModuleObject::new(
        e.graphics_device().clone(),
        &br::ShaderModuleCreateInfo::new(&shading_variant.words),
    )
    .expect("shnader_module.create");
    let dsl_ub1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[
            br::DescriptorType::UniformBuffer.make_binding(0, 1)
        ]),
    )
    .expect("dsl_ub1 new");
    let pl = br::PipelineLayoutObject::new(
        e.graphics_device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &[dsl_ub1.as_transparent_ref(), dsl_ub1.as_transparent_ref()],
            &if shading_variant.push_constant_buffer_size_bytes > 0 {
                vec![br::PushConstantRange::new(
                    br::vk::VK_SHADER_STAGE_ALL,
                    0..shading_variant.push_constant_buffer_size_bytes as _,
                )]
            } else {
                vec![]
            },
        ),
    )
    .expect("pl.create");

    let mut camera = peridot::math::Camera {
        projection: Some(peridot::math::ProjectionMethod::Perspective {
            fov: 60.0f32.to_radians(),
        }),
        position: peridot::math::Vector3(0.0, 2.0, -10.0),
        rotation: peridot::math::Quaternion::ONE,
        depth_range: 0.1..100.0,
    };
    camera.look_at(peridot::math::Vector3(0.0, 0.0, 0.0));

    let asset = peridot_mesh::AssetAsync::open(
        e.open_raw_asset_async::<peridot_mesh::AssetCore>("test-mesh0-1")
            .await
            .expect("open asset"),
    )
    .await
    .expect("read contents");

    let mut device_buffer_contents = Vec::new();
    let mut upload_buffer_ranges = Vec::new();
    let mut upload_buffer_top = 0;

    let primitive_topology = asset.header.primitive_topology.into_vk();
    let index_count = asset.index_count();
    let index_type_vk = if let peridot_mesh::IndexStream::Stream {
        index_type,
        ref buffer,
    } = asset.index_stream
    {
        device_buffer_contents.extend(asset.index_stream.buffer_content());
        upload_buffer_ranges.push(0..buffer.byte_length as u64);
        upload_buffer_top += buffer.byte_length as u64;

        Some(index_type.into_vk())
    } else {
        None
    };
    let mut v_bindings = Vec::with_capacity(asset.vertex_streams.len());
    let mut v_attributes = Vec::new();
    for (binding, (v, attributes)) in asset.vertex_streams.iter().enumerate() {
        let mut buffer_stride = 0;
        for (a, ad) in attributes {
            buffer_stride = buffer_stride.max(ad.offset as u32 + ad.element_type.size() as u32);
            if let Some(&loc) = shading_variant
                .vertex_semantic_to_location
                .get(&a.into_semantic())
            {
                v_attributes.push(br::VertexInputAttributeDescription(
                    br::vk::VkVertexInputAttributeDescription {
                        binding: binding as _,
                        location: loc,
                        format: ad.element_type.into_vk_format(),
                        offset: ad.offset as _,
                    },
                ));
            } else {
                tracing::warn!("attribute {a:?} is not supported on the shader, ignoring");
            }
        }

        device_buffer_contents.push(v.buffer_content());
        let upload_buffer_base = peridot::math::round_up_pow2n_u64(
            upload_buffer_top,
            v.buffer.device_alignment_requirement as u64,
        );
        upload_buffer_ranges
            .push(upload_buffer_base..upload_buffer_base + v.buffer.byte_length as u64);
        upload_buffer_top = upload_buffer_base + v.buffer.byte_length as u64;

        v_bindings.push(br::VertexInputBindingDescription::per_vertex(
            binding as _,
            buffer_stride,
        ));
    }

    let mut memory_manager = peridot_memory_manager::MemoryManager::new(e.graphics());
    let (device_buffer, device_buffer_offsets) = memory_manager
        .allocate_device_local_buffer_with_contents(
            e.graphics(),
            device_buffer_contents.into_iter().chain([
                peridot::BufferContent::uniform::<
                    peridot_rendering_configuration::UniformCameraParameters,
                >(),
                peridot::BufferContent::uniform::<
                    peridot_rendering_configuration::UniformObjectParameters,
                >(),
            ]),
            br::BufferUsage::TRANSFER_DEST,
        )
        .expect("device_buffer.alloc");
    struct BufferInitContent {
        camera_parameters: peridot_rendering_configuration::UniformCameraParameters,
        object_parameters: peridot_rendering_configuration::UniformObjectParameters,
    }
    let buffer_init_content_base_offset = peridot::math::round_up_pow2n_u64(upload_buffer_top, 16);
    let mut upload_buffer = memory_manager
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                (buffer_init_content_base_offset as usize + size_of::<BufferInitContent>()) as _,
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("upload_buffer.alloc");
    let upload_mapped = upload_buffer
        .map(peridot_memory_manager::BufferMapMode::Write)
        .expect("upload_buffer.map");
    unsafe {
        let vertex_buffer_start = if asset.has_index() { 1 } else { 0 };
        let index_load_job = async {
            if asset.has_index() {
                asset
                    .read_index_buffer_into(upload_mapped.ptr().slice_mut(
                        upload_buffer_ranges[0].start as _,
                        asset.index_stream_byte_length() as _,
                    ))
                    .await
                    .expect("asset.index_buffer.read");
            }
        };
        let vertex_load_jobs = upload_buffer_ranges[vertex_buffer_start..]
            .iter()
            .enumerate()
            .map(|(stream_index, upload_range)| {
                let asset = &asset;
                let upload_mapped = &upload_mapped;
                async move {
                    asset
                        .read_vertex_buffer_into(
                            stream_index,
                            upload_mapped.ptr().slice_mut(
                                upload_range.start as _,
                                asset.vertex_stream_byte_length(stream_index) as _,
                            ),
                        )
                        .await
                        .expect("asset.vertex_buffer.read");
                }
            })
            .collect::<futures_util::stream::FuturesUnordered<_>>()
            .collect::<()>();
        let _ = futures_util::join!(index_load_job, vertex_load_jobs);

        upload_mapped.ptr().write_at(
            buffer_init_content_base_offset as _,
            BufferInitContent {
                camera_parameters: peridot_rendering_configuration::UniformCameraParameters {
                    view_projection_matrix: camera.view_projection_matrix(
                        backbuffer_size.0 as f32 / backbuffer_size.1 as f32,
                    ),
                },
                object_parameters: peridot_rendering_configuration::UniformObjectParameters {
                    transform_matrix: peridot::math::Matrix4::ONE,
                },
            },
        );
    }
    upload_mapped
        .handled_end()
        .expect("upload_mapped.handled_end");
    drop(asset);
    e.submit_commands(|rec| {
        rec.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::TRANSFER,
            0,
            &[br::vk::VkMemoryBarrier {
                sType: br::vk::VkMemoryBarrier::TYPE,
                pNext: core::ptr::null(),
                srcAccessMask: 0,
                dstAccessMask: br::AccessFlags::TRANSFER.write,
            }],
            &[],
            &[],
        )
        .copy_buffer(
            &upload_buffer,
            &device_buffer,
            &upload_buffer_ranges
                .iter()
                .zip(device_buffer_offsets.iter())
                .map(|(br, &dbo)| {
                    br::BufferCopy(br::vk::VkBufferCopy {
                        srcOffset: br.start as _,
                        dstOffset: dbo as _,
                        size: (br.end - br.start) as _,
                    })
                })
                .chain([
                    br::BufferCopy::copy_data::<
                        peridot_rendering_configuration::UniformCameraParameters,
                    >(
                        (buffer_init_content_base_offset as usize
                            + core::mem::offset_of!(BufferInitContent, camera_parameters))
                            as _,
                        device_buffer_offsets[device_buffer_offsets.len() - 2],
                    ),
                    br::BufferCopy::copy_data::<
                        peridot_rendering_configuration::UniformObjectParameters,
                    >(
                        (buffer_init_content_base_offset as usize
                            + core::mem::offset_of!(BufferInitContent, object_parameters))
                            as _,
                        device_buffer_offsets[device_buffer_offsets.len() - 1],
                    ),
                ])
                .collect::<Vec<_>>(),
        )
        .pipeline_barrier(
            br::PipelineStageFlags::TRANSFER,
            br::PipelineStageFlags::VERTEX_INPUT | br::PipelineStageFlags::VERTEX_SHADER,
            0,
            &[br::vk::VkMemoryBarrier {
                sType: br::vk::VkMemoryBarrier::TYPE,
                pNext: core::ptr::null(),
                srcAccessMask: br::AccessFlags::TRANSFER.write,
                dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ
                    | br::AccessFlags::INDEX_READ
                    | br::AccessFlags::UNIFORM_READ,
            }],
            &[],
            &[],
        )
    })
    .expect("engine.submit_commands");

    let mut dp = br::DescriptorPoolObject::new(
        e.graphics_device().clone(),
        &br::DescriptorPoolCreateInfo::new(2, &[br::DescriptorType::UniformBuffer.make_size(2)]),
    )
    .expect("dp new");
    let [camera_descriptor_set, object_descriptor_set] = dp
        .alloc_array(&[dsl_ub1.as_transparent_ref(), dsl_ub1.as_transparent_ref()])
        .expect("dp.alloc");
    e.graphics_device().update_descriptor_sets(
        &[
            camera_descriptor_set
                .binding_at(0)
                .write(br::DescriptorContents::uniform_buffer(
                    &device_buffer,
                    device_buffer_offsets[device_buffer_offsets.len() - 2]
                        ..device_buffer_offsets[device_buffer_offsets.len() - 2]
                            + size_of::<peridot_rendering_configuration::UniformCameraParameters>()
                                as u64,
                )),
            object_descriptor_set
                .binding_at(0)
                .write(br::DescriptorContents::uniform_buffer(
                    &device_buffer,
                    device_buffer_offsets[device_buffer_offsets.len() - 1]
                        ..device_buffer_offsets[device_buffer_offsets.len() - 1]
                            + size_of::<peridot_rendering_configuration::UniformObjectParameters>()
                                as u64,
                )),
        ],
        &[],
    );

    let render_pass = br::RenderPassObject::new(
        e.graphics_device().clone(),
        &br::RenderPassCreateInfo::new(
            &[
                e.back_buffer_attachment_desc()
                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store),
                br::AttachmentDescription::new(
                    br::vk::VK_FORMAT_D24_UNORM_S8_UINT,
                    br::ImageLayout::Undefined,
                    br::ImageLayout::DepthStencilAttachmentOpt,
                )
                .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare),
            ],
            &[br::SubpassDescription::new()
                .color_attachments(
                    &[br::AttachmentReference::new(
                        0,
                        br::ImageLayout::ColorAttachmentOpt,
                    )],
                    &[],
                )
                .depth_stencil_attachment(&br::AttachmentReference::new(
                    1,
                    br::ImageLayout::DepthStencilAttachmentOpt,
                ))],
            &[br::vk::VkSubpassDependency {
                srcSubpass: 0,
                dstSubpass: br::vk::VK_SUBPASS_EXTERNAL,
                srcAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
                dstAccessMask: br::AccessFlags::MEMORY.read,
                srcStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
                dstStageMask: e.requesting_back_buffer_layout().1.0,
                dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT,
            }],
        ),
    )
    .expect("render_pass.create");

    let depth_buffer = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(backbuffer_size, br::vk::VK_FORMAT_D24_UNORM_S8_UINT)
                .with_usage(
                    br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT
                        | br::ImageUsageFlags::TRANSIENT_ATTACHMENT,
                ),
        )
        .expect("depth_buffer.create");
    let mut depth_buffer_view = br::ImageViewBuilder::new(
        depth_buffer,
        br::ImageSubresourceRange::new(br::AspectMask::DEPTH, 0..1, 0..1),
    )
    .create()
    .expect("depth_buffer_view.create");
    let mut backbuffers = e
        .iter_back_buffers()
        .map(|x| BackbufferImageView {
            image: x.native_ptr(),
            view: unsafe {
                br::vkfn_wrapper::create_image_view(
                    e.graphics_device().as_transparent_ref(),
                    &br::ImageViewCreateInfo::new(
                        &x,
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        e.back_buffer_format(),
                    ),
                    None,
                )
                .expect("backbuffer.image_view.create")
            },
            device: e.graphics_device().clone(),
        })
        .collect::<Vec<_>>();
    let mut framebuffers = backbuffers
        .iter()
        .map(|bb| {
            br::FramebufferObject::new(
                e.graphics_device().clone(),
                &br::FramebufferCreateInfo::new(
                    &render_pass,
                    &[
                        bb.as_transparent_ref(),
                        depth_buffer_view.as_transparent_ref(),
                    ],
                    backbuffer_size.0,
                    backbuffer_size.1,
                ),
            )
            .expect("framebuffer.create")
        })
        .collect::<Vec<_>>();

    let scissor_rects = [br::Extent2D::from(backbuffer_size).into_rect(br::Offset2D::ZERO)];
    let viewports = [scissor_rects[0].make_viewport(0.0..1.0)];
    let [gp] = e
        .graphics_device()
        .new_graphics_pipeline_array(
            &[br::GraphicsPipelineCreateInfo::new(
                &pl,
                render_pass.subpass(0),
                &shader_stage_with_entry_names
                    .iter()
                    .map(|&(s, ref e)| shader_module.on_stage(s, e))
                    .collect::<Vec<_>>(),
                &br::PipelineVertexInputStateCreateInfo::new(&v_bindings, &v_attributes),
                &br::PipelineInputAssemblyStateCreateInfo::new(primitive_topology),
                &br::PipelineViewportStateCreateInfo::new(&viewports, &scissor_rects),
                &br::PipelineRasterizationStateCreateInfo::new(
                    br::PolygonMode::Fill,
                    br::CullModeFlags::BACK,
                    br::FrontFace::CounterClockwise,
                ),
                &br::PipelineColorBlendStateCreateInfo::new(&[
                    br::PipelineColorBlendAttachmentState::NOBLEND,
                ]),
            )
            .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
            .set_depth_stencil_state(
                &br::PipelineDepthStencilStateCreateInfo::new()
                    .config_depth(Some(br::CompareOp::Less), true),
            )],
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("pipeline.create");
    let mut gp = gp.clone_parent();

    let mut render_cp = br::CommandPoolObject::new(
        e.graphics_device().clone(),
        &br::CommandPoolCreateInfo::new(e.graphics_queue_family_index()),
    )
    .expect("render_cp.create");
    let mut render_cb = br::CommandBufferObject::alloc(
        e.graphics_device().clone(),
        &br::CommandBufferAllocateInfo::new(
            &mut render_cp,
            framebuffers.len() as _,
            br::CommandBufferLevel::Primary,
        ),
    )
    .expect("render_cb.alloc");
    for (n, cb) in render_cb.iter_mut().enumerate() {
        unsafe {
            cb.begin(&br::CommandBufferBeginInfo::new())
                .expect("render_cb.begin")
        }
        .begin_render_pass(
            &br::RenderPassBeginInfo::new(
                &render_pass,
                &framebuffers[n],
                br::Rect2D {
                    offset: br::Offset2D::ZERO,
                    extent: backbuffer_size.into(),
                },
                &[
                    br::ClearValue::color_f32([0.0, 0.0, 0.0, 0.0]),
                    br::ClearValue::depth_stencil(1.0, 0),
                ],
            ),
            br::SubpassContents::Inline,
        )
        .bind_pipeline(br::PipelineBindPoint::Graphics, &gp)
        .bind_descriptor_sets(
            br::PipelineBindPoint::Graphics,
            &pl,
            0,
            &[camera_descriptor_set, object_descriptor_set],
            &[],
        )
        .bind_vertex_buffers(
            0,
            &[
                device_buffer.as_transparent_ref(),
                device_buffer.as_transparent_ref(),
            ],
            &[device_buffer_offsets[1], device_buffer_offsets[2]],
        )
        .bind_index_buffer(
            &device_buffer,
            device_buffer_offsets[0] as _,
            index_type_vk.expect("no index?"),
        )
        .draw_indexed(index_count, 1, 0, 0, 0)
        .end_render_pass()
        .end()
        .expect("render_cb.end");
    }

    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::Resize(new_size) => {}
            peridot::Event::NextFrame => {
                let fd = match e.prepare_frame() {
                    Ok(x) => x,
                    Err(peridot::PrepareFrameError::FramebufferOutOfDate) => {
                        todo!("framebuffer out of date");
                    }
                };

                let mut render_submissions = peridot::SubmissionBatchBuilder::new();
                render_submissions.add_command_buffers([
                    render_cb[fd.backbuffer_index as usize].as_transparent_ref()
                ]);
                e.do_render(fd.backbuffer_index, None, render_submissions)
                    .expect("engine.do_render");
            }
        }
    }

    unsafe {
        e.graphics_device().wait().expect("device.wait");
    }
}

struct BackbufferImageView {
    image: br::vk::VkImage,
    view: br::vk::VkImageView,
    device: peridot::VulkanGfx,
}
impl Drop for BackbufferImageView {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(
                self.device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.view),
                None,
            );
        }
    }
}
impl br::VkHandle for BackbufferImageView {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.view
    }
}
