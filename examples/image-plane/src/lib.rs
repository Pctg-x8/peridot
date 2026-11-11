use bedrock::{
    self as br, CommandBufferMut, DescriptorPoolMut, RenderPass, ShaderModule, VkHandle,
};
use br::Device;
use parking_lot::RwLock;
use peridot::audio::PreloadedPlayableWav;
use peridot::math::{Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector3};
use peridot::{CBSubmissionType, CommandBundle, SubpassDependencyTemplates};
use peridot_image::StdTexture2DAsset;
use peridot_math::Zero;
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_runtime_asset_manager::RuntimeAssetManager;
use std::collections::HashMap;
use std::ffi::CString;
use std::sync::Arc;

use peridot_command_object::{
    BeginRenderPass, BindGraphicsPipeline, BufferUsage, ColorAttachmentBlending, DescriptorSets,
    EndRenderPass, GraphicsCommand, GraphicsCommandCombiner, PipelineBarrier, RangedBuffer,
    StandardMesh,
};

struct LocalImageView {
    handle: br::vk::VkImageView,
    device: peridot::VulkanGfx,
}
impl Drop for LocalImageView {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(
                self.device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.handle),
                None,
            );
        }
    }
}
impl br::VkHandle for LocalImageView {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

pub async fn game_main<'q>(e: &mut peridot::Engine<'q, impl peridot::NativeLinker>) {
    let screen_size = e.back_buffer_size();
    let screen_aspect = screen_size.0 as f32 / screen_size.1 as f32;

    let (bgm,): (PreloadedPlayableWav,) =
        futures_util::try_join!(e.load_async("bgm")).expect("asset loading");

    // TODO: streamingなassetが複数ある時に相性が悪い どうしたものか
    let bgm = Arc::new(RwLock::new(bgm));
    e.audio_mixer().write().add_process(bgm.clone());
    e.audio_mixer().write().set_master_volume(0.5);

    let mut memory_manager = MemoryManager::new(e.graphics());
    let mut runtime_asset_manager = RuntimeAssetManager::new();

    let image = runtime_asset_manager
        .load::<StdTexture2DAsset>("images.example", e, &mut memory_manager)
        .expect("asset loading")
        .expect("missing image asset");

    let plane_mesh = peridot::Primitive::uv_plane_centric_xy(1.0, 0.0);
    let mut cam = Camera {
        projection: Some(ProjectionMethod::Physical {
            focal_length: 30.0,
            sensor_size: peridot::math::Vector2(35.0, 24.0),
            screen_fitting: peridot::math::PhysicalScreenFitting::Shrink,
            lens_shift: peridot::math::Vector2(0.0, 0.0),
        }),
        position: Vector3(-4.0, -1.0, -3.0),
        rotation: Quaternion::ONE,
        depth_range: 1.0..10.0,
    };
    cam.look_at(Vector3::ZERO, None);

    let mut prealloc = peridot::BufferPrealloc::new(e.graphics());
    let engine_uniform_camera_offset = prealloc.add(peridot::BufferContent::uniform::<
        peridot_shader::engine_params::CameraParameters,
    >());
    let engine_uniform_object_offset = prealloc.add(peridot::BufferContent::uniform::<
        peridot_shader::engine_params::ObjectParameters,
    >());
    let [vertex_buffer, engine_uniform_buffer] = memory_manager
        .allocate_device_local_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new(
                    plane_mesh.byte_length() as _,
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                prealloc
                    .build_desc()
                    .with_usage(br::BufferUsage::TRANSFER_DEST),
            ],
        )
        .expect("Failed to allocate buffers");
    let vertex_buffer = RangedBuffer::from(vertex_buffer);
    let engine_uniform_buffer = RangedBuffer::from(engine_uniform_buffer);
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&vertex_buffer.0, c"Vertex Buffer")
        .expect("Failed to set object name");
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&engine_uniform_buffer.0, c"Engine Uniform Buffer")
        .expect("Failed to set object name");

    let [vertex_buffer_stg, engine_uniform_buffer_stg] = memory_manager
        .allocate_upload_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new(
                    vertex_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
                prealloc.build_desc_custom_usage(br::BufferUsage::TRANSFER_SRC),
            ],
        )
        .expect("Failed to allocate upload buffer");
    let mut vertex_buffer_stg = RangedBuffer::from(vertex_buffer_stg);
    let mut engine_uniform_buffer_stg = RangedBuffer::from(engine_uniform_buffer_stg);
    vertex_buffer_stg
        .0
        .clone_content_from_slice(&plane_mesh.vertices)
        .expect("Failed to set upload content");
    engine_uniform_buffer_stg
        .0
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            p.ptr()
                .byte_add(engine_uniform_camera_offset as _)
                .cast()
                .write(peridot_shader::engine_params::CameraParameters {
                    view_projection_matrix: cam.view_projection_matrix(screen_aspect),
                });
            p.ptr()
                .byte_add(engine_uniform_object_offset as _)
                .cast()
                .write(peridot_shader::engine_params::ObjectParameters {
                    transform_matrix: Matrix4::ONE,
                });
        })
        .expect("Failed to write initial data");

    let pre_configure_awaiter = e
        .submit_commands_async(|r| {
            let [mut_uniform_in_barrier, mut_uniform_out_barrier] = engine_uniform_buffer_stg
                .make_ref()
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

            let in_barriers = PipelineBarrier::new()
                .with_barriers([
                    mut_uniform_in_barrier,
                    engine_uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                    vertex_buffer_stg
                        .make_ref()
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    vertex_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                ])
                .by_region();
            let out_barriers = PipelineBarrier::new()
                .with_barriers([
                    vertex_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_BUFFER),
                    engine_uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                    mut_uniform_out_barrier,
                ])
                .by_region();
            let init_vertex = vertex_buffer.byref_mirror_from(&vertex_buffer_stg);
            let init_engine_uniform =
                engine_uniform_buffer.byref_mirror_from(&engine_uniform_buffer_stg);
            let copies = (init_vertex, init_engine_uniform);

            copies.between(in_barriers, out_barriers).execute(r)
        })
        .expect("Failed to submit pre-configure commands");

    let mut update_cb =
        CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1).expect("Alloc UpdateCB");
    {
        // TODO: 本来は必要におうじてコピー範囲を限定したほうがいい(とくにモバイル)
        let engine_uniform_buffer_ref = engine_uniform_buffer.make_ref();
        let engine_uniform_buffer_stg_ref = engine_uniform_buffer_stg.make_ref();

        let [uniform_in_barrier, uniform_out_barrier] = engine_uniform_buffer_ref
            .clone()
            .usage_barrier3_switching(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST);
        let [staging_uniform_in_barrier, staging_uniform_out_barrier] =
            engine_uniform_buffer_stg_ref
                .clone()
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

        let in_barriers = [uniform_in_barrier, staging_uniform_in_barrier];
        let out_barriers = [uniform_out_barrier, staging_uniform_out_barrier];
        let copy_uniform =
            engine_uniform_buffer_ref.byref_mirror_from(&engine_uniform_buffer_stg_ref);

        copy_uniform
            .between(in_barriers, out_barriers)
            .execute_and_finish(
                update_cb
                    .synchronized_nth(0)
                    .begin(&br::CommandBufferBeginInfo::new())
                    .expect("Failed to begin recording update command"),
            )
            .expect("Failed to record update commands");
    }

    let back_buffer_attachment = e
        .back_buffer_attachment_desc()
        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store);
    let color_outputs = [br::AttachmentReference::new(
        0,
        br::ImageLayout::ColorAttachmentOpt,
    )];
    let color_render_subpass = br::SubpassDescription::new().color_attachments(&color_outputs, &[]);
    let renderpass = br::RenderPassObject::new(
        e.graphics().device().clone(),
        &br::RenderPassCreateInfo::new(
            &[back_buffer_attachment],
            &[color_render_subpass],
            &[SubpassDependencyTemplates::to_color_attachment_in(
                None, 0, true,
            )],
        ),
    )
    .expect("Create RenderPass");
    let mut backbuffer_resources = e
        .iter_back_buffers()
        .map(|x| LocalImageView {
            handle: unsafe {
                br::vkfn_wrapper::create_image_view(
                    e.graphics().device().as_transparent_ref(),
                    &br::ImageViewCreateInfo::new(
                        &x,
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        e.back_buffer_format(),
                    ),
                    None,
                )
                .expect("Failed to create backbuffer view")
            },
            device: e.graphics().device().clone(),
        })
        .collect::<Vec<_>>();
    let mut framebuffers = backbuffer_resources
        .iter()
        .map(|b| {
            br::FramebufferObject::new(
                e.graphics_device().clone(),
                &br::FramebufferCreateInfo::new(
                    &renderpass,
                    &[b.as_transparent_ref()],
                    screen_size.0,
                    screen_size.1,
                ),
            )
        })
        .collect::<Result<Vec<_>, _>>()
        .expect("Bind Framebuffer");

    let smp = br::SamplerObject::new(e.graphics().device().clone(), &br::SamplerCreateInfo::new())
        .expect("Creating Sampler");
    let shader = e
        .load::<peridot_shader::asset::AssetVk>("builtin.shaders.unlit_image2")
        .expect("Loading shader");
    for x in shader.material_properties.iter() {
        println!("{x:?}");
    }
    let mut material_uniform_buffer_top = 0;
    let mut material_uniform_buffer_base = 0;
    let mut material_uniform_buffer_range_by_descriptor =
        HashMap::<(u32, u32), core::range::Range<u32>>::new();
    let mut material_properties_sorted = shader
        .material_properties
        .iter()
        .filter(|p| p.r#type != peridot_shader::asset::AssetInputVariableType::Texture2D)
        .collect::<Vec<_>>();
    material_properties_sorted.sort_by(|a, b| {
        a.descriptor_set
            .cmp(&b.descriptor_set)
            .then(a.descriptor_binding.cmp(&b.descriptor_binding))
            .then(a.offset.cmp(&b.offset))
    });
    let mut current_descriptor = None;
    for p in material_properties_sorted {
        let element_base = material_uniform_buffer_base + p.offset;
        material_uniform_buffer_top = element_base + p.size;
        if current_descriptor
            .is_none_or(|(s, b)| p.descriptor_set != s || p.descriptor_binding != b)
        {
            // descriptor changed
            if let Some((s, b)) = current_descriptor {
                material_uniform_buffer_range_by_descriptor
                    .get_mut(&(s, b))
                    .expect("not registered")
                    .end = element_base;
            }
            material_uniform_buffer_range_by_descriptor.insert(
                (p.descriptor_set, p.descriptor_binding),
                (element_base..element_base).into(),
            );
            material_uniform_buffer_base = element_base;
            current_descriptor = Some((p.descriptor_set, p.descriptor_binding));
        }
    }
    if let Some((s, b)) = current_descriptor {
        material_uniform_buffer_range_by_descriptor
            .get_mut(&(s, b))
            .expect("not registered")
            .end = material_uniform_buffer_top;
    }
    println!("material_uniform_top: {material_uniform_buffer_top}");
    println!(
        "material_uniform_buffer_range_by_descriptor: {material_uniform_buffer_range_by_descriptor:?}"
    );
    let material_uniform_buffer = RangedBuffer::from(
        memory_manager
            .allocate_device_local_buffer(
                e.graphics(),
                br::BufferCreateInfo::new(
                    material_uniform_buffer_top as _,
                    br::BufferUsage::UNIFORM_BUFFER | br::BufferUsage::TRANSFER_DEST,
                ),
            )
            .expect("alloc material uniform buffer"),
    );
    let mut material_uniform_staging_buffer = RangedBuffer::from(
        memory_manager
            .allocate_upload_buffer(
                e.graphics(),
                br::BufferCreateInfo::new(
                    material_uniform_buffer_top as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
            )
            .expect("alloc material uniform staging buffer"),
    );

    struct PoolSize {
        combined_image_sampler: u32,
        uniform_buffer: u32,
    }
    let max_set_number = shader
        .input_variable_layout
        .iter()
        .map(|(k, _)| k.0)
        .max()
        .unwrap_or(0);
    let mut set_layouts = Vec::with_capacity((max_set_number + 1) as _);
    for set in 0..=max_set_number {
        let mut pool_size = PoolSize {
            combined_image_sampler: 0,
            uniform_buffer: 0,
        };

        let mut bindings = Vec::new();
        if let Some((p_set, p_binding)) = shader.object_parameters_placement
            && set == p_set
        {
            bindings.push(br::DescriptorType::UniformBuffer.make_binding(p_binding, 1));
            pool_size.uniform_buffer += 1;
        }
        if let Some((p_set, p_binding)) = shader.camera_parameters_placement
            && set == p_set
        {
            bindings.push(br::DescriptorType::UniformBuffer.make_binding(p_binding, 1));
            pool_size.uniform_buffer += 1;
        }
        bindings.extend(
            shader
                .input_variable_layout
                .iter()
                .filter(|(k, _)| k.0 == set)
                .map(|(k, v)| match v {
                    peridot_shader::asset::AssetInputVariableLayoutVk::CombinedImageSampler => {
                        pool_size.combined_image_sampler += 1;
                        br::DescriptorType::CombinedImageSampler.make_binding(k.1, 1)
                    }
                    peridot_shader::asset::AssetInputVariableLayoutVk::UniformBuffer { .. } => {
                        pool_size.uniform_buffer += 1;
                        br::DescriptorType::UniformBuffer.make_binding(k.1, 1)
                    }
                }),
        );

        set_layouts.push((
            br::DescriptorSetLayoutObject::new(
                e.graphics().device().clone(),
                &br::DescriptorSetLayoutCreateInfo::new(&bindings),
            )
            .expect("Create DescriptorSetLayout"),
            pool_size,
        ));
    }

    let pl = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &set_layouts
                .iter()
                .map(|x| x.0.as_transparent_ref())
                .collect::<Vec<_>>(),
            &[],
        ),
    )
    .expect("Create PipelineLayout");

    let sc = [br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO)];
    let vp = [sc[0].make_viewport(0.0..1.0)];

    let sm = br::ShaderModuleObject::new(
        e.graphics().device().clone(),
        &br::ShaderModuleCreateInfo::new(&shader.code_binary),
    )
    .expect("Failed to instantiate pass shader");
    let stage_with_ep_names = core::iter::once((
        br::ShaderStage::Vertex,
        CString::new(shader.vertex_entry_point_name.as_str()).expect("invalid entry point name"),
    ))
    .chain(shader.fragment_entry_point_name.as_deref().map(|n| {
        (
            br::ShaderStage::Fragment,
            CString::new(n).expect("invalid entry point name"),
        )
    }))
    .collect::<Vec<_>>();

    let [gp] = e
        .graphics()
        .device()
        .new_graphics_pipeline_array(
            &[br::GraphicsPipelineCreateInfo::new(
                &pl,
                renderpass.subpass(0),
                &stage_with_ep_names
                    .iter()
                    .map(|&(s, ref e)| sm.on_stage(s, e))
                    .collect::<Vec<_>>(),
                &br::PipelineVertexInputStateCreateInfo::new(
                    &[br::VertexInputBindingDescription::per_vertex_typed::<
                        peridot::VertexUV,
                    >(0)],
                    &[
                        br::VertexInputAttributeDescription(
                            br::vk::VkVertexInputAttributeDescription {
                                binding: 0,
                                location: shader.vertex_inputs
                                    [&peridot::VertexInputSemantic::Position(0)]
                                    .location as _,
                                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                offset: core::mem::offset_of!(peridot::VertexUV, pos) as _,
                            },
                        ),
                        br::VertexInputAttributeDescription(
                            br::vk::VkVertexInputAttributeDescription {
                                binding: 0,
                                location: shader.vertex_inputs
                                    [&peridot::VertexInputSemantic::Texcoord(0)]
                                    .location as _,
                                format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                offset: core::mem::offset_of!(peridot::VertexUV, uv) as _,
                            },
                        ),
                    ],
                ),
                &br::PipelineInputAssemblyStateCreateInfo::new(
                    br::PrimitiveTopology::TriangleStrip,
                ),
                &br::PipelineViewportStateCreateInfo::new_array(&vp, &sc),
                &br::PipelineRasterizationStateCreateInfo::new(
                    br::PolygonMode::Fill,
                    br::CullModeFlags::NONE,
                    br::FrontFace::CounterClockwise,
                ),
                &br::PipelineColorBlendStateCreateInfo::new(&[
                    ColorAttachmentBlending::Disabled.into_vk()
                ]),
            )
            .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())],
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Create GraphicsPipeline");
    let gp = gp.clone_parent();
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&gp, c"Main Pipeline")
        .expect("Failed to set pipeline name");

    let mut descriptor_pool = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            set_layouts.len() as _,
            &[
                br::DescriptorType::CombinedImageSampler.make_size(
                    set_layouts
                        .iter()
                        .map(|x| x.1.combined_image_sampler)
                        .sum::<u32>(),
                ),
                br::DescriptorType::UniformBuffer
                    .make_size(set_layouts.iter().map(|x| x.1.uniform_buffer).sum::<u32>()),
            ],
        ),
    )
    .expect("Create DescriptorPool");
    let descriptors = descriptor_pool
        .alloc(
            &set_layouts
                .iter()
                .map(|x| x.0.as_transparent_ref())
                .collect::<Vec<_>>(),
        )
        .expect("Create main Descriptors");

    pre_configure_awaiter
        .await
        .expect("Failed to pre-configure resources");

    // TODO: split material property setting
    material_uniform_staging_buffer
        .0
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            let prop = shader
                .material_properties
                .iter()
                .find(|x| x.name == "materialProperties.colorTint")
                .expect("no materialProperties.colorTint");
            let value_offset = material_uniform_buffer_range_by_descriptor
                [&(prop.descriptor_set, prop.descriptor_binding)]
                .start
                + prop.offset;

            p.ptr()
                .byte_add(value_offset as _)
                .cast()
                .write(peridot_math::Vector4(1.0f32, 1.0, 1.0, 1.0));
        })
        .expect("Failed to write material uniform initial value");
    let material_sync_awaiter = e
        .submit_commands_async(|r| {
            let [stg_in_barrier, stg_out_barrier] = material_uniform_staging_buffer
                .make_ref()
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

            let in_barriers = PipelineBarrier::new()
                .with_barriers([
                    stg_in_barrier,
                    material_uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                ])
                .by_region();
            let out_barriers = PipelineBarrier::new()
                .with_barriers([
                    material_uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                    stg_out_barrier,
                ])
                .by_region();
            let copies =
                material_uniform_buffer.byref_mirror_from(&material_uniform_staging_buffer);

            copies.between(in_barriers, out_barriers).execute(r)
        })
        .expect("Failed to submit material sync commands");

    {
        let mut descriptor_writes = Vec::with_capacity(3);
        if let Some((set, binding)) = shader.object_parameters_placement {
            descriptor_writes.push(descriptors[set as usize].binding_at(binding).write(
                br::DescriptorContents::UniformBuffer(vec![
                            engine_uniform_buffer
                                .subslice_ref(
                                    engine_uniform_object_offset
                                        ..engine_uniform_object_offset
                                            + size_of::<
                                                peridot_shader::engine_params::ObjectParameters,
                                            >()
                                                as u64,
                                )
                                .into_descriptor_buffer_ref(),
                        ]),
            ));
        }
        if let Some((set, binding)) = shader.camera_parameters_placement {
            descriptor_writes.push(descriptors[set as usize].binding_at(binding).write(
                br::DescriptorContents::UniformBuffer(vec![
                            engine_uniform_buffer
                                .subslice_ref(
                                    engine_uniform_camera_offset
                                        ..engine_uniform_camera_offset
                                            + size_of::<
                                                peridot_shader::engine_params::CameraParameters,
                                            >()
                                                as u64,
                                )
                                .into_descriptor_buffer_ref(),
                        ]),
            ));
        }
        for (&(set, binding), range) in material_uniform_buffer_range_by_descriptor.iter() {
            descriptor_writes.push(descriptors[set as usize].binding_at(binding).write(
                br::DescriptorContents::UniformBuffer(vec![
                        material_uniform_buffer
                            .subslice_ref(range.start as _..range.end as _)
                            .into_descriptor_buffer_ref(),
                    ]),
            ))
        }

        // TODO: split material property setting
        let p = shader
            .material_properties
            .iter()
            .find(|x| x.name == "materialProperties.tex")
            .expect("no materialProperties.tex");
        descriptor_writes.push(
            descriptors[p.descriptor_set as usize]
                .binding_at(p.descriptor_binding)
                .write(br::DescriptorContents::CombinedImageSampler(vec![
                    br::DescriptorImageInfo::new(
                        &runtime_asset_manager
                            .get_typed(&image)
                            .expect("invalid ref")
                            .gfx_resource,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )
                    .with_sampler(&smp),
                ])),
        );

        e.graphics()
            .device()
            .update_descriptor_sets(&descriptor_writes, &[]);
    }

    let plane_mesh = StandardMesh {
        vertex_buffers: vec![vertex_buffer],
        vertex_count: 4,
    };

    let descriptor_sets = DescriptorSets(descriptors.clone());
    let render_image_plane = plane_mesh
        .draw(1)
        .after_of(descriptor_sets.into_bind_graphics(&pl));
    let color_renders = BindGraphicsPipeline(&gp).then(render_image_plane);

    let mut render_cb = CommandBundle::new(
        e.graphics(),
        CBSubmissionType::Graphics,
        e.back_buffer_count(),
    )
    .expect("Alloc RenderCB");
    #[allow(unused_variables)]
    for (n, (mut cb, fb)) in render_cb.iter_mut().zip(&framebuffers).enumerate() {
        #[cfg(feature = "debug")]
        e.graphics()
            .device()
            .set_object_name(
                &cb,
                &std::ffi::CString::new(format!("Primary Render Commands #{n}"))
                    .expect("invalid sequence?"),
            )
            .expect("Failed to set render cb name");

        let begin_main_rp = BeginRenderPass::new(
            &renderpass,
            fb,
            br::Extent2D::from(screen_size).into_rect(br::vk::VkOffset2D::ZERO),
            br::SubpassContents::Inline,
        )
        .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

        (&color_renders)
            .between(begin_main_rp, EndRenderPass)
            .execute_and_finish(unsafe {
                cb.begin(&br::CommandBufferBeginInfo::new())
                    .expect("Failed to begin command recording")
            })
            .expect("Failed to record render commands");
    }

    material_sync_awaiter
        .await
        .expect("Failed to sync material params");
    bgm.write().play();
    e.start_frame_drain();

    let mut rot = 0.0f32;
    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let fd = match e.prepare_frame() {
                    Ok(fd) => fd,
                    Err(peridot::PrepareFrameError::FramebufferOutOfDate) => {
                        // resize and do nothing
                        let new_size = e.back_buffer_size();

                        e.wait_for_last_rendering_completion()
                            .expect("Failed to wait last render completion");

                        unsafe { render_cb.reset().expect("Resetting RenderCB") };
                        drop(framebuffers);
                        drop(backbuffer_resources);

                        e.resize_presenter_backbuffers(new_size);

                        backbuffer_resources = e
                            .iter_back_buffers()
                            .map(|x| LocalImageView {
                                handle: unsafe {
                                    br::vkfn_wrapper::create_image_view(
                                        e.graphics().device().as_transparent_ref(),
                                        &br::ImageViewCreateInfo::new(
                                            &x,
                                            br::ImageSubresourceRange::new(
                                                br::AspectMask::COLOR,
                                                0..1,
                                                0..1,
                                            ),
                                            br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                            e.back_buffer_format(),
                                        ),
                                        None,
                                    )
                                    .expect("Failed to create backbuffer view")
                                },
                                device: e.graphics().device().clone(),
                            })
                            .collect();
                        framebuffers = backbuffer_resources
                            .iter()
                            .map(|b| {
                                br::FramebufferObject::new(
                                    e.graphics_device().clone(),
                                    &br::FramebufferCreateInfo::new(
                                        &renderpass,
                                        &[b.as_transparent_ref()],
                                        new_size.0,
                                        new_size.1,
                                    ),
                                )
                            })
                            .collect::<Result<Vec<_>, _>>()
                            .expect("Bind Framebuffers");

                        for (mut cb, fb) in render_cb.iter_mut().zip(&framebuffers) {
                            let begin_main_rp = BeginRenderPass::new(
                                &renderpass,
                                fb,
                                br::vk::VkExtent2D::from(new_size)
                                    .into_rect(br::vk::VkOffset2D::ZERO),
                                br::SubpassContents::Inline,
                            )
                            .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

                            (&color_renders)
                                .between(begin_main_rp, EndRenderPass)
                                .execute_and_finish(unsafe {
                                    cb.begin(&br::CommandBufferBeginInfo::new())
                                        .expect("Failed to begin command recording")
                                })
                                .expect("Failed to record render commands");
                        }

                        continue;
                    }
                };

                let dtsec = fd.delta_time.as_secs() as f32
                    + fd.delta_time.subsec_micros() as f32 / 1000_0000.0;
                rot += dtsec * 15.0;
                let rot = rot;
                engine_uniform_buffer_stg
                    .0
                    .guard_map(BufferMapMode::Write, |ptr| unsafe {
                        ptr.get_mut_at::<UniformObjectParameters>(
                            engine_uniform_object_offset as _,
                        )
                        .object = Quaternion::new(rot, Vector3::up()).into();
                    })
                    .expect("Update DynamicStgBuffer");

                let update_cb = update_cb.nth_ref(0);
                let render_cb = render_cb.nth_ref(fd.backbuffer_index as _);
                let mut update_batch = peridot::SubmissionBatchBuilder::new();
                update_batch.add_command_buffers([update_cb.as_transparent_ref()]);
                let mut render_batch = peridot::SubmissionBatchBuilder::new();
                render_batch.add_command_buffers([render_cb.as_transparent_ref()]);
                e.do_render(fd.backbuffer_index, Some(update_batch), render_batch)
                    .expect("Failed to present");
            }
            peridot::Event::Resize(new_size) => {
                e.wait_for_last_rendering_completion()
                    .expect("Failed to wait last render completion");

                unsafe { render_cb.reset().expect("Resetting RenderCB") };
                drop(framebuffers);
                drop(backbuffer_resources);

                e.resize_presenter_backbuffers(new_size);

                backbuffer_resources = e
                    .iter_back_buffers()
                    .map(|x| LocalImageView {
                        handle: unsafe {
                            br::vkfn_wrapper::create_image_view(
                                e.graphics().device().as_transparent_ref(),
                                &br::ImageViewCreateInfo::new(
                                    &x,
                                    br::ImageSubresourceRange::new(
                                        br::AspectMask::COLOR,
                                        0..1,
                                        0..1,
                                    ),
                                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                                    e.back_buffer_format(),
                                ),
                                None,
                            )
                            .expect("Failed to create backbuffer view")
                        },
                        device: e.graphics().device().clone(),
                    })
                    .collect();
                framebuffers = backbuffer_resources
                    .iter()
                    .map(|b| {
                        br::FramebufferObject::new(
                            e.graphics_device().clone(),
                            &br::FramebufferCreateInfo::new(
                                &renderpass,
                                &[b.as_transparent_ref()],
                                new_size.0,
                                new_size.1,
                            ),
                        )
                    })
                    .collect::<Result<Vec<_>, _>>()
                    .expect("Bind Framebuffers");

                for (mut cb, fb) in render_cb.iter_mut().zip(&framebuffers) {
                    let begin_main_rp = BeginRenderPass::new(
                        &renderpass,
                        fb,
                        br::vk::VkExtent2D::from(new_size).into_rect(br::vk::VkOffset2D::ZERO),
                        br::SubpassContents::Inline,
                    )
                    .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

                    (&color_renders)
                        .between(begin_main_rp, EndRenderPass)
                        .execute_and_finish(unsafe {
                            cb.begin(&br::CommandBufferBeginInfo::new())
                                .expect("Failed to begin command recording")
                        })
                        .expect("Failed to record render commands");
                }
            }
        }
    }

    unsafe {
        e.graphics_device().wait().expect("Failed to wait for work");
    }
}

#[repr(C)]
struct UniformCameraParameters {
    pub camera: Matrix4F32,
}

#[repr(C)]
struct UniformObjectParameters {
    pub object: Matrix4F32,
}
