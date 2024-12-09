use bedrock::{self as br, CommandBufferMut, DescriptorPoolMut, RenderPass, VkHandle};
use br::{resources::Image, SubmissionBatch};
use br::{Device, GraphicsPipelineBuilder, ImageChild, ImageSubresourceSlice};
use log::*;
use peridot::math::{Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector3};
use peridot::{
    audio::StreamingPlayableWav, CBSubmissionType, CommandBundle, SubpassDependencyTemplates,
};
use peridot_math::Zero;
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_vertex_processing_pack::PvpShaderModules;
use std::sync::{Arc, RwLock};

#[cfg(feature = "debug")]
use br::VkObject;

use peridot_command_object::{
    BeginRenderPass, BindGraphicsPipeline, BufferImageDataDesc, BufferUsage,
    ColorAttachmentBlending, CopyBufferToImage, DescriptorSets, EndRenderPass, GraphicsCommand,
    GraphicsCommandCombiner, ImageResourceRange, PipelineBarrier, RangedBuffer, RangedImage,
    StandardMesh,
};

pub async fn game_main(e: &mut peridot::Engine<impl peridot::NativeLinker>) {
    let screen_size = e
        .back_buffer(0)
        .expect("no back buffers?")
        .image()
        .size()
        .clone();
    let screen_aspect = screen_size.width as f32 / screen_size.height as f32;

    let image_data: peridot_image::PNG = e.load("images.example").expect("No image found");
    debug!("image: {}x{}", image_data.0.size.x(), image_data.0.size.y());
    debug!("ImageFormat: {:?}", image_data.0.format);
    debug!("ImageStride: {} bytes", image_data.0.stride);

    let bgm = Arc::new(RwLock::new(
        e.streaming::<StreamingPlayableWav>("bgm")
            .expect("Loading BGM"),
    ));
    e.audio_mixer()
        .write()
        .expect("Adding AudioProcess")
        .add_process(bgm.clone());
    e.audio_mixer()
        .write()
        .expect("Setting MasterVolume")
        .set_master_volume(0.5);

    let mut memory_manager = MemoryManager::new(e.graphics());

    let plane_mesh = peridot::Primitive::uv_plane_centric_xy(1.0, 0.0);
    let mut cam = Camera {
        projection: Some(ProjectionMethod::Perspective {
            fov: 75.0f32.to_radians(),
        }),
        position: Vector3(-4.0, -1.0, -3.0),
        rotation: Quaternion::ONE,
        depth_range: 1.0..10.0,
    };
    cam.look_at(Vector3::ZERO);

    let [vertex_buffer, uniform_buffer] = memory_manager
        .allocate_device_local_buffer_array(
            e.graphics(),
            [
                br::BufferDesc::new(
                    plane_mesh.byte_length(),
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                br::BufferDesc::new_for_type::<Uniform>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
            ],
        )
        .expect("Failed to allocate buffers");
    let vertex_buffer = RangedBuffer::from(vertex_buffer);
    let uniform_buffer = RangedBuffer::from(uniform_buffer);
    #[cfg(feature = "debug")]
    vertex_buffer
        .0
        .set_name(Some(c"Vertex Buffer"))
        .expect("Failed to set object name");
    #[cfg(feature = "debug")]
    uniform_buffer
        .0
        .set_name(Some(c"Uniform Buffer"))
        .expect("Faield to set object name");

    let [vertex_buffer_stg, uniform_mut_buffer] = memory_manager
        .allocate_upload_buffer_array(
            e.graphics(),
            [
                br::BufferDesc::new(
                    vertex_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
                br::BufferDesc::new(
                    uniform_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
            ],
        )
        .expect("Failed to allocate upload buffer");
    let mut vertex_buffer_stg = RangedBuffer::from(vertex_buffer_stg);
    let mut uniform_mut_buffer = RangedBuffer::from(uniform_mut_buffer);
    vertex_buffer_stg
        .0
        .clone_content_from_slice(&plane_mesh.vertices)
        .expect("Failed to set upload content");
    uniform_mut_buffer
        .0
        .write_content(Uniform {
            camera: cam.view_projection_matrix(screen_aspect),
            object: Matrix4::ONE,
        })
        .expect("Failed to set initial data of uniform buffer");

    let image = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageDesc::new(image_data.0.size, image_data.0.format as _)
                .sampled()
                .transfer_dest()
                .init_layout(br::ImageLayout::Preinitialized),
        )
        .expect("Failed to allocate main image");
    let mut image_data_stg_buffer = memory_manager
        .allocate_upload_linear_image_buffer(
            e.graphics(),
            *image_data.0.size.x(),
            *image_data.0.size.y(),
            image_data.0.format,
            br::BufferUsage::TRANSFER_SRC,
        )
        .expect("Failed to allocate linear image buffer");
    image_data_stg_buffer
        .copy_content_from_slice(image_data.0.u8_pixels())
        .expect("Failed to set image data");

    let pre_configure_awaiter = e
        .submit_commands_async(|mut r| {
            let texture = RangedImage::single_color_plane(&image);
            let image_data_stg_buffer_ranged = RangedBuffer::from(&image_data_stg_buffer.inner);

            let [mut_uniform_in_barrier, mut_uniform_out_barrier] = uniform_mut_buffer
                .make_ref()
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);
            let [tex_init_barrier, tex_ready_barrier] = texture.barrier3(
                br::ImageLayout::Preinitialized,
                br::ImageLayout::TransferDestOpt,
                br::ImageLayout::ShaderReadOnlyOpt,
            );

            let in_barriers = PipelineBarrier::new()
                .with_barriers([
                    mut_uniform_in_barrier,
                    uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                    vertex_buffer_stg
                        .make_ref()
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    vertex_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                    image_data_stg_buffer_ranged
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                ])
                .with_barrier(tex_init_barrier)
                .by_region();
            let out_barriers = PipelineBarrier::new()
                .with_barriers([
                    vertex_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_BUFFER),
                    mut_uniform_out_barrier,
                    uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                ])
                .with_barrier(tex_ready_barrier)
                .by_region();
            let init_vertex = vertex_buffer.byref_mirror_from(&vertex_buffer_stg);
            let init_uniform = uniform_buffer.byref_mirror_from(&uniform_mut_buffer);
            let init_tex = CopyBufferToImage::new(&image_data_stg_buffer.inner, &image).with_range(
                BufferImageDataDesc::new(0, image_data_stg_buffer.row_texels),
                ImageResourceRange::for_single_color_from_rect2d(
                    image.size().wh().into_rect(br::vk::VkOffset2D::ZERO),
                ),
            );
            let copies = (init_vertex, init_uniform, init_tex);

            let _ = copies
                .between(in_barriers, out_barriers)
                .execute(r.as_dyn_ref());
            r
        })
        .expect("Failed to submit pre-configure commands");

    let mut update_cb =
        CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1).expect("Alloc UpdateCB");
    {
        let uniform_buffer_ref = uniform_buffer.make_ref();
        let uniform_mut_buffer_ref = uniform_mut_buffer.make_ref();

        let [uniform_in_barrier, uniform_out_barrier] = uniform_buffer_ref
            .usage_barrier3_switching(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST);
        let [staging_uniform_in_barrier, staging_uniform_out_barrier] = uniform_mut_buffer_ref
            .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

        let in_barriers = [uniform_in_barrier, staging_uniform_in_barrier];
        let out_barriers = [uniform_out_barrier, staging_uniform_out_barrier];
        let copy_uniform = uniform_buffer.byref_mirror_from(&uniform_mut_buffer);

        copy_uniform
            .between(in_barriers, out_barriers)
            .execute_and_finish(
                update_cb
                    .synchronized_nth(0)
                    .begin()
                    .expect("Failed to begin recording update command")
                    .as_dyn_ref(),
            )
            .expect("Failed to record update commands");
    }

    let back_buffer_attachment = e
        .back_buffer_attachment_desc()
        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store);
    let color_outputs = [br::vk::VkAttachmentReference::new(
        0,
        br::ImageLayout::ColorAttachmentOpt,
    )];
    let color_render_subpass = br::SubpassDescription::new().color_attachments(&color_outputs, &[]);
    let renderpass = br::RenderPassObject::new(
        e.graphics().device().clone(),
        &br::RenderPassBuilder::new(
            &[back_buffer_attachment],
            &[color_render_subpass],
            &[SubpassDependencyTemplates::to_color_attachment_in(
                None, 0, true,
            )],
        ),
    )
    .expect("Create RenderPass");
    let mut backbuffer_resources = e.iter_back_buffers().cloned().collect::<Vec<_>>();
    let mut framebuffers = backbuffer_resources
        .iter()
        .map(|b| br::FramebufferBuilder::new_with_attachment(&renderpass, b).create())
        .collect::<Result<Vec<_>, _>>()
        .expect("Bind Framebuffer");

    let smp = br::SamplerObject::new(e.graphics().device().clone(), &br::SamplerCreateInfo::new())
        .expect("Creating Sampler");
    let descriptor_layout = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[
            br::DescriptorType::UniformBuffer
                .make_binding(0, 1)
                .only_for_vertex(),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1, 1)
                .only_for_fragment()
                .with_immutable_samplers(&[smp.as_transparent_ref()]),
        ]),
    )
    .expect("Create DescriptorSetLayout");
    let mut descriptor_pool = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            1,
            &[
                br::DescriptorType::UniformBuffer.make_size(1),
                br::DescriptorType::CombinedImageSampler.make_size(1),
            ],
        ),
    )
    .expect("Create DescriptorPool");

    let pl = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(&[descriptor_layout.as_transparent_ref()], &[]),
    )
    .expect("Create PipelineLayout");
    let gp = {
        let shader = e
            .load("builtin.shaders.unlit_image")
            .expect("Loading shader");
        let shader_modules =
            PvpShaderModules::new(e.graphics().device(), &shader).expect("Create ShaderModules");
        let shader_stages = [
            shader_modules.pipeline_vertex_shader_stage(),
            shader_modules
                .pipeline_fragment_shader_stage()
                .expect("no fsh?"),
        ];
        let sc = [screen_size.wh().into_rect(br::vk::VkOffset2D::ZERO)];
        let vp = [sc[0].make_viewport(0.0..1.0)];
        let vps = br::VertexProcessingStages::new(
            &shader_stages,
            &shader.vertex_bindings,
            &shader.vertex_attributes,
            br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
        );
        let mut gpb = br::NonDerivedGraphicsPipelineBuilder::new(&pl, renderpass.subpass(0), vps);
        let color_blends = [ColorAttachmentBlending::Disabled.into_vk()];
        gpb.viewport_state(br::ViewportState::new(&vp, &sc))
            .multisample_state(br::MultisampleState::new().into())
            .color_blend_state(br::ColorBlendState::new(None, &color_blends, [0.0; 4]));

        gpb.create(
            e.graphics().device().clone(),
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Create GraphicsPipeline")
    };
    #[cfg(feature = "debug")]
    gp.set_name(Some(c"Main Pipeline"))
        .expect("Failed to set pipeline name");

    pre_configure_awaiter
        .await
        .expect("Failed to pre-configure resources");

    let image_view = image
        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
        .view_builder()
        .create()
        .expect("Failed to create main image view");
    let [descriptor_main] = descriptor_pool
        .alloc_array(&[descriptor_layout.as_transparent_ref()])
        .expect("Create main Descriptor");
    {
        let mut descriptor_writes = Vec::with_capacity(2);
        descriptor_writes.extend(
            br::DescriptorPointer::new(descriptor_main.into(), 0).write_continuous_bindings([
                br::DescriptorContents::UniformBuffer(vec![
                    uniform_buffer.make_descriptor_buffer_ref()
                ]),
                br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageInfo::new(
                    &image_view,
                    br::ImageLayout::ShaderReadOnlyOpt,
                )]),
            ]),
        );
        e.graphics()
            .device()
            .update_descriptor_sets(&descriptor_writes, &[]);
    }

    let plane_mesh = StandardMesh {
        vertex_buffers: vec![vertex_buffer],
        vertex_count: 4,
    };

    let descriptor_sets = DescriptorSets(vec![descriptor_main]);
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
    for (n, (cb, fb)) in render_cb.iter_mut().zip(&framebuffers).enumerate() {
        #[cfg(feature = "debug")]
        br::DebugUtilsObjectNameInfo::new(
            cb,
            Some(
                &std::ffi::CString::new(format!("Primary Render Commands #{}", n))
                    .expect("invalid sequence?"),
            ),
        )
        .apply(e.graphics().device())
        .expect("Failed to set render cb name");

        let begin_main_rp = BeginRenderPass::new(
            &renderpass,
            fb,
            screen_size.wh().into_rect(br::vk::VkOffset2D::ZERO),
        )
        .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

        (&color_renders)
            .between(begin_main_rp, EndRenderPass)
            .execute_and_finish(unsafe {
                cb.begin(e.graphics_device())
                    .expect("Failed to begin command recording")
                    .as_dyn_ref()
            })
            .expect("Failed to record render commands");
    }

    bgm.write().expect("Starting BGM").play();

    let mut rot = 0.0f32;
    while let Some(ev) = e.event_receivers().wait_for_event().await {
        match ev {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let fd = match e.prepare_frame() {
                    Ok(fd) => fd,
                    Err(peridot::PrepareFrameError::FramebufferOutOfDate) => {
                        // resize and do nothing
                        let new_size = e
                            .back_buffer(0)
                            .expect("no back buffers?")
                            .image()
                            .size()
                            .clone();
                        let new_size = peridot::math::Vector2(new_size.width, new_size.height);

                        e.wait_for_last_rendering_completion();

                        unsafe { render_cb.reset().expect("Resetting RenderCB") };
                        drop(framebuffers);
                        drop(backbuffer_resources);

                        e.resize_presenter_backbuffers(new_size);

                        backbuffer_resources = e.iter_back_buffers().cloned().collect();
                        framebuffers = backbuffer_resources
                            .iter()
                            .map(|b| {
                                br::FramebufferBuilder::new_with_attachment(&renderpass, b).create()
                            })
                            .collect::<Result<Vec<_>, _>>()
                            .expect("Bind Framebuffers");

                        for (cb, fb) in render_cb.iter_mut().zip(&framebuffers) {
                            let begin_main_rp = BeginRenderPass::new(
                                &renderpass,
                                fb,
                                br::vk::VkExtent2D::from(new_size)
                                    .into_rect(br::vk::VkOffset2D::ZERO),
                            )
                            .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

                            (&color_renders)
                                .between(begin_main_rp, EndRenderPass)
                                .execute_and_finish(unsafe {
                                    cb.begin(e.graphics_device())
                                        .expect("Failed to begin command recording")
                                        .as_dyn_ref()
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
                uniform_mut_buffer
                    .0
                    .guard_map(BufferMapMode::Write, |ptr| unsafe {
                        ptr.get_mut_at::<Uniform>(0).object =
                            Quaternion::new(rot, Vector3::up()).into();
                    })
                    .expect("Update DynamicStgBuffer");

                e.do_render(
                    fd.backbuffer_index,
                    Some(br::EmptySubmissionBatch.with_command_buffers(&update_cb)),
                    br::EmptySubmissionBatch.with_command_buffers(
                        &render_cb[fd.backbuffer_index as usize..=fd.backbuffer_index as usize],
                    ),
                )
                .expect("Failed to present");
            }
            peridot::Event::Resize(new_size) => {
                e.wait_for_last_rendering_completion();

                unsafe { render_cb.reset().expect("Resetting RenderCB") };
                drop(framebuffers);
                drop(backbuffer_resources);

                e.resize_presenter_backbuffers(new_size);

                backbuffer_resources = e.iter_back_buffers().cloned().collect();
                framebuffers = backbuffer_resources
                    .iter()
                    .map(|b| br::FramebufferBuilder::new_with_attachment(&renderpass, b).create())
                    .collect::<Result<Vec<_>, _>>()
                    .expect("Bind Framebuffers");

                for (cb, fb) in render_cb.iter_mut().zip(&framebuffers) {
                    let begin_main_rp = BeginRenderPass::new(
                        &renderpass,
                        fb,
                        br::vk::VkExtent2D::from(new_size).into_rect(br::vk::VkOffset2D::ZERO),
                    )
                    .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

                    (&color_renders)
                        .between(begin_main_rp, EndRenderPass)
                        .execute_and_finish(unsafe {
                            cb.begin(e.graphics_device())
                                .expect("Failed to begin command recording")
                                .as_dyn_ref()
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
struct Uniform {
    camera: Matrix4F32,
    object: Matrix4F32,
}
