use bedrock::{self as br, CommandBufferMut, DescriptorPoolMut, Fence, RenderPass, VkHandle};
use br::resources::Image;
use br::Device;
use log::*;
use parking_lot::RwLock;
use peridot::math::{Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector3};
use peridot::{
    audio::StreamingPlayableWav, CBSubmissionType, CommandBundle, SubpassDependencyTemplates,
};
use peridot_math::Zero;
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_semantic_shader::ShaderPackAsset;
use std::sync::Arc;

use peridot_command_object::{
    BufferImageDataDesc, BufferUsage, ColorAttachmentBlending, CopyBufferToImage, GraphicsCommand,
    GraphicsCommandCombiner, ImageResourceRange, PipelineBarrier, RangedBuffer, RangedImage,
};

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

pub async fn game_main<'q>(e: &mut peridot::Engine<'q, impl peridot::NativeLinker>) {
    let screen_size = e.back_buffer_size();
    let screen_aspect = screen_size.0 as f32 / screen_size.1 as f32;

    let image_data: peridot_image::PNG = e.load("images.example").expect("No image found");
    debug!("image: {}x{}", image_data.0.size.x(), image_data.0.size.y());
    debug!("ImageFormat: {:?}", image_data.0.format);
    debug!("ImageStride: {} bytes", image_data.0.stride);

    let bgm = Arc::new(RwLock::new(
        e.streaming::<StreamingPlayableWav>("bgm")
            .expect("Loading BGM"),
    ));
    e.audio_mixer().write().add_process(bgm.clone());
    e.audio_mixer().write().set_master_volume(0.5);

    let mut memory_manager = MemoryManager::new(e.graphics());

    let mut plane_mesh =
        peridot_std_mesh::Mesh::uv_plane_centric_xy(e.graphics(), &mut memory_manager, 1.0, 0.0);
    let mut cam = Camera {
        projection: Some(ProjectionMethod::Perspective {
            fov: 75.0f32.to_radians(),
        }),
        position: Vector3(-4.0, -1.0, -3.0),
        rotation: Quaternion::ONE,
        depth_range: 1.0..10.0,
    };
    cam.look_at(Vector3::ZERO);

    let [cam_uniform_buffer, obj_uniform_buffer] = memory_manager
        .allocate_device_local_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new_for_type::<UniformCameraParameters>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
                br::BufferCreateInfo::new_for_type::<UniformObjectParameters>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
            ],
        )
        .expect("Failed to allocate buffers");
    let cam_uniform_buffer = RangedBuffer::from(cam_uniform_buffer);
    let obj_uniform_buffer = RangedBuffer::from(obj_uniform_buffer);
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&cam_uniform_buffer.0, c"Uniform Buffer[CameraParameters]")
        .expect("Failed to set object name");
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&obj_uniform_buffer.0, c"Uniform Buffer")
        .expect("Faield to set object name");

    let [cam_uniform_buffer_stg, obj_uniform_mut_buffer] = memory_manager
        .allocate_upload_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new(
                    cam_uniform_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
                br::BufferCreateInfo::new(
                    obj_uniform_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
            ],
        )
        .expect("Failed to allocate upload buffer");
    let mut cam_uniform_buffer_stg = RangedBuffer::from(cam_uniform_buffer_stg);
    let mut obj_uniform_mut_buffer = RangedBuffer::from(obj_uniform_mut_buffer);
    cam_uniform_buffer_stg
        .0
        .write_content(UniformCameraParameters {
            camera: cam.view_projection_matrix(screen_aspect),
        })
        .expect("Failed to set initial data of camera uniform buffer");
    obj_uniform_mut_buffer
        .0
        .write_content(UniformObjectParameters {
            object: Matrix4::ONE,
        })
        .expect("Failed to set initial data of object uniform buffer");

    let image = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(image_data.0.size, image_data.0.format as _)
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
        .submit_commands_async(|r| {
            let texture = RangedImage::single_color_plane(&image);
            let image_data_stg_buffer_ranged = RangedBuffer::from(&image_data_stg_buffer.inner);

            let [mut_uniform_in_barrier, mut_uniform_out_barrier] = obj_uniform_mut_buffer
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
                    obj_uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                    cam_uniform_buffer_stg
                        .make_ref()
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    image_data_stg_buffer_ranged
                        .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                ])
                .with_barrier(tex_init_barrier)
                .by_region();
            let out_barriers = PipelineBarrier::new()
                .with_barriers([
                    cam_uniform_buffer_stg
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                    mut_uniform_out_barrier,
                    obj_uniform_buffer
                        .make_ref()
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                ])
                .with_barrier(tex_ready_barrier)
                .by_region();
            let init_cam_uniform = cam_uniform_buffer.byref_mirror_from(&cam_uniform_buffer_stg);
            let init_obj_uniform = obj_uniform_buffer.byref_mirror_from(&obj_uniform_mut_buffer);
            let init_tex = CopyBufferToImage::new(&image_data_stg_buffer.inner, &image).with_range(
                BufferImageDataDesc::new(0, image_data_stg_buffer.row_texels),
                ImageResourceRange::for_single_color_from_rect2d(
                    image.size().wh().into_rect(br::vk::VkOffset2D::ZERO),
                ),
            );
            let copies = (init_cam_uniform, init_obj_uniform, init_tex);

            copies.between(in_barriers, out_barriers).execute(r)
        })
        .expect("Failed to submit pre-configure commands");

    let mut update_cb =
        CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1).expect("Alloc UpdateCB");
    {
        let uniform_buffer_ref = obj_uniform_buffer.make_ref();
        let uniform_mut_buffer_ref = obj_uniform_mut_buffer.make_ref();

        let [uniform_in_barrier, uniform_out_barrier] = uniform_buffer_ref
            .usage_barrier3_switching(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST);
        let [staging_uniform_in_barrier, staging_uniform_out_barrier] = uniform_mut_buffer_ref
            .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

        let in_barriers = [uniform_in_barrier, staging_uniform_in_barrier];
        let out_barriers = [uniform_out_barrier, staging_uniform_out_barrier];
        let copy_uniform = obj_uniform_buffer.byref_mirror_from(&obj_uniform_mut_buffer);

        copy_uniform
            .between(in_barriers, out_barriers)
            .execute_and_finish(
                update_cb
                    .synchronized_nth(0)
                    .begin(&br::CommandBufferBeginInfo::new(), e.graphics().device())
                    .expect("Failed to begin recording update command"),
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
                    e.graphics().device().native_ptr(),
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
    let dsl_ub1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::UniformBuffer
            .make_binding(0, 1)
            .only_for_vertex()]),
    )
    .expect("Create DescriptorSetLayout with UniformBuffer(x1)");
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
            2,
            &[
                br::DescriptorType::UniformBuffer.make_size(2),
                br::DescriptorType::CombinedImageSampler.make_size(1),
            ],
        ),
    )
    .expect("Create DescriptorPool");

    let pl = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &[
                dsl_ub1.as_transparent_ref(),
                descriptor_layout.as_transparent_ref(),
            ],
            &[],
        ),
    )
    .expect("Create PipelineLayout");
    let shader = e
        .load::<ShaderPackAsset>("builtin.semantic_shaders.unlit_image")
        .expect("Loading shader")
        .instantiate(e.graphics().device().clone())
        .expect("Instantiate Shaders");
    let sc = [br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO)];
    let vp = [sc[0].make_viewport(0.0..1.0)];
    let [gp] = e
        .graphics()
        .device()
        .new_graphics_pipeline_array(
            &[br::GraphicsPipelineCreateInfo::new(
                &pl,
                renderpass.subpass(0),
                &[
                    shader.pipeline_vertex_shader(),
                    shader.pipeline_fragment_shader().expect("no fsh?"),
                ],
                &br::PipelineVertexInputStateCreateInfo::new(
                    plane_mesh.vk_vertex_input_bindings(),
                    &plane_mesh.vk_vertex_input_attributes(&shader),
                ),
                &br::PipelineInputAssemblyStateCreateInfo::new(plane_mesh.vk_primitive_topology()),
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
            .multisample_state(&br::PipelineMultisampleStateCreateInfo::new())],
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Create GraphicsPipeline");
    let gp = gp.clone_parent();
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&gp, c"Main Pipeline")
        .expect("Failed to set pipeline name");

    pre_configure_awaiter
        .await
        .expect("Failed to pre-configure resources");

    let image_view = br::ImageViewBuilder::new(
        image,
        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
    )
    .create()
    .expect("Failed to create main image view");
    let [descriptor_cam, descriptor_main] = descriptor_pool
        .alloc_array(&[
            dsl_ub1.as_transparent_ref(),
            descriptor_layout.as_transparent_ref(),
        ])
        .expect("Create main Descriptor");
    {
        let mut descriptor_writes = Vec::with_capacity(3);
        descriptor_writes.push(descriptor_cam.binding_at(0).write(
            br::DescriptorContents::UniformBuffer(vec![
                cam_uniform_buffer.make_descriptor_buffer_ref(),
            ]),
        ));
        descriptor_writes.extend(
            br::DescriptorPointer::new(descriptor_main.into(), 0).write_continuous_bindings([
                br::DescriptorContents::UniformBuffer(vec![
                    obj_uniform_buffer.make_descriptor_buffer_ref()
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

    struct BufferedFrameRenderingState {
        cb: CommandBundle<peridot::VulkanGfx>,
        completion: br::FenceObject<peridot::VulkanGfx>,
        rendering: bool,
    }
    let mut frame_render_states = (0..e.back_buffer_count())
        .map(|_| {
            let cb = CommandBundle::new(e.graphics(), CBSubmissionType::Graphics, 1)
                .expect("Alloc RenderCB");
            #[cfg(feature = "debug")]
            e.graphics()
                .device()
                .set_object_name(
                    &cb.nth_ref(0),
                    &std::ffi::CString::new(format!("Primary Render Commands #{n}"))
                        .expect("invalid sequence?"),
                )
                .expect("Failed to set render cb name");

            BufferedFrameRenderingState {
                cb,
                completion: br::FenceObject::new(
                    e.graphics().device().clone(),
                    &br::FenceCreateInfo::new(0),
                )
                .expect("Completion Fence creation"),
                rendering: false,
            }
        })
        .collect::<Vec<_>>();

    bgm.write().play();

    let mut frame_sec_samples = [0.0; 640];
    let mut frame_sec_sample_pos = 0;
    let mut frame_sec_collect_timer = std::time::Instant::now();
    let mut rot = 0.0f32;
    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::NextFrame => {
                let t0 = std::time::Instant::now();
                let fd = match e.prepare_frame() {
                    Ok(fd) => fd,
                    Err(peridot::PrepareFrameError::FramebufferOutOfDate) => {
                        // resize and do nothing
                        let new_size = e.back_buffer_size();

                        for x in frame_render_states.iter_mut() {
                            if x.rendering {
                                x.completion
                                    .wait()
                                    .expect("Failed to wait previous rendering work");
                                unsafe {
                                    x.cb.reset()
                                        .expect("Failed to reset previous rendering commands");
                                }
                                x.rendering = false;
                            }
                        }
                        drop(framebuffers);
                        drop(backbuffer_resources);

                        e.resize_presenter_backbuffers(new_size);

                        backbuffer_resources = e
                            .iter_back_buffers()
                            .map(|x| LocalImageView {
                                handle: unsafe {
                                    br::vkfn_wrapper::create_image_view(
                                        e.graphics().device().native_ptr(),
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

                        continue;
                    }
                };

                let current_render_frame_state =
                    &mut frame_render_states[fd.backbuffer_index as usize];

                if current_render_frame_state.rendering {
                    current_render_frame_state
                        .completion
                        .wait()
                        .expect("Failed to wait previous rendering work");
                    unsafe {
                        current_render_frame_state
                            .cb
                            .reset()
                            .expect("Failed to reset previous rendering commands");
                    }
                    current_render_frame_state.rendering = false;
                }

                let dtsec = fd.delta_time.as_secs() as f32
                    + fd.delta_time.subsec_micros() as f32 / 1000_0000.0;
                rot += dtsec * 15.0;
                let rot = rot;
                obj_uniform_mut_buffer
                    .0
                    .guard_map(BufferMapMode::Write, |ptr| unsafe {
                        ptr.get_mut_at::<UniformObjectParameters>(0).object =
                            Quaternion::new(rot, Vector3::up()).into();
                    })
                    .expect("Update DynamicStgBuffer");

                plane_mesh.sync_contents(e.graphics_mut());

                unsafe {
                    current_render_frame_state
                        .cb
                        .nth_ref_mut(0)
                        .begin(&br::CommandBufferBeginInfo::new(), e.graphics().device())
                        .expect("Failed to begin command recording")
                }
                .begin_render_pass(
                    &br::RenderPassBeginInfo::new(
                        &renderpass,
                        &framebuffers[fd.backbuffer_index as usize],
                        br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO),
                        &[br::ClearValue::color([0.0; 4])],
                    ),
                    br::SubpassContents::Inline,
                )
                .bind_pipeline(br::PipelineBindPoint::Graphics, &gp)
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    &pl,
                    0,
                    &[descriptor_cam, descriptor_main],
                    &[],
                )
                .inject(|r| plane_mesh.prepare_draw_buffers(r))
                .inject(|r| plane_mesh.draw(r, 0, 1))
                .end_render_pass()
                .end()
                .expect("Failed to record render commands");

                let update_cb = update_cb.nth_ref(0);
                let render_cb = current_render_frame_state.cb.nth_ref(0);
                let mut update_batch = peridot::SubmissionBatchBuilder::new();
                update_batch.add_command_buffers([update_cb.as_transparent_ref()]);
                let mut render_batch = peridot::SubmissionBatchBuilder::new();
                render_batch.add_command_buffers([render_cb.as_transparent_ref()]);

                unsafe {
                    e.do_render_to_custom_fence(
                        &mut current_render_frame_state.completion,
                        fd.backbuffer_index,
                        Some(update_batch),
                        render_batch,
                    )
                    .expect("Failed to present");
                }
                current_render_frame_state.rendering = true;

                frame_sec_samples[frame_sec_sample_pos] = t0.elapsed().as_secs_f32();
                frame_sec_sample_pos += 1;

                if frame_sec_collect_timer.elapsed() >= std::time::Duration::from_secs(1) {
                    let avg = frame_sec_samples[..frame_sec_sample_pos]
                        .iter()
                        .sum::<f32>()
                        / frame_sec_sample_pos as f32;
                    println!("frame sec avg: {avg}");

                    frame_sec_collect_timer = std::time::Instant::now();
                    frame_sec_sample_pos = 0;
                }
            }
            peridot::Event::Resize(new_size) => {
                for x in frame_render_states.iter_mut() {
                    if x.rendering {
                        x.completion
                            .wait()
                            .expect("Failed to wait previous rendering work");
                        unsafe {
                            x.cb.reset()
                                .expect("Failed to reset previous rendering commands");
                        }
                        x.rendering = false;
                    }
                }
                drop(framebuffers);
                drop(backbuffer_resources);

                e.resize_presenter_backbuffers(new_size);

                backbuffer_resources = e
                    .iter_back_buffers()
                    .map(|x| LocalImageView {
                        handle: unsafe {
                            br::vkfn_wrapper::create_image_view(
                                e.graphics().device().native_ptr(),
                                &br::ImageViewCreateInfo::new(
                                    &x,
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
