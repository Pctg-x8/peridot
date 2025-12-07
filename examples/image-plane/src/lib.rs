use bedrock::{
    self as br, CommandBufferMut, DescriptorPoolMut, RenderPass, ShaderModule, VkHandle,
};
use br::resources::Image;
use br::Device;
use ktx::Texture;
use log::*;
use parking_lot::RwLock;
use peridot::audio::PreloadedPlayableWav;
use peridot::math::{Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector3};
use peridot::{CBSubmissionType, CommandBundle, SubpassDependencyTemplates};
use peridot_math::Zero;
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_rendering_configuration as prc;
use std::ffi::CString;
use std::sync::Arc;

use peridot_command_object::{
    BeginRenderPass, BindGraphicsPipeline, BufferImageDataDesc, BufferUsage,
    ColorAttachmentBlending, CopyBufferToImage, DescriptorSets, EndRenderPass, GraphicsCommand,
    GraphicsCommandCombiner, ImageResourceRange, PipelineBarrier, RangedBuffer, RangedImage,
    StandardMesh,
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

    #[cfg(not(target_os = "android"))]
    #[cfg(not(target_os = "macos"))]
    let mut resource_container = peridot_archive::ArchiveAsync::new(
        peridot::native_io::PlatformNativeFileReaderAsync::open(
            "../../examples/image-plane/assets/resources.par",
        )
        .expect("open resources.par"),
        true,
    )
    .await
    .expect("load resources.par");
    #[cfg(not(target_os = "android"))]
    #[cfg(target_os = "macos")]
    let mut resource_container = peridot_archive::ArchiveAsync::new(
        peridot::native_io::PlatformNativeFileReaderAsync::open(
            std::env::current_exe()
                .expect("current_exe")
                .parent()
                .expect("no parent")
                .join("../Resources/assets.par"),
        )
        .expect("open resources.par"),
        true,
    )
    .await
    .expect("load resources.par");

    let (mut image_data, bgm): (peridot_image::StdTexture2DAsset, PreloadedPlayableWav) =
        futures_util::try_join!(e.load_async("images.example"), e.load_async("bgm"))
            .expect("asset loading");

    if image_data.0.needs_transcoding() {
        // TODO: Transcode先フォーマットはあとでPhysicalDeviceのクエリからみて決める必要がある(PCではASTCサポートが基本ない)
        image_data
            .0
            .transcode_basis(ktx::ffi::KTX_TTF_BC7_RGBA, ktx::TranscodeFlags::empty())
            .expect("failed to transcode to bc7");
    }
    let image_width = image_data.0.base_width();
    let image_height = image_data.0.base_height();
    let offs = image_data
        .0
        .image_offset(0, 0, 0)
        .expect("image_offset failed");
    debug!("image: {image_width}x{image_height}");
    debug!("image data size: {} offs {offs}", image_data.0.data_size());
    // debug!("ImageFormat: {:?}", image_data.0.vk_format());

    // TODO: streamingなassetが複数ある時に相性が悪い どうしたものか
    let bgm = Arc::new(RwLock::new(bgm));
    e.audio_mixer().write().add_process(bgm.clone());
    e.audio_mixer().write().set_master_volume(0.5);

    let mut memory_manager = MemoryManager::new(e.graphics());

    let plane_mesh = peridot::Primitive::uv_plane_centric_xy(1.0, 0.0);
    let mut cam = Camera {
        projection: Some(ProjectionMethod::Physical {
            focal_length: 20.0,
            sensor_width: 35.0,
            sensor_height: 24.0,
            screen_fitting: peridot::math::PhysicalScreenFitting::Shrink,
        }),
        position: Vector3(-4.0, -1.0, -3.0),
        rotation: Quaternion::ONE,
        depth_range: 1.0..10.0,
    };
    cam.look_at(Vector3::ZERO);

    let [vertex_buffer, cam_uniform_buffer, obj_uniform_buffer] = memory_manager
        .allocate_device_local_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new(
                    plane_mesh.byte_length(),
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
                br::BufferCreateInfo::new_for_type::<UniformCameraParameters>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
                br::BufferCreateInfo::new_for_type::<UniformObjectParameters>(
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
            ],
        )
        .expect("Failed to allocate buffers");
    let vertex_buffer = RangedBuffer::from(vertex_buffer);
    let cam_uniform_buffer = RangedBuffer::from(cam_uniform_buffer);
    let obj_uniform_buffer = RangedBuffer::from(obj_uniform_buffer);
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&vertex_buffer.0, c"Vertex Buffer")
        .expect("Failed to set object name");
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&cam_uniform_buffer.0, c"Uniform Buffer[CameraParameters]")
        .expect("Failed to set object name");
    #[cfg(feature = "debug")]
    e.graphics_device()
        .set_object_name(&obj_uniform_buffer.0, c"Uniform Buffer")
        .expect("Faield to set object name");

    let [vertex_buffer_stg, cam_uniform_buffer_stg, obj_uniform_mut_buffer] = memory_manager
        .allocate_upload_buffer_array(
            e.graphics(),
            [
                br::BufferCreateInfo::new(
                    vertex_buffer.byte_length() as _,
                    br::BufferUsage::TRANSFER_SRC,
                ),
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
    let mut vertex_buffer_stg = RangedBuffer::from(vertex_buffer_stg);
    let mut cam_uniform_buffer_stg = RangedBuffer::from(cam_uniform_buffer_stg);
    let mut obj_uniform_mut_buffer = RangedBuffer::from(obj_uniform_mut_buffer);
    vertex_buffer_stg
        .0
        .clone_content_from_slice(&plane_mesh.vertices)
        .expect("Failed to set upload content");
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
            br::ImageCreateInfo::new(
                br::Extent2D {
                    width: image_width,
                    height: image_height,
                },
                br::vk::VK_FORMAT_BC7_UNORM_BLOCK,
            )
            .with_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::TRANSFER_DEST)
            .init_layout(br::ImageLayout::Preinitialized),
        )
        .expect("Failed to allocate main image");
    let mut image_data_stg_buffer = memory_manager
        .allocate_upload_linear_image_buffer(
            e.graphics(),
            image_width,
            image_height,
            peridot::PixelFormat::BC7,
            br::BufferUsage::TRANSFER_SRC,
        )
        .expect("Failed to allocate linear image buffer");
    image_data_stg_buffer
        .copy_content_from_slice(unsafe {
            core::slice::from_raw_parts(image_data.0.data().add(offs), image_data.0.data_size())
        })
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
            let init_vertex = vertex_buffer.byref_mirror_from(&vertex_buffer_stg);
            let init_cam_uniform = cam_uniform_buffer.byref_mirror_from(&cam_uniform_buffer_stg);
            let init_obj_uniform = obj_uniform_buffer.byref_mirror_from(&obj_uniform_mut_buffer);
            let init_tex = CopyBufferToImage::new(&image_data_stg_buffer.inner, &image).with_range(
                BufferImageDataDesc::new(0, image_data_stg_buffer.row_texels),
                ImageResourceRange::for_single_color_from_rect2d(
                    image.size().wh().into_rect(br::vk::VkOffset2D::ZERO),
                ),
            );
            let copies = (init_vertex, init_cam_uniform, init_obj_uniform, init_tex);

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
                    .begin(&br::CommandBufferBeginInfo::new())
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
    let single_smp_refs = [smp.as_transparent_ref()];
    let rc: prc::CompiledRenderingConfigurationVk = e
        .load_async("builtin.rendering_configuration.unlit_image")
        .await
        .expect("Loading rendering configuration");
    let dsl_rc = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(
            &rc.descriptor_set_bindings
                .iter()
                .enumerate()
                .map(|(n, x)| match x {
                    prc::DescriptorTypeVk::CombinedImageSampler => {
                        // TODO: immutable sampler or dynamic sampler selection in rendering configuration
                        br::DescriptorType::CombinedImageSampler
                            .make_binding(n as _, 1)
                            .with_immutable_samplers(&single_smp_refs)
                    }
                    prc::DescriptorTypeVk::UniformBuffer { .. } => {
                        br::DescriptorType::UniformBuffer.make_binding(n as _, 1)
                    }
                    prc::DescriptorTypeVk::StorageBuffer { .. } => {
                        br::DescriptorType::StorageBuffer.make_binding(n as _, 1)
                    }
                })
                .collect::<Vec<_>>(),
        ),
    )
    .expect("Create DescriptorSetLayout for Material");
    let dsl_ub1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::UniformBuffer
            .make_binding(0, 1)
            .only_for_vertex()]),
    )
    .expect("Create DescriptorSetLayout with UniformBuffer(x1)");
    let mut descriptor_uniform_counts = 2; // camera+object
    let mut descriptor_sampler_counts = 0;
    let mut descriptor_storage_counts = 0;
    for x in rc.descriptor_set_bindings.iter() {
        match x {
            prc::DescriptorTypeVk::CombinedImageSampler => {
                descriptor_sampler_counts += 1;
            }
            prc::DescriptorTypeVk::UniformBuffer { .. } => {
                descriptor_uniform_counts += 1;
            }
            prc::DescriptorTypeVk::StorageBuffer { .. } => {
                descriptor_storage_counts += 1;
            }
        }
    }
    let mut descriptor_pool = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            3,
            &[
                br::DescriptorType::UniformBuffer.make_size(descriptor_uniform_counts),
                br::DescriptorType::StorageBuffer.make_size(descriptor_storage_counts),
                br::DescriptorType::CombinedImageSampler.make_size(descriptor_sampler_counts),
            ],
        ),
    )
    .expect("Create DescriptorPool");

    let pl = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &[
                dsl_ub1.as_transparent_ref(),
                dsl_ub1.as_transparent_ref(),
                dsl_rc.as_transparent_ref(),
            ],
            &if rc.push_constant_buffer_size_bytes > 0 {
                vec![br::PushConstantRange::new(
                    br::vk::VK_SHADER_STAGE_ALL,
                    0..rc.push_constant_buffer_size_bytes as _,
                )]
            } else {
                vec![]
            },
        ),
    )
    .expect("Create PipelineLayout");
    let [gp] = match rc.passes["Unlit"] {
        prc::ShadingPassVk::SimpleDeriveBuiltinPass { ref name } => {
            todo!("using builtin pass: {name}");
        }
        prc::ShadingPassVk::Custom {
            ref option_overrides,
            ref variants,
        } => {
            let prc::Code {
                ref words,
                ref vertex_entry_point_name,
                ref fragment_entry_point_name,
                ref vertex_semantic_to_location,
            } = variants[&prc::VariantKey { instancing: false }];
            let sc = [br::Extent2D::from(screen_size).into_rect(br::Offset2D::ZERO)];
            let vp = [sc[0].make_viewport(0.0..1.0)];

            let shader = br::ShaderModuleObject::new(
                e.graphics().device().clone(),
                &br::ShaderModuleCreateInfo::new(words),
            )
            .expect("Failed to instantiate pass shader");
            let mut stage_with_ep_names = Vec::with_capacity(2);
            if let Some(e) = vertex_entry_point_name {
                stage_with_ep_names.push((
                    br::ShaderStage::Vertex,
                    CString::new(e as &str).expect("invalid entry point name"),
                ));
            }
            if let Some(e) = fragment_entry_point_name {
                stage_with_ep_names.push((
                    br::ShaderStage::Fragment,
                    CString::new(e as &str).expect("invalid entry point name"),
                ));
            }

            // TODO: このへんのパラメータもRendering Configurationで指定できるようにする
            e.graphics()
                .device()
                .new_graphics_pipeline_array(
                    &[br::GraphicsPipelineCreateInfo::new(
                        &pl,
                        renderpass.subpass(0),
                        &stage_with_ep_names
                            .iter()
                            .map(|&(s, ref e)| shader.on_stage(s, e))
                            .collect::<Vec<_>>(),
                        &br::PipelineVertexInputStateCreateInfo::new(
                            &[br::vk::VkVertexInputBindingDescription::per_vertex_typed::<
                                peridot::VertexUV,
                            >(0)],
                            &[
                                br::vk::VkVertexInputAttributeDescription {
                                    binding: 0,
                                    location: vertex_semantic_to_location
                                        [&prc::VertexInputSemantic::Position(0)],
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    offset: core::mem::offset_of!(peridot::VertexUV, pos) as _,
                                },
                                br::vk::VkVertexInputAttributeDescription {
                                    binding: 0,
                                    location: vertex_semantic_to_location
                                        [&prc::VertexInputSemantic::Texcoord(0)],
                                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    offset: core::mem::offset_of!(peridot::VertexUV, uv) as _,
                                },
                            ],
                        ),
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::TriangleStrip,
                        ),
                        &br::PipelineViewportStateCreateInfo::new_array(&vp, &sc),
                        &br::PipelineRasterizationStateCreateInfo::new(
                            match option_overrides.mode.unwrap_or_default() {
                                prc::PolygonRasterizationMode::Point => br::PolygonMode::Point,
                                prc::PolygonRasterizationMode::Line => br::PolygonMode::Line,
                                prc::PolygonRasterizationMode::Fill => br::PolygonMode::Fill,
                            },
                            match option_overrides.culling.unwrap_or_default() {
                                prc::FaceCulling::None => br::CullModeFlags::NONE,
                                prc::FaceCulling::Front => br::CullModeFlags::FRONT,
                                prc::FaceCulling::Back => br::CullModeFlags::BACK,
                                prc::FaceCulling::Both => br::CullModeFlags::FRONT_AND_BACK,
                            },
                            match option_overrides.front_face.unwrap_or_default() {
                                prc::FrontFace::CounterClockwise => br::FrontFace::CounterClockwise,
                                prc::FrontFace::Clockwise => br::FrontFace::Clockwise,
                            },
                        ),
                        &br::PipelineColorBlendStateCreateInfo::new(&[
                            ColorAttachmentBlending::Disabled.into_vk(),
                        ]),
                    )
                    .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())],
                    None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
                )
                .expect("Create GraphicsPipeline")
        }
    };
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
    let [descriptor_cam, descriptor_obj, descriptor_mat] = descriptor_pool
        .alloc_array(&[
            dsl_ub1.as_transparent_ref(),
            dsl_ub1.as_transparent_ref(),
            dsl_rc.as_transparent_ref(),
        ])
        .expect("Create main Descriptor");
    {
        let mut descriptor_writes = Vec::with_capacity(3);
        descriptor_writes.push(descriptor_cam.binding_at(0).write(
            br::DescriptorContents::UniformBuffer(vec![
                cam_uniform_buffer.make_descriptor_buffer_ref(),
            ]),
        ));
        descriptor_writes.push(descriptor_obj.binding_at(0).write(
            br::DescriptorContents::UniformBuffer(vec![
                obj_uniform_buffer.make_descriptor_buffer_ref(),
            ]),
        ));
        // TODO: Material Parameter
        descriptor_writes.extend(
            br::DescriptorPointer::new(descriptor_mat.into(), 0).write_continuous_bindings([
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

    let descriptor_sets = DescriptorSets(vec![descriptor_cam, descriptor_obj, descriptor_mat]);
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

    bgm.write().play();

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
                obj_uniform_mut_buffer
                    .0
                    .guard_map(BufferMapMode::Write, |ptr| unsafe {
                        ptr.get_mut_at::<UniformObjectParameters>(0).object =
                            Quaternion::new(rot, Vector3::up()).into();
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
