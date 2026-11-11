use bedrock::{self as br, CommandBufferMut, DescriptorPoolMut, RenderPass, VkHandle};
use br::Device;
use peridot::{VertexInputSemantic, mthelper::SharedRef};
use peridot_command_object::{
    BeginRenderPass, BindGraphicsPipeline, BufferUsage, ColorAttachmentBlending, CopyBuffer,
    DescriptorSets, EndRenderPass, GraphicsCommand, GraphicsCommandCombiner,
    GraphicsCommandSubmission, PipelineBarrier, RangedBuffer, StandardMesh,
};
use peridot_image::StdTexture2DAsset;
use peridot_memory_manager::{BufferMapMode, MemoryManager};
use peridot_runtime_asset_manager::RuntimeAssetManager;

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

struct MainFramebuffer {
    gfx_device: peridot::VulkanGfx,
    swapchain_image_view: br::vk::VkImageView,
    framebuffer: br::vk::VkFramebuffer,
}
impl Drop for MainFramebuffer {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_framebuffer(
                self.gfx_device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.framebuffer),
                None,
            );
            br::vkfn_wrapper::destroy_image_view(
                self.gfx_device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.swapchain_image_view),
                None,
            );
        }
    }
}
impl br::VkHandle for MainFramebuffer {
    type Handle = br::vk::VkFramebuffer;

    fn native_ptr(&self) -> Self::Handle {
        self.framebuffer
    }
}
impl MainFramebuffer {
    pub fn new(
        e: &peridot::Engine<'_, impl peridot::NativeLinker>,
        render_pass: &(impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized),
        swapchain_buffer_index: usize,
    ) -> Self {
        let bb_size = e.back_buffer_size();
        let swapchain_image_view = unsafe {
            br::vkfn_wrapper::create_image_view(
                e.graphics().device().as_transparent_ref(),
                &br::ImageViewCreateInfo::new(
                    &e.back_buffer(swapchain_buffer_index)
                        .expect("no back buffer?"),
                    br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    br::vk::VK_IMAGE_VIEW_TYPE_2D,
                    e.back_buffer_format(),
                ),
                None,
            )
            .expect("Failed to create swapchain back buffer view")
        };

        let framebuffer = unsafe {
            br::vkfn_wrapper::create_framebuffer(
                e.graphics().device().as_transparent_ref(),
                &br::FramebufferCreateInfo::new(
                    render_pass,
                    &[br::VkHandleRef::dangling(swapchain_image_view)],
                    bb_size.0,
                    bb_size.1,
                ),
                None,
            )
            .expect("Failed to create framebuffer")
        };

        Self {
            gfx_device: e.graphics().device().clone(),
            swapchain_image_view,
            framebuffer,
        }
    }
}

pub async fn game_main<'q>(e: &mut peridot::Engine<'q, impl peridot::NativeLinker>) {
    init_controls(e);

    let mut memory_manager = MemoryManager::new(e.graphics());
    let mut runtime_asset_manager = RuntimeAssetManager::new();

    let bb_size = e.back_buffer_size();

    let renderpass = br::RenderPassObject::new(
        e.graphics().device().clone(),
        &br::RenderPassCreateInfo::new(
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
        ),
    )
    .expect("Failed to create RenderPass");
    let framebuffers = (0..e.back_buffer_count())
        .map(|n| MainFramebuffer::new(e, &renderpass, n))
        .collect::<Vec<_>>();

    let smp = br::SamplerObject::new(e.graphics().device().clone(), &br::SamplerCreateInfo::new())
        .expect("Failed to create sampler");

    let shader = runtime_asset_manager
        .load::<peridot_shader::asset::AssetCore>("shaders.blit", e, &mut memory_manager)
        .expect("Failed to load blit shader asset")
        .expect("Missing blit shader asset");
    let shader_ref = runtime_asset_manager
        .get_typed(&shader)
        .expect("invalid ref");
    let pl = br::PipelineLayoutObject::new(
        e.graphics().device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &shader_ref
                .descriptor_set_layouts
                .iter()
                .map(|x| x.as_transparent_ref())
                .collect::<Vec<_>>(),
            &[],
        ),
    )
    .expect("Failed to create PipelineLayout");

    let scissors = [br::Extent2D::from(bb_size).into_rect(br::Offset2D::ZERO)];
    let viewports = [scissors[0].make_viewport(0.0..1.0)];
    let [pipeline] = e
        .graphics()
        .device()
        .new_graphics_pipeline_array(
            &[br::GraphicsPipelineCreateInfo::new(
                &pl,
                renderpass.subpass(0),
                &shader_ref.pipeline_shader_stages().collect::<Vec<_>>(),
                &br::PipelineVertexInputStateCreateInfo::new(
                    &[br::VertexInputBindingDescription::per_vertex_typed::<
                        peridot::VertexUV2D,
                    >(0)],
                    &[
                        br::VertexInputAttributeDescription(
                            br::vk::VkVertexInputAttributeDescription {
                                binding: 0,
                                location: shader_ref
                                    .vertex_input_semantic_to_location(
                                        &VertexInputSemantic::Position(0),
                                    )
                                    .expect("no position input?"),
                                format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                                offset: core::mem::offset_of!(peridot::VertexUV2D, pos) as _,
                            },
                        ),
                        br::VertexInputAttributeDescription(
                            br::vk::VkVertexInputAttributeDescription {
                                binding: 0,
                                location: shader_ref
                                    .vertex_input_semantic_to_location(
                                        &VertexInputSemantic::Texcoord(0),
                                    )
                                    .expect("no texcoord input?"),
                                format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                                offset: core::mem::offset_of!(peridot::VertexUV2D, uv) as _,
                            },
                        ),
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
            .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())],
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Failed to create GraphicsPipeline");
    let pipeline = pipeline.clone_parent();

    let sprite_plane = peridot::Primitive::uv_plane_centric(32.0);

    let main_image = runtime_asset_manager
        .load::<StdTexture2DAsset>(
            "images.peridot_default_tapfx_circle",
            e,
            &mut memory_manager,
        )
        .expect("main_image.load")
        .expect("main_image.missing");

    let shader_ref = runtime_asset_manager
        .get_typed(&shader)
        .expect("invalid ref");

    let (buffer, offsets) = memory_manager
        .allocate_device_local_buffer_with_contents(
            e.graphics(),
            [
                peridot::BufferContent::vertices_for(&sprite_plane.vertices),
                peridot::BufferContent::Uniform(shader_ref.material_uniform_buffer_size, 16),
            ],
            br::BufferUsage::TRANSFER_DEST,
        )
        .expect("Failed to create device buffer");
    let buffer = SharedRef::new(buffer);
    let vertex_buffer =
        RangedBuffer::from_offset_length(buffer.clone(), offsets[0], sprite_plane.byte_length());
    let uniform_buffer = RangedBuffer::from_offset_length(
        buffer.clone(),
        offsets[1],
        shader_ref.material_uniform_buffer_size as _,
    );

    let mut uniform_mut_buffer: RangedBuffer<_> = memory_manager
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                shader_ref.material_uniform_buffer_size,
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("Failed to allocate mutable buffer")
        .into();
    let (mut staging_buffer, staging_offsets) = memory_manager
        .allocate_upload_buffer_with_contents(
            e.graphics(),
            [peridot::BufferContent::raw_for_slice(
                &sprite_plane.vertices,
            )],
            br::BufferUsage::TRANSFER_SRC,
        )
        .expect("Failed to create staging buffer");
    staging_buffer
        .guard_map(BufferMapMode::Write, |ptr| unsafe {
            ptr.clone_slice_to(staging_offsets[0] as _, &sprite_plane.vertices);
        })
        .expect("Failed to stage initial vertex buffer memory");

    {
        let all_stg_buffer = RangedBuffer::from(&staging_buffer);

        let init_vertices = CopyBuffer::new(&staging_buffer, &vertex_buffer.0).with_range(
            staging_offsets[0],
            vertex_buffer.offset(),
            sprite_plane.byte_length(),
        );
        let init = init_vertices;

        let in_barriers = PipelineBarrier::new()
            .by_region()
            .with_barrier(
                vertex_buffer
                    .make_ref()
                    .usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
            )
            .with_barrier(
                all_stg_buffer.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
            );
        let out_barriers = PipelineBarrier::new().by_region().with_barriers([
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

    let mut dp = br::DescriptorPoolObject::new(
        e.graphics().device().clone(),
        &br::DescriptorPoolCreateInfo::new(
            shader_ref.descriptor_set_layouts.len() as _,
            &[
                br::DescriptorType::UniformBuffer.make_size(
                    shader_ref
                        .descriptor_pool_size_per_set
                        .iter()
                        .map(|x| x.uniform_buffer)
                        .sum::<u32>(),
                ),
                br::DescriptorType::CombinedImageSampler.make_size(
                    shader_ref
                        .descriptor_pool_size_per_set
                        .iter()
                        .map(|x| x.combined_image_sampler)
                        .sum::<u32>(),
                ),
            ],
        ),
    )
    .expect("Failed to create DescriptorPool");
    let descriptor_sets = dp
        .alloc(
            &shader_ref
                .descriptor_set_layouts
                .iter()
                .map(|x| x.as_transparent_ref())
                .collect::<Vec<_>>(),
        )
        .expect("Failed to alloc Required Descriptors");

    let mut descriptor_writes = Vec::new();
    shader_ref.write_descriptor_sets(
        &descriptor_sets,
        &uniform_buffer.0,
        uniform_buffer.offset(),
        &mut descriptor_writes,
    );
    shader_ref.write_material_texture_property(
        "params.tex",
        br::DescriptorImageInfo::new(
            &runtime_asset_manager
                .get_typed(&main_image)
                .expect("invalid ref")
                .gfx_resource,
            br::ImageLayout::ShaderReadOnlyOpt,
        )
        .with_sampler(&smp),
        &descriptor_sets,
        &mut descriptor_writes,
    );
    e.graphics()
        .device()
        .update_descriptor_sets(&descriptor_writes, &[]);

    let projection_prop_offset = shader_ref
        .uniform_buffer_offset_for_material_property("params.projection")
        .expect("no params.projection");
    let time_prop_offset = shader_ref
        .uniform_buffer_offset_for_material_property("params.time")
        .expect("no params.time");
    let offset_prop_offset = shader_ref
        .uniform_buffer_offset_for_material_property("params.offset")
        .expect("no params.offset");
    let mut time = 1.0;
    let mut offset = peridot_math::Vector2(0.0f32, 0.0f32);
    uniform_mut_buffer
        .0
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            p.ptr().byte_add(projection_prop_offset as _).cast().write(
                peridot::math::Camera {
                    projection: Some(peridot::math::ProjectionMethod::UI {
                        design_width: bb_size.0 as _,
                        design_height: bb_size.1 as _,
                    }),
                    ..Default::default()
                }
                .projection_matrix(1.0),
            );
            p.ptr().byte_add(time_prop_offset as _).cast().write(time);
            p.ptr()
                .byte_add(offset_prop_offset as _)
                .cast()
                .write(offset);
        })
        .expect("Failed to write initial mut buffer content");

    let mut update_commands =
        peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Transfer, 1)
            .expect("Failed to allocate update commands");
    {
        let copy = CopyBuffer::new(&uniform_mut_buffer.0, &uniform_buffer.0).with_range(
            0,
            uniform_buffer.offset(),
            shader_ref.material_uniform_buffer_size as _,
        );

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
            .execute_and_finish(
                update_commands
                    .synchronized_nth(0)
                    .begin(&br::CommandBufferBeginInfo::new())
                    .expect("Failed to begin recording update commands"),
            )
            .expect("Failed to record commands");
    }

    let descriptor_sets = DescriptorSets(descriptor_sets.to_vec());
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
    for (mut b, fb) in main_commands.iter_mut().zip(&framebuffers) {
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
                b.begin(&br::CommandBufferBeginInfo::new())
                    .expect("Failed to begin recording main commands")
            })
            .expect("Failed to record commands");
    }

    e.start_frame_drain();
    let mut last_mouse_input = false;
    loop {
        match e.next_event().await {
            peridot::Event::NextFrame => {
                let fd = e.prepare_frame().expect("Failed to prepare frame");

                time += fd.delta_time.as_secs_f32();

                let current_mouse_input = e.input().button_pressing_time(INPUT_PLANE_DOWN)
                    > std::time::Duration::default();
                if !last_mouse_input && current_mouse_input {
                    time = 0.0;
                    let (ox, oy) = e.input().get_plane_position(0).unwrap_or((0.0, 0.0));
                    offset = peridot::math::Vector2(ox, oy);
                }
                last_mouse_input = current_mouse_input;

                uniform_mut_buffer
                    .0
                    .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
                        p.ptr().byte_add(time_prop_offset as _).cast().write(time);
                        p.ptr()
                            .byte_add(offset_prop_offset as _)
                            .cast()
                            .write(offset);
                    })
                    .expect("Failed to update dynamic buffer");

                let mut update_batch = peridot::SubmissionBatchBuilder::new();
                let update_cb = update_commands.nth_ref(0);
                update_batch.add_command_buffers([update_cb.as_transparent_ref()]);
                let mut render_batch = peridot::SubmissionBatchBuilder::new();
                let render_cb = main_commands.nth_ref(fd.backbuffer_index as _);
                render_batch.add_command_buffers([render_cb.as_transparent_ref()]);
                e.do_render(fd.backbuffer_index, Some(update_batch), render_batch)
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
