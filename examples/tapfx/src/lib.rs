use std::convert::TryInto;

use bedrock as br;
use br::{
    CommandBuffer, DescriptorPool, Device, Image, ImageChild, ImageSubresourceSlice,
    SubmissionBatch,
};
use peridot::mthelper::SharedRef;
use peridot::ModelData;
use peridot_command_object::{
    BeginRenderPass, BufferImageDataDesc, BufferUsage, ColorAttachmentBlending, CopyBuffer,
    CopyBufferToImage, DescriptorSets, EndRenderPass, GraphicsCommand, GraphicsCommandCombiner,
    GraphicsCommandSubmission, ImageResourceRange, Mesh, PipelineBarrier, RangedBuffer,
    RangedImage, StandardMesh,
};

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

pub struct Game<NL: peridot::NativeLinker> {
    renderpass: br::RenderPassObject<peridot::DeviceObject>,
    framebuffers: Vec<
        br::FramebufferObject<
            peridot::DeviceObject,
            SharedRef<<NL::Presenter as peridot::PlatformPresenter>::BackBuffer>,
        >,
    >,
    color_renders: Box<dyn GraphicsCommand>,
    _smp: br::SamplerObject<peridot::DeviceObject>,
    _dsl: br::DescriptorSetLayoutObject<peridot::DeviceObject>,
    _dsl2: br::DescriptorSetLayoutObject<peridot::DeviceObject>,
    _descriptor_pool: br::DescriptorPoolObject<peridot::DeviceObject>,
    descriptors: Vec<br::DescriptorSet>,
    dynamic_buffer: RangedBuffer<
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    _main_image_view: br::ImageViewObject<
        peridot::Image<
            br::ImageObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    main_commands: peridot::CommandBundle<peridot::DeviceObject>,
    update_commands: peridot::CommandBundle<peridot::DeviceObject>,
    update_data: UniformValues,
    last_mouse_input: bool,
    _ph: std::marker::PhantomData<*const NL>,
}
impl<NL: peridot::NativeLinker> peridot::FeatureRequests for Game<NL> {}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game<NL> {
    fn init(e: &mut peridot::Engine<NL>) -> Self {
        init_controls(e);

        let bb_size = e
            .back_buffer(0)
            .expect("empty back-buffers")
            .image()
            .size()
            .wh();

        let renderpass = peridot::RenderPassTemplates::single_render(
            e.back_buffer_format(),
            e.requesting_back_buffer_layout().0,
        )
        .create(e.graphics().device().clone())
        .expect("Failed to create RenderPass");
        let framebuffers: Vec<_> = (0..e.back_buffer_count())
            .map(|bb_index| {
                let b = e.back_buffer(bb_index).expect("no backbuffer?");
                e.graphics()
                    .device()
                    .clone()
                    .new_framebuffer(&renderpass, vec![b.clone()], b.image().size().as_ref(), 1)
                    .expect("Failed to create Framebuffer")
            })
            .collect();

        let smp = br::SamplerBuilder::default()
            .create(e.graphics().device().clone())
            .expect("Failed to create sampler");
        let dsl =
            br::DescriptorSetLayoutBuilder::with_bindings(vec![br::DescriptorType::UniformBuffer
                .make_binding(1)
                .only_for_vertex()])
            .create(e.graphics().device().clone())
            .expect("Failed to create DescriptorSetLayout");
        let dsl2 = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_fragment()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&smp)]),
        ])
        .create(e.graphics().device().clone())
        .expect("Failed to create DescriptorSetLayout for FragmentShader");
        let mut dp = br::DescriptorPoolBuilder::new(2)
            .with_reservations(vec![
                br::DescriptorType::UniformBuffer.with_count(1),
                br::DescriptorType::CombinedImageSampler.with_count(1),
            ])
            .create(e.graphics().device().clone())
            .expect("Failed to create DescriptorPool");
        let descriptors = dp
            .alloc(&[&dsl, &dsl2])
            .expect("Failed to alloc Required Descriptors");

        let shaders = peridot_vertex_processing_pack::PvpShaderModules::new(
            e.graphics().device(),
            e.load("shaders.blit").expect("Failed to blit shader"),
        )
        .expect("Failed to generate ShaderModules");
        let vps = shaders.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let pl = br::PipelineLayoutBuilder::new(vec![&dsl, &dsl2], vec![])
            .create(e.graphics().device().clone())
            .expect("Failed to create PipelineLayout");

        let scissors = [bb_size.clone().into_rect(br::vk::VkOffset2D::ZERO)];
        let viewports = [scissors[0].make_viewport(0.0..1.0)];
        let pipeline = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(&pl, (&renderpass, 0), vps)
        .viewport_scissors(
            br::DynamicArrayState::Static(&viewports),
            br::DynamicArrayState::Static(&scissors),
        )
        .multisample_state(br::MultisampleState::new().into())
        .set_attachment_blends(vec![ColorAttachmentBlending::PREMULTIPLIED_ALPHA.into_vk()])
        .create(
            e.graphics().device().clone(),
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Failed to create GraphicsPipeline");
        let pipeline = peridot::LayoutedPipeline::combine(pipeline, pl);

        let main_image_data: peridot_image::PNG = e
            .load("images.peridot_default_tapfx_circle")
            .expect("Failed to load main_image_data");
        let sprite_plane = peridot::Primitive::uv_plane_centric(32.0);

        let mut bp_dynamic = peridot::BufferPrealloc::new(e.graphics());
        let uniform_start_d = bp_dynamic.add(peridot::BufferContent::uniform::<UniformValues>());
        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let vertex_start = sprite_plane.prealloc(&mut bp);
        let mut bp_stg = bp.clone();
        let dynamic_start = bp.merge(&bp_dynamic);
        let main_image_bytes_start = bp_stg.add(peridot::BufferContent::raw_dynarray::<u32>(
            (main_image_data.0.u8_pixels().len() >> 2) as _,
        ));

        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp.build_transferred()
                .expect("Failed to create main buffer"),
        ));
        mb.add(peridot::MemoryBadgetEntry::Image(
            br::ImageDesc::new(
                main_image_data.0.size,
                main_image_data.0.format as _,
                br::ImageUsage::SAMPLED.transfer_dest(),
                br::ImageLayout::Preinitialized,
            )
            .create(e.graphics().device().clone())
            .expect("Failed to create main image"),
        ));
        let Ok::<[_; 2], _>([
            peridot::MemoryBoundResource::Buffer(buffer),
            peridot::MemoryBoundResource::Image(main_image)
        ]) = mb.alloc().expect("Failed to allocate device memory").try_into() else {
            unreachable!("unexpected resources");
        };

        let mut mb =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp_dynamic
                .build_upload()
                .expect("Failed to create dynamic buffer"),
        ));
        let Ok::<[_; 1], _>([
            peridot::MemoryBoundResource::Buffer(dynamic_buffer)
        ]) = mb.alloc_upload().expect("Failed to allocate dynamic memory").try_into() else {
            unreachable!("unexpected resources");
        };

        let mut mb =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp_stg
                .build_upload()
                .expect("Failed to create staging buffer"),
        ));
        let Ok::<[_; 1], _>([
            peridot::MemoryBoundResource::Buffer(mut stg_buffer)
        ]) = mb.alloc_upload().expect("Failed to allocate staging memory").try_into() else {
            unreachable!("unexpected resources");
        };

        stg_buffer
            .guard_map(0..bp_stg.total_size(), |m| {
                sprite_plane.stage_data_into(m, vertex_start);
                unsafe {
                    m.clone_from_slice_at(
                        main_image_bytes_start as _,
                        main_image_data.0.u8_pixels(),
                    );
                }
            })
            .expect("Failed to stage initial vertex buffer memory");

        let vertex_buffer =
            RangedBuffer::from_offset_length(&buffer, vertex_start, sprite_plane.byte_length());
        let uniform_buffer =
            RangedBuffer::for_type::<UniformValues>(&buffer, dynamic_start + uniform_start_d);
        let dynamic_buffer =
            RangedBuffer::for_type::<UniformValues>(dynamic_buffer, uniform_start_d);

        {
            let all_buffer = RangedBuffer::from_offset_length(&buffer, 0, bp.total_size() as _);
            let all_stg_buffer =
                RangedBuffer::from_offset_length(&stg_buffer, 0, bp_stg.total_size() as _);
            let image = RangedImage::single_color_plane(&main_image);

            let copy = CopyBuffer::new(&stg_buffer, &buffer).with_mirroring(0, dynamic_start as _);
            let image_copy = CopyBufferToImage::new(&stg_buffer, &main_image).with_range(
                BufferImageDataDesc::new(
                    main_image_bytes_start,
                    (main_image_data.0.stride / (main_image_data.0.format.bpp() >> 3)) as _,
                ),
                ImageResourceRange::for_single_color_from_rect2d(
                    br::vk::VkExtent2D::from(main_image_data.0.size)
                        .into_rect(br::vk::VkOffset2D::ZERO),
                ),
            );

            let [image_in_barrier, image_out_barrier] = image.barrier3(
                br::ImageLayout::Preinitialized,
                br::ImageLayout::TransferDestOpt,
                br::ImageLayout::ShaderReadOnlyOpt,
            );
            let in_barriers = PipelineBarrier::new()
                .by_region()
                .with_barrier(image_in_barrier)
                .with_barrier(
                    all_buffer.usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                )
                .with_barrier(
                    all_stg_buffer.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                );
            let out_barriers = PipelineBarrier::new()
                .by_region()
                .with_barrier(image_out_barrier)
                .with_barrier(
                    vertex_buffer
                        .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_BUFFER),
                )
                .with_barrier(
                    dynamic_buffer.usage_barrier(BufferUsage::UNUSED, BufferUsage::HOST_RW),
                );

            (copy, image_copy)
                .between(in_barriers, out_barriers)
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
                br::DescriptorPointer::new(descriptors[0].into(), 0).write(
                    br::DescriptorContents::UniformBuffer(vec![
                        uniform_buffer.make_descriptor_buffer_ref()
                    ]),
                ),
                br::DescriptorPointer::new(descriptors[1].into(), 0).write(
                    br::DescriptorContents::CombinedImageSampler(vec![
                        br::DescriptorImageRef::new(
                            &main_image_view,
                            br::ImageLayout::ShaderReadOnlyOpt,
                        ),
                    ]),
                ),
            ],
            &[],
        );

        let update_data = UniformValues {
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
            let dynamic_buffer = dynamic_buffer.make_ref();

            let copy = CopyBuffer::new(&dynamic_buffer.0, &buffer)
                .with_range_for_type::<UniformValues>(
                    uniform_start_d,
                    dynamic_start + uniform_start_d,
                );

            let [uniform_in_barrier, uniform_out_barrier] = uniform_buffer
                .usage_barrier3_switching(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST);
            let [dynamic_in_barrier, dynamic_out_barrier] = dynamic_buffer
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);
            let in_barriers = [uniform_in_barrier, dynamic_in_barrier];
            let out_barriers = [uniform_out_barrier, dynamic_out_barrier];

            copy.between(in_barriers, out_barriers)
                .execute_and_finish(unsafe {
                    update_commands[0]
                        .begin()
                        .expect("Failed to begin recording update commands")
                        .as_dyn_ref()
                })
                .expect("Failed to record commands");
        }

        let vertex_buffer_range = vertex_buffer.1;
        let descriptor_sets = DescriptorSets(vec![descriptors[0].into(), descriptors[1].into()]);
        let mesh = StandardMesh {
            vertex_buffers: vec![RangedBuffer(buffer, vertex_buffer_range)],
            vertex_count: 4,
        };
        let setup = (pipeline, descriptor_sets.into_bind_graphics());
        let color_renders = mesh.draw(1).after_of(setup);

        let mut main_commands = peridot::CommandBundle::new(
            e.graphics(),
            peridot::CBSubmissionType::Graphics,
            e.back_buffer_count(),
        )
        .expect("Failed to allocate render commands");
        for (b, fb) in main_commands.iter_mut().zip(&framebuffers) {
            let rp = BeginRenderPass::new(&renderpass, fb, scissors[0].clone())
                .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

            (&color_renders)
                .between(rp, EndRenderPass)
                .execute_and_finish(unsafe {
                    b.begin()
                        .expect("Failed to begin recording main commands")
                        .as_dyn_ref()
                })
                .expect("Failed to record commands");
        }

        Self {
            renderpass,
            framebuffers,
            color_renders: color_renders.boxed(),
            _smp: smp,
            _dsl: dsl,
            _dsl2: dsl2,
            _descriptor_pool: dp,
            descriptors,
            dynamic_buffer,
            main_commands,
            _main_image_view: main_image_view,
            update_data,
            update_commands,
            last_mouse_input: false,
            _ph: std::marker::PhantomData,
        }
    }

    fn update(
        &mut self,
        e: &mut peridot::Engine<NL>,
        on_back_buffer_of: u32,
        delta_time: std::time::Duration,
    ) {
        self.update_data.time += delta_time.as_secs_f32();

        let current_mouse_input =
            e.input().button_pressing_time(INPUT_PLANE_DOWN) > std::time::Duration::default();
        if !self.last_mouse_input && current_mouse_input {
            self.update_data.time = 0.0;
            let (ox, oy) = e.input().get_plane_position(0).unwrap_or((0.0, 0.0));
            self.update_data.offset = peridot::math::Vector2(ox, oy);
        }
        self.last_mouse_input = current_mouse_input;

        let update_data = &self.update_data;
        self.dynamic_buffer
            .guard_map(|m| unsafe {
                *m.get_mut(0) = update_data.clone();
            })
            .expect("Failed to map dynamic buffer");

        e.do_render(
            on_back_buffer_of,
            Some(br::EmptySubmissionBatch.with_command_buffers(&self.update_commands)),
            br::EmptySubmissionBatch.with_command_buffers(
                &self.main_commands[on_back_buffer_of as usize..=on_back_buffer_of as usize],
            ),
        )
        .expect("Failed to present");
    }
}
