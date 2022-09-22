use bedrock as br;
use bedrock::VkHandle;
use br::{CommandBuffer, DescriptorPool, Device, Image, ImageChild, SubmissionBatch};
use peridot::mthelper::{DynamicMutabilityProvider, SharedRef};
use peridot::ModelData;

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

struct BufferOffsets {
    vertex_start: u64,
}
struct FixedBufferInitializer {
    offsets: BufferOffsets,
    sprite_plane: peridot::Primitive<peridot::VertexUV2D>,
}
impl peridot::FixedBufferInitializer for FixedBufferInitializer {
    fn stage_data(&mut self, m: &br::MappedMemoryRange<impl br::DeviceMemory + ?Sized>) {
        self.sprite_plane
            .stage_data_into(m, self.offsets.vertex_start);
    }

    fn buffer_graphics_ready<Device: br::Device>(
        &self,
        tfb: &mut peridot::TransferBatch,
        buffer: &SharedRef<
            peridot::Buffer<
                impl br::Buffer<ConcreteDevice = Device> + 'static,
                impl br::DeviceMemory<ConcreteDevice = Device> + 'static,
            >,
        >,
        buffer_range: std::ops::Range<u64>,
    ) {
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT,
            buffer.clone(),
            (buffer_range.start + self.offsets.vertex_start)
                ..(buffer_range.start
                    + self.offsets.vertex_start
                    + (self.sprite_plane.vertices.len()
                        * std::mem::size_of::<peridot::VertexUV2D>()) as u64),
            br::AccessFlags::VERTEX_ATTRIBUTE_READ,
        );
    }
}

pub struct Game<NL: peridot::NativeLinker> {
    renderpass: br::RenderPassObject<peridot::DeviceObject>,
    framebuffers: Vec<
        br::FramebufferObject<
            peridot::DeviceObject,
            SharedRef<<NL::Presenter as peridot::PlatformPresenter>::Backbuffer>,
        >,
    >,
    _smp: br::SamplerObject<peridot::DeviceObject>,
    _dsl: br::DescriptorSetLayoutObject<peridot::DeviceObject>,
    _dsl2: br::DescriptorSetLayoutObject<peridot::DeviceObject>,
    _descriptor_pool: br::DescriptorPoolObject<peridot::DeviceObject>,
    descriptors: Vec<br::DescriptorSet>,
    pipeline: peridot::LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        br::PipelineLayoutObject<peridot::DeviceObject>,
    >,
    buffers: peridot::FixedMemory<
        peridot::DeviceObject,
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    main_commands: peridot::CommandBundle<peridot::DeviceObject>,
    update_commands: peridot::CommandBundle<peridot::DeviceObject>,
    update_data: UniformValues,
    uniform_start_d: u64,
    last_mouse_input: bool,
    _ph: std::marker::PhantomData<*const NL>,
}
impl<NL: peridot::NativeLinker> peridot::FeatureRequests for Game<NL> {}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game<NL> {
    fn init(e: &mut peridot::Engine<NL>) -> Self {
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

        let renderpass = peridot::RenderPassTemplates::single_render(
            e.backbuffer_format(),
            e.requesting_backbuffer_layout().0,
        )
        .create(e.graphics().device().clone())
        .expect("Failed to create RenderPass");
        let framebuffers: Vec<_> = (0..e.backbuffer_count())
            .map(|bb_index| {
                let b = e.backbuffer(bb_index).expect("no backbuffer?");
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
        let dsl = e
            .graphics()
            .device()
            .clone()
            .new_descriptor_set_layout(&[br::DescriptorSetLayoutBinding::UniformBuffer(
                1,
                br::ShaderStage::VERTEX,
            )])
            .expect("Failed to create DescriptorSetLayout");
        let dsl2 = e
            .graphics()
            .device()
            .clone()
            .new_descriptor_set_layout(&[br::DescriptorSetLayoutBinding::CombinedImageSampler(
                1,
                br::ShaderStage::FRAGMENT,
                &[smp.native_ptr()],
            )])
            .expect("Failed to create DescriptorSetLayout for FragmentShader");
        let mut dp = e
            .graphics()
            .device()
            .clone()
            .new_descriptor_pool(
                2,
                &[
                    br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1),
                    br::DescriptorPoolSize(br::DescriptorType::CombinedImageSampler, 1),
                ],
                false,
            )
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
        let pl = e
            .graphics()
            .device()
            .clone()
            .new_pipeline_layout(&[&dsl, &dsl2], &[])
            .expect("Failed to create PipelineLayout");

        let scissors = [AsRef::<br::vk::VkExtent2D>::as_ref(
            e.backbuffer(0).expect("empty backbuffers").image().size(),
        )
        .clone()
        .into_rect(br::vk::VkOffset2D { x: 0, y: 0 })];
        let viewports = [br::vk::VkViewport::from_rect_with_depth_range(
            &scissors[0],
            0.0..1.0,
        )];
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
        .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
        .create(
            e.graphics().device().clone(),
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Failed to create GraphicsPipeline");
        let pipeline = peridot::LayoutedPipeline::combine(pipeline, pl);

        let main_image_data: peridot_image::PNG = e
            .load("images.peridot_default_tapfx_circle")
            .expect("Failed to load main_image_data");
        let mut tig = peridot::TextureInitializationGroup::new(e.graphics().device().clone());
        tig.add(main_image_data);

        let sprite_plane = peridot::Primitive::uv_plane_centric(32.0);

        let mut bp_dynamic = peridot::BufferPrealloc::new(e.graphics());
        let uniform_start_d = bp_dynamic.add(peridot::BufferContent::uniform::<UniformValues>());
        let mut tfb = peridot::TransferBatch::new();
        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let vertex_start = sprite_plane.prealloc(&mut bp);
        let mut fm_init = FixedBufferInitializer {
            offsets: BufferOffsets { vertex_start },
            sprite_plane,
        };
        let buffers =
            peridot::FixedMemory::new(e.graphics(), bp, bp_dynamic, tig, &mut fm_init, &mut tfb)
                .expect("Alloc FixedBuffers");

        e.submit_commands(|r| {
            let buffer_end_barriers = [br::BufferMemoryBarrier::new(
                &buffers.mut_buffer.0,
                uniform_start_d..(uniform_start_d + std::mem::size_of::<UniformValues>() as u64),
                0,
                br::AccessFlags::HOST.write,
            )];

            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
            r.pipeline_barrier(
                br::PipelineStageFlags::BOTTOM_OF_PIPE,
                br::PipelineStageFlags::VERTEX_INPUT.host(),
                false,
                &[],
                &buffer_end_barriers,
                &[],
            );
        })
        .expect("Failed to execute init command");

        let mut dsub = peridot::DescriptorSetUpdateBatch::new();
        dsub.write(
            descriptors[0],
            0,
            br::DescriptorUpdateInfo::UniformBuffer(vec![(
                buffers.buffer.0.native_ptr(),
                (buffers.mut_buffer_placement + uniform_start_d) as usize
                    ..((buffers.mut_buffer_placement + uniform_start_d) as usize
                        + std::mem::size_of::<UniformValues>()),
            )]),
        );
        dsub.write(
            descriptors[1],
            0,
            br::DescriptorUpdateInfo::CombinedImageSampler(vec![(
                None,
                buffers.textures[0].native_ptr(),
                br::ImageLayout::ShaderReadOnlyOpt,
            )]),
        );
        dsub.submit(e.graphics().device());

        let mut main_commands = peridot::CommandBundle::new(
            e.graphics(),
            peridot::CBSubmissionType::Graphics,
            e.backbuffer_count(),
        )
        .expect("Failed to allocate render commands");
        for (b, fb) in main_commands.iter_mut().zip(&framebuffers) {
            let mut rec = unsafe { b.begin().expect("Failed to begin recording main commands") };
            rec.begin_render_pass(
                &renderpass,
                fb,
                scissors[0].clone(),
                &[br::ClearValue::color([0.0; 4])],
                true,
            );
            pipeline.bind(&mut rec);
            rec.bind_graphics_descriptor_sets(
                0,
                unsafe { std::mem::transmute(&descriptors[..]) },
                &[],
            );
            rec.bind_vertex_buffers(0, &[(&buffers.buffer.0, vertex_start as _)]);
            rec.draw(4, 1, 0, 0);
            rec.end_render_pass();
        }

        let &br::vk::VkExtent3D {
            width: bb_width,
            height: bb_height,
            ..
        } = e.backbuffer(0).expect("empty backbuffers").image().size();
        let update_data = UniformValues {
            mat: peridot::math::Camera {
                projection: Some(peridot::math::ProjectionMethod::UI {
                    design_width: bb_width as _,
                    design_height: bb_height as _,
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
            let mut r = unsafe {
                update_commands[0]
                    .begin()
                    .expect("Failed to begin recording update commands")
            };
            let enter_barriers = [
                br::BufferMemoryBarrier::new(
                    &buffers.mut_buffer.0,
                    uniform_start_d
                        ..(uniform_start_d + std::mem::size_of::<UniformValues>() as u64),
                    br::AccessFlags::HOST.write,
                    br::AccessFlags::TRANSFER.read,
                ),
                br::BufferMemoryBarrier::new(
                    &buffers.buffer.0,
                    buffers.mut_buffer_placement + uniform_start_d
                        ..(buffers.mut_buffer_placement
                            + uniform_start_d
                            + std::mem::size_of::<UniformValues>() as u64),
                    br::AccessFlags::UNIFORM_READ,
                    br::AccessFlags::TRANSFER.write,
                ),
            ];
            let leave_barriers = [
                enter_barriers[0].clone().flip(),
                enter_barriers[1].clone().flip(),
            ];

            r.pipeline_barrier(
                br::PipelineStageFlags::HOST.vertex_shader(),
                br::PipelineStageFlags::TRANSFER,
                false,
                &[],
                &enter_barriers,
                &[],
            )
            .copy_buffer(
                &buffers.mut_buffer.0,
                &buffers.buffer.0,
                &[br::vk::VkBufferCopy {
                    srcOffset: uniform_start_d,
                    dstOffset: buffers.mut_buffer_placement + uniform_start_d,
                    size: std::mem::size_of::<UniformValues>() as _,
                }],
            )
            .pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::HOST.vertex_shader(),
                false,
                &[],
                &leave_barriers,
                &[],
            );
        }

        Game {
            renderpass,
            framebuffers,
            _smp: smp,
            _dsl: dsl,
            _dsl2: dsl2,
            _descriptor_pool: dp,
            descriptors,
            pipeline,
            buffers,
            main_commands,
            update_data,
            update_commands,
            uniform_start_d,
            last_mouse_input: false,
            _ph: std::marker::PhantomData,
        }
    }

    fn update(
        &mut self,
        e: &mut peridot::Engine<NL>,
        on_backbuffer_of: u32,
        delta_time: std::time::Duration,
    ) {
        self.update_data.time +=
            delta_time.as_secs() as f32 + delta_time.subsec_micros() as f32 / 1_000_000.0;

        let current_mouse_input =
            e.input().button_pressing_time(INPUT_PLANE_DOWN) > std::time::Duration::default();
        if !self.last_mouse_input && current_mouse_input {
            self.update_data.time = 0.0;
            let (ox, oy) = e.input().get_plane_position(0).unwrap_or((0.0, 0.0));
            self.update_data.offset = peridot::math::Vector2(ox, oy);
        }
        self.last_mouse_input = current_mouse_input;

        let update_data = &self.update_data;
        DynamicMutabilityProvider::borrow_mut(&*self.buffers.mut_buffer.0)
            .guard_map(
                self.uniform_start_d
                    ..(self.uniform_start_d + std::mem::size_of::<UniformValues>() as u64),
                |m| unsafe {
                    *m.get_mut(0) = update_data.clone();
                },
            )
            .expect("Failed to map dynamic buffer");

        e.do_render(
            on_backbuffer_of,
            Some(br::EmptySubmissionBatch.with_command_buffers(&self.update_commands)),
            br::EmptySubmissionBatch.with_command_buffers(
                &self.main_commands[on_backbuffer_of as usize..=on_backbuffer_of as usize],
            ),
        )
        .expect("Failed to present");
    }
}
