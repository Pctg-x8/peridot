
use bedrock as br;
use bedrock::VkHandle;
use std::rc::Rc;

#[repr(C)]
#[derive(Clone)]
pub struct UniformValues {
    pub mat: peridot::math::Matrix4F32,
    pub time: f32, pub _resv: f32,
    pub offset: peridot::math::Vector2F32
}

pub const INPUT_PLANE_DOWN: u16 = 0;
pub const INPUT_PLANE_LEFT: u8 = 0;
pub const INPUT_PLANE_TOP: u8 = 1;

pub struct Game<NL> {
    renderpass: br::RenderPass,
    framebuffers: Vec<br::Framebuffer>,
    _dsl: br::DescriptorSetLayout,
    _descriptor_pool: br::DescriptorPool,
    descriptors: Vec<br::vk::VkDescriptorSet>,
    pipeline: peridot::LayoutedPipeline,
    buffer: peridot::Buffer,
    dynamic_buffer: peridot::Buffer,
    main_commands: peridot::CommandBundle,
    update_commands: peridot::CommandBundle,
    update_data: UniformValues,
    uniform_start_d: u64,
    last_mouse_input: bool,
    _ph: std::marker::PhantomData<*const NL>
}
impl<NL> peridot::FeatureRequests for Game<NL> {}
impl<NL> Game<NL> {
    pub const NAME: &'static str = "Peridot Examples: Tap Effect";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game<NL> {
    fn init(e: &mut peridot::Engine<NL>) -> Self {
        e.input_mut().map(peridot::NativeButtonInput::Mouse(0), INPUT_PLANE_DOWN);
        e.input_mut().map(peridot::NativeAnalogInput::MouseX, INPUT_PLANE_LEFT);
        e.input_mut().map(peridot::NativeAnalogInput::MouseY, INPUT_PLANE_TOP);

        let renderpass = peridot::RenderPassTemplates::single_render(e.backbuffer_format())
            .create(e.graphics())
            .expect("Failed to create RenderPass");
        let framebuffers: Vec<_> = e.backbuffers().iter()
            .map(|b| br::Framebuffer::new(&renderpass, &[b], b.size(), 1).expect("Failed to create Framebuffer"))
            .collect();
        
        let dsl = br::DescriptorSetLayout::new(
            e.graphics(),
            &[br::DescriptorSetLayoutBinding::UniformBuffer(1, br::ShaderStage::VERTEX)]
        ).expect("Failed to create DescriptorSetLayout");
        let dp = br::DescriptorPool::new(
            e.graphics(),
            1,
            &[br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1)],
            false
        ).expect("Failed to create DescriptorPool");
        let descriptors = dp.alloc(&[&dsl]).expect("Failed to alloc Required Descriptors");
        
        let shaders = peridot_vertex_processing_pack::PvpShaderModules::new(
            e.graphics(),
            e.load("shaders.blit").expect("Failed to blit shader")
        ).expect("Failed to generate ShaderModules");
        let vps = shaders.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let pl = Rc::new(br::PipelineLayout::new(e.graphics(), &[&dsl], &[]).expect("Failed to create PipelineLayout"));

        let scissors = [
            br::vk::VkRect2D::from(br::Extent2D::clone(e.backbuffers()[0].size().as_ref()))
        ];
        let viewports = [
            br::Viewport::from_rect_with_depth_range(&scissors[0], 0.0 .. 1.0).into_inner()
        ];
        let pipeline = br::GraphicsPipelineBuilder::new(&pl, (&renderpass, 0), vps)
            .viewport_scissors(br::DynamicArrayState::Static(&viewports), br::DynamicArrayState::Static(&scissors))
            .multisample_state(br::MultisampleState::new().into())
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied())
            .create(e.graphics(), None)
            .expect("Failed to create GraphicsPipeline");
        let pipeline = peridot::LayoutedPipeline::combine(pipeline, &pl);

        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let mut bp_dynamic = peridot::BufferPrealloc::new(e.graphics());
        let vertex_start = bp.add(peridot::BufferContent::vertices::<peridot::VertexUV2D>(4));
        let mut bp_stg = peridot::BufferPrealloc::new(e.graphics());
        bp_stg.merge(&bp);
        let uniform_start = bp.add(peridot::BufferContent::uniform::<UniformValues>());
        let uniform_start_d = bp_dynamic.add(peridot::BufferContent::uniform::<UniformValues>());
        let buffer = bp.build_transferred().expect("Failed to build Buffer");
        let buffer_dynamic = bp_dynamic.build_upload().expect("Failed to build DynamicBuffer");
        let mut mb = peridot::MemoryBadget::new(e.graphics());
        let mut mb_dynamic = peridot::MemoryBadget::new(e.graphics());
        mb.add(buffer);
        mb_dynamic.add(buffer_dynamic);
        let buffer = mb.alloc().expect("Failed to allocate Memory for Buffer")
            .pop().expect("no objects?").unwrap_buffer();
        let buffer_dynamic = mb_dynamic.alloc_upload().expect("Failed to allocate Memory for DynamicBuffer")
            .pop().expect("no objects?").unwrap_buffer();
        
        let buffer_stg = bp_stg.build_upload().expect("Faile to build InitBuffer");
        let mut mb_stg = peridot::MemoryBadget::new(e.graphics());
        mb_stg.add(buffer_stg);
        let buffer_stg = mb_stg.alloc_upload().expect("Failed to allocate Memory for InitBuffer")
            .pop().expect("no objects?").unwrap_buffer();
        buffer_stg.guard_map(0 .. bp_stg.total_size(), |m| unsafe {
            m.slice_mut(vertex_start as _, 4).clone_from_slice(&[
                peridot::VertexUV2D {
                    pos: peridot::math::Vector2(-32.0, -32.0),
                    uv: peridot::math::Vector2(0.0, 0.0)
                },
                peridot::VertexUV2D {
                    pos: peridot::math::Vector2(32.0, -32.0),
                    uv: peridot::math::Vector2(1.0, 0.0)
                },
                peridot::VertexUV2D {
                    pos: peridot::math::Vector2(-32.0, 32.0),
                    uv: peridot::math::Vector2(0.0, 1.0)
                },
                peridot::VertexUV2D {
                    pos: peridot::math::Vector2(32.0, 32.0),
                    uv: peridot::math::Vector2(1.0, 1.0)
                }
            ]);
        }).expect("Failed to map init memory");

        e.graphics().submit_commands(|r| {
            let buffer_enter_barriers = [
                br::BufferMemoryBarrier::new(
                    &buffer,
                    vertex_start .. (vertex_start + std::mem::size_of::<peridot::VertexUV2D>() as u64 * 4),
                    0, br::AccessFlags::TRANSFER.write
                ),
                br::BufferMemoryBarrier::new(
                    &buffer_stg,
                    vertex_start .. (vertex_start + std::mem::size_of::<peridot::VertexUV2D>() as u64 * 4),
                    0, br::AccessFlags::TRANSFER.read
                )
            ];
            let buffer_end_barriers = [
                br::BufferMemoryBarrier::new(
                    &buffer,
                    vertex_start .. (vertex_start + std::mem::size_of::<peridot::VertexUV2D>() as u64 * 4),
                    br::AccessFlags::TRANSFER.write,
                    br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::UNIFORM_READ
                ),
                br::BufferMemoryBarrier::new(
                    &buffer_dynamic,
                    uniform_start_d .. (uniform_start_d + std::mem::size_of::<UniformValues>() as u64),
                    0, br::AccessFlags::HOST.write
                )
            ];

            r.pipeline_barrier(
                br::PipelineStageFlags::TOP_OF_PIPE, br::PipelineStageFlags::TRANSFER, false,
                &[], &buffer_enter_barriers, &[]
            );
            r.copy_buffer(&buffer_stg, &buffer, &[br::vk::VkBufferCopy {
                srcOffset: vertex_start, dstOffset: vertex_start,
                size: std::mem::size_of::<peridot::VertexUV2D>() as u64 * 4
            }]);
            r.pipeline_barrier(
                br::PipelineStageFlags::TRANSFER, br::PipelineStageFlags::VERTEX_INPUT.vertex_shader().host(), false,
                &[], &buffer_end_barriers, &[]
            );
        }).expect("Failed to execute init command");

        e.graphics().update_descriptor_sets(&[
            br::DescriptorSetWriteInfo(
                descriptors[0], 0, 0,
                br::DescriptorUpdateInfo::UniformBuffer(
                    vec![(
                        buffer.native_ptr(), 
                        uniform_start as usize .. (
                            uniform_start as usize + std::mem::size_of::<UniformValues>()
                        )
                    )]
                )
            )
        ], &[]);

        let main_commands = peridot::CommandBundle::new(
            e.graphics(), peridot::CBSubmissionType::Graphics, e.backbuffers().len()
        ).expect("Failed to allocate render commands");
        for (b, fb) in main_commands.iter().zip(&framebuffers) {
            let mut rec = b.begin().expect("Failed to begin recording main commands");
            rec.begin_render_pass(&renderpass, fb, scissors[0].clone(), &[br::ClearValue::Color([0.0; 4])], true);
            pipeline.bind(&mut rec);
            rec.bind_graphics_descriptor_sets(0, &descriptors[..], &[]);
            rec.bind_vertex_buffers(0, &[(&buffer, vertex_start as _)]);
            rec.draw(4, 1, 0, 0);
            rec.end_render_pass();
        }

        let &br::Extent3D(bb_width, bb_height, _) = e.backbuffers()[0].size();
        let update_data = UniformValues {
            mat: peridot::math::Camera {
                projection: peridot::math::ProjectionMethod::UI {
                design_width: bb_width as _, design_height: bb_height as _
            },
                .. Default::default()
            }.projection_matrix(),
            time: 0.0,
            offset: peridot::math::Vector2(0.0, 0.0),
            _resv: 0.0
        };
        let update_commands = peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Transfer, 1)
            .expect("Failed to allocate update commands");
        {
            let mut r = update_commands[0].begin().expect("Failed to begin recording update commands");
            let enter_barriers = [
                br::BufferMemoryBarrier::new(
                    &buffer_dynamic,
                    uniform_start_d .. (uniform_start_d + std::mem::size_of::<UniformValues>() as u64),
                    br::AccessFlags::HOST.write,
                    br::AccessFlags::TRANSFER.read
                ),
                br::BufferMemoryBarrier::new(
                    &buffer,
                    uniform_start .. (uniform_start + std::mem::size_of::<UniformValues>() as u64),
                    br::AccessFlags::UNIFORM_READ,
                    br::AccessFlags::TRANSFER.write
                )
            ];
            let leave_barriers = [
                enter_barriers[0].clone().flip(),
                enter_barriers[1].clone().flip()
            ];

            r.pipeline_barrier(
                br::PipelineStageFlags::HOST.vertex_shader(),
                br::PipelineStageFlags::TRANSFER,
                false,
                &[], &enter_barriers, &[]
            ).copy_buffer(
                &buffer_dynamic, &buffer, &[
                    br::vk::VkBufferCopy {
                        srcOffset: uniform_start_d, dstOffset: uniform_start,
                        size: std::mem::size_of::<UniformValues>() as _
                    }
                ]
            ).pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::HOST.vertex_shader(),
                false,
                &[], &leave_barriers, &[]
            );
        }

        Game {
            renderpass,
            framebuffers,
            _dsl: dsl,
            _descriptor_pool: dp,
            descriptors,
            pipeline,
            buffer,
            dynamic_buffer: buffer_dynamic,
            main_commands,
            update_data,
            update_commands,
            uniform_start_d,
            last_mouse_input: false,
            _ph: std::marker::PhantomData
        }
    }

    fn update(
        &mut self, e: &peridot::Engine<NL>, on_backbuffer_of: u32, delta_time: std::time::Duration
    ) -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        self.update_data.time += delta_time.as_secs() as f32 + delta_time.subsec_micros() as f32 / 1_000_000.0;

        let current_mouse_input = e.input().button_pressing_time(INPUT_PLANE_DOWN) > std::time::Duration::default();
        if !self.last_mouse_input && current_mouse_input {
            self.update_data.time = 0.0;
            let (ox, oy) = e.input().get_plane_position(0).unwrap_or((0.0, 0.0));
            self.update_data.offset = peridot::math::Vector2(ox, oy);
        }
        self.last_mouse_input = current_mouse_input;
        
        self.dynamic_buffer.guard_map(
            self.uniform_start_d .. (self.uniform_start_d + std::mem::size_of::<UniformValues>() as u64),
            |m| unsafe { *m.get_mut(0) = self.update_data.clone(); }
        ).expect("Failed to map dynamic buffer");
        let update_submission = Some(br::SubmissionBatch {
            command_buffers: std::borrow::Cow::Borrowed(&self.update_commands[..]),
            .. Default::default()
        });

        let render_submission = br::SubmissionBatch {
            command_buffers: std::borrow::Cow::Borrowed(
                &self.main_commands[on_backbuffer_of as usize ..= on_backbuffer_of as usize]
            ),
            .. Default::default()
        };

        (update_submission, render_submission)
    }
}
