use std::convert::TryInto;
use std::mem::{align_of, size_of};

use bedrock as br;
use bedrock::VkHandle;
use br::{CommandBuffer, DescriptorPool, Device, Image, ImageChild, SubmissionBatch};
use peridot::mthelper::SharedRef;
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

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct RangeBuilder<T>(T);
impl<T> RangeBuilder<T> {
    #[inline]
    pub const fn from(base: T) -> Self {
        Self(base)
    }

    #[inline]
    pub fn to(self, to: T) -> std::ops::Range<T> {
        self.0..to
    }

    #[inline]
    pub fn length(self, length: T) -> std::ops::Range<T>
    where
        T: std::ops::Add<T, Output = T> + Copy,
    {
        self.0..(self.0 + length)
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
    buffer: peridot::Buffer<
        br::BufferObject<peridot::DeviceObject>,
        br::DeviceMemoryObject<peridot::DeviceObject>,
    >,
    dynamic_buffer: peridot::Buffer<
        br::BufferObject<peridot::DeviceObject>,
        br::DeviceMemoryObject<peridot::DeviceObject>,
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
        let sprite_plane = peridot::Primitive::uv_plane_centric(32.0);

        let mut bp_dynamic = peridot::BufferPrealloc::new(e.graphics());
        let uniform_start_d = bp_dynamic.add(peridot::BufferContent::uniform::<UniformValues>());
        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let vertex_start = sprite_plane.prealloc(&mut bp);
        let mut bp_stg = bp.clone();
        let dynamic_start = bp.merge(&bp_dynamic);
        let main_image_bytes_start = bp_stg.add(peridot::BufferContent::Raw(
            main_image_data.0.u8_pixels().len() as _,
            align_of::<u32>() as _,
        ));

        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp.build_transferred()
                .expect("Failed to create main buffer"),
        ));
        mb.add(peridot::MemoryBadgetEntry::Image(
            br::ImageDesc::new(
                &main_image_data.0.size,
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

        let dynamic_buffer_range = 0..bp_dynamic.total_size();
        let buffer_fullrange = 0..bp.total_size();
        let staging_buffer_fullrange = 0..bp_stg.total_size();
        e.submit_commands(|mut r| {
            let _ = r
                .pipeline_barrier(
                    br::PipelineStageFlags::ALL_COMMANDS,
                    br::PipelineStageFlags::TRANSFER,
                    true,
                    &[],
                    &[
                        br::BufferMemoryBarrier::new(
                            &buffer,
                            buffer_fullrange,
                            0,
                            br::AccessFlags::TRANSFER.write,
                        ),
                        br::BufferMemoryBarrier::new(
                            &stg_buffer,
                            staging_buffer_fullrange,
                            br::AccessFlags::HOST.write,
                            br::AccessFlags::TRANSFER.read,
                        ),
                    ],
                    &[br::ImageMemoryBarrier::new(
                        &main_image,
                        br::ImageSubresourceRange::color(0..1, 0..1),
                        br::ImageLayout::Preinitialized,
                        br::ImageLayout::TransferDestOpt,
                    )],
                )
                .copy_buffer(
                    &stg_buffer,
                    &buffer,
                    &[br::vk::VkBufferCopy {
                        srcOffset: 0,
                        dstOffset: 0,
                        size: dynamic_start,
                    }],
                )
                .copy_buffer_to_image(
                    &stg_buffer,
                    &main_image,
                    br::ImageLayout::TransferDestOpt,
                    &[br::vk::VkBufferImageCopy {
                        bufferOffset: main_image_bytes_start,
                        bufferRowLength: (main_image_data.0.stride
                            / (main_image_data.0.format.bpp() >> 3))
                            as _,
                        bufferImageHeight: 0,
                        imageSubresource: br::vk::VkImageSubresourceLayers {
                            aspectMask: br::vk::VK_IMAGE_ASPECT_COLOR_BIT,
                            mipLevel: 0,
                            baseArrayLayer: 0,
                            layerCount: 1,
                        },
                        imageOffset: br::vk::VkOffset3D { x: 0, y: 0, z: 0 },
                        imageExtent: br::vk::VkExtent3D {
                            width: main_image_data.0.size.0,
                            height: main_image_data.0.size.1,
                            depth: 1,
                        },
                    }],
                )
                .pipeline_barrier(
                    br::PipelineStageFlags::TRANSFER,
                    br::PipelineStageFlags::VERTEX_INPUT
                        .fragment_shader()
                        .host(),
                    true,
                    &[],
                    &[
                        br::BufferMemoryBarrier::new(
                            &buffer,
                            vertex_start..dynamic_start,
                            br::AccessFlags::TRANSFER.write,
                            br::AccessFlags::VERTEX_ATTRIBUTE_READ,
                        ),
                        br::BufferMemoryBarrier::new(
                            &dynamic_buffer,
                            dynamic_buffer_range,
                            0,
                            br::AccessFlags::HOST.write,
                        ),
                    ],
                    &[br::ImageMemoryBarrier::new(
                        &main_image,
                        br::ImageSubresourceRange::color(0..1, 0..1),
                        br::ImageLayout::TransferDestOpt,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )],
                );
            r
        })
        .expect("Failed to execute init command");

        let main_image_view = main_image
            .create_view(
                None,
                None,
                &br::ComponentMapping::default(),
                &br::ImageSubresourceRange::color(0..1, 0..1),
            )
            .expect("Failed to create main image view");

        let mut dsub = peridot::DescriptorSetUpdateBatch::new();
        dsub.write(
            descriptors[0],
            0,
            br::DescriptorUpdateInfo::UniformBuffer(vec![(
                buffer.native_ptr(),
                RangeBuilder::from((dynamic_start + uniform_start_d) as _)
                    .length(size_of::<UniformValues>()),
            )]),
        );
        dsub.write(
            descriptors[1],
            0,
            br::DescriptorUpdateInfo::CombinedImageSampler(vec![(
                None,
                main_image_view.native_ptr(),
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
            let _ = rec.begin_render_pass(
                &renderpass,
                fb,
                scissors[0].clone(),
                &[br::ClearValue::color([0.0; 4])],
                true,
            );
            pipeline.bind(&mut rec);
            let _ = rec
                .bind_graphics_descriptor_sets(
                    0,
                    unsafe { std::mem::transmute(&descriptors[..]) },
                    &[],
                )
                .bind_vertex_buffers(0, &[(&buffer, vertex_start as _)])
                .draw(4, 1, 0, 0)
                .end_render_pass();
            rec.end().expect("Failed to record commands");
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
                    &dynamic_buffer,
                    RangeBuilder::from(uniform_start_d).length(size_of::<UniformValues>() as u64),
                    br::AccessFlags::HOST.write,
                    br::AccessFlags::TRANSFER.read,
                ),
                br::BufferMemoryBarrier::new(
                    &buffer,
                    RangeBuilder::from(dynamic_start + uniform_start_d).length(size_of::<
                        UniformValues,
                    >(
                    )
                        as u64),
                    br::AccessFlags::UNIFORM_READ,
                    br::AccessFlags::TRANSFER.write,
                ),
            ];
            let leave_barriers = [
                enter_barriers[0].clone().flip(),
                enter_barriers[1].clone().flip(),
            ];

            let _ = r
                .pipeline_barrier(
                    br::PipelineStageFlags::HOST.vertex_shader(),
                    br::PipelineStageFlags::TRANSFER,
                    false,
                    &[],
                    &enter_barriers,
                    &[],
                )
                .copy_buffer(
                    &dynamic_buffer,
                    &buffer,
                    &[br::vk::VkBufferCopy {
                        srcOffset: uniform_start_d,
                        dstOffset: dynamic_start + uniform_start_d,
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
            r.end().expect("Failed to record commands");
        }

        Self {
            renderpass,
            framebuffers,
            _smp: smp,
            _dsl: dsl,
            _dsl2: dsl2,
            _descriptor_pool: dp,
            descriptors,
            pipeline,
            buffer,
            dynamic_buffer,
            main_commands,
            _main_image_view: main_image_view,
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
        self.dynamic_buffer
            .guard_map(
                RangeBuilder::from(self.uniform_start_d).length(size_of::<UniformValues>() as u64),
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
