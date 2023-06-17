use bedrock as br;
use bedrock::traits::*;
use br::{resources::Image, SubmissionBatch};
use br::{CommandBuffer, DescriptorPool, Device, ImageChild, VulkanStructure};
use log::*;
use peridot::align2;
use peridot::math::{
    Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector2, Vector2F32, Vector3,
    Vector3F32,
};
use peridot::mthelper::{make_shared_mutable_ref, DynamicMutabilityProvider, SharedRef};
use peridot::{
    audio::StreamingPlayableWav, BufferContent, BufferPrealloc, CBSubmissionType, CommandBundle,
    DescriptorSetUpdateBatch, FixedBufferInitializer, FixedMemory, LayoutedPipeline,
    SubpassDependencyTemplates, TextureInitializationGroup, TransferBatch,
};
use peridot_vertex_processing_pack::PvpShaderModules;
use std::convert::TryInto;
use std::marker::PhantomData;
use std::mem::{align_of, size_of};
use std::ops::Range;
use std::sync::{Arc, RwLock};
use std::time::Duration;

pub struct IPFixedBufferInitializer {
    vertices_offset: u64,
}
impl FixedBufferInitializer for IPFixedBufferInitializer {
    fn stage_data(
        &mut self,
        m: &br::MappedMemoryRange<impl br::DeviceMemory + br::VkHandleMut + ?Sized>,
    ) {
        unsafe {
            m.slice_mut(self.vertices_offset as _, 4)
                .clone_from_slice(&[
                    UVVert {
                        pos: Vector3(-1.0, 1.0, 0.0),
                        uv: Vector2(0.0, 0.0),
                    },
                    UVVert {
                        pos: Vector3(1.0, 1.0, 0.0),
                        uv: Vector2(1.0, 0.0),
                    },
                    UVVert {
                        pos: Vector3(-1.0, -1.0, 0.0),
                        uv: Vector2(0.0, 1.0),
                    },
                    UVVert {
                        pos: Vector3(1.0, -1.0, 0.0),
                        uv: Vector2(1.0, 1.0),
                    },
                ]);
        }
    }

    fn buffer_graphics_ready<Device: br::Device + 'static>(
        &self,
        tfb: &mut TransferBatch,
        buffer: &SharedRef<
            peridot::Buffer<
                impl br::Buffer<ConcreteDevice = Device> + 'static,
                impl br::DeviceMemory<ConcreteDevice = Device> + 'static,
            >,
        >,
        buffer_range: Range<u64>,
    ) {
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT.vertex_shader(),
            buffer.clone(),
            buffer_range.start as _..buffer_range.end as _,
            br::AccessFlags::UNIFORM_READ | br::AccessFlags::VERTEX_ATTRIBUTE_READ,
        );
    }
}

fn range_from_length<N: std::ops::Add<N, Output = N> + Copy>(
    start: N,
    length: N,
) -> std::ops::Range<N> {
    start..start + length
}

fn reverse_buffer_barriers(barriers: &[br::BufferMemoryBarrier]) -> Vec<br::BufferMemoryBarrier> {
    barriers
        .iter()
        .cloned()
        .map(br::BufferMemoryBarrier::flip)
        .rev()
        .collect()
}

pub struct Game<PL: peridot::NativeLinker> {
    ph: PhantomData<*const PL>,
    rot: f32,
    render_cb: peridot::CommandBundle<peridot::DeviceObject>,
    update_cb: peridot::CommandBundle<peridot::DeviceObject>,
    renderpass: br::RenderPassObject<peridot::DeviceObject>,
    framebuffers: Vec<
        br::FramebufferObject<
            peridot::DeviceObject,
            SharedRef<<PL::Presenter as peridot::PlatformPresenter>::Backbuffer>,
        >,
    >,
    gp_main: LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        br::PipelineLayoutObject<peridot::DeviceObject>,
    >,
    descriptor: (
        br::DescriptorSetLayoutObject<peridot::DeviceObject>,
        br::DescriptorPoolObject<peridot::DeviceObject>,
        Vec<br::DescriptorSet>,
    ),
    _sampler: br::SamplerObject<peridot::DeviceObject>,
    vertices_offset: u64,
    buffer: peridot::Buffer<
        br::BufferObject<peridot::DeviceObject>,
        br::DeviceMemoryObject<peridot::DeviceObject>,
    >,
    mutable_buffer: peridot::Buffer<
        br::BufferObject<peridot::DeviceObject>,
        br::DeviceMemoryObject<peridot::DeviceObject>,
    >,
    _image_view: br::ImageViewObject<
        peridot::Image<
            br::ImageObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    mut_uniform_offset: u64,
}
impl<PL: peridot::NativeLinker> peridot::FeatureRequests for Game<PL> {}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &mut peridot::Engine<PL>) -> Self {
        let screen_size = e
            .backbuffer(0)
            .expect("no backbuffers")
            .image()
            .size()
            .clone();
        let screen_aspect = screen_size.width as f32 / screen_size.height as f32;

        let image_data: peridot_image::PNG = e.load("images.example").expect("No image found");
        debug!("Image: {}x{}", image_data.0.size.x(), image_data.0.size.y());
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

        let plane_mesh = peridot::Primitive::uv_plane_centric_xy(1.0, 0.0);
        let mut cam = Camera {
            projection: Some(ProjectionMethod::Perspective {
                fov: 75.0f32.to_radians(),
            }),
            position: Vector3(-4.0, -1.0, -3.0),
            rotation: Quaternion::new(45.0f32.to_radians(), Vector3::UP),
            // position: Vector3(0.0, 0.0, -3.0), rotation: Quaternion::ONE,
            depth_range: 1.0..10.0,
        };
        cam.look_at(Vector3(0.0, 0.0, 0.0));

        let mut bp = BufferPrealloc::new(e.graphics());
        let vertices_offset = bp.add(BufferContent::vertices::<peridot::VertexUV3D>(
            plane_mesh.vertices.len(),
        ));

        let mut bp_stg = bp.clone();
        let copy_buffer_data_length = bp_stg.total_size();
        let staging_image_offset = bp_stg.add(BufferContent::Raw(
            image_data.0.u8_pixels().len() as _,
            align_of::<u32>() as _,
        ));

        let mut bp_mut = BufferPrealloc::new(e.graphics());
        let mut_uniform_offset = bp_mut.add(BufferContent::uniform::<Uniform>());
        let mutable_data_offset = bp.merge(&bp_mut);

        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp.build().expect("Failed to create buffer"),
        ));
        mb.add(peridot::MemoryBadgetEntry::Image(
            br::ImageDesc::new(
                &image_data.0.size,
                image_data.0.format as _,
                br::ImageUsage::SAMPLED.transfer_dest(),
                br::ImageLayout::Preinitialized,
            )
            .create(e.graphics().device().clone())
            .expect("Failed to create main image object"),
        ));
        let Ok::<[_; 2], _>([peridot::MemoryBoundResource::Buffer(buffer), peridot::MemoryBoundResource::Image(image)]) = mb.alloc().expect("Failed to allocate memory").try_into() else {
            unreachable!("invalid return combination");
        };
        let mut mb =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp_mut
                .build_upload()
                .expect("Failed to create mutable data buffer"),
        ));
        let Ok::<[_; 1], _>([peridot::MemoryBoundResource::Buffer(mut mut_buffer)]) = mb.alloc_upload().expect("Failed to allocate mutable data memory").try_into() else {
            unreachable!("invalid return combination");
        };

        let (mut buffer_staging, stg_requires_flushing) = {
            let mut mb = peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(
                e.graphics(),
            );
            mb.add(peridot::MemoryBadgetEntry::Buffer(
                bp_stg
                    .build_upload()
                    .expect("Failed to create staging buffer"),
            ));
            let Ok::<[_; 1], _>([peridot::MemoryBoundResource::Buffer(b)]) = mb.alloc_upload().expect("Failed to allocate staging memory").try_into() else {
                unreachable!("invalid return combination")
            };

            // TODO: requires flushing?
            (b, false)
        };

        buffer_staging
            .guard_map(0..bp_stg.total_size(), |r| unsafe {
                r.clone_from_slice_at(vertices_offset as _, &plane_mesh.vertices);
                r.clone_from_slice_at(staging_image_offset as _, image_data.0.u8_pixels());
            })
            .expect("Failed to setup staging data");
        mut_buffer
            .guard_map(0..bp_mut.total_size(), |r| unsafe {
                *r.get_mut(mut_uniform_offset as _) = Uniform {
                    camera: cam.view_projection_matrix(screen_aspect),
                    object: Matrix4::ONE,
                };
            })
            .expect("Failed to setup mutable data");

        let preconfigure_task = e
            .submit_commands_async(|mut r| {
                let _ = r.pipeline_barrier(
                    br::PipelineStageFlags::ALL_COMMANDS,
                    br::PipelineStageFlags::TRANSFER,
                    true,
                    &[],
                    &[
                        br::BufferMemoryBarrier::new(
                            &buffer,
                            0..bp.total_size(),
                            br::AccessFlags::MEMORY.read,
                            br::AccessFlags::TRANSFER.write,
                        ),
                        br::BufferMemoryBarrier::new(
                            &mut_buffer,
                            range_from_length(mut_uniform_offset, size_of::<Uniform>() as _),
                            br::AccessFlags::HOST.write,
                            br::AccessFlags::TRANSFER.read,
                        ),
                        br::BufferMemoryBarrier::new(
                            &buffer_staging,
                            0..bp_stg.total_size(),
                            br::AccessFlags::HOST.write,
                            br::AccessFlags::TRANSFER.read,
                        ),
                    ],
                    &[br::ImageMemoryBarrier::new(
                        &image,
                        br::ImageSubresourceRange::color(0..1, 0..1),
                        br::ImageLayout::Preinitialized,
                        br::ImageLayout::TransferDestOpt,
                    )],
                )
                .copy_buffer(
                    &buffer_staging,
                    &buffer,
                    &[br::vk::VkBufferCopy {
                        srcOffset: 0,
                        dstOffset: 0,
                        size: copy_buffer_data_length,
                    }],
                )
                .copy_buffer(
                    &mut_buffer,
                    &buffer,
                    &[br::vk::VkBufferCopy {
                        srcOffset: mut_uniform_offset,
                        dstOffset: mutable_data_offset,
                        size: size_of::<Uniform>() as _,
                    }],
                )
                .copy_buffer_to_image(
                    &buffer_staging,
                    &image,
                    br::ImageLayout::TransferDestOpt,
                    &[br::vk::VkBufferImageCopy {
                        bufferOffset: staging_image_offset,
                        bufferRowLength: image_data.0.stride as _,
                        bufferImageHeight: image_data.0.size.1,
                        imageSubresource: br::vk::VkImageSubresourceLayers {
                            aspectMask: br::vk::VK_IMAGE_ASPECT_COLOR_BIT,
                            mipLevel: 0,
                            baseArrayLayer: 0,
                            layerCount: 1,
                        },
                        imageOffset: br::vk::VkOffset3D { x: 0, y: 0, z: 0 },
                        imageExtent: br::vk::VkExtent3D {
                            width: image_data.0.size.0,
                            height: image_data.0.size.1,
                            depth: 1,
                        },
                    }],
                )
                .pipeline_barrier(
                    br::PipelineStageFlags::TRANSFER,
                    br::PipelineStageFlags::VERTEX_SHADER
                        .fragment_shader()
                        .host()
                        .vertex_input(),
                    true,
                    &[],
                    &[
                        br::BufferMemoryBarrier::new(
                            &buffer,
                            range_from_length(
                                0,
                                (size_of::<peridot::VertexUV3D>() * plane_mesh.vertices.len()) as _,
                            ),
                            br::AccessFlags::TRANSFER.write,
                            br::AccessFlags::VERTEX_ATTRIBUTE_READ,
                        ),
                        br::BufferMemoryBarrier::new(
                            &buffer,
                            range_from_length(mutable_data_offset, size_of::<Uniform>() as _),
                            br::AccessFlags::TRANSFER.write,
                            br::AccessFlags::UNIFORM_READ,
                        ),
                        br::BufferMemoryBarrier::new(
                            &mut_buffer,
                            range_from_length(mut_uniform_offset, size_of::<Uniform>() as _),
                            br::AccessFlags::TRANSFER.read,
                            br::AccessFlags::HOST.write,
                        ),
                    ],
                    &[br::ImageMemoryBarrier::new(
                        &image,
                        br::ImageSubresourceRange::color(0..1, 0..1),
                        br::ImageLayout::TransferDestOpt,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )],
                );
                r
            })
            .expect("Failed to submit preconfigure commands");

        let mut update_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1)
            .expect("Alloc UpdateCB");
        {
            let mut rec = unsafe { update_cb[0].begin().expect("Begin UpdateCmdRec") };
            let enter_buffer_barriers = &[
                br::BufferMemoryBarrier::new(
                    &mut_buffer,
                    range_from_length(mut_uniform_offset, size_of::<Uniform>() as _),
                    br::AccessFlags::HOST.write,
                    br::AccessFlags::TRANSFER.read,
                ),
                br::BufferMemoryBarrier::new(
                    &buffer,
                    range_from_length(mutable_data_offset, size_of::<Uniform>() as _),
                    br::AccessFlags::UNIFORM_READ,
                    br::AccessFlags::TRANSFER.write,
                ),
            ];

            let _ = rec.pipeline_barrier(
                // TODO: use excluding bits for less stalling
                br::PipelineStageFlags::VERTEX_SHADER,
                br::PipelineStageFlags::TRANSFER,
                true,
                &[],
                enter_buffer_barriers,
                &[],
            )
            .copy_buffer(
                &mut_buffer,
                &buffer,
                &[br::vk::VkBufferCopy {
                    srcOffset: mut_uniform_offset,
                    dstOffset: mutable_data_offset,
                    size: size_of::<Uniform>() as _,
                }],
            )
            .pipeline_barrier(
                br::PipelineStageFlags::TRANSFER,
                br::PipelineStageFlags::VERTEX_SHADER,
                true,
                &[],
                &reverse_buffer_barriers(enter_buffer_barriers),
                &[],
            );
            rec.end().expect("Failed to record update commands");
        }

        let outer_layout = e.requesting_backbuffer_layout().0;
        let attdesc =
            br::AttachmentDescription::new(e.backbuffer_format(), outer_layout, outer_layout)
                .load_op(br::LoadOp::Clear)
                .store_op(br::StoreOp::Store);
        let renderpass = br::RenderPassBuilder::new()
            .add_attachment(attdesc)
            .add_subpass(br::SubpassDescription::new().add_color_output(
                0,
                br::ImageLayout::ColorAttachmentOpt,
                None,
            ))
            .add_dependency(SubpassDependencyTemplates::to_color_attachment_in(
                None, 0, true,
            ))
            .create(e.graphics().device().clone())
            .expect("Create RenderPass");
        let framebuffers = e
            .iter_backbuffers()
            .map(|b| {
                e.graphics().device().clone().new_framebuffer(
                    &renderpass,
                    vec![b.clone()],
                    b.image().size().as_ref(),
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Bind Framebuffer");

        let smp = br::SamplerBuilder::default()
            .create(e.graphics().device().clone())
            .expect("Creating Sampler");
        let descriptor_layout = e
            .graphics()
            .device()
            .clone()
            .new_descriptor_set_layout(&[
                br::DescriptorSetLayoutBinding::UniformBuffer(1, br::ShaderStage::VERTEX),
                br::DescriptorSetLayoutBinding::CombinedImageSampler(
                    1,
                    br::ShaderStage::FRAGMENT,
                    &[smp.native_ptr()],
                ),
            ])
            .expect("Create DescriptorSetLayout");
        let mut descriptor_pool = e
            .graphics()
            .device()
            .clone()
            .new_descriptor_pool(
                1,
                &[
                    br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1),
                    br::DescriptorPoolSize(br::DescriptorType::CombinedImageSampler, 1),
                ],
                false,
            )
            .expect("Create DescriptorPool");
        let descriptor_main = descriptor_pool
            .alloc(&[&descriptor_layout])
            .expect("Create main Descriptor");

        let shaderfile = e
            .load("builtin.shaders.unlit_image")
            .expect("Loading shader");
        let shader =
            PvpShaderModules::new(e.graphics().device(), shaderfile).expect("Create ShaderModules");
        let vp = [br::vk::VkViewport {
            width: screen_size.width as _,
            height: screen_size.height as _,
            x: 0.0,
            y: 0.0,
            minDepth: 0.0,
            maxDepth: 1.0,
        }];
        let sc = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D { x: 0, y: 0 },
            extent: br::vk::VkExtent2D {
                width: screen_size.width,
                height: screen_size.height,
            },
        }];
        let pl = e
            .graphics()
            .device()
            .clone()
            .new_pipeline_layout(&[&descriptor_layout], &[])
            .expect("Create PipelineLayout");
        let vps = shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let gp = br::GraphicsPipelineBuilder::<
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
            br::DynamicArrayState::Static(&vp),
            br::DynamicArrayState::Static(&sc),
        )
        .multisample_state(br::MultisampleState::new().into())
        .add_attachment_blend(br::AttachmentColorBlendState::noblend())
        .create(
            e.graphics().device().clone(),
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Create GraphicsPipeline");
        #[cfg(feature = "debug")]
        gp.set_name(Some(
            &std::ffi::CString::new("Main Pipeline").expect("invalid sequence?"),
        ))
        .expect("Failed to set pipeline name");
        let gp = LayoutedPipeline::combine(gp, pl);

        let mut render_cb = CommandBundle::new(
            e.graphics(),
            CBSubmissionType::Graphics,
            e.backbuffer_count(),
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
            .apply(e.graphics())
            .expect("Failed to set render cb name");
            let mut cr = unsafe { cb.begin().expect("Begin CmdRecord") };
            let _ = cr.begin_render_pass(
                &renderpass,
                fb,
                fb.size()
                    .clone()
                    .into_rect(br::vk::VkOffset2D { x: 0, y: 0 }),
                &[br::ClearValue::color([0.0; 4])],
                true,
            );
            gp.bind(&mut cr);
            let _ = cr
                .bind_graphics_descriptor_sets(
                    0,
                    unsafe { std::mem::transmute(&descriptor_main[..]) },
                    &[],
                )
                .bind_vertex_buffers(0, &[(&buffer, vertices_offset as _)])
                .draw(4, 1, 0, 0)
                .end_render_pass();
            cr.end().expect("Failed to record render commands");
        }

        async_std::task::block_on(preconfigure_task).expect("Failed to preconfigure resources");

        let image_view = image
            .create_view(
                None,
                None,
                &br::ComponentMapping::default(),
                &br::ImageSubresourceRange::color(0..1, 0..1),
            )
            .expect("Failed to create main image view");
        let mut dsub = DescriptorSetUpdateBatch::new();
        dsub.write(
            descriptor_main[0],
            0,
            br::DescriptorUpdateInfo::UniformBuffer(vec![(
                buffer.native_ptr(),
                range_from_length(mutable_data_offset as _, std::mem::size_of::<Uniform>()),
            )]),
        );
        dsub.write(
            descriptor_main[0],
            1,
            br::DescriptorUpdateInfo::CombinedImageSampler(vec![(
                None,
                image_view.native_ptr(),
                br::ImageLayout::ShaderReadOnlyOpt,
            )]),
        );
        dsub.submit(e.graphics().device());

        bgm.write().expect("Starting BGM").play();

        Game {
            render_cb,
            renderpass,
            framebuffers,
            descriptor: (descriptor_layout, descriptor_pool, descriptor_main),
            gp_main: gp,
            rot: 0.0,
            vertices_offset,
            _sampler: smp,
            buffer,
            mutable_buffer: mut_buffer,
            _image_view: image_view,
            update_cb,
            mut_uniform_offset,
            ph: PhantomData,
        }
    }

    fn update(&mut self, e: &mut peridot::Engine<PL>, on_backbuffer_of: u32, delta_time: Duration) {
        let dtsec = delta_time.as_secs() as f32 + delta_time.subsec_micros() as f32 / 1000_0000.0;
        self.rot += dtsec * 15.0;
        let (mut_uniform_offset, rot) = (self.mut_uniform_offset, self.rot);
        self.mutable_buffer
            .guard_map(
                0..mut_uniform_offset + size_of::<Uniform>() as u64,
                |m| unsafe {
                    m.get_mut::<Uniform>(mut_uniform_offset as _).object =
                        Quaternion::new(rot, Vector3F32::UP).into();
                },
            )
            .expect("Update DynamicStgBuffer");

        e.do_render(
            on_backbuffer_of,
            Some(br::EmptySubmissionBatch.with_command_buffers(&self.update_cb)),
            br::EmptySubmissionBatch.with_command_buffers(
                &self.render_cb[on_backbuffer_of as usize..on_backbuffer_of as usize + 1],
            ),
        )
        .expect("Falied to present");
    }

    fn discard_backbuffer_resources(&mut self) {
        self.framebuffers.clear();
        self.render_cb.reset().expect("Resetting RenderCB");
    }
    fn on_resize(&mut self, e: &mut peridot::Engine<PL>, _new_size: Vector2<usize>) {
        self.framebuffers = e
            .iter_backbuffers()
            .map(|b| {
                e.graphics().device().clone().new_framebuffer(
                    &self.renderpass,
                    vec![b.clone()],
                    b.image().size().as_ref(),
                    1,
                )
            })
            .collect::<Result<Vec<_>, _>>()
            .expect("Bind Framebuffers");
        self.populate_render_commands();
    }
}
impl<PL: peridot::NativeLinker> Game<PL> {
    fn populate_render_commands(&mut self) {
        for (cb, fb) in self.render_cb.iter_mut().zip(&self.framebuffers) {
            let mut cr = unsafe { cb.begin().expect("Begin CmdRecord") };
            let _ = cr.begin_render_pass(
                &self.renderpass,
                fb,
                fb.size()
                    .clone()
                    .into_rect(br::vk::VkOffset2D { x: 0, y: 0 }),
                &[br::ClearValue::color([0.0; 4])],
                true,
            );
            self.gp_main.bind(&mut cr);
            let _ = cr
                .bind_graphics_descriptor_sets(
                    0,
                    unsafe { std::mem::transmute(&self.descriptor.2[..]) },
                    &[],
                )
                .bind_vertex_buffers(0, &[(&self.buffer, self.vertices_offset as _)])
                .draw(4, 1, 0, 0)
                .end_render_pass();
            cr.end().expect("Failed to record render commands");
        }
    }
}

#[derive(Clone)]
#[repr(C, align(4))]
struct UVVert {
    pos: Vector3F32,
    uv: Vector2F32,
}

#[repr(C)]
struct Uniform {
    camera: Matrix4F32,
    object: Matrix4F32,
}
