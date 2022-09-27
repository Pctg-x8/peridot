use bedrock as br;
use bedrock::traits::*;
use br::{resources::Image, SubmissionBatch};
use br::{CommandBuffer, DescriptorPool, Device, ImageChild};
use log::*;
use peridot::math::{
    Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector2, Vector2F32, Vector3,
    Vector3F32,
};
use peridot::mthelper::{DynamicMutabilityProvider, SharedRef};
use peridot::{
    audio::StreamingPlayableWav, BufferContent, BufferPrealloc, CBSubmissionType, CommandBundle,
    DescriptorSetUpdateBatch, FixedBufferInitializer, FixedMemory, LayoutedPipeline,
    SubpassDependencyTemplates, TextureInitializationGroup, TransferBatch,
};
use peridot_vertex_processing_pack::PvpShaderModules;
use std::marker::PhantomData;
use std::mem::size_of;
use std::ops::Range;
use std::sync::{Arc, RwLock};
use std::time::Duration;

pub struct IPFixedBufferInitializer {
    vertices_offset: u64,
}
impl FixedBufferInitializer for IPFixedBufferInitializer {
    fn stage_data(&mut self, m: &br::MappedMemoryRange<impl br::DeviceMemory + ?Sized>) {
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
    buffers: FixedMemory<
        peridot::DeviceObject,
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
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

        let mut bp = BufferPrealloc::new(e.graphics());
        let vertices_offset = bp.add(BufferContent::vertex::<[UVVert; 4]>());
        let mut fm_init = IPFixedBufferInitializer { vertices_offset };

        let mut ti = TextureInitializationGroup::new(e.graphics().device().clone());
        ti.add(image_data);

        let mut bp_mut = BufferPrealloc::new(e.graphics());
        let mut_uniform_offset = bp_mut.add(BufferContent::uniform::<Uniform>());

        let mut tfb = TransferBatch::new();
        let buffers = FixedMemory::new(e.graphics(), bp, bp_mut, ti, &mut fm_init, &mut tfb)
            .expect("Alloc FixedBuffers");

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
        DynamicMutabilityProvider::borrow_mut(&*buffers.mut_buffer.0)
            .guard_map(0..buffers.mut_buffer.1, |m| unsafe {
                *m.get_mut(mut_uniform_offset as _) = Uniform {
                    camera: cam.view_projection_matrix(screen_aspect),
                    object: Matrix4::ONE,
                };
            })
            .expect("Staging MutBuffer");
        let mut tfb_mut = TransferBatch::new();
        let dst_update_range = buffers.mut_buffer_placement
            ..buffers.mut_buffer_placement + size_of::<Uniform>() as u64;
        tfb.add_copying_buffer(
            peridot::DeviceBufferView {
                buffer: buffers.mut_buffer.0.clone(),
                offset: mut_uniform_offset,
            },
            peridot::DeviceBufferView {
                buffer: buffers.buffer.0.clone(),
                offset: dst_update_range.start,
            },
            size_of::<Uniform>() as _,
        );
        tfb_mut.add_copying_buffer(
            peridot::DeviceBufferView {
                buffer: buffers.mut_buffer.0.clone(),
                offset: mut_uniform_offset,
            },
            peridot::DeviceBufferView {
                buffer: buffers.buffer.0.clone(),
                offset: dst_update_range.start,
            },
            size_of::<Uniform>() as _,
        );
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_SHADER,
            buffers.buffer.0.clone(),
            dst_update_range.clone(),
            br::AccessFlags::UNIFORM_READ,
        );
        tfb_mut.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_SHADER,
            buffers.buffer.0.clone(),
            dst_update_range,
            br::AccessFlags::UNIFORM_READ,
        );

        let preconfigure_task = e
            .submit_commands_async(|r| {
                tfb.sink_transfer_commands(r);
                tfb.sink_graphics_ready_commands(r);
            })
            .expect("Failed to submit preconfigure commands");

        let mut update_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1)
            .expect("Alloc UpdateCB");
        {
            let mut rec = unsafe { update_cb[0].begin().expect("Begin UpdateCmdRec") };
            tfb_mut.sink_transfer_commands(&mut rec);
            tfb_mut.sink_graphics_ready_commands(&mut rec);
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
        let mut dsub = DescriptorSetUpdateBatch::new();
        dsub.write(
            descriptor_main[0],
            0,
            br::DescriptorUpdateInfo::UniformBuffer(vec![(
                buffers.buffer.0.native_ptr(),
                buffers.range_in_mut_buffer(0..std::mem::size_of::<Uniform>()),
            )]),
        );
        dsub.write(
            descriptor_main[0],
            1,
            br::DescriptorUpdateInfo::CombinedImageSampler(vec![(
                None,
                buffers.textures[0].native_ptr(),
                br::ImageLayout::ShaderReadOnlyOpt,
            )]),
        );
        dsub.submit(e.graphics().device());

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
            offset: br::vk::VkOffset2D::default(),
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
            cr.begin_render_pass(
                &renderpass,
                fb,
                fb.size()
                    .clone()
                    .into_rect(br::vk::VkOffset2D { x: 0, y: 0 }),
                &[br::ClearValue::color([0.0; 4])],
                true,
            );
            gp.bind(&mut cr);
            cr.bind_graphics_descriptor_sets(
                0,
                unsafe { std::mem::transmute(&descriptor_main[..]) },
                &[],
            );
            cr.bind_vertex_buffers(0, &[(&buffers.buffer.0, vertices_offset as _)]);
            cr.draw(4, 1, 0, 0);
            cr.end_render_pass();
        }

        async_std::task::block_on(preconfigure_task).expect("Failed to preconfigure resources");

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
            buffers,
            update_cb,
            mut_uniform_offset,
            ph: PhantomData,
        }
    }

    fn update(&mut self, e: &mut peridot::Engine<PL>, on_backbuffer_of: u32, delta_time: Duration) {
        let dtsec = delta_time.as_secs() as f32 + delta_time.subsec_micros() as f32 / 1000_0000.0;
        self.rot += dtsec * 15.0;
        let (mut_uniform_offset, rot) = (self.mut_uniform_offset, self.rot);
        DynamicMutabilityProvider::borrow_mut(&*self.buffers.mut_buffer.0)
            .guard_map(
                0..self.mut_uniform_offset + size_of::<Uniform>() as u64,
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
            cr.begin_render_pass(
                &self.renderpass,
                fb,
                fb.size()
                    .clone()
                    .into_rect(br::vk::VkOffset2D { x: 0, y: 0 }),
                &[br::ClearValue::color([0.0; 4])],
                true,
            );
            self.gp_main.bind(&mut cr);
            cr.bind_graphics_descriptor_sets(
                0,
                unsafe { std::mem::transmute(&self.descriptor.2[..]) },
                &[],
            );
            cr.bind_vertex_buffers(0, &[(&self.buffers.buffer.0, self.vertices_offset as _)]);
            cr.draw(4, 1, 0, 0);
            cr.end_render_pass();
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
