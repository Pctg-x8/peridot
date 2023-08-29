use bedrock as br;
use br::{resources::Image, SubmissionBatch};
use br::{CommandBuffer, DescriptorPool, Device, ImageChild, ImageSubresourceSlice};
use log::*;
use peridot::math::{
    Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector2, Vector3,
};
use peridot::mthelper::{DynamicMutabilityProvider, SharedRef};
use peridot::{
    audio::StreamingPlayableWav, BufferContent, BufferPrealloc, CBSubmissionType, CommandBundle,
    LayoutedPipeline, SubpassDependencyTemplates,
};
use peridot_memory_manager::MemoryManager;
use peridot_vertex_processing_pack::PvpShaderModules;
use std::convert::TryInto;
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};
use std::time::Duration;

#[cfg(feature = "debug")]
use br::VkObject;

use peridot_command_object::{
    BeginRenderPass, BufferImageDataDesc, BufferUsage, ColorAttachmentBlending, CopyBuffer,
    CopyBufferToImage, DescriptorSets, EndRenderPass, GraphicsCommand, GraphicsCommandCombiner,
    ImageResourceRange, PipelineBarrier, RangedBuffer, RangedImage, StandardMesh,
};

struct BufferOffsets {
    pub plane_vertices: u64,
}
struct MutableBufferOffsets {
    pub uniform: u64,
}
struct StagingBufferOffsets {
    pub image: u64,
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
            SharedRef<<PL::Presenter as peridot::PlatformPresenter>::BackBuffer>,
        >,
    >,
    descriptor: (
        br::DescriptorSetLayoutObject<peridot::DeviceObject>,
        br::DescriptorPoolObject<peridot::DeviceObject>,
        Vec<br::DescriptorSet>,
    ),
    _sampler: br::SamplerObject<peridot::DeviceObject>,
    color_renders: Box<dyn GraphicsCommand>,
    mutable_uniform_buffer: RangedBuffer<peridot_memory_manager::Buffer>,
    _uniform_buffer: RangedBuffer<peridot_memory_manager::Buffer>,
    _image_view: br::ImageViewObject<peridot_memory_manager::Image>,
}
impl<PL: peridot::NativeLinker> peridot::FeatureRequests for Game<PL> {}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &mut peridot::Engine<PL>) -> Self {
        let screen_size = e
            .back_buffer(0)
            .expect("no back buffers")
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
        cam.look_at(Vector3(0.0, 0.0, 0.0));

        let vertex_buffer: RangedBuffer<_> = memory_manager
            .allocate_device_local_buffer(
                e.graphics(),
                br::BufferDesc::new(
                    plane_mesh.byte_length(),
                    br::BufferUsage::VERTEX_BUFFER.transfer_dest(),
                ),
            )
            .expect("Failed to allocate vertex buffer")
            .into();
        #[cfg(feature = "debug")]
        vertex_buffer
            .0
            .set_name(Some(unsafe {
                core::ffi::CStr::from_bytes_with_nul_unchecked(b"Vertex Buffer\0")
            }))
            .expect("Failed to set object name");
        let mut vertex_buffer_stg: RangedBuffer<_> = memory_manager
            .allocate_upload_buffer(
                e.graphics(),
                br::BufferDesc::new(plane_mesh.byte_length(), br::BufferUsage::TRANSFER_SRC),
            )
            .expect("Failed to allocate staging vertex buffer")
            .into();
        vertex_buffer_stg
            .0
            .clone_content_from_slice(&plane_mesh.vertices)
            .expect("Failed to set upload content");

        let uniform_buffer: RangedBuffer<_> = memory_manager
            .allocate_device_local_buffer(
                e.graphics(),
                br::BufferDesc::new(
                    core::mem::size_of::<Uniform>(),
                    br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
                ),
            )
            .expect("Failed to allocate uniform buffer")
            .into();
        let mut uniform_mut_buffer: RangedBuffer<_> = memory_manager
            .allocate_upload_buffer(
                e.graphics(),
                br::BufferDesc::new(
                    core::mem::size_of::<Uniform>(),
                    br::BufferUsage::TRANSFER_SRC,
                ),
            )
            .expect("Failed to allocate mutable uniform buffer")
            .into();
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
                br::ImageDesc::new(
                    image_data.0.size,
                    image_data.0.format as _,
                    br::ImageUsage::SAMPLED.transfer_dest(),
                    br::ImageLayout::Preinitialized,
                ),
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
                let uniform_mut_buffer_ref = uniform_mut_buffer.make_ref();
                let image_data_stg_buffer_ranged = RangedBuffer::from(&image_data_stg_buffer.inner);

                let [mut_uniform_in_barrier, mut_uniform_out_barrier] = uniform_mut_buffer_ref
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
                let init_vertex = vertex_buffer.mirror_from(&vertex_buffer_stg);
                let init_uniform = uniform_buffer.mirror_from(&uniform_mut_buffer);
                let init_tex = CopyBufferToImage::new(&image_data_stg_buffer.inner, &image)
                    .with_range(
                        BufferImageDataDesc::new(0, image_data_stg_buffer.row_texels),
                        ImageResourceRange::for_single_color_from_rect2d(
                            image.size().wh().into_rect(br::vk::VkOffset2D::ZERO),
                        ),
                    );
                let copies = (init_vertex, init_uniform, init_tex);

                copies
                    .between(in_barriers, out_barriers)
                    .execute(&mut r.as_dyn_ref());
                r
            })
            .expect("Failed to submit pre-configure commands");

        let mut update_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1)
            .expect("Alloc UpdateCB");
        {
            let uniform_buffer_ref = uniform_buffer.make_ref();
            let uniform_mut_buffer_ref = uniform_mut_buffer.make_ref();

            let [uniform_in_barrier, uniform_out_barrier] = uniform_buffer_ref
                .usage_barrier3_switching(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST);
            let [staging_uniform_in_barrier, staging_uniform_out_barrier] = uniform_mut_buffer_ref
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

            let in_barriers = [uniform_in_barrier, staging_uniform_in_barrier];
            let out_barriers = [uniform_out_barrier, staging_uniform_out_barrier];
            let copy_uniform = uniform_buffer.mirror_from(&uniform_mut_buffer);

            copy_uniform
                .between(in_barriers, out_barriers)
                .execute_and_finish(unsafe {
                    update_cb
                        .synchronized_nth(0)
                        .begin()
                        .expect("Failed to begin recording update command")
                        .as_dyn_ref()
                })
                .expect("Failed to record update commands");
        }

        let back_buffer_attachment = e
            .back_buffer_attachment_desc()
            .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store);
        let color_render_subpass = br::SubpassDescription::new().add_color_output(
            0,
            br::ImageLayout::ColorAttachmentOpt,
            None,
        );
        let renderpass = br::RenderPassBuilder::new()
            .add_attachment(back_buffer_attachment)
            .add_subpass(color_render_subpass)
            .add_dependency(SubpassDependencyTemplates::to_color_attachment_in(
                None, 0, true,
            ))
            .create(e.graphics().device().clone())
            .expect("Create RenderPass");
        let framebuffers = e
            .iter_back_buffers()
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
        let descriptor_layout = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::UniformBuffer
                .make_binding(1)
                .only_for_vertex(),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_fragment()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&smp)]),
        ])
        .create(e.graphics().device().clone())
        .expect("Create DescriptorSetLayout");
        let mut descriptor_pool = br::DescriptorPoolBuilder::new(1)
            .with_reservations(vec![
                br::DescriptorType::UniformBuffer.with_count(1),
                br::DescriptorType::CombinedImageSampler.with_count(1),
            ])
            .create(e.graphics().device().clone())
            .expect("Create DescriptorPool");

        let shader = e
            .load("builtin.shaders.unlit_image")
            .expect("Loading shader");
        let shader =
            PvpShaderModules::new(e.graphics().device(), shader).expect("Create ShaderModules");
        let pl = br::PipelineLayoutBuilder::new(vec![&descriptor_layout], vec![])
            .create(e.graphics().device().clone())
            .expect("Create PipelineLayout");
        let sc = [screen_size.wh().into_rect(br::vk::VkOffset2D::ZERO)];
        let vp = [sc[0].make_viewport(0.0..1.0)];
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
        .set_attachment_blends(vec![ColorAttachmentBlending::Disabled.into_vk()])
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

        async_std::task::block_on(pre_configure_awaiter)
            .expect("Failed to pre-configure resources");

        let image_view = image
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()
            .expect("Failed to create main image view");
        let descriptor_main = descriptor_pool
            .alloc(&[&descriptor_layout])
            .expect("Create main Descriptor");
        let mut descriptor_writes = Vec::with_capacity(2);
        descriptor_writes.extend(
            br::DescriptorPointer::new(descriptor_main[0].into(), 0).write_multiple([
                br::DescriptorContents::UniformBuffer(vec![
                    uniform_buffer.make_descriptor_buffer_ref()
                ]),
                br::DescriptorContents::CombinedImageSampler(vec![br::DescriptorImageRef::new(
                    &image_view,
                    br::ImageLayout::ShaderReadOnlyOpt,
                )]),
            ]),
        );
        e.graphics()
            .device()
            .update_descriptor_sets(&descriptor_writes, &[]);

        let plane_mesh = StandardMesh {
            vertex_buffers: vec![vertex_buffer],
            vertex_count: 4,
        };

        let descriptor_sets = DescriptorSets(vec![*descriptor_main[0]]);
        let render_image_plane = plane_mesh
            .draw(1)
            .after_of(descriptor_sets.into_bind_graphics());
        let color_renders = (gp, render_image_plane);

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

            let begin_main_rp = BeginRenderPass::for_entire_framebuffer(&renderpass, fb)
                .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

            (&color_renders)
                .between(begin_main_rp, EndRenderPass)
                .execute_and_finish(unsafe {
                    cb.begin()
                        .expect("Failed to begin command recording")
                        .as_dyn_ref()
                })
                .expect("Failed to record render commands");
        }

        bgm.write().expect("Starting BGM").play();

        Self {
            render_cb,
            renderpass,
            framebuffers,
            descriptor: (descriptor_layout, descriptor_pool, descriptor_main),
            rot: 0.0,
            color_renders: color_renders.boxed(),
            mutable_uniform_buffer: uniform_mut_buffer,
            _uniform_buffer: uniform_buffer,
            _sampler: smp,
            _image_view: image_view,
            update_cb,
            ph: PhantomData,
        }
    }

    fn update(&mut self, e: &mut peridot::Engine<PL>, on_backbuffer_of: u32, delta_time: Duration) {
        let dtsec = delta_time.as_secs() as f32 + delta_time.subsec_micros() as f32 / 1000_0000.0;
        self.rot += dtsec * 15.0;
        let rot = self.rot;
        self.mutable_uniform_buffer
            .0
            .guard_map(|ptr| unsafe {
                (*(ptr as *mut Uniform)).object = Quaternion::new(rot, Vector3::up()).into();
            })
            .expect("Update DynamicStgBuffer");

        e.do_render(
            on_backbuffer_of,
            Some(br::EmptySubmissionBatch.with_command_buffers(&self.update_cb)),
            br::EmptySubmissionBatch.with_command_buffers(
                &self.render_cb[on_backbuffer_of as usize..on_backbuffer_of as usize + 1],
            ),
        )
        .expect("Failed to present");
    }

    fn discard_back_buffer_resources(&mut self) {
        self.render_cb.reset().expect("Resetting RenderCB");
        self.framebuffers.clear();
    }
    fn on_resize(&mut self, e: &mut peridot::Engine<PL>, _new_size: Vector2<usize>) {
        self.framebuffers = e
            .iter_back_buffers()
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

        for (cb, fb) in self.render_cb.iter_mut().zip(&self.framebuffers) {
            let begin_main_rp = BeginRenderPass::for_entire_framebuffer(&self.renderpass, fb)
                .with_clear_values(vec![br::ClearValue::color([0.0; 4])]);

            self.color_renders
                .as_ref()
                .between(begin_main_rp, EndRenderPass)
                .execute_and_finish(unsafe {
                    cb.begin()
                        .expect("Failed to begin command recording")
                        .as_dyn_ref()
                })
                .expect("Failed to record render commands");
        }
    }
}

#[repr(C)]
struct Uniform {
    camera: Matrix4F32,
    object: Matrix4F32,
}
