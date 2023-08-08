use bedrock as br;
use bedrock::traits::*;
use br::{resources::Image, SubmissionBatch};
use br::{DescriptorPool, Device, ImageChild};
use command_object::GraphicsCommand;
use log::*;
use peridot::math::{
    Camera, Matrix4, Matrix4F32, One, ProjectionMethod, Quaternion, Vector2, Vector3, Vector3F32,
};
use peridot::mthelper::{DynamicMutabilityProvider, SharedRef};
use peridot::{
    audio::StreamingPlayableWav, BufferContent, BufferPrealloc, CBSubmissionType, CommandBundle,
    DescriptorSetUpdateBatch, LayoutedPipeline, SubpassDependencyTemplates,
};
use peridot_vertex_processing_pack::PvpShaderModules;
use std::convert::TryInto;
use std::marker::PhantomData;
use std::sync::{Arc, RwLock};
use std::time::Duration;

#[cfg(feature = "debug")]
use br::VkObject;

use crate::command_object::{
    BeginRenderPass, BindGraphicsDescriptorSets, BufferImageDataDesc, BufferUsage, CopyBuffer,
    CopyBufferToImage, EndRenderPass, ImageResourceRange, Mesh, PipelineBarrier, RangedBuffer,
    RangedImage,
};
use crate::descriptor_pointer::DescriptorPointer;

mod command_object;
mod descriptor_pointer;

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
    plane_mesh: Mesh<
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    mutable_uniform_buffer: RangedBuffer<
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    _image_view: br::ImageViewObject<
        peridot::Image<
            br::ImageObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
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
            rotation: Quaternion::ONE,
            depth_range: 1.0..10.0,
        };
        cam.look_at(Vector3(0.0, 0.0, 0.0));

        let mut bp = BufferPrealloc::new(e.graphics());
        let buffer_offsets = BufferOffsets {
            plane_vertices: bp.add(BufferContent::vertices_for(&plane_mesh.vertices)),
        };

        let mut bp_stg = bp.clone();
        let copy_buffer_data_length = bp_stg.total_size();
        let staging_buffer_offsets = StagingBufferOffsets {
            image: bp_stg.add(BufferContent::raw_dynarray::<u32>(
                image_data.0.u8_pixels().len() >> 2,
            )),
        };

        let mut bp_mut = BufferPrealloc::new(e.graphics());
        let mutable_buffer_offsets = MutableBufferOffsets {
            uniform: bp_mut.add(BufferContent::uniform::<Uniform>()),
        };
        let mutable_data_start = bp.merge(&bp_mut);

        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp.build_transferred().expect("Failed to create buffer"),
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
        let Ok::<[_; 2], _>([
            peridot::MemoryBoundResource::Buffer(buffer),
            peridot::MemoryBoundResource::Image(image)
        ]) = mb.alloc().expect("Failed to allocate memory").try_into() else {
            unreachable!("invalid return combination");
        };
        let mut mb =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Buffer(
            bp_mut
                .build_upload()
                .expect("Failed to create mutable data buffer"),
        ));
        let Ok::<[_; 1], _>([
            peridot::MemoryBoundResource::Buffer(mut mut_buffer)
        ]) = mb.alloc_upload().expect("Failed to allocate mutable data memory").try_into() else {
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
            let Ok::<[_; 1], _>([
                peridot::MemoryBoundResource::Buffer(b)
            ]) = mb.alloc_upload().expect("Failed to allocate staging memory").try_into() else {
                unreachable!("invalid return combination")
            };

            // TODO: requires flushing?
            (b, false)
        };

        buffer_staging
            .guard_map(0..bp_stg.total_size(), |r| unsafe {
                r.clone_from_slice_at(buffer_offsets.plane_vertices as _, &plane_mesh.vertices);
                r.clone_from_slice_at(staging_buffer_offsets.image as _, image_data.0.u8_pixels());
            })
            .expect("Failed to setup staging data");
        mut_buffer
            .guard_map(0..bp_mut.total_size(), |r| unsafe {
                *r.get_mut(mutable_buffer_offsets.uniform as _) = Uniform {
                    camera: cam.view_projection_matrix(screen_aspect),
                    object: Matrix4::ONE,
                };
            })
            .expect("Failed to setup mutable data");

        let vertex_buffer = RangedBuffer::from_offset_length(
            &buffer,
            buffer_offsets.plane_vertices,
            std::mem::size_of::<peridot::VertexUV>() * plane_mesh.vertices.len(),
        );
        let uniform_buffer = RangedBuffer::for_type::<Uniform>(
            &buffer,
            mutable_data_start + mutable_buffer_offsets.uniform,
        );
        let mut_uniform_buffer =
            RangedBuffer::for_type::<Uniform>(&mut_buffer, mutable_buffer_offsets.uniform);

        let pre_configure_awaiter = e
            .submit_commands_async(|mut r| {
                let all_buffer = RangedBuffer::from_offset_length(&buffer, 0, bp.total_size() as _);
                let staging_init =
                    RangedBuffer::from_offset_length(&buffer_staging, 0, bp_stg.total_size() as _);
                let texture = RangedImage::single_color_plane(&image);

                let [mut_uniform_in_barrier, mut_uniform_out_barrier] = mut_uniform_buffer
                    .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);
                let [tex_init_barrier, tex_ready_barrier] = texture.barrier3(
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::TransferDestOpt,
                    br::ImageLayout::ShaderReadOnlyOpt,
                );

                let in_barriers = PipelineBarrier::new()
                    .with_barriers([
                        all_buffer.usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                        mut_uniform_in_barrier,
                        staging_init.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    ])
                    .with_barrier(tex_init_barrier)
                    .by_region();
                let out_barriers = PipelineBarrier::new()
                    .with_barriers([
                        vertex_buffer
                            .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_BUFFER),
                        uniform_buffer
                            .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                        mut_uniform_out_barrier,
                    ])
                    .with_barrier(tex_ready_barrier)
                    .by_region();
                let staging_copy = CopyBuffer::new(&buffer_staging, &buffer)
                    .with_mirroring(0, copy_buffer_data_length as _);
                let mutable_copy = CopyBuffer::new(mut_uniform_buffer.inner_ref(), &buffer)
                    .with_range_for_type::<Uniform>(
                        mutable_buffer_offsets.uniform,
                        mutable_data_start + mutable_buffer_offsets.uniform,
                    );
                let tex_copy = CopyBufferToImage::new(&buffer_staging, &image).with_range(
                    BufferImageDataDesc::new(
                        staging_buffer_offsets.image,
                        (image_data.0.stride / (image_data.0.format.bpp() >> 3)) as _,
                    ),
                    ImageResourceRange::for_single_color_from_rect2d(
                        br::vk::VkExtent2D::from(image_data.0.size)
                            .into_rect(br::vk::VkOffset2D { x: 0, y: 0 }),
                    ),
                );
                let copies = (staging_copy, mutable_copy, tex_copy);

                copies.between(in_barriers, out_barriers).execute(&mut r);
                r
            })
            .expect("Failed to submit pre-configure commands");

        let mut update_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1)
            .expect("Alloc UpdateCB");
        {
            let staging_uniform_buffer =
                RangedBuffer::for_type::<Uniform>(&mut_buffer, mutable_buffer_offsets.uniform);

            let [uniform_in_barrier, uniform_out_barrier] = uniform_buffer
                .usage_barrier3_switching(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST);
            let [staging_uniform_in_barrier, staging_uniform_out_barrier] = staging_uniform_buffer
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

            let in_barriers = [uniform_in_barrier, staging_uniform_in_barrier];
            let out_barriers = [uniform_out_barrier, staging_uniform_out_barrier];
            let copy_uniform = CopyBuffer::new(&mut_buffer, &buffer)
                .with_range_for_type::<Uniform>(
                    mutable_buffer_offsets.uniform,
                    uniform_buffer.offset(),
                );

            copy_uniform
                .between(in_barriers, out_barriers)
                .execute_into(unsafe { update_cb.synchronized_nth(0) })
                .expect("Failed to record update commands");
        }

        let backbuffer_attachment = e
            .backbuffer_attachment_desc()
            .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store);
        let color_render_subpass = br::SubpassDescription::new().add_color_output(
            0,
            br::ImageLayout::ColorAttachmentOpt,
            None,
        );
        let renderpass = br::RenderPassBuilder::new()
            .add_attachment(backbuffer_attachment)
            .add_subpass(color_render_subpass)
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

        let shader = e
            .load("builtin.shaders.unlit_image")
            .expect("Loading shader");
        let shader =
            PvpShaderModules::new(e.graphics().device(), shader).expect("Create ShaderModules");
        let pl = e
            .graphics()
            .device()
            .clone()
            .new_pipeline_layout(&[&descriptor_layout], &[])
            .expect("Create PipelineLayout");
        let sc = [screen_size.wh().into_rect(br::vk::VkOffset2D::ZERO)];
        let vp = [sc[0].clone().make_viewport(0.0..1.0)];
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

        async_std::task::block_on(pre_configure_awaiter)
            .expect("Failed to pre-configure resources");

        let image_view = image
            .create_view(
                None,
                None,
                &br::ComponentMapping::default(),
                &br::ImageSubresourceRange::color(0..1, 0..1),
            )
            .expect("Failed to create main image view");
        let descriptor_main = descriptor_pool
            .alloc(&[&descriptor_layout])
            .expect("Create main Descriptor");
        let main_descriptor = DescriptorPointer::new(descriptor_main[0]);
        let mut dsub = DescriptorSetUpdateBatch::new();
        main_descriptor.bound_at(0).write(
            &mut dsub,
            br::DescriptorUpdateInfo::UniformBuffer(vec![
                uniform_buffer.descriptor_uniform_buffer_write_info()
            ]),
        );
        main_descriptor.bound_at(1).write(
            &mut dsub,
            br::DescriptorUpdateInfo::CombinedImageSampler(vec![(
                None,
                image_view.native_ptr(),
                br::ImageLayout::ShaderReadOnlyOpt,
            )]),
        );
        dsub.submit(e.graphics().device());

        let RangedBuffer(_, vertex_buffer_range) = vertex_buffer;
        let plane_mesh = Mesh {
            vertex_buffers: vec![RangedBuffer(buffer, vertex_buffer_range)],
            vertex_count: 4,
        };

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
            .apply(e.graphics().device())
            .expect("Failed to set render cb name");

            let begin_main_rp = BeginRenderPass::for_entire_framebuffer(
                &renderpass,
                fb,
                vec![br::ClearValue::color([0.0; 4])],
            );
            let render_image_plane = plane_mesh
                .draw(1)
                .after_of(BindGraphicsDescriptorSets::new(vec![*descriptor_main[0]]));

            unsafe {
                (&gp, render_image_plane)
                    .between(begin_main_rp, EndRenderPass)
                    .execute_into_ext_sync(cb)
                    .expect("Failed to record render commands");
            }
        }

        bgm.write().expect("Starting BGM").play();
        let RangedBuffer(_, mutable_uniform_range) = mut_uniform_buffer;

        Game {
            render_cb,
            renderpass,
            framebuffers,
            descriptor: (descriptor_layout, descriptor_pool, descriptor_main),
            gp_main: gp,
            rot: 0.0,
            plane_mesh,
            mutable_uniform_buffer: RangedBuffer(mut_buffer, mutable_uniform_range),
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
            .guard_map(|m| unsafe {
                m.get_mut::<Uniform>(0).object = Quaternion::new(rot, Vector3F32::UP).into();
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

    fn discard_backbuffer_resources(&mut self) {
        self.render_cb.reset().expect("Resetting RenderCB");
        self.framebuffers.clear();
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
            let begin_main_rp = BeginRenderPass::for_entire_framebuffer(
                &self.renderpass,
                fb,
                vec![br::ClearValue::color([0.0; 4])],
            );
            let render_image_plane = self
                .plane_mesh
                .draw(1)
                .after_of(BindGraphicsDescriptorSets::new(vec![*self.descriptor.2[0]]));

            let commands =
                (&self.gp_main, render_image_plane).between(begin_main_rp, EndRenderPass);
            unsafe {
                commands
                    .execute_into_ext_sync(cb)
                    .expect("Failed to record render commands");
            }
        }
    }
}

#[repr(C)]
struct Uniform {
    camera: Matrix4F32,
    object: Matrix4F32,
}
