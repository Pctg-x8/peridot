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
use std::mem::{align_of, size_of};
use std::ops::Range;
use std::sync::{Arc, RwLock};
use std::time::Duration;

#[cfg(feature = "debug")]
use br::VkObject;

use crate::command_object::{
    BeginRenderPass, BindGraphicsDescriptorSets, BufferImageDataDesc, BufferUsage, CopyBuffer,
    CopyBufferToImage, DrawMesh, EndRenderPass, ImageResourceRange, PipelineBarrier, RangedBuffer,
    RangedImage,
};

mod command_object;

fn range_from_length<N>(start: N, length: N) -> Range<N>
where
    N: std::ops::Add<N, Output = N> + Copy,
{
    start..start + length
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
            rotation: Quaternion::ONE,
            // position: Vector3(0.0, 0.0, -3.0), rotation: Quaternion::ONE,
            depth_range: 1.0..10.0,
        };
        cam.look_at(Vector3(0.0, 0.0, 0.0));

        let mut bp = BufferPrealloc::new(e.graphics());
        let vertices_offset = bp.add(BufferContent::vertices::<peridot::VertexUV>(
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
        let vertex_buffer = RangedBuffer::from_offset_length(
            &buffer,
            vertices_offset,
            std::mem::size_of::<peridot::VertexUV>() * plane_mesh.vertices.len(),
        );

        let pre_configure_task = e
            .submit_commands_async(|mut r| {
                let all_buffer = RangedBuffer::from_offset_length(&buffer, 0, bp.total_size() as _);
                let mut_buffer = RangedBuffer::for_type::<Uniform>(&mut_buffer, mut_uniform_offset);
                let staging_init =
                    RangedBuffer::from_offset_length(&buffer_staging, 0, bp_stg.total_size() as _);
                let uniform_buffer =
                    RangedBuffer::for_type::<Uniform>(&buffer, mutable_data_offset);
                let texture = RangedImage::single_color_plane(&image);

                let in_barriers = PipelineBarrier::new()
                    .with_barriers([
                        all_buffer.usage_barrier(BufferUsage::UNUSED, BufferUsage::TRANSFER_DST),
                        mut_buffer.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                        staging_init.usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
                    ])
                    .with_barrier(texture.barrier(
                        br::ImageLayout::Preinitialized,
                        br::ImageLayout::TransferDestOpt,
                    ))
                    .by_region();
                let out_barriers = PipelineBarrier::new()
                    .with_barriers([
                        vertex_buffer
                            .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_BUFFER),
                        uniform_buffer
                            .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                        mut_buffer.usage_barrier(BufferUsage::TRANSFER_SRC, BufferUsage::HOST_RW),
                    ])
                    .with_barrier(texture.barrier(
                        br::ImageLayout::TransferDestOpt,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ))
                    .by_region();
                let staging_copy = CopyBuffer::new(&buffer_staging, &buffer)
                    .with_mirroring(0, copy_buffer_data_length as _);
                let mutable_copy = CopyBuffer::new(mut_buffer.inner_ref(), &buffer)
                    .with_range_for_type::<Uniform>(mut_uniform_offset, mutable_data_offset);
                let tex_copy = CopyBufferToImage::new(&buffer_staging, &image).with_range(
                    BufferImageDataDesc::new(
                        staging_image_offset,
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
            let uniform_buffer = RangedBuffer::for_type::<Uniform>(&buffer, mutable_data_offset);
            let staging_uniform_buffer =
                RangedBuffer::for_type::<Uniform>(&mut_buffer, mut_uniform_offset);

            let in_barriers = [
                uniform_buffer
                    .usage_barrier(BufferUsage::VERTEX_UNIFORM, BufferUsage::TRANSFER_DST),
                staging_uniform_buffer
                    .usage_barrier(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC),
            ];
            let out_barriers = [
                uniform_buffer
                    .usage_barrier(BufferUsage::TRANSFER_DST, BufferUsage::VERTEX_UNIFORM),
                staging_uniform_buffer
                    .usage_barrier(BufferUsage::TRANSFER_SRC, BufferUsage::HOST_RW),
            ];
            let copy_uniform = CopyBuffer::new(&mut_buffer, &buffer)
                .with_range_for_type::<Uniform>(mut_uniform_offset, mutable_data_offset);

            copy_uniform
                .between(in_barriers, out_barriers)
                .execute_into(unsafe { update_cb.synchronized_nth(0) })
                .expect("Failed to record update commands");
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

        let shader = e
            .load("builtin.shaders.unlit_image")
            .expect("Loading shader");
        let shader =
            PvpShaderModules::new(e.graphics().device(), shader).expect("Create ShaderModules");
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

        async_std::task::block_on(pre_configure_task).expect("Failed to pre-configure resources");

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
            let render_image_plane = DrawMesh::new(vec![vertex_buffer.make_ref()], 4)
                .after_of(BindGraphicsDescriptorSets::new(vec![*descriptor_main[0]]));

            unsafe {
                (&gp, render_image_plane)
                    .between(begin_main_rp, EndRenderPass)
                    .execute_into_ext_sync(cb)
                    .expect("Failed to record render commands");
            }
        }

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
        // TODO: これselfにいれたい
        let vertex_buffer = RangedBuffer::from_offset_length(
            &self.buffer,
            self.vertices_offset,
            std::mem::size_of::<peridot::VertexUV>(),
        );

        for (cb, fb) in self.render_cb.iter_mut().zip(&self.framebuffers) {
            let begin_main_rp = BeginRenderPass::for_entire_framebuffer(
                &self.renderpass,
                fb,
                vec![br::ClearValue::color([0.0; 4])],
            );
            let render_image_plane = DrawMesh::new(vec![vertex_buffer.make_ref()], 4)
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
