
use std::marker::PhantomData;
use bedrock as br; use bedrock::traits::*;
use peridot::math::{
    Vector3F32, Vector3, Vector2F32, Vector2, Camera, ProjectionMethod, Matrix4F32, Quaternion, Matrix4, Vector4, One
};
use peridot::{
    CommandBundle, CBSubmissionType, TransferBatch, BufferPrealloc, BufferContent,
    SubpassDependencyTemplates, PvpShaderModules, DescriptorSetUpdateBatch, LayoutedPipeline,
    TextureInitializationGroup, Buffer,
    FixedMemory, FixedBufferInitializer
};
use std::borrow::Cow;
use std::rc::Rc;
use std::mem::size_of;
use std::ops::Range;
use std::time::Duration;

pub struct IPFixedBufferInitializer
{
    vertices_offset: u64
}
impl FixedBufferInitializer for IPFixedBufferInitializer
{
    fn stage_data(&mut self, m: &br::MappedMemoryRange)
    {
        unsafe
        {
            m.slice_mut(self.vertices_offset as _, 4).clone_from_slice(&[
                UVVert { pos: Vector3(-1.0, -1.0, 0.0), uv: Vector2(0.0, 0.0) },
                UVVert { pos: Vector3( 1.0, -1.0, 0.0), uv: Vector2(1.0, 0.0) },
                UVVert { pos: Vector3(-1.0,  1.0, 0.0), uv: Vector2(0.0, 1.0) },
                UVVert { pos: Vector3( 1.0,  1.0, 0.0), uv: Vector2(1.0, 1.0) }
            ]);
        }
    }
    fn buffer_graphics_ready(&self, tfb: &mut TransferBatch, buffer: &Buffer, buffer_range: Range<u64>)
    {
        tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_INPUT.vertex_shader(), buffer,
            buffer_range.start as _ .. buffer_range.end as _,
            br::AccessFlags::UNIFORM_READ | br::AccessFlags::VERTEX_ATTRIBUTE_READ);
    }
}

pub struct Game<PL: peridot::NativeLinker>
{
    ph: PhantomData<*const PL>, rot: f32,
    render_cb: peridot::CommandBundle, update_cb: peridot::CommandBundle,
    renderpass: br::RenderPass, framebuffers: Vec<br::Framebuffer>,
    gp_main: LayoutedPipeline,
    descriptor: (br::DescriptorSetLayout, br::DescriptorPool, Vec<br::vk::VkDescriptorSet>),
    _sampler: br::Sampler,
    vertices_offset: u64,
    buffers: FixedMemory, mut_uniform_offset: u64
}
impl<PL: peridot::NativeLinker> Game<PL>
{
    pub const NAME: &'static str = "Peridot Examples - ImagePlane";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL>
{
    fn init(e: &peridot::Engine<Self, PL>) -> Self
    {
        let screen_size: br::Extent3D = e.backbuffers()[0].size().clone().into();
        let screen_aspect = screen_size.1 as f32 / screen_size.0 as f32;

        let image_data: peridot::PNG = e.load("images.example").expect("No image found");
        println!("Image: {}x{}", image_data.0.size.x(), image_data.0.size.y());
        println!("ImageColor: {:?}", image_data.0.color);
        println!("ImageStride: {} bytes", image_data.0.stride);

        let mut bp = BufferPrealloc::new(e.graphics());
        let vertices_offset = bp.add(BufferContent::vertex::<[UVVert; 4]>());
        let mut fm_init = IPFixedBufferInitializer
        {
            vertices_offset
        };

        let mut ti = TextureInitializationGroup::new(e.graphics());
        ti.add(image_data);

        let mut bp_mut = BufferPrealloc::new(e.graphics());
        let mut_uniform_offset = bp_mut.add(BufferContent::uniform::<Uniform>());

        let mut tfb = TransferBatch::new();
        let buffers = FixedMemory::new(e.graphics(), bp, bp_mut, ti, &mut fm_init, &mut tfb)
            .expect("Alloc FixedBuffers");
        
        let cam = Camera
        {
            projection: ProjectionMethod::Perspective { fov: 75.0f32.to_radians() },
            position: Vector3(-3.0, 0.0, -3.0), rotation: Quaternion::new(45.0f32.to_radians(), Vector3::up()),
            // position: Vector3(0.0, 0.0, -3.0), rotation: Quaternion::ONE,
            depth_range: 1.0 .. 10.0
        };
        buffers.mut_buffer.0.guard_map(buffers.mut_buffer.1, |m| unsafe
        {
            let (v, p) = cam.matrixes();
            let aspect = Matrix4::scale(Vector4(screen_aspect, 1.0, 1.0, 1.0));
            let vp = aspect * p * v;
            *m.get_mut(mut_uniform_offset as _) = Uniform
            {
                camera: vp, object: Matrix4::ONE
            };
        }).expect("Staging MutBuffer");
        let mut tfb_mut = TransferBatch::new();
        let dst_update_range = buffers.mut_buffer_placement ..
            buffers.mut_buffer_placement + size_of::<Uniform>() as u64;
        tfb.add_copying_buffer((&buffers.mut_buffer.0, mut_uniform_offset),
            (&buffers.buffer.0, dst_update_range.start), size_of::<Uniform>() as _);
        tfb_mut.add_copying_buffer((&buffers.mut_buffer.0, mut_uniform_offset),
            (&buffers.buffer.0, dst_update_range.start), size_of::<Uniform>() as _);
        tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_SHADER, &buffers.buffer.0,
            dst_update_range.clone(), br::AccessFlags::UNIFORM_READ);
        tfb_mut.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_SHADER, &buffers.buffer.0,
            dst_update_range, br::AccessFlags::UNIFORM_READ);

        e.submit_commands(|r|
        {
            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
        }).expect("Failure in transferring initial data");
        
        let update_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1)
            .expect("Alloc UpdateCB");
        {
            let mut rec = update_cb[0].begin().expect("Begin UpdateCmdRec");
            tfb_mut.sink_transfer_commands(&mut rec);
            tfb_mut.sink_graphics_ready_commands(&mut rec);
        }

        let attdesc = br::AttachmentDescription::new(e.backbuffer_format(),
            br::ImageLayout::PresentSrc, br::ImageLayout::PresentSrc)
            .load_op(br::LoadOp::Clear).store_op(br::StoreOp::Store);
        let renderpass = br::RenderPassBuilder::new().add_attachment(attdesc)
            .add_subpass(br::SubpassDescription::new().add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None))
            .add_dependency(SubpassDependencyTemplates::to_color_attachment_in(None, 0, true))
            .create(&e.graphics()).expect("Create RenderPass");
        let framebuffers = e.backbuffers().iter().map(|v| br::Framebuffer::new(&renderpass, &[v], v.size(), 1))
            .collect::<Result<Vec<_>, _>>().expect("Bind Framebuffer");
        
        let smp = br::SamplerBuilder::default().create(&e.graphics()).expect("Creating Sampler");
        let descriptor_layout = br::DescriptorSetLayout::new(&e.graphics(), &br::DSLBindings {
            uniform_buffer: Some((0, 1, br::ShaderStage::VERTEX)),
            combined_image_sampler: Some((1, 1, br::ShaderStage::FRAGMENT, vec![smp.native_ptr()])),
            .. br::DSLBindings::empty()
        }).expect("Create DescriptorSetLayout");
        let descriptor_pool = br::DescriptorPool::new(&e.graphics(), 1, &[
            br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1),
            br::DescriptorPoolSize(br::DescriptorType::CombinedImageSampler, 1)
        ], false).expect("Create DescriptorPool");
        let descriptor_main = descriptor_pool.alloc(&[&descriptor_layout]).expect("Create main Descriptor");
        let mut dsub = DescriptorSetUpdateBatch::new();
        dsub.write(descriptor_main[0], 0, br::DescriptorUpdateInfo::UniformBuffer(vec![
            (buffers.buffer.0.native_ptr(), buffers.range_in_mut_buffer(0 .. std::mem::size_of::<Uniform>()))
        ]));
        dsub.write(descriptor_main[0], 1, br::DescriptorUpdateInfo::CombinedImageSampler(vec![
            (None, buffers.textures[0].native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
        ]));
        dsub.submit(&e.graphics());

        let shaderfile = e.load("shaders.prim").expect("Loading prim");
        let shader = PvpShaderModules::new(&e.graphics(), shaderfile).expect("Create ShaderModules");
        let vp = [br::vk::VkViewport {
            width: screen_size.0 as _, height: screen_size.1 as _, x: 0.0, y: 0.0,
            minDepth: 0.0, maxDepth: 1.0
        }];
        let sc = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D::default(),
            extent: br::vk::VkExtent2D { width: screen_size.0, height: screen_size.1 }
        }];
        let pl: Rc<_> = br::PipelineLayout::new(&e.graphics(), &[&descriptor_layout], &[])
            .expect("Create PipelineLayout").into();
        let gp = br::GraphicsPipelineBuilder::new(&pl, (&renderpass, 0))
            .vertex_processing(shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP))
            .fixed_viewport_scissors(br::DynamicArrayState::Static(&vp), br::DynamicArrayState::Static(&sc))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .create(&e.graphics(), None).expect("Create GraphicsPipeline");
        let gp = LayoutedPipeline::combine(gp, &pl);

        let render_cb = CommandBundle::new(e.graphics(), CBSubmissionType::Graphics, e.backbuffers().len())
            .expect("Alloc RenderCB");
        for (cb, fb) in render_cb.iter().zip(&framebuffers) {
            let mut cr = cb.begin().expect("Begin CmdRecord");
            cr.begin_render_pass(&renderpass, fb, fb.size().clone().into(), &[br::ClearValue::Color([0.0; 4])], true);
            gp.bind(&mut cr);
            cr.bind_graphics_descriptor_sets(0, &descriptor_main, &[]);
            cr.bind_vertex_buffers(0, &[(&buffers.buffer.0, vertices_offset as _)]);
            cr.draw(4, 1, 0, 0);
            cr.end_render_pass();
        }

        Game {
            render_cb, renderpass, framebuffers,
            descriptor: (descriptor_layout, descriptor_pool, descriptor_main), gp_main: gp,
            rot: 0.0, vertices_offset, _sampler: smp,
            buffers, update_cb, mut_uniform_offset,
            ph: PhantomData
        }
    }

    fn update(&mut self, e: &peridot::Engine<Self, PL>, on_backbuffer_of: u32, delta_time: Duration)
        -> (Option<br::SubmissionBatch>, br::SubmissionBatch)
    {
        let dtsec = delta_time.as_secs() as f32 + delta_time.subsec_micros() as f32 / 1000_0000.0;
        self.rot += dtsec * 15.0;
        self.buffers.mut_buffer.0.guard_map(self.mut_uniform_offset + size_of::<Uniform>() as u64, |m| unsafe
        {
            m.get_mut::<Uniform>(self.mut_uniform_offset as _).object
                = Quaternion::new(self.rot, Vector3F32::up()).into();
        }).expect("Update DynamicStgBuffer");

        (Some(br::SubmissionBatch {
            command_buffers: Cow::Borrowed(&self.update_cb[..]),
            .. Default::default()
        }), br::SubmissionBatch {
            command_buffers: Cow::Borrowed(&self.render_cb[on_backbuffer_of as usize..on_backbuffer_of as usize + 1]),
            .. Default::default()
        })
    }

    fn discard_backbuffer_resources(&mut self) {
        self.framebuffers.clear();
        self.render_cb.reset().expect("Resetting RenderCB");
    }
    fn on_resize(&mut self, e: &peridot::Engine<Self, PL>, new_size: Vector2<usize>) {
        self.framebuffers = e.backbuffers().iter().map(|v| br::Framebuffer::new(&self.renderpass, &[v], v.size(), 1))
            .collect::<Result<Vec<_>, _>>().expect("Bind Framebuffer");
        self.populate_render_commands();
    }
}
impl<PL: peridot::NativeLinker> Game<PL> {
    fn populate_render_commands(&mut self) {
        for (cb, fb) in self.render_cb.iter().zip(&self.framebuffers) {
            let mut cr = cb.begin().expect("Begin CmdRecord");
            cr.begin_render_pass(&self.renderpass, fb, fb.size().clone().into(),
                &[br::ClearValue::Color([0.0; 4])], true);
            self.gp_main.bind(&mut cr);
            cr.bind_graphics_descriptor_sets(0, &self.descriptor.2, &[]);
            cr.bind_vertex_buffers(0, &[(&self.buffers.buffer.0, self.vertices_offset as _)]);
            cr.draw(4, 1, 0, 0);
            cr.end_render_pass();
        }
    }
}

#[derive(Clone)] #[repr(C)]
struct UVVert { pos: Vector3F32, uv: Vector2F32 }

#[repr(C)] struct Uniform { camera: Matrix4F32, object: Matrix4F32 }
