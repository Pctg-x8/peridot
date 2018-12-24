
use std::marker::PhantomData;
use bedrock as br; use bedrock::traits::*;
use peridot::math::{
    Vector3F32, Vector3, Vector2F32, Vector2, Camera, ProjectionMethod, Matrix4F32, Quaternion, Matrix4, Vector4, One
};
use peridot::{
    CommandBundle, CBSubmissionType, TransferBatch, MemoryBadget, BufferPrealloc, BufferContent,
    SubpassDependencyTemplates, PvpShaderModules, DescriptorSetUpdateBatch, LayoutedPipeline
};
use std::borrow::Cow;
use std::rc::Rc;

pub struct Game<PL: peridot::PlatformLinker> {
    ph: PhantomData<*const PL>, _buffer: peridot::Buffer, render_cb: peridot::CommandBundle,
    _renderpass: br::RenderPass, _framebuffers: Vec<br::Framebuffer>,
    _gp_main: LayoutedPipeline, _descriptor: (br::DescriptorSetLayout, br::DescriptorPool, Vec<br::vk::VkDescriptorSet>)
}
impl<PL: peridot::PlatformLinker> Game<PL> {
    pub const NAME: &'static str = "Peridot Examples - ImagePlane";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::PlatformLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &peridot::Engine<Self, PL>) -> Self {
        let screen_size: br::Extent3D = e.backbuffers()[0].size().clone().into();

        let mut bp = BufferPrealloc::new(e.graphics());
        let uniform_offset = bp.add(BufferContent::uniform::<Matrix4F32>());
        let vertices_offset = bp.add(BufferContent::vertex::<[UVVert; 4]>());
        let buffer = bp.build_transferred().expect("Alloc Buffer");
        let stg_buffer = bp.build_upload().expect("Alloc StgBuffer");
        let buffer = MemoryBadget::new(e.graphics()).alloc_with_buffer(buffer).expect("Alloc Mem");
        let stg_buffer = MemoryBadget::new(e.graphics()).alloc_with_buffer_host_visible(stg_buffer)
            .expect("Alloc StgMem");

        let cam = Camera {
            projection: ProjectionMethod::Perspective { fov: 75.0f32.to_radians() },
            position: Vector3(-3.0, 0.0, -3.0), rotation: Quaternion::new(45.0f32.to_radians(), Vector3::up()),
            // position: Vector3(0.0, 0.0, -3.0), rotation: Quaternion::ONE,
            depth_range: 1.0 .. 10.0
        };
        stg_buffer.guard_map(bp.total_size(), |r| unsafe {
            let (v, p) = cam.matrixes();
            let aspect = Matrix4::scale(Vector4(screen_size.1 as f32 / screen_size.0 as f32, 1.0, 1.0, 1.0));
            let vp = aspect * p * v;
            *r.get_mut(uniform_offset) = vp;
            r.slice_mut(vertices_offset, 4).clone_from_slice(&[
                UVVert { pos: Vector3(-1.0, -1.0, 0.0), uv: Vector2(0.0, 0.0) },
                UVVert { pos: Vector3( 1.0, -1.0, 0.0), uv: Vector2(1.0, 0.0) },
                UVVert { pos: Vector3(-1.0,  1.0, 0.0), uv: Vector2(0.0, 1.0) },
                UVVert { pos: Vector3( 1.0,  1.0, 0.0), uv: Vector2(1.0, 1.0) }
            ]);
        }).expect("Failure in constructing initial data");

        e.submit_commands(|r| {
            let mut tfb = TransferBatch::new();
            tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, bp.total_size() as _);
            tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_INPUT.vertex_shader(), &buffer,
                0 .. bp.total_size() as _, br::AccessFlags::UNIFORM_READ | br::AccessFlags::VERTEX_ATTRIBUTE_READ);
            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
        }).expect("Failure in transferring initial data");

        let attdesc = br::AttachmentDescription::new(e.backbuffer_format(),
            br::ImageLayout::PresentSrc, br::ImageLayout::PresentSrc)
            .load_op(br::LoadOp::Clear).store_op(br::StoreOp::Store);
        let renderpass = br::RenderPassBuilder::new().add_attachment(attdesc)
            .add_subpass(br::SubpassDescription::new().add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None))
            .add_dependency(SubpassDependencyTemplates::to_color_attachment_in(None, 0, true))
            .create(&e.graphics()).expect("Create RenderPass");
        let framebuffers = e.backbuffers().iter().map(|v| br::Framebuffer::new(&renderpass, &[v], v.size(), 1))
            .collect::<Result<Vec<_>, _>>().expect("Bind Framebuffer");
        
        let descriptor_layout_ub1 = br::DescriptorSetLayout::new(&e.graphics(), &br::DSLBindings {
            uniform_buffer: Some((0, 1, br::ShaderStage::VERTEX)),
            .. br::DSLBindings::empty()
        }).expect("Create DescriptorSetLayout");
        let descriptor_pool = br::DescriptorPool::new(&e.graphics(), 1,
            &[br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1)], false).expect("Create DescriptorPool");
        let descriptor_main = descriptor_pool.alloc(&[&descriptor_layout_ub1]).expect("Create main Descriptor");
        let mut dsub = DescriptorSetUpdateBatch::new();
        dsub.write(descriptor_main[0], 0, br::DescriptorUpdateInfo::UniformBuffer(vec![
            (buffer.native_ptr(), 0 .. std::mem::size_of::<Matrix4F32>())
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
        let pl: Rc<_> = br::PipelineLayout::new(&e.graphics(), &[&descriptor_layout_ub1], &[])
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
            cr.bind_vertex_buffers(0, &[(&buffer, vertices_offset)]);
            cr.draw(4, 1, 0, 0);
            cr.end_render_pass();
        }

        Game {
            _buffer: buffer, render_cb, _renderpass: renderpass, _framebuffers: framebuffers,
            _descriptor: (descriptor_layout_ub1, descriptor_pool, descriptor_main), _gp_main: gp,
            ph: PhantomData
        }
    }

    fn update(&mut self, e: &peridot::Engine<Self, PL>, on_backbuffer_of: u32)
            -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        (None, br::SubmissionBatch {
            command_buffers: Cow::Borrowed(&self.render_cb[on_backbuffer_of as usize..on_backbuffer_of as usize + 1]),
            .. Default::default()
        })
    }
}

#[derive(Clone)] #[repr(C)]
struct UVVert { pos: Vector3F32, uv: Vector2F32 }
