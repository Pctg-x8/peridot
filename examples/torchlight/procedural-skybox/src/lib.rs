//! http://publications.lib.chalmers.se/records/fulltext/203057/203057.pdf

use peridot::{NativeLinker, Engine, EngineEvents, FeatureRequests};
use peridot_vertex_processing_pack::PvpShaderModules;
use bedrock as br;
use std::rc::Rc;

pub struct SkyboxRenderer
{
    main_render_shader: PvpShaderModules<'static>,
    pipeline: peridot::LayoutedPipeline
}
impl SkyboxRenderer
{
    fn new<NL: NativeLinker>(e: &Engine<Game<NL>, NL>, drt: &DetailedRenderTargets) -> Self
    {
        let empty_layout: Rc<_> = br::PipelineLayout::new(e.graphics(), &[], &[])
            .expect("Faield to create empty pipeline")
            .into();
        let main_render_shader = PvpShaderModules::new(
            e.graphics(),
            e.load("shaders.skybox").expect("Failed to load main shader")
        ).expect("Instantiating shader failed");

        let backbuffer_size = e.backbuffers().first().expect("no backbuffers?").size().clone();
        let full_scissors = [br::vk::VkRect2D::from(br::Extent2D(backbuffer_size.0, backbuffer_size.1))];
        let full_viewports = [br::Viewport::from_rect_with_depth_range(&full_scissors[0], 0.0 .. 1.0).into_inner()];

        let pipeline = br::GraphicsPipelineBuilder::new(&empty_layout, (&drt.rp, 0))
            .vertex_processing(main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP))
            .fixed_viewport_scissors(
                br::DynamicArrayState::Static(&full_viewports),
                br::DynamicArrayState::Static(&full_scissors))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .create(e.graphics(), None)
            .expect("Creating GraphicsPipeline failed");

        SkyboxRenderer
        {
            main_render_shader,
            pipeline: peridot::LayoutedPipeline::combine(pipeline, &empty_layout)
        }
    }

    fn recreate_pipeline<NL: NativeLinker>(&mut self, e: &Engine<Game<NL>, NL>, drt: &DetailedRenderTargets)
    {
        let backbuffer_size = e.backbuffers().first().expect("no backbuffers?").size().clone();
        let full_scissors = [br::vk::VkRect2D::from(br::Extent2D(backbuffer_size.0, backbuffer_size.1))];
        let full_viewports = [br::Viewport::from_rect_with_depth_range(&full_scissors[0], 0.0 .. 1.0).into_inner()];

        let pipeline = br::GraphicsPipelineBuilder::new(self.pipeline.layout(), (&drt.rp, 0))
            .vertex_processing(self.main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP))
            .fixed_viewport_scissors(
                br::DynamicArrayState::Static(&full_viewports),
                br::DynamicArrayState::Static(&full_scissors))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .create(e.graphics(), None)
            .expect("Creating GraphicsPipeline failed");
        
        self.pipeline = peridot::LayoutedPipeline::combine(pipeline, self.pipeline.layout());
    }
}

pub struct DetailedRenderTargets
{
    rp: br::RenderPass,
    fb: Vec<br::Framebuffer>
}
impl DetailedRenderTargets
{
    pub fn new<NL: NativeLinker>(e: &Engine<Game<NL>, NL>) -> Self
    {
        let rp = peridot::RenderPassTemplates::single_render(e.backbuffer_format()).create(e.graphics())
            .expect("Creating RenderPass failed");
        let backbuffer_size = e.backbuffers().first().expect("no backbuffers?").size().clone();
        let fb = e.backbuffers().iter()
            .map(|bb| br::Framebuffer::new(&rp, &[bb], &backbuffer_size, 1).expect("Creating Framebuffer failed"))
            .collect();
        
        DetailedRenderTargets
        {
            rp, fb
        }
    }

    pub fn discard_framebuffers(&mut self) { self.fb.clear(); }
    pub fn recreate_framebuffers<NL: NativeLinker>(&mut self,
        e: &Engine<Game<NL>, NL>,
        size: &peridot::math::Vector2<usize>)
    {
        let su32: br::Extent2D = peridot::math::Vector2::<u32>(size.0 as _, size.1 as _).into();
        self.fb = e.backbuffers().iter()
            .map(|bb| br::Framebuffer::new(&self.rp, &[bb], &su32, 1).expect("Recreating Framebuffer failed"))
            .collect();
    }
}

pub struct FixedBufferOffsets
{
    fill_plane: u64
}
pub struct FixedBufferInitializer
{
    fill_plane: peridot::Primitive<peridot::VertexUV2D>,
    fill_plane_offset: u64
}
impl peridot::FixedBufferInitializer for FixedBufferInitializer
{
    fn stage_data(&mut self, m: &br::MappedMemoryRange)
    {
        unsafe
        {
            m.slice_mut(self.fill_plane_offset as _, self.fill_plane.vertices.len())
                .clone_from_slice(&self.fill_plane.vertices);
        }
    }

    fn buffer_graphics_ready(
        &self,
        tfb: &mut peridot::TransferBatch,
        buf: &peridot::Buffer,
        range: std::ops::Range<u64>)
    {
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT,
            buf,
            range.clone(),
            br::AccessFlags::VERTEX_ATTRIBUTE_READ
        );
    }
}
impl From<FixedBufferInitializer> for FixedBufferOffsets
{
    fn from(i: FixedBufferInitializer) -> Self
    {
        FixedBufferOffsets
        {
            fill_plane: i.fill_plane_offset
        }
    }
}

pub struct RenderCommands
{
    main_commands: peridot::CommandBundle
}
impl RenderCommands
{
    fn new<NL: NativeLinker>(e: &Engine<Game<NL>, NL>) -> Self
    {
        RenderCommands
        {
            main_commands: peridot::CommandBundle::new(
                e.graphics(),
                peridot::CBSubmissionType::Graphics,
                e.backbuffers().len()
            ).expect("Failed to create Main CommandBundle")
        }
    }

    fn generate_commands<NL: NativeLinker>(
        &self,
        e: &Engine<Game<NL>, NL>,
        skybox_renderer: &SkyboxRenderer,
        drt: &DetailedRenderTargets,
        buf: &peridot::FixedMemory,
        buf_offsets: &FixedBufferOffsets)
    {
        let render_area = br::vk::VkRect2D::from(
            AsRef::<br::Extent2D>::as_ref(e.backbuffers().first().expect("no backbuffers?").size()).clone()
        );

        for (b, fb) in self.main_commands.iter().zip(&drt.fb)
        {
            b.begin().expect("Failed to begin record main command")
                .begin_render_pass(&drt.rp, fb, render_area.clone(), &[br::ClearValue::Color([0.0; 4])], true)
                .bind_graphics_pipeline_pair(skybox_renderer.pipeline.pipeline(), skybox_renderer.pipeline.layout())
                .bind_vertex_buffers(0, &[(&buf.buffer.0, buf_offsets.fill_plane as _)])
                .draw(4, 1, 0, 0)
                .end_render_pass();
        }
    }
    fn clear_commands(&self)
    {
        self.main_commands.reset().expect("Resetting failed");
    }
}

pub struct Game<NL>
{
    rt: DetailedRenderTargets,
    skybox: SkyboxRenderer,
    buf: peridot::FixedMemory,
    buf_offsets: FixedBufferOffsets,
    cmds: RenderCommands,
    ph: std::marker::PhantomData<*const NL>
}
impl<NL> Game<NL>
{
    pub const NAME: &'static str = "pj-torchlight/procedural-skybox";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<NL> FeatureRequests for Game<NL> {}
impl<NL: NativeLinker> EngineEvents<NL> for Game<NL>
{
    fn init(e: &Engine<Self, NL>) -> Self
    {
        let rt = DetailedRenderTargets::new(e);
        let skybox = SkyboxRenderer::new(e, &rt);

        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let fill_plane = peridot::Primitive::uv_plane_centric(1.0);
        let fill_plane_offset = bp.add(
            peridot::BufferContent::vertices::<peridot::VertexUV2D>(fill_plane.vertices.len())
        );
        let mut mbp = peridot::BufferPrealloc::new(e.graphics());
        mbp.add(peridot::BufferContent::uniform::<f32>());
        let mut fb_data = FixedBufferInitializer
        {
            fill_plane,
            fill_plane_offset
        };
        let mut tfb = peridot::TransferBatch::new();
        let buf = peridot::FixedMemory::new(
            e.graphics(),
            bp,
            mbp,
            peridot::TextureInitializationGroup::new(e.graphics()),
            &mut fb_data,
            &mut tfb
        ).expect("Failed to initialize fixed buffers");
        let buf_offsets = FixedBufferOffsets::from(fb_data);

        let cmds = RenderCommands::new(e);

        e.submit_commands(|r|
        {
            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
        }).expect("Failed to resource memory initialization");

        Game
        {
            rt,
            skybox,
            buf,
            buf_offsets,
            cmds,
            ph: std::marker::PhantomData
        }
    }
    fn update(&mut self, _e: &Engine<Self, NL>, on_backbuffer_of: u32, _dt: std::time::Duration)
        -> (Option<br::SubmissionBatch>, br::SubmissionBatch)
    {
        (None, br::SubmissionBatch
        {
            command_buffers: std::borrow::Cow::Borrowed(
                &self.cmds.main_commands[on_backbuffer_of as usize .. (on_backbuffer_of + 1) as usize]
            ),
            .. Default::default()
        })
    }

    fn discard_backbuffer_resources(&mut self)
    {
        self.cmds.clear_commands();
        self.rt.discard_framebuffers();
    }
    fn on_resize(&mut self, e: &Engine<Self, NL>, new_size: peridot::math::Vector2<usize>)
    {
        self.rt.recreate_framebuffers(e, &new_size);
        self.skybox.recreate_pipeline(e, &self.rt);
        self.cmds.generate_commands(e, &self.skybox, &self.rt, &self.buf, &self.buf_offsets);
    }
}
