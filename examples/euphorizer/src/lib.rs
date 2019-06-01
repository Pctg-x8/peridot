
use peridot::{Engine, EngineEvents, NativeLinker};
use bedrock as br;
use peridot::{
    PvpShaderModules, RenderPassTemplates, PixelFormat, CommandBundle, CBSubmissionType,
    math::{Vector2, Vector2F32}, Buffer, BufferPrealloc, BufferContent, MemoryBadget, TransferBatch
};
use std::marker::PhantomData;
use std::borrow::Cow;
use std::time::Duration;

pub struct ShadingHeaders
{
    layout: br::PipelineLayout, renderpass: br::RenderPass
}
pub struct Shading
{
    shader_mods: PvpShaderModules<'static>, pipe: br::Pipeline
}
impl ShadingHeaders
{
    pub fn new<NL: NativeLinker>(e: &Engine<Game<NL>, NL>, bb_format: PixelFormat) -> br::Result<Self>
    {
        let layout = br::PipelineLayout::new(&e.graphics(), &[], &[(br::ShaderStage::VERTEX, 0 .. 4)])?;
        let renderpass = RenderPassTemplates::single_render(bb_format as _).create(&e.graphics())?;

        Ok(ShadingHeaders { layout, renderpass })
    }
}
impl Shading
{
    pub fn new<NL: NativeLinker>(e: &Engine<Game<NL>, NL>, headers: &ShadingHeaders, viewport_size: &Vector2F32)
        -> br::Result<Self>
    {
        let shader_mods = PvpShaderModules::new(&e.graphics(), e.load("main").expect("Loading Shader"))?;
        let pipe = br::GraphicsPipelineBuilder::new(&headers.layout, (&headers.renderpass, 0))
            .vertex_processing(shader_mods.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP))
            .fixed_viewport_scissors(br::DynamicArrayState::Dynamic(1), br::DynamicArrayState::Dynamic(1))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .create(&e.graphics(), None).expect("Create GraphicsPipeline");
        
        Ok(Shading { shader_mods, pipe })
    }
}

pub struct Memory
{
    buffer: Buffer, vbuf_offset: u64
}
impl Memory
{
    pub fn new<NL: NativeLinker>(e: &Engine<Game<NL>, NL>) -> Self
    {
        let mut bp = BufferPrealloc::new(e.graphics());
        let vertices_start = bp.add(BufferContent::vertices::<[f32; 2]>(4));

        let buffer = bp.build_transferred().expect("creating buffer object");
        let stg_buffer = bp.build_upload().expect("creating staging buffer object");

        let (mut mb, mut mb_stg) = (MemoryBadget::new(e.graphics()), MemoryBadget::new(e.graphics()));
        mb.add(buffer); mb_stg.add(stg_buffer);
        let buffer = mb.alloc().expect("allocating device memory").pop().expect("objectless").unwrap_buffer();
        let stg_buffer = mb_stg.alloc_upload().expect("allocating staging memory")
            .pop().expect("objectless").unwrap_buffer();
        
        stg_buffer.guard_map(bp.total_size(), |m| unsafe
        {
            m.slice_mut::<[f32; 2]>(vertices_start as _, 4).clone_from_slice(&[
                [-1.0, -1.0], [1.0, -1.0], [-1.0, 1.0], [1.0, 1.0]
            ]);
        }).expect("Staging Data");

        let mut tfb = TransferBatch::new();
        tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, bp.total_size());
        tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_INPUT, &buffer,
            vertices_start .. vertices_start + bp.total_size(), br::AccessFlags::VERTEX_ATTRIBUTE_READ);
        e.submit_commands(|r|
        {
            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
        }).expect("Submission of Buffer Initialization Commands");

        Memory { buffer, vbuf_offset: vertices_start }
    }

    pub fn draw_rect(&self, cmd: &mut br::CmdRecord)
    {
        cmd.bind_vertex_buffers(0, &[(&self.buffer, self.vbuf_offset as _)]);
        cmd.draw(4, 1, 0, 0);
    }
}

pub struct Game<NL: NativeLinker>
{
    sh_headers: ShadingHeaders, shading: Shading, memory: Memory,
    framebuffer: Vec<br::Framebuffer>, render_cmd: CommandBundle,
    ph: PhantomData<*const NL>
}
impl<NL: NativeLinker> Game<NL>
{
    pub const NAME: &'static str = "Peridot TechShowcase #1: Euphorizer";
    pub const VERSION: (u32, u32, u32) = (1, 0, 0);
}
impl<NL: NativeLinker> EngineEvents<NL> for Game<NL>
{
    fn init(e: &Engine<Self, NL>) -> Self
    {
        let bb_format = e.backbuffer_format();
        let sh_headers = ShadingHeaders::new(e, bb_format).expect("Creating Shading Headers");
        let shading = Shading::new(e, &sh_headers, &Vector2(640.0, 480.0)).expect("Creating Shading Pipeline");

        let memory = Memory::new(e);

        let framebuffer =
            e.backbuffers().iter().map(|b| br::Framebuffer::new(&sh_headers.renderpass, &[b], b.size(), 1))
            .collect::<Result<Vec<_>, _>>().expect("Creating Framebuffer");
        let render_cmd = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, framebuffer.len())
            .expect("Alloc CmdBundle");
        for (cb, f) in render_cmd.iter().zip(&framebuffer)
        {
            Self::commands(&mut cb.begin().expect("Beginning Recording commands"), &sh_headers, &shading, &memory, f);
        }

        Game
        {
            sh_headers, shading, framebuffer, render_cmd, memory, ph: PhantomData
        }
    }

    fn update(&mut self, e: &Engine<Self, NL>, on_backbuffer_of: u32, delta_time: Duration)
        -> (Option<br::SubmissionBatch>, br::SubmissionBatch)
    {
        (None, br::SubmissionBatch
        { 
            command_buffers: Cow::Borrowed(&self.render_cmd[on_backbuffer_of as usize..on_backbuffer_of as usize + 1]),
            .. Default::default()
        })
    }
    
    fn discard_backbuffer_resources(&mut self)
    {
        self.render_cmd.reset().expect("Resetting RenderCommands");
        self.framebuffer.clear();
    }
    fn on_resize(&mut self, e: &Engine<Self, NL>, _: Vector2<usize>)
    {
        self.framebuffer = e.backbuffers().iter()
            .map(|b| br::Framebuffer::new(&self.sh_headers.renderpass, &[b], b.size(), 1))
            .collect::<Result<Vec<_>, _>>().expect("Creating Framebuffer");
        for (cb, f) in self.render_cmd.iter().zip(&self.framebuffer)
        {
            Self::commands(&mut cb.begin().expect("Beginning Recording commands"),
                &self.sh_headers, &self.shading, &self.memory, f);
        }
    }
}
impl<NL: NativeLinker> Game<NL>
{
    fn commands(rec: &mut br::CmdRecord, sh_headers: &ShadingHeaders, shading: &Shading,
        memory: &Memory, framebuffer: &br::Framebuffer)
    {
        let frame_size = framebuffer.size();
        let render_rect = br::vk::VkRect2D
        {
            extent: br::vk::VkExtent2D { width: frame_size.0 as _, height: frame_size.1 as _ },
            .. Default::default()
        };

        rec.begin_render_pass(&sh_headers.renderpass, framebuffer, render_rect.clone(),
            &[br::ClearValue::Color([0.0; 4])], true);
        rec.bind_graphics_pipeline_pair(&shading.pipe, &sh_headers.layout);
        rec.set_viewport(0, &[br::vk::VkViewport
        {
            width: frame_size.0 as _, height: frame_size.1 as _, .. Default::default()
        }]);
        rec.set_scissor(0, &[render_rect.clone()]);
        rec.push_graphics_constant(br::ShaderStage::VERTEX, 0, &(frame_size.0 as f32 / frame_size.1 as f32));
        memory.draw_rect(rec);
        rec.end_render_pass();
    }
}
