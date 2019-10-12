
use peridot::{Engine, EngineEvents, NativeLinker};
use bedrock as br;
use bedrock::traits::*;
use peridot::{
    RenderPassTemplates, PixelFormat, CommandBundle, CBSubmissionType,
    math::{Vector2, Vector2F32}, Buffer, BufferPrealloc, BufferContent, MemoryBadget, TransferBatch,
    DescriptorSetUpdateBatch
};
use peridot_vertex_processing_pack::PvpShaderModules;
use std::marker::PhantomData;
use std::borrow::Cow;
use std::time::Duration;

#[macro_use] extern crate log;

pub struct ShadingHeaders
{
    dsl_ub1: br::DescriptorSetLayout,
    _descriptor_pool: br::DescriptorPool, descriptors: Vec<br::vk::VkDescriptorSet>,
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
        let dsl_ub1 = br::DescriptorSetLayout::new(e.graphics(), &br::DSLBindings
        {
            uniform_buffer: Some((0, 1, br::ShaderStage::FRAGMENT)),
            .. br::DSLBindings::empty()
        })?;
        let layout = br::PipelineLayout::new(&e.graphics(), &[&dsl_ub1], &[(br::ShaderStage::VERTEX, 0 .. 4)])?;
        let renderpass = RenderPassTemplates::single_render(bb_format as _).create(&e.graphics())?;

        let descriptor_pool = br::DescriptorPool::new(e.graphics(), 1, &[
            br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1)
        ], false)?;
        let descriptors = descriptor_pool.alloc(&[&dsl_ub1])?;

        Ok(ShadingHeaders
        {
            layout, renderpass, dsl_ub1, descriptors,
            _descriptor_pool: descriptor_pool
        })
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
    buffer: Buffer, vbuf_offset: u64, dynbuf_offset: u64,
    dynbuffer: Buffer
}
impl Memory
{
    pub fn new<NL: NativeLinker>(e: &Engine<Game<NL>, NL>) -> Self
    {
        let mut bp = BufferPrealloc::new(e.graphics());
        let mut bp_dyn = BufferPrealloc::new(e.graphics());
        bp_dyn.add(BufferContent::uniform::<DynamicParams>());
        let dynbuf_offset = bp.add(BufferContent::uniform::<DynamicParams>());
        let vertices_start = bp.add(BufferContent::vertices::<[f32; 2]>(4));

        let buffer = bp.build_transferred().expect("creating buffer object");
        let dynbuffer = bp_dyn.build_upload().expect("creating dynamic buffer object");
        let stg_buffer = bp.build_upload().expect("creating staging buffer object");

        let (mut mb, mut mb_stg) = (MemoryBadget::new(e.graphics()), MemoryBadget::new(e.graphics()));
        let mut mb_dyn = MemoryBadget::new(e.graphics());
        mb.add(buffer); mb_stg.add(stg_buffer); mb_dyn.add(dynbuffer);
        let buffer = mb.alloc().expect("allocating device memory").pop().expect("objectless").unwrap_buffer();
        let dynbuffer = mb_dyn.alloc_upload().expect("allocating dynamic memory")
            .pop().expect("objectless").unwrap_buffer();
        let stg_buffer = mb_stg.alloc_upload().expect("allocating staging memory")
            .pop().expect("objectless").unwrap_buffer();
        
        stg_buffer.guard_map(bp.total_size(), |m| unsafe
        {
            *m.get_mut(0) = DynamicParams { time_sec: 0.0 };
            m.slice_mut::<[f32; 2]>(vertices_start as _, 4).clone_from_slice(&[
                [-1.0, -1.0], [1.0, -1.0], [-1.0, 1.0], [1.0, 1.0]
            ]);
        }).expect("Staging Data");

        let mut tfb = TransferBatch::new();
        tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, bp.total_size());
        tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_INPUT.fragment_shader(), &buffer,
            0 .. 0 + bp.total_size(), br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::UNIFORM_READ);
        e.submit_commands(|r|
        {
            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
        }).expect("Submission of Buffer Initialization Commands");

        Memory { buffer, vbuf_offset: vertices_start, dynbuffer, dynbuf_offset }
    }
    pub fn build_dyn_batches(&self, sh_headers: &ShadingHeaders,
        tfb: &mut TransferBatch, dub: &mut DescriptorSetUpdateBatch)
    {
        let dynbuf_size = std::mem::size_of::<DynamicParams>();
        tfb.add_copying_buffer((&self.dynbuffer, 0), (&self.buffer, self.dynbuf_offset), dynbuf_size as _);
        tfb.add_buffer_graphics_ready(br::PipelineStageFlags::FRAGMENT_SHADER, &self.buffer,
            self.dynbuf_offset .. self.dynbuf_offset + dynbuf_size as u64,
            br::AccessFlags::UNIFORM_READ);
    
        dub.write(sh_headers.descriptors[0], 0, br::DescriptorUpdateInfo::UniformBuffer(vec![
            (self.buffer.native_ptr(), self.dynbuf_offset as usize .. self.dynbuf_offset as usize + dynbuf_size)
        ]));
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
    framebuffer: Vec<br::Framebuffer>, render_cmd: CommandBundle, update_cmd: CommandBundle,
    playtime: f32,
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

        let mut dub = DescriptorSetUpdateBatch::new();
        let update_cmd = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, 1)
            .expect("Alloc UpdateCmd");
        {
            let mut tfb_u = TransferBatch::new();
            memory.build_dyn_batches(&sh_headers, &mut tfb_u, &mut dub);
            let mut cr = update_cmd[0].begin().expect("Beginning update commands");
            tfb_u.sink_transfer_commands(&mut cr);
            tfb_u.sink_graphics_ready_commands(&mut cr);
        }
        dub.submit(e.graphics());

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
            sh_headers, shading, framebuffer, render_cmd, update_cmd, memory, ph: PhantomData,
            playtime: 0.0
        }
    }

    fn update(&mut self, e: &Engine<Self, NL>, on_backbuffer_of: u32, delta_time: Duration)
        -> (Option<br::SubmissionBatch>, br::SubmissionBatch)
    {
        self.playtime += delta_time.as_secs() as f32 + delta_time.subsec_micros() as f32 / 1000_000.0;
        self.memory.dynbuffer.guard_map(std::mem::size_of::<DynamicParams>() as _, |m| unsafe
        {
            m.get_mut::<DynamicParams>(0).time_sec = self.playtime;
        }).expect("Updating Params");
        let update_batch = br::SubmissionBatch
        {
            command_buffers: Cow::Borrowed(&self.update_cmd[0..1]), .. Default::default()
        };
        (Some(update_batch), br::SubmissionBatch
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
        info!("Rendering Viewport: {}x{}", frame_size.0, frame_size.1);
        rec.set_viewport(0, &[br::vk::VkViewport
        {
            width: frame_size.0 as f32 * 0.5, height: frame_size.1 as f32 * 0.5, .. Default::default()
        }]);
        rec.set_scissor(0, &[render_rect.clone()]);
        rec.push_graphics_constant(br::ShaderStage::VERTEX, 0, &(frame_size.0 as f32 / frame_size.1 as f32));
        rec.bind_graphics_descriptor_sets(0, &sh_headers.descriptors, &[]);
        memory.draw_rect(rec);
        rec.end_render_pass();
    }
}
impl<NL: NativeLinker> peridot::FeatureRequests for Game<NL>
{
    
}

#[repr(C)]
pub struct DynamicParams
{
    pub time_sec: f32
}
