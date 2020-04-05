//! http://publications.lib.chalmers.se/records/fulltext/203057/203057.pdf

use peridot::{NativeLinker, Engine, EngineEvents, FeatureRequests};
use peridot_vertex_processing_pack::PvpShaderModules;
use bedrock as br;
use br::VkHandle;
use std::rc::Rc;

#[repr(C, align(4))]
pub struct RGBA32(u8, u8, u8, u8);

pub struct SkyboxPrecomputedTextures
{
    transmittance: peridot::DeviceWorkingTexture2DRef,
    single_scatter: peridot::DeviceWorkingTexture3DRef
}
impl SkyboxPrecomputedTextures
{
    const TRANSMITTANCE_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(128, 32);
    const SINGLE_SCATTER_SIZE: peridot::math::Vector3<u32> = peridot::math::Vector3(32, 64, 32);

    pub fn prealloc(dwt_alloc: &mut peridot::DeviceWorkingTextureAllocator) -> Self
    {
        SkyboxPrecomputedTextures
        {
            transmittance: dwt_alloc.new2d(Self::TRANSMITTANCE_SIZE, peridot::PixelFormat::RGBA64F, br::ImageUsage::STORAGE.sampled()),
            single_scatter: dwt_alloc.new3d(Self::SINGLE_SCATTER_SIZE, peridot::PixelFormat::RGBA64F, br::ImageUsage::STORAGE.sampled())
        }
    }
    pub fn init<NL: NativeLinker>(&self, e: &Engine<Game<NL>, NL>, dwt: &peridot::DeviceWorkingTextureStore)
    {
        let linear_sampler = br::SamplerBuilder::default()
            .addressing(
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge
            )
            .create(e.graphics())
            .expect("Failed to create linear_sampler");
        let tex1_layout = br::DescriptorSetLayout::new(e.graphics(), &[
            br::DescriptorSetLayoutBinding::StorageImage(1, br::ShaderStage::COMPUTE)
        ]).expect("failed to create tex1_layout");
        let tex_r1w1_layout = br::DescriptorSetLayout::new(e.graphics(), &[
            br::DescriptorSetLayoutBinding::CombinedImageSampler(
                1, br::ShaderStage::COMPUTE, &[linear_sampler.native_ptr()]
            ),
            br::DescriptorSetLayoutBinding::StorageImage(1, br::ShaderStage::COMPUTE)
        ]).expect("failed to create tex_r1w1_layout");
        let dp = br::DescriptorPool::new(
            e.graphics(),
            2,
            &[
                br::DescriptorPoolSize(br::DescriptorType::StorageImage, 2),
                br::DescriptorPoolSize(br::DescriptorType::CombinedImageSampler, 1)
            ],
            false
        ).expect("failed to create DescriptorPool");
        let precompute_sets = dp.alloc(&[&tex1_layout, &tex_r1w1_layout])
            .expect("failed to allocate DescriptorSets for Precomputation");
        e.graphics().update_descriptor_sets(&[
            br::DescriptorSetWriteInfo(
                precompute_sets[0], 0, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.transmittance).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[1], 0, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(self.transmittance).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[1], 1, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.single_scatter).native_ptr(), br::ImageLayout::General)
                ])
            )
        ], &[]);

        let inputonly_layout: Rc<_> = br::PipelineLayout::new(e.graphics(), &[&tex1_layout], &[])
            .expect("inputonly_layout creating failed").into();
        let texio_layout: Rc<_> = br::PipelineLayout::new(e.graphics(), &[&tex_r1w1_layout], &[])
            .expect("texio_layout creating failed").into();
        let transmittance_compute = e.load::<peridot::SpirvShaderBlob>("shaders.precompute.transmittance")
            .expect("Failed to load precompute shader for transmittance")
            .instantiate(e.graphics())
            .expect("Compute Shader Instantiation failed");
        let single_scatter_compute = e.load::<peridot::SpirvShaderBlob>("shaders.precompute.single_scatter")
            .expect("Failed to load precompute shader for single scatter")
            .instantiate(e.graphics())
            .expect("Compute shader Instantiation failed");
        let transmittance_compute_pipeline = br::ComputePipelineBuilder::new(&inputonly_layout, br::PipelineShader
        {
            module: &transmittance_compute,
            entry_name: std::ffi::CString::new("main").expect("cstring failed"),
            specinfo: None
        });
        let single_scatter_compute_pipeline = br::ComputePipelineBuilder::new(&texio_layout, br::PipelineShader
        {
            module: &single_scatter_compute,
            entry_name: std::ffi::CString::new("main").expect("cstring failed"),
            specinfo: None
        });
        let compute_pipelines = e.graphics()
            .create_compute_pipelines(&[transmittance_compute_pipeline, single_scatter_compute_pipeline], None)
            .expect("Failed to create precomputation pipelines");
        
        e.submit_commands(|rec|
        {
            let transmittance_tex_area = br::ImageSubref::color(dwt.get(self.transmittance).underlying(), 0..1, 0..1);
            let single_scatter_tex_area = br::ImageSubref::color(dwt.get(self.single_scatter).underlying(), 0..1, 0..1);
            let ib_init = [
                br::ImageMemoryBarrier::new(
                    &transmittance_tex_area,
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write),
                br::ImageMemoryBarrier::new(
                    &single_scatter_tex_area,
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write)
            ];
            let transmittance_fin = [
                br::ImageMemoryBarrier::new(
                    &transmittance_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.write)
            ];
            let single_scatter_fin = [
                br::ImageMemoryBarrier::new(
                    &single_scatter_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.write)
            ];

            rec.pipeline_barrier(
                br::PipelineStageFlags::TOP_OF_PIPE,
                br::PipelineStageFlags::COMPUTE_SHADER,
                false,
                &[], &[], &ib_init
            ).bind_compute_pipeline_pair(&compute_pipelines[0], &inputonly_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[0]], &[])
            .dispatch(Self::TRANSMITTANCE_SIZE.0 / 32, Self::TRANSMITTANCE_SIZE.1 / 32, 1)
            .pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::COMPUTE_SHADER,
                false,
                &[], &[], &transmittance_fin
            )
            .bind_compute_pipeline_pair(&compute_pipelines[1], &texio_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[1]], &[])
            .dispatch(Self::SINGLE_SCATTER_SIZE.0 / 8, Self::SINGLE_SCATTER_SIZE.1 / 8, Self::SINGLE_SCATTER_SIZE.2 / 8)
            .pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::FRAGMENT_SHADER,
                false,
                &[], &[], &single_scatter_fin
            );
        }).expect("Dispatch Precomputation failed");
    }
}

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

        let main_vps = main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::new(&empty_layout, (&drt.rp, 0), main_vps)
            .viewport_scissors(
                br::DynamicArrayState::Static(&full_viewports),
                br::DynamicArrayState::Static(&full_scissors))
            .multisample_state(Some(mp))
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

        let main_vps = self.main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::new(self.pipeline.layout(), (&drt.rp, 0), main_vps)
            .viewport_scissors(
                br::DynamicArrayState::Static(&full_viewports),
                br::DynamicArrayState::Static(&full_scissors))
            .multisample_state(Some(mp))
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
    skybox_precomputed: SkyboxPrecomputedTextures,
    skybox: SkyboxRenderer,
    buf: peridot::FixedMemory,
    buf_offsets: FixedBufferOffsets,
    _precompute_textures: peridot::DeviceWorkingTextureStore,
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

        let mut precompute_textures = peridot::DeviceWorkingTextureAllocator::new();
        let skybox_precomputed = SkyboxPrecomputedTextures::prealloc(&mut precompute_textures);
        let precompute_textures = precompute_textures.alloc(e.graphics())
            .expect("Allocating Precomputed Textures failed");

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

        skybox_precomputed.init(e, &precompute_textures);
        let cmds = RenderCommands::new(e);

        e.submit_commands(|r|
        {
            tfb.sink_transfer_commands(r);
            tfb.sink_graphics_ready_commands(r);
        }).expect("Failed to resource memory initialization");

        Game
        {
            rt,
            skybox_precomputed,
            skybox,
            buf,
            buf_offsets,
            _precompute_textures: precompute_textures,
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
