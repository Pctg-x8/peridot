
use std::marker::PhantomData;
extern crate bedrock as br; use br::traits::*;
use peridot::{CommandBundle, LayoutedPipeline, Buffer, BufferPrealloc, BufferContent, ModelData,
    TransferBatch, DescriptorSetUpdateBatch, CBSubmissionType, RenderPassTemplates, DefaultRenderCommands,
    PvpShaderModules, vg, SpecConstantStorage, PlatformInputProcessor, FixedMemory, TextureInitializationGroup,
    ShaderBinary};
use peridot::math::{Vector2, Vector2F32};
use std::rc::Rc;
use std::borrow::Cow;
use std::mem::size_of;
// use peridot::vg::{PathBuilder, FlatPathBuilder};

#[derive(SpecConstantStorage)] #[repr(C)]
pub struct VgRendererFragmentFixedColor {
    r: f32, g: f32, b: f32, a: f32
}
#[derive(SpecConstantStorage)] #[repr(C)]
pub struct InstancedLineShaderGenParams { spacing: f32, clone_direction: i32, aspect_wh: f32 }

#[repr(C)]
pub struct ScreenProps { scaling: f32, pad: f32, offset: Vector2F32 }

#[repr(C)]
pub struct NodeRenderParam
{
    offset: [f32; 2], size: [f32; 2],
    tint_color: [f32; 4]
}
const MAX_RENDERABLE_NODE_INSTANCES: usize = 32;

const VIB_POS2: &[br::vk::VkVertexInputBindingDescription] = &[
    br::vk::VkVertexInputBindingDescription
    {
        binding: 0, stride: size_of::<[f32; 2]>() as _, inputRate: br::vk::VK_VERTEX_INPUT_RATE_VERTEX
    }
];
const VIA_POS2: &[br::vk::VkVertexInputAttributeDescription] = &[
    br::vk::VkVertexInputAttributeDescription
    {
        binding: 0, location: 0, format: br::vk::VK_FORMAT_R32G32_SFLOAT, offset: 0
    }
];

struct GridModel;
impl ModelData for GridModel {
    type PreallocOffsetType = usize;
    type RendererParams = usize;

    fn prealloc(&self, prealloc: &mut BufferPrealloc) -> usize {
        prealloc.add(BufferContent::vertex::<[Vector2F32; 4]>()) as _
    }
    fn stage_data_into(&self, mem: &br::MappedMemoryRange, offset: usize) -> usize {
        unsafe {
            mem.slice_mut(offset, 4).clone_from_slice(&[
                Vector2(0.0f32, -1.0), Vector2(0.0, 1.0),
                Vector2(-1.0, 0.0), Vector2(1.0, 0.0)
            ]);
        }
        offset
    }
}

const BACK_COLOR: [f32; 4] = [0.2, 0.18, 0.2, 1.0];

use std::collections::BTreeMap;
pub struct DescriptorManager {
    set_layouts: Vec<br::DescriptorSetLayout>, _pool: br::DescriptorPool, sets: Vec<br::vk::VkDescriptorSet>
}
impl DescriptorManager {
    pub fn new(dev: &br::Device, bindings: &[br::DSLBindings], set_allocations: &[usize]) -> br::Result<Self> {
        let set_layouts = bindings.iter().map(|b| br::DescriptorSetLayout::new(dev, b)).collect::<Result<Vec<_>, _>>()?;
        let mut allocations_per_type = BTreeMap::new();
        for b in bindings {
            if let Some((_, count, _, _)) = b.sampler {
                *allocations_per_type.entry(br::DescriptorType::Sampler).or_insert(0) += count;
            }
            if let Some((_, count, _, _)) = b.combined_image_sampler {
                *allocations_per_type.entry(br::DescriptorType::CombinedImageSampler).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.sampled_image {
                *allocations_per_type.entry(br::DescriptorType::SampledImage).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.storage_image {
                *allocations_per_type.entry(br::DescriptorType::StorageImage).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.uniform_texel_buffer {
                *allocations_per_type.entry(br::DescriptorType::UniformTexelBuffer).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.storage_texel_buffer {
                *allocations_per_type.entry(br::DescriptorType::StorageTexelBuffer).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.uniform_buffer {
                *allocations_per_type.entry(br::DescriptorType::UniformBuffer).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.storage_buffer {
                *allocations_per_type.entry(br::DescriptorType::StorageBuffer).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.uniform_buffer_dynamic {
                *allocations_per_type.entry(br::DescriptorType::UniformBufferDynamic).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.storage_buffer_dynamic {
                *allocations_per_type.entry(br::DescriptorType::StorageBufferDynamic).or_insert(0) += count;
            }
            if let Some((_, count, _)) = b.input_attachment {
                *allocations_per_type.entry(br::DescriptorType::InputAttachment).or_insert(0) += count;
            }
        }
        let sizes_per_type = allocations_per_type.into_iter()
            .filter_map(|(k, v)| if v > 0 { br::DescriptorPoolSize(k, v).into() } else { None })
            .collect::<Vec<_>>();
        let pool = br::DescriptorPool::new(dev, set_allocations.len() as _, &sizes_per_type, false)?;
        let used_layouts = set_allocations.iter().map(|&i| &set_layouts[i]).collect::<Vec<_>>();
        let sets = pool.alloc(&used_layouts)?;

        return Ok(DescriptorManager { set_layouts, _pool: pool, sets })
    }

    pub fn layouts(&self) -> &[br::DescriptorSetLayout] { &self.set_layouts }
}
impl std::ops::Deref for DescriptorManager {
    type Target = [br::vk::VkDescriptorSet];
    fn deref(&self) -> &[br::vk::VkDescriptorSet] { &self.sets }
}

#[repr(u32)] pub enum AxisInputIndices {
    Magnification, ScrollH, ScrollV
}

pub struct FixedMemoryInitializer
{
    grid_offset: usize, ipos2_rect_offset: usize,
    vg_offset: peridot::VgContextPreallocOffsets, vg_context: vg::Context
}
pub struct FixedMemoryOffsets
{
    grid_offset: usize, ipos2_rect_offset: usize,
    vg_renderer_params: peridot::VgRendererParams
}
impl peridot::FixedBufferInitializer for FixedMemoryInitializer
{
    type StagingResult = FixedMemoryOffsets;

    fn stage_data(&mut self, m: &br::MappedMemoryRange) -> FixedMemoryOffsets
    {
        let grid_offset = GridModel.stage_data_into(m, self.grid_offset);
        let vg_renderer_params = self.vg_context.stage_data_into(m, self.vg_offset.clone());
        unsafe
        {
            m.slice_mut(self.ipos2_rect_offset, 4).clone_from_slice(&[
                [-1.0f32, -1.0f32], [1.0f32, -1.0f32], [-1.0f32, 1.0f32], [1.0f32, 1.0f32]
            ]);
        }
        
        FixedMemoryOffsets
        {
            grid_offset, vg_renderer_params, ipos2_rect_offset: self.ipos2_rect_offset
        }
    }
    fn buffer_graphics_ready(&self, tfb: &mut TransferBatch, buf: &Buffer, range: std::ops::Range<u64>)
    {
        tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_INPUT.vertex_shader(), buf, range,
            br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::INDEX_READ | br::AccessFlags::SHADER.read);
    }
}

pub struct Game<PL: peridot::NativeLinker> {
    renderpass: br::RenderPass, framebuffers: Vec<br::Framebuffer>, render_cb: CommandBundle,
    _bufview: br::BufferView, descs: DescriptorManager, gp_backgrid: (LayoutedPipeline, LayoutedPipeline),
    gp_node_render: LayoutedPipeline,
    fixed_memory: FixedMemory,
    fixed_offsets: FixedMemoryOffsets,
    vg_renderer_exinst: peridot::VgRendererExternalInstances,
    ph: PhantomData<*const PL>
}
impl<PL: peridot::NativeLinker> Game<PL> {
    pub const NAME: &'static str = "Peridot DataFlowGraph Editor";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<PL: peridot::NativeLinker> peridot::FeatureRequests for Game<PL>
{
    const USE_STORAGE_BUFFERS_IN_VERTEX_SHADER: bool = true;
}
impl<PL: peridot::NativeLinker> peridot::EngineEvents<PL> for Game<PL> {
    fn init(e: &mut peridot::Engine<Self, PL>) -> Self {
        let font = vg::Font::best_match(&[vg::FamilyName::SansSerif], &vg::FontProperties::new(), 12.0)
            .expect("No Fonts");
        let mut ctx = vg::Context::new();
        ctx.text(&font, "Peridot DataFlowGraph Editor - Untitled.dfg");

        e.input_mut().link_axis(peridot::AxisEventSource::Magnification, AxisInputIndices::Magnification as _);
        e.input_mut().link_axis(peridot::AxisEventSource::ScrollHorizontal, AxisInputIndices::ScrollH as _);
        e.input_mut().link_axis(peridot::AxisEventSource::ScrollVertical, AxisInputIndices::ScrollV as _);

        let mut bp = BufferPrealloc::new(e.graphics());
        let grid_offs = GridModel.prealloc(&mut bp);
        let ipos2_rect_offset = bp.add(BufferContent::vertex::<[[f32; 2]; 4]>());
        let vg_offs = ctx.prealloc(&mut bp);
        let mut bp_mut = BufferPrealloc::new(e.graphics());
        let screen_props_placement = bp_mut.add(BufferContent::uniform::<ScreenProps>());
        let node_render_params_placement =
            bp_mut.add(BufferContent::storage::<[NodeRenderParam; MAX_RENDERABLE_NODE_INSTANCES]>());

        let mut tfb = TransferBatch::new();
        let mut fbinit = FixedMemoryInitializer
        {
            grid_offset: grid_offs, vg_offset: vg_offs, vg_context: ctx, ipos2_rect_offset: ipos2_rect_offset as _
        };
        let (fixed_memory, fixed_offsets) = FixedMemory::new(e.graphics(),
            bp, bp_mut, TextureInitializationGroup::new(e.graphics()),
            &mut fbinit, &mut tfb).expect("Allocating for Fixed Memory");
        
        fixed_memory.mut_buffer.0.guard_map(fixed_memory.mut_buffer.1, |m| unsafe
        {
            *m.get_mut(screen_props_placement as _) = ScreenProps {
                scaling: 0.01, pad: 0.0, offset: Vector2(5.0, 2.5)
            };
            m.get_mut::<[_; MAX_RENDERABLE_NODE_INSTANCES]>(node_render_params_placement as _)[0] = NodeRenderParam
            {
                offset: [0.0; 2], size: [128.0, 48.0], tint_color: [1.0, 1.0, 0.0, 1.0]
            };
        }).expect("MutMem Initialization");
        tfb.add_copying_buffer((&fixed_memory.mut_buffer.0, 0),
            (&fixed_memory.buffer.0, fixed_memory.mut_buffer_placement), fixed_memory.mut_buffer.1);
        tfb.add_buffer_graphics_ready(br::PipelineStageFlags::VERTEX_SHADER, &fixed_memory.buffer.0,
            fixed_memory.range_in_mut_buffer(0 .. fixed_memory.mut_buffer.1),
            br::AccessFlags::SHADER.read);

        let bufview = fixed_memory.buffer.0.create_view(br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
            fixed_offsets.vg_renderer_params.transforms_byterange()).expect("Creating Transform BufferView");

        e.submit_commands(|rec| {
            tfb.sink_transfer_commands(rec);
            tfb.sink_graphics_ready_commands(rec);
        }).expect("ImmResource Initialization");

        let screen_size = e.backbuffers()[0].size().clone();
        let renderpass = RenderPassTemplates::single_render(unsafe { peridot::PixelFormat::force_cast(e.backbuffer_format()) })
            .create(&e.graphics()).expect("RenderPass Creation");
        let framebuffers = e.backbuffers().iter().map(|v| br::Framebuffer::new(&renderpass, &[v], &screen_size, 1))
            .collect::<Result<Vec<_>, _>>().expect("Framebuffer Creation");
        
        let descs = DescriptorManager::new(&e.graphics(), &[
            br::DSLBindings {
                uniform_texel_buffer: (0, 1, br::ShaderStage::VERTEX).into(), .. br::DSLBindings::empty()
            },
            br::DSLBindings {
                uniform_buffer: (0, 1, br::ShaderStage::VERTEX).into(), .. br::DSLBindings::empty()
            },
            br::DSLBindings {
                storage_buffer: (0, 1, br::ShaderStage::VERTEX).into(), .. br::DSLBindings::empty()
            }
        ], &[0, 1, 2]).expect("DescriptorSet Initialization");

        let mut dub = DescriptorSetUpdateBatch::new();
        dub.write(descs[0], 0, br::DescriptorUpdateInfo::UniformTexelBuffer(vec![bufview.native_ptr()]));
        dub.write(descs[1], 0, br::DescriptorUpdateInfo::UniformBuffer(vec![
            (fixed_memory.buffer.0.native_ptr(),
                fixed_memory.range_in_mut_buffer(
                    screen_props_placement as usize .. screen_props_placement as usize + size_of::<ScreenProps>()))
        ]));
        dub.write(descs[2], 0, br::DescriptorUpdateInfo::StorageBuffer(vec![
            (fixed_memory.buffer.0.native_ptr(), fixed_memory.range_in_mut_buffer(
                node_render_params_placement as usize .. node_render_params_placement as usize +
                    size_of::<[NodeRenderParam; MAX_RENDERABLE_NODE_INSTANCES]>()
            ))
        ]));
        dub.submit(e.graphics());

        let vp = [br::vk::VkViewport {
            width: screen_size.0 as _, height: screen_size.1 as _, x: 0.0, y: 0.0,
            minDepth: 0.0, maxDepth: 1.0
        }];
        let sc = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D::default(),
            extent: br::vk::VkExtent2D { width: screen_size.0, height: screen_size.1 }
        }];
        let aspect_wh = screen_size.0 as f32 / screen_size.1 as f32;

        let shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.interiorColorFixed")
            .expect("Loading PvpContainer")).expect("Creating Shader");
        let curve_shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.curveColorFixed")
            .expect("Loading CurveShader")).expect("Creating CurveShader");
        let instline_shader = PvpShaderModules::new(&e.graphics(), e.load("shaders.instancedLine_constantColor")
            .expect("Loading InstancedLineConstantColorShader")).expect("Creating InstancedLineConstantColorShader");
        let noderender_v_shader = e.load::<ShaderBinary>("shaders.nodeRenderer_v").expect("Loading nodeRenderer_v")
            .instantiate(&e.graphics()).expect("Instantiating nodeRenderer_v");
        let noderender_f_shader = e.load::<ShaderBinary>("shaders.nodeRenderer_f").expect("Loading nodeRenderer_f")
            .instantiate(&e.graphics()).expect("Instantiating nodeRenderer_f");
        let pl: Rc<_> = br::PipelineLayout::new(&e.graphics(),
            &[&descs.layouts()[0]], &[(br::ShaderStage::VERTEX, 0 .. 4 * 4)])
            .expect("Create PipelineLayout").into();
        let pl_backgrid: Rc<_> = br::PipelineLayout::new(&e.graphics(),
            &[&descs.layouts()[1]], &[(br::ShaderStage::FRAGMENT, 0 .. 4 * 4)])
            .expect("Create PipelineLayout for GridRendering").into();
        let pl_node_render: Rc<_> = br::PipelineLayout::new(&e.graphics(),
            &[&descs.layouts()[1], &descs.layouts()[2]], &[(br::ShaderStage::VERTEX, 0 .. 4 * 2)])
            .expect("Create PipelineLayout for NodeRender").into();
        let spc_map = vec![
            br::vk::VkSpecializationMapEntry { constantID: 0, offset: 0, size: 4 },
            br::vk::VkSpecializationMapEntry { constantID: 1, offset: 4, size: 4 }
        ];
        let mut interior_vertex_processing = shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        let mut curve_vertex_processing = curve_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST);
        interior_vertex_processing.mod_vertex_shader().specinfo =
            Some((spc_map.clone(), br::DynamicDataCell::from_slice(&peridot::vg::renderer_pivot::LEFT_TOP)));
        curve_vertex_processing.mod_vertex_shader().specinfo =
            Some((spc_map.clone(), br::DynamicDataCell::from_slice(&peridot::vg::renderer_pivot::LEFT_TOP)));
        let mut backgrid_vertex_processing_x = instline_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST);
        let mut backgrid_vertex_processing_y = instline_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST);
        let backgrid_vsh_params_x = InstancedLineShaderGenParams { spacing: 10.0, clone_direction: 0, aspect_wh };
        let backgrid_vsh_params_y = InstancedLineShaderGenParams { clone_direction: 1, .. backgrid_vsh_params_x };
        backgrid_vertex_processing_x.mod_vertex_shader().specinfo = Some(backgrid_vsh_params_x.as_pair());
        backgrid_vertex_processing_y.mod_vertex_shader().specinfo = Some(backgrid_vsh_params_y.as_pair());
        
        interior_vertex_processing.mod_fragment_shader().expect("fragment shader not exist?").specinfo =
            Some(VgRendererFragmentFixedColor { r: 1.0, g: 1.0, b: 1.0, a: 1.0 }.as_pair());
        curve_vertex_processing.mod_fragment_shader().expect("fragment shader not exist?").specinfo =
            Some(VgRendererFragmentFixedColor { r: 1.0, g: 1.0, b: 1.0, a: 1.0 }.as_pair());
        let mut gpb = br::GraphicsPipelineBuilder::new(&pl, (&renderpass, 0));
        gpb.vertex_processing(interior_vertex_processing)
            .fixed_viewport_scissors(br::DynamicArrayState::Static(&vp), br::DynamicArrayState::Static(&sc))
            .add_attachment_blend(br::AttachmentColorBlendState::premultiplied());
        let gp = LayoutedPipeline::combine(gpb.create(&e.graphics(), None).expect("Create GraphicsPipeline"), &pl);
        gpb.vertex_processing(curve_vertex_processing);
        let gp_curve = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline of CurveRender"), &pl);
        let vg_renderer_exinst = peridot::VgRendererExternalInstances {
            interior_pipeline: gp, curve_pipeline: gp_curve, transform_buffer_descriptor_set: descs[0],
            target_pixels: Vector2(screen_size.0 as _, screen_size.1 as _)
        };

        gpb.vertex_processing(backgrid_vertex_processing_x).layout(&pl_backgrid);
        let gp_backgrid_x = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline of BackgridRender-X"), &pl_backgrid);
        gpb.vertex_processing(backgrid_vertex_processing_y);
        let gp_backgrid_y = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline of BackgridRender-Y"), &pl_backgrid);
        
        let shader_default_ep = std::ffi::CString::new("main").expect("building CString shader ep");
        let mut vps_node_render = br::VertexProcessingStages::new(
            br::PipelineShader { module: &noderender_v_shader, entry_name: shader_default_ep.clone(), specinfo: None },
            &VIB_POS2, &VIA_POS2, br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
        );
        vps_node_render.fragment_shader(br::PipelineShader
        {
            module: &noderender_f_shader, entry_name: shader_default_ep.clone(), specinfo: None
        });
        gpb.layout(&pl_node_render).vertex_processing(vps_node_render);
        let gp_node_render = LayoutedPipeline::combine(gpb.create(&e.graphics(), None)
            .expect("Create GraphicsPipeline for NodeRender"), &pl_node_render);

        let render_cb = CommandBundle::new(&e.graphics(), CBSubmissionType::Graphics, framebuffers.len())
            .expect("Creating RenderCB");
        for (r, f) in render_cb.iter().zip(&framebuffers) {
            let mut cbr = r.begin().expect("Start Recoding CB");
            cbr.begin_render_pass(&renderpass, f, f.size().clone().into(),
                &[br::ClearValue::Color(BACK_COLOR)], true);
            
            cbr.bind_vertex_buffers(0, &[(&fixed_memory.buffer.0, fixed_offsets.grid_offset)]);
            gp_backgrid_x.bind(&mut cbr);
            cbr.push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &[1.0f32, 1.0, 1.0, 0.5]);
            cbr.bind_graphics_descriptor_sets(0, &descs[1..2], &[]);
            cbr.draw(2, 100, 0, 0);
            gp_backgrid_y.bind(&mut cbr);
            cbr.draw(2, 100, 2, 0);
            gp_node_render.bind(&mut cbr);
            cbr.bind_graphics_descriptor_sets(0, &descs[1..3], &[]);
            cbr.bind_vertex_buffers(0, &[(&fixed_memory.buffer.0, fixed_offsets.ipos2_rect_offset)]);
            cbr.push_graphics_constant(br::ShaderStage::VERTEX, 0, &[screen_size.0 as f32, screen_size.1 as f32]);
            cbr.draw(4, 1, 0, 0);
            
            fixed_offsets.vg_renderer_params.default_render_commands(e,
                &mut cbr, &fixed_memory.buffer.0, &vg_renderer_exinst);
            cbr.end_render_pass();
        }

        Game {
            ph: PhantomData, renderpass, framebuffers, _bufview: bufview,
            descs, render_cb, vg_renderer_exinst,
            fixed_memory, fixed_offsets,
            gp_backgrid: (gp_backgrid_x, gp_backgrid_y), gp_node_render
        }
    }

    fn update(&mut self, e: &peridot::Engine<Self, PL>, on_backbuffer_of: u32, _delta_time: std::time::Duration)
            -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        /*println!("axs: {} {} {}",
            e.input().query_axis(AxisInputIndices::Magnification as _),
            e.input().query_axis(AxisInputIndices::ScrollH as _),
            e.input().query_axis(AxisInputIndices::ScrollV as _));*/
        (None, br::SubmissionBatch {
            command_buffers: Cow::Borrowed(&self.render_cb[on_backbuffer_of as usize..on_backbuffer_of as usize + 1]),
            .. Default::default()
        })
    }

    fn discard_backbuffer_resources(&mut self) {
        self.render_cb.reset().expect("Resetting RenderCB");
        self.framebuffers.clear();
    }
    fn on_resize(&mut self, e: &peridot::Engine<Self, PL>, new_size: Vector2<usize>) {
        self.framebuffers = e.backbuffers().iter().map(|v| br::Framebuffer::new(&self.renderpass, &[v], v.size(), 1))
            .collect::<Result<Vec<_>, _>>().expect("Bind Framebuffer");
        for (r, f) in self.render_cb.iter().zip(&self.framebuffers) {
            let mut cbr = r.begin().expect("Start Recording CB");
            self.render_commands(e, &mut cbr, f);
        }
    }
}

impl<PL: peridot::NativeLinker> Game<PL> {
    fn render_commands(&self, e: &peridot::Engine<Self, PL>, cmd: &mut br::CmdRecord, fb: &br::Framebuffer)
    {
        let screen_size = fb.size();

        cmd.begin_render_pass(&self.renderpass, fb, fb.size().clone().into(),
            &[br::ClearValue::Color(BACK_COLOR)], true);
            
        cmd.bind_vertex_buffers(0, &[(&self.fixed_memory.buffer.0, self.fixed_offsets.grid_offset)]);
        self.gp_backgrid.0.bind(cmd);
        cmd.push_graphics_constant(br::ShaderStage::FRAGMENT, 0, &[1.0f32, 1.0, 1.0, 0.5]);
        cmd.bind_graphics_descriptor_sets(0, &self.descs[1..2], &[]);
        cmd.draw(2, 100, 0, 0);
        self.gp_backgrid.1.bind(cmd);
        cmd.draw(2, 100, 2, 0);
        self.gp_node_render.bind(cmd);
        cmd.bind_graphics_descriptor_sets(0, &self.descs[1..3], &[]);
        cmd.bind_vertex_buffers(0, &[(&self.fixed_memory.buffer.0, self.fixed_offsets.ipos2_rect_offset)]);
        cmd.push_graphics_constant(br::ShaderStage::VERTEX, 0, &[screen_size.0 as f32, screen_size.1 as f32]);
        cmd.draw(4, 1, 0, 0);
        
        self.fixed_offsets.vg_renderer_params.default_render_commands(e, cmd,
            &self.fixed_memory.buffer.0, &self.vg_renderer_exinst);
        cmd.end_render_pass();
    }
}
