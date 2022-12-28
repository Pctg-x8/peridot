use bedrock as br;

pub struct RawVertexBuffer {
    pub data: Vec<u8>,
    pub alignment: usize,
    pub format: br::vk::VkFormat,
}

pub enum Indices {
    Short(Vec<u16>),
    Long(Vec<u32>),
}
impl Indices {
    fn make_buffer_content(&self) -> peridot::BufferContent {
        match self {
            Self::Short(xs) => peridot::BufferContent::indices::<u16>(xs.len()),
            Self::Long(xs) => peridot::BufferContent::indices::<u32>(xs.len()),
        }
    }
}

pub struct Mesh {
    pub vertex: RawVertexBuffer,
    pub index: Option<Indices>,
}

pub trait Material<Device: br::Device> {
    fn vertex_shader(&self) -> &br::ShaderModuleObject<Device>;
    fn geometry_shader(&self) -> Option<&br::ShaderModuleObject<Device>>;
    fn fragment_shader(&self) -> Option<&br::ShaderModuleObject<Device>>;
}

pub trait Renderable<Device: br::Device> {
    fn mesh(&self) -> &Mesh;
    fn material(&self) -> &dyn Material<Device>;
}

pub struct RenderGroup<Device: br::Device> {
    renderables: Vec<Box<dyn Renderable<Device>>>,
    mesh_buffer: br::BufferObject<Device>,
    commands: br::CommandBufferObject<Device>,
}
impl<Device: br::Device> RenderGroup<Device> {
    pub fn new(g: &peridot::Graphics, init_renderables: Vec<Box<dyn Renderable<Device>>>) -> Self {
        let mut tfb = peridot::TransferBatch::new();
        let mesh_buffer = Self::build_mesh_buffer(g, &init_renderables, &mut tfb);

        Self {
            mesh_buffer,
            renderables: init_renderables,
        }
    }

    pub fn commit(&mut self, new_renderables: Vec<Box<dyn Renderable<Device>>>) {
        self.renderables = new_renderables;
    }

    pub fn command_buffer(&self) -> &br::CommandBufferObject<Device> {
        &self.commands
    }

    fn build_mesh_buffer(
        g: &peridot::Graphics,
        renderables: &[Box<dyn Renderable<Device>>],
        tfb: &mut peridot::TransferBatch,
    ) -> br::BufferObject<Device> {
        let mut bp = peridot::BufferPrealloc::new(g);
        let mut buffer_placement_offsets_for_renderable = Vec::new();
        for r in renderables {
            let m = r.mesh();

            let vo = bp.add(peridot::BufferContent::Vertex(
                m.vertex.data.len() as _,
                m.vertex.alignment as _,
            ));
            let io = m.index.map(|x| bp.add(x.make_buffer_content()));
            buffer_placement_offsets_for_renderable.push((vo, io));
        }
        let (mut mb, _) = peridot::MemoryBadget::with_entries(
            g,
            vec![bp
                .build_transferred()
                .expect("Failed to build buffer object")
                .into()],
        );
        let mut resources = mb.alloc().expect("Failed to allocate memory");
        let mut buffer = resources.pop().expect("no resources?").unwrap_buffer();
        let (mut stg_mb, _) = peridot::MemoryBadget::with_entries(
            g,
            vec![bp
                .build_upload()
                .expect("Failed to build stg buffer object")
                .into()],
        );
        let mut stg_resources = stg_mb
            .alloc_upload()
            .expect("Failed to allocate stg memory");
        let mut stg_buffer = resources.pop().expect("no resources?").unwrap_buffer();
        stg_buffer
            .guard_map(0..bp.total_size(), |range| {
                for (r, (vo, io)) in renderables
                    .iter()
                    .zip(buffer_placement_offsets_for_renderable.iter())
                {
                    let m = r.mesh();

                    unsafe {
                        range.clone_from_slice_at(vo, &m.vertex.data);
                        if let Some(io) = io {
                            match m.index {
                                Some(Indices::Short(xs)) => range.clone_from_slice_at(io, &xs),
                                Some(Indices::Long(xs)) => range.clone_from_slice_at(io, &xs),
                                None => unreachable!(),
                            }
                        }
                    }
                }
            })
            .expect("Failed to initialize buffer memory");
        tfb.add_mirroring_buffer(&stg_buffer, &buffer, 0, bp.total_size());
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT,
            &buffer,
            0..bp.total_size(),
            br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::INDEX_READ,
        );

        buffer
    }
}
