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
    pub fn new(init_renderables: Vec<Box<dyn Renderable<Device>>>) -> Self {
        Self {
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
    ) -> br::BufferObject<Device> {
        let mut mb = peridot::MemoryBadget::new(g);
        let mut buffer_placement_offsets_for_renderable = Vec::new();
        for r in renderables {
            let m = r.mesh();
            let vo = mb.add(peridot::BufferContent::Vertex(
                m.vertex.data.len() as _,
                m.vertex.alignment as _,
            ));
            let io = m.index.map(|x| mb.add(x.make_buffer_content()));
            buffer_placement_offsets_for_renderable.push((vo, io));
        }
    }
}
