use std::collections::BTreeMap;

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

pub trait TextureSource {}

pub enum ShaderResource {
    UniformBuffer { content: Vec<u8>, alignment: usize },
    StorageBuffer { content: Vec<u8>, alignment: usize },
    Texture2D(Box<dyn TextureSource>),
}

pub trait Material<Device: br::Device> {
    fn vertex_shader(&self) -> &br::ShaderModuleObject<Device>;
    fn geometry_shader(&self) -> Option<&br::ShaderModuleObject<Device>>;
    fn fragment_shader(&self) -> Option<&br::ShaderModuleObject<Device>>;

    fn vertex_resource_binds(&self) -> &BTreeMap<u64, ShaderResource>;
    fn geometry_resource_binds(&self) -> &BTreeMap<u64, ShaderResource>;
    fn fragment_resource_binds(&self) -> &BTreeMap<u64, ShaderResource>;
}

pub trait Renderable<Device: br::Device> {
    fn mesh(&self) -> &Mesh;
    fn material(&self) -> &dyn Material<Device>;
}

pub struct RenderGroup {
    renderables: Vec<Box<dyn Renderable<peridot::DeviceObject>>>,
    mesh_buffer: peridot::Buffer<
        br::BufferObject<peridot::DeviceObject>,
        br::DeviceMemoryObject<peridot::DeviceObject>,
    >,
    commands: br::CommandBufferObject<peridot::DeviceObject>,
}
impl RenderGroup {
    pub fn new(
        g: &mut peridot::Graphics,
        init_renderables: Vec<Box<dyn Renderable<peridot::DeviceObject>>>,
    ) -> Self {
        let mut tfb = peridot::TransferBatch::new();
        let mesh_buffer = Self::build_mesh_buffer(g, &init_renderables, &mut tfb);
        tfb.submit(g).expect("Failed to submit initialization");

        Self {
            mesh_buffer,
            renderables: init_renderables,
        }
    }

    pub fn commit(&mut self, new_renderables: Vec<Box<dyn Renderable<peridot::DeviceObject>>>) {
        self.renderables = new_renderables;
    }

    pub fn command_buffer(&self) -> &br::CommandBufferObject<peridot::DeviceObject> {
        &self.commands
    }

    fn build_mesh_buffer(
        g: &peridot::Graphics,
        renderables: &[Box<dyn Renderable<peridot::DeviceObject>>],
        tfb: &mut peridot::TransferBatch,
    ) -> peridot::Buffer<
        br::BufferObject<peridot::DeviceObject>,
        br::DeviceMemoryObject<peridot::DeviceObject>,
    > {
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
        let (mut mb, _) =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::with_entries(
                g,
                vec![peridot::MemoryBadgetEntry::Buffer(
                    bp.build_transferred()
                        .expect("Failed to build buffer object"),
                )],
            );
        let mut resources = mb.alloc().expect("Failed to allocate memory");
        let mut buffer = resources.pop().expect("no resources?").unwrap_buffer();
        let (mut stg_mb, _) =
            peridot::MemoryBadget::<_, br::ImageObject<peridot::DeviceObject>>::with_entries(
                g,
                vec![peridot::MemoryBadgetEntry::Buffer(
                    bp.build_upload()
                        .expect("Failed to build stg buffer object"),
                )],
            );
        let mut stg_resources = stg_mb
            .alloc_upload()
            .expect("Failed to allocate stg memory");
        let mut stg_buffer = resources.pop().expect("no resources?").unwrap_buffer();
        stg_buffer
            .guard_map(0..bp.total_size(), |range| {
                for (r, &(vo, io)) in renderables
                    .iter()
                    .zip(buffer_placement_offsets_for_renderable.iter())
                {
                    let m = r.mesh();

                    unsafe {
                        range.clone_from_slice_at(vo as _, &m.vertex.data);
                        if let Some(io) = io {
                            match m.index {
                                Some(Indices::Short(xs)) => range.clone_from_slice_at(io as _, &xs),
                                Some(Indices::Long(xs)) => range.clone_from_slice_at(io as _, &xs),
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

    fn build_descriptor_sets<'m>(
        g: &peridot::Graphics,
        materials: impl Iterator<Item = &'m dyn Material<peridot::DeviceObject>>,
    ) {
    }
}
