use std::{
    collections::HashMap,
    fs::File,
    io::{BufWriter, Read, Seek, SeekFrom, Write},
    path::PathBuf,
};

use clap::Parser;

pub mod gltf;

#[derive(Parser)]
struct App {
    input: PathBuf,
    #[clap(long, short = 'o', default_value = ".")]
    out_dir: PathBuf,
    #[clap(long, short = 'p', default_value = "")]
    prefix: String,
}

fn main() {
    let args = App::parse();

    let mut reader = File::open(&args.input).expect("failed to open input file");
    let magic = read_u32(&mut reader).expect("failed to read magic");
    assert_eq!(magic, 0x46546c67, "magic mismatch");
    let version = read_u32(&mut reader).expect("failed to read version");
    let length = read_u32(&mut reader).expect("failed to read length");
    println!("glb detected: version={version} length={length}");

    let chunk0_length = read_u32(&mut reader).expect("failed to read chunk length");
    let chunk0_type =
        ChunkType::from_binary(read_u32(&mut reader).expect("failed to read chunk type"))
            .expect("invalid chunk type");
    assert_eq!(chunk0_type, ChunkType::Json, "chunk 0 must be json");
    println!("chunk 0: length={chunk0_length} type={chunk0_type:?}");
    let mut content = Vec::<u8>::with_capacity(chunk0_length as usize);
    reader
        .read_exact(unsafe {
            core::mem::transmute(&mut content.spare_capacity_mut()[..chunk0_length as usize])
        })
        .expect("failed to read chunk content");
    unsafe {
        content.set_len(chunk0_length as usize);
    }
    let content = unsafe { str::from_utf8_unchecked(&content) };
    reader
        .seek(SeekFrom::Current((4 - (chunk0_length as i64 & 3)) & 3))
        .expect("reader.seek"); // skip for padding
    let bin_chunk_base = reader.stream_position().expect("reader.stream_position");
    let parsed = serde_json::from_str::<gltf::GLTF>(content).expect("invalid gltf json");
    println!("{parsed:#?}");

    let buffers = parsed
        .buffers
        .into_iter()
        .enumerate()
        .map(|(n, b)| {
            if n == 0 && b.uri.is_none() {
                // first uri-null buffer is combined in glb file
                Buffer::Internal {
                    byte_length: b.byte_length,
                }
            } else {
                Buffer::External(b)
            }
        })
        .collect::<Vec<_>>();

    let internal_buffer_start = if buffers.iter().any(|x| matches!(x, Buffer::Internal { .. })) {
        reader
            .seek(SeekFrom::Start(bin_chunk_base))
            .expect("reader.seek.internal_buffer");
        let chunk1_length = read_u32(&mut reader).expect("failed to read chunk1 length");
        let chunk1_type =
            ChunkType::from_binary(read_u32(&mut reader).expect("failed to read chunk1 type"))
                .expect("invalid chunk1 type");
        assert_eq!(chunk1_type, ChunkType::Bin, "chunk 1 must be bin");
        println!("chunk1 length: {chunk1_length}");

        Some(reader.stream_position().expect("reader.stream_position"))
    } else {
        None
    };

    println!("meshes:");
    for (mesh_index, x) in parsed.meshes.iter().enumerate() {
        println!("  {x:?}");
        if !x.weights.is_empty() {
            eprintln!("mesh morphing is not supported by the importer");
        }

        println!("  meshprim:");
        for (prim_index, x) in x.primitives.iter().enumerate() {
            let topo = MeshPrimitiveTopology::from_gltf(x);
            println!("    topo: {topo:?}");

            #[derive(Debug)]
            struct SourceBufferData {
                buffer_index: usize,
                buffer_range: core::range::Range<usize>,
                byte_stride: usize,
            }
            let (mut index_stream, index_source_data) = if let Some(indices) = x.indices {
                let accessor = &parsed.accessors[indices];
                let buffer_view =
                    &parsed.buffer_views[accessor.buffer_view.expect("no buffer view linked?")];
                let buffer = &buffers[buffer_view.buffer];
                let buffer_element_type = BufferElementType::from_accessor(accessor);
                let byte_stride = buffer_view
                    .byte_stride
                    .unwrap_or(buffer_element_type.default_byte_stride());

                println!("    indices: {accessor:?}");
                println!("      buffer_view: {buffer_view:?}",);
                println!("      buffer: {buffer:?}");
                println!(
                    "      data: {buffer:?}[{}..{}]",
                    accessor.byte_offset + buffer_view.byte_offset,
                    accessor.byte_offset + buffer_view.byte_offset + buffer_view.byte_length
                );
                println!(
                    "      count x stride: {} x {byte_stride}[{buffer_element_type:?}]",
                    accessor.count
                );

                (
                    MeshIndexStream::Stream {
                        index_type: match buffer_element_type {
                            BufferElementType::Ushort => IndexType::UInt16,
                            _ => unreachable!("invalid index buffer element type"),
                        },
                        buffer: StreamBuffer {
                            content_location: 0, // compute later
                            byte_length: (accessor.count
                                * buffer_element_type.default_byte_stride())
                            .try_into()
                            .expect("too large index buffer"),
                            device_alignment_requirement: buffer_element_type
                                .device_alignment_requirement(),
                        },
                    },
                    Some(SourceBufferData {
                        buffer_index: buffer_view.buffer,
                        buffer_range: (accessor.byte_offset + buffer_view.byte_offset
                            ..accessor.byte_offset
                                + buffer_view.byte_offset
                                + buffer_view.byte_length)
                            .into(),
                        byte_stride: buffer_view
                            .byte_stride
                            .unwrap_or(buffer_element_type.default_byte_stride()),
                    }),
                )
            } else {
                (MeshIndexStream::None, None)
            };

            struct AttributeInfo {
                element_type: BufferElementType,
                source: SourceBufferData,
            }
            struct StreamInfo {
                attributes: HashMap<Attribute, AttributeInfo>,
            }
            let mut streams = Vec::new();
            let mut attribute_count = None;
            for (n, &x) in x.attributes.iter() {
                let Some(attr_name) = Attribute::try_from_gltf_attr_name(n) else {
                    eprintln!("{n} is unsupported attr name: skipping");
                    continue;
                };
                attr_name.assert_validate();

                let accessor = &parsed.accessors[x];
                let buffer_view =
                    &parsed.buffer_views[accessor.buffer_view.expect("no buffer view linked?")];
                let buffer = &buffers[buffer_view.buffer];
                let buffer_element_type = BufferElementType::from_accessor(accessor);
                let byte_stride = buffer_view
                    .byte_stride
                    .unwrap_or(buffer_element_type.default_byte_stride());
                let target = match buffer_view.target {
                    Some(gltf::BUFFER_VIEW_TARGET_ARRAY_BUFFER) => "array buffer".into(),
                    Some(gltf::BUFFER_VIEW_TARGET_ELEMENT_ARRAY_BUFFER) => {
                        "element array buffer".into()
                    }
                    Some(x) => format!("unknown target: {x}"),
                    None => "unknown target".into(),
                };

                println!("    {attr_name:?}: {accessor:?}");
                println!("      buffer_view: {buffer_view:?}",);
                println!("      buffer: {buffer:?}");
                println!(
                    "      data: {buffer:?}[{}..{}] ({target})",
                    accessor.byte_offset + buffer_view.byte_offset,
                    accessor.byte_offset + buffer_view.byte_offset + buffer_view.byte_length
                );
                println!(
                    "      count x stride: {} x {byte_stride}[{buffer_element_type:?}]",
                    accessor.count
                );

                let stream_index = match attr_name {
                    // Positionのみ0にする
                    Attribute::Position => 0,
                    _ => 1,
                };
                while streams.len() < stream_index + 1 {
                    streams.push(StreamInfo {
                        attributes: HashMap::new(),
                    });
                }
                match streams[stream_index].attributes.entry(attr_name) {
                    std::collections::hash_map::Entry::Occupied(e) => {
                        panic!("same attribute occured in a mesh primitive: {:?}", e.key());
                    }
                    std::collections::hash_map::Entry::Vacant(e) => {
                        e.insert(AttributeInfo {
                            element_type: buffer_element_type,
                            source: SourceBufferData {
                                buffer_index: buffer_view.buffer,
                                buffer_range: ((accessor.byte_offset + buffer_view.byte_offset)
                                    ..(accessor.byte_offset
                                        + buffer_view.byte_offset
                                        + buffer_view.byte_length))
                                    .into(),
                                byte_stride: buffer_view
                                    .byte_stride
                                    .unwrap_or(buffer_element_type.default_byte_stride()),
                            },
                        });
                    }
                }

                match attribute_count {
                    None => {
                        attribute_count = Some(accessor.count);
                    }
                    Some(count) => {
                        assert_eq!(
                            count, accessor.count,
                            "accessor count is not unique in same mesh primitive"
                        );
                    }
                }
            }

            let attribute_count = attribute_count.unwrap_or(0);
            let mut stream_attributes = streams
                .into_iter()
                .map(|s| {
                    let mut attributes = s.attributes.into_iter().collect::<Vec<_>>();
                    attributes.sort_by_key(|x| x.0.order());

                    let mut attribute_data = Vec::with_capacity(attributes.len());
                    let mut offset = 0;
                    let mut device_alignment_requirement = 1;
                    for (a, x) in attributes {
                        attribute_data.push((
                            a,
                            AttributeData {
                                offset,
                                element_type: x.element_type,
                            },
                            x.source,
                        ));
                        offset += x.element_type.default_byte_stride() as u16;
                        // Note: 2^n想定 想定が崩れたら直す必要がある
                        device_alignment_requirement = device_alignment_requirement
                            .max(x.element_type.device_alignment_requirement());
                    }

                    (
                        MeshVertexStream {
                            buffer: StreamBuffer {
                                content_location: 0,
                                byte_length: (attribute_count as usize * offset as usize) as _,
                                device_alignment_requirement,
                            },
                            attribute_count: attribute_data
                                .len()
                                .try_into()
                                .expect("too many attributes"),
                        },
                        attribute_data,
                    )
                })
                .collect::<Vec<_>>();
            // println!("{index_stream:#?}");
            // println!("{index_source_data:#?}");
            // println!("{stream_attributes:#?}");

            let pa1_mesh_file_name =
                format!("{}mesh{mesh_index}.{prim_index}.pa1-mesh", args.prefix);
            let mut mesh_out = BufWriter::new(
                File::create(args.out_dir.join(pa1_mesh_file_name)).expect("mesh_out.create"),
            );
            const PA1M_SIGNATURE: u32 = u32::from_be_bytes(*b"pa1m");
            mesh_out
                .write_all(&PA1M_SIGNATURE.to_ne_bytes())
                .expect("mesh_out.write.signature");
            MeshHeader {
                primitive_topology: topo,
                vertex_stream_count: stream_attributes
                    .len()
                    .try_into()
                    .expect("too many vertex streams"),
            }
            .serialize(&mut mesh_out)
            .expect("mesh_out.write.header");

            // compute content offsets and write headers
            let mut content_offset = 4
                + MeshHeader::serialize_size()
                + index_stream.serialize_size()
                + stream_attributes
                    .iter()
                    .map(|(_, attrs)| {
                        MeshVertexStream::serialize_size()
                            + attrs.len()
                                * (Attribute::serialize_size() + AttributeData::serialize_size())
                    })
                    .sum::<usize>();
            if let MeshIndexStream::Stream { ref mut buffer, .. } = index_stream {
                buffer.content_location = content_offset as _;
                content_offset += buffer.byte_length as usize;
            }
            index_stream
                .serialize(&mut mesh_out)
                .expect("mesh_out.write.index_stream");
            for (stream, attrs) in stream_attributes.iter_mut() {
                stream.buffer.content_location = content_offset as _;
                content_offset += stream.buffer.byte_length as usize;

                stream
                    .serialize(&mut mesh_out)
                    .expect("mesh_out.write.vertex_stream");
                for (a, d, _) in attrs {
                    a.serialize(&mut mesh_out)
                        .expect("mesh_out.write.attribute");
                    d.serialize(&mut mesh_out)
                        .expect("mesh_out.write.attribute_data");
                }
            }

            // copy buffer data with transforming
            if let Some(source) = index_source_data {
                let dest_stride = match index_stream {
                    MeshIndexStream::Stream {
                        index_type: IndexType::UInt16,
                        ..
                    } => 2,
                    MeshIndexStream::Stream {
                        index_type: IndexType::UInt32,
                        ..
                    } => 4,
                    MeshIndexStream::None => {
                        unreachable!("MeshIndexStream::None but index_source_data is some")
                    }
                };

                let mut source_ptr = source.buffer_range.start;
                while source_ptr < source.buffer_range.end {
                    // TODO: external buffer
                    reader
                        .seek(SeekFrom::Start(
                            internal_buffer_start.expect("no internal chunk found?")
                                + source_ptr as u64,
                        ))
                        .expect("reader.seek");
                    let mut buffer = Vec::with_capacity(dest_stride);
                    reader
                        .read_exact(unsafe {
                            core::mem::transmute(&mut buffer.spare_capacity_mut()[..dest_stride])
                        })
                        .expect("reader.read_exact");
                    unsafe {
                        buffer.set_len(dest_stride);
                    }
                    mesh_out.write_all(&buffer).expect("mesh_out.write_all");

                    source_ptr += source.byte_stride;
                }
            }
            for (_, attrs) in stream_attributes {
                let dest_strides = attrs
                    .iter()
                    .map(|a| a.1.element_type.default_byte_stride())
                    .collect::<Vec<_>>();
                for n in 0..attribute_count {
                    for (a, dest_stride) in attrs.iter().zip(dest_strides.iter()) {
                        let reader = match buffers[a.2.buffer_index] {
                            Buffer::Internal { .. } => {
                                reader
                                    .seek(SeekFrom::Start(
                                        internal_buffer_start
                                            .expect("no internal buffer chunk found?")
                                            + a.2.buffer_range.start as u64
                                            + (n * a.2.byte_stride) as u64,
                                    ))
                                    .expect("reader.seek");
                                &mut reader
                            }
                            Buffer::External(_) => todo!("external buffer support"),
                        };

                        let mut buffer = Vec::<u8>::with_capacity(*dest_stride);
                        reader
                            .read_exact(unsafe {
                                core::mem::transmute(
                                    &mut buffer.spare_capacity_mut()[..*dest_stride],
                                )
                            })
                            .expect("reader.read_exact");
                        unsafe {
                            buffer.set_len(*dest_stride);
                        }

                        if matches!(
                            a.0,
                            Attribute::Position | Attribute::Normal | Attribute::Tangent
                        ) {
                            // gltfはzの向きがPeridotの想定と逆なので反転させる
                            unsafe {
                                *buffer.as_mut_ptr().byte_add(8).cast::<f32>() *= -1.0;
                            }
                        }

                        mesh_out.write_all(&buffer).expect("mesh_out.write_all");
                    }
                }
            }
        }
    }
}

pub struct MeshHeader {
    pub primitive_topology: MeshPrimitiveTopology,
    pub vertex_stream_count: u8,
}
impl MeshHeader {
    pub const fn serialize_size() -> usize {
        1 + 1
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&[self.primitive_topology as u8, self.vertex_stream_count])
    }
}

#[derive(Debug)]
pub struct StreamBuffer {
    pub content_location: u64,
    pub byte_length: u32,
    pub device_alignment_requirement: u32,
}
impl StreamBuffer {
    pub const fn serialize_size() -> usize {
        8 + 4 + 4
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&self.content_location.to_ne_bytes())?;
        w.write_all(&self.byte_length.to_ne_bytes())?;
        w.write_all(&self.device_alignment_requirement.to_ne_bytes())
    }
}

#[derive(Debug)]
pub struct MeshVertexStream {
    pub buffer: StreamBuffer,
    pub attribute_count: u8,
}
impl MeshVertexStream {
    pub const fn serialize_size() -> usize {
        StreamBuffer::serialize_size() + 1
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        self.buffer.serialize(w)?;
        w.write_all(&[self.attribute_count])
    }
}

#[derive(Debug)]
pub enum MeshIndexStream {
    None,
    Stream {
        index_type: IndexType,
        buffer: StreamBuffer,
    },
}
impl MeshIndexStream {
    pub const fn serialize_size(&self) -> usize {
        match self {
            Self::None => 1,
            Self::Stream { .. } => 1 + StreamBuffer::serialize_size(),
        }
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::None => w.write_all(&[0]),
            Self::Stream { index_type, buffer } => {
                w.write_all(&[*index_type as u8])?;
                buffer.serialize(w)
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IndexType {
    UInt16 = 1,
    UInt32 = 2,
}

#[derive(Debug)]
pub struct AttributeData {
    pub offset: u16,
    pub element_type: BufferElementType,
}
impl AttributeData {
    pub const fn serialize_size() -> usize {
        2 + 2
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        w.write_all(&self.offset.to_ne_bytes())?;
        w.write_all(&(self.element_type as u16).to_ne_bytes())
    }
}

#[derive(Debug)]
pub enum Buffer {
    Internal { byte_length: usize },
    External(gltf::Buffer),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Attribute {
    Position,
    Normal,
    Tangent,
    Texcoord(u8),
    Color(u8),
    Joints(u8),
    Weights(u8),
}
impl Attribute {
    pub fn try_from_gltf_attr_name(name: &str) -> Option<Self> {
        if name.eq_ignore_ascii_case("position") {
            return Some(Self::Position);
        }

        if name.eq_ignore_ascii_case("normal") {
            return Some(Self::Normal);
        }

        if name.eq_ignore_ascii_case("tangent") {
            return Some(Self::Tangent);
        }

        if name.starts_with("TEXCOORD_") {
            return Some(Self::Texcoord(name["TEXCOORD_".len()..].parse().ok()?));
        }

        if name.starts_with("COLOR_") {
            return Some(Self::Color(name["COLOR_".len()..].parse().ok()?));
        }

        if name.starts_with("JOINTS_") {
            return Some(Self::Joints(name["JOINTS_".len()..].parse().ok()?));
        }

        if name.starts_with("WEIGHTS_") {
            return Some(Self::Weights(name["WEIGHTS_".len()..].parse().ok()?));
        }

        return None;
    }

    pub fn assert_validate(&self) {
        match self {
            Self::Position | Self::Normal | Self::Tangent => (),
            &Self::Texcoord(n) => assert!(n < 8, "too many texcoords"),
            &Self::Color(n) => assert!(n < 8, "too many colors"),
            &Self::Joints(n) => assert!(n < 8, "too many joints"),
            &Self::Weights(n) => assert!(n < 8, "too many weights"),
        }
    }

    pub const fn order(&self) -> u8 {
        match self {
            Self::Position => 0,
            Self::Normal => 1,
            Self::Tangent => 2,
            // up to 8 items
            &Self::Texcoord(n) => 0x08 + n,
            &Self::Color(n) => 0x10 + n,
            &Self::Joints(n) => 0x18 + n,
            &Self::Weights(n) => 0x20 + n,
        }
    }

    pub const fn serialize_size() -> usize {
        1
    }

    pub fn serialize(&self, w: &mut (impl Write + ?Sized)) -> std::io::Result<()> {
        match self {
            Self::Position => w.write_all(&[0]),
            Self::Normal => w.write_all(&[1]),
            Self::Tangent => w.write_all(&[2]),
            // up to 8 items
            Self::Texcoord(n) => w.write_all(&[0x08 + n]),
            Self::Color(n) => w.write_all(&[0x10 + n]),
            Self::Joints(n) => w.write_all(&[0x18 + n]),
            Self::Weights(n) => w.write_all(&[0x20 + n]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum MeshPrimitiveTopology {
    Points = 0,
    Lines = 1,
    LineLoop = 2,
    LineStrip = 3,
    Triangles = 4,
    TriangleStrip = 5,
    TriangleFan = 6,
}
impl MeshPrimitiveTopology {
    pub fn from_gltf(mesh_primitive: &gltf::MeshPrimitive) -> Self {
        match mesh_primitive.mode {
            gltf::MESH_PRIMITIVE_MODE_POINTS => Self::Points,
            gltf::MESH_PRIMITIVE_MODE_LINES => Self::Lines,
            gltf::MESH_PRIMITIVE_MODE_LINE_LOOP => Self::LineLoop,
            gltf::MESH_PRIMITIVE_MODE_LINE_STRIP => Self::LineStrip,
            gltf::MESH_PRIMITIVE_MODE_TRIANGLES => Self::Triangles,
            gltf::MESH_PRIMITIVE_MODE_TRIANGLE_STRIP => Self::TriangleStrip,
            gltf::MESH_PRIMITIVE_MODE_TRIANGLE_FAN => Self::TriangleFan,
            x => unreachable!("unhandled mesh primitive mode: {x:?}"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum BufferElementType {
    Ushort = 0,
    Float2 = 1,
    Float3 = 2,
    Float4 = 3,
}
impl BufferElementType {
    pub fn from_accessor(a: &gltf::Accessor) -> Self {
        match (a.r#type, a.component_type, a.normalized) {
            (gltf::AccessorType::Scalar, gltf::COMPONENT_TYPE_UNSIGNED_SHORT, false) => {
                Self::Ushort
            }
            (gltf::AccessorType::Vec2, gltf::COMPONENT_TYPE_FLOAT, false) => Self::Float2,
            (gltf::AccessorType::Vec3, gltf::COMPONENT_TYPE_FLOAT, false) => Self::Float3,
            (gltf::AccessorType::Vec4, gltf::COMPONENT_TYPE_FLOAT, false) => Self::Float4,
            (t, c, n) => unreachable!("unhandled element type: {t:?} {c} normalized={n}"),
        }
    }

    pub const fn default_byte_stride(&self) -> usize {
        match self {
            BufferElementType::Ushort => 2,
            BufferElementType::Float2 => 2 * 4,
            BufferElementType::Float3 => 3 * 4,
            BufferElementType::Float4 => 4 * 4,
        }
    }

    pub const fn device_alignment_requirement(&self) -> u32 {
        match self {
            BufferElementType::Ushort => 2,
            BufferElementType::Float2 => 4,
            BufferElementType::Float3 => 4,
            BufferElementType::Float4 => 4,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChunkType {
    Json,
    Bin,
}
impl ChunkType {
    pub const fn from_binary(v: u32) -> Result<Self, u32> {
        match v {
            0x4e4f534a => Ok(Self::Json),
            0x004e4942 => Ok(Self::Bin),
            _ => Err(v),
        }
    }
}

#[inline(always)]
fn read_u32(r: &mut (impl Read + ?Sized)) -> std::io::Result<u32> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf)?;
    Ok(u32::from_le_bytes(buf))
}
