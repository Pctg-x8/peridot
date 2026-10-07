use std::{
    collections::HashMap,
    fs::File,
    io::{BufWriter, Read, Seek, SeekFrom, Write},
    path::PathBuf,
};

use clap::Parser;
use peridot_mesh::{
    Attribute, AttributeData, BufferElementType, Header, IndexStream, IndexType, PrimitiveTopology,
    SIGNATURE, StreamBuffer, VertexStream,
};
use peridot_tp_gltf as gltf;

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
    let magic = gltf::binary::read_u32(&mut reader).expect("failed to read magic");
    assert_eq!(magic, gltf::binary::MAGIC, "magic mismatch");
    process_glb_file(&mut reader, args.out_dir, args.prefix);
}

fn process_glb_file(r: &mut (impl Read + Seek + ?Sized), out_dir: PathBuf, prefix: String) {
    let hdr = gltf::binary::Header::read(r).expect("failed to read header");
    println!("glb detected: {hdr:?}");

    let chunk0_hdr = gltf::binary::ChunkHeader::read(r).expect("failed to read chunk header");
    assert_eq!(
        chunk0_hdr.r#type,
        gltf::binary::ChunkType::Json,
        "chunk 0 must be json"
    );
    println!("chunk 0: {chunk0_hdr:?}");
    let mut content = Vec::<u8>::with_capacity(chunk0_hdr.length as usize);
    r.read_exact(unsafe {
        core::mem::transmute(&mut content.spare_capacity_mut()[..chunk0_hdr.length as usize])
    })
    .expect("failed to read chunk content");
    unsafe {
        content.set_len(chunk0_hdr.length as usize);
    }
    let content = unsafe { str::from_utf8_unchecked(&content) };
    r.seek(SeekFrom::Current(chunk0_hdr.padding_tail_length() as _))
        .expect("reader.seek"); // skip for padding
    let bin_chunk_base = r.stream_position().expect("reader.stream_position");
    let parsed = serde_json::from_str::<gltf::json::GLTF>(content).expect("invalid gltf json");
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
        r.seek(SeekFrom::Start(bin_chunk_base))
            .expect("reader.seek.internal_buffer");
        let chunk1_hdr = gltf::binary::ChunkHeader::read(r).expect("failed to read chunk1 header");
        assert_eq!(
            chunk1_hdr.r#type,
            gltf::binary::ChunkType::Bin,
            "chunk 1 must be bin"
        );
        println!("chunk1 length: {}", chunk1_hdr.length);

        Some(r.stream_position().expect("reader.stream_position"))
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
            let topo = primitive_topology_from_gltf(x);
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
                let buffer_element_type = buffer_element_type_from_accessor(accessor);
                let byte_stride = buffer_view
                    .byte_stride
                    .unwrap_or(buffer_element_type.size());

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
                    IndexStream::Stream {
                        index_type: match buffer_element_type {
                            BufferElementType::Ushort => IndexType::UInt16,
                            _ => unreachable!("invalid index buffer element type"),
                        },
                        buffer: StreamBuffer {
                            content_location: 0, // compute later
                            byte_length: (accessor.count * buffer_element_type.size())
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
                            .unwrap_or(buffer_element_type.size()),
                    }),
                )
            } else {
                (IndexStream::None, None)
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
                let Some(attr_name) = attribute_try_from_gltf_attr_name(n) else {
                    eprintln!("{n} is unsupported attr name: skipping");
                    continue;
                };
                attr_name.assert_validate();

                let accessor = &parsed.accessors[x];
                let buffer_view =
                    &parsed.buffer_views[accessor.buffer_view.expect("no buffer view linked?")];
                let buffer = &buffers[buffer_view.buffer];
                let buffer_element_type = buffer_element_type_from_accessor(accessor);
                let byte_stride = buffer_view
                    .byte_stride
                    .unwrap_or(buffer_element_type.size());
                let target = match buffer_view.target {
                    Some(gltf::json::BUFFER_VIEW_TARGET_ARRAY_BUFFER) => "array buffer".into(),
                    Some(gltf::json::BUFFER_VIEW_TARGET_ELEMENT_ARRAY_BUFFER) => {
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
                while streams.len() <= stream_index {
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
                                    .unwrap_or(buffer_element_type.size()),
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
                        offset += x.element_type.size() as u16;
                        // Note: 2^n想定 想定が崩れたら直す必要がある
                        device_alignment_requirement = device_alignment_requirement
                            .max(x.element_type.device_alignment_requirement());
                    }

                    (
                        VertexStream {
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

            let pa1_mesh_file_name = format!("{}mesh{mesh_index}-{prim_index}.pa1-mesh", prefix);
            let mut mesh_out = BufWriter::new(
                File::create(out_dir.join(pa1_mesh_file_name)).expect("mesh_out.create"),
            );
            mesh_out
                .write_all(&SIGNATURE.to_ne_bytes())
                .expect("mesh_out.write.signature");
            Header {
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
                + Header::serialize_size()
                + index_stream.serialize_size()
                + stream_attributes
                    .iter()
                    .map(|(_, attrs)| {
                        VertexStream::serialize_size()
                            + attrs.len()
                                * (Attribute::serialize_size() + AttributeData::serialize_size())
                    })
                    .sum::<usize>();
            if let IndexStream::Stream { ref mut buffer, .. } = index_stream {
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
                    IndexStream::Stream {
                        index_type: IndexType::UInt16,
                        ..
                    } => 2,
                    IndexStream::Stream {
                        index_type: IndexType::UInt32,
                        ..
                    } => 4,
                    IndexStream::None => {
                        unreachable!("MeshIndexStream::None but index_source_data is some")
                    }
                };

                let mut source_ptr = source.buffer_range.start;
                while source_ptr < source.buffer_range.end {
                    // TODO: external buffer
                    r.seek(SeekFrom::Start(
                        internal_buffer_start.expect("no internal chunk found?")
                            + source_ptr as u64,
                    ))
                    .expect("reader.seek");
                    let mut buffer = Vec::with_capacity(dest_stride);
                    r.read_exact(unsafe {
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
                    .map(|a| a.1.element_type.size())
                    .collect::<Vec<_>>();
                for n in 0..attribute_count {
                    for (a, dest_stride) in attrs.iter().zip(dest_strides.iter()) {
                        let reader = match buffers[a.2.buffer_index] {
                            Buffer::Internal { .. } => {
                                r.seek(SeekFrom::Start(
                                    internal_buffer_start.expect("no internal buffer chunk found?")
                                        + a.2.buffer_range.start as u64
                                        + (n * a.2.byte_stride) as u64,
                                ))
                                .expect("reader.seek");
                                &mut *r
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

fn attribute_try_from_gltf_attr_name(name: &str) -> Option<Attribute> {
    if name.eq_ignore_ascii_case("position") {
        return Some(Attribute::Position);
    }

    if name.eq_ignore_ascii_case("normal") {
        return Some(Attribute::Normal);
    }

    if name.eq_ignore_ascii_case("tangent") {
        return Some(Attribute::Tangent);
    }

    if name.starts_with("TEXCOORD_") {
        return Some(Attribute::Texcoord(name["TEXCOORD_".len()..].parse().ok()?));
    }

    if name.starts_with("COLOR_") {
        return Some(Attribute::Color(name["COLOR_".len()..].parse().ok()?));
    }

    if name.starts_with("JOINTS_") {
        return Some(Attribute::Joints(name["JOINTS_".len()..].parse().ok()?));
    }

    if name.starts_with("WEIGHTS_") {
        return Some(Attribute::Weights(name["WEIGHTS_".len()..].parse().ok()?));
    }

    return None;
}

fn primitive_topology_from_gltf(mesh_primitive: &gltf::json::MeshPrimitive) -> PrimitiveTopology {
    match mesh_primitive.mode {
        gltf::json::MESH_PRIMITIVE_MODE_POINTS => PrimitiveTopology::Points,
        gltf::json::MESH_PRIMITIVE_MODE_LINES => PrimitiveTopology::Lines,
        gltf::json::MESH_PRIMITIVE_MODE_LINE_LOOP => PrimitiveTopology::LineLoop,
        gltf::json::MESH_PRIMITIVE_MODE_LINE_STRIP => PrimitiveTopology::LineStrip,
        gltf::json::MESH_PRIMITIVE_MODE_TRIANGLES => PrimitiveTopology::Triangles,
        gltf::json::MESH_PRIMITIVE_MODE_TRIANGLE_STRIP => PrimitiveTopology::TriangleStrip,
        gltf::json::MESH_PRIMITIVE_MODE_TRIANGLE_FAN => PrimitiveTopology::TriangleFan,
        x => unreachable!("unhandled mesh primitive mode: {x:?}"),
    }
}

fn buffer_element_type_from_accessor(a: &gltf::json::Accessor) -> BufferElementType {
    match (a.r#type, a.component_type, a.normalized) {
        (gltf::json::AccessorType::Scalar, gltf::json::COMPONENT_TYPE_UNSIGNED_SHORT, false) => {
            BufferElementType::Ushort
        }
        (gltf::json::AccessorType::Vec2, gltf::json::COMPONENT_TYPE_FLOAT, false) => {
            BufferElementType::Float2
        }
        (gltf::json::AccessorType::Vec3, gltf::json::COMPONENT_TYPE_FLOAT, false) => {
            BufferElementType::Float3
        }
        (gltf::json::AccessorType::Vec4, gltf::json::COMPONENT_TYPE_FLOAT, false) => {
            BufferElementType::Float4
        }
        (t, c, n) => unreachable!("unhandled element type: {t:?} {c} normalized={n}"),
    }
}

#[derive(Debug)]
pub enum Buffer {
    Internal { byte_length: usize },
    External(gltf::json::Buffer),
}
