use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, BufWriter, Read, Seek, SeekFrom, Write},
    path::Path,
};

use peridot_mesh::{
    Attribute, AttributeData, BufferElementType, Header, IndexStream, IndexType, PrimitiveTopology,
    SIGNATURE, StreamBuffer, VertexStream,
};
use peridot_tp_gltf as gltf;

macro_rules! file_assert {
    ($cond: expr, $msg: literal) => {
        if !$cond {
            return Err(ProcessError::FileCorruption($msg));
        }
    };
}

#[derive(thiserror::Error, Debug)]
pub enum ProcessError {
    #[error(transparent)]
    IO(#[from] std::io::Error),
    #[error(transparent)]
    Json(#[from] serde_json::Error),
    #[error("file corrupt: {0}")]
    FileCorruption(&'static str),
    #[error("file corrupt: invalid index buffer element type: {0:?}")]
    InvalidIndexBufferElementType(BufferElementType),
    #[error("too large index buffer")]
    TooLargeIndexBuffer,
    #[error("too many attributes")]
    TooManyAttributes,
    #[error("too many vertex streams")]
    TooManyVertexStreams,
}

pub fn process(mut r: BufReader<File>, primary_mesh_out_path: &Path) -> Result<(), ProcessError> {
    let _hdr = gltf::binary::Header::read(&mut r)?;

    let chunk0_hdr = gltf::binary::ChunkHeader::read(&mut r)?;
    file_assert!(
        chunk0_hdr.r#type == gltf::binary::ChunkType::Json,
        "chunk 0 must be a json"
    );
    let mut content = Vec::<u8>::with_capacity(chunk0_hdr.length as usize);
    r.read_exact(unsafe {
        core::mem::transmute(&mut content.spare_capacity_mut()[..chunk0_hdr.length as usize])
    })?;
    unsafe {
        content.set_len(chunk0_hdr.length as usize);
    }
    let content = unsafe { str::from_utf8_unchecked(&content) };
    r.seek(SeekFrom::Current(chunk0_hdr.padding_tail_length() as _))?; // skip for padding
    let bin_chunk_base = r.stream_position()?;
    let parsed = serde_json::from_str::<gltf::json::GLTF>(content)?;

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
        r.seek(SeekFrom::Start(bin_chunk_base))?;
        let chunk1_hdr = gltf::binary::ChunkHeader::read(&mut r)?;
        file_assert!(
            chunk1_hdr.r#type == gltf::binary::ChunkType::Bin,
            "chunk 1 must be a bin"
        );

        Some(r.stream_position()?)
    } else {
        None
    };

    for (mesh_index, x) in parsed.meshes.iter().enumerate() {
        if !x.weights.is_empty() {
            tracing::warn!("TODO: mesh morphing is not supported by the processor");
        }

        for (prim_index, x) in x.primitives.iter().enumerate() {
            let topo = primitive_topology_from_gltf(x);

            #[derive(Debug)]
            struct SourceBufferData {
                buffer_index: usize,
                buffer_range: core::range::Range<usize>,
                byte_stride: usize,
            }
            let (mut index_stream, index_source_data) = if let Some(indices) = x.indices {
                let accessor = &parsed.accessors[indices];
                let buffer_view = match accessor.buffer_view {
                    Some(n) => &parsed.buffer_views[n],
                    None => todo!("accessor without buffer view"),
                };
                let buffer_element_type = buffer_element_type_from_accessor(accessor);

                (
                    IndexStream::Stream {
                        index_type: match buffer_element_type {
                            BufferElementType::Ushort => IndexType::UInt16,
                            e => return Err(ProcessError::InvalidIndexBufferElementType(e)),
                        },
                        buffer: StreamBuffer {
                            content_location: 0, // compute later
                            byte_length: (accessor.count * buffer_element_type.size())
                                .try_into()
                                .map_err(|_| ProcessError::TooLargeIndexBuffer)?,
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
                    tracing::warn!(name = n, "[SKIP] unsupported attr name");
                    continue;
                };
                attr_name.assert_validate();

                let accessor = &parsed.accessors[x];
                let buffer_view = match accessor.buffer_view {
                    Some(n) => &parsed.buffer_views[n],
                    None => todo!("accessor without buffer view"),
                };
                let buffer_element_type = buffer_element_type_from_accessor(accessor);

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
                        unreachable!("same attribute occured in a mesh primitive: {:?}", e.key());
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
                        file_assert!(
                            count == accessor.count,
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

                    Ok((
                        VertexStream {
                            buffer: StreamBuffer {
                                content_location: 0,
                                byte_length: (attribute_count as usize * offset as usize) as _,
                                device_alignment_requirement,
                            },
                            attribute_count: attribute_data
                                .len()
                                .try_into()
                                .map_err(|_| ProcessError::TooManyAttributes)?,
                        },
                        attribute_data,
                    ))
                })
                .collect::<Result<Vec<_>, ProcessError>>()?;
            // println!("{index_stream:#?}");
            // println!("{index_source_data:#?}");
            // println!("{stream_attributes:#?}");

            let mut opath = primary_mesh_out_path.to_owned();
            opath.set_file_name(format!(
                "{}-mesh{mesh_index}-{prim_index}.pa1-mesh",
                opath.file_stem().map_or("", |x| {
                    let src = x.to_str().expect("filepath cannot process");
                    &src[..src.len() - "-mesh0-0".len()]
                })
            ));
            let mut mesh_out = BufWriter::new(File::create(opath)?);
            mesh_out.write_all(&SIGNATURE.to_ne_bytes())?;
            Header {
                primitive_topology: topo,
                vertex_stream_count: stream_attributes
                    .len()
                    .try_into()
                    .map_err(|_| ProcessError::TooManyVertexStreams)?,
            }
            .serialize(&mut mesh_out)?;

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
            index_stream.serialize(&mut mesh_out)?;
            for (stream, attrs) in stream_attributes.iter_mut() {
                stream.buffer.content_location = content_offset as _;
                content_offset += stream.buffer.byte_length as usize;

                stream.serialize(&mut mesh_out)?;
                for (a, d, _) in attrs {
                    a.serialize(&mut mesh_out)?;
                    d.serialize(&mut mesh_out)?;
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
                    ))?;
                    let mut buffer = Vec::with_capacity(dest_stride);
                    r.read_exact(unsafe {
                        core::mem::transmute(&mut buffer.spare_capacity_mut()[..dest_stride])
                    })?;
                    unsafe {
                        buffer.set_len(dest_stride);
                    }
                    mesh_out.write_all(&buffer)?;

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
                                ))?;
                                &mut r
                            }
                            Buffer::External(_) => todo!("external buffer support"),
                        };

                        let mut buffer = Vec::<u8>::with_capacity(*dest_stride);
                        reader.read_exact(unsafe {
                            core::mem::transmute(&mut buffer.spare_capacity_mut()[..*dest_stride])
                        })?;
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

                        mesh_out.write_all(&buffer)?;
                    }
                }
            }
        }
    }

    Ok(())
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
        (gltf::json::AccessorType::Scalar, gltf::json::COMPONENT_TYPE_UNSIGNED_BYTE, false) => {
            BufferElementType::Byte
        }
        (gltf::json::AccessorType::Vec2, gltf::json::COMPONENT_TYPE_UNSIGNED_BYTE, false) => {
            BufferElementType::Byte2
        }
        (gltf::json::AccessorType::Vec3, gltf::json::COMPONENT_TYPE_UNSIGNED_BYTE, false) => {
            BufferElementType::Byte3
        }
        (gltf::json::AccessorType::Vec4, gltf::json::COMPONENT_TYPE_UNSIGNED_BYTE, false) => {
            BufferElementType::Byte4
        }
        (gltf::json::AccessorType::Scalar, gltf::json::COMPONENT_TYPE_UNSIGNED_BYTE, true) => {
            BufferElementType::ByteNormalized
        }
        (gltf::json::AccessorType::Vec2, gltf::json::COMPONENT_TYPE_UNSIGNED_BYTE, true) => {
            BufferElementType::Byte2Normalized
        }
        (gltf::json::AccessorType::Vec3, gltf::json::COMPONENT_TYPE_UNSIGNED_BYTE, true) => {
            BufferElementType::Byte3Normalized
        }
        (gltf::json::AccessorType::Vec4, gltf::json::COMPONENT_TYPE_UNSIGNED_BYTE, true) => {
            BufferElementType::Byte4Normalized
        }
        (gltf::json::AccessorType::Scalar, gltf::json::COMPONENT_TYPE_UNSIGNED_SHORT, false) => {
            BufferElementType::Ushort
        }
        (gltf::json::AccessorType::Scalar, gltf::json::COMPONENT_TYPE_UNSIGNED_SHORT, true) => {
            BufferElementType::UshortNormalized
        }
        (gltf::json::AccessorType::Vec2, gltf::json::COMPONENT_TYPE_UNSIGNED_SHORT, true) => {
            BufferElementType::Ushort2Normalized
        }
        (gltf::json::AccessorType::Vec3, gltf::json::COMPONENT_TYPE_UNSIGNED_SHORT, true) => {
            BufferElementType::Ushort3Normalized
        }
        (gltf::json::AccessorType::Vec4, gltf::json::COMPONENT_TYPE_UNSIGNED_SHORT, true) => {
            BufferElementType::Ushort4Normalized
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
