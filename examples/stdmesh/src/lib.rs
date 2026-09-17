use std::{
    fs::File,
    io::{BufReader, Read, Seek, SeekFrom},
};

use bedrock::{
    self as br, CommandBufferMut, DescriptorPoolMut, Device, RenderPass, ShaderModule,
    TypedVulkanStructure, VkHandle,
};
use peridot::math::One;

pub async fn game_main<'e>(e: &mut peridot::Engine<'e, impl peridot::NativeLinker>) {
    let mut backbuffer_size = e.back_buffer_size();

    let shader = e
        .load_async::<peridot_rendering_configuration::CompiledRenderingConfigurationVk>(
            "shaders.test",
        )
        .await
        .expect("shader load failed");
    let peridot_rendering_configuration::ShadingPassVk::Custom { ref variants, .. } =
        shader.passes["Unlit"]
    else {
        unreachable!();
    };
    let shading_variant =
        &variants[&peridot_rendering_configuration::VariantKey { instancing: false }];
    let shader_stage_with_entry_names = shading_variant
        .vertex_entry_point_name
        .as_deref()
        .map(|e| {
            (
                br::ShaderStage::Vertex,
                std::ffi::CString::new(e).expect("invalid entry point name"),
            )
        })
        .into_iter()
        .chain(
            shading_variant
                .fragment_entry_point_name
                .as_deref()
                .map(|e| {
                    (
                        br::ShaderStage::Fragment,
                        std::ffi::CString::new(e).expect("invlaid entry point name"),
                    )
                })
                .into_iter(),
        )
        .collect::<Vec<_>>();
    let shader_module = br::ShaderModuleObject::new(
        e.graphics_device().clone(),
        &br::ShaderModuleCreateInfo::new(&shading_variant.words),
    )
    .expect("shnader_module.create");
    let dsl_ub1 = br::DescriptorSetLayoutObject::new(
        e.graphics().device().clone(),
        &br::DescriptorSetLayoutCreateInfo::new(&[
            br::DescriptorType::UniformBuffer.make_binding(0, 1)
        ]),
    )
    .expect("dsl_ub1 new");
    let pl = br::PipelineLayoutObject::new(
        e.graphics_device().clone(),
        &br::PipelineLayoutCreateInfo::new(
            &[dsl_ub1.as_transparent_ref(), dsl_ub1.as_transparent_ref()],
            &if shading_variant.push_constant_buffer_size_bytes > 0 {
                vec![br::PushConstantRange::new(
                    br::vk::VK_SHADER_STAGE_ALL,
                    0..shading_variant.push_constant_buffer_size_bytes as _,
                )]
            } else {
                vec![]
            },
        ),
    )
    .expect("pl.create");

    let mut v_bindings = Vec::new();
    let mut v_attributes = Vec::new();
    let mut device_buffer_contents = Vec::new();
    let mut upload_buffer_ranges = Vec::new();

    // println!("{:?}", std::env::current_dir());
    let mut r = BufReader::new(
        File::open("../../examples/stdmesh/assets/mesh0.0.pa1-mesh").expect("file.open"),
    );
    let mut sig_buf = [0u8; 4];
    r.read_exact(&mut sig_buf).expect("r.read.sig");
    let sig = u32::from_ne_bytes(sig_buf);
    let needs_swap = if sig == PA1M_SIGNATURE {
        false
    } else if sig.swap_bytes() == PA1M_SIGNATURE {
        true
    } else {
        panic!("invalid pa1m signature");
    };
    let hdr = MeshHeader::deserialize(&mut r).expect("mesh_header.deserialize");
    println!("{needs_swap} {hdr:?}");
    let index_stream =
        MeshIndexStream::deserialize(&mut r, needs_swap).expect("index_stream.deserialize");
    println!("{index_stream:?}");
    let (index_type_vk, index_count) =
        if let MeshIndexStream::Stream { index_type, buffer } = index_stream {
            let index_count = match index_type {
                IndexType::UInt16 => {
                    device_buffer_contents.push(peridot::BufferContent::indices::<u16>(
                        (buffer.byte_length / 2) as _,
                    ));
                    upload_buffer_ranges.push((
                        0..buffer.byte_length,
                        buffer.content_location,
                        buffer.byte_length,
                    ));
                    (buffer.byte_length / 2) as _
                }
                IndexType::UInt32 => {
                    device_buffer_contents.push(peridot::BufferContent::indices::<u32>(
                        (buffer.byte_length / 4) as _,
                    ));
                    upload_buffer_ranges.push((
                        0..buffer.byte_length,
                        buffer.content_location,
                        buffer.byte_length,
                    ));
                    (buffer.byte_length / 4) as _
                }
            };

            (Some(index_type.into_vk()), index_count)
        } else {
            (None, 0)
        };
    let mut v_streams = Vec::with_capacity(hdr.vertex_stream_count as _);
    for binding in 0..hdr.vertex_stream_count {
        let vst =
            MeshVertexStream::deserialize(&mut r, needs_swap).expect("vertex_stream.deserialize");
        let mut attributes = Vec::with_capacity(vst.attribute_count as _);
        let mut buffer_stride = 0;
        for _ in 0..vst.attribute_count {
            let attr = Attribute::deserialize(&mut r).expect("attribute.deserialize");
            let attr_data =
                AttributeData::deserialize(&mut r, needs_swap).expect("attribute_data.deserialize");
            buffer_stride = buffer_stride
                .max(attr_data.offset as u32 + attr_data.element_type.default_byte_stride() as u32);
            if let Some(&loc) = shading_variant
                .vertex_semantic_to_location
                .get(&attr.into_semantic())
            {
                v_attributes.push(br::VertexInputAttributeDescription(
                    br::vk::VkVertexInputAttributeDescription {
                        binding: binding as _,
                        location: loc,
                        format: attr_data.element_type.into_vk_format(),
                        offset: attr_data.offset as _,
                    },
                ));
            } else {
                eprintln!("attribute {attr:?} is not supported on the shader, ignoring");
            }
            attributes.push((attr, attr_data));
        }
        device_buffer_contents.push(peridot::BufferContent::Vertex(
            vst.buffer.byte_length as _,
            vst.buffer.device_alignment_requirement as _,
        ));
        let upload_buffer_base = upload_buffer_ranges.last().map_or(0, |x| {
            (x.0.end + (vst.buffer.device_alignment_requirement - 1))
                & !(vst.buffer.device_alignment_requirement - 1)
        });
        upload_buffer_ranges.push((
            upload_buffer_base..upload_buffer_base + vst.buffer.byte_length,
            vst.buffer.content_location,
            vst.buffer.byte_length,
        ));
        v_streams.push((vst, attributes));
        v_bindings.push(br::VertexInputBindingDescription(
            br::vk::VkVertexInputBindingDescription {
                binding: binding as _,
                stride: buffer_stride,
                inputRate: br::vk::VK_VERTEX_INPUT_RATE_VERTEX,
            },
        ));
    }
    println!("{v_streams:?}");

    let mut camera = peridot::math::Camera {
        projection: Some(peridot::math::ProjectionMethod::Perspective {
            fov: 60.0f32.to_radians(),
        }),
        position: peridot::math::Vector3(0.0, 2.0, -10.0),
        rotation: peridot::math::Quaternion::ONE,
        depth_range: 0.1..100.0,
    };
    camera.look_at(peridot::math::Vector3(0.0, 0.0, 0.0));

    let mut memory_manager = peridot_memory_manager::MemoryManager::new(e.graphics());
    let (device_buffer, device_buffer_offsets) = memory_manager
        .allocate_device_local_buffer_with_contents(
            e.graphics(),
            device_buffer_contents.into_iter().chain([
                peridot::BufferContent::uniform::<
                    peridot_rendering_configuration::UniformCameraParameters,
                >(),
                peridot::BufferContent::uniform::<
                    peridot_rendering_configuration::UniformObjectParameters,
                >(),
            ]),
            br::BufferUsage::TRANSFER_DEST,
        )
        .expect("device_buffer.alloc");
    struct BufferInitContent {
        camera_parameters: peridot_rendering_configuration::UniformCameraParameters,
        object_parameters: peridot_rendering_configuration::UniformObjectParameters,
    }
    let buffer_init_content_base_offset = upload_buffer_ranges
        .last()
        .map_or(0, |x| (x.0.end + 15) & !15);
    let mut upload_buffer = memory_manager
        .allocate_upload_buffer(
            e.graphics(),
            br::BufferCreateInfo::new(
                (buffer_init_content_base_offset as usize + size_of::<BufferInitContent>()) as _,
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("upload_buffer.alloc");
    upload_buffer
        .guard_map(peridot_memory_manager::BufferMapMode::Write, |p| unsafe {
            for &(ref br, co, bl) in upload_buffer_ranges.iter() {
                r.seek(SeekFrom::Start(co)).expect("reader.seek");
                r.read_exact(core::slice::from_raw_parts_mut(
                    p.ptr().byte_add(br.start as _).cast::<u8>().as_ptr(),
                    (br.end - br.start) as _,
                ))
                .expect("reader.read");
            }

            p.ptr()
                .byte_add(buffer_init_content_base_offset as _)
                .cast::<BufferInitContent>()
                .write(BufferInitContent {
                    camera_parameters: peridot_rendering_configuration::UniformCameraParameters {
                        view_projection_matrix: camera.view_projection_matrix(
                            backbuffer_size.0 as f32 / backbuffer_size.1 as f32,
                        ),
                    },
                    object_parameters: peridot_rendering_configuration::UniformObjectParameters {
                        transform_matrix: peridot::math::Matrix4::ONE,
                    },
                });
        })
        .expect("upload_buffer.write");
    e.submit_commands(|rec| {
        rec.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::TRANSFER,
            0,
            &[br::vk::VkMemoryBarrier {
                sType: br::vk::VkMemoryBarrier::TYPE,
                pNext: core::ptr::null(),
                srcAccessMask: 0,
                dstAccessMask: br::AccessFlags::TRANSFER.write,
            }],
            &[],
            &[],
        )
        .copy_buffer(
            &upload_buffer,
            &device_buffer,
            &upload_buffer_ranges
                .iter()
                .zip(device_buffer_offsets.iter())
                .map(|((br, _, _), &dbo)| {
                    br::BufferCopy(br::vk::VkBufferCopy {
                        srcOffset: br.start as _,
                        dstOffset: dbo as _,
                        size: (br.end - br.start) as _,
                    })
                })
                .chain([
                    br::BufferCopy::copy_data::<
                        peridot_rendering_configuration::UniformCameraParameters,
                    >(
                        (buffer_init_content_base_offset as usize
                            + core::mem::offset_of!(BufferInitContent, camera_parameters))
                            as _,
                        device_buffer_offsets[device_buffer_offsets.len() - 2],
                    ),
                    br::BufferCopy::copy_data::<
                        peridot_rendering_configuration::UniformObjectParameters,
                    >(
                        (buffer_init_content_base_offset as usize
                            + core::mem::offset_of!(BufferInitContent, object_parameters))
                            as _,
                        device_buffer_offsets[device_buffer_offsets.len() - 1],
                    ),
                ])
                .collect::<Vec<_>>(),
        )
        .pipeline_barrier(
            br::PipelineStageFlags::TRANSFER,
            br::PipelineStageFlags::VERTEX_INPUT | br::PipelineStageFlags::VERTEX_SHADER,
            0,
            &[br::vk::VkMemoryBarrier {
                sType: br::vk::VkMemoryBarrier::TYPE,
                pNext: core::ptr::null(),
                srcAccessMask: br::AccessFlags::TRANSFER.write,
                dstAccessMask: br::AccessFlags::VERTEX_ATTRIBUTE_READ
                    | br::AccessFlags::INDEX_READ
                    | br::AccessFlags::UNIFORM_READ,
            }],
            &[],
            &[],
        )
    })
    .expect("engine.submit_commands");

    let mut dp = br::DescriptorPoolObject::new(
        e.graphics_device().clone(),
        &br::DescriptorPoolCreateInfo::new(2, &[br::DescriptorType::UniformBuffer.make_size(2)]),
    )
    .expect("dp new");
    let [camera_descriptor_set, object_descriptor_set] = dp
        .alloc_array(&[dsl_ub1.as_transparent_ref(), dsl_ub1.as_transparent_ref()])
        .expect("dp.alloc");
    e.graphics_device().update_descriptor_sets(
        &[
            camera_descriptor_set
                .binding_at(0)
                .write(br::DescriptorContents::uniform_buffer(
                    &device_buffer,
                    device_buffer_offsets[device_buffer_offsets.len() - 2]
                        ..device_buffer_offsets[device_buffer_offsets.len() - 2]
                            + size_of::<peridot_rendering_configuration::UniformCameraParameters>()
                                as u64,
                )),
            object_descriptor_set
                .binding_at(0)
                .write(br::DescriptorContents::uniform_buffer(
                    &device_buffer,
                    device_buffer_offsets[device_buffer_offsets.len() - 1]
                        ..device_buffer_offsets[device_buffer_offsets.len() - 1]
                            + size_of::<peridot_rendering_configuration::UniformObjectParameters>()
                                as u64,
                )),
        ],
        &[],
    );

    let render_pass = br::RenderPassObject::new(
        e.graphics_device().clone(),
        &br::RenderPassCreateInfo::new(
            &[
                e.back_buffer_attachment_desc()
                    .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store),
                br::AttachmentDescription::new(
                    br::vk::VK_FORMAT_D24_UNORM_S8_UINT,
                    br::ImageLayout::Undefined,
                    br::ImageLayout::DepthStencilAttachmentOpt,
                )
                .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare),
            ],
            &[br::SubpassDescription::new()
                .color_attachments(
                    &[br::AttachmentReference::new(
                        0,
                        br::ImageLayout::ColorAttachmentOpt,
                    )],
                    &[],
                )
                .depth_stencil_attachment(&br::AttachmentReference::new(
                    1,
                    br::ImageLayout::DepthStencilAttachmentOpt,
                ))],
            &[br::vk::VkSubpassDependency {
                srcSubpass: 0,
                dstSubpass: br::vk::VK_SUBPASS_EXTERNAL,
                srcAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
                dstAccessMask: br::AccessFlags::MEMORY.read,
                srcStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
                dstStageMask: e.requesting_back_buffer_layout().1.0,
                dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT,
            }],
        ),
    )
    .expect("render_pass.create");

    let depth_buffer = memory_manager
        .allocate_device_local_image(
            e.graphics(),
            br::ImageCreateInfo::new(backbuffer_size, br::vk::VK_FORMAT_D24_UNORM_S8_UINT)
                .with_usage(
                    br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT
                        | br::ImageUsageFlags::TRANSIENT_ATTACHMENT,
                ),
        )
        .expect("depth_buffer.create");
    let mut depth_buffer_view = br::ImageViewBuilder::new(
        depth_buffer,
        br::ImageSubresourceRange::new(br::AspectMask::DEPTH, 0..1, 0..1),
    )
    .create()
    .expect("depth_buffer_view.create");
    let mut backbuffers = e
        .iter_back_buffers()
        .map(|x| BackbufferImageView {
            image: x.native_ptr(),
            view: unsafe {
                br::vkfn_wrapper::create_image_view(
                    e.graphics_device().as_transparent_ref(),
                    &br::ImageViewCreateInfo::new(
                        &x,
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                        br::vk::VK_IMAGE_VIEW_TYPE_2D,
                        e.back_buffer_format(),
                    ),
                    None,
                )
                .expect("backbuffer.image_view.create")
            },
            device: e.graphics_device().clone(),
        })
        .collect::<Vec<_>>();
    let mut framebuffers = backbuffers
        .iter()
        .map(|bb| {
            br::FramebufferObject::new(
                e.graphics_device().clone(),
                &br::FramebufferCreateInfo::new(
                    &render_pass,
                    &[
                        bb.as_transparent_ref(),
                        depth_buffer_view.as_transparent_ref(),
                    ],
                    backbuffer_size.0,
                    backbuffer_size.1,
                ),
            )
            .expect("framebuffer.create")
        })
        .collect::<Vec<_>>();

    let scissor_rects = [br::Extent2D::from(backbuffer_size).into_rect(br::Offset2D::ZERO)];
    let viewports = [scissor_rects[0].make_viewport(0.0..1.0)];
    let [gp] = e
        .graphics_device()
        .new_graphics_pipeline_array(
            &[br::GraphicsPipelineCreateInfo::new(
                &pl,
                render_pass.subpass(0),
                &shader_stage_with_entry_names
                    .iter()
                    .map(|&(s, ref e)| shader_module.on_stage(s, e))
                    .collect::<Vec<_>>(),
                &br::PipelineVertexInputStateCreateInfo::new(&v_bindings, &v_attributes),
                &br::PipelineInputAssemblyStateCreateInfo::new(hdr.primitive_topology.into_vk()),
                &br::PipelineViewportStateCreateInfo::new(&viewports, &scissor_rects),
                &br::PipelineRasterizationStateCreateInfo::new(
                    br::PolygonMode::Fill,
                    br::CullModeFlags::BACK,
                    br::FrontFace::CounterClockwise,
                ),
                &br::PipelineColorBlendStateCreateInfo::new(&[
                    br::PipelineColorBlendAttachmentState::NOBLEND,
                ]),
            )
            .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
            .set_depth_stencil_state(
                &br::PipelineDepthStencilStateCreateInfo::new()
                    .config_depth(Some(br::CompareOp::Less), true),
            )],
            None::<&br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("pipeline.create");
    let mut gp = gp.clone_parent();

    let mut render_cp = br::CommandPoolObject::new(
        e.graphics_device().clone(),
        &br::CommandPoolCreateInfo::new(e.graphics_queue_family_index()),
    )
    .expect("render_cp.create");
    let mut render_cb = br::CommandBufferObject::alloc(
        e.graphics_device().clone(),
        &br::CommandBufferAllocateInfo::new(
            &mut render_cp,
            framebuffers.len() as _,
            br::CommandBufferLevel::Primary,
        ),
    )
    .expect("render_cb.alloc");
    for (n, cb) in render_cb.iter_mut().enumerate() {
        unsafe {
            cb.begin(&br::CommandBufferBeginInfo::new())
                .expect("render_cb.begin")
        }
        .begin_render_pass(
            &br::RenderPassBeginInfo::new(
                &render_pass,
                &framebuffers[n],
                br::Rect2D {
                    offset: br::Offset2D::ZERO,
                    extent: backbuffer_size.into(),
                },
                &[
                    br::ClearValue::color_f32([0.0, 0.0, 0.0, 0.0]),
                    br::ClearValue::depth_stencil(1.0, 0),
                ],
            ),
            br::SubpassContents::Inline,
        )
        .bind_pipeline(br::PipelineBindPoint::Graphics, &gp)
        .bind_descriptor_sets(
            br::PipelineBindPoint::Graphics,
            &pl,
            0,
            &[camera_descriptor_set, object_descriptor_set],
            &[],
        )
        .bind_vertex_buffers(
            0,
            &[
                device_buffer.as_transparent_ref(),
                device_buffer.as_transparent_ref(),
            ],
            &[device_buffer_offsets[1], device_buffer_offsets[2]],
        )
        .bind_index_buffer(
            &device_buffer,
            device_buffer_offsets[0] as _,
            index_type_vk.expect("no index?"),
        )
        .draw_indexed(index_count, 1, 0, 0, 0)
        .end_render_pass()
        .end()
        .expect("render_cb.end");
    }

    loop {
        match e.next_event().await {
            peridot::Event::Shutdown => break,
            peridot::Event::Resize(new_size) => {}
            peridot::Event::NextFrame => {
                let fd = match e.prepare_frame() {
                    Ok(x) => x,
                    Err(peridot::PrepareFrameError::FramebufferOutOfDate) => {
                        todo!("framebuffer out of date");
                    }
                };

                let mut render_submissions = peridot::SubmissionBatchBuilder::new();
                render_submissions.add_command_buffers([
                    render_cb[fd.backbuffer_index as usize].as_transparent_ref()
                ]);
                e.do_render(fd.backbuffer_index, None, render_submissions)
                    .expect("engine.do_render");
            }
        }
    }

    unsafe {
        e.graphics_device().wait().expect("device.wait");
    }
}

struct BackbufferImageView {
    image: br::vk::VkImage,
    view: br::vk::VkImageView,
    device: peridot::VulkanGfx,
}
impl Drop for BackbufferImageView {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image_view(
                self.device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.view),
                None,
            );
        }
    }
}
impl br::VkHandle for BackbufferImageView {
    type Handle = br::vk::VkImageView;

    fn native_ptr(&self) -> Self::Handle {
        self.view
    }
}

const PA1M_SIGNATURE: u32 = u32::from_be_bytes(*b"pa1m");

#[derive(Debug)]
struct MeshHeader {
    pub primitive_topology: MeshPrimitiveTopology,
    pub vertex_stream_count: u8,
}
impl MeshHeader {
    pub fn deserialize(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let mut primitive_topology_buf = [0u8];
        let mut vertex_stream_count_buf = [0u8];
        readva(
            r,
            &mut [
                std::io::IoSliceMut::new(&mut primitive_topology_buf),
                std::io::IoSliceMut::new(&mut vertex_stream_count_buf),
            ],
        )?;

        Ok(Self {
            primitive_topology: MeshPrimitiveTopology::try_from(primitive_topology_buf[0])
                .expect("invalid topology value"),
            vertex_stream_count: vertex_stream_count_buf[0],
        })
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
impl TryFrom<u8> for MeshPrimitiveTopology {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if Self::Points as u8 <= value && value <= Self::TriangleFan as u8 {
            Ok(unsafe { core::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}
impl MeshPrimitiveTopology {
    pub fn into_vk(self) -> br::PrimitiveTopology {
        match self {
            Self::Points => br::PrimitiveTopology::PointList,
            Self::Lines => br::PrimitiveTopology::LineList,
            Self::LineLoop => todo!("line loop support?"),
            Self::LineStrip => br::PrimitiveTopology::LineStrip,
            Self::Triangles => br::PrimitiveTopology::TriangleList,
            Self::TriangleStrip => br::PrimitiveTopology::TriangleStrip,
            Self::TriangleFan => br::PrimitiveTopology::TriangleFan,
        }
    }
}

#[derive(Debug)]
pub struct StreamBuffer {
    pub content_location: u64,
    pub byte_length: u32,
    pub device_alignment_requirement: u32,
}
impl StreamBuffer {
    pub fn deserialize(r: &mut (impl Read + ?Sized), needs_swap: bool) -> std::io::Result<Self> {
        let mut content_location_buf = [0u8; 8];
        let mut byte_length_buf = [0u8; 4];
        let mut device_alignment_requirement_buf = [0u8; 4];
        readva(
            r,
            &mut [
                std::io::IoSliceMut::new(&mut content_location_buf),
                std::io::IoSliceMut::new(&mut byte_length_buf),
                std::io::IoSliceMut::new(&mut device_alignment_requirement_buf),
            ],
        )?;
        let mut content_location = u64::from_ne_bytes(content_location_buf);
        let mut byte_length = u32::from_ne_bytes(byte_length_buf);
        let mut device_alignment_requirement = u32::from_ne_bytes(device_alignment_requirement_buf);
        if needs_swap {
            content_location = content_location.swap_bytes();
            byte_length = byte_length.swap_bytes();
            device_alignment_requirement = device_alignment_requirement.swap_bytes();
        }

        Ok(Self {
            content_location,
            byte_length,
            device_alignment_requirement,
        })
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
    pub fn deserialize(r: &mut (impl Read + ?Sized), needs_swap: bool) -> std::io::Result<Self> {
        let mut index_type_buf = [0u8];
        r.read_exact(&mut index_type_buf)?;
        if index_type_buf[0] == 0 {
            return Ok(Self::None);
        }

        let index_type = IndexType::try_from(index_type_buf[0]).expect("invalid index type");
        let buffer = StreamBuffer::deserialize(r, needs_swap)?;
        Ok(Self::Stream { index_type, buffer })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum IndexType {
    UInt16 = 1,
    UInt32 = 2,
}
impl TryFrom<u8> for IndexType {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        if Self::UInt16 as u8 <= value && value <= Self::UInt32 as u8 {
            Ok(unsafe { core::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}
impl IndexType {
    pub const fn into_vk(self) -> br::IndexType {
        match self {
            IndexType::UInt16 => br::IndexType::U16,
            IndexType::UInt32 => br::IndexType::U32,
        }
    }
}

#[derive(Debug)]
pub struct MeshVertexStream {
    pub buffer: StreamBuffer,
    pub attribute_count: u8,
}
impl MeshVertexStream {
    pub fn deserialize(r: &mut (impl Read + ?Sized), needs_swap: bool) -> std::io::Result<Self> {
        let buffer = StreamBuffer::deserialize(r, needs_swap)?;
        let mut attribute_count_buf = [0u8];
        readva(r, &mut [std::io::IoSliceMut::new(&mut attribute_count_buf)])?;

        Ok(Self {
            buffer,
            attribute_count: attribute_count_buf[0],
        })
    }
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
    pub fn deserialize(r: &mut (impl Read + ?Sized)) -> std::io::Result<Self> {
        let mut buf = [0u8];
        r.read_exact(&mut buf)?;

        match buf[0] {
            0 => Ok(Self::Position),
            1 => Ok(Self::Normal),
            2 => Ok(Self::Tangent),
            0x08..0x10 => Ok(Self::Texcoord(buf[0] - 0x08)),
            0x10..0x18 => Ok(Self::Color(buf[0] - 0x10)),
            0x18..0x20 => Ok(Self::Joints(buf[0] - 0x18)),
            0x20..0x28 => Ok(Self::Weights(buf[0] - 0x20)),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "invalid attribute",
            )),
        }
    }

    pub fn into_semantic(&self) -> peridot_rendering_configuration::VertexInputSemantic {
        match self {
            Self::Position => peridot_rendering_configuration::VertexInputSemantic::Position(0),
            Self::Normal => peridot_rendering_configuration::VertexInputSemantic::Normal(0),
            Self::Tangent => peridot_rendering_configuration::VertexInputSemantic::Tangent(0),
            &Self::Texcoord(n) => peridot_rendering_configuration::VertexInputSemantic::Texcoord(n),
            &Self::Color(n) => peridot_rendering_configuration::VertexInputSemantic::Color(n),
            &Self::Joints(n) => todo!("joints"),
            &Self::Weights(n) => todo!("weights"),
        }
    }
}

#[derive(Debug)]
pub struct AttributeData {
    pub offset: u16,
    pub element_type: BufferElementType,
}
impl AttributeData {
    pub fn deserialize(r: &mut (impl Read + ?Sized), needs_swap: bool) -> std::io::Result<Self> {
        let mut offset_buf = [0u8; 2];
        let mut element_type_buf = [0u8; 2];
        readva(
            r,
            &mut [
                std::io::IoSliceMut::new(&mut offset_buf),
                std::io::IoSliceMut::new(&mut element_type_buf),
            ],
        )?;
        let mut offset = u16::from_ne_bytes(offset_buf);
        let mut element_type_v = u16::from_ne_bytes(element_type_buf);
        if needs_swap {
            offset = offset.swap_bytes();
            element_type_v = element_type_v.swap_bytes();
        }
        let element_type =
            BufferElementType::try_from(element_type_v).expect("invalid buffer element type");

        Ok(Self {
            offset,
            element_type,
        })
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
impl TryFrom<u16> for BufferElementType {
    type Error = u16;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        if Self::Ushort as u16 <= value && value <= Self::Float4 as u16 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(value)
        }
    }
}
impl BufferElementType {
    pub const fn default_byte_stride(&self) -> usize {
        match self {
            Self::Ushort => 2,
            Self::Float2 => 2 * 4,
            Self::Float3 => 3 * 4,
            Self::Float4 => 4 * 4,
        }
    }

    pub const fn into_vk_format(&self) -> br::Format {
        match self {
            Self::Ushort => br::vk::VK_FORMAT_R16_UINT,
            Self::Float2 => br::vk::VK_FORMAT_R32G32_SFLOAT,
            Self::Float3 => br::vk::VK_FORMAT_R32G32B32_SFLOAT,
            Self::Float4 => br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
        }
    }
}

fn readva(
    r: &mut (impl Read + ?Sized),
    mut vec: &mut [std::io::IoSliceMut],
) -> std::io::Result<()> {
    std::io::IoSliceMut::advance_slices(&mut vec, 0);

    while !vec.is_empty() {
        let b = r.read_vectored(vec)?;
        std::io::IoSliceMut::advance_slices(&mut vec, b);
    }

    Ok(())
}
