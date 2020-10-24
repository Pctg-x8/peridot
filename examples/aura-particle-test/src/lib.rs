#![feature(map_first_last)]

use bedrock as br;
use br::Waitable;
use std::rc::Rc;
use peridot::math::One;

mod particle; use self::particle::*;

pub struct RenderingResources {
    lines_gp: peridot::LayoutedPipeline,
    gp: peridot::LayoutedPipeline,
    fbs: Vec<br::Framebuffer>,
    depth_buffer: peridot::Image,
    depth_buffer_view: br::ImageView
}
impl RenderingResources {
    const DEPTH_BUFFER_FORMAT: br::vk::VkFormat = br::vk::VK_FORMAT_D24_UNORM_S8_UINT;

    pub fn new<NL: peridot::NativeLinker>(
        e: &peridot::Engine<NL>, srr: &StaticRenderResources, layouts: &Layouts
    ) -> Self {
        let depth_buffer = br::ImageDesc::new(
            e.backbuffers()[0].size(), Self::DEPTH_BUFFER_FORMAT,
            br::ImageUsage::DEPTH_STENCIL_ATTACHMENT, br::ImageLayout::Undefined
        ).create(e.graphics()).expect("Failed to create depth buffer image");
        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(depth_buffer);
        let depth_buffer = mb.alloc().expect("Failed to alloc depth buffer memory")
            .pop().expect("no objects?").unwrap_image();
        let depth_buffer_view = depth_buffer.create_view(
            None, None, &br::ComponentMapping::default(), &br::ImageSubresourceRange::depth_stencil(0 .. 1, 0 .. 1)
        ).expect("Failed to create ImageView for DepthStencil");
        
        let default_shader_uv_v = e.load::<peridot::SpirvShaderBlob>("shaders.uv_v")
            .expect("Failed to load vertex shader")
            .instantiate(e.graphics())
            .expect("Failed to instantiate vertex shader");
        let default_shader_blit_f = e.load::<peridot::SpirvShaderBlob>("shaders.blit_f")
            .expect("Failed to load fragment shader")
            .instantiate(e.graphics())
            .expect("Failed to instantiate fragment shader");
        let cv_shader_v = e.load::<peridot::SpirvShaderBlob>("shaders.colored_vertex_v")
            .expect("Failed to load colored vertex shader")
            .instantiate(e.graphics())
            .expect("Failed to instantiate colored vertex shader");
        let cv_shader_f = e.load::<peridot::SpirvShaderBlob>("shaders.colored_vertex_f")
            .expect("Failed to load colored fragment shader")
            .instantiate(e.graphics())
            .expect("Failed to instantiate colored fragment shader");

        let psl_grid = Rc::new(
            br::PipelineLayout::new(e.graphics(), &[&layouts.dsl_ub1v], &[])
                .expect("Failed to create GridPipelineState")
        );
        let psl = Rc::new(
            br::PipelineLayout::new(e.graphics(), &[&layouts.dsl_ub1v, &layouts.dsl_sb1v], &[])
                .expect("Failed to create PipelineState")
        );
        let sc = br::vk::VkRect2D {
            offset: br::vk::VkOffset2D::default(),
            extent: AsRef::<br::Extent2D>::as_ref(e.backbuffers()[0].size()).clone().into()
        };
        let vp = br::Viewport::from_rect_with_depth_range(&sc, 0.0 .. 1.0).into();
        let bindings = &[
            br::VertexInputBindingDescription::per_vertex_typed::<peridot::VertexUV3D>(0)
        ];
        let bindings_cv = &[
            br::VertexInputBindingDescription::per_vertex_typed::<peridot::ColoredVertex>(0)
        ];
        let mut vps_grid = br::VertexProcessingStages::new(
            br::PipelineShader {
                module: &cv_shader_v,
                entry_name: std::ffi::CString::new("main").expect("Failed to alloc cstring"),
                specinfo: None
            },
            bindings_cv,
            &[
                br::vk::VkVertexInputAttributeDescription {
                    location: 0,
                    binding: 0,
                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                    offset: 0
                },
                br::vk::VkVertexInputAttributeDescription {
                    location: 1,
                    binding: 0,
                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                    offset: 4 * 4
                }
            ],
            br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST
        );
        vps_grid.fragment_shader(br::PipelineShader {
            module: &cv_shader_f,
            entry_name: std::ffi::CString::new("main").expect("Failed to alloc cstring"),
            specinfo: None
        });
        let mut gpb = br::GraphicsPipelineBuilder::new(&psl_grid, (&srr.rp_main, 0), vps_grid);
        gpb.multisample_state(Some(br::MultisampleState::new()));
        gpb.add_attachment_blend(br::AttachmentColorBlendState::premultiplied());
        gpb.viewport_scissors(br::DynamicArrayState::Static(&[vp]), br::DynamicArrayState::Static(&[sc]));
        gpb.depth_write_enable(true)
            .depth_test_enable(true)
            .depth_compare_op(br::CompareOp::Less);
        let lines_gp = peridot::LayoutedPipeline::combine(
            gpb.create(e.graphics(), None).expect("Failed to create Pipeline"),
            &psl_grid
        );
        let mut vps = br::VertexProcessingStages::new(
            br::PipelineShader {
                module: &default_shader_uv_v,
                entry_name: std::ffi::CString::new("main").expect("Failed to alloc cstring"),
                specinfo: None
            },
            bindings,
            &[
                br::vk::VkVertexInputAttributeDescription {
                    location: 0,
                    binding: 0,
                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                    offset: 0
                },
                br::vk::VkVertexInputAttributeDescription {
                    location: 1,
                    binding: 0,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                    offset: 4 * 4
                }
            ],
            br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP
        );
        vps.fragment_shader(br::PipelineShader {
            module: &default_shader_blit_f,
            entry_name: std::ffi::CString::new("main").expect("Failed to alloc cstring"),
            specinfo: None
        });
        gpb.layout(&psl).vertex_processing(vps);
        gpb.depth_write_enable(false);
        let gp = peridot::LayoutedPipeline::combine(
            gpb.create(e.graphics(), None).expect("Failed to create Pipeline"),
            &psl
        );

        e.submit_commands(|r| {
            r.pipeline_barrier(
                br::PipelineStageFlags::BOTTOM_OF_PIPE, br::PipelineStageFlags::EARLY_FRAGMENT_TESTS, true,
                &[], &[], &[br::ImageMemoryBarrier::new_raw(
                    &depth_buffer, &br::ImageSubresourceRange::depth_stencil(0 .. 1, 0 .. 1),
                    br::ImageLayout::Undefined, br::ImageLayout::DepthStencilAttachmentOpt
                )]
            );
        }).expect("Failed to initialize Depth Buffer");

        let fbs = e.backbuffers().iter()
            .map(|b|
                br::Framebuffer::new(&srr.rp_main, &[&b, &depth_buffer_view], b.size(), 1)
                    .expect("Failed to create Framebuffer")
            )
            .collect();

        RenderingResources {
            lines_gp, gp, fbs, depth_buffer, depth_buffer_view
        }
    }
}

pub struct Game<NL> {
    srr: StaticRenderResources,
    layouts: Layouts,
    res: Resources,
    desc: Descriptors,
    rr: peridot::Discardable<RenderingResources>,
    render_cmd: peridot::CommandBundle,
    update_cmd: peridot::CommandBundle,
    main_camera: peridot::math::Camera,
    dirty_main_camera: bool,
    aspect_wh: f32,
    _ph: std::marker::PhantomData<*const NL>
}

impl<NL> peridot::FeatureRequests for Game<NL> {

}
impl<NL> Game<NL> {
    pub const NAME: &'static str = "Peridot Aura test";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<NL: peridot::NativeLinker> peridot::EngineEvents<NL> for Game<NL> {
    fn init(e: &peridot::Engine<NL>) -> Self {
        let srr = StaticRenderResources::new(e);
        let layouts = Layouts::new(e);

        let mut tfb = peridot::TransferBatch::new();
        let res = Resources::new(e, &mut tfb);
        e.submit_commands(|b| { tfb.sink_transfer_commands(b); tfb.sink_graphics_ready_commands(b); })
            .expect("Failed to send initializers");
        
        let desc = Descriptors::new(e, &layouts, &res);
        let rr = RenderingResources::new(e, &srr, &layouts);
        let render_cmd = peridot::CommandBundle::new(
            e.graphics(), peridot::CBSubmissionType::Graphics, e.backbuffers().len()
        ).expect("Failed to alloc render commandbundle");
        Self::populate_all_render_commands(&render_cmd, &srr, &rr, &res, &desc);

        let update_cmd = peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Graphics, 1)
            .expect("Failed to allocate update cmd");
        let (particle_update_buffer, particle_update_range) = res.pe.staging_buffer_ranged();
        let barrier_in = &[
            br::BufferMemoryBarrier::new(
                &res.static_buffer.res,
                res.static_buffer.offsets.camera_range(),
                br::AccessFlags::UNIFORM_READ, br::AccessFlags::TRANSFER.write
            ),
            br::BufferMemoryBarrier::new(
                &res.static_buffer.res, res.pe.managed_instance_buffer_range(),
                br::AccessFlags::SHADER.read, br::AccessFlags::TRANSFER.write
            ),
            br::BufferMemoryBarrier::new(
                particle_update_buffer, particle_update_range.clone(),
                br::AccessFlags::HOST.write, br::AccessFlags::TRANSFER.read
            ),
            br::BufferMemoryBarrier::new(
                &res.update_buffer.res,
                res.update_buffer.offsets.camera_range(),
                br::AccessFlags::HOST.write, br::AccessFlags::TRANSFER.read
            )
        ];
        let barrier_out = &[
            br::BufferMemoryBarrier::new(
                &res.static_buffer.res,
                res.static_buffer.offsets.camera_range().into(),
                br::AccessFlags::TRANSFER.write, br::AccessFlags::UNIFORM_READ
            ),
            br::BufferMemoryBarrier::new(
                &res.static_buffer.res, res.pe.managed_instance_buffer_range(),
                br::AccessFlags::TRANSFER.write, br::AccessFlags::SHADER.read
            ),
            br::BufferMemoryBarrier::new(
                particle_update_buffer, particle_update_range.clone(),
                br::AccessFlags::TRANSFER.read, br::AccessFlags::HOST.write
            ),
            br::BufferMemoryBarrier::new(
                &res.update_buffer.res,
                res.update_buffer.offsets.camera_range(),
                br::AccessFlags::TRANSFER.read, br::AccessFlags::HOST.write
            )
        ];
        update_cmd[0].begin().expect("Failed to begin Recording Update Commands")
            .pipeline_barrier(
                br::PipelineStageFlags::HOST.vertex_shader(), br::PipelineStageFlags::TRANSFER, false,
                &[], barrier_in, &[]
            )
            .copy_buffer(&res.update_buffer.res, &res.static_buffer.res, &[
                br::vk::VkBufferCopy {
                    srcOffset: res.update_buffer.offsets.camera_matrix,
                    dstOffset: res.static_buffer.offsets.camera_matrix,
                    size: std::mem::size_of::<peridot::math::Matrix4F32>() as _
                }
            ])
            .copy_buffer(particle_update_buffer, &res.static_buffer.res, &[
                br::vk::VkBufferCopy {
                    srcOffset: 0,
                    dstOffset: res.pe.managed_instance_buffer_range().start,
                    size: particle_update_range.end - particle_update_range.start
                }
            ])
            .pipeline_barrier(
                br::PipelineStageFlags::TRANSFER, br::PipelineStageFlags::HOST.vertex_shader(), false,
                &[], barrier_out, &[]
            );

        e.graphics().wait().expect("Failed to wait initializers");
        let mut main_camera = peridot::math::Camera {
            projection: peridot::math::ProjectionMethod::Perspective { fov: 60.0f32.to_radians() },
            depth_range: 0.3 .. 20.0,
            position: peridot::math::Vector3(0.0, 2.0, 5.0),
            rotation: peridot::math::Quaternion::ONE
        };
        main_camera.look_at(peridot::math::Vector3(0.0, 0.0, 0.0));

        Game {
            srr,
            layouts,
            res,
            desc,
            rr: peridot::Discardable::from(rr),
            render_cmd,
            update_cmd,
            main_camera,
            aspect_wh: e.backbuffers()[0].size().0 as f32 / e.backbuffers()[0].size().1 as f32,
            // kicks initial updating
            dirty_main_camera: true,
            _ph: std::marker::PhantomData
        }
    }

    fn update(
        &mut self,
        _e: &peridot::Engine<NL>,
        on_backbuffer_of: u32,
        delta_time: std::time::Duration
    ) -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        self.res.pe.update(delta_time);

        let cp = {
            self.res.update_buffer.res.guard_map(self.res.update_buffer.offsets.camera_range(), |p| unsafe {
                *p.get_mut(0) = self.main_camera.projection_matrix(self.aspect_wh) * self.main_camera.view_matrix();
            }).expect("Failed to map update buffer");
            self.dirty_main_camera = false;
            Some(br::SubmissionBatch {
                command_buffers: std::borrow::Cow::Borrowed(&self.update_cmd[..]),
                .. Default::default()
            })
        };

        (cp, br::SubmissionBatch {
            command_buffers: std::borrow::Cow::Borrowed(
                &self.render_cmd[on_backbuffer_of as usize ..= on_backbuffer_of as usize]
            ),
            .. Default::default()
        })
    }

    fn discard_backbuffer_resources(&mut self) {
        self.render_cmd.reset().expect("Failed to reset render commands");
        self.rr.discard_lw();
    }
    fn on_resize(&mut self, e: &peridot::Engine<NL>, new_size: peridot::math::Vector2<usize>) {
        self.rr.set_lw(RenderingResources::new(e, &self.srr, &self.layouts));
        Self::populate_all_render_commands(&self.render_cmd, &self.srr, &self.rr.get(), &self.res, &self.desc);
        self.aspect_wh = new_size.0 as f32 / new_size.1 as f32;
        self.dirty_main_camera = true;
    }
}
impl<NL> Game<NL> {
    fn populate_all_render_commands(
        target: &peridot::CommandBundle,
        srr: &StaticRenderResources,
        rr: &RenderingResources,
        res: &Resources,
        desc: &Descriptors
    ) {
        for (fb, cmd) in rr.fbs.iter().zip(target.iter()) {
            let render_area = br::vk::VkRect2D {
                offset: br::vk::VkOffset2D::default(),
                extent: br::Extent2D::clone(fb.size().as_ref()).into()
            };
            let cv = br::ClearValue::Color([0.0; 4]);
            let cvd = br::ClearValue::DepthStencil(1.0, 0);
            let (uvrect_vb, uvrect_vb_offs, uvrect_vb_count) = res.uvrect_vb_view();
            let (grid_vb, grid_vb_offs, grid_vb_count) = res.grid_vb_view();
            cmd.begin().expect("Failed to begin Recording Commands")
                .begin_render_pass(&srr.rp_main, &fb, render_area, &[cv, cvd], true)
                .bind_graphics_pipeline_pair(rr.lines_gp.pipeline(), rr.lines_gp.layout())
                .bind_graphics_descriptor_sets(0, &[desc.camera()], &[])
                .bind_vertex_buffers(0, &[(grid_vb, grid_vb_offs)])
                .draw(grid_vb_count as _, 1, 0, 0)
                .bind_graphics_pipeline_pair(rr.gp.pipeline(), rr.gp.layout())
                .bind_graphics_descriptor_sets(1, &[desc.particle_instances()], &[])
                .bind_vertex_buffers(0, &[(uvrect_vb, uvrect_vb_offs)])
                .draw(uvrect_vb_count as _, CPUParticleDriver::MAX_RENDERED_INSTANCE_COUNT as _, 0, 0)
                .end_render_pass();
        }
    }
}

pub struct StaticRenderResources {
    pub rp_main: br::RenderPass
}
impl StaticRenderResources {
    pub fn new<NL>(e: &peridot::Engine<NL>) -> Self {
        StaticRenderResources {
            rp_main: peridot::RenderPassTemplates::single_render_with_depth_noread(
                e.backbuffer_format(), RenderingResources::DEPTH_BUFFER_FORMAT
            )
            .create(e.graphics())
            .expect("Failed to create rp_main")
        }
    }
}

pub struct Layouts {
    pub dsl_ub1v: br::DescriptorSetLayout,
    pub dsl_sb1v: br::DescriptorSetLayout
}
impl Layouts {
    pub fn new<NL>(e: &peridot::Engine<NL>) -> Self {
        Layouts {
            dsl_ub1v: br::DescriptorSetLayout::new(e.graphics(), &[
                br::DescriptorSetLayoutBinding::UniformBuffer(1, br::ShaderStage::VERTEX)
            ]).expect("Failed to create ub1v layout"),
            dsl_sb1v: br::DescriptorSetLayout::new(e.graphics(), &[
                br::DescriptorSetLayoutBinding::StorageBuffer(1, br::ShaderStage::VERTEX)
            ]).expect("Failed to create sb1v layout")
        }
    }
}

pub struct Descriptors {
    _pool: br::DescriptorPool,
    descriptors: Vec<br::vk::VkDescriptorSet>
}
impl Descriptors {
    pub fn new<NL>(e: &peridot::Engine<NL>, layouts: &Layouts, res: &Resources) -> Self {
        use br::VkHandle;
        fn as_usize_range(v: std::ops::Range<u64>) -> std::ops::Range<usize> {
            v.start as _ .. v.end as _
        }

        let pool = br::DescriptorPool::new(e.graphics(), 2, &[
            br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1),
            br::DescriptorPoolSize(br::DescriptorType::StorageBuffer, 1)
        ], false).expect("Failed to alloc descriptor pool");
        let descriptors = pool.alloc(&[&layouts.dsl_ub1v, &layouts.dsl_sb1v]).expect("Failed to allocate descriptors");

        e.graphics().update_descriptor_sets(&[
            br::DescriptorSetWriteInfo(
                descriptors[0], 0, 0, br::DescriptorUpdateInfo::UniformBuffer(vec![res.buffer_range_camera()])
            ),
            br::DescriptorSetWriteInfo(
                descriptors[1], 0, 0, br::DescriptorUpdateInfo::StorageBuffer(vec![
                    (res.static_buffer.res.native_ptr(), as_usize_range(res.pe.managed_instance_buffer_range()))
                ])
            )
        ], &[]);
        Descriptors {
            _pool: pool,
            descriptors
        }
    }

    pub fn camera(&self) -> br::vk::VkDescriptorSet { self.descriptors[0] }
    pub fn particle_instances(&self) -> br::vk::VkDescriptorSet { self.descriptors[1] }
}

pub struct SuballocatedBuffer<O> {
    pub res: peridot::Buffer,
    pub offsets: O
}

pub struct StaticOffsets {
    grid_vb: u64,
    vb: u64,
    camera_matrix: u64,
    grid_vertices_count: usize
}
impl StaticOffsets {
    pub fn camera_range(&self) -> std::ops::Range<u64> {
        self.camera_matrix .. self.camera_matrix + std::mem::size_of::<peridot::math::Matrix4F32>() as u64
    }
}
pub struct UpdateOffsets {
    camera_matrix: u64
}
impl UpdateOffsets {
    pub fn camera_range(&self) -> std::ops::Range<u64> {
        self.camera_matrix .. self.camera_matrix + std::mem::size_of::<peridot::math::Matrix4F32>() as u64
    }
}
pub struct Resources {
    pub static_buffer: SuballocatedBuffer<StaticOffsets>,
    pub update_buffer: SuballocatedBuffer<UpdateOffsets>,
    pub pe: ParticleEngine
}
impl Resources {
    pub fn new<NL>(e: &peridot::Engine<NL>, tfb: &mut peridot::TransferBatch) -> Self {
        let grid_vertices: Vec<_> = peridot::Primitive::limited_xz_grid(10).vertices
            .into_iter()
            .map(|v| peridot::ColoredVertex { pos: v, color: peridot::math::Vector4(0.6, 0.6, 0.6, 1.0) })
            .chain(peridot::Primitive::limited_coordinate_axis(100).vertices.into_iter())
            .collect();
        
        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let offs = StaticOffsets {
            grid_vb: bp.add(peridot::BufferContent::vertices::<peridot::ColoredVertex>(grid_vertices.len())),
            vb: bp.add(peridot::BufferContent::vertex::<[peridot::VertexUV3D; 4]>()),
            camera_matrix: bp.add(peridot::BufferContent::uniform::<peridot::math::Matrix4F32>()),
            grid_vertices_count: grid_vertices.len()
        };
        let pe = ParticleEngine::new(e.graphics(), &mut bp, tfb);
        let buf = bp.build_transferred().expect("Failed to build static buffer");
        let mut mem = peridot::MemoryBadget::new(e.graphics());
        mem.add(buf);
        let buf = mem.alloc().expect("Failed to allocate static memory").pop()
            .expect("less object").unwrap_buffer();

        let mut update_bp = peridot::BufferPrealloc::new(e.graphics());
        let update_offs = UpdateOffsets {
            camera_matrix: update_bp.add(peridot::BufferContent::raw::<peridot::math::Matrix4F32>())
        };
        let update_buf = update_bp.build_upload().expect("Failed to build update buffer");
        let mut mem = peridot::MemoryBadget::new(e.graphics());
        mem.add(update_buf);
        let update_buf = mem.alloc_upload().expect("Failed to allocate update memory").pop()
            .expect("less object").unwrap_buffer();

        let mut stg_bp = peridot::BufferPrealloc::new(e.graphics());
        let stg_offs_grid_vb = stg_bp.add(
            peridot::BufferContent::raw_multiple::<peridot::ColoredVertex>(grid_vertices.len())
        );
        let stg_offs_vb = stg_bp.add(peridot::BufferContent::raw::<[peridot::VertexUV3D; 4]>());
        let stg_buf = stg_bp.build_upload().expect("Failed to build staging buffer");
        let mut mem = peridot::MemoryBadget::new(e.graphics());
        mem.add(stg_buf);
        let stg_buf = mem.alloc_upload().expect("Failed to allocate staging memory").pop()
            .expect("less object").unwrap_buffer();

        stg_buf.guard_map(0 .. stg_bp.total_size(), |p| unsafe {
            p.slice_mut(stg_offs_grid_vb as _, grid_vertices.len()).clone_from_slice(&grid_vertices);
            p.slice_mut::<peridot::VertexUV3D>(stg_offs_vb as _, 4).clone_from_slice(&[
                peridot::VertexUV3D {
                    pos: peridot::math::Vector4(-0.5, -0.5, 0.0, 1.0), uv: peridot::math::Vector2(0.0, 0.0)
                },
                peridot::VertexUV3D {
                    pos: peridot::math::Vector4( 0.5, -0.5, 0.0, 1.0), uv: peridot::math::Vector2(1.0, 0.0)
                },
                peridot::VertexUV3D {
                    pos: peridot::math::Vector4(-0.5,  0.5, 0.0, 1.0), uv: peridot::math::Vector2(0.0, 1.0)
                },
                peridot::VertexUV3D {
                    pos: peridot::math::Vector4( 0.5,  0.5, 0.0, 1.0), uv: peridot::math::Vector2(1.0, 1.0)
                }
            ]);
        }).expect("Failed to map staging");

        tfb.add_copying_buffer(
            stg_buf.with_dev_offset(stg_offs_grid_vb), buf.with_dev_offset(offs.grid_vb),
            (std::mem::size_of::<peridot::ColoredVertex>() * grid_vertices.len()) as _
        );
        tfb.add_copying_buffer(
            stg_buf.with_dev_offset(stg_offs_vb), buf.with_dev_offset(offs.vb),
            std::mem::size_of::<[peridot::VertexUV3D; 4]>() as _
        );
        pe.post_transfer(tfb, &buf);

        Resources {
            static_buffer: SuballocatedBuffer { res: buf, offsets: offs },
            update_buffer: SuballocatedBuffer { res: update_buf, offsets: update_offs },
            pe
        }
    }

    pub fn buffer_range_camera(&self) -> (br::vk::VkBuffer, std::ops::Range<usize>) {
        use br::VkHandle;

        (
            self.static_buffer.res.native_ptr(),
            self.static_buffer.offsets.camera_matrix as usize ..
                self.static_buffer.offsets.camera_matrix as usize + std::mem::size_of::<peridot::math::Matrix4F32>()
        )
    }
    pub fn uvrect_vb_view(&self) -> (&br::Buffer, usize, usize) {
        (
            &self.static_buffer.res,
            self.static_buffer.offsets.vb as _,
            4
        )
    }
    pub fn grid_vb_view(&self) -> (&br::Buffer, usize, usize) {
        (
            &self.static_buffer.res,
            self.static_buffer.offsets.grid_vb as usize,
            self.static_buffer.offsets.grid_vertices_count
        )
    }
}
