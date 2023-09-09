use bedrock as br;
use br::{
    CommandBuffer, DescriptorPool, Device, Image, ImageChild, ImageSubresourceSlice,
    SubmissionBatch,
};
use peridot::{
    mthelper::{DynamicMutabilityProvider, SharedRef},
    Engine, EngineEvents, FeatureRequests, NativeLinker,
};
use peridot_command_object::{
    BeginRenderPass, BufferUsage, CopyBuffer, DescriptorSets, EndRenderPass, GraphicsCommand,
    GraphicsCommandCombiner, PipelineBarrier, RangedBuffer, StandardMesh,
};
use peridot_vertex_processing_pack::PvpShaderModules;
use std::{
    collections::{BTreeMap, HashMap},
    rc::Rc,
};

#[repr(C, align(4))]
pub struct RGBA32(u8, u8, u8, u8);

#[repr(C)]
#[derive(Clone)]
pub struct SunLightData {
    pub incident_light_dir: peridot::math::Vector4F32,
}

pub struct ImmutableDescriptorSets<Device: br::Device> {
    _pool: br::DescriptorPoolObject<Device>,
    layouts_for_set: Vec<SharedRef<br::DescriptorSetLayoutObject<Device>>>,
    sets: Vec<br::DescriptorSet>,
}
impl<Device: br::Device + Clone> ImmutableDescriptorSets<Device> {
    pub fn new<'r>(
        dev: Device,
        set_bindings: Vec<Vec<(br::ShaderStage, br::DescriptorContents<'r>)>>,
        dub: &mut peridot::DescriptorSetUpdateBatch<'r>,
    ) -> br::Result<Self> {
        let mut layouts = HashMap::new();
        let mut layouts_for_set = Vec::new();
        let mut pool_sizes = BTreeMap::new();
        for b in &set_bindings {
            let layout_bindings = b
                .iter()
                .map(|&(ss, ref e)| {
                    match e {
                        br::DescriptorContents::Sampler(ref xs) => {
                            *pool_sizes.entry(br::DescriptorType::Sampler).or_insert(0) += 1;

                            br::DescriptorType::Sampler.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::CombinedImageSampler(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::CombinedImageSampler)
                                .or_insert(0) += 1;

                            br::DescriptorType::CombinedImageSampler.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::SampledImage(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::SampledImage)
                                .or_insert(0) += 1;

                            br::DescriptorType::SampledImage.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::StorageImage(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::StorageImage)
                                .or_insert(0) += 1;

                            br::DescriptorType::StorageImage.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::InputAttachment(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::InputAttachment)
                                .or_insert(0) += 1;

                            br::DescriptorType::InputAttachment.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::UniformBuffer(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::UniformBuffer)
                                .or_insert(0) += 1;

                            br::DescriptorType::UniformBuffer.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::StorageBuffer(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::StorageBuffer)
                                .or_insert(0) += 1;

                            br::DescriptorType::StorageBuffer.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::UniformBufferDynamic(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::UniformBufferDynamic)
                                .or_insert(0) += 1;

                            br::DescriptorType::UniformBufferDynamic.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::StorageBufferDynamic(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::StorageBufferDynamic)
                                .or_insert(0) += 1;

                            br::DescriptorType::StorageBufferDynamic.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::UniformTexelBuffer(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::UniformTexelBuffer)
                                .or_insert(0) += 1;

                            br::DescriptorType::UniformTexelBuffer.make_binding(xs.len() as _)
                        }
                        br::DescriptorContents::StorageTexelBuffer(ref xs) => {
                            *pool_sizes
                                .entry(br::DescriptorType::StorageTexelBuffer)
                                .or_insert(0) += 1;

                            br::DescriptorType::StorageTexelBuffer.make_binding(xs.len() as _)
                        }
                    }
                    .for_shader_stage(ss)
                })
                .collect::<Vec<_>>();

            let layout = match layouts.entry(layout_bindings) {
                std::collections::hash_map::Entry::Vacant(v) => {
                    let r = br::DescriptorSetLayoutBuilder::with_bindings(v.key().clone())
                        .create(dev.clone())?;
                    v.insert(SharedRef::new(r)).clone()
                }
                std::collections::hash_map::Entry::Occupied(o) => o.get().clone(),
            };
            layouts_for_set.push(layout);
        }

        let mut pool = br::DescriptorPoolBuilder::new(layouts_for_set.len() as _)
            .reserve_all(pool_sizes.into_iter().map(|(ty, a)| ty.with_count(a)))
            .create(dev)?;
        let sets = pool.alloc(&layouts_for_set.iter().map(|l| &**l).collect::<Vec<_>>())?;

        for (b, s) in set_bindings.into_iter().zip(sets.iter()) {
            for (n, (_, b1)) in b.into_iter().enumerate() {
                dub.write(*s, n as _, b1);
            }
        }

        Ok(Self {
            _pool: pool,
            layouts_for_set,
            sets,
        })
    }
}
impl<Device: br::Device> std::ops::Deref for ImmutableDescriptorSets<Device> {
    type Target = [br::DescriptorSet];

    fn deref(&self) -> &Self::Target {
        &self.sets
    }
}

pub struct SkyboxPrecomputedTextures {
    transmittance: peridot::DeviceWorkingTexture2DRef,
    scatter: peridot::DeviceWorkingTexture3DRef,
    gathered: peridot::DeviceWorkingTexture2DRef,
    k_scatter: peridot::DeviceWorkingTexture3DRef,
    k_gathered: peridot::DeviceWorkingTexture2DRef,
}
impl SkyboxPrecomputedTextures {
    const TRANSMITTANCE_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(128, 32);
    const SCATTER_SIZE: peridot::math::Vector3<u32> = peridot::math::Vector3(32, 64 * 2, 32);
    const GATHERED_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(32, 32);

    pub fn prealloc(dwt_alloc: &mut peridot::DeviceWorkingTextureAllocator) -> Self {
        SkyboxPrecomputedTextures {
            transmittance: dwt_alloc.new2d(
                Self::TRANSMITTANCE_SIZE,
                peridot::PixelFormat::RGBA64F,
                br::ImageUsage::STORAGE.sampled(),
            ),
            scatter: dwt_alloc.new3d(
                Self::SCATTER_SIZE,
                peridot::PixelFormat::RGBA64F,
                br::ImageUsage::STORAGE.sampled(),
            ),
            gathered: dwt_alloc.new2d(
                Self::GATHERED_SIZE,
                peridot::PixelFormat::RGBA64F,
                br::ImageUsage::STORAGE.sampled(),
            ),
            k_scatter: dwt_alloc.new3d(
                Self::SCATTER_SIZE,
                peridot::PixelFormat::RGBA64F,
                br::ImageUsage::STORAGE.sampled(),
            ),
            k_gathered: dwt_alloc.new2d(
                Self::GATHERED_SIZE,
                peridot::PixelFormat::RGBA64F,
                br::ImageUsage::STORAGE.sampled(),
            ),
        }
    }
    pub fn init<NL: NativeLinker>(
        &self,
        e: &mut Engine<NL>,
        dwt: &peridot::DeviceWorkingTextureStore<
            peridot::Image<
                br::ImageObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
    ) {
        let linear_sampler = br::SamplerBuilder::new()
            .addressing(
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
            )
            .create(e.graphics().device().clone())
            .expect("Failed to create linear_sampler");
        let mut dub = peridot::DescriptorSetUpdateBatch::new();
        let pre_compute_sets = ImmutableDescriptorSets::new(
            e.graphics().device().clone(),
            vec![
                vec![(
                    br::ShaderStage::COMPUTE,
                    br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                        dwt.get(self.transmittance).underlying_view(),
                        br::ImageLayout::General,
                    )]),
                )],
                vec![
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(self.transmittance).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.scatter).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                ],
                vec![
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(self.scatter).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.gathered).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                ],
                vec![
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(self.transmittance).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(self.gathered).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.k_scatter).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                ],
                vec![
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(self.k_scatter).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.k_gathered).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                ],
                vec![
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.k_scatter).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.scatter).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                ],
                vec![
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.k_gathered).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.k_gathered).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                ],
                vec![
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(self.transmittance).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(self.k_gathered).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                    (
                        br::ShaderStage::COMPUTE,
                        br::DescriptorContents::StorageImage(vec![br::DescriptorImageRef::new(
                            dwt.get(self.k_scatter).underlying_view(),
                            br::ImageLayout::General,
                        )]),
                    ),
                ],
            ],
            &mut dub,
        )
        .expect("Failed to allocate descriptor sets for Pre-computation");
        dub.submit(e.graphics().device());

        let input_only_layout: Rc<_> =
            br::PipelineLayoutBuilder::new(vec![&pre_compute_sets.layouts_for_set[0]], vec![])
                .create(e.graphics().device().clone())
                .expect("input only_layout creating failed")
                .into();
        let tex_io_layout: Rc<_> =
            br::PipelineLayoutBuilder::new(vec![&pre_compute_sets.layouts_for_set[1]], vec![])
                .create(e.graphics().device().clone())
                .expect("tex-io_layout creating failed")
                .into();
        let tex_i2o_layout: Rc<_> =
            br::PipelineLayoutBuilder::new(vec![&pre_compute_sets.layouts_for_set[3]], vec![])
                .create(e.graphics().device().clone())
                .expect("tex-i2o_layout creating failed")
                .into();
        let tex_io_pure_layout: Rc<_> =
            br::PipelineLayoutBuilder::new(vec![&pre_compute_sets.layouts_for_set[5]], vec![])
                .create(e.graphics().device().clone())
                .expect("tex-io_pure_layout creating failed")
                .into();
        let transmittance_compute = e
            .load::<peridot::SpirvShaderBlob>("shaders.precompute.transmittance")
            .expect("Failed to load precompute shader for transmittance")
            .instantiate(e.graphics().device().clone())
            .expect("Compute Shader Instantiation failed");
        let single_scatter_compute = e
            .load::<peridot::SpirvShaderBlob>("shaders.precompute.single_scatter")
            .expect("Failed to load precompute shader for single scatter")
            .instantiate(e.graphics().device().clone())
            .expect("Compute shader Instantiation failed");
        let gather_compute = e
            .load::<peridot::SpirvShaderBlob>("shaders.precompute.gather")
            .expect("Failed to load precompute shader for gathering")
            .instantiate(e.graphics().device().clone())
            .expect("Compute shader Instantiation failed");
        let multiple_scatter_compute = e
            .load::<peridot::SpirvShaderBlob>("shaders.precompute.multiple_scatter")
            .expect("Failed to load precompute shader for multiple scatter")
            .instantiate(e.graphics().device().clone())
            .expect("Compute shader Instantiation failed");
        let accum2_compute = e
            .load::<peridot::SpirvShaderBlob>("shaders.precompute.accum2")
            .expect("Failed to load precompute shader for accumulation-2d")
            .instantiate(e.graphics().device().clone())
            .expect("Compute shader Instantiation failed");
        let accum3_compute = e
            .load::<peridot::SpirvShaderBlob>("shaders.precompute.accum3")
            .expect("Failed to load precompute shader for accumulation-3d")
            .instantiate(e.graphics().device().clone())
            .expect("Compute shader Instantiation failed");
        let transmittance_compute_pipeline = br::ComputePipelineBuilder::new(
            &input_only_layout,
            br::PipelineShader {
                module: &transmittance_compute,
                entry_name: std::ffi::CString::new("main").expect("cstring failed"),
                specinfo: None,
            },
        );
        let single_scatter_compute_pipeline = br::ComputePipelineBuilder::new(
            &tex_io_layout,
            br::PipelineShader {
                module: &single_scatter_compute,
                entry_name: std::ffi::CString::new("main").expect("cstring failed"),
                specinfo: None,
            },
        );
        let gather_compute_pipeline = br::ComputePipelineBuilder::new(
            &tex_io_layout,
            br::PipelineShader {
                module: &gather_compute,
                entry_name: std::ffi::CString::new("main").expect("cstring failed"),
                specinfo: None,
            },
        );
        let multiple_scatter_compute_pipeline = br::ComputePipelineBuilder::new(
            &tex_i2o_layout,
            br::PipelineShader {
                module: &multiple_scatter_compute,
                entry_name: std::ffi::CString::new("main").expect("cstring failed"),
                specinfo: None,
            },
        );
        let accum2_pipeline = br::ComputePipelineBuilder::new(
            &tex_io_pure_layout,
            br::PipelineShader {
                module: &accum2_compute,
                entry_name: std::ffi::CString::new("main").expect("cstring failed"),
                specinfo: None,
            },
        );
        let accum3_pipeline = br::ComputePipelineBuilder::new(
            &tex_io_pure_layout,
            br::PipelineShader {
                module: &accum3_compute,
                entry_name: std::ffi::CString::new("main").expect("cstring failed"),
                specinfo: None,
            },
        );
        let compute_pipelines = e
            .graphics()
            .device()
            .clone()
            .new_compute_pipelines(
                &[
                    transmittance_compute_pipeline,
                    single_scatter_compute_pipeline,
                    gather_compute_pipeline,
                    multiple_scatter_compute_pipeline,
                    accum2_pipeline,
                    accum3_pipeline,
                ],
                None::<&'_ br::PipelineCacheObject<peridot::DeviceObject>>,
            )
            .expect("Failed to create precomputation pipelines");

        e.submit_commands(|mut rec| {
            let transmittance_tex_area = dwt
                .get(self.transmittance)
                .underlying()
                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1);
            let scatter_tex_area = dwt.get(self.scatter).underlying().subresource_range(
                br::AspectMask::COLOR,
                0..1,
                0..1,
            );
            let gather_tex_area = dwt.get(self.gathered).underlying().subresource_range(
                br::AspectMask::COLOR,
                0..1,
                0..1,
            );
            let k_scatter_tex_area = dwt.get(self.k_scatter).underlying().subresource_range(
                br::AspectMask::COLOR,
                0..1,
                0..1,
            );
            let k_gather_tex_area = dwt.get(self.k_gathered).underlying().subresource_range(
                br::AspectMask::COLOR,
                0..1,
                0..1,
            );

            let ib_init = [
                transmittance_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::Preinitialized, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
                scatter_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::Preinitialized, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
                gather_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::Preinitialized, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
                k_scatter_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::Preinitialized, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
                k_gather_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::Preinitialized, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
            ];
            let transmittance_fin = [transmittance_tex_area
                .memory_barrier(br::ImageLayout::General, br::ImageLayout::ShaderReadOnlyOpt)
                .src_access_mask(br::AccessFlags::SHADER.write)];
            let scatter_to_readable = [scatter_tex_area
                .clone()
                .memory_barrier(br::ImageLayout::General, br::ImageLayout::ShaderReadOnlyOpt)
                .src_access_mask(br::AccessFlags::SHADER.write)];
            let gather_to_readable = [gather_tex_area
                .clone()
                .memory_barrier(br::ImageLayout::General, br::ImageLayout::ShaderReadOnlyOpt)
                .src_access_mask(br::AccessFlags::SHADER.write)];
            let gather_cont = [
                k_gather_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::General, br::ImageLayout::ShaderReadOnlyOpt)
                    .src_access_mask(br::AccessFlags::SHADER.read),
                k_scatter_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::General, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
            ];
            let k_scatter_to_readable = [k_scatter_tex_area
                .clone()
                .memory_barrier(br::ImageLayout::General, br::ImageLayout::ShaderReadOnlyOpt)
                .src_access_mask(br::AccessFlags::SHADER.write)];
            let ready_k_gather = [
                k_scatter_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::General, br::ImageLayout::ShaderReadOnlyOpt)
                    .src_access_mask(br::AccessFlags::SHADER.write),
                k_gather_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::ShaderReadOnlyOpt, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
            ];
            let accum_gather = [
                k_gather_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::General, br::ImageLayout::General)
                    .src_access_mask(br::AccessFlags::SHADER.write)
                    .dest_access_mask(br::AccessFlags::SHADER.read),
                k_scatter_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::ShaderReadOnlyOpt, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.read),
                scatter_tex_area
                    .clone()
                    .memory_barrier(br::ImageLayout::ShaderReadOnlyOpt, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
                gather_tex_area
                    .memory_barrier(br::ImageLayout::ShaderReadOnlyOpt, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.write),
            ];
            let accum_gather2 = [
                k_gather_tex_area
                    .memory_barrier(br::ImageLayout::General, br::ImageLayout::General)
                    .src_access_mask(br::AccessFlags::SHADER.write)
                    .dest_access_mask(br::AccessFlags::SHADER.read),
                k_scatter_tex_area
                    .memory_barrier(br::ImageLayout::ShaderReadOnlyOpt, br::ImageLayout::General)
                    .dest_access_mask(br::AccessFlags::SHADER.read),
            ];
            let render_ready = [scatter_tex_area
                .memory_barrier(br::ImageLayout::General, br::ImageLayout::ShaderReadOnlyOpt)
                .src_access_mask(br::AccessFlags::SHADER.write)];

            let _ = rec
                .pipeline_barrier(
                    br::PipelineStageFlags::TOP_OF_PIPE,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[],
                    &[],
                    &ib_init,
                )
                .bind_compute_pipeline_pair(&compute_pipelines[0], &input_only_layout)
                .bind_compute_descriptor_sets(0, &[pre_compute_sets[0].into()], &[])
                .dispatch(
                    Self::TRANSMITTANCE_SIZE.0 / 32,
                    Self::TRANSMITTANCE_SIZE.1 / 32,
                    1,
                )
                .pipeline_barrier(
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[],
                    &[],
                    &transmittance_fin,
                )
                .bind_compute_pipeline_pair(&compute_pipelines[1], &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[pre_compute_sets[1].into()], &[])
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                )
                // gather1
                .pipeline_barrier(
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[],
                    &[],
                    &scatter_to_readable,
                )
                .bind_compute_pipeline_pair(&compute_pipelines[2], &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[pre_compute_sets[2].into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                // multiple scatter
                .pipeline_barrier(
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[],
                    &[],
                    &gather_to_readable,
                )
                .bind_compute_pipeline_pair(&compute_pipelines[3], &tex_i2o_layout)
                .bind_compute_descriptor_sets(0, &[pre_compute_sets[3].into()], &[])
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                )
                // gather-k
                .pipeline_barrier(
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[],
                    &[],
                    &k_scatter_to_readable,
                )
                .bind_compute_pipeline_pair(&compute_pipelines[2], &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[pre_compute_sets[4].into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                // accum
                .pipeline_barrier(
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[],
                    &[],
                    &accum_gather,
                )
                .bind_compute_pipeline_pair(&compute_pipelines[4], &tex_io_pure_layout)
                .bind_compute_descriptor_sets(0, &[pre_compute_sets[6].into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                .bind_compute_pipeline_pair(&compute_pipelines[5], &tex_io_pure_layout)
                .bind_compute_descriptor_sets(0, &[pre_compute_sets[5].into()], &[])
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                );

            // multiple scatters after 2nd
            for _ in 0..2 {
                // multiple scatter 2
                let _ = rec
                    .pipeline_barrier(
                        br::PipelineStageFlags::COMPUTE_SHADER,
                        br::PipelineStageFlags::COMPUTE_SHADER,
                        false,
                        &[],
                        &[],
                        &gather_cont,
                    )
                    .bind_compute_pipeline_pair(&compute_pipelines[3], &tex_i2o_layout)
                    .bind_compute_descriptor_sets(0, &[pre_compute_sets[7].into()], &[])
                    .dispatch(
                        Self::SCATTER_SIZE.0 / 8,
                        Self::SCATTER_SIZE.1 / 8,
                        Self::SCATTER_SIZE.2 / 8,
                    )
                    // gather-k
                    .pipeline_barrier(
                        br::PipelineStageFlags::COMPUTE_SHADER,
                        br::PipelineStageFlags::COMPUTE_SHADER,
                        false,
                        &[],
                        &[],
                        &ready_k_gather,
                    )
                    .bind_compute_pipeline_pair(&compute_pipelines[2], &tex_io_layout)
                    .bind_compute_descriptor_sets(0, &[pre_compute_sets[4].into()], &[])
                    .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                    // accum
                    .pipeline_barrier(
                        br::PipelineStageFlags::COMPUTE_SHADER,
                        br::PipelineStageFlags::COMPUTE_SHADER,
                        false,
                        &[],
                        &[],
                        &accum_gather2,
                    )
                    .bind_compute_pipeline_pair(&compute_pipelines[4], &tex_io_pure_layout)
                    .bind_compute_descriptor_sets(0, &[pre_compute_sets[6].into()], &[])
                    .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                    .bind_compute_pipeline_pair(&compute_pipelines[5], &tex_io_pure_layout)
                    .bind_compute_descriptor_sets(0, &[pre_compute_sets[5].into()], &[])
                    .dispatch(
                        Self::SCATTER_SIZE.0 / 8,
                        Self::SCATTER_SIZE.1 / 8,
                        Self::SCATTER_SIZE.2 / 8,
                    );
            }
            let _ = rec.pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::FRAGMENT_SHADER,
                false,
                &[],
                &[],
                &render_ready,
            );

            rec
        })
        .expect("Dispatch Precomputation failed");
    }
}

pub struct Descriptors {
    _static_sampler: br::SamplerObject<peridot::DeviceObject>,
    descriptors: ImmutableDescriptorSets<peridot::DeviceObject>,
}
impl Descriptors {
    pub fn new(
        g: &peridot::Graphics,
        dwt: &peridot::DeviceWorkingTextureStore<
            peridot::Image<
                br::ImageObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
        precomputes: &SkyboxPrecomputedTextures,
        buf: &peridot::FixedMemory<
            peridot::DeviceObject,
            peridot::Buffer<
                br::BufferObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
        buf_offsets: &FixedBufferOffsets,
    ) -> Self {
        let linear_sampler = br::SamplerBuilder::new()
            .addressing(
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
            )
            .create(g.device().clone())
            .expect("Failed to create linear sampler");
        let mut dub = peridot::DescriptorSetUpdateBatch::new();
        let descriptors = ImmutableDescriptorSets::new(
            g.device().clone(),
            vec![
                vec![
                    (
                        br::ShaderStage::FRAGMENT.vertex(),
                        br::DescriptorContents::UniformBuffer(vec![br::DescriptorBufferRef::new(
                            &buf.buffer.object,
                            buf_offsets.uniform_range.start as _
                                ..buf_offsets.uniform_range.end as _,
                        )]),
                    ),
                    (
                        br::ShaderStage::FRAGMENT,
                        br::DescriptorContents::UniformBuffer(vec![br::DescriptorBufferRef::new(
                            &buf.buffer.object,
                            buf_offsets.sun_light_uniform_range.start as _
                                ..buf_offsets.sun_light_uniform_range.end as _,
                        )]),
                    ),
                ],
                vec![
                    (
                        br::ShaderStage::FRAGMENT,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(precomputes.scatter).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                    (
                        br::ShaderStage::FRAGMENT,
                        br::DescriptorContents::CombinedImageSampler(vec![
                            br::DescriptorImageRef::new(
                                dwt.get(precomputes.transmittance).underlying_view(),
                                br::ImageLayout::ShaderReadOnlyOpt,
                            )
                            .with_sampler(&linear_sampler),
                        ]),
                    ),
                ],
            ],
            &mut dub,
        )
        .expect("Failed to alloc descriptor sets");
        dub.submit(g.device());

        Descriptors {
            _static_sampler: linear_sampler,
            descriptors,
        }
    }

    pub fn camera_input_layout(
        &self,
    ) -> &impl br::DescriptorSetLayout<ConcreteDevice = peridot::DeviceObject> {
        &self.descriptors.layouts_for_set[0]
    }

    pub fn skybox_renderer_input_layout(
        &self,
    ) -> &impl br::DescriptorSetLayout<ConcreteDevice = peridot::DeviceObject> {
        &self.descriptors.layouts_for_set[1]
    }

    pub fn camera_uniform(&self) -> br::DescriptorSet {
        self.descriptors[0]
    }

    pub fn skybox_precomputed_textures(&self) -> br::DescriptorSet {
        self.descriptors[1]
    }
}

pub struct SkyboxRenderer {
    main_render_shader: PvpShaderModules<'static, peridot::DeviceObject>,
    pipeline: peridot::LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        br::PipelineLayoutObject<peridot::DeviceObject>,
    >,
}
impl SkyboxRenderer {
    fn new<NL: NativeLinker>(
        e: &Engine<NL>,
        drt: &DetailedRenderTargets,
        descriptors: &Descriptors,
    ) -> Self {
        let layout = br::PipelineLayoutBuilder::new(
            vec![
                descriptors.camera_input_layout(),
                descriptors.skybox_renderer_input_layout(),
            ],
            vec![],
        )
        .create(e.graphics().device().clone())
        .expect("Failed to create SkyboxRender pipeline");
        let main_render_shader = PvpShaderModules::new(
            e.graphics().device(),
            e.load("shaders.skybox")
                .expect("Failed to load main shader"),
        )
        .expect("Instantiating shader failed");

        let bb0 = e.back_buffer(0).expect("no backbuffers?");
        let full_scissors = [bb0.image().size().wh().into_rect(br::vk::VkOffset2D::ZERO)];
        let full_viewports = [full_scissors[0].make_viewport(0.0..1.0)];

        let main_vps =
            main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(&layout, (&drt.rp, 0), main_vps)
        .viewport_scissors(
            br::DynamicArrayState::Static(&full_viewports),
            br::DynamicArrayState::Static(&full_scissors),
        )
        .multisample_state(Some(mp))
        .depth_test_settings(Some(br::CompareOp::LessOrEqual), false)
        .add_attachment_blend(br::AttachmentColorBlendState::noblend())
        .create(
            e.graphics().device().clone(),
            None::<&'_ br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Creating GraphicsPipeline failed");

        Self {
            main_render_shader,
            pipeline: peridot::LayoutedPipeline::combine(pipeline, layout),
        }
    }

    fn recreate_pipeline<NL: NativeLinker>(&mut self, e: &Engine<NL>, drt: &DetailedRenderTargets) {
        let bb0 = e.back_buffer(0).expect("no backbuffers?");
        let backbuffer_size = bb0.image().size();
        let full_scissors = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D { x: 0, y: 0 },
            extent: br::vk::VkExtent2D {
                width: backbuffer_size.width,
                height: backbuffer_size.height,
            },
        }];
        let full_viewports = [br::vk::VkViewport::from_rect_with_depth_range(
            &full_scissors[0],
            0.0..1.0,
        )];

        let main_vps = self
            .main_render_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(self.pipeline.layout(), (&drt.rp, 0), main_vps)
        .viewport_scissors(
            br::DynamicArrayState::Static(&full_viewports),
            br::DynamicArrayState::Static(&full_scissors),
        )
        .multisample_state(Some(mp))
        .depth_test_settings(Some(br::CompareOp::LessOrEqual), false)
        .add_attachment_blend(br::AttachmentColorBlendState::noblend())
        .create(
            e.graphics().device().clone(),
            None::<&'_ br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Creating GraphicsPipeline failed");

        unsafe {
            self.pipeline.replace_pipeline(pipeline);
        }
    }
}

pub struct GridRenderer {
    main_render_shader: PvpShaderModules<'static, peridot::DeviceObject>,
    pipeline: peridot::LayoutedPipeline<
        br::PipelineObject<peridot::DeviceObject>,
        br::PipelineLayoutObject<peridot::DeviceObject>,
    >,
}
impl GridRenderer {
    pub fn new<NL: NativeLinker>(
        e: &peridot::Engine<NL>,
        drt: &DetailedRenderTargets,
        descriptors: &Descriptors,
    ) -> Self {
        let pl = br::PipelineLayoutBuilder::new(vec![descriptors.camera_input_layout()], vec![])
            .create(e.graphics().device().clone())
            .expect("Failed to create GridRender pipeline layout");

        let main_render_shader = PvpShaderModules::new(
            e.graphics().device(),
            e.load("shaders.colored")
                .expect("Failed to load colored shader"),
        )
        .expect("Instantiating grid render shader failed");

        let bb0 = e.back_buffer(0).expect("no backbuffers?");
        let full_scissors = [bb0.image().size().wh().into_rect(br::vk::VkOffset2D::ZERO)];
        let full_viewports = [full_scissors[0].make_viewport(0.0..1.0)];

        let main_vps = main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(&pl, (&drt.rp, 0), main_vps)
        .viewport_scissors(
            br::DynamicArrayState::Static(&full_viewports),
            br::DynamicArrayState::Static(&full_scissors),
        )
        .multisample_state(Some(mp))
        .depth_test_settings(Some(br::CompareOp::Less), true)
        .add_attachment_blend(br::AttachmentColorBlendState::noblend())
        .create(
            e.graphics().device().clone(),
            None::<&'_ br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Creating GraphicsPipeline failed");

        Self {
            main_render_shader,
            pipeline: peridot::LayoutedPipeline::combine(pipeline, pl),
        }
    }

    fn recreate_pipeline<NL: NativeLinker>(&mut self, e: &Engine<NL>, drt: &DetailedRenderTargets) {
        let bb0 = e.back_buffer(0).expect("no backbuffers?");
        let backbuffer_size = bb0.image().size();
        let full_scissors = [br::vk::VkRect2D {
            offset: br::vk::VkOffset2D { x: 0, y: 0 },
            extent: br::vk::VkExtent2D {
                width: backbuffer_size.width,
                height: backbuffer_size.height,
            },
        }];
        let full_viewports = [br::vk::VkViewport::from_rect_with_depth_range(
            &full_scissors[0],
            0.0..1.0,
        )];

        let main_vps = self
            .main_render_shader
            .generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::<
            _,
            br::PipelineObject<peridot::DeviceObject>,
            _,
            _,
            _,
            _,
            _,
            _,
        >::new(self.pipeline.layout(), (&drt.rp, 0), main_vps)
        .viewport_scissors(
            br::DynamicArrayState::Static(&full_viewports),
            br::DynamicArrayState::Static(&full_scissors),
        )
        .multisample_state(Some(mp))
        .depth_test_settings(Some(br::CompareOp::Less), true)
        .add_attachment_blend(br::AttachmentColorBlendState::noblend())
        .create(
            e.graphics().device().clone(),
            None::<&'_ br::PipelineCacheObject<peridot::DeviceObject>>,
        )
        .expect("Creating GraphicsPipeline failed");

        unsafe {
            self.pipeline.replace_pipeline(pipeline);
        }
    }
}

pub struct DetailedRenderTargets {
    depth_buffer: SharedRef<
        br::ImageViewObject<
            peridot::Image<
                br::ImageObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
    >,
    rp: br::RenderPassObject<peridot::DeviceObject>,
    fb: Vec<br::FramebufferObject<peridot::DeviceObject>>,
}
impl DetailedRenderTargets {
    pub fn new<NL: NativeLinker>(e: &Engine<NL>) -> Self {
        let depth_buffer = br::ImageDesc::new(
            e.back_buffer(0)
                .expect("no backbuffers?")
                .image()
                .size()
                .wh(),
            br::vk::VK_FORMAT_D32_SFLOAT_S8_UINT,
            br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
            br::ImageLayout::Undefined,
        )
        .create(e.graphics().device().clone())
        .expect("Failed to create depth buffer");
        let mut mb =
            peridot::MemoryBadget::<br::BufferObject<peridot::DeviceObject>, _>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Image(depth_buffer));
        let depth_buffer = mb
            .alloc()
            .expect("Failed to alloc depth buffer memory")
            .pop()
            .expect("no resources")
            .unwrap_image();
        let depth_buffer = SharedRef::new(
            depth_buffer
                .subresource_range(br::AspectMask::DEPTH.stencil(), 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create depth buffer view"),
        );

        let rp = peridot::RenderPassTemplates::single_render_with_depth(
            e.back_buffer_format(),
            br::vk::VK_FORMAT_D32_SFLOAT_S8_UINT,
            e.requesting_back_buffer_layout().0,
            false,
        )
        .create(e.graphics().device().clone())
        .expect("Creating RenderPass failed");
        let fb = e
            .iter_back_buffers()
            .map(|bb| {
                br::FramebufferBuilder::new_with_attachment(&rp, bb.clone())
                    .with_attachment(depth_buffer.clone())
                    .create()
                    .expect("Creating Framebuffer failed")
            })
            .collect();

        Self {
            rp,
            fb,
            depth_buffer,
        }
    }

    pub fn discard_framebuffers(&mut self) {
        self.fb.clear();
    }
    pub fn recreate_framebuffers<NL: NativeLinker>(
        &mut self,
        e: &Engine<NL>,
        size: &peridot::math::Vector2<usize>,
    ) {
        let su32: br::vk::VkExtent2D =
            peridot::math::Vector2::<u32>(size.0 as _, size.1 as _).into();

        let depth_buffer = br::ImageDesc::new(
            su32,
            br::vk::VK_FORMAT_D32_SFLOAT_S8_UINT,
            br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
            br::ImageLayout::Undefined,
        )
        .create(e.graphics().device().clone())
        .expect("Failed to create depth buffer");
        let mut mb =
            peridot::MemoryBadget::<br::BufferObject<peridot::DeviceObject>, _>::new(e.graphics());
        mb.add(peridot::MemoryBadgetEntry::Image(depth_buffer));
        let depth_buffer = mb
            .alloc()
            .expect("Failed to alloc depth buffer memory")
            .pop()
            .expect("no resources")
            .unwrap_image();
        self.depth_buffer = SharedRef::new(
            depth_buffer
                .subresource_range(br::AspectMask::DEPTH.stencil(), 0..1, 0..1)
                .view_builder()
                .create()
                .expect("Failed to create depth buffer view"),
        );

        self.fb = e
            .iter_back_buffers()
            .map(|bb| {
                br::FramebufferBuilder::new_with_attachment(&self.rp, bb.clone())
                    .with_attachment(self.depth_buffer.clone())
                    .create()
                    .expect("Recreating Framebuffer failed")
            })
            .collect();
    }

    pub fn postinit_commands(
        &self,
        r: &mut br::CmdRecord<impl br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized>,
    ) {
        let _ = r.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
            true,
            &[],
            &[],
            &[self
                .depth_buffer
                .image()
                .subresource_range(br::AspectMask::DEPTH.stencil(), 0..1, 0..1)
                .memory_barrier(
                    br::ImageLayout::Undefined,
                    br::ImageLayout::DepthStencilAttachmentOpt,
                )],
        );
    }
}

pub struct FixedBufferOffsets {
    fill_plane: u64,
    grid_vertices: u64,
    grid_vertex_count: u32,
    mut_uniform_offset: u64,
    mut_sun_light_uniform_offset: u64,
    mut_buffer_size: u64,
    uniform_range: std::ops::Range<usize>,
    sun_light_uniform_range: std::ops::Range<usize>,
}
pub struct FixedBufferInitializer {
    fill_plane: peridot::Primitive<peridot::VertexUV2D>,
    fill_plane_offset: u64,
    grid_vertices: peridot::Primitive<peridot::ColoredVertex>,
    grid_vertices_offset: u64,
}
impl peridot::FixedBufferInitializer for FixedBufferInitializer {
    fn stage_data(
        &mut self,
        m: &br::MappedMemoryRange<impl br::DeviceMemory + br::VkHandleMut + ?Sized>,
    ) {
        unsafe {
            m.slice_mut(self.fill_plane_offset as _, self.fill_plane.vertices.len())
                .clone_from_slice(&self.fill_plane.vertices);
            m.slice_mut(
                self.grid_vertices_offset as _,
                self.grid_vertices.vertices.len(),
            )
            .clone_from_slice(&self.grid_vertices.vertices);
        }
    }

    fn buffer_graphics_ready<Device: br::Device + 'static>(
        &self,
        tfb: &mut peridot::TransferBatch,
        buf: &SharedRef<
            peridot::Buffer<
                impl br::Buffer<ConcreteDevice = Device> + 'static,
                impl br::DeviceMemory<ConcreteDevice = Device> + 'static,
            >,
        >,
        range: std::ops::Range<u64>,
    ) {
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT.fragment_shader(),
            buf.clone(),
            range.clone(),
            br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::UNIFORM_READ,
        );
    }
}

pub struct RenderCommands {
    main_commands: peridot::CommandBundle<peridot::DeviceObject>,
}
impl RenderCommands {
    fn new<NL: NativeLinker>(e: &Engine<NL>) -> Self {
        Self {
            main_commands: peridot::CommandBundle::new(
                e.graphics(),
                peridot::CBSubmissionType::Graphics,
                e.back_buffer_count(),
            )
            .expect("Failed to create Main CommandBundle"),
        }
    }

    fn generate_commands<NL: NativeLinker>(
        &mut self,
        e: &Engine<NL>,
        skybox_renderer: &SkyboxRenderer,
        grid_renderer: &GridRenderer,
        descriptors: &Descriptors,
        drt: &DetailedRenderTargets,
        buf: &peridot::FixedMemory<impl br::Device, impl br::Buffer>,
        buf_offsets: &FixedBufferOffsets,
    ) {
        let clear_values = [
            br::ClearValue::color([0.0; 4]),
            br::ClearValue::depth_stencil(1.0, 0),
        ];

        for (b, fb) in self.main_commands.iter_mut().zip(&drt.fb) {
            let render_pass = BeginRenderPass::for_entire_framebuffer(&drt.rp, fb)
                .with_clear_values(clear_values.into());

            let grid = StandardMesh {
                vertex_buffers: vec![RangedBuffer::from_offset_length(
                    &buf.buffer.object,
                    buf_offsets.grid_vertices as _,
                    1,
                )],
                vertex_count: buf_offsets.grid_vertex_count,
            };
            let fill = StandardMesh {
                vertex_buffers: vec![RangedBuffer::from_offset_length(
                    &buf.buffer.object,
                    buf_offsets.fill_plane as _,
                    1,
                )],
                vertex_count: 4,
            };

            let draw_grid = grid.draw(1).after_of((
                &grid_renderer.pipeline,
                DescriptorSets(vec![descriptors.camera_uniform().into()]).into_bind_graphics(),
            ));
            let draw_skybox = fill.draw(1).after_of((
                &skybox_renderer.pipeline,
                DescriptorSets(vec![
                    descriptors.camera_uniform().into(),
                    descriptors.skybox_precomputed_textures().into(),
                ])
                .into_bind_graphics(),
            ));

            (draw_grid, draw_skybox)
                .between(render_pass, EndRenderPass)
                .execute_and_finish(unsafe {
                    b.begin()
                        .expect("Failed to begin record main command")
                        .as_dyn_ref()
                })
                .expect("Failed to finish main commands");
        }
    }
    fn clear_commands(&mut self) {
        self.main_commands.reset().expect("Resetting failed");
    }
}

#[repr(C)]
pub struct Uniform {
    pub main_view_projection: peridot::math::Matrix4F32,
    pub main_view: peridot::math::Matrix4F32,
    pub persp_fov_rad: f32,
    pub aspect_wh: f32,
}

struct Differential<T> {
    old_value: T,
}
impl<T> Differential<T>
where
    T: std::ops::Sub<T, Output = T> + Copy,
{
    pub fn new(init: T) -> Self {
        Differential { old_value: init }
    }
    pub fn update(&mut self, new_value: T) -> T {
        let diff = new_value - self.old_value;
        self.old_value = new_value;
        diff
    }
}

pub enum MouseButtonControlState {
    None,
    Camera,
    SunDirection,
}
pub struct MouseControl {
    state: MouseButtonControlState,
    x_diff: Differential<f32>,
    y_diff: Differential<f32>,
    view_zenith_angle: f32,
    azimuth_angle: f32,
    sunlight_zenith_angle: f32,
    sunlight_azimuth_angle: f32,
}
impl MouseControl {
    pub fn new() -> Self {
        Self {
            state: MouseButtonControlState::None,
            x_diff: Differential::new(0.0),
            y_diff: Differential::new(0.0),
            view_zenith_angle: 90.0,
            azimuth_angle: 0.0,
            sunlight_zenith_angle: 85.0,
            sunlight_azimuth_angle: 180.0,
        }
    }

    pub fn update(
        &mut self,
        e: &peridot::Engine<impl peridot::NativeLinker>,
        main_camera: &mut peridot::math::Camera,
        sun_light_data: &mut SunLightData,
    ) {
        let xd = self.x_diff.update(e.input().analog_value_abs(1));
        let yd = self.y_diff.update(e.input().analog_value_abs(0));

        self.state = if !e.input().button_pressing_time(0).is_zero() {
            MouseButtonControlState::Camera
        } else if !e.input().button_pressing_time(1).is_zero() {
            MouseButtonControlState::SunDirection
        } else {
            MouseButtonControlState::None
        };

        match self.state {
            MouseButtonControlState::None => { /* nop */ }
            MouseButtonControlState::Camera => {
                self.view_zenith_angle += yd * 0.1;
                self.azimuth_angle += xd * 0.1;

                main_camera.rotation = peridot::math::Quaternion::new(
                    (self.view_zenith_angle - 90.0).to_radians(),
                    peridot::math::Vector3::right(),
                ) * peridot::math::Quaternion::new(
                    self.azimuth_angle.to_radians(),
                    peridot::math::Vector3::up(),
                );
            }
            MouseButtonControlState::SunDirection => {
                self.sunlight_zenith_angle += yd * 0.1;
                self.sunlight_azimuth_angle -= xd * 0.1;

                let d1 = peridot::math::Matrix4::from(
                    peridot::math::Quaternion::new(
                        (self.sunlight_zenith_angle - 90.0).to_radians(),
                        peridot::math::Vector3::right(),
                    ) * peridot::math::Quaternion::new(
                        self.sunlight_azimuth_angle.to_radians(),
                        peridot::math::Vector3::up(),
                    ),
                ) * peridot::math::Vector3::forward();
                sun_light_data.incident_light_dir = peridot::math::Vector4(d1.0, d1.1, d1.2, 0.0);
            }
        }
    }
}

pub struct Game<NL> {
    rt: DetailedRenderTargets,
    descriptors: Descriptors,
    _skybox_precomputed: SkyboxPrecomputedTextures,
    skybox: SkyboxRenderer,
    grid: GridRenderer,
    buf: peridot::FixedMemory<
        peridot::DeviceObject,
        peridot::Buffer<
            br::BufferObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    buf_offsets: FixedBufferOffsets,
    _precompute_textures: peridot::DeviceWorkingTextureStore<
        peridot::Image<
            br::ImageObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >,
    cmds: RenderCommands,
    update_cmds: peridot::CommandBundle<peridot::DeviceObject>,
    total_time: f32,
    main_camera: peridot::math::Camera,
    aspect_wh: f32,
    sun_light_data: SunLightData,
    mouse_control: MouseControl,
    ph: std::marker::PhantomData<*const NL>,
}
impl<NL> FeatureRequests for Game<NL> {}
impl<NL: NativeLinker> EngineEvents<NL> for Game<NL> {
    fn init(e: &mut Engine<NL>) -> Self {
        e.input_mut().map(peridot::NativeButtonInput::Mouse(0), 0);
        e.input_mut().map(peridot::NativeButtonInput::Mouse(1), 1);
        e.input_mut().map(peridot::NativeAnalogInput::MouseY, 0);
        e.input_mut().map(peridot::NativeAnalogInput::MouseX, 1);

        let aspect_wh = e
            .back_buffer(0)
            .map(|b| {
                let s = b.image().size();
                s.width as f32 / s.height as f32
            })
            .expect("no backbuffers?");
        let main_camera = peridot::math::Camera {
            projection: Some(peridot::math::ProjectionMethod::Perspective {
                fov: 70.0f32.to_radians(),
            }),
            position: peridot::math::Vector3(0.0, 3.0, -5.0),
            rotation: peridot::math::Quaternion::new(0.0, peridot::math::Vector3(0.0, 1.0, 0.0)),
            depth_range: 0.1..100.0,
        };

        let mut precompute_textures = peridot::DeviceWorkingTextureAllocator::new();
        let skybox_precomputed = SkyboxPrecomputedTextures::prealloc(&mut precompute_textures);
        let precompute_textures = precompute_textures
            .alloc(e.graphics())
            .expect("Allocating Precomputed Textures failed");

        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let fill_plane = peridot::Primitive::uv_plane_centric(1.0);
        let fill_plane_offset = bp.add(peridot::BufferContent::vertices::<peridot::VertexUV2D>(
            fill_plane.vertices.len(),
        ));
        let grid_vertices = peridot::Primitive {
            vertices: peridot::Primitive::limited_xz_grid(10)
                .vertices
                .into_iter()
                .map(|v| peridot::ColoredVertex {
                    pos: v,
                    color: peridot::math::Vector4(0.6, 0.6, 0.6, 1.0),
                })
                .chain(peridot::Primitive::limited_coordinate_axis(100).vertices)
                .collect(),
        };
        let grid_vertices_offset =
            bp.add(peridot::BufferContent::vertices::<peridot::ColoredVertex>(
                grid_vertices.vertices.len(),
            ));
        let mut mbp = peridot::BufferPrealloc::new(e.graphics());
        let uniform_offset = mbp.add(peridot::BufferContent::uniform::<Uniform>());
        let sun_light_data_offset = mbp.add(peridot::BufferContent::uniform::<SunLightData>());
        let mut_buffer_size = mbp.total_size();
        let mut fb_data = FixedBufferInitializer {
            fill_plane,
            fill_plane_offset,
            grid_vertices,
            grid_vertices_offset,
        };
        let mut tfb = peridot::TransferBatch::new();
        let buf = peridot::FixedMemory::new(
            e.graphics(),
            bp,
            mbp,
            peridot::TextureInitializationGroup::new(e.graphics().device().clone()),
            &mut fb_data,
            &mut tfb,
        )
        .expect("Failed to initialize fixed buffers");
        let buf_offsets = FixedBufferOffsets {
            fill_plane: fb_data.fill_plane_offset,
            grid_vertices: fb_data.grid_vertices_offset,
            grid_vertex_count: fb_data.grid_vertices.vertices.len() as _,
            mut_uniform_offset: uniform_offset,
            mut_sun_light_uniform_offset: sun_light_data_offset,
            mut_buffer_size,
            uniform_range: (buf.mut_buffer_placement + uniform_offset) as usize
                ..(buf.mut_buffer_placement + uniform_offset) as usize
                    + std::mem::size_of::<Uniform>(),
            sun_light_uniform_range: (buf.mut_buffer_placement + sun_light_data_offset) as usize
                ..(buf.mut_buffer_placement + sun_light_data_offset) as usize
                    + std::mem::size_of::<SunLightData>(),
        };
        let sun_light_data = SunLightData {
            incident_light_dir: peridot::math::Vector4(0.0f32, -0.6, 0.8, 0.0).normalize(),
        };
        buf.mut_buffer
            .object
            .borrow_mut()
            .guard_map(0..mut_buffer_size as u64, |m| unsafe {
                *m.get_mut(uniform_offset as _) = Uniform {
                    main_view_projection: main_camera.view_projection_matrix(aspect_wh),
                    main_view: main_camera.view_matrix(),
                    persp_fov_rad: 70.0f32.to_radians(),
                    aspect_wh,
                };
                *m.get_mut(sun_light_data_offset as _) = sun_light_data.clone();
            })
            .expect("Staging MutBuffer failed");
        let mut update_cb =
            peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Graphics, 1)
                .expect("Alloc CommandBundle failed(copy)");
        {
            let mut r = unsafe { update_cb[0].begin().expect("Begin UpdateCmdRec") };

            let uniform_buffer = RangedBuffer::for_type::<Uniform>(
                &buf.buffer.object,
                buf.mut_buffer_placement + uniform_offset,
            );
            let uniform_buffer_stg =
                RangedBuffer::for_type::<Uniform>(buf.mut_buffer.object.borrow(), uniform_offset);
            let sun_light_uniform_buffer = RangedBuffer::for_type::<SunLightData>(
                &buf.buffer.object,
                buf.mut_buffer_placement + sun_light_data_offset,
            );
            let sun_light_uniform_buffer_stg = RangedBuffer::for_type::<SunLightData>(
                buf.mut_buffer.object.borrow(),
                sun_light_data_offset,
            );

            let [uniform_in_barrier, uniform_out_barrier] = uniform_buffer
                .usage_barrier3_switching(BufferUsage::FRAGMENT_UNIFORM, BufferUsage::TRANSFER_DST);
            let [sun_light_in_barrier, sun_light_out_barrier] = sun_light_uniform_buffer
                .usage_barrier3_switching(BufferUsage::FRAGMENT_UNIFORM, BufferUsage::TRANSFER_DST);
            let [uniform_stg_in_barrier, uniform_stg_out_barrier] = uniform_buffer_stg
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);
            let [sun_light_stg_in_barrier, sun_light_stg_out_barrier] = sun_light_uniform_buffer
                .usage_barrier3_switching(BufferUsage::HOST_RW, BufferUsage::TRANSFER_SRC);

            let copies = CopyBuffer::new(&uniform_buffer_stg.0, &uniform_buffer.0)
                .with_range_for_type::<Uniform>(
                    uniform_buffer_stg.offset(),
                    uniform_buffer.offset(),
                )
                .with_range_for_type::<SunLightData>(
                    sun_light_uniform_buffer_stg.offset(),
                    sun_light_uniform_buffer.offset(),
                );
            let in_barriers = PipelineBarrier::new()
                .with_barriers([uniform_in_barrier, sun_light_in_barrier])
                .with_barrier(uniform_stg_in_barrier)
                .with_barrier(sun_light_stg_in_barrier);
            let out_barriers = PipelineBarrier::new()
                .with_barriers([uniform_out_barrier, sun_light_out_barrier])
                .with_barrier(uniform_stg_out_barrier)
                .with_barrier(sun_light_stg_out_barrier);

            copies
                .between(in_barriers, out_barriers)
                .execute_and_finish(r.as_dyn_ref())
                .expect("Failed to record update commands");
        }

        let rt = DetailedRenderTargets::new(e);

        let descriptors = Descriptors::new(
            e.graphics(),
            &precompute_textures,
            &skybox_precomputed,
            &buf,
            &buf_offsets,
        );
        let skybox = SkyboxRenderer::new(e, &rt, &descriptors);
        let grid = GridRenderer::new(e, &rt, &descriptors);

        skybox_precomputed.init(e, &precompute_textures);
        let mut cmds = RenderCommands::new(e);
        cmds.generate_commands(e, &skybox, &grid, &descriptors, &rt, &buf, &buf_offsets);

        e.submit_commands(|mut r| {
            tfb.sink_transfer_commands(&mut r);
            tfb.sink_graphics_ready_commands(&mut r);
            rt.postinit_commands(&mut r);

            r
        })
        .expect("Failed to resource memory initialization");

        Game {
            rt,
            descriptors,
            _skybox_precomputed: skybox_precomputed,
            skybox,
            grid,
            buf,
            buf_offsets,
            _precompute_textures: precompute_textures,
            cmds,
            update_cmds: update_cb,
            total_time: 0.0,
            main_camera,
            aspect_wh,
            sun_light_data,
            mouse_control: MouseControl::new(),
            ph: std::marker::PhantomData,
        }
    }
    fn update(&mut self, e: &mut Engine<NL>, on_backbuffer_of: u32, dt: std::time::Duration) {
        self.mouse_control
            .update(e, &mut self.main_camera, &mut self.sun_light_data);

        self.total_time += dt.as_secs() as f32 + dt.subsec_micros() as f32 / 10_000_000.0;
        let buf_offsets = &self.buf_offsets;
        let main_camera = &self.main_camera;
        let sun_light_data = &self.sun_light_data;
        let aspect_wh = self.aspect_wh;
        self.buf
            .mut_buffer
            .object
            .borrow_mut()
            .guard_map(0..self.buf_offsets.mut_buffer_size, |m| unsafe {
                *m.get_mut(buf_offsets.mut_uniform_offset as _) = Uniform {
                    main_view_projection: main_camera.view_projection_matrix(aspect_wh),
                    main_view: main_camera.view_matrix(),
                    persp_fov_rad: 70.0f32.to_radians(),
                    aspect_wh,
                };
                *m.get_mut(buf_offsets.mut_sun_light_uniform_offset as _) = sun_light_data.clone();
            })
            .expect("Staging MutBuffer failed");

        e.do_render(
            on_backbuffer_of,
            br::EmptySubmissionBatch
                .with_command_buffers(&self.update_cmds)
                .into(),
            br::EmptySubmissionBatch.with_command_buffers(
                &self.cmds.main_commands[on_backbuffer_of as usize..=on_backbuffer_of as usize],
            ),
        )
        .expect("Failed to submit works");
    }

    fn discard_back_buffer_resources(&mut self) {
        self.cmds.clear_commands();
        self.rt.discard_framebuffers();
    }

    fn on_resize(&mut self, e: &mut Engine<NL>, new_size: peridot::math::Vector2<usize>) {
        self.aspect_wh = new_size.0 as f32 / new_size.1 as f32;

        self.rt.recreate_framebuffers(e, &new_size);
        self.skybox.recreate_pipeline(e, &self.rt);
        self.grid.recreate_pipeline(e, &self.rt);
        self.cmds.generate_commands(
            e,
            &self.skybox,
            &self.grid,
            &self.descriptors,
            &self.rt,
            &self.buf,
            &self.buf_offsets,
        );

        e.submit_commands(|mut r| {
            self.rt.postinit_commands(&mut r);

            r
        })
        .expect("Failed to execute resize commands");
    }
}
