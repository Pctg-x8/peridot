use bedrock::{
    self as br, DescriptorPool, Device, GraphicsPipelineBuilder, ImageChild, ImageSubresourceSlice,
    PipelineShaderStageProvider, VkObject,
};

use super::{MiniEngine, StdVkDevice};
use crate::{
    miniengine::{PrimaryDirectionalLightUniformData, SamplerDesc},
    ArrayBuilderOp,
};

pub struct SkyboxPrecomputedTextures {
    pub transmittance: br::ImageViewObject<peridot_memory_manager::Image>,
    pub scatter: br::ImageViewObject<peridot_memory_manager::Image>,
}
impl SkyboxPrecomputedTextures {
    const TRANSMITTANCE_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(128, 32);
    const SCATTER_SIZE: peridot::math::Vector3<u32> = peridot::math::Vector3(32, 64 * 2, 32);
    const GATHERED_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(32, 32);

    pub fn new(engine: &mut MiniEngine) -> br::Result<Self> {
        ArrayBuilderOp! {
            [try] engine.alloc_device_local_image_array, {
                transmittance <- br::ImageDesc::new(Self::TRANSMITTANCE_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
                scatter <- br::ImageDesc::new(Self::SCATTER_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
                gathered <- br::ImageDesc::new(Self::GATHERED_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
                k_scatter <- br::ImageDesc::new(Self::SCATTER_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
                k_gathered <- br::ImageDesc::new(Self::GATHERED_SIZE, br::vk::VK_FORMAT_R16G16B16A16_SFLOAT)
                    .sampled()
                    .use_as_storage(),
            }
        }
        let transmittance = transmittance
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;
        let scatter = scatter
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;
        let gathered = gathered
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;
        let k_scatter = k_scatter
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;
        let k_gathered = k_gathered
            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
            .view_builder()
            .create()?;

        transmittance
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:Transmittance"))?;
        scatter
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:Scatter"))?;
        gathered
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:Gathered"))?;
        k_scatter
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:K-Scatter"))?;
        k_gathered
            .image()
            .set_name(Some(c"PeridotSkyBox:Precompute:K-Gathered"))?;

        let sampler = engine.sampler(SamplerDesc {
            address_mode: (
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
            ),
            min_filter: br::FilterMode::Linear,
            mag_filter: br::FilterMode::Linear,
            mip_filter: br::MipmapFilterMode::Linear,
            ..Default::default()
        })?;
        let dsl_compute_si1 =
            br::DescriptorSetLayoutBuilder::with_bindings(vec![br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute()])
            .create(engine.device().clone())?;
        let dsl_compute_si1_si1 = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute(),
            br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute(),
        ])
        .create(engine.device().clone())?;
        let dsl_compute_cis1_si1 = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_compute()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&sampler)]),
            br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute(),
        ])
        .create(engine.device().clone())?;
        let dsl_compute_cis1_cis1_si1 = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_compute()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&sampler)]),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_compute()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&sampler)]),
            br::DescriptorType::StorageImage
                .make_binding(1)
                .only_for_compute(),
        ])
        .create(engine.device().clone())?;

        let input_only_layout = br::PipelineLayoutBuilder::new(vec![&dsl_compute_si1], vec![])
            .create(engine.device().clone())?;
        let tex_io_layout = br::PipelineLayoutBuilder::new(vec![&dsl_compute_cis1_si1], vec![])
            .create(engine.device().clone())?;
        let tex_i2o_layout =
            br::PipelineLayoutBuilder::new(vec![&dsl_compute_cis1_cis1_si1], vec![])
                .create(engine.device().clone())?;
        let tex_io_pure_layout = br::PipelineLayoutBuilder::new(vec![&dsl_compute_si1_si1], vec![])
            .create(engine.device().clone())?;
        let transmittance_compute =
            engine.shader("shaders/skybox/transmittance_precompute.cspv")?;
        let single_scatter_compute =
            engine.shader("shaders/skybox/single_scatter_precompute.cspv")?;
        let gather_compute = engine.shader("shaders/skybox/gather_precompute.cspv")?;
        let multiple_scatter_compute =
            engine.shader("shaders/skybox/multiple_scatter_precompute.cspv")?;
        let accum2_compute = engine.shader("shaders/skybox/accum2.cspv")?;
        let accum3_compute = engine.shader("shaders/skybox/accum3.cspv")?;
        ArrayBuilderOp! {
            [ref, try] engine.create_compute_pipeline_array, {
                transmittance_compute_pipeline <- br::ComputePipelineBuilder::new(
                    &input_only_layout,
                    br::PipelineShader2::new(&transmittance_compute, c"main".to_owned()),
                ),
                single_scatter_compute_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_io_layout,
                    br::PipelineShader2::new(&single_scatter_compute, c"main".to_owned()),
                ),
                gather_compute_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_io_layout,
                    br::PipelineShader2::new(&gather_compute, c"main".to_owned()),
                ),
                multiple_scatter_compute_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_i2o_layout,
                    br::PipelineShader2::new(&multiple_scatter_compute, c"main".to_owned()),
                ),
                accum2_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_io_pure_layout,
                    br::PipelineShader2::new(&accum2_compute, c"main".to_owned()),
                ),
                accum3_pipeline <- br::ComputePipelineBuilder::new(
                    &tex_io_pure_layout,
                    br::PipelineShader2::new(&accum3_compute, c"main".to_owned()),
                ),
            }
        }

        let mut descriptor_pool = br::DescriptorPoolBuilder::new(8)
            .reserve_all([
                br::DescriptorType::StorageImage.with_count(10),
                br::DescriptorType::CombinedImageSampler.with_count(7),
            ])
            .create(engine.device().clone())?;
        ArrayBuilderOp! {
            [ref, try] descriptor_pool.alloc_array, {
                transmittance_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_si1),
                transmittance_to_scatter_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_si1),
                scatter_to_gathered_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_si1),
                transmittance_gathered_to_k_scatter_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_cis1_si1),
                k_scatter_to_k_gathered_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_si1),
                k_scatter_to_scatter_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_si1_si1),
                k_gathered_to_k_gathered_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_si1_si1),
                transmittance_k_gathered_to_k_scatter_set <- br::DescriptorSetLayoutObjectRef::new(&dsl_compute_cis1_cis1_si1),
            }
        };
        engine.device().update_descriptor_sets(
            &[
                transmittance_set
                    .binding_at(0)
                    .write(br::DescriptorContents::storage_image(
                        &transmittance,
                        br::ImageLayout::General,
                    )),
                transmittance_to_scatter_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        &transmittance,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                transmittance_to_scatter_set.binding_at(1).write(
                    br::DescriptorContents::storage_image(&scatter, br::ImageLayout::General),
                ),
                scatter_to_gathered_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        &scatter,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                scatter_to_gathered_set
                    .binding_at(1)
                    .write(br::DescriptorContents::storage_image(
                        &gathered,
                        br::ImageLayout::General,
                    )),
                transmittance_gathered_to_k_scatter_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        &transmittance,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                transmittance_gathered_to_k_scatter_set.binding_at(1).write(
                    br::DescriptorContents::combined_image_sampler(
                        &gathered,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                transmittance_gathered_to_k_scatter_set.binding_at(2).write(
                    br::DescriptorContents::storage_image(&k_scatter, br::ImageLayout::General),
                ),
                k_scatter_to_k_gathered_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        &k_scatter,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                ),
                k_scatter_to_k_gathered_set.binding_at(1).write(
                    br::DescriptorContents::storage_image(&k_gathered, br::ImageLayout::General),
                ),
                k_scatter_to_scatter_set.binding_at(0).write(
                    br::DescriptorContents::storage_image(&k_scatter, br::ImageLayout::General),
                ),
                k_scatter_to_scatter_set.binding_at(1).write(
                    br::DescriptorContents::storage_image(&scatter, br::ImageLayout::General),
                ),
                k_gathered_to_k_gathered_set.binding_at(0).write(
                    br::DescriptorContents::storage_image(&k_gathered, br::ImageLayout::General),
                ),
                k_gathered_to_k_gathered_set.binding_at(1).write(
                    br::DescriptorContents::storage_image(&k_gathered, br::ImageLayout::General),
                ),
                transmittance_k_gathered_to_k_scatter_set
                    .binding_at(0)
                    .write(br::DescriptorContents::combined_image_sampler(
                        &transmittance,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )),
                transmittance_k_gathered_to_k_scatter_set
                    .binding_at(1)
                    .write(br::DescriptorContents::combined_image_sampler(
                        &k_gathered,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )),
                transmittance_k_gathered_to_k_scatter_set
                    .binding_at(2)
                    .write(br::DescriptorContents::storage_image(
                        &k_scatter,
                        br::ImageLayout::General,
                    )),
            ],
            &[],
        );

        engine.submit_transient_commands_and_wait(|rec| {
            let mut rec = rec
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[transmittance
                        .image()
                        .by_ref()
                        .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                        .memory_barrier2()
                        .transit_to(br::ImageLayout::General.from_undefined())],
                ))
                .bind_compute_pipeline_pair(&transmittance_compute_pipeline, &input_only_layout)
                .bind_compute_descriptor_sets(0, &[transmittance_set.into()], &[])
                .dispatch(
                    Self::TRANSMITTANCE_SIZE.0 / 32,
                    Self::TRANSMITTANCE_SIZE.1 / 32,
                    1,
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        transmittance
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                        scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_to(br::ImageLayout::General.from_undefined()),
                    ],
                ))
                .bind_compute_pipeline_pair(&single_scatter_compute_pipeline, &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[transmittance_to_scatter_set.into()], &[])
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                        gathered
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_to(br::ImageLayout::General.from_undefined()),
                    ],
                ))
                .bind_compute_pipeline_pair(&gather_compute_pipeline, &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[scatter_to_gathered_set.into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        gathered
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                        k_scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_to(br::ImageLayout::General.from_undefined()),
                    ],
                ))
                .bind_compute_pipeline_pair(&multiple_scatter_compute_pipeline, &tex_i2o_layout)
                .bind_compute_descriptor_sets(
                    0,
                    &[transmittance_gathered_to_k_scatter_set.into()],
                    &[],
                )
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                )
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        k_scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                        k_gathered
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_to(br::ImageLayout::General.from_undefined()),
                    ],
                ))
                .bind_compute_pipeline_pair(&gather_compute_pipeline, &tex_io_layout)
                .bind_compute_descriptor_sets(0, &[k_scatter_to_k_gathered_set.into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .from(
                            br::PipelineStageFlags2::COMPUTE_SHADER,
                            br::AccessFlags2::SHADER.write,
                        )
                        .to(
                            br::PipelineStageFlags2::COMPUTE_SHADER,
                            br::AccessFlags2::SHADER.read,
                        )],
                    &[],
                    &[],
                ))
                .bind_compute_pipeline_pair(&accum2_pipeline, &tex_io_pure_layout)
                .bind_compute_descriptor_sets(0, &[k_gathered_to_k_gathered_set.into()], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                .pipeline_barrier_2(&br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::ShaderReadOnlyOpt.to(br::ImageLayout::General),
                            ),
                        k_scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::ShaderReadOnlyOpt.to(br::ImageLayout::General),
                            ),
                    ],
                ))
                .bind_compute_pipeline_pair(&accum3_pipeline, &tex_io_pure_layout)
                .bind_compute_descriptor_sets(0, &[k_scatter_to_scatter_set.into()], &[])
                .dispatch(
                    Self::SCATTER_SIZE.0 / 8,
                    Self::SCATTER_SIZE.1 / 8,
                    Self::SCATTER_SIZE.2 / 8,
                );

            // multiple scatters after 2nd
            for _ in 0..2 {
                rec = rec
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[],
                        &[],
                        &[k_gathered
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            )],
                    ))
                    .bind_compute_pipeline_pair(&multiple_scatter_compute_pipeline, &tex_i2o_layout)
                    .bind_compute_descriptor_sets(
                        0,
                        &[transmittance_k_gathered_to_k_scatter_set.into()],
                        &[],
                    )
                    .dispatch(
                        Self::SCATTER_SIZE.0 / 8,
                        Self::SCATTER_SIZE.1 / 8,
                        Self::SCATTER_SIZE.2 / 8,
                    )
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[],
                        &[],
                        &[
                            k_scatter
                                .image()
                                .by_ref()
                                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                                .memory_barrier2()
                                .transit_from(
                                    br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt),
                                )
                                .from(
                                    br::PipelineStageFlags2::COMPUTE_SHADER,
                                    br::AccessFlags2::SHADER.write,
                                )
                                .to(
                                    br::PipelineStageFlags2::COMPUTE_SHADER,
                                    br::AccessFlags2::SHADER.read,
                                ),
                            k_gathered
                                .image()
                                .by_ref()
                                .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                                .memory_barrier2()
                                .transit_from(
                                    br::ImageLayout::ShaderReadOnlyOpt.to(br::ImageLayout::General),
                                ),
                        ],
                    ))
                    .bind_compute_pipeline_pair(&gather_compute_pipeline, &tex_io_layout)
                    .bind_compute_descriptor_sets(0, &[k_scatter_to_k_gathered_set.into()], &[])
                    .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[br::MemoryBarrier2::new()
                            .from(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::COMPUTE_SHADER,
                                br::AccessFlags2::SHADER.read,
                            )],
                        &[],
                        &[],
                    ))
                    .bind_compute_pipeline_pair(&accum2_pipeline, &tex_io_pure_layout)
                    .bind_compute_descriptor_sets(0, &[k_gathered_to_k_gathered_set.into()], &[])
                    .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[],
                        &[],
                        &[k_scatter
                            .image()
                            .by_ref()
                            .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                            .memory_barrier2()
                            .transit_from(
                                br::ImageLayout::ShaderReadOnlyOpt.to(br::ImageLayout::General),
                            )],
                    ))
                    .bind_compute_pipeline_pair(&accum3_pipeline, &tex_io_pure_layout)
                    .bind_compute_descriptor_sets(0, &[k_scatter_to_scatter_set.into()], &[])
                    .dispatch(
                        Self::SCATTER_SIZE.0 / 8,
                        Self::SCATTER_SIZE.1 / 8,
                        Self::SCATTER_SIZE.2 / 8,
                    );
            }

            rec.pipeline_barrier_2(&br::DependencyInfo::new(
                &[],
                &[],
                &[scatter
                    .image()
                    .by_ref()
                    .subresource_range(br::AspectMask::COLOR, 0..1, 0..1)
                    .memory_barrier2()
                    .transit_from(br::ImageLayout::General.to(br::ImageLayout::ShaderReadOnlyOpt))
                    .from(
                        br::PipelineStageFlags2::COMPUTE_SHADER,
                        br::AccessFlags2::SHADER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::SHADER.read,
                    )],
            ))
        })?;

        Ok(Self {
            transmittance,
            scatter,
        })
    }
}

pub struct SkyboxRenderer {
    pub _precomputed: SkyboxPrecomputedTextures,
    pub _descriptor_pool: br::DescriptorPoolObject<StdVkDevice>,
    pub renderer_descriptor: br::DescriptorSet,
    pub pipeline_layout: br::PipelineLayoutObject<StdVkDevice>,
    pub pipeline: br::PipelineObject<StdVkDevice>,
    pub primary_directional_light_data_buffer: peridot_memory_manager::Buffer,
}
impl SkyboxRenderer {
    pub fn new(
        engine: &mut MiniEngine,
        render_camera_descriptor_set_layout: &impl br::DescriptorSetLayout<ConcreteDevice = StdVkDevice>,
        render_subpass: br::SubpassRef<impl br::RenderPass + ?Sized>,
        precomputed: SkyboxPrecomputedTextures,
        init_light_data: PrimaryDirectionalLightUniformData,
    ) -> br::Result<Self> {
        let linear_sampler = engine.sampler(SamplerDesc {
            address_mode: (
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
            ),
            min_filter: br::FilterMode::Linear,
            mag_filter: br::FilterMode::Linear,
            mip_filter: br::MipmapFilterMode::Linear,
            ..Default::default()
        })?;
        let dsl = br::DescriptorSetLayoutBuilder::with_bindings(vec![
            br::DescriptorType::UniformBuffer
                .make_binding(1)
                .only_for_fragment(),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_fragment()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&linear_sampler)]),
            br::DescriptorType::CombinedImageSampler
                .make_binding(1)
                .only_for_fragment()
                .with_immutable_samplers(vec![br::SamplerObjectRef::new(&linear_sampler)]),
        ])
        .create(engine.device().clone())?;

        let pipeline_layout =
            br::PipelineLayoutBuilder::new(vec![render_camera_descriptor_set_layout, &dsl], vec![])
                .create(engine.device().clone())?;
        let vsh = engine.shader("shaders/skybox/vert.vspv")?;
        let fsh = engine.shader("shaders/skybox/frag.fspv")?;
        let mut pipeline = br::NonDerivedGraphicsPipelineBuilder::new(
            &pipeline_layout,
            render_subpass,
            br::VertexProcessingStages::new(
                br::VertexShaderStage::new(br::PipelineShader2::new(&vsh, c"main".to_owned()))
                    .with_fragment_shader_stage(br::PipelineShader2::new(&fsh, c"main".to_owned())),
                &[],
                &[],
                br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP,
            ),
        );
        pipeline
            .multisample_state(Some(br::MultisampleState::new()))
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .dynamic_viewport_scissors(1)
            .depth_test_settings(Some(br::CompareOp::LessOrEqual), false);
        let pipeline = pipeline.create(engine.device().clone(), Some(engine.pipeline_cache()))?;
        engine.writeback_pipeline_cache();

        struct BufferInitializationContents {
            pub primary_directional_light_data: PrimaryDirectionalLightUniformData,
        }
        let [primary_directional_light_data_buffer] =
            engine.alloc_device_local_buffer_array([br::BufferDesc::new(
                core::mem::size_of::<PrimaryDirectionalLightUniformData>(),
                br::BufferUsage::UNIFORM_BUFFER.transfer_dest(),
            )])?;
        let mut stg_buffer = engine.alloc_upload_buffer(br::BufferDesc::new(
            core::mem::size_of::<BufferInitializationContents>(),
            br::BufferUsage::TRANSFER_SRC,
        ))?;
        stg_buffer.write_content(BufferInitializationContents {
            primary_directional_light_data: init_light_data,
        })?;

        engine.submit_transient_commands_and_wait(|rec| {
            rec.copy_buffer(
                &stg_buffer,
                &primary_directional_light_data_buffer,
                &[br::BufferCopy::copy_data::<
                    PrimaryDirectionalLightUniformData,
                >(
                    core::mem::offset_of!(
                        BufferInitializationContents,
                        primary_directional_light_data
                    ) as _,
                    0,
                )],
            )
            .pipeline_barrier_2(&br::DependencyInfo::new(
                &[br::MemoryBarrier2::new()
                    .from(
                        br::PipelineStageFlags2::COPY,
                        br::AccessFlags2::TRANSFER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::SHADER.read,
                    )],
                &[],
                &[],
            ))
        })?;

        let mut dp = br::DescriptorPoolBuilder::new(1)
            .with_reservations(vec![
                br::DescriptorType::UniformBuffer.with_count(1),
                br::DescriptorType::CombinedImageSampler.with_count(2),
            ])
            .create(engine.device().clone())?;
        let [descriptor] = dp.alloc_array(&[br::DescriptorSetLayoutObjectRef::new(&dsl)])?;
        engine.device().update_descriptor_sets(
            &[
                descriptor
                    .binding_at(0)
                    .write(br::DescriptorContents::uniform_buffer(
                        &primary_directional_light_data_buffer,
                        0..core::mem::size_of::<PrimaryDirectionalLightUniformData>()
                            as br::vk::VkDeviceSize,
                    )),
                descriptor
                    .binding_at(1)
                    .write(br::DescriptorContents::combined_image_sampler(
                        &precomputed.scatter,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )),
                descriptor
                    .binding_at(2)
                    .write(br::DescriptorContents::combined_image_sampler(
                        &precomputed.transmittance,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )),
            ],
            &[],
        );

        Ok(Self {
            _precomputed: precomputed,
            _descriptor_pool: dp,
            renderer_descriptor: descriptor,
            pipeline_layout,
            pipeline,
            primary_directional_light_data_buffer,
        })
    }

    pub fn update_primary_directional_light_data(
        &self,
        e: &mut MiniEngine,
        new_data: PrimaryDirectionalLightUniformData,
    ) -> br::Result<()> {
        let mut upload_buffer = e.alloc_upload_buffer(br::BufferDesc::new(
            core::mem::size_of::<PrimaryDirectionalLightUniformData>(),
            br::BufferUsage::TRANSFER_SRC,
        ))?;
        upload_buffer.write_content(new_data)?;

        e.submit_transient_commands_and_wait(|rec| {
            rec.copy_buffer(
                &upload_buffer,
                &self.primary_directional_light_data_buffer,
                &[br::BufferCopy::mirror_data::<
                    PrimaryDirectionalLightUniformData,
                >(0)],
            )
            .pipeline_barrier_2(&br::DependencyInfo::new(
                &[br::MemoryBarrier2::new()
                    .from(
                        br::PipelineStageFlags2::COPY,
                        br::AccessFlags2::TRANSFER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::UNIFORM_READ,
                    )],
                &[],
                &[],
            ))
        })?;

        Ok(())
    }

    pub fn record_render_commands<
        'r,
        CB: br::VkHandleMut<Handle = br::vk::VkCommandBuffer> + ?Sized,
        Device: br::Device + ?Sized,
    >(
        &self,
        rec: br::CmdRecord<'r, CB, Device>,
    ) -> br::CmdRecord<'r, CB, Device> {
        rec.bind_graphics_pipeline_pair(&self.pipeline, &self.pipeline_layout)
            .bind_graphics_descriptor_sets(1, &[self.renderer_descriptor.0], &[])
            .draw(4, 1, 0, 0)
    }
}
