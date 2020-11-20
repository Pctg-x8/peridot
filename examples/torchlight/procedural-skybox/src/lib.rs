//! http://publications.lib.chalmers.se/records/fulltext/203057/203057.pdf

use peridot::{NativeLinker, Engine, EngineEvents, FeatureRequests};
use peridot_vertex_processing_pack::PvpShaderModules;
use bedrock as br;
use br::VkHandle;
use std::rc::Rc;

#[repr(C, align(4))]
pub struct RGBA32(u8, u8, u8, u8);

pub struct SkyboxPrecomputedTextures {
    transmittance: peridot::DeviceWorkingTexture2DRef,
    scatter: peridot::DeviceWorkingTexture3DRef,
    gathered: peridot::DeviceWorkingTexture2DRef,
    k_scatter: peridot::DeviceWorkingTexture3DRef,
    k_gathered: peridot::DeviceWorkingTexture2DRef
}
impl SkyboxPrecomputedTextures {
    const TRANSMITTANCE_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(128, 32);
    const SCATTER_SIZE: peridot::math::Vector3<u32> = peridot::math::Vector3(32, 64 * 2, 32);
    const GATHERED_SIZE: peridot::math::Vector2<u32> = peridot::math::Vector2(32, 32);

    pub fn prealloc(dwt_alloc: &mut peridot::DeviceWorkingTextureAllocator) -> Self {
        SkyboxPrecomputedTextures {
            transmittance: dwt_alloc.new2d(
                Self::TRANSMITTANCE_SIZE, peridot::PixelFormat::RGBA64F, br::ImageUsage::STORAGE.sampled()
            ),
            scatter: dwt_alloc.new3d(
                Self::SCATTER_SIZE, peridot::PixelFormat::RGBA64F, br::ImageUsage::STORAGE.sampled()
            ),
            gathered: dwt_alloc.new2d(
                Self::GATHERED_SIZE, peridot::PixelFormat::RGBA64F, br::ImageUsage::STORAGE.sampled()
            ),
            k_scatter: dwt_alloc.new3d(
                Self::SCATTER_SIZE, peridot::PixelFormat::RGBA64F, br::ImageUsage::STORAGE.sampled()
            ),
            k_gathered: dwt_alloc.new2d(
                Self::GATHERED_SIZE, peridot::PixelFormat::RGBA64F, br::ImageUsage::STORAGE.sampled()
            )
        }
    }
    pub fn init<NL: NativeLinker>(&self, e: &Engine<NL>, dwt: &peridot::DeviceWorkingTextureStore) {
        let linear_sampler = br::SamplerBuilder::default()
            .addressing(
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge,
                br::AddressingMode::ClampToEdge
            )
            .create(e.graphics())
            .expect("Failed to create linear_sampler");
        let tex1_layout = br::DescriptorSetLayout::new(e.graphics(), &[
            br::DescriptorSetLayoutBinding::StorageImage(1, br::ShaderStage::COMPUTE)
        ]).expect("failed to create tex1_layout");
        let tex_r1w1_layout = br::DescriptorSetLayout::new(e.graphics(), &[
            br::DescriptorSetLayoutBinding::CombinedImageSampler(
                1, br::ShaderStage::COMPUTE, &[linear_sampler.native_ptr()]
            ),
            br::DescriptorSetLayoutBinding::StorageImage(1, br::ShaderStage::COMPUTE)
        ]).expect("failed to create tex_r1w1_layout");
        let tex_r2w1_layout = br::DescriptorSetLayout::new(e.graphics(), &[
            br::DescriptorSetLayoutBinding::CombinedImageSampler(
                1, br::ShaderStage::COMPUTE, &[linear_sampler.native_ptr()]
            ),
            br::DescriptorSetLayoutBinding::CombinedImageSampler(
                1, br::ShaderStage::COMPUTE, &[linear_sampler.native_ptr()]
            ),
            br::DescriptorSetLayoutBinding::StorageImage(1, br::ShaderStage::COMPUTE)
        ]).expect("failed to create tex_r1w1_layout");
        let tex_pureio_layout = br::DescriptorSetLayout::new(e.graphics(), &[
            br::DescriptorSetLayoutBinding::StorageImage(1, br::ShaderStage::COMPUTE),
            br::DescriptorSetLayoutBinding::StorageImage(1, br::ShaderStage::COMPUTE)
        ]).expect("failed to create tex1_layout");
        let dp = br::DescriptorPool::new(
            e.graphics(),
            8,
            &[
                br::DescriptorPoolSize(br::DescriptorType::StorageImage, 10),
                br::DescriptorPoolSize(br::DescriptorType::CombinedImageSampler, 7)
            ],
            false
        ).expect("failed to create DescriptorPool");
        let precompute_sets = dp.alloc(&[
            &tex1_layout, &tex_r1w1_layout, &tex_r1w1_layout, &tex_r2w1_layout, &tex_r1w1_layout,
            &tex_pureio_layout, &tex_pureio_layout, &tex_r2w1_layout
        ]).expect("failed to allocate DescriptorSets for Precomputation");
        e.graphics().update_descriptor_sets(&[
            br::DescriptorSetWriteInfo(
                precompute_sets[0], 0, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.transmittance).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[1], 0, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(self.transmittance).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[1], 1, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.scatter).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[2], 0, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(self.scatter).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[2], 1, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.gathered).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[3], 0, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(self.transmittance).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[3], 1, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(self.gathered).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[3], 2, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.k_scatter).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[4], 0, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(self.k_scatter).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[4], 1, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.k_gathered).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[5], 0, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.k_scatter).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[5], 1, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.scatter).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[6], 0, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.k_gathered).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[6], 1, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.gathered).native_ptr(), br::ImageLayout::General)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[7], 0, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(self.transmittance).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[7], 1, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(self.k_gathered).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                precompute_sets[7], 2, 0,
                br::DescriptorUpdateInfo::StorageImage(vec![
                    (None, dwt.get(self.k_scatter).native_ptr(), br::ImageLayout::General)
                ])
            )
        ], &[]);

        let inputonly_layout: Rc<_> = br::PipelineLayout::new(e.graphics(), &[&tex1_layout], &[])
            .expect("inputonly_layout creating failed").into();
        let texio_layout: Rc<_> = br::PipelineLayout::new(e.graphics(), &[&tex_r1w1_layout], &[])
            .expect("texio_layout creating failed").into();
        let texi2o_layout: Rc<_> = br::PipelineLayout::new(e.graphics(), &[&tex_r2w1_layout], &[])
            .expect("texi2o_layout creating failed").into();
        let texio_pure_layout: Rc<_> = br::PipelineLayout::new(e.graphics(), &[&tex_pureio_layout], &[])
            .expect("texio_pure_layout creating failed").into();
        let transmittance_compute = e.load::<peridot::SpirvShaderBlob>("shaders.precompute.transmittance")
            .expect("Failed to load precompute shader for transmittance")
            .instantiate(e.graphics())
            .expect("Compute Shader Instantiation failed");
        let single_scatter_compute = e.load::<peridot::SpirvShaderBlob>("shaders.precompute.single_scatter")
            .expect("Failed to load precompute shader for single scatter")
            .instantiate(e.graphics())
            .expect("Compute shader Instantiation failed");
        let gather_compute = e.load::<peridot::SpirvShaderBlob>("shaders.precompute.gather")
            .expect("Failed to load precompute shader for gathering")
            .instantiate(e.graphics())
            .expect("Compute shader Instantiation failed");
        let multiple_scatter_compute = e.load::<peridot::SpirvShaderBlob>("shaders.precompute.multiple_scatter")
            .expect("Failed to load precompute shader for multiple scatter")
            .instantiate(e.graphics())
            .expect("Compute shader Instantiation failed");
        let accum2_compute = e.load::<peridot::SpirvShaderBlob>("shaders.precompute.accum2")
            .expect("Failed to load precompute shader for accumulation-2d")
            .instantiate(e.graphics())
            .expect("Compute shader Instantiation failed");
        let accum3_compute = e.load::<peridot::SpirvShaderBlob>("shaders.precompute.accum3")
            .expect("Failed to load precompute shader for accumulation-3d")
            .instantiate(e.graphics())
            .expect("Compute shader Instantiation failed");
        let transmittance_compute_pipeline = br::ComputePipelineBuilder::new(&inputonly_layout, br::PipelineShader {
            module: &transmittance_compute,
            entry_name: std::ffi::CString::new("main").expect("cstring failed"),
            specinfo: None
        });
        let single_scatter_compute_pipeline = br::ComputePipelineBuilder::new(&texio_layout, br::PipelineShader {
            module: &single_scatter_compute,
            entry_name: std::ffi::CString::new("main").expect("cstring failed"),
            specinfo: None
        });
        let gather_compute_pipeline = br::ComputePipelineBuilder::new(&texio_layout, br::PipelineShader {
            module: &gather_compute,
            entry_name: std::ffi::CString::new("main").expect("cstring failed"),
            specinfo: None
        });
        let multiple_scatter_compute_pipeline = br::ComputePipelineBuilder::new(&texi2o_layout, br::PipelineShader {
            module: &multiple_scatter_compute,
            entry_name: std::ffi::CString::new("main").expect("cstring failed"),
            specinfo: None
        });
        let accum2_pipeline = br::ComputePipelineBuilder::new(&texio_pure_layout, br::PipelineShader {
            module: &accum2_compute,
            entry_name: std::ffi::CString::new("main").expect("cstring failed"),
            specinfo: None
        });
        let accum3_pipeline = br::ComputePipelineBuilder::new(&texio_pure_layout, br::PipelineShader {
            module: &accum3_compute,
            entry_name: std::ffi::CString::new("main").expect("cstring failed"),
            specinfo: None
        });
        let compute_pipelines = e.graphics().create_compute_pipelines(
            &[
                transmittance_compute_pipeline, single_scatter_compute_pipeline, gather_compute_pipeline,
                multiple_scatter_compute_pipeline,
                accum2_pipeline, accum3_pipeline
            ],
            None
        ).expect("Failed to create precomputation pipelines");
        
        e.submit_commands(|rec| {
            let transmittance_tex_area = br::ImageSubref::color(dwt.get(self.transmittance).underlying(), 0..1, 0..1);
            let scatter_tex_area = br::ImageSubref::color(dwt.get(self.scatter).underlying(), 0..1, 0..1);
            let gather_tex_area = br::ImageSubref::color(dwt.get(self.gathered).underlying(), 0..1, 0..1);
            let k_scatter_tex_area = br::ImageSubref::color(dwt.get(self.k_scatter).underlying(), 0..1, 0..1);
            let k_gather_tex_area = br::ImageSubref::color(dwt.get(self.k_gathered).underlying(), 0..1, 0..1);
            let ib_init = [
                br::ImageMemoryBarrier::new(
                    &transmittance_tex_area,
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write),
                br::ImageMemoryBarrier::new(
                    &scatter_tex_area,
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write),
                br::ImageMemoryBarrier::new(
                    &gather_tex_area,
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write),
                br::ImageMemoryBarrier::new(
                    &k_scatter_tex_area,
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write),
                br::ImageMemoryBarrier::new(
                    &k_gather_tex_area,
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write)
            ];
            let transmittance_fin = [
                br::ImageMemoryBarrier::new(
                    &transmittance_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.write)
            ];
            let scatter_to_readable = [
                br::ImageMemoryBarrier::new(
                    &scatter_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.write)
            ];
            let gather_to_readable = [
                br::ImageMemoryBarrier::new(
                    &gather_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.write)
            ];
            let gather_cont = [
                br::ImageMemoryBarrier::new(
                    &k_gather_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.read),
                br::ImageMemoryBarrier::new(
                    &k_scatter_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write),
            ];
            let k_scatter_to_readable = [
                br::ImageMemoryBarrier::new(
                    &k_scatter_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.write)
            ];
            let ready_k_gather = [
                br::ImageMemoryBarrier::new(
                    &k_scatter_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.write),
                br::ImageMemoryBarrier::new(
                    &k_gather_tex_area,
                    br::ImageLayout::ShaderReadOnlyOpt,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write)
            ];
            let accum_gather = [
                br::ImageMemoryBarrier::new(
                    &k_gather_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::General
                ).src_access_mask(br::AccessFlags::SHADER.write).dest_access_mask(br::AccessFlags::SHADER.read),
                br::ImageMemoryBarrier::new(
                    &k_scatter_tex_area,
                    br::ImageLayout::ShaderReadOnlyOpt,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.read),
                br::ImageMemoryBarrier::new(
                    &scatter_tex_area,
                    br::ImageLayout::ShaderReadOnlyOpt,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write),
                br::ImageMemoryBarrier::new(
                    &gather_tex_area,
                    br::ImageLayout::ShaderReadOnlyOpt,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.write)
            ];
            let accum_gather2 = [
                br::ImageMemoryBarrier::new(
                    &k_gather_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::General
                ).src_access_mask(br::AccessFlags::SHADER.write).dest_access_mask(br::AccessFlags::SHADER.read),
                br::ImageMemoryBarrier::new(
                    &k_scatter_tex_area,
                    br::ImageLayout::ShaderReadOnlyOpt,
                    br::ImageLayout::General
                ).dest_access_mask(br::AccessFlags::SHADER.read)
            ];
            let render_ready = [
                br::ImageMemoryBarrier::new(
                    &scatter_tex_area,
                    br::ImageLayout::General,
                    br::ImageLayout::ShaderReadOnlyOpt
                ).src_access_mask(br::AccessFlags::SHADER.write)
            ];

            rec.pipeline_barrier(
                br::PipelineStageFlags::TOP_OF_PIPE,
                br::PipelineStageFlags::COMPUTE_SHADER,
                false,
                &[], &[], &ib_init
            ).bind_compute_pipeline_pair(&compute_pipelines[0], &inputonly_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[0]], &[])
            .dispatch(Self::TRANSMITTANCE_SIZE.0 / 32, Self::TRANSMITTANCE_SIZE.1 / 32, 1)
            .pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::COMPUTE_SHADER,
                false,
                &[], &[], &transmittance_fin
            )
            .bind_compute_pipeline_pair(&compute_pipelines[1], &texio_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[1]], &[])
            .dispatch(Self::SCATTER_SIZE.0 / 8, Self::SCATTER_SIZE.1 / 8, Self::SCATTER_SIZE.2 / 8)
            // gather1
            .pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::COMPUTE_SHADER,
                false,
                &[], &[], &scatter_to_readable
            )
            .bind_compute_pipeline_pair(&compute_pipelines[2], &texio_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[2]], &[])
            .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
            // multiple scatter
            .pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::COMPUTE_SHADER,
                false,
                &[], &[], &gather_to_readable
            )
            .bind_compute_pipeline_pair(&compute_pipelines[3], &texi2o_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[3]], &[])
            .dispatch(Self::SCATTER_SIZE.0 / 8, Self::SCATTER_SIZE.1 / 8, Self::SCATTER_SIZE.2 / 8)
            // gather-k
            .pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::COMPUTE_SHADER,
                false,
                &[], &[], &k_scatter_to_readable
            )
            .bind_compute_pipeline_pair(&compute_pipelines[2], &texio_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[4]], &[])
            .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
            // accum
            .pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::COMPUTE_SHADER,
                false,
                &[], &[], &accum_gather
            )
            .bind_compute_pipeline_pair(&compute_pipelines[4], &texio_pure_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[6]], &[])
            .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
            .bind_compute_pipeline_pair(&compute_pipelines[5], &texio_pure_layout)
            .bind_compute_descriptor_sets(0, &[precompute_sets[5]], &[])
            .dispatch(Self::SCATTER_SIZE.0 / 8, Self::SCATTER_SIZE.1 / 8, Self::SCATTER_SIZE.2 / 8);

            // multiple scatters after 2nd
            for _ in 0..2 {
                // multiple scatter 2
                rec.pipeline_barrier(
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[], &[], &gather_cont
                )
                .bind_compute_pipeline_pair(&compute_pipelines[3], &texi2o_layout)
                .bind_compute_descriptor_sets(0, &[precompute_sets[7]], &[])
                .dispatch(Self::SCATTER_SIZE.0 / 8, Self::SCATTER_SIZE.1 / 8, Self::SCATTER_SIZE.2 / 8)
                // gather-k
                .pipeline_barrier(
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[], &[], &ready_k_gather
                )
                .bind_compute_pipeline_pair(&compute_pipelines[2], &texio_layout)
                .bind_compute_descriptor_sets(0, &[precompute_sets[4]], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                // accum
                .pipeline_barrier(
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    br::PipelineStageFlags::COMPUTE_SHADER,
                    false,
                    &[], &[], &accum_gather2
                )
                .bind_compute_pipeline_pair(&compute_pipelines[4], &texio_pure_layout)
                .bind_compute_descriptor_sets(0, &[precompute_sets[6]], &[])
                .dispatch(Self::GATHERED_SIZE.0 / 32, Self::GATHERED_SIZE.1 / 32, 1)
                .bind_compute_pipeline_pair(&compute_pipelines[5], &texio_pure_layout)
                .bind_compute_descriptor_sets(0, &[precompute_sets[5]], &[])
                .dispatch(Self::SCATTER_SIZE.0 / 8, Self::SCATTER_SIZE.1 / 8, Self::SCATTER_SIZE.2 / 8);
            }
            rec.pipeline_barrier(
                br::PipelineStageFlags::COMPUTE_SHADER,
                br::PipelineStageFlags::FRAGMENT_SHADER,
                false,
                &[], &[], &render_ready
            );
        }).expect("Dispatch Precomputation failed");
    }
}

pub struct Descriptors {
    camera_input_layout: br::DescriptorSetLayout,
    skybox_renderer_input_layout: br::DescriptorSetLayout,
    _static_sampler: br::Sampler,
    _descriptors: br::DescriptorPool,
    descriptors: Vec<br::vk::VkDescriptorSet>
}
impl Descriptors {
    pub fn new(
        g: &peridot::Graphics, dwt: &peridot::DeviceWorkingTextureStore, precomputes: &SkyboxPrecomputedTextures,
        buf: &peridot::FixedMemory, buf_offsets: &FixedBufferOffsets
    ) -> Self {
        let linear_sampler = br::SamplerBuilder::default()
            .addressing(
                br::AddressingMode::ClampToEdge, br::AddressingMode::ClampToEdge, br::AddressingMode::ClampToEdge
            )
            .create(g)
            .expect("Failed to create linear sampler");
        let camera_input_layout = br::DescriptorSetLayout::new(g, &[
            br::DescriptorSetLayoutBinding::UniformBuffer(1, br::ShaderStage::FRAGMENT.vertex())
        ]).expect("Failed to create camera input layout");
        let skybox_renderer_input_layout = br::DescriptorSetLayout::new(g, &[
            br::DescriptorSetLayoutBinding::CombinedImageSampler(
                1, br::ShaderStage::FRAGMENT, &[linear_sampler.native_ptr()]
            ),
            br::DescriptorSetLayoutBinding::CombinedImageSampler(
                1, br::ShaderStage::FRAGMENT, &[linear_sampler.native_ptr()]
            )
        ]).expect("Failed to create skybox renderer input layout");
        
        let dp = br::DescriptorPool::new(g, 2, &[
            br::DescriptorPoolSize(br::DescriptorType::CombinedImageSampler, 2),
            br::DescriptorPoolSize(br::DescriptorType::UniformBuffer, 1)
        ], false).expect("Failed to create descriptor pool");
        let descriptors = dp.alloc(&[
            &camera_input_layout, &skybox_renderer_input_layout
        ]).expect("Failed to alloc descriptor sets");
        g.update_descriptor_sets(&[
            br::DescriptorSetWriteInfo(
                descriptors[0], 0, 0,
                br::DescriptorUpdateInfo::UniformBuffer(vec![
                    (buf.buffer.0.native_ptr(), buf_offsets.uniform_range.clone())
                ])
            ),
            br::DescriptorSetWriteInfo(
                descriptors[1], 0, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(precomputes.scatter).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
            br::DescriptorSetWriteInfo(
                descriptors[1], 1, 0,
                br::DescriptorUpdateInfo::CombinedImageSampler(vec![
                    (None, dwt.get(precomputes.transmittance).native_ptr(), br::ImageLayout::ShaderReadOnlyOpt)
                ])
            ),
        ], &[]);

        Descriptors {
            camera_input_layout, skybox_renderer_input_layout,
            _static_sampler: linear_sampler,
            _descriptors: dp, descriptors
        }
    }

    pub fn camera_uniform(&self) -> br::vk::VkDescriptorSet { self.descriptors[0] }
    pub fn skybox_precomputed_textures(&self) -> br::vk::VkDescriptorSet { self.descriptors[1] }
}

pub struct SkyboxRenderer {
    main_render_shader: PvpShaderModules<'static>,
    pipeline: peridot::LayoutedPipeline
}
impl SkyboxRenderer {
    fn new<NL: NativeLinker>(
        e: &Engine<NL>,
        drt: &DetailedRenderTargets,
        descriptors: &Descriptors
    ) -> Self {
        let layout: Rc<_> = br::PipelineLayout::new(e.graphics(), &[
            &descriptors.camera_input_layout, &descriptors.skybox_renderer_input_layout
        ], &[]).expect("Faield to create SkyboxRender pipeline").into();
        let main_render_shader = PvpShaderModules::new(
            e.graphics(),
            e.load("shaders.skybox").expect("Failed to load main shader")
        ).expect("Instantiating shader failed");

        let backbuffer_size = e.backbuffer(0).expect("no backbuffers?").size().clone();
        let full_scissors = [br::vk::VkRect2D::from(br::Extent2D(backbuffer_size.0, backbuffer_size.1))];
        let full_viewports = [br::Viewport::from_rect_with_depth_range(&full_scissors[0], 0.0 .. 1.0).into_inner()];

        let main_vps = main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::new(&layout, (&drt.rp, 0), main_vps)
            .viewport_scissors(
                br::DynamicArrayState::Static(&full_viewports),
                br::DynamicArrayState::Static(&full_scissors))
            .multisample_state(Some(mp))
            .depth_test_settings(Some(br::CompareOp::LessOrEqual), false)
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .create(e.graphics(), None)
            .expect("Creating GraphicsPipeline failed");

        SkyboxRenderer {
            main_render_shader,
            pipeline: peridot::LayoutedPipeline::combine(pipeline, &layout)
        }
    }

    fn recreate_pipeline<NL: NativeLinker>(&mut self, e: &Engine<NL>, drt: &DetailedRenderTargets) {
        let backbuffer_size = e.backbuffer(0).expect("no backbuffers?").size().clone();
        let full_scissors = [br::vk::VkRect2D::from(br::Extent2D(backbuffer_size.0, backbuffer_size.1))];
        let full_viewports = [br::Viewport::from_rect_with_depth_range(&full_scissors[0], 0.0 .. 1.0).into_inner()];

        let main_vps = self.main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_TRIANGLE_STRIP);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::new(self.pipeline.layout(), (&drt.rp, 0), main_vps)
            .viewport_scissors(
                br::DynamicArrayState::Static(&full_viewports),
                br::DynamicArrayState::Static(&full_scissors))
            .multisample_state(Some(mp))
            .depth_test_settings(Some(br::CompareOp::LessOrEqual), false)
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .create(e.graphics(), None)
            .expect("Creating GraphicsPipeline failed");
        
        self.pipeline = peridot::LayoutedPipeline::combine(pipeline, self.pipeline.layout());
    }
}

pub struct GridRenderer {
    main_render_shader: PvpShaderModules<'static>,
    pipeline: peridot::LayoutedPipeline
}
impl GridRenderer {
    pub fn new<NL: NativeLinker>(
        e: &peridot::Engine<NL>,
        drt: &DetailedRenderTargets,
        descriptors: &Descriptors
    ) -> Self {
        let pl: Rc<_> = br::PipelineLayout::new(e.graphics(), &[&descriptors.camera_input_layout], &[])
            .expect("Failed to create GridRender pipeline layout")
            .into();

        let main_render_shader = PvpShaderModules::new(
            e.graphics(),
            e.load("shaders.colored").expect("Failed to load colored shader")
        ).expect("Instantiating grid render shader failed");

        let backbuffer_size = e.backbuffer(0).expect("no backbuffers?").size().clone();
        let full_scissors = [br::vk::VkRect2D::from(br::Extent2D(backbuffer_size.0, backbuffer_size.1))];
        let full_viewports = [br::Viewport::from_rect_with_depth_range(&full_scissors[0], 0.0 .. 1.0).into_inner()];

        let main_vps = main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::new(&pl, (&drt.rp, 0), main_vps)
            .viewport_scissors(
                br::DynamicArrayState::Static(&full_viewports),
                br::DynamicArrayState::Static(&full_scissors))
            .multisample_state(Some(mp))
            .depth_test_settings(Some(br::CompareOp::Less), true)
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .create(e.graphics(), None)
            .expect("Creating GraphicsPipeline failed");
        
        GridRenderer { main_render_shader, pipeline: peridot::LayoutedPipeline::combine(pipeline, &pl) }
    }

    fn recreate_pipeline<NL: NativeLinker>(&mut self, e: &Engine<NL>, drt: &DetailedRenderTargets) {
        let backbuffer_size = e.backbuffer(0).expect("no backbuffers?").size().clone();
        let full_scissors = [br::vk::VkRect2D::from(br::Extent2D(backbuffer_size.0, backbuffer_size.1))];
        let full_viewports = [br::Viewport::from_rect_with_depth_range(&full_scissors[0], 0.0 .. 1.0).into_inner()];

        let main_vps = self.main_render_shader.generate_vps(br::vk::VK_PRIMITIVE_TOPOLOGY_LINE_LIST);
        let mp = br::MultisampleState::new();
        let pipeline = br::GraphicsPipelineBuilder::new(self.pipeline.layout(), (&drt.rp, 0), main_vps)
            .viewport_scissors(
                br::DynamicArrayState::Static(&full_viewports),
                br::DynamicArrayState::Static(&full_scissors))
            .multisample_state(Some(mp))
            .depth_test_settings(Some(br::CompareOp::Less), true)
            .add_attachment_blend(br::AttachmentColorBlendState::noblend())
            .create(e.graphics(), None)
            .expect("Creating GraphicsPipeline failed");
        
        self.pipeline = peridot::LayoutedPipeline::combine(pipeline, self.pipeline.layout());
    }
}

pub struct DetailedRenderTargets {
    depth_buffer: peridot::Image,
    depth_buffer_view: br::ImageView,
    rp: br::RenderPass,
    fb: Vec<br::Framebuffer>
}
impl DetailedRenderTargets {
    pub fn new<NL: NativeLinker>(e: &Engine<NL>) -> Self {
        let depth_buffer = br::ImageDesc::new(
            e.backbuffer(0).expect("no backbuffers?").size(),
            br::vk::VK_FORMAT_D24_UNORM_S8_UINT,
            br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
            br::ImageLayout::Undefined
        ).create(e.graphics()).expect("Failed to create depth buffer");
        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(depth_buffer);
        let depth_buffer = mb.alloc().expect("Failed to alloc depth buffer memory")
            .pop().expect("no resources").unwrap_image();
        let depth_buffer_view = depth_buffer.create_view(
            None, None, &Default::default(), &br::ImageSubresourceRange::depth_stencil(0 .. 1, 0 .. 1)
        ).expect("Failed to create depth buffer view");

        let rp = peridot::RenderPassTemplates::single_render_with_depth(
            e.backbuffer_format(), br::vk::VK_FORMAT_D24_UNORM_S8_UINT, e.requesting_backbuffer_layout().0, false
        ).create(e.graphics()).expect("Creating RenderPass failed");
        let backbuffer_size = e.backbuffer(0).expect("no backbuffers?").size().clone();
        let fb = e.iter_backbuffers()
            .map(|bb|
                br::Framebuffer::new(&rp, &[&bb, &depth_buffer_view], &backbuffer_size, 1)
                    .expect("Creating Framebuffer failed")
            )
            .collect();
        
        DetailedRenderTargets { rp, fb, depth_buffer, depth_buffer_view }
    }

    pub fn discard_framebuffers(&mut self) { self.fb.clear(); }
    pub fn recreate_framebuffers<NL: NativeLinker>(
        &mut self, e: &Engine<NL>, size: &peridot::math::Vector2<usize>
    ) {
        let su32: br::Extent2D = peridot::math::Vector2::<u32>(size.0 as _, size.1 as _).into();

        let depth_buffer = br::ImageDesc::new(
            e.backbuffer(0).expect("no backbuffers?").size(),
            br::vk::VK_FORMAT_D24_UNORM_S8_UINT,
            br::ImageUsage::DEPTH_STENCIL_ATTACHMENT,
            br::ImageLayout::Undefined
        ).create(e.graphics()).expect("Failed to create depth buffer");
        let mut mb = peridot::MemoryBadget::new(e.graphics());
        mb.add(depth_buffer);
        self.depth_buffer = mb.alloc().expect("Failed to alloc depth buffer memory")
            .pop().expect("no resources").unwrap_image();
        self.depth_buffer_view = self.depth_buffer.create_view(
            None, None, &Default::default(), &br::ImageSubresourceRange::depth_stencil(0 .. 1, 0 .. 1)
        ).expect("Failed to create depth buffer view");

        self.fb = e.iter_backbuffers()
            .map(|bb|
                br::Framebuffer::new(&self.rp, &[&bb, &self.depth_buffer_view], &su32, 1)
                    .expect("Recreating Framebuffer failed")
            ).collect();
    }

    pub fn postinit_commands(&self, r: &mut br::CmdRecord) {
        r.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE, br::PipelineStageFlags::EARLY_FRAGMENT_TESTS, true, &[], &[],&[
                br::ImageMemoryBarrier::new_raw(
                    &self.depth_buffer, &br::ImageSubresourceRange::depth_stencil(0 .. 1, 0 .. 1),
                    br::ImageLayout::Undefined, br::ImageLayout::DepthStencilAttachmentOpt
                )
            ]
        );
    }
}

pub struct FixedBufferOffsets {
    fill_plane: u64,
    grid_vertices: u64, grid_vertex_count: u32,
    mut_uniform_offset: u64,
    uniform_range: std::ops::Range<usize>
}
pub struct FixedBufferInitializer {
    fill_plane: peridot::Primitive<peridot::VertexUV2D>,
    fill_plane_offset: u64,
    grid_vertices: peridot::Primitive<peridot::ColoredVertex>,
    grid_vertices_offset: u64
}
impl peridot::FixedBufferInitializer for FixedBufferInitializer {
    fn stage_data(&mut self, m: &br::MappedMemoryRange) {
        unsafe {
            m.slice_mut(self.fill_plane_offset as _, self.fill_plane.vertices.len())
                .clone_from_slice(&self.fill_plane.vertices);
            m.slice_mut(self.grid_vertices_offset as _, self.grid_vertices.vertices.len())
                .clone_from_slice(&self.grid_vertices.vertices);
        }
    }

    fn buffer_graphics_ready(
        &self,
        tfb: &mut peridot::TransferBatch,
        buf: &peridot::Buffer,
        range: std::ops::Range<u64>
    ) {
        tfb.add_buffer_graphics_ready(
            br::PipelineStageFlags::VERTEX_INPUT.fragment_shader(),
            buf,
            range.clone(),
            br::AccessFlags::VERTEX_ATTRIBUTE_READ | br::AccessFlags::UNIFORM_READ
        );
    }
}

pub struct RenderCommands {
    main_commands: peridot::CommandBundle
}
impl RenderCommands {
    fn new<NL: NativeLinker>(e: &Engine<NL>) -> Self {
        RenderCommands {
            main_commands: peridot::CommandBundle::new(
                e.graphics(),
                peridot::CBSubmissionType::Graphics,
                e.backbuffer_count()
            ).expect("Failed to create Main CommandBundle")
        }
    }

    fn generate_commands<NL: NativeLinker>(
        &self,
        e: &Engine<NL>,
        skybox_renderer: &SkyboxRenderer,
        grid_renderer: &GridRenderer,
        descriptors: &Descriptors,
        drt: &DetailedRenderTargets,
        buf: &peridot::FixedMemory,
        buf_offsets: &FixedBufferOffsets
    ) {
        let render_area = br::vk::VkRect2D::from(
            AsRef::<br::Extent2D>::as_ref(e.backbuffer(0).expect("no backbuffers?").size()).clone()
        );

        for (b, fb) in self.main_commands.iter().zip(&drt.fb) {
            b.begin().expect("Failed to begin record main command")
                .begin_render_pass(&drt.rp, fb, render_area.clone(), &[br::ClearValue::Color([0.0; 4]), br::ClearValue::DepthStencil(1.0, 0)], true)
                .bind_graphics_pipeline_pair(grid_renderer.pipeline.pipeline(), grid_renderer.pipeline.layout())
                .bind_graphics_descriptor_sets(0, &[descriptors.camera_uniform()], &[])
                .bind_vertex_buffers(0, &[(&buf.buffer.0, buf_offsets.grid_vertices as _)])
                .draw(buf_offsets.grid_vertex_count, 1, 0, 0)
                .bind_graphics_pipeline_pair(skybox_renderer.pipeline.pipeline(), skybox_renderer.pipeline.layout())
                .bind_graphics_descriptor_sets(1, &[descriptors.skybox_precomputed_textures()], &[])
                .bind_vertex_buffers(0, &[(&buf.buffer.0, buf_offsets.fill_plane as _)])
                .draw(4, 1, 0, 0)
                .end_render_pass();
        }
    }
    fn clear_commands(&self) {
        self.main_commands.reset().expect("Resetting failed");
    }
}

#[repr(C)]
pub struct Uniform {
    pub main_view_projection: peridot::math::Matrix4F32,
    pub main_view: peridot::math::Matrix4F32,
    pub persp_fov_rad: f32
}

struct Differential<T> { old_value: T }
impl<T> Differential<T> where T: std::ops::Sub<T, Output = T> + Copy {
    pub fn new(init: T) -> Self {
        Differential { old_value: init }
    }
    pub fn update(&mut self, new_value: T) -> T {
        let diff = new_value - self.old_value;
        self.old_value = new_value;
        diff
    }
}

pub struct Game<NL> {
    rt: DetailedRenderTargets,
    descriptors: Descriptors,
    _skybox_precomputed: SkyboxPrecomputedTextures,
    skybox: SkyboxRenderer,
    grid: GridRenderer,
    buf: peridot::FixedMemory,
    buf_offsets: FixedBufferOffsets,
    _precompute_textures: peridot::DeviceWorkingTextureStore,
    cmds: RenderCommands,
    update_cmds: peridot::CommandBundle,
    total_time: f32,
    view_zenith_angle: f32,
    azimuth_angle: f32,
    main_camera: peridot::math::Camera,
    mouse_x_diff: Differential<f32>,
    mouse_y_diff: Differential<f32>,
    ph: std::marker::PhantomData<*const NL>
}
impl<NL> Game<NL> {
    pub const NAME: &'static str = "pj-torchlight/procedural-skybox";
    pub const VERSION: (u32, u32, u32) = (0, 1, 0);
}
impl<NL> FeatureRequests for Game<NL> {}
impl<NL: NativeLinker> EngineEvents<NL> for Game<NL> {
    fn init(e: &mut Engine<NL>) -> Self {
        e.input_mut().map(peridot::NativeButtonInput::Mouse(0), 0);
        e.input_mut().map(peridot::NativeAnalogInput::MouseY, 0);
        e.input_mut().map(peridot::NativeAnalogInput::MouseX, 1);

        let main_camera = peridot::math::Camera {
            projection: peridot::math::ProjectionMethod::Perspective { fov: 70.0 },
            position: peridot::math::Vector3(0.0, 3.0, -5.0),
            rotation: peridot::math::Quaternion::new(0.0, peridot::math::Vector3(0.0, 1.0, 0.0)),
            depth_range: 0.1 .. 100.0
        };

        let mut precompute_textures = peridot::DeviceWorkingTextureAllocator::new();
        let skybox_precomputed = SkyboxPrecomputedTextures::prealloc(&mut precompute_textures);
        let precompute_textures = precompute_textures.alloc(e.graphics())
            .expect("Allocating Precomputed Textures failed");

        let mut bp = peridot::BufferPrealloc::new(e.graphics());
        let fill_plane = peridot::Primitive::uv_plane_centric(1.0);
        let fill_plane_offset = bp.add(
            peridot::BufferContent::vertices::<peridot::VertexUV2D>(fill_plane.vertices.len())
        );
        let grid_vertices = peridot::Primitive {
            vertices: peridot::Primitive::limited_xz_grid(10).vertices.into_iter()
                .map(|v| peridot::ColoredVertex { pos: v, color: peridot::math::Vector4(0.6, 0.6, 0.6, 1.0) })
                .chain(peridot::Primitive::limited_coordinate_axis(100).vertices)
                .collect()
        };
        let grid_vertices_offset = bp.add(
            peridot::BufferContent::vertices::<peridot::ColoredVertex>(grid_vertices.vertices.len())
        );
        let mut mbp = peridot::BufferPrealloc::new(e.graphics());
        let uniform_offset = mbp.add(peridot::BufferContent::uniform::<Uniform>());
        let mut fb_data = FixedBufferInitializer {
            fill_plane, fill_plane_offset,
            grid_vertices, grid_vertices_offset
        };
        let mut tfb = peridot::TransferBatch::new();
        let buf = peridot::FixedMemory::new(
            e.graphics(),
            bp,
            mbp,
            peridot::TextureInitializationGroup::new(e.graphics()),
            &mut fb_data,
            &mut tfb
        ).expect("Failed to initialize fixed buffers");
        let buf_offsets = FixedBufferOffsets {
            fill_plane: fb_data.fill_plane_offset,
            grid_vertices: fb_data.grid_vertices_offset,
            grid_vertex_count: fb_data.grid_vertices.vertices.len() as _,
            mut_uniform_offset: uniform_offset,
            uniform_range: (buf.mut_buffer_placement + uniform_offset) as usize ..
                (buf.mut_buffer_placement + uniform_offset) as usize + std::mem::size_of::<Uniform>()
        };
        buf.mut_buffer.0.guard_map(
            0 .. uniform_offset + std::mem::size_of::<Uniform>() as u64,
            |m| unsafe {
                *m.get_mut(uniform_offset as _) = Uniform {
                    main_view_projection: main_camera.view_projection_matrix(),
                    main_view: main_camera.view_matrix(),
                    persp_fov_rad: 70.0f32.to_radians()
                };
            }
        ).expect("Staging MutBuffer failed");
        let mut tfb_update = peridot::TransferBatch::new();
        let mut_update_range = buf.mut_buffer_placement + uniform_offset ..
            buf.mut_buffer_placement + uniform_offset + std::mem::size_of::<Uniform>() as u64;
        tfb_update.add_copying_buffer(
            buf.mut_buffer.0.with_dev_offset(uniform_offset),
            buf.buffer.0.with_dev_offset(buf.mut_buffer_placement + uniform_offset),
            std::mem::size_of::<Uniform>() as _
        );
        tfb_update.add_buffer_graphics_ready(
            br::PipelineStageFlags::FRAGMENT_SHADER,
            &buf.buffer.0,
            mut_update_range,
            br::AccessFlags::UNIFORM_READ
        );
        let update_cb = peridot::CommandBundle::new(e.graphics(), peridot::CBSubmissionType::Graphics, 1)
            .expect("Alloc CommandBundle failed(copy)");
        {
            let mut r = update_cb[0].begin().expect("Begin UpdateCmdRec");
            tfb_update.sink_transfer_commands(&mut r);
            tfb_update.sink_graphics_ready_commands(&mut r);
        }

        let rt = DetailedRenderTargets::new(e);
        
        let descriptors = Descriptors::new(e.graphics(), &precompute_textures, &skybox_precomputed, &buf, &buf_offsets);
        let skybox = SkyboxRenderer::new(e, &rt, &descriptors);
        let grid = GridRenderer::new(e, &rt, &descriptors);

        skybox_precomputed.init(e, &precompute_textures);
        let cmds = RenderCommands::new(e);
        cmds.generate_commands(e, &skybox, &grid, &descriptors, &rt, &buf, &buf_offsets);

        e.submit_commands(|r| {
            tfb.sink_transfer_commands(r); tfb.sink_graphics_ready_commands(r);
            rt.postinit_commands(r);
        }).expect("Failed to resource memory initialization");

        Game {
            rt, descriptors,
            _skybox_precomputed: skybox_precomputed,
            skybox,
            grid,
            buf,
            buf_offsets,
            _precompute_textures: precompute_textures,
            cmds,
            update_cmds: update_cb,
            total_time: 0.0,
            view_zenith_angle: 90.0,
            azimuth_angle: 0.0,
            main_camera,
            mouse_x_diff: Differential::new(0.0),
            mouse_y_diff: Differential::new(0.0),
            ph: std::marker::PhantomData
        }
    }
    fn update(
        &mut self, e: &Engine<NL>, on_backbuffer_of: u32, dt: std::time::Duration
    ) -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        let my_diff = self.mouse_y_diff.update(e.input().analog_value_abs(0));
        let mx_diff = self.mouse_x_diff.update(e.input().analog_value_abs(1));
        if e.input().button_pressing_time(0) > std::time::Duration::new(0, 0) {
            self.view_zenith_angle -= my_diff * 0.1;
            self.azimuth_angle -= mx_diff * 0.1;
            self.main_camera.rotation = peridot::math::Quaternion::new(
                (self.view_zenith_angle - 90.0).to_radians(), peridot::math::Vector3::left()
            ) * peridot::math::Quaternion::new(
                self.azimuth_angle.to_radians(), peridot::math::Vector3::up()
            );
        }

        self.total_time += dt.as_secs() as f32 + dt.subsec_micros() as f32 / 10_000_000.0; 
        self.buf.mut_buffer.0.guard_map(
            0 .. self.buf_offsets.mut_uniform_offset + std::mem::size_of::<Uniform>() as u64,
            |m| unsafe {
                *m.get_mut(self.buf_offsets.mut_uniform_offset as _) = Uniform {
                    main_view_projection: self.main_camera.view_projection_matrix(),
                    main_view: self.main_camera.view_matrix(),
                    persp_fov_rad: 70.0f32.to_radians()
                };
            }
        ).expect("Staging MutBuffer failed");

        (br::SubmissionBatch {
            command_buffers: std::borrow::Cow::Borrowed(&self.update_cmds),
            .. Default::default()
        }.into(), br::SubmissionBatch {
            command_buffers: std::borrow::Cow::Borrowed(
                &self.cmds.main_commands[on_backbuffer_of as usize .. (on_backbuffer_of + 1) as usize]
            ),
            .. Default::default()
        })
    }

    fn discard_backbuffer_resources(&mut self) {
        self.cmds.clear_commands();
        self.rt.discard_framebuffers();
    }
    fn on_resize(&mut self, e: &Engine<NL>, new_size: peridot::math::Vector2<usize>) {
        self.rt.recreate_framebuffers(e, &new_size);
        self.skybox.recreate_pipeline(e, &self.rt);
        self.grid.recreate_pipeline(e, &self.rt);
        self.cmds.generate_commands(e, &self.skybox, &self.grid, &self.descriptors, &self.rt, &self.buf, &self.buf_offsets);

        e.submit_commands(|r| {
            self.rt.postinit_commands(r);
        }).expect("Failed to execute resize commands");
    }
}
