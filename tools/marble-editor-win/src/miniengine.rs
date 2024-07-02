use bedrock::{self as br, CommandBuffer, CommandPool, ImageSubresourceSlice};
use br::{Device, Instance, PhysicalDevice, PipelineCache, Queue, VulkanStructure};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
};

use crate::{app_subsystem_instances::AppSubsystemInstances, utils::SafeF32, SharedMut};

pub type StdVkDevice = Rc<br::DeviceObject<Rc<br::InstanceObject>>>;

pub struct MiniEngineGraphicsObjects {
    pub device: StdVkDevice,
    pub adapter: br::PhysicalDeviceObject<Rc<br::InstanceObject>>,
    pub enabled_vk_extensions: HashSet<&'static str>,
    pub graphics_queue_family: u32,
    pub graphics_queue: Rc<RefCell<br::QueueObject<StdVkDevice>>>,
    pub memory_properties: br::MemoryProperties,
    pub adapter_features: br::vk::VkPhysicalDeviceFeatures,
    pub adapter_limits: br::vk::VkPhysicalDeviceLimits,
}
impl peridot_memory_manager::AdapterPropertiesProvider for MiniEngineGraphicsObjects {
    #[inline]
    fn available_features(&self) -> &br::vk::VkPhysicalDeviceFeatures {
        &self.adapter_features
    }

    #[inline]
    fn limits(&self) -> &br::vk::VkPhysicalDeviceLimits {
        &self.adapter_limits
    }

    #[inline]
    fn memory_properties(&self) -> &br::MemoryProperties {
        &self.memory_properties
    }
}
impl peridot_memory_manager::MemoryAllocationSource for MiniEngineGraphicsObjects {
    fn device(&self) -> &peridot::DeviceObject {
        &self.device
    }

    fn adapter(&self) -> &(impl br::PhysicalDevice + ?Sized) {
        &self.adapter
    }

    fn can_request_extended_memory_requirements(&self) -> bool {
        self.enabled_vk_extensions
            .contains("VK_KHR_get_memory_requirements2")
    }

    fn dedicated_allocation_available(&self) -> bool {
        self.enabled_vk_extensions
            .contains("VK_KHR_dedicated_allocation")
    }

    fn extended_memory_binding_available(&self) -> bool {
        self.enabled_vk_extensions.contains("VK_KHR_bind_memory2")
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SamplerDesc {
    pub mag_filter: br::FilterMode,
    pub min_filter: br::FilterMode,
    pub mip_filter: br::MipmapFilterMode,
    pub address_mode: (br::AddressingMode, br::AddressingMode, br::AddressingMode),
    pub mip_lod_bias: SafeF32,
    pub max_anisotropy: Option<SafeF32>,
    pub compare_op: Option<br::CompareOp>,
    pub lod_range: core::ops::Range<SafeF32>,
    pub border_color: br::BorderColor,
    pub unnormalized_coordinates: bool,
}
impl Default for SamplerDesc {
    fn default() -> Self {
        Self {
            mag_filter: br::FilterMode::Linear,
            min_filter: br::FilterMode::Linear,
            mip_filter: br::MipmapFilterMode::Linear,
            address_mode: (
                br::AddressingMode::Repeat,
                br::AddressingMode::Repeat,
                br::AddressingMode::Repeat,
            ),
            mip_lod_bias: unsafe { SafeF32::new_unchecked(0.0) },
            max_anisotropy: None,
            compare_op: None,
            lod_range: unsafe { SafeF32::new_unchecked(0.0)..SafeF32::new_unchecked(0.0) },
            border_color: br::BorderColor::TransparentBlackF,
            unnormalized_coordinates: false,
        }
    }
}
impl SamplerDesc {
    pub fn build<Device: br::Device>(
        &self,
        device: Device,
    ) -> br::Result<br::SamplerObject<Device>> {
        unsafe {
            br::SamplerBuilder::new()
                .filter(self.mag_filter, self.min_filter)
                .addressing(
                    self.address_mode.0,
                    self.address_mode.1,
                    self.address_mode.2,
                )
                .comparison(self.compare_op)
                .lod_bias(self.mip_lod_bias.value())
                .lod_clamp(self.lod_range.start.value(), self.lod_range.end.value())
                .max_anisotropy(self.max_anisotropy.map(|x| x.value()))
                .mip_filter(self.mip_filter)
                .unnormalized_coordinates(self.unnormalized_coordinates)
                .create(device)
        }
    }
}

pub struct MiniEngine {
    pub graphics_objects: MiniEngineGraphicsObjects,
    pub memory_manager: peridot_memory_manager::MemoryManager,
    pub resources_base: PathBuf,
    pub temp_base: PathBuf,
    pub loaded_shaders: HashMap<String, Rc<br::ShaderModuleObject<StdVkDevice>>>,
    pub sampler_store: HashMap<SamplerDesc, Rc<br::SamplerObject<StdVkDevice>>>,
    pub pipeline_cache: br::PipelineCacheObject<StdVkDevice>,
    pub transient_command_pool: br::CommandPoolObject<StdVkDevice>,
    pub transient_command_buffer: br::CommandBufferObject<StdVkDevice>,
}
impl MiniEngine {
    pub fn new() -> br::Result<Self> {
        let mut instance = br::InstanceBuilder::new(
            "Peridot Marble Editor",
            (0, 1, 0),
            "Peridot-mini",
            (0, 1, 0),
        );
        instance
            .add_extensions([
                "VK_KHR_external_memory_capabilities",
                "VK_KHR_get_physical_device_properties2",
                "VK_EXT_debug_utils",
            ])
            .set_api_version(1, 3, 0);
        let instance = Rc::new(instance.create().expect("Failed to create vulkan instance"));
        let adapter = instance
            .iter_physical_devices()
            .expect("Failed to enumerate physical devices")
            .next()
            .expect("no physical devices?");
        let memory_properties = adapter.memory_properties();

        let mut adapter_features = br::vk::VkPhysicalDeviceFeatures2KHR::uninit_sink();
        let mut adapter_line_rasterization_features =
            br::vk::VkPhysicalDeviceLineRasterizationFeaturesKHR::uninit_sink();
        adapter.features2(
            unsafe { &mut *adapter_features.as_mut_ptr() },
            &mut [unsafe { &mut *adapter_line_rasterization_features.as_mut_ptr() }],
        );
        let adapter_features = unsafe { adapter_features.assume_init() };
        let adapter_line_rasterization_features =
            unsafe { adapter_line_rasterization_features.assume_init() };
        println!(
            "LineRasterizationFeatures.smoothLines: {}",
            adapter_line_rasterization_features.smoothLines
        );

        let mut adapter_properties = br::vk::VkPhysicalDeviceProperties2KHR::uninit_sink();
        let mut adapter_line_rasterization_properties =
            br::vk::VkPhysicalDeviceLineRasterizationPropertiesKHR::uninit_sink();
        adapter.properties2(
            unsafe { &mut *adapter_properties.as_mut_ptr() },
            &mut [unsafe { &mut *adapter_line_rasterization_properties.as_mut_ptr() }],
        );
        let adapter_properties = unsafe { adapter_properties.assume_init() };
        let adapter_line_rasterization_properties =
            unsafe { adapter_line_rasterization_properties.assume_init() };
        let adapter_limits = adapter_properties.properties.limits;
        println!(
            "LineRasterizationProperties.lineSubPixelPrecisionBits: {:08x}",
            adapter_line_rasterization_properties.lineSubPixelPrecisionBits
        );

        let queue_families = adapter.queue_family_properties();
        let graphics_queue_family_index = queue_families
            .find_matching_index(br::QueueFlags::GRAPHICS)
            .expect("no graphics queue?");
        let device_queue_create_info =
            [br::DeviceQueueCreateInfo::new(graphics_queue_family_index).priorities([0.0])];

        let optional_extensions = [
            "VK_KHR_dedicated_allocation",
            "VK_KHR_get_memory_requirements2",
            "VK_KHR_bind_memory2",
        ];
        let mut device_extensions = vec![
            "VK_KHR_external_memory",
            "VK_KHR_external_memory_win32",
            "VK_KHR_synchronization2",
        ];
        if adapter_line_rasterization_features.smoothLines != 0 {
            device_extensions.push("VK_KHR_line_rasterization");
        }
        for e in adapter
            .enumerate_extension_properties(None)
            .expect("Failed to enumerate device extensions")
        {
            let Some(name) = e.extensionName.as_cstr().ok().and_then(|s| s.to_str().ok()) else {
                continue;
            };

            if let Some(name_stref) = optional_extensions.iter().find(|&&x| x == name) {
                device_extensions.push(*name_stref);
            }
        }

        let mut device = br::DeviceBuilder::new(&adapter);
        device
            .add_extensions(device_extensions.iter().copied())
            .add_queues(device_queue_create_info)
            .add_extra_features(br::vk::VkPhysicalDeviceSynchronization2FeaturesKHR {
                sType: br::vk::VkPhysicalDeviceSynchronization2FeaturesKHR::TYPE,
                pNext: core::ptr::null_mut(),
                synchronization2: 1,
            })
            .add_extra_features(br::vk::VkPhysicalDeviceLineRasterizationFeaturesKHR {
                sType: br::vk::VkPhysicalDeviceLineRasterizationFeaturesKHR::TYPE,
                pNext: core::ptr::null_mut(),
                smoothLines: adapter_line_rasterization_features.smoothLines,
                rectangularLines: false as _,
                bresenhamLines: false as _,
                stippledRectangularLines: false as _,
                stippledBresenhamLines: false as _,
                stippledSmoothLines: false as _,
            });
        let device = Rc::new(
            device
                .create()
                .expect("Failed to create vulkan device object")
                .clone_parent(),
        );
        let graphics_queue = device.clone().queue(graphics_queue_family_index, 0);

        let graphics_objects = MiniEngineGraphicsObjects {
            device,
            adapter: adapter.clone_parent(),
            enabled_vk_extensions: device_extensions.into_iter().collect(),
            graphics_queue_family: graphics_queue_family_index,
            graphics_queue: Rc::new(RefCell::new(graphics_queue)),
            memory_properties,
            adapter_features: adapter_features.features,
            adapter_limits,
        };
        let memory_manager = peridot_memory_manager::MemoryManager::new(&graphics_objects);

        // TODO: これはそのうちexeの相対位置にする（今やると大変なのでいったん作業ディレクトリから見る）
        let temp_base =
            PathBuf::from(std::env::current_dir().expect("FAiled to get current dir")).join("temp");
        let pipeline_cache_at = temp_base.join("miniengine/pipeline_cache");
        let pc_init_content = if pipeline_cache_at.exists() {
            std::fs::read(&pipeline_cache_at).expect("Failed to read stored pipeline cache")
        } else {
            Vec::new()
        };
        let pipeline_cache = graphics_objects
            .device
            .clone()
            .new_pipeline_cache(&pc_init_content)
            .expect("Failed to create pipeline cache");

        let mut transient_command_pool =
            br::CommandPoolBuilder::new(graphics_objects.graphics_queue_family)
                .transient()
                .create(graphics_objects.device.clone())
                .expect("Failed to create transient command pool");
        let [transient_command_buffer] = transient_command_pool
            .alloc_array::<1>(true)
            .expect("Failed to allocate transient command buffer");

        Ok(Self {
            graphics_objects,
            memory_manager,
            // TODO: これはそのうちexeの相対位置にする（今やると大変なのでいったん作業ディレクトリから見る）
            resources_base: PathBuf::from(
                std::env::current_dir().expect("Failed to get current dir"),
            )
            .join("resources"),
            // TODO: これはそのうちexeの相対位置にする（今やると大変なのでいったん作業ディレクトリから見る）
            temp_base,
            pipeline_cache,
            loaded_shaders: HashMap::new(),
            sampler_store: HashMap::new(),
            transient_command_pool,
            transient_command_buffer,
        })
    }

    #[inline(always)]
    pub fn adapter(&self) -> &impl br::PhysicalDevice {
        &self.graphics_objects.adapter
    }

    #[inline(always)]
    pub fn device(&self) -> &StdVkDevice {
        &self.graphics_objects.device
    }

    #[inline(always)]
    pub fn graphics_queue_family_index(&self) -> u32 {
        self.graphics_objects.graphics_queue_family
    }

    #[inline(always)]
    pub fn graphics_queue(&self) -> &SharedMut<br::QueueObject<StdVkDevice>> {
        &self.graphics_objects.graphics_queue
    }

    #[inline(always)]
    pub fn command_pool_builder_for_graphics_work(&self) -> br::CommandPoolBuilder {
        br::CommandPoolBuilder::new(self.graphics_objects.graphics_queue_family)
    }

    #[inline(always)]
    pub fn submit_graphics_works_and_wait(&self, works: &[br::SubmitInfo2]) -> br::Result<()> {
        let mut q = self.graphics_objects.graphics_queue.borrow_mut();

        q.submit2(works, None::<&mut br::FenceObject<StdVkDevice>>)?;
        q.wait()?;

        Ok(())
    }

    #[inline(always)]
    pub fn pipeline_cache(&self) -> &br::PipelineCacheObject<StdVkDevice> {
        &self.pipeline_cache
    }

    pub fn create_graphics_pipeline_array<const N: usize>(
        &self,
        infos: &[br::vk::VkGraphicsPipelineCreateInfo; N],
    ) -> br::Result<[br::PipelineObject<StdVkDevice>; N]> {
        let res = self
            .graphics_objects
            .device
            .new_graphics_pipeline_array(infos, Some(&self.pipeline_cache))?;
        self.writeback_pipeline_cache();

        Ok(res)
    }

    pub fn create_compute_pipeline_array<const N: usize>(
        &self,
        infos: &[br::ComputePipelineBuilder<impl br::PipelineLayout, impl br::PipelineShaderProvider>;
             N],
    ) -> br::Result<[br::PipelineObject<StdVkDevice>; N]> {
        let res = self
            .graphics_objects
            .device
            .new_compute_pipeline_array(infos, Some(&self.pipeline_cache))?;
        self.writeback_pipeline_cache();

        Ok(res)
    }

    pub fn writeback_pipeline_cache(&self) {
        let pc_data = match self.pipeline_cache.data() {
            Ok(x) => x,
            Err(e) => {
                eprintln!("Err: pipeline cache data retrieving failed: {e:?}");
                return;
            }
        };
        if let Err(e) = std::fs::create_dir_all(self.temp_base.join("miniengine")) {
            eprintln!("Err: miniengine temp dir creation failed: {e:?}");
            return;
        }
        if let Err(e) = std::fs::write(self.temp_base.join("miniengine/pipeline_cache"), pc_data) {
            eprintln!("Err: pipeline cache storing failed: {e:?}");
        }
    }

    #[inline]
    pub fn find_device_local_memory_index(&self, type_bits: u32) -> Option<u32> {
        self.graphics_objects
            .memory_properties
            .find_device_local_index(type_bits)
    }

    #[inline]
    pub fn shader(
        &mut self,
        path: impl Into<String>,
    ) -> br::Result<Rc<br::ShaderModuleObject<StdVkDevice>>> {
        match self.loaded_shaders.entry(path.into()) {
            std::collections::hash_map::Entry::Occupied(e) => Ok(e.get().clone()),
            std::collections::hash_map::Entry::Vacant(e) => {
                let code = std::fs::read(self.resources_base.join(e.key()))
                    .expect("Failed to read shader file");
                let object = self
                    .graphics_objects
                    .device
                    .clone()
                    .new_shader_module(&code)?;

                Ok(e.insert(Rc::new(object)).clone())
            }
        }
    }

    #[inline]
    pub fn sampler(&mut self, desc: SamplerDesc) -> br::Result<Rc<br::SamplerObject<StdVkDevice>>> {
        match self.sampler_store.entry(desc) {
            std::collections::hash_map::Entry::Occupied(e) => Ok(e.get().clone()),
            std::collections::hash_map::Entry::Vacant(e) => {
                let obj = e.key().build(self.graphics_objects.device.clone())?;

                Ok(e.insert(Rc::new(obj)).clone())
            }
        }
    }

    #[inline]
    pub fn alloc_device_local_buffer(
        &mut self,
        desc: br::BufferDesc,
    ) -> br::Result<peridot_memory_manager::Buffer> {
        self.memory_manager
            .allocate_device_local_buffer(&self.graphics_objects, desc)
    }

    #[inline]
    pub fn alloc_device_local_buffer_array<const N: usize>(
        &mut self,
        descs: [br::BufferDesc; N],
    ) -> br::Result<[peridot_memory_manager::Buffer; N]> {
        self.memory_manager
            .allocate_device_local_buffer_array(&self.graphics_objects, descs)
    }

    #[inline]
    pub fn alloc_upload_buffer_array<const N: usize>(
        &mut self,
        descs: [br::BufferDesc; N],
    ) -> br::Result<[peridot_memory_manager::Buffer; N]> {
        self.memory_manager
            .allocate_upload_buffer_array(&self.graphics_objects, descs)
    }

    #[inline]
    pub fn alloc_upload_buffer(
        &mut self,
        desc: br::BufferDesc,
    ) -> br::Result<peridot_memory_manager::Buffer> {
        self.memory_manager
            .allocate_upload_buffer(&self.graphics_objects, desc)
    }

    #[inline]
    pub fn alloc_device_local_image(
        &mut self,
        desc: br::ImageDesc,
    ) -> br::Result<peridot_memory_manager::Image> {
        self.memory_manager
            .allocate_device_local_image(&self.graphics_objects, desc)
    }

    #[inline]
    pub fn alloc_device_local_image_array<const N: usize>(
        &mut self,
        descs: [br::ImageDesc; N],
    ) -> br::Result<[peridot_memory_manager::Image; N]> {
        self.memory_manager
            .allocate_device_local_image_array(&self.graphics_objects, descs)
    }

    #[inline]
    pub fn has_extra_line_rasterization_enabled(&self) -> bool {
        self.graphics_objects
            .enabled_vk_extensions
            .contains("VK_KHR_line_rasterization")
    }

    pub fn submit_transient_commands_and_wait(
        &mut self,
        rec: impl FnOnce(
            br::CmdRecord<br::CommandBufferObject<StdVkDevice>, StdVkDevice>,
        ) -> br::CmdRecord<br::CommandBufferObject<StdVkDevice>, StdVkDevice>,
    ) -> br::Result<()> {
        self.transient_command_pool.reset(true)?;
        rec(unsafe {
            self.transient_command_buffer
                .begin_once(&self.graphics_objects.device)?
        })
        .end()?;

        self.submit_graphics_works_and_wait(&[br::SubmitInfo2::new(
            &[],
            &[br::CommandBufferSubmitInfo::new(
                &self.transient_command_buffer,
            )],
            &[],
        )])
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct Vec4([f32; 4]);
impl Vec4 {
    #[inline(always)]
    pub const fn new(x: f32, y: f32, z: f32, w: f32) -> Self {
        Self([x, y, z, w])
    }
}

#[repr(transparent)]
#[derive(Clone)]
pub struct Mat4([Vec4; 4]);
impl Mat4 {
    pub const IDENTITY: Self = Self::new_rows(
        Vec4::new(1.0, 0.0, 0.0, 0.0),
        Vec4::new(0.0, 1.0, 0.0, 0.0),
        Vec4::new(0.0, 0.0, 1.0, 0.0),
        Vec4::new(0.0, 0.0, 0.0, 1.0),
    );

    #[inline(always)]
    pub const fn new_rows(a: Vec4, b: Vec4, c: Vec4, d: Vec4) -> Self {
        Self([a, b, c, d])
    }
}

#[repr(C)]
#[derive(Clone)]
pub struct ColoredVertex {
    pub pos: Vec4,
    pub color: Vec4,
}
impl ColoredVertex {
    #[inline]
    pub const fn single_binding(
        pos_location: u32,
        color_location: u32,
    ) -> (
        [br::VertexInputBindingDescription; 1],
        [br::vk::VkVertexInputAttributeDescription; 2],
    ) {
        (
            [br::VertexInputBindingDescription::per_vertex_typed::<Self>(
                0,
            )],
            [
                br::vk::VkVertexInputAttributeDescription {
                    location: pos_location,
                    binding: 0,
                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                    offset: 0,
                },
                br::vk::VkVertexInputAttributeDescription {
                    location: color_location,
                    binding: 0,
                    format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                    offset: core::mem::offset_of!(Self, color) as _,
                },
            ],
        )
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct GenericVertex {
    pub pos: peridot_math::Vector4F32,
    pub normal: peridot_math::Vector4F32,
    pub uv: peridot_math::Vector4F32,
}
impl GenericVertex {
    pub fn unit_cube() -> ([GenericVertex; 24], [u16; 36]) {
        let vertices = [
            // +X
            GenericVertex {
                pos: peridot_math::Vector4(1.0, 1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(1.0, -1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(1.0, 1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(1.0, -1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(1.0, 0.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            // +Y
            GenericVertex {
                pos: peridot_math::Vector4(1.0, 1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(1.0, 1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, 1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, 1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 1.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            // +Z
            GenericVertex {
                pos: peridot_math::Vector4(1.0, 1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, 1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(1.0, -1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, -1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 0.0, 1.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            // -X
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, 1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, 1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, -1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, -1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(-1.0, 0.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            // -Y
            GenericVertex {
                pos: peridot_math::Vector4(1.0, -1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, -1.0, 1.0, 1.0),
                normal: peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(1.0, -1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, -1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(0.0, -1.0, 0.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            // -Z
            GenericVertex {
                pos: peridot_math::Vector4(1.0, 1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(1.0, -1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, 1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
            GenericVertex {
                pos: peridot_math::Vector4(-1.0, -1.0, -1.0, 1.0),
                normal: peridot_math::Vector4(0.0, 0.0, -1.0, 0.0),
                uv: peridot_math::Vector4(0.0, 0.0, 0.0, 0.0),
            },
        ];

        let indices = [
            0, 1, 2, 2, 1, 3, 4, 5, 6, 6, 5, 7, 8, 9, 10, 10, 9, 11, 12, 13, 14, 14, 13, 15, 16,
            17, 18, 18, 17, 19, 20, 21, 22, 22, 21, 23,
        ];

        (vertices, indices)
    }
}

#[repr(C)]
#[derive(Clone, Copy)]
pub struct UVVertex2D {
    pub pos: peridot_math::Vector2F32,
    pub uv: peridot_math::Vector2F32,
}

pub struct UtilityVertices {
    pub buffer: peridot_memory_manager::Buffer,
    pub uv_triangle_strip_fill_plane2d_offset: br::vk::VkDeviceSize,
}
impl UtilityVertices {
    pub fn new(
        engine: &mut MiniEngine,
        cmdrec: &mut br::CmdRecord<
            impl br::VkHandleMut<Handle = br::vk::VkCommandBuffer>,
            StdVkDevice,
        >,
    ) -> br::Result<Self> {
        let mut buffer_prealloc = peridot::BufferPrealloc::new(engine.device(), engine.adapter());
        let uv_triangle_strip_fill_plane2d_offset =
            buffer_prealloc.add(peridot::BufferContent::vertices::<UVVertex2D>(4));
        let total_size = buffer_prealloc.total_size();

        let buffer_desc =
            buffer_prealloc.build_desc_custom_usage(br::BufferUsage::VERTEX_BUFFER.transfer_dest());
        let buffer_stg_desc =
            buffer_prealloc.build_desc_custom_usage(br::BufferUsage::TRANSFER_SRC);
        drop(buffer_prealloc);

        let buffer = engine.alloc_device_local_buffer(buffer_desc)?;
        let mut buffer_stg = engine.alloc_upload_buffer(buffer_stg_desc)?;
        buffer_stg.guard_map(peridot_memory_manager::BufferMapMode::Write, |ptr| unsafe {
            ptr.copy_slice_to(
                uv_triangle_strip_fill_plane2d_offset as _,
                &[
                    UVVertex2D {
                        pos: peridot_math::Vector2(-1.0, -1.0),
                        uv: peridot_math::Vector2(0.0, 0.0),
                    },
                    UVVertex2D {
                        pos: peridot_math::Vector2(1.0, -1.0),
                        uv: peridot_math::Vector2(1.0, 0.0),
                    },
                    UVVertex2D {
                        pos: peridot_math::Vector2(-1.0, 1.0),
                        uv: peridot_math::Vector2(0.0, 1.0),
                    },
                    UVVertex2D {
                        pos: peridot_math::Vector2(1.0, 1.0),
                        uv: peridot_math::Vector2(1.0, 1.0),
                    },
                ],
            );
        })?;

        unsafe {
            // update_inplace
            core::ptr::write(
                cmdrec,
                core::ptr::read(cmdrec)
                    .copy_buffer(
                        &buffer_stg,
                        &buffer,
                        &[br::BufferCopy::mirror(0, total_size)],
                    )
                    .pipeline_barrier_2(&br::DependencyInfo::new(
                        &[br::MemoryBarrier2::new()
                            .from(
                                br::PipelineStageFlags2::COPY,
                                br::AccessFlags2::TRANSFER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::VERTEX_INPUT,
                                br::AccessFlags2::VERTEX_ATTRIBUTE_READ,
                            )],
                        &[],
                        &[],
                    )),
            );
        }

        Ok(Self {
            buffer,
            uv_triangle_strip_fill_plane2d_offset,
        })
    }
}

pub struct TempRT {
    org_desc: br::ImageDesc<'static>,
    view_aspect_mask: br::AspectMask,
    view_mip_range: core::ops::Range<u32>,
    view_array_range: core::ops::Range<u32>,
    pub resource: Rc<br::ImageViewObject<peridot_memory_manager::Image>>,
}
impl TempRT {
    pub fn new(
        e: &mut MiniEngine,
        org_desc: br::ImageDesc<'static>,
        view_aspect_mask: br::AspectMask,
        view_mip_range: core::ops::Range<u32>,
        view_array_range: core::ops::Range<u32>,
    ) -> br::Result<Self> {
        let resource = e.alloc_device_local_image(org_desc.clone())?;
        let resource = resource
            .subresource_range(
                view_aspect_mask,
                view_mip_range.clone(),
                view_array_range.clone(),
            )
            .view_builder()
            .create()?;

        Ok(Self {
            org_desc,
            view_aspect_mask,
            view_mip_range,
            view_array_range,
            resource: Rc::new(resource),
        })
    }

    pub fn recreate_newsize(&mut self, size: impl br::ImageSize) -> br::Result<()> {
        unsafe {
            // update inplace
            core::ptr::write(
                &mut self.org_desc,
                core::ptr::read(&self.org_desc).size(size),
            );
        }

        let resource = AppSubsystemInstances::get()
            .mini_engine
            .borrow_mut()
            .alloc_device_local_image(self.org_desc.clone())?;
        self.resource = Rc::new(
            resource
                .subresource_range(
                    self.view_aspect_mask,
                    self.view_mip_range.clone(),
                    self.view_array_range.clone(),
                )
                .view_builder()
                .create()?,
        );

        Ok(())
    }
}
