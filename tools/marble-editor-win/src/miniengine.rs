use bedrock as br;
use br::{Device, Instance, PhysicalDevice, PipelineCache, VulkanStructure};
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    path::PathBuf,
    rc::Rc,
};

use crate::SharedMut;

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

pub struct MiniEngine {
    pub graphics_objects: MiniEngineGraphicsObjects,
    pub memory_manager: peridot_memory_manager::MemoryManager,
    pub resources_base: PathBuf,
    pub temp_base: PathBuf,
    pub loaded_shaders: HashMap<String, Rc<br::ShaderModuleObject<StdVkDevice>>>,
    pub pipeline_cache: br::PipelineCacheObject<StdVkDevice>,
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
            ])
            .set_api_version(1, 3, 0);
        let instance = Rc::new(instance.create().expect("Failed to create vulkan instance"));
        let adapter = instance
            .iter_physical_devices()
            .expect("Failed to enumerate physical devices")
            .next()
            .expect("no physical devices?");
        let memory_properties = adapter.memory_properties();
        let adapter_features = adapter.features();
        let adapter_limits = adapter.properties().limits;
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
            adapter_features,
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
        })
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
    pub fn pipeline_cache(&self) -> &br::PipelineCacheObject<StdVkDevice> {
        &self.pipeline_cache
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
    pub fn alloc_device_local_image(
        &mut self,
        desc: br::ImageDesc,
    ) -> br::Result<peridot_memory_manager::Image> {
        self.memory_manager
            .allocate_device_local_image(&self.graphics_objects, desc)
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
