use crate::mthelper::SharedRef;
use bedrock as br;
use br::{
    CommandBuffer, CommandPool, Device, Instance, InstanceChild, PhysicalDevice, Queue,
    SubmissionBatch,
};
use cfg_if::cfg_if;
use log::{info, warn};
use std::{collections::HashSet, ops::Deref};

pub type InstanceObject = SharedRef<br::InstanceObject>;
pub type DeviceObject = SharedRef<br::DeviceObject<InstanceObject>>;

/// Queue object with family index
pub struct QueueSet<Device: br::Device> {
    pub(crate) q: parking_lot::Mutex<br::QueueObject<Device>>,
    pub(crate) family: u32,
}

mod command_bundle;
pub use self::command_bundle::*;
#[cfg(feature = "mt")]
mod async_fence_driver;
#[cfg(feature = "mt")]
pub use self::async_fence_driver::*;

#[derive(Debug)]
pub enum GraphicsInitializationError {
    LayerEnumerationFailed(br::VkResultBox),
    ExtensionEnumerationFailed(br::VkResultBox),
    VulkanError(br::VkResultBox),
    NoPhysicalDevices,
    NoSuitableGraphicsQueue,
}
impl From<br::VkResultBox> for GraphicsInitializationError {
    fn from(value: br::VkResultBox) -> Self {
        Self::VulkanError(value)
    }
}
impl std::fmt::Display for GraphicsInitializationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LayerEnumerationFailed(r) => write!(f, "vk layer enumeration failed: {r}"),
            Self::ExtensionEnumerationFailed(r) => {
                write!(f, "vk extension enumeration failed: {r}")
            }
            Self::VulkanError(r) => std::fmt::Display::fmt(r, f),
            Self::NoPhysicalDevices => write!(f, "no physical devices available on this machine"),
            Self::NoSuitableGraphicsQueue => {
                write!(f, "no suitable graphics queue found on device")
            }
        }
    }
}
impl std::error::Error for GraphicsInitializationError {}

cfg_if! {
    if #[cfg(feature = "mt")] {
        use std::sync::OnceLock as OnceValue;
    } else {
        use std::cell::OnceCell as OnceValue;
    }
}

struct CachedAdapterProperties {
    pub available_features: OnceValue<br::vk::VkPhysicalDeviceFeatures>,
    pub properties: OnceValue<br::vk::VkPhysicalDeviceProperties>,
    pub memory_properties: OnceValue<br::MemoryProperties>,
}
impl CachedAdapterProperties {
    const fn new() -> Self {
        Self {
            available_features: OnceValue::new(),
            properties: OnceValue::new(),
            memory_properties: OnceValue::new(),
        }
    }
}

/// Graphics manager
pub struct Graphics {
    pub(crate) adapter: br::PhysicalDeviceObject<InstanceObject>,
    pub(crate) device: DeviceObject,
    pub(crate) graphics_queue: QueueSet<DeviceObject>,
    cp_onetime_submit: br::CommandPoolObject<DeviceObject>,
    pub memory_type_manager: MemoryTypeManager,
    enabled_vk_extensions: HashSet<String>,
    adapter_properties: CachedAdapterProperties,
    #[cfg(feature = "mt")]
    fence_reactor: FenceReactorThread<DeviceObject>,
    #[cfg(feature = "debug")]
    _debug_instance: br::DebugUtilsMessengerObject<InstanceObject>,
}
impl Graphics {
    pub(crate) fn new(
        app_name: &str,
        app_version: (u32, u32, u32),
        instance_extensions: Vec<&str>,
        device_extensions: Vec<&str>,
        features: br::vk::VkPhysicalDeviceFeatures,
    ) -> Result<Self, GraphicsInitializationError> {
        info!("Supported Layers: ");
        let mut validation_layer_available = false;
        for l in br::enumerate_layer_properties()
            .map_err(GraphicsInitializationError::LayerEnumerationFailed)?
        {
            let name_str = l
                .layerName
                .as_cstr()
                .expect("Failed to decode")
                .to_str()
                .expect("invalid sequence in layer name");
            info!(
                "* {name_str} :: {}/{}",
                l.specVersion, l.implementationVersion
            );

            #[cfg(debug_assertions)]
            if name_str == "VK_LAYER_KHRONOS_validation" {
                validation_layer_available = true;
            }
        }

        if !validation_layer_available {
            warn!("Validation Layer is not found!");
        }

        let mut ib =
            br::InstanceBuilder::new(app_name, app_version, "Interlude2:Peridot", (0, 1, 0));
        ib.add_extensions(instance_extensions.iter().copied());
        #[cfg(debug_assertions)]
        ib.add_extension("VK_EXT_debug_report");
        if validation_layer_available {
            ib.add_layer("VK_LAYER_KHRONOS_validation");
        }
        #[cfg(feature = "debug")]
        {
            ib.add_extension("VK_EXT_debug_utils");
            log::debug!("Debug reporting activated");
        }
        let instance = SharedRef::new(ib.create()?);

        #[cfg(feature = "debug")]
        let _debug_instance = br::DebugUtilsMessengerCreateInfo::new(crate::debug::debug_utils_out)
            .filter_severity(br::DebugUtilsMessageSeverityFlags::ERROR.and_warning())
            .create(instance.clone())?;

        let adapter = instance
            .iter_physical_devices()?
            .next()
            .ok_or(GraphicsInitializationError::NoPhysicalDevices)?;

        let mut auto_device_extensions = Vec::new();
        info!("Device Extensions: ");
        for d in adapter
            .enumerate_extension_properties(None)
            .map_err(GraphicsInitializationError::ExtensionEnumerationFailed)?
        {
            let name = d
                .extensionName
                .as_cstr()
                .expect("Failed to decode")
                .to_str()
                .expect("invalid sequence");
            info!("* {name}: {}", d.specVersion);

            if name == "VK_KHR_dedicated_allocation" || name == "VK_KHR_get_memory_requirements2" {
                auto_device_extensions.push(name.to_owned());
            }
        }

        let memory_type_manager = MemoryTypeManager::new(&adapter);
        MemoryTypeManager::diagnose_heaps(&adapter);
        memory_type_manager.diagnose_types();
        let gqf_index = adapter
            .queue_family_properties()
            .find_matching_index(br::QueueFlags::GRAPHICS)
            .ok_or(GraphicsInitializationError::NoSuitableGraphicsQueue)?;
        let device = {
            let mut db = br::DeviceBuilder::new(&adapter);
            db.add_extensions(device_extensions.iter().copied())
                .add_extensions(auto_device_extensions.iter().map(|x| x as _))
                .add_queue(br::DeviceQueueCreateInfo::new(gqf_index).add(0.0));
            if validation_layer_available {
                db.add_layer("VK_LAYER_KHRONOS_validation");
            }
            *db.mod_features() = features;
            SharedRef::new(db.create()?.clone_parent())
        };

        Ok(Self {
            cp_onetime_submit: br::CommandPoolBuilder::new(gqf_index)
                .transient()
                .create(device.clone())?,
            graphics_queue: QueueSet {
                q: parking_lot::Mutex::new(device.clone().queue(gqf_index, 0)),
                family: gqf_index,
            },
            adapter: adapter.clone_parent(),
            device,
            adapter_properties: CachedAdapterProperties::new(),
            enabled_vk_extensions: auto_device_extensions
                .into_iter()
                .chain(
                    instance_extensions
                        .into_iter()
                        .chain(device_extensions.into_iter())
                        .map(ToOwned::to_owned),
                )
                .collect(),
            memory_type_manager,
            #[cfg(feature = "mt")]
            fence_reactor: FenceReactorThread::new(),
            #[cfg(feature = "debug")]
            _debug_instance,
        })
    }

    /// Submits any commands as transient commands.
    pub fn submit_commands(
        &mut self,
        generator: impl FnOnce(
            br::CmdRecord<br::CommandBufferObject<DeviceObject>>,
        ) -> br::CmdRecord<br::CommandBufferObject<DeviceObject>>,
    ) -> br::Result<()> {
        let mut cb = LocalCommandBundle(
            self.cp_onetime_submit.alloc(1, true)?,
            &mut self.cp_onetime_submit,
        );
        generator(unsafe { cb[0].begin_once()? }).end()?;
        self.graphics_queue.q.get_mut().submit(
            &[br::EmptySubmissionBatch.with_command_buffers(&cb[..])],
            None::<&mut br::FenceObject<DeviceObject>>,
        )?;
        self.graphics_queue.q.get_mut().wait()
    }
    pub fn submit_buffered_commands(
        &mut self,
        batches: &[impl br::SubmissionBatch],
        fence: &mut (impl br::Fence + br::VkHandleMut),
    ) -> br::Result<()> {
        self.graphics_queue.q.get_mut().submit(batches, Some(fence))
    }
    pub fn submit_buffered_commands_raw(
        &mut self,
        batches: &[br::vk::VkSubmitInfo],
        fence: &mut (impl br::Fence + br::VkHandleMut),
    ) -> br::Result<()> {
        self.graphics_queue
            .q
            .get_mut()
            .submit_raw(batches, Some(fence))
    }

    /// Submits any commands as transient commands.
    /// ## Note
    /// Unlike other futures, commands are submitted **immediately**(even if not awaiting the returned future).
    #[cfg(feature = "mt")]
    pub fn submit_commands_async<'s>(
        &'s self,
        generator: impl FnOnce(
            br::CmdRecord<br::CommandBufferObject<DeviceObject>>,
        ) -> br::CmdRecord<br::CommandBufferObject<DeviceObject>>,
    ) -> br::Result<impl std::future::Future<Output = br::Result<()>> + 's> {
        let mut fence = std::sync::Arc::new(br::FenceBuilder::new().create(self.device.clone())?);

        let mut pool = br::CommandPoolBuilder::new(self.graphics_queue_family_index())
            .transient()
            .create(self.device.clone())?;
        let mut cb = CommandBundle(pool.alloc(1, true)?, pool);
        generator(unsafe { cb[0].begin_once()? }).end()?;
        self.graphics_queue.q.lock().submit(
            &[br::EmptySubmissionBatch.with_command_buffers(&cb[..])],
            Some(unsafe { std::sync::Arc::get_mut(&mut fence).unwrap_unchecked() }),
        )?;

        Ok(async move {
            self.await_fence(fence).await?;

            // keep alive command buffers while execution
            drop(cb);

            Ok(())
        })
    }

    /// Awaits fence on background thread
    #[cfg(feature = "mt")]
    pub const fn await_fence<'s>(
        &'s self,
        fence: std::sync::Arc<
            impl br::Fence<ConcreteDevice = DeviceObject> + Send + Sync + 'static,
        >,
    ) -> impl std::future::Future<Output = br::Result<()>> + 's {
        FenceWaitFuture {
            reactor: &self.fence_reactor,
            object: fence,
            registered: false,
        }
    }

    pub fn instance(&self) -> &InstanceObject {
        self.device.instance()
    }

    pub const fn adapter(&self) -> &br::PhysicalDeviceObject<InstanceObject> {
        &self.adapter
    }

    pub const fn device(&self) -> &DeviceObject {
        &self.device
    }

    pub const fn graphics_queue_family_index(&self) -> u32 {
        self.graphics_queue.family
    }

    pub fn vk_extension_is_available(&self, name: &str) -> bool {
        self.enabled_vk_extensions.contains(name)
    }

    pub fn dedicated_allocation_available(&self) -> bool {
        self.vk_extension_is_available("VK_KHR_dedicated_allocation")
    }

    pub fn can_request_extended_memory_requirements(&self) -> bool {
        self.vk_extension_is_available("VK_KHR_get_memory_requirements2")
    }
}
/// Adapter Property exports
impl Graphics {
    pub fn adapter_available_features(&self) -> &br::vk::VkPhysicalDeviceFeatures {
        self.adapter_properties
            .available_features
            .get_or_init(|| self.adapter.features())
    }

    pub fn adapter_limits(&self) -> &br::vk::VkPhysicalDeviceLimits {
        &self
            .adapter_properties
            .properties
            .get_or_init(|| self.adapter.properties())
            .limits
    }

    pub fn adapter_memory_properties(&self) -> &br::MemoryProperties {
        self.adapter_properties
            .memory_properties
            .get_or_init(|| self.adapter.memory_properties())
    }
}
impl Deref for Graphics {
    type Target = DeviceObject;

    fn deref(&self) -> &DeviceObject {
        &self.device
    }
}

#[derive(Clone)]
pub struct MemoryType(u32, br::vk::VkMemoryType);
impl MemoryType {
    pub const fn index(&self) -> u32 {
        self.0
    }

    pub const fn corresponding_mask(&self) -> u32 {
        0x01 << self.0
    }

    pub const fn has_covered_by_mask(&self, mask: u32) -> bool {
        (mask & self.corresponding_mask()) != 0
    }

    pub const fn has_property_flags(&self, other: br::MemoryPropertyFlags) -> bool {
        (self.1.propertyFlags & other.bits()) != 0
    }

    pub const fn is_device_local(&self) -> bool {
        self.has_property_flags(br::MemoryPropertyFlags::DEVICE_LOCAL)
    }

    pub const fn visible_from_host(&self) -> bool {
        self.has_property_flags(br::MemoryPropertyFlags::HOST_VISIBLE)
    }

    pub const fn is_host_coherent(&self) -> bool {
        self.has_property_flags(br::MemoryPropertyFlags::HOST_COHERENT)
    }

    pub const fn is_host_cached(&self) -> bool {
        self.has_property_flags(br::MemoryPropertyFlags::HOST_CACHED)
    }
}
impl std::fmt::Debug for MemoryType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut flags = Vec::with_capacity(6);
        if self.is_device_local() {
            flags.push("DEVICE LOCAL");
        }
        if self.visible_from_host() {
            flags.push("HOST VISIBLE");
        }

        if self.is_host_cached() {
            flags.push("CACHED");
        }
        if self.is_host_coherent() {
            flags.push("COHERENT");
        }

        if (self.1.propertyFlags & br::vk::VK_MEMORY_PROPERTY_PROTECTED_BIT) != 0 {
            flags.push("PROTECTED");
        }
        if self.has_property_flags(br::MemoryPropertyFlags::LAZILY_ALLOCATED) {
            flags.push("LAZILY ALLOCATED");
        }

        write!(
            fmt,
            "{}: [{}] in heap #{}",
            self.index(),
            flags.join("/"),
            self.1.heapIndex
        )
    }
}

pub struct MemoryTypeManager {
    device_memory_types: Vec<MemoryType>,
    host_memory_types: Vec<MemoryType>,
}
impl MemoryTypeManager {
    fn new(pd: &impl br::PhysicalDevice) -> Self {
        let mem = pd.memory_properties();
        let (mut device_memory_types, mut host_memory_types) = (Vec::new(), Vec::new());
        for mt in mem
            .types()
            .enumerate()
            .map(|(n, mt)| MemoryType(n as _, mt.clone()))
        {
            if mt.is_device_local() {
                device_memory_types.push(mt.clone());
            }
            if mt.visible_from_host() {
                host_memory_types.push(mt.clone());
            }
        }

        Self {
            device_memory_types,
            host_memory_types,
        }
    }

    pub fn exact_host_visible_index(
        &self,
        mask: u32,
        required: br::MemoryPropertyFlags,
    ) -> Option<&MemoryType> {
        self.host_memory_types
            .iter()
            .find(|mt| mt.has_covered_by_mask(mask) && mt.has_property_flags(required))
    }

    pub fn host_visible_index(
        &self,
        mask: u32,
        preference: br::MemoryPropertyFlags,
    ) -> Option<&MemoryType> {
        self.exact_host_visible_index(mask, preference).or_else(|| {
            self.host_memory_types
                .iter()
                .find(|mt| mt.has_covered_by_mask(mask))
        })
    }

    pub fn device_local_index(&self, mask: u32) -> Option<&MemoryType> {
        self.device_memory_types
            .iter()
            .find(|mt| mt.has_covered_by_mask(mask))
    }

    fn diagnose_heaps(p: &impl br::PhysicalDevice) {
        info!("Memory Heaps: ");
        for (n, h) in p.memory_properties().heaps().enumerate() {
            let (mut nb, mut unit) = (h.size as f32, "bytes");
            if nb >= 10000.0 {
                nb /= 1024.0;
                unit = "KB";
            }
            if nb >= 10000.0 {
                nb /= 1024.0;
                unit = "MB";
            }
            if nb >= 10000.0 {
                nb /= 1024.0;
                unit = "GB";
            }
            let is_device_local = (h.flags & br::vk::VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) != 0;

            info!(
                "  #{n}: {nb} {unit} {}",
                if is_device_local {
                    "[DEVICE_LOCAL]"
                } else {
                    ""
                }
            );
        }
    }

    fn diagnose_types(&self) {
        info!("Device Memory Types: ");
        for mt in &self.device_memory_types {
            info!("  {:?}", mt);
        }
        info!("Host Visible Memory Types: ");
        for mt in &self.host_memory_types {
            info!("  {:?}", mt);
        }
    }
}
