use crate::mthelper::SharedRef;
use bedrock::{self as br, CommandBufferMut, CommandPoolMut, QueueMut};
use br::{Device, Instance, InstanceChild, PhysicalDevice, SubmissionBatch};
use cfg_if::cfg_if;
use log::{info, warn};
use std::{
    collections::HashSet,
    ffi::{CStr, CString},
    ops::Deref,
};

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

#[cfg(not(feature = "mt"))]
use std::cell::OnceCell as OnceValue;
#[cfg(feature = "mt")]
use std::sync::OnceLock as OnceValue;

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
    enabled_vk_extensions: HashSet<CString>,
    adapter_properties: CachedAdapterProperties,
    #[cfg(feature = "mt")]
    fence_reactor: FenceReactorThread<DeviceObject>,
    #[cfg(feature = "debug")]
    _debug_instance: br::DebugUtilsMessengerObject<InstanceObject>,
}
impl Graphics {
    pub(crate) fn new(
        app_name: &str,
        app_version: (u16, u16, u16),
        instance_extensions: Vec<&CStr>,
        device_extensions: Vec<&CStr>,
        features: br::vk::VkPhysicalDeviceFeatures,
    ) -> Self {
        let mut validation_layer_available = false;
        match br::enumerate_layer_properties() {
            Ok(xs) => {
                info!("Supported Layers: ");

                for l in xs {
                    let name_cstr = match l.layerName.as_cstr() {
                        Ok(x) => x,
                        Err(_) => {
                            warn!("layer name contains nul byte?");
                            continue;
                        }
                    };
                    let name_str = match name_cstr.to_str() {
                        Ok(x) => x,
                        Err(e) => {
                            warn!("invalid sequence in layer name: {e:?}");
                            continue;
                        }
                    };

                    info!(
                        "* {name_str} :: {}/{}",
                        l.specVersion, l.implementationVersion
                    );

                    #[cfg(debug_assertions)]
                    if name_str == "VK_LAYER_KHRONOS_validation" {
                        validation_layer_available = true;
                    }
                }
            }
            Err(e) => {
                warn!("Failed to enumerate vk instance layers: {e:?}");
            }
        }

        if !validation_layer_available {
            warn!("Validation Layer is not found!");
        }

        let app_name = CString::new(app_name).expect("invalid sequence in app name");
        let app =
            br::ApplicationInfo::new(&app_name, app_version, c"Interluse2:Peridot", (0, 1, 0));
        let mut ib = br::InstanceBuilder::new(&app);
        ib.add_extensions(instance_extensions.iter().copied());
        #[cfg(feature = "debug")]
        {
            ib.add_extension(c"VK_EXT_debug_report");
            ib.add_extension(c"VK_EXT_debug_utils");
            if validation_layer_available {
                ib.add_layer(c"VK_LAYER_KHRONOS_validation");
            }

            log::debug!("Debug reporting activated");
        }
        let instance = SharedRef::new(ib.create().expect("Failed to create vk instance"));

        #[cfg(feature = "debug")]
        let _debug_instance = br::DebugUtilsMessengerCreateInfo::new(crate::debug::debug_utils_out)
            .filter_severity(br::DebugUtilsMessageSeverityFlags::ERROR.and_warning())
            .create(instance.clone())
            .expect("Failed to create vk debug instance");

        let Some(adapter) = instance
            .iter_physical_devices()
            .expect("Failed to enumerate physical devices")
            .next()
        else {
            log::error!("No physical devices available");
            panic!("Engine unrecoverable");
        };

        let optional_device_features = [
            "VK_KHR_dedicated_allocation",
            "VK_KHR_get_memory_requirements2",
            "VK_KHR_bind_memory2",
        ];

        let mut auto_device_extensions = Vec::new();
        match adapter.enumerate_extension_properties(None) {
            Ok(xs) => {
                info!("Device Extensions: ");

                for d in xs {
                    let name_cstr = match d.extensionName.as_cstr() {
                        Ok(x) => x,
                        Err(_) => {
                            warn!("extension name contains nul byte?");
                            continue;
                        }
                    };
                    let name = match name_cstr.to_str() {
                        Ok(x) => x,
                        Err(e) => {
                            warn!("invalid sequence in extension name: {e:?}");
                            continue;
                        }
                    };

                    info!("* {name}: {}", d.specVersion);

                    if optional_device_features.contains(&name) {
                        auto_device_extensions.push(name_cstr.to_owned());
                    }
                }
            }
            Err(e) => {
                warn!("Failed to enumerate vk device extensions: {e:?}");
            }
        }

        let memory_type_manager = MemoryTypeManager::new(&adapter);
        MemoryTypeManager::diagnose_heaps(&adapter);
        memory_type_manager.diagnose_types();
        let Some(gqf_index) = adapter
            .queue_family_properties()
            .find_matching_index(br::QueueFlags::GRAPHICS)
        else {
            log::error!("No suitable queue(graphics) found on device");
            panic!("Engine unrecoverable");
        };
        let device = {
            let mut db = br::DeviceBuilder::new(&adapter);
            db.add_extensions(device_extensions.iter().copied())
                .add_extensions(auto_device_extensions.iter().map(|x| x as _))
                .add_queue(br::DeviceQueueCreateInfo::new(gqf_index, &[0.0]));
            if validation_layer_available {
                db.add_layer(c"VK_LAYER_KHRONOS_validation");
            }
            *db.mod_features() = features;
            SharedRef::new(
                db.create()
                    .expect("Failed to create vk device")
                    .clone_parent(),
            )
        };

        Self {
            cp_onetime_submit: br::CommandPoolBuilder::new(gqf_index)
                .transient()
                .create(device.clone())
                .expect("Failed to create onetime submit command pool"),
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
        }
    }

    /// Submits any commands as transient commands.
    pub fn submit_commands(
        &mut self,
        generator: impl FnOnce(
            br::CmdRecord<br::CommandBufferObject<DeviceObject>, DeviceObject>,
        )
            -> br::CmdRecord<br::CommandBufferObject<DeviceObject>, DeviceObject>,
    ) -> br::Result<()> {
        let mut cb = LocalCommandBundle(
            self.cp_onetime_submit.alloc(1, true)?,
            &mut self.cp_onetime_submit,
        );
        generator(unsafe { cb[0].begin_once(&self.device)? }).end()?;
        self.graphics_queue.q.get_mut().submit(
            &[br::EmptySubmissionBatch.with_command_buffers(&cb[..])],
            None,
        )?;
        self.graphics_queue.q.get_mut().wait()
    }
    pub fn submit_buffered_commands(
        &mut self,
        batches: &[impl br::SubmissionBatch],
        fence: &mut impl br::FenceMut,
    ) -> br::Result<()> {
        self.graphics_queue
            .q
            .get_mut()
            .submit(batches, Some(fence.as_transparent_mut_ref()))
    }
    pub fn submit_buffered_commands_raw(
        &mut self,
        batches: &[br::vk::VkSubmitInfo],
        fence: &mut impl br::FenceMut,
    ) -> br::Result<()> {
        self.graphics_queue
            .q
            .get_mut()
            .submit_raw(batches, Some(fence.as_transparent_mut_ref()))
    }

    /// Submits any commands as transient commands.
    /// ## Note
    /// Unlike other futures, commands are submitted **immediately**(even if not awaiting the returned future).
    #[cfg(feature = "mt")]
    pub fn submit_commands_async<'s>(
        &'s self,
        generator: impl FnOnce(
            br::CmdRecord<br::CommandBufferObject<DeviceObject>, DeviceObject>,
        )
            -> br::CmdRecord<br::CommandBufferObject<DeviceObject>, DeviceObject>,
    ) -> br::Result<impl std::future::Future<Output = br::Result<()>> + 's> {
        use bedrock::FenceMut;

        let mut fence = std::sync::Arc::new(br::FenceBuilder::new().create(self.device.clone())?);

        let mut pool = br::CommandPoolBuilder::new(self.graphics_queue_family_index())
            .transient()
            .create(self.device.clone())?;
        let mut cb = CommandBundle(pool.alloc(1, true)?, pool);
        generator(unsafe { cb[0].begin_once(&self.device)? }).end()?;
        self.graphics_queue.q.lock().submit(
            &[br::EmptySubmissionBatch.with_command_buffers(&cb[..])],
            Some(unsafe {
                std::sync::Arc::get_mut(&mut fence)
                    .unwrap_unchecked()
                    .as_transparent_mut_ref()
            }),
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
            impl br::Fence + br::DeviceChild<ConcreteDevice = DeviceObject> + Send + Sync + 'static,
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

    pub fn vk_extension_is_available(&self, name: &CStr) -> bool {
        self.enabled_vk_extensions.contains(name)
    }

    pub fn dedicated_allocation_available(&self) -> bool {
        self.vk_extension_is_available(c"VK_KHR_dedicated_allocation")
    }

    pub fn can_request_extended_memory_requirements(&self) -> bool {
        self.vk_extension_is_available(c"VK_KHR_get_memory_requirements2")
    }

    pub fn extended_memory_binding_available(&self) -> bool {
        self.vk_extension_is_available(c"VK_KHR_bind_memory2")
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
            .iter()
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
        for (n, h) in p.memory_properties().heaps().iter().enumerate() {
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
