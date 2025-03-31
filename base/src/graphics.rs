use crate::mthelper::SharedRef;
use bedrock::{self as br, CommandBufferMut, QueueMut};
use br::{Device, Instance, InstanceChild, PhysicalDevice, SubmissionBatch};
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

pub struct VulkanExtension<'s> {
    name: &'s core::ffi::CStr,
    promoted: Option<br::Version>,
}
impl<'s> VulkanExtension<'s> {
    pub const fn new(name: &'s core::ffi::CStr) -> Self {
        Self {
            name,
            promoted: None,
        }
    }

    pub const fn promoted(mut self, version: br::Version) -> Self {
        self.promoted = Some(version);
        self
    }

    pub const DEBUG_REPORT_EXT: Self = Self::new(c"VK_EXT_debug_report");
    pub const DEBUG_UTILS_EXT: Self = Self::new(c"VK_EXT_debug_utils");
    pub const GET_PHYSICAL_DEVICE_PROPERTIES2_KHR: Self =
        Self::new(c"VK_KHR_get_physical_device_properties2").promoted(br::Version::new(0, 1, 1, 0));
    pub const DEDICATED_ALLOCATION_KHR: Self =
        Self::new(c"VK_KHR_dedicated_allocation").promoted(br::Version::new(0, 1, 1, 0));
    pub const GET_MEMORY_REQUIREMENTS2_KHR: Self =
        Self::new(c"VK_KHR_get_memory_requirements2").promoted(br::Version::new(0, 1, 1, 0));
    pub const BIND_MEMORY2_KHR: Self =
        Self::new(c"VK_KHR_bind_memory2").promoted(br::Version::new(0, 1, 1, 0));
    pub const SYNCHRONIZATION2_KHR: Self =
        Self::new(c"VK_KHR_synchronization2").promoted(br::Version::new(0, 1, 3, 0));
    pub const CREATE_RENDERPASS2_KHR: Self =
        Self::new(c"VK_KHR_create_renderpass2").promoted(br::Version::new(0, 1, 2, 0));
    pub const MULTIVIEW_KHR: Self =
        Self::new(c"VK_KHR_multiview").promoted(br::Version::new(0, 1, 1, 0));
    pub const MAINTENANCE2_KHR: Self =
        Self::new(c"VK_KHR_maintenance2").promoted(br::Version::new(0, 1, 1, 0));
}

/// Graphics manager
pub struct Graphics {
    pub(crate) adapter: br::PhysicalDeviceObject<InstanceObject>,
    pub(crate) device: DeviceObject,
    pub(crate) graphics_queue: QueueSet<DeviceObject>,
    cp_onetime_submit: br::CommandPoolObject<DeviceObject>,
    pub memory_type_manager: MemoryTypeManager,
    vk_version: br::Version,
    enabled_vk_extensions: HashSet<CString>,
    adapter_properties: CachedAdapterProperties,
    #[cfg(feature = "mt")]
    fence_reactor: FenceReactorThread<DeviceObject>,
    #[cfg(feature = "debug")]
    _debug_instance: Option<br::DebugUtilsMessengerObject<InstanceObject>>,
}
impl Graphics {
    pub(crate) fn new(
        app_name: &str,
        app_version: br::Version,
        mut instance_extensions: Vec<&CStr>,
        mut device_extensions: Vec<&CStr>,
        features: br::vk::VkPhysicalDeviceFeatures,
    ) -> Self {
        let vk_version = br::instance_version().expect("Failed to get vulkan version");
        tracing::info!("System Vulkan Version: v{vk_version}");

        let mut optional_instance_extensions = Vec::new();
        let mut optional_device_extensions = Vec::new();

        #[cfg(feature = "debug")]
        optional_instance_extensions.extend([
            VulkanExtension::DEBUG_UTILS_EXT.name,
            VulkanExtension::DEBUG_REPORT_EXT.name,
        ]);

        if vk_version < br::Version::new(0, 1, 1, 0) {
            optional_instance_extensions
                .extend([VulkanExtension::GET_PHYSICAL_DEVICE_PROPERTIES2_KHR.name]);
            optional_device_extensions.extend([
                VulkanExtension::MULTIVIEW_KHR.name,
                VulkanExtension::MAINTENANCE2_KHR.name,
                VulkanExtension::DEDICATED_ALLOCATION_KHR.name,
                VulkanExtension::GET_MEMORY_REQUIREMENTS2_KHR.name,
                VulkanExtension::BIND_MEMORY2_KHR.name,
            ]);
        }
        if vk_version < br::Version::new(0, 1, 2, 0) {
            optional_device_extensions.push(VulkanExtension::CREATE_RENDERPASS2_KHR.name);
        }
        if vk_version < br::Version::new(0, 1, 3, 0) {
            optional_device_extensions.push(VulkanExtension::SYNCHRONIZATION2_KHR.name);
        }
        optional_instance_extensions.sort();
        optional_device_extensions.sort();

        let mut validation_layer_available = false;
        match br::instance_extension_properties_cstr_alloc(None) {
            Ok(xs) => {
                for x in xs {
                    let name_cstr = match x.extensionName.as_cstr() {
                        Ok(x) => x,
                        Err(e) => {
                            tracing::warn!({ cause = ?e }, "invalid extension name?");
                            continue;
                        }
                    };

                    tracing::info!(
                        target: "Peridot DeviceDiag",
                        name = ?name_cstr,
                        version = x.specVersion,
                        "Vk Instance Extension"
                    );
                    let _span = tracing::info_span!(
                        target: "Peridot DeviceDiag",
                        "Vk Instance Extension",
                        name = ?name_cstr,
                        version = x.specVersion
                    );
                    let _span = _span.enter();

                    if let Ok(n) = optional_instance_extensions.binary_search(&name_cstr) {
                        // available
                        instance_extensions.push(optional_instance_extensions[n]);
                    }
                }
            }
            Err(e) => {
                tracing::warn!({ cause = ?e }, "Failed to enumerate vk instance extensions");
            }
        }

        match br::enumerate_layer_properties_alloc() {
            Ok(xs) => {
                for l in xs {
                    let name_cstr = match l.layerName.as_cstr() {
                        Ok(x) => x,
                        Err(_) => {
                            tracing::warn!("layer name contains nul byte?");
                            continue;
                        }
                    };

                    tracing::info!(
                        target: "Peridot DeviceDiag",
                        name = ?name_cstr,
                        spec_version = l.specVersion,
                        impl_version = l.implementationVersion,
                        "Vk Instance Layer"
                    );
                    let _span = tracing::info_span!(
                        target: "Peridot DeviceDiag",
                        "Vk Instance Layer",
                        name = ?name_cstr,
                        spec_version = l.specVersion,
                        impl_version = l.implementationVersion
                    );
                    let _span = _span.enter();

                    #[cfg(debug_assertions)]
                    if name_cstr == c"VK_LAYER_KHRONOS_validation" {
                        validation_layer_available = true;
                    }

                    match br::instance_extension_properties_cstr_alloc(Some(name_cstr)) {
                        Ok(xs) => {
                            for x in xs {
                                let ext_name_cstr = match x.extensionName.as_cstr() {
                                    Ok(x) => x,
                                    Err(e) => {
                                        tracing::warn!({ cause = ?e }, "invalid extension name?");
                                        continue;
                                    }
                                };

                                tracing::info!(
                                    target: "Peridot DeviceDiag",
                                    name = ?ext_name_cstr,
                                    version = x.specVersion,
                                    "Vk Instance Layer Extension"
                                );
                                let _span = tracing::info_span!(
                                    target: "Peridot DeviceDiag",
                                    "Vk Instance Layer Extension",
                                    layer_name = ?name_cstr,
                                    name = ?ext_name_cstr,
                                    version = x.specVersion
                                );
                                let _span = _span.enter();

                                if let Ok(n) =
                                    optional_instance_extensions.binary_search(&ext_name_cstr)
                                {
                                    // available
                                    instance_extensions.push(optional_instance_extensions[n]);
                                }
                            }
                        }
                        Err(e) => {
                            tracing::warn!({ cause = ?e }, "Failed to enumerate vk instance extensions");
                        }
                    }
                }
            }
            Err(e) => {
                tracing::warn!({ cause = ?e }, "Failed to enumerate vk instance layers");
            }
        }

        if !validation_layer_available {
            tracing::warn!("Validation Layer is not found!");
        }

        let app_name = CString::new(app_name).expect("invalid sequence in app name");
        let app = br::ApplicationInfo::new(
            &app_name,
            app_version,
            c"Interlude2:Peridot",
            br::Version::new(0, 0, 1, 0),
        )
        .api_version(vk_version);

        #[allow(unused_mut)]
        let mut instance_layers = Vec::new();
        #[cfg(feature = "debug")]
        {
            if validation_layer_available {
                instance_layers.push(c"VK_LAYER_KHRONOS_validation".into());
            }

            tracing::debug!("Debug reporting activated!");
        }

        tracing::debug!(?instance_layers, ?instance_extensions, "VkInstance setup");

        let instance = SharedRef::new(
            br::InstanceObject::new(&br::InstanceCreateInfo::new(
                &app,
                &instance_layers,
                &instance_extensions
                    .iter()
                    .map(|&x| x.into())
                    .collect::<Vec<_>>(),
            ))
            .expect("Failed to create vk instance"),
        );

        #[cfg(feature = "debug")]
        let _debug_instance = match br::DebugUtilsMessengerObject::new(
            instance.clone(),
            &br::DebugUtilsMessengerCreateInfo::new(
                br::vk::VK_DEBUG_UTILS_MESSAGE_SEVERITY_ERROR_BIT_EXT
                    | br::vk::VK_DEBUG_UTILS_MESSAGE_SEVERITY_WARNING_BIT_EXT,
                br::vk::VK_DEBUG_UTILS_MESSAGE_TYPE_GENERAL_BIT_EXT
                    | br::vk::VK_DEBUG_UTILS_MESSAGE_TYPE_VALIDATION_BIT_EXT
                    | br::vk::VK_DEBUG_UTILS_MESSAGE_TYPE_PERFORMANCE_BIT_EXT,
                crate::debug::debug_utils_callback,
            ),
        ) {
            Ok(x) => Some(x),
            Err(e) => {
                tracing::error!(
                    { cause = ?e },
                    "Failed to create vk debug instance. Vulkan debug logs will be unavailable."
                );
                None
            }
        };

        let Some(adapter) = instance
            .iter_physical_devices()
            .expect("Failed to enumerate physical devices")
            .next()
        else {
            tracing::error!("No physical devices available");
            panic!("Engine unrecoverable");
        };

        match adapter.enumerate_extension_properties(None) {
            Ok(xs) => {
                for d in xs {
                    let name_cstr = match d.extensionName.as_cstr() {
                        Ok(x) => x,
                        Err(_) => {
                            tracing::warn!("extension name contains nul byte?");
                            continue;
                        }
                    };

                    tracing::info!(
                        target: "Peridot DeviceDiag",
                        name = ?name_cstr,
                        version = d.specVersion,
                        "Vk Device Extension"
                    );
                    let _span = tracing::info_span!(
                        target: "Peridot DeviceDiag",
                        "Vk Device Extension",
                        name = ?name_cstr,
                        version = d.specVersion
                    );
                    let _span = _span.enter();

                    if let Ok(n) = optional_device_extensions.binary_search(&name_cstr) {
                        // available!
                        device_extensions.push(optional_device_extensions[n]);
                    }
                }
            }
            Err(e) => {
                tracing::warn!({ cause = ?e }, "Failed to enumerate vk device extensions");
            }
        }

        let memory_type_manager = MemoryTypeManager::new(&adapter);
        MemoryTypeManager::diagnose_heaps(&adapter);
        memory_type_manager.diagnose_types();
        let Some(gqf_index) = adapter
            .queue_family_properties_alloc()
            .find_matching_index(br::QueueFlags::GRAPHICS)
        else {
            tracing::error!("No suitable queue(graphics) found on device");
            panic!("Engine unrecoverable");
        };

        let mut device_layers = Vec::new();
        if validation_layer_available {
            device_layers.push(c"VK_LAYER_KHRONOS_validation".into());
        }

        tracing::debug!(?device_layers, ?device_extensions, "VkDevice setup");

        enum Features<'r> {
            Standard(br::vk::VkPhysicalDeviceFeatures),
            Extendable(br::PhysicalDeviceFeatures2<'r>),
        }
        let mut sync2 = if vk_version >= br::Version::new(0, 1, 3, 0)
            || device_extensions.contains(&VulkanExtension::SYNCHRONIZATION2_KHR.name)
        {
            Some(br::PhysicalDeviceSynchronization2Features::new(true))
        } else {
            None
        };
        let features = if vk_version >= br::Version::new(0, 1, 1, 0)
            || instance_extensions
                .contains(&VulkanExtension::GET_PHYSICAL_DEVICE_PROPERTIES2_KHR.name)
        {
            // use extendable
            let ext_base = br::PhysicalDeviceFeatures2::new(features);
            let ext_base = if let Some(ref mut s2) = sync2 {
                ext_base.with_next(s2)
            } else {
                ext_base
            };

            Features::Extendable(ext_base)
        } else {
            // use standard and no extra features available
            Features::Standard(features)
        };

        let device_queues = [br::DeviceQueueCreateInfo::new(gqf_index, &[0.0])];
        let device_cinfo_extensions = device_extensions
            .iter()
            .map(|&x| x.into())
            .collect::<Vec<_>>();
        let device_cinfo =
            br::DeviceCreateInfo::new(&device_queues, &device_layers, &device_cinfo_extensions);
        let device_cinfo = match features {
            Features::Standard(ref f) => device_cinfo.with_features(f),
            Features::Extendable(ref f) => device_cinfo.with_next(f),
        };

        let device = SharedRef::new(
            br::DeviceObject::new(&adapter, &device_cinfo)
                .expect("Failed to create vk device")
                .clone_parent(),
        );

        Self {
            cp_onetime_submit: br::CommandPoolObject::new(
                device.clone(),
                &br::CommandPoolCreateInfo::new(gqf_index).transient(),
            )
            .expect("Failed to create onetime submit command pool"),
            graphics_queue: QueueSet {
                q: parking_lot::Mutex::new(device.queue(gqf_index, 0).clone_parent()),
                family: gqf_index,
            },
            adapter: adapter.clone_parent(),
            device,
            adapter_properties: CachedAdapterProperties::new(),
            vk_version,
            enabled_vk_extensions: instance_extensions
                .into_iter()
                .chain(device_extensions.into_iter())
                .map(ToOwned::to_owned)
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
        generator: impl FnOnce(br::CmdRecord<DeviceObject>) -> br::CmdRecord<DeviceObject>,
    ) -> br::Result<()> {
        let mut cb = LocalCommandBundle(
            br::CommandBufferObject::alloc(
                self.device.clone(),
                &br::CommandBufferAllocateInfo::new(
                    &mut self.cp_onetime_submit,
                    1,
                    br::CommandBufferLevel::Primary,
                ),
            )?,
            &mut self.cp_onetime_submit,
        );
        generator(unsafe {
            cb[0].begin(
                &br::CommandBufferBeginInfo::new().onetime_submit(),
                &self.device,
            )?
        })
        .end()?;
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
            .submit(batches, Some(fence.as_transparent_ref_mut()))
    }
    pub fn submit_buffered_commands_raw(
        &mut self,
        batches: &[br::vk::VkSubmitInfo],
        fence: &mut impl br::FenceMut,
    ) -> br::Result<()> {
        unsafe {
            self.graphics_queue
                .q
                .get_mut()
                .submit_raw(batches, Some(fence.as_transparent_ref_mut()))
        }
    }

    /// Submits any commands as transient commands.
    /// ## Note
    /// Unlike other futures, commands are submitted **immediately**(even if not awaiting the returned future).
    #[cfg(feature = "mt")]
    pub fn submit_commands_async<'s>(
        &'s self,
        generator: impl FnOnce(br::CmdRecord<DeviceObject>) -> br::CmdRecord<DeviceObject>,
    ) -> br::Result<impl std::future::Future<Output = br::Result<()>> + 's> {
        use bedrock::VkHandleMut;

        let mut fence = std::sync::Arc::new(br::FenceObject::new(
            self.device().clone(),
            &br::FenceCreateInfo::new(0),
        )?);

        let mut pool = br::CommandPoolObject::new(
            self.device.clone(),
            &br::CommandPoolCreateInfo::new(self.graphics_queue_family_index()).transient(),
        )?;
        let mut cb = CommandBundle(
            br::CommandBufferObject::alloc(
                self.device.clone(),
                &br::CommandBufferAllocateInfo::new(&mut pool, 1, br::CommandBufferLevel::Primary),
            )?,
            pool,
        );
        generator(unsafe {
            cb[0].begin(
                &br::CommandBufferBeginInfo::new().onetime_submit(),
                &self.device,
            )?
        })
        .end()?;
        self.graphics_queue.q.lock().submit(
            &[br::EmptySubmissionBatch.with_command_buffers(&cb[..])],
            Some(unsafe {
                std::sync::Arc::get_mut(&mut fence)
                    .unwrap_unchecked()
                    .as_transparent_ref_mut()
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

    #[inline(always)]
    pub fn is_extension_available(&self, ext: &VulkanExtension) -> bool {
        ext.promoted.is_some_and(|x| self.vk_version >= x)
            || self.enabled_vk_extensions.contains(ext.name)
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
            if nb >= 10000.0 {
                nb /= 1024.0;
                unit = "TB";
            }
            let is_device_local = (h.flags & br::vk::VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) != 0;

            tracing::info!(
                target: "Peridot MemDiag (Heap)",
                index = n,
                is_device_local,
                "{nb} {unit}",
            );
        }
    }

    fn diagnose_types(&self) {
        for mt in &self.device_memory_types {
            tracing::info!(target: "Peridot MemDiag (Type)", ?mt, "Device Memory Type");
        }
        for mt in &self.host_memory_types {
            tracing::info!(target: "Peridot MemDiag (Type)", ?mt, "Host Visible Memory Type");
        }
    }
}
