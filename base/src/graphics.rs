use crate::mthelper::{SharedRef, SharedWeakRef};
use bedrock::{self as br, ResolverInterface};
use br::{Instance, PhysicalDevice};
use std::{
    collections::HashSet,
    ffi::{CStr, CString},
};

pub type InstanceObject = SharedRef<br::InstanceObject>;
pub type DeviceObject = SharedRef<br::DeviceObject<InstanceObject>>;

/// Queue object with family index
pub struct QueueSet {
    pub(crate) q: br::vk::VkQueue,
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
    pub available_features: OnceValue<br::PhysicalDeviceFeatures>,
    pub properties: OnceValue<br::PhysicalDeviceProperties>,
    pub memory_properties: OnceValue<br::PhysicalDeviceMemoryProperties>,
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

    #[inline(always)]
    pub fn is_promoted(&self, runtime_version: &br::Version) -> bool {
        self.promoted.as_ref().is_some_and(|v| runtime_version >= v)
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

pub(crate) struct VulkanGfxInner {
    pub(crate) instance: br::vk::VkInstance,
    pub(crate) adapter: br::vk::VkPhysicalDevice,
    pub(crate) device: br::vk::VkDevice,
    pub(crate) graphics_queue_family_index: u32,
    pub(crate) memory_type_manager: MemoryTypeManager,
    vk_version: br::Version,
    enabled_extension_names: HashSet<CString>,
    cached_adapter_properties: CachedAdapterProperties,
    #[cfg(feature = "debug")]
    debug_instance: Option<(
        br::vk::VkDebugUtilsMessengerEXT,
        br::vk::PFN_vkDestroyDebugUtilsMessengerEXT,
    )>,
    #[cfg(feature = "debug")]
    set_object_name_fn: Option<br::vk::PFN_vkSetDebugUtilsObjectNameEXT>,
    get_buffer_memory_requirements2_fn: OnceValue<br::vk::PFN_vkGetBufferMemoryRequirements2KHR>,
    get_image_memory_requirements2_fn: OnceValue<br::vk::PFN_vkGetImageMemoryRequirements2KHR>,
    bind_buffer_memory2_fn: OnceValue<br::vk::PFN_vkBindBufferMemory2KHR>,
    bind_image_memory2_fn: OnceValue<br::vk::PFN_vkBindImageMemory2KHR>,
}
unsafe impl Sync for VulkanGfxInner {}
unsafe impl Send for VulkanGfxInner {}
impl Drop for VulkanGfxInner {
    fn drop(&mut self) {
        unsafe {
            br::vkfn::destroy_device(self.device, core::ptr::null());
            #[cfg(feature = "debug")]
            if let Some((inst, destroy_fn)) = self.debug_instance.take() {
                (destroy_fn.0)(self.instance, inst, core::ptr::null());
            }
            br::vkfn::destroy_instance(self.instance, core::ptr::null());
        }
    }
}
#[repr(transparent)]
#[derive(Clone)]
pub struct VulkanGfx(pub(crate) SharedRef<VulkanGfxInner>);
impl VulkanGfx {
    const ENGINE_NAME: &'static core::ffi::CStr = c"Interlude2:Peridot";
    const ENGINE_VERSION: br::Version = br::Version::new(0, 0, 1, 0);

    pub(crate) fn new(
        app_name: &str,
        app_version: br::Version,
        mut instance_extensions: Vec<&CStr>,
        mut device_extensions: Vec<&CStr>,
        features: br::vk::VkPhysicalDeviceFeatures,
    ) -> Self {
        let vk_version = match br::instance_version() {
            Ok(x) => x,
            Err(e) => {
                tracing::warn!(cause = ?e, "Failed to get vulkan version. falling back to v1.0.0");
                br::Version::new(0, 1, 0, 0)
            }
        };
        tracing::info!("System Vulkan Version: v{vk_version}");

        let mut optional_instance_extensions = Vec::new();
        let mut optional_device_extensions = Vec::new();

        #[cfg(feature = "debug")]
        optional_instance_extensions.extend([
            VulkanExtension::DEBUG_UTILS_EXT.name,
            VulkanExtension::DEBUG_REPORT_EXT.name, // 古い環境向け
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
                            tracing::warn!(cause = ?e, "invalid extension name?");
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
                tracing::warn!(cause = ?e, "Failed to enumerate vk instance extensions");
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
                                        tracing::warn!(cause = ?e, "invalid extension name?");
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
                            tracing::warn!(cause = ?e, "Failed to enumerate vk instance extensions");
                        }
                    }
                }
            }
            Err(e) => {
                tracing::warn!(cause = ?e, "Failed to enumerate vk instance layers");
            }
        }

        if !validation_layer_available {
            tracing::warn!("Vulkan Validation Layer is not found");
        }

        let app_name = CString::new(app_name).expect("Invalid sequence in app name");
        let app = br::ApplicationInfo::new(
            &app_name,
            app_version,
            Self::ENGINE_NAME,
            Self::ENGINE_VERSION,
        )
        .api_version(vk_version);

        #[allow(unused_mut)]
        let mut instance_layers = Vec::new();
        #[cfg(feature = "debug")]
        if validation_layer_available {
            instance_layers.push(c"VK_LAYER_KHRONOS_validation".into());
            tracing::debug!("Debug reporting activated");
        }

        tracing::debug!(?instance_layers, ?instance_extensions, "VkInstance setup");
        let instance = match br::InstanceObject::new(&br::InstanceCreateInfo::new(
            &app,
            &instance_layers,
            &instance_extensions
                .iter()
                .map(|&x| x.into())
                .collect::<Vec<_>>(),
        )) {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to create VkInstance");
                std::process::abort();
            }
        };

        #[cfg(feature = "debug")]
        let debug_instance = match br::DebugUtilsMessengerObject::new(
            &instance,
            &br::DebugUtilsMessengerCreateInfo::new(
                br::DebugUtilsMessageSeverityFlags::ERROR
                    | br::DebugUtilsMessageSeverityFlags::WARNING,
                br::DebugUtilsMessageTypeFlags::GENERAL
                    | br::DebugUtilsMessageTypeFlags::VALIDATION
                    | br::DebugUtilsMessageTypeFlags::PERFORMANCE,
                crate::debug::debug_utils_callback,
            ),
        ) {
            Ok(x) => Some(x),
            Err(e) => {
                tracing::warn!(
                    cause = ?e,
                    "Failed to create Vulkan debug instance. Vulkan debug logs will not be logged"
                );
                None
            }
        };

        let adapter = match instance.iter_physical_devices() {
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to enumerate physical devices");
                std::process::abort();
            }
            Ok(mut xs) => match xs.next() {
                None => {
                    tracing::error!("No available physical devices found");
                    std::process::abort();
                }
                Some(x) => x,
            },
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
                tracing::warn!(cause = ?e, "Failed to enumerate vk device extensions");
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
            std::process::abort();
        };

        #[allow(unused_mut)]
        let mut device_layers = Vec::new();
        #[cfg(feature = "debug")]
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

        let device =
            br::DeviceObject::new(&adapter, &device_cinfo).expect("Failed to create vk device");

        let enabled_extension_names = instance_extensions
            .into_iter()
            .chain(device_extensions)
            .map(ToOwned::to_owned)
            .collect::<HashSet<_>>();

        #[cfg(feature = "debug")]
        let set_object_name_fn =
            if enabled_extension_names.contains(VulkanExtension::DEBUG_UTILS_EXT.name) {
                Some(unsafe {
                    br::VkHandle::native_ptr(&instance)
                        .load_function_unconstrainted::<br::vk::PFN_vkSetDebugUtilsObjectNameEXT>()
                })
            } else {
                None
            };

        Self(SharedRef::new(VulkanGfxInner {
            #[cfg(feature = "debug")]
            debug_instance: debug_instance.map(|x| (
                x.unmanage().0,
                unsafe {
                    br::VkHandle::native_ptr(&instance)
                        .load_function_unconstrainted::<br::vk::PFN_vkDestroyDebugUtilsMessengerEXT>()
                }
            )),
            #[cfg(feature = "debug")]
            set_object_name_fn,
            enabled_extension_names,
            vk_version,
            cached_adapter_properties: CachedAdapterProperties::new(),
            graphics_queue_family_index: gqf_index,
            memory_type_manager,
            adapter: adapter.unmanage().0,
            device: device.unmanage().0,
            instance: instance.unmanage(),
            get_buffer_memory_requirements2_fn: OnceValue::new(),
            get_image_memory_requirements2_fn: OnceValue::new(),
            bind_buffer_memory2_fn: OnceValue::new(),
            bind_image_memory2_fn: OnceValue::new(),
        }))
    }

    #[inline]
    pub fn downgrade(&self) -> VulkanGfxWeak {
        VulkanGfxWeak(SharedRef::downgrade(&self.0))
    }

    pub fn adapter_available_features(&self) -> &br::PhysicalDeviceFeatures {
        self.0
            .cached_adapter_properties
            .available_features
            .get_or_init(|| {
                let mut h = core::mem::MaybeUninit::uninit();

                unsafe {
                    br::vkfn_wrapper::get_physical_device_features(self.0.adapter, &mut h);
                    h.assume_init()
                }
            })
    }

    pub fn adapter_limits(&self) -> &br::vk::VkPhysicalDeviceLimits {
        &self
            .0
            .cached_adapter_properties
            .properties
            .get_or_init(|| {
                let mut h = core::mem::MaybeUninit::uninit();

                unsafe {
                    br::vkfn_wrapper::get_physical_device_properties(self.0.adapter, &mut h);
                    h.assume_init()
                }
            })
            .limits
    }

    pub fn adapter_memory_properties(&self) -> &br::PhysicalDeviceMemoryProperties {
        self.0
            .cached_adapter_properties
            .memory_properties
            .get_or_init(|| {
                let mut h = core::mem::MaybeUninit::uninit();

                unsafe {
                    br::vkfn_wrapper::get_physical_device_memory_properties(self.0.adapter, &mut h);
                    h.assume_init()
                }
            })
    }

    pub fn surface_support(
        &self,
        surface: &(impl br::VkHandle<Handle = br::vk::VkSurfaceKHR> + ?Sized),
    ) -> br::Result<bool> {
        unsafe {
            br::vkfn_wrapper::get_physical_device_surface_support(
                self.0.adapter,
                self.0.graphics_queue_family_index,
                surface.native_ptr(),
            )
        }
    }

    pub fn surface_capabilities(
        &self,
        surface: &(impl br::VkHandle<Handle = br::vk::VkSurfaceKHR> + ?Sized),
    ) -> br::Result<br::SurfaceCapabilities> {
        let mut sink = core::mem::MaybeUninit::uninit();
        unsafe {
            br::vkfn_wrapper::get_physical_device_surface_capabilities(
                self.0.adapter,
                surface.native_ptr(),
                &mut sink,
            )?;
        }

        Ok(unsafe { sink.assume_init() })
    }

    pub fn surface_formats(
        &self,
        surface: &(impl br::VkHandle<Handle = br::vk::VkSurfaceKHR> + ?Sized),
    ) -> br::Result<Vec<br::SurfaceFormat>> {
        let x = unsafe {
            br::vkfn_wrapper::get_physical_device_surface_format_count(
                self.0.adapter,
                surface.native_ptr(),
            )?
        };
        if x == 0 {
            // no items
            return Ok(Vec::new());
        }

        let mut sink = Vec::with_capacity(x as _);
        unsafe {
            br::vkfn_wrapper::get_physical_device_surface_formats(
                self.0.adapter,
                surface.native_ptr(),
                sink.spare_capacity_mut(),
            )?;
            sink.set_len(sink.capacity());
        }

        Ok(sink)
    }

    pub fn surface_present_modes(
        &self,
        surface: &(impl br::VkHandle<Handle = br::vk::VkSurfaceKHR> + ?Sized),
    ) -> br::Result<Vec<br::PresentMode>> {
        let x = unsafe {
            br::vkfn_wrapper::get_physical_device_surface_present_mode_count(
                self.0.adapter,
                surface.native_ptr(),
            )?
        };
        if x == 0 {
            // no items
            return Ok(Vec::new());
        }

        let mut sink = Vec::with_capacity(x as _);
        unsafe {
            br::vkfn_wrapper::get_physical_device_surface_present_modes(
                self.0.adapter,
                surface.native_ptr(),
                sink.spare_capacity_mut(),
            )?;
            sink.set_len(sink.capacity());
        }

        Ok(sink)
    }

    pub fn device_local_memory_index(&self, index_mask: u32) -> Option<u32> {
        self.0
            .memory_type_manager
            .device_local_index(index_mask)
            .map(|x| x.index())
    }

    /// Sets an object's name for debugging.
    ///
    /// On failure, this function logs a warning and does not bail.
    #[cfg(feature = "debug")]
    #[tracing::instrument(
        name = "VulkanGfx::dbg_set_object_name_raw",
        skip(self),
        fields(handle = handle.raw_handle_value())
    )]
    pub unsafe fn dbg_set_object_name_raw(
        &self,
        object_type: br::vk::VkObjectType,
        handle: &(impl br::VkRawHandle + ?Sized),
        name: &core::ffi::CStr,
    ) {
        if let Err(e) = self.set_object_name_raw(object_type, handle, name) {
            tracing::warn!(cause = ?e, "Failed to set an object's name for debugging");
        }
    }

    #[cfg(feature = "debug")]
    pub unsafe fn set_object_name_raw(
        &self,
        object_type: br::vk::VkObjectType,
        handle: &(impl br::VkRawHandle + ?Sized),
        name: &core::ffi::CStr,
    ) -> br::Result<()> {
        let Some(ref f) = self.0.set_object_name_fn else {
            return Ok(());
        };

        unsafe {
            (f.0)(
                self.0.device,
                &br::DebugUtilsObjectNameInfo::new_raw(
                    object_type,
                    handle.raw_handle_value(),
                    Some(name),
                ) as *const _ as _,
            )
            .into_result()
            .map(drop)
        }
    }

    /// Sets an object's name for debugging.
    ///
    /// On failure, this function logs a warning and does not bail.
    #[cfg(feature = "debug")]
    #[tracing::instrument(
        name = "VulkanGfx::dbg_set_object_name",
        skip(self),
        fields(object_type = H::TYPE, handle = br::VkRawHandle::raw_handle_value(&handle.native_ptr()))
    )]
    pub fn dbg_set_object_name<H>(&self, handle: &H, name: &core::ffi::CStr)
    where
        H: br::VkHandle<Handle: br::VkRawHandle> + br::VkObject + ?Sized,
    {
        if let Err(e) = self.set_object_name(handle, name) {
            tracing::warn!(cause = ?e, "Failed to set an object's name for debugging");
        }
    }

    #[cfg(feature = "debug")]
    pub fn set_object_name(
        &self,
        object: &(impl br::VkHandle<Handle: br::VkRawHandle> + br::VkObject + ?Sized),
        name: &core::ffi::CStr,
    ) -> br::Result<()> {
        let Some(ref f) = self.0.set_object_name_fn else {
            return Ok(());
        };

        unsafe {
            (f.0)(
                self.0.device,
                &br::DebugUtilsObjectNameInfo::new(object, Some(name)) as *const _ as _,
            )
            .into_result()
            .map(drop)
        }
    }

    #[inline]
    pub unsafe fn load_function<F: br::PFN>(&self) -> F {
        unsafe { self.0.device.load_function_unconstrainted::<F>() }
    }

    #[inline]
    pub fn get_buffer_memory_requirements2_fn(
        &self,
    ) -> &br::vk::PFN_vkGetBufferMemoryRequirements2KHR {
        self.0
            .get_buffer_memory_requirements2_fn
            .get_or_init(|| unsafe { self.load_function() })
    }

    #[inline]
    pub fn get_image_memory_requirements2_fn(
        &self,
    ) -> &br::vk::PFN_vkGetImageMemoryRequirements2KHR {
        self.0
            .get_image_memory_requirements2_fn
            .get_or_init(|| unsafe { self.load_function() })
    }

    #[inline]
    pub fn bind_buffer_memory2_fn(&self) -> &br::vk::PFN_vkBindBufferMemory2KHR {
        self.0
            .bind_buffer_memory2_fn
            .get_or_init(|| unsafe { self.load_function() })
    }

    #[inline]
    pub fn bind_image_memory2_fn(&self) -> &br::vk::PFN_vkBindImageMemory2KHR {
        self.0
            .bind_image_memory2_fn
            .get_or_init(|| unsafe { self.load_function() })
    }
}
impl br::VkHandle for VulkanGfx {
    type Handle = br::vk::VkDevice;

    fn native_ptr(&self) -> Self::Handle {
        self.0.device
    }
}
impl br::InstanceChild for VulkanGfx {
    type ConcreteInstance = VulkanGfxInstanceAccess;

    fn instance(&self) -> &Self::ConcreteInstance {
        unsafe { core::mem::transmute(self) }
    }
}
impl br::Device for VulkanGfx {}
impl br::DeviceBindMemory2Extension for VulkanGfx {
    fn bind_buffer_memory2_khr_fn(&self) -> bedrock::vk::PFN_vkBindBufferMemory2KHR {
        todo!("vkBindBufferMemory2KHR resolve")
    }

    fn bind_image_memory2_khr_fn(&self) -> bedrock::vk::PFN_vkBindImageMemory2KHR {
        todo!("vkBindImageMemory2KHR resolve")
    }
}
impl br::DeviceGetMemoryRequirements2Extension for VulkanGfx {
    fn get_buffer_memory_requirements_2_khr_fn(
        &self,
    ) -> br::vk::PFN_vkGetBufferMemoryRequirements2KHR {
        todo!("vkGetBufferMemoryRequirements2KHR resolve");
    }

    fn get_image_memory_requirements_2_khr_fn(
        &self,
    ) -> br::vk::PFN_vkGetImageMemoryRequirements2KHR {
        todo!("vkGetImageMemoryRequirements2KHR resolve");
    }

    fn get_image_sparse_memory_requirements_2_khr_fn(
        &self,
    ) -> br::vk::PFN_vkGetImageSparseMemoryRequirements2KHR {
        todo!("vkGetImageSparseMemoryRequirements2KHR resolve");
    }
}
impl br::DeviceSynchronization2Extension for VulkanGfx {
    fn cmd_pipeline_barrier_2_khr_fn(&self) -> bedrock::vk::PFN_vkCmdPipelineBarrier2KHR {
        todo!("vkCmdPipelineBarrier2KHR resolve")
    }

    fn queue_submit2_khr_fn(&self) -> bedrock::vk::PFN_vkQueueSubmit2KHR {
        todo!("vkQueueSubmit2KHR resolve")
    }
}

#[repr(transparent)]
pub struct VulkanGfxInstanceAccess(VulkanGfx);
impl br::VkHandle for VulkanGfxInstanceAccess {
    type Handle = br::vk::VkInstance;

    fn native_ptr(&self) -> Self::Handle {
        self.0 .0.instance
    }
}
impl br::Instance for VulkanGfxInstanceAccess {}

#[repr(transparent)]
pub struct VulkanGfxWeak(SharedWeakRef<VulkanGfxInner>);
impl VulkanGfxWeak {
    pub fn upgrade(&self) -> Option<VulkanGfx> {
        self.0.upgrade().map(VulkanGfx)
    }
}

struct LocalOnetimeSubmitCommandBuffer<'c> {
    buffer: br::vk::VkCommandBuffer,
    pool: &'c br::vk::VkCommandPool,
    device: &'c VulkanGfx,
}
unsafe impl Sync for LocalOnetimeSubmitCommandBuffer<'_> {}
unsafe impl Send for LocalOnetimeSubmitCommandBuffer<'_> {}
impl Drop for LocalOnetimeSubmitCommandBuffer<'_> {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::free_command_buffers(self.device.0.device, *self.pool, &[self.buffer])
        }
    }
}

/// Graphics manager
pub struct Graphics {
    pub(crate) gfx_device: VulkanGfx,
    pub(crate) graphics_queue: QueueSet,
    cp_onetime_submit: br::vk::VkCommandPool,
    #[cfg(feature = "mt")]
    fence_reactor: FenceReactorThread,
}
unsafe impl Sync for Graphics {}
unsafe impl Send for Graphics {}
impl Drop for Graphics {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_command_pool(
                self.gfx_device.0.device,
                self.cp_onetime_submit,
                None,
            );
        }
    }
}
impl Graphics {
    pub(crate) fn new(
        app_name: &str,
        app_version: br::Version,
        instance_extensions: Vec<&CStr>,
        device_extensions: Vec<&CStr>,
        features: br::vk::VkPhysicalDeviceFeatures,
    ) -> Self {
        let gfx_device = VulkanGfx::new(
            app_name,
            app_version,
            instance_extensions,
            device_extensions,
            features,
        );
        let graphics_queue = QueueSet {
            q: unsafe {
                br::vkfn_wrapper::get_device_queue(
                    gfx_device.0.device,
                    gfx_device.0.graphics_queue_family_index,
                    0,
                )
            },
            family: gfx_device.0.graphics_queue_family_index,
        };
        let cp_onetime_submit = match unsafe {
            br::vkfn_wrapper::create_command_pool(
                gfx_device.0.device,
                &br::CommandPoolCreateInfo::new(gfx_device.0.graphics_queue_family_index)
                    .transient(),
                None,
            )
        } {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(cause = ?e, "Failed to create onetime submit command pool");
                std::process::abort();
            }
        };

        Self {
            #[cfg(feature = "mt")]
            fence_reactor: FenceReactorThread::new(),
            gfx_device,
            graphics_queue,
            cp_onetime_submit,
        }
    }

    pub const fn device(&self) -> &VulkanGfx {
        &self.gfx_device
    }

    pub fn adapter_raw(&self) -> br::vk::VkPhysicalDevice {
        self.gfx_device.0.adapter
    }

    /// Submits any commands as transient commands.
    pub fn submit_commands(
        &mut self,
        generator: impl for<'a> FnOnce(br::CmdRecord<'a>) -> br::CmdRecord<'a>,
    ) -> br::Result<()> {
        let mut buffers = [core::mem::MaybeUninit::uninit()];
        unsafe {
            br::vkfn_wrapper::allocate_command_buffers(
                self.gfx_device.0.device,
                &br::CommandBufferAllocateInfo::new(
                    &mut br::VkHandleRefMut::dangling(self.cp_onetime_submit),
                    1,
                    br::CommandBufferLevel::Primary,
                ),
                &mut buffers,
            )?;
        }
        let cb = LocalOnetimeSubmitCommandBuffer {
            buffer: unsafe { buffers[0].assume_init() },
            pool: &self.cp_onetime_submit,
            device: &self.gfx_device,
        };
        unsafe {
            br::vkfn_wrapper::begin_command_buffer(
                cb.buffer,
                &br::CommandBufferBeginInfo::new().onetime_submit(),
            )?
        }
        generator(br::CmdRecord::new(unsafe {
            br::VkHandleRefMut::dangling(cb.buffer)
        }))
        .end()?;
        unsafe {
            br::vkfn_wrapper::queue_submit(
                br::VkHandleRefMut::dangling(self.graphics_queue.q),
                &[br::SubmitInfo::new_array(
                    &[],
                    &[],
                    &[br::VkHandleRef::dangling(cb.buffer)],
                    &[],
                )],
                None,
            )?;
            br::vkfn_wrapper::queue_wait_idle(self.graphics_queue.q)?;
        }

        Ok(())
    }
    pub fn submit_buffered_commands(
        &mut self,
        batches: &[br::SubmitInfo],
        fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
    ) -> br::Result<()> {
        unsafe {
            br::vkfn_wrapper::queue_submit(
                br::VkHandleRefMut::dangling(self.graphics_queue.q),
                batches,
                Some(fence.as_transparent_ref_mut()),
            )
        }
    }

    /// Submits any commands as transient commands.
    /// ## Note
    /// Unlike other futures, commands are submitted **immediately**(even if not awaiting the returned future).
    #[cfg(feature = "mt")]
    pub fn submit_commands_async<'s>(
        &'s self,
        generator: impl for<'a> FnOnce(br::CmdRecord<'a>) -> br::CmdRecord<'a>,
    ) -> br::Result<impl core::future::Future<Output = br::Result<()>> + 's> {
        use bedrock::VkHandleMut;

        struct StandaloneFence {
            handle: br::vk::VkFence,
            device: VulkanGfx,
        }
        unsafe impl Sync for StandaloneFence {}
        unsafe impl Send for StandaloneFence {}
        impl Drop for StandaloneFence {
            fn drop(&mut self) {
                unsafe {
                    br::vkfn_wrapper::destroy_fence(
                        br::VkHandle::native_ptr(&self.device),
                        self.handle,
                        None,
                    );
                }
            }
        }
        impl br::VkHandle for StandaloneFence {
            type Handle = br::vk::VkFence;

            fn native_ptr(&self) -> Self::Handle {
                self.handle
            }
        }
        impl br::VkHandleMut for StandaloneFence {
            fn native_ptr_mut(&mut self) -> Self::Handle {
                self.handle
            }
        }
        impl AwaitableFence for StandaloneFence {
            fn is_ready(&self) -> bedrock::Result<bool> {
                let r = unsafe {
                    br::vkfn_wrapper::get_fence_status(self.device.0.device, self.handle)?
                };

                Ok(r == br::vk::VK_SUCCESS)
            }
        }

        struct StandaloneOnetimeSubmitCommandBundle {
            buffer: br::vk::VkCommandBuffer,
            pool: br::vk::VkCommandPool,
            device: VulkanGfx,
        }
        unsafe impl Sync for StandaloneOnetimeSubmitCommandBundle {}
        unsafe impl Send for StandaloneOnetimeSubmitCommandBundle {}
        impl Drop for StandaloneOnetimeSubmitCommandBundle {
            fn drop(&mut self) {
                unsafe {
                    // CommandPoolのDestroyでCommandBufferもfreeしてくれるらしい
                    br::vkfn_wrapper::destroy_command_pool(self.device.0.device, self.pool, None);
                }
            }
        }

        let mut fence = StandaloneFence {
            handle: unsafe {
                br::vkfn_wrapper::create_fence(
                    self.gfx_device.0.device,
                    &br::FenceCreateInfo::new(0),
                    None,
                )?
            },
            device: self.gfx_device.clone(),
        };

        let pool = unsafe {
            br::vkfn_wrapper::create_command_pool(
                self.gfx_device.0.device,
                &br::CommandPoolCreateInfo::new(self.graphics_queue.family).transient(),
                None,
            )?
        };
        let mut cb = [core::mem::MaybeUninit::uninit()];
        if let Err(e) = unsafe {
            br::vkfn_wrapper::allocate_command_buffers(
                self.gfx_device.0.device,
                &br::CommandBufferAllocateInfo::new(
                    &mut br::VkHandleRefMut::dangling(pool),
                    1,
                    br::CommandBufferLevel::Primary,
                ),
                &mut cb,
            )
        } {
            unsafe {
                br::vkfn_wrapper::destroy_command_pool(self.gfx_device.0.device, pool, None);
            }

            return Err(e);
        }
        let cb = StandaloneOnetimeSubmitCommandBundle {
            buffer: unsafe { cb[0].assume_init() },
            pool,
            device: self.gfx_device.clone(),
        };
        unsafe {
            br::vkfn_wrapper::begin_command_buffer(
                cb.buffer,
                &br::CommandBufferBeginInfo::new().onetime_submit(),
            )?
        };
        generator(br::CmdRecord::new(unsafe {
            br::VkHandleRefMut::dangling(cb.buffer)
        }))
        .end()?;
        unsafe {
            br::vkfn_wrapper::queue_submit(
                br::VkHandleRefMut::dangling(self.graphics_queue.q),
                &[br::SubmitInfo::new_array(
                    &[],
                    &[],
                    &[br::VkHandleRef::dangling(cb.buffer)],
                    &[],
                )],
                Some(fence.as_transparent_ref_mut()),
            )?;
        }
        let fence = std::sync::Arc::new(fence);

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
        fence: std::sync::Arc<impl AwaitableFence + Send + Sync + 'static>,
    ) -> impl std::future::Future<Output = br::Result<()>> + 's {
        FenceWaitFuture {
            reactor: &self.fence_reactor,
            object: fence,
            registered: false,
        }
    }

    pub const fn graphics_queue_family_index(&self) -> u32 {
        self.graphics_queue.family
    }

    pub fn vk_extension_is_available(&self, name: &CStr) -> bool {
        self.gfx_device.0.enabled_extension_names.contains(name)
    }

    #[inline(always)]
    pub fn is_extension_available(&self, ext: &VulkanExtension) -> bool {
        ext.promoted
            .is_some_and(|x| self.gfx_device.0.vk_version >= x)
            || self.gfx_device.0.enabled_extension_names.contains(ext.name)
    }

    pub fn vk_version(&self) -> &br::Version {
        &self.gfx_device.0.vk_version
    }

    pub fn wait_operations(&mut self) -> br::Result<()> {
        unsafe { br::vkfn_wrapper::device_wait_idle(self.gfx_device.0.device) }
    }
}
/// Adapter Property exports
impl Graphics {
    pub fn adapter_available_features(&self) -> &br::PhysicalDeviceFeatures {
        self.gfx_device.adapter_available_features()
    }

    pub fn adapter_limits(&self) -> &br::vk::VkPhysicalDeviceLimits {
        self.gfx_device.adapter_limits()
    }

    pub fn adapter_memory_properties(&self) -> &br::PhysicalDeviceMemoryProperties {
        self.gfx_device.adapter_memory_properties()
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
