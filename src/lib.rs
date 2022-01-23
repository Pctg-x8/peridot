use log::*;
pub use peridot_archive as archive;
pub use peridot_math as math;

use bedrock as br;
use std::borrow::Cow;
use std::cell::{Ref, RefCell, RefMut};
use std::ffi::CStr;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant as InstantTimer};

mod state_track;
use self::state_track::StateFence;
mod window;
pub use self::window::SurfaceInfo;
mod resource;
pub use self::resource::*;
#[cfg(feature = "debug")]
mod debug;
#[cfg(feature = "debug")]
use self::debug::*;
pub mod utils;
pub use self::utils::*;

mod asset;
pub use self::asset::*;
mod batch;
pub use self::batch::*;
mod input;
pub use self::input::*;
pub mod audio;
mod model;
pub use self::model::*;
mod layout_cache;
pub use self::layout_cache::*;
mod presenter;
pub use self::presenter::*;

#[cfg(feature = "derive")]
pub use peridot_derive::*;

pub trait NativeLinker: Sized {
    type AssetLoader: PlatformAssetLoader;
    type Presenter: PlatformPresenter;

    fn instance_extensions(&self) -> Vec<&str>;
    fn device_extensions(&self) -> Vec<&str>;

    fn asset_loader(&self) -> &Self::AssetLoader;
    fn new_presenter(&self, g: &Graphics) -> Self::Presenter;

    fn rendering_precision(&self) -> f32 {
        1.0
    }
}

pub trait EngineEvents<PL: NativeLinker>: Sized {
    fn init(_e: &mut Engine<PL>) -> Self;
    /// Updates the game and passes copying(optional) and rendering command batches to the engine.
    fn update(
        &mut self,
        _e: &mut Engine<PL>,
        _on_backbuffer_of: u32,
        _delta_time: Duration,
    ) -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        (None, br::SubmissionBatch::default())
    }
    /// Discards backbuffer-dependent resources(i.e. Framebuffers or some of CommandBuffers)
    fn discard_backbuffer_resources(&mut self) {}
    /// Called when backbuffer has resized
    /// (called after discard_backbuffer_resources so re-create discarded resources here)
    fn on_resize(&mut self, _e: &mut Engine<PL>, _new_size: math::Vector2<usize>) {}

    // Render Resource Persistency(Recovering) //

    /// Storing recovered render resources for discarding
    fn store_render_resources(&mut self, _e: &mut Engine<PL>) {}

    /// Recovering render resources
    fn recover_render_resources(&mut self, _e: &mut Engine<PL>) {}
}
impl<PL: NativeLinker> EngineEvents<PL> for () {
    fn init(_e: &mut Engine<PL>) -> Self {
        ()
    }
}
/// Specifies which type of resource is supports sparse residency?
#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct SparseResidencySupportBits(u16);
impl SparseResidencySupportBits {
    pub const EMPTY: Self = Self(0);
    pub const BUFFER: Self = Self(0x0001);
    pub const IMAGE_2D: Self = Self(0x0002);
    pub const IMAGE_3D: Self = Self(0x0004);
    pub const SAMPLE2: Self = Self(0x0008);
    pub const SAMPLE4: Self = Self(0x0010);
    pub const SAMPLE8: Self = Self(0x0020);
    pub const SAMPLE16: Self = Self(0x0040);
    pub const SAMPLE32: Self = Self(0x0080);
    pub const ALIASED: Self = Self(0x0100);

    pub const fn buffer(self) -> Self {
        Self(self.0 | Self::BUFFER.0)
    }
    pub const fn image_2d(self) -> Self {
        Self(self.0 | Self::IMAGE_2D.0)
    }
    pub const fn image_3d(self) -> Self {
        Self(self.0 | Self::IMAGE_3D.0)
    }
    pub const fn sample2(self) -> Self {
        Self(self.0 | Self::SAMPLE2.0)
    }
    pub const fn sample4(self) -> Self {
        Self(self.0 | Self::SAMPLE4.0)
    }
    pub const fn sample8(self) -> Self {
        Self(self.0 | Self::SAMPLE8.0)
    }
    pub const fn sample16(self) -> Self {
        Self(self.0 | Self::SAMPLE16.0)
    }
    pub const fn aliased(self) -> Self {
        Self(self.0 | Self::ALIASED.0)
    }

    const fn has_buffer(self) -> bool {
        (self.0 & Self::BUFFER.0) != 0
    }
    const fn has_image_2d(self) -> bool {
        (self.0 & Self::IMAGE_2D.0) != 0
    }
    const fn has_image_3d(self) -> bool {
        (self.0 & Self::IMAGE_3D.0) != 0
    }
    const fn has_sample2(self) -> bool {
        (self.0 & Self::SAMPLE2.0) != 0
    }
    const fn has_sample4(self) -> bool {
        (self.0 & Self::SAMPLE4.0) != 0
    }
    const fn has_sample8(self) -> bool {
        (self.0 & Self::SAMPLE8.0) != 0
    }
    const fn has_sample16(self) -> bool {
        (self.0 & Self::SAMPLE16.0) != 0
    }
    const fn has_aliased(self) -> bool {
        (self.0 & Self::ALIASED.0) != 0
    }
}
pub trait FeatureRequests {
    const ENABLE_GEOMETRY_SHADER: bool = false;
    const ENABLE_TESSELLATION_SHADER: bool = false;
    const USE_STORAGE_BUFFERS_IN_VERTEX_SHADER: bool = false;
    const SPARSE_BINDING: bool = false;
    const SPARSE_RESIDENCY_SUPPORT_BITS: SparseResidencySupportBits =
        SparseResidencySupportBits::EMPTY;

    fn requested_features() -> br::vk::VkPhysicalDeviceFeatures {
        br::vk::VkPhysicalDeviceFeatures {
            geometryShader: Self::ENABLE_GEOMETRY_SHADER as _,
            tessellationShader: Self::ENABLE_TESSELLATION_SHADER as _,
            vertexPipelineStoresAndAtomics: Self::USE_STORAGE_BUFFERS_IN_VERTEX_SHADER as _,
            sparseBinding: Self::SPARSE_BINDING as _,
            sparseResidencyBuffer: Self::SPARSE_RESIDENCY_SUPPORT_BITS.has_buffer() as _,
            sparseResidencyImage2D: Self::SPARSE_RESIDENCY_SUPPORT_BITS.has_image_2d() as _,
            sparseResidencyImage3D: Self::SPARSE_RESIDENCY_SUPPORT_BITS.has_image_3d() as _,
            sparseResidency2Samples: Self::SPARSE_RESIDENCY_SUPPORT_BITS.has_sample2() as _,
            sparseResidency4Samples: Self::SPARSE_RESIDENCY_SUPPORT_BITS.has_sample4() as _,
            sparseResidency8Samples: Self::SPARSE_RESIDENCY_SUPPORT_BITS.has_sample8() as _,
            sparseResidency16Samples: Self::SPARSE_RESIDENCY_SUPPORT_BITS.has_sample16() as _,
            sparseResidencyAliased: Self::SPARSE_RESIDENCY_SUPPORT_BITS.has_aliased() as _,
            ..Default::default()
        }
    }
}

pub struct Engine<NL: NativeLinker> {
    nativelink: NL,
    presenter: NL::Presenter,
    pub(self) g: Graphics,
    ip: InputProcess,
    gametimer: GameTimer,
    last_rendering_completion: StateFence,
    amixer: Arc<RwLock<audio::Mixer>>,
}
impl<PL: NativeLinker> Engine<PL> {
    pub fn new(
        name: &str,
        version: (u32, u32, u32),
        nativelink: PL,
        requested_features: br::vk::VkPhysicalDeviceFeatures,
    ) -> Self {
        let mut g = Graphics::new(
            name,
            version,
            nativelink.instance_extensions(),
            nativelink.device_extensions(),
            requested_features,
        )
        .expect("Failed to initialize Graphics Base Driver");
        let presenter = nativelink.new_presenter(&g);
        g.submit_commands(|r| presenter.emit_initialize_backbuffer_commands(r))
            .expect("Initializing Backbuffers");

        Engine {
            ip: InputProcess::new().into(),
            gametimer: GameTimer::new(),
            last_rendering_completion: StateFence::new(&g)
                .expect("Failed to create State Fence for Rendering"),
            amixer: Arc::new(RwLock::new(audio::Mixer::new())),
            nativelink,
            g,
            presenter,
        }
    }

    pub fn postinit(&mut self) {
        trace!("PostInit BaseEngine...");
    }
}
impl<NL: NativeLinker> Engine<NL> {
    pub fn graphics(&self) -> &Graphics {
        &self.g
    }
    pub fn graphics_mut(&mut self) -> &mut Graphics {
        &mut self.g
    }
    pub fn graphics_device(&self) -> &br::Device {
        &self.g.device
    }
    pub fn graphics_queue_family_index(&self) -> u32 {
        self.g.graphics_queue_family_index()
    }
    // 将来的に分かれるかも？
    pub fn transfer_queue_family_index(&self) -> u32 {
        self.g.graphics_queue.family
    }
    pub fn backbuffer_format(&self) -> br::vk::VkFormat {
        self.presenter.format()
    }
    pub fn backbuffer_count(&self) -> usize {
        self.presenter.backbuffer_count()
    }
    pub fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> {
        self.presenter.backbuffer(index)
    }
    pub fn iter_backbuffers(&self) -> impl Iterator<Item = Rc<br::ImageView>> + '_ {
        (0..self.backbuffer_count())
            .map(move |x| self.backbuffer(x).expect("unreachable while iteration"))
    }
    pub fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.presenter.requesting_backbuffer_layout()
    }
    pub fn input(&self) -> &InputProcess {
        &self.ip
    }
    pub fn input_mut(&mut self) -> &mut InputProcess {
        &mut self.ip
    }

    pub fn submit_commands(
        &mut self,
        generator: impl FnOnce(&mut br::CmdRecord),
    ) -> br::Result<()> {
        self.g.submit_commands(generator)
    }
    pub fn submit_buffered_commands(
        &mut self,
        batches: &[br::SubmissionBatch],
        fence: &mut br::Fence,
    ) -> br::Result<()> {
        self.g.submit_buffered_commands(batches, fence)
    }

    pub fn audio_mixer(&self) -> &Arc<RwLock<audio::Mixer>> {
        &self.amixer
    }
}
impl<PL: NativeLinker> Engine<PL> {
    pub fn load<A: FromAsset>(&self, path: &str) -> Result<A, A::Error> {
        A::from_asset(self.nativelink.asset_loader().get(path, A::EXT)?)
    }
    pub fn streaming<A: FromStreamingAsset>(&self, path: &str) -> Result<A, A::Error> {
        A::from_asset(self.nativelink.asset_loader().get_streaming(path, A::EXT)?)
    }

    pub fn rendering_precision(&self) -> f32 {
        self.nativelink.rendering_precision()
    }
}
impl<PL: NativeLinker> Engine<PL> {
    pub fn do_update<EH: EngineEvents<PL>>(&mut self, userlib: &mut EH) {
        let dt = self.gametimer.delta_time();

        let bb_index = match self.presenter.next_backbuffer_index() {
            Err(e) if e.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                // Fire resize and do nothing
                self.do_resize_backbuffer(self.presenter.current_geometry_extent(), userlib);
                return;
            }
            e => e.expect("Acquiring available backbuffer index"),
        };
        self.last_rendering_completion
            .wait()
            .expect("Waiting Last command completion");

        self.ip.prepare_for_frame(dt);

        let (copy_submission, fb_submission) = userlib.update(self, bb_index, dt);
        let pr = self.presenter.render_and_present(
            &self.g,
            &self.last_rendering_completion.object(),
            &self.g.graphics_queue.q,
            bb_index,
            fb_submission,
            copy_submission,
        );
        unsafe {
            self.last_rendering_completion.signal();
        }

        match pr {
            Err(e) if e.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                // Fire resize
                self.do_resize_backbuffer(self.presenter.current_geometry_extent(), userlib);
                return;
            }
            v => v.expect("Present Submission"),
        }
    }

    pub fn do_resize_backbuffer<EH: EngineEvents<PL>>(
        &mut self,
        new_size: math::Vector2<usize>,
        userlib: &mut EH,
    ) {
        self.last_rendering_completion
            .wait()
            .expect("Waiting Last command completion");
        userlib.discard_backbuffer_resources();
        let needs_reinit_backbuffers = self.presenter.resize(&self.g, new_size.clone());
        if needs_reinit_backbuffers {
            let pres = &self.presenter;

            self.g
                .submit_commands(|r| pres.emit_initialize_backbuffer_commands(r))
                .expect("Initializing Backbuffers");
        }
        userlib.on_resize(self, new_size);
    }

    pub fn sound_backend_callback(&self, output_buffer: &mut [f32]) {
        for (n, r) in output_buffer.iter_mut().enumerate() {
            *r = (440.0 * n as f32 / 44100.0).to_radians().sin() * 0.1;
        }
    }
}
impl<NL: NativeLinker> Drop for Engine<NL> {
    fn drop(&mut self) {
        unsafe {
            self.graphics().device.wait().expect("device error");
        }
    }
}

pub struct LateInit<T>(RefCell<Option<T>>);
impl<T> LateInit<T> {
    pub fn new() -> Self {
        LateInit(RefCell::new(None))
    }
    pub fn init(&self, v: T) {
        *self.0.borrow_mut() = v.into();
    }
    pub fn get(&self) -> Ref<T> {
        Ref::map(self.0.borrow(), |x| x.as_ref().expect("uninitialized"))
    }
}
pub struct Discardable<T>(RefCell<Option<T>>);
impl<T> Discardable<T> {
    pub fn new() -> Self {
        Discardable(RefCell::new(None))
    }
    pub fn set(&self, v: T) {
        *self.0.borrow_mut() = v.into();
    }
    pub fn set_lw(&mut self, v: T) {
        *self.0.get_mut() = v.into();
    }
    pub fn get(&self) -> Ref<T> {
        Ref::map(self.0.borrow(), |x| x.as_ref().expect("uninitialized"))
    }
    pub fn get_mut(&self) -> RefMut<T> {
        RefMut::map(self.0.borrow_mut(), |x| x.as_mut().expect("uninitialized"))
    }
    pub fn get_mut_lw(&mut self) -> &mut T {
        self.0.get_mut().as_mut().expect("uninitialized")
    }
    pub fn discard(&self) {
        *self.0.borrow_mut() = None;
    }
    pub fn discard_lw(&mut self) {
        drop(self.0.get_mut().take());
    }
    pub fn is_available(&self) -> bool {
        self.0.borrow().is_some()
    }
}
impl<T> From<T> for Discardable<T> {
    fn from(v: T) -> Self {
        Discardable(RefCell::new(Some(v)))
    }
}

use br::vk::{
    VkMemoryType, VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT, VK_MEMORY_PROPERTY_HOST_CACHED_BIT,
    VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT,
    VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT, VK_MEMORY_PROPERTY_PROTECTED_BIT,
};

pub struct MemoryType(u32, VkMemoryType);
impl MemoryType {
    pub fn index(&self) -> u32 {
        self.0
    }
    pub fn corresponding_mask(&self) -> u32 {
        0x01 << self.0
    }
    pub fn has_property_flags(&self, other: br::MemoryPropertyFlags) -> bool {
        (self.1.propertyFlags & other.bits()) != 0
    }
    pub fn is_host_coherent(&self) -> bool {
        self.has_property_flags(br::MemoryPropertyFlags::HOST_COHERENT)
    }
    pub fn is_host_cached(&self) -> bool {
        self.has_property_flags(br::MemoryPropertyFlags::HOST_CACHED)
    }
}
impl std::fmt::Debug for MemoryType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut flags = Vec::with_capacity(7);
        if (self.1.propertyFlags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) != 0 {
            flags.push("DEVICE LOCAL");
        }
        if (self.1.propertyFlags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) != 0 {
            flags.push("HOST VISIBLE");
        }
        if (self.1.propertyFlags & VK_MEMORY_PROPERTY_HOST_CACHED_BIT) != 0 {
            flags.push("CACHED");
        }
        if (self.1.propertyFlags & VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) != 0 {
            flags.push("COHERENT");
        }
        if (self.1.propertyFlags & VK_MEMORY_PROPERTY_PROTECTED_BIT) != 0 {
            flags.push("PROTECTED");
        }
        if (self.1.propertyFlags & VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT) != 0 {
            flags.push("LAZILY ALLOCATED");
        }
        if (self.1.propertyFlags & VK_MEMORY_PROPERTY_PROTECTED_BIT) != 0 {
            flags.push("PROTECTED");
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
    pub fn new(pd: &br::PhysicalDevice) -> Self {
        let mem = pd.memory_properties();
        let (device_memory_types, host_memory_types): (Vec<_>, Vec<_>) = mem
            .types()
            .enumerate()
            .map(|(n, mt)| {
                let is_device_local = (mt.propertyFlags & VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) != 0;
                let is_host_visible = (mt.propertyFlags & VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) != 0;

                (
                    if is_device_local {
                        MemoryType(n as _, mt.clone()).into()
                    } else {
                        None
                    },
                    if is_host_visible {
                        MemoryType(n as _, mt.clone()).into()
                    } else {
                        None
                    },
                )
            })
            .unzip();

        MemoryTypeManager {
            device_memory_types: device_memory_types.into_iter().filter_map(|x| x).collect(),
            host_memory_types: host_memory_types.into_iter().filter_map(|x| x).collect(),
        }
    }

    pub fn exact_host_visible_index(
        &self,
        mask: u32,
        required: br::MemoryPropertyFlags,
    ) -> Option<&MemoryType> {
        self.host_memory_types
            .iter()
            .find(|mt| (mask & mt.corresponding_mask()) != 0 && mt.has_property_flags(required))
    }
    pub fn host_visible_index(
        &self,
        mask: u32,
        preference: br::MemoryPropertyFlags,
    ) -> Option<&MemoryType> {
        self.exact_host_visible_index(mask, preference).or_else(|| {
            self.host_memory_types
                .iter()
                .find(|mt| (mask & mt.corresponding_mask()) != 0)
        })
    }
    pub fn device_local_index(&self, mask: u32) -> Option<&MemoryType> {
        self.device_memory_types
            .iter()
            .find(|mt| (mask & mt.corresponding_mask()) != 0)
    }

    fn diagnose_heaps(p: &br::PhysicalDevice) {
        info!("Memory Heaps: ");
        for (n, &br::vk::VkMemoryHeap { size, flags }) in p.memory_properties().heaps().enumerate()
        {
            let (mut nb, mut unit) = (size as f32, "bytes");
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
            if (flags & br::vk::VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) != 0 {
                info!("  #{}: {} {} [DEVICE LOCAL]", n, nb, unit);
            } else {
                info!("  #{}: {} {}", n, nb, unit);
            }
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

/// Queue object with family index
pub struct Queue {
    q: br::Queue,
    family: u32,
}
/// Graphics manager
pub struct Graphics {
    pub(self) instance: br::Instance,
    pub(self) adapter: br::PhysicalDevice,
    device: br::Device,
    graphics_queue: Queue,
    cp_onetime_submit: br::CommandPool,
    pub memory_type_manager: MemoryTypeManager,
}
impl Graphics {
    fn new(
        appname: &str,
        appversion: (u32, u32, u32),
        instance_extensions: Vec<&str>,
        device_extensions: Vec<&str>,
        features: br::vk::VkPhysicalDeviceFeatures,
    ) -> br::Result<Self> {
        info!("Supported Layers: ");
        let mut validation_layer_available = false;
        #[cfg(debug_assertions)]
        for l in br::Instance::enumerate_layer_properties()
            .expect("failed to enumerate layer properties")
        {
            let name = unsafe { CStr::from_ptr(l.layerName.as_ptr()) };
            let name_str = name
                .to_str()
                .expect("unexpected invalid sequence in layer name");
            info!(
                "* {} :: {}/{}",
                name_str, l.specVersion, l.implementationVersion
            );
            if name_str == "VK_LAYER_KHRONOS_validation" {
                validation_layer_available = true;
            }
        }

        let mut ib = br::InstanceBuilder::new(appname, appversion, "Interlude2:Peridot", (0, 1, 0));
        ib.add_extensions(instance_extensions);
        #[cfg(debug_assertions)]
        ib.add_extension("VK_EXT_debug_report");
        if validation_layer_available {
            ib.add_layer("VK_LAYER_KHRONOS_validation");
        } else {
            warn!("Validation Layer is not found!");
        }
        #[cfg(feature = "debug")]
        {
            ib.add_extension("VK_EXT_debug_utils");
            ib.add_ext_structure(
                br::DebugUtilsMessengerCreateInfo::new(debug_utils_out)
                    .filter_severity(br::DebugUtilsMessageSeverityFlags::ERROR.and_warning()),
            );
            debug!("Debug reporting activated");
        }
        let instance = ib.create()?;

        let adapter = instance
            .iter_physical_devices()?
            .next()
            .expect("no physical devices");
        let memory_type_manager = MemoryTypeManager::new(&adapter);
        MemoryTypeManager::diagnose_heaps(&adapter);
        memory_type_manager.diagnose_types();
        let gqf_index = adapter
            .queue_family_properties()
            .find_matching_index(br::QueueFlags::GRAPHICS)
            .expect("No graphics queue");
        let qci = br::DeviceQueueCreateInfo(gqf_index, vec![0.0]);
        let device = {
            let mut db = br::DeviceBuilder::new(&adapter);
            db.add_extensions(device_extensions).add_queue(qci);
            if validation_layer_available {
                db.add_layer("VK_LAYER_KHRONOS_validation");
            }
            *db.mod_features() = features;
            db.create()?
        };

        return Ok(Graphics {
            cp_onetime_submit: br::CommandPool::new(&device, gqf_index, true, false)?,
            graphics_queue: Queue {
                q: device.queue(gqf_index, 0),
                family: gqf_index,
            },
            instance,
            adapter,
            device,
            memory_type_manager,
        });
    }

    /// Submits any commands as transient commands.
    pub fn submit_commands(
        &mut self,
        generator: impl FnOnce(&mut br::CmdRecord),
    ) -> br::Result<()> {
        let mut cb = LocalCommandBundle(
            self.cp_onetime_submit.alloc(1, true)?,
            &mut self.cp_onetime_submit,
        );
        generator(unsafe { &mut cb[0].begin_once()? });
        self.graphics_queue.q.submit(
            &[br::SubmissionBatch {
                command_buffers: Cow::from(&cb[..]),
                ..Default::default()
            }],
            None,
        )?;
        self.graphics_queue.q.wait()
    }
    pub fn submit_buffered_commands(
        &mut self,
        batches: &[br::SubmissionBatch],
        fence: &mut br::Fence,
    ) -> br::Result<()> {
        self.graphics_queue.q.submit(batches, Some(fence))
    }
    pub fn submit_buffered_commands_raw(
        &mut self,
        batches: &[br::vk::VkSubmitInfo],
        fence: &mut br::Fence,
    ) -> br::Result<()> {
        self.graphics_queue.q.submit_raw(batches, Some(fence))
    }

    pub fn instance(&self) -> &br::Instance {
        &self.instance
    }
    pub fn adapter(&self) -> &br::PhysicalDevice {
        &self.adapter
    }
    pub fn graphics_queue_family_index(&self) -> u32 {
        self.graphics_queue.family
    }
}
impl Deref for Graphics {
    type Target = br::Device;
    fn deref(&self) -> &br::Device {
        &self.device
    }
}

struct GameTimer(Option<InstantTimer>);
impl GameTimer {
    pub fn new() -> Self {
        GameTimer(None)
    }
    pub fn delta_time(&mut self) -> Duration {
        let d = self
            .0
            .as_ref()
            .map_or_else(|| Duration::new(0, 0), |it| it.elapsed());
        self.0 = InstantTimer::now().into();

        return d;
    }
}

struct LocalCommandBundle<'p>(Vec<br::CommandBuffer>, &'p mut br::CommandPool);
impl<'p> Deref for LocalCommandBundle<'p> {
    type Target = [br::CommandBuffer];

    fn deref(&self) -> &[br::CommandBuffer] {
        &self.0
    }
}
impl DerefMut for LocalCommandBundle<'_> {
    fn deref_mut(&mut self) -> &mut [br::CommandBuffer] {
        &mut self.0
    }
}
impl<'p> Drop for LocalCommandBundle<'p> {
    fn drop(&mut self) {
        unsafe {
            self.1.free(&self.0[..]);
        }
    }
}

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum CBSubmissionType {
    Graphics,
    Transfer,
}
pub struct CommandBundle(Vec<br::CommandBuffer>, br::CommandPool);
impl Deref for CommandBundle {
    type Target = [br::CommandBuffer];
    fn deref(&self) -> &[br::CommandBuffer] {
        &self.0
    }
}
impl DerefMut for CommandBundle {
    fn deref_mut(&mut self) -> &mut [br::CommandBuffer] {
        &mut self.0
    }
}
impl Drop for CommandBundle {
    fn drop(&mut self) {
        unsafe {
            self.1.free(&self.0[..]);
        }
    }
}
impl CommandBundle {
    pub fn new(g: &Graphics, submission_type: CBSubmissionType, count: usize) -> br::Result<Self> {
        let qf = match submission_type {
            CBSubmissionType::Graphics => g.graphics_queue.family,
            CBSubmissionType::Transfer => g.graphics_queue.family,
        };
        let mut cp = br::CommandPool::new(&g.device, qf, false, false)?;
        return Ok(CommandBundle(cp.alloc(count as _, true)?, cp));
    }
    pub fn reset(&mut self) -> br::Result<()> {
        self.1.reset(true)
    }
}

pub enum SubpassDependencyTemplates {}
impl SubpassDependencyTemplates {
    pub fn to_color_attachment_in(
        from_subpass: Option<u32>,
        occurence_subpass: u32,
        by_region: bool,
    ) -> br::vk::VkSubpassDependency {
        br::vk::VkSubpassDependency {
            dstSubpass: occurence_subpass,
            srcSubpass: from_subpass.unwrap_or(br::vk::VK_SUBPASS_EXTERNAL),
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
            dependencyFlags: if by_region {
                br::vk::VK_DEPENDENCY_BY_REGION_BIT
            } else {
                0
            },
            srcStageMask: br::PipelineStageFlags::TOP_OF_PIPE.0,
            ..Default::default()
        }
    }
}

pub enum RenderPassTemplates {}
impl RenderPassTemplates {
    pub fn single_render(
        format: br::vk::VkFormat,
        outer_requesting_layout: br::ImageLayout,
    ) -> br::RenderPassBuilder {
        let mut b = br::RenderPassBuilder::new();
        let adesc = br::AttachmentDescription::new(
            format,
            outer_requesting_layout,
            outer_requesting_layout,
        )
        .load_op(br::LoadOp::Clear)
        .store_op(br::StoreOp::Store);
        b.add_attachment(adesc);
        b.add_subpass(br::SubpassDescription::new().add_color_output(
            0,
            br::ImageLayout::ColorAttachmentOpt,
            None,
        ));
        b.add_dependency(SubpassDependencyTemplates::to_color_attachment_in(
            None, 0, true,
        ));

        return b;
    }
}

pub trait SpecConstantStorage {
    fn as_pair(&self) -> (Cow<[br::vk::VkSpecializationMapEntry]>, br::DynamicDataCell);
}

pub struct LayoutedPipeline(br::Pipeline, Rc<br::PipelineLayout>);
impl LayoutedPipeline {
    pub fn combine(p: br::Pipeline, layout: &Rc<br::PipelineLayout>) -> Self {
        LayoutedPipeline(p, layout.clone())
    }
    pub fn pipeline(&self) -> &br::Pipeline {
        &self.0
    }
    pub fn layout(&self) -> &Rc<br::PipelineLayout> {
        &self.1
    }
    pub fn bind(&self, rec: &mut br::CmdRecord) {
        rec.bind_graphics_pipeline_pair(&self.0, &self.1);
    }
}
