use async_std::stream::StreamExt;
use futures_util::FutureExt;
pub use peridot_archive as archive;
pub use peridot_math as math;

use bedrock as br;
use br::Device;
#[cfg(feature = "mt")]
use br::Status;
use parking_lot::RwLock;
use std::borrow::Cow;
use std::cell::{Ref, RefCell};
use std::ffi::CStr;
use std::ops::{Deref, DerefMut};
use std::sync::Arc;
use std::time::{Duration, Instant as InstantTimer};

mod graphics;
pub use self::graphics::{
    CBSubmissionType, CommandBundle, DeviceObject, Graphics, InstanceObject, LocalCommandBundle,
    MemoryTypeManager, VulkanExtension,
};
mod state_track;
use self::state_track::StateFence;
mod window;
pub use self::window::SurfaceInfo;
mod resource;
pub use self::resource::*;
#[cfg(feature = "debug")]
mod debug;
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
mod presenter;
pub use self::presenter::*;

pub mod mthelper;
#[allow(unused_imports)]
use mthelper::DynamicMutabilityProvider;
use mthelper::{DynamicMut, MappableGuardObject, MappableMutGuardObject, SharedRef};

#[cfg(feature = "derive")]
pub use peridot_derive::*;

pub trait NativeLinker: Sized {
    type AssetLoader: PlatformAssetLoader;
    type Presenter: PlatformPresenter;

    fn instance_extensions(&self) -> Vec<&CStr>;
    fn device_extensions(&self) -> Vec<&CStr>;

    fn asset_loader(&self) -> &Self::AssetLoader;
    fn new_presenter(&self, g: &Graphics) -> Self::Presenter;

    fn rendering_precision(&self) -> f32 {
        1.0
    }
}

pub trait EngineEvents<PL: NativeLinker>: Sized {
    fn init(_e: &mut Engine<PL>) -> Self;

    /// Updates the game.
    fn update(&mut self, _e: &mut Engine<PL>, _on_back_buffer_of: u32, _delta_time: Duration) {}

    /// Discards back-buffer-dependent resources(i.e. Framebuffers or some of CommandBuffers)
    fn discard_back_buffer_resources(&mut self) {}

    /// Called when back-buffer has resized
    /// (called after discard_back-buffer_resources so re-create discarded resources here)
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

pub enum EngineEvent {
    Shutdown,
    Resize(math::Vector2<u32>),
}

pub struct FrameData {
    pub delta_time: Duration,
    pub backbuffer_index: u32,
}

pub enum Event {
    NextFrame,
    Shutdown,
    Resize(math::Vector2<u32>),
}

#[derive(Debug)]
pub enum PrepareFrameError {
    FramebufferOutOfDate,
}

pub struct EngineEventReceiver {
    frame_timing_receiver: async_std::channel::Receiver<()>,
    other_events_receiver: async_std::channel::Receiver<EngineEvent>,
}
impl EngineEventReceiver {
    pub async fn wait_for_event(&mut self) -> Option<Event> {
        futures_util::select! {
            e = self.frame_timing_receiver.next().fuse() => match e {
                Some(()) => Some(Event::NextFrame),
                None => None,
            },
            e = self.other_events_receiver.next().fuse() => match e {
                Some(EngineEvent::Shutdown) => Some(Event::Shutdown),
                Some(EngineEvent::Resize(ns)) => Some(Event::Resize(ns)),
                None => None,
            }
        }
    }
}

pub struct Engine<NL: NativeLinker> {
    native_link: NL,
    presenter: NL::Presenter,
    pub(self) g: Graphics,
    ip: InputProcess,
    game_timer: GameTimer,
    last_rendering_completion: StateFence<br::FenceObject<DeviceObject>>,
    audio_mixer: Arc<RwLock<audio::Mixer>>,
    request_resize: bool,
    engine_events_sender: async_std::channel::Sender<EngineEvent>,
    receivers: core::cell::UnsafeCell<EngineEventReceiver>,
}
impl<PL: NativeLinker> Engine<PL> {
    pub fn new(
        name: &str,
        version: br::Version,
        native_link: PL,
        requested_features: br::vk::VkPhysicalDeviceFeatures,
        engine_events_bus: (
            async_std::channel::Sender<EngineEvent>,
            async_std::channel::Receiver<EngineEvent>,
        ),
        frame_timing_receiver: async_std::channel::Receiver<()>,
    ) -> Self {
        let mut g = Graphics::new(
            name,
            version,
            native_link.instance_extensions(),
            native_link.device_extensions(),
            requested_features,
        );
        let presenter = native_link.new_presenter(&g);
        g.submit_commands(|r| presenter.emit_initialize_back_buffer_commands(r))
            .expect("Initializing Back Buffers");

        Self {
            ip: InputProcess::new().into(),
            game_timer: GameTimer::new(),
            last_rendering_completion: StateFence::new(g.device.clone())
                .expect("Failed to create State Fence for Rendering"),
            audio_mixer: Arc::new(RwLock::new(audio::Mixer::new())),
            native_link,
            g,
            presenter,
            request_resize: false,
            engine_events_sender: engine_events_bus.0,
            receivers: core::cell::UnsafeCell::new(EngineEventReceiver {
                frame_timing_receiver,
                other_events_receiver: engine_events_bus.1,
            }),
        }
    }

    pub fn post_init(&mut self) {
        tracing::trace!("PostInit BaseEngine...");
    }
}
impl<NL: NativeLinker> Engine<NL> {
    pub fn event_receivers(&self) -> &mut EngineEventReceiver {
        unsafe { &mut *self.receivers.get() }
    }

    pub async fn quit(&self) {
        if let Err(e) = self.engine_events_sender.send(EngineEvent::Shutdown).await {
            tracing::warn!(cause = ?e, "Engine has already shutting down");
        }
    }

    pub const fn graphics(&self) -> &Graphics {
        &self.g
    }
    pub fn graphics_mut(&mut self) -> &mut Graphics {
        &mut self.g
    }

    pub const fn graphics_device(&self) -> &DeviceObject {
        &self.g.device
    }
    pub const fn graphics_queue_family_index(&self) -> u32 {
        self.g.graphics_queue_family_index()
    }
    // 将来的に分かれるかも？
    pub const fn transfer_queue_family_index(&self) -> u32 {
        self.g.graphics_queue.family
    }

    pub fn back_buffer_format(&self) -> br::vk::VkFormat {
        self.presenter.format()
    }
    pub fn back_buffer_count(&self) -> usize {
        self.presenter.back_buffer_count()
    }
    pub fn back_buffer(
        &self,
        index: usize,
    ) -> Option<&SharedRef<<NL::Presenter as PlatformPresenter>::BackBuffer>> {
        self.presenter.back_buffer(index)
    }
    pub fn iter_back_buffers<'s>(
        &'s self,
    ) -> impl Iterator<Item = &'s SharedRef<<NL::Presenter as PlatformPresenter>::BackBuffer>> + 's
    {
        (0..self.back_buffer_count())
            .map(move |x| self.back_buffer(x).expect("unreachable while iteration"))
    }
    pub fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.presenter.requesting_back_buffer_layout()
    }
    pub fn back_buffer_attachment_desc(&self) -> br::vk::VkAttachmentDescription {
        let (ol, _) = self.requesting_back_buffer_layout();

        br::vk::VkAttachmentDescription::new(self.back_buffer_format(), ol, ol)
    }

    pub fn input(&self) -> &InputProcess {
        &self.ip
    }
    pub fn input_mut(&mut self) -> &mut InputProcess {
        &mut self.ip
    }

    pub fn submit_commands(
        &mut self,
        generator: impl FnOnce(br::CmdRecord<DeviceObject>) -> br::CmdRecord<DeviceObject>,
    ) -> br::Result<()> {
        self.g.submit_commands(generator)
    }
    pub fn submit_buffered_commands(
        &mut self,
        batches: &[impl br::SubmissionBatch],
        fence: &mut impl br::FenceMut,
    ) -> br::Result<()> {
        self.g.submit_buffered_commands(batches, fence)
    }

    #[cfg(feature = "mt")]
    /// Submits any commands as transient commands.
    /// ## Note
    /// Unlike other futures, commands are submitted **immediately**(even if not awaiting the returned future).
    pub fn submit_commands_async<'s>(
        &'s self,
        generator: impl FnOnce(br::CmdRecord<DeviceObject>) -> br::CmdRecord<DeviceObject> + 's,
    ) -> br::Result<impl std::future::Future<Output = br::Result<()>> + 's> {
        self.g.submit_commands_async(generator)
    }

    #[inline]
    pub const fn audio_mixer(&self) -> &Arc<RwLock<audio::Mixer>> {
        &self.audio_mixer
    }
}
impl<PL: NativeLinker> Engine<PL> {
    pub fn load<A: FromAsset>(&self, path: &str) -> Result<A, A::Error> {
        A::from_asset(self.native_link.asset_loader().get(path, A::EXT)?)
    }
    pub fn streaming<A: FromStreamingAsset>(&self, path: &str) -> Result<A, A::Error> {
        A::from_asset(
            self.native_link
                .asset_loader()
                .get_streaming(path, A::EXT)?,
        )
    }

    pub fn rendering_precision(&self) -> f32 {
        self.native_link.rendering_precision()
    }
}
impl<PL: NativeLinker> Engine<PL> {
    pub fn prepare_frame(&mut self) -> Result<FrameData, PrepareFrameError> {
        StateFence::wait(&mut self.last_rendering_completion)
            .expect("Waiting last command completion");

        let dt = self.game_timer.delta_time();
        let backbuffer_index = match self.presenter.next_back_buffer_index() {
            Err(e) if e == br::vk::VK_ERROR_OUT_OF_DATE_KHR || e == br::vk::VK_SUBOPTIMAL_KHR => {
                return Err(PrepareFrameError::FramebufferOutOfDate);
            }
            e => e.expect("Acquiring available back-buffer index failed"),
        };

        self.ip.prepare_for_frame(dt);

        Ok(FrameData {
            delta_time: dt,
            backbuffer_index,
        })
    }

    // pub fn do_update(&mut self, callback: &mut (impl EngineEvents<PL> + ?Sized)) {
    //     let dt = self.game_timer.delta_time();

    //     let bb_index = match self.presenter.next_back_buffer_index() {
    //         Err(e) if e == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
    //             // Fire resize and do nothing
    //             self.do_resize_back_buffer(self.presenter.current_geometry_extent(), callback);
    //             return;
    //         }
    //         e => e.expect("Acquiring available back-buffer index"),
    //     };
    //     StateFence::wait(&mut self.last_rendering_completion)
    //         .expect("Waiting Last command completion");

    //     self.ip.prepare_for_frame(dt);

    //     callback.update(self, bb_index, dt);

    //     if self.request_resize {
    //         self.request_resize = false;
    //         self.do_resize_back_buffer(self.presenter.current_geometry_extent(), callback);
    //     }
    // }

    pub fn do_render(
        &mut self,
        bb_index: u32,
        copy_submission: Option<impl br::SubmissionBatch>,
        render_submission: impl br::SubmissionBatch,
    ) -> br::Result<()> {
        let pr = self.presenter.render_and_present(
            &mut self.g,
            self.last_rendering_completion.inner_mut(),
            bb_index,
            render_submission,
            copy_submission,
        );
        unsafe {
            self.last_rendering_completion.signal();
        }

        match pr {
            Err(e) if e == br::vk::VK_ERROR_OUT_OF_DATE_KHR || e == br::vk::VK_SUBOPTIMAL_KHR => {
                // Fire resize
                self.request_resize = true;

                Ok(())
            }
            v => v,
        }
    }

    pub fn wait_for_last_rendering_completion(&mut self) {
        StateFence::wait(&mut self.last_rendering_completion)
            .expect("Waiting Last command completion");
    }

    pub fn resize_presenter_backbuffers(&mut self, new_size: math::Vector2<u32>) {
        let needs_re_init_back_buffers = self
            .presenter
            .resize(&self.g, math::Vector2(new_size.0 as _, new_size.1 as _));
        if needs_re_init_back_buffers {
            let pres = &self.presenter;

            self.g
                .submit_commands(|r| pres.emit_initialize_back_buffer_commands(r))
                .expect("Initializing Back Buffers");
        }
    }

    // pub fn do_resize_back_buffer(
    //     &mut self,
    //     new_size: math::Vector2<usize>,
    //     callback: &mut (impl EngineEvents<PL> + ?Sized),
    // ) {
    //     StateFence::wait(&mut self.last_rendering_completion)
    //         .expect("Waiting Last command completion");
    //     callback.discard_back_buffer_resources();
    //     let needs_re_init_back_buffers = self.presenter.resize(&self.g, new_size.clone());
    //     if needs_re_init_back_buffers {
    //         let pres = &self.presenter;

    //         self.g
    //             .submit_commands(|r| pres.emit_initialize_back_buffer_commands(r))
    //             .expect("Initializing Back Buffers");
    //     }
    //     callback.on_resize(self, new_size);
    // }

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

pub struct Discardable1<T>(Option<T>);
impl<T> Discardable1<T> {
    pub const fn new() -> Self {
        Self(None)
    }

    pub fn set(&mut self, v: T) {
        self.0 = Some(v);
    }

    pub fn get<'v>(&'v self) -> &'v T {
        self.0.as_ref().expect("value unset")
    }

    pub fn get_mut<'v>(&'v mut self) -> &'v mut T {
        self.0.as_mut().expect("value unset")
    }

    pub fn discard(&mut self) {
        self.0 = None;
    }

    pub fn take(&mut self) -> Option<T> {
        self.0.take()
    }

    pub const fn is_available(&self) -> bool {
        self.0.is_some()
    }
}
impl<T> From<T> for Discardable1<T> {
    #[inline(always)]
    fn from(value: T) -> Self {
        Self(Some(value))
    }
}

pub struct Discardable<T>(DynamicMut<Option<T>>);
impl<T> Discardable<T> {
    pub fn new() -> Self {
        Discardable(DynamicMut::new(None))
    }
    pub fn set(&self, v: T) {
        *self.0.borrow_mut() = v.into();
    }
    pub fn set_lw(&mut self, v: T) {
        *self.0.get_mut() = v.into();
    }

    pub fn get<'v>(&'v self) -> impl Deref<Target = T> + 'v {
        self.0
            .borrow()
            .map_guarded_value(|x| x.as_ref().expect("uninitialized"))
    }

    pub fn get_mut<'v>(&'v self) -> impl Deref<Target = T> + DerefMut + 'v {
        self.0
            .borrow_mut()
            .map_guarded_value(|x| x.as_mut().expect("uninitialized"))
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
    pub fn take_lw(&mut self) -> Option<T> {
        self.0.get_mut().take()
    }
    pub fn is_available(&self) -> bool {
        self.0.borrow().is_some()
    }
}
impl<T> From<T> for Discardable<T> {
    fn from(v: T) -> Self {
        Discardable(DynamicMut::new(Some(v)))
    }
}

pub struct GameTimer(Option<InstantTimer>);
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

pub enum SubpassDependencyTemplates {}
impl SubpassDependencyTemplates {
    pub fn to_color_attachment_in(
        from_subpass: Option<u32>,
        occurrence_subpass: u32,
        by_region: bool,
    ) -> br::vk::VkSubpassDependency {
        br::vk::VkSubpassDependency {
            dstSubpass: occurrence_subpass,
            srcSubpass: from_subpass.unwrap_or(br::vk::VK_SUBPASS_EXTERNAL),
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
            dependencyFlags: if by_region {
                br::vk::VK_DEPENDENCY_BY_REGION_BIT
            } else {
                0
            },
            srcStageMask: br::PipelineStageFlags::TOP_OF_PIPE.0,
            srcAccessMask: 0,
        }
    }
}

pub trait SpecConstantStorage {
    fn as_pair(&self) -> (Cow<[br::vk::VkSpecializationMapEntry]>, Cow<[u8]>);
}

pub struct LayoutedPipeline<Pipeline: br::Pipeline, Layout: br::PipelineLayout>(Pipeline, Layout);
impl<Pipeline: br::Pipeline, Layout: br::PipelineLayout> LayoutedPipeline<Pipeline, Layout> {
    pub const fn combine(p: Pipeline, layout: Layout) -> Self {
        Self(p, layout)
    }

    pub const fn pipeline(&self) -> &Pipeline {
        &self.0
    }

    pub const fn layout(&self) -> &Layout {
        &self.1
    }

    #[inline(always)]
    pub fn bind<'r, Device: ?Sized>(
        &self,
        rec: br::CmdRecord<'r, Device>,
    ) -> br::CmdRecord<'r, Device> {
        rec.bind_pipeline(br::PipelineBindPoint::Graphics, &self.0)
    }
}
