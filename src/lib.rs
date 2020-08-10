
// extern crate font_kit;
#[macro_use] extern crate log;
extern crate libc;

extern crate pathfinder_partitioner;
extern crate bedrock;
pub extern crate peridot_math as math;
pub extern crate peridot_archive as archive;

use bedrock as br; use bedrock::traits::*;
use std::ops::Deref;
use std::rc::Rc;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::cell::{Ref, RefMut, RefCell};
use std::time::{Instant as InstantTimer, Duration};
use std::ffi::CStr;

mod window; use self::window::{WindowRenderTargets, StateFence};
pub use self::window::{PlatformRenderTarget, SurfaceInfo};
mod resource; pub use self::resource::*;
#[cfg(debug_assertions)] mod debug; #[cfg(debug_assertions)] use self::debug::DebugReport;
pub mod utils; pub use self::utils::*;

mod asset; pub use self::asset::*;
mod input; pub use self::input::*;
mod model; pub use self::model::*;

pub trait NativeLinker
{
    type AssetLoader: PlatformAssetLoader;
    type RenderTargetProvider: PlatformRenderTarget;

    fn asset_loader(&self) -> &Self::AssetLoader;
    fn render_target_provider(&self) -> &Self::RenderTargetProvider;

    fn rendering_precision(&self) -> f32 { 1.0 }
}

pub trait EngineEvents<PL: NativeLinker> : Sized
{
    fn init(_e: &mut Engine<PL>) -> Self;
    /// Updates the game and passes copying(optional) and rendering command batches to the engine.
    fn update(&mut self, _e: &Engine<PL>, _on_backbuffer_of: u32, _delta_time: Duration)
        -> (Option<br::SubmissionBatch>, br::SubmissionBatch)
    {
        (None, br::SubmissionBatch::default())
    }
    /// Discards backbuffer-dependent resources(i.e. Framebuffers or some of CommandBuffers)
    fn discard_backbuffer_resources(&mut self) {}
    /// Called when backbuffer has resized
    /// (called after discard_backbuffer_resources so re-create discarded resources here)
    fn on_resize(&mut self, _e: &Engine<PL>, _new_size: math::Vector2<usize>) {}

    // Render Resource Persistency(Recovering) //

    /// Storing recovered render resources for discarding
    fn store_render_resources(&mut self, _e: &Engine<PL>) {}

    /// Recovering render resources
    fn recover_render_resources(&mut self, _e: &Engine<PL>) {}
}
impl<PL: NativeLinker> EngineEvents<PL> for ()
{
    fn init(_e: &mut Engine<PL>) -> Self { () }
}
pub trait FeatureRequests
{
    const ENABLE_GEOMETRY_SHADER: bool = false;
    const ENABLE_TESSELLATION_SHADER: bool = false;
    const USE_STORAGE_BUFFERS_IN_VERTEX_SHADER: bool = false;
    
    fn requested_features() -> br::vk::VkPhysicalDeviceFeatures
    {
        br::vk::VkPhysicalDeviceFeatures
        {
            geometryShader: Self::ENABLE_GEOMETRY_SHADER as _,
            tessellationShader: Self::ENABLE_TESSELLATION_SHADER as _,
            vertexPipelineStoresAndAtomics: Self::USE_STORAGE_BUFFERS_IN_VERTEX_SHADER as _,
            .. Default::default()
        }
    }
}

pub struct Engine<PL>
{
    nativelink: PL,
    surface: SurfaceInfo,
    wrt: Discardable<WindowRenderTargets>,
    pub(self) g: Graphics,
    ip: InputProcess,
    gametimer: GameTimer,
    last_rendering_completion: StateFence
}
impl<PL: NativeLinker> Engine<PL>
{
    pub fn new(
        name: &str, version: (u32, u32, u32),
        nativelink: PL,
        requested_features: br::vk::VkPhysicalDeviceFeatures) -> Self
    {
        let g = Graphics::new(
            name, version,
            nativelink.render_target_provider().surface_extension_name(),
            requested_features
        ).expect("Failed to initialize Graphics Base Driver");
        let surface = nativelink.render_target_provider()
            .create_surface(&g.instance, &g.adapter, g.graphics_queue.family)
            .expect("Failed to create Surface");
        trace!("Creating WindowRenderTargets...");
        let wrt: Discardable<_> = WindowRenderTargets::new(&g, &surface, nativelink.render_target_provider())
            .expect("Failed to initialize Window Render Target Driver")
            .into();
        g.submit_commands(|r| wrt.get().emit_initialize_backbuffers_commands(r))
            .expect("Initializing Backbuffers");

        Engine
        {
            ip: InputProcess::new().into(),
            gametimer: GameTimer::new(),
            last_rendering_completion: StateFence::new(&g).expect("Failed to create State Fence"),
            nativelink,
            g,
            surface,
            wrt
        }
    }

    pub fn postinit(&mut self)
    {
        trace!("PostInit BaseEngine...");
    }
}
impl<PL> Engine<PL>
{
    pub fn graphics(&self) -> &Graphics { &self.g }
    pub fn graphics_device(&self) -> &br::Device { &self.g.device }
    pub fn graphics_queue_family_index(&self) -> u32 { self.g.graphics_queue.family }
    // 将来的に分かれるかも？
    pub fn transfer_queue_family_index(&self) -> u32 { self.g.graphics_queue.family }
    pub fn backbuffer_format(&self) -> br::vk::VkFormat { self.surface.format() }
    pub fn backbuffers(&self) -> Ref<[br::ImageView]> { Ref::map(self.wrt.get(), |x| x.backbuffers()) }
    pub fn input(&self) -> &InputProcess { &self.ip }
    pub fn input_mut(&mut self) -> &mut InputProcess { &mut self.ip }
    
    pub fn submit_commands<Gen: FnOnce(&mut br::CmdRecord)>(&self, generator: Gen) -> br::Result<()>
    {
        self.g.submit_commands(generator)
    }
    pub fn submit_buffered_commands(&self, batches: &[br::SubmissionBatch], fence: &br::Fence) -> br::Result<()>
    {
        self.g.graphics_queue.q.submit(batches, Some(fence))
    }
}
impl<PL: NativeLinker> Engine<PL>
{
    pub fn load<A: FromAsset>(&self, path: &str) -> Result<A, A::Error>
    {
        A::from_asset(self.nativelink.asset_loader().get(path, A::EXT)?)
    }
    pub fn streaming<A: FromStreamingAsset>(&self, path: &str) -> Result<A, A::Error>
    {
        A::from_asset(self.nativelink.asset_loader().get_streaming(path, A::EXT)?)
    }
    
    pub fn rendering_precision(&self) -> f32 { self.nativelink.rendering_precision() }
}
impl<PL: NativeLinker> Engine<PL>
{
    pub fn do_update<EH: EngineEvents<PL>>(&mut self, userlib: &mut EH)
    {
        let dt = self.gametimer.delta_time();
        
        let wait = br::CompletionHandler::Queue(&self.g.acquiring_backbuffer);
        let bb_index = self.wrt.get().acquire_next_backbuffer_index(None, wait);
        let bb_index = match bb_index
        {
            Err(ref v) if v.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                // Fire resize and do nothing
                let (w, h) = self.nativelink.render_target_provider().current_geometry_extent();
                self.do_resize_backbuffer(math::Vector2(w as _, h as _), userlib);
                return;
            },
            e => e.expect("Acquiring available backbuffer index")
        };
        self.last_rendering_completion.wait().expect("Waiting Last command completion");

        self.ip.prepare_for_frame(dt);

        {
            let (copy_submission, mut fb_submission) = userlib.update(self, bb_index, dt);
            if let Some(mut cs) = copy_submission
            {
                // copy -> render
                cs.signal_semaphores.to_mut().push(&self.g.buffer_ready);
                fb_submission.wait_semaphores.to_mut().extend(vec![
                    (&self.g.acquiring_backbuffer, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT),
                    (&self.g.buffer_ready, br::PipelineStageFlags::VERTEX_SHADER)]);
                fb_submission.signal_semaphores.to_mut().push(&self.g.present_ordering);
                self.submit_buffered_commands(&[cs, fb_submission], self.last_rendering_completion.object())
                    .expect("CommandBuffer Submission");
            }
            else
            {
                // render only(old logic)
                fb_submission.signal_semaphores.to_mut().push(&self.g.present_ordering);
                fb_submission.wait_semaphores.to_mut()
                    .push((&self.g.acquiring_backbuffer, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT));
                self.submit_buffered_commands(&[fb_submission], self.last_rendering_completion.object())
                    .expect("CommandBuffer Submission");
            }
        }
        unsafe { self.last_rendering_completion.signal(); }

        let pr = self.wrt.get().present_on(&self.g.graphics_queue.q, bb_index, &[&self.g.present_ordering]);
        match pr
        {
            Err(ref v) if v.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR =>
            {
                // Fire resize
                let (w, h) = self.nativelink.render_target_provider().current_geometry_extent();
                self.do_resize_backbuffer(math::Vector2(w as _, h as _), userlib);
                return;
            },
            v => v.expect("Present Submission")
        }
    }

    pub fn do_resize_backbuffer<EH: EngineEvents<PL>>(&mut self, new_size: math::Vector2<usize>, userlib: &mut EH)
    {
        self.last_rendering_completion.wait().expect("Waiting Last command completion");
        userlib.discard_backbuffer_resources();
        self.wrt.discard_lw();
        let newrt = WindowRenderTargets::new(self.graphics(), &self.surface, self.nativelink.render_target_provider())
            .expect("Recreating WindowRenderTargets");
        self.wrt.set(newrt);
        self.submit_commands(|r| self.wrt.get().emit_initialize_backbuffers_commands(r))
            .expect("Initializing Backbuffers");
        userlib.on_resize(self, new_size);
    }
}
impl<PL> Drop for Engine<PL>
{
    fn drop(&mut self)
    {
        self.graphics().device.wait().expect("device error");
    }
}

pub struct LateInit<T>(RefCell<Option<T>>);
impl<T> LateInit<T>
{
    pub fn new() -> Self { LateInit(RefCell::new(None)) }
    pub fn init(&self, v: T) { *self.0.borrow_mut() = v.into(); }
    pub fn get(&self) -> Ref<T> { Ref::map(self.0.borrow(), |x| x.as_ref().expect("uninitialized")) }
}
pub struct Discardable<T>(RefCell<Option<T>>);
impl<T> Discardable<T>
{
    pub fn new() -> Self { Discardable(RefCell::new(None)) }
    pub fn set(&self, v: T) { *self.0.borrow_mut() = v.into(); }
    pub fn set_lw(&mut self, v: T) { *self.0.get_mut() = v.into(); }
    pub fn get(&self) -> Ref<T> { Ref::map(self.0.borrow(), |x| x.as_ref().expect("uninitialized")) }
    pub fn get_mut(&self) -> RefMut<T> { RefMut::map(self.0.borrow_mut(), |x| x.as_mut().expect("uninitialized")) }
    pub fn get_mut_lw(&mut self) -> &mut T { self.0.get_mut().as_mut().expect("uninitialized") }
    pub fn discard(&self) { *self.0.borrow_mut() = None; }
    pub fn discard_lw(&mut self) { drop(self.0.get_mut().take()); }
    pub fn is_available(&self) -> bool { self.0.borrow().is_some() }
}
impl<T> From<T> for Discardable<T>
{
    fn from(v: T) -> Self { Discardable(RefCell::new(Some(v))) }
}

/// Queue object with family index
pub struct Queue { q: br::Queue, family: u32 }
/// Graphics manager
pub struct Graphics
{
    pub(self) instance: br::Instance, pub(self) adapter: br::PhysicalDevice, device: br::Device,
    graphics_queue: Queue,
    #[cfg(debug_assertions)] _d: DebugReport,
    cp_onetime_submit: br::CommandPool,
    acquiring_backbuffer: br::Semaphore, present_ordering: br::Semaphore, buffer_ready: br::Semaphore,
    memory_type_index_cache: RefCell<BTreeMap<(u32, u32), u32>>
}
impl Graphics
{
    fn new(appname: &str, appversion: (u32, u32, u32), platform_surface_extension_name: &str,
        features: br::vk::VkPhysicalDeviceFeatures) -> br::Result<Self>
    {
        info!("Supported Layers: ");
        let mut validation_layer_available = false;
        #[cfg(debug_assertions)]
        for l in br::Instance::enumerate_layer_properties().expect("failed to enumerate layer properties")
        {
            let name = unsafe { CStr::from_ptr(l.layerName.as_ptr()) };
            let name_str = name.to_str().expect("unexpected invalid sequence in layer name");
            info!("* {} :: {}/{}", name_str, l.specVersion, l.implementationVersion);
            if name_str == "VK_LAYER_KHRONOS_validation"
            {
                validation_layer_available = true;
            }
        }

        let mut ib = br::InstanceBuilder::new(appname, appversion, "Interlude2:Peridot", (0, 1, 0));
        ib.add_extensions(vec!["VK_KHR_surface", platform_surface_extension_name]);
        #[cfg(debug_assertions)] ib.add_extension("VK_EXT_debug_report");
        if validation_layer_available { ib.add_layer("VK_LAYER_KHRONOS_validation"); }
        else { warn!("Validation Layer is not found!"); }
        let instance = ib.create()?;

        #[cfg(debug_assertions)] let _d = DebugReport::new(&instance)?;
        #[cfg(debug_assertions)] debug!("Debug reporting activated");
        let adapter = instance.iter_physical_devices()?.next().expect("no physical devices");
        Self::diag_memory_properties(&adapter.memory_properties());
        let gqf_index = adapter.queue_family_properties().find_matching_index(br::QueueFlags::GRAPHICS)
            .expect("No graphics queue");
        let qci = br::DeviceQueueCreateInfo(gqf_index, vec![0.0]);
        let device =
        {
            let mut db = br::DeviceBuilder::new(&adapter);
            db.add_extension("VK_KHR_swapchain").add_queue(qci);
            if validation_layer_available { db.add_layer("VK_LAYER_KHRONOS_validation"); }
            *db.mod_features() = features;
            db.create()?
        };
        
        return Ok(Graphics
        {
            buffer_ready: br::Semaphore::new(&device)?,
            present_ordering: br::Semaphore::new(&device)?,
            acquiring_backbuffer: br::Semaphore::new(&device)?,
            cp_onetime_submit: br::CommandPool::new(&device, gqf_index, true, false)?,
            graphics_queue: Queue { q: device.queue(gqf_index, 0), family: gqf_index },
            instance, adapter, device,
            #[cfg(debug_assertions)] _d,
            memory_type_index_cache: RefCell::new(BTreeMap::new())
        });
    }

    fn diag_memory_properties(mp: &br::MemoryProperties)
    {
        info!("Memory Heaps: ");
        for (n, &br::vk::VkMemoryHeap { size, flags }) in mp.heaps().enumerate()
        {
            let (mut nb, mut unit) = (size as f32, "bytes");
            if nb >= 10000.0 { nb /= 1024.0; unit = "KB"; }
            if nb >= 10000.0 { nb /= 1024.0; unit = "MB"; }
            if nb >= 10000.0 { nb /= 1024.0; unit = "GB"; }
            if (flags & br::vk::VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) != 0
            {
                info!("  #{}: {} {} [DEVICE LOCAL]", n, nb, unit);
            }
            else { info!("  #{}: {} {}", n, nb, unit); }
        }
        info!("Memory Types: ");
        for (n, ty) in mp.types().enumerate()
        {
            let mut flags = Vec::with_capacity(6);
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) != 0 { flags.push("DEVICE LOCAL"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) != 0 { flags.push("HOST VISIBLE"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_CACHED_BIT) != 0 { flags.push("CACHED"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) != 0 { flags.push("COHERENT"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_PROTECTED_BIT) != 0 { flags.push("PROTECTED"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT) != 0
            {
                flags.push("LAZILY ALLOCATED");
            }
            info!("  {}: [{}] in heap #{}", n, flags.join("/"), ty.heapIndex);
        }
    }

    pub(self) fn memory_type_index_for(&self, mask: br::MemoryPropertyFlags, index_mask: u32) -> Option<u32>
    {
        if let Some(&mi) = self.memory_type_index_cache.borrow().get(&(mask.bits(), index_mask)) { return Some(mi); }
        for (n, m) in self.adapter.memory_properties().types().enumerate()
        {
            let lazily_allocated = (m.propertyFlags & br::MemoryPropertyFlags::LAZILY_ALLOCATED.bits()) != 0;
            if ((1 << n) & index_mask) != 0 && (m.propertyFlags & mask.bits()) == mask.bits() && !lazily_allocated
            {
                self.memory_type_index_cache.borrow_mut().insert((mask.bits(), index_mask), n as _);
                return Some(n as _);
            }
        }
        return None;
    }
    
    /// Submits any commands as transient commands.
    pub fn submit_commands<Gen: FnOnce(&mut br::CmdRecord)>(&self, generator: Gen) -> br::Result<()>
    {
        let cb = LocalCommandBundle(self.cp_onetime_submit.alloc(1, true)?, &self.cp_onetime_submit);
        generator(&mut cb[0].begin_once()?);
        self.graphics_queue.q.submit(&[br::SubmissionBatch
        {
            command_buffers: Cow::from(&cb[..]), .. Default::default()
        }], None)?;
        self.graphics_queue.q.wait()
    }
}
impl Deref for Graphics
{
    type Target = br::Device;
    fn deref(&self) -> &br::Device { &self.device }
}

struct GameTimer(Option<InstantTimer>);
impl GameTimer
{
    pub fn new() -> Self { GameTimer(None) }
    pub fn delta_time(&mut self) -> Duration
    {
        let d = self.0.as_ref().map_or_else(|| Duration::new(0, 0), |it| it.elapsed());
        self.0 = InstantTimer::now().into();

        return d;
    }
}

struct LocalCommandBundle<'p>(Vec<br::CommandBuffer>, &'p br::CommandPool);
impl<'p> Deref for LocalCommandBundle<'p>
{
    type Target = [br::CommandBuffer];
    fn deref(&self) -> &[br::CommandBuffer] { &self.0 }
}
impl<'p> Drop for LocalCommandBundle<'p>
{
    fn drop(&mut self) { self.1.free(&self.0[..]); }
}

#[derive(Clone, Copy, Debug, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum CBSubmissionType { Graphics, Transfer }
pub struct CommandBundle(Vec<br::CommandBuffer>, br::CommandPool);
impl Deref for CommandBundle
{
    type Target = [br::CommandBuffer];
    fn deref(&self) -> &[br::CommandBuffer] { &self.0 }
}
impl Drop for CommandBundle
{
    fn drop(&mut self) { self.1.free(&self.0[..]); }
}
impl CommandBundle
{
    pub fn new(g: &Graphics, submission_type: CBSubmissionType, count: usize) -> br::Result<Self>
    {
        let qf = match submission_type
        {
            CBSubmissionType::Graphics => g.graphics_queue.family,
            CBSubmissionType::Transfer => g.graphics_queue.family
        };
        let cp = br::CommandPool::new(&g.device, qf, false, false)?;
        return Ok(CommandBundle(cp.alloc(count as _, true)?, cp));
    }
    pub fn reset(&self) -> br::Result<()> { self.1.reset(true) }
}

pub enum SubpassDependencyTemplates {}
impl SubpassDependencyTemplates
{
    pub fn to_color_attachment_in(from_subpass: Option<u32>, occurence_subpass: u32, by_region: bool)
        -> br::vk::VkSubpassDependency
    {
        br::vk::VkSubpassDependency
        {
            dstSubpass: occurence_subpass, srcSubpass: from_subpass.unwrap_or(br::vk::VK_SUBPASS_EXTERNAL),
            dstStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
            dstAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
            dependencyFlags: if by_region { br::vk::VK_DEPENDENCY_BY_REGION_BIT } else { 0 },
            srcStageMask: br::PipelineStageFlags::TOP_OF_PIPE.0,
            .. Default::default()
        }
    }
}

pub enum RenderPassTemplates {}
impl RenderPassTemplates
{
    pub fn single_render(format: br::vk::VkFormat) -> br::RenderPassBuilder
    {
        let mut b = br::RenderPassBuilder::new();
        let adesc =
            br::AttachmentDescription::new(format, br::ImageLayout::PresentSrc, br::ImageLayout::PresentSrc)
            .load_op(br::LoadOp::Clear).store_op(br::StoreOp::Store);
        b.add_attachment(adesc);
        b.add_subpass(br::SubpassDescription::new().add_color_output(0, br::ImageLayout::ColorAttachmentOpt, None));
        b.add_dependency(SubpassDependencyTemplates::to_color_attachment_in(None, 0, true));

        return b;
    }
}

pub trait SpecConstantStorage
{
    fn as_pair(&self) -> (Cow<[br::vk::VkSpecializationMapEntry]>, br::DynamicDataCell);
}

pub struct LayoutedPipeline(br::Pipeline, Rc<br::PipelineLayout>);
impl LayoutedPipeline
{
    pub fn combine(p: br::Pipeline, layout: &Rc<br::PipelineLayout>) -> Self
    {
        LayoutedPipeline(p, layout.clone())
    }
    pub fn pipeline(&self) -> &br::Pipeline { &self.0 }
    pub fn layout(&self) -> &Rc<br::PipelineLayout> { &self.1 }
    pub fn bind(&self, rec: &mut br::CmdRecord) { rec.bind_graphics_pipeline_pair(&self.0, &self.1); }
}

use self::br::vk::VkBufferCopy;
use std::cmp::{PartialEq, Eq, PartialOrd, Ord, Ordering};
use std::hash::{Hash, Hasher};
use std::collections::HashMap;
use std::ops::Range;
#[derive(Clone)] pub struct ResourceKey<T: br::VkHandle>(T);
impl PartialEq for ResourceKey<Buffer>
{
    fn eq(&self, other: &Self) -> bool { (self.0.native_ptr() as u64).eq(&(other.0.native_ptr() as u64)) }
}
impl PartialEq for ResourceKey<Image>
{
    fn eq(&self, other: &Self) -> bool { (self.0.native_ptr() as u64).eq(&(other.0.native_ptr() as u64)) }
}
impl PartialOrd for ResourceKey<Buffer>
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>
    {
        (self.0.native_ptr() as u64).partial_cmp(&(other.0.native_ptr() as u64))
    }
}
impl PartialOrd for ResourceKey<Image> 
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering>
    {
        (self.0.native_ptr() as u64).partial_cmp(&(other.0.native_ptr() as u64))
    }
}
impl Eq for ResourceKey<Buffer> {}
impl Ord for ResourceKey<Buffer>
{
    fn cmp(&self, other: &Self) -> Ordering
    {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl Eq for ResourceKey<Image> {}
impl Ord for ResourceKey<Image>
{
    fn cmp(&self, other: &Self) -> Ordering
    {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl Hash for ResourceKey<Buffer> { fn hash<H: Hasher>(&self, hasher: &mut H) { self.0.native_ptr().hash(hasher) } }
impl Hash for ResourceKey<Image> { fn hash<H: Hasher>(&self, hasher: &mut H) { self.0.native_ptr().hash(hasher) } }
pub struct ReadyResourceBarriers
{
    buffer: Vec<(Buffer, Range<u64>, br::vk::VkAccessFlags)>,
    image: Vec<(Image, br::ImageSubresourceRange, br::ImageLayout)>
}
impl ReadyResourceBarriers
{
    fn new() -> Self { ReadyResourceBarriers { buffer: Vec::new(), image: Vec::new() } }
}
/// Batching Manager for Transferring Operations.
pub struct TransferBatch
{
    barrier_range_src: BTreeMap<ResourceKey<Buffer>, Range<br::vk::VkDeviceSize>>,
    barrier_range_dst: BTreeMap<ResourceKey<Buffer>, Range<br::vk::VkDeviceSize>>,
    org_layout_src: BTreeMap<ResourceKey<Image>, br::ImageLayout>,
    org_layout_dst: BTreeMap<ResourceKey<Image>, br::ImageLayout>,
    copy_buffers: HashMap<(ResourceKey<Buffer>, ResourceKey<Buffer>), Vec<VkBufferCopy>>,
    init_images: BTreeMap<ResourceKey<Image>, (Buffer, br::vk::VkDeviceSize)>,
    ready_barriers: BTreeMap<br::PipelineStageFlags, ReadyResourceBarriers>
}
impl TransferBatch
{
    pub fn new() -> Self
    {
        TransferBatch
        {
            barrier_range_src: BTreeMap::new(), barrier_range_dst: BTreeMap::new(),
            org_layout_src: BTreeMap::new(), org_layout_dst: BTreeMap::new(),
            copy_buffers: HashMap::new(), init_images: BTreeMap::new(),
            ready_barriers: BTreeMap::new()
        }
    }

    /// Add copying operation between buffers.
    pub fn add_copying_buffer(&mut self, src: DeviceBufferView, dst: DeviceBufferView, bytes: br::vk::VkDeviceSize) {
        trace!("Registering COPYING-BUFFER: ({}, {}) -> {} bytes", src.offset, dst.offset, bytes);
        let (sk, dk) = (ResourceKey(src.buffer.clone()), ResourceKey(dst.buffer.clone()));
        Self::update_barrier_range_for(&mut self.barrier_range_src, sk.clone(), src.range(bytes));
        Self::update_barrier_range_for(&mut self.barrier_range_dst, dk.clone(), dst.range(bytes));
        self.copy_buffers.entry((sk, dk)).or_insert_with(Vec::new).push(VkBufferCopy {
            srcOffset: src.offset, dstOffset: dst.offset, size: bytes
        });
    }
    /// Add copying operation between buffers.
    /// Shorthand for copying operation that both BufferViews have same offset.
    pub fn add_mirroring_buffer(
        &mut self,
        src: &Buffer,
        dst: &Buffer,
        offset: br::vk::VkDeviceSize,
        bytes: br::vk::VkDeviceSize
    ) {
        self.add_copying_buffer(src.with_dev_offset(offset), dst.with_dev_offset(offset), bytes);
    }
    /// Add image content initializing operation, from the buffer.
    pub fn init_image_from(&mut self, dest: &Image, src: DeviceBufferView)
    {
        self.init_images.insert(ResourceKey(dest.clone()), (src.buffer.clone(), src.offset));
        let size = (dest.size().0 * dest.size().1) as u64 * (dest.format().bpp() >> 3) as u64;
        Self::update_barrier_range_for(&mut self.barrier_range_src, ResourceKey(src.buffer.clone()), src.range(size));
        self.org_layout_dst.insert(ResourceKey(dest.clone()), br::ImageLayout::Preinitialized);
    }

    /// Add ready barrier for buffers.
    pub fn add_buffer_graphics_ready(&mut self, dest_stage: br::PipelineStageFlags,
        res: &Buffer, byterange: Range<br::vk::VkDeviceSize>, access_grants: br::vk::VkAccessFlags)
    {
        self.ready_barriers.entry(dest_stage).or_insert_with(ReadyResourceBarriers::new)
            .buffer.push((res.clone(), byterange, access_grants));
    }
    /// Add ready barrier for images.
    pub fn add_image_graphics_ready(&mut self, dest_stage: br::PipelineStageFlags,
        res: &Image, layout: br::ImageLayout)
    {
        self.ready_barriers.entry(dest_stage).or_insert_with(ReadyResourceBarriers::new)
            .image.push((res.clone(), br::ImageSubresourceRange::color(0, 0), layout));
    }
    /// Have add_copying_buffer, add_mirroring_buffer or init_image_from been called?
    pub fn has_copy_ops(&self) -> bool { !self.copy_buffers.is_empty() || !self.init_images.is_empty() }
    /// Have add_buffer_graphics_ready or add_image_graphics_ready been called?
    pub fn has_ready_barrier_ops(&self) -> bool { !self.ready_barriers.is_empty() }
    
    fn update_barrier_range_for(map: &mut BTreeMap<ResourceKey<Buffer>, Range<br::vk::VkDeviceSize>>,
        k: ResourceKey<Buffer>, new_range: Range<br::vk::VkDeviceSize>)
    {
        let r = map.entry(k).or_insert_with(|| new_range.clone());
        r.start = r.start.min(new_range.start);
        r.end = r.end.max(new_range.end);
    }
}
/// Sinking Commands into CommandBuffers
impl TransferBatch
{
    pub fn sink_transfer_commands(&self, r: &mut br::CmdRecord)
    {
        let src_barriers = self.barrier_range_src.iter()
            .map(|(b, r)| br::BufferMemoryBarrier::new(
                &b.0, r.clone(), br::AccessFlags::HOST.write, br::AccessFlags::TRANSFER.read
            ));
        let dst_barriers = self.barrier_range_dst.iter()
            .map(|(b, r)| br::BufferMemoryBarrier::new(
                &b.0, r.clone(), 0, br::AccessFlags::TRANSFER.write
            ));
        let barriers: Vec<_> = src_barriers.chain(dst_barriers).collect();
        let src_barriers_i = self.org_layout_src.iter().map(|(b, &l0)| br::ImageMemoryBarrier::new(
            &br::ImageSubref::color(&b.0, 0, 0), l0, br::ImageLayout::TransferSrcOpt
        ));
        let dst_barriers_i = self.org_layout_dst.iter().map(|(b, &l0)| br::ImageMemoryBarrier::new(
            &br::ImageSubref::color(&b.0, 0, 0), l0, br::ImageLayout::TransferDestOpt
        ));
        let barriers_i: Vec<_> = src_barriers_i.chain(dst_barriers_i).collect();
        
        r.pipeline_barrier(br::PipelineStageFlags::HOST, br::PipelineStageFlags::TRANSFER, false,
            &[], &barriers, &barriers_i);
        for (&(ref s, ref d), ref rs) in &self.copy_buffers { r.copy_buffer(&s.0, &d.0, &rs); }
        for (d, s) in &self.init_images {
            trace!("Copying Image: extent={:?}", br::vk::VkExtent3D::from(d.0.size().clone()));
            r.copy_buffer_to_image(&s.0, &d.0, br::ImageLayout::TransferDestOpt, &[br::vk::VkBufferImageCopy {
                bufferOffset: s.1, bufferRowLength: 0, bufferImageHeight: 0,
                imageSubresource: br::vk::VkImageSubresourceLayers::default(),
                imageOffset: br::vk::VkOffset3D { x: 0, y: 0, z: 0 },
                imageExtent: d.0.size().clone().into()
            }]);
        }
    }
    pub fn sink_graphics_ready_commands(&self, r: &mut br::CmdRecord)
    {
        for (&stg, &ReadyResourceBarriers { ref buffer, ref image, .. }) in &self.ready_barriers
        {
            let buf_barriers: Vec<_> = buffer.iter()
                .map(|&(ref r, ref br, a)| br::BufferMemoryBarrier::new(&r, br.start as _ .. br.end as _,
                    br::AccessFlags::TRANSFER.read, a)).collect();
            let img_barriers: Vec<_> = image.iter()
                .map(|(r, range, l)| br::ImageMemoryBarrier::new_raw(&r, range, br::ImageLayout::TransferDestOpt, *l))
                .collect();
            r.pipeline_barrier(br::PipelineStageFlags::TRANSFER, stg, false, &[], &buf_barriers, &img_barriers);
        }
    }
}

/// Batching mechanism for Updating Descriptor Sets
pub struct DescriptorSetUpdateBatch(Vec<br::DescriptorSetWriteInfo>, Vec<br::DescriptorSetCopyInfo>);
impl DescriptorSetUpdateBatch
{
    /// Create an Empty batch
    pub fn new() -> Self { DescriptorSetUpdateBatch(Vec::new(), Vec::new()) }
    /// Write an information to bound index and array index in destination.
    pub fn write_index(&mut self, dest: br::vk::VkDescriptorSet, bound: u32, array: u32, info: br::DescriptorUpdateInfo)
        -> &mut Self
    {
        self.0.push(br::DescriptorSetWriteInfo(dest, bound, array, info));
        return self;
    }
    /// Write an information to bound index in destination.
    pub fn write(&mut self, dest: br::vk::VkDescriptorSet, bound: u32, info: br::DescriptorUpdateInfo) -> &mut Self
    {
        self.write_index(dest, bound, 0, info)
    }
    /// Submit entire batches
    pub fn submit(&self, d: &br::Device) { d.update_descriptor_sets(&self.0, &self.1); }
}
