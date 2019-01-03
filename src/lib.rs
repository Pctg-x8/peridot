#![allow(dead_code)]

// extern crate font_kit;
#[macro_use] extern crate log;
extern crate libc;

extern crate pathfinder_partitioner;
extern crate bedrock;
pub extern crate peridot_math as math;
pub extern crate peridot_vertex_processing_pack as vertex_processing_pack;
pub extern crate peridot_archive as archive;

use bedrock as br; use bedrock::traits::*;
use std::ops::Deref;
use std::rc::Rc;
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::io::Result as IOResult;
use std::cell::{Ref, RefMut, RefCell};

mod window; use self::window::WindowRenderTargets;
pub use self::window::{PlatformRenderTarget, SurfaceInfo};
mod resource; pub use self::resource::*;
#[cfg(debug_assertions)] mod debug; #[cfg(debug_assertions)] use self::debug::DebugReport;
pub mod utils; pub use self::utils::*;

mod asset; pub use self::asset::*;
mod input; pub use self::input::*;

pub trait PluginLoader {
    type AssetLoader: PlatformAssetLoader;
    type InputProcessor: InputProcessPlugin;
    type RenderTargetProvider: PlatformRenderTarget;

    fn new_asset_loader(&self) -> Self::AssetLoader;
    fn new_render_target_provider(&self) -> Self::RenderTargetProvider;
    fn input_processor(&mut self) -> &mut Self::InputProcessor;
}
pub struct NativeLink<AL: PlatformAssetLoader, PRT: PlatformRenderTarget> {
    prt: PRT, asset_loader: AL
}
pub trait PlatformLinker {
    type AssetLoader: PlatformAssetLoader;
    type RenderTargetProvider: PlatformRenderTarget;

    fn new(al: Self::AssetLoader, prt: Self::RenderTargetProvider) -> Self;
    fn asset_loader(&self) -> &Self::AssetLoader;
    fn render_target_provider(&self) -> &Self::RenderTargetProvider;
}
impl<AL: PlatformAssetLoader, PRT: PlatformRenderTarget> PlatformLinker for NativeLink<AL, PRT> {
    type AssetLoader = AL;
    type RenderTargetProvider = PRT;

    fn new(al: AL, prt: PRT) -> Self {
        NativeLink { asset_loader: al, prt }
    }
    fn asset_loader(&self) -> &AL { &self.asset_loader }
    fn render_target_provider(&self) -> &PRT { &self.prt }
}

pub trait EngineEvents<PL: PlatformLinker> : Sized {
    fn init(_e: &Engine<Self, PL>) -> Self;
    /// Updates the game and passes copying(optional) and rendering command batches to the engine.
    fn update(&mut self, _e: &Engine<Self, PL>, _on_backbuffer_of: u32)
            -> (Option<br::SubmissionBatch>, br::SubmissionBatch) {
        (None, br::SubmissionBatch::default())
    }
    /// Discards backbuffer-dependent resources(i.e. Framebuffers or some of CommandBuffers)
    fn discard_backbuffer_resources(&mut self) {}
    /// Called when backbuffer has resized
    /// (called after discard_backbuffer_resources so re-create discarded resources here)
    fn on_resize(&mut self, _e: &Engine<Self, PL>, _new_size: math::Vector2<usize>) {}
}
impl<PL: PlatformLinker> EngineEvents<PL> for () {
    fn init(_e: &Engine<Self, PL>) -> Self { () }
}

pub struct Engine<E: EngineEvents<PL>, PL: PlatformLinker> {
    nativelink: PL, surface: SurfaceInfo, wrt: Discardable<WindowRenderTargets>,
    pub(self) g: Graphics, event_handler: Option<RefCell<E>>, ip: Rc<InputProcess>
}
impl<E: EngineEvents<NPL>, NPL: PlatformLinker> Engine<E, NPL> {
    pub fn launch<PL>(name: &str, version: (u32, u32, u32), plugin_loader: &mut PL) -> br::Result<Self>
            where PL: PluginLoader<AssetLoader=NPL::AssetLoader, RenderTargetProvider=NPL::RenderTargetProvider> {
        let nativelink = NPL::new(plugin_loader.new_asset_loader(), plugin_loader.new_render_target_provider());
        let g = Graphics::new(name, version, nativelink.render_target_provider().surface_extension_name())?;
        let surface = nativelink.render_target_provider().create_surface(&g.instance, &g.adapter,
            g.graphics_queue.family)?;
        trace!("Creating WindowRenderTargets...");
        let wrt = WindowRenderTargets::new(&g, &surface, nativelink.render_target_provider())?.into();
        let mut this = Engine {
            nativelink, g, surface, wrt, event_handler: None, ip: InputProcess::new().into()
        };
        trace!("Initializing Game...");
        let eh = E::init(&this);
        this.submit_commands(|r| this.wrt.get().emit_initialize_backbuffers_commands(r))
            .expect("Initializing Backbuffers");
        this.event_handler = Some(eh.into());
        plugin_loader.input_processor().on_start_handle(&this.ip);
        return Ok(this);
    }
    fn userlib_mut(&self) -> RefMut<E> { self.event_handler.as_ref().expect("uninitialized userlib").borrow_mut() }
    fn userlib_mut_lw(&mut self) -> &mut E { self.event_handler.as_mut().expect("uninitialized userlib").get_mut() }

    pub fn load<A: FromAsset>(&self, path: &str) -> IOResult<A> {
        self.nativelink.asset_loader().get(path, A::EXT).and_then(A::from_asset)
    }
    pub fn streaming<A: FromStreamingAsset>(&self, path: &str) -> IOResult<A> {
        self.nativelink.asset_loader().get_streaming(path, A::EXT).and_then(A::from_asset)
    }

    pub fn graphics(&self) -> &Graphics { &self.g }
    pub fn graphics_device(&self) -> &br::Device { &self.g.device }
    pub fn graphics_queue_family_index(&self) -> u32 { self.g.graphics_queue.family }
    // 将来的に分かれるかも？
    pub fn transfer_queue_family_index(&self) -> u32 { self.g.graphics_queue.family }
    pub fn backbuffer_format(&self) -> br::vk::VkFormat { self.surface.format() }
    pub fn backbuffers(&self) -> Ref<[br::ImageView]> { Ref::map(self.wrt.get(), |x| x.backbuffers()) }
    pub fn input(&self) -> &InputProcess { &self.ip }
    
    pub fn submit_commands<Gen: FnOnce(&mut br::CmdRecord)>(&self, generator: Gen) -> br::Result<()> {
        self.g.submit_commands(generator)
    }
    pub fn submit_buffered_commands(&self, batches: &[br::SubmissionBatch], fence: &br::Fence) -> br::Result<()> {
        self.g.graphics_queue.q.submit(batches, Some(fence))
    }

    pub fn do_update(&mut self)
    {
        let wait = br::CompletionHandler::Device(&self.g.acquiring_backbuffer);
        let bb_index = self.wrt.get().acquire_next_backbuffer_index(None, wait);
        match bb_index {
            Err(ref v) if v.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                // Fire resize and do nothing
                let (w, h) = self.nativelink.render_target_provider().current_geometry_extent();
                self.do_resize_backbuffer(math::Vector2(w as _, h as _));
                return;
            }
            _ => ()
        };
        let bb_index = bb_index.expect("Acquiring available backbuffer index");
        self.wrt.get_mut_lw().command_completion_for_backbuffer_mut(bb_index as _)
            .wait().expect("Waiting Previous command completion");
        self.ip.prepare_for_frame();
        {
            let bound_wrt = self.wrt.get();

            let mut eh_mut = self.event_handler.as_ref().expect("uninitialized").borrow_mut();
            let (copy_submission, mut fb_submission) = eh_mut.update(self, bb_index);
            if let Some(mut cs) = copy_submission {
                // copy -> render
                cs.signal_semaphores.to_mut().push(&self.g.buffer_ready);
                fb_submission.wait_semaphores.to_mut().extend(vec![
                    (&self.g.acquiring_backbuffer, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT),
                    (&self.g.buffer_ready, br::PipelineStageFlags::VERTEX_SHADER)]);
                fb_submission.signal_semaphores.to_mut().push(&self.g.present_ordering);
                let completion_fence = bound_wrt.command_completion_for_backbuffer(bb_index as _);
                self.submit_buffered_commands(&[cs, fb_submission], completion_fence.object())
                    .expect("CommandBuffer Submission");
            }
            else {
                // render only(old logic)
                fb_submission.signal_semaphores.to_mut().push(&self.g.present_ordering);
                fb_submission.wait_semaphores.to_mut()
                    .push((&self.g.acquiring_backbuffer, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT));
                let completion_fence = bound_wrt.command_completion_for_backbuffer(bb_index as _);
                self.submit_buffered_commands(&[fb_submission], completion_fence.object())
                    .expect("CommandBuffer Submission");
            }
        }
        unsafe {
            self.wrt.get_mut_lw().command_completion_for_backbuffer_mut(bb_index as _).signal();
        }
        self.wrt.get().present_on(&self.g.graphics_queue.q, bb_index, &[&self.g.present_ordering])
            .expect("Present Submission");
    }
    pub fn do_resize_backbuffer(&mut self, new_size: math::Vector2<usize>) {
        self.wrt.get_mut_lw().wait_all_command_completion_for_backbuffer().expect("Waiting queued commands");
        self.userlib_mut_lw().discard_backbuffer_resources();
        self.wrt.discard_lw();
        self.wrt.set(
            WindowRenderTargets::new(self.graphics(), &self.surface, self.nativelink.render_target_provider())
            .expect("Recreating WindowRenderTargets"));
        self.submit_commands(|r| self.wrt.get().emit_initialize_backbuffers_commands(r))
            .expect("Initializing Backbuffers");
        self.userlib_mut().on_resize(&self, new_size);
    }
}
impl<E: EngineEvents<PL>, PL: PlatformLinker> Drop for Engine<E, PL> {
    fn drop(&mut self) {
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
impl<T> From<T> for Discardable<T> {
    fn from(v: T) -> Self { Discardable(RefCell::new(Some(v))) }
}

pub struct Queue { q: br::Queue, family: u32 }
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
    fn new(appname: &str, appversion: (u32, u32, u32), platform_surface_extension_name: &str) -> br::Result<Self>
    {
        info!("Supported Layers: ");
        let mut validation_layer_available = false;
        for l in br::Instance::enumerate_layer_properties().expect("failed to enumerate layer properties") {
            let name = unsafe { ::std::ffi::CStr::from_ptr(l.layerName.as_ptr()) };
            info!("* {} :: {}/{}", name.to_string_lossy(), l.specVersion, l.implementationVersion);
            if !validation_layer_available && name.to_str() == Ok("VK_LAYER_LUNARG_standard_validation") {
                validation_layer_available = true;
            }
        }

        let mut ib = br::InstanceBuilder::new(appname, appversion, "Interlude2:Peridot", (0, 1, 0));
        ib.add_extensions(vec!["VK_KHR_surface", platform_surface_extension_name]);
        #[cfg(debug_assertions)] ib.add_extension("VK_EXT_debug_report");
        if validation_layer_available {
            #[cfg(all(debug_assertions, not(target_os = "android")))] ib.add_layer("VK_LAYER_LUNARG_standard_validation");
            #[cfg(all(debug_assertions, target_os = "android"))] ib
                .add_layer("VK_LAYER_LUNARG_parameter_validation")
                .add_layer("VK_LAYER_LUNARG_core_validation")
                .add_layer("VK_LAYER_LUNARG_object_tracker")
                .add_layer("VK_LAYER_GOOGLE_unique_objects")
                .add_layer("VK_LAYER_GOOGLE_threading");
        }
        else {
            warn!("Validation Layer is not found!");
        }
        let instance = ib.create()?;
        #[cfg(debug_assertions)] let _d = DebugReport::new(&instance)?;
        #[cfg(debug_assertions)] debug!("Debug reporting activated");
        let adapter = instance.iter_physical_devices()?.next().expect("no physical devices");
        Self::diag_memory_properties(&adapter.memory_properties());
        let gqf_index = adapter.queue_family_properties().find_matching_index(br::QueueFlags::GRAPHICS)
            .expect("No graphics queue");
        let qci = br::DeviceQueueCreateInfo(gqf_index, vec![0.0]);
        let device = {
            let mut db = br::DeviceBuilder::new(&adapter);
            db.add_extension("VK_KHR_swapchain").add_queue(qci);
            #[cfg(debug_assertions)] db.add_layer("VK_LAYER_LUNARG_standard_validation");
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

    fn diag_memory_properties(mp: &br::MemoryProperties) {
        info!("Memory Heaps: ");
        for (n, &br::vk::VkMemoryHeap { size, flags }) in mp.heaps().enumerate() {
            let (mut nb, mut unit) = (size as f32, "bytes");
            if nb >= 10000.0 { nb /= 1024.0; unit = "KB"; }
            if nb >= 10000.0 { nb /= 1024.0; unit = "MB"; }
            if nb >= 10000.0 { nb /= 1024.0; unit = "GB"; }
            if (flags & br::vk::VK_MEMORY_HEAP_DEVICE_LOCAL_BIT) != 0 {
                info!("  #{}: {} {} [DEVICE LOCAL]", n, nb, unit);
            }
            else {
                info!("  #{}: {} {}", n, nb, unit);
            }
        }
        info!("Memory Types: ");
        for (n, ty) in mp.types().enumerate() {
            let mut flags = Vec::with_capacity(6);
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT) != 0 { flags.push("DEVICE LOCAL"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT) != 0 { flags.push("HOST VISIBLE"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_CACHED_BIT) != 0 { flags.push("CACHED"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_HOST_COHERENT_BIT) != 0 { flags.push("COHERENT"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_PROTECTED_BIT) != 0 { flags.push("PROTECTED"); }
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT) != 0 {
                flags.push("LAZILY ALLOCATED");
            }
            info!("  {}: [{}] in heap #{}", n, flags.join("/"), ty.heapIndex);
        }
    }

    pub(self) fn memory_type_index_for(&self, mask: br::MemoryPropertyFlags, index_mask: u32) -> Option<u32> {
        if let Some(&mi) = self.memory_type_index_cache.borrow().get(&(mask.bits(), index_mask)) { return Some(mi); }
        for (n, m) in self.adapter.memory_properties().types().enumerate() {
            if ((1 << n) & index_mask) != 0 && (m.propertyFlags & mask.bits()) == mask.bits() &&
                    (m.propertyFlags & br::MemoryPropertyFlags::LAZILY_ALLOCATED.bits()) == 0 {
                self.memory_type_index_cache.borrow_mut().insert((mask.bits(), index_mask), n as _);
                return Some(n as _);
            }
        }
        return None;
    }
    
    fn submit_commands<Gen: FnOnce(&mut br::CmdRecord)>(&self, generator: Gen) -> br::Result<()>
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
impl Deref for Graphics {
    type Target = br::Device;
    fn deref(&self) -> &br::Device { &self.device }
}

struct LocalCommandBundle<'p>(Vec<br::CommandBuffer>, &'p br::CommandPool);
impl<'p> ::std::ops::Deref for LocalCommandBundle<'p>
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
impl ::std::ops::Deref for CommandBundle
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
        let qf = match submission_type {
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

use std::ffi::CString;
use vertex_processing_pack::PvpContainer;
pub struct PvpShaderModules {
    bindings: Vec<br::vk::VkVertexInputBindingDescription>, attributes: Vec<br::vk::VkVertexInputAttributeDescription>,
    vertex: br::ShaderModule, fragment: Option<br::ShaderModule>
}
impl PvpShaderModules {
    pub fn new(device: &br::Device, container: PvpContainer) -> br::Result<Self> {
        Ok(PvpShaderModules {
            vertex: br::ShaderModule::from_memory(device, &container.vertex_shader)?,
            fragment: if let Some(b) = container.fragment_shader {
                Some(br::ShaderModule::from_memory(device, &b)?)
            }
            else { None },
            bindings: container.vertex_bindings, attributes: container.vertex_attributes
        })
    }
    pub fn generate_vps(&self, primitive_topo: br::vk::VkPrimitiveTopology) -> br::VertexProcessingStages {
        let mut r = br::VertexProcessingStages::new(br::PipelineShader
        {
            module: &self.vertex, entry_name: CString::new("main").expect("unreachable"), specinfo: None
        }, &self.bindings, &self.attributes, primitive_topo);
        if let Some(ref f) = self.fragment {
            r.fragment_shader(br::PipelineShader {
                module: f, entry_name: CString::new("main").expect("unreachable"), specinfo: None
            });
        }
        return r;
    }
}

pub struct LayoutedPipeline(br::Pipeline, Rc<br::PipelineLayout>);
impl LayoutedPipeline {
    pub fn combine(p: br::Pipeline, layout: &Rc<br::PipelineLayout>) -> Self {
        LayoutedPipeline(p, layout.clone())
    }
    pub fn pipeline(&self) -> &br::Pipeline { &self.0 }
    pub fn layout(&self) -> &br::PipelineLayout { &self.1 }
    pub fn bind(&self, rec: &mut br::CmdRecord) { rec.bind_graphics_pipeline_pair(&self.0, &self.1); }
}

use self::br::vk::VkBufferCopy;
use std::cmp::{PartialEq, Eq, PartialOrd, Ord, Ordering};
use std::hash::{Hash, Hasher};
use std::collections::HashMap;
use std::ops::Range;
#[derive(Clone)] pub struct ResourceKey<T: br::VkHandle>(T);
impl PartialEq for ResourceKey<Buffer> {
    fn eq(&self, other: &Self) -> bool { (self.0.native_ptr() as u64).eq(&(other.0.native_ptr() as u64)) }
}
impl PartialEq for ResourceKey<Image> {
    fn eq(&self, other: &Self) -> bool { (self.0.native_ptr() as u64).eq(&(other.0.native_ptr() as u64)) }
}
impl PartialOrd for ResourceKey<Buffer> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.0.native_ptr() as u64).partial_cmp(&(other.0.native_ptr() as u64))
    }
}
impl PartialOrd for ResourceKey<Image> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.0.native_ptr() as u64).partial_cmp(&(other.0.native_ptr() as u64))
    }
}
impl Eq for ResourceKey<Buffer> {}
impl Ord for ResourceKey<Buffer> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl Eq for ResourceKey<Image> {}
impl Ord for ResourceKey<Image> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).expect("ord: unreachable")
    }
}
impl Hash for ResourceKey<Buffer> { fn hash<H: Hasher>(&self, hasher: &mut H) { self.0.native_ptr().hash(hasher) } }
impl Hash for ResourceKey<Image> { fn hash<H: Hasher>(&self, hasher: &mut H) { self.0.native_ptr().hash(hasher) } }
pub struct ReadyResourceBarriers {
    buffer: Vec<(Buffer, Range<u64>, br::vk::VkAccessFlags)>,
    image: Vec<(Image, br::ImageSubresourceRange, br::ImageLayout)>
}
impl ReadyResourceBarriers {
    fn new() -> Self { ReadyResourceBarriers { buffer: Vec::new(), image: Vec::new() } }
}
pub struct TransferBatch {
    barrier_range_src: BTreeMap<ResourceKey<Buffer>, Range<u64>>,
    barrier_range_dst: BTreeMap<ResourceKey<Buffer>, Range<u64>>,
    copy_buffers: HashMap<(ResourceKey<Buffer>, ResourceKey<Buffer>), Vec<VkBufferCopy>>,
    ready_barriers: BTreeMap<br::PipelineStageFlags, ReadyResourceBarriers>
}
impl TransferBatch {
    pub fn new() -> Self {
        TransferBatch {
            barrier_range_src: BTreeMap::new(), barrier_range_dst: BTreeMap::new(),
            copy_buffers: HashMap::new(),
            ready_barriers: BTreeMap::new()
        }
    }
    pub fn add_copying_buffer(&mut self, src: (&Buffer, u64), dst: (&Buffer, u64), bytes: u64) {
        trace!("Registering COPYING-BUFFER: ({}, {}) -> {} bytes", src.1, dst.1, bytes);
        let (sk, dk) = (ResourceKey(src.0.clone()), ResourceKey(dst.0.clone()));
        Self::update_barrier_range_for(&mut self.barrier_range_src, sk.clone(), src.1 .. src.1 + bytes);
        Self::update_barrier_range_for(&mut self.barrier_range_dst, dk.clone(), dst.1 .. dst.1 + bytes);
        self.copy_buffers.entry((sk, dk)).or_insert_with(Vec::new)
            .push(VkBufferCopy { srcOffset: src.1 as _, dstOffset: dst.1 as _, size: bytes as _ })
    }
    pub fn add_mirroring_buffer(&mut self, src: &Buffer, dst: &Buffer, offset: u64, bytes: u64) {
        self.add_copying_buffer((src, offset), (dst, offset), bytes);
    }
    pub fn add_buffer_graphics_ready(&mut self, dest_stage: br::PipelineStageFlags,
        res: &Buffer, byterange: Range<u64>, access_grants: br::vk::VkAccessFlags) {
        self.ready_barriers.entry(dest_stage).or_insert_with(ReadyResourceBarriers::new)
            .buffer.push((res.clone(), byterange, access_grants));
    }
    pub fn is_empty(&self) -> bool { self.copy_buffers.is_empty() }
    
    fn update_barrier_range_for(map: &mut BTreeMap<ResourceKey<Buffer>, Range<u64>>,
            k: ResourceKey<Buffer>, new_range: Range<u64>) {
        let r = map.entry(k).or_insert_with(|| new_range.clone());
        r.start = r.start.min(new_range.start);
        r.end = r.end.max(new_range.end);
    }
}
/// Sinking Commands into CommandBuffers
impl TransferBatch {
    pub fn sink_transfer_commands(&self, r: &mut br::CmdRecord) {
        let src_barriers = self.barrier_range_src.iter()
            .map(|(b, r)| br::BufferMemoryBarrier::new(
                &b.0, r.start as _ .. r.end as _, br::AccessFlags::HOST.write, br::AccessFlags::TRANSFER.read));
        let dst_barriers = self.barrier_range_dst.iter()
            .map(|(b, r)| br::BufferMemoryBarrier::new(
                &b.0, r.start as _ .. r.end as _, 0, br::AccessFlags::TRANSFER.write));
        let barriers: Vec<_> = src_barriers.chain(dst_barriers).collect();
        
        r.pipeline_barrier(br::PipelineStageFlags::HOST, br::PipelineStageFlags::TRANSFER, false,
            &[], &barriers, &[]);
        for (&(ref s, ref d), ref rs) in &self.copy_buffers { r.copy_buffer(&s.0, &d.0, &rs); }
    }
    pub fn sink_graphics_ready_commands(&self, r: &mut br::CmdRecord) {
        for (&stg, &ReadyResourceBarriers { ref buffer, .. }) in &self.ready_barriers {
            let buf_barriers: Vec<_> = buffer.iter()
                .map(|&(ref r, ref br, a)| br::BufferMemoryBarrier::new(&r, br.start as _ .. br.end as _,
                    br::AccessFlags::TRANSFER.read, a)).collect();
            r.pipeline_barrier(br::PipelineStageFlags::TRANSFER, stg, false,
                &[], &buf_barriers, &[]);
        }
    }
}

/// Batching mechanism for Updating Descriptor Sets
pub struct DescriptorSetUpdateBatch(Vec<br::DescriptorSetWriteInfo>, Vec<br::DescriptorSetCopyInfo>);
impl DescriptorSetUpdateBatch {
    /// Create an Empty batch
    pub fn new() -> Self { DescriptorSetUpdateBatch(Vec::new(), Vec::new()) }
    /// Write an information to bound index and array index in destination.
    pub fn write_index(&mut self, dest: br::vk::VkDescriptorSet,
            bound: u32, array: u32, info: br::DescriptorUpdateInfo) -> &mut Self {
        self.0.push(br::DescriptorSetWriteInfo(dest, bound, array, info));
        return self;
    }
    /// Write an information to bound index in destination.
    pub fn write(&mut self, dest: br::vk::VkDescriptorSet, bound: u32, info: br::DescriptorUpdateInfo) -> &mut Self {
        self.write_index(dest, bound, 0, info)
    }
    /// Submit entire batches
    pub fn submit(&self, d: &br::Device) { d.update_descriptor_sets(&self.0, &self.1); }
}
