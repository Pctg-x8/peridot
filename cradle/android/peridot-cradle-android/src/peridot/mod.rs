#![allow(dead_code)]

use bedrock as br; use bedrock::traits::*;
use std::rc::Rc;
use std::borrow::Cow;
use std::collections::BTreeMap;

mod window; use self::window::WindowRenderTargets;
pub use self::window::{PlatformRenderTarget, SurfaceInfo};
mod resource; pub use self::resource::*;
#[cfg(debug_assertions)] mod debug; #[cfg(debug_assertions)] use self::debug::DebugReport;

pub trait EngineEvents<AL: AssetLoader, PRT: PlatformRenderTarget> : Sized {
    fn init(_e: &Engine<Self, AL, PRT>) -> Self;
    fn update(&self, _e: &Engine<Self, AL, PRT>, _on_backbuffer_of: u32) -> br::SubmissionBatch { br::SubmissionBatch::default() }
}
impl<AL: AssetLoader, PRT: PlatformRenderTarget> EngineEvents<AL, PRT> for () { fn init(_e: &Engine<Self, AL, PRT>) -> Self { () } }

use std::io::{Read, Seek, Result as IOResult, BufReader};
pub trait AssetLoader {
    type Asset: Read + Seek;
    type StreamingAsset: Read;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>;
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::StreamingAsset>;
}
pub trait LogicalAssetData: Sized {
    fn ext() -> &'static str;
}
pub trait FromAsset: LogicalAssetData {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> IOResult<Self>;
}
pub trait FromStreamingAsset: LogicalAssetData {
    fn from_asset<Asset: Read>(asset: Asset) -> IOResult<Self>;
}
use peridot_vertex_processing_pack::*;
impl LogicalAssetData for PvpContainer { fn ext() -> &'static str { "pvp" } }
impl FromAsset for PvpContainer {
    fn from_asset<Asset: Read + Seek>(asset: Asset) -> IOResult<Self> {
        PvpContainerReader::new(BufReader::new(asset)).and_then(PvpContainerReader::into_container)
    }
}

mod input; pub use self::input::*;

pub struct Engine<E: EngineEvents<AL, PRT>, AL: AssetLoader, PRT: PlatformRenderTarget> {
    prt: PRT, surface: SurfaceInfo, wrt: WindowRenderTargets,
    pub(self) g: Graphics, event_handler: Option<E>, asset_loader: AL, ip: Rc<InputProcess>
}
impl<E: EngineEvents<AL, PRT>, AL: AssetLoader, PRT: PlatformRenderTarget> Engine<E, AL, PRT> {
    pub fn launch<IPP: InputProcessPlugin>(name: &str, version: (u32, u32, u32), prt: PRT, asset_loader: AL, ipp: &mut IPP)
            -> br::Result<Self> {
        let g = Graphics::new(name, version)?;
        let surface = prt.create_surface(&g.instance, &g.adapter, g.graphics_queue.family)?;
        let wrt = WindowRenderTargets::new(&g, &surface, &prt)?;
        let mut this = Engine { g, surface, wrt, event_handler: None, asset_loader, prt, ip: InputProcess::new().into() };
        let eh = E::init(&this);
        this.event_handler = Some(eh);
        ipp.on_start_handle(&this.ip);
        return Ok(this);
    }

    pub fn load<A: FromAsset>(&self, path: &str) -> IOResult<A> {
        self.asset_loader.get(path, A::ext()).and_then(A::from_asset)
    }
    pub fn streaming<A: FromStreamingAsset>(&self, path: &str) -> IOResult<A> {
        self.asset_loader.get_streaming(path, A::ext()).and_then(A::from_asset)
    }

    pub fn graphics(&self) -> &Graphics { &self.g }
    pub fn graphics_device(&self) -> &br::Device { &self.g.device }
    pub fn graphics_queue_family_index(&self) -> u32 { self.g.graphics_queue.family }
    pub fn backbuffer_format(&self) -> br::vk::VkFormat { self.surface.format() }
    pub fn backbuffers(&self) -> &[br::ImageView] { self.wrt.backbuffers() }
    
    pub fn submit_commands<Gen: FnOnce(&mut br::CmdRecord)>(&self, generator: Gen) -> br::Result<()> {
        self.g.submit_commands(generator)
    }
    pub fn submit_buffered_commands(&self, batches: &[br::SubmissionBatch], fence: &br::Fence) -> br::Result<()> {
        self.g.graphics_queue.q.submit(batches, Some(fence))
    }

    pub fn event_handler_ref(&self) -> &E { self.event_handler.as_ref().expect("<EMPTY EVENTHANDLER>") }
    pub fn do_update(&mut self)
    {
        let bb_index = self.wrt.acquire_next_backbuffer_index(None, br::CompletionHandler::Device(&self.g.acquiring_backbuffer))
            .expect("Acquiring available backbuffer index");
        self.wrt.command_completion_for_backbuffer_mut(bb_index as _)
            .wait().expect("Waiting Previous command completion");
        {
            let mut fb_submission = self.event_handler_ref().update(self, bb_index);
            fb_submission.signal_semaphores.to_mut().push(&self.g.present_ordering);
            fb_submission.wait_semaphores.to_mut().push((&self.g.acquiring_backbuffer, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT));
            self.submit_buffered_commands(&[fb_submission], self.wrt.command_completion_for_backbuffer(bb_index as _).object())
                .expect("CommandBuffer Submission");
        }
        unsafe {
            self.wrt.command_completion_for_backbuffer_mut(bb_index as _).signal();
        }
        self.wrt.present_on(&self.g.graphics_queue.q, bb_index, &[&self.g.present_ordering]).expect("Present Submission");
    }
}
/*impl<E: EngineEvents> EventDelegate for Engine<E>
{
    fn postinit(&self, app: &Rc<PlatformServer<E>>)
    {
        let g = Graphics::new(self.appname, self.appversion).unwrap(); self.g.init(g);
        let w = MainWindow::new(&format!("{} v{}.{}.{}", self.appname, self.appversion.0, self.appversion.1, self.appversion.2),
            512 * 10 / 16, 512, app);
        w.show();
        unsafe { *self.w.get() =  Some(w as _); }
        self.event_handler.init(self);
    }
}*/
impl<E: EngineEvents<AL, PRT>, AL: AssetLoader, PRT: PlatformRenderTarget> Drop for Engine<E, AL, PRT> {
    fn drop(&mut self) {
        self.graphics().device.wait().unwrap();
    }
}

use std::cell::{Ref, RefMut, RefCell};
pub struct LateInit<T>(RefCell<Option<T>>);
impl<T> LateInit<T>
{
    pub fn new() -> Self { LateInit(RefCell::new(None)) }
    pub fn init(&self, v: T) { *self.0.borrow_mut() = v.into(); }
    pub fn get(&self) -> Ref<T> { Ref::map(self.0.borrow(), |x| x.as_ref().unwrap()) }
}
pub struct Discardable<T>(RefCell<Option<T>>);
impl<T> Discardable<T>
{
    pub fn new() -> Self { Discardable(RefCell::new(None)) }
    pub fn set(&self, v: T) { *self.0.borrow_mut() = v.into(); }
    pub fn get(&self) -> Ref<T> { Ref::map(self.0.borrow(), |x| x.as_ref().unwrap()) }
    pub fn get_mut(&self) -> RefMut<T> { RefMut::map(self.0.borrow_mut(), |x| x.as_mut().unwrap()) }
    pub fn discard(&self) { *self.0.borrow_mut() = None; }
    pub fn is_available(&self) -> bool { self.0.borrow().is_some() }
}

pub struct Queue { q: br::Queue, family: u32 }
pub struct Graphics
{
    pub(self) instance: br::Instance, pub(self) adapter: br::PhysicalDevice, device: br::Device,
    graphics_queue: Queue,
    #[cfg(debug_assertions)] _d: DebugReport,
    cp_onetime_submit: br::CommandPool,
    acquiring_backbuffer: br::Semaphore, present_ordering: br::Semaphore,
    memory_type_index_cache: RefCell<BTreeMap<(u32, u32), u32>>
}
impl Graphics
{
    fn new(appname: &str, appversion: (u32, u32, u32)) -> br::Result<Self>
    {
        #[cfg(windows)] const VK_KHR_PLATFORM_SURFACE: &'static str = "VK_KHR_win32_surface";
        #[cfg(target_os = "android")] const VK_KHR_PLATFORM_SURFACE: &'static str = "VK_KHR_android_surface";

        info!("Supported Layers: ");
        for l in br::Instance::enumerate_layer_properties().unwrap() {
            let name = unsafe { ::std::ffi::CStr::from_ptr(l.layerName.as_ptr()) };
            info!("* {} :: {}/{}", name.to_string_lossy(), l.specVersion, l.implementationVersion);
        }

        let mut ib = br::InstanceBuilder::new(appname, appversion, "Interlude2:Peridot", (0, 1, 0));
        ib.add_extensions(vec!["VK_KHR_surface", VK_KHR_PLATFORM_SURFACE]);
        #[cfg(debug_assertions)] ib.add_extension("VK_EXT_debug_report");
        #[cfg(all(debug_assertions, not(target_os = "android")))] ib.add_layer("VK_LAYER_LUNARG_standard_validation");
        #[cfg(all(debug_assertions, target_os = "android"))] ib
            .add_layer("VK_LAYER_LUNARG_parameter_validation")
            .add_layer("VK_LAYER_LUNARG_core_validation")
            .add_layer("VK_LAYER_LUNARG_object_tracker")
            .add_layer("VK_LAYER_GOOGLE_unique_objects")
            .add_layer("VK_LAYER_GOOGLE_threading");
        let instance = ib.create()?;
        #[cfg(debug_assertions)] let _d = DebugReport::new(&instance)?;
        #[cfg(debug_assertions)] debug!("Debug reporting activated");
        let adapter = instance.iter_physical_devices()?.next().unwrap();
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
            if (ty.propertyFlags & br::vk::VK_MEMORY_PROPERTY_LAZILY_ALLOCATED_BIT) != 0 { flags.push("LAZILY ALLOCATED"); }
            info!("  {}: [{}] in heap #{}", n, flags.join("/"), ty.heapIndex);
        }
    }

    pub(self) fn memory_type_index_for(&self, mask: br::MemoryPropertyFlags, index_mask: u32) -> Option<u32> {
        if let Some(&mi) = self.memory_type_index_cache.borrow().get(&(mask.bits(), index_mask)) { return Some(mi); }
        for (n, m) in self.adapter.memory_properties().types().enumerate() {
            if ((1 << n) & index_mask) != 0 && (m.propertyFlags & mask.bits()) != 0 &&
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
    pub fn new(d: &br::Device, queue_family_index: u32, count: usize) -> br::Result<Self>
    {
        let cp = br::CommandPool::new(d, queue_family_index, false, false)?;
        return Ok(CommandBundle(cp.alloc(count as _, true)?, cp));
    }
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
use peridot_vertex_processing_pack::PvpContainer;
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
            module: &self.vertex, entry_name: CString::new("main").unwrap(), specinfo: None
        }, &self.bindings, &self.attributes, primitive_topo);
        if let Some(ref f) = self.fragment {
            r.fragment_shader(br::PipelineShader { module: f, entry_name: CString::new("main").unwrap(), specinfo: None });
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
impl Ord for ResourceKey<Buffer> { fn cmp(&self, other: &Self) -> Ordering { self.partial_cmp(other).unwrap() } }
impl Eq for ResourceKey<Image> {}
impl Ord for ResourceKey<Image> { fn cmp(&self, other: &Self) -> Ordering { self.partial_cmp(other).unwrap() } }
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
    
    fn update_barrier_range_for(map: &mut BTreeMap<ResourceKey<Buffer>, Range<u64>>, k: ResourceKey<Buffer>, new_range: Range<u64>) {
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
    pub fn write_index(&mut self, dest: br::vk::VkDescriptorSet, bound: u32, array: u32, info: br::DescriptorUpdateInfo) -> &mut Self {
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
