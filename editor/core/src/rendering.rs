use core::num::NonZeroUsize;
use std::{
    collections::{HashMap, HashSet},
    num::NonZero,
    sync::{Mutex, atomic::AtomicBool},
};

use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, DescriptorPoolMut, Device, DeviceMemoryMut,
    Fence, FenceMut, ImageChild, MemoryBound, QueueMut, RenderPass, ShaderModule,
    SpecializationConstants, SwapchainMut, VkHandle, VkHandleMut,
};
use peridot_math::{Matrix4, Matrix4F32, One, Zero};

use crate::{
    FlyoutSurfaceHandle, SyncEvent, SyncEventBus, WindowHandle,
    graphics::{
        BLEND_STATE_SINGLE_NONE, BLEND_STATE_SINGLE_PREMULTIPLIED, IA_STATE_TRILIST,
        IA_STATE_TRISTRIP, MS_STATE_EMPTY, RASTER_STATE_DEFAULT_FILL_NOCULL, UnboundVulkanSurface,
        VI_STATE_EMPTY, VulkanDevice, VulkanSurface, VulkanSwapchain,
    },
    rendering::{
        atlas::{AtlasRect, ColorTextureAtlas, TextureAtlas},
        composite::{
            BoundCompositeRenderer, CompositeRenderingData, CompositeSharedBuffers,
            CompositeStreamingData, CompositeTreeRef, CompositeTreeRender, CompositeTreeSyncBuffer,
            CustomRenderContext, CustomRenderHandlerFn, CustomRenderToken,
            EmptyCustomRenderHandler,
        },
        text::FontSet,
        vg::VectorRasterizationState,
    },
    uikit::MountTarget,
    utils::{LogicalUnit, PixelsUnit, Point, SafeF32, Size, range_from_len_u64, rup2, rup2_u64},
};

pub mod atlas;
pub mod composite;
pub mod text;
pub mod vg;

#[repr(transparent)]
pub struct NewWindowVulkanSurface(pub UnboundVulkanSurface);
unsafe impl Sync for NewWindowVulkanSurface {}
unsafe impl Send for NewWindowVulkanSurface {}

pub struct NewWindowData {
    pub key: WindowHandle,
    pub vk_surface: NewWindowVulkanSurface,
}

pub struct NewContextMenuData {
    pub w: FlyoutSurfaceHandle,
    #[cfg(not(windows))]
    pub vk_surface: NewWindowVulkanSurface,
    #[cfg(windows)]
    pub swapchain: windows::Win32::Graphics::Dxgi::IDXGISwapChain3,
    pub composite_root: CompositeTreeRef,
}

#[derive(Clone)]
pub struct Normalized2DStaticMeshTexture {
    pub vertices: &'static [[f32; 2]],
    pub indices: &'static [u16],
    pub width: f32,
    pub height: f32,
}

#[derive(Clone)]
pub struct ShaderTexture {
    pub width: f32,
    pub height: f32,
    pub shader_path: String,
}

pub enum RenderMessage {
    NewWindow(NewWindowData),
    DestroyWindow(WindowHandle, std::sync::mpsc::Sender<()>),
    NewContextMenu(NewContextMenuData),
    DestroyContextMenu(FlyoutSurfaceHandle, std::sync::mpsc::Sender<()>),
    RegisterNormalized2DStaticMeshTexture {
        id: TextureID,
        data: Normalized2DStaticMeshTexture,
    },
    RegisterShaderTexture {
        id: TextureID,
        data: ShaderTexture,
    },
}

pub type RenderMessageSender = std::sync::mpsc::Sender<RenderMessage>;

pub struct RendererSync {
    pub composite_buffer: CompositeTreeSyncBuffer<SyncEvent>,
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TextureID(NonZeroUsize);
impl TextureID {
    #[inline(always)]
    pub(self) const fn rect_index(&self) -> usize {
        self.0.get() - 1
    }
}

pub struct MainThreadTextureIDIssuer {
    next_id: NonZeroUsize,
}
impl MainThreadTextureIDIssuer {
    pub fn new() -> Self {
        Self {
            next_id: unsafe { NonZeroUsize::new_unchecked(1) },
        }
    }

    pub fn issue(&mut self) -> TextureID {
        let id = self.next_id;
        self.next_id = self.next_id.checked_add(1).expect("too many textures");
        TextureID(id)
    }
}

crate::perf_event!(SYNCPOINT = "RenderSyncPoint");
crate::perf_section!(INITIALIZATION = "RenderThread.Initialize");
crate::perf_section!(RENDERLOOP = "RenderLoop");
crate::perf_section!(PROCESS_MESSAGE = "RenderLoop.ProcessMessage");
crate::perf_section!(UPDATE_GRADIENT = "RenderLoop.UpdateGradient");
crate::perf_section!(UPDATE_WINDOW = "RenderLoop.UpdateWindow");
crate::perf_section!(ACQUIRE_WINDOW_BACKBUFFER = "RenderLoop.UpdateWindow.AcquireBackbuffer");
crate::perf_section!(UPDATE_CONTEXT_MENU = "RenderLoop.UpdateContextMenu");
crate::perf_section!(RENDER_VG_MASK = "RenderLoop.RenderVGMask");
crate::perf_section!(VALIDATE_PREVIEW_RENDERING = "RenderLoop.ValidatePreviewRendering");
crate::perf_section!(UPDATE_PREVIEW = "RenderLoop.UpdatePreview");
crate::perf_section!(POST_QUEUE = "RenderLoop.PostQueue");
crate::perf_section!(WAIT_QUEUE = "RenderLoop.WaitQueue");
#[cfg(windows)]
crate::perf_section!(WIN32_DX_PRESENT = "RenderLoop.Win32.DirectXPresent");

pub const PREVIEW_COMPOSITE: CustomRenderToken = CustomRenderToken(0);

pub struct RenderThread<'main> {
    pub vk_device: &'main VulkanDevice<'main>,
    pub shutdown_signal: &'main AtomicBool,
    pub renderer_sync: &'main Mutex<RendererSync>,
    pub global_time_base: &'main std::time::Instant,
    pub event_bus: &'main SyncEventBus,
    pub message_receiver: std::sync::mpsc::Receiver<RenderMessage>,
    pub font_set: &'main FontSet,
    pub preview_state: &'main Mutex<CommittedPreviewState>,
    #[cfg(windows)]
    pub dx_context: &'main crate::platform::windows::DxContext,
    #[cfg(windows)]
    pub d3d12_present_counter: u64,
}
impl<'main> RenderThread<'main> {
    #[allow(unused_mut)]
    pub fn run(mut self) {
        tracing::info!("Starting RenderThread...");

        crate::perf_begin!(perf = INITIALIZATION);

        let mut render_queue = self
            .vk_device
            .queue(self.vk_device.present_queue_family_index(), 0);

        #[cfg(windows)]
        let d3d12_present_fence: windows::Win32::Graphics::Direct3D12::ID3D12Fence = unsafe {
            self.dx_context
                .d3d12_device
                .CreateFence(
                    0,
                    windows::Win32::Graphics::Direct3D12::D3D12_FENCE_FLAG_NONE,
                )
                .expect("d3d12_device.CreateFence")
        };
        #[cfg(windows)]
        let d3d12_present_fence_event = unsafe {
            windows::Win32::System::Threading::CreateEventW(None, false, false, None)
                .expect("CreateEvent")
        };

        let mut composite_tree = CompositeTreeRender::new();
        let composite_shared_buffers = CompositeSharedBuffers::new(self.vk_device);
        let vg_render_formats = GlyphAtlasRenderingFormats {
            color: br::vk::VK_FORMAT_R8_UNORM,
            stencil: br::vk::VK_FORMAT_S8_UINT,
        };
        let glyph_atlas_manager_common_resources =
            GlyphAtlasManagerCommonResources::new(self.vk_device, &vg_render_formats);
        struct GlyphAtlasDataPerDpi<'d> {
            manager: MaskTextureAtlasManager<'d>,
            color_manager: ColorTextureAtlasManager<'d>,
            atlas_rects: Vec<AtlasRect>,
            vector_raster_state: VectorRasterizationState,
            shader_texture_rasterize_requests: Vec<(AtlasRect, ShaderTexture)>,
            ref_count: u64,
        }
        impl<'d> GlyphAtlasDataPerDpi<'d> {
            pub fn new(
                common_res: &GlyphAtlasManagerCommonResources<'d>,
                render_queue: &mut (impl br::QueueMut + ?Sized),
            ) -> Self {
                Self {
                    manager: MaskTextureAtlasManager::new(
                        common_res,
                        render_queue,
                        common_res.device.present_queue_family_index(),
                    ),
                    color_manager: ColorTextureAtlasManager::new(
                        common_res,
                        render_queue,
                        common_res.device.present_queue_family_index(),
                    ),
                    atlas_rects: Vec::new(),
                    vector_raster_state: VectorRasterizationState::new(),
                    shader_texture_rasterize_requests: Vec::new(),
                    ref_count: 0,
                }
            }

            pub fn ready_for_reuse(&mut self) {
                self.manager.clear();
                self.color_manager.clear();
                self.atlas_rects.clear();
                self.ref_count = 0;
            }
        }
        let mut glyph_atlas_per_scale: HashMap<SafeF32, GlyphAtlasDataPerDpi> = HashMap::new();
        let mut windows: HashMap<WindowHandle, WindowRenderer> = HashMap::new();
        let mut context_menus: HashMap<FlyoutSurfaceHandle, ContextMenuRenderer> = HashMap::new();
        let mut normalized_2d_static_mesh_textures: HashMap<
            TextureID,
            Normalized2DStaticMeshTexture,
        > = HashMap::new();
        let mut shader_textures = HashMap::<TextureID, ShaderTexture>::new();

        let mut shared_update_cp = br::CommandPoolObject::new(
            self.vk_device,
            &br::CommandPoolCreateInfo::new(self.vk_device.present_queue_family_index()),
        )
        .expect("shared_update_cp.create");
        let [mut shared_update_commands] = br::CommandBufferObject::alloc_array(
            self.vk_device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut shared_update_cp,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("shared_update_commands.alloc");
        unsafe {
            shared_update_commands
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("shared_update_commands.begin")
                .end()
                .expect("shared_update_commands.end");
        }

        let mut present_id = 0;
        self.event_bus
            .push(SyncEvent::NewPresentID { id: present_id });

        let mut preview_rt_buffer = PreviewRenderTargetBuffer::new(
            self.vk_device,
            self.preview_state
                .lock()
                .expect("poisoned")
                .viewport_size
                .to_pixels_ceil(1.0)
                .to_vk(),
        );
        let mut preview_renderer = PreviewRenderer::new(
            self.vk_device,
            &preview_rt_buffer,
            &self.preview_state.lock().expect("poisoned"),
            self.vk_device.present_queue_family_index(),
            &mut render_queue,
        );

        let dep_semaphore_preview_update =
            br::SemaphoreObject::new(self.vk_device, &br::SemaphoreCreateInfo::new())
                .expect("dep_semaphore_preview_update.create");
        let dep_semaphore_preview =
            br::SemaphoreObject::new(self.vk_device, &br::SemaphoreCreateInfo::new())
                .expect("dep_semaphore_preview.create");
        let linear_sampler = br::SamplerObject::new(self.vk_device, &br::SamplerCreateInfo::new())
            .expect("linear_sampler.create");
        let mut preview_composite = PreviewComposite::new(
            self.vk_device,
            br::VkHandleRef::from_raw_ref(&preview_rt_buffer.image_view),
            &linear_sampler,
            // あとで正しいものが設定されるので一旦ダミーで作る
            br::SubpassRef(
                br::VkHandleRef::from_raw_ref(&preview_renderer.render_pass),
                0,
            ),
            br::Extent2D {
                width: 640,
                height: 480,
            },
        );

        crate::perf_end!(perf);

        let mut any_swapchain_invalidated = false;
        'lp: while !self
            .shutdown_signal
            .load(std::sync::atomic::Ordering::Acquire)
        {
            crate::perf_scope!(RENDERLOOP);
            // unsafe {
            //     w.manual_capture_begin();
            // }

            loop {
                crate::perf_scope!(PROCESS_MESSAGE);
                match self.message_receiver.try_recv() {
                    Ok(RenderMessage::NewWindow(wd)) => {
                        let init_scale =
                            SafeF32::new(wd.key.ui_scale_factor()).expect("invalid scale");

                        let window_glyph_atlas = match glyph_atlas_per_scale.entry(init_scale) {
                            // use existing
                            std::collections::hash_map::Entry::Occupied(x) => x.into_mut(),
                            // create new one
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert(GlyphAtlasDataPerDpi::new(
                                    &glyph_atlas_manager_common_resources,
                                    &mut render_queue,
                                ))
                            }
                        };
                        window_glyph_atlas.ref_count += 1;
                        windows.insert(
                            wd.key,
                            WindowRenderer::new(
                                self.vk_device,
                                &composite_shared_buffers,
                                wd,
                                init_scale,
                                window_glyph_atlas.manager.atlas(),
                                window_glyph_atlas.color_manager.atlas(),
                                self.event_bus,
                            ),
                        );
                    }
                    Ok(RenderMessage::DestroyWindow(window_handle, done_event_bus)) => {
                        if let Some(x) = windows.remove(&window_handle) {
                            let current = glyph_atlas_per_scale
                                .get_mut(&x.active_scale)
                                .expect("invalid state");
                            current.ref_count -= 1;
                            if current.ref_count == 0 {
                                // no references
                                glyph_atlas_per_scale.remove(&x.active_scale);
                            }
                        }

                        if let Err(e) = done_event_bus.send(()) {
                            tracing::error!(reason = %e, "done_event_bus.send");
                        };
                    }
                    Ok(RenderMessage::NewContextMenu(create_data)) => {
                        let init_scale =
                            SafeF32::new(create_data.w.render_scale()).expect("invalid scale");

                        let window_glyph_atlas = match glyph_atlas_per_scale.entry(init_scale) {
                            // use existing
                            std::collections::hash_map::Entry::Occupied(x) => x.into_mut(),
                            // create new one
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert(GlyphAtlasDataPerDpi::new(
                                    &glyph_atlas_manager_common_resources,
                                    &mut render_queue,
                                ))
                            }
                        };
                        window_glyph_atlas.ref_count += 1;
                        context_menus.insert(
                            create_data.w,
                            ContextMenuRenderer::new(
                                self.vk_device,
                                &composite_shared_buffers,
                                create_data,
                                init_scale,
                                window_glyph_atlas.manager.atlas(),
                                window_glyph_atlas.color_manager.atlas(),
                                self.event_bus,
                                #[cfg(windows)]
                                self.dx_context,
                            ),
                        );
                    }
                    Ok(RenderMessage::DestroyContextMenu(handle, done_event_bus)) => {
                        if let Some(x) = context_menus.remove(&handle) {
                            let current = glyph_atlas_per_scale
                                .get_mut(&x.active_scale)
                                .expect("invalid state");
                            current.ref_count -= 1;
                            if current.ref_count == 0 {
                                // no references
                                glyph_atlas_per_scale.remove(&x.active_scale);
                            }
                        }

                        if let Err(e) = done_event_bus.send(()) {
                            tracing::error!(reason = %e, "done_event_bus.send");
                        }
                    }
                    Ok(RenderMessage::RegisterNormalized2DStaticMeshTexture { id, data }) => {
                        normalized_2d_static_mesh_textures.insert(id, data);
                    }
                    Ok(RenderMessage::RegisterShaderTexture { id, data }) => {
                        shader_textures.insert(id, data);
                    }
                    Err(std::sync::mpsc::TryRecvError::Empty) => {
                        break;
                    }
                    Err(e) => {
                        tracing::error!(reason = %e, "message_receiver.try_recv");
                        break;
                    }
                }
            }

            // TODO: 必要なら後で最適化する
            crate::perf_begin!(perf = UPDATE_GRADIENT);
            composite_tree.update_gradients(self.vk_device, &composite_shared_buffers);
            unsafe {
                shared_update_cp
                    .reset(br::CommandPoolResetFlags::EMPTY)
                    .expect("shared_update_cp.reset");
            }
            unsafe {
                shared_update_commands
                    .begin(&br::CommandBufferBeginInfo::new())
                    .expect("shared_update_commands.begin")
            }
            .inject(|r| composite_shared_buffers.sync_buffer(r))
            .end()
            .expect("shared_update_commands.end");
            render_queue
                .submit(
                    &[br::SubmitInfo::new_array(
                        &[],
                        &[],
                        &[shared_update_commands.as_transparent_ref()],
                        &[],
                    )],
                    None,
                )
                .expect("shared_update.submit");
            render_queue.wait().expect("shared_update.wait");
            crate::perf_end!(perf);

            for x in windows.values_mut() {
                if x.take_swapchain_externally_invalidation_signal() {
                    x.invalidate_swapchain();
                    any_swapchain_invalidated = true;
                }
            }
            #[cfg(not(windows))]
            for x in context_menus.values_mut() {
                if x.take_swapchain_externally_invalidation_signal() {
                    tracing::debug!("ContextMenuSwapchainInvalidation");
                    x.invalidate_swapchain();
                    any_swapchain_invalidated = true;
                }
            }

            if any_swapchain_invalidated {
                let x = std::time::Instant::now();
                render_queue.wait().expect("waiting pending queue works");
                tracing::trace!(elapsed = ?x.elapsed(), "queue waiting time during resize");

                if self
                    .shutdown_signal
                    .load(std::sync::atomic::Ordering::Acquire)
                {
                    // already shut down while waiting queue completion
                    break 'lp;
                }

                let mut descriptor_writes = Vec::new();
                for x in windows.values_mut() {
                    x.validate_swapchain(&mut descriptor_writes, self.event_bus);
                }
                for x in context_menus.values_mut() {
                    x.validate_swapchain(&mut descriptor_writes, self.event_bus);
                }
                self.vk_device
                    .update_descriptor_sets(&descriptor_writes, &[]);

                any_swapchain_invalidated = false;
            }

            // flush synchronizing buffers
            crate::perf_emit!(SYNCPOINT);
            {
                let mut renderer_sync = self.renderer_sync.lock().expect("poisoned");
                renderer_sync.composite_buffer.clean(&mut composite_tree);
            }

            for x in glyph_atlas_per_scale.values_mut() {
                x.vector_raster_state.clear();
            }

            let current_t = self.global_time_base.elapsed();
            composite_tree.update_shared(current_t.as_secs_f32());

            // いったん最適化とかは考えないで直列でまわす(パフォーマンス気になったらそのとき考える)
            struct SubmitParameters<'x> {
                render_wait_semaphores: Vec<br::VkHandleRef<'x, br::vk::VkSemaphore>>,
                render_wait_stages: Vec<br::PipelineStageFlags>,
                render_commands: Vec<br::VkHandleRef<'x, br::vk::VkCommandBuffer>>,
                render_signal_semaphores: Vec<br::VkHandleRef<'x, br::vk::VkSemaphore>>,
            }
            enum PresentKey {
                Window(WindowHandle),
                ContextMenu(FlyoutSurfaceHandle),
            }
            struct VkPresentParameters<'x> {
                key: PresentKey,
                swapchain_ref: br::VkHandleRef<'x, br::vk::VkSwapchainKHR>,
                render_signal_semaphores: Vec<br::VkHandleRef<'x, br::vk::VkSemaphore>>,
                backbuffer_index: u32,
            }
            let mut submit_parameters = Vec::with_capacity(windows.len() + context_menus.len());
            let mut present_parameters = Vec::with_capacity(windows.len() + context_menus.len());
            #[cfg(windows)]
            let mut present_swapchains = Vec::with_capacity(context_menus.len());
            let mut preview_composition_required = false;
            for (k, x) in windows.iter_mut() {
                crate::perf_scope!(UPDATE_WINDOW);

                let backbuffer_index = match x.acquire_backbuffer_with_wait() {
                    Ok(x) => x,
                    Err(e) if e.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                        x.invalidate_swapchain();
                        any_swapchain_invalidated = true;
                        continue;
                    }
                    Err(e) => Err(e).expect("acquire next"),
                };

                let new_ui_scale = x.take_latest_ui_scale_changes();

                if let Some(scale) = new_ui_scale {
                    let scale = SafeF32::new(scale).expect("scale.invalid");

                    let current = glyph_atlas_per_scale
                        .get_mut(&x.active_scale)
                        .expect("invalid state");
                    current.ref_count -= 1;
                    let removed = if current.ref_count == 0 {
                        // no references
                        glyph_atlas_per_scale.remove(&x.active_scale)
                    } else {
                        None
                    };

                    let new_atlas_mgr = match glyph_atlas_per_scale.entry(scale) {
                        // reuse existing
                        std::collections::hash_map::Entry::Occupied(o) => o.into_mut(),
                        std::collections::hash_map::Entry::Vacant(v) => match removed {
                            // reuse existing with clear
                            Some(mut data) => {
                                data.ready_for_reuse();
                                v.insert(data)
                            }
                            // new one
                            None => v.insert(GlyphAtlasDataPerDpi::new(
                                &glyph_atlas_manager_common_resources,
                                &mut render_queue,
                            )),
                        },
                    };
                    new_atlas_mgr.ref_count += 1;

                    x.active_scale = scale;
                    x.invalidate_render_commands(); // DescriptorSetをかえるときは再度つくりなおす必要がある
                    let mut descriptor_writes = Vec::with_capacity(1);
                    x.composite_renderer.rebind_glyph_atlas(
                        new_atlas_mgr.manager.atlas().as_image_view(),
                        new_atlas_mgr.color_manager.atlas().as_image_view(),
                        &mut descriptor_writes,
                    );
                    self.vk_device
                        .update_descriptor_sets(&descriptor_writes, &[]);
                }

                let glyph_atlas_mgr = glyph_atlas_per_scale
                    .get_mut(&x.active_scale)
                    .expect("invalid state");
                // TODO: ここも重いようならあとで改善する
                for (&id, e) in &normalized_2d_static_mesh_textures {
                    let rect_index = id.rect_index();
                    if glyph_atlas_mgr
                        .atlas_rects
                        .get(rect_index)
                        .is_none_or(|x| x == &AtlasRect::EMPTY)
                    {
                        tracing::trace!(?id, "rasterize mesh");
                        let rect = glyph_atlas_mgr.manager.acquire(
                            (e.width * x.active_scale.value()).ceil() as _,
                            (e.height * x.active_scale.value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= rect_index {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(rect_index + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[rect_index] = rect;
                        if glyph_atlas_mgr
                            .vector_raster_state
                            .normalized_2d_mesh_requests
                            .insert(id, (rect.left, rect.top))
                            .is_none()
                        {
                            // new entry
                            glyph_atlas_mgr
                                .vector_raster_state
                                .updated_rects
                                .push(rect.vk_rect());
                        }
                    }
                }
                for (&id, e) in &shader_textures {
                    let rect_index = id.rect_index();
                    if glyph_atlas_mgr.atlas_rects.get(rect_index).is_none_or(|x| {
                        x == &AtlasRect {
                            left: 0,
                            top: 0,
                            right: 0,
                            bottom: 0,
                        }
                    }) {
                        let rect = glyph_atlas_mgr.color_manager.acquire(
                            (e.width * x.active_scale.value()).ceil() as _,
                            (e.height * x.active_scale.value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= rect_index {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(rect_index + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[rect_index] = rect;
                        tracing::trace!(?id, ?rect, "reserve rasterize shader tex");
                        // TODO: clone負荷が気になるようであればそのときに改修する
                        glyph_atlas_mgr
                            .shader_texture_rasterize_requests
                            .push((rect, e.clone()));
                    }
                }
                let render_scale = x.active_scale;
                let needs_update_command = x.update(
                    current_t.as_secs_f32(),
                    &mut composite_tree,
                    &mut glyph_atlas_mgr.manager,
                    &glyph_atlas_mgr.color_manager,
                    &mut glyph_atlas_mgr.atlas_rects,
                    &mut glyph_atlas_mgr.vector_raster_state,
                    self.event_bus,
                    self.font_set,
                    &mut preview_composite,
                    |preview_composite, ctx| {
                        crate::perf_scope!(VALIDATE_PREVIEW_RENDERING);
                        let mut committed_state = self.preview_state.lock().expect("poisoned");
                        let resource_recreated = preview_rt_buffer.validate(
                            self.vk_device,
                            committed_state
                                .viewport_size
                                .to_pixels_ceil(render_scale.value())
                                .to_vk(),
                        );

                        if resource_recreated {
                            // 稀に同じポインタで別のオブジェクトが再生成される場合があるので強制的にキャッシュを吹き飛ばすことで必ず更新させる
                            preview_composite.force_invalidate_descriptor_set_state();
                        }
                        preview_composite.validate(
                            self.vk_device,
                            &preview_rt_buffer,
                            ctx.rt_size,
                            ctx.active_render_pass,
                            ctx.active_subpass_index,
                        );
                    },
                );

                let mut render_wait_semaphores = Vec::with_capacity(3);
                let mut render_wait_stages = Vec::with_capacity(3);

                // TODO: いったんめんどうなので毎回更新
                if true || needs_update_command {
                    x.submit_update_commands(&mut render_queue);

                    render_wait_semaphores.push(x.update_completion_semaphore_ref());
                    render_wait_stages.push(br::PipelineStageFlags::VERTEX_INPUT);
                }

                if x.render_requires_preview_composition {
                    render_wait_semaphores.push(dep_semaphore_preview.as_transparent_ref());
                    render_wait_stages.push(br::PipelineStageFlags::FRAGMENT_SHADER);
                    preview_composition_required = true;
                }

                render_wait_semaphores.push(x.backbuffer_ready_semaphore.as_transparent_ref());
                render_wait_stages.push(br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT);

                submit_parameters.push(SubmitParameters {
                    render_wait_semaphores,
                    render_wait_stages,
                    render_commands: vec![x.primary_render_commands_ref(backbuffer_index)],
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                });
                present_parameters.push(VkPresentParameters {
                    key: PresentKey::Window(*k),
                    swapchain_ref: x.swapchain_ref(),
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                    backbuffer_index,
                });
            }
            for (k, x) in context_menus.iter_mut() {
                crate::perf_scope!(UPDATE_CONTEXT_MENU);

                let backbuffer_index = match x.acquire_backbuffer_with_wait() {
                    Ok(x) => x,
                    Err(e) if e.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
                        x.invalidate_swapchain();
                        any_swapchain_invalidated = true;
                        continue;
                    }
                    Err(e) => Err(e).expect("acquire next"),
                };

                #[cfg(feature = "wayland")]
                let new_ui_scale = x.take_latest_ui_scale_changes();
                #[cfg(feature = "wayland")]
                if let Some(scale) = new_ui_scale {
                    tracing::debug!(scale, "ContextMenuRescaleRender");
                    let scale = SafeF32::new(scale).expect("scale.invalid");

                    let current = glyph_atlas_per_scale
                        .get_mut(&x.active_scale)
                        .expect("invalid state");
                    current.ref_count -= 1;
                    let removed = if current.ref_count == 0 {
                        // no references
                        glyph_atlas_per_scale.remove(&x.active_scale)
                    } else {
                        None
                    };

                    let new_atlas_mgr = match glyph_atlas_per_scale.entry(scale) {
                        // reuse existing
                        std::collections::hash_map::Entry::Occupied(o) => o.into_mut(),
                        std::collections::hash_map::Entry::Vacant(v) => match removed {
                            // reuse existing with clear
                            Some(mut data) => {
                                data.ready_for_reuse();
                                v.insert(data)
                            }
                            // new one
                            None => v.insert(GlyphAtlasDataPerDpi::new(
                                &glyph_atlas_manager_common_resources,
                                &mut render_queue,
                            )),
                        },
                    };
                    new_atlas_mgr.ref_count += 1;

                    x.rescale(scale);
                    x.invalidate_render_commands(); // DescriptorSetをかえるときは再度つくりなおす必要がある
                    let mut descriptor_writes = Vec::with_capacity(1);
                    x.composite_renderer.rebind_glyph_atlas(
                        new_atlas_mgr.manager.atlas().as_image_view(),
                        new_atlas_mgr.color_manager.atlas().as_image_view(),
                        &mut descriptor_writes,
                    );
                    self.vk_device
                        .update_descriptor_sets(&descriptor_writes, &[]);
                }

                let glyph_atlas_mgr = glyph_atlas_per_scale
                    .get_mut(&x.active_scale)
                    .expect("invalid state");
                // TODO: ここも重いようならあとで改善する
                for (&id, e) in &normalized_2d_static_mesh_textures {
                    let rect_index = id.rect_index();
                    if glyph_atlas_mgr.atlas_rects.get(rect_index).is_none_or(|x| {
                        x == &AtlasRect {
                            left: 0,
                            top: 0,
                            right: 0,
                            bottom: 0,
                        }
                    }) {
                        tracing::trace!(?id, "rasterize mesh");
                        let rect = glyph_atlas_mgr.manager.acquire(
                            (e.width * x.active_scale.value()).ceil() as _,
                            (e.height * x.active_scale.value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= rect_index {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(rect_index + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[rect_index] = rect;
                        if glyph_atlas_mgr
                            .vector_raster_state
                            .normalized_2d_mesh_requests
                            .insert(id, (rect.left, rect.top))
                            .is_none()
                        {
                            // new entry
                            glyph_atlas_mgr
                                .vector_raster_state
                                .updated_rects
                                .push(rect.vk_rect());
                        }
                    }
                }
                for (&id, e) in &shader_textures {
                    let rect_index = id.rect_index();
                    if glyph_atlas_mgr.atlas_rects.get(rect_index).is_none_or(|x| {
                        x == &AtlasRect {
                            left: 0,
                            top: 0,
                            right: 0,
                            bottom: 0,
                        }
                    }) {
                        let rect = glyph_atlas_mgr.color_manager.acquire(
                            (e.width * x.active_scale.value()).ceil() as _,
                            (e.height * x.active_scale.value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= rect_index {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(rect_index + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[rect_index] = rect;
                        tracing::trace!(?id, ?rect, "reserve rasterize shader tex");
                        // TODO: clone負荷が気になるようであればそのときに改修する
                        glyph_atlas_mgr
                            .shader_texture_rasterize_requests
                            .push((rect, e.clone()));
                    }
                }
                let needs_update_command = x.update(
                    current_t.as_secs_f32(),
                    &mut composite_tree,
                    &mut glyph_atlas_mgr.manager,
                    &glyph_atlas_mgr.color_manager,
                    &mut glyph_atlas_mgr.atlas_rects,
                    &mut glyph_atlas_mgr.vector_raster_state,
                    self.event_bus,
                    self.font_set,
                );

                let mut render_wait_semaphores = Vec::with_capacity(1);
                let mut render_wait_stages = Vec::with_capacity(1);

                // TODO: いったんめんどうなので毎回更新
                if true || needs_update_command {
                    x.submit_update_commands(&mut render_queue);

                    render_wait_semaphores.push(x.update_completion_semaphore_ref());
                    render_wait_stages.push(br::PipelineStageFlags::VERTEX_INPUT);
                }

                #[cfg(not(windows))]
                render_wait_semaphores.push(x.backbuffer_ready_semaphore.as_transparent_ref());
                #[cfg(not(windows))]
                render_wait_stages.push(br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT);

                submit_parameters.push(SubmitParameters {
                    render_wait_semaphores,
                    render_wait_stages,
                    render_commands: vec![x.primary_render_commands_ref(backbuffer_index)],
                    #[cfg(windows)]
                    render_signal_semaphores: vec![],
                    #[cfg(not(windows))]
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                });
                #[cfg(not(windows))]
                present_parameters.push(VkPresentParameters {
                    key: PresentKey::ContextMenu(*k),
                    swapchain_ref: x.swapchain.as_transparent_ref(),
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                    backbuffer_index,
                });
                #[cfg(windows)]
                {
                    present_swapchains.push(x.swapchain.clone());
                }
            }

            for (s, x) in glyph_atlas_per_scale.iter_mut() {
                if !x.vector_raster_state.is_empty() {
                    // vector rasterization required
                    crate::perf_scope!(RENDER_VG_MASK);
                    x.manager.perform_render(
                        &x.vector_raster_state,
                        &vg_render_formats,
                        &glyph_atlas_manager_common_resources,
                        &mut render_queue,
                        &normalized_2d_static_mesh_textures,
                        s.value(),
                    );
                    x.vector_raster_state.clear();
                }

                if !x.shader_texture_rasterize_requests.is_empty() {
                    x.color_manager.perform_render(
                        &x.shader_texture_rasterize_requests,
                        s.value(),
                        self.vk_device,
                        &mut render_queue,
                    );
                    x.shader_texture_rasterize_requests.clear();
                }
            }

            if preview_composition_required {
                // preview may still updating on main thread...
                if let Ok(mut st) = self.preview_state.try_lock() {
                    crate::perf_scope!(UPDATE_PREVIEW);
                    preview_renderer.update(&mut st);
                    preview_renderer.validate(self.vk_device, &preview_rt_buffer, &mut st);
                }
            }

            // enqueue next update for main thread
            present_id += 1;
            self.event_bus
                .push(SyncEvent::NewPresentID { id: present_id });

            crate::perf_begin!(perf = POST_QUEUE);
            if preview_composition_required {
                let mut render_waits = Vec::with_capacity(2);
                let mut render_wait_stages = Vec::with_capacity(2);

                if core::mem::replace(&mut preview_renderer.update_command_pending, false) {
                    render_waits.push(dep_semaphore_preview_update.as_transparent_ref());
                    render_wait_stages.push(br::PipelineStageFlags::VERTEX_SHADER);

                    render_queue
                        .submit(
                            &[br::SubmitInfo::new(
                                &[],
                                &[],
                                &[unsafe {
                                    br::VkHandleRef::dangling(
                                        preview_renderer.update_command_buffer,
                                    )
                                }],
                                &[dep_semaphore_preview_update.as_transparent_ref()],
                            )],
                            None,
                        )
                        .expect("update queue submit");
                }

                // preview_renderer.write_streaming_buffer_content(
                //     self.vk_device,
                //     PreviewStreamingBufferContent {
                //         current_sec: current_t.as_secs_f32(),
                //     },
                // );

                // 別でsubmitしないといけないらしい？(validation layerが対応するsemaphore waitを見つけられなくてエラーが出る)
                render_queue
                    .submit(
                        &[br::SubmitInfo::new(
                            &render_waits,
                            &render_wait_stages,
                            &[unsafe {
                                br::VkHandleRef::dangling(preview_renderer.command_buffer)
                            }],
                            &[dep_semaphore_preview.as_transparent_ref()],
                        )],
                        None,
                    )
                    .expect("queue submit");
            }
            if !submit_parameters.is_empty() {
                render_queue
                    .submit(
                        &submit_parameters
                            .iter()
                            .map(|x| {
                                br::SubmitInfo::new(
                                    &x.render_wait_semaphores,
                                    &x.render_wait_stages,
                                    &x.render_commands,
                                    &x.render_signal_semaphores,
                                )
                            })
                            .collect::<Vec<_>>(),
                        None,
                    )
                    .expect("queue submit");
            }

            crate::perf_wrap!(WAIT_QUEUE, render_queue.wait().expect("render_queue.wait"));
            crate::perf_end!(perf);

            if !present_parameters.is_empty() {
                let mut results = present_parameters
                    .iter()
                    .map(|_| br::vk::VK_SUCCESS)
                    .collect::<Vec<_>>();
                match render_queue.present(&br::PresentInfo::new(
                    &present_parameters
                        .iter()
                        .map(|x| x.render_signal_semaphores[0])
                        .collect::<Vec<_>>(),
                    &present_parameters
                        .iter()
                        .map(|x| x.swapchain_ref)
                        .collect::<Vec<_>>(),
                    &present_parameters
                        .iter()
                        .map(|x| x.backbuffer_index)
                        .collect::<Vec<_>>(),
                    &mut results,
                )) {
                    Ok(_) => (),
                    Err(e) if e.0 == br::vk::VK_ERROR_OUT_OF_DATE_KHR => (/* handled later */),
                    Err(e) => Err::<(), _>(e).expect("queue present"),
                }

                let present_keys = present_parameters
                    .into_iter()
                    .map(|x| x.key)
                    .collect::<Vec<_>>();
                for (r, p) in results.into_iter().zip(present_keys.into_iter()) {
                    if r == br::vk::VK_ERROR_OUT_OF_DATE_KHR {
                        match p {
                            PresentKey::Window(w) => windows
                                .get_mut(&w)
                                .expect("invalid entry")
                                .invalidate_swapchain(),
                            PresentKey::ContextMenu(w) => context_menus
                                .get_mut(&w)
                                .expect("invalid entry")
                                .invalidate_swapchain(),
                        }
                        any_swapchain_invalidated = true;
                    }
                }
            }

            crate::perf_wrap!(WAIT_QUEUE, render_queue.wait().expect("render_queue.wait"));

            #[cfg(windows)]
            crate::perf_begin!(perf = WIN32_DX_PRESENT);
            #[cfg(windows)]
            if !present_swapchains.is_empty() {
                for x in present_swapchains {
                    unsafe {
                        x.Present(0, windows::Win32::Graphics::Dxgi::DXGI_PRESENT(0))
                            .ok()
                            .expect("swaphain.Present");
                    }
                }

                self.d3d12_present_counter += 1;
                let wait_for_counter = self.d3d12_present_counter;
                unsafe {
                    d3d12_present_fence
                        .SetEventOnCompletion(wait_for_counter, d3d12_present_fence_event)
                        .expect("d3d12_present_fence.SetEventOnCompletion");
                    self.dx_context
                        .d3d12_cq
                        .Signal(&d3d12_present_fence, wait_for_counter)
                        .expect("d3d12_cq.Wait");
                    windows::Win32::System::Threading::WaitForSingleObject(
                        d3d12_present_fence_event,
                        windows::Win32::System::Threading::INFINITE,
                    );
                }
            }
            #[cfg(windows)]
            crate::perf_end!(perf);

            // unsafe {
            //     manual_capture_end();
            // }
        }

        unsafe {
            // TODO: これdanglingつかわない方法にしたい スレッド専用のDeviceをつくるか......？
            br::vkfn_wrapper::device_wait_idle(br::VkHandleRefMut::dangling(
                self.vk_device.native_ptr(),
            ))
            .expect("device wait");
        }
        #[cfg(windows)]
        if let Err(e) =
            unsafe { windows::Win32::Foundation::CloseHandle(d3d12_present_fence_event) }
        {
            tracing::error!(reason = %e, "CloseHandle");
        }

        unsafe {
            preview_composite.drop(self.vk_device);
            preview_renderer.drop(self.vk_device);
            preview_rt_buffer.drop(self.vk_device);
            composite_shared_buffers.drop(self.vk_device);
        }

        tracing::info!("RenderThread terminated");
    }
}

#[cfg(windows)]
struct CompositionSwapchainBuffer<'d> {
    vk_device: &'d VulkanDevice<'d>,
    d3d12_resource: windows::Win32::Graphics::Direct3D12::ID3D12Resource,
    shared_handle: windows::Win32::Foundation::HANDLE,
    vk_device_memory: br::vk::VkDeviceMemory,
    vk_image: br::vk::VkImage,
    vk_image_view: br::vk::VkImageView,
}
#[cfg(windows)]
impl<'d> Drop for CompositionSwapchainBuffer<'d> {
    fn drop(&mut self) {
        drop(unsafe {
            br::ImageViewObject::manage(
                self.vk_image_view,
                br::ImageObject::manage(
                    self.vk_image,
                    self.vk_device,
                    // 以下のデータはdropでは使われないので適当に埋める
                    br::vk::VK_IMAGE_TYPE_2D,
                    br::vk::VK_FORMAT_UNDEFINED,
                    br::Extent3D::spread1(1),
                ),
            )
        });
        drop(unsafe { br::DeviceMemoryObject::manage(self.vk_device_memory, self.vk_device) });
    }
}

struct ContextMenuRenderer<'d> {
    w: FlyoutSurfaceHandle,
    active_scale: SafeF32,
    vk_device: &'d VulkanDevice<'d>,
    composite_root: CompositeTreeRef,
    composite_renderer: BoundCompositeRenderer<'d>,
    last_composite_render_data: CompositeRenderingData,
    update_cp: br::CommandPoolObject<&'d VulkanDevice<'d>>,
    update_cb: br::CommandBufferObject<&'d VulkanDevice<'d>>,
    update_completion_fence: br::FenceObject<&'d VulkanDevice<'d>>,
    update_completion_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    updating: bool,
    render_cp: br::CommandPoolObject<&'d VulkanDevice<'d>>,
    render_cb: Vec<br::CommandBufferObject<&'d VulkanDevice<'d>>>,
    render_cb_invalid: bool,
    present_ready_semaphores: Vec<br::SemaphoreObject<&'d VulkanDevice<'d>>>,
    backbuffer_ready_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    swapchain_invalidated: bool,
    #[cfg(not(windows))]
    swapchain: VulkanSwapchain<'d, 'd>,
    #[cfg(not(windows))]
    surface: VulkanSurface<'d, 'd>,
    #[cfg(windows)]
    swapchain: windows::Win32::Graphics::Dxgi::IDXGISwapChain3,
    #[cfg(windows)]
    presentation_buffers: Vec<CompositionSwapchainBuffer<'d>>,
    #[cfg(windows)]
    presentation_size: br::Extent2D,
}
impl<'d> ContextMenuRenderer<'d> {
    fn new(
        device: &'d VulkanDevice<'d>,
        shared_buffers: &CompositeSharedBuffers,
        create_data: NewContextMenuData,
        init_scale: SafeF32,
        glyph_atlas: &TextureAtlas,
        color_atlas: &ColorTextureAtlas,
        event_bus: &SyncEventBus,
        #[cfg(windows)] dx_context: &crate::platform::windows::DxContext,
    ) -> Self {
        #[cfg(not(windows))]
        let surface = unsafe { create_data.vk_surface.0.bound(device) };
        #[cfg(not(windows))]
        let vk_swapchain = VulkanSwapchain::new(&surface, || create_data.w.pixels_size());

        #[cfg(windows)]
        let presentation_size = create_data.w.pixels_size().to_vk();
        #[cfg(windows)]
        let mut presentation_buffers = Vec::with_capacity(2);
        #[cfg(windows)]
        for n in 0..2 {
            use bedrock::{
                DeviceExternalMemoryWin32Extension, TypedVulkanSinkStructure, VulkanStructure,
            };

            let d3d12_resource: windows::Win32::Graphics::Direct3D12::ID3D12Resource = unsafe {
                create_data
                    .swapchain
                    .GetBuffer(n)
                    .expect("swapchain.GetBuffer")
            };
            let shared_handle = unsafe {
                dx_context
                    .d3d12_device
                    .CreateSharedHandle(
                        &d3d12_resource,
                        None,
                        windows::Win32::Foundation::GENERIC_ALL.0,
                        None,
                    )
                    .expect("d3d12_device.CreateSharedHandle")
            };

            let mut vk_image = br::ImageObject::new(
                device,
                &br::ImageCreateInfo::new(presentation_size, br::vk::VK_FORMAT_B8G8R8A8_UNORM)
                    .set_usage(br::ImageUsageFlags::COLOR_ATTACHMENT)
                    .with_next(&br::ExternalMemoryImageCreateInfo::new(
                        br::ExternalMemoryHandleTypeWin32::D3D12Resource as _,
                    )),
            )
            .expect("vk_image.create");
            let mut mreq_dedicated = br::vk::VkMemoryDedicatedRequirementsKHR::uninit_sink();
            let mut mreq = br::vk::VkMemoryRequirements2KHR::uninit_sink();
            unsafe {
                core::ptr::write(
                    core::ptr::addr_of_mut!((*mreq.as_mut_ptr()).pNext),
                    mreq_dedicated.as_mut_ptr().cast(),
                );
            }
            vk_image.requirements2().query(&mut mreq);
            let mreq = unsafe { mreq.assume_init_ref() };
            let mreq_dedicated = unsafe { mreq_dedicated.assume_init_ref() };
            let dedicated_allocation = mreq_dedicated.requiresDedicatedAllocation != 0;
            let mut import_props = br::vk::VkMemoryWin32HandlePropertiesKHR::uninit_sink();
            unsafe {
                device
                    .memory_win32_handle_properties(
                        br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                        core::mem::transmute(shared_handle),
                        &mut import_props,
                    )
                    .expect("device.memory_win32_handle_properties")
            };
            let import_props = unsafe { import_props.assume_init() };
            let memindex = device
                .find_device_local_memory_index(
                    mreq.memoryRequirements.memoryTypeBits & import_props.memoryTypeBits,
                )
                .expect("find_device_local_memory_index");
            let mut malloc_info_ext = br::ImportMemoryWin32HandleInfo::new(
                br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                unsafe { core::mem::transmute(shared_handle) },
                None,
            );
            let mut malloc_info_d =
                dedicated_allocation.then(|| br::MemoryDedicatedAllocateInfo::for_image(&vk_image));
            let mut malloc_info = br::MemoryAllocateInfo::new(1, memindex);
            br::chain_structures(
                [
                    malloc_info.as_generic_mut(),
                    malloc_info_ext.as_generic_mut(),
                ]
                .into_iter()
                .chain(
                    malloc_info_d
                        .iter_mut()
                        .map(br::VulkanStructure::as_generic_mut),
                ),
            );
            let vk_device_memory =
                br::DeviceMemoryObject::new(device, &malloc_info).expect("vk_device_memory.create");
            br::bind_memory(&mut vk_image, &vk_device_memory, 0).expect("vk_image.bind");
            let vk_image_view = br::ImageViewBuilder::new(
                vk_image,
                br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
            )
            .create()
            .expect("vk_image_view.create");

            device.dbg_set_name(&vk_image_view, c"ContextMenu.InteropImage.View");
            device.dbg_set_name(vk_image_view.image(), c"ContextMenu.InteropImage");
            device.dbg_set_name(&vk_device_memory, c"ContextMenu.InteropImage.Memory");

            let (vk_image_view, vk_image) = vk_image_view.unmanage();
            let vk_image = vk_image.unmanage().0;
            let vk_device_memory = vk_device_memory.unmanage().0;

            presentation_buffers.push(CompositionSwapchainBuffer {
                vk_device: device,
                d3d12_resource,
                shared_handle,
                // presentation_buffer,
                vk_device_memory,
                vk_image,
                vk_image_view,
            });
        }

        let mut update_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("update_cp.create");
        let [mut update_cb] = br::CommandBufferObject::alloc_array(
            device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut update_cp,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("update_cb.create");
        unsafe {
            update_cb
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("update_cb.begin")
                .end()
                .expect("update_cb.end");
        }

        let mut render_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("command pool create");
        let render_cb = br::CommandBufferObject::alloc(
            device,
            &br::CommandBufferAllocateInfo::new(
                &mut render_cp,
                #[cfg(not(windows))]
                {
                    vk_swapchain.image_count() as _
                },
                #[cfg(windows)]
                {
                    2
                },
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("command buffer alloc");
        for (n, x) in render_cb.iter().enumerate() {
            device.dbg_set_name(x, &unsafe {
                std::ffi::CString::from_vec_unchecked(
                    format!("Window Render Commands #{n}").into_bytes(),
                )
            });
        }

        let composite_renderer = BoundCompositeRenderer::new(
            device,
            shared_buffers,
            glyph_atlas.view(),
            color_atlas.view(),
            #[cfg(not(windows))]
            surface.format(),
            #[cfg(windows)]
            br::vk::VK_FORMAT_B8G8R8A8_UNORM,
            #[cfg(not(windows))]
            vk_swapchain.size(),
            #[cfg(windows)]
            presentation_size,
            #[cfg(not(windows))]
            vk_swapchain.image_view_refs(),
            #[cfg(windows)]
            presentation_buffers
                .iter()
                .map(|b| unsafe { br::VkHandleRef::dangling(b.vk_image_view) }),
        );

        event_bus.push(SyncEvent::FlyoutSurfacePostCreateRenderBuffer {
            target: create_data.w,
        });

        Self {
            w: create_data.w,
            active_scale: init_scale,

            vk_device: device,
            composite_root: create_data.composite_root,
            composite_renderer,
            last_composite_render_data: CompositeRenderingData::EMPTY,
            update_cp,
            update_cb,
            update_completion_fence: br::FenceObject::new(device, &br::FenceCreateInfo::new(0))
                .expect("update_completion_fence.create"),
            update_completion_semaphore: br::SemaphoreObject::new(
                device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("update_completion_semaphore.create"),
            updating: false,
            render_cp,
            render_cb,
            render_cb_invalid: true,
            #[cfg(not(windows))]
            present_ready_semaphores: (0..vk_swapchain.image_count())
                .map(|_| {
                    br::SemaphoreObject::new(device, &br::SemaphoreCreateInfo::new())
                        .expect("rendering_timeline_semaphore create")
                })
                .collect::<Vec<_>>(),
            #[cfg(windows)]
            present_ready_semaphores: (0..2)
                .map(|_| {
                    br::SemaphoreObject::new(device, &br::SemaphoreCreateInfo::new())
                        .expect("rendering_timeline_semaphore create")
                })
                .collect::<Vec<_>>(),
            backbuffer_ready_semaphore: br::SemaphoreObject::new(
                device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("backbuffer_ready_semaphore.create"),
            #[cfg(not(windows))]
            surface,
            #[cfg(not(windows))]
            swapchain: vk_swapchain,
            // #[cfg(windows)]
            // presentation_surface,
            #[cfg(windows)]
            swapchain: create_data.swapchain,
            #[cfg(windows)]
            presentation_buffers,
            #[cfg(windows)]
            presentation_size,
            swapchain_invalidated: false,
        }
    }

    #[cfg(feature = "wayland")]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        self.w.take_swapchain_externally_invalidation_signal()
    }
    #[cfg(not(feature = "wayland"))]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        false
    }

    // waylandのときだけRenderScaleがあとからくるので変更を受け付ける必要がある
    #[cfg(feature = "wayland")]
    pub fn take_latest_ui_scale_changes(&self) -> Option<f32> {
        self.w.take_latest_ui_scale_change()
    }

    #[cfg(not(windows))]
    pub fn rescale(&mut self, scale: SafeF32) {
        self.active_scale = scale;
    }

    pub fn update(
        &mut self,
        current_sec: f32,
        composite_tree: &mut CompositeTreeRender<SyncEvent>,
        glyph_atlas: &mut MaskTextureAtlasManager,
        color_atlas: &ColorTextureAtlasManager,
        mask_atlas_rects: &[AtlasRect],
        vector_raster_state: &mut VectorRasterizationState,
        events: &SyncEventBus,
        font_set: &FontSet,
    ) -> bool {
        let composite_render_data = self.composite_renderer.update(
            self.vk_device,
            composite_tree,
            self.composite_root,
            #[cfg(not(windows))]
            self.swapchain.size(),
            #[cfg(windows)]
            self.presentation_size,
            self.w.render_scale(),
            font_set,
            glyph_atlas,
            color_atlas,
            mask_atlas_rects,
            vector_raster_state,
            |e| events.push(e),
            current_sec,
        );
        if composite_render_data != self.last_composite_render_data {
            // requires repopulate render commands
            self.invalidate_render_commands();

            self.composite_renderer
                .prepare_input_backdrop_descriptor_sets(
                    self.vk_device,
                    composite_render_data.required_backdrop_buffer_count,
                );

            self.last_composite_render_data = composite_render_data;
        }

        self.composite_renderer
            .update_streaming_data(self.vk_device, CompositeStreamingData { current_sec });
        let needs_update_commands = self.composite_renderer.update_backdrop_resources(
            self.vk_device,
            #[cfg(not(windows))]
            self.surface.format(),
            #[cfg(windows)]
            br::vk::VK_FORMAT_B8G8R8A8_UNORM,
            #[cfg(not(windows))]
            self.swapchain.size(),
            #[cfg(windows)]
            self.presentation_size,
            self.last_composite_render_data
                .required_backdrop_buffer_count
                == 0,
        );

        // update_backdrop_resourcesでDescriptorSetの更新がはしるのでここでやる
        self.validate_render_commands();

        needs_update_commands
    }

    pub fn invalidate_swapchain(&mut self) {
        self.swapchain_invalidated = true;
    }

    pub fn validate_swapchain<'s>(
        &'s mut self,
        descriptor_writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        event_bus: &SyncEventBus,
    ) {
        if !self.swapchain_invalidated {
            // already valid
            return;
        }

        self.invalidate_render_commands();

        #[cfg(not(windows))]
        self.surface.refresh_caps();
        #[cfg(not(windows))]
        self.swapchain
            .recreate(&self.surface, || self.w.pixels_size());

        // recrease rt resources
        #[cfg(not(windows))]
        self.composite_renderer.recreate_rt_resources(
            self.vk_device,
            self.surface.format(),
            self.swapchain.image_view_refs(),
            self.swapchain.size(),
            descriptor_writes,
        );
        #[cfg(windows)]
        todo!("revalidate composition swapchain");

        event_bus.push(SyncEvent::FlyoutSurfacePostCreateRenderBuffer { target: self.w });
        self.swapchain_invalidated = false;
    }

    pub fn acquire_backbuffer_with_wait(&mut self) -> br::Result<u32> {
        #[cfg(windows)]
        {
            Ok(unsafe { self.swapchain.GetCurrentBackBufferIndex() })
        }

        #[cfg(not(windows))]
        {
            let backbuffer_index = self.swapchain.acquire_next(
                None,
                br::CompletionHandlerMut::Queue(
                    self.backbuffer_ready_semaphore.as_transparent_ref_mut(),
                ),
            )?;
            /*self.backbuffer_ready_fence
                .wait()
                .expect("last render completion fence wait");
            self.backbuffer_ready_fence
                .reset()
                .expect("last render completion fence reset");*/

            Ok(backbuffer_index)
        }
    }

    pub fn invalidate_render_commands(&mut self) {
        if self.render_cb_invalid {
            // already invalid
            return;
        }

        unsafe {
            self.render_cp
                .reset(br::CommandPoolResetFlags::EMPTY)
                .expect("render_cp.reset");
        }
        self.render_cb_invalid = true;
    }

    pub fn validate_render_commands(&mut self) {
        if !self.render_cb_invalid {
            // already valid
            return;
        }

        for (n, cb) in self.render_cb.iter_mut().enumerate() {
            unsafe {
                cb.begin(&br::CommandBufferBeginInfo::new())
                    .expect("command buffer begin")
            }
            .inject(|r| {
                self.composite_renderer.populate_commands(
                    r,
                    self.vk_device,
                    &self.last_composite_render_data,
                    #[cfg(windows)]
                    self.presentation_size,
                    #[cfg(not(windows))]
                    self.swapchain.size(),
                    #[cfg(windows)]
                    br::VkHandleRef::from_raw_ref(&self.presentation_buffers[n].vk_image),
                    #[cfg(not(windows))]
                    &self.swapchain.image_ref(n),
                    n,
                    &mut EmptyCustomRenderHandler,
                )
            })
            .inject(|r| self.vk_device.cmd_end_render_pass(r))
            .end()
            .expect("command buffer end");
        }

        self.render_cb_invalid = false;
    }

    pub fn wait_for_last_update_completion(&mut self) {
        if !self.updating {
            // no updating work
            return;
        }

        self.update_completion_fence
            .wait()
            .expect("update_completion_fence.wait");
        self.update_completion_fence
            .reset()
            .expect("update_completion_fence.reset");
        self.updating = false;
    }

    pub fn repopulate_update_commands(&mut self) {
        unsafe {
            self.update_cp
                .reset(br::CommandPoolResetFlags::EMPTY)
                .expect("update_cp.reset");
        }
        unsafe {
            self.update_cb
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("update_cb.begin")
        }
        .inject(|r| self.composite_renderer.sync_buffer(r))
        .end()
        .expect("update_cb.end");
    }

    pub fn submit_update_commands(&mut self, device_queue: &mut br::QueueObject<&'d VulkanDevice>) {
        self.wait_for_last_update_completion();
        self.repopulate_update_commands();

        device_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[self.update_cb.as_transparent_ref()],
                    &[self.update_completion_semaphore.as_transparent_ref()],
                )],
                Some(self.update_completion_fence.as_transparent_ref_mut()),
            )
            .expect("gfx.update.submit");
        self.updating = true;
    }

    /*pub fn swapchain_ref<'x>(&'x self) -> br::VkHandleRef<'x, br::vk::VkSwapchainKHR> {
        self.swapchain.as_transparent_ref()
    }*/

    pub fn update_completion_semaphore_ref<'x>(
        &'x self,
    ) -> br::VkHandleRef<'x, br::vk::VkSemaphore> {
        self.update_completion_semaphore.as_transparent_ref()
    }

    pub fn primary_render_commands_ref<'x>(
        &'x self,
        backbuffer_index: u32,
    ) -> br::VkHandleRef<'x, br::vk::VkCommandBuffer> {
        self.render_cb[backbuffer_index as usize].as_transparent_ref()
    }

    pub fn present_ready_semaphore_ref<'x>(
        &'x self,
        backbuffer_index: u32,
    ) -> br::VkHandleRef<'x, br::vk::VkSemaphore> {
        self.present_ready_semaphores[backbuffer_index as usize].as_transparent_ref()
    }
}

struct WindowRenderer<'d> {
    w: crate::WindowHandle,
    active_scale: SafeF32,
    latest_ui_scale_changes: *const Mutex<Option<f32>>,
    vk_device: &'d VulkanDevice<'d>,
    swapchain_invalidated: bool,
    composite_root: CompositeTreeRef,
    composite_renderer: BoundCompositeRenderer<'d>,
    corner_cutout_renderer: Option<CornerCutoutRenderer<'d, 'd>>,
    last_composite_render_data: CompositeRenderingData,
    update_cp: br::CommandPoolObject<&'d VulkanDevice<'d>>,
    update_cb: br::CommandBufferObject<&'d VulkanDevice<'d>>,
    update_completion_fence: br::FenceObject<&'d VulkanDevice<'d>>,
    update_completion_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    updating: bool,
    render_cp: br::CommandPoolObject<&'d VulkanDevice<'d>>,
    render_cb: Vec<br::CommandBufferObject<&'d VulkanDevice<'d>>>,
    render_cb_invalid: bool,
    render_requires_preview_composition: bool,
    present_ready_semaphores: Vec<br::SemaphoreObject<&'d VulkanDevice<'d>>>,
    backbuffer_ready_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    swapchain: VulkanSwapchain<'d, 'd>,
    surface: VulkanSurface<'d, 'd>,
}
impl<'d> WindowRenderer<'d> {
    fn new(
        device: &'d VulkanDevice<'d>,
        shared_buffers: &CompositeSharedBuffers,
        create_data: NewWindowData,
        init_scale: SafeF32,
        glyph_atlas: &TextureAtlas,
        color_atlas: &ColorTextureAtlas,
        event_bus: &SyncEventBus,
    ) -> Self {
        let surface = unsafe { create_data.vk_surface.0.bound(device) };
        let vk_swapchain = VulkanSwapchain::new(&surface, || create_data.key.pixels_client_size());

        let mut update_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("update_cp.create");
        let [mut update_cb] = br::CommandBufferObject::alloc_array(
            device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut update_cp,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("update_cb.create");
        unsafe {
            update_cb
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("update_cb.begin")
                .end()
                .expect("update_cb.end");
        }

        let mut render_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("command pool create");
        let render_cb = br::CommandBufferObject::alloc(
            device,
            &br::CommandBufferAllocateInfo::new(
                &mut render_cp,
                vk_swapchain.image_count() as _,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("command buffer alloc");
        for (n, x) in render_cb.iter().enumerate() {
            device.dbg_set_name(x, &unsafe {
                std::ffi::CString::from_vec_unchecked(
                    format!("Window Render Commands #{n}").into_bytes(),
                )
            });
        }

        let composite_renderer = BoundCompositeRenderer::new(
            device,
            shared_buffers,
            glyph_atlas.view(),
            color_atlas.view(),
            surface.format(),
            vk_swapchain.size(),
            vk_swapchain.image_view_refs(),
        );
        let corner_cutout_renderer = if create_data.key.needs_corner_cutout_rendering() {
            Some(CornerCutoutRenderer::new(
                device,
                composite_renderer.subpass_final(),
                composite_renderer.subpass_continue_final(),
            ))
        } else {
            None
        };

        event_bus.push(SyncEvent::WindowPostCreateRenderBuffer {
            window: create_data.key,
        });

        Self {
            w: create_data.key,
            active_scale: init_scale,
            latest_ui_scale_changes: create_data.key.latest_ui_scale_changes(),
            vk_device: device,
            composite_root: create_data.key.ct_root(),
            composite_renderer,
            corner_cutout_renderer,
            last_composite_render_data: CompositeRenderingData::EMPTY,
            update_cp,
            update_cb,
            update_completion_fence: br::FenceObject::new(device, &br::FenceCreateInfo::new(0))
                .expect("update_completion_fence.create"),
            update_completion_semaphore: br::SemaphoreObject::new(
                device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("update_completion_semaphore.create"),
            updating: false,
            render_cp,
            render_cb,
            render_cb_invalid: true,
            render_requires_preview_composition: false,
            present_ready_semaphores: (0..vk_swapchain.image_count())
                .map(|_| {
                    br::SemaphoreObject::new(device, &br::SemaphoreCreateInfo::new())
                        .expect("rendering_timeline_semaphore create")
                })
                .collect::<Vec<_>>(),
            backbuffer_ready_semaphore: br::SemaphoreObject::new(
                device,
                &br::SemaphoreCreateInfo::new(),
            )
            .expect("backbuffer_ready_semaphore.create"),
            surface,
            swapchain: vk_swapchain,
            swapchain_invalidated: false,
        }
    }

    pub fn take_latest_ui_scale_changes(&self) -> Option<f32> {
        unsafe { &(*self.latest_ui_scale_changes) }
            .lock()
            .expect("poisoned")
            .take()
    }

    pub fn update(
        &mut self,
        current_sec: f32,
        composite_tree: &mut CompositeTreeRender<SyncEvent>,
        glyph_atlas: &mut MaskTextureAtlasManager,
        color_atlas: &ColorTextureAtlasManager,
        mask_atlas_rects: &[AtlasRect],
        vector_raster_state: &mut VectorRasterizationState,
        events: &SyncEventBus,
        font_set: &FontSet,
        preview_composite: &mut PreviewComposite,
        mut validate_preview: impl FnMut(&mut PreviewComposite, &CustomRenderContext),
    ) -> bool {
        let composite_render_data = self.composite_renderer.update(
            self.vk_device,
            composite_tree,
            self.composite_root,
            self.swapchain.size(),
            self.w.ui_scale_factor(),
            font_set,
            glyph_atlas,
            color_atlas,
            mask_atlas_rects,
            vector_raster_state,
            |e| events.push(e),
            current_sec,
        );
        if composite_render_data != self.last_composite_render_data {
            // requires repopulate render commands
            self.invalidate_render_commands();

            self.composite_renderer
                .prepare_input_backdrop_descriptor_sets(
                    self.vk_device,
                    composite_render_data.required_backdrop_buffer_count,
                );
            self.render_requires_preview_composition = false;
            self.composite_renderer.prepare_custom_render(
                &composite_render_data,
                self.swapchain.size(),
                |t, ctx| match t {
                    PREVIEW_COMPOSITE => {
                        self.render_requires_preview_composition = true;
                        validate_preview(preview_composite, &ctx);
                    }
                    _ => (),
                },
            );

            self.last_composite_render_data = composite_render_data;
        }

        self.composite_renderer
            .update_streaming_data(self.vk_device, CompositeStreamingData { current_sec });
        let needs_update_commands = self.composite_renderer.update_backdrop_resources(
            self.vk_device,
            self.surface.format(),
            self.swapchain.size(),
            self.last_composite_render_data
                .required_backdrop_buffer_count
                == 0,
        );

        // update_backdrop_resourcesでDescriptorSetの更新がはしるのでここでやる
        self.validate_render_commands(preview_composite);

        needs_update_commands
    }

    #[cfg(any(feature = "wayland", target_os = "macos"))]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        self.w.take_swapchain_externally_invalidation_signal()
    }
    #[cfg(not(any(feature = "wayland", target_os = "macos")))]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        false
    }

    pub fn invalidate_swapchain(&mut self) {
        self.swapchain_invalidated = true;
    }

    pub fn validate_swapchain<'s>(
        &'s mut self,
        descriptor_writes: &mut Vec<br::DescriptorSetWriteInfo<'s>>,
        event_bus: &SyncEventBus,
    ) {
        if !self.swapchain_invalidated {
            // already valid
            return;
        }

        self.invalidate_render_commands();

        self.surface.refresh_caps();
        self.swapchain
            .recreate(&self.surface, || self.w.pixels_client_size());

        // recrease rt resources
        self.composite_renderer.recreate_rt_resources(
            self.vk_device,
            self.surface.format(),
            self.swapchain.image_view_refs(),
            self.swapchain.size(),
            descriptor_writes,
        );

        event_bus.push(SyncEvent::WindowPostCreateRenderBuffer { window: self.w });
        self.swapchain_invalidated = false;
    }

    pub fn acquire_backbuffer_with_wait(&mut self) -> br::Result<u32> {
        crate::perf_scope!(ACQUIRE_WINDOW_BACKBUFFER);
        let backbuffer_index = self.swapchain.acquire_next(
            None,
            br::CompletionHandlerMut::Queue(
                self.backbuffer_ready_semaphore.as_transparent_ref_mut(),
            ),
        )?;
        /*self.backbuffer_ready_fence
            .wait()
            .expect("last render completion fence wait");
        self.backbuffer_ready_fence
            .reset()
            .expect("last render completion fence reset");*/

        Ok(backbuffer_index)
    }

    pub fn invalidate_render_commands(&mut self) {
        if self.render_cb_invalid {
            // already invalid
            return;
        }

        unsafe {
            self.render_cp
                .reset(br::CommandPoolResetFlags::EMPTY)
                .expect("render_cp.reset");
        }
        self.render_cb_invalid = true;
    }

    pub fn validate_render_commands(&mut self, preview_composite: &PreviewComposite) {
        if !self.render_cb_invalid {
            // already valid
            return;
        }

        for (n, cb) in self.render_cb.iter_mut().enumerate() {
            unsafe {
                cb.begin(&br::CommandBufferBeginInfo::new())
                    .expect("command buffer begin")
            }
            .inject(|r| {
                self.composite_renderer.populate_commands(
                    r,
                    self.vk_device,
                    &self.last_composite_render_data,
                    self.swapchain.size(),
                    &self.swapchain.image_ref(n),
                    n,
                    &mut CustomRenderHandlerFn(|t, s, pm, ctx, r| match t {
                        PREVIEW_COMPOSITE => {
                            preview_composite.populate_commands(s, pm.clone(), &ctx, r)
                        }
                        _ => r,
                    }),
                )
            })
            .inject(|r| {
                if self.w.is_maximized() {
                    // no curout rendering when maximized
                    return r;
                }
                let Some(ref renderer) = self.corner_cutout_renderer else {
                    // no corner cutout rendering required
                    return r;
                };

                let rt_pixel_size = self.w.pixels_client_size();
                let rt_logical_size = self.w.client_size();

                let r = if self
                    .last_composite_render_data
                    .render_passes
                    .last()
                    .is_some_and(|x| x.continued)
                {
                    // use continued pass
                    r.bind_pipeline(br::PipelineBindPoint::Graphics, &renderer.pipeline_cont)
                } else {
                    r.bind_pipeline(br::PipelineBindPoint::Graphics, &renderer.pipeline)
                };

                r.set_viewport(
                    0,
                    &[br::Viewport {
                        x: 0.0,
                        y: 0.0,
                        width: rt_pixel_size.width as _,
                        height: rt_pixel_size.height as _,
                        minDepth: 0.0,
                        maxDepth: 1.0,
                    }],
                )
                .set_scissor(
                    0,
                    &[br::Rect2D {
                        offset: br::Offset2D::ZERO,
                        extent: br::Extent2D {
                            width: rt_pixel_size.width,
                            height: rt_pixel_size.height,
                        },
                    }],
                )
                .push_constant(
                    &renderer.pipeline_layout,
                    br::vk::VK_SHADER_STAGE_ALL_GRAPHICS,
                    0,
                    &CornerCutoutPushConstants {
                        screen_size: [rt_logical_size.width as _, rt_logical_size.height as _],
                    },
                )
                .draw(4, 4, 0, 0)
            })
            .inject(|r| self.vk_device.cmd_end_render_pass(r))
            .end()
            .expect("command buffer end");
        }

        self.render_cb_invalid = false;
    }

    pub fn wait_for_last_update_completion(&mut self) {
        if !self.updating {
            // no updating work
            return;
        }

        self.update_completion_fence
            .wait()
            .expect("update_completion_fence.wait");
        self.update_completion_fence
            .reset()
            .expect("update_completion_fence.reset");
        self.updating = false;
    }

    pub fn repopulate_update_commands(&mut self) {
        unsafe {
            self.update_cp
                .reset(br::CommandPoolResetFlags::EMPTY)
                .expect("update_cp.reset");
        }
        unsafe {
            self.update_cb
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("update_cb.begin")
        }
        .inject(|r| self.composite_renderer.sync_buffer(r))
        .end()
        .expect("update_cb.end");
    }

    pub fn submit_update_commands(&mut self, device_queue: &mut br::QueueObject<&'d VulkanDevice>) {
        self.wait_for_last_update_completion();
        self.repopulate_update_commands();

        device_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[self.update_cb.as_transparent_ref()],
                    &[self.update_completion_semaphore.as_transparent_ref()],
                )],
                Some(self.update_completion_fence.as_transparent_ref_mut()),
            )
            .expect("gfx.update.submit");
        self.updating = true;
    }

    pub fn swapchain_ref<'x>(&'x self) -> br::VkHandleRef<'x, br::vk::VkSwapchainKHR> {
        self.swapchain.as_transparent_ref()
    }

    pub fn update_completion_semaphore_ref<'x>(
        &'x self,
    ) -> br::VkHandleRef<'x, br::vk::VkSemaphore> {
        self.update_completion_semaphore.as_transparent_ref()
    }

    pub fn primary_render_commands_ref<'x>(
        &'x self,
        backbuffer_index: u32,
    ) -> br::VkHandleRef<'x, br::vk::VkCommandBuffer> {
        self.render_cb[backbuffer_index as usize].as_transparent_ref()
    }

    pub fn present_ready_semaphore_ref<'x>(
        &'x self,
        backbuffer_index: u32,
    ) -> br::VkHandleRef<'x, br::vk::VkSemaphore> {
        self.present_ready_semaphores[backbuffer_index as usize].as_transparent_ref()
    }
}

#[derive(br::SpecializationConstants)]
struct FillShaderVertexConstants {
    #[constant_id = 0]
    target_texture_width: f32,
    #[constant_id = 1]
    target_texture_height: f32,
}
#[derive(br::SpecializationConstants)]
struct CurveShaderVertexConstants {
    #[constant_id = 0]
    target_texture_width: f32,
    #[constant_id = 1]
    target_texture_height: f32,
}

#[derive(Clone)]
pub struct GlyphAtlasRenderingFormats {
    pub color: br::Format,
    pub stencil: br::Format,
}

pub struct GlyphAtlasManagerCommonResources<'d> {
    device: &'d VulkanDevice<'d>,
    fill_shader_module: br::ShaderModuleObject<&'d VulkanDevice<'d>>,
    curve_shader_module: br::ShaderModuleObject<&'d VulkanDevice<'d>>,
    vec_tri_fill_shader_module: br::ShaderModuleObject<&'d VulkanDevice<'d>>,
    fill_tri_white_shader_module: br::ShaderModuleObject<&'d VulkanDevice<'d>>,
    render_pass: br::RenderPassObject<&'d VulkanDevice<'d>>,
    pipeline_layout: br::PipelineLayoutObject<&'d VulkanDevice<'d>>,
}
impl<'d> GlyphAtlasManagerCommonResources<'d> {
    pub fn new(vk_device: &'d VulkanDevice, formats: &GlyphAtlasRenderingFormats) -> Self {
        let fill_shader_module = vk_device.require_shader("vg-fill.spv");
        let curve_shader_module = vk_device.require_shader("vg-curve.spv");
        let vec_tri_fill_shader_module = vk_device.require_shader("vec-tri-fill.spv");
        let fill_tri_white_shader_module = vk_device.require_shader("tri-fill-white.spv");

        let render_pass = br::RenderPassObject::new(
            vk_device,
            &br::RenderPassCreateInfo2::new(
                &[
                    br::AttachmentDescription2::new(formats.stencil)
                        .stencil_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
                        .layout_transition(
                            br::ImageLayout::Undefined,
                            br::ImageLayout::DepthStencilReadOnlyOpt,
                        )
                        .samples(TextureAtlas::MULTISAMPLE_LEVEL),
                    br::AttachmentDescription2::new(formats.color)
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)
                        .layout_transition(
                            br::ImageLayout::Undefined,
                            br::ImageLayout::TransferSrcOpt,
                        )
                        .samples(TextureAtlas::MULTISAMPLE_LEVEL),
                ],
                &[
                    br::SubpassDescription2::new()
                        .depth_stencil(&br::AttachmentReference2::depth_stencil_attachment_opt(0)),
                    br::SubpassDescription2::new()
                        .depth_stencil(&br::AttachmentReference2::depth_stencil_readonly_opt(0))
                        .colors(&[br::AttachmentReference2::color_attachment_opt(1)]),
                ],
                &[
                    br::SubpassDependency2::new(
                        br::SubpassIndex::Internal(0),
                        br::SubpassIndex::Internal(1),
                    )
                    .by_region()
                    .of_memory(
                        br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.write,
                        br::AccessFlags::DEPTH_STENCIL_ATTACHMENT.read,
                    )
                    .of_execution(
                        br::PipelineStageFlags::LATE_FRAGMENT_TESTS,
                        br::PipelineStageFlags::EARLY_FRAGMENT_TESTS,
                    ),
                    br::SubpassDependency2::new(
                        br::SubpassIndex::Internal(1),
                        br::SubpassIndex::External,
                    )
                    .by_region()
                    .of_memory(
                        br::AccessFlags::COLOR_ATTACHMENT.write,
                        br::AccessFlags::TRANSFER.read,
                    )
                    .of_execution(
                        br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                        br::PipelineStageFlags::TRANSFER,
                    ),
                ],
            ),
        )
        .expect("render_pass.create");
        let pipeline_layout =
            br::PipelineLayoutObject::new(vk_device, &br::PipelineLayoutCreateInfo::new(&[], &[]))
                .expect("pipeline_layout.create");

        vk_device.dbg_set_name(&render_pass, c"GlyphAtlasManager.VgRasterize.RenderPass");
        vk_device.dbg_set_name(
            &pipeline_layout,
            c"GlyphAtlasManager.VgRasterize.PipelineLayout",
        );

        Self {
            device: vk_device,
            fill_shader_module,
            curve_shader_module,
            vec_tri_fill_shader_module,
            fill_tri_white_shader_module,
            render_pass,
            pipeline_layout,
        }
    }
}

#[repr(C)]
struct CornerCutoutPushConstants {
    pub screen_size: [f32; 2],
}

struct CornerCutoutRenderer<'d, 'fs> {
    _shader: br::ShaderModuleObject<&'d VulkanDevice<'fs>>,
    pipeline_layout: br::PipelineLayoutObject<&'d VulkanDevice<'fs>>,
    pipeline: br::PipelineObject<&'d VulkanDevice<'fs>>,
    pipeline_cont: br::PipelineObject<&'d VulkanDevice<'fs>>,
}
impl<'d, 'fs> CornerCutoutRenderer<'d, 'fs> {
    fn new(
        device: &'d VulkanDevice<'fs>,
        rendered_pass: br::SubpassRef<impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized>,
        rendered_pass_cont: br::SubpassRef<
            impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized,
        >,
    ) -> Self {
        let shader = device.require_shader("corner-cutout.spv");
        let pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[],
                &[
                    br::PushConstantRange::for_type::<CornerCutoutPushConstants>(
                        br::vk::VK_SHADER_STAGE_ALL_GRAPHICS,
                        0,
                    ),
                ],
            ),
        )
        .expect("pipeline_layout.create");
        let blending = br::PipelineColorBlendStateCreateInfo::new(&[
            br::PipelineColorBlendAttachmentState(br::vk::VkPipelineColorBlendAttachmentState {
                blendEnable: true as _,
                srcColorBlendFactor: br::vk::VK_BLEND_FACTOR_ZERO,
                dstColorBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
                colorBlendOp: br::vk::VK_BLEND_OP_ADD,
                srcAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_ONE,
                dstAlphaBlendFactor: br::vk::VK_BLEND_FACTOR_SRC_ALPHA,
                alphaBlendOp: br::vk::VK_BLEND_OP_ADD,
                colorWriteMask: br::vk::VK_COLOR_COMPONENT_R_BIT
                    | br::vk::VK_COLOR_COMPONENT_G_BIT
                    | br::vk::VK_COLOR_COMPONENT_B_BIT
                    | br::vk::VK_COLOR_COMPONENT_A_BIT,
            }),
            // br::vk::VkPipelineColorBlendAttachmentState::PREMULTIPLIED,
        ]);
        let [pipeline, pipeline_cont] = device
            .create_graphics_pipelines_array(&[
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    rendered_pass,
                    &[
                        shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                        shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRISTRIP,
                    &br::PipelineViewportStateCreateInfo::new_dynamic(1),
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    &blending,
                )
                .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
                .set_dynamic_state(&br::PipelineDynamicStateCreateInfo::new(&[
                    br::vk::VK_DYNAMIC_STATE_VIEWPORT,
                    br::vk::VK_DYNAMIC_STATE_SCISSOR,
                ])),
                br::GraphicsPipelineCreateInfo::new(
                    &pipeline_layout,
                    rendered_pass_cont,
                    &[
                        shader.on_stage(br::ShaderStage::Vertex, c"vertMain"),
                        shader.on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRISTRIP,
                    &br::PipelineViewportStateCreateInfo::new_dynamic(1),
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    &blending,
                )
                .set_multisample_state(&br::PipelineMultisampleStateCreateInfo::new())
                .set_dynamic_state(&br::PipelineDynamicStateCreateInfo::new(&[
                    br::vk::VK_DYNAMIC_STATE_VIEWPORT,
                    br::vk::VK_DYNAMIC_STATE_SCISSOR,
                ])),
            ])
            .expect("pipeline.create");

        Self {
            _shader: shader,
            pipeline_layout,
            pipeline,
            pipeline_cont,
        }
    }
}

pub struct MaskTextureAtlasManager<'d> {
    device: &'d VulkanDevice<'d>,
    atlas: TextureAtlas,
    acquired_glyph_rects: HashMap<(usize, u16), AtlasRect>,
    rounded_fill_rects_by_radius: HashMap<SafeF32, AtlasRect>,
    triangle_fans_pipeline: br::PipelineObject<&'d VulkanDevice<'d>>,
    curve_pipeline: br::PipelineObject<&'d VulkanDevice<'d>>,
    colorize_pipeline: br::PipelineObject<&'d VulkanDevice<'d>>,
    fill_tri_white_pipeline: br::PipelineObject<&'d VulkanDevice<'d>>,
}
impl Drop for MaskTextureAtlasManager<'_> {
    fn drop(&mut self) {
        unsafe {
            self.atlas.drop(self.device);
        }
    }
}
impl<'d> MaskTextureAtlasManager<'d> {
    const VI_STATE_FOR_TRIS: &'static br::PipelineVertexInputStateCreateInfo<'static> =
        &br::PipelineVertexInputStateCreateInfo::new(
            &[br::VertexInputBindingDescription::per_vertex_typed::<
                [f32; 2],
            >(0)],
            &[br::VertexInputAttributeDescription(
                br::vk::VkVertexInputAttributeDescription {
                    location: 0,
                    binding: 0,
                    offset: 0,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                },
            )],
        );
    const VI_STATE_FOR_CURVE: &'static br::PipelineVertexInputStateCreateInfo<'static> =
        &br::PipelineVertexInputStateCreateInfo::new(
            &[br::VertexInputBindingDescription::per_vertex_typed::<
                [f32; 4],
            >(0)],
            &[
                br::VertexInputAttributeDescription(br::vk::VkVertexInputAttributeDescription {
                    location: 0,
                    binding: 0,
                    offset: 0,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                }),
                br::VertexInputAttributeDescription(br::vk::VkVertexInputAttributeDescription {
                    location: 1,
                    binding: 0,
                    offset: core::mem::size_of::<[f32; 2]>() as _,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                }),
            ],
        );
    const STENCIL_MASK: u32 = 0x01;
    const STENCIL_STATE_INVERT: &'static br::PipelineDepthStencilStateCreateInfo =
        &br::PipelineDepthStencilStateCreateInfo::new()
            .stencil_test(true)
            .stencil_state_front(
                br::StencilOpState::always_forall(br::StencilOp::Invert)
                    .write_mask(Self::STENCIL_MASK),
            )
            .stencil_state_back(
                br::StencilOpState::always_forall(br::StencilOp::Invert)
                    .write_mask(Self::STENCIL_MASK),
            );
    const STENCIL_STATE_FILTER_EQ_ONLY: &'static br::PipelineDepthStencilStateCreateInfo =
        &br::PipelineDepthStencilStateCreateInfo::new()
            .stencil_test(true)
            .stencil_state_front(br::StencilOpState::NOP.set_compare(
                br::CompareOp::Equal,
                Self::STENCIL_MASK,
                Self::STENCIL_MASK,
            ))
            .stencil_state_back(br::StencilOpState::NOP.set_compare(
                br::CompareOp::Equal,
                Self::STENCIL_MASK,
                Self::STENCIL_MASK,
            ));

    pub fn new(
        common_res: &GlyphAtlasManagerCommonResources<'d>,
        init_worker_queue: &mut (impl br::QueueMut + ?Sized),
        init_worker_queue_family_index: u32,
    ) -> Self {
        let atlas = TextureAtlas::new(common_res.device);

        let viewports = [atlas
            .size()
            .into_rect(br::Offset2D::ZERO)
            .make_viewport(0.0..1.0)];
        let scissors = [atlas.size().into_rect(br::Offset2D::ZERO)];
        let vp_state = br::PipelineViewportStateCreateInfo::new(&viewports, &scissors);
        let ms_state = br::PipelineMultisampleStateCreateInfo::new()
            .rasterization_samples(TextureAtlas::MULTISAMPLE_LEVEL as _);
        let [
            triangle_fans_pipeline,
            curve_pipeline,
            colorize_pipeline,
            fill_tri_white_pipeline,
        ] = common_res
            .device
            .create_graphics_pipelines_array(&[
                br::GraphicsPipelineCreateInfo::new(
                    &common_res.pipeline_layout,
                    common_res.render_pass.subpass(0),
                    &[
                        common_res
                            .fill_shader_module
                            .on_stage(br::ShaderStage::Vertex, c"vertMain")
                            .with_specialization_info(&br::SpecializationInfo::new(
                                &FillShaderVertexConstants {
                                    target_texture_width: atlas.size().width as _,
                                    target_texture_height: atlas.size().height as _,
                                },
                            )),
                        common_res
                            .fill_shader_module
                            .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    Self::VI_STATE_FOR_TRIS,
                    IA_STATE_TRILIST,
                    &vp_state,
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(&ms_state)
                .set_depth_stencil_state(Self::STENCIL_STATE_INVERT),
                br::GraphicsPipelineCreateInfo::new(
                    &common_res.pipeline_layout,
                    common_res.render_pass.subpass(0),
                    &[
                        common_res
                            .curve_shader_module
                            .on_stage(br::ShaderStage::Vertex, c"vertMain")
                            .with_specialization_info(&br::SpecializationInfo::new(
                                &CurveShaderVertexConstants {
                                    target_texture_width: atlas.size().width as _,
                                    target_texture_height: atlas.size().height as _,
                                },
                            )),
                        common_res
                            .curve_shader_module
                            .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    Self::VI_STATE_FOR_CURVE,
                    IA_STATE_TRILIST,
                    &vp_state,
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(&ms_state)
                .set_depth_stencil_state(Self::STENCIL_STATE_INVERT),
                br::GraphicsPipelineCreateInfo::new(
                    &common_res.pipeline_layout,
                    common_res.render_pass.subpass(1),
                    &[
                        common_res
                            .vec_tri_fill_shader_module
                            .on_stage(br::ShaderStage::Vertex, c"vertMain"),
                        common_res
                            .vec_tri_fill_shader_module
                            .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRILIST,
                    &vp_state,
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(&ms_state)
                .set_depth_stencil_state(Self::STENCIL_STATE_FILTER_EQ_ONLY),
                br::GraphicsPipelineCreateInfo::new(
                    &common_res.pipeline_layout,
                    common_res.render_pass.subpass(1),
                    &[
                        common_res
                            .fill_tri_white_shader_module
                            .on_stage(br::ShaderStage::Vertex, c"vertMain"),
                        common_res
                            .fill_tri_white_shader_module
                            .on_stage(br::ShaderStage::Fragment, c"fragMain"),
                    ],
                    Self::VI_STATE_FOR_TRIS,
                    IA_STATE_TRILIST,
                    &vp_state,
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(&ms_state),
            ])
            .expect("create vector rasterize pipelines");

        let mut init_cp = br::CommandPoolObject::new(
            common_res.device,
            &br::CommandPoolCreateInfo::new(init_worker_queue_family_index),
        )
        .expect("init_cp.create");
        let [mut init_cb] = br::CommandBufferObject::alloc_array(
            common_res.device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut init_cp,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("init_cb.create");
        unsafe {
            init_cb
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("init_cb.begin")
        }
        .inject(|r| {
            common_res.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        br::ImageMemoryBarrier2::new(&atlas.image(), atlas.image_range_entire())
                            .transit_to(br::ImageLayout::TransferDestOpt.from_undefined()),
                    ],
                ),
            )
        })
        .clear_color_image(
            &atlas.image(),
            br::ImageLayout::TransferDestOpt,
            &[br::ClearColorValue::from([0.0; 4])],
            &[br::ImageSubresourceRange::new(
                br::AspectMask::COLOR,
                0..1,
                0..1,
            )],
        )
        .inject(|r| {
            common_res.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[],
                    &[],
                    &[
                        br::ImageMemoryBarrier2::new(&atlas.image(), atlas.image_range_entire())
                            .transit_to(
                                br::ImageLayout::ShaderReadOnlyOpt
                                    .from(br::ImageLayout::TransferDestOpt),
                            )
                            .from(
                                br::PipelineStageFlags2::CLEAR,
                                br::AccessFlags2::TRANSFER.write,
                            )
                            .to(
                                br::PipelineStageFlags2::FRAGMENT_SHADER,
                                br::AccessFlags2::SHADER.read,
                            ),
                    ],
                ),
            )
        })
        .end()
        .expect("init_cb.end");
        init_worker_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[init_cb.as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("init_cb.submit");
        init_worker_queue.wait().expect("init_cb.wait");

        Self {
            device: common_res.device,
            atlas,
            acquired_glyph_rects: HashMap::new(),
            rounded_fill_rects_by_radius: HashMap::new(),
            triangle_fans_pipeline,
            curve_pipeline,
            colorize_pipeline,
            fill_tri_white_pipeline,
        }
    }

    #[inline(always)]
    pub fn acquire(&mut self, width: u32, height: u32) -> AtlasRect {
        self.atlas.acquire(width, height)
    }

    pub fn acquire_for_glyph(
        &mut self,
        key: (usize, u16),
        width: u32,
        height: u32,
    ) -> (AtlasRect, bool) {
        match self.acquired_glyph_rects.entry(key) {
            std::collections::hash_map::Entry::Vacant(x) => {
                (x.insert(self.atlas.acquire(width, height)).clone(), true)
            }
            std::collections::hash_map::Entry::Occupied(x) => (x.get().clone(), false),
        }
    }

    pub fn acquire_for_rounded_fill_rect(&mut self, radius: SafeF32) -> (AtlasRect, bool) {
        match self.rounded_fill_rects_by_radius.entry(radius) {
            std::collections::hash_map::Entry::Vacant(x) => (
                x.insert(self.atlas.acquire(
                    (radius.value() * 2.0 + 1.0).ceil() as _,
                    (radius.value() * 2.0 + 1.0).ceil() as _,
                ))
                .clone(),
                true,
            ),
            std::collections::hash_map::Entry::Occupied(x) => (x.get().clone(), false),
        }
    }

    fn perform_render(
        &self,
        state: &VectorRasterizationState,
        formats: &GlyphAtlasRenderingFormats,
        common_res: &GlyphAtlasManagerCommonResources,
        render_worker_queue: &mut (impl br::QueueMut + ?Sized),
        normalized_2d_static_mesh_textures: &HashMap<TextureID, Normalized2DStaticMeshTexture>,
        scale: f32,
    ) {
        // TODO: 最適化はあとで
        let mut normalized_2d_static_mesh_vertices_fused = Vec::new();
        let mut normalized_2d_static_mesh_indices_fused = Vec::new();
        for (x, &(ox, oy)) in state.normalized_2d_mesh_requests.iter() {
            let e = &normalized_2d_static_mesh_textures[x];
            let v_offset = normalized_2d_static_mesh_vertices_fused.len();

            normalized_2d_static_mesh_vertices_fused.extend(e.vertices.iter().map(|&[x, y]| {
                [
                    2.0 * (x * e.width * scale + ox as f32) / self.atlas.size().width as f32 - 1.0,
                    2.0 * (y * e.height * scale + oy as f32) / self.atlas.size().height as f32
                        - 1.0,
                ]
            }));
            normalized_2d_static_mesh_indices_fused
                .extend(e.indices.iter().map(|i| *i + v_offset as u16));
        }

        let filltri_points_offset = 0;
        let filltri_indices_offset =
            filltri_points_offset + core::mem::size_of_val(&state.fill_tri_points[..]);
        let curve_triangles_offset = (filltri_indices_offset
            + core::mem::size_of_val(&state.fill_tri_indices[..])
            + (core::mem::size_of::<[f32; 4]>() - 1))
            & !(core::mem::size_of::<[f32; 4]>() - 1);
        let normalized_2d_static_mesh_vertices_offset =
            curve_triangles_offset + core::mem::size_of_val(&state.curve_tris[..]);
        let normalized_2d_static_mesh_indices_offset = normalized_2d_static_mesh_vertices_offset
            + core::mem::size_of_val(&normalized_2d_static_mesh_vertices_fused[..]);

        let vector_draw_buffer_total_size = normalized_2d_static_mesh_indices_offset
            + core::mem::size_of_val(&normalized_2d_static_mesh_indices_fused[..]);
        let mut vector_draw_buffer = br::BufferObject::new(
            self.device,
            &br::BufferCreateInfo::new(
                vector_draw_buffer_total_size as _,
                br::BufferUsage::VERTEX_BUFFER
                    | br::BufferUsage::INDEX_BUFFER
                    | br::BufferUsage::TRANSFER_DEST,
            ),
        )
        .expect("vector_draw_buffer create");
        let vector_draw_buffer_memreq = vector_draw_buffer.requirements();
        let vector_draw_buffer_memory = br::DeviceMemoryObject::new(
            self.device,
            &br::MemoryAllocateInfo::new(
                vector_draw_buffer_memreq.size,
                self.device
                    .find_device_local_memory_index(vector_draw_buffer_memreq.memoryTypeBits)
                    .expect("no suitable memory"),
            ),
        )
        .expect("vector_draw_buffer malloc");
        vector_draw_buffer
            .bind(&vector_draw_buffer_memory, 0)
            .expect("vector_draw_buffer bind");

        let mut vector_draw_init_buffer = br::BufferObject::new(
            self.device,
            &br::BufferCreateInfo::new(
                vector_draw_buffer_total_size as _,
                br::BufferUsage::TRANSFER_SRC,
            ),
        )
        .expect("vector_draw_init_buffer create");
        let vector_draw_init_buffer_memreq = vector_draw_init_buffer.requirements();
        let vector_draw_init_buffer_memindex = self
            .device
            .find_host_visible_memory_index(vector_draw_init_buffer_memreq.memoryTypeBits)
            .expect("no suitable memory");
        let mut vector_draw_init_buffer_memory = br::DeviceMemoryObject::new(
            self.device,
            &br::MemoryAllocateInfo::new(
                vector_draw_init_buffer_memreq.size,
                vector_draw_init_buffer_memindex,
            ),
        )
        .expect("vector_draw_init_buffer malloc");
        vector_draw_init_buffer
            .bind(&vector_draw_init_buffer_memory, 0)
            .expect("vector_draw_init_buffer bind");
        let p = vector_draw_init_buffer_memory
            .map(0..vector_draw_buffer_total_size)
            .expect("vector_draw_init_buffer_memory map");
        unsafe {
            core::ptr::copy_nonoverlapping(
                state.fill_tri_points.as_ptr(),
                p.ptr().byte_add(filltri_points_offset).cast(),
                state.fill_tri_points.len(),
            );
            core::ptr::copy_nonoverlapping(
                state.fill_tri_indices.as_ptr(),
                p.ptr().byte_add(filltri_indices_offset).cast(),
                state.fill_tri_indices.len(),
            );
            core::ptr::copy_nonoverlapping(
                state.curve_tris.as_ptr(),
                p.ptr().byte_add(curve_triangles_offset).cast(),
                state.curve_tris.len(),
            );
            core::ptr::copy_nonoverlapping(
                normalized_2d_static_mesh_vertices_fused.as_ptr(),
                p.ptr()
                    .byte_add(normalized_2d_static_mesh_vertices_offset)
                    .cast(),
                normalized_2d_static_mesh_vertices_fused.len(),
            );
            core::ptr::copy_nonoverlapping(
                normalized_2d_static_mesh_indices_fused.as_ptr(),
                p.ptr()
                    .byte_add(normalized_2d_static_mesh_indices_offset)
                    .cast(),
                normalized_2d_static_mesh_indices_fused.len(),
            );
        }
        if !self
            .device
            .is_coherent_memory(vector_draw_init_buffer_memindex)
        {
            unsafe {
                self.device
                    .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new(
                        &vector_draw_init_buffer_memory,
                        0..vector_draw_buffer_total_size as u64,
                    )])
                    .expect("flush_mapped_memory_ranges");
            }
        }
        unsafe {
            vector_draw_init_buffer_memory.unmap();
        }

        let mut vector_color_ms_buffer = br::ImageObject::new(
            self.device,
            &br::ImageCreateInfo::new(*self.atlas.size(), formats.color)
                .set_usage(
                    br::ImageUsageFlags::COLOR_ATTACHMENT | br::ImageUsageFlags::TRANSFER_SRC,
                )
                .sample_counts(TextureAtlas::MULTISAMPLE_LEVEL),
        )
        .expect("vector color_ms buffer create");
        self.device
            .dbg_set_name(&vector_color_ms_buffer, c"Vector::color_ms_buffer");
        let mut vector_stencil_buffer = br::ImageObject::new(
            self.device,
            &br::ImageCreateInfo::new(*self.atlas.size(), formats.stencil)
                .set_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT)
                .sample_counts(TextureAtlas::MULTISAMPLE_LEVEL),
        )
        .expect("vector stencil buffer create");
        self.device
            .dbg_set_name(&vector_stencil_buffer, c"Vector::stencil_buffer");
        let vector_color_ms_buffer_memreq = vector_color_ms_buffer.requirements();
        let vector_stencil_buffer_memreq = vector_stencil_buffer.requirements();
        tracing::debug!(
            ?vector_color_ms_buffer_memreq,
            ?vector_stencil_buffer_memreq
        );
        let vector_color_ms_buffer_mem = br::DeviceMemoryObject::new(
            self.device,
            &br::MemoryAllocateInfo::new(
                vector_color_ms_buffer_memreq.size,
                self.device
                    .find_lazily_allocatable_device_local_memory_index(
                        vector_color_ms_buffer_memreq.memoryTypeBits,
                    )
                    .expect("no suitable memory"),
            ),
        )
        .expect("vector color_ms buffer malloc");
        vector_color_ms_buffer
            .bind(&vector_color_ms_buffer_mem, 0)
            .expect("vector color_ms buffer bind");
        let vector_color_ms_buffer = br::ImageViewBuilder::new(
            vector_color_ms_buffer,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("vector color_ms buffer imageview create");
        let vector_stencil_buffer_mem = br::DeviceMemoryObject::new(
            self.device,
            &br::MemoryAllocateInfo::new(
                vector_stencil_buffer_memreq.size,
                self.device
                    .find_lazily_allocatable_device_local_memory_index(
                        vector_stencil_buffer_memreq.memoryTypeBits,
                    )
                    .expect("no suitable memory"),
            ),
        )
        .expect("vector stencil buffer malloc");
        vector_stencil_buffer
            .bind(&vector_stencil_buffer_mem, 0)
            .expect("vector stencil buffer bind");
        let vector_stencil_buffer = br::ImageViewBuilder::new(
            vector_stencil_buffer,
            br::ImageSubresourceRange::new(br::AspectMask::STENCIL, 0..1, 0..1),
        )
        .create()
        .expect("vector stencil buffer imageview create");
        let vector_framebuffer = br::FramebufferObject::new(
            self.device,
            &br::FramebufferCreateInfo::new(
                &common_res.render_pass,
                &[
                    vector_stencil_buffer.as_transparent_ref(),
                    vector_color_ms_buffer.as_transparent_ref(),
                ],
                self.atlas.size().width,
                self.atlas.size().height,
            ),
        )
        .expect("vector framebuffer create");

        let mut cp = br::CommandPoolObject::new(
            self.device,
            &br::CommandPoolCreateInfo::new(self.device.present_queue_family_index()),
        )
        .expect("cp init");
        let mut cb = br::CommandBufferObject::alloc(
            self.device,
            &br::CommandBufferAllocateInfo::new(&mut cp, 1, br::CommandBufferLevel::Primary),
        )
        .expect("alloc cb");
        unsafe {
            cb[0]
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("cb begin")
        }
        .copy_buffer(
            &vector_draw_init_buffer,
            &vector_draw_buffer,
            &[br::BufferCopy::mirror(
                0,
                vector_draw_buffer_total_size as _,
            )],
        )
        .inject(|r| {
            self.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .from(
                            br::PipelineStageFlags2::COPY,
                            br::AccessFlags2::TRANSFER.write,
                        )
                        .to(
                            br::PipelineStageFlags2::VERTEX_INPUT,
                            br::AccessFlags2::VERTEX_ATTRIBUTE_READ | br::AccessFlags2::INDEX_READ,
                        )],
                    &[],
                    &[],
                ),
            )
        })
        .begin_render_pass(
            &br::RenderPassBeginInfo::new(
                &common_res.render_pass,
                &vector_framebuffer,
                self.atlas.size().into_rect(br::Offset2D::ZERO),
                &[
                    br::ClearValue::depth_stencil(1.0, 0),
                    br::ClearValue::color_f32([0.0; 4]),
                ],
            ),
            br::SubpassContents::Inline,
        )
        .bind_pipeline(
            br::PipelineBindPoint::Graphics,
            &self.triangle_fans_pipeline,
        )
        .bind_vertex_buffer_array(
            0,
            &[vector_draw_buffer.as_transparent_ref()],
            &[filltri_points_offset as _],
        )
        .bind_index_buffer(
            &vector_draw_buffer,
            filltri_indices_offset,
            br::IndexType::U16,
        )
        .draw_indexed(state.fill_tri_indices.len() as _, 1, 0, 0, 0)
        .bind_pipeline(br::PipelineBindPoint::Graphics, &self.curve_pipeline)
        .bind_vertex_buffer_array(
            0,
            &[vector_draw_buffer.as_transparent_ref()],
            &[curve_triangles_offset as _],
        )
        .draw(state.curve_tris.len() as _, 1, 0, 0)
        .next_subpass(br::SubpassContents::Inline)
        .bind_pipeline(br::PipelineBindPoint::Graphics, &self.colorize_pipeline)
        .draw(3, 1, 0, 0)
        .inject(|r| {
            if normalized_2d_static_mesh_vertices_fused.is_empty() {
                // no render requested
                return r;
            }

            r.bind_pipeline(
                br::PipelineBindPoint::Graphics,
                &self.fill_tri_white_pipeline,
            )
            .bind_vertex_buffer_array(
                0,
                &[vector_draw_buffer.as_transparent_ref()],
                &[normalized_2d_static_mesh_vertices_offset as _],
            )
            .bind_index_buffer(
                &vector_draw_buffer,
                normalized_2d_static_mesh_indices_offset as _,
                br::IndexType::U16,
            )
            .draw_indexed(
                normalized_2d_static_mesh_indices_fused.len() as _,
                1,
                0,
                0,
                0,
            )
        })
        .end_render_pass()
        .inject(|r| {
            self.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[],
                    &[],
                    &[br::ImageMemoryBarrier2::new(
                        &self.atlas.image(),
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    )
                    .transferring_layout(
                        br::ImageLayout::ShaderReadOnlyOpt,
                        br::ImageLayout::TransferDestOpt,
                    )],
                ),
            )
        })
        .resolve_image(
            vector_color_ms_buffer.image(),
            br::ImageLayout::TransferSrcOpt,
            &self.atlas.image(),
            br::ImageLayout::TransferDestOpt,
            &state
                .updated_rects
                .iter()
                .map(|r| br::vk::VkImageResolve {
                    srcSubresource: br::ImageSubresourceLayers::new(br::AspectMask::COLOR, 0, 0..1)
                        .0,
                    srcOffset: r.offset.with_z(0),
                    dstSubresource: br::ImageSubresourceLayers::new(br::AspectMask::COLOR, 0, 0..1)
                        .0,
                    dstOffset: r.offset.with_z(0),
                    extent: r.extent.with_depth(1),
                })
                .collect::<Vec<_>>(),
        )
        .inject(|r| {
            self.device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[],
                    &[],
                    &[br::ImageMemoryBarrier2::new(
                        &self.atlas.image(),
                        br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
                    )
                    .from(
                        br::PipelineStageFlags2::RESOLVE,
                        br::AccessFlags2::TRANSFER.write,
                    )
                    .to(
                        br::PipelineStageFlags2::FRAGMENT_SHADER,
                        br::AccessFlags2::SHADER.read,
                    )
                    .transferring_layout(
                        br::ImageLayout::TransferDestOpt,
                        br::ImageLayout::ShaderReadOnlyOpt,
                    )],
                ),
            )
        })
        .end()
        .expect("cb end");

        render_worker_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[cb[0].as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("vector render submit");
        render_worker_queue.wait().expect("vector render wait");
    }

    pub const fn atlas(&self) -> &TextureAtlas {
        &self.atlas
    }

    pub fn atlas_mut(&mut self) -> &mut TextureAtlas {
        &mut self.atlas
    }

    pub fn clear(&mut self) {
        self.atlas.clear();
        self.acquired_glyph_rects.clear();
    }
}

pub struct ColorTextureAtlasManager<'d> {
    device: &'d VulkanDevice<'d>,
    atlas: ColorTextureAtlas,
}
impl Drop for ColorTextureAtlasManager<'_> {
    fn drop(&mut self) {
        unsafe {
            self.atlas.drop(self.device);
        }
    }
}
impl<'d> ColorTextureAtlasManager<'d> {
    pub fn new(
        common_res: &GlyphAtlasManagerCommonResources<'d>,
        init_worker_queue: &mut (impl br::QueueMut + ?Sized),
        init_worker_queue_family_index: u32,
    ) -> Self {
        let atlas = ColorTextureAtlas::new(common_res.device);

        let init_rp = br::RenderPassObject::new(
            common_res.device,
            &br::RenderPassCreateInfo::new(
                &[br::AttachmentDescription::new(
                    atlas.format(),
                    br::ImageLayout::Undefined,
                    br::ImageLayout::ShaderReadOnlyOpt,
                )
                .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)],
                &[br::SubpassDescription::new().color_attachments(
                    &[br::AttachmentReference::new(
                        0,
                        br::ImageLayout::ColorAttachmentOpt,
                    )],
                    &[],
                )],
                &[br::vk::VkSubpassDependency {
                    srcSubpass: 0,
                    dstSubpass: br::vk::VK_SUBPASS_EXTERNAL,
                    srcAccessMask: br::AccessFlags::COLOR_ATTACHMENT.write,
                    dstAccessMask: br::AccessFlags::SHADER.read,
                    srcStageMask: br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT.0,
                    dstStageMask: br::PipelineStageFlags::FRAGMENT_SHADER.0,
                    dependencyFlags: br::vk::VK_DEPENDENCY_BY_REGION_BIT,
                }],
            ),
        )
        .expect("init_rp.create");
        let init_fb = br::FramebufferObject::new(
            common_res.device,
            &br::FramebufferCreateInfo::new(
                &init_rp,
                &[atlas.as_image_view().as_transparent_ref()],
                atlas.size().width,
                atlas.size().height,
            ),
        )
        .expect("init_fb.create");

        let mut init_cp = br::CommandPoolObject::new(
            common_res.device,
            &br::CommandPoolCreateInfo::new(init_worker_queue_family_index),
        )
        .expect("init_cp.create");
        let [mut init_cb] = br::CommandBufferObject::alloc_array(
            common_res.device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut init_cp,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("init_cb.create");
        unsafe {
            init_cb
                .begin(&br::CommandBufferBeginInfo::new())
                .expect("init_cb.begin")
        }
        .begin_render_pass(
            &br::RenderPassBeginInfo::new(
                &init_rp,
                &init_fb,
                atlas.size().into_rect(br::Offset2D::ZERO),
                &[br::ClearValue::color_f32([0.0; 4])],
            ),
            br::SubpassContents::Inline,
        )
        .end_render_pass()
        .end()
        .expect("init_cb.end");
        init_worker_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[init_cb.as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("init_cb.submit");
        init_worker_queue.wait().expect("init_cb.wait");

        Self {
            device: common_res.device,
            atlas,
        }
    }

    #[inline(always)]
    pub fn acquire(&mut self, width: u32, height: u32) -> AtlasRect {
        self.atlas.acquire(width, height)
    }

    fn perform_render(
        &self,
        shader_texture_rasterize_requests: &[(AtlasRect, ShaderTexture)],
        render_scale: f32,
        vk_device: &VulkanDevice,
        render_worker_queue: &mut (impl br::QueueMut + ?Sized),
    ) {
        #[derive(SpecializationConstants)]
        struct SpecConstantStorage {
            #[constant_id = 0]
            render_width_px: f32,
            #[constant_id = 1]
            render_height_px: f32,
        }

        // TODO: 最適化はあとで
        let pipeline_layout =
            br::PipelineLayoutObject::new(vk_device, &br::PipelineLayoutCreateInfo::new(&[], &[]))
                .expect("pipeline_layout.create");
        let render_pass = br::RenderPassObject::new(
            vk_device,
            &br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(self.atlas.format())
                    .color_memory_op(br::LoadOp::DontCare, br::StoreOp::Store)
                    .with_layout_to(br::ImageLayout::ShaderReadOnlyOpt.from_undefined())],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .by_region()
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::SHADER.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::FRAGMENT_SHADER,
                )],
            ),
        )
        .expect("render_pass.create");
        let fb = br::FramebufferObject::new(
            vk_device,
            &br::FramebufferCreateInfo::new(
                &render_pass,
                &[self.atlas.as_image_view().as_transparent_ref()],
                self.atlas.size().width,
                self.atlas.size().height,
            ),
        )
        .expect("fb.create");
        let render_resources = shader_texture_rasterize_requests
            .iter()
            .map(|(r, s)| {
                let shader = vk_device.require_shader(&s.shader_path);
                let viewport = [r.vk_rect().make_viewport(0.0..1.0)];
                let scissor = [r.vk_rect()];
                let spec_const = SpecConstantStorage {
                    render_width_px: r.width() as _,
                    render_height_px: r.height() as _,
                };
                let [pipeline] = vk_device
                    .create_graphics_pipelines_array(&[br::GraphicsPipelineCreateInfo::new(
                        &pipeline_layout,
                        render_pass.subpass(0),
                        &[
                            shader
                                .on_stage(br::ShaderStage::Vertex, c"vertMain")
                                .with_specialization_info(&br::SpecializationInfo::new(
                                    &spec_const,
                                )),
                            shader
                                .on_stage(br::ShaderStage::Fragment, c"fragMain")
                                .with_specialization_info(&br::SpecializationInfo::new(
                                    &spec_const,
                                )),
                        ],
                        VI_STATE_EMPTY,
                        IA_STATE_TRILIST,
                        &br::PipelineViewportStateCreateInfo::new(&viewport, &scissor),
                        RASTER_STATE_DEFAULT_FILL_NOCULL,
                        BLEND_STATE_SINGLE_NONE,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)])
                    .expect("pipeline.create");

                (shader, pipeline)
            })
            .collect::<Vec<_>>();

        let mut cp = br::CommandPoolObject::new(
            vk_device,
            &br::CommandPoolCreateInfo::new(vk_device.present_queue_family_index()).transient(),
        )
        .expect("cp.create");
        let [mut cb] = br::CommandBufferObject::alloc_array(
            vk_device,
            &br::CommandBufferFixedCountAllocateInfo::new(&mut cp, br::CommandBufferLevel::Primary),
        )
        .expect("cb.alloc");
        unsafe {
            cb.begin(&br::CommandBufferBeginInfo::new().onetime_submit())
                .expect("cb.begin")
        }
        .inject(|r| {
            vk_device.cmd_begin_render_pass(
                r,
                &br::RenderPassBeginInfo::new(
                    &render_pass,
                    &fb,
                    self.atlas.size().into_rect(br::Offset2D::ZERO),
                    &[],
                ),
            )
        })
        .inject(|r| {
            render_resources
                .iter()
                .zip(shader_texture_rasterize_requests.iter())
                .fold(r, |r, (res, tex)| {
                    tracing::debug!(path = ?tex.1.shader_path, rect = ?tex.0, "raster shader tex");
                    r.bind_pipeline(br::PipelineBindPoint::Graphics, &res.1)
                        .draw(3, 1, 0, 0)
                })
        })
        .inject(|r| vk_device.cmd_end_render_pass(r))
        .end()
        .expect("cb.end");

        render_worker_queue
            .submit(
                &[br::SubmitInfo::new(
                    &[],
                    &[],
                    &[cb.as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("vector render submit");
        render_worker_queue.wait().expect("vector render wait");
    }

    pub const fn atlas(&self) -> &ColorTextureAtlas {
        &self.atlas
    }

    pub fn atlas_mut(&mut self) -> &mut ColorTextureAtlas {
        &mut self.atlas
    }

    pub fn clear(&mut self) {
        self.atlas.clear();
    }
}

pub struct PreviewRenderTargetBuffer {
    memory: br::vk::VkDeviceMemory,
    image: br::vk::VkImage,
    image_view: br::vk::VkImageView,
    depth_image: br::vk::VkImage,
    depth_view: br::vk::VkImageView,
    size: br::Extent2D,
}
impl PreviewRenderTargetBuffer {
    pub unsafe fn drop(self, device: &VulkanDevice) {
        drop(unsafe {
            br::ImageViewObject::manage(
                self.depth_view,
                br::ImageObject::manage(
                    self.depth_image,
                    device,
                    // dropでは使わない情報なので適当に埋める
                    br::vk::VK_IMAGE_TYPE_2D,
                    br::vk::VK_FORMAT_UNDEFINED,
                    br::Extent3D::spread1(1),
                ),
            )
        });
        drop(unsafe {
            br::ImageViewObject::manage(
                self.image_view,
                br::ImageObject::manage(
                    self.image,
                    device,
                    // dropでは使わない情報なので適当に埋める
                    br::vk::VK_IMAGE_TYPE_2D,
                    br::vk::VK_FORMAT_UNDEFINED,
                    br::Extent3D::spread1(1),
                ),
            )
        });
        drop(unsafe { br::DeviceMemoryObject::manage(self.memory, device) });
    }

    // TODO: おそらく本当はDeviceCaps見て選定したほうがいい
    pub const COLOR_FORMAT: br::Format = br::vk::VK_FORMAT_R8G8B8A8_UNORM;
    pub const DEPTH_FORMAT: br::Format = br::vk::VK_FORMAT_D24_UNORM_S8_UINT;

    pub fn new(device: &VulkanDevice, init_size: br::Extent2D) -> Self {
        let mut image = br::ImageObject::new(
            device,
            &br::ImageCreateInfo::new(init_size, Self::COLOR_FORMAT)
                .set_usage(br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT),
        )
        .expect("preview_rt.image.create");
        let mut depth_image = br::ImageObject::new(
            device,
            &br::ImageCreateInfo::new(init_size, Self::DEPTH_FORMAT)
                .set_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT),
        )
        .expect("preview_rt.depth_image.create");

        let memreq = image.requirements();
        let depth_memreq = depth_image.requirements();
        // できるだけAlignmentによるPaddingが少なくなるように配置する
        let (image_offset, depth_offset, memory_size);
        if memreq.alignment < depth_memreq.alignment {
            depth_offset = 0;
            image_offset = rup2_u64(depth_memreq.size, memreq.alignment);
            memory_size = image_offset + memreq.size;
        } else {
            image_offset = 0;
            depth_offset = rup2_u64(memreq.size, depth_memreq.alignment);
            memory_size = depth_offset + depth_memreq.size;
        }
        let memory = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(
                memory_size,
                device
                    .find_device_local_memory_index(
                        memreq.memoryTypeBits & depth_memreq.memoryTypeBits,
                    )
                    .expect("preview_rt.memory.index"),
            ),
        )
        .expect("preview_rt.memory.alloc");
        image
            .bind(&memory, image_offset)
            .expect("preview_rt.image.bind");
        depth_image
            .bind(&memory, depth_offset)
            .expect("preview_rt.depth_image.bind");

        let image_view = br::ImageViewBuilder::new(
            image,
            br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
        )
        .create()
        .expect("preview_rt.image_view.create");
        let depth_view = br::ImageViewBuilder::new(
            depth_image,
            br::ImageSubresourceRange::new(
                br::AspectMask::DEPTH | br::AspectMask::STENCIL,
                0..1,
                0..1,
            ),
        )
        .create()
        .expect("preview_rt.depth_view.create");

        let (image_view, image) = image_view.unmanage();
        let (image, _, _, _, _) = image.unmanage();
        let (depth_view, depth_image) = depth_view.unmanage();
        let (depth_image, _, _, _, _) = depth_image.unmanage();
        let (memory, _) = memory.unmanage();
        Self {
            memory,
            image,
            image_view,
            depth_image,
            depth_view,
            size: init_size,
        }
    }

    pub fn validate(&mut self, device: &VulkanDevice, active_size: br::Extent2D) -> bool {
        let mut resource_recreated = false;
        if self.size != active_size {
            drop(unsafe {
                br::ImageViewObject::manage(
                    self.depth_view,
                    br::ImageObject::manage(
                        self.depth_image,
                        device,
                        // dropでは使わない情報なので適当に埋める
                        br::vk::VK_IMAGE_TYPE_2D,
                        br::vk::VK_FORMAT_UNDEFINED,
                        br::Extent3D::spread1(1),
                    ),
                )
            });
            drop(unsafe {
                br::ImageViewObject::manage(
                    self.image_view,
                    br::ImageObject::manage(
                        self.image,
                        device,
                        // dropでは使わない情報なので適当に埋める
                        br::vk::VK_IMAGE_TYPE_2D,
                        br::vk::VK_FORMAT_UNDEFINED,
                        br::Extent3D::spread1(1),
                    ),
                )
            });
            drop(unsafe { br::DeviceMemoryObject::manage(self.memory, device) });

            let mut image = br::ImageObject::new(
                device,
                &br::ImageCreateInfo::new(active_size, Self::COLOR_FORMAT).set_usage(
                    br::ImageUsageFlags::SAMPLED | br::ImageUsageFlags::COLOR_ATTACHMENT,
                ),
            )
            .expect("preview_rt.validate.image.create");
            let mut depth_image = br::ImageObject::new(
                device,
                &br::ImageCreateInfo::new(active_size, Self::DEPTH_FORMAT)
                    .set_usage(br::ImageUsageFlags::DEPTH_STENCIL_ATTACHMENT),
            )
            .expect("preview_rt.validate.depth_image.create");

            let memreq = image.requirements();
            let depth_memreq = depth_image.requirements();
            // できるだけAlignmentによるPaddingが少なくなるように配置する
            let (image_offset, depth_offset, memory_size);
            if memreq.alignment < depth_memreq.alignment {
                depth_offset = 0;
                image_offset = rup2_u64(depth_memreq.size, memreq.alignment);
                memory_size = image_offset + memreq.size;
            } else {
                image_offset = 0;
                depth_offset = rup2_u64(memreq.size, depth_memreq.alignment);
                memory_size = depth_offset + depth_memreq.size;
            }
            let memory = br::DeviceMemoryObject::new(
                device,
                &br::MemoryAllocateInfo::new(
                    memory_size,
                    device
                        .find_device_local_memory_index(
                            memreq.memoryTypeBits & depth_memreq.memoryTypeBits,
                        )
                        .expect("preview_rt.memory.index"),
                ),
            )
            .expect("preview_rt.validate.memory.alloc");
            image
                .bind(&memory, image_offset)
                .expect("preview_rt.validate.image.bind");
            depth_image
                .bind(&memory, depth_offset)
                .expect("preview_rt.validate.depth_image.bind");

            let image_view = br::ImageViewBuilder::new(
                image,
                br::ImageSubresourceRange::new(br::AspectMask::COLOR, 0..1, 0..1),
            )
            .create()
            .expect("preview_rt.validate.image_view.create");
            let depth_view = br::ImageViewBuilder::new(
                depth_image,
                br::ImageSubresourceRange::new(
                    br::AspectMask::DEPTH | br::AspectMask::STENCIL,
                    0..1,
                    0..1,
                ),
            )
            .create()
            .expect("preview_rt.validate.depth_view.create");

            let (image_view, image) = image_view.unmanage();
            let (image, _, _, _, _) = image.unmanage();
            let (depth_view, depth_image) = depth_view.unmanage();
            let (depth_image, _, _, _, _) = depth_image.unmanage();
            let (memory, _) = memory.unmanage();
            self.image_view = image_view;
            self.image = image;
            self.depth_view = depth_view;
            self.depth_image = depth_image;
            self.memory = memory;
            resource_recreated = true;
        }

        self.size = active_size;
        resource_recreated
    }

    pub const fn image_view_tref<'a>(&'a self) -> br::VkHandleRef<'a, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.image_view) }
    }

    pub const fn depth_view_tref<'a>(&'a self) -> br::VkHandleRef<'a, br::vk::VkImageView> {
        unsafe { br::VkHandleRef::dangling(self.depth_view) }
    }

    pub const fn aspect_wh(&self) -> f32 {
        self.size.width as f32 / self.size.height as f32
    }
}

// std140 layout
#[repr(C)]
pub struct PreviewStreamingBufferContent {
    pub current_sec: f32,
}

pub struct PreviewOriginAxesVertex {
    dir: [f32; 4],
    offset: [f32; 4],
}

// std140 layout
#[repr(C)]
pub struct PreviewCameraData {
    world_to_clip_space: Matrix4F32,
    world_to_camera_space: Matrix4F32,
    camera_to_clip_space: Matrix4F32,
    camera_pos: [f32; 4],
}
impl PreviewCameraData {
    fn new(camera: &peridot_math::Camera, aspect_wh: f32) -> Self {
        Self {
            world_to_clip_space: camera.view_projection_matrix(aspect_wh).transpose(),
            world_to_camera_space: camera.view_matrix().transpose(),
            camera_to_clip_space: camera.projection_matrix(aspect_wh).transpose(),
            camera_pos: [camera.position.0, camera.position.1, camera.position.2, 1.0],
        }
    }
}

// std430 layout
#[repr(C)]
pub struct PreviewGridPushConstantData {
    dir: [f32; 4],
    start: [f32; 4],
    altdir: [f32; 4],
    scale: f32,
}

const PREVIEW_VS_ORIGIN_AXES: &[PreviewOriginAxesVertex] = &[
    PreviewOriginAxesVertex {
        dir: [1.0, 0.0, 0.0, 1.0],
        offset: [1000.0, 0.0, 0.0, 0.0],
    },
    PreviewOriginAxesVertex {
        dir: [1.0, 0.0, 0.0, 1.0],
        offset: [-1000.0, 0.0, 0.0, 0.0],
    },
    PreviewOriginAxesVertex {
        dir: [0.0, 1.0, 0.0, 1.0],
        offset: [0.0, 1000.0, 0.0, 0.0],
    },
    PreviewOriginAxesVertex {
        dir: [0.0, 1.0, 0.0, 1.0],
        offset: [0.0, -1000.0, 0.0, 0.0],
    },
    PreviewOriginAxesVertex {
        dir: [0.0, 0.0, 1.0, 1.0],
        offset: [0.0, 0.0, 1000.0, 0.0],
    },
    PreviewOriginAxesVertex {
        dir: [0.0, 0.0, 1.0, 1.0],
        offset: [0.0, 0.0, -1000.0, 0.0],
    },
];

pub struct PreviewHandleVertex {
    pos: [f32; 4],
    col: [f32; 4],
}

struct PreviewScratchStagingBuffer {
    buffer: br::vk::VkBuffer,
    memory: br::vk::VkDeviceMemory,
    should_flush: bool,
    mapped_ptr: *mut core::ffi::c_void,
    unused_top: usize,
}
impl PreviewScratchStagingBuffer {
    unsafe fn drop(self, device: &VulkanDevice) {
        unsafe {
            br::vkfn_wrapper::unmap_memory(
                device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.memory),
            );
        }

        drop(unsafe { br::BufferObject::manage(self.buffer, device) });
        drop(unsafe { br::DeviceMemoryObject::manage(self.memory, device) });
    }

    const INIT_SIZE: br::DeviceSize = 1024 * 1024;

    fn new(device: &VulkanDevice) -> Self {
        let mut buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new(Self::INIT_SIZE, br::BufferUsage::TRANSFER_SRC),
        )
        .expect("preview_scratch_staging.buffer.create");
        device.dbg_set_name(&buffer, c"Preview.ScratchStaging.Buffer");
        let memreq = buffer.requirements();
        let memindex = device
            .find_host_visible_memory_index(memreq.memoryTypeBits)
            .expect("preview_scratch_staging.memory.index");
        let should_flush = !device.is_coherent_memory(memindex);
        let memory = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(memreq.size, memindex),
        )
        .expect("preview_scratch_staging.memory.alloc");
        device.dbg_set_name(&memory, c"Preview.ScratchStaging.Memory");
        buffer
            .bind(&memory, 0)
            .expect("preview_scratch_staging.buffer.bind");

        let (buffer, _) = buffer.unmanage();
        let (memory, _) = memory.unmanage();
        let mapped_ptr = unsafe {
            br::vkfn_wrapper::map_memory(
                device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(memory),
                0..Self::INIT_SIZE,
                0,
            )
            .expect("preview_scratch_staging.map")
        };
        Self {
            buffer,
            memory,
            should_flush,
            mapped_ptr,
            unused_top: 0,
        }
    }

    fn reset(&mut self) {
        self.unused_top = 0;
    }

    fn reserve(&mut self, size: usize) -> usize {
        let r = self.unused_top;
        self.unused_top += size;
        if self.unused_top >= Self::INIT_SIZE as usize {
            todo!("resizing scratch staging buffer");
        }

        r
    }

    fn ops_before_copy(&self, device: &VulkanDevice) {
        if self.should_flush {
            unsafe {
                device
                    .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new_raw(
                        self.memory,
                        0,
                        Self::INIT_SIZE,
                    )])
                    .expect("preview.scratch_staging.flush");
            }
        }
    }
}

/// # Examples
///
/// ```
/// assert_eq!(most_top_bit_pos_u64(0), 0);
/// assert_eq!(most_top_bit_pos_u64(64), 6);
/// assert_eq!(most_top_bit_pos_u64(128), 7);
/// ```
const fn most_top_bit_pos_u64(v: u64) -> u32 {
    64 - v.leading_zeros() - 1
}

/// # Examples
///
/// ```
/// assert_eq!(lowest_bit_pos_u64(1), 0);
/// assert_eq!(lowest_bit_pos_u64(5), 0);
/// assert_eq!(lowest_bit_pos_u64(10), 1);
/// assert_eq!(lowest_bit_pos_u64(0x24), 2);
/// ```
const fn lowest_bit_pos_u16(v: u16) -> u8 {
    v.trailing_zeros() as _
}

const fn find_lowest_bit_pos_from_u16(v: u16, bitpos: u16) -> Option<u8> {
    match v & (!0 << bitpos) {
        0 => None,
        x => Some(lowest_bit_pos_u16(x)),
    }
}

enum DynamicBufferBlockState {
    Free {
        size: NonZero<br::DeviceSize>,
        prev_block: br::DeviceSize,
        prev_free_block: Option<br::DeviceSize>,
        next_free_block: Option<br::DeviceSize>,
    },
    Used {
        size: NonZero<br::DeviceSize>,
        prev_block: br::DeviceSize,
    },
}

/// 最小アロケーション単位
const DB_TLSF_ALLOC_GRANULARITY: br::DeviceSize = 64; // float4x4
/// ページサイズ（一括でDeviceMemory/Bufferとして確保するサイズ）
const DB_TLSF_PAGE_SIZE: br::DeviceSize = 64 * 1024; // 64kb
/// 最小アロケーション単位のビット位置
const DB_TLSF_ALLOC_GRANULARITY_BITS: u32 = 6;
/// ページサイズのビット位置
const DB_TLSF_PAGE_SIZE_BIT: u32 = 6 + 10;
/// Second Levelの分割数のビット位置
const DB_TLSF_LV2_SUBDIV_BITS: u32 = 4;
/// Second Levelを抽出するためのビットマスク（大きさにからSecond Levelを抽出するのに使う）
const DB_TLSF_LV2_MASK: br::DeviceSize = (1 << DB_TLSF_LV2_SUBDIV_BITS) - 1;
/// First Levelの数
const DB_TLSF_FL_COUNT: usize = (DB_TLSF_PAGE_SIZE_BIT
    - DB_TLSF_ALLOC_GRANULARITY_BITS
    - DB_TLSF_LV2_SUBDIV_BITS
    + 2/* idx0(2^0 based) + idxlast(entire size of page) */)
    as usize;
/// First Level 1段階におけるSecond Levelの数
const DB_TLSF_SL_PER_FL: usize = 1 << DB_TLSF_LV2_SUBDIV_BITS;
/// First Level 0におくサイズの最大値
const DB_TLSF_FL0_MAX_SIZE: br::DeviceSize =
    1 << (DB_TLSF_ALLOC_GRANULARITY_BITS + DB_TLSF_LV2_SUBDIV_BITS);
struct DynamicBufferPage {
    /// 1で空きあり
    first_level_freemap: u16,
    /// 1で空きあり
    second_level_freemap: [u16; DB_TLSF_FL_COUNT],
    block_list_headings: [br::DeviceSize; DB_TLSF_FL_COUNT * DB_TLSF_SL_PER_FL],
    block_states: HashMap<br::DeviceSize, DynamicBufferBlockState>,
}
impl DynamicBufferPage {
    fn new() -> Self {
        let mut block_states = HashMap::new();
        block_states.insert(
            0,
            DynamicBufferBlockState::Free {
                size: unsafe { NonZero::new_unchecked(DB_TLSF_PAGE_SIZE) },
                prev_block: 0, // self
                next_free_block: None,
                prev_free_block: None,
            },
        );
        let (first_f, first_s) = Self::mapping(DB_TLSF_PAGE_SIZE);
        let mut second_level_freemap = [0; DB_TLSF_FL_COUNT];
        second_level_freemap[first_f as usize] |= 1 << first_s;

        Self {
            first_level_freemap: 1 << first_f,
            second_level_freemap,
            block_list_headings: [0; _],
            block_states,
        }
    }

    /// maps size to (first level index, second level index)
    const fn mapping(size: br::DeviceSize) -> (u32, u32) {
        if size < DB_TLSF_FL0_MAX_SIZE {
            // force level0(2^0..2^(DB_TLSF_ALLOC_GRANULARITY_BIT + DB_TLSF_LV2_SUBDIV_BITS9))
            return (
                0,
                ((size >> DB_TLSF_ALLOC_GRANULARITY_BITS) & DB_TLSF_LV2_MASK) as _,
            );
        }

        let f =
            most_top_bit_pos_u64(size) - DB_TLSF_ALLOC_GRANULARITY_BITS - DB_TLSF_LV2_SUBDIV_BITS
                + 1;
        assert!(f >= 1);
        (
            f,
            ((size >> (f - 1 + DB_TLSF_ALLOC_GRANULARITY_BITS)) & DB_TLSF_LV2_MASK) as _,
        )
    }

    const fn block_list_index(f: u32, s: u32) -> usize {
        f as usize * DB_TLSF_SL_PER_FL + s as usize
    }

    const fn sl_is_fully_occupied(&self, fl: u32) -> bool {
        self.second_level_freemap[fl as usize] == 0
    }
    const fn has_free_block(&self, fl: u32, sl: u32) -> bool {
        (self.second_level_freemap[fl as usize] & (1 << sl)) != 0
    }
    fn mark_free(&mut self, fl: u32, sl: u32) {
        self.second_level_freemap[fl as usize] |= 1 << sl;
        self.first_level_freemap |= 1 << fl;
    }
    fn mark_no_free(&mut self, fl: u32, sl: u32) {
        self.second_level_freemap[fl as usize] &= !(1 << sl);
        if self.sl_is_fully_occupied(fl) {
            // also first level has no free
            self.first_level_freemap &= !(1 << fl);
        }
    }

    fn find_free_at_least(&self, least_f: u32, least_s: u32) -> Option<(u32, u32)> {
        if let Some(usable_bit) =
            find_lowest_bit_pos_from_u16(self.second_level_freemap[least_f as usize], least_s as _)
        {
            // available in this first level
            return Some((least_f, usable_bit as _));
        }

        // use more upper level
        let Some(usable_bit) =
            find_lowest_bit_pos_from_u16(self.first_level_freemap, least_f as u16 + 1)
        else {
            tracing::warn!("no usable block");
            return None;
        };

        let actual_f = usable_bit as _;
        assert!(
            !self.sl_is_fully_occupied(actual_f),
            "selected first-level could not be used?"
        );
        Some((
            actual_f,
            lowest_bit_pos_u16(self.second_level_freemap[actual_f as usize]) as _,
        ))
    }

    #[tracing::instrument(name = "DynamicBufferPage::try_alloc", skip(self), ret(level = tracing::Level::TRACE))]
    fn try_alloc(&mut self, size: br::DeviceSize) -> Option<br::DeviceSize> {
        let size = rup2_u64(size, DB_TLSF_ALLOC_GRANULARITY);
        assert!(0 < size && size <= DB_TLSF_PAGE_SIZE);

        let (f, s) = Self::mapping(size);
        tracing::debug!(f, s, "tlsf level");
        let (f, s) = self.find_free_at_least(f, s)?;
        tracing::debug!(f, s, "free found");

        let head = self.block_list_headings[Self::block_list_index(f, s)];
        let Some(DynamicBufferBlockState::Free {
            size: block_size,
            prev_block,
            prev_free_block,
            next_free_block,
        }) = self.block_states.remove(&head)
        else {
            unreachable!();
        };
        // this should be the first
        assert!(prev_free_block.is_none());

        self.block_states.insert(
            head,
            DynamicBufferBlockState::Used {
                size: unsafe { NonZero::new_unchecked(size) },
                prev_block,
            },
        );
        if let Some(next) = next_free_block {
            // move head ptr to next
            self.block_list_headings[Self::block_list_index(f, s)] = next;
            let Some(&mut DynamicBufferBlockState::Free {
                prev_free_block: ref mut next_prev_free_block,
                ..
            }) = self.block_states.get_mut(&next)
            else {
                unreachable!();
            };
            *next_prev_free_block = None;
        } else {
            // no free block for this size class
            self.mark_no_free(f, s);
        }

        let left_block_size = block_size.get() - size;
        if left_block_size > 0 {
            // subdiv needed
            let (left_f, left_s) = Self::mapping(left_block_size);
            if !self.has_free_block(left_f, left_s) {
                // this is first free block
                self.block_list_headings[Self::block_list_index(left_f, left_s)] = head + size;
                self.block_states.insert(
                    head + size,
                    DynamicBufferBlockState::Free {
                        size: unsafe { NonZero::new_unchecked(left_block_size) },
                        prev_block: head,
                        prev_free_block: None,
                        next_free_block: None,
                    },
                );
            } else {
                // connect to head of free list
                let old_free_head = core::mem::replace(
                    &mut self.block_list_headings[Self::block_list_index(left_f, left_s)],
                    head + size,
                );
                if old_free_head == head {
                    // sipmle replacement
                    self.block_states.insert(
                        head + size,
                        DynamicBufferBlockState::Free {
                            size: unsafe { NonZero::new_unchecked(left_block_size) },
                            prev_block: head,
                            prev_free_block: None,
                            next_free_block,
                        },
                    );
                } else {
                    // chaining needed
                    self.block_states.insert(
                        head + size,
                        DynamicBufferBlockState::Free {
                            size: unsafe { NonZero::new_unchecked(left_block_size) },
                            prev_block: head,
                            prev_free_block: None,
                            next_free_block: Some(old_free_head),
                        },
                    );
                    let Some(&mut DynamicBufferBlockState::Free {
                        prev_free_block: ref mut old_free_prev_free_block,
                        ..
                    }) = self.block_states.get_mut(&old_free_head)
                    else {
                        unreachable!();
                    };
                    assert!(old_free_prev_free_block.is_none());
                    *old_free_prev_free_block = Some(head + size);
                }
            }

            self.mark_free(left_f, left_s);
        }

        Some(head)
    }
}

/// TLSF based dynamic allocatable gpu buffer
struct DynamicBuffer {
    page_pools: Vec<DynamicBufferPage>,
}
impl DynamicBuffer {
    fn new() -> Self {
        Self {
            page_pools: Vec::new(),
        }
    }

    fn alloc(&mut self, size: br::DeviceSize) -> br::DeviceSize {
        if let Some(found_offs) = self.page_pools.iter_mut().find_map(|x| x.try_alloc(size)) {
            return found_offs;
        }

        // allocate new one
        let mut new_page = DynamicBufferPage::new();
        let found_offs = unsafe { new_page.try_alloc(size).unwrap_unchecked() };
        self.page_pools.push(new_page);
        found_offs
    }
}

#[derive(Debug, Clone, Copy)]
pub enum IndexType {
    U16,
    U32,
}

pub struct CommittedPreviewMeshData {
    pub vertices: std::sync::Arc<[u8]>,
    pub vertex_stride: usize,
    pub indices: std::sync::Arc<[u8]>,
    pub index_type: IndexType,
    pub sub_mesh_ranges: std::sync::Arc<[core::range::Range<usize>]>,
}

pub struct CommittedPreviewRenderData {
    pub object_to_world: Matrix4F32,
    pub mesh_id: usize,
}

pub struct CommittedPreviewState {
    pub viewport_size: Size<LogicalUnit>,
    pub main_camera: peridot_math::Camera,
    pub main_camera_dirtified: bool,
    pub pushed_meshes: Vec<CommittedPreviewMeshData>,
    pub dirty_meshes: HashMap<usize, CommittedPreviewMeshData>,
    pub removed_meshes: HashSet<usize>,
    pub pushed_render_data: Vec<CommittedPreviewRenderData>,
    pub dirty_render_data: HashMap<usize, CommittedPreviewRenderData>,
    pub removed_render_data: HashSet<usize>,
}

struct PreviewSubMeshData {
    vertex_range: core::range::Range<br::DeviceSize>,
    index_range: core::range::Range<br::DeviceSize>,
}

struct PreviewRenderData {
    object_uniform_range: core::range::Range<br::DeviceSize>,
    mesh_id: usize,
}

pub struct PreviewRenderer {
    common_descriptor_set_layout: br::vk::VkDescriptorSetLayout,
    descriptor_pool: br::vk::VkDescriptorPool,
    common_descriptor_set: br::DescriptorSet,
    streaming_buffer: br::vk::VkBuffer,
    streaming_memory: br::vk::VkDeviceMemory,
    streaming_memory_should_flush: bool,
    active_rt_size: br::Extent2D,
    active_framebuffer_resource_handle: br::vk::VkImageView,
    render_pass: br::vk::VkRenderPass,
    framebuffer: core::mem::MaybeUninit<br::vk::VkFramebuffer>,
    origin_axes_pipeline_layout: br::vk::VkPipelineLayout,
    origin_axes_shader: br::vk::VkShaderModule,
    origin_axes_pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    grid_pipeline_layout: br::vk::VkPipelineLayout,
    grid_shader: br::vk::VkShaderModule,
    grid_pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    unlit_colored_shader: br::vk::VkShaderModule,
    unlit_colored_object_pipeline_layout: br::vk::VkPipelineLayout,
    gizmos_pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    command_pool: br::vk::VkCommandPool,
    command_buffer: br::vk::VkCommandBuffer,
    update_command_pool: br::vk::VkCommandPool,
    update_command_buffer: br::vk::VkCommandBuffer,
    update_command_pending: bool,
    scratch_staging: PreviewScratchStagingBuffer,
    pending_camera_data_updates: Option<usize>,
    internal_mesh_buffer: br::vk::VkBuffer,
    origin_axes_vbuf_range: core::ops::Range<br::DeviceSize>,
    translate_handle_vbuf_range: core::ops::Range<br::DeviceSize>,
    translate_handle_ibuf_range: core::ops::Range<br::DeviceSize>,
    internal_uniform_buffer: br::vk::VkBuffer,
    camera_data_ubuf_range: core::ops::Range<br::DeviceSize>,
    internal_data_memory: br::vk::VkDeviceMemory,
    dynamic_buffer: DynamicBuffer,
    dynamic_ubuf: DynamicBuffer,
    user_meshes: Vec<Vec<PreviewSubMeshData>>,
    user_renders: Vec<PreviewRenderData>,
    valid: bool,
}
impl PreviewRenderer {
    pub unsafe fn drop(self, device: &VulkanDevice) {
        drop(unsafe { br::CommandPoolObject::manage(self.update_command_pool, device) });
        drop(unsafe { br::CommandPoolObject::manage(self.command_pool, device) });

        unsafe {
            self.scratch_staging.drop(device);
        }

        if self.valid {
            drop(unsafe { br::PipelineObject::manage(self.gizmos_pipeline.assume_init(), device) });
            drop(unsafe { br::PipelineObject::manage(self.grid_pipeline.assume_init(), device) });
            drop(unsafe {
                br::PipelineObject::manage(self.origin_axes_pipeline.assume_init(), device)
            });
            drop(unsafe { br::FramebufferObject::manage(self.framebuffer.assume_init(), device) });
        }

        drop(unsafe { br::ShaderModuleObject::manage(self.unlit_colored_shader, device) });
        drop(unsafe {
            br::PipelineLayoutObject::manage(self.unlit_colored_object_pipeline_layout, device)
        });
        drop(unsafe { br::ShaderModuleObject::manage(self.grid_shader, device) });
        drop(unsafe { br::PipelineLayoutObject::manage(self.grid_pipeline_layout, device) });
        drop(unsafe { br::ShaderModuleObject::manage(self.origin_axes_shader, device) });
        drop(unsafe { br::PipelineLayoutObject::manage(self.origin_axes_pipeline_layout, device) });
        drop(unsafe { br::RenderPassObject::manage(self.render_pass, device) });
        drop(unsafe { br::DeviceMemoryObject::manage(self.streaming_memory, device) });
        drop(unsafe { br::BufferObject::manage(self.streaming_buffer, device) });
        drop(unsafe { br::DescriptorPoolObject::manage(self.descriptor_pool, device) });
        drop(unsafe {
            br::DescriptorSetLayoutObject::manage(self.common_descriptor_set_layout, device)
        });
        drop(unsafe { br::BufferObject::manage(self.internal_mesh_buffer, device) });
        drop(unsafe { br::BufferObject::manage(self.internal_uniform_buffer, device) });
        drop(unsafe { br::DeviceMemoryObject::manage(self.internal_data_memory, device) });
    }

    pub fn new(
        device: &VulkanDevice,
        init_rt: &PreviewRenderTargetBuffer,
        init_state: &CommittedPreviewState,
        work_queue_family_index: u32,
        work_queue: &mut (impl br::QueueMut + ?Sized),
    ) -> Self {
        let mut streaming_buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new_for_type::<PreviewStreamingBufferContent>(
                br::BufferUsage::UNIFORM_BUFFER,
            ),
        )
        .expect("preview.streaming_buffer.create");
        let memreq = streaming_buffer.requirements();
        let memindex = device
            .find_direct_memory_index(memreq.memoryTypeBits)
            .expect("preview.streaming_memory.index");
        let streaming_memory_should_flush = !device.is_coherent_memory(memindex);
        let streaming_memory = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(memreq.size, memindex),
        )
        .expect("preview.streaming_memory.alloc");
        streaming_buffer
            .bind(&streaming_memory, 0)
            .expect("preview_streaming_buffer.bind");

        let render_pass = br::RenderPassObject::new(
            device,
            &br::RenderPassCreateInfo2::new(
                &[
                    br::AttachmentDescription2::new(PreviewRenderTargetBuffer::COLOR_FORMAT)
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::Store)
                        .with_layout_to(br::ImageLayout::ShaderReadOnlyOpt.from_undefined()),
                    br::AttachmentDescription2::new(PreviewRenderTargetBuffer::DEPTH_FORMAT)
                        .color_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
                        .stencil_memory_op(br::LoadOp::Clear, br::StoreOp::DontCare)
                        .with_layout_to(
                            br::ImageLayout::DepthStencilAttachmentOpt.from_undefined(),
                        ),
                ],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])
                    .depth_stencil(&br::AttachmentReference2::depth_stencil_attachment_opt(1))],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .by_region()
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::SHADER.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags::FRAGMENT_SHADER,
                )],
            ),
        )
        .expect("preview.render_pass.create");

        let common_descriptor_set_layout = br::DescriptorSetLayoutObject::new(
            device,
            &br::DescriptorSetLayoutCreateInfo::new(&[
                br::DescriptorType::UniformBuffer.make_binding(0, 1)
            ]),
        )
        .expect("preview.common_descriptor_set_layout.create");

        let origin_axes_pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[common_descriptor_set_layout.as_transparent_ref()],
                &[],
            ),
        )
        .expect("preview.origin_axes.pipeline_layout.create");
        let grid_pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[common_descriptor_set_layout.as_transparent_ref()],
                &[
                    br::PushConstantRange::for_type::<PreviewGridPushConstantData>(
                        br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                        0,
                    ),
                ],
            ),
        )
        .expect("preview.grid.pipeline_layout.create");
        let unlit_colored_object_pipeline_layout = br::PipelineLayoutObject::new(
            device,
            &br::PipelineLayoutCreateInfo::new(
                &[common_descriptor_set_layout.as_transparent_ref()],
                &[],
            ),
        )
        .expect("preview.unlit_colored_object.pipeline_layout.create");
        let origin_axes_shader = device.require_shader("preview/origin_axes.spv");
        let grid_shader = device.require_shader("preview/grid.spv");
        let unlit_colored_shader = device.require_shader("preview/unlit_colored.spv");

        const TRANSLATE_HANDLE_BAR_LENGTH: f32 = 0.2;
        const TRANSLATE_HANDLE_ARROW_SIZE: f32 = 0.05;
        const TRANSLATE_HANDLE_BAR_RADIUS: f32 = 0.005;
        const TRANSLATE_HANDLE_ARROW_RADIUS: f32 = 0.02;
        const TRANSLATE_HANDLE_BAR_DIVISION: u32 = 6;
        const TRANSLATE_HANDLE_ARROW_DIVISION: u32 = 12;
        let translate_handle_vcount = (TRANSLATE_HANDLE_BAR_DIVISION as usize * 2
            + TRANSLATE_HANDLE_ARROW_DIVISION as usize
            + 1)
            * 3;
        let translate_handle_icount = (TRANSLATE_HANDLE_BAR_DIVISION as usize * 6
            + TRANSLATE_HANDLE_ARROW_DIVISION as usize * 3
            + (TRANSLATE_HANDLE_ARROW_DIVISION as usize - 2) * 3)
            * 3;

        let origin_axes_vbuf_range = 0..size_of_val(PREVIEW_VS_ORIGIN_AXES) as br::DeviceSize;
        let translate_handle_vbuf_range = range_from_len_u64(
            rup2_u64(
                origin_axes_vbuf_range.end,
                align_of::<PreviewHandleVertex>() as _,
            ),
            (size_of::<PreviewHandleVertex>() * translate_handle_vcount) as _,
        );
        let translate_handle_ibuf_range = range_from_len_u64(
            rup2_u64(translate_handle_vbuf_range.end, align_of::<u16>() as _),
            (size_of::<u16>() * translate_handle_icount) as _,
        );
        let mut internal_mesh_buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new(
                translate_handle_ibuf_range.end,
                br::BufferUsage::VERTEX_BUFFER
                    | br::BufferUsage::INDEX_BUFFER
                    | br::BufferUsage::TRANSFER_DEST,
            ),
        )
        .expect("preview.internal_mesh_buffer.create");
        let camera_data_ubuf_range = 0..size_of::<PreviewCameraData>() as br::DeviceSize;
        let gizmos_camera_data_ubuf_range = range_from_len_u64(
            camera_data_ubuf_range.end,
            size_of::<PreviewCameraData>() as _,
        );
        let mut internal_uniform_buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new(
                gizmos_camera_data_ubuf_range.end,
                br::BufferUsage::UNIFORM_BUFFER | br::BufferUsage::TRANSFER_DEST,
            ),
        )
        .expect("preview.internal_uniform_buffer.create");
        let internal_mesh_buffer_memreq = internal_mesh_buffer.requirements();
        let internal_uniform_buffer_memreq = internal_uniform_buffer.requirements();
        let internal_mesh_buffer_offset = rup2_u64(
            internal_uniform_buffer_memreq.size,
            internal_mesh_buffer_memreq.alignment,
        );
        let internal_data_memory = device.alloc_device_local_memory(
            internal_mesh_buffer_offset + internal_mesh_buffer_memreq.size,
            internal_mesh_buffer_memreq.memoryTypeBits
                & internal_uniform_buffer_memreq.memoryTypeBits,
        );
        internal_mesh_buffer
            .bind(&internal_data_memory, internal_mesh_buffer_offset)
            .expect("preview.internal_mesh_buffer.bind");
        internal_uniform_buffer
            .bind(&internal_data_memory, 0)
            .expect("preview.internal_uniform_buffer.bind");

        struct UploadBufferData {
            origin_axes_vbuf: [PreviewOriginAxesVertex; PREVIEW_VS_ORIGIN_AXES.len()],
            camera_data_ubuf: PreviewCameraData,
        }
        let translate_handle_vbuf_upload_offset = rup2(
            size_of::<UploadBufferData>(),
            align_of::<PreviewHandleVertex>(),
        );
        let translate_handle_ibuf_upload_offset = rup2(
            translate_handle_vbuf_upload_offset
                + size_of::<UploadBufferData>() * translate_handle_vcount,
            align_of::<u16>(),
        );
        let upload_size =
            translate_handle_ibuf_upload_offset + size_of::<u16>() * translate_handle_icount;
        let mut upload_buffer = br::BufferObject::new(
            device,
            &br::BufferCreateInfo::new(upload_size as _, br::BufferUsage::TRANSFER_SRC),
        )
        .expect("preview.upload_buffer.create");
        let memreq = upload_buffer.requirements();
        let memindex = device
            .find_host_visible_memory_index(memreq.memoryTypeBits)
            .expect("preview.upload_memory.index");
        let should_flush = !device.is_coherent_memory(memindex);
        let mut mem = br::DeviceMemoryObject::new(
            device,
            &br::MemoryAllocateInfo::new(memreq.size, memindex),
        )
        .expect("preview.upload_memory.alloc");
        upload_buffer
            .bind(&mem, 0)
            .expect("preview.upload_buffer.bind");
        let memhandle = mem.native_ptr();
        let ptr = mem
            .map(0..upload_size as _)
            .expect("preview.upload_memory.map");
        unsafe {
            let p = ptr.ptr().cast::<UploadBufferData>();
            core::ptr::copy_nonoverlapping(
                PREVIEW_VS_ORIGIN_AXES.as_ptr(),
                (*p).origin_axes_vbuf.as_mut_ptr(),
                PREVIEW_VS_ORIGIN_AXES.len(),
            );
            core::ptr::write(
                &raw mut (*p).camera_data_ubuf,
                PreviewCameraData::new(&init_state.main_camera, init_rt.aspect_wh()),
            );

            let vs = ptr
                .ptr()
                .byte_add(translate_handle_vbuf_upload_offset)
                .cast::<PreviewHandleVertex>();
            let is = ptr
                .ptr()
                .byte_add(translate_handle_ibuf_upload_offset)
                .cast::<u16>();
            let base_vindex_x = 0;
            let base_vindex_y = base_vindex_x
                + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2
                + 1
                + TRANSLATE_HANDLE_ARROW_DIVISION as usize;
            let base_vindex_z = base_vindex_y
                + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2
                + 1
                + TRANSLATE_HANDLE_ARROW_DIVISION as usize;
            let mut iindex_x = 0;
            let mut iindex_y = iindex_x
                + TRANSLATE_HANDLE_BAR_DIVISION as usize * 6
                + TRANSLATE_HANDLE_ARROW_DIVISION as usize * 3
                + (TRANSLATE_HANDLE_ARROW_DIVISION as usize - 2) * 3;
            let mut iindex_z = iindex_y
                + TRANSLATE_HANDLE_BAR_DIVISION as usize * 6
                + TRANSLATE_HANDLE_ARROW_DIVISION as usize * 3
                + (TRANSLATE_HANDLE_ARROW_DIVISION as usize - 2) * 3;
            for r in 0..TRANSLATE_HANDLE_BAR_DIVISION {
                let (s, c) = (core::f32::consts::TAU * r as f32
                    / TRANSLATE_HANDLE_BAR_DIVISION as f32)
                    .sin_cos();

                vs.add(base_vindex_x + r as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            0.0,
                            TRANSLATE_HANDLE_BAR_RADIUS * s,
                            TRANSLATE_HANDLE_BAR_RADIUS * c,
                            1.0,
                        ],
                        col: [1.0, 0.0, 0.0, 1.0],
                    });
                vs.add(base_vindex_x + r as usize + TRANSLATE_HANDLE_BAR_DIVISION as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            TRANSLATE_HANDLE_BAR_LENGTH,
                            TRANSLATE_HANDLE_BAR_RADIUS * s,
                            TRANSLATE_HANDLE_BAR_RADIUS * c,
                            1.0,
                        ],
                        col: [1.0, 0.0, 0.0, 1.0],
                    });
                vs.add(base_vindex_y + r as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            TRANSLATE_HANDLE_BAR_RADIUS * s,
                            0.0,
                            TRANSLATE_HANDLE_BAR_RADIUS * c,
                            1.0,
                        ],
                        col: [0.0, 1.0, 0.0, 1.0],
                    });
                vs.add(base_vindex_y + r as usize + TRANSLATE_HANDLE_BAR_DIVISION as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            TRANSLATE_HANDLE_BAR_RADIUS * s,
                            TRANSLATE_HANDLE_BAR_LENGTH,
                            TRANSLATE_HANDLE_BAR_RADIUS * c,
                            1.0,
                        ],
                        col: [0.0, 1.0, 0.0, 1.0],
                    });
                vs.add(base_vindex_z + r as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            TRANSLATE_HANDLE_BAR_RADIUS * s,
                            TRANSLATE_HANDLE_BAR_RADIUS * c,
                            0.0,
                            1.0,
                        ],
                        col: [0.0, 0.0, 1.0, 1.0],
                    });
                vs.add(base_vindex_z + r as usize + TRANSLATE_HANDLE_BAR_DIVISION as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            TRANSLATE_HANDLE_BAR_RADIUS * s,
                            TRANSLATE_HANDLE_BAR_RADIUS * c,
                            TRANSLATE_HANDLE_BAR_LENGTH,
                            1.0,
                        ],
                        col: [0.0, 0.0, 1.0, 1.0],
                    });

                let prev_r = if r > 0 {
                    r as u16
                } else {
                    TRANSLATE_HANDLE_BAR_DIVISION as u16
                } - 1;

                let a0 = base_vindex_x as u16 + prev_r;
                let b0 = base_vindex_x as u16 + prev_r + TRANSLATE_HANDLE_BAR_DIVISION as u16;
                let a1 = base_vindex_x as u16 + r as u16;
                let b1 = base_vindex_x as u16 + r as u16 + TRANSLATE_HANDLE_BAR_DIVISION as u16;
                is.add(iindex_x + 0).write(a0);
                is.add(iindex_x + 1).write(b0);
                is.add(iindex_x + 2).write(a1);
                is.add(iindex_x + 3).write(a1);
                is.add(iindex_x + 4).write(b1);
                is.add(iindex_x + 5).write(b0);
                iindex_x += 6;

                let a0 = base_vindex_y as u16 + prev_r;
                let b0 = base_vindex_y as u16 + prev_r + TRANSLATE_HANDLE_BAR_DIVISION as u16;
                let a1 = base_vindex_y as u16 + r as u16;
                let b1 = base_vindex_y as u16 + r as u16 + TRANSLATE_HANDLE_BAR_DIVISION as u16;
                is.add(iindex_y + 0).write(a0);
                is.add(iindex_y + 1).write(b0);
                is.add(iindex_y + 2).write(a1);
                is.add(iindex_y + 3).write(a1);
                is.add(iindex_y + 4).write(b1);
                is.add(iindex_y + 5).write(b0);
                iindex_y += 6;

                let a0 = base_vindex_z as u16 + prev_r;
                let b0 = base_vindex_z as u16 + prev_r + TRANSLATE_HANDLE_BAR_DIVISION as u16;
                let a1 = base_vindex_z as u16 + r as u16;
                let b1 = base_vindex_z as u16 + r as u16 + TRANSLATE_HANDLE_BAR_DIVISION as u16;
                is.add(iindex_z + 0).write(a0);
                is.add(iindex_z + 1).write(b0);
                is.add(iindex_z + 2).write(a1);
                is.add(iindex_z + 3).write(a1);
                is.add(iindex_z + 4).write(b1);
                is.add(iindex_z + 5).write(b0);
                iindex_z += 6;
            }
            let arrow_top_vindex_x = base_vindex_x + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2;
            let arrow_top_vindex_y = base_vindex_y + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2;
            let arrow_top_vindex_z = base_vindex_z + TRANSLATE_HANDLE_BAR_DIVISION as usize * 2;
            vs.add(arrow_top_vindex_x).write(PreviewHandleVertex {
                pos: [
                    TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
                    0.0,
                    0.0,
                    1.0,
                ],
                col: [1.0, 0.0, 0.0, 1.0],
            });
            vs.add(arrow_top_vindex_y).write(PreviewHandleVertex {
                pos: [
                    0.0,
                    TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
                    0.0,
                    1.0,
                ],
                col: [0.0, 1.0, 0.0, 1.0],
            });
            vs.add(arrow_top_vindex_z).write(PreviewHandleVertex {
                pos: [
                    0.0,
                    0.0,
                    TRANSLATE_HANDLE_BAR_LENGTH + TRANSLATE_HANDLE_ARROW_SIZE,
                    1.0,
                ],
                col: [0.0, 0.0, 1.0, 1.0],
            });
            let base_vindex_x = arrow_top_vindex_x + 1;
            let base_vindex_y = arrow_top_vindex_y + 1;
            let base_vindex_z = arrow_top_vindex_z + 1;
            for r in 0..TRANSLATE_HANDLE_ARROW_DIVISION {
                let (s, c) = (core::f32::consts::TAU * r as f32
                    / TRANSLATE_HANDLE_ARROW_DIVISION as f32)
                    .sin_cos();

                vs.add(base_vindex_x + r as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            TRANSLATE_HANDLE_BAR_LENGTH,
                            TRANSLATE_HANDLE_ARROW_RADIUS * s,
                            TRANSLATE_HANDLE_ARROW_RADIUS * c,
                            1.0,
                        ],
                        col: [1.0, 0.0, 0.0, 1.0],
                    });
                vs.add(base_vindex_y + r as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            TRANSLATE_HANDLE_ARROW_RADIUS * s,
                            TRANSLATE_HANDLE_BAR_LENGTH,
                            TRANSLATE_HANDLE_ARROW_RADIUS * c,
                            1.0,
                        ],
                        col: [0.0, 1.0, 0.0, 1.0],
                    });
                vs.add(base_vindex_z + r as usize)
                    .write(PreviewHandleVertex {
                        pos: [
                            TRANSLATE_HANDLE_ARROW_RADIUS * s,
                            TRANSLATE_HANDLE_ARROW_RADIUS * c,
                            TRANSLATE_HANDLE_BAR_LENGTH,
                            1.0,
                        ],
                        col: [0.0, 0.0, 1.0, 1.0],
                    });

                let prev_r = if r > 0 {
                    r as u16
                } else {
                    TRANSLATE_HANDLE_ARROW_DIVISION as u16
                } - 1;
                is.add(iindex_x + 0).write(arrow_top_vindex_x as u16);
                is.add(iindex_x + 1).write(base_vindex_x as u16 + prev_r);
                is.add(iindex_x + 2).write(base_vindex_x as u16 + r as u16);
                iindex_x += 3;
                is.add(iindex_y + 0).write(arrow_top_vindex_y as u16);
                is.add(iindex_y + 1).write(base_vindex_y as u16 + prev_r);
                is.add(iindex_y + 2).write(base_vindex_y as u16 + r as u16);
                iindex_y += 3;
                is.add(iindex_z + 0).write(arrow_top_vindex_z as u16);
                is.add(iindex_z + 1).write(base_vindex_z as u16 + prev_r);
                is.add(iindex_z + 2).write(base_vindex_z as u16 + r as u16);
                iindex_z += 3;

                if r > 1 {
                    is.add(iindex_x + 0).write(base_vindex_x as u16 + 0);
                    is.add(iindex_x + 1)
                        .write(base_vindex_x as u16 + r as u16 - 1);
                    is.add(iindex_x + 2).write(base_vindex_x as u16 + r as u16);
                    iindex_x += 3;
                    is.add(iindex_y + 0).write(base_vindex_y as u16 + 0);
                    is.add(iindex_y + 1)
                        .write(base_vindex_y as u16 + r as u16 - 1);
                    is.add(iindex_y + 2).write(base_vindex_y as u16 + r as u16);
                    iindex_y += 3;
                    is.add(iindex_z + 0).write(base_vindex_z as u16 + 0);
                    is.add(iindex_z + 1)
                        .write(base_vindex_z as u16 + r as u16 - 1);
                    is.add(iindex_z + 2).write(base_vindex_z as u16 + r as u16);
                    iindex_z += 3;
                }
            }
        }
        if should_flush {
            unsafe {
                device
                    .flush_mapped_memory_ranges(&[br::MappedMemoryRange::new_raw(
                        memhandle,
                        0,
                        upload_size as _,
                    )])
                    .expect("preview.upload_memory.flush");
            }
        }
        drop(ptr);

        let mut init_cp = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(work_queue_family_index).transient(),
        )
        .expect("preview.init_cp.create");
        let [mut init_cb] = br::CommandBufferObject::alloc_array(
            device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut init_cp,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("preview.init_cb.alloc");
        unsafe {
            init_cb
                .begin(&br::CommandBufferBeginInfo::new().onetime_submit())
                .expect("preview.init_cb.begin")
        }
        .copy_buffer(
            &upload_buffer,
            &internal_mesh_buffer,
            &[
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: core::mem::offset_of!(UploadBufferData, origin_axes_vbuf) as _,
                    dstOffset: 0,
                    size: size_of_val(PREVIEW_VS_ORIGIN_AXES) as _,
                }),
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: translate_handle_vbuf_upload_offset as _,
                    dstOffset: translate_handle_vbuf_range.start,
                    size: translate_handle_vbuf_range.end - translate_handle_vbuf_range.start,
                }),
                br::BufferCopy(br::vk::VkBufferCopy {
                    srcOffset: translate_handle_ibuf_upload_offset as _,
                    dstOffset: translate_handle_ibuf_range.start,
                    size: translate_handle_ibuf_range.end - translate_handle_ibuf_range.start,
                }),
            ],
        )
        .copy_buffer(
            &upload_buffer,
            &internal_uniform_buffer,
            &[br::BufferCopy::copy_data::<PreviewCameraData>(
                core::mem::offset_of!(UploadBufferData, camera_data_ubuf) as _,
                camera_data_ubuf_range.start,
            )],
        )
        .inject(|r| {
            device.cmd_pipeline_barrier(
                r,
                &br::DependencyInfo::new(
                    &[br::MemoryBarrier2::new()
                        .from(
                            br::PipelineStageFlags2::COPY,
                            br::AccessFlags2::TRANSFER.write,
                        )
                        .to(
                            br::PipelineStageFlags2::VERTEX_ATTRIBUTE_INPUT
                                | br::PipelineStageFlags2::VERTEX_SHADER,
                            br::AccessFlags2::VERTEX_ATTRIBUTE_READ
                                | br::AccessFlags2::UNIFORM_READ,
                        )],
                    &[],
                    &[],
                ),
            )
        })
        .end()
        .expect("preview.init_cb.end");
        work_queue
            .submit(
                &[br::SubmitInfo::new_array(
                    &[],
                    &[],
                    &[init_cb.as_transparent_ref()],
                    &[],
                )],
                None,
            )
            .expect("preview.init_cb.submit");

        let scratch_staging = PreviewScratchStagingBuffer::new(device);

        let mut descriptor_pool = br::DescriptorPoolObject::new(
            device,
            &br::DescriptorPoolCreateInfo::new(
                1,
                &[br::DescriptorType::UniformBuffer.make_size(1)],
            ),
        )
        .expect("preview.descriptor_pool.create");
        let [common_descriptor_set] = descriptor_pool
            .alloc_array(&[common_descriptor_set_layout.as_transparent_ref()])
            .expect("preview.descriptor.alloc");
        device.update_descriptor_sets(
            &[common_descriptor_set
                .binding_at(0)
                .write(br::DescriptorContents::uniform_buffer(
                    &internal_uniform_buffer,
                    camera_data_ubuf_range.clone(),
                ))],
            &[],
        );

        let mut command_pool = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("preview.command_pool.create");
        let [command_buffer] = br::CommandBufferObject::alloc_array(
            device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut command_pool,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("preview.command_buffer.create");

        let mut update_command_pool = br::CommandPoolObject::new(
            device,
            &br::CommandPoolCreateInfo::new(device.present_queue_family_index()),
        )
        .expect("preview.update_command_pool.create");
        let [update_command_buffer] = br::CommandBufferObject::alloc_array(
            device,
            &br::CommandBufferFixedCountAllocateInfo::new(
                &mut update_command_pool,
                br::CommandBufferLevel::Primary,
            ),
        )
        .expect("preview.update_command_buffer.alloc");

        work_queue.wait().expect("preview.init_cb.wait");
        // keep alive
        drop(mem);
        drop(upload_buffer);

        let (update_command_pool, _) = update_command_pool.unmanage();
        let (command_pool, _) = command_pool.unmanage();
        let (internal_data_memory, _) = internal_data_memory.unmanage();
        let (internal_uniform_buffer, _) = internal_uniform_buffer.unmanage();
        let (internal_mesh_buffer, _) = internal_mesh_buffer.unmanage();
        let (unlit_colored_shader, _) = unlit_colored_shader.unmanage();
        let (unlit_colored_object_pipeline_layout, _) =
            unlit_colored_object_pipeline_layout.unmanage();
        let (grid_shader, _) = grid_shader.unmanage();
        let (grid_pipeline_layout, _) = grid_pipeline_layout.unmanage();
        let (origin_axes_shader, _) = origin_axes_shader.unmanage();
        let (origin_axes_pipeline_layout, _) = origin_axes_pipeline_layout.unmanage();
        let (render_pass, _) = render_pass.unmanage();
        let (streaming_memory, _) = streaming_memory.unmanage();
        let (streaming_buffer, _) = streaming_buffer.unmanage();
        let (descriptor_pool, _) = descriptor_pool.unmanage();
        let (common_descriptor_set_layout, _) = common_descriptor_set_layout.unmanage();
        Self {
            common_descriptor_set_layout,
            descriptor_pool,
            common_descriptor_set,
            streaming_buffer,
            streaming_memory,
            streaming_memory_should_flush,
            active_rt_size: init_rt.size,
            active_framebuffer_resource_handle: init_rt.image_view,
            render_pass,
            framebuffer: core::mem::MaybeUninit::uninit(),
            origin_axes_pipeline_layout,
            origin_axes_shader,
            origin_axes_pipeline: core::mem::MaybeUninit::uninit(),
            grid_pipeline_layout,
            grid_shader,
            grid_pipeline: core::mem::MaybeUninit::uninit(),
            unlit_colored_object_pipeline_layout,
            unlit_colored_shader,
            gizmos_pipeline: core::mem::MaybeUninit::uninit(),
            internal_mesh_buffer,
            origin_axes_vbuf_range,
            translate_handle_vbuf_range,
            translate_handle_ibuf_range,
            internal_uniform_buffer,
            camera_data_ubuf_range,
            internal_data_memory,
            scratch_staging,
            pending_camera_data_updates: None,
            command_pool,
            command_buffer: command_buffer.native_ptr(),
            update_command_pool,
            update_command_buffer: update_command_buffer.native_ptr(),
            update_command_pending: false,
            dynamic_buffer: DynamicBuffer::new(),
            dynamic_ubuf: DynamicBuffer::new(),
            user_meshes: Vec::new(),
            user_renders: Vec::new(),
            valid: false,
        }
    }

    pub fn update(&mut self, committed_state: &mut CommittedPreviewState) {
        self.scratch_staging.reset();

        for m in committed_state.pushed_meshes.drain(..) {
            let index_stride = match m.index_type {
                IndexType::U16 => 2,
                IndexType::U32 => 4,
            };

            let vbuf = self.dynamic_buffer.alloc(m.vertices.len() as _);
            let ibuf = self.dynamic_buffer.alloc(m.indices.len() as _);
        }
    }

    pub fn validate(
        &mut self,
        device: &VulkanDevice,
        active_rt: &PreviewRenderTargetBuffer,
        committed_state: &mut CommittedPreviewState,
    ) {
        let mut framebuffer_changed = false;
        if !self.valid
            || active_rt.size != self.active_rt_size
            || active_rt.image_view != self.active_framebuffer_resource_handle
        {
            // Note: color bufferとdepth bufferは同時に変わるのでどっちかだけ見ればいい
            if self.valid {
                drop(unsafe {
                    br::FramebufferObject::manage(self.framebuffer.assume_init(), device)
                });
            }

            self.framebuffer.write(
                br::FramebufferObject::new(
                    device,
                    &br::FramebufferCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.render_pass),
                        &[active_rt.image_view_tref(), active_rt.depth_view_tref()],
                        active_rt.size.width,
                        active_rt.size.height,
                    ),
                )
                .expect("preview.validate.framebuffer")
                .unmanage()
                .0,
            );

            framebuffer_changed = true;
        }

        let mut origin_axes_pipeline_changed = false;
        if !self.valid || active_rt.size != self.active_rt_size {
            if self.valid {
                drop(unsafe {
                    br::PipelineObject::manage(self.origin_axes_pipeline.assume_init(), device)
                });
                drop(unsafe {
                    br::PipelineObject::manage(self.grid_pipeline.assume_init(), device)
                });
                drop(unsafe {
                    br::PipelineObject::manage(self.gizmos_pipeline.assume_init(), device)
                });
            }

            let [origin_axes_pipeline, grid_pipeline, gizmos_pipeline] = device
                .create_graphics_pipelines_array(&[
                    br::GraphicsPipelineCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.origin_axes_pipeline_layout),
                        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0),
                        &[
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Vertex,
                                br::VkHandleRef::from_raw_ref(&self.origin_axes_shader),
                                c"vertMain",
                            ),
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Fragment,
                                br::VkHandleRef::from_raw_ref(&self.origin_axes_shader),
                                c"fragMain",
                            ),
                        ],
                        &br::PipelineVertexInputStateCreateInfo::new(
                            &[br::VertexInputBindingDescription(
                                br::vk::VkVertexInputBindingDescription {
                                    binding: 0,
                                    stride: size_of::<PreviewOriginAxesVertex>() as _,
                                    inputRate: br::vk::VK_VERTEX_INPUT_RATE_VERTEX,
                                },
                            )],
                            &[
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 0,
                                        binding: 0,
                                        offset: core::mem::offset_of!(PreviewOriginAxesVertex, dir)
                                            as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 1,
                                        binding: 0,
                                        offset: core::mem::offset_of!(
                                            PreviewOriginAxesVertex,
                                            offset
                                        ) as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                            ],
                        ),
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::LineList,
                        ),
                        &br::PipelineViewportStateCreateInfo::new(
                            &[active_rt
                                .size
                                .into_rect(br::Offset2D::ZERO)
                                .make_viewport(0.0..1.0)],
                            &[active_rt.size.into_rect(br::Offset2D::ZERO)],
                        ),
                        &br::PipelineRasterizationStateCreateInfo::new(
                            br::PolygonMode::Fill,
                            br::CullModeFlags::NONE,
                            br::FrontFace::CounterClockwise,
                        ),
                        BLEND_STATE_SINGLE_PREMULTIPLIED,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .config_depth(Some(br::CompareOp::LessOrEqual), false),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0),
                        &[
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Vertex,
                                br::VkHandleRef::from_raw_ref(&self.grid_shader),
                                c"vertMain",
                            ),
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Fragment,
                                br::VkHandleRef::from_raw_ref(&self.grid_shader),
                                c"fragMain",
                            ),
                        ],
                        VI_STATE_EMPTY,
                        &br::PipelineInputAssemblyStateCreateInfo::new(
                            br::PrimitiveTopology::LineList,
                        ),
                        &br::PipelineViewportStateCreateInfo::new(
                            &[active_rt
                                .size
                                .into_rect(br::Offset2D::ZERO)
                                .make_viewport(0.0..1.0)],
                            &[active_rt.size.into_rect(br::Offset2D::ZERO)],
                        ),
                        &br::PipelineRasterizationStateCreateInfo::new(
                            br::PolygonMode::Fill,
                            br::CullModeFlags::NONE,
                            br::FrontFace::CounterClockwise,
                        ),
                        BLEND_STATE_SINGLE_PREMULTIPLIED,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .config_depth(Some(br::CompareOp::Less), false),
                    ),
                    br::GraphicsPipelineCreateInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.unlit_colored_object_pipeline_layout),
                        br::SubpassRef(br::VkHandleRef::from_raw_ref(&self.render_pass), 0),
                        &[
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Vertex,
                                br::VkHandleRef::from_raw_ref(&self.unlit_colored_shader),
                                c"vertMain",
                            ),
                            br::PipelineShaderStage::new(
                                br::ShaderStage::Fragment,
                                br::VkHandleRef::from_raw_ref(&self.unlit_colored_shader),
                                c"fragMain",
                            ),
                        ],
                        &br::PipelineVertexInputStateCreateInfo::new(
                            &[br::VertexInputBindingDescription::per_vertex_typed::<
                                PreviewHandleVertex,
                            >(0)],
                            &[
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 0,
                                        binding: 0,
                                        offset: core::mem::offset_of!(PreviewHandleVertex, pos)
                                            as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                                br::VertexInputAttributeDescription(
                                    br::vk::VkVertexInputAttributeDescription {
                                        location: 1,
                                        binding: 0,
                                        offset: core::mem::offset_of!(PreviewHandleVertex, col)
                                            as _,
                                        format: br::vk::VK_FORMAT_R32G32B32A32_SFLOAT,
                                    },
                                ),
                            ],
                        ),
                        IA_STATE_TRILIST,
                        &br::PipelineViewportStateCreateInfo::new(
                            &[active_rt
                                .size
                                .into_rect(br::Offset2D::ZERO)
                                .make_viewport(0.0..1.0)],
                            &[active_rt.size.into_rect(br::Offset2D::ZERO)],
                        ),
                        RASTER_STATE_DEFAULT_FILL_NOCULL,
                        BLEND_STATE_SINGLE_NONE,
                    )
                    .set_multisample_state(MS_STATE_EMPTY)
                    .set_depth_stencil_state(
                        &br::PipelineDepthStencilStateCreateInfo::new()
                            .config_depth(Some(br::CompareOp::Less), false),
                    ),
                ])
                .expect("preview.validate.origin_axes.pipelines.create");
            self.origin_axes_pipeline
                .write(origin_axes_pipeline.unmanage().0);
            self.grid_pipeline.write(grid_pipeline.unmanage().0);
            self.gizmos_pipeline.write(gizmos_pipeline.unmanage().0);

            origin_axes_pipeline_changed = true;
        }

        let main_camera_dirtified =
            core::mem::replace(&mut committed_state.main_camera_dirtified, false);
        if main_camera_dirtified || active_rt.size != self.active_rt_size {
            let buffer_offset = *self.pending_camera_data_updates.get_or_insert_with(|| {
                self.scratch_staging.reserve(size_of::<PreviewCameraData>())
            });
            unsafe {
                core::ptr::write(
                    self.scratch_staging
                        .mapped_ptr
                        .byte_add(buffer_offset)
                        .cast::<PreviewCameraData>(),
                    PreviewCameraData::new(&committed_state.main_camera, active_rt.aspect_wh()),
                );
            }
        }

        self.update_command_pending = false;
        if self.pending_camera_data_updates.is_some() {
            // needs update device data
            self.scratch_staging.ops_before_copy(device);

            unsafe {
                br::vkfn_wrapper::reset_command_pool(
                    device.as_transparent_ref(),
                    br::VkHandleRefMut::dangling(self.update_command_pool),
                    br::CommandPoolResetFlags::EMPTY,
                )
                .expect("preview.validate.update_command_pool.reset");
            }
            unsafe {
                br::vkfn_wrapper::begin_command_buffer(
                    br::VkHandleRefMut::dangling(self.update_command_buffer),
                    &br::CommandBufferBeginInfo::new(),
                )
                .expect("preview.validate.update_command_buffer.begin");
            }
            br::CmdRecord::new(unsafe { br::VkHandleRefMut::dangling(self.update_command_buffer) })
                .inject(|r| match self.pending_camera_data_updates.take() {
                    None => r,
                    Some(bo) => r.copy_buffer(
                        br::VkHandleRef::from_raw_ref(&self.scratch_staging.buffer),
                        br::VkHandleRef::from_raw_ref(&self.internal_uniform_buffer),
                        &[br::BufferCopy::copy_data::<PreviewCameraData>(
                            bo as _,
                            self.camera_data_ubuf_range.start,
                        )],
                    ),
                })
                .inject(|r| {
                    device.cmd_pipeline_barrier(
                        r,
                        &br::DependencyInfo::new(
                            &[br::MemoryBarrier2::new()
                                .from(
                                    br::PipelineStageFlags2::COPY,
                                    br::AccessFlags2::TRANSFER.write,
                                )
                                .to(
                                    br::PipelineStageFlags2::VERTEX_SHADER,
                                    br::AccessFlags2::UNIFORM_READ,
                                )],
                            &[],
                            &[],
                        ),
                    )
                })
                .end()
                .expect("preview.validate.update_command_buffer.end");
            self.update_command_pending = true;
        }

        if framebuffer_changed
            || origin_axes_pipeline_changed
            || active_rt.size != self.active_rt_size
        {
            unsafe {
                br::vkfn_wrapper::reset_command_pool(
                    device.as_transparent_ref(),
                    br::VkHandleRefMut::dangling(self.command_pool),
                    br::CommandPoolResetFlags::RELEASE_RESOURCES,
                )
                .expect("preview.validate.command_pool.reset");
            }

            unsafe {
                br::vkfn_wrapper::begin_command_buffer(
                    br::VkHandleRefMut::dangling(self.command_buffer),
                    &br::CommandBufferBeginInfo::new(),
                )
                .expect("preview.validate.command_buffer.begin");
            }
            br::CmdRecord::new(unsafe { br::VkHandleRefMut::dangling(self.command_buffer) })
                .begin_render_pass(
                    &br::RenderPassBeginInfo::new(
                        br::VkHandleRef::from_raw_ref(&self.render_pass),
                        br::VkHandleRef::from_raw_ref(unsafe {
                            self.framebuffer.assume_init_ref()
                        }),
                        active_rt.size.into_rect(br::Offset2D::ZERO),
                        &[
                            br::ClearValue::color_f32([0.0, 0.0, 0.0, 1.0]),
                            br::ClearValue::depth_stencil(1.0, 0),
                        ],
                    ),
                    br::SubpassContents::Inline,
                )
                .bind_pipeline(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(unsafe { self.grid_pipeline.assume_init_ref() }),
                )
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    0,
                    &[self.common_descriptor_set],
                    &[],
                )
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                    &PreviewGridPushConstantData {
                        dir: [1.0, 0.0, 0.0, 0.0],
                        start: [0.0, 0.0, -250.0, 1.0],
                        altdir: [0.0, 0.0, 1.0, 0.0],
                        scale: 1.0,
                    },
                )
                .draw(2, 500, 0, 0)
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                    &PreviewGridPushConstantData {
                        dir: [0.0, 0.0, 1.0, 0.0],
                        start: [-250.0, 0.0, 0.0, 1.0],
                        altdir: [1.0, 0.0, 0.0, 0.0],
                        scale: 1.0,
                    },
                )
                .draw(2, 500, 0, 0)
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                    &PreviewGridPushConstantData {
                        dir: [1.0, 0.0, 0.0, 0.0],
                        start: [0.0, 0.0, -250.0, 1.0],
                        altdir: [0.0, 0.0, 1.0, 0.0],
                        scale: 0.1,
                    },
                )
                .draw(2, 500, 0, 0)
                .push_constant(
                    br::VkHandleRef::from_raw_ref(&self.grid_pipeline_layout),
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                    &PreviewGridPushConstantData {
                        dir: [0.0, 0.0, 1.0, 0.0],
                        start: [-250.0, 0.0, 0.0, 1.0],
                        altdir: [1.0, 0.0, 0.0, 0.0],
                        scale: 0.1,
                    },
                )
                .draw(2, 500, 0, 0)
                .bind_pipeline(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(unsafe {
                        self.origin_axes_pipeline.assume_init_ref()
                    }),
                )
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.origin_axes_pipeline_layout),
                    0,
                    &[self.common_descriptor_set],
                    &[],
                )
                .bind_vertex_buffer_array(
                    0,
                    &[unsafe { br::VkHandleRef::dangling(self.internal_mesh_buffer) }],
                    &[self.origin_axes_vbuf_range.start],
                )
                .draw(PREVIEW_VS_ORIGIN_AXES.len() as _, 1, 0, 0)
                // clear depth for gizmos rendering
                .clear_attachments(
                    &[br::vk::VkClearAttachment {
                        aspectMask: (br::AspectMask::DEPTH | br::AspectMask::STENCIL).bits(),
                        colorAttachment: 0,
                        clearValue: br::ClearValue::depth_stencil(1.0, 0).0,
                    }],
                    &[br::vk::VkClearRect {
                        rect: active_rt.size.into_rect(br::Offset2D::ZERO),
                        baseArrayLayer: 0,
                        layerCount: 1,
                    }],
                )
                .bind_pipeline(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(unsafe {
                        self.gizmos_pipeline.assume_init_ref()
                    }),
                )
                .bind_descriptor_sets(
                    br::PipelineBindPoint::Graphics,
                    br::VkHandleRef::from_raw_ref(&self.unlit_colored_object_pipeline_layout),
                    0,
                    &[self.common_descriptor_set],
                    &[],
                )
                .bind_vertex_buffer_array(
                    0,
                    &[unsafe { br::VkHandleRef::dangling(self.internal_mesh_buffer) }],
                    &[self.translate_handle_vbuf_range.start],
                )
                .bind_index_buffer(
                    br::VkHandleRef::from_raw_ref(&self.internal_mesh_buffer),
                    self.translate_handle_ibuf_range.start as _,
                    br::IndexType::U16,
                )
                // TODO: あとでちゃんと計算する
                .draw_indexed(102 * 3, 1, 0, 0, 0)
                .end_render_pass()
                .end()
                .expect("preview.validate.command_buffer.end");
        }

        self.active_rt_size = active_rt.size;
        self.active_framebuffer_resource_handle = active_rt.image_view;
        self.valid = true;
    }

    // pub fn write_streaming_buffer_content(
    //     &mut self,
    //     device: &VulkanDevice,
    //     data: PreviewStreamingBufferContent,
    // ) {
    //     let ptr = unsafe {
    //         br::vkfn_wrapper::map_memory(
    //             device.as_transparent_ref(),
    //             br::VkHandleRefMut::dangling(self.streaming_memory),
    //             0..size_of::<PreviewStreamingBufferContent>() as _,
    //             0,
    //         )
    //         .expect("preview.write_streaming_buffer_content.map")
    //     };
    //     unsafe {
    //         ptr.cast::<PreviewStreamingBufferContent>().write(data);
    //     }
    //     if self.streaming_memory_should_flush {
    //         br::vkfn_wrapper::flush_mapped_memory_ranges(
    //             device.as_transparent_ref(),
    //             &[br::MappedMemoryRange::new(
    //                 br::VkHandleRef::from_raw_ref(&self.streaming_memory),
    //                 0..size_of::<PreviewStreamingBufferContent>() as _,
    //             )],
    //         )
    //         .expect("preview.write_streaming_buffer_content.flush");
    //     }
    //     unsafe {
    //         br::vkfn_wrapper::unmap_memory(
    //             device.as_transparent_ref(),
    //             br::VkHandleRefMut::dangling(self.streaming_memory),
    //         );
    //     }
    // }
}

#[repr(C)]
struct PreviewCompositePushConstants {
    pub position_modifier_matrix: Matrix4<SafeF32>,
    pub element_size: [f32; 2],
    pub screen_size: [f32; 2],
}

struct PreviewComposite {
    descriptor_set_layout: br::vk::VkDescriptorSetLayout,
    descriptor_pool: br::vk::VkDescriptorPool,
    descriptor_set: br::DescriptorSet,
    descriptor_bound_resource_handle: br::vk::VkImageView,
    pipeline_layout: br::vk::VkPipelineLayout,
    shader: br::vk::VkShaderModule,
    pipeline: core::mem::MaybeUninit<br::vk::VkPipeline>,
    pipeline_target_rt_size: br::Extent2D,
    pipeline_target_render_pass_handle: br::vk::VkRenderPass,
    pipeline_target_subpass: u32,
    valid: bool,
}
impl PreviewComposite {
    unsafe fn drop(self, vk_device: &VulkanDevice) {
        if self.valid {
            drop(unsafe { br::PipelineObject::manage(self.pipeline.assume_init(), vk_device) });
        }

        drop(unsafe { br::DescriptorPoolObject::manage(self.descriptor_pool, vk_device) });
        drop(unsafe { br::ShaderModuleObject::manage(self.shader, vk_device) });
        drop(unsafe { br::PipelineLayoutObject::manage(self.pipeline_layout, vk_device) });
        drop(unsafe {
            br::DescriptorSetLayoutObject::manage(self.descriptor_set_layout, vk_device)
        });
    }

    fn new(
        vk_device: &VulkanDevice,
        init_render_tex: &(impl br::VkHandle<Handle = br::vk::VkImageView> + ?Sized),
        smp: &(impl br::VkHandle<Handle = br::vk::VkSampler> + ?Sized),
        target_pass: br::SubpassRef<impl br::VkHandle<Handle = br::vk::VkRenderPass> + ?Sized>,
        init_screen_size: br::Extent2D,
    ) -> Self {
        let descriptor_set_layout = br::DescriptorSetLayoutObject::new(
            vk_device,
            &br::DescriptorSetLayoutCreateInfo::new(&[br::DescriptorType::CombinedImageSampler
                .make_binding(0, 1)
                .with_immutable_samplers(&[smp.as_transparent_ref()])]),
        )
        .expect("preview_composite.descriptor_set_layout.create");
        let pipeline_layout = br::PipelineLayoutObject::new(
            vk_device,
            &br::PipelineLayoutCreateInfo::new(
                &[descriptor_set_layout.as_transparent_ref()],
                &[br::PushConstantRange::for_type::<
                    PreviewCompositePushConstants,
                >(
                    br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
                    0,
                )],
            ),
        )
        .expect("preview_composite.pipeline_layout.create");
        let shader = vk_device.require_shader("simple_blit.spv");

        let mut descriptor_pool = br::DescriptorPoolObject::new(
            vk_device,
            &br::DescriptorPoolCreateInfo::new(
                1,
                &[br::DescriptorType::CombinedImageSampler.make_size(1)],
            ),
        )
        .expect("preview_composite.descriptor_pool.create");
        let [descriptor_set] = descriptor_pool
            .alloc_array(&[descriptor_set_layout.as_transparent_ref()])
            .expect("preview_composite.descriptor_set.alloc");
        vk_device.update_descriptor_sets(
            &[descriptor_set
                .binding_at(0)
                .write(br::DescriptorContents::combined_image_sampler(
                    init_render_tex,
                    br::ImageLayout::ShaderReadOnlyOpt,
                ))],
            &[],
        );

        let (descriptor_pool, _) = descriptor_pool.unmanage();
        let (shader, _) = shader.unmanage();
        let (pipeline_layout, _) = pipeline_layout.unmanage();
        let (descriptor_set_layout, _) = descriptor_set_layout.unmanage();
        Self {
            descriptor_set_layout,
            descriptor_pool,
            descriptor_set,
            descriptor_bound_resource_handle: init_render_tex.native_ptr(),
            pipeline_layout,
            shader,
            pipeline_target_rt_size: init_screen_size,
            pipeline_target_render_pass_handle: target_pass.0.native_ptr(),
            pipeline_target_subpass: target_pass.1,
            pipeline: core::mem::MaybeUninit::uninit(),
            valid: false,
        }
    }

    pub fn force_invalidate_descriptor_set_state(&mut self) {
        #[allow(invalid_value)]
        {
            self.descriptor_bound_resource_handle = unsafe { core::mem::transmute(0u64) };
        }
    }

    pub fn validate(
        &mut self,
        device: &VulkanDevice,
        content_rt: &PreviewRenderTargetBuffer,
        new_rt_size: br::Extent2D,
        new_target_render_pass_handle: br::vk::VkRenderPass,
        new_target_subpass: u32,
    ) {
        if !self.valid
            || self.pipeline_target_rt_size != new_rt_size
            || self.pipeline_target_render_pass_handle != new_target_render_pass_handle
            || self.pipeline_target_subpass != new_target_subpass
        {
            if self.valid {
                drop(unsafe { br::PipelineObject::manage(self.pipeline.assume_init(), device) });
            }

            let [pipeline] = device
                .create_graphics_pipelines_array(&[br::GraphicsPipelineCreateInfo::new(
                    br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
                    br::SubpassRef(
                        br::VkHandleRef::from_raw_ref(&new_target_render_pass_handle),
                        new_target_subpass,
                    ),
                    &[
                        br::PipelineShaderStage::new(
                            br::ShaderStage::Vertex,
                            br::VkHandleRef::from_raw_ref(&self.shader),
                            c"vertMain",
                        ),
                        br::PipelineShaderStage::new(
                            br::ShaderStage::Fragment,
                            br::VkHandleRef::from_raw_ref(&self.shader),
                            c"fragMain",
                        ),
                    ],
                    VI_STATE_EMPTY,
                    IA_STATE_TRISTRIP,
                    &br::PipelineViewportStateCreateInfo::new(
                        &[new_rt_size
                            .into_rect(br::Offset2D::ZERO)
                            .make_viewport(0.0..1.0)],
                        &[new_rt_size.into_rect(br::Offset2D::ZERO)],
                    ),
                    RASTER_STATE_DEFAULT_FILL_NOCULL,
                    BLEND_STATE_SINGLE_NONE,
                )
                .set_multisample_state(MS_STATE_EMPTY)])
                .expect("preview_composite.pipeline.create");

            self.pipeline.write(pipeline.unmanage().0);
            self.pipeline_target_rt_size = new_rt_size;
            self.pipeline_target_render_pass_handle = new_target_render_pass_handle;
            self.pipeline_target_subpass = new_target_subpass;
        }

        if self.descriptor_bound_resource_handle != content_rt.image_view {
            device.update_descriptor_sets(
                &[self.descriptor_set.binding_at(0).write(
                    br::DescriptorContents::combined_image_sampler(
                        br::VkHandleRef::from_raw_ref(&content_rt.image_view),
                        br::ImageLayout::ShaderReadOnlyOpt,
                    ),
                )],
                &[],
            );

            self.descriptor_bound_resource_handle = content_rt.image_view;
        }

        self.valid = true;
    }

    pub fn populate_commands<'r>(
        &self,
        size: Size<PixelsUnit>,
        position_modifier_matrix: Matrix4<SafeF32>,
        ctx: &CustomRenderContext,
        rec: br::CmdRecord<'r>,
    ) -> br::CmdRecord<'r> {
        debug_assert!(self.valid);

        rec.bind_pipeline(
            br::PipelineBindPoint::Graphics,
            br::VkHandleRef::from_raw_ref(unsafe { self.pipeline.assume_init_ref() }),
        )
        .push_constant(
            br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
            br::vk::VK_SHADER_STAGE_VERTEX_BIT | br::vk::VK_SHADER_STAGE_FRAGMENT_BIT,
            0,
            &PreviewCompositePushConstants {
                position_modifier_matrix: position_modifier_matrix.transpose(),
                element_size: [size.width as _, size.height as _],
                screen_size: [ctx.rt_size.width as _, ctx.rt_size.height as _],
            },
        )
        .bind_descriptor_sets(
            br::PipelineBindPoint::Graphics,
            &br::VkHandleRef::from_raw_ref(&self.pipeline_layout),
            0,
            &[self.descriptor_set],
            &[],
        )
        .draw(4, 1, 0, 0)
    }
}
