use std::{
    collections::HashMap,
    sync::{Mutex, atomic::AtomicBool},
};

use bedrock::{
    self as br, CommandBufferMut, CommandPoolMut, Device, DeviceMemoryMut, Fence, FenceMut,
    ImageChild, MemoryBound, QueueMut, RenderPass, ShaderModule, Swapchain, VkHandle, VkHandleMut,
};

use crate::{
    Event, RendererSync, SyncEventBus, WindowHandle,
    graphics::{
        BLEND_STATE_SINGLE_NONE, IA_STATE_TRILIST, IA_STATE_TRISTRIP,
        RASTER_STATE_DEFAULT_FILL_NOCULL, UnboundVulkanSurface, VI_STATE_EMPTY, VulkanDevice,
        VulkanSurface, VulkanSwapchain,
    },
    rendering::{
        atlas::{AtlasRect, TextureAtlas},
        composite::{
            BoundCompositeRenderer, CompositeRenderingData, CompositeStreamingData,
            CompositeTreeRef, CompositeTreeRender, VectorRasterizationState,
        },
        text::{PerWindowFontSet, RootFontSet},
    },
    utils::{SafeF32, UnboundedRef},
};

pub mod atlas;
pub mod composite;
pub mod text;

#[repr(transparent)]
pub struct NewWindowVulkanSurface(pub UnboundVulkanSurface);
unsafe impl Sync for NewWindowVulkanSurface {}
unsafe impl Send for NewWindowVulkanSurface {}

pub struct NewWindowData {
    pub key: WindowHandle,
    pub vk_surface: NewWindowVulkanSurface,
    #[cfg(not(any(feature = "wayland", windows)))]
    pub latest_ui_scale_changes: UnboundedRef<Mutex<Option<f32>>>,
    #[cfg(not(any(feature = "wayland", windows)))]
    pub init_scale: SafeF32,
}

pub enum RenderMessage {
    NewWindow(NewWindowData),
    DestroyWindow(WindowHandle, std::sync::mpsc::Sender<()>),
    RegisterNormalized2DStaticMeshTexture {
        id: usize,
        vertices: &'static [[f32; 2]],
        indices: &'static [u16],
        width: f32,
        height: f32,
    },
}

struct Normalized2DStaticMeshTextureEntry {
    width: f32,
    height: f32,
    vertices: &'static [[f32; 2]],
    indices: &'static [u16],
}

pub struct RenderThread<'main> {
    pub vk_device: &'main VulkanDevice<'main>,
    pub shutdown_signal: &'main AtomicBool,
    pub renderer_sync: &'main Mutex<RendererSync>,
    pub global_time_base: &'main std::time::Instant,
    pub event_bus: &'main SyncEventBus,
    pub message_receiver: std::sync::mpsc::Receiver<RenderMessage>,
}
impl<'main> RenderThread<'main> {
    pub fn run(self) {
        tracing::info!("Starting RenderThread...");
        let mut render_queue = self
            .vk_device
            .queue(self.vk_device.present_queue_family_index(), 0);

        let mut composite_tree = CompositeTreeRender::new();
        let vg_render_formats = GlyphAtlasRenderingFormats {
            color: br::vk::VK_FORMAT_R8_UNORM,
            stencil: br::vk::VK_FORMAT_S8_UINT,
        };
        let glyph_atlas_manager_common_resources =
            GlyphAtlasManagerCommonResources::new(self.vk_device, &vg_render_formats);
        struct GlyphAtlasDataPerDpi<'d> {
            manager: MaskTextureAtlasManager<'d>,
            atlas_rects: Vec<AtlasRect>,
            vector_raster_state: VectorRasterizationState,
            ref_count: u64,
        }
        let mut glyph_atlas_per_scale: HashMap<SafeF32, GlyphAtlasDataPerDpi> = HashMap::new();
        let font_set = RootFontSet::new();
        let mut windows: HashMap<WindowHandle, WindowRenderer> = HashMap::new();
        let mut normalized_2d_static_mesh_textures: HashMap<
            usize,
            Normalized2DStaticMeshTextureEntry,
        > = HashMap::new();

        let mut any_swapchain_invalidated = false;
        'lp: while !self
            .shutdown_signal
            .load(std::sync::atomic::Ordering::Acquire)
        {
            // unsafe {
            //     w.manual_capture_begin();
            // }

            loop {
                match self.message_receiver.try_recv() {
                    Ok(RenderMessage::NewWindow(wd)) => {
                        #[cfg(feature = "wayland")]
                        let init_scale = SafeF32::new(
                            wd.key
                                .state()
                                .committed_state
                                .lock()
                                .expect("poisoned")
                                .active_buffer_scale,
                        )
                        .expect("invalid scale");
                        #[cfg(windows)]
                        let init_scale =
                            SafeF32::new(wd.key.ui_scale_factor()).expect("invalid scale");
                        #[cfg(not(any(feature = "wayland", windows)))]
                        let init_scale = wd.init_scale;

                        let window_glyph_atlas = match glyph_atlas_per_scale.entry(init_scale) {
                            // use existing
                            std::collections::hash_map::Entry::Occupied(x) => x.into_mut(),
                            // create new one
                            std::collections::hash_map::Entry::Vacant(x) => {
                                x.insert(GlyphAtlasDataPerDpi {
                                    manager: MaskTextureAtlasManager::new(
                                        &glyph_atlas_manager_common_resources,
                                        &mut render_queue,
                                        self.vk_device.present_queue_family_index(),
                                    ),
                                    atlas_rects: Vec::new(),
                                    vector_raster_state: VectorRasterizationState::new(),
                                    ref_count: 0,
                                })
                            }
                        };
                        window_glyph_atlas.ref_count += 1;
                        windows.insert(
                            wd.key,
                            WindowRenderer::new(
                                self.vk_device,
                                wd,
                                init_scale,
                                window_glyph_atlas.manager.atlas(),
                                &font_set,
                            ),
                        );
                    }
                    Ok(RenderMessage::DestroyWindow(window_handle, done_event_bus)) => {
                        if let Some(x) = windows.remove(&window_handle) {
                            let current = glyph_atlas_per_scale
                                .get_mut(&x.active_scale())
                                .expect("invalid state");
                            current.ref_count -= 1;
                            if current.ref_count == 0 {
                                // no references
                                glyph_atlas_per_scale.remove(&x.active_scale());
                            }
                        }

                        if let Err(e) = done_event_bus.send(()) {
                            tracing::error!(reason = %e, "done_event_bus.send");
                        };
                    }
                    Ok(RenderMessage::RegisterNormalized2DStaticMeshTexture {
                        id,
                        vertices,
                        indices,
                        width,
                        height,
                    }) => {
                        normalized_2d_static_mesh_textures.insert(
                            id,
                            Normalized2DStaticMeshTextureEntry {
                                width,
                                height,
                                vertices,
                                indices,
                            },
                        );
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

            for x in windows.values_mut() {
                if x.take_swapchain_externally_invalidation_signal() {
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
                self.vk_device
                    .update_descriptor_sets(&descriptor_writes, &[]);

                any_swapchain_invalidated = false;
            }

            // flush synchronizing buffers
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
                renderer: &'x WindowRenderer<'x>,
                render_wait_semaphores: Vec<br::VkHandleRef<'x, br::vk::VkSemaphore>>,
                render_wait_stages: Vec<br::PipelineStageFlags>,
                render_commands: Vec<br::VkHandleRef<'x, br::vk::VkCommandBuffer>>,
                render_signal_semaphores: Vec<br::VkHandleRef<'x, br::vk::VkSemaphore>>,
                present_backbuffer_index: u32,
            }
            let mut submit_parameters = Vec::with_capacity(windows.len());
            for x in windows.values_mut() {
                let backbuffer_index = match x.acquire_backbuffer_with_wait() {
                    Ok(x) => x,
                    Err(e) if e == br::vk::VK_ERROR_OUT_OF_DATE_KHR => {
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
                        .get_mut(&x.active_scale())
                        .expect("invalid state");
                    current.ref_count -= 1;
                    let removed = if current.ref_count == 0 {
                        // no references
                        glyph_atlas_per_scale.remove(&x.active_scale())
                    } else {
                        None
                    };

                    let new_atlas_mgr = match glyph_atlas_per_scale.entry(scale) {
                        // reuse existing
                        std::collections::hash_map::Entry::Occupied(o) => o.into_mut(),
                        std::collections::hash_map::Entry::Vacant(v) => match removed {
                            // reuse existing with clear
                            Some(mut data) => {
                                data.manager.clear();
                                data.atlas_rects.clear();
                                data.ref_count = 0;
                                v.insert(data)
                            }
                            // new one
                            None => v.insert(GlyphAtlasDataPerDpi {
                                manager: MaskTextureAtlasManager::new(
                                    &glyph_atlas_manager_common_resources,
                                    &mut render_queue,
                                    self.vk_device.present_queue_family_index(),
                                ),
                                atlas_rects: Vec::new(),
                                vector_raster_state: VectorRasterizationState::new(),
                                ref_count: 0,
                            }),
                        },
                    };
                    new_atlas_mgr.ref_count += 1;

                    x.rescale(scale);
                    x.invalidate_render_commands(); // DescriptorSetをかえるときは再度つくりなおす必要がある
                    let mut descriptor_writes = Vec::with_capacity(1);
                    x.composite_renderer.rebind_glyph_atlas(
                        new_atlas_mgr.manager.atlas().as_image_view(),
                        &mut descriptor_writes,
                    );
                    self.vk_device
                        .update_descriptor_sets(&descriptor_writes, &[]);
                }

                let glyph_atlas_mgr = glyph_atlas_per_scale
                    .get_mut(&x.active_scale())
                    .expect("invalid state");
                // TODO: ここも重いようならあとで改善する
                for (&id, e) in &normalized_2d_static_mesh_textures {
                    if glyph_atlas_mgr.atlas_rects.get(id).is_none_or(|x| {
                        x == &AtlasRect {
                            left: 0,
                            top: 0,
                            right: 0,
                            bottom: 0,
                        }
                    }) {
                        tracing::trace!(id, "rasterize mesh");
                        let rect = glyph_atlas_mgr.manager.acquire(
                            (e.width * x.active_scale().value()).ceil() as _,
                            (e.height * x.active_scale().value()).ceil() as _,
                        );
                        if glyph_atlas_mgr.atlas_rects.len() <= id {
                            // extend with zero
                            glyph_atlas_mgr
                                .atlas_rects
                                .resize_with(id + 1, || AtlasRect {
                                    left: 0,
                                    top: 0,
                                    right: 0,
                                    bottom: 0,
                                });
                        }
                        glyph_atlas_mgr.atlas_rects[id] = rect;

                        // TODO: rasterize mesh
                    }
                }
                let needs_update_command = x.update(
                    current_t.as_secs_f32(),
                    &mut composite_tree,
                    &mut glyph_atlas_mgr.manager,
                    &mut glyph_atlas_mgr.atlas_rects,
                    &mut glyph_atlas_mgr.vector_raster_state,
                    self.event_bus,
                );

                let mut render_wait_semaphores = Vec::with_capacity(1);
                let mut render_wait_stages = Vec::with_capacity(1);

                // TODO: いったんめんどうなので毎回更新
                if true || needs_update_command {
                    x.submit_update_commands(&mut render_queue);

                    render_wait_semaphores.push(x.update_completion_semaphore_ref());
                    render_wait_stages.push(br::PipelineStageFlags::VERTEX_INPUT);
                }

                render_wait_semaphores.push(x.backbuffer_ready_semaphore.as_transparent_ref());
                render_wait_stages.push(br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT);

                submit_parameters.push(SubmitParameters {
                    renderer: x,
                    render_wait_semaphores,
                    render_wait_stages,
                    render_commands: vec![x.primary_render_commands_ref(backbuffer_index)],
                    render_signal_semaphores: vec![x.present_ready_semaphore_ref(backbuffer_index)],
                    present_backbuffer_index: backbuffer_index,
                });
            }

            for x in glyph_atlas_per_scale.values() {
                if x.vector_raster_state.is_empty() {
                    // no vector rasterization required
                    continue;
                }

                x.manager.perform_render(
                    &x.vector_raster_state,
                    &vg_render_formats,
                    &glyph_atlas_manager_common_resources,
                    &mut render_queue,
                );
            }

            if !submit_parameters.is_empty() {
                unsafe {
                    render_queue
                        .submit_raw(
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
                        .expect("queue submit")
                };
                let mut results = submit_parameters
                    .iter()
                    .map(|_| br::vk::VK_SUCCESS)
                    .collect::<Vec<_>>();
                match render_queue.present(&br::PresentInfo::new(
                    &submit_parameters
                        .iter()
                        .map(|x| x.render_signal_semaphores[0])
                        .collect::<Vec<_>>(),
                    &submit_parameters
                        .iter()
                        .map(|x| x.renderer.swapchain_ref())
                        .collect::<Vec<_>>(),
                    &submit_parameters
                        .iter()
                        .map(|x| x.present_backbuffer_index)
                        .collect::<Vec<_>>(),
                    &mut results,
                )) {
                    Ok(_) => (),
                    Err(e) if e == br::vk::VK_ERROR_OUT_OF_DATE_KHR => (/* handled later */),
                    Err(e) => Err::<(), _>(e).expect("queue present"),
                }

                for (r, w) in results.iter().zip(windows.values_mut()) {
                    if *r == br::vk::VK_ERROR_OUT_OF_DATE_KHR {
                        w.invalidate_swapchain();
                        any_swapchain_invalidated = true;
                    }
                }

                render_queue.wait().expect("render_queue.wait");
            }

            // unsafe {
            //     manual_capture_end();
            // }
        }

        unsafe {
            self.vk_device.wait().expect("device wait");
        }
        tracing::info!("RenderThread terminated");
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
    present_ready_semaphores: Vec<br::SemaphoreObject<&'d VulkanDevice<'d>>>,
    backbuffer_ready_semaphore: br::SemaphoreObject<&'d VulkanDevice<'d>>,
    primary_framebuffers: Vec<br::FramebufferObject<'d, &'d VulkanDevice<'d>>>,
    primary_render_pass: br::RenderPassObject<&'d VulkanDevice<'d>>,
    swapchain: VulkanSwapchain<'d, 'd>,
    surface: VulkanSurface<'d, 'd>,
    font_set: PerWindowFontSet<'d>,
}
impl<'d> WindowRenderer<'d> {
    fn new(
        device: &'d VulkanDevice<'d>,
        create_data: NewWindowData,
        init_scale: SafeF32,
        glyph_atlas: &TextureAtlas,
        root_font_set: &'d RootFontSet,
    ) -> Self {
        #[allow(unused_mut)]
        let mut font_set = PerWindowFontSet::new(root_font_set);
        #[cfg(feature = "wayland")]
        font_set.rescale((init_scale.value() * 72.0) as _);

        let surface = unsafe { create_data.vk_surface.0.bound(device) };
        let vk_swapchain = VulkanSwapchain::new(
            &surface,
            #[cfg(any(windows, feature = "wayland"))]
            || create_data.key.pixels_client_size(),
            #[cfg(target_os = "macos")]
            || {
                *create_data
                    .key
                    .state()
                    .active_rt_size
                    .lock()
                    .expect("poisoned")
            },
        );

        // TODO: 同じ構造のものがあれば使い回したい
        let vk_render_pass = br::RenderPassObject::new(
            device,
            &br::RenderPassCreateInfo2::new(
                &[br::AttachmentDescription2::new(surface.format())
                    .color_memory_op(br::LoadOp::Load, br::StoreOp::Store)
                    .layout_transition(br::ImageLayout::PresentSrc, br::ImageLayout::PresentSrc)],
                &[br::SubpassDescription2::new()
                    .colors(&[br::AttachmentReference2::color_attachment_opt(0)])],
                &[br::SubpassDependency2::new(
                    br::SubpassIndex::Internal(0),
                    br::SubpassIndex::External,
                )
                .by_region()
                .of_memory(
                    br::AccessFlags::COLOR_ATTACHMENT.write,
                    br::AccessFlags::MEMORY.read,
                )
                .of_execution(
                    br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT,
                    br::PipelineStageFlags(0),
                )],
            ),
        )
        .expect("render pass create");
        let vk_framebuffers = vk_swapchain
            .image_view_refs()
            .map(|bb| {
                br::FramebufferObject::new(
                    device,
                    &br::FramebufferCreateInfo::new(
                        &vk_render_pass,
                        &[bb],
                        vk_swapchain.size().width,
                        vk_swapchain.size().height,
                    ),
                )
                .expect("framebuffer create")
            })
            .collect::<Vec<_>>();

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
                vk_framebuffers.len() as _,
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
            glyph_atlas.view(),
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

        Self {
            w: create_data.key,
            #[cfg(any(feature = "wayland", windows))]
            active_scale: init_scale,
            #[cfg(not(any(feature = "wayland", windows)))]
            active_scale: create_data.init_scale,
            latest_ui_scale_changes: &create_data.key.state().latest_ui_scale_changes,
            font_set,
            vk_device: device,
            composite_root: create_data.key.composite_root(),
            composite_renderer,
            corner_cutout_renderer,
            last_composite_render_data: CompositeRenderingData {
                instructions: Vec::new(),
                render_passes: Vec::new(),
                required_backdrop_buffer_count: 0,
            },
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
            present_ready_semaphores: (0..vk_framebuffers.len())
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
            primary_render_pass: vk_render_pass,
            primary_framebuffers: vk_framebuffers,
        }
    }

    pub fn active_scale(&self) -> SafeF32 {
        self.active_scale
    }

    pub fn take_latest_ui_scale_changes(&self) -> Option<f32> {
        unsafe { &(*self.latest_ui_scale_changes) }
            .lock()
            .expect("poisoned")
            .take()
    }

    pub fn rescale(&mut self, scale: SafeF32) {
        self.active_scale = scale;

        #[cfg(feature = "freetype")]
        self.font_set.rescale((scale.value() * 72.0) as _);
    }

    pub fn update(
        &mut self,
        current_sec: f32,
        composite_tree: &mut CompositeTreeRender<Event>,
        glyph_atlas: &mut MaskTextureAtlasManager,
        mask_atlas_rects: &[AtlasRect],
        vector_raster_state: &mut VectorRasterizationState,
        events: &SyncEventBus,
    ) -> bool {
        let composite_render_data = self.composite_renderer.update(
            self.vk_device,
            composite_tree,
            self.composite_root,
            self.swapchain.size(),
            &self.font_set,
            glyph_atlas,
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
            self.surface.format(),
            self.swapchain.size(),
            self.last_composite_render_data
                .required_backdrop_buffer_count
                == 0,
        );

        // update_backdrop_resourcesでDescriptorSetの更新がはしるのでここでやる
        self.validate_render_commands();

        needs_update_commands
    }

    #[cfg(feature = "wayland")]
    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        self.w
            .state()
            .swapchain_externally_invalidation_signal
            .compare_exchange_weak(
                true,
                false,
                std::sync::atomic::Ordering::Relaxed,
                std::sync::atomic::Ordering::Relaxed,
            )
            == Ok(true)
    }
    #[cfg(not(feature = "wayland"))]
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
        self.primary_framebuffers.clear();

        self.surface.refresh_caps();
        self.swapchain.recreate(
            &self.surface,
            #[cfg(any(windows, feature = "wayland"))]
            || self.w.pixels_client_size(),
            #[cfg(target_os = "macos")]
            || *self.w.state().active_rt_size.lock().expect("poisoned"),
        );

        // recrease rt resources
        self.primary_framebuffers
            .extend(self.swapchain.image_view_refs().map(|bb| {
                br::FramebufferObject::new(
                    self.vk_device,
                    &br::FramebufferCreateInfo::new(
                        &self.primary_render_pass,
                        &[bb],
                        self.swapchain.size().width,
                        self.swapchain.size().height,
                    ),
                )
                .expect("framebuffer create")
            }));
        self.composite_renderer.recreate_rt_resources(
            self.vk_device,
            self.surface.format(),
            self.swapchain.image_view_refs(),
            self.swapchain.size(),
            descriptor_writes,
        );

        event_bus.push(Event::WindowPostResizeRenderBuffer { window: self.w });
        self.swapchain_invalidated = false;
    }

    pub fn acquire_backbuffer_with_wait(&mut self) -> br::Result<u32> {
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
                    self.swapchain.size(),
                    &self.swapchain.image_ref(n),
                    n,
                    |_, r| r,
                )
            })
            .inject(|r| {
                let Some(ref renderer) = self.corner_cutout_renderer else {
                    // no corner cutout rendering required
                    return r;
                };

                #[cfg(any(windows, feature = "wayland"))]
                let rt_pixel_size = self.w.pixels_client_size();
                #[cfg(target_os = "macos")]
                let rt_pixel_size = *self.w.state().active_rt_size.lock().expect("poisoned");
                #[cfg(any(windows, feature = "wayland"))]
                let rt_logical_size = self.w.client_size();
                #[cfg(target_os = "macos")]
                let rt_logical_size = *self.w.state().active_rt_size.lock().expect("poisoned");

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

        unsafe {
            device_queue
                .submit_raw(
                    &[br::SubmitInfo::new(
                        &[],
                        &[],
                        &[self.update_cb.as_transparent_ref()],
                        &[self.update_completion_semaphore.as_transparent_ref()],
                    )],
                    Some(self.update_completion_fence.as_transparent_ref_mut()),
                )
                .expect("gfx.update.submit");
        }
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
    render_pass: br::RenderPassObject<&'d VulkanDevice<'d>>,
    pipeline_layout: br::PipelineLayoutObject<&'d VulkanDevice<'d>>,
}
impl<'d> GlyphAtlasManagerCommonResources<'d> {
    pub fn new(vk_device: &'d VulkanDevice, formats: &GlyphAtlasRenderingFormats) -> Self {
        let fill_shader_module = vk_device.require_shader("vg-fill.spv");
        let curve_shader_module = vk_device.require_shader("vg-curve.spv");
        let vec_tri_fill_shader_module = vk_device.require_shader("vec-tri-fill.spv");

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
            br::vk::VkPipelineColorBlendAttachmentState {
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
            },
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
}
impl Drop for MaskTextureAtlasManager<'_> {
    fn drop(&mut self) {
        unsafe {
            self.atlas.drop(self.device);
        }
    }
}
impl<'d> MaskTextureAtlasManager<'d> {
    const VI_STATE_FOR_TRI_FANS: &'static br::PipelineVertexInputStateCreateInfo<'static> =
        &br::PipelineVertexInputStateCreateInfo::new(
            &[br::VertexInputBindingDescription::per_vertex_typed::<
                [f32; 2],
            >(0)],
            &[br::VertexInputAttributeDescription {
                location: 0,
                binding: 0,
                offset: 0,
                format: br::vk::VK_FORMAT_R32G32_SFLOAT,
            }],
        );
    const VI_STATE_FOR_CURVE: &'static br::PipelineVertexInputStateCreateInfo<'static> =
        &br::PipelineVertexInputStateCreateInfo::new(
            &[br::VertexInputBindingDescription::per_vertex_typed::<
                [f32; 4],
            >(0)],
            &[
                br::VertexInputAttributeDescription {
                    location: 0,
                    binding: 0,
                    offset: 0,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                },
                br::VertexInputAttributeDescription {
                    location: 1,
                    binding: 0,
                    offset: core::mem::size_of::<[f32; 2]>() as _,
                    format: br::vk::VK_FORMAT_R32G32_SFLOAT,
                },
            ],
        );
    const STENCIL_MASK: u32 = 0x01;
    const STENCIL_STATE_INVERT: &'static br::PipelineDepthStencilStateCreateInfo =
        &br::PipelineDepthStencilStateCreateInfo::new()
            .stencil_test(true)
            .stencil_state_front(
                br::vk::VkStencilOpState::always_forall(br::StencilOp::Invert)
                    .write_mask(Self::STENCIL_MASK),
            )
            .stencil_state_back(
                br::vk::VkStencilOpState::always_forall(br::StencilOp::Invert)
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
        let [triangle_fans_pipeline, curve_pipeline, colorize_pipeline] = common_res
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
                    Self::VI_STATE_FOR_TRI_FANS,
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
        unsafe {
            init_worker_queue
                .submit_raw(
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
        }

        Self {
            device: common_res.device,
            atlas,
            acquired_glyph_rects: HashMap::new(),
            rounded_fill_rects_by_radius: HashMap::new(),
            triangle_fans_pipeline,
            curve_pipeline,
            colorize_pipeline,
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

    pub fn perform_render(
        &self,
        state: &VectorRasterizationState,
        formats: &GlyphAtlasRenderingFormats,
        common_res: &GlyphAtlasManagerCommonResources,
        render_worker_queue: &mut (impl br::QueueMut + ?Sized),
    ) {
        // TODO: 最適化はあとで
        let filltri_points_offset = 0;
        let filltri_indices_offset =
            filltri_points_offset + core::mem::size_of_val(&state.fill_tri_points[..]);
        let curve_triangles_offset = (filltri_indices_offset
            + core::mem::size_of_val(&state.fill_tri_indices[..])
            + (core::mem::size_of::<[f32; 4]>() - 1))
            & !(core::mem::size_of::<[f32; 4]>() - 1);
        let vector_draw_buffer_total_size =
            curve_triangles_offset + core::mem::size_of_val(&state.curve_tris[..]);
        let mut vector_draw_buffer = br::BufferObject::new(
            self.device,
            &br::BufferCreateInfo::new(
                vector_draw_buffer_total_size,
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
                vector_draw_buffer_total_size,
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
                    srcSubresource: br::ImageSubresourceLayers::new(br::AspectMask::COLOR, 0, 0..1),
                    srcOffset: r.offset.with_z(0),
                    dstSubresource: br::ImageSubresourceLayers::new(br::AspectMask::COLOR, 0, 0..1),
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
        unsafe {
            render_worker_queue
                .submit_raw(
                    &[br::SubmitInfo::new(
                        &[],
                        &[],
                        &[cb[0].as_transparent_ref()],
                        &[],
                    )],
                    None,
                )
                .expect("vector render submit");
        }
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
