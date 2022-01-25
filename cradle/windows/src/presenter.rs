use bedrock as br;

#[cfg(not(feature = "mt"))]
use std::rc::Rc as SharedPtr;
#[cfg(feature = "mt")]
use std::sync::Arc as SharedPtr;

use crate::ThreadsafeWindowOps;

#[cfg(not(feature = "transparent"))]
pub struct Presenter {
    _window: SharedPtr<ThreadsafeWindowOps>,
    sc: peridot::IntegratedSwapchain,
}
#[cfg(not(feature = "transparent"))]
impl Presenter {
    pub fn new(g: &peridot::Graphics, window: HWND) -> Self {
        if !g
            .adapter()
            .win32_presentation_support(g.graphics_queue_family_index())
        {
            panic!("WindowSubsystem does not support Vulkan Rendering");
        }
        let s = br::Surface::new_win32(g.instance(), super::module_handle(), window)
            .expect("Failed to create Surface");
        let support = g
            .adapter()
            .surface_support(g.graphics_queue_family_index(), &s)
            .expect("Failed to query Surface Support");
        if !support {
            panic!("Vulkan does not support this surface to render");
        }

        Presenter {
            _window: window,
            sc: peridot::IntegratedSwapchain::new(g, s, peridot::math::Vector2(0, 0)),
        }
    }
}
#[cfg(not(feature = "transparent"))]
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }
    fn backbuffer_count(&self) -> usize {
        self.sc.backbuffer_count()
    }
    fn backbuffer(&self, index: usize) -> Option<SharedPtr<br::ImageView>> {
        self.sc.backbuffer(index)
    }

    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        self.sc.emit_initialize_backbuffer_commands(recorder)
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_backbuffer_index()
    }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_backbuffer_layout()
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut br::Fence,
        backbuffer_index: u32,
        render_submission: br::SubmissionBatch<'s>,
        update_submission: Option<br::SubmissionBatch<'s>>,
    ) -> br::Result<()> {
        self.sc.render_and_present(
            g,
            last_render_fence,
            backbuffer_index,
            render_submission,
            update_submission,
        )
    }
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }
    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> {
        peridot::math::Vector2(0, 0)
    }
}

#[cfg(feature = "transparent")]
#[repr(transparent)]
struct UnsafeThreadsafeHandle(winapi::shared::ntdef::HANDLE);
#[cfg(feature = "transparent")]
impl Drop for UnsafeThreadsafeHandle {
    fn drop(&mut self) {
        unsafe {
            winapi::um::handleapi::CloseHandle(self.0);
        }
    }
}
#[cfg(feature = "transparent")]
impl From<winapi::shared::ntdef::HANDLE> for UnsafeThreadsafeHandle {
    fn from(h: winapi::shared::ntdef::HANDLE) -> Self {
        Self(h)
    }
}
#[cfg(feature = "transparent")]
impl UnsafeThreadsafeHandle {
    pub fn handle(&self) -> winapi::shared::ntdef::HANDLE {
        self.0
    }
}
unsafe impl Sync for UnsafeThreadsafeHandle {}
unsafe impl Send for UnsafeThreadsafeHandle {}

#[cfg(feature = "transparent")]
#[repr(transparent)]
struct ThreadsafeEvent(winapi::shared::ntdef::HANDLE);
#[cfg(feature = "transparent")]
impl Drop for ThreadsafeEvent {
    fn drop(&mut self) {
        unsafe {
            winapi::um::handleapi::CloseHandle(self.0);
        }
    }
}
#[cfg(feature = "transparent")]
impl ThreadsafeEvent {
    pub fn new(manual_reset: bool, init_signaled: bool) -> std::io::Result<Self> {
        let e = unsafe {
            winapi::um::synchapi::CreateEventA(
                std::ptr::null_mut(),
                manual_reset as _,
                init_signaled as _,
                std::ptr::null(),
            )
        };
        if e.is_null() {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(Self(e))
        }
    }

    pub fn wait(&mut self, timeout: winapi::shared::minwindef::DWORD) {
        unsafe {
            winapi::um::synchapi::WaitForSingleObject(self.0, timeout);
        }
    }
}
unsafe impl Sync for ThreadsafeEvent {}
unsafe impl Send for ThreadsafeEvent {}

#[cfg(feature = "transparent")]
struct InteropBackbufferResource {
    _shared_handle: UnsafeThreadsafeHandle,
    _image: peridot::Image,
    image_view: SharedPtr<br::ImageView>,
}
#[cfg(feature = "transparent")]
impl InteropBackbufferResource {
    pub fn new(
        g: &peridot::Graphics,
        device: &comdrive::d3d12::Device,
        resource: &comdrive::d3d12::Resource,
        name_suffix: u32,
        size: &br::vk::VkExtent2D,
        format: br::vk::VkFormat,
    ) -> Self {
        use br::{Chainable, MemoryBound};

        let hname = widestring::WideCString::from_str(format!(
            "LocalPeridotApiInteropHandleCradle{}",
            name_suffix
        ))
        .expect("Failed to encode to WideString");
        let shared_handle = UnsafeThreadsafeHandle(
            device
                .create_shared_handle(resource, None, &hname)
                .expect("Failed to create SharedHandle from D3D12"),
        );
        let image_ext =
            br::ExternalMemoryImageCreateInfo::new(br::ExternalMemoryHandleTypes::D3D12_RESOURCE);
        let image = br::ImageDesc::new(
            size,
            format,
            br::ImageUsage::COLOR_ATTACHMENT,
            br::ImageLayout::Preinitialized,
        )
        .chain(&image_ext)
        .create(&g)
        .expect("Failed to create Interop Image");
        let image_mreq = image.requirements();
        let handle_import_props = g
            .get_memory_win32_handle_properties(
                br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                shared_handle.handle(),
            )
            .expect("Failed to query Handle Memory Properties");
        let memory = SharedPtr::new(
            br::DeviceMemory::import_win32(
                &g,
                image_mreq.size as _,
                g.memory_type_manager
                    .device_local_index(
                        image_mreq.memoryTypeBits & handle_import_props.memoryTypeBits,
                    )
                    .expect("Failed to find matching memory type for importing")
                    .index(),
                br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                shared_handle.handle(),
                &hname,
            )
            .expect("Failed to import External Memory from D3D12Resource")
            .into(),
        );
        let image =
            peridot::Image::bound(image, &memory, 0).expect("Failed to bind image backing memory");
        let image_view = image
            .create_view(
                None,
                None,
                &Default::default(),
                &br::ImageSubresourceRange::color(0..1, 0..1),
            )
            .expect("Failed to create ImageView for Rendering")
            .into();

        InteropBackbufferResource {
            _shared_handle: shared_handle,
            _image: image,
            image_view,
        }
    }
}

#[cfg(feature = "transparent")]
struct Composition {
    _device: comdrive::dcomp::Device,
    _target: comdrive::dcomp::Target,
    _root: comdrive::dcomp::Visual,
}
#[cfg(feature = "transparent")]
impl Composition {
    fn new(w: &ThreadsafeWindowOps, swapchain: &comdrive::dxgi::SwapChain) -> Self {
        use comdrive::dcomp::TargetProvider;

        let device =
            comdrive::dcomp::Device::new(None).expect("Failed to create DirectComposition Device");
        let target = device
            .new_target_for(&w.0)
            .expect("Failed to create DirectComposition Target");
        let root = device
            .new_visual()
            .expect("Failed to create DirectComposition Visual");

        root.set_content(Some(swapchain))
            .expect("Failed to set Swapchain for Composition");
        target
            .set_root(&root)
            .expect("Failed to set Composition Root Visual");
        device.commit().expect("Failed to commit composition");

        Composition {
            _device: device,
            _target: target,
            _root: root,
        }
    }
}

#[cfg(feature = "transparent")]
pub struct Presenter {
    _window: SharedPtr<ThreadsafeWindowOps>,
    _comp: Composition,
    device12: comdrive::d3d12::Device,
    q: comdrive::d3d12::CommandQueue,
    sc: comdrive::dxgi::SwapChain,
    backbuffers: Vec<InteropBackbufferResource>,
    buffer_ready_order: br::Semaphore,
    present_order: br::Semaphore,
    render_completion_fence: comdrive::d3d12::Fence,
    present_completion_fence: comdrive::d3d12::Fence,
    render_completion_counter: u64,
    present_completion_counter: u64,
    _render_completion_fence_handle: UnsafeThreadsafeHandle,
    present_completion_event: ThreadsafeEvent,
    present_inflight: bool,
}
#[cfg(feature = "transparent")]
impl Presenter {
    pub fn new(g: &peridot::Graphics, window: SharedPtr<ThreadsafeWindowOps>) -> Self {
        let rc = window.get_client_rect();

        let factory = comdrive::dxgi::Factory::new(cfg!(debug_assertions))
            .expect("Failed to create DXGI Factory");
        let adapter = factory.adapter(0).expect("Failed to query primary adapter");

        if cfg!(debug_assertions) {
            comdrive::d3d12::Device::enable_debug_layer()
                .expect("Failed to enable D3D12 Debug Layer");
        }
        let device12 = comdrive::d3d12::Device::new(&adapter, comdrive::d3d::FeatureLevel::v11)
            .expect("Failed to create Direct3D12 Device");
        let q = device12
            .new_command_queue(comdrive::d3d12::CommandType::Direct, 0)
            .expect("Failed to create Primary CommandQueue");
        let sc = factory
            .new_swapchain(
                &q,
                metrics::Size2U((rc.right - rc.left) as _, (rc.bottom - rc.top) as _),
                comdrive::dxgi::DXGI_FORMAT_R8G8B8A8_UNORM,
                comdrive::dxgi::AlphaMode::Premultiplied,
                2,
                false,
            )
            .expect("Failed to create SwapChain");
        let comp = Composition::new(&window, &sc);
        let bb_size = br::vk::VkExtent2D {
            width: (rc.right - rc.left) as _,
            height: (rc.bottom - rc.top) as _,
        };
        let backbuffers = (0..2)
            .map(|bb_index| {
                let backbuffer = sc
                    .back_buffer(bb_index)
                    .expect("Failed to get Backbuffer from Swapchain");

                InteropBackbufferResource::new(
                    g,
                    &device12,
                    &backbuffer,
                    bb_index as _,
                    &bb_size,
                    br::vk::VK_FORMAT_R8G8B8A8_UNORM,
                )
            })
            .collect();

        let buffer_ready_order =
            br::Semaphore::new(g).expect("Failed to create Buffer Ready Semaphore");
        let present_order =
            br::Semaphore::new(g).expect("Failed to create Present Order Semaphore");
        let render_completion_fence = device12
            .new_fence(0, comdrive::d3d12::FENCE_FLAG_SHARED)
            .expect("Failed to create Render Completion Fence");
        let present_completion_fence = device12
            .new_fence(0, comdrive::d3d12::FENCE_FLAG_NONE)
            .expect("Failed to create Present Completion Fence");
        let render_completion_fence_name =
            widestring::WideCString::from_str("LocalRenderCompletionFenceShared")
                .expect("Failed to encode widestring");
        let render_completion_fence_handle = UnsafeThreadsafeHandle(
            device12
                .create_shared_handle(
                    &render_completion_fence,
                    None,
                    &render_completion_fence_name,
                )
                .expect("Failed to create Shared Handle for Render Completion Fence"),
        );
        g.import_semaphore_win32_handle(
            &present_order,
            br::ExternalSemaphoreHandleWin32::D3DFence(render_completion_fence_handle.handle()),
            &render_completion_fence_name,
        )
        .expect("Failed to import Render Completion Fence");
        let present_completion_event =
            ThreadsafeEvent::new(false, true).expect("Failed to create Present Completion Event");

        Presenter {
            _window: window,
            _comp: comp,
            device12,
            q,
            sc,
            backbuffers,
            buffer_ready_order,
            present_order,
            render_completion_fence,
            present_completion_fence,
            _render_completion_fence_handle: render_completion_fence_handle,
            render_completion_counter: 0,
            present_completion_counter: 0,
            present_completion_event,
            present_inflight: false,
        }
    }
}
#[cfg(feature = "transparent")]
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat {
        br::vk::VK_FORMAT_R8G8B8A8_UNORM
    }
    fn backbuffer_count(&self) -> usize {
        2
    }
    fn backbuffer(&self, index: usize) -> Option<SharedPtr<br::ImageView>> {
        self.backbuffers.get(index).map(|b| b.image_view.clone())
    }

    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        let barriers = self
            .backbuffers
            .iter()
            .map(|b| {
                br::ImageMemoryBarrier::new(
                    &br::ImageSubref::color(&b.image_view, 0..1, 0..1),
                    br::ImageLayout::Preinitialized,
                    br::ImageLayout::General,
                )
            })
            .collect::<Vec<_>>();

        recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::TOP_OF_PIPE,
            true,
            &[],
            &[],
            &barriers,
        );
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> {
        Ok(self.sc.current_back_buffer_index())
    }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (
            br::ImageLayout::General,
            br::PipelineStageFlags::TOP_OF_PIPE,
        )
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut br::Fence,
        _backbuffer_index: u32,
        mut render_submission: br::SubmissionBatch<'s>,
        update_submission: Option<br::SubmissionBatch<'s>>,
    ) -> br::Result<()> {
        use br::VkHandle;

        let signal_counters = [self.render_completion_counter + 1];
        let signal_info = br::vk::VkD3D12FenceSubmitInfoKHR::from(br::D3D12FenceSubmitInfo::new(
            &[],
            &signal_counters,
        ));
        if let Some(mut cs) = update_submission {
            // copy -> render
            cs.signal_semaphores.to_mut().push(&self.buffer_ready_order);
            render_submission.wait_semaphores.to_mut().extend(vec![(
                &self.buffer_ready_order,
                br::PipelineStageFlags::VERTEX_INPUT,
            )]);
            render_submission
                .signal_semaphores
                .to_mut()
                .push(&self.present_order);

            let (render_wait_semaphores, render_wait_stages): (Vec<_>, Vec<_>) = render_submission
                .wait_semaphores
                .iter()
                .map(|(s, st)| (s.native_ptr(), st.0))
                .unzip();
            let render_signal_semaphores = render_submission
                .signal_semaphores
                .iter()
                .map(|s| s.native_ptr())
                .collect::<Vec<_>>();
            let render_buffers = render_submission
                .command_buffers
                .iter()
                .map(|s| s.native_ptr())
                .collect::<Vec<_>>();
            let (update_wait_semaphores, update_wait_stages): (Vec<_>, Vec<_>) = cs
                .wait_semaphores
                .iter()
                .map(|(s, st)| (s.native_ptr(), st.0))
                .unzip();
            let update_signal_semaphores = cs
                .signal_semaphores
                .iter()
                .map(|s| s.native_ptr())
                .collect::<Vec<_>>();
            let update_buffers = cs
                .command_buffers
                .iter()
                .map(|s| s.native_ptr())
                .collect::<Vec<_>>();
            let render_submission = br::vk::VkSubmitInfo {
                pNext: &signal_info as *const _ as _,
                waitSemaphoreCount: render_wait_semaphores.len() as _,
                pWaitSemaphores: render_wait_semaphores.as_ptr(),
                pWaitDstStageMask: render_wait_stages.as_ptr(),
                signalSemaphoreCount: render_signal_semaphores.len() as _,
                pSignalSemaphores: render_signal_semaphores.as_ptr(),
                commandBufferCount: render_buffers.len() as _,
                pCommandBuffers: render_buffers.as_ptr(),
                ..Default::default()
            };
            let update_submission = br::vk::VkSubmitInfo {
                waitSemaphoreCount: update_wait_semaphores.len() as _,
                pWaitSemaphores: update_wait_semaphores.as_ptr(),
                pWaitDstStageMask: update_wait_stages.as_ptr(),
                signalSemaphoreCount: update_signal_semaphores.len() as _,
                pSignalSemaphores: update_signal_semaphores.as_ptr(),
                commandBufferCount: update_buffers.len() as _,
                pCommandBuffers: update_buffers.as_ptr(),
                ..Default::default()
            };

            g.submit_buffered_commands_raw(
                &[update_submission, render_submission],
                last_render_fence,
            )
            .expect("Failed to submit render and update commands");
        } else {
            // render only (old logic)
            render_submission
                .signal_semaphores
                .to_mut()
                .push(&self.present_order);

            let (render_wait_semaphores, render_wait_stages): (Vec<_>, Vec<_>) = render_submission
                .wait_semaphores
                .iter()
                .map(|(s, st)| (s.native_ptr(), st.0))
                .unzip();
            let render_signal_semaphores = render_submission
                .signal_semaphores
                .iter()
                .map(|s| s.native_ptr())
                .collect::<Vec<_>>();
            let render_buffers = render_submission
                .command_buffers
                .iter()
                .map(|s| s.native_ptr())
                .collect::<Vec<_>>();
            let render_submission = br::vk::VkSubmitInfo {
                pNext: &signal_info as *const _ as _,
                waitSemaphoreCount: render_wait_semaphores.len() as _,
                pWaitSemaphores: render_wait_semaphores.as_ptr(),
                pWaitDstStageMask: render_wait_stages.as_ptr(),
                signalSemaphoreCount: render_signal_semaphores.len() as _,
                pSignalSemaphores: render_signal_semaphores.as_ptr(),
                commandBufferCount: render_buffers.len() as _,
                pCommandBuffers: render_buffers.as_ptr(),
                ..Default::default()
            };

            g.submit_buffered_commands_raw(&[render_submission], last_render_fence)
                .expect("Failed to submit render commands");
        }

        if self.present_inflight {
            self.present_completion_event
                .wait(winapi::um::winbase::INFINITE);
            self.present_inflight = false;
        }

        self.render_completion_counter += 1;
        self.q
            .wait(
                &self.render_completion_fence,
                self.render_completion_counter,
            )
            .expect("Failed to wait Render Completion Fence");
        self.sc.present().expect("Failed to present");
        self.q
            .signal(
                &self.present_completion_fence,
                self.present_completion_counter + 1,
            )
            .expect("Failed to signal Render Completion Fence");
        self.present_completion_counter += 1;
        self.present_completion_fence
            .set_event_notification(
                self.present_completion_counter,
                self.present_completion_event.0,
            )
            .expect("Failed to set Completion Event");
        self.present_inflight = true;

        Ok(())
    }
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        if self.present_inflight {
            self.present_completion_event
                .wait(winapi::um::winbase::INFINITE);
            self.present_inflight = false;
        }

        self.backbuffers.clear();
        self.sc
            .resize(metrics::Size2U(new_size.0 as _, new_size.1 as _))
            .expect("Failed to resize backbuffers");
        for bb_index in 0..2 {
            let backbuffer = self
                .sc
                .back_buffer(bb_index)
                .expect("Failed to get Backbuffer from Swapchain");

            self.backbuffers.push(InteropBackbufferResource::new(
                g,
                &self.device12,
                &backbuffer,
                bb_index as _,
                &br::vk::VkExtent2D {
                    width: new_size.0 as _,
                    height: new_size.1 as _,
                },
                br::vk::VK_FORMAT_R8G8B8A8_UNORM,
            ));
        }
        true
    }
    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> {
        peridot::math::Vector2(0, 0)
    }
}
#[cfg(feature = "transparent")]
impl Drop for Presenter {
    fn drop(&mut self) {
        self.present_completion_event
            .wait(winapi::um::winbase::INFINITE);
    }
}
