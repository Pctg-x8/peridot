use bedrock::{self as br, Device, ImageChild, SubmissionBatch};
use br::Image;
use peridot::mthelper::SharedRef;
#[cfg(feature = "transparent")]
use windows::core::Interface;
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Direct3D::D3D_FEATURE_LEVEL_11_0;
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Direct3D12::{
    D3D12CreateDevice, D3D12GetDebugInterface, ID3D12CommandQueue, ID3D12Debug, ID3D12Device,
    ID3D12Fence, ID3D12Resource, D3D12_COMMAND_LIST_TYPE_DIRECT, D3D12_COMMAND_QUEUE_DESC,
    D3D12_FENCE_FLAG_NONE,
};
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::DirectComposition::{
    DCompositionCreateDevice3, IDCompositionDesktopDevice, IDCompositionTarget,
    IDCompositionVisual2,
};
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Dxgi::Common::{
    DXGI_ALPHA_MODE_PREMULTIPLIED, DXGI_FORMAT_R8G8B8A8_UNORM, DXGI_SAMPLE_DESC,
};
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Dxgi::{
    CreateDXGIFactory2, IDXGIFactory2, IDXGISwapChain3, DXGI_CREATE_FACTORY_DEBUG,
    DXGI_SCALING_STRETCH, DXGI_SWAP_CHAIN_DESC1, DXGI_SWAP_EFFECT_DISCARD,
    DXGI_USAGE_RENDER_TARGET_OUTPUT,
};
#[cfg(feature = "transparent")]
use windows::Win32::System::SystemServices::GENERIC_ALL;

use crate::ThreadsafeWindowOps;

#[cfg(not(feature = "transparent"))]
pub struct Presenter {
    _window: SharedRef<ThreadsafeWindowOps>,
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<peridot::InstanceObject>>,
}
#[cfg(not(feature = "transparent"))]
impl Presenter {
    pub fn new(g: &peridot::Graphics, window: SharedRef<ThreadsafeWindowOps>) -> Self {
        use bedrock::PhysicalDevice;

        if !g
            .adapter()
            .win32_presentation_support(g.graphics_queue_family_index())
        {
            panic!("WindowSubsystem does not support Vulkan Rendering");
        }
        let s = g
            .adapter()
            .new_surface_win32(super::module_handle(), window.0)
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
    type Backbuffer = br::ImageViewObject<
        br::SwapchainImage<
            SharedRef<
                br::SwapchainObject<
                    peridot::DeviceObject,
                    br::SurfaceObject<peridot::InstanceObject>,
                >,
            >,
        >,
    >;

    fn format(&self) -> br::vk::VkFormat {
        self.sc.format()
    }
    fn backbuffer_count(&self) -> usize {
        self.sc.backbuffer_count()
    }
    fn backbuffer(&self, index: usize) -> Option<SharedRef<Self::Backbuffer>> {
        self.sc.backbuffer(index)
    }

    fn emit_initialize_backbuffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut + ?Sized>,
    ) {
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
        last_render_fence: &mut (impl br::Fence + br::VkHandleMut),
        backbuffer_index: u32,
        render_submission: impl br::SubmissionBatch,
        update_submission: Option<impl br::SubmissionBatch>,
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
struct UnsafeThreadsafeHandle(windows::Win32::Foundation::HANDLE);
#[cfg(feature = "transparent")]
impl Drop for UnsafeThreadsafeHandle {
    fn drop(&mut self) {
        unsafe {
            windows::Win32::Foundation::CloseHandle(self.0);
        }
    }
}
#[cfg(feature = "transparent")]
impl From<windows::Win32::Foundation::HANDLE> for UnsafeThreadsafeHandle {
    fn from(h: windows::Win32::Foundation::HANDLE) -> Self {
        Self(h)
    }
}
#[cfg(feature = "transparent")]
impl UnsafeThreadsafeHandle {
    #[inline]
    pub const fn handle(&self) -> windows::Win32::Foundation::HANDLE {
        self.0
    }
}
#[cfg(feature = "transparent")]
unsafe impl Sync for UnsafeThreadsafeHandle {}
#[cfg(feature = "transparent")]
unsafe impl Send for UnsafeThreadsafeHandle {}

#[cfg(feature = "transparent")]
#[repr(transparent)]
struct ThreadsafeEvent(windows::Win32::Foundation::HANDLE);
#[cfg(feature = "transparent")]
impl Drop for ThreadsafeEvent {
    fn drop(&mut self) {
        unsafe {
            windows::Win32::Foundation::CloseHandle(self.0);
        }
    }
}
#[cfg(feature = "transparent")]
impl ThreadsafeEvent {
    #[inline]
    pub fn new(manual_reset: bool, init_signaled: bool) -> windows::core::Result<Self> {
        unsafe {
            windows::Win32::System::Threading::CreateEventA(None, manual_reset, init_signaled, None)
                .map(Self)
        }
    }

    #[inline]
    pub fn wait(&mut self, timeout: u32) {
        unsafe {
            windows::Win32::System::Threading::WaitForSingleObject(self.0, timeout);
        }
    }
}
#[cfg(feature = "transparent")]
unsafe impl Sync for ThreadsafeEvent {}
#[cfg(feature = "transparent")]
unsafe impl Send for ThreadsafeEvent {}

#[cfg(feature = "transparent")]
struct InteropBackbufferResource {
    _shared_handle: UnsafeThreadsafeHandle,
    image_view: SharedRef<
        br::ImageViewObject<
            peridot::Image<
                br::ImageObject<peridot::DeviceObject>,
                br::DeviceMemoryObject<peridot::DeviceObject>,
            >,
        >,
    >,
}
#[cfg(feature = "transparent")]
impl InteropBackbufferResource {
    pub fn new(
        g: &peridot::Graphics,
        device: &ID3D12Device,
        resource: &ID3D12Resource,
        name_suffix: u32,
        size: &br::vk::VkExtent2D,
        format: br::vk::VkFormat,
    ) -> Self {
        use br::{Chainable, MemoryBound};

        let hname = widestring::WideCString::from_str(format!(
            "LocalPeridotApiInteropHandleCradle{name_suffix}"
        ))
        .expect("Failed to encode to WideString");
        let shared_handle = UnsafeThreadsafeHandle(unsafe {
            device
                .CreateSharedHandle(
                    resource,
                    None,
                    GENERIC_ALL,
                    windows::core::PCWSTR(hname.as_ptr()),
                )
                .expect("Failed to create SharedHandle from D3D12")
        });
        let image_ext =
            br::ExternalMemoryImageCreateInfo::new(br::ExternalMemoryHandleTypes::D3D12_RESOURCE);
        let image = br::ImageDesc::new(
            size,
            format,
            br::ImageUsage::COLOR_ATTACHMENT,
            br::ImageLayout::Preinitialized,
        )
        .chain(&image_ext)
        .create(g.device().clone())
        .expect("Failed to create Interop Image");
        let image_mreq = image.requirements();
        let handle_import_props = g
            .get_memory_win32_handle_properties(
                br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                shared_handle.handle(),
            )
            .expect("Failed to query Handle Memory Properties");
        let memory = SharedRef::new(
            g.device()
                .clone()
                .import_memory_win32(
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

        Self {
            _shared_handle: shared_handle,
            image_view,
        }
    }
}

#[cfg(feature = "transparent")]
struct Composition {
    device: IDCompositionDesktopDevice,
    target: IDCompositionTarget,
    root: IDCompositionVisual2,
}
#[cfg(feature = "transparent")]
impl Composition {
    fn new(w: &ThreadsafeWindowOps, swapchain: &IDXGISwapChain3) -> Self {
        let mut dh = std::ptr::null_mut();
        unsafe {
            DCompositionCreateDevice3(None, &IDCompositionDesktopDevice::IID, &mut dh)
                .expect("Failed to create DirectComposition Device")
        };
        let device = unsafe { std::mem::transmute::<_, IDCompositionDesktopDevice>(dh) };
        let target = unsafe {
            device
                .CreateTargetForHwnd(w.0, true)
                .expect("Failed to create DirectComposition Target")
        };
        let root = unsafe {
            device
                .CreateVisual()
                .expect("Failed to create DirectComposition Visual")
        };

        unsafe {
            root.SetContent(swapchain)
                .expect("Failed to set Swapchain for Composition");
            target
                .SetRoot(&root)
                .expect("Failed to set Composition Root Visual");
            device.Commit().expect("Failed to commit composition");
        }

        Composition {
            device,
            target,
            root,
        }
    }
}

#[cfg(feature = "transparent")]
pub struct Presenter {
    _window: SharedRef<ThreadsafeWindowOps>,
    _comp: Composition,
    device12: ID3D12Device,
    q: ID3D12CommandQueue,
    sc: IDXGISwapChain3,
    backbuffers: Vec<InteropBackbufferResource>,
    buffer_ready_order: br::SemaphoreObject<peridot::DeviceObject>,
    present_order: br::SemaphoreObject<peridot::DeviceObject>,
    render_completion_fence: ID3D12Fence,
    present_completion_fence: ID3D12Fence,
    render_completion_counter: u64,
    present_completion_counter: u64,
    _render_completion_fence_handle: UnsafeThreadsafeHandle,
    present_completion_event: ThreadsafeEvent,
    present_inflight: bool,
}
#[cfg(feature = "transparent")]
impl Presenter {
    pub fn new(g: &peridot::Graphics, window: SharedRef<ThreadsafeWindowOps>) -> Self {
        let rc = window.get_client_rect();

        let factory: IDXGIFactory2 = unsafe {
            CreateDXGIFactory2(if cfg!(debug_assertions) {
                DXGI_CREATE_FACTORY_DEBUG
            } else {
                0
            })
            .expect("Failed to create DXGI Factory")
        };
        let adapter = unsafe {
            factory
                .EnumAdapters(0)
                .expect("Failed to query primary adapter")
        };

        if cfg!(debug_assertions) {
            let mut interface = std::mem::MaybeUninit::<Option<ID3D12Debug>>::uninit();
            unsafe {
                D3D12GetDebugInterface(interface.as_mut_ptr())
                    .expect("Failed to get D3D12 Debug Layer");
                interface
                    .assume_init_ref()
                    .as_ref()
                    .expect("no debug interface?")
                    .EnableDebugLayer();
            }
        }
        let mut device12 = std::mem::MaybeUninit::<Option<ID3D12Device>>::uninit();
        unsafe {
            D3D12CreateDevice(&adapter, D3D_FEATURE_LEVEL_11_0, device12.as_mut_ptr())
                .expect("Failed to create Direct3D12 Device")
        };
        let device12 = unsafe { device12.assume_init().expect("no device created?") };
        let q = unsafe {
            device12
                .CreateCommandQueue(&D3D12_COMMAND_QUEUE_DESC {
                    Type: D3D12_COMMAND_LIST_TYPE_DIRECT,
                    Priority: 0,
                    NodeMask: 0,
                    Flags: Default::default(),
                })
                .expect("Failed to create Primary CommandQueue")
        };
        let sc = unsafe {
            factory
                .CreateSwapChainForComposition(
                    &q,
                    &DXGI_SWAP_CHAIN_DESC1 {
                        BufferCount: 2,
                        BufferUsage: DXGI_USAGE_RENDER_TARGET_OUTPUT,
                        Format: DXGI_FORMAT_R8G8B8A8_UNORM,
                        AlphaMode: DXGI_ALPHA_MODE_PREMULTIPLIED,
                        Width: (rc.right - rc.left) as _,
                        Height: (rc.bottom - rc.top) as _,
                        Stereo: false.into(),
                        SampleDesc: DXGI_SAMPLE_DESC {
                            Count: 1,
                            Quality: 0,
                        },
                        SwapEffect: DXGI_SWAP_EFFECT_DISCARD,
                        Scaling: DXGI_SCALING_STRETCH,
                        Flags: Default::default(),
                    },
                    None,
                )
                .expect("Failed to create SwapChain")
        };
        let sc = unsafe {
            sc.cast::<IDXGISwapChain3>()
                .expect("Failed to get swapchain 3 interface")
        };
        let comp = Composition::new(&window, &sc);
        let bb_size = br::vk::VkExtent2D {
            width: (rc.right - rc.left) as _,
            height: (rc.bottom - rc.top) as _,
        };
        let backbuffers = (0..2)
            .map(|bb_index| {
                let backbuffer = unsafe {
                    sc.GetBuffer(bb_index)
                        .expect("Failed to get Backbuffer from Swapchain")
                };

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

        let buffer_ready_order = g
            .device()
            .clone()
            .new_semaphore()
            .expect("Failed to create Buffer Ready Semaphore");
        let present_order = g
            .device()
            .clone()
            .new_semaphore()
            .expect("Failed to create Present Order Semaphore");
        let render_completion_fence = unsafe {
            device12
                .CreateFence(0, D3D12_FENCE_FLAG_NONE)
                .expect("Failed to create Render Completion Fence")
        };
        let present_completion_fence = unsafe {
            device12
                .CreateFence(0, D3D12_FENCE_FLAG_NONE)
                .expect("Failed to create Present Completion Fence")
        };
        let render_completion_fence_name =
            widestring::WideCString::from_str("LocalRenderCompletionFenceShared")
                .expect("Failed to encode widestring");
        let render_completion_fence_handle = UnsafeThreadsafeHandle(unsafe {
            device12
                .CreateSharedHandle(
                    &render_completion_fence,
                    None,
                    GENERIC_ALL,
                    windows::core::PCWSTR(render_completion_fence_name.as_ptr()),
                )
                .expect("Failed to create Shared Handle for Render Completion Fence")
        });
        g.device()
            .import_semaphore_win32_handle(
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
    type Backbuffer = br::ImageViewObject<
        peridot::Image<
            br::ImageObject<peridot::DeviceObject>,
            br::DeviceMemoryObject<peridot::DeviceObject>,
        >,
    >;

    fn format(&self) -> br::vk::VkFormat {
        br::vk::VK_FORMAT_R8G8B8A8_UNORM
    }
    fn backbuffer_count(&self) -> usize {
        2
    }
    fn backbuffer(&self, index: usize) -> Option<SharedRef<Self::Backbuffer>> {
        self.backbuffers.get(index).map(|b| b.image_view.clone())
    }

    fn emit_initialize_backbuffer_commands(
        &self,
        recorder: &mut br::CmdRecord<impl br::CommandBuffer + br::VkHandleMut + ?Sized>,
    ) {
        let barriers = self
            .backbuffers
            .iter()
            .map(|b| {
                br::ImageMemoryBarrier::new(
                    b.image_view.image(),
                    br::ImageSubresourceRange::color(0, 0),
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
        Ok(unsafe { self.sc.GetCurrentBackBufferIndex() })
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
        last_render_fence: &mut (impl br::Fence + br::VkHandleMut),
        _backbuffer_index: u32,
        render_submission: impl br::SubmissionBatch,
        update_submission: Option<impl br::SubmissionBatch>,
    ) -> br::Result<()> {
        let signal_counters = [self.render_completion_counter + 1];
        let signal_info = br::vk::VkD3D12FenceSubmitInfoKHR::from(br::D3D12FenceSubmitInfo::new(
            &[],
            &signal_counters,
        ));
        if let Some(cs) = update_submission {
            // copy -> render
            let update_signals = [&self.buffer_ready_order];
            let render_waits = [(
                &self.buffer_ready_order,
                br::PipelineStageFlags::VERTEX_INPUT,
            )];
            let render_signals = [&self.present_order];

            let update_submission = cs.with_signal_semaphores(&update_signals);
            let render_submission = render_submission
                .with_wait_semaphores(&render_waits)
                .with_signal_semaphores(&render_signals);

            let render_submission = br::vk::VkSubmitInfo {
                pNext: &signal_info as *const _ as _,
                ..render_submission.make_info_struct()
            };
            let update_submission = update_submission.make_info_struct();

            g.submit_buffered_commands_raw(
                &[update_submission, render_submission],
                last_render_fence,
            )
            .expect("Failed to submit render and update commands");
        } else {
            // render only (old logic)
            let render_signals = [&self.present_order];

            let render_submission = render_submission.with_signal_semaphores(&render_signals);

            let render_submission = br::vk::VkSubmitInfo {
                pNext: &signal_info as *const _ as _,
                ..render_submission.make_info_struct()
            };

            g.submit_buffered_commands_raw(&[render_submission], last_render_fence)
                .expect("Failed to submit render commands");
        }

        if self.present_inflight {
            self.present_completion_event
                .wait(windows::Win32::System::WindowsProgramming::INFINITE);
            self.present_inflight = false;
        }

        self.render_completion_counter += 1;
        unsafe {
            self.q
                .Wait(
                    &self.render_completion_fence,
                    self.render_completion_counter,
                )
                .expect("Failed to wait Render Completion Fence");
            self.sc.Present(0, 0).ok().expect("Failed to present");
            self.q
                .Signal(
                    &self.present_completion_fence,
                    self.present_completion_counter + 1,
                )
                .expect("Failed to signal Render Completion Fence");
            self.present_completion_counter += 1;
            self.present_completion_fence
                .SetEventOnCompletion(
                    self.present_completion_counter,
                    self.present_completion_event.0,
                )
                .expect("Failed to set Completion Event");
        }
        self.present_inflight = true;

        Ok(())
    }
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        if self.present_inflight {
            self.present_completion_event
                .wait(windows::Win32::System::WindowsProgramming::INFINITE);
            self.present_inflight = false;
        }

        self.backbuffers.clear();
        unsafe {
            self.sc
                .ResizeBuffers(
                    2,
                    new_size.0 as _,
                    new_size.1 as _,
                    DXGI_FORMAT_R8G8B8A8_UNORM,
                    0,
                )
                .expect("Failed to resize backbuffers");
        }
        for bb_index in 0..2 {
            let backbuffer = unsafe {
                self.sc
                    .GetBuffer(bb_index)
                    .expect("Failed to get Backbuffer from Swapchain")
            };

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
            .wait(windows::Win32::System::WindowsProgramming::INFINITE);
    }
}
