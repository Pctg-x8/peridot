use crate::ThreadsafeWindowOps;
#[cfg(feature = "transparent")]
use bedrock::TypedVulkanSinkStructure;
use bedrock::{self as br, Device, VkHandle};
#[cfg(not(feature = "transparent"))]
use bedrock::{InstanceChild, SurfaceCreateInfo};
use parking_lot::RwLock;
use std::sync::Arc;
#[cfg(feature = "transparent")]
use windows::core::ComInterface;
#[cfg(feature = "transparent")]
use windows::Win32::Foundation::GENERIC_ALL;
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Direct3D::D3D_FEATURE_LEVEL_11_0;
#[cfg(feature = "transparent")]
use windows::Win32::Graphics::Direct3D12::{
    D3D12CreateDevice, D3D12GetDebugInterface, ID3D12CommandQueue, ID3D12Debug, ID3D12Device,
    ID3D12Fence, ID3D12Resource, D3D12_COMMAND_LIST_TYPE_DIRECT, D3D12_COMMAND_QUEUE_DESC,
    D3D12_FENCE_FLAG_NONE, D3D12_FENCE_FLAG_SHARED,
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
    DXGI_SCALING_STRETCH, DXGI_SWAP_CHAIN_DESC1, DXGI_SWAP_EFFECT_FLIP_DISCARD,
    DXGI_USAGE_RENDER_TARGET_OUTPUT,
};

#[cfg(not(feature = "transparent"))]
struct Surface {
    device: peridot::VulkanGfx,
    handle: br::vk::VkSurfaceKHR,
}
#[cfg(not(feature = "transparent"))]
impl Drop for Surface {
    #[inline]
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_surface(
                self.device.instance().as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.handle),
                None,
            );
        }
    }
}
#[cfg(not(feature = "transparent"))]
impl br::VkHandle for Surface {
    type Handle = br::vk::VkSurfaceKHR;

    #[inline]
    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

#[cfg(not(feature = "transparent"))]
pub struct Presenter {
    _window: Arc<RwLock<ThreadsafeWindowOps>>,
    sc: peridot::IntegratedSwapchain<Surface>,
}
#[cfg(not(feature = "transparent"))]
impl Presenter {
    pub fn new(g: &peridot::Graphics, window: Arc<RwLock<ThreadsafeWindowOps>>) -> Self {
        if !br::vkfn_wrapper::get_physical_device_win32_presentation_support(
            g.native_adapter_ref(),
            g.graphics_queue_family_index(),
        ) {
            panic!("WindowSubsystem does not support Vulkan Rendering");
        }
        let s = Surface {
            handle: unsafe {
                br::Win32SurfaceCreateInfo::new(
                    core::mem::transmute(super::module_handle()),
                    core::mem::transmute(window.read().0),
                )
                .execute(g.device().instance(), None)
                .expect("Failed to create Surface")
            },
            device: g.device().clone(),
        };
        let support = g
            .device()
            .surface_support(&s)
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

    fn back_buffer_count(&self) -> usize {
        self.sc.back_buffer_count()
    }

    fn back_buffer_size(&self) -> peridot::math::Vector2<u32> {
        self.sc.back_buffer_size()
    }

    fn back_buffer<'a>(&'a self, index: usize) -> Option<br::VkHandleRef<'a, br::vk::VkImage>> {
        self.sc.back_buffer(index)
    }

    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: br::CmdRecord<'r>,
    ) -> br::CmdRecord<'r> {
        self.sc.emit_initialize_back_buffer_commands(recorder)
    }

    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        self.sc.acquire_next_back_buffer_index()
    }

    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
    }

    fn render_and_present<'s, 'r>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        back_buffer_index: u32,
        render_submission: peridot::SubmissionBatchBuilder<'r>,
        update_submission: Option<peridot::SubmissionBatchBuilder<'r>>,
    ) -> br::Result<()>
    where
        's: 'r,
    {
        self.sc.render_and_present(
            g,
            last_render_fence,
            back_buffer_index,
            render_submission,
            update_submission,
        )
    }

    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs re-initializing back-buffer resource
        true
    }

    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        peridot::math::Vector2(0, 0)
    }
}

#[cfg(feature = "transparent")]
#[repr(transparent)]
struct UnsafeThreadsafeHandle(windows::Win32::Foundation::HANDLE);
#[cfg(feature = "transparent")]
impl Drop for UnsafeThreadsafeHandle {
    fn drop(&mut self) {
        if let Err(e) = unsafe { windows::Win32::Foundation::CloseHandle(self.0) } {
            tracing::warn!(cause = ?e, "Error closing a handle");
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
        if let Err(e) = unsafe { windows::Win32::Foundation::CloseHandle(self.0) } {
            tracing::warn!(cause = ?e, "Error closing an event handle");
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
    device: peridot::VulkanGfx,
    memory: br::vk::VkDeviceMemory,
    image: br::vk::VkImage,
}
#[cfg(feature = "transparent")]
impl Drop for InteropBackbufferResource {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_image(
                self.device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.image),
                None,
            );
            br::vkfn_wrapper::free_memory(
                self.device.as_transparent_ref(),
                br::VkHandleRefMut::dangling(self.memory),
                None,
            );
        }
    }
}
#[cfg(feature = "transparent")]
impl br::VkHandle for InteropBackbufferResource {
    type Handle = br::vk::VkImage;

    fn native_ptr(&self) -> Self::Handle {
        self.image
    }
}
#[cfg(feature = "transparent")]
impl InteropBackbufferResource {
    pub fn new(
        g: &peridot::Graphics,
        memory_property_fn: br::vk::PFN_vkGetMemoryWin32HandlePropertiesKHR,
        device: &ID3D12Device,
        resource: &ID3D12Resource,
        name_suffix: u32,
        size: br::vk::VkExtent2D,
        format: br::vk::VkFormat,
    ) -> Self {
        let hname = widestring::WideCString::from_str(format!(
            "LocalPeridotApiInteropHandleCradle{name_suffix}"
        ))
        .expect("Failed to encode to WideString");
        let shared_handle = UnsafeThreadsafeHandle(unsafe {
            device
                .CreateSharedHandle(
                    resource,
                    None,
                    GENERIC_ALL.0,
                    windows::core::PCWSTR(hname.as_ptr()),
                )
                .expect("Failed to create SharedHandle from D3D12")
        });
        let exportable = br::ExternalMemoryImageCreateInfo::new(
            br::ExternalMemoryHandleTypeWin32::D3D12Resource as _,
        );
        let image = br::vkfn_wrapper::create_image(
            g.device().as_transparent_ref(),
            &br::ImageCreateInfo::new(size, format)
                .with_usage(br::ImageUsageFlags::COLOR_ATTACHMENT)
                .with_next(&exportable),
            None,
        )
        .expect("Failed to create Interop Image");
        let image_mreq = unsafe {
            br::vkfn_wrapper::get_image_memory_requirements(
                g.device().as_transparent_ref(),
                br::VkHandleRef::dangling(image),
            )
        };
        let handle_import_props = {
            let mut sink = br::vk::VkMemoryWin32HandlePropertiesKHR::uninit_sink();

            br::error::translate_vk_result(unsafe {
                (memory_property_fn.0)(
                    g.device().native_ptr(),
                    br::ExternalMemoryHandleTypeWin32::D3D12Resource as _,
                    core::mem::transmute(shared_handle.handle()),
                    sink.as_mut_ptr(),
                )
            })
            .expect("Failed to query Handle Memory Properties");

            unsafe { sink.assume_init() }
        };
        let memory_type_index = g
            .device()
            .device_local_memory_index(
                image_mreq.memoryTypeBits & handle_import_props.memoryTypeBits,
            )
            .expect("Failed to find matching memory type for importing");
        let memory = unsafe {
            br::vkfn_wrapper::allocate_memory(
                g.device().as_transparent_ref(),
                &br::MemoryAllocateInfo::new(1, memory_type_index).with_next(
                    &br::ImportMemoryWin32HandleInfo::new(
                        br::ExternalMemoryHandleTypeWin32::D3D12Resource,
                        core::mem::transmute(shared_handle.handle()),
                        Some(&hname),
                    ),
                ),
                None,
            )
            .expect("Failed to import memory")
        };
        unsafe {
            g.device()
                .bind_image_raw(
                    br::VkHandleRefMut::dangling(image),
                    br::VkHandleRef::dangling(memory),
                    0,
                )
                .expect("Failed to bind image backing memory");
        }

        Self {
            _shared_handle: shared_handle,
            device: g.device().clone(),
            memory,
            image,
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
        let device: IDCompositionDesktopDevice = unsafe {
            DCompositionCreateDevice3(None).expect("Failed to create DirectComposition Device")
        };
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

        Self {
            device,
            target,
            root,
        }
    }
}

#[cfg(feature = "transparent")]
pub struct Presenter {
    _window: Arc<RwLock<ThreadsafeWindowOps>>,
    _comp: Composition,
    device12: ID3D12Device,
    q: ID3D12CommandQueue,
    sc: IDXGISwapChain3,
    size: peridot::math::Vector2<u32>,
    back_buffers: Vec<InteropBackbufferResource>,
    buffer_ready_order: br::SemaphoreObject<peridot::VulkanGfx>,
    present_order: br::SemaphoreObject<peridot::VulkanGfx>,
    render_completion_fence: ID3D12Fence,
    present_completion_fence: ID3D12Fence,
    render_completion_counter: u64,
    present_completion_counter: u64,
    _render_completion_fence_handle: UnsafeThreadsafeHandle,
    present_completion_event: ThreadsafeEvent,
    present_inflight: bool,
}
#[cfg(feature = "transparent")]
unsafe impl Sync for Presenter {}
#[cfg(feature = "transparent")]
unsafe impl Send for Presenter {}
#[cfg(feature = "transparent")]
impl Presenter {
    pub fn new(g: &peridot::Graphics, window: Arc<RwLock<ThreadsafeWindowOps>>) -> Self {
        let rc = window.read().get_client_rect();

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
                        SwapEffect: DXGI_SWAP_EFFECT_FLIP_DISCARD,
                        Scaling: DXGI_SCALING_STRETCH,
                        Flags: Default::default(),
                    },
                    None,
                )
                .expect("Failed to create SwapChain")
        };
        let sc = sc
            .cast::<IDXGISwapChain3>()
            .expect("Failed to get swapchain 3 interface");
        let comp = Composition::new(&window.read(), &sc);
        let bb_size = br::vk::VkExtent2D {
            width: (rc.right - rc.left) as _,
            height: (rc.bottom - rc.top) as _,
        };
        let memory_property_fn = unsafe {
            g.device()
                .load_function::<br::vk::PFN_vkGetMemoryWin32HandlePropertiesKHR>()
        };
        let back_buffers = (0..2)
            .map(|bb_index| {
                let back_buffer = unsafe {
                    sc.GetBuffer(bb_index)
                        .expect("Failed to get Backbuffer from Swapchain")
                };

                InteropBackbufferResource::new(
                    g,
                    memory_property_fn,
                    &device12,
                    &back_buffer,
                    bb_index as _,
                    bb_size.clone(),
                    br::vk::VK_FORMAT_R8G8B8A8_UNORM,
                )
            })
            .collect();

        let buffer_ready_order =
            br::SemaphoreObject::new(g.device().clone(), &br::SemaphoreCreateInfo::new())
                .expect("Failed to create Buffer Ready Semaphore");
        let present_order =
            br::SemaphoreObject::new(g.device().clone(), &br::SemaphoreCreateInfo::new())
                .expect("Failed to create Present Order Semaphore");
        let render_completion_fence = unsafe {
            device12
                .CreateFence(0, D3D12_FENCE_FLAG_SHARED)
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
                    GENERIC_ALL.0,
                    windows::core::PCWSTR(render_completion_fence_name.as_ptr()),
                )
                .expect("Failed to create Shared Handle for Render Completion Fence")
        });
        br::error::translate_vk_result(unsafe {
            (g.device()
                .load_function::<br::vk::PFN_vkImportSemaphoreWin32HandleKHR>()
                .0)(
                g.device().native_ptr(),
                &br::ImportSemaphoreWin32HandleInfo::by_handle(
                    &present_order,
                    br::ExternalSemaphoreHandleTypeWin32::D3DFence.with_handle(
                        core::mem::transmute(render_completion_fence_handle.handle()),
                    ),
                )
                .into_raw(),
            )
        })
        .expect("Failed to import Render Completion Fence");
        let present_completion_event =
            ThreadsafeEvent::new(false, true).expect("Failed to create Present Completion Event");

        Self {
            _window: window,
            _comp: comp,
            device12,
            q,
            sc,
            size: bb_size.into(),
            back_buffers,
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

    fn back_buffer_count(&self) -> usize {
        2
    }

    fn back_buffer_size(&self) -> peridot::math::Vector2<u32> {
        self.size
    }

    fn back_buffer<'a>(&'a self, index: usize) -> Option<br::VkHandleRef<'a, br::vk::VkImage>> {
        self.back_buffers
            .get(index)
            .map(br::VkHandle::as_transparent_ref)
    }

    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: br::CmdRecord<'r>,
    ) -> br::CmdRecord<'r> {
        let barriers = self
            .back_buffers
            .iter()
            .map(|b| {
                br::ImageMemoryBarrier::new(
                    b,
                    br::vk::VkImageSubresourceRange {
                        aspectMask: br::AspectMask::COLOR.bits(),
                        baseMipLevel: 0,
                        levelCount: 1,
                        baseArrayLayer: 0,
                        layerCount: 1,
                    },
                    br::ImageLayout::Undefined.to(br::ImageLayout::General),
                )
            })
            .collect::<Vec<_>>();

        recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE,
            br::PipelineStageFlags::TOP_OF_PIPE,
            br::vk::VK_DEPENDENCY_BY_REGION_BIT,
            &[],
            &[],
            &barriers,
        )
    }

    fn next_back_buffer_index(&mut self) -> br::Result<u32> {
        Ok(unsafe { self.sc.GetCurrentBackBufferIndex() })
    }

    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (
            br::ImageLayout::General,
            br::PipelineStageFlags::TOP_OF_PIPE,
        )
    }

    fn render_and_present<'s, 'r>(
        &'s mut self,
        g: &mut peridot::Graphics,
        last_render_fence: &mut impl br::VkHandleMut<Handle = br::vk::VkFence>,
        _backbuffer_index: u32,
        mut render_submission: peridot::SubmissionBatchBuilder<'r>,
        update_submission: Option<peridot::SubmissionBatchBuilder<'r>>,
    ) -> br::Result<()>
    where
        's: 'r,
    {
        let signal_counters = [self.render_completion_counter + 1];
        let signal_info = br::D3D12FenceSubmitInfo::new(&[0], &signal_counters);
        render_submission.add_signal_semaphores([self.present_order.as_transparent_ref()]);
        if let Some(mut cs) = update_submission {
            // copy -> render
            cs.add_signal_semaphores([self.buffer_ready_order.as_transparent_ref()]);
            render_submission.add_wait_semaphores([(
                self.buffer_ready_order.as_transparent_ref(),
                br::PipelineStageFlags::VERTEX_INPUT,
            )]);

            g.submit_buffered_commands(
                &[
                    cs.build(),
                    render_submission.build().with_next(&signal_info),
                ],
                last_render_fence,
            )
            .expect("Failed to submit render and update commands");
        } else {
            // render only (old logic)
            g.submit_buffered_commands(
                &[render_submission.build().with_next(&signal_info)],
                last_render_fence,
            )
            .expect("Failed to submit render commands");
        }

        if self.present_inflight {
            self.present_completion_event
                .wait(windows::Win32::System::Threading::INFINITE);
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

    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<u32>) -> bool {
        if self.present_inflight {
            self.present_completion_event
                .wait(windows::Win32::System::Threading::INFINITE);
            self.present_inflight = false;
        }

        self.back_buffers.clear();
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
        let memory_property_fn = unsafe {
            g.device()
                .load_function::<br::vk::PFN_vkGetMemoryWin32HandlePropertiesKHR>()
        };
        for bb_index in 0..2 {
            let back_buffer = unsafe {
                self.sc
                    .GetBuffer(bb_index)
                    .expect("Failed to get Backbuffer from Swapchain")
            };

            self.back_buffers.push(InteropBackbufferResource::new(
                g,
                memory_property_fn,
                &self.device12,
                &back_buffer,
                bb_index as _,
                br::vk::VkExtent2D {
                    width: new_size.0 as _,
                    height: new_size.1 as _,
                },
                br::vk::VK_FORMAT_R8G8B8A8_UNORM,
            ));
        }
        true
    }

    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        peridot::math::Vector2(0, 0)
    }
}
#[cfg(feature = "transparent")]
impl Drop for Presenter {
    fn drop(&mut self) {
        self.present_completion_event
            .wait(windows::Win32::System::Threading::INFINITE);
    }
}
