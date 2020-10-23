use winapi::um::winuser::{
    DefWindowProcA, CreateWindowExA, PeekMessageA, DispatchMessageA, TranslateMessage, WNDCLASSEXA, RegisterClassExA,
    AdjustWindowRectEx, WS_OVERLAPPEDWINDOW, WS_EX_APPWINDOW, WS_EX_NOREDIRECTIONBITMAP, CW_USEDEFAULT,
    ShowWindow, SW_SHOWNORMAL, WM_SIZE, PostQuitMessage, PM_REMOVE,
    LoadCursorA, IDC_ARROW, SetWindowLongPtrA, GetWindowLongPtrA, GWLP_USERDATA
};
use winapi::um::shellscalingapi::{SetProcessDpiAwareness, PROCESS_SYSTEM_DPI_AWARE};
use winapi::um::winuser::{WM_DESTROY, WM_QUIT};
use winapi::um::libloaderapi::GetModuleHandleA;
use winapi::um::handleapi::CloseHandle;
use winapi::shared::windef::{RECT, HWND};
use winapi::shared::minwindef::{LRESULT, WPARAM, LPARAM, UINT, HINSTANCE, LOWORD, HIWORD};
use winapi::shared::ntdef::HANDLE;

use std::mem::MaybeUninit;
#[macro_use] extern crate log;
mod userlib;
use peridot::{EngineEvents, FeatureRequests};

const LPSZCLASSNAME: &str = concat!(env!("PERIDOT_WINDOWS_APPID"), ".mainWindow\0");

fn module_handle() -> HINSTANCE { unsafe { GetModuleHandleA(std::ptr::null()) } }

pub struct GameDriver {
    base: peridot::Engine<NativeLink>,
    usercode: userlib::Game<NativeLink>,
    current_size: peridot::math::Vector2<usize>
}
impl GameDriver {
    fn new(window: HWND, init_size: peridot::math::Vector2<usize>) -> Self {
        let nl = NativeLink {
            al: AssetProvider::new(),
            window,
            input: InputHandler::new()
        };
        let mut base = peridot::Engine::new(
            userlib::Game::<NativeLink>::NAME, userlib::Game::<NativeLink>::VERSION,
            nl, userlib::Game::<NativeLink>::requested_features()
        );
        let usercode = userlib::Game::init(&base);
        base.postinit();

        GameDriver { base, usercode, current_size: init_size }
    }

    fn update(&mut self) {
        self.base.do_update(&mut self.usercode);
    }
    fn resize(&mut self, size: peridot::math::Vector2<usize>) {
        self.base.do_resize_backbuffer(size, &mut self.usercode);
    }
}

fn main() {
    env_logger::init();

    unsafe { SetProcessDpiAwareness(PROCESS_SYSTEM_DPI_AWARE); }

    let wca = WNDCLASSEXA {
        cbSize: std::mem::size_of::<WNDCLASSEXA>() as _, hInstance: module_handle(),
        lpszClassName: LPSZCLASSNAME.as_ptr() as *const _,
        lpfnWndProc: Some(window_callback),
        hCursor: unsafe { LoadCursorA(std::ptr::null_mut(), IDC_ARROW as _) },
        .. unsafe { MaybeUninit::zeroed().assume_init() }
    };
    let wcatom = unsafe { RegisterClassExA(&wca) };
    if wcatom <= 0 { panic!("Register Class Failed!"); }

    let wname = format!("{} v{}.{}.{}",
        userlib::Game::<NativeLink>::NAME,
        userlib::Game::<NativeLink>::VERSION.0,
        userlib::Game::<NativeLink>::VERSION.1,
        userlib::Game::<NativeLink>::VERSION.2);
    let wname_c = std::ffi::CString::new(wname).expect("Unable to generate a c-style string");

    let style = WS_OVERLAPPEDWINDOW;
    let mut wrect = RECT { left: 0, top: 0, right: 640, bottom: 480 };
    unsafe { AdjustWindowRectEx(&mut wrect, style, false as _, WS_EX_APPWINDOW); }
    let w = unsafe {
        CreateWindowExA(WS_EX_APPWINDOW | WS_EX_NOREDIRECTIONBITMAP, wcatom as _, wname_c.as_ptr(), style,
            CW_USEDEFAULT, CW_USEDEFAULT, wrect.right - wrect.left, wrect.bottom - wrect.top,
            std::ptr::null_mut(), std::ptr::null_mut(), wca.hInstance, std::ptr::null_mut())
    };
    if w.is_null() { panic!("Create Window Failed!"); }
    
    let mut driver = GameDriver::new(w, peridot::math::Vector2(640, 480));
    unsafe { SetWindowLongPtrA(w, GWLP_USERDATA, &mut driver as *mut GameDriver as _); }
    unsafe { ShowWindow(w, SW_SHOWNORMAL); }

    while process_message_all() { driver.update(); }
}
extern "system" fn window_callback(w: HWND, msg: UINT, wparam: WPARAM, lparam: LPARAM) -> LRESULT {
    match msg {
        WM_DESTROY => unsafe { PostQuitMessage(0); return 0; },
        WM_SIZE => unsafe {
            let p = GetWindowLongPtrA(w, GWLP_USERDATA) as *mut GameDriver;
            if let Some(driver) = p.as_mut() {
                let (w, h) = (LOWORD(lparam as _), HIWORD(lparam as _));
                let size = peridot::math::Vector2(w as usize, h as usize);
                if driver.current_size != size {
                    driver.current_size = size.clone();
                    driver.resize(size);
                    driver.update();
                }
            }
            return 0;
        },
        _ => unsafe { DefWindowProcA(w, msg, wparam, lparam) }
    }
}

fn process_message_all() -> bool {
    let mut msg = MaybeUninit::uninit();
    while unsafe { PeekMessageA(msg.as_mut_ptr(), std::ptr::null_mut(), 0, 0, PM_REMOVE) != 0 } {
        if unsafe { (*msg.as_ptr()).message } == WM_QUIT { return false; }
        unsafe { TranslateMessage(msg.as_mut_ptr()); DispatchMessageA(msg.as_mut_ptr()); }
    }
    
    true
}

use std::rc::Rc;
use bedrock as br;
use std::path::PathBuf;

struct AssetProvider { base: PathBuf }
impl AssetProvider {
    fn new() -> Self {
        #[cfg(feature = "UseExternalAssetPath")] let base = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))] let base = {
            let mut exe = std::env::current_exe().expect("Unable to determine the location of exe file");
            exe.pop(); exe.push("/assets"); exe
        };
        trace!("Asset BaseDirectory={}", base.display());
        AssetProvider { base }
    }
}
impl peridot::PlatformAssetLoader for AssetProvider {
    type Asset = std::fs::File;
    type StreamingAsset = std::fs::File;

    fn get(&self, path: &str, ext: &str) -> std::io::Result<Self::Asset> {
        let mut p = self.base.clone();
        p.push(path.replace('.', "/"));
        p.set_extension(ext);
        return std::fs::File::open(&p);
    }
    fn get_streaming(&self, path: &str, ext: &str) -> std::io::Result<Self::StreamingAsset> {
        let mut p = self.base.clone();
        p.push(path.replace('.', "/"));
        p.set_extension(ext);
        return std::fs::File::open(&p);
    }
}

struct Composition {
    _device: comdrive::dcomp::Device,
    _target: comdrive::dcomp::Target,
    _root: comdrive::dcomp::Visual
}
impl Composition {
    fn new(w: HWND, swapchain: &comdrive::dxgi::SwapChain) -> Self {
        use comdrive::dcomp::TargetProvider;

        let device = comdrive::dcomp::Device::new(None).expect("Failed to create DirectComposition Device");
        let target = device.new_target_for(&w).expect("Failed to create DirectComposition Target");
        let root = device.new_visual().expect("Failed to create DirectComposition Visual");

        root.set_content(Some(swapchain)).expect("Failed to set Swapchain for Composition");
        target.set_root(&root).expect("Failed to set Composition Root Visual");
        device.commit().expect("Failed to commit composition");

        Composition { _device: device, _target: target, _root: root }
    }
}

#[repr(transparent)]
struct OwnedHandle(HANDLE);
impl Drop for OwnedHandle {
    fn drop(&mut self) { unsafe { CloseHandle(self.0); } }
}
impl From<HANDLE> for OwnedHandle { fn from(h: HANDLE) -> Self { OwnedHandle(h) } }
impl OwnedHandle {
    pub fn handle(&self) -> HANDLE { self.0 }
}

struct InteropBackbufferResource {
    _shared_handle: OwnedHandle,
    _image: peridot::Image,
    image_view: Rc<br::ImageView>
}
impl InteropBackbufferResource {
    pub fn new(
        g: &peridot::Graphics,
        device: &comdrive::d3d12::Device,
        resource: &comdrive::d3d12::Resource,
        name_suffix: u32,
        size: &br::Extent2D,
        format: br::vk::VkFormat,
    ) -> Self {
        use br::{Chainable, MemoryBound};

        let hname = widestring::WideCString::from_str(format!("LocalPeridotApiInteropHandleCradle{}", name_suffix))
            .expect("Failed to encode to WideString");
        let shared_handle = OwnedHandle(
            device.create_shared_handle(resource, None, &hname).expect("Failed to create SharedHandle from D3D12")
        );
        let image_ext = br::ExternalMemoryImageCreateInfo::new(br::ExternalMemoryHandleTypes::D3D12_RESOURCE);
        let image = br::ImageDesc::new(size, format, br::ImageUsage::COLOR_ATTACHMENT, br::ImageLayout::Preinitialized)
            .chain(&image_ext)
            .create(&g).expect("Failed to create Interop Image");
        let image_mreq = image.requirements();
        let handle_import_props = g.get_memory_win32_handle_properties(
            br::ExternalMemoryHandleTypeWin32::D3D12Resource, shared_handle.handle()
        ).expect("Failed to query Handle Memory Properties");
        let memory = br::DeviceMemory::import_win32(
            &g,
            image_mreq.size as _,
            g.memory_type_index_for(
                br::MemoryPropertyFlags::DEVICE_LOCAL, image_mreq.memoryTypeBits & handle_import_props.memoryTypeBits
            ).expect("Failed to find matching memory type for importing"),
            br::ExternalMemoryHandleTypeWin32::D3D12Resource,
            shared_handle.handle(),
            &hname
        ).expect("Failed to import External Memory from D3D12Resource").into();
        let image = peridot::Image::bound(image, &memory, 0).expect("Failed to bind image backing memory");
        let image_view = image.create_view(
            None, None, &Default::default(), &br::ImageSubresourceRange::color(0 .. 1, 0 .. 1)
        ).expect("Failed to create ImageView for Rendering").into();

        InteropBackbufferResource {
            _shared_handle: shared_handle, _image: image, image_view
        }
    }
}

struct Presenter {
    _window: HWND,
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
    _render_completion_fence_handle: OwnedHandle,
    present_completion_event: OwnedHandle
}
impl Presenter {
    fn new(g: &peridot::Graphics, window: HWND) -> Self {
        let mut rc = std::mem::MaybeUninit::uninit();
        let rc = unsafe {
            winapi::um::winuser::GetClientRect(window, rc.as_mut_ptr());
            rc.assume_init()
        };

        let factory = comdrive::dxgi::Factory::new(cfg!(debug_assertions)).expect("Failed to create DXGI Factory");
        let adapter = factory.adapter(0).expect("Failed to query primary adapter");
        
        if cfg!(debug_assertions) {
            comdrive::d3d12::Device::enable_debug_layer().expect("Failed to enable D3D12 Debug Layer");
        }
        let device12 = comdrive::d3d12::Device::new(&adapter, comdrive::d3d::FeatureLevel::v11)
            .expect("Failed to create Direct3D12 Device");
        let q = device12.new_command_queue(comdrive::d3d12::CommandType::Direct, 0)
            .expect("Failed to create Primary CommandQueue");
        let sc = factory.new_swapchain(
            &q,
            metrics::Size2U((rc.right - rc.left) as _, (rc.bottom - rc.top) as _),
            comdrive::dxgi::DXGI_FORMAT_R8G8B8A8_UNORM,
            comdrive::dxgi::AlphaMode::Premultiplied,
            2,
            false
        ).expect("Failed to create SwapChain");
        let comp = Composition::new(window, &sc);
        let bb_size = br::Extent2D((rc.right - rc.left) as _, (rc.bottom - rc.top) as _);
        let backbuffers = (0 .. 2).map(|bb_index| {
            let backbuffer = sc.back_buffer(bb_index).expect("Failed to get Backbuffer from Swapchain");

            InteropBackbufferResource::new(
                g, &device12, &backbuffer, bb_index as _, &bb_size, br::vk::VK_FORMAT_R8G8B8A8_UNORM
            )
        }).collect();

        let buffer_ready_order = br::Semaphore::new(g).expect("Failed to create Buffer Ready Semaphore");
        let present_order = br::Semaphore::new(g).expect("Failed to create Present Order Semaphore");
        let render_completion_fence = device12.new_fence(0, comdrive::d3d12::FENCE_FLAG_SHARED)
            .expect("Failed to create Render Completion Fence");
        let present_completion_fence = device12.new_fence(0, comdrive::d3d12::FENCE_FLAG_NONE)
            .expect("Failed to create Present Completion Fence");
        let render_completion_fence_name = widestring::WideCString::from_str("LocalRenderCompletionFenceShared")
            .expect("Failed to encode widestring");
        let render_completion_fence_handle = OwnedHandle(
            device12.create_shared_handle(
                &render_completion_fence, None, &render_completion_fence_name
            ).expect("Failed to create Shared Handle for Render Completion Fence")
        );
        g.import_semaphore_win32_handle(
            &present_order,
            br::ExternalSemaphoreHandleWin32::D3DFence(render_completion_fence_handle.handle()),
            &render_completion_fence_name
        ).expect("Failed to import Render Completion Fence");
        let pce = unsafe {
            winapi::um::synchapi::CreateEventA(
                std::ptr::null_mut(), false as _, true as _, b"LocalPeridotPresentNotification".as_ptr() as _
            )
        };
        if pce.is_null() {
            Result::<(), _>::Err(std::io::Error::last_os_error()).expect("Failed to create Present Completion Event");
        }
        let present_completion_event = OwnedHandle(pce);

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
            present_completion_event
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::vk::VkFormat { br::vk::VK_FORMAT_R8G8B8A8_UNORM }
    fn backbuffer_count(&self) -> usize { 2 }
    fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> {
        self.backbuffers.get(index).map(|b| b.image_view.clone())
    }

    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        let barriers = self.backbuffers.iter().map(|b| br::ImageMemoryBarrier::new(
            &br::ImageSubref::color(&b.image_view, 0 .. 1, 0 .. 1),
            br::ImageLayout::Preinitialized, br::ImageLayout::General
        )).collect::<Vec<_>>();

        recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE, br::PipelineStageFlags::TOP_OF_PIPE, true,
            &[], &[], &barriers
        );
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> { Ok(self.sc.current_back_buffer_index()) }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (br::ImageLayout::General, br::PipelineStageFlags::TOP_OF_PIPE)
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &peridot::Graphics,
        last_render_fence: &br::Fence,
        _present_queue: &br::Queue,
        _backbuffer_index: u32,
        mut render_submission: br::SubmissionBatch<'s>,
        update_submission: Option<br::SubmissionBatch<'s>>
    ) -> br::Result<()> {
        use br::VkHandle;

        let signal_counters = [self.render_completion_counter + 1];
        let signal_info = br::vk::VkD3D12FenceSubmitInfoKHR::from(br::D3D12FenceSubmitInfo::new(&[], &signal_counters));
        if let Some(mut cs) = update_submission {
            // copy -> render
            cs.signal_semaphores.to_mut().push(&self.buffer_ready_order);
            render_submission.wait_semaphores.to_mut().extend(vec![
                (&self.buffer_ready_order, br::PipelineStageFlags::VERTEX_INPUT)
            ]);
            render_submission.signal_semaphores.to_mut().push(&self.present_order);

            let (render_wait_semaphores, render_wait_stages): (Vec<_>, Vec<_>) = render_submission.wait_semaphores
                .iter()
                .map(|(s, st)| (s.native_ptr(), st.0))
                .unzip();
            let render_signal_semaphores = render_submission.signal_semaphores.iter().map(|s| s.native_ptr())
                .collect::<Vec<_>>();
            let render_buffers = render_submission.command_buffers.iter().map(|s| s.native_ptr()).collect::<Vec<_>>();
            let (update_wait_semaphores, update_wait_stages): (Vec<_>, Vec<_>) = cs.wait_semaphores
                .iter()
                .map(|(s, st)| (s.native_ptr(), st.0))
                .unzip();
            let update_signal_semaphores = cs.signal_semaphores.iter().map(|s| s.native_ptr()).collect::<Vec<_>>();
            let update_buffers = cs.command_buffers.iter().map(|s| s.native_ptr()).collect::<Vec<_>>();
            let render_submission = br::vk::VkSubmitInfo {
                pNext: &signal_info as *const _ as _,
                waitSemaphoreCount: render_wait_semaphores.len() as _,
                pWaitSemaphores: render_wait_semaphores.as_ptr(),
                pWaitDstStageMask: render_wait_stages.as_ptr(),
                signalSemaphoreCount: render_signal_semaphores.len() as _,
                pSignalSemaphores: render_signal_semaphores.as_ptr(),
                commandBufferCount: render_buffers.len() as _,
                pCommandBuffers: render_buffers.as_ptr(),
                .. Default::default()
            };
            let update_submission = br::vk::VkSubmitInfo {
                waitSemaphoreCount: update_wait_semaphores.len() as _,
                pWaitSemaphores: update_wait_semaphores.as_ptr(),
                pWaitDstStageMask: update_wait_stages.as_ptr(),
                signalSemaphoreCount: update_signal_semaphores.len() as _,
                pSignalSemaphores: update_signal_semaphores.as_ptr(),
                commandBufferCount: update_buffers.len() as _,
                pCommandBuffers: update_buffers.as_ptr(),
                .. Default::default()
            };

            g.submit_buffered_commands_raw(&[update_submission, render_submission], last_render_fence)
                .expect("Failed to submit render and update commands");
        } else {
            // render only (old logic)
            render_submission.signal_semaphores.to_mut().push(&self.present_order);

            let (render_wait_semaphores, render_wait_stages): (Vec<_>, Vec<_>) = render_submission.wait_semaphores
                .iter()
                .map(|(s, st)| (s.native_ptr(), st.0))
                .unzip();
            let render_signal_semaphores = render_submission.signal_semaphores.iter().map(|s| s.native_ptr())
                .collect::<Vec<_>>();
            let render_buffers = render_submission.command_buffers.iter().map(|s| s.native_ptr()).collect::<Vec<_>>();
            let render_submission = br::vk::VkSubmitInfo {
                pNext: &signal_info as *const _ as _,
                waitSemaphoreCount: render_wait_semaphores.len() as _,
                pWaitSemaphores: render_wait_semaphores.as_ptr(),
                pWaitDstStageMask: render_wait_stages.as_ptr(),
                signalSemaphoreCount: render_signal_semaphores.len() as _,
                pSignalSemaphores: render_signal_semaphores.as_ptr(),
                commandBufferCount: render_buffers.len() as _,
                pCommandBuffers: render_buffers.as_ptr(),
                .. Default::default()
            };

            g.submit_buffered_commands_raw(&[render_submission], last_render_fence)
                .expect("Failed to submit render commands");
        }

        unsafe {
            winapi::um::synchapi::WaitForSingleObject(
                self.present_completion_event.handle(), winapi::um::winbase::INFINITE
            );
        }

        self.render_completion_counter += 1;
        self.q.wait(&self.render_completion_fence, self.render_completion_counter)
            .expect("Failed to wait Render Completion Fence");
        self.sc.present().expect("Failed to present");
        self.q.signal(&self.present_completion_fence, self.present_completion_counter + 1)
            .expect("Failed to signal Render Completion Fence");
        self.present_completion_counter += 1;
        self.present_completion_fence.set_event_notification(
            self.present_completion_counter, self.present_completion_event.handle()
        ).expect("Failed to set Completion Event");

        Ok(())
    }
    /// Returns whether re-initializing is needed for backbuffer resources
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        self.backbuffers.clear();
        self.sc.resize(metrics::Size2U(new_size.0 as _, new_size.1 as _)).expect("Failed to resize backbuffers");
        for bb_index in 0 .. 2 {
            let backbuffer = self.sc.back_buffer(bb_index).expect("Failed to get Backbuffer from Swapchain");

            self.backbuffers.push(
                InteropBackbufferResource::new(
                    g, &self.device12, &backbuffer, bb_index as _, &br::Extent2D(new_size.0 as _, new_size.1 as _),
                    br::vk::VK_FORMAT_R8G8B8A8_UNORM
                )
            );
        }
        true
    }
    // unimplemented?
    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> { peridot::math::Vector2(0, 0) }
}
impl Drop for Presenter {
    fn drop(&mut self) {
        unsafe {
            winapi::um::synchapi::WaitForSingleObject(
                self.present_completion_event.handle(), winapi::um::winbase::INFINITE
            );
        }
    }
}

struct InputHandler(Option<Rc<peridot::InputProcess>>);
impl InputHandler {
    fn new() -> Self {
        InputHandler(None)
    }
}
impl peridot::InputProcessPlugin for InputHandler {
    fn on_start_handle(&mut self, processor: &Rc<peridot::InputProcess>)
    {
        self.0 = Some(processor.clone());
    }
}

struct NativeLink {
    al: AssetProvider, window: HWND, input: InputHandler
}
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = AssetProvider;
    type Presenter = Presenter;
    type InputProcessor = InputHandler;

    fn instance_extensions(&self) -> Vec<&str> { vec![] }
    fn device_extensions(&self) -> Vec<&str> { vec!["VK_KHR_external_memory_win32", "VK_KHR_external_semaphore_win32"] }

    fn asset_loader(&self) -> &AssetProvider { &self.al }
    fn new_presenter(&self, g: &peridot::Graphics) -> Presenter { Presenter::new(g, self.window) }
    fn input_processor_mut(&mut self) -> &mut InputHandler { &mut self.input }
}
