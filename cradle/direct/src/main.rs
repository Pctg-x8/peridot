#![feature(map_first_last)]

use log::*;

use std::fs::File;
use std::path::PathBuf;
use std::io::Result as IOResult;
use std::rc::Rc;
use bedrock as br;
use peridot::{EngineEvents, FeatureRequests};
use std::os::unix::io::{RawFd, AsRawFd};

mod udev;
mod epoll;
mod kernel_input;
mod input;
mod userlib;

mod drm;
mod gbm;
mod egl;
mod gl;

#[repr(transparent)]
pub struct OwnedFileDescriptor(libc::c_int);
impl Drop for OwnedFileDescriptor {
    fn drop(&mut self) {
        unsafe { libc::close(self.0); }
    }
}

pub struct PlatformAssetLoader { basedir: PathBuf }
impl PlatformAssetLoader
{
    fn new() -> Self
    {
        #[cfg(feature = "UseExternalAssetPath")] let basedir = PathBuf::from(env!("PERIDOT_EXTERNAL_ASSET_PATH"));
        #[cfg(not(feature = "UseExternalAssetPath"))] let basedir =
        {
            let mut binloc = std::env::current_exe().expect("Getting exe directory");
            binloc.pop(); binloc.push("assets"); binloc
        };

        trace!("Using Assets in {}", basedir.display());
        PlatformAssetLoader { basedir }
    }
}
impl peridot::PlatformAssetLoader for PlatformAssetLoader
{
    type Asset = File;
    type StreamingAsset = File;

    fn get(&self, path: &str, ext: &str) -> IOResult<Self::Asset>
    {
        let mut apath = self.basedir.clone();
        apath.push(path.replace(".", "/")); apath.set_extension(ext);
        return File::open(apath);
    }
    fn get_streaming(&self, path: &str, ext: &str) -> IOResult<Self::Asset> { self.get(path, ext) }
}
pub struct WindowHandler {
    device_fd: libc::c_int,
    page_flip_completion_signal: *mut bool
}
#[allow(dead_code)]
pub struct DrmRenderBuffer {
    buffer_object: gbm::BufferObject,
    framebuffer_id: u32,
    render_target_memory: br::DeviceMemory,
    render_target_image: br::Image,
    render_target_view: Rc<br::ImageView>,
    readback_image: br::Image,
    readback_memory: br::DeviceMemory,
    readback_command_pool: br::CommandPool,
    readback_command: br::CommandBuffer,
    width: u32,
    height: u32
}
impl DrmRenderBuffer {
    pub fn sync(&mut self, q: &br::Queue, sem: &br::Semaphore, fence: &br::Fence) {
        // readback then blit to mapped bo memory
        q.submit(&[br::SubmissionBatch {
            command_buffers: std::borrow::Cow::Borrowed(&[self.readback_command]),
            wait_semaphores: std::borrow::Cow::Borrowed(&[(sem, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT)]),
            .. Default::default()
        }], Some(fence)).expect("Failed to send readback command");
        fence.wait_timeout(std::u64::MAX).expect("Failed to wait readback fence");
        fence.reset().expect("Failed to reset readback fence");

        let bomap = self.buffer_object.map(0, 0, self.width, self.height, gbm::BO_TRANSFER_WRITE)
            .expect("Failed to map bo");
        let readbacked = self.readback_memory.map(0 .. self.width as usize * self.height as usize * 4)
            .expect("Failed to map readbacked memory");
        for y in 0 .. self.height as usize {
            let src = unsafe { readbacked.slice::<u8>(y * self.width as usize * 4, self.width as usize * 4) };
            let dst = unsafe { bomap.pointer().add(y * bomap.stride() as usize) as *mut u8 };
            unsafe { core::ptr::copy_nonoverlapping(src.as_ptr(), dst, self.width as usize * 4) };
        }
        unsafe { self.readback_memory.unmap(); }
    }
}
#[allow(dead_code)]
pub struct DrmPresenter {
    gbm_device: gbm::Device,
    buffer_objects: Vec<DrmRenderBuffer>,
    current_backbuffer_index: usize,
    drm_device_fd: libc::c_int,
    connector: drm::mode::ConnectorPtr,
    crtc_id: u32,
    extent: peridot::math::Vector2<usize>,
    rendering_order: br::Semaphore,
    buffer_ready_order: br::Semaphore,
    present_order: br::Semaphore,
    render_completion_await: br::Fence,
    page_flip_completion_signal: *mut bool
}
impl DrmPresenter {
    const BUFFER_COUNT: usize = 2;

    pub fn new(g: &peridot::Graphics, w: &WindowHandler) -> Self {
        use br::MemoryBound;

        let drm_fd = w.device_fd;
        let res = unsafe {
            drm::raw::drmModeGetResources(drm_fd).as_ref()
                .unwrap_or_else(|| panic!("Failed to drmModeGetResources: {}", std::io::Error::last_os_error()))
        };
        println!("Resources: ");
        println!("- MinGeometry: {}x{}", res.min_width, res.min_height);
        println!("- MaxGeometry: {}x{}", res.max_width, res.max_height);
        println!("- fbs: {}", res.count_fbs);
        println!("- crtcs: {}", res.count_crtcs);
        println!("- connectors: {}", res.count_connectors);
        println!("- encoders: {}", res.count_encoders);

        // find connected connector
        let connector = res.connectors().iter()
            .filter_map(|&cid| drm::mode::ConnectorPtr::get(drm_fd, cid))
            .find(|c| c.connection == drm::mode::Connection::Connected)
            .expect("no available connectors");

        // find preferred or highest-resolution mode
        let mode = connector.modes().iter().find(|m| m.is_preferred()).or_else(||
            connector.modes().iter().map(|x| (x, x.hdisplay * x.vdisplay)).max_by_key(|&(_, k)| k).map(|(m, _)| m)
        ).expect("no available modes?");
        println!("selected mode: {}x{}", mode.hdisplay, mode.vdisplay);

        // find encoder
        let encoder = res.encoders().iter()
            .filter_map(|&eid| drm::mode::EncoderPtr::get(drm_fd, eid))
            .find(|e| e.encoder_id == connector.encoder_id);
        
        // find crtc id
        let crtc_id = match encoder {
            Some(e) => e.crtc_id,
            None => res.encoders().iter()
                .filter_map(|&eid| drm::mode::EncoderPtr::get(drm_fd, eid))
                .filter_map(|e|
                    res.crtcs().iter().enumerate()
                        .find(|&(crx, _)| e.has_possible_crtc_index_bit(crx))
                        .map(|(_, &id)| id)
                )
                .next().expect("no available crtc id")
        };

        // Graphics Execution Managerの初期化(GEM_CREATE)がハードによって違うそうでだいぶ難しいので、バッファアロケーションはGBMに任せちゃう
        let gbm_device = gbm::Device::new(drm_fd).expect("Failed to initialize GBM");
        println!("GBM backend: {}", gbm_device.backend_name().to_string_lossy());

        let buffer_objects = (0 .. Self::BUFFER_COUNT)
            .map(|_| {
                let bo = gbm_device.new_bo_with_modifiers(mode.hdisplay as _, mode.vdisplay as _, gbm::BO_FORMAT_XRGB8888, &[0])
                    .or_else(|| gbm_device.new_bo(mode.hdisplay as _, mode.vdisplay as _, gbm::BO_FORMAT_XRGB8888, gbm::BO_USE_SCANOUT | gbm::BO_USE_RENDERING))
                    .expect("Failed to create bo");
                
                // create drm fb
                let bo_format = bo.format();
                let bo_modifier = bo.modifier();
                let mut modifiers = [0u64; gbm::MAX_PLANES];
                let mut handles = [0u32; gbm::MAX_PLANES];
                let mut offsets = [0u32; gbm::MAX_PLANES];
                let mut strides = [0u32; gbm::MAX_PLANES];
                for p in 0 .. bo.plane_count() {
                    modifiers[p] = bo_modifier;
                    handles[p] = bo.handle(Some(p)) as u32;
                    offsets[p] = bo.offset(p);
                    strides[p] = bo.stride(Some(p));
                }
                // 0x01: DRM_MODE_FB_MODIFIERS
                let flags = if modifiers[0] != 0 { 1 << 1 } else { 0 };
                let mut fbid = 0;
                let r = unsafe {
                    drm::raw::drmModeAddFB2WithModifiers(
                        drm_fd, mode.hdisplay as _, mode.vdisplay as _, bo_format,
                        handles.as_ptr(), strides.as_ptr(), offsets.as_ptr(), modifiers.as_ptr(),
                        &mut fbid, flags
                    )
                };
                if r != 0 {
                    let r = unsafe {
                        drm::raw::drmModeAddFB2(
                            drm_fd, mode.hdisplay as _, mode.vdisplay as _, bo_format,
                            handles.as_ptr(), strides.as_ptr(), offsets.as_ptr(), &mut fbid, 0
                        )
                    };
                    if r != 0 {
                        panic!("Failed to add drm fb");
                    }
                }

                // create vk color buffer image
                let render_target_image = br::ImageDesc::new(
                    &br::Extent2D(mode.hdisplay as _, mode.vdisplay as _),
                    br::vk::VK_FORMAT_B8G8R8A8_UNORM,
                    br::ImageUsage::COLOR_ATTACHMENT.transfer_src(),
                    br::ImageLayout::Undefined
                ).create(g).expect("Failed to create color buffer image");
                let req = render_target_image.requirements();
                let render_target_memory = br::DeviceMemory::allocate(
                    g, req.size as _,
                    g.memory_type_index_for(
                        br::MemoryPropertyFlags::DEVICE_LOCAL, req.memoryTypeBits
                    ).expect("no suitable memory for color buffer image")
                ).expect("Failed to allocate backing memory");
                render_target_image.bind(&render_target_memory, 0).expect("Failed to bind image backing store");

                // create readback image
                let readback_image = br::ImageDesc::new(
                    &br::Extent2D(mode.hdisplay as _, mode.vdisplay as _),
                    br::vk::VK_FORMAT_B8G8R8A8_UNORM,
                    br::ImageUsage::TRANSFER_DEST,
                    br::ImageLayout::Preinitialized
                ).use_linear_tiling().create(g).expect("Failed to create readback image");
                let req = readback_image.requirements();
                let readback_memory = br::DeviceMemory::allocate(
                    g, req.size as _,
                    g.memory_type_index_for(
                        br::MemoryPropertyFlags::HOST_VISIBLE.host_cached(), req.memoryTypeBits
                    ).expect("no suitable memory for readback image")
                ).expect("Failed to allocate readback backing memory");
                readback_image.bind(&readback_memory, 0).expect("Failed to bind readback image backing store");

                let readback_command_pool = br::CommandPool::new(g, g.graphics_queue_family_index(), false, false)
                    .expect("Failed to create readback command pool");
                let readback_command = readback_command_pool.alloc(1, true).expect("Failed to alloc readback cb")[0];
                readback_command.begin().expect("Failed to begin recording command")
                    .pipeline_barrier(
                        br::PipelineStageFlags::HOST, br::PipelineStageFlags::TRANSFER, false,
                        &[], &[], &[
                            br::ImageMemoryBarrier::new(
                                &br::ImageSubref::color(&readback_image, 0..1, 0..1),
                                br::ImageLayout::General, br::ImageLayout::TransferDestOpt
                            )
                        ]
                    )
                    .copy_image(
                        &render_target_image, br::ImageLayout::TransferSrcOpt,
                        &readback_image, br::ImageLayout::TransferDestOpt,
                        &[br::vk::VkImageCopy {
                            srcOffset: br::vk::VkOffset3D { x: 0, y: 0, z: 0 },
                            dstOffset: br::vk::VkOffset3D { x: 0, y: 0, z: 0 },
                            srcSubresource: br::vk::VkImageSubresourceLayers {
                                aspectMask: br::vk::VK_IMAGE_ASPECT_COLOR_BIT,
                                mipLevel: 0, baseArrayLayer: 0, layerCount: 1
                            },
                            dstSubresource: br::vk::VkImageSubresourceLayers {
                                aspectMask: br::vk::VK_IMAGE_ASPECT_COLOR_BIT,
                                mipLevel: 0, baseArrayLayer: 0, layerCount: 1
                            },
                            extent: br::vk::VkExtent3D {
                                width: mode.hdisplay as _, height: mode.vdisplay as _, depth: 1
                            }
                        }]
                    )
                    .pipeline_barrier(
                        br::PipelineStageFlags::TRANSFER, br::PipelineStageFlags::HOST, false,
                        &[], &[], &[
                            br::ImageMemoryBarrier::new(
                                &br::ImageSubref::color(&readback_image, 0..1, 0..1),
                                br::ImageLayout::TransferDestOpt, br::ImageLayout::General
                            )
                        ]
                    );

                DrmRenderBuffer {
                    buffer_object: bo,
                    framebuffer_id: fbid,
                    render_target_view: render_target_image.create_view(
                        None, None, &Default::default(), &br::ImageSubresourceRange::color(0 .. 1, 0 .. 1)
                    ).expect("Failed to create image view").into(),
                    render_target_image, render_target_memory,
                    readback_image, readback_memory,
                    readback_command_pool, readback_command,
                    width: mode.hdisplay as _, height: mode.vdisplay as _
                }
            }).collect::<Vec<_>>();

        // modeset!
        let r = unsafe {
            let mut cid = connector.connector_id;
            drm::raw::drmModeSetCrtc(drm_fd, crtc_id, buffer_objects[0].framebuffer_id, 0, 0, &mut cid, 1, mode as *const _ as *mut _)
        };
        if r != 0 { panic!("Failed to drmModeSetCrtc: {}", r); }

        DrmPresenter {
            gbm_device,
            buffer_objects,
            current_backbuffer_index: 0,
            extent: peridot::math::Vector2(mode.hdisplay as _, mode.vdisplay as _),
            drm_device_fd: drm_fd,
            connector,
            crtc_id,
            rendering_order: br::Semaphore::new(g).expect("Failed to create rendering order semaphore"),
            buffer_ready_order: br::Semaphore::new(g).expect("Failed to create buffer ready order semaphore"),
            present_order: br::Semaphore::new(g).expect("Failed to create present order semaphore"),
            render_completion_await: br::Fence::new(g, false).expect("Failed to create render completion awaiter"),
            page_flip_completion_signal: w.page_flip_completion_signal
        }
    }
}
impl peridot::PlatformPresenter for DrmPresenter {
    fn format(&self) -> br::vk::VkFormat { br::vk::VK_FORMAT_B8G8R8A8_UNORM }
    fn backbuffer_count(&self) -> usize { Self::BUFFER_COUNT }
    fn backbuffer(&self, index: usize) -> Option<Rc<br::ImageView>> {
        self.buffer_objects.get(index).map(|o| o.render_target_view.clone())
    }
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        (br::ImageLayout::TransferSrcOpt, br::PipelineStageFlags::TRANSFER)
    }
    
    fn emit_initialize_backbuffer_commands(&self, recorder: &mut br::CmdRecord) {
        let barriers = self.buffer_objects.iter().flat_map(|o| vec![
            br::ImageMemoryBarrier::new(
                &br::ImageSubref::color(&o.render_target_image, 0 .. 1, 0 .. 1),
                br::ImageLayout::Undefined, br::ImageLayout::TransferSrcOpt
            ),
            br::ImageMemoryBarrier::new(
                &br::ImageSubref::color(&o.readback_image, 0 .. 1, 0 .. 1),
                br::ImageLayout::Preinitialized, br::ImageLayout::General
            )
        ]).collect::<Vec<_>>();
        recorder.pipeline_barrier(
            br::PipelineStageFlags::BOTTOM_OF_PIPE, br::PipelineStageFlags::TRANSFER.host(), true,
            &[], &[], &barriers
        );
    }
    fn next_backbuffer_index(&mut self) -> br::Result<u32> {
        // if self.current_backbuffer_index > 0 { return Ok(self.current_backbuffer_index as _); }

        let next = self.current_backbuffer_index;
        self.current_backbuffer_index = (self.current_backbuffer_index + 1) % Self::BUFFER_COUNT;
        Ok(next as _)
    }
    fn render_and_present<'s>(
        &'s mut self,
        g: &peridot::Graphics,
        last_render_fence: &br::Fence,
        present_queue: &br::Queue,
        backbuffer_index: u32,
        mut render_submission: br::SubmissionBatch<'s>,
        update_submission: Option<br::SubmissionBatch<'s>>
    ) -> br::Result<()> {
        if let Some(mut cs) = update_submission {
            // copy -> render
            cs.signal_semaphores.to_mut().push(&self.buffer_ready_order);
            render_submission.wait_semaphores.to_mut().extend(vec![
                // (&self.rendering_order, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT),
                (&self.buffer_ready_order, br::PipelineStageFlags::VERTEX_INPUT)
            ]);
            render_submission.signal_semaphores.to_mut().push(&self.present_order);
            g.submit_buffered_commands(&[cs, render_submission], last_render_fence)
                .expect("Failed to submit render and update commands");
        } else {
            // render only (old logic)
            render_submission.signal_semaphores.to_mut().push(&self.present_order);
            /*render_submission.wait_semaphores.to_mut().push(
                (&self.rendering_order, br::PipelineStageFlags::COLOR_ATTACHMENT_OUTPUT)
            );*/
            g.submit_buffered_commands(&[render_submission], last_render_fence)
                .expect("Failed to submit render commands");
        }
        self.buffer_objects[backbuffer_index as usize].sync(present_queue, &self.present_order, &self.render_completion_await);

        let r = unsafe {
            drm::raw::drmModePageFlip(
                self.drm_device_fd, self.crtc_id,
                self.buffer_objects[backbuffer_index as usize].framebuffer_id, drm::raw::DRM_MODE_PAGE_FLIP_EVENT,
                self.page_flip_completion_signal as _
            )
        };
        if r != 0 { panic!("drmModePageFlip failed: {}", r); }

        Ok(())

        // self.swapchain.get().swapchain.queue_present(q, bb_index, &[&self.present_order])
    }
    fn resize(&mut self, _g: &peridot::Graphics, _new_size: peridot::math::Vector2<usize>) -> bool {
        unimplemented!("no resizing will be occured in direct rendering");
    }

    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> { self.extent.clone() }
}

extern "C" fn page_flip_handler(_: libc::c_int, _: libc::c_uint, _: libc::c_uint, _: libc::c_uint, userdata: *mut libc::c_void) {
    unsafe { *(userdata as *mut bool) = true; }
}

pub struct NativeLink { al: PlatformAssetLoader, wh: WindowHandler }
impl peridot::NativeLinker for NativeLink {
    type AssetLoader = PlatformAssetLoader;
    type Presenter = DrmPresenter;

    fn instance_extensions(&self) -> Vec<&str> { vec!["VK_KHR_surface", "VK_KHR_display", "VK_KHR_get_physical_device_properties2", "VK_KHR_external_memory_capabilities"] }
    fn device_extensions(&self) -> Vec<&str> { vec!["VK_KHR_swapchain", "VK_KHR_external_memory", "VK_KHR_external_memory_fd"] }

    fn asset_loader(&self) -> &PlatformAssetLoader { &self.al }
    fn new_presenter(&self, g: &peridot::Graphics) -> DrmPresenter {
        DrmPresenter::new(g, &self.wh)
    }
}

pub struct GameDriver {
    engine: peridot::Engine<NativeLink>,
    usercode: userlib::Game<NativeLink>
}
impl GameDriver {
    fn new(wh: WindowHandler) -> Self {
        let nl = NativeLink {
            al: PlatformAssetLoader::new(),
            wh
        };
        let mut engine = peridot::Engine::new(
            userlib::Game::<NativeLink>::NAME, userlib::Game::<NativeLink>::VERSION,
            nl, userlib::Game::<NativeLink>::requested_features()
        );
        let usercode = userlib::Game::init(&mut engine);
        engine.input_mut().set_nativelink(Box::new(input::InputNativeLink::new()));
        engine.postinit();

        GameDriver { engine, usercode }
    }

    fn update(&mut self) { self.engine.do_update(&mut self.usercode); }
}

struct ScopeGuard<F: FnMut()>(F);
impl<F: FnMut()> Drop for ScopeGuard<F> {
    fn drop(&mut self) { (self.0)(); }
}

fn main() {
    env_logger::init();

    let egl_device = egl::Device::query().into_iter()
        .find(|d| d.get_string(egl::raw::EGL_EXTENSIONS)
            .and_then(|exts| exts.to_str().expect("invalid sequence").split(' ').find(|&e| e == "EGL_EXT_device_drm"))
            .is_some()
        )
        .expect("no available devices");
    let device_file_name = egl_device.get_string(egl::raw::EGL_DRM_DEVICE_FILE_EXT)
        .expect("no drm device file name");
    println!("device drm file name: {}", device_file_name.to_str().expect("invalid sequence"));
    let device_fd = unsafe { libc::open(device_file_name.as_ptr(), libc::O_RDWR) };
    if device_fd < 0 {
        panic!("Failed to open egl device drm");
    }

    // DisplayやContextを作る前にDRM側の準備をしておく必要がある
    // EGLStreamsを使う場合、以下のClient Capが必要になる
    let res = unsafe { drm::raw::drmSetClientCap(device_fd, drm::raw::DRM_CLIENT_CAP_UNIVERSAL_PLANES, 1) };
    if res != 0 {
        panic!("Unable to set drm client capability");
    }
    
    let res = unsafe {
        drm::raw::drmModeGetResources(device_fd).as_ref()
            .unwrap_or_else(|| panic!("Failed to drmModeGetResources: {}", std::io::Error::last_os_error()))
    };
    println!("Resources: ");
    println!("- MinGeometry: {}x{}", res.min_width, res.min_height);
    println!("- MaxGeometry: {}x{}", res.max_width, res.max_height);
    println!("- fbs: {}", res.count_fbs);
    println!("- crtcs: {}", res.count_crtcs);
    println!("- connectors: {}", res.count_connectors);
    println!("- encoders: {}", res.count_encoders);

    // find connected connector
    let connector = res.connectors().iter()
        .filter_map(|&cid| drm::mode::ConnectorPtr::get(device_fd, cid))
        .find(|c| c.connection == drm::mode::Connection::Connected)
        .expect("no available connectors");
    println!("connector id: {}", connector.connector_id);

    // find preferred or highest-resolution mode
    let mode = connector.modes().iter().find(|m| m.is_preferred()).or_else(||
        connector.modes().iter().map(|x| (x, x.hdisplay * x.vdisplay)).max_by_key(|&(_, k)| k).map(|(m, _)| m)
    ).expect("no available modes?");
    println!("selected mode: {}x{}", mode.hdisplay, mode.vdisplay);

    // find encoder
    let encoder = res.encoders().iter()
        .filter_map(|&eid| drm::mode::EncoderPtr::get(device_fd, eid))
        .find(|e| e.encoder_id == connector.encoder_id);
    
    // find crtc id
    let crtc_id = match encoder {
        Some(e) => e.crtc_id,
        None => res.encoders().iter()
            .filter_map(|&eid| drm::mode::EncoderPtr::get(device_fd, eid))
            .filter_map(|e|
                res.crtcs().iter().enumerate()
                    .find(|&(crx, _)| e.has_possible_crtc_index_bit(crx))
                    .map(|(_, &id)| id)
            )
            .next().expect("no available crtc id")
    };
    let crtc_index = res.crtcs().iter().enumerate()
        .find(|&(_, &cid)| cid == crtc_id)
        .expect("Failed to find crtc index").0;
    println!("crtc id: {}", crtc_id);
    println!("crtc index: {}", crtc_index);

    // find primary plane for crtc
    let plane_id = drm::mode::PlaneResourcesPtr::get(device_fd).expect("Failed to get plane resources")
        .planes()
        .iter()
        .copied()
        .find(|&pid| drm::mode::PlanePtr::get(device_fd, pid).map_or(false, |p| {
            if (p.possible_crtcs & (1 << crtc_index)) == 0 { return false; }
            let props = drm::mode::ObjectProperties::get(device_fd, pid, drm::mode::ObjectType::Plane);
            let (prop_ids, prop_values): (&[u32], &[u64]) = props.as_ref().map_or((&[], &[]), |p| (p.props(), p.prop_values()));
            prop_ids.iter().copied().zip(prop_values.iter().copied())
                .find(|&(propid, _)|
                    drm::mode::Property::get(device_fd, propid)
                        .map_or(false, |p| p.name().to_str().map_or(false, |s| s == "type"))
                )
                .map_or(false, |(_, v)| v == drm::raw::DRM_PLANE_TYPE_PRIMARY)
        }))
        .expect("no suitable primary plane");
    println!("primary plane id: {}", plane_id);

    // create dumb buffer and fb
    let mut dumb_info = drm::raw::drm_mode_create_dumb::new_request(mode.hdisplay as _, mode.vdisplay as _, 32, 0);
    let r = unsafe {
        drm::raw::drmIoctl(device_fd, drm::raw::DRM_IOCTL_MODE_CREATE_DUMB, &mut dumb_info as *mut _ as _)
    };
    if r < 0 {
        panic!("Failed to create dumb buffer: {}", r);
    }
    let mut fb = 0;
    let r = unsafe {
        drm::raw::drmModeAddFB(device_fd, mode.hdisplay as _, mode.vdisplay as _, 24, 32, dumb_info.pitch, dumb_info.handle, &mut fb)
    };
    if r != 0 {
        panic!("Failed to add dumb buffer as FB: {}", r);
    }
    println!("fb: {}", fb);
    
    // 先にmodesetしておく必要がある
    let r = unsafe {
        let mut cid = connector.connector_id;
        drm::raw::drmModeSetCrtc(device_fd, crtc_id, fb, 0, 0, &mut cid, 1, mode as *const _ as *mut _)
    };
    if r != 0 { panic!("Failed to drmModeSetCrtc: {}", r); }

    let egl_display = egl::DisplayRef::from_device(
        &egl_device,
        &[egl::raw::EGL_DRM_MASTER_FD_EXT, device_fd, egl::raw::EGL_NONE as _]
    ).expect("Failed to get display");
    let (major, minor) = egl::initialize(egl_display).expect("Failed to initialize EGL");
    println!("egl version: {}.{}", major, minor);
    println!("egl vendor: {}", egl_display.vendor_name().expect("no vendor name").to_str().expect("invalid sequence"));

    if !egl::bind_api(egl::API::OpenGL) { panic!("Failed to bind api"); }
    let cfg = egl::Config::choose(egl_display, &[
        egl::raw::EGL_SURFACE_TYPE as _, egl::raw::EGL_STREAM_BIT_KHR as _,
        egl::raw::EGL_RENDERABLE_TYPE as _, egl::raw::EGL_OPENGL_BIT as _,
        egl::raw::EGL_RED_SIZE as _, 1,
        egl::raw::EGL_GREEN_SIZE as _, 1,
        egl::raw::EGL_BLUE_SIZE as _, 1,
        egl::raw::EGL_ALPHA_SIZE as _, 0,
        egl::raw::EGL_DEPTH_SIZE as _, 1,
        egl::raw::EGL_NONE as _
    ]).expect("Failed to choose config");
    let ctx = egl::Context::new(egl_display, Some(cfg), None, &[egl::raw::EGL_NONE as _])
        .expect("Failed to create egl context");
    
    let output_layer = egl::OutputLayer::get_first_layer(egl_display, &[
        egl::raw::EGL_DRM_PLANE_EXT as _, plane_id as _, egl::raw::EGL_NONE
    ]).expect("Failed to get first output layer plane");
    let stream = egl::Stream::new(egl_display, &[egl::raw::EGL_NONE as _]).expect("Failed to create stream");
    if !stream.set_consumer_output(output_layer) {
        panic!("Failed to set stream consumer output");
    }
    let surface = egl::Surface::new_stream_producer(egl_display, cfg, &stream, &[
        egl::raw::EGL_WIDTH as _, mode.hdisplay as _,
        egl::raw::EGL_HEIGHT as _, mode.vdisplay as _,
        egl::raw::EGL_NONE as _
    ]).expect("Failed to create producer surface");
    if !ctx.make_current(Some(&surface), Some(&surface)) {
        panic!("MakeCurrent failed");
    }

    let mut page_flip_completion_signal = false;
    let mut gd = GameDriver::new(
        WindowHandler { device_fd, page_flip_completion_signal: &mut page_flip_completion_signal as _ }
    );

    let mut drm_event_context = drm::raw::drmEventContext {
        version: 2,
        vblank_handler: None,
        page_flip_handler: Some(page_flip_handler),
        page_flip_handler2: None,
        sequence_handler: None
    };
    let ep = epoll::Epoll::new().expect("Failed to create epoll interface");
    ep.add_fd(device_fd, libc::EPOLLIN as _, 0).expect("Failed to add drm fd");
    let mut input = input::InputSystem::new(&ep, 1, 2);

    gd.update();
    let mut events = vec![unsafe { std::mem::MaybeUninit::zeroed().assume_init() }; 2 + input.managed_devices_count()];
    'app: loop {
        if events.len() != 2 + input.managed_devices_count() {
            // resize
            events.resize(2 + input.managed_devices_count(), unsafe { std::mem::MaybeUninit::zeroed().assume_init() });
        }

        let count = ep.wait(&mut events, Some(1)).expect("Failed to waiting epoll");
        for e in &events[..count as usize] {
            if e.u64 == 0 {
                unsafe { drm::raw::drmHandleEvent(device_fd, &mut drm_event_context); }
                if page_flip_completion_signal {
                    page_flip_completion_signal = false;
                    gd.update();
                }
            } else if e.u64 == 1 {
                input.process_monitor_event(&ep);
            } else {
                input.process_device_event(gd.engine.input(), e.u64);
            }
        }
    }
    info!("Terminating Program...");
}
