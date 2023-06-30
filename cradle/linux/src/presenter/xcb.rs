use std::os::fd::{AsRawFd, RawFd};

use br::PhysicalDevice;
use peridot::mthelper::{DynamicMutabilityProvider, SharedMutableRef, SharedRef};
use wayland_backend::io_lifetimes::BorrowedFd;
use xcb::XidNew;

use crate::{input::PointerPositionProvider, userlib};

use super::{BorrowFd, EventProcessor, PresenterProvider, WindowBackend};
use bedrock as br;

pub struct X11ReadinessGuard {
    con_fd: RawFd,
}
impl BorrowFd for X11ReadinessGuard {
    fn borrow_fd<'fd>(&'fd self) -> std::os::fd::BorrowedFd<'fd> {
        unsafe { BorrowedFd::borrow_raw(self.con_fd) }
    }
}

#[allow(dead_code)]
pub struct X11 {
    con: xcb::Connection,
    wm_protocols: xcb::x::Atom,
    wm_delete_window: xcb::x::Atom,
    vis: xcb::x::Visualid,
    mainwnd_id: xcb::x::Window,
    cached_window_size: peridot::math::Vector2<usize>,
    has_close_requested: bool,
}
impl X11 {
    pub fn try_init() -> Option<Self> {
        let Ok((con, screen_index)) = xcb::Connection::connect(None) else {
            return None;
        };

        let s0 = con
            .get_setup()
            .roots()
            .nth(screen_index as _)
            .expect("No screen");
        let vis = s0.root_visual();

        let wm_protocols = con.send_request(&xcb::x::InternAtom {
            only_if_exists: false,
            name: b"WM_PROTOCOLS\0",
        });
        let wm_delete_window = con.send_request(&xcb::x::InternAtom {
            only_if_exists: false,
            name: b"WM_DELETE_WINDOW\0",
        });
        con.flush().expect("Failed to flush");
        let wm_protocols = con
            .wait_for_reply(wm_protocols)
            .expect("No WM_PROTOCOLS")
            .atom();
        let wm_delete_window = con
            .wait_for_reply(wm_delete_window)
            .expect("No WM_DELETE_WINDOW")
            .atom();

        let mainwnd_id = con.generate_id();
        con.send_request(&xcb::x::CreateWindow {
            wid: mainwnd_id,
            parent: s0.root(),
            x: 0,
            y: 0,
            width: 640,
            height: 480,
            border_width: 0,
            class: xcb::x::WindowClass::InputOutput,
            depth: s0.root_depth(),
            visual: vis,
            value_list: &[xcb::x::Cw::EventMask(xcb::x::EventMask::RESIZE_REDIRECT)],
        });
        con.send_request(&xcb::x::ChangeProperty {
            mode: xcb::x::PropMode::Replace,
            window: mainwnd_id,
            property: xcb::x::ATOM_WM_NAME,
            r#type: xcb::x::ATOM_STRING,
            data: userlib::APP_TITLE.as_bytes(),
        });
        con.send_request(&xcb::x::ChangeProperty {
            mode: xcb::x::PropMode::Append,
            window: mainwnd_id,
            property: wm_protocols,
            r#type: xcb::x::ATOM_ATOM,
            data: &[wm_delete_window],
        });
        con.flush().expect("Failed to flush");

        Some(Self {
            con,
            wm_protocols,
            wm_delete_window,
            vis,
            mainwnd_id,
            cached_window_size: peridot::math::Vector2(640, 480),
            has_close_requested: false,
        })
    }

    pub fn fd(&self) -> RawFd {
        self.con.as_raw_fd()
    }

    pub fn flush(&self) {
        self.con.flush().expect("Failed to flush");
    }

    fn mainwnd_geometry(&self) -> &peridot::math::Vector2<usize> {
        &self.cached_window_size
    }
}
impl PresenterProvider for SharedMutableRef<X11> {
    type Presenter = Presenter;
    const SURFACE_EXT_NAME: &'static str = "VK_KHR_xcb_surface";

    fn create(&self, g: &peridot::Graphics) -> Self::Presenter {
        Presenter::new(g, g.graphics_queue_family_index(), self)
    }
}
impl PointerPositionProvider for X11 {
    fn get_pointer_position(&self) -> Option<(f32, f32)> {
        let ptrinfo = {
            let ck = self.con.send_request(&xcb::x::QueryPointer {
                window: self.mainwnd_id,
            });
            self.flush();
            self.con
                .wait_for_reply(ck)
                .expect("Failed to query pointer to xcb")
        };

        if ptrinfo.same_screen() {
            // Note: なぜかLinux/XCBでも5.0だけずれるんですけど！！
            Some((ptrinfo.win_x() as _, ptrinfo.win_y() as f32 - 5.0))
        } else {
            debug!("Fixme: Handle same_screen = false");
            None
        }
    }

    fn query_input_focus(&self) -> bool {
        let focus_ck = self.con.send_request(&xcb::x::GetInputFocus {});
        self.flush();
        let focus = self
            .con
            .wait_for_reply(focus_ck)
            .expect("Failed to query focus info")
            .focus();

        focus == self.mainwnd_id
    }

    fn query_input_focus_and_pointer_entered(&self) -> (bool, bool) {
        let focus_ck = self.con.send_request(&xcb::x::GetInputFocus {});
        let qp_cookie = self.con.send_request(&xcb::x::QueryPointer {
            window: self.mainwnd_id,
        });
        self.flush();
        let ptr = self
            .con
            .wait_for_reply(qp_cookie)
            .expect("Failed to query pointer");
        let geometry = self.mainwnd_geometry();
        let focus = self
            .con
            .wait_for_reply(focus_ck)
            .expect("Failed to query focus info")
            .focus();

        let in_geometry = (0..=geometry.0 as i16).contains(&ptr.win_x())
            && (0..=geometry.1 as i16).contains(&ptr.win_y());
        (focus == self.mainwnd_id, in_geometry)
    }
}
impl EventProcessor for X11 {
    type ReadinessGuard = X11ReadinessGuard;

    fn readiness_guard(&mut self) -> Self::ReadinessGuard {
        X11ReadinessGuard {
            con_fd: self.con.as_raw_fd(),
        }
    }

    fn process_all_events(&mut self, _: Self::ReadinessGuard) {
        while let Some(ev) = self
            .con
            .poll_for_event()
            .expect("Failed to poll window system events")
        {
            match ev {
                xcb::Event::X(xcb::x::Event::ClientMessage(e)) => match e.data() {
                    xcb::x::ClientMessageData::Data32(d)
                        if unsafe { xcb::x::Atom::new(d[0]) } == self.wm_delete_window =>
                    {
                        self.has_close_requested = true;
                    }
                    _ => {}
                },
                xcb::Event::X(xcb::x::Event::ResizeRequest(e)) => {
                    self.cached_window_size =
                        peridot::math::Vector2(e.width() as _, e.height() as _);
                }
                _ => {
                    debug!("Unhandled Event: {ev:?}");
                }
            }
        }
    }

    fn has_close_requested(&self) -> bool {
        self.has_close_requested
    }
}
impl WindowBackend for X11 {
    fn show(&mut self) {
        self.con.send_request(&xcb::x::MapWindow {
            window: self.mainwnd_id,
        });
        self.con.flush().expect("Failed to flush");
    }

    fn geometry(&self) -> peridot::math::Vector2<usize> {
        self.cached_window_size
    }
}

pub struct Presenter {
    x11_ref: SharedMutableRef<X11>,
    sc: peridot::IntegratedSwapchain<br::SurfaceObject<peridot::InstanceObject>>,
}
impl Presenter {
    fn new(g: &peridot::Graphics, renderer_queue_family: u32, w: &SharedMutableRef<X11>) -> Self {
        let wlock = w.borrow();

        if !g.adapter().xcb_presentation_support(
            renderer_queue_family,
            wlock.con.get_raw_conn(),
            wlock.vis,
        ) {
            panic!("Vulkan Presentation is not supported!");
        }
        let so = g
            .adapter()
            .new_surface_xcb(wlock.con.get_raw_conn(), wlock.mainwnd_id)
            .expect("Failed to create Surface object");
        if !g
            .adapter()
            .surface_support(renderer_queue_family, &so)
            .expect("Failed to query surface support")
        {
            panic!("Vulkan Surface is not supported");
        }
        let sc = peridot::IntegratedSwapchain::new(g, so, peridot::math::Vector2(120, 120));
        drop(wlock);

        Self {
            sc,
            x11_ref: w.clone(),
        }
    }
}
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
    fn requesting_backbuffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_backbuffer_layout()
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
    fn resize(&mut self, g: &peridot::Graphics, new_size: peridot::math::Vector2<usize>) -> bool {
        self.sc.resize(g, new_size);
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }

    fn current_geometry_extent(&self) -> peridot::math::Vector2<usize> {
        self.x11_ref.borrow().mainwnd_geometry().clone()
    }
}
