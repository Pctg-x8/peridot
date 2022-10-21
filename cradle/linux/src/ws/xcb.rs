//! XCB implementation for WindowSystem Backend

use std::os::fd::{AsRawFd, RawFd};

use bedrock as br;

use super::WindowSystemBackend;

#[allow(dead_code)]
pub struct X11 {
    con: xcb::Connection,
    wm_protocols: xcb::Atom,
    wm_delete_window: xcb::Atom,
    vis: xcb::Visualid,
    mainwnd_id: xcb::Window,
    cached_window_size: peridot::math::Vector2<usize>,
}
impl super::WindowSystemBackend for X11 {
    fn init() -> Self {
        let (con, screen_index) = xcb::Connection::connect(None).expect("Connecting with xcb");
        let s0 = con
            .get_setup()
            .roots()
            .nth(screen_index as _)
            .expect("No screen");
        let vis = s0.root_visual();

        let wm_protocols = xcb::intern_atom_unchecked(&con, false, "WM_PROTOCOLS");
        let wm_delete_window = xcb::intern_atom_unchecked(&con, false, "WM_DELETE_WINDOW");
        con.flush();
        let wm_protocols = wm_protocols.get_reply().expect("No WM_PROTOCOLS").atom();
        let wm_delete_window = wm_delete_window
            .get_reply()
            .expect("No WM_DELETE_WINDOW")
            .atom();

        let mainwnd_id = con.generate_id();
        xcb::create_window(
            &con,
            s0.root_depth(),
            mainwnd_id,
            s0.root(),
            0,
            0,
            640,
            480,
            0,
            xcb::WINDOW_CLASS_INPUT_OUTPUT as _,
            vis,
            &[(xcb::CW_EVENT_MASK, xcb::EVENT_MASK_RESIZE_REDIRECT)],
        );
        xcb::change_property(
            &con,
            xcb::PROP_MODE_REPLACE as _,
            mainwnd_id,
            xcb::ATOM_WM_NAME,
            xcb::ATOM_STRING,
            8,
            crate::userlib::APP_TITLE.as_bytes(),
        );
        xcb::change_property(
            &con,
            xcb::PROP_MODE_APPEND as _,
            mainwnd_id,
            wm_protocols,
            xcb::ATOM_ATOM,
            32,
            &[wm_delete_window],
        );
        con.flush();

        Self {
            con,
            wm_protocols,
            wm_delete_window,
            vis,
            mainwnd_id,
            cached_window_size: peridot::math::Vector2(640, 480),
        }
    }

    fn fd(&self) -> RawFd {
        self.con.as_raw_fd()
    }

    fn flush(&self) {
        if !self.con.flush() {
            panic!("Failed to flush");
        }
    }

    fn show(&self) {
        xcb::map_window(&self.con, self.mainwnd_id);
        self.con.flush();
    }

    /// Returns false if application has beed exited
    fn process_all_events(&mut self) -> bool {
        while let Some(ev) = self.con.poll_for_event() {
            let event_type = ev.response_type() & 0x7f;
            if event_type == xcb::CLIENT_MESSAGE {
                let e: &xcb::ClientMessageEvent = unsafe { xcb::cast_event(&ev) };
                if e.data().data32()[0] == self.wm_delete_window {
                    return false;
                }
            } else if event_type == xcb::RESIZE_REQUEST {
                let e: &xcb::ResizeRequestEvent = unsafe { xcb::cast_event(&ev) };
                self.cached_window_size = peridot::math::Vector2(e.width() as _, e.height() as _);
            } else {
                debug!("Generic Event: {:?}", ev.response_type());
            }
        }
        return true;
    }

    fn mainwnd_geometry(&self) -> &peridot::math::Vector2<usize> {
        &self.cached_window_size
    }
}
impl super::VulkanPresentable for X11 {
    const REQUIRED_INSTANCE_EXTENSION: &'static str = "VK_KHR_xcb_surface";

    fn presentation_support(
        &self,
        adapter: &impl br::PhysicalDevice,
        render_queue_family: u32,
    ) -> bool {
        adapter.xcb_presentation_support(render_queue_family, self.con.get_raw_conn(), self.vis)
    }

    fn create_surface<
        PhysicalDevice: br::PhysicalDevice + br::InstanceChild + br::InstanceChildTransferrable,
    >(
        &self,
        adapter: PhysicalDevice,
    ) -> br::Result<br::SurfaceObject<PhysicalDevice::ConcreteInstance>> {
        adapter.new_surface_xcb(self.con.get_raw_conn(), self.mainwnd_id)
    }
}
impl super::InputSystemBackend for X11 {
    fn get_pointer_position(&self) -> Option<(f32, f32)> {
        let ptrinfo = {
            let ck = xcb::query_pointer(&self.con, self.mainwnd_id);
            self.flush();
            ck.get_reply().expect("Failed to query pointer to xcb")
        };

        if ptrinfo.same_screen() {
            // Note: なぜかLinux/XCBでも5.0だけずれるんですけど！！
            Some((ptrinfo.win_x() as _, ptrinfo.win_y() as f32 - 5.0))
        } else {
            debug!("Fixme: Handle same_screen = false");
            None
        }
    }

    fn is_focused(&self) -> bool {
        let focus_ck = xcb::get_input_focus(&self.con);
        self.flush();
        let focus = focus_ck
            .get_reply()
            .expect("Failed to query focus info")
            .focus();

        focus == self.mainwnd_id
    }

    fn query_states_batched(&self) -> (bool, (i16, i16)) {
        let focus_ck = xcb::get_input_focus(&self.con);
        let qp_cookie = xcb::query_pointer(&self.con, self.mainwnd_id);
        self.flush();
        let ptr = qp_cookie.get_reply().expect("Failed to query pointer");
        let focus = focus_ck
            .get_reply()
            .expect("Failed to query focus info")
            .focus();

        (focus == self.mainwnd_id, (ptr.win_x(), ptr.win_y()))
    }
}
