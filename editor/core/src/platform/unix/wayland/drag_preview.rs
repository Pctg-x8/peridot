use peridot_tp_wayland as wl;

use crate::{
    platform::unix::wayland::event_trace,
    utils::{LogicalUnit, Point, Rect, Size},
};

pub struct Controller {
    buf: Buffer,
    dnd_icon: Option<DndIcon>,
    surface: Option<MappedSurface>,
    surface_rect: Rect<LogicalUnit>,
}
impl Controller {
    pub fn new(
        display_server: &super::DisplayServerContext,
        static_pixbufs: &super::StaticPixbufs,
    ) -> Self {
        Self {
            buf: static_pixbufs.create_drag_preview_popover_bufs(&display_server.global_interfaces),
            dnd_icon: None,
            surface: None,
            surface_rect: Rect::from_lt_size(
                Point::new_logical(0.0, 0.0),
                Size::new_logical(100.0, 100.0),
            ),
        }
    }

    pub fn setup_dnd_icon_surface(
        &mut self,
        interfaces: &super::GlobalInterfaces,
        offset: Point<LogicalUnit>,
        size: &Size<LogicalUnit>,
    ) {
        let surface = interfaces
            .compositor
            .create_surface()
            .expect("surface.create");
        let viewport = interfaces
            .viewporter
            .get_viewport(&surface)
            .expect("surface.get_viewport");

        viewport
            .set_source(
                wl::Fixed::ZERO,
                wl::Fixed::ZERO,
                wl::Fixed::ONE,
                wl::Fixed::ONE,
            )
            .expect("viewport.set_source");
        viewport
            .set_destination(size.width.ceil() as _, size.height.ceil() as _)
            .expect("viewport.set_destination");

        let blur = if let Some(ref bm) = interfaces.kde_blur_manager {
            let blur = bm.create(&surface).expect("blur.create");
            blur.commit().expect("blur.commit");

            Some(blur)
        } else {
            None
        };

        self.dnd_icon = Some(DndIcon {
            shown: false,
            offset,
            blur,
            _viewport: viewport,
            surface,
        });
    }

    pub fn post_commit_dnd_icon(&self) {
        let Some(ref x) = self.dnd_icon else {
            return;
        };

        x.surface.commit().expect("surface.commit");
    }

    pub fn show_dnd_icon(&mut self) {
        let Some(ref mut x) = self.dnd_icon else {
            return;
        };

        if x.shown {
            return;
        }

        x.surface
            .attach(Some(&self.buf.buffer()), 0, 0)
            .expect("surface.attach");
        x.surface
            .offset(x.offset.x.round() as _, x.offset.y.round() as _)
            .expect("surface.offset");
        x.surface.damage(0, 0, -1, -1).expect("surface.damage");
        x.surface.commit().expect("surface.commit");
        if let Some(ref b) = x.blur {
            b.commit().expect("blur.commit");
        }
        x.shown = true;
    }

    pub fn dnd_icon_surface(&self) -> &wl::Surface {
        &self.dnd_icon.as_ref().expect("no dnd icon setup").surface
    }

    pub fn hide_dnd_icon(&mut self) {
        let Some(ref mut x) = self.dnd_icon else {
            return;
        };

        if !x.shown {
            return;
        }

        // buffer detach前にleft-topを0, 0にもどす そうしないとだんだんズレていく
        x.surface
            .offset(-x.offset.x.round() as _, -x.offset.y.round() as _)
            .expect("surface.offset");
        x.surface.attach(None, 0, 0).expect("surface.attach");
        x.surface.damage(0, 0, -1, -1).expect("surface.damage");
        x.surface.commit().expect("surface.commit");
        x.shown = false;
    }

    pub fn show_surface(
        &mut self,
        interfaces: &super::GlobalInterfaces,
        on_surface: &wl::XdgSurface,
    ) {
        let surface = interfaces
            .compositor
            .create_surface()
            .expect("wl_popup_surface.create");
        let mut xdg_surface = interfaces
            .xdg_wm_base
            .get_xdg_surface(&surface)
            .expect("xdg_popup_surface.create");
        let viewport = interfaces
            .viewporter
            .get_viewport(&surface)
            .expect("popup_viewport.create");

        let positioner = interfaces
            .xdg_wm_base
            .create_positioner()
            .expect("pos.create");
        positioner
            .set_size(self.surface_rect.width as _, self.surface_rect.height as _)
            .expect("pos.set_size");
        positioner
            .set_offset(self.surface_rect.left as _, self.surface_rect.top as _)
            .expect("pos.set_offset");
        positioner
            .set_anchor(wl::XdgPositionerAnchor::TopLeft)
            .expect("pos.set_anchor");
        positioner
            .set_anchor_rect(0, 0, 1, 1)
            .expect("pos.set_anchor_rect");
        positioner
            .set_gravity(wl::XdgPositionerGravity::BottomRight)
            .expect("pos.set_gravity");
        positioner
            .set_constraint_adjustment(wl::XdgPositionerConstraintAdjustment::None)
            .expect("pos.set_constraint_adjustment");
        let mut pp = xdg_surface
            .get_popup(Some(on_surface), &positioner)
            .expect("pop.create");
        let mut state = Box::new(PopupState {
            surface_ptr: surface.as_ptr(),
            xdg_surface_ptr: xdg_surface.as_ptr(),
            viewport,
            buffer_ptr: self.buf.buffer(),
            buffer_attached: false,
            active_size: self.surface_rect.size(),
            pending_new_width: None,
            pending_new_height: None,
        });
        xdg_surface
            .set_listener(&mut *state)
            .into_result()
            .expect("xdg_popup_surface.set_listener");
        pp.set_listener(&mut *state)
            .into_result()
            .expect("pop.set_listener");

        // ignore all inputs for popup surface
        surface
            .set_input_region(Some(
                &interfaces
                    .compositor
                    .create_region()
                    .expect("input_region.create"),
            ))
            .expect("wl_popup_surface.set_input_region");

        state
            .viewport
            .set_source(
                wl::Fixed::ZERO,
                wl::Fixed::ZERO,
                wl::Fixed::ONE,
                wl::Fixed::ONE,
            )
            .expect("viewport.set_source");
        state
            .viewport
            .set_destination(self.surface_rect.width as _, self.surface_rect.height as _)
            .expect("viewport.set_destination");

        let blur = if let Some(ref bm) = interfaces.kde_blur_manager {
            let blur = bm.create(&surface).expect("blur.create");
            blur.commit().expect("blur.commit");

            Some(blur)
        } else {
            None
        };

        surface.commit().expect("wl_popup_surface.commit");

        self.surface = Some(MappedSurface {
            _blur: blur,
            xdg_popup: pp,
            _xdg_surface: xdg_surface,
            _surface: surface,
            _state: state,
        });
    }

    pub fn set_surface_rect(
        &mut self,
        interfaces: &super::GlobalInterfaces,
        r: &Rect<LogicalUnit>,
    ) {
        self.surface_rect = r.clone();

        let Some(ref mut x) = self.surface else {
            return;
        };

        let pos = interfaces
            .xdg_wm_base
            .create_positioner()
            .expect("pos.create");
        pos.set_offset(r.left as _, r.top as _)
            .expect("pos.set_offset");
        pos.set_size(r.width as _, r.height as _)
            .expect("pos.set_size");
        pos.set_anchor(wl::XdgPositionerAnchor::TopLeft)
            .expect("pos.set_anchor");
        pos.set_anchor_rect(0, 0, 1, 1)
            .expect("pos.set_anchor_rect");
        pos.set_gravity(wl::XdgPositionerGravity::BottomRight)
            .expect("pos.set_gravity");
        x.xdg_popup.reposition(&pos, 0).expect("pp.reposition");
    }

    pub fn hide_surface(&mut self) {
        self.surface = None;
    }

    pub fn teardown_dynamic_resoureces(&mut self) {
        self.surface = None;
        self.dnd_icon = None;
    }
}

#[allow(dead_code)]
pub enum Buffer {
    SinglePixel(wl::Owned<wl::Buffer>),
    Shm { buf: wl::Owned<wl::Buffer> },
}
impl Buffer {
    #[inline(always)]
    fn buffer(&self) -> &wl::Buffer {
        match self {
            Self::SinglePixel(x) => x,
            Self::Shm { buf, .. } => buf,
        }
    }
}

struct DndIcon {
    shown: bool,
    offset: Point<LogicalUnit>,
    blur: Option<wl::Owned<wl::OrgKdeKwinBlur>>,
    _viewport: wl::Owned<wl::WpViewport>,
    surface: wl::Owned<wl::Surface>,
}

struct MappedSurface {
    _blur: Option<wl::Owned<wl::OrgKdeKwinBlur>>,
    xdg_popup: wl::Owned<wl::XdgPopup>,
    _xdg_surface: wl::Owned<wl::XdgSurface>,
    _surface: wl::Owned<wl::Surface>,
    _state: Box<PopupState>,
}

struct PopupState {
    surface_ptr: *mut wl::Surface,
    xdg_surface_ptr: *mut wl::XdgSurface,
    viewport: wl::Owned<wl::WpViewport>,
    buffer_ptr: *const wl::Buffer,
    buffer_attached: bool,
    active_size: Size<LogicalUnit>,
    pending_new_width: Option<i32>,
    pending_new_height: Option<i32>,
}
impl wl::XdgSurfaceEventListener for PopupState {
    #[tracing::instrument(name = "xdg_surface(Popup)::configure", skip(self, sender))]
    fn configure(&mut self, sender: &mut wl::XdgSurface, serial: u32) {
        event_trace!();
        sender.ack_configure(serial).expect("popup.ack_configure");

        if !self.buffer_attached {
            unsafe { &*self.surface_ptr }
                .attach(Some(unsafe { &*self.buffer_ptr }), 0, 0)
                .expect("wl_popup_surface.attach");
            unsafe { &*self.surface_ptr }
                .damage(0, 0, -1, -1)
                .expect("wl_popup_surface.damage");
            self.buffer_attached = true;
        }

        if self.pending_new_width.is_some() || self.pending_new_height.is_some() {
            // resize occured
            let new_logical_size = Size::new_logical(
                self.pending_new_width
                    .take()
                    .map_or(self.active_size.width, |x| x as _),
                self.pending_new_height
                    .take()
                    .map_or(self.active_size.height, |x| x as _),
            );
            self.active_size = new_logical_size;

            unsafe { &*self.xdg_surface_ptr }
                .set_window_geometry(
                    0,
                    0,
                    self.active_size.width.ceil() as _,
                    self.active_size.height.ceil() as _,
                )
                .expect("xdg_surface.set_window_geometry");
            self.viewport
                .set_destination(
                    self.active_size.width.ceil() as _,
                    self.active_size.height.ceil() as _,
                )
                .expect("viewport.set_destination");
        }

        unsafe {
            (*self.surface_ptr).commit().expect("popup.surface.commit");
        }
    }
}
impl wl::XdgPopupEventListener for PopupState {
    #[tracing::instrument(name = "xdg_popup::configure", skip(self, _sender))]
    fn configure(&mut self, _sender: &mut wl::XdgPopup, x: i32, y: i32, width: i32, height: i32) {
        event_trace!();

        if width != 0 {
            self.pending_new_width = Some(width);
        }
        if height != 0 {
            self.pending_new_height = Some(height);
        }
    }

    #[tracing::instrument(name = "xdg_popup::popup_done", skip(self, _sender))]
    fn popup_done(&mut self, _sender: &mut wl::XdgPopup) {
        event_trace!();
    }

    #[tracing::instrument(name = "xdg_popup::repositioned", skip(self, _sender))]
    fn repositioned(&mut self, _sender: &mut wl::XdgPopup, token: u32) {
        event_trace!();
    }
}
