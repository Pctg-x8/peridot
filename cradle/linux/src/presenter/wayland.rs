use std::{
    cell::UnsafeCell,
    collections::HashMap,
    ffi::{CStr, CString},
    os::fd::{AsRawFd, BorrowedFd},
    pin::Pin,
    ptr::NonNull,
};

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo, VkHandle};
use peridot::mthelper::{DynamicMutabilityProvider, SharedMutableRef};
use peridot_tp_wayland::{self as wl, Interface};

use crate::input::PointerPositionProvider;

use super::{BorrowFd, EventProcessor, PresenterProvider, WindowBackend};

#[repr(transparent)]
pub struct ReadinessGuard(core::ffi::c_int);
impl BorrowFd for ReadinessGuard {
    fn borrow_fd<'fd>(&'fd self) -> BorrowedFd<'fd> {
        unsafe { BorrowedFd::borrow_raw(self.0) }
    }
}

pub struct State {
    close_requested: bool,
    geometry: peridot::math::Vector2<u32>,
    pointer_entered: bool,
    pointer_position: peridot::math::Vector2<usize>,
}
impl wl::SurfaceEventListener for State {
    fn enter(
        &mut self,
        _surface: &mut peridot_tp_wayland::Surface,
        _output: &mut peridot_tp_wayland::Output,
    ) {
    }

    fn leave(
        &mut self,
        _surface: &mut peridot_tp_wayland::Surface,
        _output: &mut peridot_tp_wayland::Output,
    ) {
    }

    fn preferred_buffer_scale(&mut self, _surface: &mut peridot_tp_wayland::Surface, _factor: i32) {
    }

    fn preferred_buffer_transform(
        &mut self,
        _surface: &mut peridot_tp_wayland::Surface,
        _transform: u32,
    ) {
    }
}
impl wl::ZxdgToplevelDecorationV1EventListener for State {
    fn configure(
        &mut self,
        _sender: &mut peridot_tp_wayland::ZxdgToplevelDecorationV1,
        _mode: peridot_tp_wayland::ZxdgToplevelDecorationMode,
    ) {
    }
}
impl wl::XdgWmBaseEventListener for State {
    #[tracing::instrument(name = "<State as XdgWmBaseEventListener>::ping", skip(self, wm_base))]
    fn ping(&mut self, wm_base: &mut peridot_tp_wayland::XdgWmBase, serial: u32) {
        if let Err(e) = wm_base.pong(serial) {
            tracing::warn!(reason = ?e, "pong failed");
        }
    }
}
impl wl::XdgSurfaceEventListener for State {
    #[tracing::instrument(
        name = "<State as XdgSurfaceEventListener>::configure",
        skip(self, surface)
    )]
    fn configure(&mut self, surface: &mut peridot_tp_wayland::XdgSurface, serial: u32) {
        tracing::trace!("configure xdgsurface");

        if let Err(e) = surface.ack_configure(serial) {
            tracing::warn!(reason = ?e, "ack_configure failed");
            return;
        }

        if self.geometry.0 > 0 && self.geometry.1 > 0 {
            if let Err(e) =
                surface.set_window_geometry(0, 0, self.geometry.0 as _, self.geometry.1 as _)
            {
                tracing::warn!(reason = ?e, "set_window_geometry failed");
            }
        }
    }
}
impl wl::XdgToplevelEventListener for State {
    #[tracing::instrument(
        name = "<State as XdgToplevelEventListener>::configure",
        skip(self, _toplevel)
    )]
    fn configure(
        &mut self,
        _toplevel: &mut peridot_tp_wayland::XdgToplevel,
        width: i32,
        height: i32,
        states: &[i32],
    ) {
        tracing::trace!(width, height, ?states, "configure xdgtoplevel");

        if width > 0 && height > 0 {
            self.geometry = peridot::math::Vector2(width as _, height as _);
        }
    }

    fn configure_bounds(
        &mut self,
        _toplevel: &mut peridot_tp_wayland::XdgToplevel,
        _width: i32,
        _height: i32,
    ) {
    }

    fn close(&mut self, _toplevel: &mut peridot_tp_wayland::XdgToplevel) {
        self.close_requested = true;
    }

    fn wm_capabilities(
        &mut self,
        _toplevel: &mut peridot_tp_wayland::XdgToplevel,
        _capabilities: &[i32],
    ) {
    }
}
impl wl::SeatEventListener for State {
    fn capabilities(&mut self, _seat: &mut peridot_tp_wayland::Seat, _capabilities: u32) {}

    #[tracing::instrument(name = "<State as SeatEventListener>::name", skip(self, _seat))]
    fn name(&mut self, _seat: &mut peridot_tp_wayland::Seat, name: &core::ffi::CStr) {
        tracing::debug!(?name);
    }
}
impl wl::PointerEventListener for State {
    fn enter(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        _serial: u32,
        _surface: &mut peridot_tp_wayland::Surface,
        surface_x: peridot_tp_wayland::Fixed,
        surface_y: peridot_tp_wayland::Fixed,
    ) {
        self.pointer_entered = true;
        self.pointer_position =
            peridot::math::Vector2(surface_x.to_f32() as _, surface_y.to_f32() as _);
    }

    fn leave(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        _serial: u32,
        _surface: &mut peridot_tp_wayland::Surface,
    ) {
        self.pointer_entered = false;
    }

    fn motion(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        _time: u32,
        surface_x: peridot_tp_wayland::Fixed,
        surface_y: peridot_tp_wayland::Fixed,
    ) {
        self.pointer_position =
            peridot::math::Vector2(surface_x.to_f32() as _, surface_y.to_f32() as _);
    }

    fn frame(&mut self, _pointer: &mut peridot_tp_wayland::Pointer) {}

    fn button(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        _serial: u32,
        _time: u32,
        _button: u32,
        _state: peridot_tp_wayland::PointerButtonState,
    ) {
    }

    fn axis(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        _time: u32,
        _axis: u32,
        _value: peridot_tp_wayland::Fixed,
    ) {
    }

    fn axis_discrete(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        _axis: u32,
        _discrete: i32,
    ) {
    }

    fn axis_relative_direction(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        _axis: u32,
        _direction: u32,
    ) {
    }

    fn axis_source(&mut self, _pointer: &mut peridot_tp_wayland::Pointer, _axis_source: u32) {}

    fn axis_stop(&mut self, _pointer: &mut peridot_tp_wayland::Pointer, _time: u32, _axis: u32) {}

    fn axis_value120(
        &mut self,
        _pointer: &mut peridot_tp_wayland::Pointer,
        _axis: u32,
        _value120: i32,
    ) {
    }
}

macro_rules! err_warn {
    ($e: expr, $msg: literal) => {
        if let Err(e) = $e {
            tracing::warn!(reason = ?e, $msg);
        }
    }
}

macro_rules! err_fatal_bailout {
    ($e: expr, $msg: literal) => {
        match $e {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, $msg);
                std::process::abort();
            }
        }
    };
    (opt $e: expr, $msg: literal) => {
        match $e {
            Some(x) => x,
            None => {
                tracing::error!($msg);
                std::process::abort();
            }
        }
    }
}

struct DisplayState {
    head_ptr: *const wl::ZwlrOutputHeadV1,
    original_mode_ptr: *const wl::ZwlrOutputModeV1,
    original_scale: wl::Fixed,
}
impl DisplayState {
    pub fn restore(
        &self,
        configuration_head: &wl::ZwlrOutputConfigurationHeadV1,
    ) -> Result<(), std::io::Error> {
        tracing::info!(?self.original_mode_ptr, ?self.original_scale, "Restoring Display modes");

        configuration_head.set_mode(unsafe { &*self.original_mode_ptr })?;
        configuration_head.set_scale(self.original_scale)?;

        Ok(())
    }
}

struct Listener {
    heads: Vec<(wl::Owned<wl::ZwlrOutputHeadV1>, Pin<Box<HeadState>>)>,
    last_done_serial: u32,
}
impl wl::ZwlrOutputManagerV1EventListener for Listener {
    fn head(
        &mut self,
        _sender: &mut wl::ZwlrOutputManagerV1,
        mut head: wl::Owned<wl::ZwlrOutputHeadV1>,
    ) {
        let mut l = Box::pin(HeadState {
            name: None,
            description: None,
            physical_width: 0,
            physical_height: 0,
            modes: Vec::new(),
            current_mode_ptr: core::ptr::null_mut(),
            enabled: false,
            position_x: 0,
            position_y: 0,
            transform: wl::OutputTransform::Normal,
            scale: wl::Fixed::from_f32_lossy(1.0),
            finished: false,
        });
        err_warn!(head.set_listener(&mut *l), "head set_listener failed");
        self.heads.push((head, l));
    }

    fn done(&mut self, sender: &mut peridot_tp_wayland::ZwlrOutputManagerV1, serial: u32) {
        println!("done {serial}");

        for (_, h) in self.heads.iter() {
            println!("head {:?}: {:?}", h.name, h.description);
            println!("* enabled: {}", h.enabled);
            println!(
                "* physical size: {}x{}",
                h.physical_width, h.physical_height
            );
            println!("* position: {}x{}", h.position_x, h.position_y);
            println!("* transform: {:?}", h.transform);
            println!("* scale: x{}", h.scale.to_f32());
            if !h.current_mode_ptr.is_null()
                && let Some((_, m)) = h
                    .modes
                    .iter()
                    .find(|(p, _)| p.ref_eq(unsafe { &*h.current_mode_ptr }))
            {
                println!(
                    "* current mode: {}x{} @ {}.{:03}hz",
                    m.width, m.height, m.refresh_ipart, m.refresh_fpart
                );
            } else {
                println!("* current mode: <unknown>");
            }

            for (_, m) in h.modes.iter() {
                println!(
                    "* mode: {}x{} @ {}.{:03}hz {}",
                    m.width,
                    m.height,
                    m.refresh_ipart,
                    m.refresh_fpart,
                    if m.preferred { "[preferred]" } else { "" }
                );
            }
        }

        self.last_done_serial = serial;
    }

    fn finished(&mut self, _sender: &mut wl::ZwlrOutputManagerV1) {
        println!("finished");
    }
}
impl wl::ZwlrOutputConfigurationV1EventListener for Listener {
    fn succeeded(&mut self, sender: &mut peridot_tp_wayland::ZwlrOutputConfigurationV1) {
        println!("cfg ok!");
    }

    fn failed(&mut self, sender: &mut peridot_tp_wayland::ZwlrOutputConfigurationV1) {
        println!("cfg failed");
    }

    fn cancelled(&mut self, sender: &mut peridot_tp_wayland::ZwlrOutputConfigurationV1) {
        println!("cfg cancelled");
    }
}

struct HeadState {
    name: Option<std::ffi::CString>,
    description: Option<std::ffi::CString>,
    physical_width: i32,
    physical_height: i32,
    modes: Vec<(wl::Owned<wl::ZwlrOutputModeV1>, Pin<Box<ModeState>>)>,
    current_mode_ptr: *mut wl::ZwlrOutputModeV1,
    enabled: bool,
    position_x: i32,
    position_y: i32,
    transform: wl::OutputTransform,
    scale: wl::Fixed,
    finished: bool,
}
impl wl::ZwlrOutputHeadV1EventListener for HeadState {
    fn name(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, name: &core::ffi::CStr) {
        self.name = Some(name.into());
    }

    fn description(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, description: &core::ffi::CStr) {
        self.description = Some(description.into());
    }

    fn physical_size(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, width: i32, height: i32) {
        self.physical_width = width;
        self.physical_height = height;
    }

    fn mode(
        &mut self,
        _sender: &mut wl::ZwlrOutputHeadV1,
        mut mode: wl::Owned<wl::ZwlrOutputModeV1>,
    ) {
        let mut state = Box::pin(ModeState {
            width: 0,
            height: 0,
            refresh_ipart: 0,
            refresh_fpart: 0,
            preferred: false,
            finished: false,
        });
        err_warn!(mode.set_listener(&mut *state), "mode set_listener failed");
        self.modes.push((mode, state));
    }

    fn current_mode(
        &mut self,
        _sender: &mut wl::ZwlrOutputHeadV1,
        mode: &mut wl::ZwlrOutputModeV1,
    ) {
        self.current_mode_ptr = mode;
    }

    fn enabled(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, enabled: bool) {
        self.enabled = enabled;
    }

    fn position(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, x: i32, y: i32) {
        self.position_x = x;
        self.position_y = y;
    }

    fn transform(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, transform: wl::OutputTransform) {
        self.transform = transform;
    }

    fn scale(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, scale: wl::Fixed) {
        self.scale = scale;
    }

    fn finished(&mut self, _sender: &mut wl::ZwlrOutputHeadV1) {
        self.finished = true;
    }

    fn make(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, _make: &core::ffi::CStr) {}

    fn model(&mut self, _sender: &mut wl::ZwlrOutputHeadV1, _model: &core::ffi::CStr) {}

    fn serial_number(
        &mut self,
        _sender: &mut wl::ZwlrOutputHeadV1,
        _serial_number: &core::ffi::CStr,
    ) {
    }

    fn adaptive_sync(
        &mut self,
        _sender: &mut wl::ZwlrOutputHeadV1,
        state: wl::ZwlrOutputHeadV1AdaptiveSyncState,
    ) {
        println!("head adaptive sync: {state:?}");
    }
}

struct ModeState {
    width: i32,
    height: i32,
    refresh_ipart: i32,
    refresh_fpart: i32,
    preferred: bool,
    finished: bool,
}
impl wl::ZwlrOutputModeV1EventListener for ModeState {
    fn size(
        &mut self,
        _sender: &mut peridot_tp_wayland::ZwlrOutputModeV1,
        width: i32,
        height: i32,
    ) {
        self.width = width;
        self.height = height;
    }

    fn refresh(&mut self, _sender: &mut peridot_tp_wayland::ZwlrOutputModeV1, refresh: i32) {
        self.refresh_ipart = refresh / 1000;
        self.refresh_fpart = refresh % 1000;
    }

    fn preferred(&mut self, _sender: &mut peridot_tp_wayland::ZwlrOutputModeV1) {
        self.preferred = true;
    }

    fn finished(&mut self, _sender: &mut peridot_tp_wayland::ZwlrOutputModeV1) {
        self.finished = true;
    }
}

struct OutputConfigurationResultReceiver;
impl wl::ZwlrOutputConfigurationV1EventListener for OutputConfigurationResultReceiver {
    fn succeeded(&mut self, sender: &mut peridot_tp_wayland::ZwlrOutputConfigurationV1) {
        println!("output cfg ok!");
        unsafe {
            sender.destruct();
        }
    }

    fn failed(&mut self, sender: &mut peridot_tp_wayland::ZwlrOutputConfigurationV1) {
        println!("output cfg failed");
        unsafe {
            sender.destruct();
        }
    }

    fn cancelled(&mut self, sender: &mut peridot_tp_wayland::ZwlrOutputConfigurationV1) {
        println!("output cfg cancelled");
        unsafe {
            sender.destruct();
        }
    }
}

struct DisplayManager {
    mgr_objects: Pin<Box<Listener>>,
    output_manager: wl::Owned<wl::ZwlrOutputManagerV1>,
    preserved_state: Option<DisplayState>,
    output_cfg_result_receiver: UnsafeCell<Pin<Box<OutputConfigurationResultReceiver>>>,
}
impl DisplayManager {
    pub fn set_mode(&mut self, head_index: usize, mode_index: usize) {
        if let Some(ref ps) = self.preserved_state
            && !core::ptr::addr_eq(ps.head_ptr, &self.mgr_objects.heads[head_index].0)
        {
            // 別のheadのモードセットをするので前のをもどす
            self.restore();
        }

        if self.preserved_state.is_none() {
            let new_head_state_ref = self.mgr_objects.heads[head_index].1.as_ref();
            self.preserved_state = Some(DisplayState {
                head_ptr: &*self.mgr_objects.heads[head_index].0,
                original_mode_ptr: new_head_state_ref.current_mode_ptr,
                original_scale: new_head_state_ref.scale,
            });
        }

        let head = &self.mgr_objects.heads[head_index].0;
        let mode = &self.mgr_objects.heads[head_index].1.modes[mode_index].0;

        let mut output_cfg = self
            .output_manager
            .create_configuration(self.mgr_objects.last_done_serial)
            .expect("create_configuration failed");
        output_cfg
            .set_listener(unsafe { &mut **self.output_cfg_result_receiver.get() })
            .expect("output_cfg set_listener failed");
        let cfg_head = output_cfg
            .enable_head(head)
            .expect("output_configuration enable_head failed");
        cfg_head
            .set_scale(wl::Fixed::from_f32_lossy(1.0))
            .expect("cfg_head set_scale failed");
        cfg_head.set_mode(mode).expect("set_mode failed");
        output_cfg.apply().expect("Failed to apply mode");
        output_cfg.leak();
    }

    pub fn restore(&mut self) {
        let Some(ps) = self.preserved_state.take() else {
            // 前のがない(モードセットしてない)
            return;
        };

        let mut output_cfg = self
            .output_manager
            .create_configuration(self.mgr_objects.last_done_serial)
            .expect("Failed to create output configuration");
        output_cfg
            .set_listener(unsafe { &mut **self.output_cfg_result_receiver.get() })
            .expect("output_cfg set_listener failed");
        let cfg_head = output_cfg
            .enable_head(unsafe { &*ps.head_ptr })
            .expect("Failed to enable head for configuration");
        ps.restore(&cfg_head).expect("Failed to restore");
        output_cfg.apply().expect("Failed to apply mode");
        output_cfg.leak();
    }
}

pub struct Wayland {
    con: wl::Display,
    surface: NonNull<wl::Surface>,
    state: Pin<Box<State>>,
    display_mgr: Option<DisplayManager>,
}
impl Drop for Wayland {
    fn drop(&mut self) {
        if let Some(ref mut d) = self.display_mgr {
            d.restore();
            err_warn!(self.con.roundtrip(), "Failed to restore roundtrip");
        }
    }
}
impl Wayland {
    #[tracing::instrument(name = "Wayland::try_init")]
    pub fn try_init() -> Option<Self> {
        let Some(con) = wl::Display::connect() else {
            tracing::error!("Unable to connect to wayland display");
            return None;
        };

        tracing::info!("Using Wayland as window backend");

        let mut interfaces = RegistryCollector(HashMap::new());
        let mut registry = match con.get_registry() {
            Ok(x) => x,
            Err(e) => {
                tracing::error!(reason = ?e, "Failed to get wayland registry object");
                std::process::abort();
            }
        };
        err_warn!(
            registry.set_listener(&mut interfaces),
            "registry set_listener failed"
        );
        err_warn!(con.roundtrip(), "roundtrip failed");

        let mut state = Box::pin(State {
            close_requested: false,
            geometry: peridot::math::Vector2(640, 480),
            pointer_entered: false,
            pointer_position: peridot::math::Vector2(0, 0),
        });
        let compositor = err_fatal_bailout!(
            opt err_fatal_bailout!(interfaces.bind_interface::<wl::Compositor>(&registry), "Failed to bind interface"),
            "No compositor interface found"
        );
        let mut surface = err_fatal_bailout!(compositor.create_surface(), "create_surface failed");
        err_warn!(
            surface.set_listener(&mut *state),
            "surface set_listener failed"
        );
        let mut xdg_wm_base = err_fatal_bailout!(
            opt err_fatal_bailout!(interfaces.bind_interface::<wl::XdgWmBase>(&registry), "Failed to bind interface"),
            "No xdg_wm_base interface found"
        );
        err_warn!(
            xdg_wm_base.set_listener(&mut *state),
            "xdg_wm_base set_listener failed"
        );
        let mut xdg_surface = err_fatal_bailout!(
            xdg_wm_base.get_xdg_surface(&surface),
            "get_xdg_surface failed"
        );
        err_warn!(
            xdg_surface.set_listener(&mut *state),
            "xdg_surface set_listener failed"
        );
        let mut xdg_toplevel =
            err_fatal_bailout!(xdg_surface.get_toplevel(), "get_toplevel failed");
        err_warn!(
            xdg_toplevel.set_listener(&mut *state),
            "xdg_toplevel set_listener failed"
        );
        err_warn!(
            xdg_surface.set_window_geometry(0, 0, 640, 480),
            "set_window_geometry failed"
        );
        err_warn!(
            xdg_toplevel.set_app_id(&unsafe {
                CString::from_vec_unchecked(crate::userlib::APP_IDENTIFIER.as_bytes().into())
            }),
            "set_app_id failed"
        );
        err_warn!(
            xdg_toplevel.set_title(&unsafe {
                CString::from_vec_unchecked(
                    format!(
                        "{} v{}",
                        crate::userlib::APP_TITLE,
                        crate::userlib::APP_VERSION,
                    )
                    .into_bytes(),
                )
            }),
            "set_title failed"
        );
        let xdg_decoration_manager = err_fatal_bailout!(
            opt err_fatal_bailout!(
                interfaces.bind_interface::<wl::ZxdgDecorationManagerV1>(&registry),
                "Failed to bind interface"
            ),
            "No decoration manager interface found"
        );
        let xdg_decoration = err_fatal_bailout!(
            xdg_decoration_manager.get_toplevel_decoration(&xdg_toplevel),
            "get_toplevel_decoration failed"
        );
        err_warn!(surface.commit(), "surface commit failed");

        let mut seat = err_fatal_bailout!(
            opt err_fatal_bailout!(interfaces.bind_interface::<wl::Seat>(&registry), "Failed to bind interface"),
            "No seat interface found"
        );
        err_warn!(seat.set_listener(&mut *state), "seat set_listener failed");
        let mut pointer = err_fatal_bailout!(seat.get_pointer(), "seat get_pointer failed");
        err_warn!(
            pointer.set_listener(&mut *state),
            "pointer set_listener failed"
        );
        // err_warn!(
        //     xdg_toplevel.set_fullscreen(),
        //     "xdg_toplevel set_fullscreen failed"
        // );
        //

        let mut display_mgr;
        if let Some(mut output_manager) = err_fatal_bailout!(
            interfaces.bind_interface::<wl::ZwlrOutputManagerV1>(&registry),
            "Failed to bind interface"
        ) {
            let mut l = Box::pin(Listener {
                heads: Vec::new(),
                last_done_serial: 0,
            });
            err_warn!(
                output_manager.set_listener(&mut *l),
                "output_manager set_listener failed"
            );
            err_warn!(con.roundtrip(), "Failed to roundtrip");

            display_mgr = Some(DisplayManager {
                output_manager,
                mgr_objects: l,
                preserved_state: None,
                output_cfg_result_receiver: UnsafeCell::new(Box::pin(
                    OutputConfigurationResultReceiver,
                )),
            });
        } else {
            display_mgr = None;
        }

        if let Some(ref mut ds) = display_mgr {
            let mode_index = ds.mgr_objects.heads[1]
                .1
                .modes
                .iter()
                .position(|(_, m)| m.width == 1920 && m.height == 1080)
                .expect("no expected size");
            ds.set_mode(1, mode_index);
        }

        err_warn!(con.roundtrip(), "Failed to final roundtrip");

        pointer.leak();
        seat.leak();
        xdg_decoration.leak();
        xdg_decoration_manager.leak();
        xdg_toplevel.leak();
        xdg_surface.leak();
        xdg_wm_base.leak();
        compositor.leak();

        Some(Self {
            con,
            surface: surface.unwrap(),
            state,
            display_mgr,
        })
    }
}
impl WindowBackend for Wayland {
    fn show(&mut self) {}

    fn geometry(&self) -> peridot::math::Vector2<u32> {
        self.state.geometry
    }
}
impl PresenterProvider for SharedMutableRef<Wayland> {
    type Presenter = Presenter;
    const SURFACE_EXT_NAME: &'static CStr = c"VK_KHR_wayland_surface";

    fn create(&self, g: &peridot::Graphics) -> Self::Presenter {
        Presenter::new(g, g.graphics_queue_family_index(), self)
    }
}
impl EventProcessor for Wayland {
    type ReadinessGuard = ReadinessGuard;

    fn readiness_guard(&mut self) -> Self::ReadinessGuard {
        loop {
            match self.con.prepare_read() {
                Ok(_) => break,
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    if let Err(e) = self.con.dispatch_pending() {
                        tracing::error!(reason = ?e, "Faield to dispatch pending events");
                        std::process::abort();
                    }
                }
                Err(e) => {
                    tracing::error!(reason = ?e, "Failed to prepare reading events");
                    std::process::abort();
                }
            }
        }

        err_warn!(self.con.flush(), "Failed to flush outgoing events");
        ReadinessGuard(self.con.as_raw_fd())
    }

    fn process_all_events(&mut self, _guard: Self::ReadinessGuard) {
        err_warn!(self.con.read_events(), "Failed to read events");
        err_warn!(self.con.dispatch_pending(), "Failed to dispatch events");
    }

    fn cancel_read(&mut self) {
        self.con.cancel_read();
    }

    fn has_close_requested(&self) -> bool {
        self.state.close_requested
    }
}

struct Surface {
    handle: br::vk::VkSurfaceKHR,
    device: peridot::VulkanGfx,
}
impl Drop for Surface {
    fn drop(&mut self) {
        unsafe {
            br::vkfn_wrapper::destroy_surface(
                self.device.instance().native_ptr(),
                self.handle,
                None,
            );
        }
    }
}
impl br::VkHandle for Surface {
    type Handle = br::vk::VkSurfaceKHR;

    fn native_ptr(&self) -> Self::Handle {
        self.handle
    }
}

pub struct Presenter {
    window_backend: SharedMutableRef<Wayland>,
    sc: peridot::IntegratedSwapchain<Surface>,
}
impl Presenter {
    fn new(
        g: &peridot::Graphics,
        renderer_queue_family: u32,
        w: &SharedMutableRef<Wayland>,
    ) -> Self {
        let wlock = w.borrow();

        if !unsafe {
            br::vkfn_wrapper::get_physical_device_wayland_presentation_support(
                g.adapter_raw(),
                renderer_queue_family,
                wlock.con.as_raw() as _,
            )
        } {
            panic!("Vulkan Presentation is not supported!");
        }
        let so = Surface {
            handle: unsafe {
                br::WaylandSurfaceCreateInfo::new(
                    wlock.con.as_raw() as _,
                    wlock.surface.as_ptr() as _,
                )
                .execute(g.device().instance(), None)
                .expect("Failed to create surface object")
            },
            device: g.device().clone(),
        };
        if !g
            .device()
            .surface_support(&so)
            .expect("Failed to query surface support")
        {
            panic!("Vulkan Surface is not supported");
        }
        let sc = peridot::IntegratedSwapchain::new(g, so, peridot::math::Vector2(640, 480));
        drop(wlock);

        Self {
            sc,
            window_backend: w.clone(),
        }
    }
}
impl peridot::PlatformPresenter for Presenter {
    fn format(&self) -> br::Format {
        self.sc.format()
    }

    fn back_buffer_size(&self) -> peridot::math::Vector2<u32> {
        self.sc.back_buffer_size()
    }

    fn back_buffer_count(&self) -> usize {
        self.sc.back_buffer_count()
    }

    fn back_buffer<'a>(
        &'a self,
        index: usize,
    ) -> Option<bedrock::VkHandleRef<'a, bedrock::vk::VkImage>> {
        self.sc.back_buffer(index)
    }

    fn requesting_back_buffer_layout(&self) -> (br::ImageLayout, br::PipelineStageFlags) {
        self.sc.requesting_back_buffer_layout()
    }

    fn emit_initialize_back_buffer_commands<'r>(
        &self,
        recorder: bedrock::CmdRecord<'r>,
    ) -> bedrock::CmdRecord<'r> {
        self.sc.emit_initialize_back_buffer_commands(recorder)
    }

    fn next_back_buffer_index(&mut self) -> bedrock::Result<u32> {
        self.sc.acquire_next_back_buffer_index()
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
        // WSI integrated swapchain needs reinitializing backbuffer resource
        true
    }

    fn current_geometry_extent(&self) -> peridot::math::Vector2<u32> {
        self.window_backend.borrow().state.geometry
    }
}
impl PointerPositionProvider for Wayland {
    fn get_pointer_position(&self) -> Option<(f32, f32)> {
        self.state.pointer_entered.then(|| {
            (
                self.state.pointer_position.0 as _,
                self.state.pointer_position.1 as _,
            )
        })
    }

    fn query_input_focus(&self) -> bool {
        self.state.pointer_entered
    }

    fn query_input_focus_and_pointer_entered(&self) -> (bool, bool) {
        (self.state.pointer_entered, self.state.pointer_entered)
    }
}

struct RegistryCollector(HashMap<CString, (u32, u32)>);
impl RegistryCollector {
    fn bind_interface<I>(
        &self,
        registry: &wl::Registry,
    ) -> Result<Option<wl::Owned<I>>, std::io::Error>
    where
        I: wl::Interface,
    {
        self.0
            .get(unsafe { core::ffi::CStr::from_ptr(I::def().name) })
            .map(|&(name, version)| registry.bind(name, version))
            .transpose()
    }
}
impl wl::RegistryListener for RegistryCollector {
    fn global(
        &mut self,
        _registry: &mut peridot_tp_wayland::Registry,
        name: u32,
        interface: &core::ffi::CStr,
        version: u32,
    ) {
        tracing::debug!(?interface, version, "Wayland registry collected");
        self.0.insert(interface.into(), (name, version));
    }

    fn global_remove(&mut self, _registry: &mut peridot_tp_wayland::Registry, name: u32) {
        self.0.retain(|_, &mut (n, _)| n != name)
    }
}
