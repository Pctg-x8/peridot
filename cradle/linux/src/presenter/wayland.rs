use std::{
    collections::HashMap,
    ffi::{CStr, CString},
    os::fd::{AsRawFd, BorrowedFd},
    pin::Pin,
    ptr::NonNull,
};

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo, VkHandle};
use peridot::mthelper::{DynamicMutabilityProvider, SharedMutableRef};
use peridot_tp_wayland as wl;

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
        _mode: peridot_tp_wayland::ZxdgToplevelDecorationV1Mode,
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
        skip(self, _toplevel, states)
    )]
    fn configure(
        &mut self,
        _toplevel: &mut peridot_tp_wayland::XdgToplevel,
        width: i32,
        height: i32,
        states: &mut wl::ffi::Array,
    ) {
        let states = unsafe { core::slice::from_raw_parts(states.data as _, states.size) };
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
        _capabilities: &mut wl::ffi::Array,
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

pub struct Wayland {
    con: wl::Display,
    surface: NonNull<wl::Surface>,
    state: Pin<Box<State>>,
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
        let _ = registry.set_listener(&mut interfaces);
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
        let _ = surface.set_listener(&mut *state);
        let mut xdg_wm_base = err_fatal_bailout!(
            opt err_fatal_bailout!(interfaces.bind_interface::<wl::XdgWmBase>(&registry), "Failed to bind interface"),
            "No xdg_wm_base interface found"
        );
        let _ = xdg_wm_base.set_listener(&mut *state);
        let mut xdg_surface = err_fatal_bailout!(
            xdg_wm_base.get_xdg_surface(&surface),
            "get_xdg_surface failed"
        );
        let _ = xdg_surface.set_listener(&mut *state);
        let mut xdg_toplevel =
            err_fatal_bailout!(xdg_surface.get_toplevel(), "get_toplevel failed");
        let _ = xdg_toplevel.set_listener(&mut *state);
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

        'optin_content_type: {
            let ct = match interfaces.bind_interface::<wl::WpContentTypeManagerV1>(&registry) {
                Ok(Some(x)) => x,
                Ok(None) => {
                    // no content type extension
                    break 'optin_content_type;
                }
                Err(e) => {
                    tracing::error!(cause = ?e, "Failed to bind interface");
                    break 'optin_content_type;
                }
            };

            let ct_state = match ct.get_surface_content_type(&surface) {
                Ok(x) => x,
                Err(e) => {
                    tracing::error!(cause = ?e, "get_surface_content_type failed");
                    break 'optin_content_type;
                }
            };
            if let Err(e) = ct_state.set_content_type(wl::WpContentTypeV1Type::Game) {
                tracing::error!(cause = ?e, "ct_state set_content_type failed");
                break 'optin_content_type;
            }

            // destroyで状態もどっちゃうのでleakさせておく
            ct_state.leak();
        }

        err_warn!(surface.commit(), "surface commit failed");

        let mut seat = err_fatal_bailout!(
            opt err_fatal_bailout!(interfaces.bind_interface::<wl::Seat>(&registry), "Failed to bind interface"),
            "No seat interface found"
        );
        let _ = seat.set_listener(&mut *state);
        let mut pointer = err_fatal_bailout!(seat.get_pointer(), "seat get_pointer failed");
        let _ = pointer.set_listener(&mut *state);

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
            .get(unsafe { core::ffi::CStr::from_ptr((*I::DEF).name) })
            .map(|&(name, version)| registry.bind(name, version))
            .transpose()
    }
}
impl wl::RegistryListener for RegistryCollector {
    fn global(
        &mut self,
        _registry: &mut wl::Registry,
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
