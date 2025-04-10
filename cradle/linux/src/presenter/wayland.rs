use std::{collections::HashMap, ffi::CStr};

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo, VkHandle};
use peridot::mthelper::{DynamicMutabilityProvider, SharedMutableRef};
use wayland_backend::client::ReadEventsGuard;
use wayland_client::{
    protocol::{
        wl_compositor::WlCompositor, wl_pointer::WlPointer, wl_registry::WlRegistry,
        wl_seat::WlSeat, wl_surface::WlSurface,
    },
    Connection, Dispatch, EventQueue, Proxy, QueueHandle,
};
use wayland_protocols::xdg::{
    decoration::zv1::client::{
        zxdg_decoration_manager_v1::ZxdgDecorationManagerV1,
        zxdg_toplevel_decoration_v1::ZxdgToplevelDecorationV1,
    },
    shell::client::{xdg_surface::XdgSurface, xdg_toplevel::XdgToplevel, xdg_wm_base::XdgWmBase},
};

use crate::input::PointerPositionProvider;

use super::{BorrowFd, EventProcessor, PresenterProvider, WindowBackend};

#[repr(transparent)]
pub struct ReadinessGuard(ReadEventsGuard);
impl BorrowFd for ReadinessGuard {
    fn borrow_fd<'fd>(&'fd self) -> std::os::fd::BorrowedFd<'fd> {
        self.0.connection_fd()
    }
}

pub struct State {
    close_requested: bool,
    geometry: peridot::math::Vector2<u32>,
    pointer_entered: bool,
    pointer_position: peridot::math::Vector2<usize>,
}
wayland_client::delegate_noop!(State: ignore WlCompositor);
wayland_client::delegate_noop!(State: ignore WlSurface);
wayland_client::delegate_noop!(State: ignore ZxdgDecorationManagerV1);
wayland_client::delegate_noop!(State: ignore ZxdgToplevelDecorationV1);
impl Dispatch<XdgWmBase, ()> for State {
    fn event(
        _state: &mut Self,
        proxy: &XdgWmBase,
        event: <XdgWmBase as Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &QueueHandle<Self>,
    ) {
        match event {
            wayland_protocols::xdg::shell::client::xdg_wm_base::Event::Ping { serial } => {
                proxy.pong(serial);
            }
            _ => (),
        }
    }
}
impl Dispatch<XdgSurface, ()> for State {
    #[tracing::instrument(skip(state, proxy, _data, _conn, _qhandle))]
    fn event(
        state: &mut Self,
        proxy: &XdgSurface,
        event: <XdgSurface as Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &QueueHandle<Self>,
    ) {
        match event {
            wayland_protocols::xdg::shell::client::xdg_surface::Event::Configure { serial } => {
                tracing::trace!("configure xdgsurface");

                proxy.ack_configure(serial);
                if state.geometry.0 > 0 && state.geometry.1 > 0 {
                    proxy.set_window_geometry(0, 0, state.geometry.0 as _, state.geometry.1 as _);
                }
            }
            _ => (),
        }
    }
}
impl Dispatch<XdgToplevel, ()> for State {
    #[tracing::instrument(skip(state, _proxy, _data, _conn, _qhandle))]
    fn event(
        state: &mut Self,
        _proxy: &XdgToplevel,
        event: <XdgToplevel as Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &QueueHandle<Self>,
    ) {
        match event {
            wayland_protocols::xdg::shell::client::xdg_toplevel::Event::Close => {
                state.close_requested = true;
            }
            wayland_protocols::xdg::shell::client::xdg_toplevel::Event::Configure {
                width,
                height,
                states,
            } => {
                tracing::trace!({ width, height, ?states }, "Configure XdgToplevel");

                if width > 0 && height > 0 {
                    state.geometry = peridot::math::Vector2(width as _, height as _);
                }
            }
            _ => (),
        }
    }
}
wayland_client::delegate_noop!(State: ignore WlSeat);
impl Dispatch<WlPointer, WlSurface> for State {
    fn event(
        state: &mut Self,
        _proxy: &WlPointer,
        event: <WlPointer as Proxy>::Event,
        data: &WlSurface,
        _conn: &Connection,
        _qhandle: &QueueHandle<Self>,
    ) {
        match event {
            wayland_client::protocol::wl_pointer::Event::Enter {
                surface,
                surface_x,
                surface_y,
                ..
            } if surface == *data => {
                state.pointer_entered = true;
                state.pointer_position = peridot::math::Vector2(surface_x as _, surface_y as _);
            }
            wayland_client::protocol::wl_pointer::Event::Leave { surface, .. }
                if surface == *data =>
            {
                state.pointer_entered = false;
            }
            wayland_client::protocol::wl_pointer::Event::Motion {
                surface_x,
                surface_y,
                ..
            } => {
                state.pointer_position = peridot::math::Vector2(surface_x as _, surface_y as _);
            }
            _ => (),
        }
    }
}

pub struct Wayland {
    con: Connection,
    event_queue: EventQueue<State>,
    surface: WlSurface,
    state: State,
    _refs: (
        XdgSurface,
        XdgToplevel,
        ZxdgToplevelDecorationV1,
        WlCompositor,
        XdgWmBase,
        ZxdgDecorationManagerV1,
        WlSeat,
        WlPointer,
    ),
}
impl Wayland {
    pub fn try_init() -> Option<Self> {
        let Ok(con) = wayland_client::Connection::connect_to_env() else {
            return None;
        };

        tracing::info!("Using Wayland as window backend");

        let mut registry_queue = con.new_event_queue();
        let mut interfaces = RegistryCollector(HashMap::new());
        let registry = con.display().get_registry(&registry_queue.handle(), ());
        registry_queue
            .roundtrip(&mut interfaces)
            .expect("Failed to roundtrip registry collection");
        drop(registry_queue);

        let mut state = State {
            close_requested: false,
            geometry: peridot::math::Vector2(640, 480),
            pointer_entered: false,
            pointer_position: peridot::math::Vector2(0, 0),
        };
        let mut event_queue = con.new_event_queue();
        let compositor: WlCompositor = interfaces
            .bind_interface(&registry, &event_queue.handle(), ())
            .expect("No compositor interface found");
        let surface = compositor.create_surface(&event_queue.handle(), ());
        let xdg_wm_base: XdgWmBase = interfaces
            .bind_interface(&registry, &event_queue.handle(), ())
            .expect("No xdg_wm_base interface found");
        let xdg_surface = xdg_wm_base.get_xdg_surface(&surface, &event_queue.handle(), ());
        let xdg_toplevel = xdg_surface.get_toplevel(&event_queue.handle(), ());
        xdg_surface.set_window_geometry(0, 0, 640, 480);
        xdg_toplevel.set_app_id(String::from(crate::userlib::APP_IDENTIFIER));
        xdg_toplevel.set_title(format!(
            "{} v{}",
            crate::userlib::APP_TITLE,
            crate::userlib::APP_VERSION,
        ));
        let xdg_decoration_manager: ZxdgDecorationManagerV1 = interfaces
            .bind_interface(&registry, &event_queue.handle(), ())
            .expect("No decoration manager interface found");
        let xdg_decoration = xdg_decoration_manager.get_toplevel_decoration(
            &xdg_toplevel,
            &event_queue.handle(),
            (),
        );
        surface.commit();

        let seat: WlSeat = interfaces
            .bind_interface(&registry, &event_queue.handle(), ())
            .expect("No seat interface found");
        let pointer = seat.get_pointer(&event_queue.handle(), surface.clone());

        event_queue
            .roundtrip(&mut state)
            .expect("Failed to final roundtrip");

        Some(Self {
            event_queue,
            con,
            surface,
            state,
            _refs: (
                xdg_surface,
                xdg_toplevel,
                xdg_decoration,
                compositor,
                xdg_wm_base,
                xdg_decoration_manager,
                seat,
                pointer,
            ),
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
        self.event_queue.flush().expect("Failed to flush queue");
        self.event_queue
            .dispatch_pending(&mut self.state)
            .expect("Failed to dispatch events");

        ReadinessGuard(
            self.event_queue
                .prepare_read()
                .expect("Failed to lock queue for read"),
        )
    }

    fn process_all_events(&mut self, guard: Self::ReadinessGuard) {
        let _ = guard.0.read().expect("Failed to read events");
        self.event_queue
            .dispatch_pending(&mut self.state)
            .expect("Failed to roundtrip");
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
                wlock.con.display().id().as_ptr() as _,
            )
        } {
            panic!("Vulkan Presentation is not supported!");
        }
        let so = Surface {
            handle: unsafe {
                br::WaylandSurfaceCreateInfo::new(
                    wlock.con.display().id().as_ptr() as _,
                    wlock.surface.id().as_ptr() as _,
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
        recorder: bedrock::CmdRecord<'r, peridot::VulkanGfx>,
    ) -> bedrock::CmdRecord<'r, peridot::VulkanGfx> {
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

struct RegistryCollector(HashMap<String, (u32, u32)>);
impl RegistryCollector {
    fn bind_interface<I, D, S>(
        &self,
        registry: &WlRegistry,
        queue_handle: &QueueHandle<D>,
        state: S,
    ) -> Option<I>
    where
        I: Proxy + 'static,
        D: Dispatch<I, S> + 'static,
        S: Send + Sync + 'static,
    {
        self.0
            .get(I::interface().name)
            .map(|&(name, version)| registry.bind(name, version, queue_handle, state))
    }
}
impl Dispatch<WlRegistry, ()> for RegistryCollector {
    fn event(
        state: &mut Self,
        _proxy: &WlRegistry,
        event: <WlRegistry as Proxy>::Event,
        _data: &(),
        _conn: &Connection,
        _qhandle: &QueueHandle<Self>,
    ) {
        match event {
            wayland_client::protocol::wl_registry::Event::Global {
                name,
                interface,
                version,
            } => {
                tracing::debug!({ interface, version }, "Wayland Registry collected");
                state.0.insert(interface, (name, version));
            }
            wayland_client::protocol::wl_registry::Event::GlobalRemove { name } => {
                state.0.retain(|_, &mut (n, _)| n != name)
            }
            _ => (),
        }
    }
}
