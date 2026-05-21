use core::{pin::Pin, ptr::NonNull, sync::atomic::AtomicBool};
use std::sync::Mutex;

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};
use peridot_tp_dbus as dbus;
use peridot_tp_wayland::{self as wl, ProxyObject};

use crate::{
    Event, LogicFiberEventDispatcher, WindowType,
    graphics::{VulkanDevice, VulkanSurface},
    input::{
        KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, PerWindowKeyboardFocusState,
        hittest::{HitTestTreeData, HitTestTreeManager, HitTestTreeRef},
    },
    platform::unix::wayland::{
        APPMENU_OBJECT_PATH, DisplayServerContext, GlobalInterfaces, PointerEventID,
        SurfaceScaling, SurfaceState, SurfaceStateTag,
    },
    rendering::{
        NewWindowData, NewWindowVulkanSurface, RenderMessage, RenderMessageSender,
        composite::{CompositeRect, CompositeTree, CompositeTreeRef},
    },
    utils::{LogicalUnit, PixelsUnit, Size},
};

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Handle(pub(super) NonNull<wl::Surface>);
unsafe impl Send for Handle {}
unsafe impl Sync for Handle {}
impl Handle {
    #[inline(always)]
    pub(super) const fn from_mut(ptr: &mut wl::Surface) -> Self {
        Self(NonNull::from_mut(ptr))
    }

    #[inline(always)]
    fn event_listener(&self) -> &EventListener {
        unsafe { &*self.0.as_ref().user_data().cast::<EventListener>() }
    }

    #[inline(always)]
    fn state(&self) -> &InstanceState {
        unsafe {
            &(*self
                .0
                .as_ref()
                .user_data()
                .cast::<SurfaceState<InstanceState>>())
            .data
        }
    }

    #[inline(always)]
    fn state_mut(&mut self) -> &mut InstanceState {
        unsafe {
            &mut (*self
                .0
                .as_mut()
                .user_data()
                .cast::<SurfaceState<InstanceState>>())
            .data
        }
    }

    #[inline(always)]
    pub fn associate_extra_data<T>(&mut self, data: Box<T>) {
        self.state_mut().extra_data = Box::into_raw(data) as _;
    }

    #[inline(always)]
    pub unsafe fn extra_data_ref<T>(&self) -> &T {
        unsafe { &*self.state().extra_data.cast() }
    }

    #[inline(always)]
    pub unsafe fn extra_data_mut<T>(&mut self) -> &mut T {
        unsafe { &mut *self.state_mut().extra_data.cast() }
    }

    #[inline(always)]
    pub unsafe fn take_extra_data<T>(&mut self) -> Box<T> {
        let r = unsafe { Box::from_raw(self.state_mut().extra_data.cast()) };
        self.state_mut().extra_data = core::ptr::null_mut();

        r
    }

    #[inline(always)]
    pub fn needs_system_command_buttons(&self) -> bool {
        self.event_listener().needs_system_command_buttons
    }

    #[inline(always)]
    pub fn needs_corner_cutout_rendering(&self) -> bool {
        self.event_listener().decoration.is_some()
    }

    #[inline(always)]
    pub fn client_size(&self) -> Size<LogicalUnit> {
        self.state()
            .committed_state
            .lock()
            .expect("poisoned")
            .active_size_logical
    }

    #[inline(always)]
    pub fn pixels_client_size(&self) -> Size<PixelsUnit> {
        self.state()
            .committed_state
            .lock()
            .expect("poisoned")
            .active_size
    }

    #[inline(always)]
    pub fn ui_scale_factor(&self) -> f32 {
        self.state()
            .committed_state
            .lock()
            .expect("poisoned")
            .active_buffer_scale
    }

    #[inline(always)]
    pub fn keyboard_focus_state(&self) -> &PerWindowKeyboardFocusState {
        &self.state().keyboard_focus_state
    }

    #[inline(always)]
    pub fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        &mut self.state_mut().keyboard_focus_state
    }

    #[inline(always)]
    pub fn keyboard_focus_group(&self) -> KeyboardFocusGroupRef {
        self.state().kf_root_group
    }

    #[inline(always)]
    pub(super) fn xdg_surface(&self) -> &wl::XdgSurface {
        &self.event_listener().state.data.xdg_surface
    }

    #[inline(always)]
    pub fn latest_ui_scale_changes(&self) -> &Mutex<Option<f32>> {
        &self.event_listener().state.data.latest_ui_scale_changes
    }

    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        self.state()
            .swapchain_externally_invalidation_signal
            .compare_exchange_weak(
                true,
                false,
                std::sync::atomic::Ordering::Relaxed,
                std::sync::atomic::Ordering::Relaxed,
            )
            == Ok(true)
    }

    pub fn on_click_sys_close_button(&self) {
        // TODO: 自身がMainかSubかでやることが変わる
        tracing::warn!("TODO: on_click_sys_close_button");
    }

    pub fn on_click_sys_maximize_button(&self) {
        self.state()
            .xdg_toplevel
            .set_maximized()
            .expect("xdg_toplevel.set_maximized");
    }

    pub fn on_click_sys_minimize_button(&self) {
        self.state()
            .xdg_toplevel
            .set_minimized()
            .expect("xdg_toplevel.set_maximized");
    }

    pub fn on_click_sys_restore_button(&self) {
        self.state()
            .xdg_toplevel
            .unset_maximized()
            .expect("xdg_toplevel.set_maximized");
    }

    pub fn begin_drag(&self, event_id: PointerEventID) {
        self.state()
            .xdg_toplevel
            .r#move(unsafe { &*event_id.seat_ptr }, event_id.serial)
            .expect("xdg_toplevel.move");
    }

    pub fn update_manual_scaling(&self) {
        let el = self.event_listener();
        if let SurfaceScaling::Manual { ref viewport, .. } = el.scaling {
            let committed_state = el.state.data.committed_state.lock().expect("poisoned");
            viewport
                .set_source(
                    wl::Fixed::from_f32_lossy(0.0),
                    wl::Fixed::from_f32_lossy(0.0),
                    wl::Fixed::from_f32_lossy(committed_state.active_size.width as _),
                    wl::Fixed::from_f32_lossy(committed_state.active_size.height as _),
                )
                .expect("viewport.set_source");
            viewport
                .set_destination(
                    committed_state.active_size_logical.width as _,
                    committed_state.active_size_logical.height as _,
                )
                .expect("viewport.set_destination");
        }
    }
}
impl crate::input::ShellPointerActions for Handle {
    #[inline(always)]
    fn capture_pointer(&self) {
        // Waylandはなし(勝手にキャプチャ状態になってるらしい)
    }

    #[inline(always)]
    fn release_pointer(&self) {
        // Waylandはなし(勝手にキャプチャ状態になってるらしい)
    }
}
impl crate::uikit::MountTarget for Handle {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.event_listener().state.data.composite_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.event_listener().state.data.ht_root
    }
}

struct CommittedState {
    active_buffer_scale: f32,
    active_size: Size<PixelsUnit>,
    active_size_logical: Size<LogicalUnit>,
    maximized: bool,
}

struct InstanceState {
    surface_ptr: NonNull<wl::Surface>,
    xdg_surface: wl::Owned<wl::XdgSurface>,
    xdg_toplevel: wl::Owned<wl::XdgToplevel>,
    _deco: Option<wl::Owned<wl::ZxdgToplevelDecorationV1>>,
    _appmenu: Option<wl::Owned<wl::OrgKdeKwinAppmenu>>,
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    extra_data: *mut core::ffi::c_void,
    committed_state: Mutex<CommittedState>,
    swapchain_externally_invalidation_signal: AtomicBool,
    latest_ui_scale_changes: Mutex<Option<f32>>,
    keyboard_focus_state: PerWindowKeyboardFocusState,
    kf_root_group: KeyboardFocusGroupRef,
}
unsafe impl Sync for InstanceState {}
unsafe impl Send for InstanceState {}

#[repr(C)] // place state at 0 always: WindowEventListener can be reinterpreted as SurfaceState
struct EventListener {
    state: SurfaceState<InstanceState>,
    window_type: WindowType,
    scaling: SurfaceScaling,
    decoration: Option<Pin<Box<Decoration>>>,
    needs_system_command_buttons: bool,
    pending_configure_size: (Option<i32>, Option<i32>),
    pending_configure_buffer_scale: Option<f32>,
    pending_activated_changes: Option<bool>,
    pending_maximized_changes: Option<bool>,
    event_dispatcher: LogicFiberEventDispatcher,
}
impl wl::SurfaceEventListener for EventListener {
    #[tracing::instrument(name = "wl_surface::enter", skip(self, _surface, _output))]
    fn enter(&mut self, _surface: &mut wl::Surface, _output: &mut wl::Output) {
        super::event_trace!();
    }

    #[tracing::instrument(name = "wl_surface::leave", skip(self, _surface, _output))]
    fn leave(&mut self, _surface: &mut wl::Surface, _output: &mut wl::Output) {
        super::event_trace!();
    }

    #[tracing::instrument(name = "wl_surface::preferred_buffer_scale", skip(self, _surface), fields(has_fractional_scale_support = self.scaling.is_manual()))]
    fn preferred_buffer_scale(&mut self, _surface: &mut wl::Surface, factor: i32) {
        super::event_trace!();

        if self.scaling.is_manual() {
            return;
        }

        self.pending_configure_buffer_scale = Some(factor as _);
    }

    #[tracing::instrument(name = "wl_surface::preferred_buffer_transform", skip(self, _surface))]
    fn preferred_buffer_transform(&mut self, _surface: &mut wl::Surface, transform: u32) {
        super::event_trace!();
    }
}
impl wl::XdgSurfaceEventListener for EventListener {
    #[tracing::instrument(name = "xdg_surface::configure", skip(self, sender))]
    fn configure(&mut self, sender: &mut wl::XdgSurface, serial: u32) {
        super::event_trace!();

        self.commit();
        sender
            .ack_configure(serial)
            .expect("xdg_surface.ack_configure");
    }
}
impl wl::XdgToplevelEventListener for EventListener {
    #[tracing::instrument(name = "xdg_toplevel::close", skip(self, _sender))]
    fn close(&mut self, _sender: &mut wl::XdgToplevel) {
        super::event_trace!();

        match self.window_type {
            WindowType::Main {
                ref termination_event,
            } => {
                termination_event.inc(1).expect("termination_event.inc");
            }
            WindowType::Sub => {
                self.event_dispatcher.dispatch(Event::SubWindowClose {
                    window: Handle(self.state.data.surface_ptr),
                });
            }
        }
    }

    #[tracing::instrument(name = "xdg_toplevel::configure", skip(self, _sender), fields(states = ?unsafe { states.as_slice::<wl::XdgToplevelState>() }))]
    fn configure(
        &mut self,
        _sender: &mut wl::XdgToplevel,
        width: i32,
        height: i32,
        states: &mut wl::ffi::Array,
    ) {
        super::event_trace!();

        let states = unsafe { states.as_slice::<wl::XdgToplevelState>() };

        self.pending_configure_size = (
            if width == 0 {
                self.pending_configure_size.0
            } else {
                Some(width)
            },
            if height == 0 {
                self.pending_configure_size.1
            } else {
                Some(height)
            },
        );
        if let Some(ref d) = self.decoration {
            if states.contains(&wl::XdgToplevelState::Maximized) {
                d.hide();
            } else {
                d.show();

                if states.contains(&wl::XdgToplevelState::Activated) {
                    d.active();
                } else {
                    d.inactive();
                }
            }
        }

        self.pending_activated_changes = Some(states.contains(&wl::XdgToplevelState::Activated));
        self.pending_maximized_changes = Some(states.contains(&wl::XdgToplevelState::Maximized));
    }

    #[tracing::instrument(name = "xdg_toplevel::configure_bounds", skip(self, _sender))]
    fn configure_bounds(&mut self, _sender: &mut wl::XdgToplevel, width: i32, height: i32) {
        super::event_trace!();
    }

    #[tracing::instrument(name = "xdg_toplevel::wm_capabilities", skip(self, _sender), fields(capabilities = ?unsafe { capabilities.as_slice::<wl::XdgToplevelWmCapabilities>() }))]
    fn wm_capabilities(
        &mut self,
        _sender: &mut wl::XdgToplevel,
        capabilities: &mut wl::ffi::Array,
    ) {
        super::event_trace!();
    }
}
impl wl::ZxdgToplevelDecorationV1EventListener for EventListener {
    #[tracing::instrument(name = "zxdg_toplevel_decoration_v1::configure", skip(self, _sender))]
    fn configure(
        &mut self,
        _sender: &mut wl::ZxdgToplevelDecorationV1,
        mode: wl::ZxdgToplevelDecorationV1Mode,
    ) {
        super::event_trace!();

        match mode {
            wl::ZxdgToplevelDecorationV1Mode::ClientSide => {
                tracing::warn!("TODO: client side decoration impl");
            }
            wl::ZxdgToplevelDecorationV1Mode::ServerSide => {
                tracing::warn!("server side decoration?");
            }
        }
    }
}
impl wl::WpFractionalScaleV1EventListener for EventListener {
    #[tracing::instrument(name = "wp_fractional_scale_v1::preferred_scale", skip(self, _sender))]
    fn preferred_scale(&mut self, _sender: &mut wl::WpFractionalScaleV1, scale: u32) {
        super::event_trace!();

        self.pending_configure_buffer_scale = Some(scale as f32 / 120.0);
    }
}
impl EventListener {
    fn commit(&mut self) {
        let mut delayed_event_queue = Vec::with_capacity(8);

        let mut committed_state_ref = self.state.data.committed_state.lock().expect("poisoned");
        let mut rescaled = false;
        if let Some(s) = self.pending_configure_buffer_scale.take() {
            match self.scaling {
                SurfaceScaling::Automatic => {
                    unsafe { self.state.data.surface_ptr.as_ref() }
                        .set_buffer_scale(s as _)
                        .expect("wl_surface.set_buffer_scale");
                }
                SurfaceScaling::Manual { .. } => {
                    // fractional scaleでは1固定にして、viewporterでスケールを適用する必要がある
                    unsafe { self.state.data.surface_ptr.as_ref() }
                        .set_buffer_scale(1)
                        .expect("wl_surface.set_buffer_scale");
                }
            }

            committed_state_ref.active_buffer_scale = s;
            delayed_event_queue.push(Event::WindowRescaleUI {
                window: Handle(self.state.data.surface_ptr),
                new_scale: s,
            });
            *self
                .state
                .data
                .latest_ui_scale_changes
                .lock()
                .expect("poisoned") = Some(s);
            rescaled = true;
        }

        let (w, h) = (
            self.pending_configure_size.0.take(),
            self.pending_configure_size.1.take(),
        );
        if rescaled || w.is_some() || h.is_some() {
            // potentially size changes
            let logical_size = Size::new_logical(
                w.map_or(committed_state_ref.active_size_logical.width, |x| x as _),
                h.map_or(committed_state_ref.active_size_logical.height, |y| y as _),
            );
            let pixels_size = logical_size.to_pixels_ceil(committed_state_ref.active_buffer_scale);
            if pixels_size != committed_state_ref.active_size {
                self.state
                    .data
                    .xdg_surface
                    .set_window_geometry(0, 0, logical_size.width as _, logical_size.height as _)
                    .expect("xdg_surface.set_window_geometry");

                if let Some(ref d) = self.decoration {
                    d.adjust_for_frame(logical_size.width as _, logical_size.height as _);
                }

                committed_state_ref.active_size = pixels_size;
                committed_state_ref.active_size_logical = logical_size;
                self.state
                    .data
                    .swapchain_externally_invalidation_signal
                    .store(true, std::sync::atomic::Ordering::Relaxed);

                delayed_event_queue.push(Event::WindowResize {
                    window: Handle(self.state.data.surface_ptr),
                    size: logical_size,
                });
            }
        }

        if let Some(new_maximized) = self.pending_maximized_changes.take()
            && new_maximized != committed_state_ref.maximized
        {
            committed_state_ref.maximized = new_maximized;
            delayed_event_queue.push(Event::WindowMaximizeStateChanged {
                window: Handle(self.state.data.surface_ptr),
                is_maximized: new_maximized,
            });
        }
        drop(committed_state_ref);

        if let Some(ref d) = self.decoration {
            d.commit_all();
        }

        if self.pending_activated_changes.take() == Some(false) {
            // window deactivated
            delayed_event_queue.push(Event::MenuCloseAll);
        }

        for x in delayed_event_queue {
            self.event_dispatcher.dispatch(x);
        }
    }
}

#[inline(always)]
pub(super) const fn should_client_decoration(wl_interfaces: &GlobalInterfaces) -> bool {
    wl_interfaces.zxdg_decoration_manager.is_some()
}

pub(super) struct NativeWindow {
    surface: wl::Owned<wl::Surface>,
}
impl Drop for NativeWindow {
    #[inline(always)]
    fn drop(&mut self) {
        panic!("Window::drop called unexpectedly!");
    }
}
impl NativeWindow {
    #[inline(always)]
    pub fn make_handle(&self) -> Handle {
        Handle(unsafe { self.surface.copy_ptr() })
    }

    #[inline(always)]
    pub const fn into_handle(self) -> Handle {
        let surface = unsafe { core::ptr::read(&self.surface) };
        core::mem::forget(self);

        Handle(surface.unwrap())
    }

    #[inline(always)]
    pub const fn from_handle(h: Handle) -> Self {
        Self {
            surface: unsafe { wl::Owned::wrap_unchecked(h.0) },
        }
    }

    pub fn new<E>(
        r#type: WindowType,
        dpsv: &DisplayServerContext,
        dbus: &dbus::Connection,
        event_dispatcher: LogicFiberEventDispatcher,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
        deco_pixbuf: Option<&DecorationPixbuf>,
        vk_device: &VulkanDevice,
        rt_sender: &RenderMessageSender,
    ) -> Self {
        let mut surface = dpsv
            .global_interfaces
            .compositor
            .create_surface()
            .expect("wl_surface create");
        let xdg_surface = dpsv
            .global_interfaces
            .xdg_wm_base
            .get_xdg_surface(&surface)
            .expect("xdg_surface create");
        let xdg_toplevel = xdg_surface.get_toplevel().expect("xdg_toplevel create");
        xdg_toplevel
            .set_title(c"Peridot Marble Editor")
            .expect("xdg_toplevel.set_title");
        xdg_surface
            .set_window_geometry(0, 0, 640, 480)
            .expect("xdg_surface.set_window_geometry");

        let appmenu = if let Some(ref am) = dpsv.global_interfaces.kde_appmenu_manager {
            let a = am.create(&surface).expect("appmenu.create");
            a.set_address(dbus.unique_name().expect("no name"), APPMENU_OBJECT_PATH)
                .expect("appmenu.set_address");

            Some(a)
        } else {
            None
        };

        // memo: HyprlandのViewporterはsrcの座標範囲の判定が間違っているので特殊判定して影を出さないようにする
        let (deco, decoration) = if let Some(ref dm) =
            dpsv.global_interfaces.zxdg_decoration_manager
            && !dpsv.global_interfaces.is_hyprland
        {
            let d = dm
                .get_toplevel_decoration(&xdg_toplevel)
                .expect("decoration.get_toplevel");
            d.set_mode(wl::ZxdgToplevelDecorationV1Mode::ClientSide)
                .expect("decoration.set_mode");

            (
                Some(d),
                Some(Decoration::new(
                    &dpsv.global_interfaces,
                    deco_pixbuf.expect("pixbuf required"),
                    &surface,
                    &xdg_toplevel,
                )),
            )
        } else {
            (None, None)
        };

        let mut window_scaling =
            if let Some(ref fs) = dpsv.global_interfaces.fractional_scale_manager {
                let f = fs
                    .get_fractional_scale(&surface)
                    .expect("fractional_scale.create");
                let vp = dpsv
                    .global_interfaces
                    .viewporter
                    .get_viewport(&surface)
                    .expect("viewporter.get_viewport");

                SurfaceScaling::Manual {
                    fractional_scale: f,
                    viewport: vp,
                }
            } else {
                SurfaceScaling::Automatic
            };
        let fractional_scale_ptr = if let SurfaceScaling::Manual {
            ref mut fractional_scale,
            ..
        } = window_scaling
        {
            fractional_scale.as_ptr()
        } else {
            core::ptr::null_mut()
        };

        let composite_root = composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            ..Default::default()
        });
        let ht_root = ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            root_of_window: Some(Handle::from_mut(&mut surface)),
            ..Default::default()
        });
        let kf_root_group = keyboard_focus_registry.acquire_group();

        let xdg_surface_ptr = xdg_surface.as_ptr();
        let xdg_toplevel_ptr = xdg_toplevel.as_ptr();
        let deco_ptr = deco.as_ref().map(wl::Owned::as_ptr);
        let mut event_listener = Box::new(EventListener {
            state: SurfaceState {
                tag: SurfaceStateTag::ToplevelWindow,
                data: InstanceState {
                    surface_ptr: unsafe { NonNull::new_unchecked(surface.as_ptr()) },
                    xdg_surface,
                    xdg_toplevel,
                    _appmenu: appmenu,
                    _deco: deco,
                    composite_root,
                    ht_root,
                    extra_data: core::ptr::null_mut(),
                    committed_state: Mutex::new(CommittedState {
                        active_buffer_scale: 1.0,
                        active_size: Size::new_pixels(640, 480),
                        active_size_logical: Size::new_logical(640.0, 480.0),
                        maximized: false,
                    }),
                    swapchain_externally_invalidation_signal: std::sync::atomic::AtomicBool::new(
                        false,
                    ),
                    latest_ui_scale_changes: Mutex::new(None),
                    keyboard_focus_state: PerWindowKeyboardFocusState::new(kf_root_group),
                    kf_root_group,
                },
            },
            window_type: r#type,
            scaling: window_scaling,
            needs_system_command_buttons: decoration.is_some(),
            decoration,
            pending_configure_size: (None, None),
            pending_configure_buffer_scale: None,
            pending_activated_changes: None,
            pending_maximized_changes: None,
            event_dispatcher,
        });
        surface
            .set_listener(event_listener.as_mut())
            .into_result()
            .expect("wl_surface set listener");
        unsafe { &mut *xdg_surface_ptr }
            .set_listener(event_listener.as_mut())
            .into_result()
            .expect("xdg_surface set listener");
        unsafe { &mut *xdg_toplevel_ptr }
            .set_listener(event_listener.as_mut())
            .into_result()
            .expect("xdg_toplevel set listener");
        if let Some(x) = deco_ptr {
            unsafe { &mut *x }
                .set_listener(event_listener.as_mut())
                .into_result()
                .expect("zxdg_toplevel_decoration_v1.set_listener");
        }
        if !fractional_scale_ptr.is_null() {
            unsafe {
                (*fractional_scale_ptr)
                    .set_listener(event_listener.as_mut())
                    .into_result()
                    .expect("wp_fractional_scale_v1.set_listener");
            }
        }
        // owns EventListener in wl_surface
        surface.set_user_data(Box::into_raw(event_listener).cast());

        // commits initial state
        surface.commit().expect("wl_surface.commit");

        // ready for rendering
        rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                key: Handle::from_mut(&mut surface),
                vk_surface: NewWindowVulkanSurface(
                    VulkanSurface::new(vk_device, unsafe {
                        br::WaylandSurfaceCreateInfo::new(
                            dpsv.dp.as_raw().cast(),
                            surface.as_raw().cast(),
                        )
                        .execute(vk_device.instance(), None)
                        .expect("vk_surface.create")
                    })
                    .unbound()
                    .1,
                ),
            }))
            .expect("rt_sender.send");

        Self { surface }
    }

    pub fn terminate<E>(
        mut self,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        let e = unsafe { Box::from_raw(self.surface.user_data().cast::<EventListener>()) };
        keyboard_focus_registry.release_group(e.state.data.kf_root_group);
        composite_tree.free_all(e.state.data.composite_root);
        ht_manager.free_all(e.state.data.ht_root);
        drop(e);

        unsafe {
            core::ptr::drop_in_place(&mut self.surface);
        }
        core::mem::forget(self);
    }

    pub fn commit(&self) {
        if let &Some(ref d) =
            unsafe { &(*self.surface.user_data().cast::<EventListener>()).decoration }
        {
            d.commit_all();
        }
        self.surface.commit().expect("wl_surface.commit");
    }
}

pub(super) struct DecorationPixbuf {
    buffer_corner: wl::Owned<wl::Buffer>,
    buffer_edge: wl::Owned<wl::Buffer>,
}
impl DecorationPixbuf {
    // (corner(size * size) + edge(size * 1)) * 4(bytes per pixel)
    pub const REQUIRED_BYTE_LENGTH: usize =
        Decoration::SIZE as usize * (Decoration::SIZE as usize + 1) * 4;
    pub const REQUIRED_BYTE_ALIGNMENT: usize = 4;

    pub fn generate_content(head_ptr: *mut core::ffi::c_void) {
        for x in 0..Decoration::SIZE {
            let a = (1.0 - x as f32 / Decoration::SIZE as f32).powi(2);
            let v = (a * 255.0) as u32;

            unsafe {
                core::ptr::write(head_ptr.byte_add(x as usize * 4).cast::<u32>(), v << 24);
            }
        }
        for x in 0..Decoration::SIZE {
            for y in 0..Decoration::SIZE {
                let d = ((x as f32 / Decoration::SIZE as f32).powi(2)
                    + (y as f32 / Decoration::SIZE as f32).powi(2))
                .sqrt();
                let a = (1.0 - d).clamp(0.0, 1.0).powi(2);
                let v = (a * 255.0) as u32;

                unsafe {
                    core::ptr::write(
                        head_ptr
                            .byte_add((x * Decoration::SIZE + y) as usize * 4)
                            .cast::<u32>(),
                        v << 24,
                    );
                }
            }
        }
    }

    pub fn new(shm_pool: &wl::ShmPool, offset: usize) -> Self {
        let buffer_edge = shm_pool
            .create_buffer(
                offset as _,
                Decoration::SIZE as _,
                1,
                Decoration::SIZE as i32 * 4,
                wl::ShmFormat::ARGB8888,
            )
            .expect("shm_pool.create_buffer");
        let buffer_corner = shm_pool
            .create_buffer(
                (offset + Decoration::SIZE as usize * 4) as _,
                Decoration::SIZE as _,
                Decoration::SIZE as _,
                Decoration::SIZE as i32 * 4,
                wl::ShmFormat::ARGB8888,
            )
            .expect("shm_pool.create_buffer");

        Self {
            buffer_corner,
            buffer_edge,
        }
    }
}

struct DecorationCornerSurface {
    surface: wl::Owned<wl::Surface>,
    subsurface: wl::Owned<wl::Subsurface>,
    alpha_modifier: Option<wl::Owned<wl::WpAlphaModifierSurfaceV1>>,
}
impl DecorationCornerSurface {
    fn new(wl_interfaces: &GlobalInterfaces, parent_surface: &wl::Surface) -> Self {
        let surface = wl_interfaces
            .compositor
            .create_surface()
            .expect("compositor.create_surface");
        let subsurface = wl_interfaces
            .subcompositor
            .get_subsurface(&surface, parent_surface)
            .expect("compositor.get_surface");
        let alpha_modifier = wl_interfaces.alpha_modifier.as_ref().map(|alpha_modifier| {
            alpha_modifier
                .get_surface(&surface)
                .expect("alpha_modifier.get_surface")
        });

        Self {
            surface,
            subsurface,
            alpha_modifier,
        }
    }

    fn set_input_rect(
        &self,
        compositor: &wl::Compositor,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) -> wl::Result<()> {
        let r = compositor.create_region()?;
        r.add(x, y, width, height)?;
        self.surface.set_input_region(Some(&r))?;

        Ok(())
    }

    fn active(&self, buffer: &wl::Buffer) {
        if let Some(ref a) = self.alpha_modifier {
            a.set_multiplier(u32::MAX)
                .expect("alpha_modifier.set_multiplier");
        } else {
            self.surface
                .attach(Some(buffer), 0, 0)
                .expect("surface.attach");
        }
    }

    fn inactive(&self) {
        if let Some(ref a) = self.alpha_modifier {
            a.set_multiplier(u32::MAX >> 1)
                .expect("alpha_modifier.set_multiplier");
        } else {
            self.surface.attach(None, 0, 0).expect("surface.attach");
        }
    }

    fn show(&self, buffer: &wl::Buffer) {
        self.surface
            .attach(Some(buffer), 0, 0)
            .expect("surface.attach");
    }

    fn hide(&self) {
        self.surface.attach(None, 0, 0).expect("surface.attach");
    }
}

struct DecorationEdgeSurface {
    surface: wl::Owned<wl::Surface>,
    subsurface: wl::Owned<wl::Subsurface>,
    viewport: wl::Owned<wl::WpViewport>,
    alpha_modifier: Option<wl::Owned<wl::WpAlphaModifierSurfaceV1>>,
}
impl DecorationEdgeSurface {
    fn new(wl_interfaces: &GlobalInterfaces, parent_surface: &wl::Surface) -> Self {
        let surface = wl_interfaces
            .compositor
            .create_surface()
            .expect("compositor.create_surface");
        let subsurface = wl_interfaces
            .subcompositor
            .get_subsurface(&surface, parent_surface)
            .expect("compositor.get_surface");
        let viewport = wl_interfaces
            .viewporter
            .get_viewport(&surface)
            .expect("viewporter.get_viewport");
        let alpha_modifier = wl_interfaces.alpha_modifier.as_ref().map(|alpha_modifier| {
            alpha_modifier
                .get_surface(&surface)
                .expect("alpha_modifier.get_surface")
        });

        Self {
            surface,
            subsurface,
            viewport,
            alpha_modifier,
        }
    }

    fn set_input_rect(
        &self,
        compositor: &wl::Compositor,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) -> wl::Result<()> {
        let r = compositor.create_region()?;
        r.add(x, y, width, height)?;
        self.surface.set_input_region(Some(&r))?;

        Ok(())
    }

    fn active(&self, buffer: &wl::Buffer) {
        if let Some(ref a) = self.alpha_modifier {
            a.set_multiplier(u32::MAX)
                .expect("alpha_modifier.set_multiplier");
        } else {
            self.surface
                .attach(Some(buffer), 0, 0)
                .expect("surface.attach");
        }
    }

    fn inactive(&self) {
        if let Some(ref a) = self.alpha_modifier {
            a.set_multiplier(u32::MAX >> 1)
                .expect("alpha_modifier.set_multiplier");
        } else {
            self.surface.attach(None, 0, 0).expect("surface.attach");
        }
    }

    fn show(&self, buffer: &wl::Buffer) {
        self.surface
            .attach(Some(buffer), 0, 0)
            .expect("surface.attach");
    }

    fn hide(&self) {
        self.surface.attach(None, 0, 0).expect("surface.attach");
    }
}

struct Decoration {
    compositor_ptr: *const wl::Compositor,
    pixbuf_ptr: *const DecorationPixbuf,
    left: DecorationEdgeSurface,
    right: DecorationEdgeSurface,
    top: DecorationEdgeSurface,
    bottom: DecorationEdgeSurface,
    lt: DecorationCornerSurface,
    rt: DecorationCornerSurface,
    lb: DecorationCornerSurface,
    rb: DecorationCornerSurface,
    _left_data: SurfaceState<ResizeEdgeSurfaceData>,
    _right_data: SurfaceState<ResizeEdgeSurfaceData>,
    _top_data: SurfaceState<ResizeEdgeSurfaceData>,
    _bottom_data: SurfaceState<ResizeEdgeSurfaceData>,
    _lt_data: SurfaceState<ResizeEdgeSurfaceData>,
    _rt_data: SurfaceState<ResizeEdgeSurfaceData>,
    _lb_data: SurfaceState<ResizeEdgeSurfaceData>,
    _rb_data: SurfaceState<ResizeEdgeSurfaceData>,
    _pinned: core::marker::PhantomPinned,
}
impl Decoration {
    const SIZE: u32 = 64;
    const INTERACT_SIZE: u32 = 8;
    const INSET: u32 = 32;
    const SHIFT_DOWN_AMOUNT: u32 = 4;

    fn new(
        wl_interfaces: &GlobalInterfaces,
        pixbuf: &DecorationPixbuf,
        parent_surface: &wl::Surface,
        target_toplevel: &wl::XdgToplevel,
    ) -> Pin<Box<Self>> {
        // construct
        let left = DecorationEdgeSurface::new(wl_interfaces, parent_surface);
        let right = DecorationEdgeSurface::new(wl_interfaces, parent_surface);
        let top = DecorationEdgeSurface::new(wl_interfaces, parent_surface);
        let bottom = DecorationEdgeSurface::new(wl_interfaces, parent_surface);
        let lt = DecorationCornerSurface::new(wl_interfaces, parent_surface);
        let rt = DecorationCornerSurface::new(wl_interfaces, parent_surface);
        let lb = DecorationCornerSurface::new(wl_interfaces, parent_surface);
        let rb = DecorationCornerSurface::new(wl_interfaces, parent_surface);

        tracing::debug!(
            left = left.viewport.id(),
            right = right.viewport.id(),
            top = top.viewport.id(),
            bottom = bottom.viewport.id()
        );

        // attach appropriate buffer
        left.surface
            .attach(Some(&pixbuf.buffer_edge), 0, 0)
            .expect("surface.attach");
        right
            .surface
            .attach(Some(&pixbuf.buffer_edge), 0, 0)
            .expect("surface.attach");
        top.surface
            .attach(Some(&pixbuf.buffer_edge), 0, 0)
            .expect("surface.attach");
        bottom
            .surface
            .attach(Some(&pixbuf.buffer_edge), 0, 0)
            .expect("surface.attach");
        lt.surface
            .attach(Some(&pixbuf.buffer_corner), 0, 0)
            .expect("surface.attach");
        rt.surface
            .attach(Some(&pixbuf.buffer_corner), 0, 0)
            .expect("surface.attach");
        lb.surface
            .attach(Some(&pixbuf.buffer_corner), 0, 0)
            .expect("surface.attach");
        rb.surface
            .attach(Some(&pixbuf.buffer_corner), 0, 0)
            .expect("surface.attach");

        // viewport fixed setup
        left.viewport
            .set_source(
                wl::Fixed::ZERO,
                wl::Fixed::ZERO,
                wl::Fixed::from_f32_lossy(Self::SIZE as _),
                wl::Fixed::ONE,
            )
            .expect("viewport.set_source");
        right
            .viewport
            .set_source(
                wl::Fixed::ZERO,
                wl::Fixed::ZERO,
                wl::Fixed::from_f32_lossy(Self::SIZE as _),
                wl::Fixed::ONE,
            )
            .expect("viewport.set_source");
        top.viewport
            .set_source(
                wl::Fixed::ZERO,
                wl::Fixed::ZERO,
                wl::Fixed::ONE,
                wl::Fixed::from_f32_lossy(Self::SIZE as _),
            )
            .expect("viewport.set_source");
        bottom
            .viewport
            .set_source(
                wl::Fixed::ZERO,
                wl::Fixed::ZERO,
                wl::Fixed::ONE,
                wl::Fixed::from_f32_lossy(Self::SIZE as _),
            )
            .expect("viewport.set_source");

        // apply rotation
        top.surface
            .set_buffer_transform(wl::OutputTransform::Rot270)
            .expect("surface.set_buffer_transform");
        left.surface
            .set_buffer_transform(wl::OutputTransform::Flipped)
            .expect("surface.set_buffer_transform");
        bottom
            .surface
            .set_buffer_transform(wl::OutputTransform::Rot90)
            .expect("surface.set_buffer_transform");
        rt.surface
            .set_buffer_transform(wl::OutputTransform::Rot270)
            .expect("surface.set_buffer_transform");
        lt.surface
            .set_buffer_transform(wl::OutputTransform::Rot180)
            .expect("surface.set_buffer_transform");
        lb.surface
            .set_buffer_transform(wl::OutputTransform::Rot90)
            .expect("surface.set_buffer_transform");

        // placing
        left.subsurface
            .place_below(parent_surface)
            .expect("subsurface.place_below");
        right
            .subsurface
            .place_below(parent_surface)
            .expect("subsurface.place_below");
        top.subsurface
            .place_below(parent_surface)
            .expect("subsurface.place_below");
        bottom
            .subsurface
            .place_below(parent_surface)
            .expect("subsurface.place_below");
        lt.subsurface
            .place_below(parent_surface)
            .expect("subsurface.place_below");
        rt.subsurface
            .place_below(parent_surface)
            .expect("subsurface.place_below");
        lb.subsurface
            .place_below(parent_surface)
            .expect("subsurface.place_below");
        rb.subsurface
            .place_below(parent_surface)
            .expect("subsurface.place_below");

        // positioning(fixed)
        left.subsurface
            .set_position(
                -(Self::SIZE as i32) + Self::INSET as i32,
                Self::INSET as i32 + Self::SHIFT_DOWN_AMOUNT as i32,
            )
            .expect("subsurface.set_position");
        top.subsurface
            .set_position(
                Self::INSET as i32,
                -(Self::SIZE as i32) + Self::INSET as i32 + Self::SHIFT_DOWN_AMOUNT as i32,
            )
            .expect("subsurface.set_position");
        lt.subsurface
            .set_position(
                -(Self::SIZE as i32) + Self::INSET as i32,
                -(Self::SIZE as i32) + Self::INSET as i32 + Self::SHIFT_DOWN_AMOUNT as i32,
            )
            .expect("subsurface.set_position");

        // input region(fixed)
        lt.set_input_rect(
            &wl_interfaces.compositor,
            Self::SIZE as i32 - Self::INSET as i32 - Self::INTERACT_SIZE as i32,
            Self::SIZE as i32
                - Self::INSET as i32
                - Self::INTERACT_SIZE as i32
                - Self::SHIFT_DOWN_AMOUNT as i32,
            Self::INSET as i32 + Self::INTERACT_SIZE as i32,
            Self::INSET as i32 + Self::INTERACT_SIZE as i32 + Self::SHIFT_DOWN_AMOUNT as i32,
        )
        .expect("corner_surface.set_input_rect");
        lb.set_input_rect(
            &wl_interfaces.compositor,
            Self::SIZE as i32 - Self::INSET as i32 - Self::INTERACT_SIZE as i32,
            0,
            Self::INSET as i32 + Self::INTERACT_SIZE as i32,
            Self::INSET as i32 + Self::INTERACT_SIZE as i32 + Self::SHIFT_DOWN_AMOUNT as i32,
        )
        .expect("corner_surface.set_input_rect");
        rt.set_input_rect(
            &wl_interfaces.compositor,
            0,
            Self::SIZE as i32
                - Self::INSET as i32
                - Self::INTERACT_SIZE as i32
                - Self::SHIFT_DOWN_AMOUNT as i32,
            Self::INSET as i32 + Self::INTERACT_SIZE as i32,
            Self::INSET as i32 + Self::INTERACT_SIZE as i32 + Self::SHIFT_DOWN_AMOUNT as i32,
        )
        .expect("corner_surface.set_input_rect");
        rb.set_input_rect(
            &wl_interfaces.compositor,
            0,
            0,
            Self::INSET as i32 + Self::INTERACT_SIZE as i32,
            Self::INSET as i32 + Self::INTERACT_SIZE as i32 + Self::SHIFT_DOWN_AMOUNT as i32,
        )
        .expect("corner_surface.set_input_rect");

        let mut this = Box::new(Self {
            compositor_ptr: wl_interfaces.compositor.as_ptr(),
            pixbuf_ptr: pixbuf,
            left,
            right,
            top,
            bottom,
            lt,
            rt,
            lb,
            rb,
            _left_data: SurfaceState {
                tag: SurfaceStateTag::ResizeEdge,
                data: ResizeEdgeSurfaceData {
                    edge: wl::XdgToplevelResizeEdge::Left,
                    target_toplevel,
                },
            },
            _right_data: SurfaceState {
                tag: SurfaceStateTag::ResizeEdge,
                data: ResizeEdgeSurfaceData {
                    edge: wl::XdgToplevelResizeEdge::Right,
                    target_toplevel,
                },
            },
            _top_data: SurfaceState {
                tag: SurfaceStateTag::ResizeEdge,
                data: ResizeEdgeSurfaceData {
                    edge: wl::XdgToplevelResizeEdge::Top,
                    target_toplevel,
                },
            },
            _bottom_data: SurfaceState {
                tag: SurfaceStateTag::ResizeEdge,
                data: ResizeEdgeSurfaceData {
                    edge: wl::XdgToplevelResizeEdge::Bottom,
                    target_toplevel,
                },
            },
            _lt_data: SurfaceState {
                tag: SurfaceStateTag::ResizeEdge,
                data: ResizeEdgeSurfaceData {
                    edge: wl::XdgToplevelResizeEdge::TopLeft,
                    target_toplevel,
                },
            },
            _rt_data: SurfaceState {
                tag: SurfaceStateTag::ResizeEdge,
                data: ResizeEdgeSurfaceData {
                    edge: wl::XdgToplevelResizeEdge::TopRight,
                    target_toplevel,
                },
            },
            _lb_data: SurfaceState {
                tag: SurfaceStateTag::ResizeEdge,
                data: ResizeEdgeSurfaceData {
                    edge: wl::XdgToplevelResizeEdge::BottomLeft,
                    target_toplevel,
                },
            },
            _rb_data: SurfaceState {
                tag: SurfaceStateTag::ResizeEdge,
                data: ResizeEdgeSurfaceData {
                    edge: wl::XdgToplevelResizeEdge::BottomRight,
                    target_toplevel,
                },
            },
            _pinned: core::marker::PhantomPinned,
        });
        this.left
            .surface
            .set_user_data(&mut this._left_data as *mut _ as _);
        this.right
            .surface
            .set_user_data(&mut this._right_data as *mut _ as _);
        this.top
            .surface
            .set_user_data(&mut this._top_data as *mut _ as _);
        this.bottom
            .surface
            .set_user_data(&mut this._bottom_data as *mut _ as _);
        this.lt
            .surface
            .set_user_data(&mut this._lt_data as *mut _ as _);
        this.rt
            .surface
            .set_user_data(&mut this._rt_data as *mut _ as _);
        this.lb
            .surface
            .set_user_data(&mut this._lb_data as *mut _ as _);
        this.rb
            .surface
            .set_user_data(&mut this._rb_data as *mut _ as _);

        Box::into_pin(this)
    }

    fn adjust_for_frame(&self, parent_width: i32, parent_height: i32) {
        tracing::debug!("adjust_for_frame");

        // positioning
        let rp = parent_width - Self::INSET as i32;
        let bp = parent_height - Self::INSET as i32;
        self.right
            .subsurface
            .set_position(rp, Self::INSET as i32 + Self::SHIFT_DOWN_AMOUNT as i32)
            .expect("subsurface.set_position");
        self.bottom
            .subsurface
            .set_position(Self::INSET as i32, bp + Self::SHIFT_DOWN_AMOUNT as i32)
            .expect("subsurface.set_position");
        self.rt
            .subsurface
            .set_position(
                rp,
                -(Self::SIZE as i32) + Self::INSET as i32 + Self::SHIFT_DOWN_AMOUNT as i32,
            )
            .expect("subsurface.set_position");
        self.lb
            .subsurface
            .set_position(
                -(Self::SIZE as i32) + Self::INSET as i32,
                bp + Self::SHIFT_DOWN_AMOUNT as i32,
            )
            .expect("subsurface.set_position");
        self.rb
            .subsurface
            .set_position(rp, bp + Self::SHIFT_DOWN_AMOUNT as i32)
            .expect("subsurface.set_position");

        // sizing
        self.left
            .viewport
            .set_destination(Self::SIZE as _, parent_height - Self::INSET as i32 * 2)
            .expect("viewport.set_destination");
        self.right
            .viewport
            .set_destination(Self::SIZE as _, parent_height - Self::INSET as i32 * 2)
            .expect("viewport.set_destination");
        self.top
            .viewport
            .set_destination(parent_width - Self::INSET as i32 * 2, Self::SIZE as _)
            .expect("viewport.set_destination");
        self.bottom
            .viewport
            .set_destination(parent_width - Self::INSET as i32 * 2, Self::SIZE as _)
            .expect("viewport.set_destination");

        // adjsut input region
        self.left
            .set_input_rect(
                unsafe { &*self.compositor_ptr },
                Self::SIZE as i32 - Self::INSET as i32 - Self::INTERACT_SIZE as i32,
                0,
                Self::INTERACT_SIZE as i32,
                parent_height - Self::INSET as i32 * 2,
            )
            .expect("edge_surface.set_input_rect");
        self.right
            .set_input_rect(
                unsafe { &*self.compositor_ptr },
                Self::INSET as i32,
                0,
                Self::INTERACT_SIZE as i32,
                parent_height - Self::INSET as i32 * 2,
            )
            .expect("edge_surface.set_input_rect");
        self.top
            .set_input_rect(
                unsafe { &*self.compositor_ptr },
                0,
                Self::SIZE as i32
                    - Self::INSET as i32
                    - Self::INTERACT_SIZE as i32
                    - Self::SHIFT_DOWN_AMOUNT as i32,
                parent_width - Self::INSET as i32 * 2,
                Self::INTERACT_SIZE as i32,
            )
            .expect("edge_surface.set_input_rect");
        self.bottom
            .set_input_rect(
                unsafe { &*self.compositor_ptr },
                0,
                Self::INSET as i32 - Self::SHIFT_DOWN_AMOUNT as i32,
                parent_width - Self::INSET as i32 * 2,
                Self::INTERACT_SIZE as i32,
            )
            .expect("edge_surface.set_input_rect");
    }

    fn active(&self) {
        self.left.active(unsafe { &(*self.pixbuf_ptr).buffer_edge });
        self.right
            .active(unsafe { &(*self.pixbuf_ptr).buffer_edge });
        self.top.active(unsafe { &(*self.pixbuf_ptr).buffer_edge });
        self.bottom
            .active(unsafe { &(*self.pixbuf_ptr).buffer_edge });
        self.lt.active(unsafe { &(*self.pixbuf_ptr).buffer_corner });
        self.rt.active(unsafe { &(*self.pixbuf_ptr).buffer_corner });
        self.lb.active(unsafe { &(*self.pixbuf_ptr).buffer_corner });
        self.rb.active(unsafe { &(*self.pixbuf_ptr).buffer_corner });
    }

    fn inactive(&self) {
        self.left.inactive();
        self.right.inactive();
        self.top.inactive();
        self.bottom.inactive();
        self.lt.inactive();
        self.rt.inactive();
        self.lb.inactive();
        self.rb.inactive();
    }

    fn show(&self) {
        self.left.show(unsafe { &(*self.pixbuf_ptr).buffer_edge });
        self.right.show(unsafe { &(*self.pixbuf_ptr).buffer_edge });
        self.top.show(unsafe { &(*self.pixbuf_ptr).buffer_edge });
        self.bottom.show(unsafe { &(*self.pixbuf_ptr).buffer_edge });
        self.lt.show(unsafe { &(*self.pixbuf_ptr).buffer_corner });
        self.rt.show(unsafe { &(*self.pixbuf_ptr).buffer_corner });
        self.lb.show(unsafe { &(*self.pixbuf_ptr).buffer_corner });
        self.rb.show(unsafe { &(*self.pixbuf_ptr).buffer_corner });
    }

    fn hide(&self) {
        self.left.hide();
        self.right.hide();
        self.top.hide();
        self.bottom.hide();
        self.lt.hide();
        self.rt.hide();
        self.lb.hide();
        self.rb.hide();
    }

    fn commit_all(&self) {
        self.left.surface.commit().expect("surface.commit");
        self.right.surface.commit().expect("surface.commit");
        self.top.surface.commit().expect("surface.commit");
        self.bottom.surface.commit().expect("surface.commit");
        self.lt.surface.commit().expect("surface.commit");
        self.rt.surface.commit().expect("surface.commit");
        self.lb.surface.commit().expect("surface.commit");
        self.rb.surface.commit().expect("surface.commit");
    }
}

pub(super) struct ResizeEdgeSurfaceData {
    edge: wl::XdgToplevelResizeEdge,
    target_toplevel: *const wl::XdgToplevel,
}
impl ResizeEdgeSurfaceData {
    pub const fn cursor_shape(&self) -> wl::WpCursorShapeDeviceV1Shape {
        match self.edge {
            wl::XdgToplevelResizeEdge::Top | wl::XdgToplevelResizeEdge::Bottom => {
                wl::WpCursorShapeDeviceV1Shape::NsResize
            }
            wl::XdgToplevelResizeEdge::Left | wl::XdgToplevelResizeEdge::Right => {
                wl::WpCursorShapeDeviceV1Shape::EwResize
            }
            wl::XdgToplevelResizeEdge::TopLeft | wl::XdgToplevelResizeEdge::BottomRight => {
                wl::WpCursorShapeDeviceV1Shape::NwseResize
            }
            wl::XdgToplevelResizeEdge::TopRight | wl::XdgToplevelResizeEdge::BottomLeft => {
                wl::WpCursorShapeDeviceV1Shape::NeswResize
            }
            wl::XdgToplevelResizeEdge::None => wl::WpCursorShapeDeviceV1Shape::Default,
        }
    }

    #[inline(always)]
    pub fn perform_resize(&self, seat: &wl::Seat, serial: u32) {
        unsafe {
            (*self.target_toplevel)
                .resize(seat, serial, self.edge)
                .expect("toplevel.resize");
        }
    }
}
