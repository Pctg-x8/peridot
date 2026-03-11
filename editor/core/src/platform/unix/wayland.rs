use std::{
    collections::HashMap,
    sync::{Arc, Mutex, atomic::AtomicBool},
};

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};
use peridot_tp_dbus as dbus;
use peridot_tp_wayland as wl;
use peridot_tp_xkbcommon as xkbcommon;

use crate::{
    Event, LogicFiberEventDispatcher, WindowType,
    graphics::{VulkanDevice, VulkanSurface},
    input::{
        PointerInputUnit,
        hittest::{
            CursorShape, HitTestTreeCreate, HitTestTreeData, HitTestTreeManager, HitTestTreeRef,
        },
    },
    rendering::{
        NewWindowData, NewWindowVulkanSurface, RenderMessage,
        composite::{CompositeRect, CompositeTree, CompositeTreeRef},
    },
    utils::{LogicalUnit, PixelsUnit, Point, Size},
};

pub const APPMENU_OBJECT_PATH: &core::ffi::CStr = c"/AppMenu";

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct WindowHandle(*mut wl::Surface);
unsafe impl Send for WindowHandle {}
unsafe impl Sync for WindowHandle {}
impl WindowHandle {
    #[inline(always)]
    pub fn state(&self) -> &WindowState {
        unsafe { &*(*self.0).user_data().cast() }
    }

    #[inline(always)]
    fn state_mut(&mut self) -> &mut WindowState {
        unsafe { &mut *(*self.0).user_data().cast() }
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
    pub fn client_size(&self) -> Size<LogicalUnit> {
        self.state()
            .committed_state
            .lock()
            .expect("poisoned")
            .active_size_logical
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
    pub fn composite_root(&self) -> CompositeTreeRef {
        self.state().composite_root
    }

    #[inline(always)]
    pub fn ht_root(&self) -> HitTestTreeRef {
        self.state().ht_root
    }
}
impl crate::ShellPointerActions for WindowHandle {
    #[inline(always)]
    fn capture_pointer(&self) {
        // Waylandはなし(勝手にキャプチャ状態になってるらしい)
    }

    #[inline(always)]
    fn release_pointer(&self) {
        // Waylandはなし(勝手にキャプチャ状態になってるらしい)
    }
}

pub struct DragPreviewPopoverHandle {
    pub display: *mut wl::Display,
    pub wl_interfaces: *const GlobalInterfaces,
    pub root_window: core::cell::Cell<*mut wl::XdgSurface>,
    pub buf: DragPreviewPopoverBuffer,
    pub popup: core::cell::UnsafeCell<
        Option<(
            Option<wl::Owned<wl::OrgKdeKwinBlur>>,
            wl::Owned<wl::XdgPopup>,
            wl::Owned<wl::XdgSurface>,
            wl::Owned<wl::WpViewport>,
            wl::Owned<wl::Surface>,
            Box<PopupState>,
        )>,
    >,
}
impl DragPreviewPopoverHandle {
    pub fn bind_parent_window(&self, window: WindowHandle) {
        self.root_window.set(window.state().xdg_surface_ptr);
    }

    pub fn show(&self, pos: &Point<PointerInputUnit>, size: &Size<LogicalUnit>) {
        let wl_popup_surface = unsafe {
            (*self.wl_interfaces)
                .compositor
                .create_surface()
                .expect("wl_popup_surface.create")
        };
        let mut xdg_popup_surface = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .get_xdg_surface(&wl_popup_surface)
                .expect("xdg_popup_surface.create")
        };

        let positioner = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .create_positioner()
                .expect("pos.create")
        };
        positioner
            .set_size(size.width as _, size.height as _)
            .expect("pos.set_size");
        positioner
            .set_offset(pos.x as _, pos.y as _)
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
        let mut pp = unsafe {
            xdg_popup_surface
                .get_popup(Some(&*self.root_window.get()), &positioner)
                .expect("pop.create")
        };
        let mut popup_state = Box::new(PopupState {
            surface_ptr: wl_popup_surface.as_ptr(),
        });
        xdg_popup_surface
            .set_listener(&mut *popup_state)
            .into_result()
            .expect("xdg_popup_surface.set_listener");
        pp.set_listener(&mut *popup_state)
            .into_result()
            .expect("pop.set_listener");
        wl_popup_surface.commit().expect("wl_popup_surface.commit");
        unsafe {
            // process configure event...(Kwinとかはconfigureくるまえにattachするとエラーが出ておちる)
            (*self.display).roundtrip().expect("roundtrip");
        }

        wl_popup_surface
            .attach(Some(self.buf.buffer()), 0, 0)
            .expect("wl_popup_surface.attach");
        wl_popup_surface
            .damage(0, 0, -1, -1)
            .expect("wl_popup_surface.damage");
        let viewport = unsafe {
            (*self.wl_interfaces)
                .viewporter
                .get_viewport(&wl_popup_surface)
                .expect("popup_viewport.create")
        };
        viewport
            .set_source(
                wl::Fixed::from_f32_lossy(0.0),
                wl::Fixed::from_f32_lossy(0.0),
                wl::Fixed::from_f32_lossy(1.0),
                wl::Fixed::from_f32_lossy(1.0),
            )
            .expect("viewport.set_source");
        viewport
            .set_destination(size.width as _, size.height as _)
            .expect("viewport.set_destination");

        let blur = if let Some(bm) = unsafe { (*self.wl_interfaces).kde_blur_manager.as_ref() } {
            let blur = bm.create(&wl_popup_surface).expect("blur.create");
            blur.commit().expect("blur.commit");

            Some(blur)
        } else {
            None
        };

        wl_popup_surface.commit().expect("wl_popup_surface.commit");

        unsafe {
            (*self.popup.get()) = Some((
                blur,
                pp,
                xdg_popup_surface,
                viewport,
                wl_popup_surface,
                popup_state,
            ));
        }
    }

    pub fn r#move(&self, p: &Point<PointerInputUnit>) {
        let Some((_, pp, _, _, _, _)) = (unsafe { &*self.popup.get() }) else {
            return;
        };

        let pos = unsafe {
            (*self.wl_interfaces)
                .xdg_wm_base
                .create_positioner()
                .expect("pos.create")
        };
        pos.set_offset(p.x as _, p.y as _).expect("pos.set_offset");
        pp.reposition(&pos, 0).expect("pp.reposition");
    }

    pub fn hide(&self) {
        unsafe {
            (*self.popup.get()) = None;
        }
    }
}

pub struct DisplayServerLink {
    pub wl_display: *mut wl::Display,
    pub wl_global_interfaces: *const GlobalInterfaces,
    pub pointer_state_ref: *const Option<PointerState>,
    pub window_registry: *mut WindowRegistry,
}

impl crate::SystemLink<'_> {
    pub fn init_main_window(
        dp: &wl::Display,
        wl_interfaces: &GlobalInterfaces,
        window_registry: &mut WindowRegistry,
        dbus: &dbus::Connection,
        composite_tree: &mut CompositeTree<Event>,
        ht_manager: &mut HitTestTreeManager,
        dispatcher: LogicFiberEventDispatcher,
        #[cfg(target_os = "linux")] terminate_event: &Arc<linux_eventfd::EventFD>,
        vk_device: &VulkanDevice,
        rt_sender: &std::sync::mpsc::Sender<RenderMessage>,
    ) -> WindowHandle {
        let w = Window::new(
            WindowType::Main {
                #[cfg(target_os = "linux")]
                termination_event: terminate_event.clone(),
            },
            &wl_interfaces,
            &dbus,
            dispatcher,
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            ht_manager.create(HitTestTreeData {
                width_adjustment_factor: 1.0,
                height_adjustment_factor: 1.0,
                ..Default::default()
            }),
        );
        let main_window_handle = w.make_handle();

        let vk_surface = w.create_vk_surface(dp, vk_device);
        rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                key: main_window_handle,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");
        w.commit();

        window_registry.objects.insert(main_window_handle, w);
        main_window_handle
    }

    pub fn open_window<'h>(
        &self,
        composite_tree: &mut CompositeTree<Event>,
        hit_tree: &mut (impl HitTestTreeCreate<'h> + ?Sized),
    ) -> WindowHandle {
        let w = Window::new(
            WindowType::Sub,
            unsafe { &*self.display_server.wl_global_interfaces },
            unsafe { &*self.dbus },
            unsafe { &*self.event_dispatcher }.clone(),
            composite_tree.create(CompositeRect {
                relative_size_adjustment: [1.0, 1.0],
                ..Default::default()
            }),
            hit_tree.create(HitTestTreeData {
                width_adjustment_factor: 1.0,
                height_adjustment_factor: 1.0,
                ..Default::default()
            }),
        );
        let window_handle = w.make_handle();

        let vk_surface = w.create_vk_surface(unsafe { &*self.display_server.wl_display }, unsafe {
            &*self.vk_device
        });
        self.rt_sender
            .send(RenderMessage::NewWindow(NewWindowData {
                key: window_handle,
                vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            }))
            .expect("rt_sender.send");
        w.commit();

        unsafe {
            (*self.display_server.window_registry)
                .objects
                .insert(window_handle, w);
            (*self.event_dispatcher).dispatch(Event::SubWindowOpen {
                window: window_handle,
            });
        }
        window_handle
    }

    pub fn close_window(
        &self,
        window_handle: WindowHandle,
        composite_tree: &mut CompositeTree<Event>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
        self.rt_sender
            .send(RenderMessage::DestroyWindow(
                window_handle,
                done_event_sender,
            ))
            .expect("rt_sender.send.destroy_window");
        done_event_receiver
            .recv()
            .expect("done_event_receiver.recv");

        composite_tree.free_all(window_handle.composite_root());
        ht_manager.free_all(window_handle.ht_root());

        unsafe {
            (*self.display_server.window_registry)
                .objects
                .remove(&window_handle);
        }
    }

    pub fn set_cursor(&self, _pointer_id: &PointerID, cursor: CursorShape) {
        if let Some(&PointerState {
            enter_state: Some(PointerEnterState { serial, .. }),
            cursor: Some(ref shape_device),
            ..
        }) = unsafe { (*self.display_server.pointer_state_ref).as_ref() }
        {
            shape_device
                .set_shape(serial, cursor.as_wayland())
                .expect("cursor_shape_device.set_cursor");
        }
    }

    pub fn notify_ui_scale_changes_to_render(&self, window: WindowHandle, new_scale: f32) {
        *window
            .state()
            .latest_ui_scale_changes
            .lock()
            .expect("poisoned") = Some(new_scale);
    }
}

pub fn dp_prepare_read(dp: &mut wl::Display) -> Result<(), ()> {
    loop {
        match dp.prepare_read() {
            Ok(_) => break,
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                dp.dispatch_pending().expect("wl.display.dispatch_pending");
            }
            Err(e) => {
                tracing::error!(reason = ?e, "wl.display.prepare_read");
                return Err(());
            }
        }
    }

    dp.flush().expect("wl.display.flush");
    Ok(())
}

pub struct WindowCommittedState {
    pub active_buffer_scale: f32,
    pub active_size: Size<PixelsUnit>,
    pub active_size_logical: Size<LogicalUnit>,
}

pub struct WindowState {
    surface_ptr: *mut wl::Surface,
    xdg_surface_ptr: *mut wl::XdgSurface,
    composite_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    extra_data: *mut core::ffi::c_void,
    pub committed_state: Mutex<WindowCommittedState>,
    pub swapchain_externally_invalidation_signal: AtomicBool,
    pub latest_ui_scale_changes: Mutex<Option<f32>>,
}
unsafe impl Sync for WindowState {}
unsafe impl Send for WindowState {}

pub struct Window {
    surface: wl::Owned<wl::Surface>,
    xdg_surface: wl::Owned<wl::XdgSurface>,
    xdg_toplevel: wl::Owned<wl::XdgToplevel>,
    deco: Option<wl::Owned<wl::ZxdgToplevelDecorationV1>>,
    fractional_scale: Option<wl::Owned<wl::WpFractionalScaleV1>>,
    _appmenu: Option<wl::Owned<wl::OrgKdeKwinAppmenu>>,
}
impl Drop for Window {
    #[inline(always)]
    fn drop(&mut self) {
        drop(self.unbind_listener());
    }
}
impl Window {
    fn new(
        r#type: WindowType,
        wl_interfaces: &GlobalInterfaces,
        dbus: &dbus::Connection,
        event_dispatcher: LogicFiberEventDispatcher,
        composite_root: CompositeTreeRef,
        ht_root: HitTestTreeRef,
    ) -> Self {
        let mut surface = wl_interfaces
            .compositor
            .create_surface()
            .expect("wl_surface create");
        let mut xdg_surface = wl_interfaces
            .xdg_wm_base
            .get_xdg_surface(&surface)
            .expect("xdg_surface create");
        let mut xdg_toplevel = xdg_surface.get_toplevel().expect("xdg_toplevel create");
        xdg_toplevel
            .set_title(c"Peridot Marble Editor")
            .expect("xdg_toplevel.set_title");
        xdg_surface
            .set_window_geometry(0, 0, 640, 480)
            .expect("xdg_surface.set_window_geometry");

        let appmenu = if let Some(ref am) = wl_interfaces.kde_appmenu_manager {
            let a = am.create(&surface).expect("appmenu.create");
            a.set_address(dbus.unique_name().expect("no name"), APPMENU_OBJECT_PATH)
                .expect("appmenu.set_address");

            Some(a)
        } else {
            None
        };

        let mut deco = if let Some(ref dm) = wl_interfaces.zxdg_decoration_manager {
            let d = dm
                .get_toplevel_decoration(&xdg_toplevel)
                .expect("decoration.get_toplevel");
            d.set_mode(wl::ZxdgToplevelDecorationV1Mode::ClientSide)
                .expect("decoration.set_mode");

            Some(d)
        } else {
            None
        };

        let mut fractional_scale = if let Some(ref fs) = wl_interfaces.fractional_scale_manager {
            let f = fs
                .get_fractional_scale(&surface)
                .expect("fractional_scale.create");

            Some(f)
        } else {
            None
        };

        let mut event_listener = Box::new(WindowEventListener {
            state: WindowState {
                surface_ptr: surface.as_ptr(),
                xdg_surface_ptr: xdg_surface.as_ptr(),
                composite_root,
                ht_root,
                extra_data: core::ptr::null_mut(),
                committed_state: Mutex::new(WindowCommittedState {
                    active_buffer_scale: 1.0,
                    active_size: Size::new_pixels(640, 480),
                    active_size_logical: Size::new_logical(640.0, 480.0),
                }),
                swapchain_externally_invalidation_signal: std::sync::atomic::AtomicBool::new(false),
                latest_ui_scale_changes: Mutex::new(None),
            },
            window_type: r#type,
            has_fractional_scale_support: fractional_scale.is_some(),
            pending_configure_size: (None, None),
            pending_configure_buffer_scale: None,
            event_dispatcher,
        });
        surface
            .set_listener(event_listener.as_mut())
            .into_result()
            .expect("wl_surface set listener");
        xdg_surface
            .set_listener(event_listener.as_mut())
            .into_result()
            .expect("xdg_surface set listener");
        xdg_toplevel
            .set_listener(event_listener.as_mut())
            .into_result()
            .expect("xdg_toplevel set listener");
        if let Some(ref mut x) = deco {
            x.set_listener(event_listener.as_mut())
                .into_result()
                .expect("zxdg_toplevel_decoration_v1.set_listener");
        }
        if let Some(ref mut x) = fractional_scale {
            x.set_listener(event_listener.as_mut())
                .into_result()
                .expect("wp_fractional_scale_v1.set_listener");
        }
        // owns EventListener in wl_surface
        surface.set_user_data(Box::into_raw(event_listener).cast());

        // commits initial state
        surface.commit().expect("wl_surface.commit");

        Self {
            surface,
            xdg_surface,
            xdg_toplevel,
            _appmenu: appmenu,
            deco,
            fractional_scale,
        }
    }

    #[inline(always)]
    const fn make_handle(&self) -> WindowHandle {
        WindowHandle(self.surface.as_ptr())
    }

    fn unbind_listener(&mut self) -> Box<WindowEventListener> {
        let p = unsafe { Box::from_raw(self.surface.user_data().cast::<WindowEventListener>()) };
        self.surface.set_user_data(core::ptr::null_mut());
        p
    }

    fn create_vk_surface<'d, 'fs>(
        &self,
        dp: &wl::Display,
        vk_device: &'d VulkanDevice<'fs>,
    ) -> VulkanSurface<'d, 'fs> {
        VulkanSurface::new(&vk_device, unsafe {
            br::WaylandSurfaceCreateInfo::new(dp.as_raw().cast(), self.surface.as_raw().cast())
                .execute(vk_device.instance(), None)
                .expect("vk_surface.create")
        })
    }

    pub fn rebind_event_dispatcher(&mut self, event_dispatcher: LogicFiberEventDispatcher) {
        unsafe {
            (*self.surface.user_data().cast::<WindowEventListener>()).event_dispatcher =
                event_dispatcher;
        }
    }

    fn commit(&self) {
        self.surface.commit().expect("wl_surface.commit");
    }
}

#[repr(C)] // place state at 0 always: WaylandWindowEventListener can be reinterpreted as WaylandWindowState
pub struct WindowEventListener {
    state: WindowState,
    window_type: WindowType,
    has_fractional_scale_support: bool,
    pending_configure_size: (Option<i32>, Option<i32>),
    pending_configure_buffer_scale: Option<f32>,
    event_dispatcher: LogicFiberEventDispatcher,
}
impl wl::SurfaceEventListener for WindowEventListener {
    #[tracing::instrument(skip(self, _surface, _output))]
    fn enter(&mut self, _surface: &mut wl::Surface, _output: &mut wl::Output) {}

    #[tracing::instrument(skip(self, _surface, _output))]
    fn leave(&mut self, _surface: &mut wl::Surface, _output: &mut wl::Output) {}

    #[tracing::instrument(skip(self, _surface))]
    fn preferred_buffer_scale(&mut self, _surface: &mut wl::Surface, factor: i32) {
        tracing::trace!(
            has_fractional_scale = self.has_fractional_scale_support,
            "perferred buffer scale"
        );
        if self.has_fractional_scale_support {
            // fractional_scaleがある場合はこっちは処理しなくていい
            return;
        }

        self.pending_configure_buffer_scale = Some(factor as _);
    }

    #[tracing::instrument(skip(self, _surface))]
    fn preferred_buffer_transform(&mut self, _surface: &mut wl::Surface, transform: u32) {
        tracing::trace!("preferred buffer transform");
    }
}
impl wl::XdgSurfaceEventListener for WindowEventListener {
    #[tracing::instrument(skip(self, sender))]
    fn configure(&mut self, sender: &mut wl::XdgSurface, serial: u32) {
        tracing::trace!("xdg surface configure");

        self.commit();
        sender
            .ack_configure(serial)
            .expect("xdg_surface.ack_configure");
    }
}
impl wl::XdgToplevelEventListener for WindowEventListener {
    #[tracing::instrument(skip(self, _sender))]
    fn close(&mut self, _sender: &mut wl::XdgToplevel) {
        tracing::trace!("xdg toplevel close");
        match self.window_type {
            WindowType::Main {
                ref termination_event,
            } => {
                termination_event.inc(1).expect("termination_event.inc");
            }
            WindowType::Sub => {
                self.event_dispatcher.dispatch(Event::SubWindowClose {
                    window: WindowHandle(self.state.surface_ptr),
                });
            }
        }
    }

    #[tracing::instrument(skip(self, sender), fields(states = ?unsafe { states.as_slice::<u32>() }))]
    fn configure(
        &mut self,
        sender: &mut wl::XdgToplevel,
        width: i32,
        height: i32,
        states: &mut wl::ffi::Array,
    ) {
        tracing::trace!("xdg toplevel configure");

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
    }

    fn configure_bounds(&mut self, _sender: &mut wl::XdgToplevel, _width: i32, _height: i32) {}

    fn wm_capabilities(
        &mut self,
        _sender: &mut wl::XdgToplevel,
        _capabilities: &mut wl::ffi::Array,
    ) {
    }
}
impl wl::ZxdgToplevelDecorationV1EventListener for WindowEventListener {
    fn configure(
        &mut self,
        _sender: &mut wl::ZxdgToplevelDecorationV1,
        mode: wl::ZxdgToplevelDecorationV1Mode,
    ) {
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
impl wl::WpFractionalScaleV1EventListener for WindowEventListener {
    #[tracing::instrument(skip(self, _sender))]
    fn preferred_scale(&mut self, _sender: &mut wl::WpFractionalScaleV1, scale: u32) {
        tracing::trace!("fractional scale");
        self.pending_configure_buffer_scale = Some(scale as f32 / 120.0);
    }
}
impl WindowEventListener {
    fn commit(&mut self) {
        let mut delayed_event_queue = Vec::with_capacity(2);

        {
            let mut committed_state_ref = self.state.committed_state.lock().expect("poisoned");
            let mut rescaled = false;
            if let Some(s) = self.pending_configure_buffer_scale.take() {
                if self.has_fractional_scale_support {
                    // fractional scaleでは1固定にする必要がある
                    unsafe { &*self.state.surface_ptr }
                        .set_buffer_scale(1)
                        .expect("wl_surface.set_buffer_scale");
                } else {
                    unsafe { &*self.state.surface_ptr }
                        .set_buffer_scale(s as _)
                        .expect("wl_surface.set_buffer_scale");
                }

                committed_state_ref.active_buffer_scale = s;
                delayed_event_queue.push(Event::WindowRescaleUI {
                    window: WindowHandle(self.state.surface_ptr),
                    new_scale: s,
                });
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
                let pixels_size =
                    logical_size.to_pixels_ceil(committed_state_ref.active_buffer_scale);
                if pixels_size != committed_state_ref.active_size {
                    committed_state_ref.active_size = pixels_size;
                    committed_state_ref.active_size_logical = logical_size;
                    self.state
                        .swapchain_externally_invalidation_signal
                        .store(true, std::sync::atomic::Ordering::Relaxed);

                    delayed_event_queue.push(Event::WindowResize {
                        window: WindowHandle(self.state.surface_ptr),
                        size: logical_size,
                    });
                }
            }
        }

        for x in delayed_event_queue {
            self.event_dispatcher.dispatch(x);
        }
    }
}

pub struct WindowRegistry {
    objects: HashMap<WindowHandle, Window>,
}
impl WindowRegistry {
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            objects: HashMap::new(),
        }
    }

    #[inline(always)]
    pub fn get(&self, h: WindowHandle) -> Option<&Window> {
        self.objects.get(&h)
    }

    #[inline(always)]
    pub fn get_mut(&mut self, h: WindowHandle) -> Option<&mut Window> {
        self.objects.get_mut(&h)
    }
}

pub struct PopupState {
    surface_ptr: *mut wl::Surface,
}
impl wl::XdgSurfaceEventListener for PopupState {
    #[tracing::instrument(skip(self, sender))]
    fn configure(&mut self, sender: &mut peridot_tp_wayland::XdgSurface, serial: u32) {
        tracing::trace!("popup.surface.configure");
        sender.ack_configure(serial).expect("popup.ack_configure");

        unsafe {
            (*self.surface_ptr).commit().expect("popup.surface.commit");
        }
    }
}
impl wl::XdgPopupEventListener for PopupState {
    #[tracing::instrument(skip(self, sender))]
    fn configure(
        &mut self,
        sender: &mut peridot_tp_wayland::XdgPopup,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) {
        tracing::trace!("popup.configure");
    }

    #[tracing::instrument(skip(self, sender))]
    fn popup_done(&mut self, sender: &mut peridot_tp_wayland::XdgPopup) {
        tracing::trace!("popup.popup_done");
    }

    #[tracing::instrument(skip(self, sender))]
    fn repositioned(&mut self, sender: &mut peridot_tp_wayland::XdgPopup, token: u32) {
        tracing::trace!("popup.repositioned");
    }
}

#[allow(dead_code)]
pub enum DragPreviewPopoverBuffer {
    SinglePixel(wl::Owned<wl::Buffer>),
    Shm {
        shm_region: crate::utils::platform::unix::TemporalSharedMemory,
        mapped: crate::utils::platform::unix::MappedMemory,
        shm_pool: wl::Owned<wl::ShmPool>,
        buf: wl::Owned<wl::Buffer>,
    },
}
impl DragPreviewPopoverBuffer {
    #[inline(always)]
    pub fn buffer(&self) -> &wl::Buffer {
        match self {
            Self::SinglePixel(x) => x,
            Self::Shm { buf, .. } => buf,
        }
    }
}

#[derive(Clone, Copy)]
pub struct PointerID();

struct PointerEnterState {
    pub surface: *mut wl::Surface,
    pub serial: u32,
}

pub struct PointerState {
    _wl_object: wl::Owned<wl::Pointer>,
    cursor: Option<wl::Owned<wl::WpCursorShapeDeviceV1>>,
    pos: Point<LogicalUnit>,
    enter_state: Option<PointerEnterState>,
}

pub struct KeyboardState {
    _wl_object: wl::Owned<wl::Keyboard>,
    xkb_keymap: Option<xkbcommon::Keymap>,
    xkb_state: Option<xkbcommon::State>,
    _text_input: Option<wl::Owned<wl::ZwpTextInputV3>>,
}

pub struct GlobalMessaging {
    pub text_input_manager: *mut wl::ZwpTextInputManagerV3,
    pub xkb_context: xkbcommon::Context,
    pub keyboard: Option<KeyboardState>,
    pub pointer: Option<PointerState>,
    pub cursor_shape_manager: Option<*mut wl::WpCursorShapeManagerV1>,
    pub event_dispatcher: LogicFiberEventDispatcher,
    pub _pinned: core::marker::PhantomPinned,
}
impl wl::XdgWmBaseEventListener for GlobalMessaging {
    #[inline(always)]
    fn ping(&mut self, sender: &mut peridot_tp_wayland::XdgWmBase, serial: u32) {
        sender.pong(serial).expect("xdg_wm_base pong");
    }
}
impl wl::SeatEventListener for GlobalMessaging {
    fn capabilities(
        &mut self,
        seat: &mut peridot_tp_wayland::Seat,
        capabilities: wl::SeatCapability,
    ) {
        tracing::trace!(?capabilities, "seat::capabilities");

        if capabilities.contains(wl::SeatCapability::POINTER) {
            // pointer
            let mut p = seat.get_pointer().expect("seat.get_pointer");
            p.set_listener(self)
                .into_result()
                .expect("pointer.set_listener");
            let c = if let Some(mgr) = self.cursor_shape_manager {
                Some(unsafe {
                    (*mgr)
                        .get_pointer(&p)
                        .expect("cursor_shape_manager.get_pointer")
                })
            } else {
                None
            };

            self.pointer = Some(PointerState {
                _wl_object: p,
                cursor: c,
                pos: Point::new_logical(0.0, 0.0),
                enter_state: None,
            });
        } else {
            // remove pointer
            self.pointer = None;
        }

        if capabilities.contains(wl::SeatCapability::KEYBOARD) {
            let mut k = seat.get_keyboard().expect("seat.get_keyboard");
            k.set_listener(self)
                .into_result()
                .expect("keyboard.set_listener");
            let mut ti = unsafe {
                (*self.text_input_manager)
                    .get_text_input(seat)
                    .expect("text_input_manager.get_text_input")
            };
            ti.set_listener(self)
                .into_result()
                .expect("text_input.set_listener");

            self.keyboard = Some(KeyboardState {
                _wl_object: k,
                xkb_keymap: None,
                xkb_state: None,
                _text_input: Some(ti),
            });
        } else {
            // remove keyboard
            self.keyboard = None;
        }
    }

    fn name(&mut self, _seat: &mut peridot_tp_wayland::Seat, name: &core::ffi::CStr) {
        tracing::trace!(?name, "seat::name");
    }
}
impl wl::PointerEventListener for GlobalMessaging {
    #[tracing::instrument(skip(self, _pointer, surface), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn enter(
        &mut self,
        _pointer: &mut wl::Pointer,
        serial: u32,
        surface: &mut wl::Surface,
        surface_x: wl::Fixed,
        surface_y: wl::Fixed,
    ) {
        let state = self.pointer.as_mut().expect("no pointer state initialized");

        state.enter_state = Some(PointerEnterState {
            surface: surface as *mut _,
            serial,
        });
        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());

        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(surface as *mut _),
            client_pos: state.pos,
        });
    }

    #[tracing::instrument(skip(self, _pointer, _surface))]
    fn leave(
        &mut self,
        _pointer: &mut wl::Pointer,
        serial: u32,
        _surface: Option<&mut wl::Surface>,
    ) {
        let state = self.pointer.as_mut().expect("no pointer state initialized");

        state.enter_state = None;
    }

    #[tracing::instrument(skip(self, _pointer), fields(surface_x = surface_x.to_f32(), surface_y = surface_y.to_f32()))]
    fn motion(
        &mut self,
        _pointer: &mut wl::Pointer,
        time: u32,
        surface_x: wl::Fixed,
        surface_y: wl::Fixed,
    ) {
        let state = self.pointer.as_mut().expect("no pointer state initialized");
        let Some(ref enter_state) = state.enter_state else {
            return;
        };

        state.pos = Point::new_logical(surface_x.to_f32(), surface_y.to_f32());
        self.event_dispatcher.dispatch(Event::PointerMove {
            pointer_id: PointerID(),
            window: WindowHandle(enter_state.surface),
            client_pos: state.pos,
        });
    }

    #[tracing::instrument(skip(self, _pointer), fields(state = state as u32))]
    fn button(
        &mut self,
        _pointer: &mut wl::Pointer,
        serial: u32,
        time: u32,
        button: u32,
        state: wl::PointerButtonState,
    ) {
        let pointer_state = self.pointer.as_ref().expect("no pointer state initialized");
        let Some(ref enter_state) = pointer_state.enter_state else {
            return;
        };

        if state == wl::PointerButtonState::Pressed {
            self.event_dispatcher.dispatch(Event::PointerDown {
                window: WindowHandle(enter_state.surface),
            });
        } else if state == wl::PointerButtonState::Released {
            self.event_dispatcher.dispatch(Event::PointerUp {
                window: WindowHandle(enter_state.surface),
            });
        }
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis(&mut self, _pointer: &mut wl::Pointer, time: u32, axis: u32, value: wl::Fixed) {
        tracing::trace!("pointer.axis");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn frame(&mut self, _pointer: &mut wl::Pointer) {
        // tracing::trace!("pointer.frame");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_source(&mut self, _pointer: &mut wl::Pointer, axis_source: u32) {
        tracing::trace!("pointer.axis_source");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_stop(&mut self, _pointer: &mut wl::Pointer, time: u32, axis: u32) {
        tracing::trace!("pointer.axis_stop");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_discrete(&mut self, _pointer: &mut wl::Pointer, axis: u32, discrete: i32) {
        tracing::trace!("pointer.axis_discrete");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_value120(&mut self, _pointer: &mut wl::Pointer, axis: u32, value120: i32) {
        tracing::trace!("pointer.axis_value120");
    }

    #[tracing::instrument(skip(self, _pointer))]
    fn axis_relative_direction(&mut self, _pointer: &mut wl::Pointer, axis: u32, direction: u32) {
        tracing::trace!("pointer.axis_relative_direction");
    }
}
impl wl::KeyboardEventListener for GlobalMessaging {
    #[tracing::instrument(skip(self, _sender))]
    fn keymap(
        &mut self,
        _sender: &mut wl::Keyboard,
        format: wl::KeyboardKeymapFormat,
        fd: i32,
        size: u32,
    ) {
        let state = self.keyboard.as_mut().expect("keyboard_state.uninit");
        if format != wl::KeyboardKeymapFormat::XkbV1 {
            unimplemented!("unknown keymap format: {format:?}");
        }

        let mapped = crate::utils::platform::unix::MappedMemory::new(
            None,
            size as _,
            libc::PROT_READ,
            libc::MAP_PRIVATE,
            &fd,
            0,
        )
        .expect("keyboard.keymap.mmap");
        let content = unsafe {
            core::ffi::CStr::from_bytes_with_nul(core::slice::from_raw_parts(
                mapped.as_ptr().cast::<u8>(),
                size as _,
            ))
            .expect("invalid content")
            .to_str()
            .expect("invalid content")
        };
        let keymap = xkbcommon::Keymap::from_buffer(
            &self.xkb_context,
            unsafe { core::slice::from_raw_parts(content.as_ptr(), size as _) },
            xkbcommon::KeymapFormat::TextV1,
            xkbcommon::KeymapCompileFlags::NO_FLAGS,
        )
        .expect("xkb_keymap.create");
        let xkb_state = xkbcommon::State::new(&keymap).expect("xkb_state.create");

        state.xkb_keymap = Some(keymap);
        state.xkb_state = Some(xkb_state);
    }

    #[tracing::instrument(skip(self, _sender, _surface))]
    fn enter(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        _surface: &mut wl::Surface,
        keys: &[u32],
    ) {
        tracing::trace!("keyboard::enter");
    }

    #[tracing::instrument(skip(self, _sender, _surface))]
    fn leave(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        _surface: Option<&mut wl::Surface>,
    ) {
        tracing::trace!("keyboard::leave");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn key(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        time: u32,
        key: u32,
        state: wl::KeyboardKeyState,
    ) {
        let state = self.keyboard.as_mut().expect("keyboard_state.uninit");
        tracing::trace!("keyboard::key");

        if let Some(ref mut x) = state.xkb_state {
            let mut buf = Vec::with_capacity(32);
            // evdevのスキャンコードでくるので、xkbのスキャンコードにする(8を足せばいいらしい: https://wayland-book.com/seat/keyboard.html)
            let mut alen = x.key_get_utf8(key + 8, buf.spare_capacity_mut());
            if alen > buf.capacity() {
                buf.reserve(alen - buf.capacity());
                alen = x.key_get_utf8(key + 8, buf.spare_capacity_mut());
            }
            unsafe {
                buf.set_len(alen);
            }
            tracing::trace!(
                alen,
                text = unsafe { core::str::from_utf8_unchecked(&buf) },
                "keyboard translated"
            );
        }
    }

    #[tracing::instrument(skip(self, _sender))]
    fn modifiers(
        &mut self,
        _sender: &mut wl::Keyboard,
        serial: u32,
        mods_depressed: u32,
        mods_latched: u32,
        mods_locked: u32,
        group: u32,
    ) {
        let state = self.keyboard.as_mut().expect("keyboard_state.uninit");
        tracing::trace!("keyboard::modifiers");

        if let Some(ref mut x) = state.xkb_state {
            x.update_mask(
                mods_depressed,
                mods_latched,
                mods_locked,
                group,
                group,
                group,
            );
        }
    }

    #[tracing::instrument(skip(self, _sender))]
    fn repeat_info(&mut self, _sender: &mut wl::Keyboard, rate: i32, delay: i32) {
        tracing::trace!("keyboard::repeat_info");
    }
}
impl wl::ZwpTextInputV3EventListener for GlobalMessaging {
    #[tracing::instrument(skip(self, sender, _surface))]
    fn enter(&mut self, sender: &mut wl::ZwpTextInputV3, _surface: Option<&mut wl::Surface>) {
        tracing::trace!("textinputv3::enter");
        sender.enable().expect("text_input.enable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(skip(self, sender, _surface))]
    fn leave(&mut self, sender: &mut wl::ZwpTextInputV3, _surface: Option<&mut wl::Surface>) {
        tracing::trace!("textinputv3::leave");
        sender.disable().expect("text_input.disable");
        sender.commit().expect("text_input.commit");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn preedit_string(
        &mut self,
        _sender: &mut wl::ZwpTextInputV3,
        text: Option<&core::ffi::CStr>,
        cursor_begin: i32,
        cursor_end: i32,
    ) {
        tracing::trace!("textinputv3::preedit_string");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn commit_string(&mut self, _sender: &mut wl::ZwpTextInputV3, text: Option<&core::ffi::CStr>) {
        tracing::trace!("textinputv3::commit_string");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn delete_surrounding_text(
        &mut self,
        _sender: &mut wl::ZwpTextInputV3,
        before_length: u32,
        after_length: u32,
    ) {
        tracing::trace!("textinputv3::delete_surrounding_text");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn done(&mut self, _sender: &mut wl::ZwpTextInputV3, serial: u32) {
        tracing::trace!("textinputv3::done");
    }
}
impl wl::ZwlrLayerSurfaceV1EventListener for GlobalMessaging {
    #[tracing::instrument(skip(self, sender))]
    fn configure(
        &mut self,
        sender: &mut wl::ZwlrLayerSurfaceV1,
        serial: u32,
        width: u32,
        height: u32,
    ) {
        tracing::trace!("layer surface configure");
        sender
            .ack_configure(serial)
            .expect("layer_surface.ack_configure");
    }

    #[tracing::instrument(skip(self, _sender))]
    fn closed(&mut self, _sender: &mut wl::ZwlrLayerSurfaceV1) {
        tracing::trace!("layer surface closed");
    }
}

pub struct GlobalInterfaces {
    pub outputs: Vec<wl::Owned<wl::Output>>,
    pub compositor: wl::Owned<wl::Compositor>,
    pub xdg_wm_base: wl::Owned<wl::XdgWmBase>,
    pub seat: wl::Owned<wl::Seat>,
    pub shm: wl::Owned<wl::Shm>,
    pub viewporter: wl::Owned<wl::WpViewporter>,
    pub text_input_manager: wl::Owned<wl::ZwpTextInputManagerV3>,
    // optional requirements
    pub single_pixel_buffer_manager: Option<wl::Owned<wl::WpSinglePixelBufferManagerV1>>,
    pub kde_blur_manager: Option<wl::Owned<wl::OrgKdeKwinBlurManager>>,
    pub kde_appmenu_manager: Option<wl::Owned<wl::OrgKdeKwinAppmenuManager>>,
    pub zxdg_decoration_manager: Option<wl::Owned<wl::ZxdgDecorationManagerV1>>,
    pub cursor_shape_manager: Option<wl::Owned<wl::WpCursorShapeManagerV1>>,
    pub fractional_scale_manager: Option<wl::Owned<wl::WpFractionalScaleManagerV1>>,
}
impl GlobalInterfaces {
    pub fn collect_sync(display: &wl::Display) -> std::io::Result<Self> {
        let mut wl_registry = display.get_registry()?;
        let mut rl = RegistryListener::default();
        wl_registry
            .set_listener(&mut rl)
            .into_result()
            .expect("wl_registry.set_listener");
        display.roundtrip()?;

        Ok(Self {
            outputs: rl.outputs,
            compositor: rl.compositor.expect("no compositor"),
            xdg_wm_base: rl.xdg_wm_base.expect("no xdg-shell"),
            seat: rl.seat.expect("no seat"),
            shm: rl.shm.expect("no shm"),
            viewporter: rl.viewporter.expect("no viewporter"),
            text_input_manager: rl.text_input_manager.expect("no text-input"),
            single_pixel_buffer_manager: rl.single_pixel_buffer_manager,
            kde_blur_manager: rl.kde_blur_manager,
            kde_appmenu_manager: rl.kde_appmenu_manager,
            zxdg_decoration_manager: rl.zxdg_decoration_manager,
            cursor_shape_manager: rl.cursor_shape_manager,
            fractional_scale_manager: rl.fractional_scale_manager,
        })
    }
}

#[derive(Default)]
struct RegistryListener {
    compositor: Option<wl::Owned<wl::Compositor>>,
    outputs: Vec<wl::Owned<wl::Output>>,
    xdg_wm_base: Option<wl::Owned<wl::XdgWmBase>>,
    seat: Option<wl::Owned<wl::Seat>>,
    shm: Option<wl::Owned<wl::Shm>>,
    viewporter: Option<wl::Owned<wl::WpViewporter>>,
    text_input_manager: Option<wl::Owned<wl::ZwpTextInputManagerV3>>,
    single_pixel_buffer_manager: Option<wl::Owned<wl::WpSinglePixelBufferManagerV1>>,
    kde_blur_manager: Option<wl::Owned<wl::OrgKdeKwinBlurManager>>,
    kde_appmenu_manager: Option<wl::Owned<wl::OrgKdeKwinAppmenuManager>>,
    zxdg_decoration_manager: Option<wl::Owned<wl::ZxdgDecorationManagerV1>>,
    cursor_shape_manager: Option<wl::Owned<wl::WpCursorShapeManagerV1>>,
    fractional_scale_manager: Option<wl::Owned<wl::WpFractionalScaleManagerV1>>,
}
impl wl::RegistryListener for RegistryListener {
    fn global(
        &mut self,
        registry: &mut peridot_tp_wayland::Registry,
        name: u32,
        interface: &core::ffi::CStr,
        version: u32,
    ) {
        tracing::info!(target: "wl::diag::global_interface", name, ?interface, version);

        if interface == c"wl_compositor" {
            self.compositor = Some(registry.bind(name, version).expect("bind compositor"));
        } else if interface == c"wl_output" {
            self.outputs
                .push(registry.bind(name, version).expect("bind output"));
        } else if interface == c"xdg_wm_base" {
            self.xdg_wm_base = Some(registry.bind(name, version).expect("bind xdg_wm_base"));
        } else if interface == c"wl_seat" {
            assert!(self.seat.is_none(), "multiple seat?");
            self.seat = Some(registry.bind(name, version).expect("bind seat"));
        } else if interface == c"wl_shm" {
            self.shm = Some(registry.bind(name, version).expect("bind shm"));
        } else if interface == c"wp_viewporter" {
            self.viewporter = Some(registry.bind(name, version).expect("bind viewporter"));
        } else if interface == c"wp_single_pixel_buffer_manager_v1" {
            self.single_pixel_buffer_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind single_pixel_buffer_manager"),
            );
        } else if interface == c"org_kde_kwin_blur_manager" {
            self.kde_blur_manager =
                Some(registry.bind(name, version).expect("bind kde_blur_manager"));
        } else if interface == c"org_kde_kwin_appmenu_manager" {
            self.kde_appmenu_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind kde_appmenu_manager"),
            );
        } else if interface == c"zxdg_decoration_manager_v1" {
            self.zxdg_decoration_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind zxdg_decoration_manager"),
            );
        } else if interface == c"zwp_text_input_manager_v3" {
            self.text_input_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind text_input_manager"),
            );
        } else if interface == c"wp_cursor_shape_manager_v1" {
            self.cursor_shape_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind cursor_shape_manager"),
            );
        } else if interface == c"wp_fractional_scale_manager_v1" {
            self.fractional_scale_manager = Some(
                registry
                    .bind(name, version)
                    .expect("bind fractional_scale_manager"),
            );
        }
    }

    fn global_remove(&mut self, _registry: &mut peridot_tp_wayland::Registry, name: u32) {
        tracing::info!(target: "wl::diag", name, "wl interface remove");
    }
}
