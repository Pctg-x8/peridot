use std::{
    rc::Rc,
    sync::{Mutex, atomic::AtomicBool},
};

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};
use peridot_tp_wayland as wl;

use crate::{
    ContextMenuHandle, Event, LogicFiberEventDispatcher, SyncEvent, SystemLink,
    graphics::VulkanSurface,
    input::{
        PerWindowKeyboardFocusState,
        hittest::{HitTestTreeCreate, HitTestTreeData, HitTestTreeManager, HitTestTreeRef},
    },
    platform::unix::wayland::{SurfaceState, SurfaceStateTag, WindowScaling},
    rendering::{
        NewContextMenuData, NewWindowVulkanSurface, RenderMessage,
        composite::{
            AnimatableColor, CompositeMode, CompositeRect, CompositeTree, CompositeTreeRef,
        },
    },
    uikit::{
        MenuBaseSurfaceEventHandler, MenuItemLayout, MenuItemView, MountTarget, ViewInitContext,
    },
    utils::{LogicalUnit, PixelsUnit, Point, Size, platform::linux::TimerFD},
};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Handle(pub(super) *mut wl::Surface);
unsafe impl Sync for Handle {}
unsafe impl Send for Handle {}
impl Handle {
    #[inline(always)]
    fn data(&self) -> &InstanceData {
        &unsafe { &*(*self.0).user_data().cast::<SurfaceState<InstanceData>>() }.data
    }

    #[inline(always)]
    fn data_mut(&mut self) -> &mut InstanceData {
        &mut unsafe { &mut *(*self.0).user_data().cast::<SurfaceState<InstanceData>>() }.data
    }

    #[inline(always)]
    pub fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        &mut self.data_mut().keyboard_focus_state
    }

    #[inline(always)]
    pub fn logical_size(&self) -> Size<LogicalUnit> {
        self.data().committed_state.lock().expect("poisoned").size
    }

    #[inline(always)]
    pub fn pixels_size(&self) -> Size<PixelsUnit> {
        self.logical_size().to_pixels_ceil(
            self.data()
                .committed_state
                .lock()
                .expect("poisoned")
                .buffer_scale,
        )
    }

    #[inline(always)]
    pub fn render_scale(&self) -> f32 {
        self.data()
            .committed_state
            .lock()
            .expect("poisoned")
            .buffer_scale
    }

    #[inline(always)]
    pub fn view(&self, index: usize) -> Option<&MenuItemView> {
        self.data().views.get(index)
    }

    pub fn submenu_pop_position(&self, index: usize) -> Option<Point<LogicalUnit>> {
        match self.data().views.get(index)? {
            MenuItemView::SubMenu(x) => {
                let base = self.data().spawned_position;
                let size = self.data().committed_state.lock().expect("poisoned").size;
                Some(Point::new_logical(
                    base.x + size.width,
                    base.y + x.placement_y,
                ))
            }
            _ => None,
        }
    }

    pub fn take_latest_ui_scale_change(&self) -> Option<f32> {
        self.data()
            .latest_ui_scale_changes
            .lock()
            .expect("poisoned")
            .take()
    }

    pub fn take_swapchain_externally_invalidation_signal(&self) -> bool {
        self.data()
            .swapchain_externally_invalidation_signal
            .compare_exchange_weak(
                true,
                false,
                std::sync::atomic::Ordering::Relaxed,
                std::sync::atomic::Ordering::Relaxed,
            )
            == Ok(true)
    }

    pub fn rescale<E>(&self, scale: f32, composite_tree: &mut CompositeTree<E>) {
        for v in self.data().views.iter() {
            v.rescale(scale, composite_tree);
        }
    }

    pub fn update_manual_scaling(&self) {
        let el = self.data();
        if let WindowScaling::Manual { ref viewport, .. } = el.scaling {
            let committed_state = el.committed_state.lock().expect("poisoned");
            viewport
                .set_source(
                    wl::Fixed::from_f32_lossy(0.0),
                    wl::Fixed::from_f32_lossy(0.0),
                    wl::Fixed::from_f32_lossy(committed_state.size_pixels.width as _),
                    wl::Fixed::from_f32_lossy(committed_state.size_pixels.height as _),
                )
                .expect("viewport.set_source");
            viewport
                .set_destination(
                    committed_state.size.width as _,
                    committed_state.size.height as _,
                )
                .expect("viewport.set_destination");
        }
    }
}
impl MountTarget for Handle {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.data().ct_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.data().ht_root
    }
}

struct CommittedState {
    pub size: Size<LogicalUnit>,
    pub size_pixels: Size<PixelsUnit>,
    pub buffer_scale: f32,
}

struct InstanceData {
    surface_ptr: *mut wl::Surface,
    scaling: WindowScaling,
    xdg_surface: wl::Owned<wl::XdgSurface>,
    xdg_popup: wl::Owned<wl::XdgPopup>,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    keyboard_focus_state: PerWindowKeyboardFocusState,
    spawned_position: Point<LogicalUnit>,
    committed_state: Mutex<CommittedState>,
    pub swapchain_externally_invalidation_signal: AtomicBool,
    pub latest_ui_scale_changes: Mutex<Option<f32>>,
    pending_configure_size: (Option<i32>, Option<i32>),
    pending_configure_buffer_scale: Option<f32>,
    event_dispatcher: LogicFiberEventDispatcher,
    _base_surface_event_handler: Rc<MenuBaseSurfaceEventHandler>,
    views: Vec<MenuItemView>,
}

#[repr(transparent)]
struct EventHandler(SurfaceState<InstanceData>);
impl wl::SurfaceEventListener for EventHandler {
    fn enter(
        &mut self,
        surface: &mut peridot_tp_wayland::Surface,
        output: &mut peridot_tp_wayland::Output,
    ) {
        tracing::debug!("context menu enter");
    }

    fn leave(
        &mut self,
        surface: &mut peridot_tp_wayland::Surface,
        output: &mut peridot_tp_wayland::Output,
    ) {
        tracing::debug!("context menu leave");
    }

    fn preferred_buffer_scale(&mut self, surface: &mut peridot_tp_wayland::Surface, factor: i32) {
        let has_fractional_scale_support = self.0.data.scaling.is_manual();
        tracing::debug!(
            has_fractional_scale_support,
            factor,
            "context menu preferred buffer scale"
        );
        if has_fractional_scale_support {
            // Fractional Scaleがある場合はこっちは無視
            return;
        }

        self.0.data.pending_configure_buffer_scale = Some(factor as _);
    }

    fn preferred_buffer_transform(
        &mut self,
        surface: &mut peridot_tp_wayland::Surface,
        transform: u32,
    ) {
        tracing::debug!(transform, "context menu preferred buffer transform");
    }
}
impl wl::XdgSurfaceEventListener for EventHandler {
    fn configure(&mut self, sender: &mut peridot_tp_wayland::XdgSurface, serial: u32) {
        tracing::debug!(serial, "context menu configure(surface)");
        let mut delayed_event_queue = Vec::with_capacity(1);

        let mut committed_state_ref = self.0.data.committed_state.lock().expect("poisoned");
        let mut rescaled = false;
        if let Some(s) = self.0.data.pending_configure_buffer_scale.take() {
            match self.0.data.scaling {
                WindowScaling::Automatic => {
                    unsafe { &*self.0.data.surface_ptr }
                        .set_buffer_scale(s as _)
                        .expect("wl_surface.set_buffer_scale");
                }
                WindowScaling::Manual { .. } => {
                    // fractional scaleでは1固定にして、viewporterでスケールを適用する必要がある
                    unsafe { &*self.0.data.surface_ptr }
                        .set_buffer_scale(1)
                        .expect("wl_surface.set_buffer_scale");
                }
            }

            committed_state_ref.buffer_scale = s;
            *self
                .0
                .data
                .latest_ui_scale_changes
                .lock()
                .expect("poisoned") = Some(s);
            delayed_event_queue.push(Event::ContextMenuRescale { scale: s });
            rescaled = true;
        }

        let (w, h) = (
            self.0.data.pending_configure_size.0.take(),
            self.0.data.pending_configure_size.1.take(),
        );
        if rescaled || w.is_some() || h.is_some() {
            // recompute size
            let logical_size = Size::new_logical(
                w.map_or(committed_state_ref.size.width, |x| x as _),
                h.map_or(committed_state_ref.size.height, |y| y as _),
            );
            let pixels_size = logical_size.to_pixels_ceil(committed_state_ref.buffer_scale);
            if pixels_size != committed_state_ref.size_pixels {
                self.0
                    .data
                    .xdg_surface
                    .set_window_geometry(0, 0, logical_size.width as _, logical_size.height as _)
                    .expect("xdg_surface.set_window_geometry");

                committed_state_ref.size_pixels = pixels_size;
                committed_state_ref.size = logical_size;
                self.0
                    .data
                    .swapchain_externally_invalidation_signal
                    .store(true, std::sync::atomic::Ordering::Relaxed);
            }
        }

        drop(committed_state_ref);
        unsafe { &*self.0.data.surface_ptr }
            .commit()
            .expect("surface.commit");
        for e in delayed_event_queue {
            self.0.data.event_dispatcher.dispatch(e);
        }
        sender.ack_configure(serial).expect("ack_configure");
    }
}
impl wl::XdgPopupEventListener for EventHandler {
    fn configure(
        &mut self,
        sender: &mut peridot_tp_wayland::XdgPopup,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) {
        tracing::debug!(x, y, width, height, "context menu configure");
    }

    fn popup_done(&mut self, sender: &mut peridot_tp_wayland::XdgPopup) {
        tracing::debug!("context menu done");
    }

    fn repositioned(&mut self, sender: &mut peridot_tp_wayland::XdgPopup, token: u32) {
        tracing::debug!(token, "context menu repositioned");
    }
}
impl wl::WpFractionalScaleV1EventListener for EventHandler {
    fn preferred_scale(
        &mut self,
        sender: &mut peridot_tp_wayland::WpFractionalScaleV1,
        scale: u32,
    ) {
        tracing::debug!(scale, "context menu preferred scale(fractional)");
        self.0.data.pending_configure_buffer_scale = Some(scale as f32 / 120.0);
    }
}

pub struct SharedState {
    pub delayed_action_timer: TimerFD,
}

pub fn reserve_delayed_action(syslink: &SystemLink) {
    syslink
        .context_menu
        .delayed_action_timer
        .set(0, 400 * 1000 * 1000)
        .expect("timerfd.set");
}

pub fn unreserve_delayed_action(syslink: &SystemLink) {
    syslink
        .context_menu
        .delayed_action_timer
        .unset()
        .expect("timerfd.unset");
}

pub fn pop(
    parent: super::WindowHandle,
    syslink: &SystemLink,
    view_init_context: &mut ViewInitContext,
    depth: usize,
    surface_pos: Point<LogicalUnit>,
    layouted_items: impl FnOnce(f32) -> Vec<MenuItemLayout>,
    setup_contents: impl FnOnce(
        Vec<MenuItemLayout>,
        ContextMenuHandle,
        &mut ViewInitContext,
    ) -> Vec<MenuItemView>,
) -> Handle {
    let layouted_items = layouted_items(view_init_context.ui_scale_factor);
    let width = MenuItemLayout::min_width(layouted_items.iter());
    let height = MenuItemLayout::height(layouted_items.iter());
    tracing::debug!(%width, %height, "pop context menu");

    let mut surface = unsafe { &*syslink.display_server.wl_global_interfaces }
        .compositor
        .create_surface()
        .expect("compositor.create_surface");
    let xdg_surface = unsafe { &*syslink.display_server.wl_global_interfaces }
        .xdg_wm_base
        .get_xdg_surface(&surface)
        .expect("xdg_wm_base.get_xdg_surface");
    let scaling = if let Some(ref fs) =
        unsafe { &*syslink.display_server.wl_global_interfaces }.fractional_scale_manager
    {
        let f = fs
            .get_fractional_scale(&surface)
            .expect("fractional_scale.create");
        let vp = unsafe { &*syslink.display_server.wl_global_interfaces }
            .viewporter
            .get_viewport(&surface)
            .expect("viewporter.get_viewport");

        WindowScaling::Manual {
            fractional_scale: f,
            viewport: vp,
        }
    } else {
        WindowScaling::Automatic
    };

    let pos = unsafe { &*syslink.display_server.wl_global_interfaces }
        .xdg_wm_base
        .create_positioner()
        .expect("create_positioner");
    pos.set_offset(surface_pos.x.round() as _, surface_pos.y.round() as _)
        .expect("pos.set_offset");
    pos.set_size(width.value().ceil() as _, height.value().ceil() as _)
        .expect("pos.set_size");
    pos.set_anchor_rect(0, 0, 1, 1)
        .expect("pos.set_anchor_rect");
    pos.set_gravity(wl::XdgPositionerGravity::BottomRight)
        .expect("pos.set_gravity");
    let xdg_popup = xdg_surface
        .get_popup(Some(&parent.event_listener().state.data.xdg_surface), &pos)
        .expect("xdg_surface.get_popup");

    let ct_root = view_init_context.composite_tree.create(CompositeRect {
        relative_size_adjustment: [1.0, 1.0],
        has_bitmap: true,
        composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.375])),
        ..Default::default()
    });
    let ht_root = view_init_context.ht_manager.create(HitTestTreeData {
        width_adjustment_factor: 1.0,
        height_adjustment_factor: 1.0,
        ..Default::default()
    });
    let base_surface_event_handler = Rc::new(MenuBaseSurfaceEventHandler::new(depth));
    view_init_context
        .ht_manager
        .set_action_handler(ht_root, &base_surface_event_handler);
    let mut eh = Box::new(EventHandler(SurfaceState {
        tag: SurfaceStateTag::ContextMenu,
        data: InstanceData {
            surface_ptr: surface.as_ptr(),
            scaling,
            xdg_surface,
            xdg_popup,
            ct_root,
            ht_root,
            keyboard_focus_state: PerWindowKeyboardFocusState::new(),
            spawned_position: surface_pos,
            committed_state: Mutex::new(CommittedState {
                size: Size::new_logical(width.value(), height.value()),
                size_pixels: Size::new_logical(width.value(), height.value())
                    .to_pixels_ceil(view_init_context.ui_scale_factor),
                buffer_scale: view_init_context.ui_scale_factor,
            }),
            swapchain_externally_invalidation_signal: AtomicBool::new(false),
            latest_ui_scale_changes: Mutex::new(None),
            pending_configure_buffer_scale: None,
            pending_configure_size: (None, None),
            event_dispatcher: unsafe { &*syslink.event_dispatcher }.clone(),
            _base_surface_event_handler: base_surface_event_handler,
            views: vec![],
        },
    }));
    surface
        .set_listener(&mut *eh)
        .into_result()
        .expect("surface.set_listener");
    unsafe { &mut *eh.0.data.xdg_surface.as_ptr() }
        .set_listener(&mut *eh)
        .into_result()
        .expect("xdg_surface.set_listener");
    unsafe { &mut *eh.0.data.xdg_popup.as_ptr() }
        .set_listener(&mut *eh)
        .into_result()
        .expect("xdg_popup.set_listener");
    if let WindowScaling::Manual {
        ref mut fractional_scale,
        ..
    } = eh.0.data.scaling
    {
        unsafe { &mut *fractional_scale.as_ptr() }
            .set_listener(&mut *eh)
            .into_result()
            .expect("fractional_scale.set_listener");
    }

    // give ownership
    surface.set_user_data(Box::into_raw(eh).cast());

    let vk_surface = unsafe {
        br::WaylandSurfaceCreateInfo::new(
            (*syslink.display_server.wl_display).as_raw().cast(),
            surface.as_ptr().cast(),
        )
        .execute((&*syslink.vk_device).instance(), None)
        .expect("vk_surface.create")
    };
    let vk_surface = VulkanSurface::new(unsafe { &*syslink.vk_device }, vk_surface);
    syslink
        .rt_sender
        .send(RenderMessage::NewContextMenu(NewContextMenuData {
            w: Handle(surface.as_ptr()),
            vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            composite_root: ct_root,
        }))
        .expect("rt_sender.send");

    surface.commit().expect("surface.commit");
    let views = setup_contents(layouted_items, Handle(surface.as_ptr()), view_init_context);
    unsafe { &mut *surface.user_data().cast::<SurfaceState<InstanceData>>() }
        .data
        .views = views;
    Handle(surface.unwrap().as_ptr())
}

pub fn close(
    handle: Handle,
    syslink: &SystemLink,
    composite_tree: &mut CompositeTree<SyncEvent>,
    ht_manager: &mut HitTestTreeManager,
) {
    let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
    syslink
        .rt_sender
        .send(RenderMessage::DestroyContextMenu(handle, done_event_sender))
        .expect("rt_sender.send");
    done_event_receiver
        .recv()
        .expect("done_event_receiver.recv");

    let eh = unsafe { Box::from_raw((&*handle.0).user_data().cast::<EventHandler>()) };
    composite_tree.free_all(eh.0.data.ct_root);
    ht_manager.free_all(eh.0.data.ht_root);
    drop(eh);

    drop(unsafe { wl::Owned::wrap_unchecked(core::ptr::NonNull::new_unchecked(handle.0)) });
}
