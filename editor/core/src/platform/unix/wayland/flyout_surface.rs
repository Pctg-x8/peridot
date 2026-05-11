use core::ptr::NonNull;
use std::sync::{Mutex, atomic::AtomicBool};

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};
use peridot_tp_wayland as wl;

use crate::{
    Event, LogicFiberEventDispatcher, SystemLink,
    graphics::VulkanSurface,
    input::{
        KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, PerWindowKeyboardFocusState,
        hittest::{HitTestTreeData, HitTestTreeManager, HitTestTreeRef},
    },
    platform::unix::wayland::{SurfaceState, SurfaceStateTag, WindowScaling},
    rendering::{
        NewContextMenuData, NewWindowVulkanSurface, RenderMessage,
        composite::{
            AnimatableColor, CompositeMode, CompositeRect, CompositeTree, CompositeTreeRef,
        },
    },
    uikit::{MenuItemSubMenuView, MountTarget},
    utils::{LogicalUnit, PixelsUnit, Point, Size, platform::linux::TimerFD},
};

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Handle(pub(super) NonNull<wl::Surface>);
unsafe impl Sync for Handle {}
unsafe impl Send for Handle {}
impl Handle {
    #[inline(always)]
    fn data(&self) -> &InstanceData {
        &unsafe {
            &*self
                .0
                .as_ref()
                .user_data()
                .cast::<SurfaceState<InstanceData>>()
        }
        .data
    }

    #[inline(always)]
    fn data_mut(&mut self) -> &mut InstanceData {
        &mut unsafe {
            &mut *self
                .0
                .as_mut()
                .user_data()
                .cast::<SurfaceState<InstanceData>>()
        }
        .data
    }

    pub fn close<E>(
        mut self,
        syslink: &SystemLink,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
        syslink
            .rt_sender
            .send(RenderMessage::DestroyContextMenu(self, done_event_sender))
            .expect("rt_sender.send");
        done_event_receiver
            .recv_timeout(std::time::Duration::from_millis(1000))
            .expect("done_event_receiver.recv");
        tracing::debug!("render surface done");

        let eh = unsafe { Box::from_raw(self.0.as_mut().user_data().cast::<EventHandler>()) };
        composite_tree.free_all(eh.0.data.ct_root);
        ht_manager.free_all(eh.0.data.ht_root);
        keyboard_focus_registry.release_group(eh.0.data.kf_root_group);
        drop(eh);

        drop(unsafe { wl::Owned::wrap_unchecked(self.0) });
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

    pub fn submenu_pop_position(&self, view: &MenuItemSubMenuView) -> Point<LogicalUnit> {
        let base = self.data().spawned_position;
        let size = self.data().committed_state.lock().expect("poisoned").size;

        Point::new_logical(base.x + size.width, base.y + view.placement_y)
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
    _blur: Option<wl::Owned<wl::OrgKdeKwinBlur>>,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    keyboard_focus_state: PerWindowKeyboardFocusState,
    kf_root_group: KeyboardFocusGroupRef,
    spawned_position: Point<LogicalUnit>,
    committed_state: Mutex<CommittedState>,
    pub swapchain_externally_invalidation_signal: AtomicBool,
    pub latest_ui_scale_changes: Mutex<Option<f32>>,
    pending_configure_size: (Option<i32>, Option<i32>),
    pending_configure_buffer_scale: Option<f32>,
    event_dispatcher: LogicFiberEventDispatcher,
    _pinned: core::marker::PhantomPinned,
}

#[repr(transparent)]
struct EventHandler(SurfaceState<InstanceData>);
impl wl::SurfaceEventListener for EventHandler {
    fn enter(
        &mut self,
        _surface: &mut peridot_tp_wayland::Surface,
        _output: &mut peridot_tp_wayland::Output,
    ) {
        tracing::debug!("context menu enter");
    }

    fn leave(
        &mut self,
        _surface: &mut peridot_tp_wayland::Surface,
        _output: &mut peridot_tp_wayland::Output,
    ) {
        tracing::debug!("context menu leave");
    }

    fn preferred_buffer_scale(&mut self, _surface: &mut peridot_tp_wayland::Surface, factor: i32) {
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
        _surface: &mut peridot_tp_wayland::Surface,
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
        _sender: &mut peridot_tp_wayland::XdgPopup,
        x: i32,
        y: i32,
        width: i32,
        height: i32,
    ) {
        tracing::debug!(x, y, width, height, "context menu configure");
    }

    fn popup_done(&mut self, _sender: &mut peridot_tp_wayland::XdgPopup) {
        tracing::debug!("context menu done");
    }

    fn repositioned(&mut self, _sender: &mut peridot_tp_wayland::XdgPopup, token: u32) {
        tracing::debug!(token, "context menu repositioned");
    }
}
impl wl::WpFractionalScaleV1EventListener for EventHandler {
    fn preferred_scale(
        &mut self,
        _sender: &mut peridot_tp_wayland::WpFractionalScaleV1,
        scale: u32,
    ) {
        tracing::debug!(scale, "context menu preferred scale(fractional)");
        self.0.data.pending_configure_buffer_scale = Some(scale as f32 / 120.0);
    }
}

pub struct SharedState {
    pub delayed_action_timer: TimerFD,
}
impl SharedState {
    pub fn reserve_delayed_action(&self) {
        self.delayed_action_timer
            .set(0, 400 * 1000 * 1000)
            .expect("timerfd.set");
    }

    pub fn unreserve_delayed_action(&self) {
        self.delayed_action_timer.unset().expect("timerfd.unset");
    }
}

pub fn new_surface<E>(
    parent: super::WindowHandle,
    pos: Point<LogicalUnit>,
    size: Size<LogicalUnit>,
    syslink: &SystemLink,
    composite_tree: &mut CompositeTree<E>,
    ht_manager: &mut HitTestTreeManager,
    keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ref_scale_factor: f32,
) -> Handle {
    let mut surface = unsafe { &*syslink.display_server.context }
        .global_interfaces
        .compositor
        .create_surface()
        .expect("compositor.create_surface");
    let xdg_surface = unsafe { &*syslink.display_server.context }
        .global_interfaces
        .xdg_wm_base
        .get_xdg_surface(&surface)
        .expect("xdg_wm_base.get_xdg_surface");
    let scaling = if let Some(ref fs) = unsafe { &*syslink.display_server.context }
        .global_interfaces
        .fractional_scale_manager
    {
        let f = fs
            .get_fractional_scale(&surface)
            .expect("fractional_scale.create");
        let vp = unsafe { &*syslink.display_server.context }
            .global_interfaces
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
    let blur = if let Some(ref bm) = unsafe { &*syslink.display_server.context }
        .global_interfaces
        .kde_blur_manager
    {
        let b = bm.create(&surface).expect("blur_manager.create");
        b.commit().expect("blur.commit");

        Some(b)
    } else {
        None
    };

    let p = unsafe { &*syslink.display_server.context }
        .global_interfaces
        .xdg_wm_base
        .create_positioner()
        .expect("create_positioner");
    p.set_offset(pos.x.round() as _, pos.y.round() as _)
        .expect("pos.set_offset");
    p.set_size(size.width.ceil() as _, size.height.ceil() as _)
        .expect("pos.set_size");
    p.set_anchor_rect(0, 0, 1, 1).expect("pos.set_anchor_rect");
    p.set_gravity(wl::XdgPositionerGravity::BottomRight)
        .expect("pos.set_gravity");
    let xdg_popup = xdg_surface
        .get_popup(Some(&parent.event_listener().state.data.xdg_surface), &p)
        .expect("xdg_surface.get_popup");

    let ct_root = composite_tree.create(CompositeRect {
        relative_size_adjustment: [1.0, 1.0],
        has_bitmap: true,
        composite_mode: CompositeMode::FillColor(AnimatableColor::Value([0.0, 0.0, 0.0, 0.375])),
        ..Default::default()
    });
    let ht_root = ht_manager.create(HitTestTreeData {
        width_adjustment_factor: 1.0,
        height_adjustment_factor: 1.0,
        ..Default::default()
    });
    let kf_root_group = keyboard_focus_registry.acquire_group();
    let mut eh = Box::new(EventHandler(SurfaceState {
        tag: SurfaceStateTag::FlyoutSurface,
        data: InstanceData {
            surface_ptr: surface.as_ptr(),
            scaling,
            xdg_surface,
            xdg_popup,
            _blur: blur,
            ct_root,
            ht_root,
            keyboard_focus_state: PerWindowKeyboardFocusState::new(kf_root_group),
            kf_root_group,
            spawned_position: pos,
            committed_state: Mutex::new(CommittedState {
                size: size,
                size_pixels: size.to_pixels_ceil(ref_scale_factor),
                buffer_scale: ref_scale_factor,
            }),
            swapchain_externally_invalidation_signal: AtomicBool::new(false),
            latest_ui_scale_changes: Mutex::new(None),
            pending_configure_buffer_scale: None,
            pending_configure_size: (None, None),
            event_dispatcher: unsafe { &*syslink.event_dispatcher }.clone(),
            _pinned: core::marker::PhantomPinned,
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
            (*syslink.display_server.context).dp.as_raw().cast(),
            surface.as_ptr().cast(),
        )
        .execute((&*syslink.vk_device).instance(), None)
        .expect("vk_surface.create")
    };
    let vk_surface = VulkanSurface::new(unsafe { &*syslink.vk_device }, vk_surface);
    syslink
        .rt_sender
        .send(RenderMessage::NewContextMenu(NewContextMenuData {
            w: Handle(unsafe { NonNull::new_unchecked(surface.as_ptr()) }),
            vk_surface: NewWindowVulkanSurface(vk_surface.unbound().1),
            composite_root: ct_root,
        }))
        .expect("rt_sender.send");

    surface.commit().expect("surface.commit");
    Handle(surface.unwrap())
}
