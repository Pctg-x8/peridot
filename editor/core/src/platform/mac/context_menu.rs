use core::ptr::NonNull;

use bedrock::{self as br, InstanceChild, SurfaceCreateInfo};

use crate::{
    Event, LogicFiberEventDispatcher, SyncEvent, SystemLink,
    graphics::VulkanSurface,
    input::{
        KeyboardFocusGroupRef, KeyboardFocusTokenRegistry, PerWindowKeyboardFocusState,
        hittest::{HitTestTreeData, HitTestTreeManager, HitTestTreeRef, PointerButton},
    },
    rendering::{
        NewContextMenuData, NewWindowVulkanSurface, RenderMessage,
        composite::{CompositeRect, CompositeTree, CompositeTreeRef},
    },
    uikit::{MenuItemView, MountTarget},
    utils::{LogicalUnit, PixelsUnit, Point, Size},
};

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Handle(NonNull<super::bridge::ContextMenuSurface>);
unsafe impl Sync for Handle {}
unsafe impl Send for Handle {}
impl MountTarget for Handle {
    #[inline(always)]
    fn ct_root(&self) -> CompositeTreeRef {
        self.instance_vars().ct_root
    }

    #[inline(always)]
    fn ht_root(&self) -> HitTestTreeRef {
        self.instance_vars().ht_root
    }
}
impl Handle {
    pub(super) fn new<E>(
        parent: super::WindowHandle,
        depth: usize,
        surface_pos: Point<LogicalUnit>,
        syslink: &SystemLink,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) -> Self {
        let ct_root = composite_tree.create(CompositeRect {
            relative_size_adjustment: [1.0, 1.0],
            // macの場合は背景は不要（NSVisualEffectViewが背景がわりになる）
            ..Default::default()
        });
        let ht_root = ht_manager.create(HitTestTreeData {
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            ..Default::default()
        });
        let kf_root_group = keyboard_focus_registry.acquire_group();
        let h = Self(unsafe {
            NonNull::new_unchecked(super::bridge::ni_create_context_menu_surface(
                parent.0,
                surface_pos.x,
                surface_pos.y,
                Box::into_raw(Box::new(InstanceVars {
                    event_dispatcher: syslink.event_dispatcher,
                    depth,
                    ct_root,
                    ht_root,
                    kf_state: PerWindowKeyboardFocusState::new(kf_root_group),
                    kf_root_group,
                    views: Vec::new(),
                    spawned_position: surface_pos,
                    size: Size::new_logical(0.0, 0.0),
                }))
                .cast(),
                Box::into_raw(Box::new(super::bridge::ContextMenuSurfaceCallbacks {
                    on_pointer_down: Self::pointer_down,
                    on_pointer_move: Self::pointer_move,
                    on_pointer_up: Self::pointer_up,
                    on_pointer_leave: Self::pointer_leave,
                })),
            ))
        });

        h
    }

    pub(super) fn set_views(&mut self, views: Vec<MenuItemView>) {
        unsafe {
            &mut *super::bridge::ni_context_menu_instance_vars_ptr(self.0.as_ptr())
                .cast::<InstanceVars>()
        }
        .views = views;
    }

    pub(super) fn create_render_thread_objects(&self, syslink: &SystemLink) {
        syslink
            .rt_sender
            .send(RenderMessage::NewContextMenu(NewContextMenuData {
                w: *self,
                vk_surface: NewWindowVulkanSurface(
                    VulkanSurface::new(unsafe { &*syslink.vk_device }, unsafe {
                        br::MetalSurfaceCreateInfo::new(
                            super::bridge::ni_context_menu_get_metal_layer(self.0.as_ptr())
                                .cast_const(),
                        )
                        .execute((&*syslink.vk_device).instance(), None)
                        .expect("vk_surface.create")
                    })
                    .unbound()
                    .1,
                ),
                composite_root: self.instance_vars().ct_root,
            }))
            .expect("rt_sender.send");
    }

    pub fn close(
        self,
        syslink: &SystemLink,
        composite_tree: &mut CompositeTree<SyncEvent>,
        ht_manager: &mut HitTestTreeManager,
        keyboard_focus_registry: &mut KeyboardFocusTokenRegistry,
    ) {
        let (done_event_sender, done_event_receiver) = std::sync::mpsc::channel();
        syslink
            .rt_sender
            .send(RenderMessage::DestroyContextMenu(self, done_event_sender))
            .expect("rt_sender.send");
        let tpctx = unsafe { super::bridge::ni_degreade_thread_priroity_temporarily() };
        done_event_receiver
            .recv()
            .expect("done_event_receiver.recv");
        unsafe {
            super::bridge::ni_restore_thread_priority(tpctx);
        }

        let mut instance_vars = core::mem::MaybeUninit::uninit();
        let mut callbacks = core::mem::MaybeUninit::uninit();
        unsafe {
            super::bridge::ni_release_context_menu_surface(
                self.0.as_ptr(),
                instance_vars.as_mut_ptr(),
                callbacks.as_mut_ptr(),
            );
        }
        let instance_vars =
            unsafe { Box::from_raw(instance_vars.assume_init().cast::<InstanceVars>()) };
        let callbacks = unsafe { Box::from_raw(callbacks.assume_init()) };
        composite_tree.free_all(instance_vars.ct_root);
        ht_manager.free_all(instance_vars.ht_root);
        keyboard_focus_registry.release_group(instance_vars.kf_root_group);
        drop(callbacks);
    }

    pub(super) fn resize(&mut self, size: Size<LogicalUnit>) {
        unsafe {
            super::bridge::ni_context_menu_resize(self.0.as_ptr(), size.width, size.height);
        }
        self.instance_vars_mut().size = size;
    }

    #[inline(always)]
    fn instance_vars(&self) -> &InstanceVars {
        unsafe {
            &*super::bridge::ni_context_menu_instance_vars_ptr(self.0.as_ptr())
                .cast::<InstanceVars>()
        }
    }

    #[inline(always)]
    fn instance_vars_mut(&mut self) -> &mut InstanceVars {
        unsafe {
            &mut *super::bridge::ni_context_menu_instance_vars_ptr(self.0.as_ptr())
                .cast::<InstanceVars>()
        }
    }

    pub fn logical_size(&self) -> Size<LogicalUnit> {
        self.instance_vars().size
    }

    pub fn pixels_size(&self) -> Size<PixelsUnit> {
        self.instance_vars()
            .size
            .to_pixels_ceil(self.render_scale())
    }

    pub fn render_scale(&self) -> f32 {
        unsafe { super::bridge::ni_context_menu_get_content_scale(self.0.as_ptr()) }
    }

    #[inline(always)]
    pub fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        &mut self.instance_vars_mut().kf_state
    }

    #[inline(always)]
    pub fn view(&self, index: usize) -> Option<&MenuItemView> {
        self.instance_vars().views.get(index)
    }

    pub fn submenu_pop_position(&self, index: usize) -> Option<Point<LogicalUnit>> {
        let ivars = self.instance_vars();

        match ivars.views.get(index)? {
            MenuItemView::SubMenu(x) => {
                let base = ivars.spawned_position;
                let size = ivars.size;

                Some(Point::new_logical(
                    base.x + size.width,
                    base.y + x.placement_y,
                ))
            }
            _ => None,
        }
    }

    extern "C" fn pointer_down(
        sender: *mut super::bridge::ContextMenuSurface,
        x: f64,
        y: f64,
        button: super::bridge::MouseButton,
    ) {
        let h = Self(unsafe { NonNull::new_unchecked(sender) });

        // move then down
        h.instance_vars()
            .dispatch_event(Event::ContextMenuPointerMove {
                pointer_id: super::PointerID(),
                target: h,
                client_pos: Point::new_logical(x as _, y as _),
            });
        h.instance_vars()
            .dispatch_event(Event::ContextMenuPointerDown {
                pointer_id: super::PointerID(),
                target: h,
                button: match button {
                    super::bridge::MouseButton::Left => PointerButton::Primary,
                    super::bridge::MouseButton::Right => PointerButton::Secondary,
                },
            });
    }

    extern "C" fn pointer_move(sender: *mut super::bridge::ContextMenuSurface, x: f64, y: f64) {
        let h = Self(unsafe { NonNull::new_unchecked(sender) });

        h.instance_vars()
            .dispatch_event(Event::ContextMenuPointerMove {
                pointer_id: super::PointerID(),
                target: h,
                client_pos: Point::new_logical(x as _, y as _),
            });
    }

    extern "C" fn pointer_up(
        sender: *mut super::bridge::ContextMenuSurface,
        button: super::bridge::MouseButton,
    ) {
        let h = Self(unsafe { NonNull::new_unchecked(sender) });

        h.instance_vars()
            .dispatch_event(Event::ContextMenuPointerUp {
                pointer_id: super::PointerID(),
                target: h,
                button: match button {
                    super::bridge::MouseButton::Left => PointerButton::Primary,
                    super::bridge::MouseButton::Right => PointerButton::Secondary,
                },
            });
    }

    extern "C" fn pointer_leave(sender: *mut super::bridge::ContextMenuSurface) {
        let h = Self(unsafe { NonNull::new_unchecked(sender) });

        h.instance_vars()
            .dispatch_event(Event::ContextMenuDeselectItem {
                depth: h.instance_vars().depth,
            });
    }
}

struct InstanceVars {
    event_dispatcher: *mut LogicFiberEventDispatcher,
    depth: usize,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    kf_state: PerWindowKeyboardFocusState,
    kf_root_group: KeyboardFocusGroupRef,
    views: Vec<MenuItemView>,
    spawned_position: Point<LogicalUnit>,
    size: Size<LogicalUnit>,
}
impl InstanceVars {
    fn dispatch_event(&self, event: Event) {
        unsafe { &*self.event_dispatcher }.dispatch(event);
    }
}

pub struct SharedState {
    pub event_dispatcher: *mut LogicFiberEventDispatcher,
}
impl SharedState {
    pub fn reserve_delayed_action(&self) {
        extern "C" fn cb(ctx: *mut core::ffi::c_void) {
            unsafe { &*ctx.cast::<LogicFiberEventDispatcher>() }
                .dispatch(Event::ContextMenuPerformDelayedAction);
        }

        unsafe {
            super::bridge::ni_context_menu_reserve_delayed_action(
                400,
                cb,
                self.event_dispatcher.cast(),
            );
        }
    }

    pub fn unreserve_delayed_action(&self) {
        unsafe {
            super::bridge::ni_context_menu_unreserve_delayed_action();
        }
    }

    pub fn observe_global_click(&self) {
        extern "C" fn cb(ctx: *mut core::ffi::c_void, on_context_menu_surface: u8) {
            if on_context_menu_surface == 0 {
                // コンテキストメニュー以外でクリックが入った
                unsafe { &*ctx.cast::<LogicFiberEventDispatcher>() }
                    .dispatch(Event::ContextMenuCloseAll);
            }
        }

        unsafe {
            super::bridge::ni_context_menu_observe_global_click(cb, self.event_dispatcher.cast());
        }
    }

    pub fn unobserve_global_click(&self) {
        unsafe {
            super::bridge::ni_context_menu_unobserve_global_click();
        }
    }
}
