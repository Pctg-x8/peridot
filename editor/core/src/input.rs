use std::{
    collections::{BTreeSet, HashMap},
    rc::{Rc, Weak},
    time::{Duration, Instant},
};

use bitflags::bitflags;

use crate::{
    ContextMenuHandle, DragPreviewPopoverHandle, PointerID, SyncEvent, SystemLink, WindowHandle,
    input::hittest::{
        CursorShape, HitTestTreeManager, HitTestTreeRef, PointerActionArgs, PointerButton,
        PointerButtonActionArgs, Role,
    },
    rendering::composite::CompositeTree,
    utils::{LogicalUnit, Point, Rect, Size},
};

pub mod hittest;
pub type PointerInputUnit = LogicalUnit;

const CLICK_DETECTION_MAX_DISTANCE: f32 = 4.0;
const DOUBLE_CLICK_DETECTION_MAX_DISTANCE: f32 = 4.0;
const DOUBLE_CLICK_DETECTION_MAX_TIME: Duration = Duration::from_millis(500);
pub const POINTER_HOVER_TIMEOUT_MS: u32 = 400;

pub struct InputEventContext<'env, 'sys, 'h> {
    pub current_sec: f32,
    pub composite_tree: &'env mut CompositeTree<SyncEvent>,
    pub system_link: &'env mut SystemLink<'sys>,
    pub drag_preview_popover: &'env DragPreviewPopoverHandle,
    pub ht_manager: &'env HitTestTreeManager<'h>,
}

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct EventContinueControl: usize {
        const STOP_PROPAGATION = 1 << 0;
        const CAPTURE_ELEMENT = 1 << 1;
        const RELEASE_CAPTURE_ELEMENT = 1 << 2;
        const RECOMPUTE_POINTER_ENTER = 1 << 3;
    }
}
impl EventContinueControl {
    #[inline(always)]
    fn releasing_capture(&self) -> bool {
        self.contains(Self::RELEASE_CAPTURE_ELEMENT)
    }

    #[inline(always)]
    fn needs_recompute_pointer_enter(&self) -> bool {
        // captureがreleaseされたときもenterを再計算する必要がある
        self.contains(Self::RECOMPUTE_POINTER_ENTER | Self::RELEASE_CAPTURE_ELEMENT)
    }
}

#[derive(Debug)]
enum PointerFocusState {
    None,
    Entering(HitTestTreeRef),
    Capturing(HitTestTreeRef),
}

enum PointerDownGestureState {
    None,
    Click {
        base_client_pos: Point<PointerInputUnit>,
        initiator_button: PointerButton,
    },
    Drag,
}
impl PointerDownGestureState {
    const fn is_dragging(&self) -> bool {
        matches!(self, Self::Drag)
    }

    #[inline(always)]
    fn is_click(&self, button: PointerButton) -> bool {
        matches!(self, &Self::Click { initiator_button, .. } if initiator_button == button)
    }
}

pub trait ShellPointerActions {
    fn capture_pointer(&self);
    fn release_pointer(&self);
}

struct LastClickState {
    count: usize,
    surface: NativeDesktopSurface,
    button: PointerButton,
    pos: Point<PointerInputUnit>,
    time: Instant,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum NativeDesktopSurface {
    Window(WindowHandle),
    ContextMenu(ContextMenuHandle),
}
impl ShellPointerActions for NativeDesktopSurface {
    #[inline(always)]
    fn capture_pointer(&self) {
        match self {
            NativeDesktopSurface::Window(w) => w.capture_pointer(),
            NativeDesktopSurface::ContextMenu(_) => {
                unimplemented!("not implemented for context menu")
            }
        }
    }

    #[inline(always)]
    fn release_pointer(&self) {
        match self {
            NativeDesktopSurface::Window(w) => w.release_pointer(),
            NativeDesktopSurface::ContextMenu(_) => {
                unimplemented!("not implemented for context menu")
            }
        }
    }
}
impl NativeDesktopSurface {
    #[inline(always)]
    fn size(&self) -> Size<PointerInputUnit> {
        match self {
            NativeDesktopSurface::Window(w) => w.client_size(),
            NativeDesktopSurface::ContextMenu(w) => w.logical_size(),
        }
    }

    #[inline(always)]
    fn keyboard_focus_state_mut(&mut self) -> &mut PerWindowKeyboardFocusState {
        match self {
            NativeDesktopSurface::Window(w) => w.keyboard_focus_state_mut(),
            NativeDesktopSurface::ContextMenu(w) => w.keyboard_focus_state_mut(),
        }
    }
}

// TODO: マルチタッチ対応（PointerIDごとにジェスチャー管理を分ける必要があるはず）
pub struct PointerInputManager {
    last_client_pointer_pos: HashMap<PointerID, (NativeDesktopSurface, Point<PointerInputUnit>)>,
    pointer_focus: PointerFocusState,
    down_gesture: PointerDownGestureState,
    last_click: Option<LastClickState>,
}
impl PointerInputManager {
    pub fn new() -> Self {
        PointerInputManager {
            last_client_pointer_pos: HashMap::new(),
            pointer_focus: PointerFocusState::None,
            down_gesture: PointerDownGestureState::None,
            last_click: None,
        }
    }

    fn dispatch_pointer_enter(
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_target: HitTestTreeRef,
    ) {
        for ht_ref in ht.iter_ascending_from(ht_target) {
            let Some(a) = ht.get_data(ht_ref).action_handler() else {
                continue;
            };

            let flags = a.on_pointer_enter(ht_ref, action_context, action_args);
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }
    }

    fn dispatch_pointer_leave(
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_target: HitTestTreeRef,
    ) {
        for ht_ref in ht.iter_ascending_from(ht_target) {
            let Some(a) = ht.get_data(ht_ref).action_handler() else {
                continue;
            };

            let flags = a.on_pointer_leave(ht_ref, action_context, action_args);
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }
    }

    fn dispatch_pointer_down(
        surface: &mut NativeDesktopSurface,
        action_args: &PointerButtonActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_target: HitTestTreeRef,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) -> (bool, Option<HitTestTreeRef>) {
        let mut needs_recompute_pointer_enter = false;
        let mut new_captured = None;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_pointer_down(ht_ref, action_context, action_args)
                });

            Self::update_keyboard_focus(
                surface,
                ht.get_data(ht_ref).keyboard_focus,
                action_context,
                kf_registry,
            );

            if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                needs_recompute_pointer_enter = true;
            }
            if flags.contains(EventContinueControl::CAPTURE_ELEMENT) {
                new_captured = Some(ht_ref);
                surface.capture_pointer();
            }
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }

        (needs_recompute_pointer_enter, new_captured)
    }

    fn dispatch_pointer_move(
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_target: HitTestTreeRef,
    ) -> bool {
        let mut needs_recompute_pointer_enter = false;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let Some(a) = ht.get_data(ht_ref).action_handler() else {
                continue;
            };

            let flags = a.on_pointer_move(ht_ref, action_context, action_args);
            if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                needs_recompute_pointer_enter = true;
            }
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }

        needs_recompute_pointer_enter
    }

    fn dispatch_pointer_up(
        sh: &(impl ShellPointerActions + ?Sized),
        action_args: &PointerButtonActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_target: HitTestTreeRef,
    ) -> (bool, bool) {
        let mut needs_recompute_pointer_enter = false;
        let mut capture_released = false;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let Some(a) = ht.get_data(ht_ref).action_handler() else {
                continue;
            };

            let flags = a.on_pointer_up(ht_ref, action_context, action_args);
            if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                needs_recompute_pointer_enter = true;
            }
            if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                sh.release_pointer();
                capture_released = true;
            }
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }

        (needs_recompute_pointer_enter, capture_released)
    }

    fn begin_drag(
        &mut self,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        action_args: &PointerButtonActionArgs,
        shell: &(impl ShellPointerActions + ?Sized),
    ) {
        self.down_gesture = PointerDownGestureState::Drag;

        match self.pointer_focus {
            PointerFocusState::None => unreachable!("drag started without focus?"),
            PointerFocusState::Capturing(e) => {
                let _ = ht
                    .get_data(e)
                    .action_handler()
                    .map_or(EventContinueControl::empty(), |h| {
                        h.on_drag_start(e, action_context, action_args)
                    });
                // TODO: begin_dragでなんらかのフラグを処理する必要があるか？
            }
            PointerFocusState::Entering(e) => {
                for ht_ref in ht.iter_ascending_from(e) {
                    let Some(a) = ht.get_data(ht_ref).action_handler() else {
                        continue;
                    };

                    let flags = a.on_drag_start(ht_ref, action_context, action_args);
                    if flags.contains(EventContinueControl::CAPTURE_ELEMENT) {
                        self.pointer_focus = PointerFocusState::Capturing(ht_ref);
                        shell.capture_pointer();
                    }
                    if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                        break;
                    }
                }
            }
        }
    }

    fn end_drag(
        &mut self,
        sh: &(impl ShellPointerActions + ?Sized),
        ht: &HitTestTreeManager,
        ht_root: HitTestTreeRef,
        action_context: &mut InputEventContext,
        button: PointerButton,
        pointer_id: PointerID,
        client_pos: Point<PointerInputUnit>,
        window_size: Size<PointerInputUnit>,
    ) {
        let mut needs_recompute_pointer_enter = false;

        let args = PointerButtonActionArgs {
            button,
            pointer_id,
            client_pos,
            client_size: window_size,
        };
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht
                    .get_data(ht_ref)
                    .action_handler()
                    .map_or(EventContinueControl::empty(), |h| {
                        h.on_drag_end(ht_ref, action_context, &args)
                    });

                if flags.releasing_capture() {
                    sh.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                }
                needs_recompute_pointer_enter = flags.needs_recompute_pointer_enter();
            }
            PointerFocusState::Entering(ht_ref) => {
                let mut capture_released = false;

                for ht_ref in ht.iter_ascending_from(ht_ref) {
                    let Some(a) = ht.get_data(ht_ref).action_handler() else {
                        continue;
                    };

                    let flags = a.on_drag_end(ht_ref, action_context, &args);
                    if flags.releasing_capture() {
                        sh.release_pointer();
                        capture_released = true;
                    }
                    if flags.needs_recompute_pointer_enter() {
                        needs_recompute_pointer_enter = true;
                    }
                    if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                        break;
                    }
                }

                if capture_released {
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                }
            }
            PointerFocusState::None => (),
        }

        if needs_recompute_pointer_enter {
            self.update_pointer_enter(
                &window_size,
                pointer_id,
                client_pos,
                ht,
                action_context,
                ht_root,
            );
        }
    }

    fn update_pointer_enter<'env, 'sys, 'h>(
        &mut self,
        window_size: &Size<PointerInputUnit>,
        pointer_id: PointerID,
        client_pos: Point<PointerInputUnit>,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext<'env, 'sys, 'h>,
        ht_root: HitTestTreeRef,
    ) {
        let new_hit = ht.test(
            ht_root,
            &client_pos,
            &Rect::from_lt_size(Point::new_logical(0.0, 0.0), *window_size),
        );
        let (new_leave, new_enter) = match (&self.pointer_focus, new_hit) {
            // in capturing, this routine is never called
            (&PointerFocusState::Capturing(_), _) => unreachable!(),
            // entering changed
            (&PointerFocusState::Entering(old), Some(new)) if old != new => (Some(old), Some(new)),
            // nothing changed
            (&PointerFocusState::Entering(_), Some(_)) => (None, None),
            // just leave
            (&PointerFocusState::Entering(old), None) => (Some(old), None),
            // just enter
            (&PointerFocusState::None, Some(new)) => (None, Some(new)),
            // nothing changed
            (&PointerFocusState::None, None) => (None, None),
        };

        if let Some(ht_ref) = new_leave {
            Self::dispatch_pointer_leave(
                &PointerActionArgs {
                    pointer_id,
                    client_pos,
                    client_size: *window_size,
                },
                ht,
                action_context,
                ht_ref,
            );
            self.pointer_focus = PointerFocusState::None;
            // leaveしたときはジェスチャもなかったことにする
            self.down_gesture = PointerDownGestureState::None;
            action_context.system_link.kill_pointer_hovering_timeout();
        }

        if let Some(ht_ref) = new_enter {
            self.pointer_focus = PointerFocusState::Entering(ht_ref);
            Self::dispatch_pointer_enter(
                &PointerActionArgs {
                    pointer_id,
                    client_pos,
                    client_size: *window_size,
                },
                ht,
                action_context,
                ht_ref,
            );
            action_context.system_link.set_pointer_hovering_timeout();
        }
    }

    pub fn handle_mouse_leave<'env, 'sys, 'h>(
        &mut self,
        pointer_id: PointerID,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext<'env, 'sys, 'h>,
    ) {
        let Some((entering_window, client_pos)) = self.last_client_pointer_pos.remove(&pointer_id)
        else {
            // not entered pointer
            return;
        };

        let new_leave = match &self.pointer_focus {
            // in capturing, this routine does nothing
            &PointerFocusState::Capturing(_) => {
                return;
            }
            // just leave
            &PointerFocusState::Entering(old) => Some(old),
            // nothing changed
            &PointerFocusState::None => None,
        };

        if let Some(ht_ref) = new_leave {
            Self::dispatch_pointer_leave(
                &PointerActionArgs {
                    pointer_id,
                    // TODO: leaveにclient_posいるか？
                    client_pos,
                    client_size: entering_window.size(),
                },
                ht,
                action_context,
                ht_ref,
            );
            self.pointer_focus = PointerFocusState::None;
            // leaveしたときはジェスチャもなかったことにする
            self.down_gesture = PointerDownGestureState::None;
            action_context.system_link.kill_pointer_hovering_timeout();
        }
    }

    pub fn handle_mouse_move<'env, 'sys, 'h>(
        &mut self,
        surface: NativeDesktopSurface,
        pointer_id: PointerID,
        client_pos: Point<PointerInputUnit>,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext<'env, 'sys, 'h>,
        ht_root: HitTestTreeRef,
    ) {
        self.last_client_pointer_pos
            .insert(pointer_id, (surface, client_pos));
        let ws = surface.size();

        if let PointerDownGestureState::Click {
            base_client_pos,
            initiator_button,
        } = self.down_gesture
            && client_pos.distance_sq(&base_client_pos) >= CLICK_DETECTION_MAX_DISTANCE.powi(2)
        {
            // 動きすぎたのでクリック状態をドラッグ化
            self.begin_drag(
                ht,
                action_context,
                &PointerButtonActionArgs {
                    button: initiator_button,
                    pointer_id,
                    client_pos,
                    client_size: ws,
                },
                &surface,
            );
        }

        if let PointerFocusState::Capturing(ht_ref) = self.pointer_focus {
            // キャプチャ中の要素があればそれにだけ流す
            if let Some(h) = ht.get_data(ht_ref).action_handler() {
                let args = PointerActionArgs {
                    pointer_id,
                    client_pos,
                    client_size: ws,
                };

                if self.down_gesture.is_dragging() {
                    h.on_drag_move(ht_ref, action_context, &args);
                } else {
                    h.on_pointer_move(ht_ref, action_context, &args);
                }
            }

            return;
        }

        self.update_pointer_enter(&ws, pointer_id, client_pos, ht, action_context, ht_root);

        if let PointerFocusState::Entering(ht_ref) = self.pointer_focus {
            let args = PointerActionArgs {
                pointer_id,
                client_pos,
                client_size: ws,
            };

            let mut needs_recompute_pointer_enter = false;
            if self.down_gesture.is_dragging() {
                for ht_ref in ht.iter_ascending_from(ht_ref) {
                    let Some(a) = ht.get_data(ht_ref).action_handler() else {
                        continue;
                    };

                    let flags = a.on_drag_move(ht_ref, action_context, &args);
                    if flags.needs_recompute_pointer_enter() {
                        needs_recompute_pointer_enter = true;
                    }
                    if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                        break;
                    }
                }
            } else {
                needs_recompute_pointer_enter =
                    Self::dispatch_pointer_move(&args, ht, action_context, ht_ref);
            }

            if needs_recompute_pointer_enter {
                self.update_pointer_enter(&ws, pointer_id, client_pos, ht, action_context, ht_root);
            }
        }
    }

    pub fn handle_pointer_hover(&mut self, action_context: &mut InputEventContext) {
        match self.pointer_focus {
            PointerFocusState::None => (),
            PointerFocusState::Entering(e) => {
                for ht_ref in action_context.ht_manager.iter_ascending_from(e) {
                    let flags = action_context
                        .ht_manager
                        .get_data(ht_ref)
                        .action_handler()
                        .map_or(EventContinueControl::empty(), |h| {
                            h.on_pointer_hover(
                                ht_ref,
                                action_context,
                                // TODO: Pointer IDでホバー分けて管理したほうがいいかも
                                &PointerActionArgs {
                                    #[allow(invalid_value)]
                                    pointer_id: unsafe {
                                        core::mem::MaybeUninit::uninit().assume_init()
                                    },
                                    client_pos: Point::new_logical(0.0, 0.0),
                                    client_size: Size::new_logical(0.0, 0.0),
                                },
                            )
                        });

                    if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                        break;
                    }
                }
            }
            PointerFocusState::Capturing(ht_ref) => {
                // キャプチャ対象にだけ通知
                action_context
                    .ht_manager
                    .get_data(ht_ref)
                    .action_handler()
                    .map_or(EventContinueControl::empty(), |h| {
                        h.on_pointer_hover(
                            ht_ref,
                            action_context,
                            // TODO: Pointer IDでホバー分けて管理したほうがいいかも
                            &PointerActionArgs {
                                #[allow(invalid_value)]
                                pointer_id: unsafe {
                                    core::mem::MaybeUninit::uninit().assume_init()
                                },
                                client_pos: Point::new_logical(0.0, 0.0),
                                client_size: Size::new_logical(0.0, 0.0),
                            },
                        )
                    });
            }
        }
    }

    pub fn handle_mouse_down<'env, 'sys, 'h>(
        &mut self,
        pointer_id: PointerID,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext<'env, 'sys, 'h>,
        button: PointerButton,
        ht_root: HitTestTreeRef,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        let Some(&(mut entering_surface, client_pos)) =
            self.last_client_pointer_pos.get(&pointer_id)
        else {
            // no pointer on the surface
            return;
        };
        let ws = entering_surface.size();

        self.down_gesture = PointerDownGestureState::Click {
            base_client_pos: client_pos,
            initiator_button: button,
        };

        let args = PointerButtonActionArgs {
            button,
            pointer_id,
            client_pos,
            client_size: ws,
        };
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht
                    .get_data(ht_ref)
                    .action_handler()
                    .map_or(EventContinueControl::empty(), |h| {
                        h.on_pointer_down(ht_ref, action_context, &args)
                    });
                Self::update_keyboard_focus(
                    &mut entering_surface,
                    ht.get_data(ht_ref).keyboard_focus,
                    action_context,
                    kf_registry,
                );

                if flags.releasing_capture() {
                    entering_surface.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                }
                if flags.needs_recompute_pointer_enter() {
                    self.update_pointer_enter(
                        &ws,
                        pointer_id,
                        client_pos,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let (needs_recompute_pointer_enter, new_captured) = Self::dispatch_pointer_down(
                    &mut entering_surface,
                    &args,
                    ht,
                    action_context,
                    ht_ref,
                    kf_registry,
                );

                if let Some(h) = new_captured {
                    self.pointer_focus = PointerFocusState::Capturing(h);
                }
                if needs_recompute_pointer_enter {
                    self.update_pointer_enter(
                        &ws,
                        pointer_id,
                        client_pos,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::None => (),
        }
    }

    pub fn handle_mouse_up<'env, 'sys, 'h>(
        &mut self,
        pointer_id: PointerID,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext<'env, 'sys, 'h>,
        button: PointerButton,
        ht_root: HitTestTreeRef,
    ) {
        let Some(&(entering_surface, client_pos)) = self.last_client_pointer_pos.get(&pointer_id)
        else {
            // no pointer on the surface
            return;
        };
        let ws = entering_surface.size();

        if self.down_gesture.is_dragging() {
            // ドラッグ状態だった
            self.end_drag(
                &entering_surface,
                ht,
                ht_root,
                action_context,
                button,
                pointer_id,
                client_pos,
                ws,
            );
        }

        let args = PointerButtonActionArgs {
            button,
            pointer_id,
            client_pos,
            client_size: ws,
        };
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht
                    .get_data(ht_ref)
                    .action_handler()
                    .map_or(EventContinueControl::empty(), |h| {
                        h.on_pointer_up(ht_ref, action_context, &args)
                    });

                if flags.releasing_capture() {
                    entering_surface.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                }
                if flags.needs_recompute_pointer_enter() {
                    self.update_pointer_enter(
                        &ws,
                        pointer_id,
                        client_pos,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let (needs_recompute_pointer_enter, capture_released) =
                    Self::dispatch_pointer_up(&entering_surface, &args, ht, action_context, ht_ref);

                if capture_released {
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                }
                if capture_released || needs_recompute_pointer_enter {
                    // PointerCaptureを解除したときもEnter/Leaveの再計算をさせる
                    self.update_pointer_enter(
                        &ws,
                        pointer_id,
                        client_pos,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::None => (),
        };

        if self.down_gesture.is_click(button) {
            // クリック判定持続してた

            match self.last_click {
                // double click
                Some(ref last_click)
                    if last_click.count == 1
                        && last_click.time.elapsed() <= DOUBLE_CLICK_DETECTION_MAX_TIME
                        && last_click.surface == entering_surface
                        && last_click.button == button
                        && last_click.pos.distance_sq(&client_pos)
                            <= DOUBLE_CLICK_DETECTION_MAX_DISTANCE.powi(2) =>
                {
                    self.perform_double_click(
                        entering_surface,
                        ws,
                        pointer_id,
                        button,
                        client_pos,
                        action_context,
                        ht,
                        ht_root,
                    )
                }
                _ => self.perform_single_click(
                    entering_surface,
                    ws,
                    pointer_id,
                    button,
                    client_pos,
                    action_context,
                    ht,
                    ht_root,
                ),
            }
        }

        self.down_gesture = PointerDownGestureState::None;
    }

    fn perform_single_click<'env, 'sys, 'h>(
        &mut self,
        surface: NativeDesktopSurface,
        surface_size: Size<PointerInputUnit>,
        pointer_id: PointerID,
        button: PointerButton,
        client_pos: Point<PointerInputUnit>,
        action_context: &mut InputEventContext<'env, 'sys, 'h>,
        ht: &HitTestTreeManager,
        ht_root: HitTestTreeRef,
    ) {
        self.last_click = Some(LastClickState {
            count: 1,
            surface,
            button,
            pos: client_pos,
            time: Instant::now(),
        });

        let args = PointerButtonActionArgs {
            button,
            pointer_id,
            client_pos,
            client_size: surface_size,
        };
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht
                    .get_data(ht_ref)
                    .action_handler()
                    .map_or(EventContinueControl::empty(), |h| {
                        h.on_click(ht_ref, action_context, &args)
                    });

                if flags.releasing_capture() {
                    surface.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                }
                if flags.needs_recompute_pointer_enter() {
                    self.update_pointer_enter(
                        &surface_size,
                        pointer_id,
                        client_pos,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let mut needs_recompute_pointer_enter = false;
                let mut new_captured = None;

                for ht_ref in ht.iter_ascending_from(ht_ref) {
                    let Some(a) = ht.get_data(ht_ref).action_handler() else {
                        continue;
                    };

                    let flags = a.on_click(ht_ref, action_context, &args);
                    if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                        needs_recompute_pointer_enter = true;
                    }
                    if flags.contains(EventContinueControl::CAPTURE_ELEMENT) {
                        new_captured = Some(ht_ref);
                        surface.capture_pointer();
                    }
                    if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                        break;
                    }
                }

                if let Some(h) = new_captured {
                    self.pointer_focus = PointerFocusState::Capturing(h);
                }
                if needs_recompute_pointer_enter {
                    self.update_pointer_enter(
                        &surface_size,
                        pointer_id,
                        client_pos,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::None => (),
        }
    }

    fn perform_double_click<'env, 'sys, 'h>(
        &mut self,
        surface: NativeDesktopSurface,
        surface_size: Size<PointerInputUnit>,
        pointer_id: PointerID,
        button: PointerButton,
        client_pos: Point<PointerInputUnit>,
        action_context: &mut InputEventContext<'env, 'sys, 'h>,
        ht: &HitTestTreeManager,
        ht_root: HitTestTreeRef,
    ) {
        self.last_click = Some(LastClickState {
            count: 2,
            surface,
            button,
            pos: client_pos,
            time: Instant::now(),
        });

        let args = PointerButtonActionArgs {
            button,
            pointer_id,
            client_pos,
            client_size: surface_size,
        };
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht
                    .get_data(ht_ref)
                    .action_handler()
                    .map_or(EventContinueControl::empty(), |h| {
                        h.on_double_click(ht_ref, action_context, &args)
                    });

                if flags.releasing_capture() {
                    surface.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                }
                if flags.needs_recompute_pointer_enter() {
                    self.update_pointer_enter(
                        &surface_size,
                        pointer_id,
                        client_pos,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let mut needs_recompute_pointer_enter = false;
                let mut new_captured = None;

                for ht_ref in ht.iter_ascending_from(ht_ref) {
                    let Some(a) = ht.get_data(ht_ref).action_handler() else {
                        continue;
                    };

                    let flags = a.on_double_click(ht_ref, action_context, &args);
                    if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                        needs_recompute_pointer_enter = true;
                    }
                    if flags.contains(EventContinueControl::CAPTURE_ELEMENT) {
                        new_captured = Some(ht_ref);
                        surface.capture_pointer();
                    }
                    if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                        break;
                    }
                }

                if let Some(h) = new_captured {
                    self.pointer_focus = PointerFocusState::Capturing(h);
                }
                if needs_recompute_pointer_enter {
                    self.update_pointer_enter(
                        &surface_size,
                        pointer_id,
                        client_pos,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::None => (),
        }
    }

    pub fn cursor_shape(&self, ht: &HitTestTreeManager) -> CursorShape {
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => ht.get_data(ht_ref).cursor_shape,
            PointerFocusState::Entering(ht_ref) => ht
                .iter_ascending_from(ht_ref)
                .map(|hr| ht.get_data(hr))
                .find(|x| x.opaque)
                .map_or(CursorShape::Default, |h| h.cursor_shape),
            PointerFocusState::None => CursorShape::Default,
        }
    }

    pub fn role_focus(&self, ht: &HitTestTreeManager) -> Option<Role> {
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                // キャプチャ中の要素があればそれだけを見る
                ht.get_data(ht_ref).role
            }
            PointerFocusState::Entering(ht_ref) => ht
                .iter_ascending_from(ht_ref)
                .find_map(|x| ht.get_data(x).role),
            PointerFocusState::None => None,
        }
    }

    pub fn role(
        &self,
        client_pos: &Point<PointerInputUnit>,
        client_size: &Size<PointerInputUnit>,
        ht: &HitTestTreeManager,
        ht_root: HitTestTreeRef,
    ) -> Option<Role> {
        if let PointerFocusState::Capturing(ht_ref) = self.pointer_focus {
            // キャプチャ中の要素があればそれだけを見る
            return ht.get_data(ht_ref).role;
        }

        // roleの検査(WM_NCHITTEST)ではEnter/Leaveの更新をしないので直接testを呼ぶ
        let Some(hit) = ht.test(ht_root, client_pos, &client_size.into()) else {
            // なにもヒットしなかった
            return None;
        };

        ht.iter_ascending_from(hit)
            .find_map(|x| ht.get_data(x).role)
    }

    fn update_keyboard_focus(
        surface: &mut NativeDesktopSurface,
        new_focus: Option<FocusTargetToken>,
        action_context: &mut InputEventContext,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        let (released, taken) = match new_focus {
            Some(x) => surface.keyboard_focus_state_mut().set_focus(x),
            None => (surface.keyboard_focus_state_mut().clear_focus(), None),
        };

        if let Some(eh) = released.and_then(|x| kf_registry.event_handler(x)) {
            eh.focus_released(action_context);
        }

        if let Some(eh) = taken.and_then(|x| kf_registry.event_handler(x)) {
            eh.focus_taken(action_context);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FocusTargetToken(usize);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum KeyInputCode {
    Character(char),
    LeftShift,
    LeftAlt,
    LeftControl,
    LeftSuper,
    RightShift,
    RightAlt,
    RightControl,
    RightSuper,
    LeftArrow,
    RightArrow,
    UpArrow,
    DownArrow,
    Home,
    End,
    PageUp,
    PageDown,
    Insert,
    UnknownNativeCode(u32),
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    pub struct ModifierKey : u8 {
        const SHIFT = 0x01;
        const ALT = 0x02;
        const CONTROL = 0x04;
        const SUPER = 0x08;
    }
}

pub trait KeyInputEventHandler {
    #[allow(unused_variables)]
    fn focus_taken(&self, context: &mut InputEventContext) {}
    #[allow(unused_variables)]
    fn focus_released(&self, context: &mut InputEventContext) {}

    #[allow(unused_variables)]
    fn keydown(&self, context: &mut InputEventContext, code: KeyInputCode) {}
    #[allow(unused_variables)]
    fn keyup(&self, context: &mut InputEventContext, code: KeyInputCode) {}

    #[allow(unused_variables)]
    fn ime_state_changes(
        &self,
        context: &mut InputEventContext,
        new_committed_string: &str,
        new_preedit_string: &str,
    ) {
    }
}

pub struct PerWindowKeyboardFocusState {
    window_focused: bool,
    current_focus: Option<usize>,
    active_group_stack: Vec<KeyboardFocusGroupRef>,
}
impl PerWindowKeyboardFocusState {
    pub fn new(root_group: KeyboardFocusGroupRef) -> Self {
        Self {
            current_focus: None,
            window_focused: false,
            active_group_stack: vec![root_group],
        }
    }

    pub fn push_tab_stop_group(
        &mut self,
        group: KeyboardFocusGroupRef,
        action_context: &mut InputEventContext,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        self.active_group_stack.push(group);
        self.clear_focus_with_event(action_context, kf_registry);
    }

    pub fn pop_tab_stop_group(&mut self) {
        self.active_group_stack.pop();
        if self.active_group_stack.is_empty() {
            panic!("root group pop!");
        }
    }

    pub fn has_focus(&self, tok: &FocusTargetToken) -> bool {
        self.window_focused && self.current_focus.is_some_and(|x| x == tok.0)
    }

    pub fn notify_window_focus(
        &mut self,
        context: &mut InputEventContext,
        registry: &KeyboardFocusTokenRegistry,
    ) {
        self.window_focused = true;

        let Some(f) = self.current_focus else {
            return;
        };
        let Some(eh) = registry.event_handler(FocusTargetToken(f)) else {
            return;
        };

        eh.focus_taken(context);
    }

    pub fn notify_window_lost_focus(
        &mut self,
        context: &mut InputEventContext,
        registry: &KeyboardFocusTokenRegistry,
    ) {
        self.window_focused = false;

        let Some(f) = self.current_focus else {
            return;
        };
        let Some(eh) = registry.event_handler(FocusTargetToken(f)) else {
            return;
        };

        eh.focus_released(context);
    }

    fn set_focus(
        &mut self,
        tok: FocusTargetToken,
    ) -> (Option<FocusTargetToken>, Option<FocusTargetToken>) {
        if self.current_focus == Some(tok.0) {
            // no changes
            return (None, None);
        }

        let released_focus = self.current_focus.replace(tok.0);
        (released_focus.map(FocusTargetToken), Some(tok))
    }

    fn clear_focus(&mut self) -> Option<FocusTargetToken> {
        self.current_focus.take().map(FocusTargetToken)
    }

    fn clear_focus_with_event(
        &mut self,
        action_context: &mut InputEventContext,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        if self.current_focus.is_none() {
            // already cleared
            return;
        }

        if let Some(eh) = self
            .current_focus
            .take()
            .and_then(|x| kf_registry.event_handler(FocusTargetToken(x)))
        {
            eh.focus_released(action_context);
        }
    }

    pub fn update_focus_with_event(
        &mut self,
        new_focus: FocusTargetToken,
        action_context: &mut InputEventContext,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        if self.current_focus == Some(new_focus.0) {
            // no changes
            return;
        }

        let released_focus = self.current_focus.replace(new_focus.0);

        if let Some(eh) =
            released_focus.and_then(|x| kf_registry.event_handler(FocusTargetToken(x)))
        {
            eh.focus_released(action_context);
        }

        if let Some(eh) = kf_registry.event_handler(new_focus) {
            eh.focus_taken(action_context);
        }
    }

    pub fn next_focus(&self, registry: &KeyboardFocusTokenRegistry) -> Option<FocusTargetToken> {
        let active_group = self.active_group_stack.last().expect("no group");
        if registry.groups[active_group.0].links.is_empty() {
            // nothing in this group
            return None;
        }

        let Some(current) = self.current_focus else {
            // current_focusがない
            let index = registry.groups[active_group.0].first_order_index;
            return Some(registry.groups[active_group.0].links[index].token);
        };

        let Some(current_bound_group) = registry.token_data[current].tab_order_group else {
            // current is not bound to any group(そんなことある？)
            let index = registry.groups[active_group.0].first_order_index;
            return Some(registry.groups[active_group.0].links[index].token);
        };

        if &current_bound_group.0 != active_group {
            // 現在のフォーカスと別のgroupがactive
            let index = registry.groups[active_group.0].first_order_index;
            return Some(registry.groups[active_group.0].links[index].token);
        }

        let link_data_index = current_bound_group.1;
        if link_data_index == registry.groups[active_group.0].last_order_index {
            // 最後のTab Order ループさせる
            let index = registry.groups[active_group.0].first_order_index;
            return Some(registry.groups[active_group.0].links[index].token);
        }

        let index = registry.groups[active_group.0].links[link_data_index].next;
        return Some(registry.groups[active_group.0].links[index].token);
    }

    pub fn prev_focus(&self, registry: &KeyboardFocusTokenRegistry) -> Option<FocusTargetToken> {
        let active_group = self.active_group_stack.last().expect("no group");
        if registry.groups[active_group.0].links.is_empty() {
            // nothing registered in this group
            return None;
        }

        let Some(current) = self.current_focus else {
            // current_focusがない
            let index = registry.groups[active_group.0].last_order_index;
            return Some(registry.groups[active_group.0].links[index].token);
        };

        let Some(current_bound_group) = registry.token_data[current].tab_order_group else {
            // current is not bound to any group(そんなことある？)
            let index = registry.groups[active_group.0].last_order_index;
            return Some(registry.groups[active_group.0].links[index].token);
        };

        if &current_bound_group.0 != active_group {
            // 現在のフォーカスと別のgroupがactive
            let index = registry.groups[active_group.0].last_order_index;
            return Some(registry.groups[active_group.0].links[index].token);
        }

        let link_data_index = current_bound_group.1;
        if link_data_index == registry.groups[active_group.0].first_order_index {
            // 最初のTab Order ループさせる
            let index = registry.groups[active_group.0].last_order_index;
            return Some(registry.groups[active_group.0].links[index].token);
        }

        let index = registry.groups[active_group.0].links[link_data_index].prev;
        return Some(registry.groups[active_group.0].links[index].token);
    }

    pub fn handle_keydown(
        &self,
        code: KeyInputCode,
        context: &mut InputEventContext,
        registry: &KeyboardFocusTokenRegistry,
    ) {
        let Some(eh) = self
            .current_focus
            .and_then(|x| registry.event_handler(FocusTargetToken(x)))
        else {
            return;
        };

        eh.keydown(context, code);
    }

    pub fn handle_keyup(
        &self,
        code: KeyInputCode,
        context: &mut InputEventContext,
        registry: &KeyboardFocusTokenRegistry,
    ) {
        let Some(eh) = self
            .current_focus
            .and_then(|x| registry.event_handler(FocusTargetToken(x)))
        else {
            return;
        };

        eh.keyup(context, code);
    }

    pub fn handle_ime_state_changes(
        &self,
        new_committed_string: &str,
        new_preedit_string: &str,
        context: &mut InputEventContext,
        registry: &KeyboardFocusTokenRegistry,
    ) {
        let Some(eh) = self
            .current_focus
            .and_then(|x| registry.event_handler(FocusTargetToken(x)))
        else {
            return;
        };

        eh.ime_state_changes(context, new_committed_string, new_preedit_string);
    }
}

struct KeyboardFocusTokenData {
    event_handler: Option<Weak<dyn KeyInputEventHandler>>,
    tab_order_group: Option<(KeyboardFocusGroupRef, usize)>,
}
impl KeyboardFocusTokenData {
    fn reset(&mut self) {
        self.event_handler = None;
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct KeyboardFocusGroupRef(usize);

#[derive(Debug)]
struct KeyboardFocusTabOrderData {
    token: FocusTargetToken,
    next: usize,
    prev: usize,
}

struct KeyboardFocusTabOrderGroup {
    links: Vec<KeyboardFocusTabOrderData>,
    first_order_index: usize,
    last_order_index: usize,
}

pub struct KeyboardFocusTokenRegistry {
    last_token: usize,
    unused_token: BTreeSet<usize>,
    token_data: Vec<KeyboardFocusTokenData>,
    unused_groups: BTreeSet<usize>,
    groups: Vec<KeyboardFocusTabOrderGroup>,
}
impl KeyboardFocusTokenRegistry {
    pub fn new() -> Self {
        Self {
            last_token: 0,
            unused_token: BTreeSet::new(),
            token_data: Vec::new(),
            unused_groups: BTreeSet::new(),
            groups: Vec::new(),
        }
    }

    pub fn acquire_group(&mut self) -> KeyboardFocusGroupRef {
        if let Some(x) = self.unused_groups.pop_first() {
            return KeyboardFocusGroupRef(x);
        }

        let g = KeyboardFocusGroupRef(self.groups.len());
        self.groups.push(KeyboardFocusTabOrderGroup {
            links: Vec::new(),
            first_order_index: 0,
            last_order_index: 0,
        });
        g
    }

    pub fn release_group(&mut self, r: KeyboardFocusGroupRef) {
        // unlink all group-token relations
        for l in self.groups[r.0].links.drain(..) {
            self.token_data[l.token.0].tab_order_group = None;
        }

        if r.0 == self.groups.len() - 1 {
            self.groups.pop();
        } else {
            self.unused_groups.insert(r.0);
        }
    }

    pub fn join_group(&mut self, group: KeyboardFocusGroupRef, token: FocusTargetToken) {
        // ensure no group is bound to the token
        self.leave_group(token);

        self.token_data[token.0].tab_order_group = Some((group, self.groups[group.0].links.len()));
        if self.groups[group.0].links.is_empty() {
            // first link
            self.groups[group.0].links.push(KeyboardFocusTabOrderData {
                token,
                next: 0,
                prev: 0,
            });
        } else {
            // link to last
            let new_last_order_index = self.groups[group.0].links.len();
            let prev_last_order_index = core::mem::replace(
                &mut self.groups[group.0].last_order_index,
                new_last_order_index,
            );
            self.groups[group.0].links.push(KeyboardFocusTabOrderData {
                token,
                next: 0,
                prev: prev_last_order_index,
            });
            self.groups[group.0].links[prev_last_order_index].next = new_last_order_index;
        }
    }

    pub fn leave_group(&mut self, token: FocusTargetToken) {
        let Some((group, data_index)) = self.token_data[token.0].tab_order_group.take() else {
            // not bound with group
            return;
        };

        if data_index == self.groups[group.0].first_order_index {
            if data_index == self.groups[group.0].last_order_index {
                // removed all
                self.groups[group.0].links.clear();
                self.groups[group.0].first_order_index = 0;
                self.groups[group.0].last_order_index = 0;
            } else {
                // remove first
                let removed_data = self.groups[group.0].links.swap_remove(data_index);
                self.groups[group.0].first_order_index = removed_data.next;
                // nextの戻り先だけ更新
                let next_index = self.groups[group.0].links[data_index].next;
                self.groups[group.0].links[next_index].prev = data_index;
            }
        } else {
            if data_index == self.groups[group.0].last_order_index {
                // remove last
                let removed_data = self.groups[group.0].links.swap_remove(data_index);
                self.groups[group.0].last_order_index = removed_data.prev;
                // prevの次だけ更新
                let prev_index = self.groups[group.0].links[data_index].prev;
                self.groups[group.0].links[prev_index].next = data_index;
            } else {
                self.groups[group.0].links.swap_remove(data_index);

                // 純粋につなぎ直し
                let next_index = self.groups[group.0].links[data_index].next;
                let prev_index = self.groups[group.0].links[data_index].prev;
                self.groups[group.0].links[next_index].prev = data_index;
                self.groups[group.0].links[prev_index].next = data_index;
            }
        }
    }

    pub fn acquire_token(&mut self) -> FocusTargetToken {
        if let Some(x) = self.unused_token.pop_first() {
            return FocusTargetToken(x);
        }

        let t = FocusTargetToken(self.last_token);
        self.last_token += 1;
        self.token_data.push(KeyboardFocusTokenData {
            event_handler: None,
            tab_order_group: None,
        });
        t
    }

    pub fn release_token(&mut self, tok: FocusTargetToken) {
        // ensure no group is bound to the token
        self.leave_group(tok);

        if tok.0 == self.last_token - 1 {
            self.last_token -= 1;
        } else {
            self.unused_token.insert(tok.0);
        }

        self.token_data[tok.0].reset();
    }

    #[inline(always)]
    pub fn set_event_handler(
        &mut self,
        tok: FocusTargetToken,
        handler: &Rc<impl KeyInputEventHandler + 'static>,
    ) {
        self.token_data[tok.0].event_handler = Some(Rc::downgrade(handler) as _);
    }

    #[inline(always)]
    fn event_handler(&self, tok: FocusTargetToken) -> Option<Rc<dyn KeyInputEventHandler>> {
        self.token_data[tok.0]
            .event_handler
            .as_ref()
            .and_then(Weak::upgrade)
    }
}
