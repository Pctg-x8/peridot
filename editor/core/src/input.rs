use std::{
    collections::{BTreeSet, HashMap},
    rc::{Rc, Weak},
    time::{Duration, Instant},
};

use bitflags::bitflags;

use crate::{
    DragPreviewPopoverHandle, Event, SyncEvent, SystemLink, WindowHandle,
    input::hittest::{
        CursorShape, HitTestTreeManager, HitTestTreeManagerCreateOnlyAccess, HitTestTreeRef,
        PointerActionArgs, Role,
    },
    rendering::composite::CompositeTree,
    utils::{LogicalUnit, Point, Size},
};

pub mod hittest;
pub type PointerInputUnit = LogicalUnit;

pub struct InputEventContext<'env, 'h> {
    pub sender_window: WindowHandle,
    pub current_sec: f32,
    pub composite_tree: &'env mut CompositeTree<SyncEvent>,
    pub drag_preview: &'env DragPreviewPopoverHandle,
    pub system_link: &'env SystemLink<'env>,
    pub ht_create_only_access: &'env mut HitTestTreeManagerCreateOnlyAccess<'h>,
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
    },
    Drag,
}
impl PointerDownGestureState {
    const fn is_dragging(&self) -> bool {
        matches!(self, Self::Drag)
    }

    const fn is_click(&self) -> bool {
        matches!(self, Self::Click { .. })
    }
}

pub trait ShellPointerActions {
    fn capture_pointer(&self);
    fn release_pointer(&self);
}

struct LastClickState {
    count: usize,
    window: WindowHandle,
    pos: Point<PointerInputUnit>,
    time: Instant,
}

pub struct PointerInputManager {
    last_client_pointer_pos: Option<(WindowHandle, Point<PointerInputUnit>)>,
    pointer_focus: PointerFocusState,
    down_gesture: PointerDownGestureState,
    client_size_by_window: HashMap<WindowHandle, Size<PointerInputUnit>>,
    last_click: Option<LastClickState>,
}
impl PointerInputManager {
    const CLICK_DETECTION_MAX_DISTANCE: f32 = 4.0;
    const DOUBLE_CLICK_DETECTION_MAX_DISTANCE: f32 = 4.0;
    const DOUBLE_CLICK_DETECTION_MAX_TIME: Duration = Duration::from_millis(500);

    pub fn new() -> Self {
        PointerInputManager {
            last_client_pointer_pos: None,
            pointer_focus: PointerFocusState::None,
            down_gesture: PointerDownGestureState::None,
            client_size_by_window: HashMap::new(),
            last_click: None,
        }
    }

    pub fn set_client_size(&mut self, window: WindowHandle, client_size: Size<PointerInputUnit>) {
        self.client_size_by_window.insert(window, client_size);
    }

    fn dispatch_pointer_enter(
        &self,
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
        &self,
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
        &self,
        sh: &(impl ShellPointerActions + ?Sized),
        action_args: &PointerActionArgs,
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
                ht.get_data(ht_ref).keyboard_focus,
                action_context,
                kf_registry,
            );

            if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                needs_recompute_pointer_enter = true;
            }
            if flags.contains(EventContinueControl::CAPTURE_ELEMENT) {
                new_captured = Some(ht_ref);
                sh.capture_pointer();
            }
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }

        (needs_recompute_pointer_enter, new_captured)
    }

    fn dispatch_pointer_move(
        &self,
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
        &self,
        sh: &(impl ShellPointerActions + ?Sized),
        action_args: &PointerActionArgs,
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

    fn dispatch_click(
        &self,
        sh: &(impl ShellPointerActions + ?Sized),
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_target: HitTestTreeRef,
    ) -> (bool, Option<HitTestTreeRef>) {
        let mut needs_recompute_pointer_enter = false;
        let mut new_captured = None;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let Some(a) = ht.get_data(ht_ref).action_handler() else {
                continue;
            };

            let flags = a.on_click(ht_ref, action_context, action_args);
            if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                needs_recompute_pointer_enter = true;
            }
            if flags.contains(EventContinueControl::CAPTURE_ELEMENT) {
                new_captured = Some(ht_ref);
                sh.capture_pointer();
            }
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }

        (needs_recompute_pointer_enter, new_captured)
    }

    fn begin_drag(
        &mut self,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        action_args: &PointerActionArgs,
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

    fn dispatch_drag_move(
        &self,
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

            let flags = a.on_drag_move(ht_ref, action_context, action_args);
            if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                needs_recompute_pointer_enter = true;
            }
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }

        needs_recompute_pointer_enter
    }

    fn dispatch_drag_end(
        &self,
        sh: &(impl ShellPointerActions + ?Sized),
        action_args: &PointerActionArgs,
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

            let flags = a.on_drag_end(ht_ref, action_context, action_args);
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

    fn handle_mouse_enter_leave(
        &mut self,
        window: WindowHandle,
        client_pos: Point<PointerInputUnit>,
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_root: HitTestTreeRef,
    ) {
        let new_hit = ht.test(
            ht_root,
            &client_pos,
            &self.client_size_by_window[&window].into(),
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
            self.dispatch_pointer_leave(
                &PointerActionArgs {
                    client_pos,
                    client_size: self.client_size_by_window[&window],
                },
                ht,
                action_context,
                ht_ref,
            );
            self.pointer_focus = PointerFocusState::None;
            // leaveしたときはジェスチャもなかったことにする
            self.down_gesture = PointerDownGestureState::None;
        }

        if let Some(ht_ref) = new_enter {
            self.pointer_focus = PointerFocusState::Entering(ht_ref);
            self.dispatch_pointer_enter(
                &PointerActionArgs {
                    client_pos,
                    client_size: self.client_size_by_window[&window],
                },
                ht,
                action_context,
                ht_ref,
            );
        }
    }

    pub fn handle_mouse_move(
        &mut self,
        window: WindowHandle,
        client_pos: Point<PointerInputUnit>,
        sh: &(impl ShellPointerActions + ?Sized),
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_root: HitTestTreeRef,
    ) {
        self.last_client_pointer_pos = Some((window, client_pos));

        if let PointerDownGestureState::Click { base_client_pos } = self.down_gesture
            && client_pos.distance_sq(&base_client_pos)
                >= Self::CLICK_DETECTION_MAX_DISTANCE.powi(2)
        {
            // 動きすぎたのでクリック状態をドラッグ化
            self.begin_drag(
                ht,
                action_context,
                &PointerActionArgs {
                    client_pos,
                    client_size: self.client_size_by_window[&window],
                },
                sh,
            );
        }

        if let PointerFocusState::Capturing(ht_ref) = self.pointer_focus {
            // キャプチャ中の要素があればそれにだけ流す
            if let Some(h) = ht.get_data(ht_ref).action_handler() {
                if self.down_gesture.is_dragging() {
                    h.on_drag_move(
                        ht_ref,
                        action_context,
                        &PointerActionArgs {
                            client_pos,
                            client_size: self.client_size_by_window[&window],
                        },
                    );
                } else {
                    h.on_pointer_move(
                        ht_ref,
                        action_context,
                        &PointerActionArgs {
                            client_pos,
                            client_size: self.client_size_by_window[&window],
                        },
                    );
                }
            }

            return;
        }

        self.handle_mouse_enter_leave(window, client_pos, ht, action_context, ht_root);

        if let PointerFocusState::Entering(ht_ref) = self.pointer_focus {
            let needs_recompute_pointer_enter = if self.down_gesture.is_dragging() {
                self.dispatch_drag_move(
                    &PointerActionArgs {
                        client_pos,
                        client_size: self.client_size_by_window[&window],
                    },
                    ht,
                    action_context,
                    ht_ref,
                )
            } else {
                self.dispatch_pointer_move(
                    &PointerActionArgs {
                        client_pos,
                        client_size: self.client_size_by_window[&window],
                    },
                    ht,
                    action_context,
                    ht_ref,
                )
            };

            if needs_recompute_pointer_enter {
                self.handle_mouse_enter_leave(window, client_pos, ht, action_context, ht_root);
            }
        }
    }

    pub fn handle_mouse_left_down(
        &mut self,
        sh: &(impl ShellPointerActions + ?Sized),
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_root: HitTestTreeRef,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        let Some(client_pos) = self.last_client_pointer_pos else {
            // no pointer on the surface
            return;
        };

        self.down_gesture = PointerDownGestureState::Click {
            base_client_pos: client_pos.1,
        };

        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht.get_data(ht_ref).action_handler().map_or(
                    EventContinueControl::empty(),
                    |h| {
                        h.on_pointer_down(
                            ht_ref,
                            action_context,
                            &PointerActionArgs {
                                client_pos: client_pos.1,
                                client_size: self.client_size_by_window[&client_pos.0],
                            },
                        )
                    },
                );
                Self::update_keyboard_focus(
                    ht.get_data(ht_ref).keyboard_focus,
                    action_context,
                    kf_registry,
                );

                if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                    self.handle_mouse_enter_leave(
                        client_pos.0,
                        client_pos.1,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
                if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                    sh.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                    self.handle_mouse_enter_leave(
                        client_pos.0,
                        client_pos.1,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let (needs_recompute_pointer_enter, new_captured) = self.dispatch_pointer_down(
                    sh,
                    &PointerActionArgs {
                        client_pos: client_pos.1,
                        client_size: self.client_size_by_window[&client_pos.0],
                    },
                    ht,
                    action_context,
                    ht_ref,
                    kf_registry,
                );

                if let Some(h) = new_captured {
                    self.pointer_focus = PointerFocusState::Capturing(h);
                } else if needs_recompute_pointer_enter {
                    self.handle_mouse_enter_leave(
                        client_pos.0,
                        client_pos.1,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::None => (),
        }
    }

    pub fn handle_mouse_left_up(
        &mut self,
        sh: &(impl ShellPointerActions + ?Sized),
        ht: &HitTestTreeManager,
        action_context: &mut InputEventContext,
        ht_root: HitTestTreeRef,
    ) {
        let Some(client_pos) = self.last_client_pointer_pos else {
            // no pointer on the surface
            return;
        };

        if self.down_gesture.is_dragging() {
            // ドラッグ状態だった場合はここでendする
            match self.pointer_focus {
                PointerFocusState::Capturing(ht_ref) => {
                    let flags = ht.get_data(ht_ref).action_handler().map_or(
                        EventContinueControl::empty(),
                        |h| {
                            h.on_drag_end(
                                ht_ref,
                                action_context,
                                &PointerActionArgs {
                                    client_pos: client_pos.1,
                                    client_size: self.client_size_by_window[&client_pos.0],
                                },
                            )
                        },
                    );
                    if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                        self.handle_mouse_enter_leave(
                            client_pos.0,
                            client_pos.1,
                            ht,
                            action_context,
                            ht_root,
                        );
                    }
                    if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                        sh.release_pointer();
                        self.pointer_focus = PointerFocusState::Entering(ht_ref);
                        self.handle_mouse_enter_leave(
                            client_pos.0,
                            client_pos.1,
                            ht,
                            action_context,
                            ht_root,
                        );
                    }
                }
                PointerFocusState::Entering(ht_ref) => {
                    let (needs_recompute_pointer_enter, capture_released) = self.dispatch_drag_end(
                        sh,
                        &PointerActionArgs {
                            client_pos: client_pos.1,
                            client_size: self.client_size_by_window[&client_pos.0],
                        },
                        ht,
                        action_context,
                        ht_ref,
                    );

                    if capture_released {
                        self.pointer_focus = PointerFocusState::Entering(ht_ref);
                    }
                    if capture_released || needs_recompute_pointer_enter {
                        // PointerCaptureを解除したときもEnter/Leaveの再計算をさせる
                        self.handle_mouse_enter_leave(
                            client_pos.0,
                            client_pos.1,
                            ht,
                            action_context,
                            ht_root,
                        );
                    }
                }
                PointerFocusState::None => (),
            }
        }

        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht.get_data(ht_ref).action_handler().map_or(
                    EventContinueControl::empty(),
                    |h| {
                        h.on_pointer_up(
                            ht_ref,
                            action_context,
                            &PointerActionArgs {
                                client_pos: client_pos.1,
                                client_size: self.client_size_by_window[&client_pos.0],
                            },
                        )
                    },
                );
                if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                    self.handle_mouse_enter_leave(
                        client_pos.0,
                        client_pos.1,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
                if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                    sh.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                    self.handle_mouse_enter_leave(
                        client_pos.0,
                        client_pos.1,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let (needs_recompute_pointer_enter, capture_released) = self.dispatch_pointer_up(
                    sh,
                    &PointerActionArgs {
                        client_pos: client_pos.1,
                        client_size: self.client_size_by_window[&client_pos.0],
                    },
                    ht,
                    action_context,
                    ht_ref,
                );

                if capture_released {
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                }
                if capture_released || needs_recompute_pointer_enter {
                    // PointerCaptureを解除したときもEnter/Leaveの再計算をさせる
                    self.handle_mouse_enter_leave(
                        client_pos.0,
                        client_pos.1,
                        ht,
                        action_context,
                        ht_root,
                    );
                }
            }
            PointerFocusState::None => (),
        };

        if self.down_gesture.is_click() {
            // クリック判定持続してた

            match self.last_click {
                // double click
                Some(ref mut last_click)
                    if last_click.count == 1
                        && last_click.time.elapsed() <= Self::DOUBLE_CLICK_DETECTION_MAX_TIME
                        && last_click.window == client_pos.0
                        && last_click.pos.distance_sq(&client_pos.1)
                            <= Self::DOUBLE_CLICK_DETECTION_MAX_DISTANCE
                                * Self::DOUBLE_CLICK_DETECTION_MAX_DISTANCE =>
                {
                    last_click.count = 2;
                    last_click.pos = client_pos.1;
                    last_click.time = Instant::now();

                    match self.pointer_focus {
                        PointerFocusState::Capturing(ht_ref) => {
                            let flags = ht.get_data(ht_ref).action_handler().map_or(
                                EventContinueControl::empty(),
                                |h| {
                                    h.on_double_click(
                                        ht_ref,
                                        action_context,
                                        &PointerActionArgs {
                                            client_pos: client_pos.1,
                                            client_size: self.client_size_by_window[&client_pos.0],
                                        },
                                    )
                                },
                            );
                            if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                                self.handle_mouse_enter_leave(
                                    client_pos.0,
                                    client_pos.1,
                                    ht,
                                    action_context,
                                    ht_root,
                                );
                            }
                            if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                                sh.release_pointer();
                                self.pointer_focus = PointerFocusState::Entering(ht_ref);
                                self.handle_mouse_enter_leave(
                                    client_pos.0,
                                    client_pos.1,
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

                                let flags = a.on_double_click(
                                    ht_ref,
                                    action_context,
                                    &PointerActionArgs {
                                        client_pos: client_pos.1,
                                        client_size: self.client_size_by_window[&client_pos.0],
                                    },
                                );
                                if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                                    needs_recompute_pointer_enter = true;
                                }
                                if flags.contains(EventContinueControl::CAPTURE_ELEMENT) {
                                    new_captured = Some(ht_ref);
                                    sh.capture_pointer();
                                }
                                if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                                    break;
                                }
                            }

                            if let Some(h) = new_captured {
                                self.pointer_focus = PointerFocusState::Capturing(h);
                            } else if needs_recompute_pointer_enter {
                                self.handle_mouse_enter_leave(
                                    client_pos.0,
                                    client_pos.1,
                                    ht,
                                    action_context,
                                    ht_root,
                                );
                            }
                        }
                        PointerFocusState::None => (),
                    }
                }
                // single click
                _ => {
                    self.last_click = Some(LastClickState {
                        count: 1,
                        window: client_pos.0,
                        pos: client_pos.1,
                        time: Instant::now(),
                    });

                    match self.pointer_focus {
                        PointerFocusState::Capturing(ht_ref) => {
                            let flags = ht.get_data(ht_ref).action_handler().map_or(
                                EventContinueControl::empty(),
                                |h| {
                                    h.on_click(
                                        ht_ref,
                                        action_context,
                                        &PointerActionArgs {
                                            client_pos: client_pos.1,
                                            client_size: self.client_size_by_window[&client_pos.0],
                                        },
                                    )
                                },
                            );
                            if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                                self.handle_mouse_enter_leave(
                                    client_pos.0,
                                    client_pos.1,
                                    ht,
                                    action_context,
                                    ht_root,
                                );
                            }
                            if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                                sh.release_pointer();
                                self.pointer_focus = PointerFocusState::Entering(ht_ref);
                                self.handle_mouse_enter_leave(
                                    client_pos.0,
                                    client_pos.1,
                                    ht,
                                    action_context,
                                    ht_root,
                                );
                            }
                        }
                        PointerFocusState::Entering(ht_ref) => {
                            let (needs_recompute_pointer_enter, new_captured) = self
                                .dispatch_click(
                                    sh,
                                    &PointerActionArgs {
                                        client_pos: client_pos.1,
                                        client_size: self.client_size_by_window[&client_pos.0],
                                    },
                                    ht,
                                    action_context,
                                    ht_ref,
                                );

                            if let Some(h) = new_captured {
                                self.pointer_focus = PointerFocusState::Capturing(h);
                            } else if needs_recompute_pointer_enter {
                                self.handle_mouse_enter_leave(
                                    client_pos.0,
                                    client_pos.1,
                                    ht,
                                    action_context,
                                    ht_root,
                                );
                            }
                        }
                        PointerFocusState::None => (),
                    }
                }
            }
        }

        self.down_gesture = PointerDownGestureState::None;
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
        new_focus: Option<FocusTargetToken>,
        action_context: &mut InputEventContext,
        kf_registry: &KeyboardFocusTokenRegistry,
    ) {
        let (released, taken) = match new_focus {
            Some(x) => action_context
                .sender_window
                .keyboard_focus_state_mut()
                .set_focus(x),
            None => (
                action_context
                    .sender_window
                    .keyboard_focus_state_mut()
                    .clear_focus(),
                None,
            ),
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

#[derive(Clone, Debug)]
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

struct KeyboardFocusTokenData {
    event_handler: Option<Weak<dyn KeyInputEventHandler>>,
}
impl KeyboardFocusTokenData {
    fn reset(&mut self) {
        self.event_handler = None;
    }
}

pub struct PerWindowKeyboardFocusState {
    current_focus: Option<usize>,
}
impl PerWindowKeyboardFocusState {
    pub fn new() -> Self {
        Self {
            current_focus: None,
        }
    }

    pub fn has_focus(&self, tok: &FocusTargetToken) -> bool {
        self.current_focus.is_some_and(|x| x == tok.0)
    }

    pub fn set_focus(
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

    pub fn clear_focus(&mut self) -> Option<FocusTargetToken> {
        self.current_focus.take().map(FocusTargetToken)
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

pub struct KeyboardFocusTokenRegistry {
    last_token: usize,
    unused_token: BTreeSet<usize>,
    token_data: Vec<KeyboardFocusTokenData>,
}
impl KeyboardFocusTokenRegistry {
    pub fn new() -> Self {
        Self {
            last_token: 0,
            unused_token: BTreeSet::new(),
            token_data: Vec::new(),
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
        });
        t
    }

    pub fn release_token(&mut self, tok: FocusTargetToken) {
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
