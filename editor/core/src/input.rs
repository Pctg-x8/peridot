use std::collections::BTreeSet;

use bitflags::bitflags;

use crate::{
    hittest::{
        CursorShape, HitTestEventContext, HitTestTreeManager, HitTestTreeRef, PointerActionArgs,
        Role,
    },
    utils::{LogicalUnit, Point, Size},
};

pub type PointerInputUnit = LogicalUnit;

bitflags! {
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct EventContinueControl: usize {
        const STOP_PROPAGATION = 1 << 0;
        const CAPTURE_ELEMENT = 1 << 1;
        const RELEASE_CAPTURE_ELEMENT = 1 << 2;
        const RECOMPUTE_POINTER_ENTER = 1 << 3;
    }
}

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

pub struct PointerInputManager {
    last_client_pointer_pos: Option<Point<PointerInputUnit>>,
    pointer_focus: PointerFocusState,
    down_gesture: PointerDownGestureState,
    client_size: Size<PointerInputUnit>,
}
impl PointerInputManager {
    const CLICK_DETECTION_MAX_DISTANCE: f32 = 4.0;

    pub fn new(client_size: Size<PointerInputUnit>) -> Self {
        PointerInputManager {
            last_client_pointer_pos: None,
            pointer_focus: PointerFocusState::None,
            down_gesture: PointerDownGestureState::None,
            client_size,
        }
    }

    pub fn set_client_size(&mut self, client_size: Size<PointerInputUnit>) {
        self.client_size = client_size;
    }

    fn dispatch_pointer_enter(
        &self,
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) {
        for ht_ref in ht.iter_ascending_from(ht_target) {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_pointer_enter(ht_ref, action_context, action_args)
                });
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }
        }
    }

    fn dispatch_pointer_leave(
        &self,
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) {
        for ht_ref in ht.iter_ascending_from(ht_target) {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_pointer_leave(ht_ref, action_context, action_args)
                });
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
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
        kfm: &mut KeyboardFocusManager,
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

            match ht
                .get_data(ht_ref)
                .action_handler()
                .and_then(|x| x.keyboard_focus(ht_ref))
            {
                Some(x) => kfm.set_focus(x),
                None => kfm.clear_focus(),
            }

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
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) -> bool {
        let mut needs_recompute_pointer_enter = false;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_pointer_move(ht_ref, action_context, action_args)
                });
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
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) -> (bool, Option<HitTestTreeRef>) {
        let mut needs_recompute_pointer_enter = false;
        let mut new_captured = None;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_pointer_up(ht_ref, action_context, action_args)
                });
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

    fn dispatch_click(
        &self,
        sh: &(impl ShellPointerActions + ?Sized),
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) -> (bool, Option<HitTestTreeRef>) {
        let mut needs_recompute_pointer_enter = false;
        let mut new_captured = None;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_click(ht_ref, action_context, action_args)
                });
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
        action_context: &mut HitTestEventContext,
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
                    let flags = ht
                        .get_data(ht_ref)
                        .action_handler()
                        .map_or(EventContinueControl::empty(), |h| {
                            h.on_drag_start(ht_ref, action_context, action_args)
                        });
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
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) -> bool {
        let mut needs_recompute_pointer_enter = false;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_drag_move(ht_ref, action_context, action_args)
                });
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
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) -> (bool, Option<HitTestTreeRef>) {
        let mut needs_recompute_pointer_enter = false;
        let mut new_captured = None;

        for ht_ref in ht.iter_ascending_from(ht_target) {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_drag_end(ht_ref, action_context, action_args)
                });
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

    fn handle_mouse_enter_leave(
        &mut self,
        client_pos: Point<PointerInputUnit>,
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_root: HitTestTreeRef,
    ) {
        let new_hit = ht.test(ht_root, &client_pos, &self.client_size.into());
        let (new_leave, new_enter) = match (&self.pointer_focus, new_hit) {
            // in capturing, this routine is never called
            (&PointerFocusState::Capturing(_), _) => unreachable!("never happens"),
            (&PointerFocusState::Entering(old), Some(new)) => {
                if old != new {
                    // entering changed
                    (Some(old), Some(new))
                } else {
                    // nothing changed
                    (None, None)
                }
            }
            (&PointerFocusState::Entering(old), None) => {
                // just leave
                (Some(old), None)
            }
            (&PointerFocusState::None, Some(new)) => {
                // just enter
                (None, Some(new))
            }
            // nothing changed
            (&PointerFocusState::None, None) => (None, None),
        };

        if let Some(ht_ref) = new_leave {
            self.dispatch_pointer_leave(
                &PointerActionArgs {
                    client_pos,
                    client_size: self.client_size,
                },
                ht,
                action_context,
                ht_ref,
            );

            // leaveしたときはジェスチャもなかったことにする
            self.down_gesture = PointerDownGestureState::None;
        }

        self.pointer_focus = match new_hit {
            None => PointerFocusState::None,
            Some(ht_ref) => PointerFocusState::Entering(ht_ref),
        };

        if let Some(ht_ref) = new_enter {
            self.dispatch_pointer_enter(
                &PointerActionArgs {
                    client_pos,
                    client_size: self.client_size,
                },
                ht,
                action_context,
                ht_ref,
            );
        }
    }

    pub fn handle_mouse_move(
        &mut self,
        client_pos: Point<PointerInputUnit>,
        sh: &(impl ShellPointerActions + ?Sized),
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_root: HitTestTreeRef,
    ) {
        self.last_client_pointer_pos = Some(client_pos);

        match self.down_gesture {
            PointerDownGestureState::None => (),
            PointerDownGestureState::Click { base_client_pos } => {
                if client_pos.distance_sq(&base_client_pos)
                    >= Self::CLICK_DETECTION_MAX_DISTANCE.powi(2)
                {
                    // 動きすぎたのでドラッグ化を解除
                    self.begin_drag(
                        ht,
                        action_context,
                        &PointerActionArgs {
                            client_pos,
                            client_size: self.client_size,
                        },
                        sh,
                    );
                }
            }
            PointerDownGestureState::Drag => (),
        };

        if let PointerFocusState::Capturing(ht_ref) = self.pointer_focus {
            // キャプチャ中の要素があればそれにだけ流す
            if let Some(h) = ht.get_data(ht_ref).action_handler() {
                if self.down_gesture.is_dragging() {
                    h.on_drag_move(
                        ht_ref,
                        action_context,
                        &PointerActionArgs {
                            client_pos,
                            client_size: self.client_size,
                        },
                    );
                } else {
                    h.on_pointer_move(
                        ht_ref,
                        action_context,
                        &PointerActionArgs {
                            client_pos,
                            client_size: self.client_size,
                        },
                    );
                }
            }

            return;
        }

        self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);

        if let PointerFocusState::Entering(ht_ref) = self.pointer_focus {
            let needs_recompute_pointer_enter = if self.down_gesture.is_dragging() {
                self.dispatch_drag_move(
                    &PointerActionArgs {
                        client_pos,
                        client_size: self.client_size,
                    },
                    ht,
                    action_context,
                    ht_ref,
                )
            } else {
                self.dispatch_pointer_move(
                    &PointerActionArgs {
                        client_pos,
                        client_size: self.client_size,
                    },
                    ht,
                    action_context,
                    ht_ref,
                )
            };

            if needs_recompute_pointer_enter {
                self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
            }
        }
    }

    pub fn handle_mouse_left_down(
        &mut self,
        sh: &(impl ShellPointerActions + ?Sized),
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_root: HitTestTreeRef,
        kfm: &mut KeyboardFocusManager,
    ) {
        let Some(client_pos) = self.last_client_pointer_pos else {
            // no pointer on the surface
            return;
        };

        self.down_gesture = PointerDownGestureState::Click {
            base_client_pos: client_pos,
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
                                client_pos,
                                client_size: self.client_size,
                            },
                        )
                    },
                );
                match ht
                    .get_data(ht_ref)
                    .action_handler()
                    .and_then(|x| x.keyboard_focus(ht_ref))
                {
                    Some(x) => kfm.set_focus(x),
                    None => kfm.clear_focus(),
                }

                if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                    self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                }
                if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                    sh.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                    self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let (needs_recompute_pointer_enter, new_captured) = self.dispatch_pointer_down(
                    sh,
                    &PointerActionArgs {
                        client_pos,
                        client_size: self.client_size,
                    },
                    ht,
                    action_context,
                    ht_ref,
                    kfm,
                );

                if let Some(h) = new_captured {
                    self.pointer_focus = PointerFocusState::Capturing(h);
                } else if needs_recompute_pointer_enter {
                    self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                }
            }
            PointerFocusState::None => (),
        }
    }

    pub fn handle_mouse_left_up(
        &mut self,
        sh: &(impl ShellPointerActions + ?Sized),
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
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
                                    client_pos,
                                    client_size: self.client_size,
                                },
                            )
                        },
                    );
                    if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                        self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                    }
                    if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                        sh.release_pointer();
                        self.pointer_focus = PointerFocusState::Entering(ht_ref);
                        self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                    }
                }
                PointerFocusState::Entering(ht_ref) => {
                    let (needs_recompute_pointer_enter, new_captured) = self.dispatch_drag_end(
                        sh,
                        &PointerActionArgs {
                            client_pos,
                            client_size: self.client_size,
                        },
                        ht,
                        action_context,
                        ht_ref,
                    );

                    if let Some(h) = new_captured {
                        self.pointer_focus = PointerFocusState::Capturing(h);
                    } else if needs_recompute_pointer_enter {
                        self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
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
                                client_pos,
                                client_size: self.client_size,
                            },
                        )
                    },
                );
                if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                    self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                }
                if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                    sh.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                    self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let (needs_recompute_pointer_enter, new_captured) = self.dispatch_pointer_up(
                    sh,
                    &PointerActionArgs {
                        client_pos,
                        client_size: self.client_size,
                    },
                    ht,
                    action_context,
                    ht_ref,
                );

                if let Some(h) = new_captured {
                    self.pointer_focus = PointerFocusState::Capturing(h);
                } else if needs_recompute_pointer_enter {
                    self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                }
            }
            PointerFocusState::None => (),
        }

        if self.down_gesture.is_click() {
            // クリック判定持続してた
            match self.pointer_focus {
                PointerFocusState::Capturing(ht_ref) => {
                    let flags = ht.get_data(ht_ref).action_handler().map_or(
                        EventContinueControl::empty(),
                        |h| {
                            h.on_click(
                                ht_ref,
                                action_context,
                                &PointerActionArgs {
                                    client_pos,
                                    client_size: self.client_size,
                                },
                            )
                        },
                    );
                    if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                        self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                    }
                    if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                        sh.release_pointer();
                        self.pointer_focus = PointerFocusState::Entering(ht_ref);
                        self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                    }
                }
                PointerFocusState::Entering(ht_ref) => {
                    let (needs_recompute_pointer_enter, new_captured) = self.dispatch_click(
                        sh,
                        &PointerActionArgs {
                            client_pos,
                            client_size: self.client_size,
                        },
                        ht,
                        action_context,
                        ht_ref,
                    );

                    if let Some(h) = new_captured {
                        self.pointer_focus = PointerFocusState::Capturing(h);
                    } else if needs_recompute_pointer_enter {
                        self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
                    }
                }
                PointerFocusState::None => (),
            }
        }

        self.down_gesture = PointerDownGestureState::None;
    }

    pub fn recompute_enter_leave(
        &mut self,
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_root: HitTestTreeRef,
    ) {
        let Some(client_pos) = self.last_client_pointer_pos else {
            return;
        };

        self.handle_mouse_enter_leave(client_pos, ht, action_context, ht_root);
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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FocusTargetToken(usize);

pub struct KeyboardFocusManager {
    last_token: usize,
    unused_token: BTreeSet<usize>,
    current_focus: Option<usize>,
}
impl KeyboardFocusManager {
    pub fn new() -> Self {
        Self {
            last_token: 0,
            unused_token: BTreeSet::new(),
            current_focus: None,
        }
    }

    pub fn acquire_token(&mut self) -> FocusTargetToken {
        if let Some(x) = self.unused_token.pop_first() {
            return FocusTargetToken(x);
        }

        let t = FocusTargetToken(self.last_token);
        self.last_token += 1;
        t
    }

    pub fn release_token(&mut self, tok: FocusTargetToken) {
        if tok.0 == self.last_token - 1 {
            self.last_token -= 1;
        } else {
            self.unused_token.insert(tok.0);
        }
    }

    pub fn has_focus(&self, tok: &FocusTargetToken) -> bool {
        self.current_focus.is_some_and(|x| x == tok.0)
    }

    pub fn set_focus(&mut self, tok: FocusTargetToken) {
        self.current_focus = Some(tok.0);
    }

    pub fn clear_focus(&mut self) {
        self.current_focus = None;
    }
}
