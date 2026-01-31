use std::collections::BTreeSet;

use bitflags::bitflags;

use crate::hittest::{
    CursorShape, HitTestEventContext, HitTestOptions, HitTestTreeManager, HitTestTreeRef,
    PointerActionArgs, Role,
};

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

pub trait ShellPointerActions {
    fn capture_pointer(&self);
    fn release_pointer(&self);
}

pub struct PointerInputManager {
    last_client_pointer_pos: Option<(f32, f32)>,
    pointer_focus: PointerFocusState,
    click_base_client_pointer_pos: Option<(f32, f32)>,
    client_size: (f32, f32),
}
impl PointerInputManager {
    const CLICK_DETECTION_MAX_DISTANCE: f32 = 4.0;

    pub fn new(client_width: f32, client_height: f32) -> Self {
        PointerInputManager {
            last_client_pointer_pos: None,
            pointer_focus: PointerFocusState::None,
            click_base_client_pointer_pos: None,
            client_size: (client_width, client_height),
        }
    }

    pub fn set_client_size(&mut self, client_width: f32, client_height: f32) {
        self.client_size = (client_width, client_height);
    }

    fn dispatch_pointer_enter(
        &self,
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) {
        let mut p = Some(ht_target);
        while let Some(ht_ref) = p {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_pointer_enter(ht_ref, action_context, action_args)
                });
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }

            p = ht.parent_of(ht_ref);
        }
    }

    fn dispatch_pointer_leave(
        &self,
        action_args: &PointerActionArgs,
        ht: &HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_target: HitTestTreeRef,
    ) {
        let mut p = Some(ht_target);
        while let Some(ht_ref) = p {
            let flags = ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(EventContinueControl::empty(), |h| {
                    h.on_pointer_leave(ht_ref, action_context, action_args)
                });
            if flags.contains(EventContinueControl::STOP_PROPAGATION) {
                break;
            }

            p = ht.parent_of(ht_ref);
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

        let mut p = Some(ht_target);
        while let Some(ht_ref) = p {
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

            p = ht.parent_of(ht_ref);
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
        let mut p = Some(ht_target);
        while let Some(ht_ref) = p {
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

            p = ht.parent_of(ht_ref);
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

        let mut p = Some(ht_target);
        while let Some(ht_ref) = p {
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

            p = ht.parent_of(ht_ref);
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

        let mut p = Some(ht_target);
        while let Some(ht_ref) = p {
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

            p = ht.parent_of(ht_ref);
        }

        (needs_recompute_pointer_enter, new_captured)
    }

    fn handle_mouse_enter_leave(
        &mut self,
        client_x: f32,
        client_y: f32,
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_root: HitTestTreeRef,
    ) {
        let (client_width, client_height) = self.client_size;

        let new_hit = ht.test(
            ht_root,
            client_x,
            client_y,
            crate::hittest::Rect {
                left: 0.0,
                top: 0.0,
                width: client_width,
                height: client_height,
            },
            HitTestOptions::ONLY_ACTION_HANDLED,
        );
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
                    client_x,
                    client_y,
                    client_width,
                    client_height,
                },
                ht,
                action_context,
                ht_ref,
            );

            // leaveしたときはclick状態もなかったことにする
            self.click_base_client_pointer_pos = None;
        }

        self.pointer_focus = match new_hit {
            None => PointerFocusState::None,
            Some(ht_ref) => PointerFocusState::Entering(ht_ref),
        };

        if let Some(ht_ref) = new_enter {
            self.dispatch_pointer_enter(
                &PointerActionArgs {
                    client_x,
                    client_y,
                    client_width,
                    client_height,
                },
                ht,
                action_context,
                ht_ref,
            );
        }
    }

    pub fn handle_mouse_move(
        &mut self,
        client_x: f32,
        client_y: f32,
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_root: HitTestTreeRef,
    ) {
        let (client_width, client_height) = self.client_size;
        self.last_client_pointer_pos = Some((client_x, client_y));

        if let Some((cbx, cby)) = self.click_base_client_pointer_pos {
            let d_sq = (client_x - cbx).powi(2) + (client_y - cby).powi(2);

            if d_sq >= Self::CLICK_DETECTION_MAX_DISTANCE.powi(2) {
                // 動きすぎたのでクリック状態を解除
                self.click_base_client_pointer_pos = None;
            }
        }

        if let PointerFocusState::Capturing(ht_ref) = self.pointer_focus {
            // キャプチャ中の要素があればそれにだけ流す
            if let Some(h) = ht.get_data(ht_ref).action_handler() {
                h.on_pointer_move(
                    ht_ref,
                    action_context,
                    &PointerActionArgs {
                        client_x,
                        client_y,
                        client_width,
                        client_height,
                    },
                );
            }

            return;
        }

        self.handle_mouse_enter_leave(client_x, client_y, ht, action_context, ht_root);

        if let PointerFocusState::Entering(ht_ref) = self.pointer_focus {
            let needs_recompute_pointer_enter = self.dispatch_pointer_move(
                &PointerActionArgs {
                    client_x,
                    client_y,
                    client_width,
                    client_height,
                },
                ht,
                action_context,
                ht_ref,
            );

            if needs_recompute_pointer_enter {
                self.handle_mouse_enter_leave(client_x, client_y, ht, action_context, ht_root);
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
        let Some((client_x, client_y)) = self.last_client_pointer_pos else {
            // no pointer on the surface
            return;
        };
        let (client_width, client_height) = self.client_size;

        self.click_base_client_pointer_pos = Some((client_x, client_y));

        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht.get_data(ht_ref).action_handler().map_or(
                    EventContinueControl::empty(),
                    |h| {
                        h.on_pointer_down(
                            ht_ref,
                            action_context,
                            &PointerActionArgs {
                                client_x,
                                client_y,
                                client_width,
                                client_height,
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
                    self.handle_mouse_enter_leave(client_x, client_y, ht, action_context, ht_root);
                }
                if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                    sh.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                    self.handle_mouse_enter_leave(client_x, client_y, ht, action_context, ht_root);
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let (needs_recompute_pointer_enter, new_captured) = self.dispatch_pointer_down(
                    sh,
                    &PointerActionArgs {
                        client_x,
                        client_y,
                        client_width,
                        client_height,
                    },
                    ht,
                    action_context,
                    ht_ref,
                    kfm,
                );

                if let Some(h) = new_captured {
                    self.pointer_focus = PointerFocusState::Capturing(h);
                } else if needs_recompute_pointer_enter {
                    self.handle_mouse_enter_leave(client_x, client_y, ht, action_context, ht_root);
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
        let Some((client_x, client_y)) = self.last_client_pointer_pos else {
            // no pointer on the surface
            return;
        };
        let (client_width, client_height) = self.client_size;

        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                let flags = ht.get_data(ht_ref).action_handler().map_or(
                    EventContinueControl::empty(),
                    |h| {
                        h.on_pointer_up(
                            ht_ref,
                            action_context,
                            &PointerActionArgs {
                                client_x,
                                client_y,
                                client_width,
                                client_height,
                            },
                        )
                    },
                );
                if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                    self.handle_mouse_enter_leave(client_x, client_y, ht, action_context, ht_root);
                }
                if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                    sh.release_pointer();
                    self.pointer_focus = PointerFocusState::Entering(ht_ref);
                    self.handle_mouse_enter_leave(client_x, client_y, ht, action_context, ht_root);
                }
            }
            PointerFocusState::Entering(ht_ref) => {
                let (needs_recompute_pointer_enter, new_captured) = self.dispatch_pointer_up(
                    sh,
                    &PointerActionArgs {
                        client_x,
                        client_y,
                        client_width,
                        client_height,
                    },
                    ht,
                    action_context,
                    ht_ref,
                );

                if let Some(h) = new_captured {
                    self.pointer_focus = PointerFocusState::Capturing(h);
                } else if needs_recompute_pointer_enter {
                    self.handle_mouse_enter_leave(client_x, client_y, ht, action_context, ht_root);
                }
            }
            PointerFocusState::None => (),
        }

        if self.click_base_client_pointer_pos.take().is_some() {
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
                                    client_x,
                                    client_y,
                                    client_width,
                                    client_height,
                                },
                            )
                        },
                    );
                    if flags.contains(EventContinueControl::RECOMPUTE_POINTER_ENTER) {
                        self.handle_mouse_enter_leave(
                            client_x,
                            client_y,
                            ht,
                            action_context,
                            ht_root,
                        );
                    }
                    if flags.contains(EventContinueControl::RELEASE_CAPTURE_ELEMENT) {
                        sh.release_pointer();
                        self.pointer_focus = PointerFocusState::Entering(ht_ref);
                        self.handle_mouse_enter_leave(
                            client_x,
                            client_y,
                            ht,
                            action_context,
                            ht_root,
                        );
                    }
                }
                PointerFocusState::Entering(ht_ref) => {
                    let (needs_recompute_pointer_enter, new_captured) = self.dispatch_click(
                        sh,
                        &PointerActionArgs {
                            client_x,
                            client_y,
                            client_width,
                            client_height,
                        },
                        ht,
                        action_context,
                        ht_ref,
                    );

                    if let Some(h) = new_captured {
                        self.pointer_focus = PointerFocusState::Capturing(h);
                    } else if needs_recompute_pointer_enter {
                        self.handle_mouse_enter_leave(
                            client_x,
                            client_y,
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

    pub fn recompute_enter_leave(
        &mut self,
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
        ht_root: HitTestTreeRef,
    ) {
        let Some((last_client_x, last_client_y)) = self.last_client_pointer_pos else {
            return;
        };

        self.handle_mouse_enter_leave(last_client_x, last_client_y, ht, action_context, ht_root);
    }

    pub fn cursor_shape(
        &self,
        ht: &mut HitTestTreeManager,
        action_context: &mut HitTestEventContext,
    ) -> CursorShape {
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => ht
                .get_data(ht_ref)
                .action_handler()
                .map_or(CursorShape::Default, |h| {
                    h.cursor_shape(ht_ref, action_context)
                }),
            PointerFocusState::Entering(ht_ref) => {
                let mut p = Some(ht_ref);
                while let Some(ht_ref) = p {
                    if let Some(cursor_shape) = ht
                        .get_data(ht_ref)
                        .action_handler()
                        .map(|h| h.cursor_shape(ht_ref, action_context))
                    {
                        return cursor_shape;
                    }

                    p = ht.parent_of(ht_ref);
                }

                // fallback
                CursorShape::Default
            }
            PointerFocusState::None => CursorShape::Default,
        }
    }

    pub fn role_focus(&self, ht: &HitTestTreeManager) -> Option<Role> {
        match self.pointer_focus {
            PointerFocusState::Capturing(ht_ref) => {
                // キャプチャ中の要素があればそれだけを見る
                ht.get_data(ht_ref).role
            }
            PointerFocusState::Entering(ht_ref) => {
                let mut p = Some(ht_ref);
                while let Some(ht_ref) = p {
                    if let Some(role) = ht.get_data(ht_ref).role {
                        return Some(role);
                    }

                    p = ht.parent_of(ht_ref);
                }

                // fallback
                None
            }
            PointerFocusState::None => None,
        }
    }

    pub fn role(
        &self,
        client_x: f32,
        client_y: f32,
        client_width: f32,
        client_height: f32,
        ht: &HitTestTreeManager,
        ht_root: HitTestTreeRef,
    ) -> Option<Role> {
        if let PointerFocusState::Capturing(ht_ref) = self.pointer_focus {
            // キャプチャ中の要素があればそれだけを見る
            return ht.get_data(ht_ref).role;
        }

        // roleの検査(WM_NCHITTEST)ではEnter/Leaveの更新をしないので直接testを呼ぶ
        let Some(hit) = ht.test(
            ht_root,
            client_x,
            client_y,
            crate::hittest::Rect {
                left: 0.0,
                top: 0.0,
                width: client_width,
                height: client_height,
            },
            // roleの判定ではaction_handlerの有無を見ない
            HitTestOptions::empty(),
        ) else {
            // なにもヒットしなかった
            return None;
        };

        let mut p = Some(hit);
        while let Some(ht_ref) = p {
            if let Some(role) = ht.get_data(ht_ref).role {
                return Some(role);
            }

            p = ht.parent_of(ht_ref);
        }

        // fallback
        None
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
