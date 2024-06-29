use std::{
    collections::HashMap,
    rc::Rc,
    sync::atomic::{AtomicUsize, Ordering},
};

use windows::{
    core::PCSTR,
    Foundation::Rect,
    Win32::{
        Foundation::HWND,
        UI::{
            Input::KeyboardAndMouse::{ReleaseCapture, SetCapture},
            WindowsAndMessaging::{
                LoadCursorA, SetCursor, HTCLIENT, IDC_ARROW, IDC_SIZENS, IDC_SIZEWE,
            },
        },
    },
};

use crate::{
    empty_weak_mut, new_shared_mut,
    uikit::{CursorStyle, InputContext, InputEventHandler},
    utils::RectExtensions,
    SharedMut, WeakMut,
};

pub enum InputAction {
    PointerLeave(Rc<dyn InputEventHandler>),
    PointerEnter(Rc<dyn InputEventHandler>),
    PointerDown(Rc<dyn InputEventHandler>),
    PointerUp(Rc<dyn InputEventHandler>),
    Click(Rc<dyn InputEventHandler>),
    BeginDrag(Rc<dyn InputEventHandler>),
    DragMove(Rc<dyn InputEventHandler>),
    EndDrag(Rc<dyn InputEventHandler>),
}
impl InputAction {
    #[inline]
    pub fn execute(self, x: f32, y: f32, mut ctx: &mut dyn InputContext, window: HWND) {
        match self {
            Self::PointerLeave(e) => e.on_pointer_leave(ctx),
            Self::PointerEnter(e) => e.on_pointer_enter(ctx),
            Self::PointerDown(e) => e.on_pointer_down(x, y, ctx),
            Self::PointerUp(e) => e.on_pointer_up(x, y, ctx),
            Self::Click(e) => e.on_click(window, &mut ctx),
            Self::BeginDrag(e) => e.on_begin_drag(x, y, window, ctx),
            Self::DragMove(e) => e.on_drag_move(x, y, window, ctx),
            Self::EndDrag(e) => e.on_end_drag(x, y, window, ctx),
        }
    }
}

const DRAG_THRESHOLD_DIST2: f32 = 5.0 * 5.0;
pub struct InputState {
    bound_window: HWND,
    ht_tree: HitTestTree,
    mouse_capturing_element: Option<HitTestTreeWeakRef>,
    mouse_current_enter_element: Option<HitTestTreeWeakRef>,
    mouse_down_point: Option<(f32, f32, Option<HitTestTreeWeakRef>)>,
    is_mouse_dragging: bool,
}
impl InputState {
    pub fn new(bound_window: HWND, ht_tree: &HitTestTree) -> Self {
        Self {
            bound_window,
            ht_tree: ht_tree.clone(),
            mouse_capturing_element: None,
            mouse_current_enter_element: None,
            mouse_down_point: None,
            is_mouse_dragging: false,
        }
    }

    #[inline]
    fn mouse_current_entering_strong_ref(&self) -> Option<HitTestTree> {
        self.mouse_current_enter_element
            .as_ref()
            .and_then(HitTestTreeWeakRef::upgrade)
    }

    #[inline]
    fn mouse_capturing_strong_ref(&self) -> Option<HitTestTree> {
        self.mouse_capturing_element
            .as_ref()
            .and_then(HitTestTreeWeakRef::upgrade)
    }

    fn update_mouse_pos(&mut self, x: f32, y: f32, actions: &mut Vec<InputAction>) {
        let current_entering = self.mouse_current_entering_strong_ref();
        let over_tree = self.ht_tree.check(x, y, Rect::empty());
        let over_changes = match (current_entering.as_ref(), over_tree.as_ref()) {
            (Some(_), None) | (None, Some(_)) => true,
            (Some(old), Some(new)) => !old.ptr_eq(new),
            _ => false,
        };

        if over_changes {
            actions.extend(
                current_entering
                    .and_then(|x| x.clone_event_handler())
                    .map(InputAction::PointerLeave),
            );
        }

        self.mouse_current_enter_element = over_tree.as_ref().map(HitTestTree::weak_ref);

        if over_changes {
            actions.extend(
                over_tree
                    .and_then(|x| x.clone_event_handler())
                    .map(InputAction::PointerEnter),
            );
        }
    }

    pub fn capture_mouse(&mut self) {
        if self.mouse_current_enter_element.is_none() {
            return;
        }

        self.mouse_capturing_element = self.mouse_current_enter_element.clone();
        unsafe {
            SetCapture(self.bound_window);
        }
    }

    pub fn release_mouse_capture(&mut self) {
        if self.mouse_capturing_element.is_none() {
            return;
        }

        unsafe {
            ReleaseCapture().expect("Failed to release captured mouse");
        }
        self.mouse_capturing_element = None;
    }

    pub fn on_mouse_move(&mut self, x: f32, y: f32) -> Vec<InputAction> {
        let mut actions = Vec::with_capacity(16);

        if let Some(e) = self.mouse_capturing_strong_ref() {
            if let Some((dx, dy, _)) = self.mouse_down_point.as_ref() {
                if !self.is_mouse_dragging {
                    // 閾値を超えた後は永続的にドラッグ状態になる
                    let dist2 = (dx - x).powi(2) + (dy - y).powi(2);
                    if dist2 >= DRAG_THRESHOLD_DIST2 {
                        self.is_mouse_dragging = true;
                        actions.extend(e.clone_event_handler().map(InputAction::BeginDrag));
                    }
                }

                if self.is_mouse_dragging {
                    actions.extend(e.clone_event_handler().map(InputAction::DragMove));
                }
            }

            return actions;
        }

        self.update_mouse_pos(x, y, &mut actions);

        if let Some((dx, dy, down_element)) = self.mouse_down_point.as_ref() {
            if !self.is_mouse_dragging {
                // 閾値を超えた後は永続的にドラッグ状態になる
                let dist2 = (dx - x).powi(2) + (dy - y).powi(2);
                if dist2 >= DRAG_THRESHOLD_DIST2 {
                    self.is_mouse_dragging = true;
                    actions.extend(
                        down_element
                            .as_ref()
                            .and_then(|x| x.upgrade()?.clone_event_handler())
                            .map(InputAction::BeginDrag),
                    );
                }
            }

            if self.is_mouse_dragging {
                actions.extend(
                    down_element
                        .as_ref()
                        .and_then(|x| x.upgrade()?.clone_event_handler())
                        .map(InputAction::DragMove),
                );
            }
        }

        actions
    }

    pub fn on_mouse_down(&mut self, x: f32, y: f32) -> Vec<InputAction> {
        let mut actions = Vec::with_capacity(16);

        let active_target = if let Some(e) = self.mouse_capturing_strong_ref() {
            // キャプチャしている要素があるならこれを優先する
            Some(e)
        } else {
            self.update_mouse_pos(x, y, &mut actions);

            self.mouse_current_entering_strong_ref()
        };

        self.mouse_down_point = Some((x, y, self.mouse_current_enter_element.clone()));
        self.is_mouse_dragging = false;
        actions.extend(
            active_target
                .as_ref()
                .and_then(HitTestTree::clone_event_handler)
                .map(InputAction::PointerDown),
        );

        actions
    }

    pub fn on_mouse_up(&mut self, x: f32, y: f32) -> Vec<InputAction> {
        let mut actions = Vec::with_capacity(16);

        if let Some(e) = self.mouse_capturing_strong_ref() {
            actions.extend(e.clone_event_handler().map(InputAction::PointerUp));

            if !self.is_mouse_dragging {
                actions.extend(e.clone_event_handler().map(InputAction::Click));
            } else {
                actions.extend(e.clone_event_handler().map(InputAction::EndDrag));
            }

            self.mouse_down_point = None;
            return actions;
        }

        self.update_mouse_pos(x, y, &mut actions);

        if !self.is_mouse_dragging {
            actions.extend(
                self.mouse_current_entering_strong_ref()
                    .and_then(|x| x.clone_event_handler())
                    .map(InputAction::Click),
            );
        } else {
            // こっち（ドラッグ終了イベント）はマウスダウンした時の対象に送る
            actions.extend(
                self.mouse_down_point
                    .as_ref()
                    .and_then(|x| x.2.as_ref()?.upgrade()?.clone_event_handler())
                    .map(InputAction::EndDrag),
            );
        }
        self.mouse_down_point = None;

        actions
    }

    pub fn on_mouse_leave(&mut self) -> Vec<InputAction> {
        self.mouse_current_enter_element
            .take()
            .and_then(|x| x.upgrade()?.clone_event_handler())
            .map(InputAction::PointerLeave)
            .into_iter()
            .collect()
    }

    pub fn set_cursor(&self) -> bool {
        if let Some(e) = self.mouse_capturing_strong_ref() {
            // TODO: caching loaded cursors
            let c = match e
                .event_handler_ref()
                .as_ref()
                .map(InputEventHandler::hover_cursor)
                .unwrap_or(CursorStyle::Arrow)
            {
                CursorStyle::Arrow => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_ARROW))
                },
                CursorStyle::SizeNS => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_SIZENS))
                },
                CursorStyle::SizeEW => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_SIZEWE))
                },
            };

            unsafe { SetCursor(c.expect("Failed to load cursor")) };
            return true;
        }

        if let Some(e) = self.mouse_current_entering_strong_ref() {
            // TODO: caching loaded cursors
            let c = match e
                .event_handler_ref()
                .as_ref()
                .map(InputEventHandler::hover_cursor)
                .unwrap_or(CursorStyle::Arrow)
            {
                CursorStyle::Arrow => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_ARROW))
                },
                CursorStyle::SizeNS => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_SIZENS))
                },
                CursorStyle::SizeEW => unsafe {
                    LoadCursorA(None, core::mem::transmute::<_, PCSTR>(IDC_SIZEWE))
                },
            };

            unsafe { SetCursor(c.expect("Failed to load cursor")) };
            return true;
        }

        false
    }

    pub fn nc_hittest(&self, x: f32, y: f32) -> u32 {
        let Some(active_element) = self
            .mouse_capturing_strong_ref()
            .or_else(|| self.ht_tree.check(x, y, Rect::empty()))
        else {
            return HTCLIENT;
        };

        // Note: ここはなんかライフタイム推論がうまくいかなくて変数きらないといけないっぽい
        let r = active_element
            .event_handler_ref()
            .as_ref()
            .map_or(HTCLIENT, InputEventHandler::nc_hittest);
        r
    }
}

pub struct HitTestTreeState {
    eh: Option<Rc<dyn InputEventHandler>>,
    id: usize,
    rect: Rect,
    relative_adjustments: Rect,
    parent: WeakMut<HitTestTreeState>,
    children: HashMap<usize, HitTestTree>,
}

#[derive(Clone)]
#[repr(transparent)]
pub struct HitTestTreeWeakRef(WeakMut<HitTestTreeState>);
impl HitTestTreeWeakRef {
    #[inline]
    pub fn upgrade(&self) -> Option<HitTestTree> {
        self.0.upgrade().map(HitTestTree)
    }
}

#[derive(Clone)]
#[repr(transparent)]
pub struct HitTestTree(SharedMut<HitTestTreeState>);
impl HitTestTree {
    #[inline]
    pub fn new(
        eh: Option<impl InputEventHandler + 'static>,
        id: usize,
        rect: Rect,
        relative_adjustments: Rect,
    ) -> Self {
        Self(new_shared_mut(HitTestTreeState {
            eh: eh.map::<Rc<dyn InputEventHandler>, _>(|x| Rc::new(x)),
            id,
            rect,
            relative_adjustments,
            parent: empty_weak_mut(),
            children: HashMap::new(),
        }))
    }

    #[inline]
    pub fn new_unsized(
        eh: Option<impl InputEventHandler + 'static>,
        id: usize,
        left: f32,
        top: f32,
    ) -> Self {
        Self::new(
            eh,
            id,
            Rect {
                X: left,
                Y: top,
                Width: f32::MAX,
                Height: f32::MAX,
            },
            Rect::empty(),
        )
    }

    #[inline]
    pub fn new_fit_to_parent(eh: Option<impl InputEventHandler + 'static>, id: usize) -> Self {
        Self(new_shared_mut(HitTestTreeState {
            eh: eh.map::<Rc<dyn InputEventHandler>, _>(|x| Rc::new(x)),
            id,
            rect: Rect::empty(),
            relative_adjustments: Rect::from_size(1.0, 1.0),
            parent: empty_weak_mut(),
            children: HashMap::new(),
        }))
    }

    #[inline]
    pub fn ptr_eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.0, &other.0)
    }

    #[inline]
    pub fn weak_ref(&self) -> HitTestTreeWeakRef {
        HitTestTreeWeakRef(Rc::downgrade(&self.0))
    }

    #[inline]
    pub fn clone_event_handler(&self) -> Option<Rc<dyn InputEventHandler>> {
        self.0.borrow().eh.clone()
    }

    #[inline]
    pub fn event_handler_ref(&self) -> core::cell::Ref<Option<Rc<dyn InputEventHandler>>> {
        core::cell::Ref::map(self.0.borrow(), |x| &x.eh)
    }

    #[inline]
    pub fn id(&self) -> usize {
        self.0.borrow().id
    }

    #[inline]
    pub fn add_child(&self, child: &Self) {
        child.0.borrow_mut().parent = Rc::downgrade(&self.0);
        let cid = child.0.borrow().id;
        self.0.borrow_mut().children.insert(cid, child.clone());
    }

    #[inline]
    pub fn remove_child(&self, child: &HitTestTree) {
        let cb = child.0.borrow();
        self.0.borrow_mut().children.remove(&cb.id);
        drop(cb);
        child.0.borrow_mut().parent = empty_weak_mut();
    }

    #[inline]
    pub fn remove_all_children(&self) {
        for (_, c) in self.0.borrow_mut().children.drain() {
            c.0.borrow_mut().parent = empty_weak_mut();
        }
    }

    #[inline]
    pub fn unmount(&self) {
        let Some(parent) = self.0.borrow().parent.upgrade() else {
            // 親がいないツリーはunmountしようがないのでなにもしない
            return;
        };

        let id = self.id();
        parent.borrow_mut().children.remove(&id);
        self.0.borrow_mut().parent = empty_weak_mut();
    }

    #[inline]
    pub fn global_rect(&self) -> Rect {
        let parent_rect = self.0.borrow().parent.upgrade().map_or_else(
            || Rect {
                X: 0.0,
                Y: 0.0,
                Width: 0.0,
                Height: 0.0,
            },
            |p| HitTestTree(p).global_rect(),
        );
        let thisref = self.0.borrow();

        Rect {
            X: parent_rect.X + parent_rect.Width * thisref.relative_adjustments.X + thisref.rect.X,
            Y: parent_rect.Y + parent_rect.Height * thisref.relative_adjustments.Y + thisref.rect.Y,
            Width: parent_rect.Width * thisref.relative_adjustments.Width + thisref.rect.Width,
            Height: parent_rect.Height * thisref.relative_adjustments.Height + thisref.rect.Height,
        }
    }

    #[inline]
    pub fn rect(&self) -> Rect {
        self.0.borrow().rect.clone()
    }

    #[inline]
    pub fn set_rect(&self, left: f32, top: f32, width: f32, height: f32) {
        self.0.borrow_mut().rect = Rect {
            X: left,
            Y: top,
            Width: width,
            Height: height,
        };
    }

    #[inline]
    pub fn set_size(&self, width: f32, height: f32) {
        self.0.borrow_mut().rect.Width = width;
        self.0.borrow_mut().rect.Height = height;
    }
    #[inline]
    pub fn set_offset(&self, left: f32, top: f32) {
        self.0.borrow_mut().rect.X = left;
        self.0.borrow_mut().rect.Y = top;
    }
    #[inline]
    pub fn set_left(&self, left: f32) {
        self.0.borrow_mut().rect.X = left;
    }
    #[inline]
    pub fn set_right(&self, right: f32) {
        self.0.borrow_mut().rect.X = right - self.0.borrow_mut().rect.Width;
    }
    #[inline]
    pub fn set_top(&self, top: f32) {
        self.0.borrow_mut().rect.Y = top;
    }
    #[inline]
    pub fn set_width(&self, width: f32) {
        self.0.borrow_mut().rect.Width = width;
    }
    #[inline]
    pub fn set_relative_width(&self, rate: f32, offset: f32) {
        self.0.borrow_mut().relative_adjustments.Width = rate;
        self.0.borrow_mut().rect.Width = offset;
    }
    #[inline]
    pub fn set_relative_left(&self, rate: f32, offset: f32) {
        self.0.borrow_mut().rect.X = offset;
        self.0.borrow_mut().relative_adjustments.X = rate;
    }

    pub fn check(&self, x: f32, y: f32, ref_rect: Rect) -> Option<Self> {
        let this1 = self.0.borrow();
        let real_rect = Rect {
            X: ref_rect.Width * this1.relative_adjustments.X + this1.rect.X,
            Y: ref_rect.Height * this1.relative_adjustments.Y + this1.rect.Y,
            Width: ref_rect.Width * this1.relative_adjustments.Width + this1.rect.Width,
            Height: ref_rect.Height * this1.relative_adjustments.Height + this1.rect.Height,
        };
        if real_rect.contains_point(x, y) {
            let child = this1.children.values().find_map(|c| {
                Self::check(
                    c,
                    x - real_rect.X,
                    y - real_rect.Y,
                    Rect {
                        X: 0.0,
                        Y: 0.0,
                        ..real_rect
                    },
                )
            });

            match child {
                Some(c) => Some(c),
                // EventHandlerの設定がない場合はイベント透過なので親に戻す
                None if self.0.borrow().eh.is_some() => Some(self.clone()),
                None => None,
            }
        } else {
            None
        }
    }
}
impl core::fmt::Debug for HitTestTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let thisref = self.0.borrow();

        f.debug_struct("HitTestTree")
            .field("id", &thisref.id)
            .field("left", &thisref.rect.X)
            .field("top", &thisref.rect.Y)
            .field("width", &thisref.rect.Width)
            .field("height", &thisref.rect.Height)
            .field("children", &thisref.children)
            .finish_non_exhaustive()
    }
}

pub struct HitTestTreeContext {
    current_id: AtomicUsize,
}
impl HitTestTreeContext {
    pub const fn new() -> Self {
        Self {
            current_id: AtomicUsize::new(0),
        }
    }

    #[inline]
    pub fn new_id(&self) -> usize {
        self.current_id.fetch_add(1, Ordering::AcqRel)
    }
}
