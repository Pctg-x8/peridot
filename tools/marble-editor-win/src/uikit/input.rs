use std::{
    collections::HashMap,
    rc::{Rc, Weak},
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
    ht_tree: SharedMut<HitTestTree>,
    mouse_capturing_element: Option<WeakMut<HitTestTree>>,
    mouse_current_enter_element: Option<WeakMut<HitTestTree>>,
    mouse_down_point: Option<(f32, f32, Option<WeakMut<HitTestTree>>)>,
    is_mouse_dragging: bool,
}
impl InputState {
    pub fn new(bound_window: HWND, ht_tree: &SharedMut<HitTestTree>) -> Self {
        Self {
            bound_window,
            ht_tree: ht_tree.clone(),
            mouse_capturing_element: None,
            mouse_current_enter_element: None,
            mouse_down_point: None,
            is_mouse_dragging: false,
        }
    }

    fn update_mouse_pos(&mut self, x: f32, y: f32, actions: &mut Vec<InputAction>) {
        let over_tree = HitTestTree::check(&self.ht_tree, x, y);
        let over_changes = over_tree.as_ref().map(|x| x.borrow().id)
            != self
                .mouse_current_enter_element
                .as_ref()
                .and_then(Weak::upgrade)
                .map(|x| x.borrow().id);
        if let Some(x) = self
            .mouse_current_enter_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            if Some(x.borrow().id) != over_tree.as_ref().map(|x| x.borrow().id) {
                // leave
                actions.push(InputAction::PointerLeave(x.borrow().eh.clone()));
            }
        }
        self.mouse_current_enter_element = over_tree.as_ref().map(Rc::downgrade);
        if over_changes {
            if let Some(x) = self
                .mouse_current_enter_element
                .as_ref()
                .and_then(Weak::upgrade)
            {
                actions.push(InputAction::PointerEnter(x.borrow().eh.clone()));
            }
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

        if let Some(e) = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            if let Some((dx, dy, _)) = self.mouse_down_point.as_ref() {
                if !self.is_mouse_dragging {
                    // 閾値を超えた後は永続的にドラッグ状態になる
                    let dist2 = (dx - x).powi(2) + (dy - y).powi(2);
                    if dist2 >= DRAG_THRESHOLD_DIST2 {
                        self.is_mouse_dragging = true;
                        actions.push(InputAction::BeginDrag(e.borrow().eh.clone()));
                    }
                }

                if self.is_mouse_dragging {
                    actions.push(InputAction::DragMove(e.borrow().eh.clone()));
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
                    if let Some(e) = down_element.as_ref().and_then(Weak::upgrade) {
                        actions.push(InputAction::BeginDrag(e.borrow().eh.clone()));
                    }
                }
            }

            if self.is_mouse_dragging {
                if let Some(e) = down_element.as_ref().and_then(Weak::upgrade) {
                    actions.push(InputAction::DragMove(e.borrow().eh.clone()));
                }
            }
        }

        actions
    }

    pub fn on_mouse_down(&mut self, x: f32, y: f32) -> Vec<InputAction> {
        let mut actions = Vec::with_capacity(16);

        let active_target = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
            .or_else(|| {
                self.update_mouse_pos(x, y, &mut actions);
                self.mouse_current_enter_element
                    .as_ref()
                    .and_then(Weak::upgrade)
            });

        self.mouse_down_point = Some((x, y, self.mouse_current_enter_element.clone()));
        self.is_mouse_dragging = false;
        if let Some(e) = active_target {
            actions.push(InputAction::PointerDown(e.borrow().eh.clone()));
        }

        actions
    }

    pub fn on_mouse_up(&mut self, x: f32, y: f32) -> Vec<InputAction> {
        let mut actions = Vec::with_capacity(16);

        if let Some(e) = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            actions.push(InputAction::PointerUp(e.borrow().eh.clone()));
            if !self.is_mouse_dragging {
                actions.push(InputAction::Click(e.borrow().eh.clone()));
            } else {
                actions.push(InputAction::EndDrag(e.borrow().eh.clone()));
            }
            self.mouse_down_point = None;

            return actions;
        }

        self.update_mouse_pos(x, y, &mut actions);

        if !self.is_mouse_dragging {
            if let Some(x) = self
                .mouse_current_enter_element
                .as_ref()
                .and_then(Weak::upgrade)
            {
                actions.push(InputAction::Click(x.borrow().eh.clone()));
            }
        } else {
            if let Some(x) = self
                .mouse_down_point
                .as_ref()
                .and_then(|x| x.2.as_ref())
                .and_then(std::rc::Weak::upgrade)
            {
                actions.push(InputAction::EndDrag(x.borrow().eh.clone()));
            }
        }
        self.mouse_down_point = None;

        actions
    }

    pub fn set_cursor(&self) -> bool {
        if let Some(e) = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            // TODO: caching loaded cursors
            let c = match e.borrow().eh.hover_cursor() {
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

        if let Some(e) = self
            .mouse_current_enter_element
            .as_ref()
            .and_then(Weak::upgrade)
        {
            // TODO: caching loaded cursors
            let c = match e.borrow().eh.hover_cursor() {
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

            true
        } else {
            false
        }
    }

    pub fn nc_hittest(&self, x: f32, y: f32) -> u32 {
        let Some(active_element) = self
            .mouse_capturing_element
            .as_ref()
            .and_then(Weak::upgrade)
            .or_else(|| HitTestTree::check(&self.ht_tree, x, y))
        else {
            return HTCLIENT;
        };

        let r = active_element.borrow().eh.nc_hittest();
        r
    }
}

pub struct HitTestTree {
    eh: Rc<dyn InputEventHandler>,
    id: usize,
    rect: Rect,
    parent: WeakMut<HitTestTree>,
    children: HashMap<usize, SharedMut<HitTestTree>>,
}
impl HitTestTree {
    #[inline]
    pub fn new(
        eh: &Rc<impl InputEventHandler + 'static>,
        id: usize,
        rect: Rect,
    ) -> SharedMut<Self> {
        new_shared_mut(Self {
            eh: eh.clone(),
            id,
            rect,
            parent: empty_weak_mut(),
            children: HashMap::new(),
        })
    }
    #[inline]
    pub fn new_unsized(
        eh: &Rc<impl InputEventHandler + 'static>,
        id: usize,
        left: f32,
        top: f32,
    ) -> SharedMut<Self> {
        Self::new(
            eh,
            id,
            Rect {
                X: left,
                Y: top,
                Width: f32::MAX,
                Height: f32::MAX,
            },
        )
    }

    #[inline]
    pub fn add_child(this: &SharedMut<Self>, child: SharedMut<HitTestTree>) {
        child.borrow_mut().parent = Rc::downgrade(this);
        let cid = child.borrow().id;
        this.borrow_mut().children.insert(cid, child);
    }

    #[inline]
    pub fn remove_child(&mut self, child: &SharedMut<HitTestTree>) {
        let cb = child.borrow();
        self.children.remove(&cb.id);
        drop(cb);
        child.borrow_mut().parent = empty_weak_mut();
    }

    #[inline]
    pub fn remove_all_children(&mut self) {
        for c in self.children.values() {
            c.borrow_mut().parent = empty_weak_mut();
        }

        self.children.clear();
    }

    #[inline]
    pub fn unmount(&mut self) {
        if let Some(parent) = self.parent.upgrade() {
            parent.borrow_mut().children.remove(&self.id);
            self.parent = empty_weak_mut();
        }
    }

    #[inline]
    pub const fn rect(&self) -> &Rect {
        &self.rect
    }
    #[inline]
    pub fn set_rect(&mut self, left: f32, top: f32, width: f32, height: f32) {
        self.rect = Rect {
            X: left,
            Y: top,
            Width: width,
            Height: height,
        };
    }
    #[inline]
    pub fn set_size(&mut self, width: f32, height: f32) {
        self.rect.Width = width;
        self.rect.Height = height;
    }
    #[inline]
    pub fn set_offset(&mut self, left: f32, top: f32) {
        self.rect.X = left;
        self.rect.Y = top;
    }
    #[inline]
    pub fn set_left(&mut self, left: f32) {
        self.rect.X = left;
    }
    #[inline]
    pub fn set_right(&mut self, right: f32) {
        self.rect.X = right - self.rect.Width;
    }
    #[inline]
    pub fn set_width(&mut self, width: f32) {
        self.rect.Width = width;
    }

    pub fn check(this: &SharedMut<Self>, x: f32, y: f32) -> Option<SharedMut<Self>> {
        let this1 = this.borrow();
        if this1.rect.contains_point(x, y) {
            let child = this1
                .children
                .values()
                .find_map(|c| Self::check(c, x - this1.rect.X, y - this1.rect.Y));
            Some(child.unwrap_or(this.clone()))
        } else {
            None
        }
    }
}
impl core::fmt::Debug for HitTestTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HitTestTree")
            .field("id", &self.id)
            .field("left", &self.rect.X)
            .field("top", &self.rect.Y)
            .field("width", &self.rect.Width)
            .field("height", &self.rect.Height)
            .field("children", &self.children)
            .finish_non_exhaustive()
    }
}

pub struct HitTestTreeContext {
    current_id: usize,
}
impl HitTestTreeContext {
    pub fn new() -> Self {
        Self { current_id: 0 }
    }

    pub fn new_id(&mut self) -> usize {
        self.current_id += 1;
        self.current_id
    }
}
