use std::collections::BTreeSet;

use crate::{
    PointerID, WindowHandle,
    input::{
        EventContinueControl, FocusTargetToken, InputEventContext, ModifierKey, PointerInputUnit,
    },
    utils::{LogicalUnit, Point, Rect, Size},
};

pub struct HitTestTreeData<'h> {
    pub active: bool,
    pub opaque: bool,
    pub left: f32,
    pub top: f32,
    pub left_adjustment_factor: f32,
    pub top_adjustment_factor: f32,
    pub width: f32,
    pub height: f32,
    pub width_adjustment_factor: f32,
    pub height_adjustment_factor: f32,
    pub role: Option<Role>,
    pub cursor_shape: CursorShape,
    pub keyboard_focus: Option<FocusTargetToken>,
    pub root_of_window: Option<WindowHandle>,
    pub action_handler: Option<std::rc::Weak<dyn HitTestTreeActionHandler + 'h>>,
    /// 子要素以降をこのRectでクリップする
    pub clip_children: bool,
    #[cfg(windows)]
    pub native_text_deferrable_event_handler:
        Option<std::rc::Weak<dyn crate::platform::windows::CoreTextDeferrableEventHandler + 'h>>,
    pub screen_reposition_handler:
        Option<std::rc::Weak<dyn HitTestTreeScreenRepositionHandler + 'h>>,
}
impl Default for HitTestTreeData<'_> {
    #[inline]
    fn default() -> Self {
        Self {
            active: true,
            opaque: true,
            left: 0.0,
            top: 0.0,
            left_adjustment_factor: 0.0,
            top_adjustment_factor: 0.0,
            width: 0.0,
            height: 0.0,
            width_adjustment_factor: 0.0,
            height_adjustment_factor: 0.0,
            role: None,
            cursor_shape: CursorShape::Default,
            keyboard_focus: None,
            root_of_window: None,
            action_handler: None,
            clip_children: false,
            #[cfg(windows)]
            native_text_deferrable_event_handler: None,
            screen_reposition_handler: None,
        }
    }
}
impl<'h> HitTestTreeData<'h> {
    #[inline]
    pub fn action_handler(&self) -> Option<std::rc::Rc<dyn HitTestTreeActionHandler + 'h>> {
        self.action_handler
            .as_ref()
            .and_then(std::rc::Weak::upgrade)
    }

    #[cfg(windows)]
    #[inline]
    pub fn native_text_deferrable_event_handler(
        &self,
    ) -> Option<std::rc::Rc<dyn crate::platform::windows::CoreTextDeferrableEventHandler + 'h>>
    {
        self.native_text_deferrable_event_handler
            .as_ref()
            .and_then(std::rc::Weak::upgrade)
    }

    #[inline]
    pub fn screen_reposition_handler(
        &self,
    ) -> Option<std::rc::Rc<dyn HitTestTreeScreenRepositionHandler + 'h>> {
        self.screen_reposition_handler
            .as_ref()
            .and_then(std::rc::Weak::upgrade)
    }
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HitTestTreeRef(usize);

struct HitTestTreeRelationData {
    parent: Option<HitTestTreeRef>,
    children: Vec<HitTestTreeRef>,
}

pub struct HitTestTreeManager<'h> {
    data: Vec<HitTestTreeData<'h>>,
    relations: Vec<HitTestTreeRelationData>,
    free_index: BTreeSet<usize>,
}
impl<'h> HitTestTreeManager<'h> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            relations: Vec::new(),
            free_index: BTreeSet::new(),
        }
    }

    pub fn create(&mut self, data: HitTestTreeData<'h>) -> HitTestTreeRef {
        if let Some(x) = self.free_index.pop_first() {
            self.data[x] = data;
            self.relations[x].parent = None;
            self.relations[x].children.clear();

            return HitTestTreeRef(x);
        }

        self.data.push(data);
        self.relations.push(HitTestTreeRelationData {
            parent: None,
            children: Vec::new(),
        });

        HitTestTreeRef(self.data.len() - 1)
    }

    #[inline]
    pub fn free(&mut self, r: HitTestTreeRef) {
        self.remove_child(r);
        self.free_index.insert(r.0);
    }

    pub fn free_all(&mut self, r: HitTestTreeRef) {
        let mut stack = vec![r];
        while let Some(c) = stack.pop() {
            stack.extend(self.relations[c.0].children.drain(..));
            self.free(c);
        }
    }

    #[inline]
    pub fn get_data<'d>(&'d self, r: HitTestTreeRef) -> &'d HitTestTreeData<'h> {
        &self.data[r.0]
    }

    #[inline]
    pub fn get_data_mut<'d>(&'d mut self, r: HitTestTreeRef) -> &'d mut HitTestTreeData<'h> {
        &mut self.data[r.0]
    }

    #[inline]
    pub fn set_action_handler(
        &mut self,
        r: HitTestTreeRef,
        h: &std::rc::Rc<impl HitTestTreeActionHandler + 'h>,
    ) {
        self.data[r.0].action_handler = Some(std::rc::Rc::downgrade(h) as _);
    }

    #[cfg(windows)]
    #[inline]
    pub fn set_native_text_deferrable_event_handler(
        &mut self,
        r: HitTestTreeRef,
        h: &std::rc::Rc<impl crate::platform::windows::CoreTextDeferrableEventHandler + 'h>,
    ) {
        self.data[r.0].native_text_deferrable_event_handler = Some(std::rc::Rc::downgrade(h) as _);
    }

    #[inline]
    pub fn set_screen_reposition_handler(
        &mut self,
        r: HitTestTreeRef,
        h: &std::rc::Rc<impl HitTestTreeScreenRepositionHandler + 'h>,
    ) {
        self.data[r.0].screen_reposition_handler = Some(std::rc::Rc::downgrade(h) as _);
    }

    #[inline]
    pub fn parent_of(&self, r: HitTestTreeRef) -> Option<HitTestTreeRef> {
        self.relations.get(r.0)?.parent
    }

    #[inline(always)]
    pub const fn iter_ascending_from<'d>(
        &'d self,
        r: HitTestTreeRef,
    ) -> impl Iterator<Item = HitTestTreeRef> {
        AscendingIterator {
            ht_manager: self,
            pointing: Some(r),
        }
    }

    pub fn add_child(&mut self, parent: HitTestTreeRef, child: HitTestTreeRef) {
        if let Some(old_parent) = self.relations[child.0].parent.replace(parent) {
            // 古い親から外す
            self.relations[old_parent.0]
                .children
                .retain(|&x| x != child);
        }

        self.relations[parent.0].children.push(child);
    }

    pub fn remove_child(&mut self, child: HitTestTreeRef) {
        let Some(p) = self.relations[child.0].parent.take() else {
            // 親なし
            return;
        };

        self.relations[p.0].children.retain(|&x| x != child);
    }

    pub fn dump(&self, root: HitTestTreeRef) {
        fn rec(sink: &mut String, this: &HitTestTreeManager, r: HitTestTreeRef, indent: usize) {
            use std::fmt::Write;

            for _ in 0..indent {
                sink.push_str("  ");
            }

            let HitTestTreeData {
                active,
                opaque,
                left,
                top,
                left_adjustment_factor,
                top_adjustment_factor,
                width,
                height,
                width_adjustment_factor,
                height_adjustment_factor,
                ..
            } = this.data[r.0];

            let mut flags = Vec::with_capacity(2);
            if active {
                flags.push("active");
            }
            if opaque {
                flags.push("opaque");
            }

            let _ = writeln!(
                sink,
                "#{r}: (x{left_adjustment_factor}+{left}, x{top_adjustment_factor}+{top}) size (x{width_adjustment_factor}+{width}, x{height_adjustment_factor}+{height}) {flags}",
                r = r.0,
                flags = flags.join("/")
            );

            for &c in &this.relations[r.0].children {
                rec(sink, this, c, indent + 1);
            }
        }

        let mut buf = String::from("\n");
        rec(&mut buf, self, root, 0);
        tracing::debug!(hit_test_tree = %buf);
    }

    pub fn translate_client_to_tree_local(
        &self,
        target: HitTestTreeRef,
        client_x: f32,
        client_y: f32,
        client_width: f32,
        client_height: f32,
    ) -> (f32, f32, f32, f32) {
        let d = &self.data[target.0];
        match self.relations[target.0].parent {
            None => {
                // parent = clientなので直接計算する
                let effective_left = client_width * d.left_adjustment_factor + d.left;
                let effective_top = client_height * d.top_adjustment_factor + d.top;

                (
                    client_x - effective_left,
                    client_y - effective_top,
                    client_width * d.width_adjustment_factor + d.width,
                    client_height * d.height_adjustment_factor + d.height,
                )
            }
            Some(p) => {
                // 親でいっかい計算して、その中のローカル座標として計算する
                let (
                    parent_local_x,
                    parent_local_y,
                    parent_effective_width,
                    parent_effective_height,
                ) = self.translate_client_to_tree_local(
                    p,
                    client_x,
                    client_y,
                    client_width,
                    client_height,
                );
                let effective_left = parent_effective_width * d.left_adjustment_factor + d.left;
                let effective_top = parent_effective_height * d.top_adjustment_factor + d.top;

                (
                    parent_local_x - effective_left,
                    parent_local_y - effective_top,
                    parent_effective_width * d.width_adjustment_factor + d.width,
                    parent_effective_height * d.height_adjustment_factor + d.height,
                )
            }
        }
    }

    fn compute_global_rect(
        &self,
        r: HitTestTreeRef,
        root_width: f32,
        root_height: f32,
    ) -> (f32, f32, f32, f32, HitTestTreeRef) {
        let d = &self.data[r.0];
        match self.relations[r.0].parent {
            None => (
                root_width * d.left_adjustment_factor + d.left,
                root_height * d.top_adjustment_factor + d.top,
                root_width * d.width_adjustment_factor + d.width,
                root_height * d.height_adjustment_factor + d.height,
                r,
            ),
            Some(parent) => {
                let (parent_x, parent_y, parent_w, parent_h, root_ht) =
                    self.compute_global_rect(parent, root_width, root_height);
                (
                    parent_x + parent_w * d.left_adjustment_factor + d.left,
                    parent_y + parent_h * d.top_adjustment_factor + d.top,
                    parent_w * d.width_adjustment_factor + d.width,
                    parent_h * d.height_adjustment_factor + d.height,
                    root_ht,
                )
            }
        }
    }

    pub fn compute_global_rect_autoroot(
        &self,
        r: HitTestTreeRef,
    ) -> (f32, f32, f32, f32, HitTestTreeRef) {
        let d = &self.data[r.0];
        match self.relations[r.0].parent {
            None => {
                let (root_width, root_height) = match d.root_of_window {
                    None => (0.0, 0.0),
                    Some(root) => {
                        let s = root.client_size();
                        (s.width, s.height)
                    }
                };

                (
                    root_width * d.left_adjustment_factor + d.left,
                    root_height * d.top_adjustment_factor + d.top,
                    root_width * d.width_adjustment_factor + d.width,
                    root_height * d.height_adjustment_factor + d.height,
                    r,
                )
            }
            Some(parent) => {
                let (parent_x, parent_y, parent_w, parent_h, root_ht) =
                    self.compute_global_rect_autoroot(parent);
                (
                    parent_x + parent_w * d.left_adjustment_factor + d.left,
                    parent_y + parent_h * d.top_adjustment_factor + d.top,
                    parent_w * d.width_adjustment_factor + d.width,
                    parent_h * d.height_adjustment_factor + d.height,
                    root_ht,
                )
            }
        }
    }

    #[cfg(windows)]
    pub fn compute_screen_rect_pixels_with_insets(
        &self,
        r: HitTestTreeRef,
        inset_lt: Point<LogicalUnit>,
        inset_rb: Point<LogicalUnit>,
    ) -> Rect<crate::utils::PixelsUnit> {
        let (gx, gy, gw, gh, root_ht) = self.compute_global_rect_autoroot(r);
        let (wx, wy, s) = match self.data[root_ht.0].root_of_window {
            None => (0, 0, 1.0),
            Some(window_handle) => {
                let p = window_handle.screen_position();
                (p.x, p.y, window_handle.ui_scale_factor())
            }
        };

        Rect::from_lt_size(
            Point::new_pixels(
                ((gx + inset_lt.x) * s).ceil() as i32 + wx,
                ((gy + inset_lt.y) * s).ceil() as i32 + wy,
            ),
            Size::new_logical(gw - inset_lt.x - inset_rb.x, gh - inset_lt.y - inset_rb.y)
                .to_pixels_ceil(s),
        )
    }

    #[inline]
    pub fn query_root_window(&self, r: HitTestTreeRef) -> Option<WindowHandle> {
        match self.relations[r.0].parent {
            Some(p) => self.query_root_window(p),
            None => self.data[r.0].root_of_window,
        }
    }

    #[inline]
    pub fn translate_tree_local_to_root(
        &self,
        from: HitTestTreeRef,
        x: f32,
        y: f32,
        root_width: f32,
        root_height: f32,
    ) -> (f32, f32) {
        let (gx, gy, _, _, _) = self.compute_global_rect(from, root_width, root_height);
        (gx + x, gy + y)
    }

    #[inline]
    pub fn translate_tree_local_to_root_autoroot(
        &self,
        from: HitTestTreeRef,
        x: f32,
        y: f32,
    ) -> (f32, f32) {
        let (gx, gy, _, _, _) = self.compute_global_rect_autoroot(from);
        (gx + x, gy + y)
    }

    pub fn test(
        &self,
        root: HitTestTreeRef,
        global_pos: &Point<LogicalUnit>,
        parent_effective_global_rect: &Rect<LogicalUnit>,
    ) -> Option<HitTestTreeRef> {
        let d = &self.data[root.0];
        if !d.active {
            // hit disabled
            return None;
        }

        // グローバル座標での実際の自身のジオメトリを計算
        let effective_global_rect = Rect::from_lt_size(
            Point::new_logical(
                parent_effective_global_rect.left
                    + parent_effective_global_rect.width * d.left_adjustment_factor
                    + d.left,
                parent_effective_global_rect.top
                    + parent_effective_global_rect.height * d.top_adjustment_factor
                    + d.top,
            ),
            Size::new_logical(
                parent_effective_global_rect.width * d.width_adjustment_factor + d.width,
                parent_effective_global_rect.height * d.height_adjustment_factor + d.height,
            ),
        );
        if d.clip_children && !effective_global_rect.point_in_inclusive(global_pos) {
            // clipped but not hit to self
            return None;
        }

        // 後ろにあるほうが上なので優先して見る
        if let Some(t) = self.relations[root.0]
            .children
            .iter()
            .rev()
            .find_map(|&c| self.test(c, global_pos, &effective_global_rect))
        {
            // 子にヒット
            return Some(t);
        }

        if !d.opaque {
            // not opaque
            return None;
        }

        if !effective_global_rect.point_in_inclusive(global_pos) {
            // out of bounds
            return None;
        }

        if d.action_handler().is_some_and(|a| {
            !a.hittest(
                root,
                &HitTestArgs {
                    tree_local_x: global_pos.x - effective_global_rect.left,
                    tree_local_y: global_pos.y - effective_global_rect.top,
                },
            )
        }) {
            // hittest failed
            return None;
        }

        Some(root)
    }
}

pub struct AscendingIterator<'ht, 'h> {
    ht_manager: &'ht HitTestTreeManager<'h>,
    pointing: Option<HitTestTreeRef>,
}
impl<'ht, 'h> Iterator for AscendingIterator<'ht, 'h> {
    type Item = HitTestTreeRef;

    fn next(&mut self) -> Option<Self::Item> {
        match self.pointing {
            None => None,
            Some(p) => {
                self.pointing = self.ht_manager.parent_of(p);
                Some(p)
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CursorShape {
    Default,
    Pointer,
    IBeam,
    ResizeHorizontal,
    ResizeVertical,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    ForceClient,
    TitleBar,
    CloseButton,
    MaximizeButton,
    MinimizeButton,
    RestoreButton,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PointerButton {
    /// だいたいは左
    Primary,
    /// だいたいは右
    Secondary,
}

pub struct PointerActionArgs {
    pub pointer_id: PointerID,
    pub client_pos: Point<LogicalUnit>,
    pub client_size: Size<LogicalUnit>,
}

pub struct PointerButtonActionArgs {
    pub button: PointerButton,
    pub pointer_id: PointerID,
    pub client_pos: Point<LogicalUnit>,
    pub client_size: Size<LogicalUnit>,
    pub key_modifier: ModifierKey,
}

pub struct ScrollWheelActionArgs {
    pub amount: f32,
    pub key_modifier: ModifierKey,
}
impl ScrollWheelActionArgs {
    #[inline(always)]
    pub const fn make_empty_response(&self) -> ScrollWheelActionResponse {
        ScrollWheelActionResponse {
            continue_flags: EventContinueControl::empty(),
            left_amount: self.amount,
        }
    }
}
pub struct ScrollWheelActionResponse {
    pub continue_flags: EventContinueControl,
    pub left_amount: f32,
}

pub struct GrabDeltaMoveActionArgs {
    pub pointer_id: PointerID,
    pub delta: Point<LogicalUnit>,
}

pub struct HitTestArgs {
    pub tree_local_x: f32,
    pub tree_local_y: f32,
}

pub trait HitTestTreeActionHandler {
    #[allow(unused_variables)]
    fn hittest(&self, target: HitTestTreeRef, args: &HitTestArgs) -> bool {
        true
    }

    #[allow(unused_variables)]
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_hover(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_up(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_double_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_drag_start(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_drag_end(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_scroll_wheel(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &ScrollWheelActionArgs,
    ) -> ScrollWheelActionResponse {
        args.make_empty_response()
    }

    #[allow(unused_variables)]
    fn grab_delta_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &GrabDeltaMoveActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }
}

pub trait HitTestTreeScreenRepositionHandler {
    fn on_screen_reposition_required(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        window_screen_pos: Point<PointerInputUnit>,
    );
}
