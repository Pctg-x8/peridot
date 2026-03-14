use std::collections::BTreeSet;

use crate::{
    DragPreviewPopoverHandle, Event, WindowHandle,
    input::{EventContinueControl, FocusTargetToken},
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
    pub action_handler: Option<std::rc::Weak<dyn HitTestTreeActionHandler + 'h>>,
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
            action_handler: None,
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
}

#[repr(transparent)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HitTestTreeRef(usize);

struct HitTestTreeRelationData {
    parent: Option<usize>,
    children: Vec<usize>,
}

pub trait HitTestTreeCreate<'h> {
    fn create(&mut self, data: HitTestTreeData<'h>) -> HitTestTreeRef;
}

pub struct HitTestTreeManagerCreateOnlyAccess<'h> {
    ptr: *mut HitTestTreeManager<'h>,
}
impl<'h> HitTestTreeCreate<'h> for HitTestTreeManagerCreateOnlyAccess<'h> {
    #[inline(always)]
    fn create(&mut self, data: HitTestTreeData<'h>) -> HitTestTreeRef {
        unsafe { (*self.ptr).create(data) }
    }
}

pub struct HitTestTreeManager<'h> {
    data: Vec<HitTestTreeData<'h>>,
    relations: Vec<HitTestTreeRelationData>,
    free_index: BTreeSet<usize>,
}
impl<'h> HitTestTreeCreate<'h> for HitTestTreeManager<'h> {
    fn create(&mut self, data: HitTestTreeData<'h>) -> HitTestTreeRef {
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
}
impl<'h> HitTestTreeManager<'h> {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            relations: Vec::new(),
            free_index: BTreeSet::new(),
        }
    }

    pub const fn derive_create_only_access(&mut self) -> HitTestTreeManagerCreateOnlyAccess<'h> {
        HitTestTreeManagerCreateOnlyAccess { ptr: self }
    }

    pub fn free(&mut self, r: HitTestTreeRef) {
        self.free_index.insert(r.0);
    }

    pub fn free_all(&mut self, r: HitTestTreeRef) {
        let mut stack = vec![r.0];
        while let Some(c) = stack.pop() {
            stack.extend(self.relations[c].children.drain(..));
            self.free(HitTestTreeRef(c));
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

    #[inline]
    pub fn parent_of(&self, r: HitTestTreeRef) -> Option<HitTestTreeRef> {
        self.relations
            .get(r.0)
            .and_then(|r| r.parent)
            .map(HitTestTreeRef)
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
        if let Some(old_parent) = self.relations[child.0].parent.replace(parent.0) {
            // 古い親から外す
            self.relations[old_parent]
                .children
                .retain(|&x| x != child.0);
        }

        self.relations[parent.0].children.push(child.0);
    }

    pub fn remove_child(&mut self, child: HitTestTreeRef) {
        let Some(p) = self.relations[child.0].parent.take() else {
            // 親なし
            return;
        };

        self.relations[p].children.retain(|&x| x != child.0);
    }

    pub fn dump(&self, root: HitTestTreeRef) {
        fn rec(sink: &mut String, this: &HitTestTreeManager, r: usize, indent: usize) {
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
            } = this.data[r];

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
                flags = flags.join("/")
            );

            for &c in &this.relations[r].children {
                rec(sink, this, c, indent + 1);
            }
        }

        let mut buf = String::from("\n");
        rec(&mut buf, self, root.0, 0);
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
                    HitTestTreeRef(p),
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

        // 後ろにあるほうが上なので優先して見る
        if let Some(t) = self.relations[root.0]
            .children
            .iter()
            .rev()
            .find_map(|&c| self.test(HitTestTreeRef(c), global_pos, &effective_global_rect))
        {
            // 子にヒット
            return Some(t);
        }

        if d.opaque && effective_global_rect.point_in_inclusive(global_pos) {
            // 自分にヒット(不透明の場合のみ 透過で指定されている場合はヒットしてない扱いにする)
            return Some(root);
        }

        // なににもヒットしなかった
        None
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
}
impl CursorShape {
    #[cfg(feature = "wayland")]
    pub const fn as_wayland(&self) -> peridot_tp_wayland::WpCursorShapeDeviceV1Shape {
        match self {
            Self::Default => peridot_tp_wayland::WpCursorShapeDeviceV1Shape::Default,
            Self::Pointer => peridot_tp_wayland::WpCursorShapeDeviceV1Shape::Pointer,
            Self::IBeam => peridot_tp_wayland::WpCursorShapeDeviceV1Shape::Text,
            Self::ResizeHorizontal => peridot_tp_wayland::WpCursorShapeDeviceV1Shape::EwResize,
        }
    }
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

pub struct PointerActionArgs {
    pub client_pos: Point<LogicalUnit>,
    pub client_size: Size<LogicalUnit>,
}

pub struct HitTestEventContext<'env, 'h> {
    pub sender_window: WindowHandle,
    pub current_sec: f32,
    pub composite_tree: &'env mut crate::rendering::composite::CompositeTree<Event>,
    pub drag_preview: &'env DragPreviewPopoverHandle,
    pub system_link: &'env crate::SystemLink<'env>,
    pub ht_create_only_access: &'env mut HitTestTreeManagerCreateOnlyAccess<'h>,
}

pub trait HitTestTreeActionHandler {
    #[allow(unused_variables)]
    #[inline]
    fn keyboard_focus(&self, sender: HitTestTreeRef) -> Option<FocusTargetToken> {
        None
    }

    #[allow(unused_variables)]
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_pointer_up(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_click(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_drag_start(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_drag_move(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }

    #[allow(unused_variables)]
    fn on_drag_end(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        EventContinueControl::empty()
    }
}
