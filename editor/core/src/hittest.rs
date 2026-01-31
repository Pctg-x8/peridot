use std::collections::BTreeSet;

use crate::input::{EventContinueControl, FocusTargetToken};

pub struct HitTestTreeData<'h> {
    pub left: f32,
    pub top: f32,
    pub left_adjustment_factor: f32,
    pub top_adjustment_factor: f32,
    pub width: f32,
    pub height: f32,
    pub width_adjustment_factor: f32,
    pub height_adjustment_factor: f32,
    pub action_handler: Option<std::rc::Weak<dyn HitTestTreeActionHandler + 'h>>,
}
impl Default for HitTestTreeData<'_> {
    #[inline]
    fn default() -> Self {
        Self {
            left: 0.0,
            top: 0.0,
            left_adjustment_factor: 0.0,
            top_adjustment_factor: 0.0,
            width: 0.0,
            height: 0.0,
            width_adjustment_factor: 0.0,
            height_adjustment_factor: 0.0,
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
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct HitTestTreeRef(usize);

struct HitTestTreeRelationData {
    parent: Option<usize>,
    children: Vec<usize>,
}

pub struct HitTestTreeManager<'h> {
    data: Vec<HitTestTreeData<'h>>,
    relations: Vec<HitTestTreeRelationData>,
    free_index: BTreeSet<usize>,
}
impl<'h> HitTestTreeManager<'h> {
    pub const ROOT: HitTestTreeRef = HitTestTreeRef(0);

    pub fn new() -> Self {
        let mut this = Self {
            data: Vec::new(),
            relations: Vec::new(),
            free_index: BTreeSet::new(),
        };

        // root(simply fits to client_width/client_height)
        this.create(HitTestTreeData {
            left: 0.0,
            top: 0.0,
            left_adjustment_factor: 0.0,
            top_adjustment_factor: 0.0,
            width: 0.0,
            height: 0.0,
            width_adjustment_factor: 1.0,
            height_adjustment_factor: 1.0,
            action_handler: None,
        });

        this
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
    pub fn get_data(&self, r: HitTestTreeRef) -> &HitTestTreeData {
        &self.data[r.0]
    }

    #[inline]
    pub fn get_data_mut(&mut self, r: HitTestTreeRef) -> &mut HitTestTreeData<'h> {
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
            let _ = writeln!(
                sink,
                "#{r}: (x{left_adjustment_factor}+{left}, x{top_adjustment_factor}+{top}) size (x{width_adjustment_factor}+{width}, x{height_adjustment_factor}+{height})"
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
        global_x: f32,
        global_y: f32,
        parent_global_left: f32,
        parent_global_top: f32,
        parent_effective_width: f32,
        parent_effective_height: f32,
    ) -> Option<HitTestTreeRef> {
        let d = &self.data[root.0];
        if d.action_handler
            .as_ref()
            .and_then(std::rc::Weak::upgrade)
            .is_some_and(|x| !x.hit_active(root))
        {
            // hit disabled
            return None;
        }

        let effective_width = parent_effective_width * d.width_adjustment_factor + d.width;
        let effective_height = parent_effective_height * d.height_adjustment_factor + d.height;
        let global_left =
            parent_global_left + parent_effective_width * d.left_adjustment_factor + d.left;
        let global_top =
            parent_global_top + parent_effective_height * d.top_adjustment_factor + d.top;
        let global_right = global_left + effective_width;
        let global_bottom = global_top + effective_height;

        // 後ろにあるほうが上なので優先して見る
        if let Some(t) = self.relations[root.0].children.iter().rev().find_map(|&c| {
            self.test(
                HitTestTreeRef(c),
                global_x,
                global_y,
                global_left,
                global_top,
                effective_width,
                effective_height,
            )
        }) {
            // 子にヒット
            return Some(t);
        }

        if global_left <= global_x
            && global_x <= global_right
            && global_top <= global_y
            && global_y <= global_bottom
            && d.action_handler.is_some()
        {
            // 自分にヒット ただしaction handlerが設定されていない場合は透過とみなす(うしろにあるHitTestTreeにあたってほしい)
            return Some(root);
        }

        // なににもヒットしなかった
        None
    }
}

#[derive(Clone, Copy, Debug)]
pub enum CursorShape {
    Default,
    Pointer,
    IBeam,
    ResizeHorizontal,
}

pub struct PointerActionArgs {
    pub client_x: f32,
    pub client_y: f32,
    pub client_width: f32,
    pub client_height: f32,
}

#[derive(Debug, Clone, Copy)]
pub enum Role {
    ForceClient,
    TitleBar,
    CloseButton,
    MaximizeButton,
    MinimizeButton,
    RestoreButton,
}

pub struct HitTestEventContext {}

// 将来的にはAppUpdateContextへの直接依存を剥がしたいが、associated typeに局所的なlifetime与える方法がない
pub trait HitTestTreeActionHandler {
    #[allow(unused_variables)]
    #[inline]
    fn role(&self, sender: HitTestTreeRef) -> Option<Role> {
        None
    }

    #[allow(unused_variables)]
    #[inline]
    fn hit_active(&self, sender: HitTestTreeRef) -> bool {
        true
    }

    #[allow(unused_variables)]
    #[inline]
    fn cursor_shape(
        &self,
        sender: HitTestTreeRef,
        context: &mut HitTestEventContext,
    ) -> CursorShape {
        CursorShape::Default
    }

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
}
