use core::cell::Cell;
use std::rc::Rc;

use crate::{
    Event,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs,
        },
    },
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectText, CompositeRectTextHorizontalAlignment, CompositeRectTextRun,
            CompositeRectTextVerticalAlignment, CompositeTree, CompositeTreeRef,
        },
        text::{FontID, TextLayout},
    },
    uikit::{MenuItem, MountContext, MountTarget, RawMountTarget, ViewInitContext},
    utils::Point,
};

pub struct View {
    eh: Rc<EventHandler>,
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
}
impl View {
    pub fn new(ctx: &mut ViewInitContext, top: f32, labels: Vec<(String, Vec<MenuItem>)>) -> Self {
        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            offset: [AnimatableFloat::Value(0.0), AnimatableFloat::Value(top)],
            size: [
                AnimatableFloat::Value(0.0),
                AnimatableFloat::Value(ItemView::ITEM_HEIGHT),
            ],
            relative_size_adjustment: [1.0, 0.0],
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            top,
            height: ItemView::ITEM_HEIGHT,
            width_adjustment_factor: 1.0,
            ..Default::default()
        });

        let mut item_views = Vec::with_capacity(labels.len());
        let mut item_left = 0.0;
        for (label, items) in labels {
            let v = ItemView::new(ctx, label, item_left, items);
            v.mount(ctx, &RawMountTarget { ct_root, ht_root });
            item_left += v.width + ItemView::PADDING_INLINE * 2.0;
            item_views.push(v);
        }

        let eh = Rc::new(EventHandler {
            items: item_views,
            opening: Cell::new(false),
            last_lit_index: Cell::new(None),
            ignore_next_pointer_down_event: Cell::new(false),
        });
        for x in eh.items.iter() {
            x.bind_event_handler(&eh, ctx.ht_manager);
        }

        Self {
            eh,
            ct_root,
            ht_root,
        }
    }

    pub fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(target.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.ht_root);
    }

    pub fn rescale<E>(&self, new_scale: f32, composite_tree: &mut CompositeTree<E>) {
        composite_tree.get_mut(self.ct_root).base_scale_factor = new_scale;
        composite_tree.mark_dirty(self.ct_root);

        for x in self.eh.items.iter() {
            x.rescale(new_scale, composite_tree);
        }
    }

    pub fn on_global_mouse_click<E>(
        &self,
        composite_tree: &mut CompositeTree<E>,
        current_sec: f32,
    ) {
        self.on_close_all(composite_tree, current_sec);
        if self.eh.last_lit_index.get().is_some() {
            // 連続で開いちゃうので次のPointerDownを無視する
            self.eh.ignore_next_pointer_down_event.set(true);
        }
    }

    pub fn on_close_all<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        self.eh.opening.set(false);

        if self.eh.last_lit_index.get().is_none() {
            for x in self.eh.items.iter() {
                x.unlit(composite_tree, current_sec);
            }
        }
    }
}

struct EventHandler {
    items: Vec<ItemView>,
    opening: Cell<bool>,
    last_lit_index: Cell<Option<usize>>,
    ignore_next_pointer_down_event: Cell<bool>,
}
impl HitTestTreeActionHandler for EventHandler {
    fn on_pointer_enter(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        let mut new_lit_index = None;
        for (n, x) in self.items.iter().enumerate() {
            if x.ht_root == sender {
                new_lit_index = Some(n);
                break;
            }
        }

        let last_lit = self.last_lit_index.replace(new_lit_index);
        if last_lit != new_lit_index {
            if self.opening.get()
                && let Some(x) = last_lit
            {
                self.items[x].unlit(context.composite_tree, context.current_sec);
            }
            if let Some(x) = new_lit_index {
                self.items[x].lit(context.composite_tree, context.current_sec);
                if self.opening.get() {
                    for (n, v) in self.items.iter().enumerate() {
                        if n != x {
                            v.unlit(context.composite_tree, context.current_sec);
                        }
                    }

                    let items = self.items[x].items.clone();
                    let parent = context
                        .ht_manager
                        .query_root_window(self.items[x].ht_root)
                        .expect("not mounted?");
                    let (x, y, _, h, _) = context
                        .ht_manager
                        .compute_global_rect_autoroot(self.items[x].ht_root);
                    context.system_link.dispatch_event(Event::MenuReopen {
                        parent,
                        items,
                        surface_pos: Point::new_logical(x, y + h),
                    });
                }
            }
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_leave(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.last_lit_index.set(None);

        if self.opening.get() {
            // メニュー開いてるときはunlitしない
            return EventContinueControl::STOP_PROPAGATION;
        }

        for x in self.items.iter() {
            if x.ht_root == sender {
                x.unlit(context.composite_tree, context.current_sec);
                break;
            }
        }

        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        if self.ignore_next_pointer_down_event.replace(false) {
            return EventContinueControl::STOP_PROPAGATION;
        }

        self.opening.set(true);
        let (x, y, _, h, _) = context.ht_manager.compute_global_rect_autoroot(sender);
        context.system_link.dispatch_event(Event::MenuOpen {
            parent: context
                .ht_manager
                .query_root_window(sender)
                .expect("not mounted?"),
            items: self
                .items
                .iter()
                .find(|x| x.ht_root == sender)
                .expect("invalid sender")
                .items
                .clone(),
            surface_pos: Point::new_logical(x, y + h),
        });

        EventContinueControl::STOP_PROPAGATION
    }
}

struct ItemView {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    width: f32,
    items: Vec<MenuItem>,
    lit: core::cell::Cell<bool>,
}
impl ItemView {
    const ITEM_HEIGHT: f32 = 20.0;
    const PADDING_INLINE: f32 = 8.0;

    fn new(ctx: &mut ViewInitContext, label: String, left: f32, items: Vec<MenuItem>) -> Self {
        let text_width =
            TextLayout::measure_visual_width(&label, FontID::UIDefault, ctx.system_link.font_set());

        let ct_root = ctx.mount_context.composite_tree.create(CompositeRect {
            base_scale_factor: ctx.ui_scale_factor,
            offset: [AnimatableFloat::Value(left), AnimatableFloat::Value(0.0)],
            size: [
                AnimatableFloat::Value(text_width + Self::PADDING_INLINE * 2.0),
                AnimatableFloat::Value(ItemView::ITEM_HEIGHT),
            ],
            has_bitmap: true,
            text: Some(CompositeRectText {
                runs: vec![CompositeRectTextRun {
                    font_id: FontID::UIDefault,
                    content: label,
                    color: AnimatableColor::Value([1.0, 1.0, 1.0, 1.0]),
                    ..Default::default()
                }],
                vertical_alignment: CompositeRectTextVerticalAlignment::Middle,
                horizontal_alignment: CompositeRectTextHorizontalAlignment::Middle,
                ..Default::default()
            }),
            ..Default::default()
        });
        let ht_root = ctx.ht_manager.create(HitTestTreeData {
            left,
            width: text_width + Self::PADDING_INLINE * 2.0,
            height: ItemView::ITEM_HEIGHT,
            ..Default::default()
        });

        Self {
            ct_root,
            ht_root,
            width: text_width,
            items,
            lit: core::cell::Cell::new(false),
        }
    }

    #[inline(always)]
    fn bind_event_handler(
        &self,
        event_handler: &Rc<EventHandler>,
        ht_manager: &mut HitTestTreeManager,
    ) {
        ht_manager.set_action_handler(self.ht_root, event_handler);
    }

    fn mount(&self, ctx: &mut MountContext, target: &(impl MountTarget + ?Sized)) {
        ctx.composite_tree.add_child(target.ct_root(), self.ct_root);
        ctx.ht_manager.add_child(target.ht_root(), self.ht_root);
    }

    fn rescale<E>(&self, new_scale: f32, composite_tree: &mut CompositeTree<E>) {
        composite_tree.get_mut(self.ct_root).base_scale_factor = new_scale;
        composite_tree.mark_dirty_all(self.ct_root);
    }

    fn lit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.lit.replace(true) {
            // already lit
            return;
        }

        composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.25],
                start_sec: current_sec,
                end_sec: current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            });
        composite_tree.mark_dirty(self.ct_root);
    }

    fn unlit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if !self.lit.replace(false) {
            // already unlit
            return;
        }

        composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.25],
                to_value: [1.0, 1.0, 1.0, 0.0],
                start_sec: current_sec,
                end_sec: current_sec + 0.1,
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            });
        composite_tree.mark_dirty(self.ct_root);
    }
}
