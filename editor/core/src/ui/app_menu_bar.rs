use core::cell::Cell;
use std::rc::Rc;

use crate::{
    Event, SystemLink,
    input::{
        EventContinueControl, InputEventContext,
        hittest::{
            HitTestTreeActionHandler, HitTestTreeData, HitTestTreeManager, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs,
        },
    },
    model::ApplicationMutation,
    rendering::{
        composite::{
            AnimatableColor, AnimatableFloat, AnimationCurve, CompositeMode, CompositeRect,
            CompositeRectScaleFactor, CompositeRectText, CompositeRectTextHorizontalAlignment,
            CompositeRectTextRun, CompositeRectTextVerticalAlignment, CompositeTree,
            CompositeTreeRef,
        },
        text::{FontID, TextLayout},
    },
    uikit::{MenuCommandSelectionHandler, MenuItem, ViewRenderElements},
    utils::{Point, Size},
};

pub struct View {
    top: f32,
    eh: Option<Rc<EventHandler>>,
    labels: Vec<(String, Vec<MenuItem>)>,
}
impl View {
    pub const HEIGHT: f32 = ItemSubView::ITEM_HEIGHT;

    pub fn new(top: f32, labels: Vec<(String, Vec<MenuItem>)>) -> Self {
        Self {
            top,
            eh: None,
            labels,
        }
    }

    pub fn on_close_all<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        let eh = self.eh.as_ref().expect("not rendered?");
        eh.opening.set(false);

        if eh.last_lit_index.get().is_none() {
            for x in eh.items.iter() {
                x.unlit(composite_tree, current_sec);
            }
        }

        // if eh.last_lit_index.get().is_some() {
        //     // 連続で開いちゃうので次のPointerDownを無視する
        //     eh.ignore_next_pointer_down_event.set(true);
        // }
    }
}
impl crate::uikit::View for View {
    fn render(
        &mut self,
        _layout_rect: crate::utils::Rect<crate::utils::LogicalUnit>,
        ctx: &mut crate::uikit::RenderContext,
        _layout_state: &crate::uikit::ViewLayoutStateStore,
    ) -> crate::uikit::ViewRenderElements {
        let e = match self.eh {
            Some(ref e) => e,
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .expand_width()
                    .offset_imm(0.0, self.top)
                    .size_imm(0.0, ItemSubView::ITEM_HEIGHT)
                    .create(ctx.composite_tree);
                let ht_root = ctx.ht_manager.create(HitTestTreeData {
                    top: self.top,
                    height: ItemSubView::ITEM_HEIGHT,
                    width_adjustment_factor: 1.0,
                    ..Default::default()
                });

                let mut item_views = Vec::with_capacity(self.labels.len());
                let mut item_left = 0.0;
                for (label, items) in self.labels.iter() {
                    let v = ItemSubView::new(
                        label.clone(),
                        item_left,
                        items.clone(),
                        ctx.composite_tree,
                        ctx.ht_manager,
                        ctx.system_link,
                    );
                    ctx.composite_tree.add_child(ct_root, v.ct_root);
                    ctx.ht_manager.add_child(ht_root, v.ht_root);
                    item_left += v.width + ItemSubView::PADDING_INLINE * 2.0;
                    item_views.push(v);
                }

                let eh = Rc::new(EventHandler {
                    ct_root,
                    ht_root,
                    items: item_views,
                    opening: Cell::new(false),
                    last_lit_index: Cell::new(None),
                    ignore_next_pointer_down_event: Cell::new(false),
                });
                for x in eh.items.iter() {
                    x.bind_event_handler(&eh, ctx.ht_manager);
                }

                &*self.eh.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut crate::uikit::TeardownContext) {
        let Some(e) = self.eh.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(e.ct_root);
        ctx.ht_manager.free_all(e.ht_root);
    }

    fn measure_preferred_content_size(
        &self,
        _ctx: &mut crate::uikit::MeasureContext,
    ) -> Size<crate::utils::LogicalUnit> {
        Size::new_logical(0.0, ItemSubView::ITEM_HEIGHT)
    }
}

struct EventHandler {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    items: Vec<ItemSubView>,
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
                        command_handler: (Box::new(AppMenuCommandHandler)
                            as Box<dyn MenuCommandSelectionHandler>)
                            .into(),
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
            command_handler: (Box::new(AppMenuCommandHandler)
                as Box<dyn MenuCommandSelectionHandler>)
                .into(),
            surface_pos: Point::new_logical(x, y + h),
        });

        EventContinueControl::STOP_PROPAGATION
    }
}

struct ItemSubView {
    ct_root: CompositeTreeRef,
    ht_root: HitTestTreeRef,
    width: f32,
    items: Vec<MenuItem>,
    lit: core::cell::Cell<bool>,
}
impl ItemSubView {
    const ITEM_HEIGHT: f32 = 20.0;
    const PADDING_INLINE: f32 = 8.0;

    fn new<E>(
        label: String,
        left: f32,
        items: Vec<MenuItem>,
        composite_tree: &mut CompositeTree<E>,
        ht_manager: &mut HitTestTreeManager,
        syslink: &SystemLink,
    ) -> Self {
        let text_width =
            TextLayout::measure_visual_width(&label, FontID::UIDefault, syslink.font_set());

        let ct_root = composite_tree.create(CompositeRect {
            scale_factor: CompositeRectScaleFactor::UI,
            offset: [AnimatableFloat::Value(left), AnimatableFloat::Value(0.0)],
            size: [
                AnimatableFloat::Value(text_width + Self::PADDING_INLINE * 2.0),
                AnimatableFloat::Value(ItemSubView::ITEM_HEIGHT),
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
        let ht_root = ht_manager.create(HitTestTreeData {
            left,
            width: text_width + Self::PADDING_INLINE * 2.0,
            height: ItemSubView::ITEM_HEIGHT,
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

    fn lit<E>(&self, composite_tree: &mut CompositeTree<E>, current_sec: f32) {
        if self.lit.replace(true) {
            // already lit
            return;
        }

        composite_tree.get_mut(self.ct_root).composite_mode =
            CompositeMode::FillColor(AnimatableColor::Animated {
                from_value: [1.0, 1.0, 1.0, 0.0],
                to_value: [1.0, 1.0, 1.0, 0.25],
                sec_duration: (current_sec..current_sec + 0.1).into(),
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
                sec_duration: (current_sec..current_sec + 0.1).into(),
                curve: AnimationCurve::Linear,
                event_on_complete: None,
            });
        composite_tree.mark_dirty(self.ct_root);
    }
}

struct AppMenuCommandHandler;
impl MenuCommandSelectionHandler for AppMenuCommandHandler {
    fn on_select_command(&mut self, command_id: u64, _context: &mut ApplicationMutation) {
        tracing::trace!(command_id, "todo: app menu command selection");
    }
}
