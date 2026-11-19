use core::cell::Cell;
use std::rc::Rc;

use shared::{LogicalUnit, Point, Rect, Size};

use crate::{
    input::{
        EventContinueControl, InputEventContext, PointerInputUnit,
        hittest::{
            CursorShape, HitTestTreeActionHandler, HitTestTreeData, HitTestTreeRef,
            PointerActionArgs, PointerButtonActionArgs,
        },
    },
    rendering::composite::{CompositeRect, CompositeTreeRef},
    ui::dock::{DESIGN_METRICS, DockID},
    uicore::{
        MeasureContext, RenderContext, TeardownContext, ViewConstructor, ViewLayoutStateStore,
        ViewRenderElements,
    },
};

/// Pane分割方向
#[derive(Clone, Copy)]
pub enum Direction {
    /// 横
    Horizontal,
    /// 縦
    Vertical,
}
impl Direction {
    /// Splitterに適切なカーソル形状を得る
    pub const fn cursor_shape(&self) -> CursorShape {
        match self {
            Self::Horizontal => CursorShape::ResizeHorizontal,
            Self::Vertical => CursorShape::ResizeVertical,
        }
    }

    /// Splitterが制御する方向の値を得る
    pub const fn dominant_coordinate(&self, p: &Point<LogicalUnit>) -> f32 {
        match self {
            Self::Horizontal => p.x,
            Self::Vertical => p.y,
        }
    }
}

/// `View`の初期データ
pub struct ViewInit {
    pub dir: Direction,
    pub controlling_dock: DockID,
}
impl ViewConstructor for ViewInit {
    type ConcreteView = View;

    #[inline(always)]
    fn construct(
        self,
        _id: crate::uicore::TypedViewIdentifier<Self::ConcreteView>,
    ) -> Self::ConcreteView {
        View {
            dir: self.dir,
            controlling_dock: self.controlling_dock,
            entity: None,
        }
    }
}

/// Dock間のSplitter
pub struct View {
    dir: Direction,
    controlling_dock: DockID,
    entity: Option<Rc<Entity>>,
}
impl View {
    /// 制御対象のDockを変更
    pub(super) fn rebind_controlling_dock(&mut self, dock: DockID) {
        self.controlling_dock = dock;
        if let Some(ref entity) = self.entity {
            entity.controlling_dock.set(dock);
        }
    }
}
impl crate::uicore::View for View {
    fn render(
        &mut self,
        layout_rect: Rect<LogicalUnit>,
        ctx: &mut RenderContext,
        _layout_state: &ViewLayoutStateStore,
    ) -> ViewRenderElements {
        let e = match self.entity {
            Some(ref e) => {
                // relayout
                ctx.composite_tree
                    .begin_mod_chain(e.ct_root)
                    .rect_imm(layout_rect.clone())
                    .apply();
                ctx.ht_manager.mod_chain(e.ht_root).rect(layout_rect);

                e
            }
            None => {
                // first render
                let ct_root = CompositeRect::build()
                    .rect_imm(layout_rect.clone())
                    .composite_fill_color_imm([1.0, 1.0, 1.0, 0.125])
                    .opacity_imm(0.0)
                    .create(ctx.composite_tree);
                let ht_root = HitTestTreeData::build()
                    .rect(layout_rect)
                    .cursor_shape(self.dir.cursor_shape())
                    .create(ctx.ht_manager);

                let eh = Rc::new(Entity {
                    dir: self.dir,
                    controlling_dock: Cell::new(self.controlling_dock),
                    ct_root,
                    ht_root,
                    pressing: Cell::new(false),
                    drag_delta: Cell::new(0.0),
                });
                ctx.ht_manager.set_action_handler(eh.ht_root, &eh);

                &*self.entity.insert(eh)
            }
        };

        ViewRenderElements {
            composite_tree: Some(e.ct_root),
            hit_tree: Some(e.ht_root),
            ..ViewRenderElements::EMPTY
        }
    }

    fn teardown(&mut self, ctx: &mut TeardownContext) {
        let Some(entity) = self.entity.take() else {
            // not rendered
            return;
        };

        ctx.composite_tree.free_all(entity.ct_root);
        ctx.ht_manager.free_all(entity.ht_root);
    }

    fn measure_preferred_content_size(&self, _ctx: &mut MeasureContext) -> Size<LogicalUnit> {
        Size::new_logical(
            DESIGN_METRICS.splitter_thickness,
            DESIGN_METRICS.splitter_thickness,
        )
    }
}

/// Splitterの実態/イベントハンドラ
struct Entity {
    /// 分割方向
    dir: Direction,
    /// 制御対象のDock
    controlling_dock: Cell<DockID>,
    /// ビジュアルツリー
    ct_root: CompositeTreeRef,
    /// 入力ツリー
    ht_root: HitTestTreeRef,
    /// ポインタ押下中か？
    pressing: Cell<bool>,
    /// ドラッグ操作のオフセット
    drag_delta: Cell<f32>,
}
impl HitTestTreeActionHandler for Entity {
    fn on_pointer_down(
        &self,
        sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.drag_delta.set(match self.dir {
            Direction::Horizontal => {
                args.client_pos.x - context.ht_manager.compute_global_rect_autoroot(sender).0
            }
            Direction::Vertical => {
                args.client_pos.y - context.ht_manager.compute_global_rect_autoroot(sender).1
            }
        });
        self.pressing.set(true);

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::CAPTURE_ELEMENT
    }

    fn on_pointer_move(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        if !self.pressing.get() {
            return EventContinueControl::empty();
        }

        self.r#move(
            &args.client_pos,
            context.dock_store,
            &mut super::PaneContentResizeContext {
                view_instance_store: context.view_instance_store,
                view_render_queue: context.view_render_queue,
                view_tree_relation_store: context.view_tree_relation_store,
            },
        );
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_drag_move(
        &self,
        _sender: HitTestTreeRef,
        context: &mut InputEventContext,
        args: &PointerActionArgs,
    ) -> EventContinueControl {
        self.r#move(
            &args.client_pos,
            context.dock_store,
            &mut super::PaneContentResizeContext {
                view_instance_store: context.view_instance_store,
                view_render_queue: context.view_render_queue,
                view_tree_relation_store: context.view_tree_relation_store,
            },
        );
        EventContinueControl::STOP_PROPAGATION
    }

    fn on_pointer_up(
        &self,
        _sender: HitTestTreeRef,
        _context: &mut InputEventContext,
        _args: &PointerButtonActionArgs,
    ) -> EventContinueControl {
        self.pressing.set(false);

        EventContinueControl::STOP_PROPAGATION | EventContinueControl::RELEASE_CAPTURE_ELEMENT
    }
}
impl Entity {
    /// 動かす
    fn r#move(
        &self,
        client_pos: &Point<PointerInputUnit>,
        dock_store: &mut super::DockStore,
        resize_context: &mut super::PaneContentResizeContext,
    ) {
        super::move_splitter(
            self.controlling_dock.get(),
            dock_store,
            self.dir.dominant_coordinate(client_pos) + self.drag_delta.get(),
            resize_context,
        );
    }
}
